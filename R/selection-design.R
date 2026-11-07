# Design-array assembly for fit_selection().
#
# Translates a parsed Selection specification into the three predictor blocks
# amen consumes -- row (sender), column (receiver), and dyadic -- for one
# (group, outcome wave). Everything here is rnetis-side: the adapter receives
# finished arrays and never reaches back into netis_data.
#
# Performance note. Each term resolves its own data through
# netis_nodes()/netis_ties(), so a specification with several terms
# reconstructs the long-form frames several times per (group, wave). That
# looks like an obvious hoisting opportunity and is not worth taking: design
# assembly is a negligible fraction of the cost of the amen fit that follows
# it. Hoisting would trade readable per-term resolution for a saving nobody
# can observe. Revisit only if a profile on real data says otherwise.
#
# Two rules govern the output and neither is negotiable downstream.
#
# Column names are constructed here, not inherited. amen returned ".dyad" for
# a single named dyadic covariate (verified against 1.4.5), so trusting the
# backend to carry names through would put ".dyad" in front of users in
# summary(). The names the parser resolved are the names that ship.
#
# No NA reaches the arrays. amen's design_array() silently replaces NA with
# zero, and for a lagged tie predictor zero asserts that no prior tie existed.
# Every cell is resolved, masked, or explicitly zero-filled before this
# returns, and .assert_design_complete() checks it at the boundary.

#' Assemble the predictor blocks for one (group, wave)
#'
#' @param spec a parsed Selection formula.
#' @param data a [netis_data()] object.
#' @param group the group to assemble.
#' @param wave the outcome wave.
#' @param ids the group's actor roster, in the order the outcome matrix uses.
#' @param binarize `NULL`, or the binarization rule applied to the outcome. The
#'   same rule is applied to the lagged relation read by [prior_tie()] and
#'   [prior_reciprocity()], so the lagged predictor is the earlier wave's value
#'   of the very tie being modeled.
#' @return a list with `Xrow`, `Xcol`, `Xdyad` (matrices/arrays, or `NULL`
#'   where the specification has no terms of that kind), each carrying rnetis
#'   column names, plus `excluded` and `exclusion_findings`.
#' @noRd
.assemble_selection_design <- function(spec, data, group, wave, ids,
                                       binarize = NULL, lag_sequence = NULL) {
  schema <- netis_schema(data)
  # The waves this fit's lags step back through (see .lag_sequence()). Carried
  # on the schema copy so that every helper below resolves a lag the same way:
  # a lag of 1 reads `from` of the named transition, never the wave that
  # happens to sit before `to` in the data.
  schema$lag_sequence <- lag_sequence
  n <- length(ids)

  # Every predictor that cannot be resolved for a dyad makes that dyad's
  # linear predictor incomputable, so the dyads are accumulated here and
  # masked out of the outcome once, rather than each term deciding
  # independently what to do about its own gaps.
  unresolvable <- matrix(FALSE, n, n, dimnames = list(ids, ids))
  findings <- list()

  # The focal outcome is built before the predictor blocks so that data-loss
  # findings can be counted against dyads the model could actually have used:
  # a covariate gap on a cell the outcome already excludes (a prohibited
  # self-tie, a structurally absent dyad) is not lost data and must not be
  # reported as such.
  focal <- .build_selection_y(data, spec$outcome, group, wave, ids)
  # AN AME MODEL HAS NO SELF-DYAD. amen::ame() and the hierarchical sampler both
  # treat the diagonal as missing, so a self-tie the data permits and records
  # never reaches the likelihood. It leaves the outcome here, is reported, and
  # is not counted as a dyad by anything that reads the design afterwards.
  n_self <- sum(diag(focal$status) == "observed")
  if (n_self > 0L) {
    findings <- c(findings, list(.selection_self_ties_finding(n_self)))
    diag(focal$Y) <- NA_real_
    diag(focal$status) <- "structural"
  }
  focal_observed <- focal$status == "observed"

  node_block <- function(terms_list, role) {
    if (length(terms_list) == 0L) {
      return(NULL)
    }
    cols <- vector("list", length(terms_list))
    for (i in seq_along(terms_list)) {
      col <- .selection_node_column(data, schema, terms_list[[i]], group, wave, ids)
      cols[[i]] <- col$values
      if (any(!col$resolvable)) {
        # A missing sender attribute costs that actor's outgoing dyads; a
        # missing receiver attribute costs the incoming ones. The actor stays
        # in the fit -- their other role is still estimable -- which is why
        # this masks rather than refusing the whole specification.
        affected <- if (identical(role, "row")) {
          !col$resolvable %o% rep(TRUE, n)
        } else {
          rep(TRUE, n) %o% !col$resolvable
        }
        unresolvable <<- unresolvable | affected
        findings <<- c(findings, list(.selection_missing_predictor_finding(
          terms_list[[i]], ids[!col$resolvable]
        )))
      }
    }
    out <- do.call(cbind, cols)
    dimnames(out) <- list(ids, vapply(terms_list, function(t) t$name, character(1)))
    out
  }

  Xrow <- node_block(spec$senders, "row")
  Xcol <- node_block(spec$receivers, "col")

  dyad_layers <- list()
  dyad_names <- character(0)
  excluded_total <- 0L

  for (term in spec$dyads) {
    layer <- .selection_dyad_column(data, schema, term, group, wave, ids)
    dyad_layers <- c(dyad_layers, list(layer$M))
    dyad_names <- c(dyad_names, term$name)
    # Counted against otherwise-estimable dyads only: a covariate gap on a
    # cell the outcome already excludes (the diagonal under prohibited
    # self-ties, a structurally absent outcome dyad) costs nothing and would
    # overstate the loss if reported.
    affected <- !layer$resolvable & focal_observed
    if (any(affected)) {
      unresolvable <- unresolvable | !layer$resolvable
      findings <- c(findings, list(.selection_missing_predictor_finding(
        term, character(0), n_dyads = sum(affected)
      )))
    }
  }

  Y <- focal$Y
  status <- focal$status

  # Dyads whose predictors could not be resolved leave the outcome before the
  # lagged terms are considered, so a dyad lost to a missing covariate is not
  # counted again as lost to a missing lag.
  newly_masked <- unresolvable & status == "observed"
  Y[newly_masked] <- NA_real_
  status[newly_masked] <- "missing"
  excluded_total <- excluded_total + sum(newly_masked)

  for (entry in list(
    list(term = spec$prior_tie, reciprocal = FALSE),
    list(term = spec$prior_reciprocity, reciprocal = TRUE)
  )) {
    term <- entry$term
    if (is.null(term)) {
      next
    }
    lag_wave <- .selection_lag_wave(schema, wave, term$lag)
    lagged <- .build_selection_y(data, spec$outcome, group, lag_wave, ids)
    # The diagonal is not a dyad, at any wave.
    if (any(diag(lagged$status) == "observed")) {
      diag(lagged$Y) <- NA_real_
      diag(lagged$status) <- "structural"
    }
    if (!is.null(binarize)) {
      lag_obs <- lagged$status == "observed" & !is.na(lagged$Y)
      lagged$Y[lag_obs] <- .apply_outcome_binarize(lagged$Y[lag_obs], binarize,
                                                   spec$outcome)
    }
    if (!entry$reciprocal) {
      .assert_lag_differs_from_outcome(
        if (is.null(binarize)) focal$Y else .apply_outcome_binarize(focal$Y, binarize, spec$outcome),
        focal$status, lagged, spec$outcome, group, wave, lag_wave
      )
    }
    resolved <- .resolve_lagged_tie_predictor(
      list(Y = Y, status = status), lagged, reciprocal = entry$reciprocal
    )
    # Masking compounds: a dyad excluded by either lagged term is excluded
    # from the outcome for both, so the running Y/status carry forward rather
    # than each term masking an independent copy.
    Y <- resolved$Y
    status <- resolved$status
    dyad_layers <- c(dyad_layers, list(resolved$x))
    dyad_names <- c(dyad_names, term$name)
    excluded_total <- excluded_total + resolved$n_excluded
    findings <- c(findings, .diagnose_selection_lag_exclusions(
      resolved$n_excluded, sum(status == "observed"),
      sprintf("%s()", .selection_term_label(class(term)))
    ))
  }

  Xdyad <- NULL
  if (length(dyad_layers) > 0L) {
    Xdyad <- array(
      unlist(dyad_layers), dim = c(n, n, length(dyad_layers)),
      dimnames = list(ids, ids, dyad_names)
    )
  }

  for (block in list(list(x = Xrow, what = "sender terms"),
                     list(x = Xcol, what = "receiver terms"),
                     list(x = Xdyad, what = "dyadic terms"))) {
    if (!is.null(block$x)) {
      .assert_design_complete(block$x, block$what)
    }
  }

  list(
    Y = Y, status = status,
    Xrow = Xrow, Xcol = Xcol, Xdyad = Xdyad,
    n_excluded = excluded_total,
    exclusion_findings = findings
  )
}

#' Resolve the wave a lag points at
#'
#' Waves are discrete and ordered but not necessarily numeric (v1 lags are
#' wave positions, not elapsed time), so a lag steps back through the fit's
#' wave sequence rather than subtracting from a wave label. The sequence is
#' `schema$lag_sequence` when the fit set one -- the observed waves up to
#' `from`, then `to`, so a lag of 1 reads `from` -- and the observed waves
#' otherwise.
#'
#' @noRd
.selection_lag_wave <- function(schema, wave, lag) {
  sequence <- schema$lag_sequence %||% schema$waves
  pos <- match(as.character(wave), as.character(sequence))
  if (is.na(pos)) {
    stop(sprintf("Wave '%s' is not one of the observed waves.", wave), call. = FALSE)
  }
  target <- pos - lag
  if (target < 1L) {
    stop(
      sprintf(
        "A lag of %d from wave '%s' reaches before the first wave available to this fit (%s). Reduce the lag, or supply data at earlier waves.",
        lag, wave, paste(sequence, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  sequence[[target]]
}

#' One node-level predictor column, with the actors it cannot supply
#'
#' A missing value is masked and counted rather than raised as an error. That
#' is consistent with how the same problem is handled one function away -- a
#' missing lagged tie masks its dyad and is counted -- and an error would be
#' the harsher choice: one missing attribute would refuse an entire fit, when
#' the actor's dyads can be masked while the actor stays in the model through
#' their other role.
#'
#' The zero-fill is placeholder only. Every dyad it touches is masked out of
#' the outcome by the caller, so no fitted quantity depends on the value; it
#' exists because the backend requires a complete array.
#'
#' @return a list with `values` (numeric, zero-filled where missing) and
#'   `resolvable` (logical, per actor).
#' @noRd
.selection_node_column <- function(data, schema, term, group, wave, ids) {
  nodes <- netis_nodes(data)
  target_wave <- if (term$lag == 0L) wave else .selection_lag_wave(schema, wave, term$lag)
  rows <- .filter_group_wave(nodes, schema, as.character(group), target_wave)
  values <- rows[[term$variable]][match(ids, as.character(rows[[schema$node_col]]))]

  resolvable <- !is.na(values)
  values[!resolvable] <- 0
  list(values = as.numeric(values), resolvable = resolvable)
}

#' Report dyads lost to a predictor the data could not supply
#'
#' Separate from the lagged-tie exclusion finding because the cause differs
#' and so does the fix: a missing covariate is a gap in the attribute data,
#' not in the network history.
#'
#' @noRd
.selection_missing_predictor_finding <- function(term, actor_ids, n_dyads = NULL) {
  label <- .selection_term_label(class(term))
  detail <- if (length(actor_ids) > 0L) {
    sprintf(
      "actor(s) %s have no value, so their dyads in that role are affected",
      paste(actor_ids, collapse = ", ")
    )
  } else {
    sprintf("%d dyad(s) have no value", n_dyads)
  }
  .finding(
    "WARNING", "data", "selection_predictor_unresolvable",
    sprintf(
      "%s(%s) could not be resolved for every dyad: %s. Those dyads are excluded from the outcome rather than being given a filled-in predictor value -- a zero would assert a real quantity (no attribute, no tie) rather than an absent one. Check whether the gaps concentrate in particular actors or waves, since dyads lost this way need not be a random subset.",
      label, term$variable, detail
    ),
    details = list(term = term$name, variable = term$variable,
                   actors = actor_ids, n_dyads = n_dyads)
  )
}

#' One dyadic predictor layer, with the dyads it cannot supply
#'
#' Built through the same three-way-status machinery as the outcome. Cells the
#' data cannot supply are zero-filled so the backend receives a complete array
#' (no NA reaches the backend), **and reported**, so the outcome can be masked for exactly those
#' dyads.
#'
#' The zero-fill alone would be the defect this package forbids elsewhere: a
#' missing `advice` tie becoming `advice = 0` asserts that no advice
#' relationship existed, which is a claim about the world rather than an
#' absence of information. `prior_tie()` gets mask-count-warn treatment,
#' and an ordinary dyadic covariate is the same problem and gets
#' the same treatment rather than a quieter one.
#'
#' @return a list with `M` (zero-filled layer) and `resolvable` (logical
#'   matrix, `TRUE` where the covariate is genuinely observed).
#' @noRd
.selection_dyad_column <- function(data, schema, term, group, wave, ids) {
  target_wave <- if (term$lag == 0L) wave else .selection_lag_wave(schema, wave, term$lag)
  built <- .build_selection_y(data, term$variable, group, target_wave, ids)
  if (any(diag(built$status) == "observed")) {
    # The diagonal is not a dyad (see .assemble_selection_design()).
    diag(built$status) <- "structural"
  }

  # A covariate cell supplies a value only when it is genuinely observed. A
  # *structural* covariate cell -- a prohibited self-tie, or a zero the schema
  # declares to mean structural absence -- has no defined value either: filling
  # it with 0 and keeping the dyad would assert "covariate = 0" for a cell
  # where the covariate does not exist, the exact measured-zero /
  # structural-absence conflation the package forbids for the outcome (and the
  # treatment prior_tie() applies to its own structural lag cells).
  # Both non-observed kinds are therefore unresolvable here; the caller
  # intersects with the focal outcome's observed cells before masking or
  # counting, which is what keeps the diagonal of every complete network --
  # structural in covariate and outcome alike -- from being reported as
  # missing data.
  resolvable <- built$status == "observed"
  M <- built$Y
  M[built$status != "observed"] <- 0
  list(
    M = M[ids, ids, drop = FALSE],
    resolvable = resolvable[ids, ids, drop = FALSE]
  )
}

#' Report self-ties that an AME model cannot use
#'
#' @noRd
.selection_self_ties_finding <- function(n_self) {
  .finding(
    "WARNING", "data", "selection_self_ties_not_modeled",
    sprintf(
      "%d observed self-tie(s) were left out of this fit. The data permits self-ties, but an AME model has no dyad of an actor with itself, so they cannot enter the likelihood and are not counted as dyads anywhere in the result.",
      n_self
    ),
    details = list(n_self_ties = n_self)
  )
}

#' Refuse a lagged relation that is the outcome itself
#'
#' The static-relation refusal (`.check_selection_static_outcome()`) sees the
#' whole panel, so it misses a tie variable that changes somewhere in the panel
#' but is identical at the two waves this fit reads. There too the lagged
#' predictor is the response, and no defensible estimate exists.
#'
#' @noRd
.assert_lag_differs_from_outcome <- function(focal_Y, focal_status, lagged, variable,
                                             group, wave, lag_wave) {
  both <- focal_status == "observed" & lagged$status == "observed"
  if (any(both) && all(focal_Y[both] == lagged$Y[both])) {
    stop(
      sprintf(
        "Tie variable '%s' in group '%s' is identical at waves '%s' and '%s', so a lagged relation term between them would be the response itself. Choose a transition over which the relation changes, or drop the lagged term.",
        variable, group, lag_wave, wave
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
