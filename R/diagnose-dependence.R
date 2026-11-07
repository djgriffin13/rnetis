# The `dependence` diagnostic family.
#
# TWO OUTPUTS, AND THE SPLIT IS THE POINT. This file returns descriptive
# QUANTITIES and, separately, FINDINGS. A quantity whose substantive reading
# depends on a cutoff is reported as a number and never becomes a finding:
# making the arbitrary threshold user-configurable would
# relocate the epistemic problem rather than solve it. Findings are reserved for
# facts with a threshold-free interpretation -- a statistic could not be
# computed, a statistic is undefined, a specification does not represent
# something it conditions on.
#
# So netis_dependence() reports "residual network autocorrelation: I = 0.31" and
# its findings never include "WARNING: significant residual network
# autocorrelation". There is no reference distribution for the Moran-type
# statistic: rnetis's W is substantively directed, the Cliff-Ord cross-product is
# computable for asymmetric W, and the analytical moments customarily quoted beside
# it are not derived for that object. So the statistic is reported without inference.
#
# Vocabulary rules, encoded here because a
# message that drifts is how a diagnostics family starts implying remedies:
#
#   - disturbance dependence (correlated residuals) and network-induced
#     regressor coupling (one node's outcome is literally an input to another's
#     exposure) are named separately, always;
#   - a remedy for one endogeneity problem is never described as addressing
#     another;
#   - no expected direction of bias is stated for the random-effects case, and
#     "Nickell bias" is not written for it.
#
# THIS FAMILY'S STATUS VOCABULARY IS WIDER THAN computed / not_computed /
# not_applicable, ON PURPOSE. Each status names a distinct cause with a distinct
# remedy -- `not_computable` (no snapshot has enough non-isolated actors) and
# `residuals_unavailable` (this estimator reports none) both mean "not
# computed" and send a reader to completely different places, and collapsing
# them would throw away the only actionable part.

#' Compute the dependence family's quantities and findings
#'
#' @param ctx a context list assembled by `netis_dependence()`. Fields:
#'   `table` (the assembled estimating table), `residuals`/`residual_type`
#'   (from `backend_extract()`), `residual_unavailable_reason`, `schema`,
#'   `exposure_terms` (parsed exposure terms), `data` (the `netis_data`),
#'   `var_corr`,
#'   `re_groupings` (character vector of grouping factors the model estimated),
#'   `group_is_network` (logical).
#' @return a list with `quantities` (a `netis_dependence` object) and
#'   `diagnostics` (a `netis_diagnostics` collection).
#' @noRd
.diagnose_dependence <- function(ctx) {
  findings <- list()

  aligned <- .align_residuals(ctx$table, ctx$residuals)
  # The fit's one outcome wave, as a column of the aligned rows. Data with no wave
  # dimension has no outcome wave at all; NA keeps the column (assigning NULL
  # would delete it) and stands for "the one cross-section".
  if (!is.null(aligned)) {
    aligned$.outcome_wave <- if (length(ctx$to_wave) == 0L) {
      rep(NA, nrow(aligned))
    } else {
      rep(ctx$to_wave, nrow(aligned))
    }
  }

  # A fit analyses one transition or one occasion, so each unit has one outcome
  # row and there is no within-unit sequence for a serial correlation to run
  # along. The statement is the result, on every route.
  serial <- list(
    status = "not_applicable",
    reason = "each unit contributes one outcome row, so there is no within-unit sequence for a lag-1 correlation to run along"
  )
  moran <- .dependence_network_autocorrelation(aligned, ctx)
  eta_squared <- .dependence_residual_eta_squared(aligned, ctx)

  if (identical(moran$status, "all_isolates")) {
    findings <- c(findings, list(.finding(
      "NOTE", "dependence", "network_autocorrelation_undefined",
      sprintf(
        "Residual network autocorrelation is undefined for the '%s' conduit: every actor in every resolved snapshot is an isolate under it, so there are no cross-products to form. An isolate is structurally without alters, which is a different condition from an alter whose value is missing, and no residual statistic distinguishes the two on its own.",
        moran$term
      ),
      details = moran
    )))
  }

  findings <- c(findings, .dependence_structural_notes(ctx))

  list(
    quantities = structure(
      list(serial = serial, network_autocorrelation = moran, residual_eta_squared = eta_squared,
           residual_type = ctx$residual_type,
           # Carried so any later consumer reuses THIS alignment
           # rather than repeating it. The residual-to-row mapping is by row
           # NAME, not position, and doing it twice invites the two copies to
           # drift apart -- at which point two diagnostics would describe
           # different rows while appearing to describe the same fit.
           aligned = aligned),
      class = "netis_dependence"
    ),
    diagnostics = .combine_diagnostics(findings)
  )
}

#' Attach residuals to the estimating table's rows
#'
#' Backends drop incomplete rows, so the residual vector is generally shorter
#' than the table. `lm()`/`lmer()` name their residuals by the model frame's
#' original row labels, which is what makes the mapping recoverable rather than
#' positional -- a positional join would silently misalign every row after the
#' first dropped one, and every downstream statistic would be computed on
#' mismatched pairs while looking perfectly healthy.
#'
#' @return the table with a `.resid` column, rows without a residual removed;
#'   `NULL` when residuals were unavailable.
#' @noRd
.align_residuals <- function(table, residuals) {
  if (is.null(residuals) || length(residuals) == 0L || nrow(table) == 0L) {
    return(NULL)
  }
  nm <- names(residuals)
  # UNNAMED RESIDUALS ARE REFUSED, not aligned by position. A length equal to
  # the table's row count is not evidence that the two are in the same order,
  # and a length SHORTER than it is the ordinary case: the `lnam` adapter drops
  # incomplete rows inside model.frame() and labels what survived, precisely so
  # this join can be by name. A positional fallback would pair the wrong rows
  # the moment anything was dropped, with nothing downstream able to tell. An absent dependence record with its reason is the
  # honest answer; a scrambled one is a wrong number.
  if (is.null(nm)) return(NULL)
  idx <- suppressWarnings(as.integer(nm))
  if (anyNA(idx) || any(idx < 1L) || any(idx > nrow(table))) {
    return(NULL)
  }
  out <- table[idx, , drop = FALSE]
  out$.resid <- as.numeric(residuals)
  out
}

#' A Moran-type cross-product statistic on the residuals, per group x snapshot
#'
#' The operator is the one the exposure term actually applied -- same conduit,
#' same direction, same normalization -- rebuilt through the exposure engine's
#' own `.build_canonical_W()`/`.apply_normalization()` rather than a second
#' construction that could disagree with it (`.dependence_operator()`). Exposure
#' direction and centrality direction are separate settings, and this reads the
#' exposure one, because the question is about the operator that entered the model.
#'
#' Reported per snapshot, one per group at the fit's wave, with the distribution
#' across snapshots rather than one pooled number.
#'
#' The normalized operator is then restricted to the actors with a residual: the
#' statistic is over the residuals that exist, so a tie to an actor with no residual
#' has nothing to pair with. Its weight still counts in the row sum the term was
#' divided by, so a restricted row need not sum to one.
#'
#' NO REFERENCE DISTRIBUTION. The statistic is reported with its
#' weight total and node count and nothing else -- no analytical moments, no
#' permutation, no standardized deviate, no p-value. It is not confined to
#' `[-1, 1]`, with or without row normalization: for an asymmetric `W` its bound is
#' set by the symmetric part of `W`, whose largest eigenvalue can exceed one even
#' when every row sums to one.
#'
#' @noRd
.dependence_network_autocorrelation <- function(aligned, ctx) {
  terms <- ctx$exposure_terms
  if (length(terms) == 0L) {
    return(list(status = "not_applicable",
                reason = "the specification has no exposure term, so there is no operator to test residuals against"))
  }
  if (is.null(aligned)) {
    return(list(status = "residuals_unavailable", reason = ctx$residual_unavailable_reason))
  }

  s <- ctx$schema
  nodes_all <- netis_nodes(ctx$data)
  ties_all <- netis_ties(ctx$data)
  per_term <- list()

  for (term in terms) {
    snapshots <- .dependence_snapshots(aligned, s, term, ctx$waves)
    rows <- list()
    skipped <- list()
    n_all_isolate <- 0L

    for (k in seq_len(nrow(snapshots))) {
      gid <- as.character(snapshots$group[k])
      # No wave dimension: the one cross-section, read with no wave filter.
      no_waves <- length(s$waves) == 0L
      wv <- if (no_waves) NULL else snapshots$wave[[k]]
      sel <- as.character(aligned[[s$group_col]]) == gid &
        (no_waves | as.character(aligned$.outcome_wave) %in% as.character(snapshots$to_wave[[k]]))
      snap <- aligned[sel, , drop = FALSE]
      snap <- snap[!is.na(snap$.resid), , drop = FALSE]
      ids <- as.character(snap[[s$node_col]])
      if (length(ids) == 0L) next

      ties_focal <- .filter_group_wave(ties_all, s, gid, wv)
      W <- .dependence_operator(term, ctx, s, gid, wv, ties_focal, ids,
                                snapshots$to_wave[[k]], nodes_all, ties_all)
      W[!is.finite(W)] <- 0
      W <- W[ids, ids, drop = FALSE]

      n_non_isolate <- sum(rowSums(abs(W)) > 0)
      if (n_non_isolate == 0L) {
        n_all_isolate <- n_all_isolate + 1L
        skipped[[length(skipped) + 1L]] <- list(
          group = gid, wave = wv, reason = "every actor is an isolate under this conduit")
        next
      }
      # A package convention. Below 10 non-isolated actors the cross-product is
      # dominated by a handful of pairs; the skip is reported with the snapshot
      # named rather than the snapshot vanishing from the series.
      if (n_non_isolate < 10L) {
        skipped[[length(skipped) + 1L]] <- list(
          group = gid, wave = wv,
          reason = sprintf("only %d non-isolated actors (10 required)", n_non_isolate))
        next
      }

      z <- snap$.resid - mean(snap$.resid)
      s0 <- sum(W)
      denom <- sum(z^2)
      if (!is.finite(s0) || s0 == 0 || denom == 0) {
        skipped[[length(skipped) + 1L]] <- list(
          group = gid, wave = wv,
          reason = "the weight total or the residual variance is zero")
        next
      }
      rows[[length(rows) + 1L]] <- data.frame(
        group = gid, wave = if (is.null(wv)) NA_character_ else as.character(wv),
        I = .moran_cross_product(z, W),
        n = length(z), n_non_isolate = n_non_isolate, weight_total = s0,
        stringsAsFactors = FALSE
      )
    }

    if (n_all_isolate > 0L && length(rows) == 0L) {
      per_term[[term$name]] <- list(status = "all_isolates", term = term$name)
      next
    }
    if (length(rows) == 0L) {
      per_term[[term$name]] <- list(
        status = "not_computable", term = term$name,
        skipped = skipped,
        reason = "no resolved snapshot met the minimum of 10 non-isolated actors")
      next
    }
    tbl <- do.call(rbind, rows)
    per_term[[term$name]] <- list(
      status = "computed", term = term$name,
      conduit = .conduit_label(term$conduit), normalize = term$normalize,
      direction = term$direction,
      by_snapshot = tbl,
      median = stats::median(tbl$I), n_snapshots = nrow(tbl),
      skipped = skipped
    )
  }

  # The all-isolate case is promoted to the top level only when it is the whole
  # story, so the finding fires on a genuinely degenerate conduit rather than on
  # a fit that has one bad snapshot among many.
  statuses <- vapply(per_term, function(p) p$status, character(1))
  if (length(statuses) && all(statuses == "all_isolates")) {
    return(c(per_term[[1]], list(per_term = per_term)))
  }
  list(status = "computed", per_term = per_term)
}

#' The operator a term applied, for the actors of one snapshot
#'
#' The row sums a normalized operator is divided by depend on which actors the
#' matrix holds at that moment, and the routes differ, so each is rebuilt as it ran.
#'
#' An ordinary exposure term is computed before the estimating sample is known, over
#' the content wave's roster and everyone the ties it reads reach
#' (`.exposure_matrix_ids()`). An actor who later has no residual still counts in its
#' alters' denominators, so the matrix is built and divided over that whole set and
#' only then restricted by the caller.
#'
#' The simultaneous term is fitted by `sna::lnam()`, whose adapter removes incomplete
#' rows from `W1` first and normalizes afterwards (`.lnam_normalize_w1()`), so there
#' the matrix is divided over the actors with a residual.
#'
#' `data`/`group`/`wave` are threaded so a derived conduit resolves through the
#' construction layer the fit used.
#'
#' `isolate_rule` is not asserted again: the fit applied it to the rows it reports, and
#' the caller treats an isolate's row as zero under every rule, so `"zero"` here gives
#' the same operator without a second refusal.
#'
#' @param ids the snapshot's actors with a residual.
#' @param to_wave the outcome wave.
#' @return the normalized operator, dimnamed by node id.
#' @noRd
.dependence_operator <- function(term, ctx, s, gid, wv, ties_focal, ids, to_wave,
                                 nodes_all, ties_all) {
  build <- function(over) {
    .build_canonical_W(
      ties_focal, over,
      source_col = s$source_col, target_col = s$target_col,
      value_col = term$conduit, direction = term$direction,
      directed = isTRUE(s$directed),
      data = ctx$data, group = gid, wave = wv
    )
  }
  simultaneous <- identical(ctx$backend, "lnam") &&
    length(.find_endogenous_exposures(list(term), ctx$outcome)) > 0L
  if (simultaneous) {
    return(.apply_normalization(build(sort(unique(ids))), term$normalize, "zero"))
  }

  roster <- unique(as.character(
    nodes_all[[s$node_col]][as.character(nodes_all[[s$group_col]]) == gid]
  ))
  # The wave the exposure was resolved at: its content wave, or for a history
  # mechanism one step later, with every earlier wave's ties in use.
  history <- .mechanism_reads_history(term$mechanism)
  resolve_wave <- if (length(s$waves) == 0L) {
    NULL
  } else {
    sequence <- .lag_sequence(s$waves, ctx$waves)
    sequence[[match(as.character(to_wave), as.character(sequence)) -
                as.integer(term$x_lag) + as.integer(history)]]
  }
  focal_ids <- as.character(
    .filter_group_wave(nodes_all, s, gid, resolve_wave)[[s$node_col]]
  )
  ties_used <- if (history) {
    .filter_group_waves(ties_all, s, gid, c(resolve_wave, .history_waves(s, resolve_wave)))
  } else {
    ties_focal
  }
  over <- .exposure_matrix_ids(unique(c(focal_ids, ids)), ties_used, s, roster)
  .apply_normalization(build(over), term$normalize, "zero")
}

#' The Cliff-Ord cross-product statistic
#'
#' `I = (n / S0) * (z' W z) / (z' z)` on already-centered `z`. Isolates -- rows
#' of `W` that are entirely zero -- contribute nothing to the numerator and
#' nothing to `S0`, so they need no special handling here; they are counted and
#' reported by the caller.
#'
#' Separated out so it can carry a known-answer test. The whole statistic is
#' four arithmetic operations, and testing it only through a fitted model would
#' mean never checking the arithmetic against a value computed by hand.
#'
#' @param z centered residuals.
#' @param W the operator, in canonical `W[i,j] = j influences i` orientation.
#' @return a single numeric, **not** confined to `[-1, 1]`. Row normalization does
#'   not confine it either, when `W` is asymmetric.
#' @noRd
.moran_cross_product <- function(z, W) {
  (length(z) / sum(W)) * as.numeric(t(z) %*% W %*% z) / sum(z^2)
}

#' The (group, snapshot) grid a term's operator is resolved on
#'
#' The operator is the term's NETWORK, so it is built at the wave the conduit
#' was read at: `w_lag` (which defaults to `x_lag`) steps back from the outcome
#' wave, **positionally** through the fit's wave sequence, never by arithmetic
#' on the wave label. With observed waves 1, 3, 5, a lag of 1 from wave 5 is
#' wave 3; subtracting 1 from the label would give a wave that does not exist.
#' The sequence is the fit's own (`.lag_sequence()`), so for a named transition
#' `c(from, to)` a lag of 1 is `from` even when the two are not adjacent.
#'
#' Data with no wave dimension has one snapshot per group, with `wave` `NA`.
#'
#' @param waves the fit's analyzed waves (`spec$waves`), or `NULL`.
#' @return a data frame of `group`, `to_wave` (the outcome occasion) and `wave`
#'   (the occasion the operator is built at, in the schema's own label type).
#' @noRd
.dependence_snapshots <- function(aligned, schema, term, waves = NULL) {
  grid <- unique(aligned[, c(schema$group_col, ".outcome_wave"), drop = FALSE])
  names(grid) <- c("group", "to_wave")
  observed <- schema$waves
  if (length(observed) == 0L) {
    grid$wave <- rep(NA, nrow(grid))
    return(grid)
  }
  lag <- as.integer(term$w_lag %||% term$x_lag)
  sequence <- if (length(waves) > 0L) .lag_sequence(observed, waves) else observed
  pos <- match(as.character(grid$to_wave), as.character(sequence)) - lag
  keep <- !is.na(pos) & pos >= 1L
  grid <- grid[keep, , drop = FALSE]
  # Carried in the schema's own label type rather than as a character, because
  # the tie table stores waves in their original type and the filter compares
  # against it directly.
  grid$wave <- sequence[pos[keep]]
  grid
}

#' Eta-squared of the residuals on the group factor
#'
#' A one-way decomposition -- between-group sum of squares over total -- reported as
#' a proportion beside the group count, because a group-level quantity's inferential
#' information is carried by the number of groups and not by the number of rows. No
#' score test and no p-value: this is a described quantity, not a hypothesis.
#'
#' With no group effect at all its expectation is `(J - 1) / (N - 1)`, which is
#' positive, so the value is returned beside it. An intraclass correlation is a
#' different quantity: ICC(1) subtracts the within-group mean square and this does not.
#'
#' @noRd
.dependence_residual_eta_squared <- function(aligned, ctx) {
  s <- ctx$schema
  if (is.null(aligned)) {
    return(list(status = "residuals_unavailable", reason = ctx$residual_unavailable_reason))
  }
  groups <- as.character(aligned[[s$group_col]])
  n_groups <- length(unique(groups))

  if (n_groups <= 1L) {
    return(list(status = "undefined", n_groups = n_groups,
                reason = "there is one group, so there is no between-group sum of squares. Undefined is not zero."))
  }
  if (n_groups < 5L) {
    return(list(status = "not_computed", n_groups = n_groups,
                reason = sprintf("eta-squared from %d groups is not usable; the floor is 5", n_groups)))
  }
  if (s$group_col %in% ctx$re_groupings) {
    # Restating it from residuals would report a number near zero BY
    # CONSTRUCTION -- the estimator has already absorbed that grouping -- and a
    # near-zero number reads as reassurance. The fitted component is the honest
    # answer to the same question.
    vc <- ctx$var_corr
    sd <- if (!is.null(vc)) vc$sdcor[vc$grp == s$group_col & is.na(vc$var2)][1] else NA_real_
    return(list(status = "modeled", n_groups = n_groups, grouping = s$group_col,
                fitted_sd = sd,
                reason = "the estimator models this grouping, so eta-squared of its residuals would be near zero by construction rather than by evidence; the fitted variance component is reported instead"))
  }

  r <- aligned$.resid
  keep <- is.finite(r)
  r <- r[keep]
  g <- groups[keep]
  grand <- mean(r)
  means <- tapply(r, g, mean)
  counts <- tapply(r, g, length)
  between <- sum(counts * (means - grand)^2)
  total <- sum((r - grand)^2)
  if (!is.finite(total) || total == 0) {
    return(list(status = "not_computed", n_groups = n_groups,
                reason = "the residuals have no variance"))
  }
  list(status = "computed", eta_squared = between / total, n_groups = n_groups,
       n_rows = length(r),
       # What it averages with no group effect at all: positive, and larger the
       # smaller the groups, so the value is never read against zero.
       expected_no_group_effect = (n_groups - 1) / (length(r) - 1),
       # The information is carried by the group count. Stated in the object, not
       # only in whatever prints it.
       information_carried_by = "the number of groups",
       group_is_network = isTRUE(ctx$group_is_network))
}

#' The two structural notes: network formation, and the network-exposure route
#'
#' Both are statements about what the SPECIFICATION does and does not represent.
#' Neither asserts that a defect is present, because neither is knowable from a
#' fit: a network may have been experimentally assigned or physically
#' determined, and the fit has no way to tell.
#'
#' @noRd
.dependence_structural_notes <- function(ctx) {
  findings <- list()

  # Unconditional: no Influence specification in this package models network
  # formation.
  findings <- c(findings, list(.finding(
    "NOTE", "dependence", "network_formation_not_assessed",
    "This influence specification conditions on the observed network. The fit does not establish whether the network can be treated as exogenous; that depends on the network's formation and design process, which this specification does not model. An experimentally assigned or physically determined structure may well be exogenous -- the fit simply has no way to tell, so it reports what it did rather than what is true of the world.",
    details = list(family = "influence")
  )))

  # DERIVED FROM THE SPECIFICATION, not from a classifier. The note is
  # assumption INFORMATION rather than routing -- it says that an exposure to
  # the OUTCOME through the network is regressor coupling rather than
  # disturbance dependence, which is true of the specification whatever
  # estimator runs -- so the condition is read off the terms directly.
  #
  # Any lag counts. A lagged same-outcome exposure couples one node's
  # regressor to another node's realized outcome just as a contemporaneous one
  # does; the lag changes the timing, not the coupling. The note names the timing
  # the specification has.
  outcome_terms <- Filter(function(e) identical(e$content, ctx$outcome),
                          ctx$exposure_terms %||% list())
  if (length(outcome_terms) > 0L) {
    contemporaneous <- vapply(outcome_terms, function(e) identical(as.integer(e$x_lag), 0L),
                              logical(1))
    lead <- if (all(contemporaneous)) {
      "A contemporaneous exposure to the outcome"
    } else if (any(contemporaneous)) {
      "An exposure to the outcome, contemporaneous or lagged,"
    } else {
      "A lagged exposure to the outcome"
    }
    findings <- c(findings, list(.finding(
      "NOTE", "dependence", "network_exposure_route_present",
      paste(lead, "introduces a distinct potential endogeneity route -- separate from the temporal one, and present whether or not ar() is in the formula -- unless network assignment or formation can be treated as exogenous, or is modeled. Note also that this is regressor coupling, not disturbance dependence: one node's observed outcome is literally an input to another node's exposure, so the rows are linked through the predictors even under conditionally independent errors. A cluster-robust standard error addresses some of the second kind and none of the first."),
      details = list(outcome_exposure_terms = vapply(
        outcome_terms, function(e) e$name %||% e$content, character(1)
      ))
    )))
  }

  findings
}

#' Print or format the residual dependence quantities
#'
#' How the `netis_dependence` object from [netis_dependence()] displays: the
#' residual-dependence quantities rnetis computes, each with its status, and an
#' explicit line wherever one was skipped and why.
#'
#' # Descriptive quantities, not tests
#'
#' Everything here is reported as a **described quantity**. None of it is
#' compared to a threshold, none of it carries a p-value, and none of it
#' produces a pass/fail statement about your model.
#'
#' The network-autocorrelation quantity is the sharpest case. rnetis ships **no
#' reference distribution for a directed operator**, so there is no null against
#' which the statistic could be read, and the output says so on its own line
#' rather than leaving a bare number inviting the reader to supply a null from
#' memory.
#'
#' # A skip is information, not a blank
#'
#' Each quantity states why it was not computed when it was not: fewer than 10
#' non-isolated actors in a snapshot, no exposure term, one group, fewer than 5
#' groups, or a grouping the estimator already models. An absent number with a
#' stated reason is a usable result; a silently missing one is not.
#'
#' # What this does not address
#'
#' These quantities describe **disturbance dependence** — correlation among the
#' residuals. They say nothing about **network-induced regressor coupling**,
#' where one node's observed outcome is literally an input to another node's
#' exposure, which creates cross-row linkage even under conditionally
#' independent errors. Nor do they address network-formation endogeneity, which
#' is a property of how the network arose and is not detectable from a fit.
#'
#' @param x a `netis_dependence` object, from [netis_dependence()].
#' @param ... unused; present for S3 consistency with the generics.
#' @return `format()` a character vector, one element per line. `print()`
#'   returns `x` invisibly and is called for its output.
#' @seealso [fit_influence()], [summary.netis_influence()],
#'   [netis-diagnostics-methods] for the findings collection these accompany
#' @name netis-dependence-methods
NULL

#' @rdname netis-dependence-methods
#' @export
format.netis_dependence <- function(x, ...) {
  lines <- "Residual dependence structure:"

  lines <- c(lines, "  Within-unit serial correlation: not applicable (one outcome row per unit).")

  lines <- c(lines, .format_dependence_moran(x$network_autocorrelation))

  eta <- x$residual_eta_squared
  lines <- c(lines, switch(
    eta$status,
    computed = sprintf(
      "  Residual eta-squared on group (between-group over total sum of squares): %.3f over %d groups and %d rows. With no group effect its expectation is (J - 1) / (N - 1) = %.3f, so it is not read against zero. Its inferential information is carried by the number of groups, not the number of rows%s.",
      eta$eta_squared, eta$n_groups, eta$n_rows, eta$expected_no_group_effect,
      if (isTRUE(eta$group_is_network)) "; and because the group IS the network here, this cannot separate group-level shocks from between-network differences" else ""),
    undefined = sprintf("  Residual eta-squared on group: undefined -- %s", eta$reason),
    not_computed = sprintf("  Residual eta-squared on group: not computed -- %s.", eta$reason),
    modeled = sprintf(
      "  Residual eta-squared on group: not reported -- %s. Fitted SD for '%s': %s.",
      eta$reason, eta$grouping,
      if (is.na(eta$fitted_sd)) "unavailable" else sprintf("%.4f", eta$fitted_sd)),
    residuals_unavailable = "  Residual eta-squared on group: unavailable (no per-row residuals).",
    sprintf("  Residual eta-squared on group: %s.", eta$status)
  ))

  lines
}

#' @noRd
.format_dependence_moran <- function(m) {
  if (identical(m$status, "not_applicable")) {
    return("  Residual network autocorrelation: not applicable (no exposure term).")
  }
  if (identical(m$status, "residuals_unavailable")) {
    return("  Residual network autocorrelation: unavailable (no per-row residuals).")
  }
  if (identical(m$status, "all_isolates")) {
    return(sprintf("  Residual network autocorrelation ('%s'): undefined -- every actor is an isolate.", m$term))
  }
  unlist(lapply(m$per_term, function(p) {
    head <- switch(
      p$status,
      computed = sprintf(
        "  Residual network autocorrelation ('%s', %s, normalize = \"%s\"): median I = %.3f over %d snapshot(s). On a directed operator this is not confined to [-1, 1].",
        p$term, p$direction, p$normalize, p$median, p$n_snapshots),
      all_isolates = sprintf("  Residual network autocorrelation ('%s'): undefined -- every actor is an isolate.", p$term),
      sprintf("  Residual network autocorrelation ('%s'): %s -- %s", p$term, p$status, p$reason %||% "")
    )
    skips <- if (length(p$skipped)) {
      sprintf("    %d snapshot(s) skipped: %s.", length(p$skipped),
              paste(vapply(p$skipped, function(s) sprintf("group %s%s (%s)", s$group,
                                                                       if (is.null(s$wave)) "" else paste0(" wave ", s$wave),
                                                                       s$reason),
                           character(1)), collapse = "; "))
    } else {
      NULL
    }
    # No reference distribution is quoted, and the absence is stated rather than
    # left to be noticed: a bare statistic invites the reader to supply a null.
    tail <- if (identical(p$status, "computed")) {
      "    Reported as a described quantity: rnetis ships no reference distribution for a directed operator, so there is no p-value to read here."
    } else {
      NULL
    }
    c(head, skips, tail)
  }), use.names = FALSE)
}

#' @rdname netis-dependence-methods
#' @export
print.netis_dependence <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  # The findings carry the REASONS -- why a quantity is undefined, what the
  # specification does not model. Printing them here rather than leaving them
  # for a second call means a reader of a missing number sees why it is
  # missing in the same place.
  if (!is.null(x$diagnostics) && length(x$diagnostics) > 0L) {
    cat("\n")
    print(x$diagnostics)
  }
  invisible(x)
}

#' Residual dependence in a fitted Influence model
#'
#' Reports what the fit's residuals look like with respect to the network: a
#' Moran-type residual network autocorrelation, and eta-squared of the residuals on
#' the group factor where there are enough groups to report it. Within-unit
#' serial correlation is reported as not applicable (see below).
#'
#' @section Computed on request, not at fit time:
#' [fit_influence()] returns estimates, and this reads a completed fit. It is
#' separate for two reasons: it is the expensive part (it rebuilds the conduit
#' matrix from the data), and a fit that silently carried an interpretation would
#' invite the interpretation to be read as part of the estimate.
#'
#' @section Quantities, not verdicts:
#' Everything here is a **number with its provenance**, never a pass or a
#' fail. A quantity whose substantive reading depends on a cutoff is reported
#' and does not become a finding: making the cutoff configurable would
#' relocate the arbitrariness rather than remove it. So this returns
#' "residual network autocorrelation: I = 0.31" and never "WARNING:
#' significant residual network autocorrelation".
#'
#' In particular there is **no reference distribution** for the Moran-type
#' statistic and none is implied. rnetis's `W` is substantively directed, the
#' Cliff-Ord cross-product is computable for an asymmetric `W`, and the
#' analytical moments usually quoted beside it are not derived for that
#' object, so the statistic is reported without inference. It is not confined to
#' `[-1, 1]`: on an asymmetric `W` it can exceed that range even when every row
#' sums to one.
#'
#' Findings are reserved for facts that need no cutoff: a statistic could not
#' be computed, a statistic is undefined, or the specification does not model
#' something it conditions on.
#'
#' @section What a two-wave fit can and cannot show:
#' Each unit contributes one outcome row, so there is no within-unit sequence and
#' **within-unit serial correlation is not applicable**, which the record states
#' (`$serial$status`, `$serial$reason`). That is not the same as showing there is
#' none: it is unmeasured.
#'
#' @section The operator:
#' The network autocorrelation reads the matrix the term applied, with its conduit,
#' direction and normalization. An ordinary exposure term is normalized over every
#' actor it was computed for, so an actor outside the estimating sample still counts
#' in its alters' row sums. The simultaneous (`sna::lnam`) term is normalized over
#' the actors the fit retained, because that is how `W1` is fitted. Either way the
#' statistic is then computed over the actors with a residual.
#'
#' @section Residual eta-squared:
#' `$residual_eta_squared$eta_squared` is the between-group sum of squares of the
#' residuals over their total sum of squares, on the group factor. It is descriptive,
#' and it is above zero in expectation with no group effect at all: `(J - 1) / (N - 1)`
#' for `J` groups and `N` rows, which the record gives as `expected_no_group_effect`.
#' That expectation is larger when groups are small (Bliese and Halverson 1998), so a
#' value is read against it and not against zero. It is a different quantity from an
#' intraclass correlation, and comparing it with one misleads; [icc1()] reports ICC(1)
#' of the outcome.
#'
#' @section Package conventions:
#' Two floors are this package's own, not published values. The statistic is computed
#' for a snapshot with at least 10 non-isolated actors, and eta-squared for at least
#' 5 groups. Below them the record gives the reason in place of a number.
#'
#' @param object a fitted model from [fit_influence()].
#' @param data the [netis_data()] the fit was built from. Required, because the
#'   network autocorrelation rebuilds the conduit matrix and a fitted object does not
#'   retain its data. It is not recovered from the fit's call, which holds only a
#'   name that can refer to another object by the time this runs, and nothing checks
#'   that the object passed here is the one fitted.
#' @return an object of class `netis_dependence`, with a `print()` method and a
#'   [plot()][netis-dependence-plot] method: a list of
#'   \describe{
#'     \item{`serial`}{`status` (`"not_applicable"`) and `reason`.}
#'     \item{`network_autocorrelation`}{`status` and, when computed, `per_term`: for
#'       each exposure term its `status`, `term`, `conduit`, `normalize`, `direction`,
#'       `by_snapshot` (a data frame of `group`, `wave`, `I`, `n`, `n_non_isolate`,
#'       `weight_total`), `median`, `n_snapshots` and `skipped`.}
#'     \item{`residual_eta_squared`}{the record for eta-squared of the residuals on
#'       the group factor: `status` (`"computed"`, `"undefined"`, `"not_computed"`,
#'       `"modeled"` or `"residuals_unavailable"`), `n_groups`, and when computed
#'       `eta_squared`, `n_rows`, `expected_no_group_effect` (see above),
#'       `information_carried_by` and `group_is_network`; when modeled, `grouping` and
#'       `fitted_sd`; otherwise `reason`.}
#'     \item{`residual_type`}{`"response"` (least squares), `"conditional"`
#'       (`lme4`) or `"filtered"` (`sna::lnam`).}
#'     \item{`aligned`}{the estimating rows with their residual in `.resid`.}
#'     \item{`diagnostics`}{the findings, a `netis_diagnostics` collection.}
#'   }
#' @references
#' Moran, P. A. P. (1950) Notes on continuous stochastic phenomena. *Biometrika*
#' **37**(1-2), 17-23.
#'
#' Cliff, A. D. and Ord, J. K. (1981) *Spatial Processes: Models and Applications*.
#' Pion, London.
#'
#' Bliese, P. D. and Halverson, R. R. (1998) Group size and measures of group-level
#' properties: an examination of eta-squared and ICC values. *Journal of Management*
#' **24**(2), 157-172.
#'
#' Richardson, J. T. E. (2011) Eta squared and partial eta squared as measures of
#' effect size in educational research. *Educational Research Review* **6**(2),
#' 135-147.
#' @seealso [fit_influence()], [icc1()], [netis_assumptions()] -- assumption A3 is the
#'   one these quantities bear on, and [influence_se_comparison()] for what a
#'   clustered variance does about it
#' @examples
#' set.seed(1)
#' ids <- letters[1:12]
#' nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = 12),
#'                     y = stats::rnorm(24), x = stats::rnorm(24))
#' ties <- data.frame(group = 1, source = ids[-12], target = ids[-1],
#'                    wave = 1, tie = 1)
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
#' netis_dependence(fit, nd)
#' @export
netis_dependence <- function(object, data) {
  if (!inherits(object, "netis_influence")) {
    stop("netis_dependence() is defined for netis_influence fits.", call. = FALSE)
  }
  # REQUIRED, AND NEVER RECOVERED FROM THE FIT'S CALL. The call holds a symbol, and
  # by the time this runs that symbol can name a different object, which would give
  # a dependence record for another network with nothing to say so.
  if (missing(data)) {
    stop(
      "netis_dependence() needs the netis_data the fit was built from: a fitted object does not retain its data. Pass it: netis_dependence(fit, your_data).",
      call. = FALSE
    )
  }
  .check_netis_data(data)

  s <- object$spec$data_schema
  waves <- object$spec$waves
  re_terms <- object$spec$random_effects %||% list()

  out <- .diagnose_dependence(list(
    table = object$model_frame,
    residuals = object$residuals,
    residual_type = object$residual_type,
    residual_unavailable_reason = NULL,
    schema = s,
    data = data,
    to_wave = if (length(waves)) waves[[length(waves)]] else NULL,
    waves = waves,
    exposure_terms = object$spec$exposures,
    outcome = object$spec$outcome,
    backend = object$backend,
    var_corr = object$var_corr,
    re_groupings = vapply(re_terms, function(rt) rt$group, character(1)),
    # In the rnetis data model a group IS a separate network -- ties never
    # cross groups -- so residual eta-squared on group can never separate
    # group-level shocks from between-network differences. Stated as a field
    # rather than assumed downstream.
    group_is_network = TRUE
  ))

  # ONE OBJECT, NOT TWO. `.diagnose_dependence()` returns the quantities and
  # the findings separately because `fit_influence()` files them in
  # different slots of the fitted object. A user asking for the dependence
  # record wants both, and the findings are the part that says why a quantity
  # is missing -- separating them at this surface would hand back a number
  # with its explanation in another return value.
  out$quantities$diagnostics <- out$diagnostics
  out$quantities
}
