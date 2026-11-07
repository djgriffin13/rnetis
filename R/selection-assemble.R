# Outcome-matrix assembly for fit_selection().
#
# The whole difficulty here is that a Selection outcome matrix has three kinds
# of cell, not two, and collapsing any pair of them is a specific, known
# error:
#
#   observed   -- the dyad's tie status is known. A 0 here is information: the
#                 pair could have formed a tie and did not.
#   missing    -- the dyad's status is unknown. Excluded from the likelihood.
#   structural -- the dyad cannot exist (a self-tie under a prohibiting
#                 policy, or a zero the schema declares to mean structural
#                 absence). Scoring it 0 would tell the model a pair declined
#                 to form a tie that was never available to them.
#
# A recoding such as `M[M == 0] <- NA` collapses observed-zero into missing, discarding
# exactly the observations that carry the most information about what does NOT
# form. The opposite collapse -- scoring a structurally impossible dyad as an
# observed 0 -- is equally wrong and easier to commit by accident, because a
# matrix of zeros looks complete.
#
# Which reading applies is not guessable from the values; it comes from the
# per-tie-variable descriptors (presence_mode, zero_meaning), and
# where those leave it undetermined this refuses rather than defaulting.

#' @noRd
.SELECTION_CELL_STATUS <- c("observed", "missing", "structural")

#' Build one Selection outcome matrix for a (group, wave)
#'
#' @param data a [netis_data()] object.
#' @param variable the outcome tie variable.
#' @param group the group to build for.
#' @param wave the wave to build for, or `NULL` for wave-free data.
#' @param ids the actors the matrix covers. `NULL` (the default) is everyone
#'   the group's node data ever names; a fit passes the roster of the waves it
#'   reads, so an actor seen only at a wave the fit does not read has no row.
#' @return a list with `Y` (numeric matrix, `NA` wherever the cell is not
#'   observed), `status` (character matrix over `.SELECTION_CELL_STATUS`), and
#'   `counts` (named integer vector of cell counts by status).
#' @noRd
.build_selection_y <- function(data, variable, group, wave, ids = NULL) {
  schema <- netis_schema(data)
  nodes <- netis_nodes(data)
  ties <- netis_ties(data)
  .assert_columns_present(ties, variable, "tie data")

  descriptor <- schema$tie_descriptors[[variable]]
  if (is.null(descriptor)) {
    stop(sprintf("No schema descriptor found for tie variable '%s'.", variable), call. = FALSE)
  }

  roster <- sort(unique(as.character(nodes[[schema$node_col]][
    as.character(nodes[[schema$group_col]]) == as.character(group)
  ])))
  if (is.null(ids)) ids <- roster
  n <- length(ids)
  Y <- matrix(NA_real_, n, n, dimnames = list(ids, ids))
  status <- matrix("missing", n, n, dimnames = list(ids, ids))

  # Absent rows mean "no tie" only for a sparse edge list. For every other
  # presence_mode a row is what records that the dyad was *observed*, so a
  # dyad with no row was not observed and stays missing, as it does for a
  # network that carries an exposure.
  #
  # A network nobody observed at this wave is the same case one level up: its
  # dyads have no rows because no one looked, so none of them is a known 0. An
  # observed empty network has no rows and every dyad is a known 0.
  observed_by_default <- identical(descriptor$presence_mode, "row_presence") &&
    !.network_absent_at_wave(ties, .filter_group_wave(ties, schema, as.character(group), wave),
                             schema, as.character(group), wave)
  if (observed_by_default) {
    status[] <- "observed"
    Y[] <- 0
  }

  # A prohibited self-tie is structurally impossible, not merely absent. This
  # is applied after the default fill so it cannot be overwritten by it.
  if (!identical(schema$self_ties, "permitted")) {
    diag(status) <- "structural"
    diag(Y) <- NA_real_
  }

  rows <- .filter_group_wave(ties, schema, as.character(group), wave)
  .assert_tie_endpoints_on_roster(rows, schema, as.character(group), roster)
  if (nrow(rows) > 0L) {
    src <- as.character(rows[[schema$source_col]])
    tgt <- as.character(rows[[schema$target_col]])
    keep <- src %in% ids & tgt %in% ids
    # A tie recorded AT this wave for an actor the node data does not place at
    # the waves this fit reads contradicts the roster. (A static tie table is
    # reused at every wave, so its rows for such an actor simply do not apply.)
    if (any(!keep) && !variable %in% schema$tie_vars$static) {
      absent <- setdiff(c(src, tgt), ids)
      stop(
        sprintf(
          "Tie variable '%s' in group '%s' has rows at wave '%s' for actor(s) with no node row at the waves this fit reads: %s. Add their node rows, or remove those ties.",
          variable, group, wave, paste(utils::head(absent, 5), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    src <- src[keep]
    tgt <- tgt[keep]
    vals <- rows[[variable]][keep]

    if (length(src) > 0L) {
      idx <- cbind(src, tgt)
      # A stored zero is the ambiguous case, and its meaning is declared, not
      # inferred. "unknown" is only an error when such a zero actually exists
      # -- demanding a declaration the data never needed would be noise.
      is_zero <- !is.na(vals) & vals == 0
      if (any(is_zero) && identical(descriptor$zero_meaning, "unknown")) {
        stop(
          sprintf(
            "Tie variable '%s' has stored zero values but its zero_meaning is undeclared, so rnetis cannot tell whether a zero means the pair was observed not to form a tie (information the model should use) or that the dyad could not exist (a cell that must be excluded). Say which in netis_data(). If the pair was observed not to form a tie, pass:\n  %s\nIf the dyad could not exist, pass:\n  %s",
            variable,
            .tie_meta_example(variable, "zero_meaning = \"measured_zero\""),
            .tie_meta_example(variable, "zero_meaning = \"structural_absence\"")
          ),
          call. = FALSE
        )
      }

      Y[idx] <- vals
      status[idx] <- "observed"

      # A row that exists but carries no value is not an observation. This is
      # distinct from an absent row: the dyad was in the design and the value
      # is simply missing, which is the ordinary missing-at-random case. Left
      # as "observed" it would put NA into a cell the status matrix calls
      # known, breaking the invariant asserted at the end of this function.
      na_idx <- idx[is.na(vals), , drop = FALSE]
      if (nrow(na_idx) > 0L) {
        status[na_idx] <- "missing"
      }

      if (any(is_zero) && identical(descriptor$zero_meaning, "structural_absence")) {
        zero_idx <- idx[is_zero, , drop = FALSE]
        Y[zero_idx] <- NA_real_
        status[zero_idx] <- "structural"
      }

      # An undirected network stores one row per pair; the reverse cell is the
      # same observation, not an unobserved one.
      if (!isTRUE(schema$directed)) {
        rev_idx <- cbind(tgt, src)
        Y[rev_idx] <- Y[idx]
        status[rev_idx] <- status[idx]
      }
    }
  }

  # The diagonal is re-asserted last: an undirected mirror or a stored
  # self-tie row could otherwise reintroduce a cell the policy prohibits.
  if (!identical(schema$self_ties, "permitted")) {
    diag(status) <- "structural"
    diag(Y) <- NA_real_
  }

  # NA is the backend-facing encoding for both non-observed kinds, so assert
  # the two representations agree rather than trusting them to stay in step.
  stopifnot(all(is.na(Y) == (status != "observed")))

  list(Y = Y, status = status, counts = .selection_status_counts(status))
}

#' @noRd
.selection_status_counts <- function(status) {
  counts <- vapply(
    .SELECTION_CELL_STATUS,
    function(s) sum(status == s),
    integer(1)
  )
  names(counts) <- .SELECTION_CELL_STATUS
  counts
}

#' Resolve a lagged-relation predictor and the estimable set it implies
#'
#' A transition dyad is estimable for a lagged-tie model only when **both** the
#' focal outcome and the required lagged tie are observed. Dyads failing that
#' are excluded from the outcome -- masked, counted, and reported -- rather
#' than being given a fabricated predictor value.
#'
#' The reason this is rnetis's job rather than the backend's was established by
#' source verification against `amen` 1.4.5: `design_array()` replaces `NA` in
#' the design array with **zero**, emitting only a base R warning, and a fit
#' given `NA` at a design cell is bit-identical to one given an explicit `0`
#' there. rnetis does not get to nominate a sentinel -- the backend picks zero
#' -- and for a lagged tie predictor zero is not neutral. It is the
#' substantive claim *"no prior tie existed"*, which is exactly the
#' measured-zero / structural-absence conflation the package forbids
#' everywhere else.
#'
#' So the design row of an excluded dyad is zero-filled **here, deliberately
#' and recorded**, and its outcome cell is masked, so that the value cannot be
#' mistaken for data. The masked cell is still model-imputed by the sampler,
#' which is documented rather than hidden: the imputation consumes the
#' zero-filled row.
#'
#' @param y_focal the focal wave's outcome, from `.build_selection_y()`.
#' @param y_lagged the lagged wave's outcome, from the same.
#' @param reciprocal if `TRUE`, transpose the lagged matrix, giving
#'   `y[j, i]` at the prior wave -- delayed reciprocity rather than
#'   persistence.
#' @return a list with `x` (the predictor matrix, zero-filled where
#'   unresolvable), `Y` (the focal outcome with unresolvable dyads masked),
#'   `excluded` (logical matrix), and `n_excluded`.
#' @noRd
.resolve_lagged_tie_predictor <- function(y_focal, y_lagged, reciprocal = FALSE) {
  lag_status <- y_lagged$status
  lag_values <- y_lagged$Y
  if (reciprocal) {
    lag_status <- t(lag_status)
    lag_values <- t(lag_values)
  }

  if (!identical(dim(lag_values), dim(y_focal$Y)) ||
      !identical(dimnames(lag_values)[[1]], dimnames(y_focal$Y)[[1]])) {
    stop(
      "The focal and lagged outcome matrices do not share an actor set, so a lagged tie cannot be aligned to its dyad. This should have been caught by the fixed-roster check.",
      call. = FALSE
    )
  }

  # A dyad already excluded from the focal wave contributes nothing whether or
  # not its lag resolves, so only cells that would otherwise be estimable can
  # be *newly* excluded. Counting the rest would overstate what was lost.
  focal_observed <- y_focal$status == "observed"
  lag_observed <- lag_status == "observed"
  excluded <- focal_observed & !lag_observed

  Y <- y_focal$Y
  status <- y_focal$status
  Y[excluded] <- NA_real_
  status[excluded] <- "missing"

  x <- lag_values
  # The zero-fill: applied to every non-observed lag cell, including ones
  # whose focal outcome was already unusable, because the backend requires a
  # complete design array regardless of which outcome cells are masked.
  x[!lag_observed] <- 0

  list(
    x = x,
    Y = Y,
    status = status,
    excluded = excluded,
    n_excluded = sum(excluded),
    counts = .selection_status_counts(status)
  )
}

#' Refuse to hand a backend a design array containing NA
#'
#' The no-NA-in-design invariant, asserted at the seam rather than trusted.
#' `amen` would substitute zero and warn; a warning buried in MCMC output is
#' not adequate notice that a predictor was invented, and since rnetis
#' assembles the array rnetis is the party that would have introduced the `NA`.
#'
#' @param x a design matrix or array about to be passed to a backend.
#' @param what a label naming the term, used in the error.
#' @return invisibly `TRUE`, or an error.
#' @noRd
.assert_design_complete <- function(x, what) {
  n_na <- sum(is.na(x))
  if (n_na == 0L) {
    return(invisible(TRUE))
  }
  stop(
    sprintf(
      "Internal error: the design array for %s still contains %d missing value(s) at the backend boundary. amen would silently replace these with zero, which for a lagged tie predictor asserts that no prior tie existed. Every unresolvable dyad must be masked and zero-filled explicitly before this point.",
      what, n_na
    ),
    call. = FALSE
  )
}

#' Report dyads excluded for an unresolvable lag
#'
#' Exclusion is never silent. This is a WARNING rather than a NOTE
#' because the excluded dyads are not missing at random with respect to the
#' model -- a dyad whose prior wave went unobserved is plausibly different
#' from one whose did not.
#'
#' @noRd
.diagnose_selection_lag_exclusions <- function(n_excluded, n_estimable, term_label) {
  if (n_excluded == 0L) {
    return(list())
  }
  list(.finding(
    "WARNING", "data", "lagged_tie_dyads_excluded",
    sprintf(
      "%d dyad(s) were excluded from the outcome because %s could not be resolved: the focal tie was observed but the prior wave's was not. They are masked rather than scored, since filling the predictor with zero would assert that no prior tie existed. %d dyad(s) remain estimable. Dyads whose prior wave went unobserved need not be a random subset, so check whether the exclusions concentrate in particular actors or waves.",
      n_excluded, term_label, n_estimable
    ),
    details = list(n_excluded = n_excluded, n_estimable = n_estimable, term = term_label)
  ))
}

#' Refuse a binary Selection outcome built from a valued tie variable
#'
#' rnetis's invariant that "unweighted" never means `w > 0` applies with full
#' force to the outcome: a valued relation reduced to present/absent by an
#' implicit sign test is a modeling decision made silently. The rule must be
#' explicit and recorded, and the two honest resolutions are named rather than
#' one being picked.
#'
#' The rule is applied through the existing network-construction layer
#' ([binarize()]) rather than a second implementation inside the Selection
#' code, so this function only enforces that a rule was supplied.
#'
#' @param descriptor the tie variable's schema descriptor.
#' @param variable the tie variable name.
#' @param rule a `netis_binarize_rule`, or `NULL`.
#' @param family the requested outcome family.
#' @return invisibly `TRUE`, or an error.
#' @noRd
.assert_binary_outcome_rule <- function(descriptor, variable, rule, family) {
  if (!identical(family, "bin") || !isTRUE(descriptor$weighted) || !is.null(rule)) {
    return(invisible(TRUE))
  }
  stop(
    sprintf(
      "Tie variable '%s' holds values other than 0/1, so a binary Selection model needs an explicit rule for what counts as a tie -- rnetis will not assume one. Either supply a binarization rule (e.g. binarize(%s, rule = nonzero()) or threshold(2)), or fit family = \"nrm\" to model the values themselves.",
      variable, variable
    ),
    call. = FALSE
  )
}
