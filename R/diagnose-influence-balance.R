# Unbalanced-design diagnostics for Influence fits.
#
# The default backend behavior for an incomplete row is listwise deletion. That
# is a reasonable default and an invisible one: a node missing a value at the
# wave a term reads loses its one row, and nothing in the output says so. This file
# reports it, so that data is not silently dropped.
#
# These are diagnostics, not refusals. An unbalanced panel is a normal thing
# to have; what is not acceptable is not being told.

#' Diagnose units that contribute nothing to the fit
#'
#' @param tbl the assembled estimating table, *before* the backend drops
#'   incomplete rows.
#' @param schema the data schema.
#' @param predictor_cols the assembled predictor columns, used to determine
#'   which rows the backend will actually keep.
#' @noRd
.diagnose_influence_balance <- function(tbl, schema, predictor_cols) {
  findings <- list()
  if (nrow(tbl) == 0L) {
    return(.combine_diagnostics(findings))
  }

  key <- .netis_unit_key(tbl[[schema$group_col]], tbl[[schema$node_col]])

  # A row survives to the fit only if the outcome and every predictor it uses
  # are present. This is the backend's own rule, computed here so the report
  # describes the sample that will actually be estimated rather than the one
  # that was assembled.
  used_cols <- c(".outcome", predictor_cols)
  used_cols <- used_cols[used_cols %in% names(tbl)]
  complete <- stats::complete.cases(tbl[, used_cols, drop = FALSE])

  rows_assembled <- table(key)
  rows_complete <- table(factor(key[complete], levels = names(rows_assembled)))

  # Units dropped from the fit: the case listwise deletion makes least visible.
  fully_dropped <- names(rows_complete)[rows_complete == 0L & rows_assembled > 0L]
  if (length(fully_dropped) > 0L) {
    findings <- c(findings, list(.finding(
      "WARNING", "design", "units_dropped_entirely",
      sprintf(
        "%d of %d %s(s) contribute no usable rows and will be dropped from the fit entirely, because every one of their rows has a missing outcome or predictor. Examples: %s.",
        length(fully_dropped), length(rows_assembled), schema$node_col,
        paste(utils::head(sub("\r", "/", fully_dropped), 5L), collapse = ", ")
      ),
      details = list(n_dropped = length(fully_dropped), units = fully_dropped)
    )))
  }

  .combine_diagnostics(findings)
}

#' Diagnose whole groups that never reach the estimating sample
#'
#' A group observed at some waves but not at the ones the fit resolved to
#' contributes **nothing** and vanishes without trace. `.diagnose_influence_balance()`
#' cannot see it: that function works from the assembled table, and a group with
#' no rows at the resolved waves was never assembled into it, so there is no row
#' whose absence could be noticed.
#'
#' Example: with three groups and group 2 missing at wave 3, `waves = c(2, 3)`
#' produces a two-group fit whose only other diagnostic would be `too_few_groups_for_random_effects` -- a symptom, and one
#' that names the wrong cause. Dropping an entire group is dropping data, which
#' must be recorded and surfaced.
#'
#' A WARNING rather than a NOTE. In the rnetis data model a group is a separate
#' network, so this is not one unit lost at the margin: it is a whole network's
#' worth of observations leaving the estimand, and on multi-group data the
#' number of groups is what carries the inferential information for anything
#' group-level.
#'
#' @param data the `netis_data` the fit was given.
#' @param schema the data schema.
#' @param tbl the assembled estimating table.
#' @param waves the resolved wave pair or sequence, for the message.
#' @noRd
.diagnose_groups_absent <- function(data, schema, tbl, waves) {
  in_data <- unique(as.character(netis_nodes(data)[[schema$group_col]]))
  in_fit <- unique(as.character(tbl[[schema$group_col]]))
  missing_groups <- setdiff(in_data, in_fit)
  if (length(missing_groups) == 0L) {
    return(.combine_diagnostics(list()))
  }
  .combine_diagnostics(list(.finding(
    "WARNING", "design", "groups_absent_at_resolved_waves",
    sprintf(
      "%d of %d %s(s) contribute no rows at the resolved wave(s) %s and are absent from the fit entirely: %s. Each is a separate network, so this is not a marginal loss of observations -- the estimand is now over the remaining %d %s(s), and any group-level quantity's precision follows that reduced count rather than the row count. Resolve to waves those groups were observed at, or fit them separately.",
      length(missing_groups), length(in_data), schema$group_col,
      paste(waves, collapse = " -> "),
      paste(utils::head(missing_groups, 5L), collapse = ", "),
      length(in_fit), schema$group_col
    ),
    details = list(absent = missing_groups, n_absent = length(missing_groups),
                   n_present = length(in_fit), waves = waves)
  )))
}

#' Which groups had an unobserved network, per exposure term
#'
#' @param resolved the fit's `resolved_exposures`.
#' @noRd
.absent_network_groups_by_term <- function(resolved) {
  lapply(resolved, function(r) {
    unique(c(r$absent_network_groups, r$absent_network_history_groups))
  })
}

#' Diagnose exposure lost to missingness in alters, not in the node itself
#'
#' A node's exposure is defined only if every alter it has a tie to is
#' observed, so node-level missingness **amplifies** into exposure-level
#' missingness at a rate that rises steeply with degree. If a share `m` of
#' nodes are missing the content variable and a node has `d` alters, the
#' chance its exposure is defined is about `(1 - m)^d`.
#'
#' That is not a rounding effect: at degree 30, a 5% node-level missingness
#' rate leaves about `0.95^30`, or roughly a fifth, of exposures defined, and
#' 10% leaves about `0.9^30`, or roughly 4%.
#'
#' It deserves its own finding because the arithmetic is counter-intuitive and
#' the loss is invisible in the input data. A user looking at 5% missing
#' outcomes has no reason to expect to lose most of the analysis sample, and
#' the rows disappear inside the backend's listwise deletion with nothing
#' saying why. Note also that no amount of full-information likelihood
#' machinery recovers it: the regressor's *inputs* are gone, which is a
#' different problem from the regressor being unobserved.
#'
#'
#' @param tbl the assembled estimating table.
#' @param exposure_cols names of the assembled exposure columns.
#' @param absent_by_term named list mapping exposure column to the groups whose
#'   network was unobserved, from `.absent_network_groups_by_term()`.
#' @param group_col the schema's group column, for locating those rows.
#' @noRd
.diagnose_exposure_missingness <- function(tbl, exposure_cols,
                                           absent_by_term = list(), group_col = NULL) {
  findings <- list()
  exposure_cols <- exposure_cols[exposure_cols %in% names(tbl)]
  if (length(exposure_cols) == 0L || nrow(tbl) == 0L) {
    return(.combine_diagnostics(findings))
  }
  outcome_missing <- if (".outcome" %in% names(tbl)) mean(is.na(tbl[[".outcome"]])) else 0

  for (col in exposure_cols) {
    is_missing <- is.na(tbl[[col]])

    # ATTRIBUTION BEFORE MAGNITUDE. An exposure undefined because the network at
    # that snapshot was never recorded has a different cause and a different fix
    # from one undefined because an alter's value is missing, and asserting the
    # second for both would point a user at their alters' data when the problem is a
    # missing network. Rows in an affected group are removed
    # from the alter-amplification count and reported separately.
    absent_groups <- absent_by_term[[col]]
    from_absent_network <- if (length(absent_groups) > 0L && !is.null(group_col) &&
                                 group_col %in% names(tbl)) {
      is_missing & as.character(tbl[[group_col]]) %in% absent_groups
    } else {
      rep(FALSE, nrow(tbl))
    }
    if (any(from_absent_network)) {
      findings <- c(findings, list(.finding(
        "WARNING", "design", "exposure_missing_via_absent_network",
        sprintf(
          "%.0f%% of '%s' values are undefined because the network itself was not observed at that snapshot, not because an alter's value is missing. The affected %s(s): %s. Nothing about the alters' data will fix this -- either record the missing snapshot's ties, or resolve the fit to waves where the network was observed.",
          100 * mean(from_absent_network), col, group_col %||% "group",
          paste(utils::head(absent_groups, 5L), collapse = ", ")
        ),
        details = list(column = col, groups = absent_groups,
                       share_missing = mean(from_absent_network))
      )))
    }

    is_missing <- is_missing & !from_absent_network
    share <- mean(is_missing)
    if (share == 0) {
      next
    }
    # ONE SEVERITY, NOT TWO. Escalating to a WARNING above some share would
    # need package-owned cutoffs turning a continuous quantity into a severity
    # -- a statistic becoming a finding on magnitude. Making the cutoffs
    # configurable would relocate the arbitrariness rather than remove it.
    #
    # The share and the outcome's own missingness are both in the message and
    # both in `details`, so a reader can apply whatever standard their design
    # calls for. What is NOT rnetis's to decide is where 19% stops being fine
    # and 21% starts being a problem.
    findings <- c(findings, list(.finding(
      "NOTE",
      "design", "exposure_missing_via_alters",
      sprintf(
        "%.0f%% of '%s' values are undefined because an alter's value is missing, against %.0f%% of outcomes missing. A node's exposure needs every alter observed, so node-level missingness amplifies with degree -- roughly (1 - m)^d of nodes keep a defined exposure for missingness rate m and degree d. These rows are dropped by the fit. Reducing the conduit's degree, or a network with fewer alters per node, lessens the loss; imputing alters' values would change the estimand and is not done for you.",
        100 * share, col, 100 * outcome_missing
      ),
      details = list(
        column = col, share_missing = share, outcome_share_missing = outcome_missing
      )
    )))
  }
  .combine_diagnostics(findings)
}
