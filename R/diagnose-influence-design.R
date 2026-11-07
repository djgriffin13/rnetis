# Design diagnostics for fit_influence().
#
# Runs for every fit regardless of backend -- temporal ordering is a
# property of the specification (which wave each term is resolved at),
# not of which estimator ends up fitting it.

#' @noRd
.diagnose_influence_design <- function(exposure_terms, outcome, ar_terms = NULL) {
  findings <- list()

  # An ar() term is not mandatory: a contemporaneous same-outcome
  # specification routed to the simultaneous estimator is a legitimate model
  # with no lagged outcome in it, and requiring one would make that model
  # unwriteable. What the absence does change is interpretation --
  # without a prior-state control, an exposure coefficient carries whatever
  # stable differences between nodes also predict their alters' values. That
  # is a NOTE rather than a warning: it is a property of the design the user
  # chose, not a defect in it.
  if (!is.null(ar_terms) && length(ar_terms) == 0L && length(exposure_terms) > 0L) {
    findings <- c(findings, list(.finding(
      "NOTE", "design", "no_autoregressive_term",
      sprintf(
        "The formula has no ar() term, so the exposure coefficient(s) are not adjusted for '%s' at the prior wave. Stable differences between nodes that also predict their alters' values will load onto exposure. Add ar() if a prior-state control is intended.",
        outcome
      )
    )))
  }
  for (exp_term in exposure_terms) {
    # Contemporaneity is a property of the lag itself, not of wave labels:
    # x_lag = 0 means the focal wave under both positional and label
    # arithmetic, so no wave subtraction is needed (or valid -- lags are
    # positional steps through the observed wave sequence).
    is_contemporaneous <- identical(as.integer(exp_term$x_lag), 0L)
    is_same_outcome <- identical(exp_term$content, outcome)
    if (is_contemporaneous && !is_same_outcome) {
      findings <- c(findings, list(.finding(
        "WARNING", "design", "contemporaneous_exposure",
        sprintf(
          "exposure(%s, via = %s, x_lag = 0) is measured at the same wave as the outcome, not a prior one. It is fit as an ordinary predictor, but temporal ordering between %s and the outcome is not established by this specification -- contemporaneous confounding may remain. Consider x_lag = 1 (the default) if %s at the prior wave is a more defensible predictor.",
          exp_term$content, .conduit_label(exp_term$conduit), exp_term$content, exp_term$content
        ),
        details = list(term = exp_term$name, content = exp_term$content, conduit = .conduit_label(exp_term$conduit))
      )))
    }
  }
  .combine_diagnostics(findings)
}

#' Name a 0/1 outcome fitted by a linear model
#'
#' All three routes fit a linear model with Gaussian errors. On a 0/1 outcome
#' that is a linear probability model: a choice the user may well have made, and
#' one the coefficient table does not reveal.
#'
#' @noRd
.diagnose_influence_outcome <- function(tbl, outcome) {
  y <- tbl[[".outcome"]]
  y <- y[!is.na(y)]
  if (length(y) == 0L || !all(y %in% c(0, 1))) {
    return(.combine_diagnostics(list()))
  }
  .combine_diagnostics(list(.finding(
    "NOTE", "design", "binary_outcome_fitted_as_gaussian",
    sprintf(
      "The outcome '%s' holds only 0 and 1 in this table. It is fitted by a linear model with Gaussian errors, a linear probability model: fitted values can leave [0, 1] and the residual variance is not constant.",
      outcome
    ),
    details = list(outcome = outcome)
  )))
}

#' Diagnose lagged() terms applied to a genuinely static variable
#'
#' A static variable's value is the same at every wave, so lagging it
#' changes nothing -- often a sign the wrong variable was named (a genuinely
#' dynamic variable was expected), but sometimes exactly what was intended
#' (a deliberate baseline covariate written as lagged() for documentation
#' clarity). NOTE, not WARNING or ERROR. Level-resolution errors (unknown variable,
#' tie-level, ambiguous node/group) are not re-raised here -- they already
#' surface with a clear message from .assemble_influence_table(), which
#' calls .resolve_lagged_level() directly; this function only needs to know
#' whether a *successfully resolved* term happens to be static, so an
#' unresolvable term is simply skipped rather than reported twice.
#'
#' @noRd
.diagnose_influence_lagged <- function(lagged_terms, schema) {
  findings <- list()
  for (lt in lagged_terms) {
    level <- tryCatch(.resolve_lagged_level(schema, lt$variable), error = function(e) NULL)
    if (is.null(level)) {
      next
    }
    is_static <- if (identical(level, "node")) {
      lt$variable %in% schema$node_vars$static
    } else {
      lt$variable %in% schema$group_vars$static
    }
    if (is_static) {
      findings <- c(findings, list(.finding(
        "NOTE", "design", "lagged_static_variable",
        sprintf(
          "lagged(%s) requests a lag of a variable that does not vary across waves in this data, so lagging it has no effect -- the same value is returned regardless of the requested lag. This is often intentional (e.g. a baseline covariate written as lagged() for clarity), but is surfaced in case it is not.",
          lt$variable
        ),
        details = list(variable = lt$variable, level = level)
      )))
    }
  }
  .combine_diagnostics(findings)
}
