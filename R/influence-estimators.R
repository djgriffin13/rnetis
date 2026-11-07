# The Influence estimator registry.
#
# ESTIMATOR IS NOT BACKEND. An *estimator* identifies the statistical procedure
# and the modeling assumptions used. A *backend* is the computational
# implementation of a given estimator, selected internally, because changing
# backend must not change the estimand.
#
# TWO ENTRIES, AND NEITHER IS CHOSEN BY NAME. `pooled` and `mixed` are two
# estimators, but the choice between them is carried by the FORMULA -- the
# presence or absence of a random-effects term -- not by an argument. The
# registry exists so that an ordinary least-squares fit is *labeled* as one
# rather than reported as "the mixed-effects fit with a backend it never used".
#
# WHAT IS NOT HERE. Dynamic-panel estimators (correlated random effects,
# difference and system GMM, wide-format likelihood models) need more than the one
# transition a fit analyses. They are published methods excluded by scope, not by
# any doubt about them, and nothing here may imply otherwise. With the
# estimator selected by the formula there is no eligibility or routing
# machinery to decide between estimators.
#
# `lnam` is the third entry, for the contemporaneous network-autocorrelation
# route. It is a cross-sectional simultaneous model and needs none of the
# machinery above. Like the other two it is never named by the user: the
# FORMULA selects it, by putting the outcome's own contemporaneous exposure on
# the right-hand side.

#' The estimator registry
#'
#' Each entry records what the estimator is called, which backend implements it,
#' and, where more than the backend's own package is needed, what it requires.
#'
#' @return a named list of registry entries.
#' @noRd
.estimator_registry <- function() {
  list(
    pooled = list(
      label = "pooled ordinary least squares",
      backend = "stats"
    ),
    mixed = list(
      label = "mixed-effects (lme4)",
      backend = "lme4"
    ),
    lnam = list(
      label = "simultaneous network autocorrelation (sna::lnam)",
      backend = "lnam",
      requires = c("sna", "numDeriv")
    )
  )
}

#' Resolve which estimator a specification uses
#'
#' The formula decides, in every case. A same-outcome contemporaneous
#' `exposure()` term means `lnam`; a random-effects term means `mixed`; neither
#' means `pooled`. There is no `estimator =` argument and no name to pass,
#' because two ways to express one choice is how they drift apart.
#'
#' The endogenous term is checked FIRST because it is the strongest constraint:
#' the specification is not estimable by least squares at all, so nothing about
#' the random-effects term could override it. `fit_influence()` refuses the
#' combination of the two upstream, so the ordering here decides nothing that
#' could reach this point ambiguous -- it is written in precedence order so
#' that it stays correct if that refusal is ever relaxed.
#'
#' @param has_random_effects whether the parsed formula carries a
#'   random-effects term.
#' @param has_endogenous whether it carries a same-outcome contemporaneous
#'   exposure term.
#' @return the estimator name, carrying a `reason` attribute for `summary()`.
#' @noRd
.resolve_estimator <- function(has_random_effects, has_endogenous = FALSE) {
  if (isTRUE(has_endogenous)) {
    return(structure(
      "lnam",
      reason = "the formula puts the outcome's own contemporaneous network exposure on the right-hand side"
    ))
  }
  if (isTRUE(has_random_effects)) {
    structure("mixed", reason = "the formula declares a random-effects term")
  } else {
    structure("pooled", reason = "the formula declares no random-effects term")
  }
}

#' Refuse an estimator whose backend package is not installed
#'
#' Checked before the backend runs, so the message names the package and the
#' estimator rather than surfacing as an opaque failure inside the adapter.
#'
#' @param estimator_name the resolved estimator.
#' @return `TRUE` invisibly, or an error.
#' @noRd
.assert_estimator_available <- function(estimator_name) {
  reg <- .estimator_registry()[[estimator_name]]
  if (is.null(reg)) {
    stop(sprintf("Internal: unknown estimator '%s'.", estimator_name), call. = FALSE) # nocov
  }
  # `requires` where an entry declares one, falling back to the single-package
  # mapping. lnam needs TWO: sna for the estimator, and numDeriv for the
  # numerically differentiated information matrix its standard errors come
  # from. numDeriv is sna's own Suggests, so a fresh sna install can lack it
  # and the fit then silently returns no standard errors -- which is why it is
  # required here rather than left to chance.
  pkgs <- reg$requires %||% switch(reg$backend, lme4 = "lme4", stats = NULL, NULL)
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "The %s estimator needs the %s package%s, which %s not installed: %s. %s",
        estimator_name,
        if (length(pkgs) > 1L) "" else "'", if (length(pkgs) > 1L) "s" else "'",
        if (length(missing) == 1L) "is" else "are",
        paste(missing, collapse = ", "),
        if (identical(estimator_name, "lnam")) {
          "Install them, or replace the contemporaneous exposure() term with a lagged one, which is fitted without them."
        } else {
          "Install it, or fit without the random-effects term to use ordinary least squares instead."
        }
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' One-line estimator description for `summary()`
#'
#' @param estimator_name the resolved estimator.
#' @param reason the `reason` attribute `.resolve_estimator()` attached.
#' @return a character vector of display lines.
#' @noRd
.format_estimator <- function(estimator_name, reason = NULL) {
  reg <- .estimator_registry()[[estimator_name]]
  head <- sprintf("Estimator: %s", reg$label)
  if (!is.null(reason) && nzchar(reason)) {
    head <- sprintf("%s (%s)", head, reason)
  }
  head
}
