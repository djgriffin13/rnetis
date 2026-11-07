# Backend adapter generics.
#
# Real S3 dispatch rather than plain per-backend functions, because several
# backends (stats, lme4, lnam, amen) produce one fitted-object shape.
# Adding a further backend means implementing these four
# generics for a new `backend` class; fit_influence() itself never changes.
#
# `backend` is a small classed identifier (.new_backend()), not the backend
# package itself -- dispatch happens on it so a backend module can hold
# per-backend state later (e.g. control options) without changing the
# generic signatures.

#' Identify the estimator behind a backend
#'
#' @param backend a `netis_backend_*` identifier from `.new_backend()`.
#' @return a list with `name` (the rnetis backend name), `package` (the package
#'   actually doing the estimation), and `version` (that package's version, as a
#'   string). Recorded on the fit so a result can say what produced it.
#' @noRd
backend_info <- function(backend) UseMethod("backend_info")

#' Fit the assembled model with this backend's estimator
#'
#' @param backend a `netis_backend_*` identifier.
#' @param formula the estimating formula, already translated into whatever
#'   syntax this backend expects.
#' @param data the assembled estimating table.
#' @param ... backend-specific control arguments.
#' @return the backend package's own fitted object, which stays inside the
#'   adapter: nothing above `backend_extract()` may touch it.
#' @noRd
backend_fit <- function(backend, formula, data, ...) UseMethod("backend_fit")

#' Translate a backend's fitted object into rnetis's own result vocabulary
#'
#' What makes backends replaceable, so it is the part that must not drift: every
#' adapter returns the same named list, and a quantity the backend
#' does not supply is `NULL` or `NA` rather than fabricated or approximated
#' from something else (absent, never invented).
#'
#' @param backend a `netis_backend_*` identifier.
#' @param fit the object `backend_fit()` returned.
#' @return a named list. `coefficients`, `vcov` and `convergence` (a list
#'   carrying at least `converged`) are supplied by every adapter (`stats`,
#'   `lme4`, `lnam`, `amen`). The rest are present where the estimator has
#'   them and absent where it does not:
#'   \itemize{
#'     \item `residuals` — `stats`, `lme4` and `lnam`; **named by the
#'       estimating table's row labels**, so a dependence diagnostic can align
#'       them after listwise deletion without reaching into a backend object.
#'       `amen` supplies none.
#'     \item `loglik`, `sigma`, `residual_df`, `r_squared`, `adj_r_squared`,
#'       `random_effects`, `var_corr` — where the estimator defines them.
#'     \item `rho`, `w1_spectral_radius`, `sar_operator_min_modulus`,
#'       `sar_operator_invertible`, `dropped_nodes` — `lnam` only.
#'     \item `aliased` and `leverage` — `stats` only. `aliased` is carried out
#'       deliberately because `lm()` does not fail on a dropped term, it sets
#'       the coefficient `NA` and continues, so the fact is invisible
#'       downstream unless the adapter reports it. `leverage` is a summary
#'       against the conventional `2p/n` reference, never a per-row verdict.
#'   }
#'   A quantity the backend does not supply is `NULL`/`NA`, never fabricated or
#'   approximated from something else.
#' @noRd
backend_extract <- function(backend, fit) UseMethod("backend_extract")

#' Predict from a backend's fitted object
#'
#' @param backend a `netis_backend_*` identifier.
#' @param fit the object `backend_fit()` returned.
#' @param newdata a table with the estimating table's columns.
#' @return a numeric vector of predictions, one per row of `newdata`.
#' @noRd
backend_predict <- function(backend, fit, newdata) UseMethod("backend_predict")

#' @noRd
.new_backend <- function(name) {
  structure(list(name = name), class = c(paste0("netis_backend_", name), "netis_backend"))
}
