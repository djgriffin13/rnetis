# Backend adapter for base stats::lm().
#
# Implements the backend_info/fit/extract/predict generics (R/backend-contract.R)
# for the single-network OLS case. Backend objects (an lm fit) never escape
# this file into the public API -- fit_influence() only ever touches what
# backend_extract() returns.

#' @exportS3Method
backend_info.netis_backend_stats <- function(backend) {
  list(name = "stats", package = "stats", version = as.character(getRversion()))
}

#' @exportS3Method
backend_fit.netis_backend_stats <- function(backend, formula, data, ...) {
  stats::lm(formula, data = data)
}

#' @exportS3Method
backend_extract.netis_backend_stats <- function(backend, fit) {
  s <- summary(fit)

  # Rank deficiency and hat geometry are read here rather than in the
  # diagnostics layer because they need the raw fit, and every backend object stays
  # inside its own adapter file. `lm()` does not fail on
  # an aliased term -- it drops it to NA and carries on -- so the fact has to
  # be carried out deliberately or it is invisible in everything downstream.
  aliased <- names(stats::coef(fit))[is.na(stats::coef(fit))]
  hat <- tryCatch(stats::hatvalues(fit), error = function(e) NULL)
  p <- fit$rank
  n <- length(fit$residuals)
  leverage <- if (is.null(hat) || !length(hat) || anyNA(p) || n == 0L) {
    NULL
  } else {
    # 2p/n is the conventional reference line for "high leverage" and is
    # reported as a COUNT against it, never as a per-row verdict: a design
    # point with high leverage is a fact about the design matrix, not a
    # defective observation, and rnetis ships no row-deletion influence
    # measure for exactly that reason.
    list(max = max(hat), threshold = 2 * p / n,
         n_above = sum(hat > 2 * p / n), n = n, p = p)
  }

  list(
    coefficients = stats::coef(fit),
    vcov = stats::vcov(fit),
    random_effects = NULL,
    var_corr = NULL,
    convergence = list(converged = TRUE),  # lm() is a closed-form fit
    loglik = tryCatch(stats::logLik(fit), error = function(e) NA),
    r_squared = s$r.squared,
    adj_r_squared = s$adj.r.squared,
    residual_df = fit$df.residual,
    sigma = s$sigma,
    aliased = aliased,
    leverage = leverage,
    # Carried out of the adapter for the dependence family,
    # which needs residuals aligned to the estimating table's rows and must not
    # reach into a backend object to get them. `lm()` names them by the model
    # frame's original row labels, so listwise deletion stays recoverable: the
    # names are how the diagnostic knows which table rows these belong to.
    residuals = stats::residuals(fit),
    residual_type = "response",
    warnings = character(0)
  )
}

#' @exportS3Method
backend_predict.netis_backend_stats <- function(backend, fit, newdata) {
  stats::predict(fit, newdata = newdata)
}
