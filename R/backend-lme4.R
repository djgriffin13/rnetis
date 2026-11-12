# Backend adapter for lme4::lmer(), used when fit_influence()'s formula
# includes a random-effects term.
#
# lme4 objects never escape this file into the public API -- fit_influence()
# only ever touches what backend_extract() returns, as with backend-stats.R.

#' @exportS3Method
backend_info.netis_backend_lme4 <- function(backend) {
  list(name = "lme4", package = "lme4", version = as.character(utils::packageVersion("lme4")))
}

#' @exportS3Method
backend_fit.netis_backend_lme4 <- function(backend, formula, data, ...) {
  # lmerTest::lmer() IS lme4::lmer() -- the same optimiser, the same estimates,
  # an object that extends `lmerMod` -- with the derivatives Satterthwaite's
  # degrees of freedom need computed while the data are still in scope. An
  # `lmerMod` cannot be converted afterwards from here: the conversion
  # re-evaluates the model call, whose `data` no longer exists.
  lmerTest::lmer(formula, data = data, ...)
}

#' Satterthwaite t tests for the fixed effects, as lmerTest reports them
#'
#' `NULL` for a plain `lmerMod` (a fit saved before the backend fitted through
#' lmerTest), and `NULL` where lmerTest cannot compute them -- the printed table
#' then says there are none, and reports no degrees of freedom of its own.
#'
#' @param fit the backend's fitted model.
#' @return a data frame with `term`, `df`, `statistic`, `p.value`, or `NULL`.
#' @noRd
.lme4_coef_tests <- function(fit) {
  if (!methods::is(fit, "lmerModLmerTest")) return(NULL)
  cf <- tryCatch(stats::coef(summary(fit, ddf = "Satterthwaite")),
                 error = function(e) NULL)
  if (is.null(cf) || !all(c("df", "t value", "Pr(>|t|)") %in% colnames(cf))) {
    return(NULL)
  }
  data.frame(
    term = gsub("^`|`$", "", rownames(cf)),
    df = unname(cf[, "df"]),
    statistic = unname(cf[, "t value"]),
    p.value = unname(cf[, "Pr(>|t|)"]),
    stringsAsFactors = FALSE
  )
}

#' Extract fields from an lme4 fit
#'
#' r_squared/adj_r_squared are left NA rather than computed: there is no single
#' agreed variance-explained definition for a mixed model the way there is for
#' OLS (marginal vs. conditional R-squared answer different questions), and
#' guessing which one a caller wants could give a silently wrong number.
#' residual_df is lme4's conditional n - p, recorded as
#' the backend reports it. It is not a degrees of freedom for a fixed-effect
#' test, and nothing uses it as one: a coefficient's test and its default Wald
#' interval use Satterthwaite's degrees of freedom from `coef_tests` (see
#' `.influence_coef_df()`).
#'
#' @noRd
#' @exportS3Method
backend_extract.netis_backend_lme4 <- function(backend, fit) {
  conv_messages <- fit@optinfo$conv$lme4$messages

  # A BOUNDARY (SINGULAR) FIT IS NOT NON-CONVERGENCE. lme4 files its
  # "boundary (singular) fit" note in the same `conv$lme4$messages` slot it
  # uses for genuine optimiser trouble, so reading that slot alone reports a
  # perfectly converged fit -- one where a variance component simply sits at
  # zero -- as having failed. Any comparison that reports convergence as an
  # outcome would then penalise lme4-backed estimators for a mislabeled
  # diagnostic rather than a real failure.
  #
  # Singularity is already reported on its own, and the reliability grader
  # already treats it as a caution rather than a failure (a boundary fit is
  # the correct answer when a variance really is zero: the fixed effects
  # remain usable, and it is inference on the affected component that is not).
  # So it is removed from the convergence test and left to that path.
  singular <- lme4::isSingular(fit)
  fatal_messages <- conv_messages[!grepl("singular", conv_messages, ignore.case = TRUE)]

  list(
    coefficients = lme4::fixef(fit),
    vcov = as.matrix(stats::vcov(fit)),
    random_effects = lme4::ranef(fit),
    var_corr = as.data.frame(lme4::VarCorr(fit)),
    convergence = list(
      converged = length(fatal_messages) == 0L,
      singular = singular,
      # Every message is still carried through, singularity included, so the
      # grade names the evidence rather than hiding it.
      messages = conv_messages
    ),
    loglik = tryCatch(stats::logLik(fit), error = function(e) NA),
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    # Fixed-part fitted values (population-level predictions, random effects
    # set to zero): the variance of these is the fixed-effect variance the
    # Nakagawa-Schielzeth decomposition needs (R/pseudo-r2.R). Computed here
    # because predict(re.form = ) is lme4 API and must not leak past the
    # adapter.
    fixed_fitted = stats::predict(fit, re.form = NA),
    # CONDITIONAL residuals -- lme4's default, and the right ones here. The
    # dependence family asks what structure survives the FITTED
    # model, so the random effects have to be in the prediction being residualized
    # against. Marginal residuals answer a different question and would show the
    # group structure the model already accounts for. The type is recorded rather
    # than assumed by the consumer.
    residuals = stats::residuals(fit),
    residual_type = "conditional",
    residual_df = stats::df.residual(fit),
    coef_tests = .lme4_coef_tests(fit),
    sigma = stats::sigma(fit),
    warnings = character(0)
  )
}

#' @exportS3Method
backend_predict.netis_backend_lme4 <- function(backend, fit, newdata) {
  stats::predict(fit, newdata = newdata, allow.new.levels = FALSE)
}
