#' Standard R model methods for a fitted Influence model
#'
#' `netis_influence` objects (from [fit_influence()]) support the ordinary R
#' modeling verbs rather than inventing package-specific accessors: `coef()`,
#' `vcov()`, `fitted()`, `residuals()`, `predict()`, `confint()`, `update()`,
#' `logLik()`, `AIC()`, `BIC()`, in addition to [print()], [summary()] and
#' [plot()][plot.netis_influence].
#'
#' @param object a `netis_influence` object.
#' @param ... for `update()`, named arguments to override in the original
#'   `fit_influence()` call (e.g. `waves = c(2, 3)`), unevaluated and
#'   substituted into the stored call exactly as [stats::update()] does. For
#'   `AIC()`, passed through unused (present for S3 consistency with other
#'   `AIC()` methods). Unused by the remaining methods.
#' @return see each generic's usual return type (a named numeric vector for
#'   `coef()`, a matrix for `vcov()`/`confint()`, numeric vectors for
#'   `fitted()`/`residuals()`/`predict()`, a single number for `AIC()`/`BIC()`,
#'   an object of class `logLik` for `logLik()`, a refitted `netis_influence`
#'   for `update()`).
#' @seealso [fit_influence()], [summary.netis_influence()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = rep(c("a", "b", "c", "d"), 2),
#'   wave = rep(1:2, each = 4),
#'   y = c(0.2, -0.6, 0.5, -0.1, 0.4, -0.3, 0.7, 0.0),
#'   x = c(1, 0, 1, 0, 1, 0, 1, 0)
#' )
#' ties <- data.frame(
#'   group = 1, source = c("a", "b", "c"), target = c("b", "c", "d"),
#'   wave = 1, tie = 1
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
#' coef(fit)
#' vcov(fit)
#' fitted(fit)
#' residuals(fit)
#' logLik(fit)
#' @name netis-influence-methods
NULL

#' Print an Influence fit
#'
#' Reports what was fitted and what it was fitted to, with the coefficient table.
#' Diagnostics and assumptions, recorded at fit time, and effect sizes, computed on
#' request, are not printed here: see [summary.netis_influence()],
#' [netis_diagnostics()], [netis_assumptions()] and [influence_effect()].
#'
#' @param x a `netis_influence` fit.
#' @param ... unused; present for S3 consistency.
#' @return `x`, invisibly. Called for its output.
#' @seealso [summary.netis_influence()], [fit_influence()]
#' @export
print.netis_influence <- function(x, ...) {
  cat('<netis_influence>\n')
  cat('  ', deparse(x$spec$formula), '\n', sep = '')
  cat('  ', .format_estimator(x$spec$estimator, x$spec$estimator_reason),
      '  [backend: ', x$backend, ']\n', sep = '')
  cat('  ', .format_wave_selection(x$spec), '\n', sep = '')
  n_used <- .influence_nobs(x)
  n_assembled <- nrow(x$model_frame)
  cat('  ', n_used, ' rows in the estimating sample', sep = '')
  if (isTRUE(n_used < n_assembled)) {
    cat(' (', n_assembled - n_used, ' of ', n_assembled,
        ' assembled rows dropped as incomplete)', sep = '')
  }
  cat('\n')
  bi <- .format_bi_parameters(x$resolved_exposures)
  if (length(bi)) cat(paste0('  ', bi, '\n'), sep = '')
  sar <- .format_sar_system(x)
  if (length(sar)) cat(paste0('  ', sar, '\n'), sep = '')
  st <- .format_structural_terms(x)
  if (length(st)) cat(paste0('  ', st, '\n'), sep = '')
  la <- .format_latent_adjustment(x)
  if (length(la)) cat(paste0('  ', la, '\n'), sep = '')
  cat('\n')
  print(.format_estimate_table(.estimate_frame(x)))
  invisible(x)
}

#' The number of rows the backend actually fitted
#'
#' Not `nrow(model_frame)`: the model frame is the ASSEMBLED table, and every
#' backend drops rows with a missing value before fitting. `stats::nobs()`
#' answers for `lm` and `lmer`; `sna::lnam()` has no `nobs()` method, so its
#' count is the rows the adapter recorded as kept.
#'
#' @param fit a `netis_influence` fit.
#' @return a single integer, `NA` when the backend cannot say.
#' @noRd
.influence_nobs <- function(fit) {
  n <- if (identical(fit$backend, 'lnam')) {
    rn <- attr(fit$backend_fit, 'netis_row_names')
    if (is.null(rn)) NA_integer_ else length(rn)
  } else {
    tryCatch(stats::nobs(fit$backend_fit), error = function(e) NA_integer_)
  }
  as.integer(n)
}

#' Report the simultaneous system a SAR fit actually estimated
#'
#' Empty on every other route. Three things belong in front of a reader of an
#' `lnam` fit and nowhere else:
#'
#' * **Whether the fitted system is invertible.** The model is
#'   `Y = rho * W1 Y + X beta + e`, whose reduced form needs
#'   `(I - rho * W1)^-1` to exist. The operator's eigenvalues are
#'   `1 - rho * lambda` over the eigenvalues `lambda` of W1, so it is singular
#'   exactly when one of them is zero. What is printed is that check: the
#'   smallest modulus `min |1 - rho * lambda|` and whether it is numerically
#'   zero. `|rho| * spectral_radius(W1) < 1` is a sufficient condition for
#'   invertibility, not a necessary one, so a product above 1 is not reported
#'   as a singular operator.
#' * **Nodes removed from the network.** Recorded by the adapter when a row
#'   was incomplete. In a simultaneous model this is not listwise deletion of
#'   an observation; it changes the network the remaining actors sit in.
#' * **Missing standard errors.** `sna::lnam` omits its information matrix
#'   when `numDeriv` is unavailable, and missing standard errors are said to
#'   be missing rather than left to be inferred from an empty column.
#'
#' @param fit a `netis_influence` fit.
#' @return a character vector of display lines, empty off the SAR route.
#' @noRd
.format_sar_system <- function(fit) {
  if (!identical(fit$backend, 'lnam')) {
    return(character(0))
  }
  out <- character(0)
  rho <- fit$rho
  sr <- fit$w1_spectral_radius
  mm <- fit$sar_operator_min_modulus
  if (!is.null(rho) && is.finite(rho) && !is.null(sr) && is.finite(sr)) {
    out <- c(out, sprintf(
      'Simultaneous system: rho = %.4f, spectral radius of W1 = %.4f',
      rho, sr
    ))
  }
  if (!is.null(mm) && is.finite(mm) && !is.na(fit$sar_operator_invertible %||% NA)) {
    out <- c(out, if (isTRUE(fit$sar_operator_invertible)) {
      sprintf('  Smallest eigenvalue modulus of (I - rho * W1), min |1 - rho * lambda| over the eigenvalues of W1: %.4g. It is not zero, so (I - rho * W1) is invertible and the reduced form exists.', mm)
    } else {
      sprintf('  Smallest eigenvalue modulus of (I - rho * W1), min |1 - rho * lambda| over the eigenvalues of W1: %.4g, numerically zero. (I - rho * W1) is singular at this estimate, so the implied reduced form does not exist.', mm)
    })
  }
  if (!is.null(fit$dropped_nodes) && length(fit$dropped_nodes)) {
    out <- c(out, sprintf(
      'Removed from the network: %d node%s with an incomplete row, or no row at this wave (%s).',
      length(fit$dropped_nodes),
      if (length(fit$dropped_nodes) == 1L) '' else 's',
      paste(utils::head(fit$dropped_nodes, 10), collapse = ', ')
    ), '  In a simultaneous model this removes the node from the network, not just an observation from the sample: rho describes the remaining subnetwork.')
  }
  if (isFALSE(fit$information_matrix_available)) {
    out <- c(out, 'Standard errors are unavailable: sna::lnam returned no information matrix (its numDeriv-based step did not run).')
  }
  out
}

#' State which waves were analyzed, and which were not
#'
#' Both halves, always. Requiring `waves =` on a dataset with more than two
#' exists precisely so that a ten-wave study is not silently analyzed through
#' one transition; saying only which waves were used would leave that half
#' unsaid.
#'
#' @param spec a fit's `spec`.
#' @return a single display line.
#' @noRd
.format_wave_selection <- function(spec) {
  used <- as.character(spec$waves)
  n_avail <- length(spec$waves_available)
  n_unused <- length(spec$waves_unused)
  if (n_avail == 0L) {
    return('Waves: none; a single cross-section (contemporaneous specification)')
  }
  if (n_avail == 1L) {
    return('Waves: one wave; contemporaneous specification')
  }
  sprintf(
    'Waves: analyzed %s of %d present; %d wave%s not used',
    paste(used, collapse = ' -> '), n_avail, n_unused,
    if (n_unused == 1L) '' else 's'
  )
}

#' Report the B-I parameters of any path-dependent exposure term
#'
#' `delta`, `alpha` and `gamma` are supplied rather than estimated, so a result
#' is conditional on them and the values belong in the output rather than only
#' in the call.
#'
#' @param resolved the fit's `resolved_exposures`.
#' @return a character vector, empty when no B-I term is present.
#' @noRd
.format_bi_parameters <- function(resolved) {
  out <- character(0)
  for (nm in names(resolved)) {
    prov <- attr(resolved[[nm]]$term$mechanism, 'mechanism_provenance')
    if (is.null(prov) || !identical(prov$package, 'rnetis')) next
    if (!grepl('^bi', prov$name %||% '')) next
    p <- prov$params
    # bi_components() takes no delta: it reports the two components delta weighs.
    delta <- if (is.null(p$delta)) "" else sprintf("delta = %s, ", format(p$delta))
    out <- c(out, sprintf(
      'Exposure %s: %s(%salpha = %s, gamma = %s) -- supplied, not estimated',
      nm, prov$name, delta, format(p$alpha), format(p$gamma)
    ))
  }
  out
}

#' @noRd
.estimate_frame <- function(x, level = 0.95) {
  se <- sqrt(diag(x$vcov))[names(x$coefficients)]
  # The same critical value confint() and the effect-size layer use: a t
  # quantile for least squares, a normal one for lme4 and lnam.
  crit <- .netis_wald_critical(.influence_inference_df(x$backend, x$residual_df), level)
  data.frame(
    term = names(x$coefficients),
    estimate = unname(x$coefficients),
    std.error = unname(se),
    conf.low = unname(x$coefficients - crit * se),
    conf.high = unname(x$coefficients + crit * se),
    stringsAsFactors = FALSE
  )
}

#' @rdname netis-influence-methods
#' @export
coef.netis_influence <- function(object, ...) {
  object$coefficients
}

#' @rdname netis-influence-methods
#' @param type for `vcov()`, which variance to return. Only `"model"` (the
#'   estimator's own) is accepted; clustered alternatives are compared against
#'   it by [influence_se_comparison()].
#'   Robust variance changes the uncertainty, never the coefficients, and no
#'   variance estimator repairs a biased coefficient -- see
#'   `vignette("model-fit")` and [influence_se_comparison()].
#' @export
vcov.netis_influence <- function(object, type = c("model"), ...) {
  # ONE ROUTE. Cluster-robust standard errors are reported through
  # influence_se_comparison(). A robust estimator is something the user asks
  # for, never something switched on for them.
  type <- match.arg(type)
  object$vcov
}

#' @rdname netis-influence-methods
#' @export
fitted.netis_influence <- function(object, ...) {
  stats::fitted(object$backend_fit)
}

#' @rdname netis-influence-methods
#' @export
residuals.netis_influence <- function(object, ...) {
  # THE ADAPTER'S RESIDUALS FIRST. Reaching into `backend_fit` returns whatever
  # the backend happens to hold, which for `sna::lnam` is an UNNAMED vector --
  # and one shorter than the estimating table whenever a row was incomplete.
  # Pairing those with rows by position is wrong in exactly the case the length
  # difference signals. The adapter is the layer that still knows which rows
  # survived, so it labels them and this returns what it labeled. The fallback is
  # for a fit object without the field.
  if (!is.null(object$residuals)) {
    return(object$residuals)
  }
  stats::residuals(object$backend_fit)
}

#' @rdname netis-influence-methods
#' @param newdata `NULL` (default, returns fitted values) or a `data.frame`
#'   already containing the same assembled predictor columns as
#'   `object$model_frame` -- the `ar()`/`exposure()` column names exactly as
#'   `fit_influence()` produced them, as `predict.lm()` requires
#'   that newdata columns must match the terms the model was fit with.
#'   Re-deriving those columns from a second [netis_data()] object (a fresh
#'   wave lookup and [compute_exposure()] call) is not performed here -- that
#'   would require deciding which wave "newdata" is relative to, which this
#'   method does not resolve; assemble the columns yourself first. An `sna::lnam` fit
#'   refuses `newdata`: predicting needs `(I - rho * W1)^-1` for the new data's own
#'   network, which is not implemented.
#' @export
predict.netis_influence <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(stats::fitted(object$backend_fit))
  }
  backend_predict(.new_backend(object$backend), object$backend_fit, newdata)
}

#' @rdname netis-influence-methods
#' @param parm parameters to compute intervals
#' for; see [stats::confint()]. If not supplied, intervals are returned for all
#' coefficients (the missing-ness of `parm` propagates through to
#' [stats::confint()]'s own default-selection logic, the same wrapper pattern
#' `stats` uses internally for e.g. `confint.glm()`).
#' @param level confidence
#' level; see [stats::confint()].
#' @param method how the interval is
#' constructed. `"wald"` (the default) builds it from the fit's own `coef()`
#' and `vcov()`: with a t quantile on the residual degrees of freedom for an
#' ordinary least squares (`stats`) fit, and a normal quantile for `lme4` and
#' `sna::lnam` fits.
#' On an `lme4` fit `"profile"` (lme4's own default), `"boot"` (bootstrap) and
#' `"Wald"` are passed to [lme4::confint.merMod()] as its `method` argument, which also
#' return rows for the variance parameters unless `parm` selects (see that page for
#' `nsim`, `boot.type`, `parallel`, ...), and `"backend"` delegates to it without naming
#' a method. On a least-squares fit `"backend"` delegates to `stats::confint.lm()`, which
#' has no `method` argument. A `method` the fit's backend does not apply is refused:
#' on a least-squares fit anything but `"wald"` and `"backend"`, and on an `sna::lnam`
#' fit anything but `"wald"`.
#' @details **One fit, one interval construction**. `"wald"` is the
#' default for every backend because otherwise one object could carry several
#' constructions: an `lme4` fit's printed table would carry profile-likelihood
#' intervals while the effect-size layer reported Wald ones for the same
#' coefficient. The critical value is the same one the effects layer uses, so
#' the two agree by construction rather than by coincidence.
#'
#'   The reference distribution is the one the backend's own inference
#'   supports. For a `stats`-backed fit it is the exact t distribution, and
#'   the result is identical to `stats::confint.lm()`. `lme4` reports no
#'   denominator degrees of freedom for a fixed effect, and its own Wald
#'   interval (`confint(fit, method = "Wald")`) uses the normal quantile, so
#'   the default interval on an `lme4` fit is identical to that one.
#'   `sna::lnam` estimates by maximum likelihood with asymptotically normal
#'   inference (its own `summary()` reports z values), so it gets the normal
#'   quantile too. `sna` has no `confint()` method to delegate to.
#'
#'   On an `lme4` fit `method = "profile"` gives the profile interval, a better
#'   interval where it can be computed. It is not the default because it would be
#'   recomputed on every `print()`, `summary()` and `tidy()` of the same object.
#' @export
confint.netis_influence <- function(object, parm, level = 0.95,
                                    method = "wald", ...) {
  .assert_confint_method(method, object$backend)
  if (identical(method, "wald")) {
    return(.wald_confint(object$coefficients, object$vcov,
                         .influence_inference_df(object$backend, object$residual_df),
                         parm, level))
  }
  # Anything other than "backend" is lme4's method vocabulary, forwarded as its
  # `method` argument; "backend" delegates without one, which is what a backend
  # with no such argument (stats::confint.lm()) accepts.
  extra <- c(list(...), if (identical(method, "backend")) NULL else list(method = method))
  # object$backend_fit is the RAW backend object (lm()/lmer()), whose own
  # coefficient names still carry whatever backtick-quoting
  # .safe_predictor_formula()/.backtick_join() needed to stop a moderated
  # term's product column (e.g. "x:z") being reinterpreted as formula syntax --
  # `fit_influence()` strips those backticks from
  # `object$coefficients`/`object$vcov` right after extraction, but never
  # touches `backend_fit` itself. Without this translation, `parm` supplied in
  # the clean name a user sees from coef(fit) would not match here, and the
  # RESULT's rownames would not match `names(object$coefficients)` either.
  #
  # Read from vcov(): coef() of an lmer fit is a list of per-group data frames, whose
  # names are the grouping factors and not the fixed effects.
  backend_names <- rownames(stats::vcov(object$backend_fit))
  clean_to_backend <- stats::setNames(backend_names, gsub("^`|`$", "", backend_names))
  # confint.lm()/confint.default() key their "use every parameter" default off
  # missing(parm), not is.null(parm) -- passing parm = NULL explicitly (rather
  # than omitting the argument) skips that default and silently returns a
  # zero-row matrix instead. Branch to two call shapes rather than a single
  # `parm = parm_backend` call so a NULL/missing parm is genuinely omitted.
  out <- if (missing(parm) || is.null(parm)) {
    do.call(stats::confint,
            c(list(object$backend_fit, level = level), extra))
  } else {
    parm_backend <- if (is.character(parm)) unname(clean_to_backend[parm]) else parm
    do.call(stats::confint,
            c(list(object$backend_fit, parm = parm_backend, level = level), extra))
  }
  rownames(out) <- gsub("^`|`$", "", rownames(out))
  out
}

#' Refuse an interval method the fit's backend does not apply
#'
#' `stats::confint.lm()` takes no `method` argument and would ignore one, returning
#' its t interval under whatever name was asked for. `sna` has no `confint()` method
#' at all.
#'
#' @noRd
.assert_confint_method <- function(method, backend) {
  available <- switch(
    backend,
    stats = c("wald", "backend"),
    lme4 = c("wald", "backend", "profile", "boot", "Wald"),
    "wald"
  )
  if (!is.character(method) || length(method) != 1L || !method %in% available) {
    stop(
      sprintf(
        "confint(method = %s) is not available on this %s fit, whose backend does not apply it. Available: %s.",
        paste(deparse(method), collapse = ""), backend,
        paste(sprintf("\"%s\"", available), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' The Wald interval every rnetis Influence fit reports by default
#'
#' Shares its critical value with `.effects_critical_value()` through
#' `.netis_wald_critical()`, which is what makes the printed table and the
#' effect-size layer agree on one fit rather than reporting two intervals for
#' one coefficient.
#'
#' @noRd
.wald_confint <- function(coefs, vcov_mat, residual_df, parm, level) {
  if (missing(parm) || is.null(parm)) {
    parm <- names(coefs)
  } else if (is.numeric(parm)) {
    parm <- names(coefs)[parm]
  }
  se <- sqrt(diag(vcov_mat))[parm]
  crit <- .netis_wald_critical(residual_df, level)
  est <- coefs[parm]
  out <- cbind(est - crit * se, est + crit * se)
  colnames(out) <- sprintf("%.1f %%", 100 * c((1 - level) / 2, 1 - (1 - level) / 2))
  rownames(out) <- parm
  out
}

#' @rdname netis-influence-methods
#' @importFrom stats logLik
#' @export
logLik.netis_influence <- function(object, ...) {
  object$loglik
}

#' @rdname netis-influence-methods
#' @param k penalty per parameter for `AIC()`; see [stats::AIC()]. Default `2`
#'   (the classical AIC).
#' @details `AIC()`/`BIC()` are computed from the fit's own log-likelihood, with its
#'   `df` and `nobs` attributes, on every backend. `sna::lnam` has no `logLik()`
#'   method, so that value is assembled from the model log-likelihood and degrees of
#'   freedom it returns. `NA` if the backend could not produce a valid log-likelihood.
#' @importFrom stats AIC
#' @export
AIC.netis_influence <- function(object, ..., k = 2) {
  ll <- object$loglik
  if (!inherits(ll, "logLik") || is.na(as.numeric(ll))) {
    return(NA_real_)
  }
  -2 * as.numeric(ll) + k * attr(ll, "df")
}

#' @rdname netis-influence-methods
#' @importFrom stats BIC
#' @export
BIC.netis_influence <- function(object, ...) {
  ll <- object$loglik
  if (!inherits(ll, "logLik") || is.na(as.numeric(ll))) {
    return(NA_real_)
  }
  -2 * as.numeric(ll) + log(attr(ll, "nobs")) * attr(ll, "df")
}

#' @rdname netis-influence-methods
#' @export
update.netis_influence <- function(object, ...) {
  call <- object$call
  extras <- match.call(expand.dots = FALSE)$...
  if (length(extras) > 0L) {
    call[names(extras)] <- extras
  }
  eval(call, envir = parent.frame())
}

#' Summarize an Influence fit
#'
#' Prints the formula, estimator and backend, the waves analyzed and not used, the
#' estimand, any supplied mechanism parameters, the simultaneous system on an
#' `sna::lnam` fit, the coefficient table with 95% intervals, what a moderated term's
#' coefficient is conditional on, the random-effects level count, a compact assumption
#' ledger, and how many design and estimation findings were recorded at fit time.
#'
#' @param object a `netis_influence` fit.
#' @param ... unused; present for S3 consistency.
#' @return an object of class `summary.netis_influence`, a list holding the `fit`,
#'   with a `print()` method that returns it invisibly.
#' @seealso [print.netis_influence()], [fit_influence()]
#' @export
summary.netis_influence <- function(object, ...) {
  structure(list(fit = object), class = 'summary.netis_influence')
}

#' @rdname summary.netis_influence
#' @param x a `summary.netis_influence` object.
#' @export
print.summary.netis_influence <- function(x, ...) {
  fit <- x$fit
  cat('Influence model\n')
  cat('  Formula:   ', deparse(fit$spec$formula), '\n', sep = '')
  cat('  ', .format_estimator(fit$spec$estimator, fit$spec$estimator_reason), '\n', sep = '')
  cat('  Backend:   ', fit$backend, ' ', fit$backend_version, '\n', sep = '')
  cat('  ', .format_wave_selection(fit$spec), '\n', sep = '')
  cat('  Estimand:  ', fit$spec$estimand, '\n', sep = '')
  bi <- .format_bi_parameters(fit$resolved_exposures)
  if (length(bi)) cat(paste0('  ', bi, '\n'), sep = '')
  sar <- .format_sar_system(fit)
  if (length(sar)) cat(paste0('  ', sar, '\n'), sep = '')
  st <- .format_structural_terms(fit)
  if (length(st)) cat(paste0('  ', st, '\n'), sep = '')
  la <- .format_latent_adjustment(fit)
  if (length(la)) cat(paste0('  ', la, '\n'), sep = '')
  mp <- .format_moderation_provenance(fit)
  if (length(mp)) cat(paste0('  ', mp, '\n'), sep = '')
  cat('\nCoefficients:\n')
  print(.format_estimate_table(.estimate_frame(fit)))
  mod <- .format_moderated_terms(fit)
  if (length(mod)) cat('\n', paste0(mod, '\n'), sep = '')
  if (!is.null(fit$re_levels)) {
    cat('\nRandom effects: ',
        paste(sprintf('%s (%d levels)', names(fit$re_levels), fit$re_levels),
              collapse = ', '), '\n', sep = '')
  }
  if (!is.null(fit$assumptions)) {
    cat('\n')
    cat(.format_assumptions_compact(fit$assumptions), sep = '\n')
    cat('\n')
  }
  # POINT AT THE ACCESSORS. Everything a printed summary shows is also
  # available as data, and a reader who wants the numbers should not have to
  # discover that by searching the manual. The interpretation surfaces are
  # named here too, because "not computed at fit time" is only half an answer
  # without saying where to ask for them.
  cat('\nAs data: netis_estimates(), netis_glance(), netis_variance_components().\n')
  cat('Computed on request: netis_dependence(), netis_assumptions(), pseudo_r2(), influence_effect().\n')
  n_found <- length(fit$diagnostics %||% list())
  if (n_found > 0L) {
    cat(sprintf('Design and estimation findings recorded at fit time: %d; netis_diagnostics() lists them.\n',
                n_found))
  } else {
    cat('Design and estimation findings recorded at fit time: none.\n')
  }
  invisible(x)
}

#' State that a moderated term's coefficient is conditional
#'
#' In `y ~ x * z` the coefficient printed for `x` is the effect of `x` at
#' `z = 0`, not an effect that holds at every `z`, and the table alone does not
#' say so. One line per moderated term names the moderator, the value the
#' printed coefficient is conditional on, and where the effect at other values
#' is reported. For a moderated `ar()` term it also says that the coefficient
#' is not an unconditional stability parameter.
#'
#' @param fit a `netis_influence` fit.
#' @return a character vector of display lines, empty without moderation.
#' @noRd
.format_moderated_terms <- function(fit) {
  inter <- fit$spec$interactions %||% list()
  if (length(inter) == 0L) {
    return(character(0))
  }
  lines <- vapply(inter, function(i) {
    moderand <- .interaction_moderand_colname(i)
    prod <- .interaction_product_colname(i)
    if (identical(i$moderand_kind, "covariate")) {
      # Two plain covariates: the pairing is symmetric, so each main effect is
      # conditional on the other.
      return(sprintf(
        "Moderated term: the coefficients on '%s' and '%s' are each the effect conditional on the other being 0, not effects that hold at every value of it; '%s' carries the dependence.",
        moderand, i$moderator, prod
      ))
    }
    base <- sprintf(
      "Moderated term: the coefficient on '%s' is its effect conditional on %s = 0, not an effect that holds at every value of %s. At another value z the effect is %s + %s * z.",
      moderand, i$moderator, i$moderator, moderand, prod
    )
    if (identical(i$moderand_kind, "ar")) {
      base <- paste(base, "It is therefore not an unconditional stability parameter.")
    }
    base
  }, character(1))
  c(lines, "  Conditional estimates, intervals and Johnson-Neyman boundaries: influence_effect().")
}
#' Format an estimate frame for printing
#'
#' Deliberately spare: term, estimate, standard error, interval. Printing a
#' column this fit cannot populate would be worse than not printing it.
#'
#' @param est a frame from `.estimate_frame()`.
#' @param digits rounding for display.
#' @return a data frame ready to `print()`.
#' @noRd
.format_estimate_table <- function(est, digits = 4) {
  interval <- ifelse(
    is.na(est$conf.low) | is.na(est$conf.high),
    "not available",
    sprintf("[%s, %s]", format(round(est$conf.low, digits), trim = TRUE),
            format(round(est$conf.high, digits), trim = TRUE))
  )
  out <- data.frame(
    Term = est$term,
    Estimate = round(est$estimate, digits),
    SE = round(est$std.error, digits),
    Interval = interval,
    stringsAsFactors = FALSE
  )
  # NAME THE MASS. "Interval" alone leaves the reader to assume 95%, and an
  # interval whose coverage is assumed rather than stated is the kind of
  # number that gets quoted with the wrong label.
  level <- attr(est, "level") %||% 0.95
  names(out)[names(out) == "Interval"] <-
    sprintf("%g%% interval", 100 * level)
  out
}

#' Name the structural terms, and say what carries their precision
#'
#' Two things the coefficient table cannot say on its own.
#'
#' A `centrality()` or `network_stat()` term is a STRUCTURAL predictor -- a
#' property of the network rather than of the node's own attributes -- and the
#' distinction matters for reading the coefficient. Naming them on their own
#' line is how a reader tells `cent_degree_w` apart from an ordinary covariate
#' that happens to be named after a network.
#'
#' And a `network_stat()` term is GROUP-LEVEL: one value per group per
#' occasion. Its inferential information is carried by the NUMBER OF GROUPS,
#' not the number of rows -- ten groups of a hundred nodes give ten
#' observations of a group attribute, not a thousand. A coefficient printed
#' beside a standard error computed from a thousand rows invites exactly the
#' wrong reading, which is why the caveat prints with the term rather than
#' living in the manual.
#'
#' @param fit a `netis_influence` fit.
#' @return a character vector of display lines, empty when neither kind of term
#'   is present.
#' @noRd
.format_structural_terms <- function(fit) {
  cent <- names(fit$resolved_centralities %||% list())
  nstat <- names(fit$resolved_network_stats %||% list())
  if (length(cent) == 0L && length(nstat) == 0L) {
    return(character(0))
  }
  out <- sprintf("Structural (centrality / network_stat): %s",
                 paste(c(cent, nstat), collapse = ", "))
  if (length(nstat) > 0L) {
    out <- c(out, paste(
      "  network_stat() terms are group-level: their inferential information is",
      "carried by the number of groups, not the number of rows."
    ))
  }
  out
}

#' State the selection model behind a latent-space adjustment
#'
#' A position column is meaningless without the model that produced it, and
#' three things about it change what the coefficient means.
#'
#' WHAT WAS CONDITIONED ON. Xu's whole argument is that the positions
#' approximate an unobserved trait only after the observed selection
#' mechanisms are controlled for; a reader cannot judge the adjustment without
#' seeing which ones were.
#'
#' THAT THE POSITIONS ARE ESTIMATED, NOT OBSERVED. They come from a fitted
#' model, and their scale and location are arbitrary -- a position coefficient
#' is not the effect of the trait, and printing it in the same table as
#' ordinary covariates without saying so invites exactly that reading.
#'
#' THAT IT IS A PLUG-IN. Stage-one uncertainty does not reach stage two.
#'
#' @param fit a `netis_influence` fit.
#' @return a character vector of display lines, empty when no `latent()` term
#'   is present.
#' @noRd
.format_latent_adjustment <- function(fit) {
  resolved <- fit$resolved_latent %||% list()
  if (length(resolved) == 0L) {
    return(character(0))
  }
  out <- character(0)
  for (nm in names(resolved)) {
    r <- resolved[[nm]]
    out <- c(out, sprintf(
      "Latent-space adjustment %s (Xu 2018, 2021): %s, at wave %s%s.",
      nm, r$selection_formula, as.character(r$feature_wave),
      if (is.null(r$seed)) "" else sprintf(" (stage-one seed %s)", r$seed)
    ))
  }
  c(out,
    "  Positions are ESTIMATED, and their scale and location are arbitrary: a",
    "  position coefficient is not the effect of the unobserved trait. The",
    "  adjustment is plug-in -- the selection model's uncertainty does not enter",
    "  these standard errors.")
}

#' Say which derived column a moderated term actually referenced
#'
#' A coefficient named `x_within:y_lag1` does not carry the fact that
#' `x_within` came from [add_decomposition()] and is the within-actor component
#' of `x` rather than `x` itself. The term named a column; only the data layer
#' knows what that column IS.
#'
#' This matters for reading the interaction, not merely for tidiness: the same
#' formula written against `x` and against `x_within` estimates different
#' moderations, and the coefficient tables look alike.
#'
#' @param fit a `netis_influence` fit.
#' @return a character vector of display lines, empty when no moderated term
#'   referenced a derived column.
#' @noRd
.format_moderation_provenance <- function(fit) {
  prov <- fit$moderation_provenance %||% character(0)
  if (length(prov) == 0L) {
    return(character(0))
  }
  c("Moderated term(s) reference a derived variable (add_centering() / add_decomposition()):",
    paste0("  ", prov))
}
