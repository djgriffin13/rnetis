# Pseudo R-squared measures.
#
# Several named measures rather than one: different measures answer different
# questions, and picking one silently would hide that.
#
#   Selection: a latent-scale VARIANCE SHARE (the package's own construction,
#   declared as such), TJUR'S D on the probability scale (Tjur 2009), and --
#   for R = 0 fits only -- a NULL COMPARISON against a refitted model with no
#   predictors.
#
#   Influence: ordinary R2 for an OLS fit; Nakagawa-Schielzeth marginal and
#   conditional for a mixed fit, which is exactly the case they define.

#' Pseudo R-squared measures for a rnetis fit
#'
#' Returns several named variance-explained measures. They answer different
#' questions and are reported together deliberately; there is no single
#' number called "the" R-squared for these models. The returned frame names
#' the scale of each measure, because they are not on a common one.
#'
#' @section Selection measures:
#' **Latent-scale variance share** (always reported). The fraction of the
#' latent variance of one dyad that the fixed effects account for,
#' \deqn{\frac{Var(x'\beta)}{Var(x'\beta) + v_a + v_b + V_{uv} + v_e},}{Var(x'beta) / (Var(x'beta) + va + vb + V_uv + ve),}
#' computed at each posterior draw over the modeled dyads and summarized by
#' its posterior mean and a posterior interval. `va`, `vb` and `ve` are the
#' sender, receiver and dyadic residual variances (`ve` is fixed at 1 under the
#' probit); on a symmetric fit the actor term is `2 va`. There is no `2 cab`
#' term: a dyad's additive offset is `a_i + b_j` for two different, independent
#' actors, and `cab = Cov(a_i, b_i)` is a within-actor covariance.
#'
#' **This measure is the package's own construction, not a published
#' statistic.** No published work defines how to partition a latent index
#' containing the bilinear term `u_i'v_j`; McKelvey and Zavoina's R-squared has
#' no random effects at all, and Nakagawa and Schielzeth's marginal R-squared
#' is not defined over a bilinear term. Report it as this package's
#' construction.
#'
#' With `R > 0`, `V_uv` is the variance over the modeled dyads of the
#' **posterior-mean** product `U V'`, because `amen::ame()` reports the latent
#' factors only as a posterior mean. That understates the multiplicative
#' component, so the variance share is **biased upward**, and its interval
#' does not reflect uncertainty in that component. The returned notes say so.
#'
#' **Tjur's D** (binary outcomes). The coefficient of discrimination of Tjur
#' (2009): the mean predicted tie probability over observed ties minus the
#' mean over observed non-ties. The predicted probabilities are the fit's
#' posterior-predictive tie probabilities. It is a probability-scale quantity,
#' not a variance share, and is not comparable with an R-squared.
#'
#' **Null-comparison pseudo R-squared** (`R = 0` fits only;
#' `include_null_comparison = TRUE`). The proportional reduction in the
#' unexplained latent variance of a dyad, `va + vb + ve`, between this model
#' and a refitted one with no predictors:
#' `1 - unexplained(full) / unexplained(null)`, each side at its posterior
#' means. Also the package's own construction. The null model is refitted at
#' the parent fit's chain length with its own derived seed, so the call costs
#' one more MCMC run. The call stops with an error, naming the group, when
#' `amen::ame()` would start the null model's dyadic correlation at +/-1; set
#' `include_null_comparison = FALSE` for the other measures. On a binary probit `ve` is fixed at 1, so the reduction
#' can appear only in the additive components and the measure understates
#' explained variation relative to a linear-scale analogue; a note says so.
#' The value can be negative and is not clipped. With `R > 0` the null
#' comparison is **not reported**: the multiplicative variance is available
#' only as the variance of a posterior-mean product, which does not difference
#' on a matched basis between two separately fitted models. A note says so;
#' the other measures are unaffected.
#'
#' **Not reported.** Snijders and Bosker's R-squared-1 (a probit fixes the
#' level-1 variance it is built around); McFadden, Cox--Snell and Nagelkerke
#' (each needs a scalar model likelihood, conditional or marginal over the
#' actor effects, which the model does not determine).
#'
#' **Hierarchical multi-group fits** (`pooling = selection_pooling()`) are
#' refused: the measures above are defined for a per-group `amen::ame()` fit,
#' not for one hierarchical model over all groups. Use [netis_hierarchy()] for
#' the between-group layer.
#'
#' @references
#' Cox, D. R., & Snell, E. J. (1989). *Analysis of Binary Data* (2nd ed.). Chapman
#' and Hall.
#'
#' Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth's
#' \eqn{R^2_{GLMM}} to random slopes models. *Methods in Ecology and Evolution*,
#' 5(9), 944--946.
#'
#' McFadden, D. (1974). Conditional logit analysis of qualitative choice behavior.
#' In P. Zarembka (Ed.), *Frontiers in Econometrics* (pp. 105--142). Academic Press.
#'
#' McKelvey, R. D., & Zavoina, W. (1975). A statistical model for the analysis of
#' ordinal level dependent variables. *Journal of Mathematical Sociology*, 4(1),
#' 103--120.
#'
#' Nagelkerke, N. J. D. (1991). A note on a general definition of the coefficient
#' of determination. *Biometrika*, 78(3), 691--692.
#'
#' Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
#' obtaining R2 from generalized linear mixed-effects models. *Methods in
#' Ecology and Evolution*, 4(2), 133--142.
#'
#' Snijders, T. A. B., & Bosker, R. J. (1994). Modeled variance in two-level
#' models. *Sociological Methods & Research*, 22(3), 342--363.
#'
#' Tjur, T. (2009). Coefficients of determination in logistic regression
#' models -- A new proposal: The coefficient of discrimination. *The American
#' Statistician*, 63(4), 366--372.
#'
#' @param object a fitted rnetis model.
#' @param x a `netis_pseudo_r2` object, for the `print()` method.
#' @param group which group, when a Selection fit covers several.
#' @param level interval mass for the variance share's posterior interval.
#' @param include_null_comparison Selection only: whether to refit a
#'   predictor-free null model and report the null comparison. `TRUE` by
#'   default; it roughly doubles the time of the call, so set `FALSE` when only
#'   the variance share and Tjur's D are wanted. Ignored, with a note, when
#'   `R > 0`.
#' @param ... Selection: estimator arguments for the null-model refit (for
#'   example `nscan`), overriding the parent fit's. Unused by `print()`.
#' @return an object of class `netis_pseudo_r2`: a list with `model`,
#'   `family`, `group`, `scale`, `type`, `method`, `measures` (a data frame
#'   with `measure`, `estimate`, `lo`, `hi`, and on a Selection fit `scale`),
#'   `notes` and `level`.
#' @seealso [fit_selection()], [fit_influence()], [netis_estimates()]
#' @examples
#' # Cost: an amen MCMC fit, over CRAN's example budget at any usable chain length.
#' \donttest{
#' set.seed(1)
#' ids <- sprintf("n%02d", 1:10)
#' nodes <- data.frame(group = 1, node = ids, x = stats::rnorm(10))
#' pairs <- subset(expand.grid(source = ids, target = ids,
#'                             stringsAsFactors = FALSE), source != target)
#' ties <- data.frame(group = 1, pairs,
#'                    tie = stats::rbinom(nrow(pairs), 1, 0.25))
#' nd <- netis_data(nodes, ties,
#'                  tie_meta = list(tie = list(zero_meaning = "measured_zero")))
#' fit <- fit_selection(tie ~ sender(x), data = nd,
#'                      nscan = 200, burn = 50, odens = 10)
#' pseudo_r2(fit)
#' }
#' @export
pseudo_r2 <- function(object, ...) {
  UseMethod("pseudo_r2")
}

#' @rdname pseudo_r2
#' @export
pseudo_r2.default <- function(object, ...) {
  stop("pseudo_r2() is defined for netis_selection and netis_influence fits.",
       call. = FALSE)
}

#' @rdname pseudo_r2
#' @export
pseudo_r2.netis_selection <- function(object, group = NULL, level = 0.95,
                                      include_null_comparison = TRUE, ...) {
  if (.selection_is_hierarchical(object)) {
    stop(
      "pseudo_r2() is not defined for a hierarchical multi-group Selection fit (pooling = selection_pooling()). Its measures are defined for a per-group amen::ame() fit, not for one hierarchical model over all groups. Use netis_hierarchy() for the between-group layer, or fit with pooling = NULL to get per-group measures.",
      call. = FALSE
    )
  }
  g <- .resolve_effect_group(object, group)
  extract <- object$per_group[[g]]
  design <- object$designs[[g]]
  R <- object$R %||% 0L

  draws <- extract$draws
  vc <- extract$variance_components
  X <- .effect_design_matrix(design, colnames(draws))
  keep <- !is.na(as.vector(design$Y))
  Xk <- X[keep, , drop = FALSE]
  n_draws <- nrow(draws)
  dyad_mask <- .selection_dyad_mask(design)
  alpha <- (1 - level) / 2
  notes <- character(0)

  # The multiplicative variance: exactly zero at R = 0; at R > 0 the variance
  # of the posterior-mean product, the only version amen reports.
  v_uv <- if (R == 0L) 0 else stats::var(extract$multiplicative[dyad_mask])

  # A symmetric fit's VC block has no vb (one additive effect); Var(a_i + a_j)
  # over two independent actors is 2 * va, so va stands in for the missing vb.
  has_vb <- "vb" %in% colnames(vc)
  share <- vapply(seq_len(n_draws), function(s) {
    v_fixed <- stats::var(as.vector(Xk %*% draws[s, ]))
    v_actor <- vc[s, "va"] + if (has_vb) vc[s, "vb"] else vc[s, "va"]
    v_fixed / (v_fixed + v_actor + v_uv + vc[s, "ve"])
  }, numeric(1))

  measures <- data.frame(
    measure = "Latent-scale variance share (fixed effects)",
    scale = "latent",
    estimate = mean(share),
    lo = unname(stats::quantile(share, alpha)),
    hi = unname(stats::quantile(share, 1 - alpha)),
    stringsAsFactors = FALSE
  )
  notes <- c(notes, "The latent-scale variance share is this package's own construction, not a published statistic.")
  if (R > 0L) {
    notes <- c(notes, "With R > 0 the multiplicative-term variance is the variance of the posterior-mean product, which is what amen reports. That understates the multiplicative component, so the variance share is biased upward and its interval understates uncertainty in that component.")
  }

  if (identical(object$family, "bin")) {
    Y <- design$Y
    obs <- !is.na(Y)
    p <- extract$posterior_predictive[obs]
    y <- Y[obs]
    tjur <- if (any(y == 1) && any(y == 0)) {
      mean(p[y == 1]) - mean(p[y == 0])
    } else {
      NA_real_
    }
    measures <- rbind(measures, data.frame(
      measure = "Tjur's D (probability scale)",
      scale = "probability",
      estimate = tjur, lo = NA_real_, hi = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  if (isTRUE(include_null_comparison)) {
    if (R > 0L) {
      notes <- c(notes, "The null-comparison pseudo R2 is reported only for R = 0 fits. With R > 0 the multiplicative variance is available only as the variance of a posterior-mean product, which does not difference on a matched basis between two separately fitted models. The other measures are unaffected.")
    } else {
      null_vc <- .selection_null_variance(object, g, ...)
      # Var(a_i + b_j) for a modeled dyad: va + vb, no cab cross term. A
      # symmetric fit has no vb and va stands in.
      unexplained <- function(vc_means) {
        vb <- if ("vb" %in% names(vc_means)) vc_means[["vb"]] else vc_means[["va"]]
        vc_means[["va"]] + vb + vc_means[["ve"]]
      }
      r2_null <- 1 - unexplained(colMeans(vc)) / unexplained(null_vc)
      measures <- rbind(measures, data.frame(
        measure = "Null-comparison pseudo R2 (latent scale)",
        scale = "latent",
        estimate = r2_null, lo = NA_real_, hi = NA_real_,
        stringsAsFactors = FALSE
      ))
      if (identical(object$family, "bin")) {
        notes <- c(notes, paste(
          "The null comparison on a binary probit: the residual variance is",
          "fixed at 1 for identification, so predictors cannot reduce it and",
          "the reduction can appear only in the additive components. It",
          "therefore understates explained variation relative to a",
          "linear-scale analogue, and is not comparable with a linear-model",
          "R-squared."
        ))
      }
      if (!is.na(r2_null) && r2_null < 0) {
        notes <- c(notes, paste(
          "The null comparison is negative. It is deliberately not clipped to",
          "zero: the full model did not reduce the unexplained latent variance",
          "relative to the null. Because the two are separate MCMC fits, a",
          "small negative value can also be Monte Carlo error; a longer chain",
          "distinguishes the two."
        ))
      }
    }
  }

  structure(
    list(model = "selection", family = object$family, group = g,
         # Two scales are reported (latent for the variance measures,
         # probability for Tjur's D), recorded per measure in measures$scale.
         scale = "mixed (see measures$scale)",
         type = "variance share, discrimination, null comparison",
         method = "rnetis (see ?pseudo_r2)",
         measures = measures, notes = notes, level = level),
    class = "netis_pseudo_r2"
  )
}

#' The dyads a variance decomposition is computed over
#'
#' Observed, self-ties excluded. This is the right population for `U`/`V`: the
#' fixed-effect variance it is compared against is
#' computed over masked, off-diagonal dyads too, and a ratio of quantities
#' taken over different populations is not a ratio of comparable things.
#'
#' @param design an assembled Selection design.
#' @return a logical n-by-n matrix, `TRUE` where a dyad is modeled.
#' @noRd
.selection_dyad_mask <- function(design) {
  mask <- !is.na(design$Y)
  diag(mask) <- FALSE
  mask
}

#' Why each Influence backend does or does not decompose its variance
#'
#' Recorded as a registry rather than as a chain of `if`s, because these are
#' decisions with reasons and a reader should be able to find all of them in
#' one place -- including the ones that are refusals.
#'
#' @param backend the backend name.
#' @return `NULL` where a decomposition is defined, or the refusal message.
#' @noRd
.influence_r2_refusal <- function(backend) {
  switch(
    backend,
    lme4 = NULL,
    stats = NULL,
    lnam = "pseudo_r2() is not defined for a SAR (lnam-backed) fit. Several variance decompositions exist for spatial-autoregressive models and they disagree, because part of the outcome variance is generated by the autoregressive feedback itself rather than by the predictors -- so there is no single quantity 'variance explained' names here. The fit's sigma is reported by netis_glance().",
    sprintf("pseudo_r2() has no variance decomposition defined for backend '%s'.",
            backend)
  )
}

#' @noRd
.influence_r2_ols <- function(object) {
  measures <- data.frame(
    measure = c("R-squared (ordinary least squares)",
                "Adjusted R-squared"),
    scale = "observed",
    estimate = c(object$r_squared, object$adj_r_squared),
    lo = NA_real_, hi = NA_real_,
    stringsAsFactors = FALSE
  )
  structure(
    list(model = "influence", family = "gaussian", group = NULL,
         # A Gaussian fit's R2 is an OBSERVED-scale quantity. Recorded so it
         # cannot be silently compared with a Selection fit's latent-scale
         # one, which answers a different question.
         scale = "observed", type = "total", method = "ols",
         measures = measures,
         notes = paste(
           "This is an ordinary R-squared, not a pseudo-R-squared: an OLS fit",
           "has a genuine proportion-of-variance-explained and it is reported",
           "under its own name. It is the same r.squared netis_glance() reports."
         ),
         level = NA_real_),
    class = "netis_pseudo_r2"
  )
}

#' @rdname pseudo_r2
#'
#' @section Influence measures: Each backend declares a defensible
#' decomposition or a documented refusal. No backend is given a number because
#' another backend has one.
#'
#' - **`lme4`** -- the **Nakagawa–Schielzeth** decomposition: marginal R²
#'   (fixed effects over total variance) and conditional R² (fixed plus random
#'   over total). Random-intercept fits only; the random-slope extension
#'   (Johnson 2014) is not implemented, and a fit with a random slope is
#'   refused with that reason rather than given a wrong number.
#' - **`stats` (OLS)** -- ordinary R² and adjusted R², which is a real R² and
#'   is labeled as one rather than as a pseudo-measure. It is the same
#'   `r.squared` that [netis_glance()] reports.
#' - **`lnam`** -- refused, with its reason in the message: part of the outcome
#'   variance is generated by the autoregressive feedback itself, so published
#'   decompositions disagree and no single "variance explained" is defined.
#'
#' Snijders–Bosker \eqn{R^2_1} is **not** reported for Influence either: its null
#' model contains no predictors at all, and [fit_influence()] refuses a formula with
#' no predictor, so that null model cannot be fitted here. Reporting a
#' differently-nulled quantity under the established name would mislead.
#'
#' @references
#' Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth's R2GLMM to random
#' slopes models. *Methods in Ecology and Evolution*, 5(9), 944--946.
#'
#' Snijders, T. A. B., & Bosker, R. J. (1994). Modeled variance in two-level models.
#' *Sociological Methods & Research*, 22(3), 342--363.
#'
#' @examples
#' nodes <- do.call(rbind, lapply(1:8, function(g) data.frame(
#'   group = g, node = rep(c("a", "b", "c", "d"), 2),
#'   wave = rep(1:2, each = 4), y = round(rnorm(8, mean = g), 2)
#' )))
#' ties <- do.call(rbind, lapply(1:8, function(g) data.frame(
#'   group = g, source = c("a", "b", "c"), target = c("b", "c", "d"), w = 1
#' )))
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   fit <- fit_influence(y ~ ar() + exposure(y, via = w) + (1 | group), data = nd)
#'   pseudo_r2(fit)
#' }
#' @export
pseudo_r2.netis_influence <- function(object, ...) {
  refusal <- .influence_r2_refusal(object$backend)
  if (!is.null(refusal)) {
    stop(refusal, call. = FALSE)
  }
  if (identical(object$backend, "stats")) {
    return(.influence_r2_ols(object))
  }
  ex <- backend_extract(.new_backend("lme4"), object$backend_fit)
  vc <- ex$var_corr

  re_rows <- vc[vc$grp != "Residual", , drop = FALSE]
  if (any(re_rows$var1 != "(Intercept)" | !is.na(re_rows$var2))) {
    stop(
      "This fit has a random slope. The Nakagawa-Schielzeth decomposition implemented here covers random intercepts only; the random-slope extension (Johnson 2014) is not implemented, and reporting the intercept-only formula for this model would be wrong rather than approximate.",
      call. = FALSE
    )
  }

  var_fixed <- stats::var(as.numeric(ex$fixed_fitted))
  var_random <- sum(re_rows$vcov)
  var_resid <- ex$sigma^2
  total <- var_fixed + var_random + var_resid

  measures <- data.frame(
    measure = c("Nakagawa-Schielzeth marginal R2 (fixed effects)",
                "Nakagawa-Schielzeth conditional R2 (fixed + random)"),
    scale = "observed",
    estimate = c(var_fixed / total, (var_fixed + var_random) / total),
    lo = NA_real_, hi = NA_real_,
    stringsAsFactors = FALSE
  )

  structure(
    list(model = "influence", family = "gaussian", group = NULL,
         # THE GENUINE STATISTIC HERE, unlike the Selection side. A Gaussian
         # mixed model with random intercepts is exactly the case Nakagawa &
         # Schielzeth define R2_GLMM over, and it is an observed-scale quantity.
         scale = "observed", type = "marginal_and_conditional",
         method = "nakagawa_schielzeth",
         measures = measures,
         notes = "Marginal answers 'how much do the predictors explain'; conditional adds the group structure. They are different questions, which is why both are reported.",
         level = NA_real_),
    class = "netis_pseudo_r2"
  )
}

#' @rdname pseudo_r2
#' @export
print.netis_pseudo_r2 <- function(x, ...) {
  cat("Pseudo R-squared (", x$model, " model",
      if (!is.null(x$group)) paste0(", group ", x$group), ")\n", sep = "")
  cat(strrep("-", 60), "\n", sep = "")
  for (i in seq_len(nrow(x$measures))) {
    m <- x$measures[i, ]
    if (is.na(m$lo)) {
      cat(sprintf("  %-48s %6.3f\n", m$measure, m$estimate))
    } else {
      cat(sprintf("  %-48s %6.3f  [%.3f, %.3f]\n",
                  m$measure, m$estimate, m$lo, m$hi))
    }
  }
  for (note in x$notes) {
    cat("\n  ", paste(strwrap(note, 66), collapse = "\n  "), "\n", sep = "")
  }
  cat("\nDifferent measures answer different questions; none is 'the' R-squared.\n")
  invisible(x)
}

#' Posterior-mean variance components of the predictor-free null model
#'
#' The same outcome, route, family and `R` (always 0 here), with every design
#' block empty: additive structure only. Fitted through the same backend path
#' as any Selection fit, so both sides of the comparison are estimated the
#' same way. The estimation regime is inherited from the parent fit, with the
#' caller's own `...` winning where it names something; the seed is derived
#' from the group's own fit seed.
#'
#' @return the named vector of posterior-mean variance components.
#' @noRd
.selection_null_variance <- function(object, g, ...) {
  design <- object$designs[[g]]
  backend <- .new_backend("amen")

  controls <- object$estimation_controls
  if (!is.null(controls$group_seeds) && g %in% names(controls$group_seeds)) {
    controls$seed <- controls$group_seeds[[g]]
  }
  # The refit has its own start, so it can reach a bound the full model did not.
  if (!isTRUE(object$symmetric) &&
      .amen_start_rho_at_bound(list(Y = design$Y), object$family)) {
    stop(sprintf(
      "The null comparison cannot be computed for group '%s': without predictors, %s",
      g, sub("^Group '[^']*' has", "the group has", .selection_rho_bound_message(g, nrow(design$Y)))
    ), call. = FALSE)
  }
  dots <- list(...)
  inherited <- .refit_backend_args(controls, "null_model")
  args <- c(dots, inherited[setdiff(names(inherited), names(dots))])

  null_fit <- do.call(backend_fit, c(
    list(
      backend,
      # symmetric travels with the null design: dropping it would refit a
      # symmetric relation's null through the DIRECTED sampler.
      list(Y = design$Y, Xrow = NULL, Xcol = NULL, Xdyad = NULL,
           family = object$family, R = object$R,
           symmetric = isTRUE(object$symmetric)),
      data = NULL
    ),
    args
  ))
  colMeans(backend_extract(backend, null_fit)$variance_components)
}
