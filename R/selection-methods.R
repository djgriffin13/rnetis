# S3 methods for netis_selection.
#
# Methods return a quantity only where it is statistically valid. logLik/AIC/BIC
# are refused: amen returns no log-likelihood, and reconstructing one from
# posterior draws would be a different quantity under the same name.
#
# Everything printed here speaks in Selection terms -- senders, receivers,
# dyads, transitions -- never in the backend's vocabulary.

#' Standard R model methods for a fitted Selection model
#'
#' `netis_selection` objects (from [fit_selection()]) support `print()`,
#' [summary()][summary.netis_selection], `coef()`, `vcov()`, `confint()`, and
#' `nobs()`. Which methods return a value is a statistical statement, not an
#' oversight -- see *Methods that refuse* below.
#'
#' `print()` gives the compact view: the relation being modeled, the outcome
#' family, the transition structure, the pooling statement, the coefficient
#' table with its posterior summaries and significance marks (see
#' [print.netis_selection()]), and under it the count of observed dyads, the
#' pseudo R-squared measures, one line of posterior predictive check and the
#' count of fit-time findings. [summary()][summary.netis_selection] adds the
#' scale the coefficients are on and the posterior predictive table -- each
#' network statistic's observed value beside its simulated 95% range and its
#' `ppp` -- with [netis_gof()] and [netis_diagnostics()] named as the ways to
#' get the detail; on a hierarchical fit it also adds the worst convergence
#' diagnostics. On
#' the per-group route the diagnostics are in `fit$convergence`, one entry per
#' group.
#'
#' `confint()` returns the intervals [netis_estimates()] reports: posterior
#' quantile intervals, or normal intervals from the pooled standard errors on a
#' pooled multi-group fit. `nobs()` counts observed dyads, as
#' [netis_glance()] does.
#'
#' # Methods that refuse
#'
#' `logLik()`, `AIC()`, and `BIC()` **stop with an error**. The `amen` backend
#' returns no log-likelihood, and reconstructing one from posterior draws would
#' be a different quantity shipped under a familiar name.
#' For the question those would have been reached for -- how much variation
#' do the predictors account for -- use [pseudo_r2()].
#'
#' `fitted()`, `residuals()`, `sigma()` and `predict()` likewise stop: an AME
#' model fits an effect per actor, so an in-sample predicted tie probability is
#' optimistic by construction, and a `predict()` that impersonated a regression
#' `predict()` would invite exactly that misreading. Use [selection_effect()]
#' for probability-scale contrasts.
#'
#' # Errors and refusals
#'
#' `vcov()` **errors on any multi-group fit from the per-group route**
#' (`pooling = NULL`), for two different reasons that both come to the same place:
#' no covariance matrix was ever estimated, and rnetis will not invent one.
#'
#' Under `pool_beta = FALSE` each group is a separate model, so there is no
#' joint covariance; returning one would imply a joint model that was never
#' fitted, and its block structure would be a fabrication.
#'
#' Under `pool_beta = TRUE` the pooling runs **one coefficient at a time** —
#' each group contributes a posterior mean and a marginal variance, and no
#' between-coefficient covariance survives that. Assembling a matrix from the
#' pooled variances would assert zero covariances nobody estimated. Anything
#' needing a joint covariance — a contrast, a multi-coefficient Wald test, a
#' joint credible region — needs a single group's fit, which has one.
#'
#' `coef()` has no such restriction and returns the coefficients as stored: the
#' pooled vector when beta was pooled, a matrix of per-group rows when it was
#' not. On a hierarchical fit `coef()` and `vcov()` are population-level and
#' share one set of names; a group-specific (`unpooled`) coefficient has no
#' population value and is `NA` in both (see [netis_hierarchy()] and
#' [netis_estimates()] for its per-group values). `confint()` refuses a fit with
#' uncombined groups, since there is no one interval per term.
#'
#' @param object a `netis_selection` object.
#' @param parm,level for `confint()`: which terms (default all), and the
#'   interval level.
#' @param newdata unused: `predict()` refuses.
#' @param ... unused; present for S3 consistency with the generics.
#' @return `coef()` a named numeric vector of coefficients on the scale the
#'   outcome family implies (the probit latent scale for `family = "bin"`), or a
#'   matrix with one row per group under `pool_beta = FALSE`.
#'   `vcov()` the coefficient covariance matrix, with row and column names
#'   matching `coef()`, or an error (above). `confint()` a two-column matrix of
#'   interval limits. `nobs()` the number of observed dyads.
#' @seealso [fit_selection()], [summary.netis_selection()],
#'   [netis_estimates()] for the coefficient table as data,
#'   [pseudo_r2()] for variance accounted for
#' @examples
#' # Two waves of a small network: wave 1 is the initial condition that
#' # prior_tie() reaches back to, wave 2 is the modeled outcome.
#' nodes <- data.frame(
#'   group = 1, node = letters[1:6], wave = rep(1:2, each = 6),
#'   x = c(1:6, 2:7)
#' )
#' pairs <- subset(expand.grid(source = letters[1:6], target = letters[1:6],
#'                             stringsAsFactors = FALSE), source != target)
#' ties <- rbind(
#'   data.frame(group = 1, pairs, wave = 1, tie = 0:1),
#'   data.frame(group = 1, pairs, wave = 2, tie = 1:0)
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave",
#'                  tie_meta = list(tie = list(zero_meaning = "measured_zero")))
#'
#' # Cost: an amen MCMC fit, over CRAN's example budget at any usable chain length.
#' \donttest{
#' # nscan is tiny so this runs in example time. A chain this short says
#' # nothing about convergence -- see `vignette("selection")` for an
#' # analysis-scale run.
#' fit <- fit_selection(
#'   tie ~ sender(x) + prior_tie(),
#'   data = nd, nscan = 100, burn = 50, odens = 10
#' )
#' fit
#' coef(fit)
#' vcov(fit)
#' }
#' @name netis-selection-methods
NULL

#' Print a Selection fit
#'
#' Reports what was fitted and what it was fitted to, then the coefficient table
#' in a layout parallel to `summary.lm()`'s, labelled for a posterior: the
#' posterior mean, the posterior SD, the 95% posterior interval, the ratio of
#' the mean to the SD (`Est/SD`), and a two-sided posterior tail probability
#' (`Tail prob.`, twice the posterior probability that the coefficient has the
#' opposite sign) with significance marks on `lm()`'s cutoffs. These are
#' posterior summaries and not tests, and the note under the table says so. The
#' tail probability is counted from the retained draws, so it resolves nothing
#' below `2 / n_draws`: a value below that prints as `<` the bound and is marked
#' from the bound. A two-stage pooled fit keeps no pooled draws and reports z
#' tests from its pooled standard errors instead.
#'
#' Under the table: the number of observed dyads, the pseudo R-squared measures
#' that need no refit (see [pseudo_r2()]), one line of posterior predictive
#' check -- how many of the observed network statistics fall inside the central
#' 95% of their simulated distributions, naming those that do not, with
#' [netis_gof()] and `plot(fit, what = "gof")` named for the table and the
#' histograms -- and the count of fit-time findings. Effect sizes are not
#' computed at fit time and are not printed here.
#'
#' @param x a `netis_selection` fit.
#' @param ... unused; present for S3 consistency.
#' @return `x`, invisibly. Called for its output.
#' @seealso [fit_selection()], [summary.netis_selection()]
#' @export
print.netis_selection <- function(x, ...) {
  cat("<netis_selection>\n")
  cat("  ", paste(deparse(x$call$formula), collapse = " "), "\n", sep = "")
  cat("  Outcome family: ", .selection_family_label(x$family), "\n", sep = "")
  cat("  Groups: ", length(x$groups), " (", paste(utils::head(x$groups, 4), collapse = ", "),
      if (length(x$groups) > 4) ", ..." else "", ")\n", sep = "")
  cat("  ", .selection_backend_line(x), "\n", sep = "")
  cat("  ", .format_selection_waves(x$waves), "\n", sep = "")
  pool <- .format_beta_pooling(x$pooled_beta)
  if (length(pool)) cat(paste0("  ", pool, "\n"), sep = "")
  if (.selection_is_hierarchical(x)) {
    cat(paste0("  ", .selection_regime_lines(x), "\n"), sep = "")
  }
  cat("\nCoefficients:\n")
  .print_coef_table(.selection_estimate_table(x))
  cat("\n")
  .cat_fit_lines(.selection_fit_lines(x))
  counts <- .diagnostic_counts(x$diagnostics)
  cat(sprintf("Fit-time findings: %d WARNING, %d NOTE; netis_diagnostics(fit) lists them. More: summary(fit).\n",
              counts[["n_warning"]], counts[["n_note"]]))
  invisible(x)
}

#' Whether a Selection fit came from the hierarchical multi-group sampler
#'
#' @noRd
.selection_is_hierarchical <- function(x) {
  identical(x$backend, "rnetis-mgc")
}

#' The backend line, naming the sampler that actually ran
#'
#' A hierarchical fit does not call `amen::ame()`: it runs one chain over all
#' groups using amen's conditional updates, and the line says so.
#'
#' @noRd
.selection_backend_line <- function(x) {
  if (.selection_is_hierarchical(x)) {
    return("Backend: rnetis hierarchical sampler -- every group in one chain, using amen's conditional updates (see ?hierarchical_selection)")
  }
  "Backend: amen::ame(), one observed network per group"
}

#' One line per coefficient regime of a hierarchical fit
#'
#' @noRd
.selection_regime_lines <- function(x) {
  reg <- x$mgc_draws$regime
  cov <- x$mgc_draws$covariance %||% "diagonal"
  part <- function(kind) names(reg)[reg == kind]
  out <- character(0)
  if (length(part("shared"))) {
    out <- c(out, sprintf("Shared across groups: %s", paste(part("shared"), collapse = ", ")))
  }
  if (length(part("random"))) {
    out <- c(out, sprintf(
      "Varying across groups (random), %s between-group covariance: %s",
      if (identical(cov, "unstructured")) "unstructured" else "diagonal",
      paste(part("random"), collapse = ", ")))
  }
  if (length(part("group"))) {
    out <- c(out, sprintf("Estimated separately in each group (unpooled): %s",
                          paste(part("group"), collapse = ", ")))
  }
  if (length(x$hierarchical$moderators)) {
    out <- c(out, sprintf(
      "Moderators of every random coefficient's mean: %s; a random coefficient's population row is its value at every moderator = 0",
      paste(x$hierarchical$moderators, collapse = ", ")))
  }
  n_flat <- sum(!is.na(x$tie_variation %||% NA_character_))
  if (n_flat) {
    out <- c(out, sprintf(
      "%d group(s) admitted with no variation in their binary ties (random intercept; see ?hierarchical_selection)",
      n_flat))
  }
  out
}

#' The standard errors behind the printed table
#'
#' Two sources, because a pooled fit has no covariance matrix: the pooled
#' marginal variances when beta was pooled across groups, and the diagonal of
#' the single-group covariance otherwise. Both are per-coefficient standard
#' errors, which is all the printed table uses.
#'
#' @noRd
.selection_coef_se <- function(x, nms) {
  if (.selection_is_pooled(x)) {
    return(sqrt(x$pooled_beta$var)[nms])
  }
  sqrt(diag(x$vcov))[nms]
}

#' @noRd
.selection_estimate_table <- function(x) {
  if (.selection_is_hierarchical(x)) {
    return(.selection_estimate_table_hierarchical(x))
  }
  est <- x$coefficients
  interval <- function(e) {
    sprintf("[%s, %s]", format(round(e$conf.low, 4), trim = TRUE),
            format(round(e$conf.high, 4), trim = TRUE))
  }
  if (is.matrix(est)) {
    # Unpooled multi-group: one row per group, and no single table of
    # estimates to print. Report per group rather than inventing an average.
    # Each group is its own posterior, so each row's summaries come from that
    # group's draws.
    e <- netis_estimates(x)
    out <- data.frame(
      Group = e$group, Term = e$term,
      Estimate = round(e$estimate, 4),
      `Post.SD` = round(e$std.error, 4),
      `95% interval` = interval(e),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    tails <- lapply(seq_len(nrow(e)), function(i) {
      g <- match(e$group[i], as.character(x$groups))
      beta <- .selection_beta_draws(x$per_group[[g]]$draws, e$term[i])
      if (is.null(beta)) NULL else .posterior_tail(beta[, e$term[i]])
    })
    return(.add_posterior_columns(out, tails))
  }
  # The same interval netis_estimates() reports, so that the printed table and
  # the accessor cannot disagree: a posterior quantile interval from the draws,
  # or a normal interval from the pooled SE where no pooled draws exist.
  e <- netis_estimates(x)
  out <- data.frame(
    Term = e$term,
    Estimate = round(e$estimate, 4),
    SE = round(e$std.error, 4),
    Interval = sprintf("[%s, %s]",
                       format(round(e$conf.low, 4), trim = TRUE),
                       format(round(e$conf.high, 4), trim = TRUE)),
    stringsAsFactors = FALSE
  )
  # NAME THE MASS, as the Influence table does. "Interval" alone invites the
  # reader to supply "95%" from habit, and an interval whose coverage is
  # assumed rather than stated is the kind of number that gets quoted with the
  # wrong label. Both families say it, so neither has to be remembered
  # separately.
  names(out)[names(out) == "Interval"] <- "95% interval"
  beta <- .selection_beta_draws(x$draws, e$term)
  if (is.null(beta)) {
    # No draws of the reported coefficients: a two-stage pooled fit, whose
    # estimates are precision-weighted means. Its test is the normal one its
    # interval already uses.
    z <- e$estimate / e$std.error
    p <- 2 * stats::pnorm(-abs(z))
    out[["z value"]] <- ifelse(is.na(z), "", sprintf("%.3f", z))
    out[["Pr(>|z|)"]] <- .netis_format_p(p)
    out[[" "]] <- .netis_stars(p)
    attr(out, "netis_notes") <- .SELECTION_POOLED_NOTE
    return(out)
  }
  # A POSTERIOR SD, and named as one: the column is the spread of the draws,
  # not the standard error of an estimator.
  names(out)[names(out) == "SE"] <- "Post.SD"
  .add_posterior_columns(out, lapply(e$term, function(k) .posterior_tail(beta[, k])))
}

#' Summarize a Selection fit
#'
#' @param object a `netis_selection` fit.
#' @param ... unused; present for S3 consistency.
#' @return an object of class `summary.netis_selection`.
#' @seealso [print.netis_selection()], [fit_selection()]
#' @export
summary.netis_selection <- function(object, ...) {
  structure(list(fit = object), class = "summary.netis_selection")
}

#' @rdname summary.netis_selection
#' @param x a `summary.netis_selection` object.
#' @export
print.summary.netis_selection <- function(x, ...) {
  fit <- x$fit
  cat("  Formula: ", paste(deparse(fit$call$formula), collapse = " "), "\n", sep = "")
  cat("  Family:  ", .selection_family_label(fit$family), "\n", sep = "")
  cat("  Groups:  ", length(fit$groups), "\n", sep = "")
  cat("  ", .selection_backend_line(fit), "\n", sep = "")
  cat("  ", .format_selection_waves(fit$waves), "\n", sep = "")
  if (.selection_is_hierarchical(fit)) {
    cat(paste0("  ", .selection_regime_lines(fit), "\n"), sep = "")
  }
  note <- .selection_scale_note(fit$family)
  if (length(note)) cat(paste0("  ", note, "\n"), sep = "")
  pool <- .format_beta_pooling(fit$pooled_beta)
  if (length(pool)) {
    cat("\n")
    cat(paste0("  ", pool, "\n"), sep = "")
  }
  cat("\nCoefficients:\n")
  .print_coef_table(.selection_estimate_table(fit))
  cat("\nModel fit:\n")
  fit_lines <- .selection_fit_lines(fit, full = TRUE)
  .cat_fit_lines(paste0("  ", fit_lines))
  ppc <- attr(fit_lines, "ppc_table")
  if (!is.null(ppc)) {
    cat("\n")
    .print_one_block(ppc, right = FALSE)
    if ("ppp" %in% names(ppc)) {
      cat("  ppp is one-sided: near 0 and near 1 are both misfit, 0.5 is agreement.\n")
    }
  }
  if (.selection_is_hierarchical(fit)) {
    conv <- .selection_convergence_lines(fit)
    if (length(conv)) {
      cat("\nConvergence:\n")
      cat(paste0("  ", conv, "\n"), sep = "")
    }
    cat("\nBetween-group SDs, group coefficients and correlations: netis_hierarchy(fit).\n")
  }
  counts <- .diagnostic_counts(fit$diagnostics)
  cat(sprintf("\nFit-time findings: %d WARNING, %d NOTE. List them with netis_diagnostics(fit).\n",
              counts[["n_warning"]], counts[["n_note"]]))
  invisible(x)
}

#' The population-level table of a hierarchical fit
#'
#' Posterior mean and SD of each coefficient's population value -- the shared
#' value, or a random coefficient's hierarchical mean -- with an equal-tailed
#' 95% posterior quantile interval taken from the draws, not mean +/- 1.96 SD.
#' A group-specific (unpooled) coefficient has no population value and is
#' listed with its regime instead of a number.
#'
#' @noRd
.selection_estimate_table_hierarchical <- function(x) {
  d <- x$mgc_draws
  reg <- d$regime
  has_mod <- length(x$hierarchical$moderators) > 0L
  rows <- lapply(names(reg), function(k) {
    if (identical(unname(reg[k]), "group")) {
      return(data.frame(Term = k, Regime = "unpooled", Estimate = NA_real_,
                        `Post.SD` = NA_real_, `95% interval` = "see per-group results",
                        check.names = FALSE, stringsAsFactors = FALSE))
    }
    v <- d$mu[, k]
    q <- stats::quantile(v, c(0.025, 0.975), names = FALSE)
    data.frame(
      Term = k,
      Regime = if (identical(unname(reg[k]), "random")) {
        if (has_mod) "random (mean at z = 0)" else "random (mean)"
      } else "shared",
      Estimate = round(mean(v), 4), `Post.SD` = round(stats::sd(v), 4),
      `95% interval` = sprintf("[%s, %s]", format(round(q[1], 4), trim = TRUE),
                               format(round(q[2], 4), trim = TRUE)),
      check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  .add_posterior_columns(out, lapply(names(reg), function(k) {
    if (identical(unname(reg[k]), "group")) NULL else .posterior_tail(d$mu[, k])
  }))
}

#' Convergence summary lines for a hierarchical fit, without a verdict
#'
#' @noRd
.selection_convergence_lines <- function(fit) {
  tab <- fit$convergence_table
  if (is.null(tab) || !nrow(tab)) return(character(0))
  ok <- tab$status == "ok"
  worst <- function(col, fun) {
    v <- tab[[col]]
    keep <- ok & is.finite(v)
    if (!any(keep)) return(NULL)
    i <- which(keep)[fun(v[keep])]
    sprintf("%.3g (%s)", v[i], tab$parameter[i])
  }
  r <- worst("rhat", which.max)
  e <- worst("ess_bulk", which.min)
  t <- worst("ess_tail", which.min)
  out <- c(
    sprintf("%d chain(s) x %d retained draws; diagnostics %s.",
            attr(tab, "n_chains") %||% NA_integer_, attr(tab, "n_draws") %||% NA_integer_,
            attr(tab, "scope") %||% "unavailable"),
    if (!is.null(r)) paste("Largest R-hat (rank-normalized, folded):", r),
    if (!is.null(e)) paste("Smallest bulk ESS:", e),
    if (!is.null(t)) paste("Smallest tail ESS:", t)
  )
  n_na <- sum(!ok)
  if (n_na) {
    out <- c(out, sprintf("%d quantity(ies) have no diagnostic (%s); see fit$convergence_table.",
                          n_na, paste(unique(tab$status[!ok]), collapse = ", ")))
  }
  c(out, attr(tab, "note"))
}
#' @rdname netis-selection-methods
#' @export
coef.netis_selection <- function(object, ...) object$coefficients

#' @rdname netis-selection-methods
#' @export
vcov.netis_selection <- function(object, ...) {
  if (is.null(object$vcov)) {
    if (!is.null(object$pooled_beta)) {
      stop(
        "This fit pooled its coefficients across groups, and the pooling runs one coefficient at a time: each group contributes its posterior mean and its marginal variance, and no covariance between coefficients survives that. There is therefore no covariance matrix to return, and building one from the pooled variances would assert zero covariances that were never estimated. Per-coefficient standard errors are in summary(); for anything needing a joint covariance -- a contrast, a multi-coefficient Wald test, a joint credible region -- use a single group's fit, which has one.",
        call. = FALSE
      )
    }
    stop(
      "This fit covers several groups estimated independently (pool_beta = FALSE), so there is no joint covariance matrix to return. Returning one would imply a joint model that was not fitted. Use the per-group results, or refit with pool_beta = TRUE, which combines the per-group coefficients by inverse-variance weighting.",
      call. = FALSE
    )
  }
  object$vcov
}

#' @rdname netis-selection-methods
#' @export
confint.netis_selection <- function(object, parm, level = 0.95, ...) {
  if ("method" %in% names(list(...))) {
    stop(
      "confint() on a Selection fit has no `method` argument: it returns the intervals netis_estimates() reports, from the posterior, and nothing else is available.",
      call. = FALSE
    )
  }
  est <- netis_estimates(object, level = level)
  if (!.selection_is_hierarchical(object) && any(!is.na(est$group))) {
    stop(
      "This fit covers several groups estimated independently (pool_beta = FALSE), so it has one interval per group and term, not one per term. netis_estimates() returns them, with a group column.",
      call. = FALSE
    )
  }
  est <- est[is.na(est$group) & est$role != "moderator", , drop = FALSE]
  nms <- names(object$coefficients)
  a <- (1 - level) / 2
  out <- matrix(NA_real_, length(nms), 2L,
                dimnames = list(nms, sprintf("%s %%", format(100 * c(a, 1 - a), trim = TRUE, digits = 3))))
  at <- match(est$term, nms)
  out[at, 1L] <- est$conf.low
  out[at, 2L] <- est$conf.high
  if (!missing(parm)) out <- out[parm, , drop = FALSE]
  out
}

#' @rdname netis-selection-methods
#' @exportS3Method stats::nobs
nobs.netis_selection <- function(object, ...) .selection_nobs(object)

#' @noRd
.selection_no_loglik <- function() {
  stop(
    "A Selection fit has no log-likelihood: amen::ame() returns none, and reconstructing one from posterior draws would be a different quantity under the same name, so logLik(), AIC() and BIC() are not defined. For how much variation the predictors account for, use pseudo_r2().",
    call. = FALSE
  )
}

#' @rdname netis-selection-methods
#' @export
logLik.netis_selection <- function(object, ...) .selection_no_loglik()

#' @noRd
.selection_no_fitted <- function(what) {
  stop(
    sprintf(
      "%s() is not defined for a Selection fit: an AME model fits an effect per actor, so in-sample quantities of this kind are optimistic by construction and do not mean what they mean for a regression. Use selection_effect() for probability-scale contrasts and netis_estimates() for the coefficients.",
      what
    ),
    call. = FALSE
  )
}

#' @rdname netis-selection-methods
#' @export
fitted.netis_selection <- function(object, ...) .selection_no_fitted("fitted")

#' @rdname netis-selection-methods
#' @export
residuals.netis_selection <- function(object, ...) .selection_no_fitted("residuals")

#' @rdname netis-selection-methods
#' @exportS3Method stats::sigma
sigma.netis_selection <- function(object, ...) .selection_no_fitted("sigma")

#' @rdname netis-selection-methods
#' @export
predict.netis_selection <- function(object, newdata = NULL, ...) {
  backend_predict(.new_backend("amen"), object, newdata)
}

#' Whether a Selection fit reports coefficients pooled across groups
#'
#' THE ONE PLACE this question is answered. The pooled result lives in
#' `pooled_beta`. Every accessor reads this one predicate, which is what keeps
#' the outputs of a single fit consistent -- an accessor asking some other
#' field could report a pooled fit as unpooled while `coef()` and `summary()`
#' report it as pooled.
#'
#' Pooling over one group is the identity (the K = 1 reduction), so a
#' single-group fit is never reported as pooled.
#'
#' @param x a `netis_selection` fit.
#' @return `TRUE` or `FALSE`.
#' @noRd
.selection_is_pooled <- function(x) {
  isTRUE(x$n_groups > 1L) && !is.null(x$pooled_beta)
}

#' Say plainly what was, and was not, pooled
#'
#' The single most likely way this version misleads someone is a list of
#' independent fits read as a joint result, so the sentence is explicit and
#' phrased in the terms a reader would use rather than as a field name.
#'
#' @noRd
.selection_pooling_sentence <- function(x) {
  if (!isTRUE(x$n_groups > 1L)) {
    return("One group, so there is nothing to pool across.")
  }
  if (.selection_is_hierarchical(x)) {
    return(paste(
      sprintf("%d groups estimated together in one hierarchical model.", x$n_groups),
      paste(.selection_regime_lines(x), collapse = ". "),
      "Group-specific nuisance structure (sender and receiver variances, dyadic correlation, latent factors) is never pooled.",
      sep = " "))
  }
  if (!.selection_is_pooled(x)) {
    return(sprintf(
      "%d groups fitted independently; no information was pooled across groups. Coefficients are one row per group and are not comparable as though they came from a joint model.",
      x$n_groups
    ))
  }
  paste(
    sprintf(
      "%d groups fitted independently, and their coefficients then combined by two-stage precision weighting, one coefficient at a time; every other parameter is group-specific.",
      x$n_groups
    ),
    "This is complete pooling of the coefficients -- one common value, not a distribution of group-specific ones -- and it assumes each coefficient means the same thing in every group.",
    collapse = " "
  )
}

#' @noRd
.selection_family_label <- function(family) {
  switch(
    family,
    bin = "binary tie (probit)",
    nrm = "valued tie (normal)",
    family
  )
}

#' State the scale coefficients live on
#'
#' Required because a probit coefficient is not an odds ratio, is not a
#' probability, and shifts with the modeled heterogeneity -- three ways to
#' misread it that a bare table invites.
#'
#' @noRd
.selection_scale_note <- function(family) {
  if (!identical(family, "bin")) {
    return("Coefficients are on the scale of the tie values.")
  }
  paste(
    "Coefficients are on the latent probit scale, not a probability or",
    "odds scale: exp(coefficient) is not an odds ratio for this model. Because",
    "they shift with the modeled sender, receiver, and dyadic heterogeneity,",
    "comparing them across specifications is not like comparing linear",
    "regression coefficients. Report probability-scale contrasts for magnitude."
  )
}
