# Estimation diagnostics for Influence fits.
#
# Consumes only what backend_extract() returned plus the assembled table --
# never a raw backend fit object -- holding to the same "everything downstream
# of backend_extract() stays out of the backend's own file" boundary. Returns an ordinary
# netis_diagnostics collection (R/diagnose-framework.R), the same type
# diagnose_data()/diagnose_design() return, so it composes with the rest of the
# diagnostics system rather than being a one-off structure fit_influence()
# alone understands.
#
# ONE CHECK SET PER BACKEND, not one list applied to all of them. Singularity
# means nothing for an OLS fit and R-hat means nothing for an optimizer; a
# common check list would therefore have to be the intersection, which is
# empty. Every backend gets its own branch, so none silently reports no
# estimation diagnostics -- which a user could not tell apart from a clean
# fit.
#
# What a backend does NOT report is not evidence of health. Where a check needs
# a quantity the adapter could not supply, the finding says the quantity was
# unavailable; it never resolves to silence.

#' Group counts below which mixed-model variance-component estimates are treated as
#' unstable: 5 for a random intercept and 10 for a random slope. A package convention,
#' not a published cutoff. Reported as a finding, not an ERROR: the fixed-effect point
#' estimates remain usable even when the variance components are shaky.
#'
#' @noRd
.min_groups_for_random_effects <- function(has_slope) if (has_slope) 10L else 5L

#' Estimation diagnostics, dispatched on the backend that actually fit
#'
#' @param backend_name the resolved backend identifier.
#' @param extracted the `backend_extract()` return value.
#' @param table the assembled estimating table.
#' @param group_col,random_terms,re_levels grouping information; only the
#'   `lme4` route uses them, and it is the only route with random effects.
#' @return a `netis_diagnostics` collection, possibly empty.
#' @noRd
.diagnose_influence_estimation <- function(backend_name, extracted, table, group_col,
                                           random_terms = list(), re_levels = NULL) {
  switch(
    backend_name,
    lme4 = .diagnose_estimation_lme4(extracted, table, group_col, random_terms, re_levels),
    stats = .diagnose_estimation_stats(extracted),
    lnam = .diagnose_estimation_lnam(extracted),
    # THREE BACKENDS, so three branches. An unrecognized name is a
    # programming error, not a user-facing condition, and earns no finding.
    .combine_diagnostics()
  )
}

#' Rank, residual degrees of freedom, and hat geometry for an `lm` fit
#'
#' The three facts about an ordinary least-squares fit that are true or false
#' without anyone choosing a cutoff, apart from the 0.5 hat value below, a package
#' convention. No row-deletion influence measure is computed here: in network data a
#' row is not an independent sampling unit -- a node's alters' rows enter its own
#' regressors through the exposure operator -- so deleting one row deletes part of other rows' predictors. The defensible
#' deletion analysis operates at the group/network level.
#'
#' @noRd
.diagnose_estimation_stats <- function(extracted) {
  findings <- list()

  if (length(extracted$aliased) > 0L) {
    findings <- c(findings, list(.finding(
      "WARNING", "estimation", "rank_deficient_fit",
      sprintf(
        "The design matrix is rank deficient: %s could not be estimated and %s reported as NA, because %s an exact linear combination of the other predictors. Estimates for the remaining terms are conditional on that term being absent from the model, which is not the model that was written. Drop the redundant term, or check for a constant predictor or a fully collinear pair.",
        paste(sprintf("'%s'", extracted$aliased), collapse = ", "),
        if (length(extracted$aliased) == 1L) "is" else "are",
        if (length(extracted$aliased) == 1L) "it is" else "they are"
      ),
      details = list(aliased = extracted$aliased)
    )))
  }

  df <- extracted$residual_df
  if (length(df) == 1L && !is.na(df) && df <= 0) {
    findings <- c(findings, list(.finding(
      "WARNING", "estimation", "no_residual_degrees_of_freedom",
      "The fit has no residual degrees of freedom: there are as many estimated parameters as observations. Point estimates reproduce the data exactly, the residual variance is undefined, and no standard error or confidence interval is available -- which is why they are reported as NA rather than as zero. Fit fewer terms, or supply more observations.",
      details = list(residual_df = df)
    )))
  }

  # LEVERAGE ABOVE 0.5, NOT ABOVE 2p/n. The conventional 2p/n reference is a
  # screening line, and a handful of rows sit above it in almost any design, so a
  # finding keyed there would fire on essentially every fit. A NOTE on every fit
  # trains the reader to skip the block, and then the one that matters is skipped
  # with it.
  #
  # 0.5 is a different kind of line. A hat value of 1 means the fitted surface
  # passes exactly through that observation, so above 0.5 the row is supplying
  # most of its own fitted value -- a structural statement about the design
  # geometry rather than a screening cutoff. The 2p/n count is still reported in
  # `details` for anyone who wants the screening view; it just does not raise a
  # finding on its own.
  lev <- extracted$leverage
  if (!is.null(lev) && is.finite(lev$max) && lev$max > 0.5) {
    findings <- c(findings, list(.finding(
      "NOTE", "estimation", "high_leverage_design_points",
      sprintf(
        "The highest leverage in this design is %.3f (of a possible 1). A row above 0.5 supplies most of its own fitted value, so the estimates lean heavily on that predictor combination. This describes the DESIGN, not the data quality: it is not a claim that any observation is erroneous or should be removed. rnetis reports no row-deletion influence measure, because in network data a row is not an independent sampling unit: a node's alters' rows enter its own regressors through the exposure operator.",
        lev$max
      ),
      details = lev
    )))
  }

  .combine_diagnostics(findings)
}

#' Boundary and information-matrix reporting for an `lnam` fit
#'
#' `sna::lnam` reports no convergence flag of its own, so the checks here are
#' the two structural facts that can be established from what it returns: the
#' fitted simultaneous system's invertibility, and whether an information
#' matrix was computed at all.
#'
#' @noRd
.diagnose_estimation_lnam <- function(extracted) {
  findings <- list()
  rho <- extracted$rho
  mm <- extracted$sar_operator_min_modulus

  # (I - rho1 * W1) is singular exactly when 1 - rho1 * lambda = 0 for some
  # eigenvalue lambda of W1. That is a fact about the fitted operator rather
  # than a judgement about how large rho is -- the distinction between a
  # finding and a number. (|rho1| * spectral_radius < 1 is only sufficient for
  # invertibility; it is not the test.)
  if (length(rho) == 1L && !is.na(rho) && isFALSE(extracted$sar_operator_invertible)) {
    findings <- c(findings, list(.finding(
      "WARNING", "estimation", "sar_operator_not_invertible",
      sprintf(
        "At the estimated network-autocorrelation parameter rho = %.4f, the smallest eigenvalue modulus of (I - rho*W), min |1 - rho*lambda| over the eigenvalues lambda of W, is %.4g -- numerically zero. The operator is singular, so the implied reduced form (I - rho*W)^-1 does not exist at this estimate. Treat the estimate as a boundary result rather than as a network effect of that size.",
        rho, mm
      ),
      details = list(rho = rho, min_modulus = mm,
                     spectral_radius = extracted$w1_spectral_radius)
    )))
  }

  if (!isTRUE(extracted$information_matrix_available)) {
    findings <- c(findings, list(.finding(
      "WARNING", "estimation", "information_matrix_unavailable",
      "sna::lnam() could not produce an asymptotic covariance matrix for this fit, so every standard error and confidence interval is NA rather than zero. The usual cause is that the numDeriv package -- which lnam's information-matrix step needs, and which rnetis declares in its own Suggests for that reason -- is not installed. Install numDeriv and refit if you need uncertainty for this specification.",
      details = list(information_matrix_available = FALSE)
    )))
  }

  .combine_diagnostics(findings)
}

#' The `lme4` checks
#'
#' Singularity, optimizer messages, the group count, near-zero variance components, a
#' group-level slope with no intercept, and within-group variance of a slope's
#' predictor. Three of them use package conventions: the group counts above, a
#' component SD under 1% of the residual SD, and a mean within-group variance under
#' `1e-8`.
#'
#' @noRd
.diagnose_estimation_lme4 <- function(extracted, table, group_col, random_terms, re_levels = NULL) {
  findings <- list()
  random_terms <- if (length(random_terms)) random_terms else list(list(slopes = character(0), group = group_col))
  # ANY term carrying a slope raises the group-count recommendation, not just
  # the first one. Both slope reads iterate every grouping factor, as the
  # variance-component check does: keyed to random_terms[[1]], a second term's
  # slopes would be invisible to the recommendation below and to the
  # within-group variance check at the end.
  has_slope <- any(vapply(random_terms, function(t) length(t$slopes) > 0L, logical(1)))

  if (isTRUE(extracted$convergence$singular)) {
    findings <- c(findings, list(.finding(
      "WARNING", "estimation", "singular_fit",
      "The mixed-effects fit is singular: at least one random-effects variance (or correlation) is estimated at its boundary (zero variance, or a +/-1 correlation). Fixed-effect point estimates are still reported, but standard errors and inference involving the affected variance component(s) are unreliable. Consider a simpler random-effects structure (e.g. drop the random slope) or more groups."
    )))
  }
  if (!isTRUE(extracted$convergence$converged)) {
    findings <- c(findings, list(.finding(
      "WARNING", "estimation", "convergence_messages",
      sprintf("lme4 reported convergence warnings: %s", paste(extracted$convergence$messages, collapse = "; ")),
      details = list(messages = extracted$convergence$messages)
    )))
  }

  # Counted per grouping factor as recorded on the fit, falling back to the
  # number of networks when no per-factor count was supplied.
  if (is.null(re_levels)) {
    re_levels <- stats::setNames(
      length(unique(table[[group_col]])),
      random_terms[[1]]$group
    )
  }
  min_groups <- .min_groups_for_random_effects(has_slope)
  for (i in seq_along(re_levels)) {
    n_levels <- re_levels[[i]]
    if (n_levels >= min_groups) {
      next
    }
    findings <- c(findings, list(.finding(
      if (n_levels < ceiling(min_groups / 2)) "WARNING" else "NOTE",
      "estimation", "too_few_groups_for_random_effects",
      sprintf(
        "The random-effects grouping factor '%s' has only %d levels for a %s structure; variance-component estimates are conventionally treated as unstable below %d. Fixed-effect point estimates are still usable; treat variance-component estimates and their standard errors cautiously.",
        names(re_levels)[i], n_levels,
        if (has_slope) "random-slope" else "random-intercept", min_groups
      ),
      details = list(grouping = names(re_levels)[i], n_groups = n_levels,
                     min_recommended = min_groups)
    )))
  }

  if (!is.null(extracted$var_corr)) {
    vc <- extracted$var_corr
    resid_sd <- vc$sdcor[vc$grp == "Residual"][1]
    # Every grouping factor the fit holds.
    # "Residual" is excluded because it is the comparison, not a component
    # being compared against it.
    re_var_rows <- vc[vc$grp != "Residual" & is.na(vc$var2), , drop = FALSE]
    for (i in seq_len(nrow(re_var_rows))) {
      if (!is.na(resid_sd) && resid_sd > 0 && re_var_rows$sdcor[i] < 0.01 * resid_sd) {
        findings <- c(findings, list(.finding(
          "NOTE", "estimation", "near_zero_variance_component",
          sprintf(
            "The SD for %s within grouping factor '%s' is %.4g, under 1%% of the residual SD (%.4g) -- essentially no variation was detected in this component. A simpler random-effects structure may describe this data about as well.",
            re_var_rows$var1[i], re_var_rows$grp[i], re_var_rows$sdcor[i], resid_sd
          ),
          details = list(term = re_var_rows$var1[i], grouping = re_var_rows$grp[i],
                         group_sd = re_var_rows$sdcor[i], residual_sd = resid_sd)
        )))
      }
    }
  }

  # A GROUP-LEVEL SLOPE WITH NO GROUP-LEVEL INTERCEPT. `(0 + ex | group)` lets
  # the slope vary across groups and fits ONE intercept for all of them. That
  # is a valid model whose substantive assumption is that the groups do not
  # differ in baseline outcome level. rnetis refuses to pool groups into one
  # intercept SILENTLY -- a multi-group formula with no random-effects term at all
  # -- because nothing in it says the pooling was meant. A slope-only group term
  # states the shared intercept explicitly, so it is fitted, and the assumption is
  # reported here so that it is not read as the usual random-intercept model. The
  # model is not invalid.
  n_groups_table <- length(unique(table[[group_col]]))
  at_group <- function(t) identical(gsub("\\s+", "", t$group %||% ""), group_col)
  group_terms <- Filter(at_group, random_terms)
  if (n_groups_table > 1L && length(group_terms) &&
      !any(vapply(group_terms, function(t) isTRUE(t$intercept), logical(1)))) {
    findings <- c(findings, list(.finding(
      "WARNING", "estimation", "common_intercept_across_groups",
      sprintf(
        "The random-effects term for '%s' has no intercept, so one intercept is shared by all %d groups while the slope varies across them. That assumes the groups do not differ in baseline outcome level. rnetis fits this because the formula states it explicitly; to estimate baseline differences between groups instead, include the intercept: (1 + %s | %s).",
        group_col, n_groups_table,
        paste(unique(unlist(lapply(group_terms, `[[`, "slopes"))), collapse = " + "),
        group_col
      ),
      details = list(grouping = group_col, n_groups = n_groups_table)
    )))
  }

  # EVERY random term's slopes, for the same reason.
  for (slope_var in unique(unlist(lapply(random_terms, function(t) t$slopes)))) {
    within_var <- tapply(table[[slope_var]], table[[group_col]], stats::var, na.rm = TRUE)
    mean_within_var <- mean(within_var, na.rm = TRUE)
    if (!is.na(mean_within_var) && mean_within_var < 1e-8) {
      findings <- c(findings, list(.finding(
        "WARNING", "estimation", "insufficient_within_group_exposure_variance",
        sprintf(
          "%s has near-zero within-group variance (mean %.4g across groups); its random slope is not well identified from within-group variation and the estimate may be driven mostly by between-group differences instead.",
          slope_var, mean_within_var
        ),
        details = list(term = slope_var, mean_within_group_variance = mean_within_var)
      )))
    }
  }

  .combine_diagnostics(findings)
}
