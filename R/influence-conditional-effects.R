# Conditional effects for moderated terms.
#
# WHY THIS IS ONE FILE FOR ALL THREE MODERANDS, NOT THREE. Without
# interaction terms the marginal effect of an exposure term is the coefficient
# itself. Under moderation that is false, and
# it is false in exactly the same way whether the moderated term is a covariate,
# an exposure(), or an ar() -- the marginal effect of ANY moderated term is
# `beta_term + z * beta_interaction`, a function of the moderator, not a
# constant. One computation, several callers: `.compute_influence_effects()`
# calls it for a moderated exposure() term; `.compute_ar_effects()` calls it for
# a moderated ar() term. A plain covariate's moderation is not effect-sized
# here: effect sizes are scoped to exposure(), and ar() is effect-sized only when
# moderated.
#
# THE COVARIANCE TERM IS THE WHOLE POINT. `Var(beta_term + z *
# beta_inter) = Var(beta_term) + z^2 * Var(beta_inter) + 2z * Cov(beta_term,
# beta_inter)`. Dropping the last term is the natural mistake -- it produces an
# interval that is the right order of magnitude, monotone in z, and wrong at
# every z != 0, which is exactly the failure mode that looks like a working
# implementation until checked against a hand computation.

#' The conditional effect of a moderated term, at one or more moderator values
#'
#' @param beta_term,beta_inter the moderated term's own coefficient and the
#'   interaction coefficient.
#' @param var_term,var_inter,cov_term_inter entries of the coefficient
#'   covariance matrix for the pair (`vcov[term, term]`, `vcov[inter, inter]`,
#'   `vcov[term, inter]`).
#' @param z moderator values to evaluate at.
#' @param crit critical value for the interval (already resolved per-backend
#'   by the caller -- t or normal, matching whatever `.compute_influence_effects()`
#'   uses for the unmoderated case, so a moderated and an unmoderated term on
#'   the same fit report intervals on the same basis).
#' @return a data frame: `z`, `estimate`, `se`, `ci_low`, `ci_high`.
#' @noRd
.conditional_effect <- function(beta_term, beta_inter, var_term, var_inter,
                                cov_term_inter, z, crit) {
  estimate <- beta_term + z * beta_inter
  se <- sqrt(var_term + z^2 * var_inter + 2 * z * cov_term_inter)
  data.frame(
    z = z,
    estimate = estimate,
    se = se,
    ci_low = estimate - crit * se,
    ci_high = estimate + crit * se
  )
}

#' Where a conditional effect's interval crosses zero (Johnson-Neyman)
#'
#' Solves `(beta_term + z*beta_inter)^2 = crit^2 * se(z)^2` for `z`, which is
#' the boundary between "the interval includes zero" and "the interval
#' excludes zero" -- a quadratic in `z` because `se(z)^2` is quadratic in `z`.
#' Verified against a brute-force grid scan before adoption, not derived and
#' trusted on inspection.
#'
#' RETURNS CROSSING POINTS ONLY, NEVER A VERDICT. Whether the
#' interval excludes zero INSIDE or OUTSIDE the returned boundaries is for the
#' caller to state descriptively (by checking the sign of the interval at a
#' reference point, e.g. `z = 0`), because "excludes zero for z > 2.3" is a
#' fact about an interval and "significant for z > 2.3" is the verdict this
#' package does not print.
#'
#' Applicability is the CALLER's job, not this function's: a categorical
#' moderator has no J-N region at all, and rnetis refuses every categorical
#' moderator before this is reached.
#'
#' @return a list: `boundaries` (0, 1, or 2 real z values, ascending) and
#'   `applicable` (`FALSE` when `crit` itself is not finite, e.g. a
#'   saturated fit with no residual degrees of freedom -- OR when any of the coefficients
#'   or covariance entries feeding the quadratic are not finite, e.g. a
#'   near-singular fit reporting `NaN` standard errors).
#' @noRd
.johnson_neyman_boundaries <- function(beta_term, beta_inter, var_term, var_inter,
                                       cov_term_inter, crit) {
  # A NEAR-SINGULAR OR OTHERWISE UNRELIABLE FIT CAN REPORT A FINITE crit
  # ALONGSIDE A NON-FINITE vcov ENTRY -- e.g. a static moderator with a
  # near-degenerate covariance structure can produce NaN standard errors while
  # `crit` itself (qnorm(0.975), not derived from this fit's own reliability)
  # stays perfectly finite. A `!is.finite(crit)` guard alone would let a NaN `disc` reach `if (disc < 0)`
  # below, which errors on `NA` rather than returning the same "not applicable"
  # this function already has a shape for. Guarding every input once, here,
  # keeps this a reported limitation instead of an uninformative
  # base-R crash.
  if (!is.finite(crit) || !is.finite(beta_term) || !is.finite(beta_inter) ||
      !is.finite(var_term) || !is.finite(var_inter) || !is.finite(cov_term_inter)) {
    return(list(boundaries = numeric(0), applicable = FALSE))
  }
  a <- beta_inter^2 - crit^2 * var_inter
  b <- 2 * (beta_term * beta_inter - crit^2 * cov_term_inter)
  cc <- beta_term^2 - crit^2 * var_term

  # A ~ 0: the quadratic degenerates to linear. Genuinely reachable --
  # beta_inter^2 == crit^2 * var_inter happens whenever the interaction
  # coefficient's own Wald statistic sits exactly at the critical value,
  # not just as a measure-zero coincidence to wave away.
  #
  # SCALE-FREE. Rescaling the moderator by k multiplies `a` by 1/k^2 and `b`
  # by 1/k, so a test of `a` or `b` against an absolute zero changes its
  # answer with the moderator's units (and moves the boundaries). Each is
  # compared instead with the magnitude of the two terms it is the difference
  # of, which rescale with it.
  tol <- 1e-10
  a_scale <- beta_inter^2 + crit^2 * var_inter
  b_scale <- 2 * (abs(beta_term * beta_inter) + crit^2 * abs(cov_term_inter))
  if (abs(a) <= tol * a_scale) {
    if (abs(b) <= tol * b_scale) {
      return(list(boundaries = numeric(0), applicable = TRUE))
    }
    return(list(boundaries = -cc / b, applicable = TRUE))
  }

  disc <- b^2 - 4 * a * cc
  if (disc < 0) {
    return(list(boundaries = numeric(0), applicable = TRUE))
  }
  # The cancellation-free form of the quadratic formula: the root computed as
  # (-b -/+ sqrt(disc)) / 2a loses its digits when b^2 dominates 4ac, which a
  # small `a` makes common.
  q <- -0.5 * (b + if (b >= 0) sqrt(disc) else -sqrt(disc))
  roots <- if (q == 0) c(0, 0) else c(q / a, cc / q)
  list(boundaries = sort(roots), applicable = TRUE)
}

#' Conditional stability effects for moderated ar() terms
#'
#' Mirrors `.compute_influence_effects()`'s exposure treatment, for the one
#' other moderand the framing extends to: a moderated
#' `ar()` term's conditional stability `gamma + theta * z`. Unmoderated `ar()`
#' terms get no entry here: `ar()` terms and plain covariates are not effect-sized,
#' except for a moderated `ar()` term.
#'
#' @param ar_terms `parsed$ar`.
#' @param interactions `parsed$interactions`.
#' @param extracted the backend's extracted fit (coefficients already
#'   backtick-stripped by the caller).
#' @param table the estimating sample, for the moderator's observed values.
#' @param crit critical value, computed once by the caller and shared with
#'   `.compute_influence_effects()` so the two report intervals on the same
#'   basis.
#' @return a named list, one entry per moderated `ar()` term (usually zero or
#'   one; multiple only if a formula legitimately carries several `ar()`
#'   terms at different lags and moderates more than one).
#' @noRd
.compute_ar_effects <- function(ar_terms, interactions, extracted, table, crit,
                                probs = c(0.1, 0.9)) {
  if (length(ar_terms) == 0L || length(interactions) == 0L) {
    return(list())
  }
  coefs <- extracted$coefficients
  vc <- extracted$vcov

  out <- list()
  for (ar_term in ar_terms) {
    ar_col <- paste0(ar_term$variable, "_lag", ar_term$lag)
    inter <- Filter(function(i) {
      identical(i$moderand_kind, "ar") &&
        identical(.interaction_moderand_colname(i), ar_col)
    }, interactions)
    if (length(inter) == 0L) {
      next
    }
    # At most one interaction is permitted per (moderand, moderator) pair, via the hierarchy/pairing checks in .resolve_influence_interactions(),
    # so this is never ambiguous.
    inter <- inter[[1]]
    prod_col <- .interaction_product_colname(inter)
    if (!(ar_col %in% names(coefs)) || !(prod_col %in% names(coefs))) {
      next
    }
    # z = 0 first, same convention as .compute_influence_effects() -- the
    # AT-ZERO value is needed directly in `conditional`, not just in the
    # separate `coefficient_at_z0` scalar below.
    z_vals <- c(0, stats::quantile(table[[inter$moderator]], probs = probs,
                                   na.rm = TRUE, names = FALSE))
    beta_term <- unname(coefs[ar_col])
    beta_inter <- unname(coefs[prod_col])
    var_term <- vc[ar_col, ar_col]
    var_inter <- vc[prod_col, prod_col]
    cov_ti <- vc[ar_col, prod_col]

    cond <- .conditional_effect(beta_term, beta_inter, var_term, var_inter, cov_ti, z_vals, crit)
    jn <- .johnson_neyman_boundaries(beta_term, beta_inter, var_term, var_inter, cov_ti, crit)

    out[[ar_col]] <- list(
      term = ar_col,
      moderator = inter$moderator,
      # The AR coefficient AS REPORTED (coef(fit)[ar_col]) already IS
      # the conditional effect at z = 0 -- gamma + theta*0 = gamma, exactly,
      # by construction of the fitted model. Recorded explicitly rather than
      # left for a reader to work out: rnetis STATES
      # this, rather than merely having it be arithmetically true.
      coefficient_at_z0 = beta_term,
      conditional = cond,
      jn_boundaries = jn$boundaries,
      jn_applicable = jn$applicable
    )
  }
  out
}

#' Print a conditional-effect table inside `print.netis_influence_effects()`
#'
#' Shared by the exposure and ar() blocks so the two report in
#' an identical shape -- a reader comparing a moderated exposure() term to a
#' moderated ar() term should not have to learn two table layouts for the
#' same underlying computation.
#'
#' @param cond the `conditional` data frame (`z`, `estimate`, `se`, `ci_low`,
#'   `ci_high`, and `predicted_change` when `predicted_change` is `TRUE`).
#' @param moderator the moderator's column name, for the row labels.
#' @param predicted_change whether to print the predicted-change column
#' (exposure terms only -- the framing has no ar() counterpart).
#' @return invisible `NULL`, called for its printed side effect.
#' @noRd
.print_conditional_table <- function(cond, moderator, predicted_change) {
  for (i in seq_len(nrow(cond))) {
    label <- if (isTRUE(all.equal(cond$z[i], 0))) {
      sprintf("%s = 0 (reference)", moderator)
    } else {
      sprintf("%s = %s", moderator, format(cond$z[i], digits = 4))
    }
    if (predicted_change) {
      cat(sprintf(
        "      %s: coef %.4f (95%% CI [%.4f, %.4f]), predicted change %.4f\n",
        label, cond$estimate[i], cond$ci_low[i], cond$ci_high[i], cond$predicted_change[i]
      ))
    } else {
      cat(sprintf(
        "      %s: coef %.4f (95%% CI [%.4f, %.4f])\n",
        label, cond$estimate[i], cond$ci_low[i], cond$ci_high[i]
      ))
    }
  }
  invisible(NULL)
}

#' Describe a Johnson-Neyman region in words, without a significance verdict
#'
#' "Excludes zero" is a fact about an interval; "significant"
#' is a verdict this package does not print. Kept deliberately simple rather
#' than attempting to fully characterise which side of each boundary excludes
#' zero in general English -- the z = 0 row already printed by
#' `.print_conditional_table()` gives the reader one concrete, unambiguous data
#' point, and the boundaries are reported as plain crossing points alongside
#' it.
#'
#' @param eff one moderated entry of `influence_effect()`'s `exposures` or `ar`,
#'   containing `jn_applicable`, `jn_boundaries`, and `conditional` (whose first
#'   row is always `z = 0`).
#' @return a single string.
#' @noRd
.describe_jn_region <- function(eff) {
  if (!isTRUE(eff$jn_applicable)) {
    return(paste(
      "Johnson-Neyman boundary not available (either this fit has no residual degrees of",
      "freedom to base an interval on, or a coefficient/covariance entry it would need is not",
      "finite -- often a sign of a near-singular or unreliable fit; check the fit's",
      "convergence/reliability diagnostics)."
    ))
  }
  b <- eff$jn_boundaries
  # isTRUE() rather than a bare ||: `.johnson_neyman_boundaries()` guards ITS
  # OWN inputs, but `.conditional_effect()`'s `se` (a different computation
  # from the same coefficients/vcov) is not guarded the same way, so a
  # comparison against a NaN ci_low/ci_high here would otherwise crash
  # `if(NA)` one line down.
  at_zero_excludes <- isTRUE(eff$conditional$ci_low[1] > 0) || isTRUE(eff$conditional$ci_high[1] < 0)
  at_zero_state <- if (at_zero_excludes) "excludes" else "includes"
  if (length(b) == 0L) {
    return(sprintf(
      "The interval %s zero at every value of the moderator (no Johnson-Neyman crossing).",
      at_zero_state
    ))
  }
  sprintf(
    "At the moderator's value of 0 the interval %s zero; the interval's relationship to zero changes at %s = %s.",
    at_zero_state, eff$moderator, paste(format(b, digits = 4), collapse = " and ")
  )
}

#' The rows of an Influence fit's assembled table the backend estimated from
#'
#' Those complete on the outcome and every predictor. Every Influence backend
#' (`lm()`, `lmer()`, `lnam()`) drops incomplete rows listwise, so this is the
#' row set the coefficients come from.
#'
#' @noRd
.influence_estimating_sample <- function(fit) {
  mf <- fit$model_frame
  used <- setdiff(names(mf), fit$spec$data_schema$node_col)
  mf[stats::complete.cases(mf[used]), , drop = FALSE]
}

#' Effect sizes for the exposure terms of an Influence fit
#'
#' One entry per `exposure()` term. `ar()` terms and
#' plain covariates are not effect-sized here: the predicted-change framing
#' is specific to exposure (a moderated ar() term gets the
#' parallel treatment from `.compute_ar_effects()` instead).
#'
#' AN UNMODERATED TERM HAS A SCALAR SHAPE (`coefficient`, `ci_low`, `ci_high`,
#' `predicted_change` as single numbers): on a least-squares or mixed fit the model
#' is linear with no interaction term touching it, so the marginal effect is the
#' coefficient itself regardless of where other predictors sit. A simultaneous fit
#' does not have that property, and `.sar_coefficient_scale()` relabels its entries.
#' A MODERATED term (`interactions`) gets a `$conditional`
#' data frame instead -- the scalar fields would be a wrong number
#' for it, not a simplification: `.johnson_neyman_boundaries()`'s whole reason
#' to exist is that a single "the effect" stops meaning anything once it is a
#' function of `z`.
#'
#' When an exposure() term also carries a random slope (`random_terms`,
#' `group_col`), its entry also reports heterogeneity across groups: the between-
#' group SD of the slope (from backend_extract()'s `var_corr`) and the range
#' of group-specific slope estimates (the fixed coefficient plus each
#' group's own deviation, from backend_extract()'s `random_effects` --
#' i.e. lme4's BLUPs). Both come from what backend_extract() already
#' returned; nothing here touches a raw backend fit object. Untouched by
#' moderation: a random slope and a moderator are neighboring but distinct
#' things, and are never conflated.
#'
#' @param table the estimating sample: the rows the fit used, from
#' `.influence_estimating_sample()`. Quantiles over the assembled table would
#' describe rows the fit dropped.
#' @param interactions `parsed$interactions`; `list()` for a fit with no
#' moderated terms, which gives the unmoderated scalar shape exactly.
#' @noRd
.compute_influence_effects <- function(exposure_terms, extracted, table, random_terms = NULL,
                                       group_col = NULL, interactions = list(),
                                       probs = c(0.1, 0.9)) {
  if (length(exposure_terms) == 0L) {
    return(list())
  }
  coefs <- extracted$coefficients
  se <- sqrt(diag(extracted$vcov))
  crit <- .effects_critical_value(extracted)

  # EVERY random term is searched for the exposure column, not only the first,
  # so the result does not depend on the order the terms were written in.
  random_slopes <- unique(unlist(lapply(random_terms %||% list(), `[[`, "slopes")))

  out <- list()
  for (exp_term in exposure_terms) {
    nm <- exp_term$name
    if (!(nm %in% names(coefs)) || !(nm %in% names(table))) {
      next
    }
    beta <- unname(coefs[nm])
    beta_se <- unname(se[nm])
    q <- stats::quantile(table[[nm]], probs = probs, na.rm = TRUE, names = FALSE)

    inter <- Filter(function(i) {
      identical(i$moderand_kind, "exposure") && identical(.interaction_moderand_colname(i), nm)
    }, interactions)

    if (length(inter) == 0L) {
      # The coefficient's own interval, on the coefficient's own degrees of
      # freedom: identical to confint() for this term.
      crit_coef <- .effects_coef_critical(extracted, nm)
      entry <- list(
        term = nm,
        moderated = FALSE,
        coefficient = beta,
        ci_low = beta - crit_coef * beta_se,
        ci_high = beta + crit_coef * beta_se,
        # THE CONTRAST, AND THE CONVENTION THAT CHOSE ITS ENDPOINTS. The
        # quantity is an ordinary model contrast -- yhat at `contrast_high`
        # minus yhat at `contrast_low`, which for a linear term is
        # beta * (high - low). What is rnetis's is the RANGE: the observed
        # 10th-to-90th percentile by default. That is a reporting convention,
        # not part of any effect-size definition, so it is named on the
        # result and settable rather than baked in.
        contrast_low = q[1],
        contrast_high = q[2],
        contrast_probs = probs,
        contrast_rule = sprintf(
          "observed %g-to-%g percentile of '%s' in the estimating sample",
          100 * probs[1], 100 * probs[2], nm
        ),
        scale = "outcome",
        uncertainty = "Wald interval on the coefficient; the contrast range is treated as fixed",
        predicted_change = beta * (q[2] - q[1])
      )
    } else {
      inter <- inter[[1]]
      prod_col <- .interaction_product_colname(inter)
      beta_inter <- unname(coefs[prod_col])
      # z = 0 is always the first reported value, alongside the moderator's
      # observed 10th/90th percentile (matching the exposure contrast's
      # own data-driven convention rather than an arbitrary +-1 SD): the
      # AT-ZERO value must be directly accessible without a second lookup,
      # since it is the value a bare coefficient reads as if unmoderated.
      z_vals <- c(0, stats::quantile(table[[inter$moderator]], probs = probs, na.rm = TRUE, names = FALSE))
      vc <- extracted$vcov
      cond <- .conditional_effect(
        beta, beta_inter, vc[nm, nm], vc[prod_col, prod_col], vc[nm, prod_col], z_vals, crit
      )
      jn <- .johnson_neyman_boundaries(
        beta, beta_inter, vc[nm, nm], vc[prod_col, prod_col], vc[nm, prod_col], crit
      )
      entry <- list(
        term = nm,
        moderated = TRUE,
        moderator = inter$moderator,
        contrast_low = q[1],
        contrast_high = q[2],
        # The predicted-change framing, evaluated AT each reported z --
        # the same coefficient * (high - low) arithmetic, applied to the conditional coefficient rather than a constant
        # one, so it stays comparable to the unmoderated case's number.
        conditional = cbind(cond, predicted_change = cond$estimate * (q[2] - q[1])),
        jn_boundaries = jn$boundaries,
        jn_applicable = jn$applicable
      )
    }

    if (nm %in% random_slopes) {
      # The GROUP-level variance of the slope: lme4 labels it by the grouping
      # factor, so a slope that varies only at another level (node) has no
      # group row and reports NA rather than borrowing that level's variance.
      vc <- extracted$var_corr
      vc_row <- if (is.null(vc) || is.null(group_col)) NULL else {
        vc[vc$grp == group_col & vc$var1 == nm & is.na(vc$var2), , drop = FALSE]
      }
      entry$group_sd <- if (isTRUE(nrow(vc_row) == 1L)) vc_row$sdcor[1] else NA_real_
      group_ranef <- if (is.null(group_col)) NULL else extracted$random_effects[[group_col]][[nm]]
      entry$group_range <- if (!is.null(group_ranef)) beta + range(group_ranef) else c(NA_real_, NA_real_)
    }

    out[[nm]] <- entry
  }
  out
}

#' Relabel effect entries from a simultaneous fit as coefficient-scale quantities
#'
#' The arithmetic is the same, coefficient times contrast. What differs is what it
#' means: without the reduced form it is not a change in the outcome, so the entry
#' does not carry a field called one.
#'
#' @param entries the `exposures` list from `.compute_influence_effects()`.
#' @return the entries, with `predicted_change` renamed and `scale` restated.
#' @noRd
.sar_coefficient_scale <- function(entries) {
  lapply(entries, function(e) {
    e$coefficient_times_contrast <- e$predicted_change
    e$predicted_change <- NULL
    e$scale <- "coefficient"
    e
  })
}

#' Effect sizes for a fitted Influence model
#'
#' Returns, for each [exposure()] term, its coefficient with a Wald interval
#' and the predicted change in the outcome across that exposure's own observed
#' 10th-to-90th-percentile range -- a magnitude on the outcome's scale rather
#' than a coefficient a reader has to convert. For a *moderated* term the entry
#' carries a conditional-effect table instead, and for a moderated [ar()] term
#' the same shape is returned under `ar`.
#'
#' A moderated term's conditional table and Johnson-Neyman boundaries use the fit's
#' 95% critical value for a linear combination of coefficients: the `t` quantile
#' on the residual degrees of freedom for least squares, and the normal quantile
#' for `lme4` and `sna::lnam` fits. On an `lme4` fit that differs from the
#' coefficient table, whose intervals use Satterthwaite's degrees of freedom per
#' coefficient: a conditional effect is a combination of two coefficients, whose
#' Satterthwaite degrees of freedom change with the moderator value, and the
#' Johnson-Neyman solution is defined for one critical value. `probs` sets the
#' moderator values reported as well as the contrast.
#'
#' @section Computed on request, not at fit time:
#' [fit_influence()] returns estimates, and this reads a completed fit. A fitted
#' object carries what was estimated, and quantities derived to answer a further
#' question are computed when that question is asked. An effect size is a derived
#' magnitude: it picks a range, evaluates a change over it, and reports that as the
#' practical size of the effect.
#'
#' Its Selection counterpart is [selection_effect()].
#'
#' A mechanism with several columns (such as [bi_components()]) has no single
#' column to give a contrast, so its term has no entry here.
#'
#' @section What the interval covers:
#' A Wald interval built from the fit's own `coef()` and `vcov()`, with the
#' same critical value as [confint()][netis-influence-methods]: a `t` quantile
#' on the residual degrees of freedom for an ordinary least squares fit, a `t`
#' quantile on the coefficient's Satterthwaite degrees of freedom for an `lme4`
#' fit, and a normal quantile for an `sna::lnam` fit. It covers uncertainty in the
#' COEFFICIENTS. The percentile range the change is evaluated over is taken
#' from the estimating sample and treated as fixed, so the interval does not
#' carry uncertainty about the range itself.
#'
#' @section On a simultaneous (`sna::lnam`) fit:
#' There the entry holds `coefficient_times_contrast`, in place of
#' `predicted_change`: the coefficient times the contrast range, a **coefficient-scale
#' quantity that excludes feedback**. It is not the change in the outcome from changing
#' the predictor, for any actor or on average. In `y = rho * W1 y + X b + e` a
#' predictor reaches the outcome through `(I - rho * W1)^-1`, so a change in one actor's
#' predictor moves every actor connected to it in `W1`, and comes back. That quantity
#' depends on `rho`, on `W1` and on which actors change, and rnetis does not compute it.
#' The result says so (`excludes_network_feedback`, `scale`, `scope_note`), and so do
#' its printed form and [plot()][plot.netis_influence]. The same-outcome term itself has
#' no entry: its coefficient is `rho`, not a slope over a contrast.
#'
#' @section The range is a convention, and it is named:
#' The contrast runs between two values of the predictor, and rnetis picks them
#' by default: the observed 10th and 90th percentiles in the estimating
#' sample. That is a **reporting convention**, this package's own, and not part of
#' any effect-size definition, so it is settable through `probs` and reported back
#' on the result: `contrast_low` and `contrast_high` on each entry, `contrast_probs`
#' and `contrast_rule` on an unmoderated entry, and `contrast_probs`, `scale` and
#' `uncertainty` on the whole.
#'
#' @param object a fitted model from [fit_influence()].
#' @param probs two increasing quantiles in `[0, 1]` giving the predictor
#'   values the contrast runs between. Defaults to `c(0.1, 0.9)` -- the
#'   observed 10th-to-90th percentile. Changing it changes the reported
#'   magnitude and nothing else; the coefficient is unaffected.
#' @param ... unused; present for S3 consistency.
#' @return an object of class `netis_influence_effects`, with a `print()` method: a
#'   list of `exposures`, `ar`, and the `contrast_probs`, `scale` and `uncertainty`
#'   that describe what the numbers are. On a simultaneous fit also
#'   `excludes_network_feedback` (`TRUE`) and `scope_note`, the sentence saying so, and
#'   there `scale` is `"coefficient"` rather than `"outcome"`.
#'
#'   `exposures` has one entry per [exposure()] term. An unmoderated entry holds
#'   `term`, `moderated` (`FALSE`), `coefficient`, `ci_low`, `ci_high`, `contrast_low`,
#'   `contrast_high`, `contrast_probs`, `contrast_rule`, `scale`, `uncertainty` and
#'   `predicted_change` (`coefficient_times_contrast` on a simultaneous fit). A moderated
#'   entry holds `term`, `moderated` (`TRUE`),
#'   `moderator`, `contrast_low`, `contrast_high`, `conditional` (a data frame of `z`,
#'   `estimate`, `se`, `ci_low`, `ci_high` and `predicted_change`, whose first row is
#'   `z = 0`), `jn_boundaries` (zero, one or two moderator values where the interval's
#'   relationship to zero changes) and `jn_applicable`. A term with a random slope
#'   adds `group_sd` and `group_range`.
#'
#'   `ar` has one entry per moderated [ar()] term, and is empty when none is
#'   moderated: `term`, `moderator`, `coefficient_at_z0`, `conditional` (without
#'   `predicted_change`), `jn_boundaries` and `jn_applicable`.
#' @references
#' Johnson, P. O. and Neyman, J. (1936) Tests of certain linear hypotheses and
#' their application to some educational problems. *Statistical Research Memoirs*
#' **1**, 57-93.
#'
#' Bauer, D. J. and Curran, P. J. (2005) Probing interactions in fixed and
#' multilevel regression: inferential and graphical techniques. *Multivariate
#' Behavioral Research* **40**(3), 373-400.
#' @seealso [fit_influence()], [selection_effect()], [netis_estimates()]
#' @examples
#' set.seed(1)
#' ids <- letters[1:10]
#' nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = 10),
#'                     y = stats::rnorm(20), x = stats::rnorm(20))
#' ties <- data.frame(group = 1, source = ids[-10], target = ids[-1],
#'                    wave = 1, tie = 1)
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
#' influence_effect(fit)
#' @export
influence_effect <- function(object, probs = c(0.1, 0.9), ...) {
  if (!inherits(object, "netis_influence")) {
    stop(
      "influence_effect() is defined for netis_influence fits. For a Selection fit, use selection_effect().",
      call. = FALSE
    )
  }
  spec <- object$spec
  # The random-effects fields travel too: a random exposure slope reports its
  # between-group SD and range from them, and without them that branch indexed
  # NULL and stopped with "argument is of length zero".
  extracted <- list(
    coefficients = object$coefficients,
    vcov = object$vcov,
    # The INFERENCE degrees of freedom, so the effect intervals use the same
    # critical value as confint(): t for least squares, normal otherwise.
    residual_df = .influence_inference_df(object$backend, object$residual_df),
    # Per-coefficient Satterthwaite degrees of freedom on the lme4 route, for
    # the unmoderated entries, which report a coefficient's own interval.
    coef_tests = object$coef_tests,
    var_corr = object$var_corr,
    random_effects = object$random_effects
  )
  if (!is.numeric(probs) || length(probs) != 2L || anyNA(probs) ||
      any(probs < 0) || any(probs > 1) || probs[1] >= probs[2]) {
    stop(
      "probs must be two increasing values in [0, 1]: the quantiles of the predictor that the reported contrast runs between.",
      call. = FALSE
    )
  }
  estimating_sample <- .influence_estimating_sample(object)
  simultaneous <- identical(object$backend, "lnam")
  exposures <- .compute_influence_effects(
    spec$exposures, extracted, estimating_sample,
    random_terms = spec$random_effects,
    group_col = spec$data_schema$group_col,
    interactions = spec$interactions %||% list(),
    probs = probs
  )
  if (simultaneous) {
    exposures <- .sar_coefficient_scale(exposures)
  }
  structure(
    c(list(
      exposures = exposures,
      ar = .compute_ar_effects(
        spec$ar, spec$interactions %||% list(), extracted, estimating_sample,
        .effects_critical_value(extracted), probs = probs
      ),
      # WHAT THIS OBJECT IS, stated rather than inferred from the numbers. The
      # contrast is an ordinary model contrast; the RANGE it runs between is a
      # reporting convention rnetis chose, and naming it here is what stops a
      # default becoming an unnamed effect-size definition.
      contrast_probs = probs,
      scale = if (simultaneous) "coefficient" else "outcome",
      uncertainty = "Wald interval on the coefficients; the contrast range is treated as fixed"
    ), if (simultaneous) list(
      excludes_network_feedback = TRUE,
      scope_note = "Each value is the coefficient times the contrast, a coefficient-scale quantity. It excludes the network feedback through (I - rho * W1)^-1, by which a change in one actor's predictor reaches every actor connected to it in W1, so it is not the change in the outcome from changing the predictor. rnetis does not compute that change."
    )),
    class = "netis_influence_effects"
  )
}

#' @rdname influence_effect
#' @param x a `netis_influence_effects` object.
#' @export
print.netis_influence_effects <- function(x, ...) {
  simultaneous <- isTRUE(x$excludes_network_feedback)
  if (simultaneous) {
    cat("Influence coefficient-scale contrasts (excluding network feedback)\n")
    cat(sprintf("  Coefficient times the observed %g-to-%g percentile range of each exposure.\n",
                100 * x$contrast_probs[1], 100 * x$contrast_probs[2]))
  } else {
    cat("Influence effect sizes\n")
    cat(sprintf("  Predicted change across the observed %g-to-%g percentile of each exposure, on the outcome's scale.\n",
                100 * x$contrast_probs[1], 100 * x$contrast_probs[2]))
  }
  if (length(x$exposures) == 0L) {
    cat("  No exposure() term on this fit has an entry.\n")
  }
  for (e in x$exposures) {
    cat(sprintf("\n  %s (contrast %s to %s)\n", e$term, format(e$contrast_low, digits = 4),
                format(e$contrast_high, digits = 4)))
    if (isTRUE(e$moderated)) {
      .print_conditional_table(e$conditional, e$moderator, predicted_change = TRUE)
      cat(sprintf("      %s\n", .describe_jn_region(e)))
    } else if (simultaneous) {
      cat(sprintf("      coef %.4f (95%% CI [%.4f, %.4f]), coefficient times contrast %.4f\n",
                  e$coefficient, e$ci_low, e$ci_high, e$coefficient_times_contrast))
    } else {
      cat(sprintf("      coef %.4f (95%% CI [%.4f, %.4f]), predicted change %.4f\n",
                  e$coefficient, e$ci_low, e$ci_high, e$predicted_change))
    }
    if (!is.null(e$group_sd)) {
      cat(sprintf("      between-group SD of the slope %.4f; group slopes range %.4f to %.4f\n",
                  e$group_sd, e$group_range[1], e$group_range[2]))
    }
  }
  for (a in x$ar) {
    cat(sprintf("\n  %s, moderated by %s (conditional stability)\n", a$term, a$moderator))
    .print_conditional_table(a$conditional, a$moderator, predicted_change = FALSE)
    cat(sprintf("      %s\n", .describe_jn_region(a)))
  }
  if (!is.null(x$scope_note)) {
    cat(sprintf("\n  %s\n", x$scope_note))
  }
  invisible(x)
}
