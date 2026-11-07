# ICC(1) and ICC(2) as Bliese (2000) defines them: the one-way random-effects
# ANOVA quantities of the multilevel-aggregation literature.
#
# Two different variance partitions are reported by this package and they must
# never share a label:
#
#   icc1()/icc2()                      UNCONDITIONAL, from a one-way ANOVA of
#                                      the outcome on the grouping factor
#   netis_variance_components()        CONDITIONAL, from the fitted model
#
# The first asks "how clustered is the outcome before the substantive
# predictors explain anything"; the second asks "where does the variance the
# model did NOT explain sit". A large ICC(1) beside a small conditional share
# says the predictors absorbed much of the between-group heterogeneity, and
# collapsing the two into one number destroys exactly that comparison.

#' Resolve which grouping factor an ICC is being asked about
#'
#' Never guessed when there is a choice. A fit with one random-effects
#' grouping factor resolves to it; a fit with several requires `group =`.
#'
#' @noRd
.resolve_icc_group <- function(fit, group) {
  re <- fit$spec$random_effects
  if (is.null(re)) re <- list()
  candidates <- unique(vapply(re, function(r) r$group, character(1)))

  if (is.null(group)) {
    if (length(candidates) == 1L) {
      return(candidates)
    }
    if (length(candidates) == 0L) {
      stop(
        "This fit has no random-effects grouping factor, so there is no aggregation unit for an ICC. Name the grouping column explicitly with group = , or fit a model with a grouping factor.",
        call. = FALSE
      )
    }
    stop(
      sprintf(
        "This fit has more than one grouping factor (%s), so rnetis will not guess which one the ICC is about. Name it: group = \"%s\".",
        paste(candidates, collapse = ", "), candidates[1]
      ),
      call. = FALSE
    )
  }

  if (!is.character(group) || length(group) != 1L || is.na(group)) {
    stop("group must be a single grouping-column name.", call. = FALSE)
  }
  # RESTRICTED TO THE FIT'S OWN RANDOM-EFFECTS GROUPING FACTORS, not to any
  # column of the estimating sample. Two reasons, and the second is the
  # substantive one:
  #
  #  - a declared grouping factor is one the user has already asserted is a
  #    meaningful level of clustering, which is what an aggregation unit is;
  #  - an arbitrary column need not identify a unit at all. `node` is the case
  #    that makes this concrete: node labels in this package are unique only
  #    WITHIN group, so `table(node)` on multi-group data counts the same label
  #    across different groups as one level and a null model keyed on it would
  #    silently pool distinct people.
  if (!group %in% candidates) {
    stop(
      sprintf(
        "group = \"%s\" is not one of this fit's random-effects grouping factors (%s). icc1()/icc2() are defined for a grouping factor the model declared, not for any column of the estimating sample -- an undeclared column need not identify an aggregation unit at all.",
        group,
        if (length(candidates) == 0L) "none" else paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  group
}

#' Refuse the cases where an ANOVA ICC is not the quantity it appears to be
#'
#' Each refusal has its own reason and names it.
#'
#' @noRd
.assert_icc_family <- function(fit) {
  # Runs BEFORE grouping resolution: a Selection fit has no grouping factor in
  # the sense this file means, so resolving one first would report the wrong
  # problem ("no grouping factor") instead of the real one ("wrong family").
  if (!inherits(fit, "netis_influence")) {
    stop(
      "icc1()/icc2() are defined for Influence fits only. A Selection fit's outcome is a tie and its grouping factor is a network, so 'reliability of the group mean' has no aggregation-justification reading there.",
      call. = FALSE
    )
  }

  fam <- fit$spec$outcome_family
  if (is.null(fam)) fam <- "gaussian"
  if (!identical(fam, "gaussian")) {
    stop(
      sprintf(
        "icc1()/icc2() are defined for a Gaussian outcome; this fit's outcome family is '%s'. For a non-Gaussian outcome the observation-level variance is not freely estimated and the ICC formula is not this one.",
        fam
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' The estimating sample an ICC describes
#'
#' The rows of the assembled table that the fit actually estimated from: those
#' complete on the outcome, the grouping column and every model predictor.
#' Every Influence backend (`lm()`, `lmer()`, `lnam()`) drops incomplete rows
#' listwise, so this is the same row set the coefficients come from. Reading
#' the assembled table instead would count rows the backend never saw, which
#' changes both the ANOVA mean squares and every group's `n_j`.
#'
#' @noRd
.icc_estimating_sample <- function(fit, group) {
  mf <- fit$model_frame
  s <- fit$spec$data_schema
  used <- setdiff(names(mf), s$node_col)
  used <- union(c(".outcome", group), used)
  used <- intersect(used, names(mf))
  keep <- stats::complete.cases(mf[used])
  mf[keep, , drop = FALSE]
}

#' @noRd
.assert_icc_supported <- function(fit, group) {
  # A grouping factor whose every level holds one row of the estimating sample
  # has no group mean to characterise and its variance is not separately
  # identified from the residual.
  sizes <- table(.icc_estimating_sample(fit, group)[[group]])
  if (all(sizes <= 1L)) {
    stop(
      sprintf(
        "Every level of '%s' contributes at most one row to the estimating sample, so its variance is not separately identified from the residual and there is no group mean to characterise.",
        group
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' One-way random-effects ANOVA of the outcome on the grouping factor
#'
#' Returns the mean squares, the group sizes and the unbalanced-design
#' effective group size `n0`, or a `failed` record naming why the ANOVA is not
#' defined on this sample.
#'
#' @noRd
.icc_anova <- function(y, g) {
  g <- factor(g)
  g <- droplevels(g)
  n_j <- as.numeric(table(g))
  names(n_j) <- levels(g)
  k <- length(n_j)
  N <- sum(n_j)
  out <- list(n_j = n_j, k = k, N = N, msb = NA_real_, msw = NA_real_,
              n0 = NA_real_, failed = FALSE, message = NULL)
  if (k < 2L) {
    out$failed <- TRUE
    out$message <- "The estimating sample has fewer than two groups, so there is no between-group mean square."
    return(out)
  }
  if (N - k < 1L) {
    out$failed <- TRUE
    out$message <- "Every group contributes one row to the estimating sample, so there is no within-group mean square and the group variance is not separately identified from the residual."
    return(out)
  }
  means <- tapply(y, g, mean)
  grand <- mean(y)
  out$msb <- sum(n_j * (means - grand)^2) / (k - 1)
  out$msw <- sum((y - means[as.integer(g)])^2) / (N - k)
  out$n0 <- (N - sum(n_j^2) / N) / (k - 1)
  if (!is.finite(out$msw) || out$msw <= 0 || !is.finite(out$msb)) {
    out$failed <- TRUE
    out$message <- "The within-group mean square is zero or not finite, so the ICC is not defined."
  }
  out
}

#' Intraclass correlation ICC(1): unconditional clustering by group
#'
#' The proportion of outcome variance associated with group membership
#' **before** conditioning on the substantive model's predictors: Bliese's
#' (2000) ICC(1), computed from a one-way random-effects ANOVA of the outcome
#' on the grouping factor over this fit's own estimating sample,
#'
#' `ICC(1) = (MSB - MSW) / (MSB + (n0 - 1) * MSW)`,
#'
#' where `MSB` and `MSW` are the between- and within-group mean squares and
#' `n0 = (N - sum(n_j^2) / N) / (k - 1)` is the effective group size of the
#' unbalanced one-way ANOVA (`N` rows in `k` groups of size `n_j`). With equal
#' group sizes `n0` is the common group size and this is Bliese's formula
#' exactly; under unequal sizes `n0` is the standard unbalanced-design
#' replacement for it. Some software evaluates the formula at the arithmetic
#' mean group size `N / k` instead, which gives a different value under
#' imbalance.
#'
#' # Which rows
#'
#' The estimating sample is the set of rows the fit itself estimated from:
#' rows complete on the outcome, the grouping column and every model
#' predictor. Rows the backend dropped for a missing outcome, a missing lag or
#' a missing exposure are not counted, so `n_obs` equals the fit's own number
#' of observations and a group that lost rows contributes its reduced size.
#'
#' # Which ICC this is
#'
#' This is the ANOVA ICC(1) of the multilevel-aggregation literature,
#' numerically the same quantity as Shrout & Fleiss's (1979) `ICC(1,1)`. It
#' is **not** the psychometric inter-rater `ICC(2)`/`ICC(3)` family that
#' `psych::ICC()` reports, which requires a crossed design (the same raters
#' rating every target) that nested groups do not have. Nor is it a
#' restricted-maximum-likelihood variance share from a fitted
#' `y ~ 1 + (1 | group)` model: the two agree on balanced data and differ
#' under imbalance.
#'
#' It is also **not** the same as [netis_variance_components()], which is a
#' *conditional* share computed from the fitted model. The two answer
#' different questions and can differ substantially; a large ICC(1) beside a
#' small conditional share says the predictors account for much of the
#' original between-group heterogeneity.
#'
#' # Negative values
#'
#' When `MSB <= MSW` the ANOVA estimate is zero or negative. It is returned as
#' computed, with `status = "boundary"`: the sample shows no evidence of
#' between-group variance, and replacing the estimate by zero would change
#' the quantity Bliese defines.
#'
#' # What it is evidence for
#'
#' ICC(1) is evidence relevant to whether aggregating to the group level is
#' defensible. It does not settle it: theory, construct definition, and
#' within-group agreement (see [rwg()]) all bear on that decision, and a high
#' agreement index can coexist with low between-group differentiation.
#'
#' @param fit a `netis_influence` fit.
#' @param group the grouping factor: the fit's random-effects grouping factor, which
#'   is also the default. Any other column is refused, as is a fit with no
#'   random-effects term, because an undeclared column need not identify an aggregation
#'   unit.
#' @return a `netis_icc1` object: `value`, `status` (`"ok"`, `"boundary"` or
#'   `"failed"`), `reason`, `group`, `n_groups` and `n_obs` (groups and rows in
#'   the estimating sample), `n0` (the effective group size used), `anova` (the
#'   mean squares `msb` and `msw`), `components` (the ANOVA estimates of
#'   the between-group variance, `(MSB - MSW) / n0`, and the within-group
#'   variance, `MSW`), and `n_j` (the group sizes). `value` is the ANOVA estimate, zero or negative with
#'   `status = "boundary"` when `MSB <= MSW`, and `NA` with
#'   `status = "failed"` only when the ANOVA is not defined (fewer than two
#'   groups, or no within-group replication).
#' @references
#' Bliese, P. D. (2000). Within-group agreement, non-independence, and
#' reliability: Implications for data aggregation and analysis. In K. J. Klein
#' & S. W. J. Kozlowski (Eds.), *Multilevel Theory, Research, and Methods in
#' Organizations* (pp. 349-381). Jossey-Bass.
#'
#' Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
#' assessing rater reliability. *Psychological Bulletin*, 86(2), 420-428.
#' @seealso [icc2()] for group-mean reliability, [rwg()] for within-group
#'   agreement, [netis_reliability()] for all of them at once, and
#'   [netis_variance_components()] for the *conditional* partition.
#' @examples
#' # Four groups of seven, two waves.
#' set.seed(4)
#' gs <- c("a", "b", "c", "d")
#' nodes <- do.call(rbind, lapply(gs, function(g) {
#'   ids <- sprintf("%s%02d", g, 1:7)
#'   data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = 7),
#'              y = stats::rnorm(14), x = stats::rnorm(14))
#' }))
#' ties <- do.call(rbind, lapply(gs, function(g) {
#'   ids <- sprintf("%s%02d", g, 1:7)
#'   data.frame(group = g, source = ids[-7], target = ids[-1], wave = 1, tie = 1)
#' }))
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(
#'   y ~ ar() + exposure(x, via = tie, normalize = "row") + (1 | group),
#'   data = nd
#' )
#' icc1(fit)
#' @export
icc1 <- function(fit, group = NULL) {
  .assert_icc_family(fit)
  group <- .resolve_icc_group(fit, group)
  .assert_icc_supported(fit, group)

  es <- .icc_estimating_sample(fit, group)
  a <- .icc_anova(es[[".outcome"]], es[[group]])
  if (isTRUE(a$failed)) {
    return(.new_icc1(NA_real_, "failed", a$message, group, a))
  }

  value <- (a$msb - a$msw) / (a$msb + (a$n0 - 1) * a$msw)
  if (a$msb <= a$msw) {
    return(.new_icc1(
      value, "boundary",
      "The between-group mean square does not exceed the within-group mean square, so the ANOVA estimate of ICC(1) is zero or negative: the sample shows no evidence of between-group variance. The estimate is reported as computed; at a small number of groups it is also very imprecisely estimated, which this value does not express.",
      group, a
    ))
  }
  .new_icc1(value, "ok", NULL, group, a)
}

#' @noRd
.new_icc1 <- function(value, status, reason, group, a) {
  between <- if (is.finite(a$msb) && is.finite(a$n0)) (a$msb - a$msw) / a$n0 else NA_real_
  structure(
    list(
      value = value, status = status, reason = reason, group = group,
      n_groups = as.integer(a$k),
      n_obs = as.integer(a$N),
      n0 = a$n0,
      anova = c(msb = a$msb, msw = a$msw),
      components = c(between = between, within = a$msw),
      n_j = a$n_j
    ),
    class = "netis_icc1"
  )
}

#' Group-mean reliability ICC(2)
#'
#' How reliably the group means are differentiated: Bliese's (2000) ICC(2),
#' computed from the same one-way ANOVA [icc1()] uses over this fit's
#' estimating sample.
#'
#' # The overall value
#'
#' The default overall value is Bliese's ICC(2),
#' `ICC(2) = (MSB - MSW) / MSB`. It equals the Spearman-Brown projection of
#' ICC(1) to the effective group size `n0`,
#' `n0 * ICC(1) / (1 + (n0 - 1) * ICC(1))`, which is why `overall = "n0"` is
#' the name of this choice and `overall_n` reports `n0`.
#'
#' `"harmonic"` and `"arithmetic"` instead evaluate the Spearman-Brown
#' projection of ICC(1) at the harmonic or arithmetic mean group size. Both
#' are in circulation; neither is Bliese's ANOVA ratio, and under imbalance
#' `"arithmetic"` is the most optimistic of the three when ICC(1) is positive. **With equal group
#' sizes all three coincide**, which is why a balanced example cannot show the
#' difference.
#'
#' # Per-group values
#'
#' `by_group` gives each group's Spearman-Brown reliability at its own size,
#' `ICC(2)_j = n_j * ICC(1) / (1 + (n_j - 1) * ICC(1))`, the form Bliese
#' (2000) gives for the reliability of a mean of `n_j` members. `n_j` counts
#' the group's rows in the estimating sample, so a group that lost rows to
#' missing data is reported at its reduced size.
#'
#' # `n_j` counts units, not rows
#'
#' `n_j` is the number of distinct lower-level units contributing to the group
#' aggregate: each unit contributes one row.
#'
#' @param fit a `netis_influence` fit.
#' @param group the grouping factor; see [icc1()].
#' @param overall which overall value to report: `"n0"` (default, Bliese's
#'   `(MSB - MSW) / MSB`), `"harmonic"`, or `"arithmetic"`. Pass `"none"` for
#'   the per-group values only.
#' @return a `netis_icc2` object: `by_group` (a data frame of `group`, `n_j`
#'   and `reliability`), `overall`, `overall_method`, `overall_n`, `group`, and the
#'   `icc1` result it was derived from, whose `status` carries any boundary
#'   note.
#' @references
#' Bliese, P. D. (2000). Within-group agreement, non-independence, and
#' reliability: Implications for data aggregation and analysis. In K. J. Klein
#' & S. W. J. Kozlowski (Eds.), *Multilevel Theory, Research, and Methods in
#' Organizations* (pp. 349-381). Jossey-Bass.
#' @seealso [icc1()], [netis_reliability()], [rwg()].
#' @examples
#' # Four groups of seven, two waves.
#' set.seed(4)
#' gs <- c("a", "b", "c", "d")
#' nodes <- do.call(rbind, lapply(gs, function(g) {
#'   ids <- sprintf("%s%02d", g, 1:7)
#'   data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = 7),
#'              y = stats::rnorm(14), x = stats::rnorm(14))
#' }))
#' ties <- do.call(rbind, lapply(gs, function(g) {
#'   ids <- sprintf("%s%02d", g, 1:7)
#'   data.frame(group = g, source = ids[-7], target = ids[-1], wave = 1, tie = 1)
#' }))
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(
#'   y ~ ar() + exposure(x, via = tie, normalize = "row") + (1 | group),
#'   data = nd
#' )
#' icc2(fit)
#' icc2(fit, overall = "harmonic")
#' @export
icc2 <- function(fit, group = NULL,
                 overall = c("n0", "harmonic", "arithmetic", "none")) {
  overall <- match.arg(overall)
  one <- icc1(fit, group = group)
  group <- one$group

  n_j <- as.numeric(one$n_j)
  rho <- one$value

  # Spearman-Brown per group, from each group's OWN size in the estimating
  # sample. NA propagates from a failed ANOVA rather than being replaced.
  rel <- (n_j * rho) / (1 + (n_j - 1) * rho)

  by_group <- data.frame(
    group = names(one$n_j) %||% character(0), n_j = n_j, reliability = rel,
    stringsAsFactors = FALSE, row.names = NULL
  )
  names(by_group)[1] <- group

  n_used <- NA_real_
  value <- NA_real_
  if (!identical(overall, "none") && is.finite(rho)) {
    k <- length(n_j)
    if (identical(overall, "n0")) {
      # Bliese's ICC(2) directly from the mean squares; algebraically the
      # Spearman-Brown projection of the ANOVA ICC(1) to n0.
      n_used <- one$n0
      value <- (one$anova[["msb"]] - one$anova[["msw"]]) / one$anova[["msb"]]
    } else {
      n_used <- switch(
        overall,
        harmonic = if (all(n_j > 0)) k / sum(1 / n_j) else NA_real_,
        arithmetic = mean(n_j)
      )
      if (is.finite(n_used)) {
        value <- (n_used * rho) / (1 + (n_used - 1) * rho)
      }
    }
  }

  structure(
    list(
      by_group = by_group, overall = value,
      overall_method = if (identical(overall, "none")) NA_character_ else overall,
      overall_n = n_used, group = group, icc1 = one
    ),
    class = "netis_icc2"
  )
}

#' @export
print.netis_icc2 <- function(x, ...) {
  cat(sprintf("ICC(2): group-mean reliability for '%s'   [one-way ANOVA]\n", x$group))
  if (!identical(x$icc1$status, "ok")) {
    cat(sprintf("  ICC(1) status: %s -- %s\n", x$icc1$status, x$icc1$reason))
  }
  cat("\n")
  print(x$by_group, row.names = FALSE)
  cat("\n")
  if (is.na(x$overall_method)) {
    cat("  No scalar summary (overall = \"none\").\n")
  } else if (is.na(x$overall)) {
    cat("  Overall: not available.\n")
  } else {
    # The method and the n are printed WITH the value, never the value alone:
    # under unequal group sizes a bare ICC(2) does not identify which of three
    # constructions produced it.
    cat(sprintf("  Overall: %.4f  (evaluated at %s n = %.3f)\n",
                x$overall, x$overall_method, x$overall_n))
  }
  invisible(x)
}

#' @export
print.netis_icc1 <- function(x, ...) {
  cat(sprintf("ICC(1): %s   [one-way ANOVA, unconditional]\n",
              if (is.na(x$value)) "not available" else format(round(x$value, 4))))
  cat(sprintf("  grouping: %s (%d groups, %d observations, n0 = %s)\n",
              x$group, x$n_groups, x$n_obs,
              if (is.finite(x$n0)) format(round(x$n0, 3)) else "NA"))
  if (!identical(x$status, "ok")) {
    cat(sprintf("  status: %s -- %s\n", x$status, x$reason))
  }
  cat("  Share of outcome variance associated with group membership BEFORE\n")
  cat("  conditioning on this model's predictors. Not the same quantity as\n")
  cat("  netis_variance_components()'s conditional share.\n")
  invisible(x)
}
