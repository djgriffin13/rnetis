# Two-stage common-beta precision pooling for multi-group Selection.
#
# WHAT THIS IS, NAMED CORRECTLY. Each group is fitted on its own, and the
# per-group posteriors are then combined coefficient by coefficient by
# inverse-variance weighting. That is FIXED-EFFECT INVERSE-VARIANCE
# META-ANALYSIS under a normal approximation -- Cochran (1954); Hedges & Olkin
# (1985); Borenstein, Hedges, Higgins & Rothstein (2009, Introduction to
# Meta-Analysis, Wiley) -- equivalently a
# normal-normal conjugate combination of independent posteriors. It is not
# "complete pooling": the two stages are the substance, and calling it complete
# pooling suggests a single joint model, which this is not.
#
# WHY THIS IS THE DEFAULT. It is the published procedure. The alternative --
# one model over every group, drawing the coefficients from their exact joint
# full conditional and, if asked, giving them a between-group distribution --
# is available as a separate route, `pooling = selection_pooling()`
# (R/selection-hierarchical*.R, R/mgc-*.R). That route is rnetis's own
# construction from published components, documented as such at
# ?hierarchical_selection. It is never substituted for this one silently: the
# two are different models, chosen by different arguments.
#
# The construction reproduces `lame::lame_multi()` (Dorff, Minhas & Salau),
# which fits each panel with `lame()` and then pools the per-panel posterior
# means and marginal variances by precision weighting. rnetis implements it
# rather than depending on it -- the computation is a weighted mean, and taking
# a sixteen-Import dependency for that would be out of proportion -- and an
# equivalence test against `lame_multi()` is retained as corroboration.
#
# WHAT THE EQUIVALENCE CLAIMS, PRECISELY. rnetis reproduces lame's POOLING
# ALGORITHM. It does not thereby reproduce an exact joint Bayesian posterior,
# and this package does not describe it as one even though lame's own
# documentation calls the pooling exact when panels are conditionally
# independent given beta. Two reasons to keep the weaker claim:
#
#   * The algorithm is not what that description implies. Each panel's draws
#     are reduced to a mean and a marginal variance, pooled in information
#     form, and lame then generates its joint beta by drawing independent
#     normals around the pooled coefficient-wise moments. That is a normal
#     approximation applied twice, not a posterior carried through.
#   * Each panel fit carries its OWN PRIOR. Treating K independent posteriors
#     as independent likelihood contributions to one shared-beta posterior
#     counts that prior K times. Under a diffuse prior and a well-identified
#     beta the difference is small; it is not nothing, and it is not something
#     a software equivalence test can measure.
#
# So: two-stage common-beta precision pooling, with coefficient-wise pooled
# inference. That is what is implemented and that is what is claimed.
#
# FOUR PROPERTIES, ALL DOCUMENTED RATHER THAN DISCOVERED:
#
#   1. Complete pooling of beta, with no between-group variance component. No
#      tau^2, no partial pooling, no hierarchical prior. The model asserts beta
#      is common across groups; heterogeneity in beta is assumed away, not
#      estimated.
#   2. Coefficient-wise, so the posterior COVARIANCE among coefficients is
#      discarded. Per-coefficient intervals are fine; a contrast, a
#      multi-coefficient Wald test, or a joint credible region computed from
#      pooled draws would be WRONG.
#   3. Order-invariant by construction -- the pooling is a sum over groups, and
#      summation commutes. A sequential posterior-as-prior scheme would
#      instead depend on the order in which groups were processed.
#   4. A normal approximation to each group's marginal posterior. Fine for a
#      well-identified beta; poor for a small group whose marginal is skewed or
#      whose variance is barely identified.

#' The numerical guard, which is NOT part of the pooling method
#'
#' A separate named constant so that it cannot be read as a modeling choice.
#' It exists for one reason: `1 / v` is not computable when `v` is zero, and a
#' variance that has underflowed is not evidence of infinite precision.
#'
#' It is rnetis's own constant. `lame::lame_multi()` happens to use the same
#' value, and the equivalence test between them is deliberately built on a
#' fixture where NO variance comes near it -- so that the test exercises the
#' pooling formula rather than a shared numerical convention. If lame changed
#' its guard tomorrow, nothing here would need to change.
#'
#' @noRd
.POOL_VARIANCE_FLOOR <- 1e-8

#' Pool per-group Selection coefficients by inverse-variance weighting
#'
#' Coefficients are matched across groups BY NAME. Pooling the third column of
#' one group with the third column of another because both have three columns
#' would combine different parameters into one estimate, and nothing downstream
#' could detect it -- the result would have the right shape and the wrong
#' meaning.
#'
#' @param per_group the list of per-group fit results, each carrying an
#'   `extracted$draws` matrix of retained posterior draws.
#' @return a list with `mean`, `var`, `weights` (the per-group precisions, for
#'   reporting), `n_groups`, and `floored` naming any coefficient whose
#'   variance was small enough to reach the numerical guard in some group.
#' @noRd
.pool_beta_precision <- function(per_group) {
  # `extracted$draws` IS the matrix of retained coefficient draws, one column
  # per coefficient. Read it directly rather than through a `$BETA` element,
  # which is amen's own naming rather than the adapter's.
  betas <- lapply(per_group, function(g) g$extracted$draws)
  keep <- !vapply(betas, is.null, logical(1))
  betas <- betas[keep]
  if (length(betas) == 0L) {
    return(NULL)
  }

  # NAMES ARE THE JOIN KEY, so they have to exist and be unambiguous. Without
  # them every alignment below would be positional, which is the failure this
  # function must not have.
  for (k in seq_along(betas)) {
    cn <- colnames(betas[[k]])
    if (is.null(cn) || anyNA(cn) || !all(nzchar(cn))) {
      stop(
        sprintf(
          "Cannot pool coefficients across groups: group %s has unnamed coefficient columns, so rnetis cannot tell which coefficient is which. Pooling matches coefficients BY NAME -- combining them by column position would pool different parameters into one estimate whenever two groups ordered their columns differently.",
          names(betas)[k] %||% k
        ),
        call. = FALSE
      )
    }
    if (anyDuplicated(cn)) {
      stop(
        sprintf(
          "Cannot pool coefficients across groups: group %s has duplicated coefficient names (%s), so matching by name is ambiguous.",
          names(betas)[k] %||% k,
          paste(unique(cn[duplicated(cn)]), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  nms <- colnames(betas[[1]])
  p <- length(nms)
  for (k in seq_along(betas)) {
    cn <- colnames(betas[[k]])
    if (!setequal(cn, nms)) {
      only_here <- setdiff(cn, nms)
      only_first <- setdiff(nms, cn)
      stop(
        sprintf(
          "Cannot pool coefficients across groups: the groups were fitted with different coefficients. Group %s has %s%s%s. Pooling requires one common specification, since a shared beta has to mean the same thing in every group.",
          names(betas)[k] %||% k,
          if (length(only_here)) sprintf("%s, which the first group does not", paste(only_here, collapse = ", ")) else "",
          if (length(only_here) && length(only_first)) "; and it lacks " else "",
          if (length(only_first)) sprintf("%s, which the first group has", paste(only_first, collapse = ", ")) else ""
        ),
        call. = FALSE
      )
    }
  }

  # Reordered to a COMMON ORDER by name before any arithmetic. A group whose
  # columns arrive in a different order is not an error -- it is the ordinary
  # consequence of fitting groups independently -- but pooling it positionally
  # would be.
  summaries <- lapply(betas, function(b) {
    b <- b[, nms, drop = FALSE]
    list(mean = colMeans(b), var = apply(b, 2L, stats::var))
  })
  means <- vapply(summaries, function(x) x$mean, numeric(p))
  vars <- vapply(summaries, function(x) x$var, numeric(p))
  if (p == 1L) {
    means <- matrix(means, nrow = 1L)
    vars <- matrix(vars, nrow = 1L)
  }

  # A DEGENERATE VARIANCE IS REFUSED, NOT FLOORED. Zero (or non-finite) is not
  # a very small variance -- it means the sampler never moved that coefficient
  # in that group, so the draws carry no information about its uncertainty.
  # Flooring it would hand that group essentially all the weight and return one
  # group's estimate wearing the pooled label, which is a wrong number rather
  # than an imprecise one.
  bad <- !is.finite(vars) | vars <= 0
  if (any(bad)) {
    where <- which(bad, arr.ind = TRUE)
    stop(
      sprintf(
        "Cannot pool coefficients across groups: %s has zero or non-finite posterior variance in group %s. A coefficient whose draws never moved carries no information about its own uncertainty, and inverse-variance weighting would give that group effectively all of the weight -- returning one group's estimate under a pooled label. Check that group's fit (a constant draw column usually means the sampler did not update the coefficient), or refit without pooling to inspect the groups separately.",
        paste(unique(nms[where[, 1]]), collapse = ", "),
        paste(unique(colnames(vars)[where[, 2]] %||% where[, 2]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # What remains is the pooling formula itself, with one numerical guard whose
  # only job is to keep 1/v computable when v has underflowed:
  #
  #   beta_pooled = sum_g (beta_g / V_g) / sum_g (1 / V_g)
  #   V_pooled    = 1 / sum_g (1 / V_g)
  #
  # `w_sum` is a sum of strictly positive precisions, so it needs no guard of
  # its own; a second floor would protect against nothing.
  floored <- nms[apply(vars < .POOL_VARIANCE_FLOOR, 1L, any)]
  prec <- 1 / pmax(vars, .POOL_VARIANCE_FLOOR)
  w_sum <- rowSums(prec)
  pooled_mean <- rowSums(prec * means) / w_sum
  pooled_var <- 1 / w_sum

  names(pooled_mean) <- nms
  names(pooled_var) <- nms
  dimnames(prec) <- list(nms, names(betas))

  list(
    mean = pooled_mean,
    var = pooled_var,
    weights = prec,
    n_groups = length(betas),
    floored = floored
  )
}

#' The sentence `summary()` must print wherever pooling is in effect
#'
#' A default that encodes an assumption has to state it, and pooled beta is the
#' default on multi-group data.
#'
#' @param pooled the result of `.pool_beta_precision()`, or `NULL`.
#' @return a character vector of display lines, empty when nothing was pooled.
#' @noRd
.format_beta_pooling <- function(pooled) {
  if (is.null(pooled)) {
    return(character(0))
  }
  # Nothing to say about pooling one group: the combination is the identity,
  # and a paragraph of caveats about what is assumed "across groups" would be
  # both noise and slightly false where there is only one.
  if (pooled$n_groups < 2L) {
    return(character(0))
  }
  out <- c(
    sprintf(
      "Coefficients pooled across %d groups by two-stage common-beta precision weighting.",
      pooled$n_groups
    ),
    "  This assumes beta is COMMON across groups: there is no between-group variance",
    "  component, so heterogeneity in beta is assumed away rather than estimated.",
    "  Pooling is coefficient-wise, so no covariance between coefficients survives",
    "  it -- per-coefficient intervals are valid, joint tests are not."
  )
  if (length(pooled$floored)) {
    out <- c(out, sprintf(
      "  Variance hit the numerical floor for: %s. Those coefficients' weights are not meaningful.",
      paste(pooled$floored, collapse = ", ")
    ))
  }
  out
}
