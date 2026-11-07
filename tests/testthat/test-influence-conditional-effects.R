# influence_effect() on random-effects fits, the random-slope name check, and
# the pooling policy for a slope-only group term.
#
# The first three tests guard against two defects:
#   * influence_effect() stopping with "argument is of length zero" on a fit
#     with a random exposure slope, because the effect layer is handed no
#     var_corr;
#   * .check_random_effects_slopes() existing but never being called, so a
#     misspelled slope reaches lme4 raw.

test_that("influence_effect() reports a random exposure slope's group SD and range", {
  skip_if_not_installed("lme4")
  fit <- .vc_slope_fit()
  eff <- influence_effect(fit)
  e <- eff$exposures[["ex_x_tie"]]
  expect_false(is.null(e))

  # REFERENCE EQUIVALENCE: the same model fitted by lme4 directly on the
  # assembled table, not numbers remembered from an earlier run.
  mf <- fit$model_frame
  direct <- suppressWarnings(lme4::lmer(.outcome ~ y_lag1 + ex_x_tie + (1 + ex_x_tie | group),
                                        data = mf, REML = TRUE))
  vc <- as.data.frame(lme4::VarCorr(direct))
  sd_ref <- vc$sdcor[vc$grp == "group" & vc$var1 == "ex_x_tie" & is.na(vc$var2)]
  expect_equal(e$group_sd, sd_ref, tolerance = 1e-6)
  re <- lme4::ranef(direct)$group[["ex_x_tie"]]
  expect_equal(e$group_range, unname(lme4::fixef(direct)["ex_x_tie"]) + range(re),
               tolerance = 1e-6)
})

test_that("a random-intercept fit reports no slope heterogeneity", {
  skip_if_not_installed("lme4")
  eff <- influence_effect(.vc_intercept_fit())
  e <- eff$exposures[["ex_x_tie"]]
  expect_null(e$group_sd)
  expect_null(e$group_range)
})

test_that("a random slope must name a column the formula produced", {
  skip_if_not_installed("lme4")
  expect_error(
    fit_influence(y ~ ar() + exposure(x, via = tie, normalize = "row") + (1 + ex_x_ti | group),
                  data = .vc_data()),
    "'ex_x_ti' .* is not one of this formula's fixed-effects columns"
  )
})

test_that("a slope-only group term is fitted, and its shared intercept is reported", {
  skip_if_not_installed("lme4")
  # A valid model with a substantive assumption, so it
  # is fitted -- not refused -- and the assumption is a design finding.
  fit <- suppressWarnings(fit_influence(
    y ~ ar() + exposure(x, via = tie, normalize = "row") + (0 + ex_x_tie | group),
    data = .vc_data()
  ))
  expect_identical(fit$backend, "lme4")
  ids <- vapply(fit$diagnostics, function(f) f$code %||% NA_character_, character(1))
  sev <- vapply(fit$diagnostics, function(f) f$level %||% NA_character_, character(1))
  expect_true("common_intercept_across_groups" %in% ids)
  expect_identical(unname(sev[ids == "common_intercept_across_groups"]), "WARNING")

  # The positive control: with the intercept in the term, nothing is reported.
  fit2 <- .vc_slope_fit()
  ids2 <- vapply(fit2$diagnostics, function(f) f$code %||% NA_character_, character(1))
  expect_false("common_intercept_across_groups" %in% ids2)
})

# ---- Johnson-Neyman boundaries --------------------------------------------

test_that("Johnson-Neyman boundaries are the brute-force crossings", {
  b0 <- 0.365; b1 <- -0.9; v0 <- 0.02; v1 <- 0.05; c01 <- -0.004; crit <- 2.0
  jn <- .johnson_neyman_boundaries(b0, b1, v0, v1, c01, crit)
  # Hand truth: the roots of (b0 + z b1)^2 = crit^2 (v0 + z^2 v1 + 2 z c01).
  a <- b1^2 - crit^2 * v1
  b <- 2 * (b0 * b1 - crit^2 * c01)
  cc <- b0^2 - crit^2 * v0
  truth <- sort((-b + c(-1, 1) * sqrt(b^2 - 4 * a * cc)) / (2 * a))
  expect_length(jn$boundaries, 2L)
  expect_lt(max(abs(jn$boundaries - truth)), 1e-12)
  expect_lt(abs(jn$boundaries[1] - 0.0937355), 1e-6)
  expect_lt(abs(jn$boundaries[2] - 0.9308547), 1e-6)
})

test_that("Johnson-Neyman boundaries rescale with the moderator's units", {
  # Measuring the moderator in units k times smaller multiplies its values by
  # k: the interaction coefficient divides by k, its variance by k^2, the
  # covariance by k, and every boundary multiplies by k.
  b0 <- 0.365; b1 <- -0.9; v0 <- 0.02; v1 <- 0.05; c01 <- -0.004; crit <- 2.0
  base <- .johnson_neyman_boundaries(b0, b1, v0, v1, c01, crit)$boundaries
  for (k in c(1e-3, 1e3, 1e5, 1e6, 1e8)) {
    jn <- .johnson_neyman_boundaries(b0, b1 / k, v0, v1 / k^2, c01 / k, crit)
    expect_length(jn$boundaries, 2L)
    expect_lt(max(abs(jn$boundaries / k - base)), 1e-6 * max(abs(base)))
  }
})

test_that("a no-crossing fit has no boundary at any moderator scale", {
  # Both coefficients imprecise: the interval includes zero at every moderator
  # value, so the quadratic has a < 0 and no real root.
  b0 <- 0.1; b1 <- 0.1; v0 <- 1; v1 <- 1; c01 <- 0; crit <- 1.96
  for (k in c(1, 1e3, 1e6)) {
    jn <- .johnson_neyman_boundaries(b0, b1 / k, v0, v1 / k^2, c01 / k, crit)
    expect_length(jn$boundaries, 0L)
  }
})

# ---- node-level random effects are refused with their reason ---------------

test_that("(1 | node) and (1 | group/node) are refused as not identified", {
  skip_if_not_installed("lme4")
  d <- .vc_data()
  expect_error(
    fit_influence(y ~ ar() + x + (1 | node), data = d),
    "node-level variance is not separately identified"
  )
  expect_error(
    fit_influence(y ~ ar() + x + (1 | group/node), data = d),
    "(1 | group/node) cannot be fitted", fixed = TRUE
  )
  # The supported term still fits.
  fit <- suppressWarnings(fit_influence(y ~ ar() + x + (1 | group), data = d))
  expect_identical(fit$backend, "lme4")
})

test_that("(1 | node) on a single network is refused with the same reason", {
  ids <- letters[1:10]
  set.seed(2)
  nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = 10),
                      y = stats::rnorm(20), x = stats::rnorm(20))
  ties <- data.frame(group = 1, source = ids[-10], target = ids[-1], wave = 1, tie = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(fit_influence(y ~ ar() + x + (1 | node), data = nd),
               "each node contributes exactly one outcome row")
})
