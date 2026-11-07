# ANOVA ICC(1): the estimand and the refusals.
#
# The load-bearing assertion in this file is the FIRST one: icc1() must not be
# computable from the fitted model's variance components. That conflation is
# a known defect, and it is the reason icc1() is a separate
# function rather than a rename of netis_variance_components()$residual_variance_share.

# A BALANCED multi-group two-wave fixture, built by hand.
#
# Balanced deliberately, and the tests below say so: with equal
# group sizes the three n_j conventions COINCIDE, which is a property of the
# fixture rather than of the statistics. The unbalanced case is a separate
# test with its own fixture, and it is what shows the three apart.
.rel_data <- function(sizes = c(a = 7, b = 7, c = 7, d = 7), seed = 4) {
  set.seed(seed)
  gs <- names(sizes)
  nodes <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("%s%02d", g, seq_len(sizes[[g]]))
    n <- length(ids)
    data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = n),
               y = stats::rnorm(2 * n) + stats::rnorm(1, sd = 1.5),
               x = stats::rnorm(2 * n), stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("%s%02d", g, seq_len(sizes[[g]]))
    n <- length(ids)
    data.frame(group = g, source = ids[-n], target = ids[-1], wave = 1,
               tie = 1, stringsAsFactors = FALSE)
  }))
  netis_data(nodes, ties, wave_col = "wave")
}

.icc_fit_groups <- function() {
  suppressWarnings(fit_influence(
    y ~ ar() + exposure(x, via = tie, normalize = "row") + (1 | group),
    data = .rel_data(), waves = c(1, 2)
  ))
}

test_that("ICC(1) is the null-model quantity, not the fitted model's share", {
  fit <- .icc_fit_groups()
  unconditional <- icc1(fit)$value
  conditional <- netis_variance_components(fit)
  conditional_share <- conditional$residual_variance_share[conditional$grouping == "group"]

  # Both are shares of variance associated with `group`, both in [0, 1], and
  # they answer different questions -- one before conditioning on ar() and
  # exposure(), one after. If icc1() were (incorrectly) reading the fitted
  # model's components these would be identical by construction.
  expect_gte(unconditional, 0)
  expect_lte(unconditional, 1)
  expect_false(isTRUE(all.equal(unconditional, conditional_share)))
})

test_that("ICC(1) equals the one-way ANOVA computed with stats::anova()", {
  # Independent route to the mean squares, on the rows the backend kept
  # (each chain's first member has no exposure value and is dropped, so the
  # groups are balanced at six and n0 = 6).
  fit <- .icc_fit_groups()
  mf <- fit$model_frame[names(fit$residuals), ]
  expect_equal(as.numeric(table(mf$group)), rep(6, 4))
  a <- stats::anova(stats::lm(.outcome ~ factor(group), data = mf))
  msb <- a$`Mean Sq`[1]
  msw <- a$`Mean Sq`[2]
  expected <- (msb - msw) / (msb + (6 - 1) * msw)
  expect_lt(abs(icc1(fit)$value - expected), 1e-10)
})

test_that("an undeclared grouping column refuses, even when it exists", {
  # `node` IS a column of the estimating sample, and is still refused: node
  # labels are unique only WITHIN group here, so table(node) counts the same
  # label across different groups as one level and a null model keyed on it
  # would silently pool distinct people. rnetis group-qualifies nested labels in
  # its own random-effects machinery; icc1() declines to build a second,
  # parallel notion of what a unit is, and accepts only declared grouping
  # factors.
  fit <- .icc_fit_groups()
  expect_true("node" %in% names(fit$model_frame))
  expect_error(icc1(fit, group = "node"), "not one of this fit's random-effects grouping factors")
})

test_that("an unknown grouping column refuses and names the real one", {
  fit <- .icc_fit_groups()
  expect_error(icc1(fit, group = "school"), "not one of this fit's random-effects grouping factors")
  expect_error(icc1(fit, group = "school"), "group")
})

test_that("a Selection fit refuses rather than returning a number", {
  # The outcome is a tie and the grouping factor is a network, so group-mean
  # reliability has no aggregation-justification reading.
  fake <- structure(list(spec = list()), class = c("netis_selection", "netis_fit"))
  expect_error(icc1(fake, group = "g"), "Influence fits only")
})

test_that("the result carries its own status, independent of diagnostics", {
  # The status must travel WITH the value: a 0 pulled programmatically into a
  # results table would otherwise read as "measured no clustering". This is
  # also why 48.2 does not depend on 48.3's check framework existing.
  fit <- .icc_fit_groups()
  res <- icc1(fit)
  expect_true(res$status %in% c("ok", "boundary", "failed"))
  expect_true(is.numeric(res$value))
  expect_equal(res$group, "group")
})
