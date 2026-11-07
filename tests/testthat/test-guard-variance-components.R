# What netis_variance_components() may and may not report.
#
# These tests state the contract rather than whatever the code happens to do,
# so they fail against an implementation with either defect below.
#
# Guard rather than influence: defect A is a number with no estimand presented as a coherent variance decomposition -- and defect B
# breaks a documented contract about what the accessor discards. Both are
# cross-cutting invariants rather than one feature's behavior.

# The fixtures (.vc_data(), .vc_slope_fit(), .vc_intercept_fit()) live in
# helper-influence-random.R.

test_that("a random-slope fit reports no scalar variance share (defect A)", {
  # With (1 + x | group) the between-group variance is
  #   tau00 + 2*x*tau01 + x^2*tau11
  # -- a function of the slope predictor, so NO row of a scalar share column is
  # defined, not merely the slope row. The shipped defect computed
  # component/total for every row, which summed to exactly 1 and therefore read
  # as a coherent decomposition while being a ratio with no estimand.
  vc <- netis_variance_components(.vc_slope_fit())
  expect_true(all(is.na(vc$residual_variance_share)))
})

test_that("the undefined variance share carries a reason", {
  # The package refuses to return the number; the user still has to be told why, or
  # an NA column is indistinguishable from a bug.
  vc <- netis_variance_components(.vc_slope_fit())
  expect_true(!is.null(attr(vc, "icc_reason")))
  expect_match(attr(vc, "icc_reason"), "random slope", ignore.case = TRUE)
})

test_that("a random-intercept-only fit keeps its scalar variance share", {
  # The positive control for defect A's fix: blanking the column unconditionally
  # would be an over-correction. Where the model has one random intercept the
  # partition is a well-defined scalar and must survive.
  vc <- netis_variance_components(.vc_intercept_fit())
  expect_false(anyNA(vc$residual_variance_share))
  expect_equal(sum(vc$residual_variance_share), 1, tolerance = 1e-8)
})

test_that("the intercept-slope covariance is reported, not dropped (defect B)", {
  # The Influence path filtered covariance rows out entirely while the roxygen
  # promised nothing the model estimated goes unreported -- true for Selection,
  # false here.
  vc <- netis_variance_components(.vc_slope_fit())
  expect_true("covariance" %in% vc$kind)
  cov_rows <- vc[vc$kind == "covariance", , drop = FALSE]
  expect_equal(nrow(cov_rows), 1L)
})

test_that("the covariance ESTIMATE is recoverable, not just its row", {
  # Restoring a labeled row with every numeric field NA would satisfy the
  # letter of the contract and not its purpose. The estimate itself must be
  # readable off the returned frame.
  vc <- netis_variance_components(.vc_slope_fit())
  cov_rows <- vc[vc$kind == "covariance", , drop = FALSE]
  expect_true(is.finite(cov_rows$estimate))
  # A covariance is not a variance and does not get an SD, so the
  # variance-specific columns stay NA rather than carrying a mislabeled value.
  expect_true(is.na(cov_rows$variance))
  expect_true(is.na(cov_rows$sd))
})

test_that("every estimated random-effects parameter survives the accessor", {
  # lme4 estimates four parameters for (1 + x | group): the two variances,
  # their covariance, and the residual variance. This is the invariant defect B
  # violated, stated directly rather than through any one row.
  vc <- netis_variance_components(.vc_slope_fit())
  expect_equal(nrow(vc), 4L)
})

test_that("the share column is residual_variance_share, and `icc` is gone", {
  # The name is not aliased. Keeping both names alive
  # would keep reachable exactly the confusion the rename exists to end: `icc`
  # here is a CONDITIONAL share, while icc1() is the unconditional null-model
  # quantity. rnetis is pre-release, so there is no installed base that would
  # justify carrying the ambiguous name forward.
  vc <- netis_variance_components(.vc_intercept_fit())
  expect_true("residual_variance_share" %in% names(vc))
  expect_false("icc" %in% names(vc))
})

test_that("estimate carries the variance on ordinary variance rows", {
  # `estimate` is the generic value column, so it is populated everywhere --
  # not only on the rows that have nowhere else to put a number.
  vc <- netis_variance_components(.vc_intercept_fit())
  expect_equal(vc$estimate, vc$variance)
})
