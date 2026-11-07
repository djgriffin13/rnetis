# REPRODUCTION -- Frank's influence tutorial, on his own toy data.
#
# Source: the `influence.R` tutorial distributed with Toyatt.csv and
# Toynet.csv from <https://sites.google.com/msu.edu/kenfrank/social-network-resources>.
# Six actors, twelve directed nominations, one behavioral transition.
#
# EXACTNESS IS THE RIGHT EXPECTATION HERE, so it is what is asserted. The
# tutorial's estimator -- mean exposure, then lm(yvar2 ~ yvar1 +
# exposure_mean) -- is closed form at every step. No sampler, no tuning, no
# tolerance to negotiate. The tolerances below are set at double-precision
# noise; anything larger is a rnetis defect, not a difference of opinion.

test_that("the exposure vector is Frank's mean_exposure, value for value", {
  d <- .frank_toy()
  ref <- .frank_toy_reference(d)
  fit <- .frank_toy_netis(d)

  mf <- fit$model_frame
  got <- mf[["ex_y_tie"]][match(as.character(ref$final$node), as.character(mf$node))]
  expect_equal(got, ref$final$exposure_mean, tolerance = 1e-12)
})

test_that("the fitted coefficients and standard errors are Frank's", {
  d <- .frank_toy()
  ref <- .frank_toy_reference(d)
  fit <- .frank_toy_netis(d)

  rc <- stats::coef(ref$model)
  nc <- stats::coef(fit)
  expect_equal(unname(nc[c("(Intercept)", "y_lag1", "ex_y_tie")]),
               unname(rc[c("(Intercept)", "yvar1", "exposure_mean")]),
               tolerance = 1e-12)

  expect_equal(unname(sqrt(diag(stats::vcov(fit)))[c("(Intercept)", "y_lag1", "ex_y_tie")]),
               unname(summary(ref$model)$coefficients[, 2]),
               tolerance = 1e-12)
})

test_that("both readings of Frank's disagreeing materials reproduce", {
  # THE SOURCE MATERIALS CONTRADICT THEMSELVES AT ONE CELL. His script
  # hard-codes
  #   yvar2 <- c(2, 2, 1, -0.5, -2, -0.5)
  # while Toyatt.csv gives node 6 attr2 = +0.5, not -0.5. Every other cell
  # agrees, and rnetis reproduces BOTH readings exactly -- so nothing in this
  # comparison rides on the discrepancy. It is asserted rather than mentioned
  # because a future reader who notices the mismatch should find it already
  # accounted for rather than reopening it.
  d <- .frank_toy()
  scripted <- c(2, 2, 1, -0.5, -2, -0.5)

  ref <- .frank_toy_reference(d, yvar2 = scripted)
  fit <- .frank_toy_netis(d, yvar2 = scripted)

  expect_equal(unname(stats::coef(fit)[c("(Intercept)", "y_lag1", "ex_y_tie")]),
               unname(stats::coef(ref$model)[c("(Intercept)", "yvar1", "exposure_mean")]),
               tolerance = 1e-12)

  # And the two readings really are different fits, so the test above is not
  # accidentally asserting the same thing twice.
  ref_csv <- .frank_toy_reference(d)
  expect_false(isTRUE(all.equal(stats::coef(ref$model), stats::coef(ref_csv$model))))
})
