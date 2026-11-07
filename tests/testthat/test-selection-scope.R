# The package's SCOPE, asserted positively.
#
# rnetis covers the published selection and influence methodology plus the
# hierarchical multi-group extension. These tests pin what a Selection fit
# carries and what the package offers, so that surface growing beyond it fails
# here rather than at review.
#
# Stated as "this is what exists" rather than as a list of things that must
# not: a denial list is a description of whatever it denies, and it dates badly
# -- every name on it has to be maintained forever, and the list itself becomes
# the documentation of a boundary it was meant to enforce quietly.

test_that("a Selection fit carries the draws contract and nothing beyond it", {
  skip_if_not_installed("amen")
  fit <- .selection_fixture_fit("bin", n_actors = 6)

  # The draws object keeps genuinely per-draw values apart from quantities the
  # sampler reduced to posterior means. That separation is the whole point of
  # the class -- it is what stops a summary being reported as though its
  # uncertainty had been propagated -- so its shape is pinned.
  # `chain` is part of the contract because stacked draws from several chains
  # can be diagnosed per chain only if the chain of each draw travels with them.
  # It is NULL for a single-chain fit.
  expect_identical(
    sort(names(fit$draws)),
    sort(c("parameters", "summarized", "n_draws", "n_chains", "chain", "note"))
  )
  expect_true(is.matrix(fit$draws$parameters))

  # amen::ame() reports the actor effects as posterior means, and that is where
  # they live: in `summarized`, not among the per-draw parameters.
  expect_true(all(c("sender_effects", "receiver_effects") %in%
                    names(fit$draws$summarized)))
  expect_false(any(c("sender_effects", "receiver_effects") %in%
                     colnames(fit$draws$parameters)))
})

test_that("fit_selection() takes exactly the arguments it documents", {
  expect_identical(
    names(formals(fit_selection)),
    c("formula", "data", "family", "waves", "binarize", "R", "pool_beta",
      "symmetric", "pooling", "...")
  )
})

test_that("the Influence model's own exposure surface works", {
  # `exposure()` and `compute_exposure()` are Influence-model terms, part of the
  # package's exported surface alongside the Selection API.
  exports <- getNamespaceExports("rnetis")
  expect_true(all(c("exposure", "compute_exposure") %in% exports))
  expect_true(is.function(exposure))
})

test_that("standard AME selection fits and reports", {
  skip_if_not_installed("amen")
  fit <- .selection_fixture_fit("bin", n_actors = 6)
  expect_s3_class(fit, "netis_selection")
  expect_true(length(coef(fit)) > 0L)
  expect_s3_class(netis_estimates(fit), "data.frame")

  # The conditional estimand holds the actor effects at their posterior means,
  # and its coverage sentence must say so rather than leaving a narrower
  # interval to read as a fully propagated one.
  eff <- selection_effect(fit, term = "prior_tie", type = "conditional")
  expect_s3_class(eff, "netis_selection_effect")
  expect_match(eff$coverage, "posterior means")
})

test_that("hierarchical multi-group selection fits and reports", {
  skip_if_not_installed("amen")
  fit <- .hierarchical_fixture_fit()
  h <- netis_hierarchy(fit)
  expect_identical(h$covariance, "diagonal")
  expect_s3_class(h$population, "data.frame")
  expect_s3_class(h$groups, "data.frame")
})
