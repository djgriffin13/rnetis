# Every accessor must agree with coef()/summary() about whether a fit pooled.
#
# THE BUG THIS PINS. netis_estimates()/tidy(), netis_glance(),
# netis_estimand(), the estimand sentence and selection_effect()'s group
# refusal must not read `object$pooling` -- a field the fitted object never
# carries. The pooled result lives in `pooled_beta`. Reading the wrong field on
# the DEFAULT multi-group path (pool_beta = TRUE) makes coef() and summary()
# report the pooled vector while the tidy table returns per-group rows and
# netis_estimand() says "no information was pooled across groups": the outputs
# of one fit contradict each other.
#
# Built by hand from fixed draws rather than by running the sampler: the
# property is about which fields the accessors read, and a deterministic object
# makes every expected value computable in the test.

.pooled_fixture_extract <- function(draws) {
  list(
    coefficients = colMeans(draws),
    vcov = stats::cov(draws),
    draws = draws,
    variance_components = NULL,
    additive_effects = list(sender = NULL, receiver = NULL),
    multiplicative = NULL,
    convergence = list(n_draws = nrow(draws), note = "fixture"),
    symmetric = FALSE
  )
}

.pooled_fixture_y <- function() {
  Y <- matrix(0, 5, 5)
  diag(Y) <- NA
  Y
}

.pooled_fixture <- function(pool = TRUE) {
  set.seed(11)
  nms <- c("intercept", "snd_x", "rcv_x")
  mk <- function(mu, sd) {
    m <- sapply(seq_along(mu), function(k) stats::rnorm(200, mu[k], sd[k]))
    colnames(m) <- nms
    m
  }
  per_group <- list(
    A = list(extracted = .pooled_fixture_extract(mk(c(-1, 0.5, 0.2), c(0.2, 0.1, 0.3))),
             design = list(Y = .pooled_fixture_y()), ids = letters[1:5], n_excluded = 0, n_actors = 5L),
    B = list(extracted = .pooled_fixture_extract(mk(c(-0.6, 0.1, 0.4), c(0.3, 0.2, 0.1))),
             design = list(Y = .pooled_fixture_y()), ids = letters[6:10], n_excluded = 0, n_actors = 5L)
  )
  pooled <- if (pool) .pool_beta_precision(per_group) else NULL
  .assemble_netis_selection(
    spec = list(), call = quote(fit_selection(tie ~ sender(x))),
    per_group = per_group, groups = c("A", "B"),
    route = list(), family = "bin", binarize = NULL, R = 0,
    diagnostics = list(), pooled_beta = pooled
  )
}

test_that("netis_estimates() reports the pooled coefficients for a pooled fit", {
  skip_if_not_installed("amen")
  fit <- .pooled_fixture(pool = TRUE)
  est <- netis_estimates(fit)

  # One row per term: the pooled vector, not a row set per group.
  expect_identical(est$term, names(fit$pooled_beta$mean))
  expect_true(all(is.na(est$group)))
  expect_equal(est$estimate, unname(coef(fit)), tolerance = 1e-12)
  expect_equal(est$estimate, unname(fit$pooled_beta$mean), tolerance = 1e-12)
  # The pooled SE is what summary() prints; the interval is the normal one it
  # implies, because two-stage pooling produces moments and no pooled draws.
  se <- unname(sqrt(fit$pooled_beta$var))
  expect_equal(est$std.error, se, tolerance = 1e-12)
  z <- stats::qnorm(0.975)
  expect_equal(est$conf.low, est$estimate - z * se, tolerance = 1e-12)
  expect_equal(est$conf.high, est$estimate + z * se, tolerance = 1e-12)
})

test_that("tidy() agrees with netis_estimates() on a pooled fit", {
  skip_if_not_installed("amen")
  fit <- .pooled_fixture(pool = TRUE)
  expect_identical(tidy(fit), netis_estimates(fit))
})

test_that("netis_glance() and netis_estimand() say a pooled fit pooled", {
  skip_if_not_installed("amen")
  fit <- .pooled_fixture(pool = TRUE)
  g <- netis_glance(fit)
  expect_true(g$pooled)
  expect_identical(g$estimand, "tie formation")

  e <- netis_estimand(fit)
  expect_identical(e$estimand, "tie formation")
  expect_identical(e$pooling, "two_stage")
  expect_identical(e$pooled_parameters, names(fit$pooled_beta$mean))
  expect_no_match(e$description, "no information was pooled", fixed = TRUE)
  expect_match(e$description, "two-stage", fixed = TRUE)
  # It must not describe a joint model this package does not fit.
  expect_no_match(e$description, "one chain", fixed = TRUE)
})

test_that("an unpooled multi-group fit still reports per group", {
  skip_if_not_installed("amen")
  fit <- .pooled_fixture(pool = FALSE)
  est <- netis_estimates(fit)
  expect_setequal(unique(est$group), c("A", "B"))
  expect_false(netis_glance(fit)$pooled)
  expect_identical(netis_glance(fit)$estimand, "per-group tie formation")
  e <- netis_estimand(fit)
  expect_identical(e$pooling, "none")
  expect_identical(e$pooled_parameters, character(0))
  expect_match(e$description, "no information was pooled", fixed = TRUE)
})

test_that("selection_effect()'s group refusal names the pooling actually used", {
  skip_if_not_installed("amen")
  expect_error(.resolve_effect_group(.pooled_fixture(pool = TRUE), NULL),
               "with pooled coefficients", fixed = TRUE)
  expect_error(.resolve_effect_group(.pooled_fixture(pool = FALSE), NULL),
               "estimated independently", fixed = TRUE)
})

test_that("pooling over a single group is not reported as pooling", {
  # One group: the combination is the identity (the K = 1 reduction), so the
  # fit is an ordinary single-group fit and every accessor must say so.
  fit <- list(n_groups = 1L, groups = "A", pooled_beta = list(mean = c(a = 1)))
  expect_false(.selection_is_pooled(fit))
  expect_false(.selection_is_pooled(list(n_groups = 2L, pooled_beta = NULL)))
  expect_true(.selection_is_pooled(list(n_groups = 2L, pooled_beta = list(mean = 1))))
})
