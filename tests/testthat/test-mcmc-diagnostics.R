# The shared MCMC convergence diagnostics (R/mcmc-diagnostics.R):
# rank-normalized, folded split R-hat and bulk/tail ESS (Vehtari et al. 2021).
#
# The REFERENCE is the `posterior` package (1.7.0), which is not a dependency,
# so the broader cross-check against it is outside the package test suite. Four
# fixtures' reference values are pinned below: they were computed by posterior
# on those exact fixtures, which makes them reference values rather than
# remembered rnetis output.

.ar1_chains <- function(n_chains, n, phi, seed) {
  set.seed(seed)
  ar1 <- function() {
    x <- numeric(n)
    x[1] <- stats::rnorm(1)
    for (t in 2:n) x[t] <- phi * x[t - 1] + stats::rnorm(1, 0, sqrt(1 - phi^2))
    x
  }
  sapply(seq_len(n_chains), function(c) ar1())
}

test_that("R-hat and ESS reproduce the reference implementation's values", {
  x <- .ar1_chains(3, 200, 0.6, seed = 11)
  d <- .netis_mcmc_diagnostics(lapply(1:3, function(j) matrix(x[, j], ncol = 1)))
  # posterior 1.7.0: rhat(), ess_bulk(), ess_tail(), and the bulk and folded
  # components of rhat() on this fixture.
  expect_equal(unname(d$rhat), 1.012399422191, tolerance = 1e-10)
  expect_equal(unname(d$ess_bulk), 168.792797870174, tolerance = 1e-10)
  expect_equal(unname(d$ess_tail), 288.021439057697, tolerance = 1e-10)
  expect_equal(unname(d$rhat_bulk), 1.012399422191, tolerance = 1e-10)
  expect_equal(unname(d$rhat_folded), 1.001910188328, tolerance = 1e-10)
})

test_that("R-hat and ESS match the reference on autocorrelated, heavy-tailed and drifting chains", {
  # posterior 1.7.0 on these exact fixtures: a sticky chain, tails with no finite
  # variance, and one chain drifting away from the others.
  check <- function(m, rhat, bulk, tail) {
    d <- .netis_mcmc_diagnostics(lapply(seq_len(ncol(m)), function(j) m[, j, drop = FALSE]))
    expect_lt(abs(unname(d$rhat) - rhat), 1e-8)
    expect_lt(abs(unname(d$ess_bulk) - bulk), 1e-6)
    expect_lt(abs(unname(d$ess_tail) - tail), 1e-6)
  }
  set.seed(101)
  check(sapply(1:2, function(i) as.numeric(stats::filter(stats::rnorm(200), 0.7, "recursive"))),
        0.9965394077, 91.46023835, 191.14681997)
  set.seed(102)
  check(matrix(stats::rt(400, df = 2), 100, 4), 0.9976986909, 448.79090155, 371.00709849)
  set.seed(103)
  drift <- matrix(stats::rnorm(300), 100, 3)
  drift[, 3] <- drift[, 3] + seq(0, 2, length.out = 100)
  check(drift, 1.1775446605, 13.15467821, 40.90897465)
})

test_that("chains that disagree in LOCATION are detected by bulk R-hat", {
  set.seed(2)
  x <- sapply(c(0, 0, 0, 1.5), function(m) stats::rnorm(300, m))
  d <- .netis_mcmc_diagnostics(lapply(1:4, function(j) matrix(x[, j], ncol = 1)))
  expect_gt(unname(d$rhat_bulk), 1.1)
  expect_identical(unname(d$rhat), max(unname(d$rhat_bulk), unname(d$rhat_folded)))
})

test_that("chains that disagree only in SCALE are detected by folded R-hat, not bulk", {
  # The failure classic R-hat cannot see: every chain centered on the same value,
  # one four times as spread. Bulk R-hat stays near 1; folding catches it.
  set.seed(3)
  x <- sapply(c(1, 1, 1, 4), function(s) stats::rnorm(300, 0, s))
  d <- .netis_mcmc_diagnostics(lapply(1:4, function(j) matrix(x[, j], ncol = 1)))
  expect_lt(unname(d$rhat_bulk), 1.02)
  expect_gt(unname(d$rhat_folded), 1.1)
  expect_identical(unname(d$rhat), unname(d$rhat_folded))
})

test_that("chain identity is kept: a chain vector equals the per-chain list", {
  set.seed(4)
  x <- sapply(c(0, 0, 1), function(m) stats::rnorm(100, m))
  stacked <- matrix(c(x), ncol = 1, dimnames = list(NULL, "p"))
  by_vector <- .netis_mcmc_diagnostics(stacked, chain = rep(1:3, each = 100))
  by_list <- .netis_mcmc_diagnostics(lapply(1:3, function(j) matrix(x[, j], ncol = 1,
                                                                   dimnames = list(NULL, "p"))))
  expect_equal(by_vector$table, by_list$table)
  expect_identical(by_vector$n_chains, 3L)
  # Without the chain vector the stack is ONE chain: a different, weaker check
  # that must not be what a multi-chain fit reports.
  as_one <- .netis_mcmc_diagnostics(stacked)
  expect_identical(as_one$n_chains, 1L)
  expect_false(isTRUE(all.equal(unname(as_one$rhat), unname(by_vector$rhat))))
})

test_that("unavailable diagnostics are NA with a stated reason", {
  set.seed(5)
  good <- stats::rnorm(100)
  m <- cbind(ok = good, nonfinite = replace(good, 7, NA), constant = rep(2, 100))
  d <- .netis_mcmc_diagnostics(m, chain = rep(1:2, each = 50))
  expect_identical(unname(d$status), c("ok", "nonfinite", "constant"))
  expect_true(all(is.na(d$table[2:3, c("rhat", "rhat_bulk", "rhat_folded", "ess_bulk", "ess_tail")])))
  expect_true(all(is.finite(unlist(d$table[1, c("rhat", "ess_bulk", "ess_tail")]))))

  short <- .netis_mcmc_diagnostics(matrix(stats::rnorm(6), ncol = 1))
  expect_identical(unname(short$status), "too_few_draws")
  expect_true(is.na(short$rhat))
})

test_that("a single chain is reported as a within-chain check", {
  set.seed(6)
  d <- .netis_mcmc_diagnostics(matrix(stats::rnorm(200), ncol = 1))
  expect_identical(d$scope, "within one chain")
  expect_match(d$note, "One chain")
  d2 <- .netis_mcmc_diagnostics(matrix(stats::rnorm(200), ncol = 1), chain = rep(1:2, each = 100))
  expect_identical(d2$scope, "between and within chains")
})

test_that("chains of unequal length are refused rather than silently compared", {
  expect_error(
    .netis_mcmc_diagnostics(list(matrix(stats::rnorm(50)), matrix(stats::rnorm(40)))),
    "unequal lengths"
  )
})
