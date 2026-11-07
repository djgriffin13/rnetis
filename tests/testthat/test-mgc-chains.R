# Several chains in the hierarchical sampler: dispersed starts, chain identity,
# and convergence diagnostics that cover every quantity a fit reports.
#
# The chain-identity and coverage tests below fail if every chain starts from
# the same state, or if a fit's convergence diagnostics treat the stacked
# chains as one.

.chain_groups <- function() .mgc_moderator_groups(G = 4, n = 8, seed = 23)

test_that("chain 1 is unchanged by adding chains: same start, same draws", {
  skip_if_not_installed("amen")
  reg <- c(intercept = "random", x.row = "shared")
  one <- .mgc_sample(.chain_groups(), regime = reg, nscan = 40, burn = 20, odens = 5,
                     seed = 6, chains = 1L)
  two <- .mgc_sample(.chain_groups(), regime = reg, nscan = 40, burn = 20, odens = 5,
                     seed = 6, chains = 2L)
  first <- two$chain == 1L
  expect_identical(one$theta, two$theta[first, , , drop = FALSE])
  expect_identical(one$tau, two$tau[first, , drop = FALSE])
  expect_identical(two$starts, c("amen", "dispersed"))
})

test_that("dispersed starts are finite, valid, and move every coefficient", {
  skip_if_not_installed("amen")
  prep <- .mgc_prepare_groups(.chain_groups(), "bin", 0, NULL)
  theta <- t(vapply(prep, function(gr) .mgc_init_group(gr, "bin", 0)$theta, numeric(2)))
  rs <- list(tau2 = c(0.3, 0.2), lambda = c(1, 1), omega_y = 0, Omega = diag(2))
  set.seed(1)
  out <- .mgc_disperse_start(theta, rs, prep, "bin")
  expect_true(all(is.finite(out$theta)))
  expect_true(all(out$theta != theta))
  expect_true(all(is.finite(out$rs$tau2) & out$rs$tau2 > 0))
  # A valid correlation matrix: unit diagonal, positive definite.
  expect_equal(diag(out$rs$Omega), c(1, 1))
  expect_true(all(eigen(out$rs$Omega, symmetric = TRUE)$values > 0))
})

test_that("dispersal has a finite fallback when the groups' starts agree", {
  skip_if_not_installed("amen")
  prep <- .mgc_prepare_groups(.chain_groups(), "bin", 0, NULL)
  # Every group's start identical: the across-group spread is exactly zero, so
  # the reference scale is what disperses them.
  theta <- matrix(0.2, 4, 2, dimnames = list(names(prep), c("intercept", "x.row")))
  set.seed(2)
  out <- .mgc_disperse_start(theta, NULL, prep, "bin")
  expect_true(all(is.finite(out$theta)))
  expect_gt(stats::sd(out$theta[, "intercept"]), 0)
  # One group: no spread is defined at all.
  set.seed(3)
  one <- .mgc_disperse_start(theta[1, , drop = FALSE], NULL, prep[1], "bin")
  expect_true(all(is.finite(one$theta)))
})

test_that("a two-chain fit's diagnostics compare the chains, for every reported quantity", {
  skip_if_not_installed("amen")
  d <- .mgc_sample(.chain_groups(), regime = c(intercept = "random", x.row = "shared"),
                   nscan = 60, burn = 20, odens = 5, seed = 6, chains = 2L)
  tab <- .mgc_convergence_table(d, .mgc_starts_label(d))
  expect_identical(attr(tab, "n_chains"), 2L)
  expect_identical(attr(tab, "scope"), "between and within chains")
  expect_match(attr(tab, "note"), "dispersed values")
  fam <- table(tab$family)
  expect_equal(unname(fam[["shared"]]), 1)
  expect_equal(unname(fam[["population_mean"]]), 1)
  expect_equal(unname(fam[["between_group_sd"]]), 1)
  expect_equal(unname(fam[["group_coefficient"]]), 4)         # the random intercept, per group
  expect_equal(unname(fam[["nuisance"]]), 4 * 4)              # va, cab, vb, rho per group
  expect_true(all(c("mu:intercept", "tau:intercept", "intercept[a]", "rho[d]") %in% tab$parameter))
  # Diagnosed per chain: the same column diagnosed as one stacked chain differs.
  stacked <- .netis_mcmc_diagnostics(matrix(d$mu[, "intercept"], ncol = 1))
  per_chain <- tab$rhat[tab$parameter == "mu:intercept"]
  expect_false(isTRUE(all.equal(unname(stacked$rhat), per_chain)))
})

test_that("a held-fixed tau is reported as constant, not as converged", {
  skip_if_not_installed("amen")
  # The between-group update is replaced, since no fit holds tau fixed.
  testthat::local_mocked_bindings(.mgc_update_random = function(zeta_k, state, prior) {
    state$tau2 <- rep(0.4^2, ncol(as.matrix(zeta_k)))
    state
  })
  d <- .mgc_sample(.chain_groups(), regime = c(intercept = "random", x.row = "shared"),
                   nscan = 40, burn = 10, odens = 5, seed = 6)
  tab <- .mgc_convergence_table(d)
  expect_identical(tab$status[tab$parameter == "tau:intercept"], "constant")
  expect_true(is.na(tab$rhat[tab$parameter == "tau:intercept"]))
})
