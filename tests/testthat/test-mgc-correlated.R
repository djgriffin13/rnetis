# A correlated between-group covariance.
#
#   zeta_g ~ N(0, Sigma),  Sigma = diag(tau) Omega diag(tau)
#
# These tests cover the parameterization and the reductions; recovery at
# realistic G is a simulation question outside the package test suite, not a
# unit-test one.

test_that("the unconstrained map always yields a valid correlation matrix", {
  # EVERY y IS VALID, which is the property the sampler relies on: it never
  # proposes an invalid Omega, so no rejection step exists for validity alone
  # and an acceptance rate means what it says.
  set.seed(4)
  for (q in 2:4) {
    for (rep in 1:20) {
      y <- stats::rnorm(q * (q - 1) / 2, 0, 3)   # deliberately extreme
      om <- .mgc_omega_unconstrained(y, q)
      expect_equal(diag(om$Omega), rep(1, q), info = paste(q, rep))
      expect_equal(om$Omega, t(om$Omega), info = paste(q, rep))
      expect_true(all(eigen(om$Omega, symmetric = TRUE, only.values = TRUE)$values > 0),
                  info = paste(q, rep))
      expect_true(all(abs(om$Omega[upper.tri(om$Omega)]) < 1), info = paste(q, rep))
      # And L really is its Cholesky factor, rather than a matrix that happens
      # to be lower triangular.
      expect_equal(tcrossprod(om$L), om$Omega, tolerance = 1e-12, info = paste(q, rep))
    }
  }
})

test_that("q = 2 has the correlation written out by hand", {
  # KNOWN ANSWER. With two coefficients there is one free parameter and the
  # construction collapses to Omega[1,2] = tanh(y): L = [[1,0],[z,sqrt(1-z^2)]],
  # so the off-diagonal of L L' is z exactly.
  for (y in c(-2, -0.5, 0, 0.5, 2)) {
    om <- .mgc_omega_unconstrained(y, 2L)
    expect_equal(om$Omega[1, 2], tanh(y))
    expect_equal(om$Omega[2, 1], tanh(y))
  }
  # y = 0 is the identity, which is the diagonal model: the correlated
  # parameterization CONTAINS the independent one rather than replacing it.
  expect_equal(.mgc_omega_unconstrained(0, 2L)$Omega, diag(2))
})

test_that("the zeta log density matches a direct multivariate normal", {
  # Computed against dmvnorm written out from first principles, so the
  # factored form is checked by arithmetic that shares none of its reasoning.
  set.seed(7)
  q <- 3; G <- 9
  zeta <- matrix(stats::rnorm(G * q), G, q)
  tau <- c(0.4, 1.2, 0.7)
  Om <- .mgc_omega_unconstrained(c(0.6, -0.3, 0.9), q)$Omega
  Sigma <- outer(tau, tau) * Om

  direct <- sum(apply(zeta, 1, function(z) {
    -0.5 * as.numeric(determinant(Sigma, logarithm = TRUE)$modulus) -
      0.5 * as.numeric(t(z) %*% solve(Sigma) %*% z)
  }))
  expect_equal(.mgc_zeta_logdens(zeta, tau, Om), direct, tolerance = 1e-10)
})

test_that("at q = 1 the correlated update IS the independent update, RNG included", {
  # THE REDUCTION, and it is bit-level rather than approximate. With one
  # coefficient Omega is 1x1 with no free parameter, so the model is exactly
  # the independent-coefficient model's -- and its exact Gibbs step is strictly better than a
  # Metropolis step at the same target. Re-deriving the scalar case here would
  # be a second implementation of a working thing, and the two would agree
  # until they did not.
  #
  # Identical RNG consumption is the part that makes a whole q = 1 FIT
  # bit-identical to an independent-coefficient fit, not merely close.
  zeta <- matrix(c(0.3, -0.2, 0.5, 0.1, -0.6, 0.2), ncol = 1)
  prior <- list(nu = 3, A = 0.5)
  st <- list(tau2 = 0.25, lambda = 0.25)

  set.seed(99); a <- .mgc_update_random(zeta, st, prior);      ra <- stats::runif(3)
  set.seed(99); b <- .mgc_update_random_corr(zeta, st, prior); rb <- stats::runif(3)
  expect_identical(a$tau2, b$tau2)
  expect_identical(a$lambda, b$lambda)
  expect_identical(ra, rb)
})

test_that("Sigma is diag(tau) Omega diag(tau), and the diagonal case is exact", {
  st <- list(tau2 = c(0.25, 0.04), Omega = .mgc_omega_unconstrained(0.8, 2L)$Omega)
  S <- .mgc_sigma(st, 2L)
  expect_equal(diag(S), c(0.25, 0.04))
  expect_equal(S[1, 2] / (0.5 * 0.2), tanh(0.8))

  # With no Omega recorded the covariance is diagonal EXACTLY -- the zeros are
  # structural, not a correlation that happened to be estimated near zero.
  st0 <- list(tau2 = c(0.25, 0.04))
  expect_identical(.mgc_sigma(st0, 2L)[1, 2], 0)
})

test_that("a Metropolis step that never moves is visible in the state", {
  # ACCEPTANCE IS REPORTED, NOT ASSUMED. A step that never accepts returns the
  # correlation it started from and has no other symptom: the chain looks
  # stable, the posterior looks tight, and the number is the initial value.
  # Driven here with a step size of zero, which is the degenerate version of
  # that failure -- the proposal is always the current state.
  set.seed(3)
  zeta <- matrix(stats::rnorm(12), 6, 2)
  prior <- list(nu = 3, A = c(0.5, 0.5), eta = 2)
  st <- list(tau2 = c(0.25, 0.25), lambda = c(1, 1), omega_y = 0.5,
             step_tau = 0, step_omega = 0)
  out <- .mgc_update_random_corr(zeta, st, prior)

  expect_equal(out$n_sweeps, 1)
  # The counts exist and the correlation is unchanged, so a diagnostic can tell
  # "did not move" from "moved and came back".
  expect_equal(out$omega_y, 0.5)
  expect_equal(out$tau2, c(0.25, 0.25))
})

test_that("the sampler moves toward a correlation that is really there", {
  # NOT A RECOVERY CLAIM -- that is a simulation question at realistic G. This asks
  # only whether the Metropolis step is wired up: given deviations generated
  # with a strong positive correlation, and started at zero, the chain should
  # move away from zero in the right direction.
  set.seed(11)
  q <- 2; G <- 60
  Om_true <- matrix(c(1, 0.8, 0.8, 1), 2, 2)
  tau_true <- c(0.5, 0.5)
  Sig <- outer(tau_true, tau_true) * Om_true
  zeta <- matrix(stats::rnorm(G * q), G, q) %*% chol(Sig)

  prior <- list(nu = 3, A = c(0.5, 0.5), eta = 2)
  st <- list(tau2 = c(0.25, 0.25), lambda = c(1, 1), omega_y = 0)
  keep <- numeric(400)
  for (i in seq_len(400)) {
    st <- .mgc_update_random_corr(zeta, st, prior)
    keep[i] <- st$Omega[1, 2]
  }
  # Moved, and in the right direction. The last half is used so the start value
  # is not being averaged in.
  expect_gt(mean(keep[201:400]), 0.5)
  expect_gt(st$omega_accept, 0)
})

# --- wired into the sampler -------------------------------------------------

test_that("an unstructured fit with one random coefficient is refused", {
  # A CORRELATION NEEDS TWO THINGS TO CORRELATE. With one random coefficient
  # Omega is 1x1 with no free parameter, so accepting the word would let a
  # caller believe they had asked for something they did not get -- the fit
  # would run, report a "correlated" covariance, and be the diagonal model.
  expect_error(
    .mgc_regime(c("intercept", "x"), c("group", "random"), covariance = "unstructured"),
    "at least two random coefficients")
  expect_error(
    .mgc_regime(c("intercept", "x"), c("group", "random"), covariance = "banded"),
    "diagonal.*unstructured")
})

test_that("a two-coefficient unstructured fit runs and reports its covariance", {
  skip_if_not_installed("amen")
  gs <- .mgc_moderator_groups(G = 4, n = 8, seed = 31)
  reg <- c(intercept = "group", x.row = "random", .dyad = "random")
  # A second random coefficient: a dyadic prior tie, so the two differ in type.
  gs <- lapply(gs, function(g) {
    W <- matrix(stats::rbinom(64, 1, 0.3), 8); diag(W) <- 0
    g$Xdyad <- array(W, c(8, 8, 1), dimnames = list(NULL, NULL, "prior"))
    g
  })
  fit <- .mgc_sample(gs, regime = reg, family = "bin", R = 0L,
                     nscan = 60, burn = 20, odens = 5, seed = 5,
                     covariance = "unstructured")

  expect_identical(fit$covariance, "unstructured")
  expect_equal(dim(fit$Omega), c(nrow(fit$tau), 2L, 2L))
  # Every retained draw is a valid correlation matrix, not merely the last one.
  for (s in seq_len(dim(fit$Omega)[1])) {
    Om <- fit$Omega[s, , ]
    expect_equal(unname(diag(Om)), c(1, 1), info = as.character(s))
    expect_true(abs(Om[1, 2]) < 1, info = as.character(s))
  }
})

test_that("a diagonal fit carries no Omega at all", {
  skip_if_not_installed("amen")
  gs <- .mgc_moderator_groups(G = 3, n = 8, seed = 32)
  fit <- .mgc_sample(gs, regime = c(intercept = "group", x.row = "random"),
                     family = "bin", R = 0L, nscan = 40, burn = 10, odens = 5,
                     seed = 6)
  # ABSENT, NOT AN IDENTITY MATRIX. An identity would read as "the correlations
  # were estimated and came out at zero"; NULL says the model has none. Same
  # rule as components a fit cannot produce.
  expect_null(fit$Omega)
  expect_identical(fit$covariance, "diagonal")
})

test_that("the dense block prior reduces to the diagonal one exactly", {
  # THE KERNEL SEES THE SAME MATRIX EITHER WAY when Sigma is diagonal, so an
  # unstructured fit at zero correlation cannot differ from a diagonal fit
  # through this path. Checked as arithmetic rather than assumed, because the
  # dense branch inverts where the diagonal branch reciprocates.
  reg <- .mgc_regime(c("intercept", "x", "w"), c("group", "random", "random"),
                     covariance = "unstructured")
  static <- list(P = matrix(4, 1, 1), m = 0.2)
  tau2 <- c(0.25, 0.04)
  a <- .mgc_block_prior(static, reg, tau2 = tau2)
  b <- .mgc_block_prior(static, reg, tau2 = tau2, Sigma = diag(tau2))
  expect_equal(a$P, b$P, tolerance = 1e-12)
  expect_equal(a$m, b$m)
})

test_that("Omega and acceptance are combined across chains, not taken from the first", {
  # THE DEFECT THIS PINS WAS SILENT AND FOUND BY ACCIDENT. .mgc_combine_chains()
  # seeds its result from runs[[1]], so any field it does not explicitly reduce
  # silently describes CHAIN 1 ONLY. Omega was such a field: a four-chain fit
  # returned 10 correlation draws beside 40 draws of every other parameter.
  #
  # Nothing in the fit announced it. It surfaced because a convergence
  # diagnostic split 10 draws by a 40-long chain vector and returned NA -- and
  # an NA R-hat reads like a diagnostic quirk, not like half the posterior
  # missing. Had the chain count divided evenly it would have returned a
  # plausible number computed from one chain's draws relabeled as four.
  skip_if_not_installed("amen")
  gs <- .mgc_moderator_groups(G = 4, n = 8, seed = 41)
  gs <- lapply(gs, function(g) {
    W <- matrix(stats::rbinom(64, 1, 0.3), 8); diag(W) <- 0
    g$Xdyad <- array(W, c(8, 8, 1), dimnames = list(NULL, NULL, "prior"))
    g
  })
  fit <- .mgc_sample(gs, regime = c(intercept = "group", x.row = "random", .dyad = "random"),
                     family = "bin", R = 0L, nscan = 40, burn = 10, odens = 5,
                     seed = 9, chains = 3L, covariance = "unstructured")

  # One row of Omega per retained draw, and the draw count is what `chain` says.
  expect_equal(dim(fit$Omega)[1], length(fit$chain))
  expect_equal(dim(fit$Omega)[1], nrow(fit$tau))
  expect_equal(length(unique(fit$chain)), 3L)

  # Acceptance covers every chain's sweeps, not the first chain's -- and the
  # retained phase (nscan = 40 per chain) is counted apart from burn-in (10 per
  # chain), because only the retained draws come from the frozen kernel.
  expect_equal(fit$acceptance$chains, 3L)
  expect_equal(fit$acceptance$sweeps, 3L * 40L)
  expect_equal(fit$acceptance$warmup$sweeps, 3L * 10L)
  expect_true(fit$acceptance$omega >= 0 && fit$acceptance$omega <= 1)
})

test_that("a per-draw field that disagrees with the chain vector is refused", {
  # THE CLASS, not the instance. The validator's named checks covered the
  # fields that existed when it was written; gamma and Omega were added later
  # and neither was covered, which is how a one-chain Omega shipped beside a
  # four-chain everything-else. Enumerating fields in the validator would
  # reproduce exactly the omission that caused the bug, so the check is over
  # whatever per-draw fields are present.
  skip_if_not_installed("amen")
  gs <- .mgc_moderator_groups(G = 3, n = 8, seed = 44)
  gs <- lapply(gs, function(g) {
    W <- matrix(stats::rbinom(64, 1, 0.3), 8); diag(W) <- 0
    g$Xdyad <- array(W, c(8, 8, 1), dimnames = list(NULL, NULL, "prior"))
    g
  })
  fit <- .mgc_sample(gs, regime = c(intercept = "group", x.row = "random", .dyad = "random"),
                     family = "bin", R = 0L, nscan = 30, burn = 10, odens = 5,
                     seed = 12, chains = 2L, covariance = "unstructured")
  expect_silent(.mgc_validate_draws(fit))

  # Truncated to one chain's worth, which is precisely what the uncombined
  # field looked like -- real values, wrong shape, no error anywhere.
  broken <- fit
  broken$Omega <- fit$Omega[seq_len(dim(fit$Omega)[1] / 2), , , drop = FALSE]
  expect_error(.mgc_validate_draws(broken), "retained draw")
})

test_that("the LKJ default is eta = 1, and eta changes the target it should", {
  # A USER-VISIBLE DEFAULT, pinned. eta = 1 is uniform over correlation
  # matrices, so the prior contributes the least shrinkage the parameterization
  # allows; eta = 2 remains available as a regularizing sensitivity option.
  # A deliberate choice over the stronger default, NOT a correction of a
  # defect.
  #
  # Pinned because a prior default is exactly the kind of thing that drifts in
  # a refactor while every test still passes: nothing else in the suite would
  # notice, and a fit would quietly carry more shrinkage than was chosen.
  set.seed(5)
  zeta <- matrix(stats::rnorm(24), 12, 2)
  base <- list(nu = 3, A = c(0.5, 0.5))
  st <- list(tau2 = c(0.25, 0.25), lambda = c(1, 1), omega_y = 0.9,
             step_tau = 0, step_omega = 0)

  # With eta absent the density used must be the eta = 1 one, and under eta = 1
  # the LKJ term is identically zero for every Omega -- uniform means uniform.
  Om <- .mgc_omega_unconstrained(0.9, 2L)$Omega
  expect_equal(.mgc_lkj_logdens(Om, 1), 0)
  expect_lt(.mgc_lkj_logdens(Om, 2), 0)   # eta = 2 penalizes a correlated Omega

  # The default really is 1: an explicit eta = 1 and no eta at all must give
  # the same draw from the same seed, and eta = 2 must not.
  set.seed(8); a <- .mgc_update_random_corr(zeta, st, base)
  set.seed(8); b <- .mgc_update_random_corr(zeta, st, c(base, list(eta = 1)))
  set.seed(8); d <- .mgc_update_random_corr(zeta, st, c(base, list(eta = 6)))
  expect_identical(a$omega_y, b$omega_y)
  # With a zero step size the chain cannot move, so the check that eta MATTERS
  # is on the density rather than on the draw.
  expect_false(isTRUE(all.equal(.mgc_lkj_logdens(Om, 1), .mgc_lkj_logdens(Om, 6))))
  expect_identical(d$tau2, a$tau2)
})

# --- the prior the update targets --------------------------------------------

test_that("the log Jacobian is that of y -> Omega's off-diagonal elements", {
  # Guards against omitting the L -> Omega factor. It is 1 at q = 2, so only
  # q >= 3 shows the defect; checked against a numerical Jacobian that
  # shares none of the derivation.
  skip_if_not_installed("numDeriv")
  for (q in 2:4) {
    set.seed(10 + q)
    y <- stats::rnorm(q * (q - 1) / 2, 0, 0.7)
    offdiag <- function(v) {
      Om <- .mgc_omega_unconstrained(v, q)$Omega
      Om[lower.tri(Om)]
    }
    J <- numDeriv::jacobian(offdiag, y)
    expect_equal(.mgc_omega_unconstrained(y, q)$log_jacobian, log(abs(det(J))),
                 tolerance = 1e-6, info = sprintf("q = %d", q))
  }
})

test_that("the target on the unconstrained scale is the LKJ partial-correlation form", {
  # Under LKJ(eta) the canonical partial correlation in column j is
  # Beta(b_j, b_j) on (-1, 1) with b_j = eta + (q - 1 - j)/2 (Lewandowski,
  # Kurowicka and Joe 2009), so on y = atanh(z) the log target is
  # sum_j b_j log(1 - z^2) up to a constant. Compared as differences between two
  # points, which removes the constant.
  target <- function(y, q, eta) {
    om <- .mgc_omega_unconstrained(y, q)
    .mgc_lkj_logdens(om$Omega, eta) + om$log_jacobian
  }
  reference <- function(y, q, eta) {
    z <- tanh(y)
    col_of <- unlist(lapply(2:q, function(i) seq_len(i - 1L)))
    sum((eta + (q - 1 - col_of) / 2) * log1p(-z^2))
  }
  for (q in 3:4) for (eta in c(1, 2)) {
    set.seed(q * 10 + eta)
    y1 <- stats::rnorm(q * (q - 1) / 2)
    y2 <- stats::rnorm(q * (q - 1) / 2)
    expect_equal(target(y1, q, eta) - target(y2, q, eta),
                 reference(y1, q, eta) - reference(y2, q, eta),
                 tolerance = 1e-10, info = sprintf("q = %d, eta = %g", q, eta))
  }
})

# --- eta on the public surface ------------------------------------------------

test_that("eta is validated, recorded, and refused where it would be ignored", {
  p <- selection_pooling(random = c("a", "b"), covariance = "unstructured", eta = 2)
  expect_identical(p$eta, 2)
  expect_identical(selection_pooling(random = "a")$eta, 1)
  expect_error(selection_pooling(random = c("a", "b"), covariance = "unstructured", eta = 0),
               "finite positive")
  expect_error(selection_pooling(random = c("a", "b"), covariance = "unstructured", eta = Inf),
               "finite positive")
  expect_error(selection_pooling(random = c("a", "b"), covariance = "unstructured", eta = c(1, 2)),
               "finite positive")
  expect_error(selection_pooling(random = c("a", "b"), eta = 2),
               "only covariance = \"unstructured\"")
})

test_that("an omitted eta and an explicit eta = 1 give the same draws", {
  skip_if_not_installed("amen")
  gs <- .mgc_moderator_groups(G = 4, n = 8, seed = 41)
  reg <- c(intercept = "random", x.row = "random")
  a <- .mgc_sample(gs, regime = reg, nscan = 30, burn = 60, odens = 5, seed = 2,
                   covariance = "unstructured")
  b <- .mgc_sample(gs, regime = reg, nscan = 30, burn = 60, odens = 5, seed = 2,
                   covariance = "unstructured", hyper = list(eta = 1))
  expect_identical(a$Omega, b$Omega)
  expect_identical(a$theta, b$theta)
  expect_identical(a$priors$random$eta, 1)
})

# --- proposal-scale adaptation: burn-in only ------------------------------------

test_that("the adaptation plan follows the proposal blocks' dimensions", {
  reg2 <- .mgc_regime(c("intercept", "a"), c(intercept = "random", a = "random"),
                      covariance = "unstructured")
  plan <- .mgc_adaptation_plan(reg2, burn = 500)
  expect_true(plan$active)
  expect_equal(plan$target_tau, 0.234 + 0.206 / 2)   # log tau block, d = q = 2
  expect_equal(plan$target_omega, 0.44)              # one correlation, d = 1
  expect_false(.mgc_adaptation_plan(reg2, burn = 20)$active)
  regd <- .mgc_regime(c("intercept", "a"), c(intercept = "random", a = "random"))
  expect_null(.mgc_adaptation_plan(regd, burn = 500))
})

test_that("a batch boundary moves each scale toward its target, and nothing else does", {
  plan <- list(active = TRUE, batch = 50L, target_tau = 0.3, target_omega = 0.44,
               last = c(tau = 0, omega = 0, sweeps = 0))
  rs <- list(step_tau = 0.3, step_omega = 0.3, tau_accept = 45, omega_accept = 5, n_sweeps = 50)
  out <- .mgc_adapt_step(rs, plan, s = 50L)
  expect_gt(out$step_tau, 0.3)     # 90% acceptance: steps too small, grow
  expect_lt(out$step_omega, 0.3)   # 10% acceptance: steps too large, shrink
  expect_identical(.mgc_adapt_step(rs, plan, s = 49L), rs)
  expect_identical(.mgc_adapt_step(rs, utils::modifyList(plan, list(active = FALSE)), s = 50L), rs)
})

test_that("the scales are frozen at the end of burn-in", {
  # Same seed and burn-in, different numbers of retained scans: if anything
  # adapted after burn-in, the final scales would depend on nscan. They must
  # not, and the shared retained prefix must be draw-for-draw identical.
  skip_if_not_installed("amen")
  gs <- .mgc_moderator_groups(G = 4, n = 8, seed = 41)
  reg <- c(intercept = "random", x.row = "random")
  short <- .mgc_sample(gs, regime = reg, nscan = 30, burn = 100, odens = 5, seed = 4,
                       covariance = "unstructured")
  long <- .mgc_sample(gs, regime = reg, nscan = 60, burn = 100, odens = 5, seed = 4,
                      covariance = "unstructured")
  expect_identical(short$acceptance$steps$tau, long$acceptance$steps$tau)
  expect_identical(short$acceptance$steps$omega, long$acceptance$steps$omega)
  expect_true(short$acceptance$steps$adapted)
  expect_identical(short$Omega, long$Omega[seq_len(dim(short$Omega)[1]), , , drop = FALSE])
  expect_equal(short$acceptance$warmup$sweeps, 100)
  expect_equal(short$acceptance$sweeps, 30)
})

test_that("a burn-in shorter than one batch leaves the starting scales in place", {
  skip_if_not_installed("amen")
  gs <- .mgc_moderator_groups(G = 4, n = 8, seed = 41)
  fit <- .mgc_sample(gs, regime = c(intercept = "random", x.row = "random"),
                     nscan = 20, burn = 49, odens = 5, seed = 4, covariance = "unstructured")
  expect_false(fit$acceptance$steps$adapted)
  expect_identical(fit$acceptance$steps$tau, 0.3)
  expect_identical(fit$acceptance$steps$omega, 0.3)
})

test_that("the resolved prior has no setting that holds the sampler fixed", {
  skip_if_not_installed("amen")
  prep <- .mgc_prepare_groups(.mgc_moderator_groups(G = 4, n = 8, seed = 41), "bin", 0, NULL)
  reg <- .mgc_regime(dimnames(prep[[1]]$X)[[3]], c(intercept = "random", x.row = "shared"))
  pri <- .mgc_resolve_priors(prep, reg, "bin",
                             hyper = list(fix_tau = 0.4, step_tau = 0.2, step_omega = 0.2))
  expect_false(any(c("fix_tau", "step_tau", "step_omega") %in% names(pri$random)))
})

test_that("on a fixed hyper-layer target, adapted scales give moderate acceptance", {
  # Fixture-specific and short: zeta given (G = 60), the update alone, steps
  # started far too small (0.02, which accepts almost everything). After 500
  # adapting sweeps and 1000 frozen ones, acceptance should sit in a broad band
  # around the targets -- the check is that adaptation moved the scale into
  # the useful region, not that it hit a number.
  set.seed(12)
  q <- 2L
  zeta <- matrix(stats::rnorm(60 * q), 60, q) %*% chol(matrix(c(1, 0.5, 0.5, 1), 2) * 0.25)
  prior <- list(nu = 3, A = c(0.5, 0.5), eta = 1)
  plan <- list(active = TRUE, batch = 50L, target_tau = .mgc_mh_target(q),
               target_omega = .mgc_mh_target(1), last = c(tau = 0, omega = 0, sweeps = 0))
  st <- list(tau2 = c(0.25, 0.25), lambda = c(1, 1), omega_y = 0,
             step_tau = 0.02, step_omega = 0.02, tau_accept = 0, omega_accept = 0, n_sweeps = 0)
  for (s in 1:500) {
    st <- .mgc_update_random_corr(zeta, st, prior)
    st <- .mgc_adapt_step(st, plan, s)
  }
  frozen <- c(st$step_tau, st$step_omega)
  st$tau_accept <- 0; st$omega_accept <- 0; st$n_sweeps <- 0
  for (s in 1:1000) st <- .mgc_update_random_corr(zeta, st, prior)
  expect_identical(c(st$step_tau, st$step_omega), frozen)
  expect_gt(st$step_tau, 0.02)
  acc <- c(st$tau_accept, st$omega_accept) / st$n_sweeps
  expect_true(all(acc > 0.15 & acc < 0.65), info = paste(round(acc, 3), collapse = ", "))
})
