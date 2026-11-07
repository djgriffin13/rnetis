# Does the between-group (hyperparameter) layer of the hierarchical sampler
# target the distribution it claims to? Two checks, each with its reference
# distribution stated before any statistic, and each shown able to fail.
#
# 1. DIAGONAL (half-t Gibbs). With the group deviations zeta FIXED, the update
#    targets the conditional posterior
#
#        p(tau | zeta) ∝ tau^-G exp(-S / (2 tau^2)) (1 + tau^2/(nu A^2))^-((nu+1)/2),
#        S = sum(zeta^2),
#
#    a one-dimensional density computed here by numerical integration,
#    independently of the sampler. Statistics: E[log tau] and
#    P(tau <= exact posterior median) = 1/2, both with every moment finite.
#
# 2. UNSTRUCTURED (Metropolis on log tau and on the correlation matrix), q = 3.
#    A Geweke (2004) joint-distribution test: alternate an EXACT draw
#    zeta_g ~ N(0, Sigma(tau, Omega)) with one sampler update of (tau, Omega)
#    given zeta. Each step leaves the joint prior-and-data distribution
#    invariant, so (tau, Omega) must have the PRIOR marginals: tau_k ~
#    half-t(nu, A), and every correlation r with (r + 1)/2 ~ Beta(b, b),
#    b = eta - 1 + q/2, so E[r^2] = 1 / (2b + 1) = 1/4 at q = 3, eta = 1.
#    Statistics: E[log tau] pooled over k, and E[r_ij^2] for each pair.
#
# TOLERANCE. Each statistic is a z-score, (estimate - exact) / MCSE, with the
# MCSE from the effective sample size of the statistic's own draws. Passing
# requires |z| < 4.5 for every statistic (a Bonferroni-scale band over the
# handful tested here).
#
# THE CHECKS CAN FAIL. Each is rerun with a deliberately incorrect update and
# must then report |z| > 4.5:
#   1. the inverse-gamma shape written as (nu + G - 1)/2, the classic slip;
#   2. the change of variables without its L -> Omega factor -- a defect
#      that is invisible at q = 2.
#
# Full-model simulation-based calibration and recovery studies are NOT here;
# they belong to the separate simulation study.

.mcse_z <- function(x, exact) {
  ess <- .mcmc_ess_basic(matrix(x, ncol = 1))
  (mean(x) - exact) / (stats::sd(x) / sqrt(ess))
}

.half_t_mean_log <- function(nu, A) {
  stats::integrate(function(t) log(t) * 2 * stats::dt(t / A, df = nu) / A, 0, Inf,
                   rel.tol = 1e-10)$value
}

# --- 1. diagonal: conditional posterior by quadrature --------------------------

.diag_reference <- function(zeta, nu, A) {
  G <- length(zeta)
  S <- sum(zeta^2)
  # On u = log(tau), including the Jacobian e^u.
  logf <- function(u) {
    t <- exp(u)
    -G * u - S / (2 * t^2) - (nu + 1) / 2 * log1p(t^2 / (nu * A^2)) + u
  }
  mode_u <- stats::optimize(logf, c(-10, 5), maximum = TRUE)$maximum
  c0 <- logf(mode_u)
  dens <- function(u) exp(logf(u) - c0)
  lo <- mode_u - 8; hi <- mode_u + 8
  Z <- stats::integrate(dens, lo, hi, rel.tol = 1e-10)$value
  mean_log <- stats::integrate(function(u) u * dens(u), lo, hi, rel.tol = 1e-10)$value / Z
  cdf <- function(u) stats::integrate(dens, lo, u, rel.tol = 1e-10)$value / Z
  med <- stats::uniroot(function(u) cdf(u) - 0.5, c(lo, hi), tol = 1e-10)$root
  list(mean_log = mean_log, median = exp(med))
}

.run_diag <- function(update, zeta, prior, n_iter, seed) {
  set.seed(seed)
  st <- list(tau2 = 1, lambda = prior$A^2)
  out <- numeric(n_iter)
  for (i in seq_len(n_iter)) {
    st <- update(matrix(zeta, ncol = 1), st, prior)
    out[i] <- sqrt(st$tau2)
  }
  out[-seq_len(500)]
}

test_that("the half-t Gibbs step targets the exact conditional posterior of tau", {
  zeta <- c(0.31, -0.12, 0.45, -0.38, 0.05, 0.22, -0.27, 0.40)
  prior <- list(nu = 3, A = 0.5)
  ref <- .diag_reference(zeta, prior$nu, prior$A)
  tau <- .run_diag(.mgc_update_random, zeta, prior, 20500, seed = 101)
  z_mean <- .mcse_z(log(tau), ref$mean_log)
  z_med <- .mcse_z(as.numeric(tau <= ref$median), 0.5)
  expect_lt(abs(z_mean), 4.5)
  expect_lt(abs(z_med), 4.5)

  # A deliberately wrong update, built by editing the real one's source, must
  # be caught. The substitution is checked so the test cannot pass vacuously.
  # width.cutoff keeps each statement on one line, so the substitution sees it whole.
  src <- paste(deparse(body(.mgc_update_random), width.cutoff = 500L), collapse = "\n")
  bad_src <- sub("(prior$nu + G)/2", "(prior$nu + G - 1)/2", src, fixed = TRUE)
  expect_false(identical(src, bad_src))
  bad <- .mgc_update_random
  body(bad) <- parse(text = bad_src)[[1]]
  tau_bad <- .run_diag(bad, zeta, prior, 20500, seed = 101)
  expect_gt(abs(.mcse_z(log(tau_bad), ref$mean_log)), 4.5)
})

# --- 2. unstructured: Geweke joint-distribution invariance --------------------

.run_geweke_corr <- function(q, G, prior, n_iter, seed) {
  set.seed(seed)
  st <- list(tau2 = rep_len(prior$A^2, q), lambda = rep(1, q),
             omega_y = rep(0, q * (q - 1) / 2), step_tau = 0.6, step_omega = 0.6)
  st$Omega <- diag(q)
  pairs <- which(lower.tri(diag(q)), arr.ind = TRUE)
  out <- matrix(NA_real_, n_iter, 1 + nrow(pairs))
  for (i in seq_len(n_iter)) {
    Sig <- .mgc_sigma(st, q)
    zeta <- matrix(stats::rnorm(G * q), G, q) %*% chol(Sig)
    st <- .mgc_update_random_corr(zeta, st, prior)
    out[i, ] <- c(mean(log(sqrt(st$tau2))), st$Omega[pairs]^2)
  }
  colnames(out) <- c("mean_log_tau", sprintf("r2_%d%d", pairs[, 1], pairs[, 2]))
  out[-seq_len(1000), , drop = FALSE]
}

.geweke_z <- function(draws, q, eta, nu, A) {
  b <- eta - 1 + q / 2
  exact <- c(.half_t_mean_log(nu, A), rep(1 / (2 * b + 1), ncol(draws) - 1L))
  stats::setNames(vapply(seq_len(ncol(draws)), function(j) .mcse_z(draws[, j], exact[j]),
                         numeric(1)), colnames(draws))
}

test_that("the correlated update leaves the half-t and LKJ prior invariant (q = 3)", {
  q <- 3L
  prior <- list(nu = 3, A = rep(0.5, q), eta = 1)
  draws <- .run_geweke_corr(q, G = 5L, prior, n_iter = 41000, seed = 202)
  z <- .geweke_z(draws, q, prior$eta, prior$nu, 0.5)
  expect_true(all(abs(z) < 4.5), info = paste(names(z), round(z, 2), collapse = "; "))
})

test_that("the invariance test detects the missing L -> Omega Jacobian factor", {
  # The defective map: the same construction without the L -> Omega term.
  without_factor <- function(y, q) {
    if (q == 1L) return(list(Omega = matrix(1, 1, 1), L = matrix(1, 1, 1), log_jacobian = 0))
    z <- tanh(y)
    L <- matrix(0, q, q); L[1, 1] <- 1
    k <- 0L; logJ <- 0
    for (i in 2:q) {
      rem <- 1
      for (j in seq_len(i - 1L)) {
        k <- k + 1L
        L[i, j] <- z[k] * sqrt(rem)
        logJ <- logJ + log1p(-z[k]^2) + 0.5 * log(rem)
        rem <- rem * (1 - z[k]^2)
      }
      L[i, i] <- sqrt(rem)
    }
    Om <- tcrossprod(L); Om <- (Om + t(Om)) / 2; diag(Om) <- 1
    list(Omega = Om, L = L, log_jacobian = logJ)
  }
  testthat::local_mocked_bindings(.mgc_omega_unconstrained = without_factor)
  q <- 3L
  prior <- list(nu = 3, A = rep(0.5, q), eta = 1)
  draws <- .run_geweke_corr(q, G = 5L, prior, n_iter = 41000, seed = 202)
  z <- .geweke_z(draws, q, prior$eta, prior$nu, 0.5)
  # The first canonical partial correlation is the one the missing factor
  # mis-weights: its prior becomes Beta(1, 1), E[r^2] = 1/3 instead of 1/4.
  expect_gt(abs(z[["r2_21"]]), 4.5)
})
