# PORTABLE MULTI-GROUP CORE (mgc) -- between-group (hyperparameter) updates.
# See mgc-regime.R for what this core may depend on.
#
# THE MODEL for each random coefficient k (independently across k):
#
#     theta_kg = mu_k + zeta_kg,   zeta_kg ~ N(0, tau_k^2),   g = 1..G
#     mu_k     ~ N(mu_mean, mu_sd^2)
#     tau_k    ~ half-t(nu, A)
#
# WHERE mu IS DRAWN, and why not here. mu_k is NOT updated in this file. It is
# a shared parameter of the coefficient kernel (mgc-regime.R,
# .mgc_expanded_map): every group's likelihood sees mu_k + zeta_kg, so mu_k,
# the shared slopes and every group's zeta_g are drawn in ONE exact joint
# Gaussian draw. Drawing mu_k here from the current theta_kg -- the textbook
# centered Gibbs step -- mixes badly when tau is small: mu | theta and
# theta | mu pin each other, mu moves by about tau / sqrt(G) per scan, and as
# tau -> 0 the chain effectively stops. The joint draw removes that coupling
# for every tau. What remains here is tau | zeta, which is exactly what mixing
# diagnostics for small tau should watch (the hierarchical funnel): the tau
# chain can still mix poorly when tau is small.
#
# THE HALF-t IS SAMPLED BY GIBBS through its scale-mixture representation
# (Wand, Ormerod, Padoan & Fruhwirth 2011; Makalic & Schmidt 2016, A simple
# sampler for the horseshoe estimator, IEEE Signal Processing Letters 23(1),
# 179-182):
#
#     tau^2 | lambda ~ InvGamma(nu / 2, nu / lambda)
#     lambda         ~ InvGamma(1 / 2, 1 / A^2)
#
# marginalizes to tau ~ half-t(nu, A). Both full conditionals are then
# inverse-gamma:
#
#     tau^2  | zeta, lambda ~ InvGamma((nu + G) / 2, nu / lambda + sum_g zeta_g^2 / 2)
#     lambda | tau^2        ~ InvGamma((nu + 1) / 2, nu / tau^2 + 1 / A^2)
#
# TESTED against an independent reference (tests/testthat/comprehensive/
# test-mgc-hyper-invariance.R): with zeta fixed, the draws of
# tau match the exact conditional posterior computed by quadrature, and the
# test is shown to catch a deliberately wrong shape ((nu + G - 1)/2).
#
# GROUP-LEVEL PREDICTORS (moderators) do not change this file: mu_k becomes
# w_g' gamma_k, i.e. more shared columns in the kernel's expanded map.

#' One Gibbs sweep of a random coefficient's between-group SD
#'
#' @param zeta_k a G x q matrix of current group deviations `theta_kg - mu_k`,
#'   one column per random coefficient.
#' @param state a list with `tau2` and `lambda`, each length q.
#' @param prior the resolved between-group prior (`.mgc_resolve_priors()$random`).
#' @return the updated state.
#' @noRd
.mgc_update_random <- function(zeta_k, state, prior) {
  # zeta_k is G x q: one column per random coefficient. INDEPENDENT between-group
  # SDs -- the between-group covariance is DIAGONAL here, so each column's
  # tau_k and lambda_k are drawn from its own conditional and no
  # cross-coefficient term exists. Correlated random coefficients are a
  # different model, updated in mgc-hyper-correlated.R, never approximated by
  # this diagonal.
  zeta_k <- as.matrix(zeta_k)
  q <- ncol(zeta_k)
  G <- nrow(zeta_k)
  A <- rep(prior$A, length.out = q)
  for (k in seq_len(q)) {
    state$tau2[k] <- 1 / stats::rgamma(
      1, shape = (prior$nu + G) / 2,
      rate = prior$nu / state$lambda[k] + sum(zeta_k[, k]^2) / 2)
    state$lambda[k] <- 1 / stats::rgamma(
      1, shape = (prior$nu + 1) / 2,
      rate = prior$nu / state$tau2[k] + 1 / A[k]^2)
  }
  state
}
