# PORTABLE MULTI-GROUP CORE (mgc) -- correlated between-group covariance.
# See mgc-regime.R for what this core may depend on.
#
# THE MODEL. The diagonal model (mgc-hyper.R) gives each random coefficient
# its own independent between-group SD. Here the deviations share a
# covariance:
#
#     zeta_g ~ N(0, Sigma),   Sigma = diag(tau) Omega diag(tau)
#
# A DIAGONAL IS NOT AN APPROXIMATION TO THIS. Fitting a diagonal where the truth
# is correlated does not recover a slightly-wrong Sigma; it assumes the
# correlations away, which is a different model. That is why `covariance` is a
# declared field every consumer can read rather than an implementation detail.
#
# WHY SEPARATION (tau and Omega) RATHER THAN AN INVERSE-WISHART ON Sigma.
# An inverse-Wishart is conjugate and would be far less code. It is refused
# because it couples the scales to the correlations through a single degrees-of-
# freedom parameter: each tau_k's MARGINAL prior would then depend on q and on
# the correlation structure. That would discard the contrast-scale calibration
# of the between-group prior and its A0 = 0.5 default, invisibly in any single
# fit. Under separation each tau_k keeps EXACTLY the half-t prior it has in the
# diagonal model, which is also what makes the reductions below checkable.
#
# THE q = 1 REDUCTION IS EXACT BY CONSTRUCTION, NOT BY COINCIDENCE. With one
# random coefficient Omega is the 1x1 identity and carries no free parameter,
# so this file hands the draw straight back to the diagonal scalar path --
# including its RNG consumption, so a q = 1 fit is bit-identical to a
# diagonal-covariance fit under the same seed. Re-deriving the scalar case
# here would be a second implementation of a thing that already works, and the two would agree until
# they did not.

#' Unconstrained parameters to a correlation matrix, via canonical partial
#' correlations
#'
#' `y` (length q(q-1)/2) maps to partial correlations `tanh(y)` in (-1, 1),
#' which fill a lower-triangular factor whose rows are normalized to unit
#' length. Every `y` gives a valid correlation matrix and every correlation
#' matrix is reachable, so the sampler never proposes an invalid Omega and
#' never needs a rejection step for validity alone.
#'
#' THE JACOBIAN IS OF y -> Omega, because the LKJ density is a density over
#' Omega's free (off-diagonal) elements. It has three factors:
#'
#'   y -> z (tanh)               prod (1 - z^2)
#'   z -> L (row normalization)  prod over (i, j < i) of sqrt(remaining mass)
#'   L -> Omega = L L'           prod over i of L_ii^(q - i)
#'
#' The third factor is 1 when q = 2, so omitting it would leave q = 2 fits
#' unaffected while making the prior actually sampled for q >= 3 something
#' other than LKJ(eta). With all three, the
#' canonical partial correlation at column j has the marginal the LKJ
#' construction requires, Beta(eta + (q - 1 - j)/2) on (-1, 1) (Lewandowski,
#' Kurowicka & Joe 2009). A test checks the total against a numerical Jacobian.
#'
#' @param y unconstrained vector, filled column-major over the strict lower
#'   triangle.
#' @param q the number of random coefficients.
#' @return a list with `Omega`, its Cholesky factor `L` (lower), and
#'   `log_jacobian` of the map from `y` to Omega's off-diagonal elements.
#' @noRd
.mgc_omega_unconstrained <- function(y, q) {
  if (q == 1L) {
    return(list(Omega = matrix(1, 1, 1), L = matrix(1, 1, 1), log_jacobian = 0))
  }
  z <- tanh(y)
  L <- matrix(0, q, q)
  L[1, 1] <- 1
  k <- 0L
  logJ <- 0
  for (i in 2:q) {
    rem <- 1
    for (j in seq_len(i - 1L)) {
      k <- k + 1L
      L[i, j] <- z[k] * sqrt(rem)
      # y -> z contributes log(1 - z^2); z -> L contributes half a log of the
      # remaining mass.
      logJ <- logJ + log1p(-z[k]^2) + 0.5 * log(rem)
      rem <- rem * (1 - z[k]^2)
    }
    L[i, i] <- sqrt(rem)
    # L -> Omega: L_ii^(q - i), i.e. (q - i)/2 times the log of the mass left
    # on the diagonal.
    logJ <- logJ + (q - i) * 0.5 * log(rem)
  }
  Omega <- tcrossprod(L)
  # Numerical symmetry and an exact unit diagonal: the construction gives both
  # up to rounding, and downstream Cholesky factorizations are less forgiving
  # of a diagonal that is 1 - 2e-16 than the algebra is.
  Omega <- (Omega + t(Omega)) / 2
  diag(Omega) <- 1
  list(Omega = Omega, L = L, log_jacobian = logJ)
}

#' Log density of the LKJ prior, up to an additive constant
#'
#' `LKJ(eta)`: density proportional to `det(Omega)^(eta - 1)`. `eta = 1` is
#' uniform over correlation matrices; larger `eta` concentrates toward the
#' identity.
#'
#' @noRd
.mgc_lkj_logdens <- function(Omega, eta) {
  if (nrow(Omega) == 1L) {
    return(0)
  }
  ld <- determinant(Omega, logarithm = TRUE)
  if (ld$sign <= 0) {
    return(-Inf)
  }
  (eta - 1) * as.numeric(ld$modulus)
}

#' Log density of the group deviations under Sigma = diag(tau) Omega diag(tau)
#'
#' @param zeta a G x q matrix of deviations.
#' @param tau length-q scales; `Omega` the correlation matrix.
#' @return the log density, up to an additive constant free of tau and Omega.
#' @noRd
.mgc_zeta_logdens <- function(zeta, tau, Omega) {
  G <- nrow(zeta)
  q <- ncol(zeta)
  ch <- tryCatch(chol(Omega), error = function(e) NULL)
  if (is.null(ch)) {
    return(-Inf)
  }
  # Scale first, then solve against the correlation: Sigma^-1 quadratic form
  # equals || R^-T (zeta / tau) ||^2 with R the upper Cholesky of Omega. Done
  # this way the tau part never enters a matrix solve, so its contribution to
  # the log determinant stays exactly -G * sum(log(tau)) and the q = 1 case is
  # the scalar expression verbatim.
  S <- sweep(zeta, 2, tau, "/")
  W <- backsolve(ch, t(S), transpose = TRUE)
  -G * sum(log(tau)) - G * sum(log(diag(ch))) - 0.5 * sum(W^2)
}

#' One Metropolis sweep of a correlated between-group covariance
#'
#' Updates `tau` (half-t, on the log scale) and `Omega` (LKJ, through the
#' unconstrained parameterization above). Both are random-walk Metropolis: the
#' LKJ has no conjugate Gibbs update, and the half-t's scale-mixture Gibbs step
#' used in `mgc-hyper.R` is the `Omega = I` case of a matrix update rather than
#' a component of one, so it does not generalize.
#'
#' @param zeta a G x q matrix of current deviations.
#' @param state a list with `tau2`, `lambda`, and (when q > 1) `omega_y`,
#'   `omega_accept`, `tau_accept`, `n_sweeps`.
#' @param prior the resolved between-group prior; `eta` is the LKJ shape.
#' @return the updated state.
#' @noRd
.mgc_update_random_corr <- function(zeta, state, prior) {
  zeta <- as.matrix(zeta)
  q <- ncol(zeta)
  # THE q = 1 REDUCTION, taken here and not re-derived. With one coefficient
  # Omega is 1x1 and has no free parameter, so the model IS the diagonal one --
  # and the diagonal model's exact Gibbs step is strictly better than a
  # Metropolis step at the same target. Handing it back also keeps the RNG
  # stream identical, which is what makes a q = 1 fit bit-identical to a
  # diagonal-covariance fit rather than merely close.
  if (q == 1L) {
    return(.mgc_update_random(zeta, state, prior))
  }

  # LKJ(1) BY DEFAULT: uniform over correlation matrices, so the prior
  # contributes the least shrinkage the parameterization allows. It is a
  # choice for weak regularization, not a correction: a stronger setting
  # such as eta = 2 is a legitimate sensitivity option for anyone who wants
  # it.
  eta <- .mgc_or(prior$eta, 1)
  A <- rep(prior$A, length.out = q)
  nu <- prior$nu
  # The proposal scales live on the STATE, because burn-in adapts them
  # (mgc-tuning.R). 0.3 is the starting value, not a claim about the right
  # scale.
  step_tau <- .mgc_or(state$step_tau, 0.3)
  step_omega <- .mgc_or(state$step_omega, 0.3)

  tau <- sqrt(state$tau2)
  om <- .mgc_omega_unconstrained(state$omega_y, q)

  # Half-t(nu, A) on tau, written directly rather than through the scale
  # mixture: the mixture exists to make a Gibbs step possible, and there is no
  # Gibbs step here to make possible.
  log_half_t <- function(t, a) sum(-(nu + 1) / 2 * log1p((t / a)^2 / nu))

  cur <- .mgc_zeta_logdens(zeta, tau, om$Omega)

  # --- tau, one block on the log scale ------------------------------------
  prop_tau <- tau * exp(stats::rnorm(q, 0, step_tau))
  prop <- .mgc_zeta_logdens(zeta, prop_tau, om$Omega)
  # + sum(log(prop_tau)) - sum(log(tau)) is the log-scale Jacobian.
  logr <- (prop + log_half_t(prop_tau, A) + sum(log(prop_tau))) -
    (cur + log_half_t(tau, A) + sum(log(tau)))
  if (is.finite(logr) && log(stats::runif(1)) < logr) {
    tau <- prop_tau
    cur <- prop
    state$tau_accept <- .mgc_or(state$tau_accept, 0) + 1
  }

  # --- Omega ----------------------------------------------------------------
  y_prop <- state$omega_y + stats::rnorm(length(state$omega_y), 0, step_omega)
  om_prop <- .mgc_omega_unconstrained(y_prop, q)
  prop <- .mgc_zeta_logdens(zeta, tau, om_prop$Omega)
  logr <- (prop + .mgc_lkj_logdens(om_prop$Omega, eta) + om_prop$log_jacobian) -
    (cur + .mgc_lkj_logdens(om$Omega, eta) + om$log_jacobian)
  if (is.finite(logr) && log(stats::runif(1)) < logr) {
    state$omega_y <- y_prop
    om <- om_prop
    state$omega_accept <- .mgc_or(state$omega_accept, 0) + 1
  }

  state$tau2 <- tau^2
  state$Omega <- om$Omega
  # ACCEPTANCE IS REPORTED, not assumed. A Metropolis step that never moves
  # returns the correlation it started from, with no other symptom: the chain
  # looks stable, the posterior looks tight, and the number is the initial
  # value. The counts travel so a diagnostic can say so.
  state$n_sweeps <- .mgc_or(state$n_sweeps, 0) + 1
  state
}

#' Sigma from the current state, for the kernel's block prior
#'
#' @noRd
.mgc_sigma <- function(state, q) {
  if (q == 1L) {
    return(matrix(state$tau2[1], 1, 1))
  }
  tau <- sqrt(state$tau2)
  Om <- .mgc_or(state$Omega, diag(q))
  S <- outer(tau, tau) * Om
  (S + t(S)) / 2
}
