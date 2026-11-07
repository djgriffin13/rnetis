# Moments of the AME latent tie variable.
#
# THE MODEL. For a directed dyad (i != j) in one network,
#
#     z_ij = x_ij'beta + a_i + b_j + u_i'v_j + e_ij
#     (a_i, b_i) ~ N(0, [[va, cab], [cab, vb]])     one draw per actor
#     (e_ij, e_ji) ~ N(0, s2 [[1, rho], [rho, 1]])  one draw per unordered pair
#
# and for a symmetric relation z_ij = x_ij'beta + a_i + a_j + u_i' L u_j + e_ij.
#
# THE RESULT THIS FILE EXISTS TO STATE ONCE. Conditional on x, a single dyad's
# latent variance is
#
#     directed:   va + vb + V_uv + s2
#     symmetric:  2 va    + V_uv + s2
#
# with NO 2*cab term. a_i and b_j belong to DIFFERENT actors and are
# independent, so their covariance is zero; cab = Cov(a_i, b_i) is a
# within-actor covariance. It enters other moments -- the reciprocal-pair
# covariance Cov(z_ij, z_ji) = 2 cab + tr(C C) + rho s2, the two-path
# covariance Cov(z_ij, z_jk) = cab, and an actor's combined sending-plus-
# receiving variance Var(a_i + b_i) = va + vb + 2 cab -- but never the variance
# of one dyad. Including 2*cab here is an easy error to make and invisible in
# any single fit, which is why the formula lives in one function with a
# known-answer test rather than inline at each caller. The moment was derived
# analytically and checked by direct Monte Carlo simulation.
#
# V_uv is the variance of the multiplicative term over dyads. Callers decide
# which version they can supply -- the population value from the latent-factor
# covariance, or the realized variance of u_i'v_j over the network's dyads --
# and say which in their output.

#' Contributions of each AME component to one dyad's latent variance
#'
#' @param va sender variance (directed) or actor variance (symmetric).
#' @param vb receiver variance; ignored when `symmetric = TRUE`.
#' @param cab sender-receiver covariance. Accepted so a caller can pass the
#'   whole covariance block, and deliberately unused: it does not enter a
#'   single dyad's variance (see the file header).
#' @param v_uv variance of the multiplicative term over dyads; `0` when `R = 0`.
#' @param s2 dyadic residual variance (`1` under the probit).
#' @param symmetric whether the relation is symmetric.
#' @return a named numeric vector: `sender`, `receiver`, `multiplicative`,
#'   `residual` and `total` for a directed relation; `actor`, `multiplicative`,
#'   `residual` and `total` for a symmetric one.
#' @noRd
.ame_dyad_variance_parts <- function(va, vb = NULL, cab = NULL, v_uv = 0, s2,
                                     symmetric = FALSE) {
  if (isTRUE(symmetric)) {
    out <- c(actor = 2 * va, multiplicative = v_uv, residual = s2)
  } else {
    if (is.null(vb)) {
      stop("Internal error: a directed dyad's variance needs the receiver variance vb.",
           call. = FALSE)
    }
    out <- c(sender = va, receiver = vb, multiplicative = v_uv, residual = s2)
  }
  c(out, total = sum(out))
}
