# Parts of this file derive from the amen package (Hoff, Fosdick, Volfovsky and
# He; GPL-3), modified in 2026 for the hierarchical multi-group model. See
# inst/COPYRIGHTS.
# PORTABLE MULTI-GROUP CORE (mgc) -- per-group design and sufficient statistics.
# See mgc-regime.R for what this core may depend on.
#
# The statistics follow `amen::rbeta_ab_fc()` (amen 1.4.5), split into a
# data-summary step and a draw. They are pure data terms: priors are added
# once, by the kernel, per regime, so agreement with amen is posterior
# equivalence, not chain identity.
#
# THE DERIVATION the statistics rest on. Condition on a group's U, V. Over its
# vectorized dyads,
#
#     z_g = X_g theta_g + M_g w_g + e_g,   Cov(w_g) = I kron Sab_g,
#     Cov(e_g) = block-diagonal over dyad pairs, s2_g [[1, rho_g], [rho_g, 1]],
#
# with w_g the stacked additive effects and M_g the actor-incidence map. amen
# draws the coefficients MARGINALLY over w_g, so the likelihood for theta_g is
# N(X_g theta_g, V_g) with V_g = M_g (I kron Sab_g) M_g' + Sigma_g, and
#
#     Q_g = X_g' V_g^-1 X_g,     l_g = X_g' V_g^-1 z_g.
#
# amen's H-correction is Woodbury for V_g^-1 (test-mgc-kernel.R checks Q_g and
# l_g against a dense build). Q_g and l_g are therefore complete data summaries for ANY
# coefficient regime: partitioning them by regime gives the exact Gaussian full
# conditional of shared, group and random coefficients jointly, still marginal
# over every group's additive effects. That is why one kernel can serve every
# regime -- and why the random intercept is drawn marginal over a and b, which
# matters: an intercept and the mean of a group's actor effects are strongly
# correlated a posteriori, and drawing them separately would mix badly.

#' Build a group's design array, always carrying an intercept column
#'
#' `amen::design_array()` silently drops the intercept whenever any supplied
#' column is constant. Under multi-group regimes the intercept's regime is a
#' modeling decision, so its presence cannot depend on the data. The array is
#' then passed through amen's exported `precomputeX()`, whose cached products
#' the statistics read.
#'
#' @param Xrow,Xcol n x pr / n x pc matrices, or `NULL`.
#' @param Xdyad an n x n x pd array, or `NULL`.
#' @param n the number of actors.
#' @return a precomputed n x n x p design array with `"intercept"` first.
#' @noRd
.mgc_design <- function(Xrow = NULL, Xcol = NULL, Xdyad = NULL, n) {
  X <- amen::design_array(Xrow, Xcol, Xdyad, intercept = TRUE, n = n)
  nm <- unlist(dimnames(X)[[3]])
  if (!"intercept" %in% nm) {
    with_int <- array(dim = dim(X) + c(0L, 0L, 1L))
    with_int[, , 1] <- 1
    if (length(nm) > 0L) with_int[, , -1] <- X
    X <- with_int
    nm <- c("intercept", nm)
  }
  dimnames(X) <- list(NULL, NULL, nm)
  amen::precomputeX(X)
}

#' Data summaries Q_g, l_g for one group, marginal over its additive effects
#'
#' Statement-for-statement with the data half of `amen::rbeta_ab_fc()`, minus
#' the prior (added once, by the kernel). Also returns the context that
#' `.mgc_draw_ab()` needs afterwards.
#'
#' @param Z the latent n x n matrix, already less `U V'`.
#' @param Sab,rho,s2 the group's additive covariance, dyadic correlation, and
#'   dyadic variance.
#' @param X a design array from `.mgc_design()`.
#' @return a list with `Q` (p x p), `l` (length p), and `ctx`.
#' @noRd
.mgc_group_stats <- function(Z, Sab, rho, s2, X) {
  Xr <- attributes(X)$Xr; Xc <- attributes(X)$Xc
  mX <- attributes(X)$mX; mXt <- attributes(X)$mXt
  XX <- attributes(X)$XX; XXt <- attributes(X)$XXt
  n <- nrow(Z)

  Se <- matrix(c(1, rho, rho, 1), 2, 2) * s2
  iSe2 <- amen::mhalf(solve(Se))
  td <- iSe2[1, 1]
  to <- iSe2[1, 2]
  Sabs <- iSe2 %*% Sab %*% iSe2
  eig <- eigen(Sabs)
  k <- sum(zapsmall(eig$val) > 0)

  mXs <- td * mX + to * mXt
  Q <- (to^2 + td^2) * XX + 2 * to * td * XXt
  Zs <- td * Z + to * t(Z)
  zr <- rowSums(Zs); zc <- colSums(Zs); zs <- sum(zc)
  l <- crossprod(mXs, c(Zs))

  ctx <- list(k = k, n = n, td = td, to = to, iSe2 = iSe2, Zs = Zs, X = X)
  if (k > 0) {
    G <- eig$vec[, 1:k, drop = FALSE] %*% sqrt(diag(eig$val[1:k], nrow = k))
    K <- matrix(c(0, 1, 1, 0), 2, 2)
    A <- n * t(G) %*% G + diag(k)
    B <- t(G) %*% K %*% G
    iA0 <- solve(A)
    C0 <- -solve(A + n * B) %*% B %*% iA0
    iA <- G %*% iA0 %*% t(G)
    C <- G %*% C0 %*% t(G)
    Xsr <- td * Xr + to * Xc
    Xsc <- td * Xc + to * Xr
    Xss <- colSums(Xsc)
    l <- l - (iA[1, 1] * crossprod(Xsr, zr) + iA[2, 2] * crossprod(Xsc, zc) +
                iA[1, 2] * (crossprod(Xsr, zc) + crossprod(Xsc, zr)) +
                sum(C) * Xss * zs)
    cross <- crossprod(Xsr, Xsc)
    Q <- Q - (iA[1, 1] * crossprod(Xsr, Xsr) + iA[2, 2] * crossprod(Xsc, Xsc) +
                iA[2, 1] * (cross + t(cross)) + sum(C) * Xss %*% t(Xss))
    ctx$G <- G; ctx$iA0 <- iA0; ctx$C0 <- C0
  }
  nm <- dimnames(X)[[3]]
  Q <- (Q + t(Q)) / 2
  dimnames(Q) <- list(nm, nm)
  list(Q = Q, l = stats::setNames(c(l), nm), ctx = ctx)
}

#' Draw one group's additive effects given its full coefficient vector
#'
#' The tail of `amen::rbeta_ab_fc()`. `theta` is the group's complete
#' coefficient vector in design order, whatever mix of shared, group and random
#' values it came from.
#'
#' @param ctx the context from `.mgc_group_stats()`.
#' @param theta length-p coefficients for this group.
#' @return a list with `a` and `b`.
#' @noRd
.mgc_draw_ab <- function(ctx, theta) {
  n <- ctx$n
  if (ctx$k == 0L) {
    return(list(a = rep(0, n), b = rep(0, n)))
  }
  X <- ctx$X
  E <- ctx$Zs - amen::Xbeta(ctx$td * X + ctx$to * aperm(X, c(2, 1, 3)), theta)
  er <- rowSums(E); ec <- colSums(E); es <- sum(ec)
  G <- ctx$G; iA0 <- ctx$iA0; C0 <- ctx$C0
  m <- t(t(crossprod(rbind(er, ec), t(iA0 %*% t(G)))) + rowSums(es * C0 %*% t(G)))
  hiA0 <- amen::mhalf(iA0)
  e <- matrix(stats::rnorm(n * ctx$k), n, ctx$k)
  w <- m + t(t(e %*% hiA0) - c(((hiA0 - amen::mhalf(iA0 + n * C0)) / n) %*% colSums(e)))
  ab <- w %*% t(G) %*% solve(ctx$iSe2)
  list(a = ab[, 1], b = ab[, 2])
}
