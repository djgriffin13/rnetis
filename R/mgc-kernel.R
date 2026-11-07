# PORTABLE MULTI-GROUP CORE (mgc) -- the coefficient kernel.
# See mgc-regime.R for what this core may depend on.
#
# ONE EXACT DRAW OF EVERY COEFFICIENT IN EVERY GROUP, for any regime.
#
# Partition the p columns into the shared set S and the group-varying block r
# (the "group" columns followed by the "random" ones). Given every group's data
# summaries (Q_g, l_g; mgc-stats.R), a prior N(m0, P0^-1) on the shared
# coefficients, and a per-group prior N(m_g, P_g^-1) on its block, the joint
# full conditional of (beta_S, theta_r1, ..., theta_rG) is Gaussian with a
# BLOCK-ARROW precision: one s x s shared block, G independent r x r group
# blocks, and cross terms only between each group and the shared block.
#
#     A_g   = Q_g[r,r] + P_g
#     S     = P0 + sum_g ( Q_g[s,s] - Q_g[s,r] A_g^-1 Q_g[r,s] )
#     h     = P0 m0 + sum_g ( l_g[s] - Q_g[s,r] A_g^-1 (l_g[r] + P_g m_g) )
#     beta_S           ~ N( S^-1 h, S^-1 )
#     theta_rg | beta_S ~ N( A_g^-1 (l_g[r] + P_g m_g - Q_g[r,s] beta_S), A_g^-1 )
#
# The regimes are prior configurations of this one draw, never separate code:
#
#   no pooling           s = 0; each group's block is every column under its own prior
#   complete pooling     r empty; the prior P0 is added once, not once per group
#   shared slopes        r = intercept, P_g its group-specific prior
#   random intercept     r = intercept, P_g = 1 / tau^2 and m_g = mu (mgc-hyper.R)
#
# Two traps, both of which give plausible numbers rather than obvious breakage:
#   * the shared prior is added ONCE -- adding it per group inflates the shared
#     precision by (G - 1) prior copies, invisibly in any one fit;
#   * the marginalization over additive effects is per group (inside Q_g) and
#     must never be applied across groups -- groups share no actors.
#
# The tests check this algebra against a dense solve of the full precision
# (helper-mgc-dense.R), arithmetic that shares none of its reasoning.

#' Conditional moments of all coefficients, in factored form
#'
#' @param stats a list over groups of `.mgc_group_stats()` results.
#' @param regime a `.mgc_regime()` object.
#' @param prior_shared a list with `P` (s x s) and `m` (length s), or `NULL`
#'   when nothing is shared.
#' @param prior_block a list over groups, each with `P` (r x r) and `m`
#'   (length r); `NULL` when the block is empty.
#' @return a list with the shared mean `m_s`, the upper Cholesky factor
#'   `S_chol` of the shared precision, and per group the block's upper Cholesky
#'   factor `A_chol`, conditional mean offset `a` and regression `B`
#'   (so `E[theta_r | beta_S] = a - B beta_S`).
#' @noRd
.mgc_coef_conditional <- function(stats, regime, prior_shared = NULL, prior_block = NULL) {
  s <- regime$shared
  r <- regime$block
  G <- length(stats)

  groups <- vector("list", G)
  S <- if (length(s)) prior_shared$P else NULL
  h <- if (length(s)) c(prior_shared$P %*% prior_shared$m) else NULL

  for (g in seq_len(G)) {
    Q <- stats[[g]]$Q
    l <- stats[[g]]$l
    entry <- list()
    if (length(r)) {
      A <- Q[r, r, drop = FALSE] + prior_block[[g]]$P
      A_chol <- chol((A + t(A)) / 2)
      rhs <- l[r] + c(prior_block[[g]]$P %*% prior_block[[g]]$m)
      a <- backsolve(A_chol, forwardsolve(t(A_chol), rhs))
      entry$A_chol <- A_chol
      entry$a <- a
      if (length(s)) {
        Q_rs <- Q[r, s, drop = FALSE]
        B <- backsolve(A_chol, forwardsolve(t(A_chol), Q_rs))
        entry$B <- B
        S <- S + Q[s, s, drop = FALSE] - t(Q_rs) %*% B
        h <- h + l[s] - c(t(Q_rs) %*% a)
      }
    } else {
      S <- S + Q[s, s, drop = FALSE]
      h <- h + l[s]
    }
    groups[[g]] <- entry
  }

  out <- list(groups = groups)
  if (length(s)) {
    S_chol <- chol((S + t(S)) / 2)
    out$S_chol <- S_chol
    out$m_s <- backsolve(S_chol, forwardsolve(t(S_chol), h))
  }
  out
}

#' Draw every group's coefficients jointly
#'
#' @inheritParams .mgc_coef_conditional
#' @return a list with `theta` (G x p, design column order, shared columns
#'   repeated in every row), `beta_shared` (length s), and `block_precision`
#'   (G x r diagonal of the data part `Q_g[r, r]`, kept for shrinkage).
#' @noRd
.mgc_draw_coefficients <- function(stats, regime, prior_shared = NULL, prior_block = NULL) {
  cond <- .mgc_coef_conditional(stats, regime, prior_shared, prior_block)
  s <- regime$shared
  r <- regime$block
  G <- length(stats)
  p <- length(regime$coef_names)

  beta_s <- numeric(0)
  if (length(s)) {
    # x = m + R^-1 z with S = R'R gives Cov(x) = S^-1.
    beta_s <- cond$m_s + backsolve(cond$S_chol, stats::rnorm(length(s)))
  }
  theta <- matrix(NA_real_, G, p, dimnames = list(names(stats), regime$coef_names))
  if (length(s)) theta[, s] <- matrix(beta_s, G, length(s), byrow = TRUE)
  block_precision <- matrix(NA_real_, G, length(r), dimnames = list(names(stats), regime$coef_names[r]))
  for (g in seq_len(G)) {
    if (length(r)) {
      gc <- cond$groups[[g]]
      mean_r <- gc$a
      if (length(s)) mean_r <- mean_r - c(gc$B %*% beta_s)
      theta[g, r] <- mean_r + backsolve(gc$A_chol, stats::rnorm(length(r)))
      block_precision[g, ] <- diag(stats[[g]]$Q)[r]
    }
  }
  list(theta = theta, beta_shared = beta_s, block_precision = block_precision)
}
