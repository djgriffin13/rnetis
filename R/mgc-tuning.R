# PORTABLE MULTI-GROUP CORE (mgc) -- starting values for later chains, and the
# burn-in adaptation of the correlated covariance's Metropolis proposals.
# See mgc-regime.R for what this core may depend on.

#' Dispersed starting values for a chain after the first
#'
#' Chain 1 starts from amen's own initial state. Every later chain starts from
#' that state moved, deterministically given the chain's seed:
#'
#' * each group's starting coefficients by independent normal noise with SD
#'   `scale_k` per coefficient: twice the spread of the groups' initial
#'   estimates, held between half and five times a reference scale `ref_k`.
#'   `ref_k` is the coefficient change that moves the linear predictor by one
#'   outcome-scale unit for a one-SD change in the predictor (the outcome scale
#'   itself for the intercept or a constant column). The floor gives dispersion
#'   when the groups' initial estimates agree, or when there is one group and no
#'   spread is defined; the cap keeps an extreme initial estimate -- a small or
#'   degenerate group's ridge start -- from flinging the chain arbitrarily far.
#' * each between-group SD by a log-normal factor, `tau * exp(N(0, 1))`, which
#'   keeps it finite and positive.
#' * an unstructured correlation matrix by drawing its unconstrained
#'   parameters from `N(0, 0.5^2)`; the canonical-partial-correlation map turns
#'   any such vector into a valid correlation matrix.
#'
#' None of this guarantees the starts are overdispersed relative to the
#' posterior -- nothing short of knowing the posterior could -- so the fit's
#' diagnostic note says the starts were dispersed, not that they bracket it.
#'
#' @param theta the G x p starting coefficients; `rs` the between-group state
#'   (or `NULL`).
#' @return a list with `theta` and `rs`.
#' @noRd
.mgc_disperse_start <- function(theta, rs, prep, family) {
  p <- ncol(theta)
  G <- nrow(theta)
  out_scale <- .mgc_outcome_scale(lapply(prep, `[[`, "Y"), family)
  if (!is.finite(out_scale) || out_scale <= 0) out_scale <- 1
  ref <- vapply(seq_len(p), function(k) {
    v <- unlist(lapply(prep, function(gr) gr$X[, , k][gr$mask]))
    s <- if (length(v) > 1L) stats::sd(v) else NA_real_
    if (is.finite(s) && s > 0) out_scale / s else out_scale
  }, numeric(1))
  spread <- if (G > 1L) apply(theta, 2, stats::sd) else rep(NA_real_, p)
  scale <- vapply(seq_len(p), function(k) {
    if (!is.finite(spread[k])) return(ref[k])
    min(max(2 * spread[k], 0.5 * ref[k]), 5 * ref[k])
  }, numeric(1))
  theta <- theta + matrix(stats::rnorm(G * p), G, p) * rep(scale, each = G)

  if (!is.null(rs)) {
    q <- length(rs$tau2)
    rs$tau2 <- rs$tau2 * exp(2 * stats::rnorm(q))
    if (!is.null(rs$omega_y) && length(rs$omega_y)) {
      rs$omega_y <- stats::rnorm(length(rs$omega_y), 0, 0.5)
      rs$Omega <- .mgc_omega_unconstrained(rs$omega_y, q)$Omega
    }
  }
  list(theta = theta, rs = rs)
}

#' Target acceptance rate for a d-dimensional Gaussian random-walk block
#'
#' The two limits are established: about 0.44 is optimal for a one-dimensional
#' Gaussian random walk, and 0.234 is the limit as the dimension grows (Roberts,
#' Gelman & Gilks 1997; Roberts & Rosenthal 2001). Between them this
#' interpolates linearly in 1/d -- a heuristic, not an optimality result.
#' Efficiency is flat near the optimum, so the exact target matters less than
#' moving a badly scaled proposal into that broad region.
#'
#' @noRd
.mgc_mh_target <- function(d) {
  0.234 + (0.44 - 0.234) / max(1, d)
}

#' Whether, and how, a fit adapts its Metropolis proposal scales
#'
#' Only the unstructured covariance has Metropolis steps (the diagonal path is
#' exact Gibbs). Two blocks: `log(tau)`, of dimension q, and the
#' unconstrained correlation parameters, of dimension q(q - 1)/2, each a joint
#' isotropic Gaussian random walk with one scale -- so each gets the target for
#' its own dimension. Both start at 0.3. Nothing adapts when burn-in is shorter
#' than one batch of 50 sweeps.
#'
#' @return `NULL` when the fit has no Metropolis step, else a list of settings.
#' @noRd
.mgc_adaptation_plan <- function(reg, burn) {
  q <- length(reg$random)
  if (!identical(reg$covariance, "unstructured") || q < 2L) {
    return(NULL)
  }
  list(
    initial_tau = 0.3,
    initial_omega = 0.3,
    active = burn >= 50L,
    batch = 50L,
    target_tau = .mgc_mh_target(q),
    target_omega = .mgc_mh_target(q * (q - 1L) / 2L),
    last = c(tau = 0, omega = 0, sweeps = 0),
    batches = 0L
  )
}

#' Adapt the proposal scales, once per batch, during burn-in
#'
#' Every `batch` sweeps of burn-in, each block's log scale moves by
#' `(acceptance in the batch - target) * min(1, 2 / sqrt(j))` for the j-th
#' batch, bounded to `[0.001, 10]`. The gain shrinks so later batches refine
#' rather than oscillate. Nothing changes after burn-in: the retained draws come
#' from one fixed Metropolis kernel, so adaptation cannot disturb the
#' stationary distribution they are drawn from.
#'
#' @param rs the between-group state; `adapt` the plan; `s` the scan index.
#' @noRd
.mgc_adapt_step <- function(rs, adapt, s) {
  if (!isTRUE(adapt$active) || s %% adapt$batch != 0L) {
    return(rs)
  }
  last <- .mgc_or(rs$adapt_last, adapt$last)
  j <- .mgc_or(rs$adapt_batches, 0L) + 1L
  sweeps <- rs$n_sweeps - last[["sweeps"]]
  if (sweeps <= 0) return(rs)
  acc_tau <- (rs$tau_accept - last[["tau"]]) / sweeps
  acc_omega <- (rs$omega_accept - last[["omega"]]) / sweeps
  gain <- min(1, 2 / sqrt(j))
  bound <- function(v) min(max(v, log(0.001)), log(10))
  rs$step_tau <- exp(bound(log(rs$step_tau) + (acc_tau - adapt$target_tau) * gain))
  rs$step_omega <- exp(bound(log(rs$step_omega) + (acc_omega - adapt$target_omega) * gain))
  rs$adapt_last <- c(tau = rs$tau_accept, omega = rs$omega_accept, sweeps = rs$n_sweeps)
  rs$adapt_batches <- j
  rs
}
