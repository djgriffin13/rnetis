# The coefficient kernel's Schur algebra, checked against a dense solve.
#
# WHY THIS FILE EXISTS. `.mgc_coef_joint_dense()` was written to be the
# independent cross-check of `.mgc_coef_conditional()`: it builds the full
# block-arrow precision and inverts it directly, sharing none of the Schur
# reasoning. Nothing called it, so the check the kernel's own header claims was
# dead code -- and the Schur marginal is what produces the shared coefficient
# draw that every group then sees, which is the arithmetic behind the `joint`
# coupling label.
#
# The comparison is of the CONDITIONAL MOMENTS, not of draws: two
# implementations of the same Gaussian conditional must agree exactly on mean
# and covariance, while draws agree only in distribution.

.kernel_fixture <- function(G = 3L, p = 3L, seed = 4) {
  set.seed(seed)
  # Q must be a valid precision: symmetric positive definite.
  stats <- stats::setNames(lapply(seq_len(G), function(g) {
    A <- matrix(stats::rnorm(p * p), p, p)
    Q <- crossprod(A) + diag(p) * 1.5
    list(Q = Q, l = stats::rnorm(p))
  }), paste0("g", seq_len(G)))
  list(stats = stats, p = p, G = G)
}

.kernel_priors <- function(n_shared, n_block, G, seed = 7) {
  set.seed(seed)
  mk <- function(k) {
    if (k == 0L) return(list(P = matrix(0, 0, 0), m = numeric(0)))
    A <- matrix(stats::rnorm(k * k), k, k)
    list(P = crossprod(A) + diag(k), m = stats::rnorm(k))
  }
  list(shared = mk(n_shared),
       block = lapply(seq_len(G), function(g) mk(n_block)))
}

test_that("the Schur marginal for the shared block matches a dense solve", {
  fx <- .kernel_fixture()
  nm <- c("intercept", "b1", "b2")
  reg <- .mgc_regime(nm, c(intercept = "shared", b1 = "shared", b2 = "group"))
  pri <- .kernel_priors(length(reg$shared), length(reg$block), fx$G)

  cond <- .mgc_coef_conditional(fx$stats, reg, pri$shared, pri$block)
  dense <- .mgc_coef_joint_dense(fx$stats, reg, pri$shared, pri$block)

  # The shared block's conditional mean, marginal over every group block.
  shared_idx <- grep("^shared:", dense$index)
  expect_equal(as.numeric(cond$m_s), dense$mean[shared_idx], tolerance = 1e-10)

  # And its covariance: S_chol is the Cholesky of the marginal PRECISION, so
  # its inverse-crossproduct is the covariance the dense solve reports.
  S <- crossprod(cond$S_chol)
  expect_equal(unname(solve(S)), unname(dense$cov[shared_idx, shared_idx, drop = FALSE]),
               tolerance = 1e-10)
})

test_that("each group's conditional given the shared block matches the dense solve", {
  fx <- .kernel_fixture(G = 4L)
  nm <- c("intercept", "b1", "b2")
  reg <- .mgc_regime(nm, c(intercept = "shared", b1 = "group", b2 = "group"))
  pri <- .kernel_priors(length(reg$shared), length(reg$block), fx$G)

  cond <- .mgc_coef_conditional(fx$stats, reg, pri$shared, pri$block)
  dense <- .mgc_coef_joint_dense(fx$stats, reg, pri$shared, pri$block)
  shared_idx <- grep("^shared:", dense$index)
  beta_s <- dense$mean[shared_idx]          # condition at the dense mean

  for (g in seq_len(fx$G)) {
    gi <- grep(paste0("^", names(fx$stats)[g], ":"), dense$index)
    # Dense conditional of block g given the shared block, by the standard
    # Gaussian conditioning formula on the JOINT covariance.
    V <- dense$cov
    mu_g <- dense$mean[gi] +
      V[gi, shared_idx, drop = FALSE] %*%
      solve(V[shared_idx, shared_idx, drop = FALSE]) %*% (beta_s - dense$mean[shared_idx])
    entry <- cond$groups[[g]]
    # The kernel stores a (the mean at beta_s = 0) and B = A^-1 Q_rs, so the
    # conditional mean is a MINUS B beta_s: conditioning on a precision block
    # subtracts the cross-precision term. Getting this sign wrong was my first
    # draft of this test, and the code was right -- when a new test fails,
    # suspect the fixture before the code.
    kern_mu <- entry$a - as.numeric(entry$B %*% beta_s)
    expect_equal(as.numeric(kern_mu), as.numeric(mu_g), tolerance = 1e-10,
                 info = paste("group", g))
  }
})

test_that("with no shared block the groups are exactly independent", {
  fx <- .kernel_fixture(G = 3L, p = 2L)
  nm <- c("intercept", "b1")
  reg <- .mgc_regime(nm, c(intercept = "group", b1 = "group"))
  pri <- .kernel_priors(0L, length(reg$block), fx$G)
  dense <- .mgc_coef_joint_dense(fx$stats, reg, pri$shared, pri$block)

  # Groups share no actors and no coefficients here, so the joint covariance
  # must be block diagonal -- if it is not, the kernel is coupling groups that
  # have nothing in common.
  for (g in seq_len(fx$G - 1L)) {
    gi <- grep(paste0("^g", g, ":"), dense$index)
    gj <- grep(paste0("^g", g + 1L, ":"), dense$index)
    expect_equal(max(abs(dense$cov[gi, gj, drop = FALSE])), 0, tolerance = 1e-12)
  }
})

test_that("a random coefficient's groups are coupled only through the shared block", {
  # This is the structure behind the `joint` label: given the shared block --
  # which includes mu for a random coefficient -- the group blocks are
  # conditionally independent, so drawing them independently given ONE shared
  # draw is an exact joint draw rather than an approximation.
  fx <- .kernel_fixture(G = 3L)
  nm <- c("intercept", "b1", "b2")
  reg <- .mgc_regime(nm, c(intercept = "shared", b1 = "shared", b2 = "random"))
  pri <- .kernel_priors(length(reg$shared), length(reg$block), fx$G)
  dense <- .mgc_coef_joint_dense(fx$stats, reg, pri$shared, pri$block)

  shared_idx <- grep("^shared:", dense$index)
  V <- dense$cov
  # Conditional covariance of the group blocks given the shared block.
  gi <- setdiff(seq_along(dense$index), shared_idx)
  Vc <- V[gi, gi, drop = FALSE] -
    V[gi, shared_idx, drop = FALSE] %*% solve(V[shared_idx, shared_idx, drop = FALSE]) %*%
    V[shared_idx, gi, drop = FALSE]
  per <- length(reg$block)
  for (g in seq_len(fx$G - 1L)) {
    rows <- (g - 1L) * per + seq_len(per)
    cols <- g * per + seq_len(per)
    expect_equal(max(abs(Vc[rows, cols, drop = FALSE])), 0, tolerance = 1e-10,
                 info = paste("groups", g, "and", g + 1))
  }
})

# --- the per-group data summaries, against a dense build -------------------------

.dense_group_covariance <- function(n, rho, s2) {
  idx <- expand.grid(i = seq_len(n), j = seq_len(n))
  N <- nrow(idx)
  Ve <- matrix(0, N, N)
  for (r in seq_len(N)) for (c in seq_len(N)) {
    same <- idx$i[r] == idx$i[c] && idx$j[r] == idx$j[c]
    flip <- idx$i[r] == idx$j[c] && idx$j[r] == idx$i[c]
    # A cell on the diagonal pairs with itself, so its variance is s2 (1 + rho).
    Ve[r, c] <- s2 * (same + rho * flip)
  }
  M <- matrix(0, N, 2L * n)   # columns a_1..a_n, b_1..b_n
  for (r in seq_len(N)) {
    M[r, idx$i[r]] <- 1
    M[r, n + idx$j[r]] <- 1
  }
  list(Ve = Ve, M = M, N = N)
}

test_that("a group's data summaries match a dense solve of its marginal covariance", {
  skip_if_not_installed("amen")
  set.seed(3)
  n <- 5L
  Xrow <- matrix(stats::rnorm(n), ncol = 1, dimnames = list(NULL, "x"))
  Xd <- array(stats::rnorm(n * n), c(n, n, 1), dimnames = list(NULL, NULL, "d"))
  X <- .mgc_design(Xrow, NULL, Xd, n)
  Sab <- matrix(c(0.7, 0.2, 0.2, 0.5), 2)
  rho <- 0.4
  s2 <- 1.3
  Z <- matrix(stats::rnorm(n * n), n, n)
  st <- .mgc_group_stats(Z, Sab, rho, s2, X)

  dn <- .dense_group_covariance(n, rho, s2)
  Sa <- kronecker(Sab, diag(n))     # the (a, b) prior over 2n effects
  V <- dn$M %*% Sa %*% t(dn$M) + dn$Ve
  Xm <- matrix(X, dn$N, dim(X)[3])
  expect_lt(max(abs(st$Q - t(Xm) %*% solve(V, Xm))), 1e-10)
  expect_lt(max(abs(st$l - t(Xm) %*% solve(V, c(Z)))), 1e-10)
})

test_that("draws of the actor effects center on their dense conditional mean", {
  skip_if_not_installed("amen")
  set.seed(5)
  n <- 4L
  Xrow <- matrix(stats::rnorm(n), ncol = 1, dimnames = list(NULL, "x"))
  X <- .mgc_design(Xrow, NULL, NULL, n)
  Sab <- matrix(c(0.8, -0.3, -0.3, 0.6), 2)   # asymmetric in sender and receiver
  rho <- 0.3
  s2 <- 1
  Z <- matrix(stats::rnorm(n * n), n, n)
  theta <- c(0.4, -0.7)
  ctx <- .mgc_group_stats(Z, Sab, rho, s2, X)$ctx

  dn <- .dense_group_covariance(n, rho, s2)
  y <- c(Z) - matrix(X, dn$N, dim(X)[3]) %*% theta
  P <- kronecker(solve(Sab), diag(n)) + t(dn$M) %*% solve(dn$Ve, dn$M)
  truth <- c(solve(P, t(dn$M) %*% solve(dn$Ve, y)))
  sds <- sqrt(diag(solve(P)))

  S <- 6000L
  draws <- t(replicate(S, {
    ab <- .mgc_draw_ab(ctx, theta)
    c(ab$a, ab$b)
  }))
  z <- (colMeans(draws) - truth) / (sds / sqrt(S))
  expect_lt(max(abs(z)), 4.5)
  expect_lt(max(abs(apply(draws, 2, stats::sd) / sds - 1)), 0.08)
})
