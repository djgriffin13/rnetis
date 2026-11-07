# A dense reference for the coefficient kernel: the test compares the factored
# draw in R/mgc-kernel.R with a direct solve of the full precision.

#' Exact joint moments by direct dense solve (test reference only)
#'
#' Builds the full precision over `(beta_S, theta_r1, ..., theta_rG)` and
#' inverts it. Cubic in the total dimension, so never used for sampling.
#'
#' @inheritParams .mgc_coef_conditional
#' @return a list with `mean` and `cov` over the stacked vector, and `index`
#'   naming each position (`"shared:<coef>"` or `"<group>:<coef>"`).
#' @noRd
.mgc_coef_joint_dense <- function(stats, regime, prior_shared = NULL, prior_block = NULL) {
  s <- regime$shared
  r <- regime$block
  G <- length(stats)
  nm <- regime$coef_names
  idx_s <- seq_along(s)
  offset <- length(s)
  idx_r <- lapply(seq_len(G), function(g) offset + (g - 1L) * length(r) + seq_along(r))
  D <- offset + G * length(r)
  P <- matrix(0, D, D)
  b <- numeric(D)
  if (length(s)) {
    P[idx_s, idx_s] <- prior_shared$P
    b[idx_s] <- c(prior_shared$P %*% prior_shared$m)
  }
  for (g in seq_len(G)) {
    Q <- stats[[g]]$Q
    l <- stats[[g]]$l
    if (length(s)) {
      P[idx_s, idx_s] <- P[idx_s, idx_s] + Q[s, s, drop = FALSE]
      b[idx_s] <- b[idx_s] + l[s]
    }
    if (length(r)) {
      ir <- idx_r[[g]]
      P[ir, ir] <- Q[r, r, drop = FALSE] + prior_block[[g]]$P
      b[ir] <- l[r] + c(prior_block[[g]]$P %*% prior_block[[g]]$m)
      if (length(s)) {
        P[ir, idx_s] <- Q[r, s, drop = FALSE]
        P[idx_s, ir] <- Q[s, r, drop = FALSE]
      }
    }
  }
  V <- solve(P)
  index <- c(if (length(s)) paste0("shared:", nm[s]),
             unlist(lapply(seq_len(G), function(g) paste0(names(stats)[g], ":", nm[r]))))
  list(mean = c(V %*% b), cov = V, index = index)
}
