# NA-aware building blocks for exposure arithmetic.
#
# THIS IS ONE MASKING RULE FOR THE PACKAGE, not a helper belonging to any single
# mechanism: the path-accumulating and linear kernels both use them, so they
# are named for what they do rather than for either kernel.
#
# The rule they implement, stated once:
#
#   A result cell is undefined exactly when an *unknown* quantity could
#   contribute to it -- an `NA` factor whose partner is nonzero or itself
#   unknown. A KNOWN ZERO on either side contributes exactly nothing regardless
#   of what the other factor is, so it never contaminates.
#
# That distinction is the whole point, and it is why a plain `M %*% e` cannot be
# used. `0 * NA` is `NA` in R, so a structural zero -- a pair with no tie at all
# -- would contribute `NA` rather than nothing, and one missing alter value
# would turn EVERY node's exposure into `NA`, including nodes with no tie to the
# missing one. The test fixtures therefore include incomplete inputs: with
# only complete fixtures that failure would be unreachable by construction
# rather than ruled out by verification.
#
# Equally important in the other direction: an `NA` is never replaced by a
# substantive zero in the *result*. The zero substitution below is internal
# arithmetic only, and every cell an unknown could have reached is re-marked
# `NA` afterwards. Deleting the information and inventing it are both refused.
#
# With complete inputs these reduce bit-identically to the plain operations.

#' Masked matrix-vector product
#'
#' `M %*% e`, with the missingness rule above applied cellwise.
#'
#' @param M a weight matrix, possibly containing `NA`.
#' @param e a content vector, possibly containing `NA`.
#' @return a numeric vector, `NA` exactly at the entries an unknown could reach.
#' @noRd
.mask_matvec <- function(M, e) {
  M_na <- is.na(M); e_na <- is.na(e)
  M0 <- M; M0[M_na] <- 0
  e0 <- e; e0[e_na] <- 0
  out <- as.numeric(M0 %*% e0)
  contaminated <- as.numeric(M_na %*% ((e0 != 0) | e_na)) +
    as.numeric((M0 != 0) %*% e_na)
  out[contaminated > 0] <- NA_real_
  out
}

#' Which result cells an unknown quantity could have reached
#'
#' Counts, per result cell, the unknown-could-contribute pairs: an `NA` on the
#' left meeting a nonzero-or-unknown on the right, plus a known-nonzero on the
#' left meeting an `NA` on the right. Logical matrices coerce to 0/1 under
#' `%*%`, so a positive count marks contamination.
#'
#' @noRd
.mask_contamination <- function(A_na, A0, B_na, B0) {
  (A_na %*% ((B0 != 0) | B_na)) + ((A0 != 0) %*% B_na)
}

#' Masked matrix product
#'
#' @noRd
.mask_matmul <- function(A, B) {
  # The contamination bookkeeping costs two extra n^3 logical products per
  # step, and on complete inputs it can only ever produce all-FALSE. Path
  # accumulation calls this once per step per wave of history, so skipping it
  # when there is nothing missing is free and exact -- the masked path reduces
  # to this same product.
  if (!anyNA(A) && !anyNA(B)) return(A %*% B)
  A_na <- is.na(A); B_na <- is.na(B)
  A0 <- A; A0[A_na] <- 0
  B0 <- B; B0[B_na] <- 0
  out <- A0 %*% B0
  out[.mask_contamination(A_na, A0, B_na, B0) > 0] <- NA_real_
  out
}

#' Masked max-times product
#'
#' @noRd
.mask_maxtimes <- function(A, B) {
  if (!anyNA(A) && !anyNA(B)) return(.maxtimes_matrix(A, B))
  A_na <- is.na(A); B_na <- is.na(B)
  A0 <- A; A0[A_na] <- 0
  B0 <- B; B0[B_na] <- 0
  out <- .maxtimes_matrix(A0, B0)
  # The same contamination rule is valid for the max-times product because
  # weights are nonnegative: an unknown product is somewhere in [0, Inf) and
  # could exceed any known maximum, so any cell it could reach is undefined.
  out[.mask_contamination(A_na, A0, B_na, B0) > 0] <- NA_real_
  out
}

#' Max-times ("bottleneck path") matrix product
#'
#' `result[i, j] = max_a( A[i, a] * B[a, j] )`. The (max, times) analogue of
#' ordinary matrix multiplication. This is the operator Frank & Fahrbach write
#' as `Omega` in equation (8): equation (6)'s all-path accumulation and (8)'s
#' maximal-path accumulation are the same recursion, differing only in whether
#' the intermediate index is summed or maximised over.
#'
#' @noRd
.maxtimes_matrix <- function(A, B) {
  n <- nrow(A)
  p <- ncol(B)
  out <- matrix(0, nrow = n, ncol = p, dimnames = list(rownames(A), colnames(B)))
  for (j in seq_len(p)) {
    prod_j <- A * matrix(B[, j], nrow = n, ncol = ncol(A), byrow = TRUE)
    out[, j] <- apply(prod_j, 1, max)
  }
  out
}
