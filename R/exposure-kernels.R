# Built-in exposure mechanism kernels.
#
# A kernel is `function(context, spec) -> numeric vector, length n`, called once
# per group by the exposure engine (R/exposure-engine.R) with an already-aligned,
# already-direction-resolved context. What the engine guarantees a kernel --
# alignment, direction, and any normalization -- is stated on compute_exposure()'s
# help page, under Custom kernels.
#
# `requires` and `supports_normalize` are recorded as attributes on the kernel
# function itself; .resolve_mechanism() (R/exposure-spec.R) reads them when a
# custom kernel is supplied. This is the "declaration travels with the
# function" mechanism; there is no separate kernel registry.

#' @noRd
.kernel_linear <- function(context, spec) {
  # E_i = sum_j W[i,j] * X_j -- ordinary matrix-vector product. Normalization
  # (row-standardizing W) is applied by the engine before this kernel is
  # called: a linear mechanism is one step, so normalizing W and normalizing
  # the result are the same operation and the engine may do it.
  #
  # The plain product `W %*% X` cannot be used when X has any NA, because
  # `0 * NA` is NA in R: a structural zero -- a pair with no tie at all --
  # would contribute NA rather than nothing, and the NA would spread to
  # *every* node's exposure regardless of whether it is connected to the
  # missing one. That turns one missing alter value into a completely
  # missing exposure column, which is silent, total data loss.
  #
  # The correct rule follows from what exposure means: node i's exposure is
  # undefined exactly when some alter j that i actually has a tie to (W[i,j] !=
  # 0) has a missing value. Unconnected nodes are unaffected. Note this
  # deliberately does *not* renormalize over the observed alters under row
  # normalization -- averaging over the alters that happen to be present is
  # imputation, and would silently change the estimand.
  x <- context$X
  if (!anyNA(x) && !anyNA(context$W)) {
    return(as.numeric(context$W %*% x))
  }
  # ONE MASKING RULE FOR THE PACKAGE. `.mask_matvec()` already implements
  # exactly the rule stated above, for the path-accumulating kernel, and it
  # also covers a missing TIE WEIGHT rather than a missing content value. A
  # simpler expression (`rowSums(W[, missing_x] != 0) > 0`) is NA wherever W is, and an NA logical index is an
  # error -- "NAs are not allowed in subscripted assignments" -- rather than an
  # undefined exposure. With complete weights the two agree exactly.
  out <- .mask_matvec(context$W, x)
  # A row that is NA from end to end has no defined weights at all: an isolate
  # under isolate_rule = "na", or an undefined denominator. Its exposure is
  # undefined whatever the content is, including content that is all zero, which
  # the cellwise rule above reads as contributing nothing.
  out[rowSums(!is.na(context$W)) == 0L] <- NA_real_
  out
}
attr(.kernel_linear, "requires") <- "focal"
attr(.kernel_linear, "supports_normalize") <- "row"
