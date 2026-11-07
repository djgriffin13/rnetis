# Row normalization under a missing tie weight.
#
# A known-answer test guarding against `normalize = "row"` crashing with
# "missing value where TRUE/FALSE needed" when a tie weight is NA and NO genuine
# isolate is present. `any(zero_rows)` is NA in that case, and a guard can only
# appear to work because a real isolate elsewhere makes `any()` return TRUE
# first.
#
# The distinction it protects is easy to "simplify" away:
#
#   AN UNKNOWN DENOMINATOR IS NOT AN ISOLATE.
#
# An isolate has no reference-set members, and `isolate_rule` governs what its
# undefined denominator becomes. A row with a partly unknown weight has a
# denominator nobody can compute, and its normalized weights are NA regardless
# of `isolate_rule`. Renormalizing over the observed alters instead would be
# imputation, and would silently change the estimand.

.na_norm_fixture <- function(with_isolate = FALSE) {
  nodes <- data.frame(
    group = 1,
    node = if (with_isolate) c("a", "b", "c", "d") else c("a", "b", "c"),
    x = if (with_isolate) c(10, 20, 30, 40) else c(10, 20, 30)
  )
  # a's outgoing weight to b is unknown; c is fully observed. Under the
  # canonical W[i, j] = "j influences i", a receives from b and c.
  ties <- data.frame(
    group = 1,
    source = c("b", "c", "a"),
    target = c("a", "a", "c"),
    w = c(NA, 3, 1)
  )
  netis_data(nodes, ties)
}

test_that("a missing tie weight with NO isolate present does not crash row normalization", {
  nd <- .na_norm_fixture(with_isolate = FALSE)

  # The regression: this call used to fail with "missing value where TRUE/FALSE
  # needed" rather than returning anything at all.
  expect_no_error(
    out <- compute_exposure(nd, "x", "w", normalize = "row")
  )
  expect_s3_class(out, "data.frame")
})

test_that("an unknown denominator yields NA for that node and exact values for every other", {
  nd <- .na_norm_fixture(with_isolate = FALSE)
  out <- compute_exposure(nd, "x", "w", normalize = "row")
  ex <- setNames(out[[grep("^ex_", names(out))]], out$node)

  # Hand-computed. Rows of the canonical W, with x = (a = 10, b = 20, c = 30):
  #   a <- b (NA) and c (3): row sum is unknown, so a's exposure is undefined.
  #   c <- a (1): row sum 1, normalized weight 1, exposure 1 * 10 = 10.
  #   b: no incoming ties -- an isolate, and under the default isolate_rule its
  #      denominator is undefined too, but for a different and stated reason.
  expect_true(is.na(ex[["a"]]))
  expect_equal(ex[["c"]], 10)
})

test_that("the isolate rule governs isolates and leaves the unknown-denominator row alone", {
  nd <- .na_norm_fixture(with_isolate = FALSE)

  # isolate_rule = "zero" resolves b, the genuine isolate, to 0. It must NOT
  # also resolve a, whose denominator is unknown rather than empty -- that
  # would be imputation.
  out <- compute_exposure(nd, "x", "w", normalize = "row", isolate_rule = "zero")
  ex <- setNames(out[[grep("^ex_", names(out))]], out$node)

  expect_equal(ex[["b"]], 0)
  expect_true(is.na(ex[["a"]]))
  expect_equal(ex[["c"]], 10)
})

test_that("isolate_rule = 'error' refuses on the isolate, not on the unknown denominator", {
  nd <- .na_norm_fixture(with_isolate = FALSE)
  expect_error(
    compute_exposure(nd, "x", "w", normalize = "row", isolate_rule = "error"),
    "isolate"
  )
})

test_that("with no missing weights at all, row normalization is exactly the plain quotient", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), x = c(10, 20, 30))
  ties <- data.frame(group = 1, source = c("b", "c"), target = c("a", "a"),
                     w = c(1, 3))
  nd <- netis_data(nodes, ties)

  out <- compute_exposure(nd, "x", "w", normalize = "row", isolate_rule = "zero")
  ex <- setNames(out[[grep("^ex_", names(out))]], out$node)

  # a <- b (1) and c (3): denominator 4, so (1 * 20 + 3 * 30) / 4 = 27.5.
  expect_equal(ex[["a"]], 27.5)
})
