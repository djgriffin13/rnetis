# The conduit contract for compute_exposure()/exposure().
#
# `via =` accepts network specifications, which runs through
# `.build_canonical_W()` -- the one transposition point in the package, and the
# place where a plausible implementation can be silently wrong. These are the
# pins that say the conduit handling moves no answer.
#
# They are HAND-COMPUTED rather than snapshotted. A pin that records whatever
# the code currently prints protects the code from change; a pin that records
# what the arithmetic requires protects the USER from the code. The known-answer
# layer is the higher-value one, and here it costs nothing extra
# because the fixture is small enough to do on paper.
#
# THE FIXTURE, and why it is a directed 3-cycle:
#
#     a --2--> b --3--> c --5--> a          x = (a=1, b=10, c=100)
#
# Every node has exactly one in-neighbor and one out-neighbor, and the weights
# and contents are chosen so that no two products collide. A transposition
# therefore changes every single value, and the two directions cannot be
# confused by coincidence:
#
#   source_influences_target   W[i,j] = j influences i, so i sums over its
#                              IN-neighbors
#     E_a = 5 * x_c = 500      E_b = 2 * x_a = 2       E_c = 3 * x_b = 30
#
#   target_influences_source   the reverse reading of the same rows
#     E_a = 2 * x_b = 20       E_b = 3 * x_c = 300     E_c = 5 * x_a = 5

.cx_data <- function(...) {
  nodes <- data.frame(
    group = 1, node = c("a", "b", "c"), x = c(1, 10, 100),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = 1,
    source = c("a", "b", "c"),
    target = c("b", "c", "a"),
    w = c(2, 3, 5),
    stringsAsFactors = FALSE
  )
  netis_data(nodes, ties, ...)
}

.cx_values <- function(res) {
  stats::setNames(res$ex_x_w, res$node)
}

test_that("KNOWN ANSWER: default direction sums over in-neighbors", {
  res <- compute_exposure(.cx_data(), content = "x", via = "w")
  expect_equal(.cx_values(res), c(a = 500, b = 2, c = 30))
})

test_that("KNOWN ANSWER: the reverse direction gives entirely different values", {
  # Not merely different -- disjoint. If a refactor transposed, the first test
  # would produce these numbers and this one would produce those.
  res <- compute_exposure(
    .cx_data(), content = "x", via = "w",
    direction = "target_influences_source"
  )
  expect_equal(.cx_values(res), c(a = 20, b = 300, c = 5))
})

test_that("KNOWN ANSWER: row normalization divides by the row sum", {
  # Each node has exactly one in-neighbor, so row-normalized exposure is the
  # alter's value itself -- which is also a check that the denominator is the
  # weight sum and not the alter count.
  res <- compute_exposure(.cx_data(), content = "x", via = "w", normalize = "row")
  expect_equal(.cx_values(res), c(a = 100, b = 1, c = 10))
})

test_that("KNOWN ANSWER: an isolate gets 0 unnormalized and NA row-normalized", {
  nodes <- data.frame(group = 1, node = c("a", "b", "z"), x = c(1, 10, 100),
                      stringsAsFactors = FALSE)
  ties <- data.frame(group = 1, source = "a", target = "b", w = 2,
                     stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties)

  raw <- compute_exposure(nd, "x", "w")
  expect_equal(stats::setNames(raw$ex_x_w, raw$node)[["z"]], 0)

  # Under row normalization the denominator is zero, which is undefined rather
  # than zero -- the two are different claims and rnetis does not conflate them.
  norm <- compute_exposure(nd, "x", "w", normalize = "row")
  expect_true(is.na(stats::setNames(norm$ex_x_w, norm$node)[["z"]]))
})

test_that("KNOWN ANSWER: a structural zero contributes nothing, and does not poison the row", {
  # The the exposure engine-to-15 defect this package carries a scar from: `0 * NA` is NA in
  # R, so a plain `W %*% X` made one missing alter destroy every node's
  # exposure. `b` has a missing value; `a`'s exposure must still be defined,
  # because `a` has no tie to `b` at all.
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), x = c(1, NA, 100),
                      stringsAsFactors = FALSE)
  ties <- data.frame(group = 1, source = c("c", "b"), target = c("a", "c"),
                     w = c(5, 3), stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties)
  v <- .cx_values(compute_exposure(nd, "x", "w"))
  expect_equal(v[["a"]], 500) # 5 * x_c, untouched by b's missingness
  expect_true(is.na(v[["c"]])) # 3 * x_b, genuinely undefined
})

test_that("the default column name is built from content and conduit", {
  res <- compute_exposure(.cx_data(), content = "x", via = "w")
  expect_true("ex_x_w" %in% names(res))
})

test_that("KNOWN ANSWER: exposure resolves per group, never across them", {
  nodes <- data.frame(
    group = c("A", "A", "B", "B"), node = c("a", "b", "c", "d"),
    x = c(1, 10, 100, 1000), stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = c("A", "B"), source = c("a", "c"), target = c("b", "d"),
    w = c(2, 3), stringsAsFactors = FALSE
  )
  v <- .cx_values(compute_exposure(netis_data(nodes, ties), "x", "w"))
  expect_equal(v[["b"]], 2) # 2 * x_a
  expect_equal(v[["d"]], 300) # 3 * x_c, not touched by group A
  expect_equal(v[["a"]], 0)
  expect_equal(v[["c"]], 0)
})

test_that("KNOWN ANSWER: a lagged conduit reads the prior wave's network", {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b"), each = 2), wave = rep(1:2, times = 2),
    x = c(1, 2, 10, 20), stringsAsFactors = FALSE
  )
  # The network REVERSES between waves, so reading the wrong wave is visible.
  ties <- data.frame(
    group = 1, source = c("a", "b"), target = c("b", "a"), wave = c(1, 2),
    w = c(2, 7), stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  # At wave 2 with the focal network: b <- a is gone; a <- b with weight 7.
  v2 <- .cx_values(compute_exposure(nd, "x", "w", wave = 2))
  expect_equal(v2[["a"]], 7 * 20)
  expect_equal(v2[["b"]], 0)
})

test_that("the absent-network warning names the conduit", {
  # The conduit must be genuinely DYNAMIC for this rule to apply: a static
  # network is reused across waves and so has no per-wave existence to be
  # missing. An earlier version of this fixture had ties at one wave only, which
  # rnetis correctly classified as static -- and no warning fired, correctly.
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b"), each = 3), wave = rep(1:3, times = 2),
    x = c(1, 2, 3, 10, 20, 30), stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = 1, source = "a", target = "b", wave = c(1, 2), w = c(2, 7),
    stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")
  # Wave 3 has no ties for a network that demonstrably varies: unobserved, not
  # empty, so exposure is NA there rather than 0.
  expect_warning(compute_exposure(nd, "x", "w", wave = 3), "w")
})

test_that("a misspelled conduit is refused, naming what it looked for", {
  expect_error(compute_exposure(.cx_data(), "x", "nope"), "nope")
})
