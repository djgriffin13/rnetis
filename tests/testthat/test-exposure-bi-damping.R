# Why Frank & Fahrbach's alpha cannot be absorbed into the fitted coefficient.
#
# THE ARGUMENT THIS FILE SETTLES. Using the raw conduit at every step, omitting
# the paper's lambda -- alpha between actors, gamma for self-retention -- makes
# a depth-v cross-actor path carry `prod(k)` rather than `alpha^v prod(k)`.
#
# The question is whether that matters. If the damped and undamped columns
# were PROPORTIONAL, omitting alpha would be harmless: a fitted coefficient on
# the exposure column would absorb it and only the interpretation would change,
# making it a documentation problem. The tests below show they are not
# proportional, because different nodes receive different MIXES of depth-1 and
# depth-2 contributions and alpha rescales those depths differently. No single
# scalar repairs that.
#
# A non-absorbable shape parameter is the same class of object as `delta` --
# supplied, never estimated, swept for sensitivity -- so `bi()` takes `alpha`
# and `gamma` and builds the paper's combined matrix (R/exposure-bi.R). These
# fixtures are the regression evidence for why the argument exists: if someone later proposes
# dropping `alpha` as "just a scale factor", this file is the answer.
#
# They exercise `.bi_accumulate()` directly, below the kernel, so they keep
# measuring the accumulator's own behavior rather than the argument plumbing.
# `test-exposure-bi-equations.R` covers the public surface against the erratum.

# Two-step history on a 4-node network with genuine path redundancy: node d is
# reachable from a at depth 2 by two routes (a->b->d and a->c->d), while b and c
# are reachable only at depth 1. That asymmetry is the whole point -- nodes
# receive different MIXES of depth-1 and depth-2 contributions, which is what a
# single scalar cannot repair.
.bi_hist <- function() {
  ids <- c("a", "b", "c", "d")
  K <- matrix(0, 4, 4, dimnames = list(ids, ids))
  # W[i, j] = j influences i (rnetis's canonical orientation).
  K["b", "a"] <- 1
  K["c", "a"] <- 1
  K["d", "b"] <- 1
  K["d", "c"] <- 1
  # Same conduit at both historical waves.
  list(K_hist = list(K, K), e_hist = list(c(a = 1, b = 0, c = 0, d = 0),
                                          c(a = 1, b = 0, c = 0, d = 0)))
}

# The paper's weighting, applied to the same accumulation: every step of a
# depth-v path carries alpha, so the depth-v term is scaled by alpha^v.
.bi_damped <- function(h, alpha, delta) {
  n <- length(h$e_hist[[1]])
  total <- numeric(n)
  for (v in seq_along(h$K_hist)) {
    one <- .bi_accumulate(h$K_hist[seq_len(v)], h$e_hist[seq_len(v)], n, alpha = 1, gamma = 0)
    prev <- if (v == 1L) list(A = numeric(n), C = numeric(n)) else
      .bi_accumulate(h$K_hist[seq_len(v - 1L)], h$e_hist[seq_len(v - 1L)], n, alpha = 1, gamma = 0)
    # The depth-v contribution alone, as the difference of two truncated sums.
    mp_v <- one$A - prev$A
    q_v <- one$C - prev$C
    total <- total + alpha^v * ((1 - delta) * mp_v + delta * q_v)
  }
  total
}

test_that("the accumulator's truncated sums decompose by depth, as the damping needs", {
  # A guard on the helper above rather than on the package: if `.bi_accumulate`
  # over v waves were not the sum of its per-depth terms, every number below
  # would be measuring the helper instead of the claim.
  h <- .bi_hist()
  n <- 4L
  full <- .bi_accumulate(h$K_hist, h$e_hist, n, alpha = 1, gamma = 0)
  by_depth <- .bi_damped(h, alpha = 1, delta = 1)
  expect_equal(unname(by_depth), unname(full$C), tolerance = 1e-12)
})

test_that("alpha is NOT absorbable: damped and undamped columns are not proportional", {
  h <- .bi_hist()
  delta <- 1  # all-paths, where redundancy is fully counted
  undamped <- .bi_damped(h, alpha = 1, delta = delta)
  damped <- .bi_damped(h, alpha = 0.5, delta = delta)

  # Only nodes that actually receive something can carry information about the
  # ratio; a 0/0 says nothing either way.
  live <- undamped != 0
  expect_gt(sum(live), 1L)
  ratio <- damped[live] / undamped[live]

  # THE ASSERTION. If alpha were a scale factor the fitted coefficient could
  # absorb, this ratio would be constant across nodes. It is not: nodes reached
  # only at depth 1 scale by alpha, nodes reached at depth 2 by alpha^2.
  expect_gt(diff(range(ratio)), 0.1)
  expect_equal(min(ratio), 0.5^2, tolerance = 1e-12)   # depth-2 only (node d)
  expect_equal(max(ratio), 0.5^1, tolerance = 1e-12)   # depth-1 only (b and c)
})

test_that("with a single wave of history alpha IS just a scale factor", {
  # The boundary case, and it is what makes the result above a statement about
  # DEPTH rather than about damping in general: with one historical wave there
  # is only depth 1, every node scales by alpha^1, and a fitted coefficient
  # would absorb it exactly. The non-absorbability appears only once the sum
  # runs over more than one depth -- which is the case bi() exists for.
  h <- .bi_hist()
  h1 <- list(K_hist = h$K_hist[1], e_hist = h$e_hist[1])
  undamped <- .bi_damped(h1, alpha = 1, delta = 1)
  damped <- .bi_damped(h1, alpha = 0.5, delta = 1)

  live <- undamped != 0
  ratio <- damped[live] / undamped[live]
  expect_equal(diff(range(ratio)), 0, tolerance = 1e-12)
  expect_equal(unique(round(ratio, 12)), 0.5)
})

test_that("the depth mix, not the delta setting, is what makes it non-absorbable", {
  # Repeating the check at delta = 0 (maximum-path only, redundancy removed)
  # shows this is a property of summing over depths, not an artifact of
  # counting redundant paths.
  h <- .bi_hist()
  undamped <- .bi_damped(h, alpha = 1, delta = 0)
  damped <- .bi_damped(h, alpha = 0.5, delta = 0)

  live <- undamped != 0
  ratio <- damped[live] / undamped[live]
  expect_gt(diff(range(ratio)), 0.1)
})
