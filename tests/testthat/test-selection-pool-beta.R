# The pooling gate hierarchy, in order of authority.
#
#   1. hand-computed known answer   <- what the pooling MUST be
#   2. K = 1 reduction              <- pooling one group changes nothing
#   3. group-order invariance       <- by construction, since summation commutes
#   4. agreement with lame_multi()  <- corroboration, NOT authority
#
# THE ORDER IS THE POINT. Gates 1-3 are the mathematical claim; gate 4 says the
# claim matches another implementation of the same idea. If rnetis and
# `lame::lame_multi()` ever disagree, the first three decide which is right and
# the discrepancy gets investigated, not papered over by adjusting rnetis until
# it matches.
#
# THE NUMERICAL FLOOR IS NOT UNDER TEST BY ANY OF THEM, deliberately. Every
# fixture below keeps variances far above `.POOL_VARIANCE_FLOOR`, and the gates
# assert that nothing was floored. Otherwise gate 4 could pass because rnetis
# and lame share a numerical convention rather than because they compute the
# same pooling formula -- and a guard against underflow would have quietly
# become part of the method. The floor gets its own test, far below, where it
# is the subject rather than a confound.

.pool_fixture <- function(k = 3, p = 2, n_draws = 400, seed = 5) {
  # Per-group "fits" carrying only what the pooler reads: a matrix of retained
  # coefficient draws. Constructed rather than sampled from amen, because the
  # arithmetic under test is the combination, not the sampler -- and a
  # deterministic input is what makes a hand-computed answer possible.
  set.seed(seed)
  lapply(seq_len(k), function(g) {
    draws <- vapply(seq_len(p), function(j) {
      stats::rnorm(n_draws, mean = g * 0.1 * j, sd = sqrt(g) * 0.2)
    }, numeric(n_draws))
    colnames(draws) <- paste0("b", seq_len(p))
    list(extracted = list(draws = draws))
  })
}

# ---- gate 1: the hand-computed answer ---------------------------------------

test_that("pooling is the inverse-variance weighted mean, computed by hand", {
  # Two groups, one coefficient, exact numbers chosen so the arithmetic can be
  # done on paper:
  #
  #   group 1: mean 2, variance 1     -> precision 1
  #   group 2: mean 5, variance 1/4   -> precision 4
  #
  #   pooled mean = (1*2 + 4*5) / (1 + 4) = 22 / 5 = 4.4
  #   pooled var  = 1 / (1 + 4)           = 0.2
  #
  # The pooled mean sits nearer the PRECISE group, which is the whole content
  # of inverse-variance weighting.
  mk <- function(mu, sd) {
    z <- scale(stats::rnorm(500))[, 1]          # exactly mean 0, sd 1
    d <- matrix(mu + sd * z, ncol = 1L, dimnames = list(NULL, "b1"))
    list(extracted = list(draws = d))
  }
  set.seed(1)
  pooled <- .pool_beta_precision(list(mk(2, 1), mk(5, 0.5)))

  expect_equal(unname(pooled$mean[["b1"]]), 4.4, tolerance = 1e-8)
  expect_equal(unname(pooled$var[["b1"]]), 0.2, tolerance = 1e-8)

  # Variances of 1 and 1/4 are eight orders of magnitude above the guard, so
  # this is the pooling formula and nothing else.
  expect_length(pooled$floored, 0L)
})

test_that("the pooled variance is smaller than either group's, as combining implies", {
  set.seed(1)
  mk <- function(mu, sd) {
    z <- scale(stats::rnorm(500))[, 1]
    list(extracted = list(draws = matrix(mu + sd * z, ncol = 1L,
                                         dimnames = list(NULL, "b1"))))
  }
  pooled <- .pool_beta_precision(list(mk(2, 1), mk(5, 0.5)))
  expect_lt(pooled$var[["b1"]], 0.25)   # below the more precise group's own
})

test_that("weights are the per-group precisions, reported for inspection", {
  set.seed(1)
  mk <- function(mu, sd) {
    z <- scale(stats::rnorm(500))[, 1]
    list(extracted = list(draws = matrix(mu + sd * z, ncol = 1L,
                                         dimnames = list(NULL, "b1"))))
  }
  pooled <- .pool_beta_precision(list(a = mk(2, 1), b = mk(5, 0.5)))
  expect_equal(unname(pooled$weights["b1", ]), c(1, 4), tolerance = 1e-8)
})

# ---- gate 2: the K = 1 reduction --------------------------------------------

test_that("pooling ONE group returns that group's own answer", {
  # The reduction gate. If pooling a single group moved anything, the pooling
  # would be doing something other than combining -- and every multi-group
  # result would be suspect for the same reason.
  one <- .pool_fixture(k = 1, p = 3)
  pooled <- .pool_beta_precision(one)
  draws <- one[[1]]$extracted$draws

  expect_equal(unname(pooled$mean), unname(colMeans(draws)), tolerance = 1e-10)
  expect_equal(unname(pooled$var), unname(apply(draws, 2L, stats::var)),
               tolerance = 1e-10)
})

# ---- gate 3: order invariance -----------------------------------------------

test_that("permuting the groups changes nothing, because summation commutes", {
  # This is the property whose ABSENCE made the historical sequential
  # posterior-as-prior scheme unacceptable: there, whichever network was
  # entered first shaped the prior for the rest. Here it is true by
  # construction rather than measured after the fact, and the test pins the
  # construction.
  groups <- .pool_fixture(k = 4, p = 3)
  a <- .pool_beta_precision(groups)
  b <- .pool_beta_precision(rev(groups))
  c3 <- .pool_beta_precision(groups[c(3, 1, 4, 2)])

  expect_equal(a$mean, b$mean, tolerance = 1e-12)
  expect_equal(a$mean, c3$mean, tolerance = 1e-12)
  expect_equal(a$var, b$var, tolerance = 1e-12)
  expect_equal(a$var, c3$var, tolerance = 1e-12)
})

# ---- refusals and reporting -------------------------------------------------

test_that("groups fitted with different coefficient counts cannot be pooled", {
  g1 <- .pool_fixture(k = 1, p = 2)[[1]]
  g2 <- .pool_fixture(k = 1, p = 3, seed = 9)[[1]]
  expect_error(.pool_beta_precision(list(g1, g2)), "same thing in every group")
})

test_that("a ZERO posterior variance is refused, not floored", {
  # A constant draw column means the sampler never moved that coefficient in
  # that group. Zero is not a very small variance -- it carries no information
  # about the coefficient's uncertainty at all, and inverse-variance weighting
  # would hand that group effectively all of the weight, returning one group's
  # estimate under a pooled label. That is a wrong number rather than an
  # imprecise one, so it refuses.
  const <- matrix(rep(1.5, 200), ncol = 1L, dimnames = list(NULL, "b1"))
  set.seed(2)
  ordinary <- matrix(stats::rnorm(200), ncol = 1L, dimnames = list(NULL, "b1"))
  err <- tryCatch(
    .pool_beta_precision(list(
      list(extracted = list(draws = const)),
      list(extracted = list(draws = ordinary))
    )),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "zero or non-finite posterior variance", fixed = TRUE)
  expect_match(err, "b1", fixed = TRUE)
  # It says what the situation usually means and what to do about it.
  expect_match(err, "sampler did not update the coefficient", fixed = TRUE)
})

test_that("a variance small enough to reach the guard is NAMED, not absorbed", {
  # THE FLOOR'S OWN TEST, where it is the subject rather than a confound. A
  # positive-but-underflowing variance is not degenerate -- the sampler did
  # move -- so it is floored to keep 1/v computable, and the fact is reported
  # rather than left to be inferred from an implausibly tight interval.
  set.seed(2)
  tiny <- matrix(1.5 + stats::rnorm(200, sd = 1e-9), ncol = 1L,
                 dimnames = list(NULL, "b1"))
  ordinary <- matrix(stats::rnorm(200), ncol = 1L, dimnames = list(NULL, "b1"))
  expect_lt(stats::var(tiny[, 1]), .POOL_VARIANCE_FLOOR)

  pooled <- .pool_beta_precision(list(
    list(extracted = list(draws = tiny)),
    list(extracted = list(draws = ordinary))
  ))
  expect_identical(pooled$floored, "b1")
  expect_match(paste(.format_beta_pooling(pooled), collapse = " "),
               "numerical floor", fixed = TRUE)
})

test_that("the floor is rnetis's own constant, not inherited as methodology", {
  # It exists so that 1/v is computable when v has underflowed. `lame` happens
  # to use the same value; that is a coincidence of two implementations
  # guarding the same division, and the equivalence gate is built so it cannot
  # depend on it. If this constant changed, no result on a well-behaved
  # fixture would move.
  expect_true(is.numeric(.POOL_VARIANCE_FLOOR))
  expect_length(.POOL_VARIANCE_FLOOR, 1L)
  expect_gt(.POOL_VARIANCE_FLOOR, 0)

  ordinary <- .pool_fixture(k = 3, p = 2)
  expect_length(.pool_beta_precision(ordinary)$floored, 0L)
})

# ---- coefficients are matched BY NAME, never by position --------------------

test_that("groups whose columns arrive in a different ORDER still pool correctly", {
  # THE FAILURE THIS PREVENTS. Independently fitted panels can present the same
  # coefficients in different column orders. Pooling the third column with the
  # third column because both have three would combine different parameters
  # into one estimate -- and the result would have the right shape, the right
  # names, and the wrong meaning, so nothing downstream could detect it.
  mk <- function(mu, sd, nms) {
    set.seed(11)
    z1 <- scale(stats::rnorm(400))[, 1]
    set.seed(12)
    z2 <- scale(stats::rnorm(400))[, 1]
    d <- cbind(mu[1] + sd[1] * z1, mu[2] + sd[2] * z2)
    colnames(d) <- nms
    list(extracted = list(draws = d))
  }
  in_order <- mk(c(2, 10), c(1, 1), c("a", "b"))
  # Same two coefficients, columns swapped.
  swapped_raw <- mk(c(10, 2), c(1, 1), c("b", "a"))

  pooled <- .pool_beta_precision(list(in_order, swapped_raw))

  # `a` has mean 2 in both groups and `b` has mean 10 in both, so a correct
  # name-matched pooling returns exactly those. A positional pooling would
  # average 2 with 10 and give 6 for both.
  expect_equal(unname(pooled$mean[["a"]]), 2, tolerance = 1e-8)
  expect_equal(unname(pooled$mean[["b"]]), 10, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(unname(pooled$mean[["a"]]), 6)))
})

test_that("a group with DIFFERENT coefficients is refused, naming the difference", {
  mk <- function(nms) {
    set.seed(3)
    d <- matrix(stats::rnorm(100 * length(nms)), ncol = length(nms),
                dimnames = list(NULL, nms))
    list(extracted = list(draws = d))
  }
  err <- tryCatch(.pool_beta_precision(list(mk(c("a", "b")), mk(c("a", "c")))),
                  error = function(e) conditionMessage(e))
  expect_match(err, "different coefficients", fixed = TRUE)
  # It NAMES which, rather than reporting a count.
  expect_match(err, "c", fixed = TRUE)
  expect_match(err, "b", fixed = TRUE)
})

test_that("unnamed coefficient columns are refused rather than pooled by position", {
  d <- matrix(stats::rnorm(200), ncol = 2L)
  err <- tryCatch(
    .pool_beta_precision(list(list(extracted = list(draws = d)),
                              list(extracted = list(draws = d)))),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "unnamed coefficient columns", fixed = TRUE)
  expect_match(err, "BY NAME", fixed = TRUE)
})

test_that("duplicated coefficient names are refused as ambiguous", {
  d <- matrix(stats::rnorm(200), ncol = 2L, dimnames = list(NULL, c("a", "a")))
  expect_error(
    .pool_beta_precision(list(list(extracted = list(draws = d)),
                              list(extracted = list(draws = d)))),
    "duplicated coefficient names"
  )
})

test_that("the pooling sentence states the assumption it encodes", {
  pooled <- .pool_beta_precision(.pool_fixture(k = 2))
  txt <- paste(.format_beta_pooling(pooled), collapse = " ")
  # A default that encodes an assumption has to surface it, and pooling is the
  # default on multi-group data.
  expect_match(txt, "COMMON across groups", fixed = TRUE)
  expect_match(txt, "no between-group variance", fixed = TRUE)
  expect_match(txt, "joint tests are not", fixed = TRUE)
})

test_that("nothing is printed when pooling was turned off", {
  expect_length(.format_beta_pooling(NULL), 0L)
})
