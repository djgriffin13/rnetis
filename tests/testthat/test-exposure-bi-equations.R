# Known-answer tests for bi(), written from Frank & Fahrbach (1999) equations
# (5), (6), (8) and (11) AS CORRECTED BY THE ERRATUM (Organization Science
# 10(4), 514-515), not from the previous implementation.
#
# Every expected value below is computed by hand from the equations and written
# into the test. Where the old implementation would give a different answer,
# that is said at the assertion, because the point of this file is that the two
# disagree.

# ---- fixtures ---------------------------------------------------------------

# A parallel-path network. a's wave-1 shock reaches b by two 2-step routes,
# a->c->b and a->d->b, and by no 1-step route. Chosen because it is the
# smallest network where the all-path and maximal-path accumulations differ.
.bi_parallel <- function(shock_a = 10) {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c", "d"), 3), wave = rep(1:3, each = 4),
    shock = c(shock_a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
  ties <- data.frame(
    group = 1,
    source = c("a", "a", "c", "d"),
    target = c("c", "d", "b", "b"),
    wave = c(1, 1, 2, 2),
    link = 1
  )
  netis_data(nodes, ties, wave_col = "wave")
}

.bi_ex <- function(nd, ...) {
  out <- compute_exposure(nd, "shock", "link", wave = 3, ...)
  stats::setNames(out[[grep("^ex_|^A_maxpath$", names(out))[1]]], out$node)
}

# ---- equation (11): the delta blend at its published endpoints --------------

test_that("delta = 0 gives the maximal path only (Model 10), hand-computed", {
  ex <- .bi_ex(.bi_parallel(), mechanism = bi(delta = 0, alpha = 1, gamma = 0))
  # Two 2-step paths, each of product 1 * 1 = 1 with alpha = 1. The maximum over
  # them is 1, applied to a's wave-1 shock of 10.
  expect_equal(unname(ex[["b"]]), 10)
})

test_that("delta = 1 accumulates both parallel paths (Model 3), hand-computed", {
  ex <- .bi_ex(.bi_parallel(), mechanism = bi(delta = 1, alpha = 1, gamma = 0))
  # Sum over both routes: 1 * 10 + 1 * 10 = 20.
  expect_equal(unname(ex[["b"]]), 20)
})

test_that("a midrange delta interpolates exactly as equation (11) says", {
  ex <- .bi_ex(.bi_parallel(), mechanism = bi(delta = 0.8, alpha = 1, gamma = 0))
  # MP + delta (Q - MP) = 10 + 0.8 * (20 - 10) = 18. Their Figure 4 value.
  expect_equal(unname(ex[["b"]]), 18)
})

test_that("delta outside [0, 1] is refused, naming the published endpoints", {
  expect_error(bi(delta = 1.5, alpha = 1, gamma = 0), "\\[0, 1\\]")
  expect_error(bi(delta = -0.1, alpha = 1, gamma = 0), "\\[0, 1\\]")
})

# ---- alpha: the interpersonal transmission factor ---------------------------

test_that("alpha enters once per BETWEEN-ACTOR step, so a 2-step path carries alpha^2", {
  ex <- .bi_ex(.bi_parallel(), mechanism = bi(delta = 1, alpha = 0.5, gamma = 0))
  # Each route is two between-actor steps: (0.5 * 1) * (0.5 * 1) = 0.25.
  # Two routes, so 2 * 0.25 * 10 = 5.
  expect_equal(unname(ex[["b"]]), 5)

  # The old implementation had no alpha at all, i.e. alpha == 1 always, and
  # would have returned 20 here.
  expect_false(isTRUE(all.equal(unname(ex[["b"]]), 20)))
})

test_that("alpha's depth profile is not absorbable into one scale factor", {
  # THE POINT OF THIS TEST. If alpha only rescaled the exposure column, a single
  # regression coefficient could compensate for getting it wrong. It does not:
  # depth-1 and depth-2 contributions are scaled by alpha and alpha^2, so their
  # RATIO moves with alpha, and no scalar can undo that.
  #
  # A network with one 1-step route (a->b at wave 2) and one 2-step route
  # (a->c at wave 1, c->b at wave 2), carrying shocks at both wave 1 and wave 2.
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c"), 3), wave = rep(1:3, each = 3),
    shock = c(10, 0, 0,   10, 0, 0,   0, 0, 0)
  )
  ties <- data.frame(
    group = 1,
    source = c("a", "a", "c"),
    target = c("c", "b", "b"),
    wave = c(1, 2, 2),
    link = 1
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  half <- .bi_ex(nd, mechanism = bi(delta = 1, alpha = 0.5, gamma = 0))
  one <- .bi_ex(nd, mechanism = bi(delta = 1, alpha = 1, gamma = 0))

  # alpha = 1: depth-1 gives 1 * 10 = 10, depth-2 gives 1 * 10 = 10, total 20.
  expect_equal(unname(one[["b"]]), 20)
  # alpha = 0.5: depth-1 gives 0.5 * 10 = 5, depth-2 gives 0.25 * 10 = 2.5.
  expect_equal(unname(half[["b"]]), 7.5)
  # 7.5 is not 20 times any single power of 0.5 that also fixes both depths --
  # the depth-1 and depth-2 shares are 2:1 at alpha = 1 and 2:1 at 0.5 only in
  # this contrived balance; what matters is that 7.5 != 0.5 * 20 and
  # 7.5 != 0.25 * 20.
  expect_false(isTRUE(all.equal(unname(half[["b"]]), 0.5 * 20)))
  expect_false(isTRUE(all.equal(unname(half[["b"]]), 0.25 * 20)))
})

# ---- gamma: self-retention as an INTERIOR path step -------------------------

# One tie, a->b, present at BOTH prior waves -- written out per wave rather than
# left to rnetis's static-network reuse, so the arithmetic below can be checked
# without knowing that rule. Under the canonical W[i, j] = "j influences i", the
# combined matrix at each wave is
#
#            a      b
#   a  [  gamma     0   ]
#   b  [  alpha   gamma ]
#
# and a carries a shock of 10 at wave 1 only.
.bi_hold <- function() {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b"), 3), wave = rep(1:3, each = 2),
    shock = c(10, 0,  0, 0,  0, 0)
  )
  ties <- data.frame(group = 1, source = "a", target = "b",
                     wave = c(1, 2), link = 1)
  netis_data(nodes, ties, wave_col = "wave")
}

test_that("gamma weights a within-actor step INSIDE a path, which ar() cannot do", {
  # There are exactly two depth-2 routes from a's wave-1 shock to b, and each
  # uses gamma at one interior step:
  #
  #   a holds at wave 1 (gamma), then a->b at wave 2 (alpha)   -> alpha * gamma
  #   a->b at wave 1 (alpha),    then b holds at wave 2 (gamma) -> gamma * alpha
  #
  # so Q_2[b, a] = 2 * alpha * gamma. This is the whole reason gamma cannot be
  # replaced by ar(): an additive lagged-outcome term applied at the outermost
  # level of the regression contributes no interior step, so neither of these
  # routes would exist.
  no_hold <- .bi_ex(.bi_hold(), mechanism = bi(delta = 1, alpha = 1, gamma = 0))
  hold <- .bi_ex(.bi_hold(), mechanism = bi(delta = 1, alpha = 1, gamma = 0.5))

  # With gamma = 0 both routes vanish and the depth-2 contribution is exactly 0.
  expect_equal(unname(no_hold[["b"]]), 0)
  # With gamma = 0.5: 2 * 1 * 0.5 = 1, times the shock of 10.
  expect_equal(unname(hold[["b"]]), 10)
})

test_that("the maximal path takes ONE gamma route, the all-path sum takes both", {
  # THE SHARPEST TEST HERE: MP and Q differ through gamma alone, on a network
  # with no parallel interpersonal routes at all.
  #   MP_2[b, a] = max(alpha*gamma, gamma*alpha) = alpha * gamma      = 0.5
  #   Q_2 [b, a] =     alpha*gamma + gamma*alpha = 2 * alpha * gamma  = 1.0
  mp <- .bi_ex(.bi_hold(), mechanism = bi(delta = 0, alpha = 1, gamma = 0.5))
  q <- .bi_ex(.bi_hold(), mechanism = bi(delta = 1, alpha = 1, gamma = 0.5))
  expect_equal(unname(mp[["b"]]), 5)
  expect_equal(unname(q[["b"]]), 10)
})

test_that("gamma = 1 retains the shock undiminished across each held step", {
  ex <- .bi_ex(.bi_hold(), mechanism = bi(delta = 1, alpha = 1, gamma = 1))
  # 2 * alpha * gamma = 2, times 10.
  expect_equal(unname(ex[["b"]]), 20)
})

# ---- the diagonal convention ------------------------------------------------

test_that("a conduit with a nonzero diagonal is REFUSED, not silently hollowed", {
  # Equation (5)'s k_ii is the identity and gamma is the only free quantity on
  # the diagonal, so a supplied self-loop has no slot in the published model.
  # `gamma * I + alpha * k` against a non-hollow k would compute
  # gamma + alpha * k_ii and double-count self-retention in silence.
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b"), 3), wave = rep(1:3, each = 2),
    shock = c(10, 0,  0, 0,  0, 0)
  )
  ties <- data.frame(
    group = 1,
    source = c("a", "a"), target = c("b", "a"),   # a->a is the self-loop
    wave = c(2, 2), link = c(1, 0.3)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave", self_ties = "permitted")

  expect_error(
    compute_exposure(nd, "shock", "link", wave = 3,
                     mechanism = bi(delta = 1, alpha = 1, gamma = 0.5)),
    "off-diagonal only"
  )
})

test_that("the refusal names gamma as where self-retention belongs", {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b"), 3), wave = rep(1:3, each = 2),
    shock = c(10, 0,  0, 0,  0, 0)
  )
  ties <- data.frame(
    group = 1, source = c("a", "a"), target = c("b", "a"),
    wave = c(2, 2), link = c(1, 0.3)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave", self_ties = "permitted")
  err <- tryCatch(
    compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 1, alpha = 1, gamma = 0)),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "gamma", fixed = TRUE)
  expect_match(err, "twice", fixed = TRUE)
})

# ---- bi_components(alpha = 1, gamma = 0): the two published quantities ------

test_that("bi_components(alpha = 1, gamma = 0) returns the maximal path and the redundant remainder", {
  out <- compute_exposure(.bi_parallel(), "shock", "link", wave = 3,
                          mechanism = bi_components(alpha = 1, gamma = 0))
  b <- out[out$node == "b", ]
  expect_equal(b$A_maxpath, 10)      # MP: the single strongest route
  expect_equal(b$B_redundant, 10)    # Q - MP: the second, redundant route
})

test_that("a network with no parallel paths has an exactly zero redundant part", {
  # A single chain a->c->b: one route, so Q == MP and the remainder vanishes.
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c"), 3), wave = rep(1:3, each = 3),
    shock = c(10, 0, 0,  0, 0, 0,  0, 0, 0)
  )
  ties <- data.frame(group = 1, source = c("a", "c"), target = c("c", "b"),
                     wave = c(1, 2), link = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  out <- compute_exposure(nd, "shock", "link", wave = 3,
                          mechanism = bi_components(alpha = 1, gamma = 0))
  expect_equal(out$B_redundant[out$node == "b"], 0)
})

test_that("bi_components(alpha = 1, gamma = 0) agrees with bi() at both endpoints, by construction", {
  comp <- compute_exposure(.bi_parallel(), "shock", "link", wave = 3,
                           mechanism = bi_components(alpha = 1, gamma = 0))
  d0 <- .bi_ex(.bi_parallel(), mechanism = bi(delta = 0, alpha = 1, gamma = 0))
  d1 <- .bi_ex(.bi_parallel(), mechanism = bi(delta = 1, alpha = 1, gamma = 0))
  expect_equal(comp$A_maxpath, unname(d0[comp$node]))
  expect_equal(comp$A_maxpath + comp$B_redundant, unname(d1[comp$node]))
})

# ---- refusals inherited from the mechanism's assumptions --------------------

test_that("a negative conduit weight is refused, naming the maximal-path reason", {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b"), 3), wave = rep(1:3, each = 2),
    shock = c(10, 0,  0, 0,  0, 0)
  )
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 2,
                     link = -1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(
    compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 1, alpha = 1, gamma = 0)),
    "nonnegative"
  )
})

test_that("row normalization is refused for a path-accumulating mechanism", {
  expect_error(
    compute_exposure(.bi_parallel(), "shock", "link", wave = 3,
                     mechanism = bi(delta = 0, alpha = 1, gamma = 0), normalize = "row"),
    "normaliz"
  )
})

# ---- provenance -------------------------------------------------------------

test_that("all three parameters are recorded, so a result can say what produced it", {
  p <- attr(bi(delta = 0.8, alpha = 0.5, gamma = 0.2), "mechanism_provenance")
  expect_identical(p$name, "bi")
  expect_equal(p$params$delta, 0.8)
  expect_equal(p$params$alpha, 0.5)
  expect_equal(p$params$gamma, 0.2)
})

# ---- bi_sweep(): sensitivity over the published parameters ------------------

test_that("bi_sweep() evaluates every combination and labels each row with it", {
  s <- bi_sweep(.bi_parallel(), "shock", "link",
                alpha = c(1, 0.5), gamma = 0, delta = c(0, 1), wave = 3)
  expect_equal(nrow(s), 4 * 4)                      # 4 combinations, 4 nodes
  expect_true(all(c("delta", "alpha", "gamma") %in% names(s)))
  expect_setequal(unique(s$delta), c(0, 1))
  expect_setequal(unique(s$alpha), c(1, 0.5))
})

test_that("bi_sweep() requires alpha and gamma, like bi() itself", {
  expect_error(bi_sweep(.bi_parallel(), "shock", "link", wave = 3))
})

test_that("each swept row equals the single call it stands for", {
  s <- bi_sweep(.bi_parallel(), "shock", "link", alpha = 1, gamma = 0,
                delta = c(0, 1), wave = 3)
  b0 <- s$ex_shock_link[s$node == "b" & s$delta == 0]
  b1 <- s$ex_shock_link[s$node == "b" & s$delta == 1]
  expect_equal(b0, 10)   # the maximal path alone
  expect_equal(b1, 20)   # both parallel routes
})

# ---- alpha and gamma are mandatory ------------------------------------------

test_that("bi() requires alpha and gamma, with no silent default", {
  # A DEFAULT HERE WOULD DELETE PUBLISHED MODEL COMPONENTS IN SILENCE.
  # gamma = 0 removes self-retention paths from the mechanism entirely, and
  # ar() does not replace them -- an additive outer term contributes no interior
  # path step. alpha = 1 asserts transmission is unattenuated beyond the conduit
  # weights, a legitimate special case that Frank & Fahrbach do not establish as
  # neutral. Both must be stated by whoever fits the model.
  expect_silent(bi(delta = 0.5, alpha = 1, gamma = 0))
  expect_error(bi(delta = 0.5))
  expect_error(bi(delta = 0.5, alpha = 1))
  expect_error(bi(delta = 0.5, gamma = 0))
})

test_that("bi_components() requires them too", {
  expect_error(bi_components())
  expect_error(bi_components(alpha = 1))
  expect_silent(bi_components(alpha = 1, gamma = 0))
})
