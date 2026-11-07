# Known-answer tests for the network construction layer: binarize(),
# symmetrize(), multiplex(), and the shared resolver .resolve_network().
#
# .resolve_network() is called directly (an internal, unexported function)
# rather than through a public formula term, because centrality() and
# network_stat() are built on top of this layer and testing through them would
# not isolate it.

# ---- fixtures ---------------------------------------------------------------

# Signed network: a->b = 3 (positive), a->c = 0 (measured zero), a->d = -2
# (negative). The motivating example for the whole rule set: every binarize
# rule must be shown to disagree with the others on it where it should.
#
# zero_meaning IS DECLARED HERE, and that is the point of the fixture rather than
# boilerplate. The zero is a measured zero-strength relationship, which is what
# makes `presence()` and `nonzero()` give different answers on this network. It
# used to be left to inference, and rnetis now refuses that: an inferred
# row_presence table containing stored zeros cannot say whether a zero is an
# observed zero-strength tie or an encoding of absence, and it will not guess.
# Declaring it makes the fixture state the assumption its assertions rest on.
.signed_fixture <- function() {
  nodes <- data.frame(group = 1, node = c("a", "b", "c", "d"))
  ties <- data.frame(group = 1, source = c("a", "a", "a"), target = c("b", "c", "d"), w = c(3, 0, -2))
  netis_data(nodes, ties,
             tie_meta = list(w = list(zero_meaning = "measured_zero")))
}

# Deliberately asymmetric directed network: a->b = 10, b->a = 1.
.asymmetric_fixture <- function() {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "a"), w = c(10, 1))
  netis_data(nodes, ties, self_ties = "prohibited")
}

# ---- binarize(): the signed/measured-zero known-answer suite ----------------

test_that("presence() reads structural presence, not value: a measured zero still counts", {
  nd <- .signed_fixture()
  r <- .resolve_network(binarize(w, rule = presence()), nd, "1", NULL)
  expect_equal(r$M["a", "b"], 1) # present, positive value
  expect_equal(r$M["a", "c"], 1) # present, value is a measured zero
  expect_equal(r$M["a", "d"], 1) # present, negative value
  expect_equal(r$M["b", "a"], 0) # no row at all
  expect_false(r$weighted)
})

test_that("nonzero() counts both the positive and the negative value, not the measured zero", {
  nd <- .signed_fixture()
  r <- .resolve_network(binarize(w, rule = nonzero()), nd, "1", NULL)
  expect_equal(r$M["a", "b"], 1)
  expect_equal(r$M["a", "c"], 0)
  expect_equal(r$M["a", "d"], 1)
})

test_that("positive() counts only the positive value", {
  nd <- .signed_fixture()
  r <- .resolve_network(binarize(w, rule = positive()), nd, "1", NULL)
  expect_equal(r$M["a", "b"], 1)
  expect_equal(r$M["a", "c"], 0)
  expect_equal(r$M["a", "d"], 0)
})

test_that("threshold(c) is a strict greater-than", {
  nd <- .signed_fixture()
  r <- .resolve_network(binarize(w, rule = threshold(2)), nd, "1", NULL)
  expect_equal(r$M["a", "b"], 1) # 3 > 2
  expect_equal(r$M["a", "c"], 0)
  expect_equal(r$M["a", "d"], 0) # -2 is not > 2
  r2 <- .resolve_network(binarize(w, rule = threshold(3)), nd, "1", NULL)
  expect_equal(r2$M["a", "b"], 0) # 3 is not > 3
})

test_that("abs_threshold(c) counts both a strongly positive and a strongly negative value", {
  nd <- .signed_fixture()
  r <- .resolve_network(binarize(w, rule = abs_threshold(1)), nd, "1", NULL)
  expect_equal(r$M["a", "b"], 1) # |3| > 1
  expect_equal(r$M["a", "c"], 0) # |0| not > 1
  expect_equal(r$M["a", "d"], 1) # |-2| > 1
})

test_that("presence() on a complete_dyadic network with undetermined presence errors", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- expand.grid(source = c("a", "b", "c"), target = c("a", "b", "c"), stringsAsFactors = FALSE)
  ties <- ties[ties$source != ties$target, ]
  ties$group <- 1
  ties$w <- 0
  nd <- netis_data(nodes, ties, self_ties = "prohibited")
  expect_equal(netis_schema(nd)$tie_descriptors$w$presence_mode, "complete_dyadic")
  expect_error(.resolve_network(binarize(w, rule = presence()), nd, "1", NULL), "undetermined")
})

test_that("presence() on a derived (symmetrized or multiplexed) network refuses", {
  nd <- .asymmetric_fixture()
  expect_error(
    .resolve_network(binarize(symmetrize(w, rule = "max"), rule = presence()), nd, "1", NULL),
    "derived network"
  )
})

test_that("an invalid rule argument to binarize() is rejected", {
  nd <- .signed_fixture()
  expect_error(binarize(w, rule = "presence"), "rule must be")
})

# ---- binarize(): data-derived thresholds and scope --------------------------

test_that("above_mean()/above_median()/quantile_threshold() compute the documented statistic", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c", "d"))
  ties <- data.frame(group = 1, source = "a", target = c("b", "c", "d"), w = c(1, 2, 9))
  nd <- netis_data(nodes, ties)
  # mean(1,2,9) = 4 -> only 9 is above it
  r_mean <- .resolve_network(binarize(w, rule = above_mean()), nd, "1", NULL)
  expect_equal(unname(r_mean$M["a", ]), c(0, 0, 0, 1))
  expect_equal(r_mean$resolved$threshold_used, 4)
  # median(1,2,9) = 2 -> only 9 is above it
  r_median <- .resolve_network(binarize(w, rule = above_median()), nd, "1", NULL)
  expect_equal(unname(r_median$M["a", ]), c(0, 0, 0, 1))
})

test_that("quantile_threshold() computes the real stats::quantile() value, not a stand-in for mean/median", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c", "d"))
  ties <- data.frame(group = 1, source = "a", target = c("b", "c", "d"), w = c(1, 2, 9))
  nd <- netis_data(nodes, ties)
  # stats::quantile(c(1, 2, 9), probs = 0.9) with the default (type 7)
  # interpolation: position = 1 + (3-1)*0.9 = 2.8, between sorted[2]=2 and
  # sorted[3]=9 -> 2 + 0.8*(9-2) = 7.6 -- distinct from both the mean (4)
  # and the median (2), so this genuinely exercises quantile() itself.
  r <- .resolve_network(binarize(w, rule = quantile_threshold(0.9)), nd, "1", NULL)
  expect_equal(r$resolved$threshold_used, 7.6, tolerance = 1e-10)
  expect_equal(unname(r$M["a", ]), c(0, 0, 0, 1)) # only 9 > 7.6
})

test_that("threshold scope 'group' pools across waves within the resolved group only", {
  # group 1: value 10 at every wave. group 2: value 1 at every wave.
  nodes <- data.frame(group = rep(c(1, 2), each = 6), node = rep(rep(c("a", "b", "c"), 2), 2), wave = rep(rep(1:2, each = 3), 2))
  ties <- rbind(
    data.frame(group = 1, source = "a", target = "b", wave = 1, w = 10),
    data.frame(group = 1, source = "a", target = "b", wave = 2, w = 10),
    data.frame(group = 2, source = "a", target = "b", wave = 1, w = 1),
    data.frame(group = 2, source = "a", target = "b", wave = 2, w = 1)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  # scope = "group": pool is group 1's own values across both waves (10, 10)
  # -> mean 10 -> 10 > 10 is FALSE.
  r_group <- .resolve_network(binarize(w, rule = above_mean(scope = "group")), nd, "1", 1)
  expect_equal(r_group$M["a", "b"], 0)
  expect_equal(r_group$resolved$threshold_used, 10)

  # scope = "wave": pool is every group's value at wave 1 (10, 1) -> mean
  # 5.5 -> 10 > 5.5 is TRUE. Genuinely different from the "group" scope
  # result above, proving the two scopes pool over different axes.
  r_wave <- .resolve_network(binarize(w, rule = above_mean(scope = "wave")), nd, "1", 1)
  expect_equal(r_wave$M["a", "b"], 1)
  expect_equal(r_wave$resolved$threshold_used, 5.5)
})

test_that("threshold scope 'snapshot' vs 'global' can produce different results (anti-leakage)", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b", "c"), 2), wave = rep(1:2, each = 3))
  ties1 <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), wave = c(1, 1), w = c(5, 15))
  ties2 <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), wave = c(2, 2), w = c(1, 1000))
  nd <- netis_data(nodes, rbind(ties1, ties2), wave_col = "wave")

  # snapshot: mean(5, 15) = 10 -> a-c (15) is above it.
  r_snap <- .resolve_network(binarize(w, rule = above_mean(scope = "snapshot")), nd, "1", 1)
  expect_equal(r_snap$M["a", "c"], 1)
  expect_equal(r_snap$resolved$threshold_used, 10)

  # global: mean(5, 15, 1, 1000) = 255.25 -> a-c (15) is no longer above it,
  # even though we are still resolving wave 1 -- the later wave's data
  # changed an earlier wave's network, exactly the leak the default guards
  # against.
  r_glob <- .resolve_network(binarize(w, rule = above_mean(scope = "global")), nd, "1", 1)
  expect_equal(r_glob$M["a", "c"], 0)
  expect_false(identical(r_snap$M, r_glob$M))
})

test_that("a data-derived threshold on a multiplex-derived network (no single variable) errors", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", x = 1, y = 1)
  nd <- netis_data(nodes, ties)
  spec <- binarize(multiplex(x, y, combine = "count"), rule = above_mean())
  expect_error(.resolve_network(spec, nd, "1", NULL), "direct tie-variable reference")
})

test_that("a data-derived threshold on a symmetrized network (no faithful snapshot pool) errors", {
  nd <- .asymmetric_fixture()
  spec <- binarize(symmetrize(w, rule = "max"), rule = above_mean())
  expect_error(.resolve_network(spec, nd, "1", NULL), "direct tie-variable reference")
})

# ---- symmetrize(): all six rules, hand-verified -----------------------------

test_that("symmetrize() rules on a deliberately asymmetric network (a->b=10, b->a=1)", {
  nd <- .asymmetric_fixture()

  r_max <- .resolve_network(symmetrize(w, rule = "max"), nd, "1", NULL)
  expect_equal(r_max$M["a", "b"], 10)
  expect_equal(r_max$M["b", "a"], 10)

  r_min <- .resolve_network(symmetrize(w, rule = "min"), nd, "1", NULL)
  expect_equal(r_min$M["a", "b"], 1)
  expect_equal(r_min$M["b", "a"], 1)

  r_sum <- .resolve_network(symmetrize(w, rule = "sum"), nd, "1", NULL)
  expect_equal(r_sum$M["a", "b"], 11)
  expect_equal(r_sum$M["b", "a"], 11)

  r_mean <- .resolve_network(symmetrize(w, rule = "mean"), nd, "1", NULL)
  expect_equal(r_mean$M["a", "b"], 5.5)
  expect_equal(r_mean$M["b", "a"], 5.5)

  r_any <- .resolve_network(symmetrize(w, rule = "any"), nd, "1", NULL)
  expect_equal(r_any$M["a", "b"], 1)
  expect_equal(r_any$M["a", "c"], 0)

  r_both <- .resolve_network(symmetrize(w, rule = "both"), nd, "1", NULL)
  expect_equal(r_both$M["a", "b"], 1) # reciprocal
  expect_equal(r_both$M["a", "c"], 0) # not reciprocal (no tie at all)

  for (r in list(r_max, r_min, r_sum, r_mean, r_any, r_both)) {
    expect_false(r$directed)
    expect_true(isSymmetric(unname(r$M)))
  }
})

test_that("symmetrize() builds a derived matrix without mutating the canonical network", {
  nd <- .asymmetric_fixture()
  before <- netis_ties(nd)
  .resolve_network(symmetrize(w, rule = "max"), nd, "1", NULL)
  after <- netis_ties(nd)
  expect_equal(before, after)
})

test_that("an invalid symmetrize() rule is rejected by match.arg()", {
  expect_error(symmetrize(w, rule = "bogus"))
})

# ---- multiplex(): set-like and value-combining rules ------------------------

.multiplex_fixture <- function() {
  # This table is sparse (2 of 6 possible ordered pairs among 3 nodes), so
  # presence_mode infers to "row_presence" -- under which a comm/adv value
  # of 0 at an EXISTING row still counts as present (the row exists; by
  # default only presence-vs-absence-of-a-row, not the value, decides that). The fixture's own intent is
  # value-based presence (0 means "this relation doesn't apply here"), so
  # that must be declared explicitly via tie_meta -- exactly the "never
  # assume zero = absent unless the schema says so" invariant this fixture
  # would otherwise silently violate.
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- rbind(
    data.frame(group = 1, source = "a", target = "b", comm = 1, adv = 0, fr = 1),
    data.frame(group = 1, source = "a", target = "c", comm = 0, adv = 1, fr = 1)
  )
  presence_nonzero <- list(presence_mode = "value_rule", presence_rule = function(w) w != 0)
  netis_data(nodes, ties, tie_meta = list(comm = presence_nonzero, adv = presence_nonzero, fr = presence_nonzero))
}

test_that("multiplex() union/intersection/k_of_n/count agree with hand computation", {
  nd <- .multiplex_fixture()
  # a-b: comm=1, adv=0, fr=1 (2 of 3 present). a-c: comm=0, adv=1, fr=1 (2 of 3 present).

  r_union <- .resolve_network(multiplex(comm, adv, fr, combine = "union"), nd, "1", NULL)
  expect_equal(r_union$M["a", "b"], 1)
  expect_equal(r_union$M["a", "c"], 1)

  r_inter <- .resolve_network(multiplex(comm, adv, fr, combine = "intersection"), nd, "1", NULL)
  expect_equal(r_inter$M["a", "b"], 0) # adv is 0 at a-b
  expect_equal(r_inter$M["a", "c"], 0) # comm is 0 at a-c

  r_k2 <- .resolve_network(multiplex(comm, adv, fr, combine = "k_of_n", k = 2), nd, "1", NULL)
  expect_equal(r_k2$M["a", "b"], 1)
  expect_equal(r_k2$M["a", "c"], 1)

  r_k3 <- .resolve_network(multiplex(comm, adv, fr, combine = "k_of_n", k = 3), nd, "1", NULL)
  expect_equal(r_k3$M["a", "b"], 0)
  expect_equal(r_k3$M["a", "c"], 0)

  r_count <- .resolve_network(multiplex(comm, adv, fr, combine = "count"), nd, "1", NULL)
  expect_equal(r_count$M["a", "b"], 2)
  expect_equal(r_count$M["a", "c"], 2)
  expect_true(r_count$weighted)
})

test_that("multiplex() combine = 'any'/'all' are aliases for union/intersection", {
  nd <- .multiplex_fixture()
  r_any <- .resolve_network(multiplex(comm, adv, fr, combine = "any"), nd, "1", NULL)
  r_union <- .resolve_network(multiplex(comm, adv, fr, combine = "union"), nd, "1", NULL)
  expect_equal(r_any$M, r_union$M)

  r_all <- .resolve_network(multiplex(comm, adv, fr, combine = "all"), nd, "1", NULL)
  r_inter <- .resolve_network(multiplex(comm, adv, fr, combine = "intersection"), nd, "1", NULL)
  expect_equal(r_all$M, r_inter$M)
})

test_that("multiplex(combine = 'sum'/'mean') on compatible-scale weighted networks succeeds", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", freq1 = 3, freq2 = 5)
  nd <- netis_data(nodes, ties, tie_meta = list(
    freq1 = list(weight_semantics = "count"), freq2 = list(weight_semantics = "count")
  ))
  r_sum <- .resolve_network(multiplex(freq1, freq2, combine = "sum"), nd, "1", NULL)
  expect_equal(r_sum$M["a", "b"], 8)
  r_mean <- .resolve_network(multiplex(freq1, freq2, combine = "mean"), nd, "1", NULL)
  expect_equal(r_mean$M["a", "b"], 4)
})

test_that("multiplex(combine = 'sum') refuses incompatible declared weight_semantics", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", freq = 5, fship = 2)
  nd <- netis_data(nodes, ties, tie_meta = list(
    freq = list(weight_semantics = "count"), fship = list(weight_semantics = "strength")
  ))
  expect_error(.resolve_network(multiplex(freq, fship, combine = "sum"), nd, "1", NULL), "incompatible")
})

test_that("multiplex(combine = 'sum') refuses mixing a weighted and a binary component", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", freq = 5, tie = 1)
  nd <- netis_data(nodes, ties)
  expect_error(.resolve_network(multiplex(freq, tie, combine = "sum"), nd, "1", NULL), "weighted network with a binary")
})

test_that("multiplex(combine = 'union') never triggers the scale-compatibility check", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", freq = 5, fship = 2)
  nd <- netis_data(nodes, ties, tie_meta = list(
    freq = list(weight_semantics = "count"), fship = list(weight_semantics = "strength")
  ))
  r <- .resolve_network(multiplex(freq, fship, combine = "union"), nd, "1", NULL)
  expect_type(r, "list")
  expect_equal(r$M["a", "b"], 1)
})

test_that("multiplex() requires at least two networks", {
  expect_error(multiplex(w, combine = "union"), "at least two")
})

test_that("multiplex(combine = 'k_of_n') requires a valid k", {
  expect_error(multiplex(a, b, combine = "k_of_n"), "k")
  expect_error(multiplex(a, b, combine = "k_of_n", k = 5), "k")
})

# ---- nested construction ----------------------------------------------------

test_that("network-construction calls compose: symmetrize(binarize(x))", {
  nd <- .signed_fixture()
  r <- .resolve_network(symmetrize(binarize(w, rule = nonzero()), rule = "max"), nd, "1", NULL)
  expect_equal(r$M["a", "b"], 1)
  expect_equal(r$M["b", "a"], 1) # symmetrized
  expect_equal(r$M["a", "c"], 0) # the measured zero, correctly excluded by nonzero()
  expect_equal(r$M["a", "d"], 1)
})

test_that("a bare column name is never evaluated against an unrelated same-named variable", {
  # A pathological case: a variable named exactly like the tie column exists
  # in the calling environment. binarize()'s NSE capture must still treat it
  # as a column-name reference, never evaluate it.
  w <- "not a network spec at all"
  nd <- .signed_fixture()
  spec <- binarize(w, rule = nonzero())
  expect_s3_class(spec$network, "netis_network_ref")
  expect_equal(spec$network$variable, "w")
})

test_that("an unknown tie-variable name errors naming the real candidates", {
  nd <- .signed_fixture()
  expect_error(.resolve_network(binarize(bogus, rule = nonzero()), nd, "1", NULL), "bogus")
})

test_that("a non-network-spec argument to a network-construction call is rejected", {
  expect_error(binarize(1 + 1, rule = nonzero()), "network")
})

# ---- regression: multiplex() presence resolution bypassed .resolve_tie_presence() --
# (guards against the case where union/intersection/k_of_n/count used a naive
# `value != 0` on the raw resolved matrix instead of routing through
# .resolve_component_presence(), silently misreading a complete_dyadic/
# indicator-declared network's real presence semantics.)

test_that("multiplex() resolves each component's presence via its declared presence_mode, not raw value != 0", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- expand.grid(source = c("a", "b", "c"), target = c("a", "b", "c"), stringsAsFactors = FALSE)
  ties <- ties[ties$source != ties$target, ]
  rownames(ties) <- paste(ties$source, ties$target, sep = "->")
  ties$group <- 1
  ties$comm <- 0 # every measured value is 0
  ties$present <- FALSE
  ties["a->b", "present"] <- TRUE # a->b is the only real tie, per the indicator
  ties$adv <- 0
  ties["a->b", "adv"] <- 1
  nd <- netis_data(
    nodes, ties, self_ties = "prohibited",
    tie_meta = list(
      comm = list(presence_mode = "indicator", presence_col = "present"),
      adv = list(presence_mode = "value_rule", presence_rule = function(w) w != 0)
    )
  )
  r <- .resolve_network(multiplex(comm, adv, combine = "union"), nd, "1", NULL)
  expect_equal(r$M["a", "b"], 1) # comm present via indicator, adv present via value_rule
  expect_equal(r$M["a", "c"], 0) # comm NOT present (indicator FALSE) despite the row existing with value 0
})

# ---- regression: multiplex(sum/mean) discarded validated weight_semantics ---
# (guards against the case where hardcoded "unitless" even after
# .check_multiplex_scale_compatibility() confirmed a shared semantics category,
# silently disabling the network-feature weight-semantics guardrail downstream.)

test_that("multiplex(combine = 'sum'/'mean') propagates the validated shared weight_semantics", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", d1 = 2, d2 = 4)
  nd <- netis_data(nodes, ties, tie_meta = list(
    d1 = list(weight_semantics = "strength"), d2 = list(weight_semantics = "strength")
  ))
  r_sum <- .resolve_network(multiplex(d1, d2, combine = "sum"), nd, "1", NULL)
  expect_equal(r_sum$weight_semantics, "strength")
  r_mean <- .resolve_network(multiplex(d1, d2, combine = "mean"), nd, "1", NULL)
  expect_equal(r_mean$weight_semantics, "strength")
})

test_that("multiplex(combine = 'sum') stays 'unitless' when every component is undeclared", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", d1 = 2, d2 = 4)
  nd <- netis_data(nodes, ties) # no tie_meta -> both default to "unitless"
  r <- .resolve_network(multiplex(d1, d2, combine = "sum"), nd, "1", NULL)
  expect_equal(r$weight_semantics, "unitless")
})

# ---- regression: value-based binarize rules could turn absent ties into phantom ties --
# (guards against the case where a predicate satisfied at v = 0 -- e.g.
# threshold(c) with c < 0, or a custom rule -- was applied to M's fill value
# for cells with no underlying row, silently marking every non-edge present.)

test_that("threshold(c) with a negative c does not turn absent ties into phantom ties", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 3) # only a->b exists
  nd <- netis_data(nodes, ties) # sparse -> row_presence

  r <- .resolve_network(binarize(w, rule = threshold(-1)), nd, "1", NULL)
  expect_equal(r$M["a", "b"], 1) # 3 > -1, and the row genuinely exists
  expect_equal(r$M["a", "c"], 0) # no row a->c: must stay absent even though 0 > -1
  expect_equal(r$M["b", "a"], 0)
})

test_that("a custom_binarize_rule() true at v = 0 does not turn absent ties into phantom ties", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 3)
  nd <- netis_data(nodes, ties)
  r <- .resolve_network(binarize(w, rule = custom_binarize_rule(function(v) v >= 0)), nd, "1", NULL)
  expect_equal(r$M["a", "b"], 1)
  expect_equal(r$M["a", "c"], 0)
})

test_that("a value-based rule still applies normally to a complete_dyadic network's genuinely-recorded zeros", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- expand.grid(source = c("a", "b", "c"), target = c("a", "b", "c"), stringsAsFactors = FALSE)
  ties <- ties[ties$source != ties$target, ]
  ties$group <- 1
  ties$w <- 0
  ties$w[ties$source == "a" & ties$target == "b"] <- 3
  nd <- netis_data(
    nodes, ties, self_ties = "prohibited",
    tie_meta = list(w = list(presence_mode = "value_rule", presence_rule = function(x) TRUE))
  )
  r <- .resolve_network(binarize(w, rule = threshold(-1)), nd, "1", NULL)
  # Every dyad has a genuinely recorded row (complete_dyadic); a value rule
  # legitimately applies everywhere a row exists, including recorded zeros.
  expect_equal(r$M["a", "c"], 1)
})

test_that("custom_binarize_rule() rejects a non-function fn", {
  expect_error(custom_binarize_rule("not a function"), "function")
})
