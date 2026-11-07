# Selection outcome-matrix assembly.
#
# These are known-answer tests on hand-built fixtures small enough to reason
# about cell by cell, which is the only way the three-way status distinction
# can actually be checked -- a matrix of the right shape full of plausible
# zeros is exactly the failure mode being guarded against.

test_that("a sparse edge list treats absent cells as observed zeros", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = "a", target = "b", friendship = 1)
  nd <- netis_data(nodes, ties)

  out <- .build_selection_y(nd, "friendship", group = 1, wave = NULL)

  expect_equal(out$Y["a", "b"], 1)
  expect_equal(out$Y["a", "c"], 0)          # observed non-tie, not missing
  expect_equal(out$status["a", "c"], "observed")
  expect_true(is.na(out$Y["a", "a"]))       # self-tie prohibited by default
  expect_equal(out$status["a", "a"], "structural")
  expect_equal(unname(out$counts["structural"]), 3L)
  expect_equal(unname(out$counts["missing"]), 0L)
})

test_that("a complete dyadic table treats absent cells as missing, not zero", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  # Rows for every ordered pair except b->c, declared complete_dyadic so that
  # a row means "observed" rather than "tie exists".
  pairs <- expand.grid(source = c("a", "b", "c"), target = c("a", "b", "c"),
                       stringsAsFactors = FALSE)
  pairs <- pairs[pairs$source != pairs$target, ]
  pairs <- pairs[!(pairs$source == "b" & pairs$target == "c"), ]
  ties <- data.frame(group = 1, pairs, friendship = 1, presence = TRUE)
  nd <- netis_data(
    nodes, ties,
    tie_meta = list(friendship = list(presence_mode = "complete_dyadic",
                                      presence_col = "presence",
                                      zero_meaning = "measured_zero"))
  )

  out <- .build_selection_y(nd, "friendship", group = 1, wave = NULL)

  expect_equal(out$status["b", "c"], "missing")
  expect_true(is.na(out$Y["b", "c"]))
  expect_equal(out$status["a", "b"], "observed")
  expect_equal(unname(out$counts["missing"]), 1L)
})

test_that("a zero declared structural is masked, not scored as a non-tie", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(
    group = 1,
    source = c("a", "a"), target = c("b", "c"),
    friendship = c(1, 0)
  )
  nd <- netis_data(
    nodes, ties,
    tie_meta = list(friendship = list(zero_meaning = "structural_absence"))
  )

  out <- .build_selection_y(nd, "friendship", group = 1, wave = NULL)

  # The a->c dyad could not exist, so it must not read as "chose not to tie".
  expect_equal(out$status["a", "c"], "structural")
  expect_true(is.na(out$Y["a", "c"]))
})

test_that("a zero declared measured is kept as information", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(
    group = 1,
    source = c("a", "a"), target = c("b", "c"),
    friendship = c(1, 0)
  )
  nd <- netis_data(
    nodes, ties,
    tie_meta = list(friendship = list(zero_meaning = "measured_zero"))
  )

  out <- .build_selection_y(nd, "friendship", group = 1, wave = NULL)
  expect_equal(out$Y["a", "c"], 0)
  expect_equal(out$status["a", "c"], "observed")
})

test_that("an undeclared zero_meaning is refused only when a zero exists", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))

  with_zero <- netis_data(
    nodes,
    data.frame(group = 1, source = "a", target = "b", friendship = 0)
  )
  expect_error(
    .build_selection_y(with_zero, "friendship", group = 1, wave = NULL),
    "zero_meaning is undeclared"
  )

  # No stored zero, so no declaration is needed and none is demanded.
  without_zero <- netis_data(
    nodes,
    data.frame(group = 1, source = "a", target = "b", friendship = 1)
  )
  expect_silent(.build_selection_y(without_zero, "friendship", group = 1, wave = NULL))
})

test_that("waves are kept separate", {
  nodes <- expand.grid(node = c("a", "b", "c"), wave = 1:2, stringsAsFactors = FALSE)
  nodes$group <- 1
  ties <- data.frame(
    group = 1, source = "a", target = "b", wave = 1:2,
    friendship = c(1, 5)
  )
  nd <- netis_data(nodes[c("group", "node", "wave")], ties, wave_col = "wave")

  expect_equal(.build_selection_y(nd, "friendship", 1, wave = 1)$Y["a", "b"], 1)
  expect_equal(.build_selection_y(nd, "friendship", 1, wave = 2)$Y["a", "b"], 5)
})

test_that("self-ties are observable when the policy permits them", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "a", friendship = 1)
  nd <- netis_data(nodes, ties, self_ties = "permitted")

  out <- .build_selection_y(nd, "friendship", group = 1, wave = NULL)
  expect_equal(out$status["a", "a"], "observed")
  expect_equal(out$Y["a", "a"], 1)
  expect_equal(unname(out$counts["structural"]), 0L)
})

test_that("an undirected network mirrors its single stored row", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = "a", target = "b", friendship = 1)
  nd <- netis_data(nodes, ties, directed = FALSE)

  out <- .build_selection_y(nd, "friendship", group = 1, wave = NULL)
  expect_equal(out$Y["b", "a"], 1)
  expect_equal(out$status["b", "a"], "observed")
})

test_that("the NA encoding and the status matrix cannot drift apart", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = "a", target = "b", friendship = 1)
  out <- .build_selection_y(netis_data(nodes, ties), "friendship", 1, NULL)
  expect_equal(is.na(out$Y), out$status != "observed")
})

test_that("a valued outcome under a binary family demands an explicit rule", {
  weighted <- list(weighted = TRUE)
  expect_error(
    .assert_binary_outcome_rule(weighted, "friendship", rule = NULL, family = "bin"),
    "explicit rule for what counts as a tie"
  )
  # Both honest resolutions are named rather than one being chosen.
  err <- tryCatch(
    .assert_binary_outcome_rule(weighted, "friendship", NULL, "bin"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "binarize\\(friendship")
  expect_match(err, "nrm")

  # A rule supplied, a binary variable, or a valued family all pass.
  expect_true(.assert_binary_outcome_rule(weighted, "f", nonzero(), "bin"))
  expect_true(.assert_binary_outcome_rule(list(weighted = FALSE), "f", NULL, "bin"))
  expect_true(.assert_binary_outcome_rule(weighted, "f", NULL, "nrm"))
})

# Lag-completeness. The invariant under test is that rnetis never lets an
# unresolvable lag reach the backend, because amen substitutes zero for NA
# silently and zero means "no prior tie" -- a substantive claim, not a neutral
# filler.

# Built from parts rather than round-tripped through netis_nodes(): node data
# carrying no attributes does not reconstruct a wave column, so rebuilding a
# modified object needs the original frames.
.SEL_IDS <- c("a", "b", "c")

.sel_nodes <- function() {
  n <- expand.grid(node = .SEL_IDS, wave = 1:2, stringsAsFactors = FALSE)
  n$group <- 1
  n[c("group", "node", "wave")]
}

.sel_pairs <- function() {
  p <- expand.grid(source = .SEL_IDS, target = .SEL_IDS, stringsAsFactors = FALSE)
  p[p$source != p$target, ]
}

# complete_dyadic so that an absent row means "not observed" rather than "no
# tie" -- the only representation in which a lag can be unresolvable.
.sel_data <- function(ties) {
  netis_data(
    .sel_nodes(), ties, wave_col = "wave",
    tie_meta = list(friendship = list(presence_mode = "complete_dyadic",
                                      presence_col = "seen",
                                      zero_meaning = "measured_zero"))
  )
}

# Values must differ between waves. A tie variable constant across waves is
# stored as *static* -- one row per dyad, no wave dimension, reused at every
# wave -- so per-wave observation status becomes unrepresentable and the focal
# and lagged matrices come back identical. That is correct storage behavior and
# is refused for lagged terms by .check_selection_static_outcome(); it just
# makes a constant fixture unable to express the case under test here.
.sel_both_waves <- function() {
  pairs <- .sel_pairs()
  rbind(
    data.frame(group = 1, pairs, wave = 1, friendship = 1, seen = TRUE),
    data.frame(group = 1, pairs, wave = 2, friendship = 2, seen = TRUE)
  )
}

test_that("a dyad whose prior wave is unobserved is masked, not zero-filled", {
  # Drop a->b from wave 1 only, so its wave-2 outcome is observed but its lag
  # is not -- exactly the unresolvable case.
  ties <- .sel_both_waves()
  ties <- ties[!(ties$wave == 1 & ties$source == "a" & ties$target == "b"), ]
  nd <- .sel_data(ties)

  focal <- .build_selection_y(nd, "friendship", 1, wave = 2)
  lagged <- .build_selection_y(nd, "friendship", 1, wave = 1)
  res <- .resolve_lagged_tie_predictor(focal, lagged)

  expect_true(res$excluded["a", "b"])
  expect_true(is.na(res$Y["a", "b"]))        # outcome masked
  expect_equal(res$x["a", "b"], 0)           # predictor zero-filled, recorded
  expect_equal(res$n_excluded, 1L)
  # A dyad with both waves observed is untouched.
  expect_false(res$excluded["b", "a"])
  expect_false(is.na(res$Y["b", "a"]))
})

test_that("the design array reaching a backend never contains NA", {
  nd <- .sel_data(.sel_both_waves())

  focal <- .build_selection_y(nd, "friendship", 1, wave = 2)
  lagged <- .build_selection_y(nd, "friendship", 1, wave = 1)
  res <- .resolve_lagged_tie_predictor(focal, lagged)

  # Structural cells (the diagonal) are non-observed and so are zero-filled
  # too: the backend needs a complete array regardless of outcome masking.
  expect_false(anyNA(res$x))
  expect_true(.assert_design_complete(res$x, "prior_tie()"))
})

test_that("the completeness assertion fires rather than trusting the backend", {
  x <- matrix(c(1, NA, 0, 1), 2, 2)
  expect_error(.assert_design_complete(x, "prior_tie()"), "no prior tie existed")
  expect_error(.assert_design_complete(x, "prior_tie()"), "1 missing value")
})

test_that("delayed reciprocity transposes the lagged relation", {
  # Make wave 1 asymmetric so a transpose is detectable: a->b is 1, b->a is 0.
  ties <- .sel_both_waves()
  ties$friendship[ties$wave == 1 & ties$source == "b" & ties$target == "a"] <- 0
  nd <- .sel_data(ties)

  focal <- .build_selection_y(nd, "friendship", 1, wave = 2)
  lagged <- .build_selection_y(nd, "friendship", 1, wave = 1)

  persistence <- .resolve_lagged_tie_predictor(focal, lagged, reciprocal = FALSE)
  reciprocity <- .resolve_lagged_tie_predictor(focal, lagged, reciprocal = TRUE)

  expect_equal(persistence$x["a", "b"], 1)   # y_ab at t-1
  expect_equal(reciprocity$x["a", "b"], 0)   # y_ba at t-1
  expect_false(identical(persistence$x, reciprocity$x))
})

test_that("exclusions are reported, never silent", {
  none <- .diagnose_selection_lag_exclusions(0L, 12L, "prior_tie()")
  expect_length(none, 0)

  some <- .diagnose_selection_lag_exclusions(3L, 9L, "prior_tie()")
  expect_length(some, 1)
  expect_equal(some[[1]]$level, "WARNING")
  expect_match(some[[1]]$message, "3 dyad")
  expect_match(some[[1]]$message, "need not be a random subset")
})

test_that("a mismatched actor set is refused rather than silently misaligned", {
  nodes_a <- data.frame(group = 1, node = c("a", "b", "c"))
  nodes_b <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", friendship = 1)
  big <- .build_selection_y(netis_data(nodes_a, ties), "friendship", 1, NULL)
  small <- .build_selection_y(netis_data(nodes_b, ties), "friendship", 1, NULL)
  expect_error(.resolve_lagged_tie_predictor(big, small), "do not share an actor set")
})
