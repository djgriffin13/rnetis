# A guard-group test: rnetis will not guess what a stored zero means.
#
# THE DEFECT THIS PREVENTS. `presence_mode` is inferred from the tie table's
# shape — above a completeness threshold the table reads as `complete_dyadic`,
# below it as `row_presence`. Above the threshold, an undeclared network refuses
# every presence operation. Below it, rnetis used to read *every stored row* as a
# present tie, measured zeros included, and say nothing.
#
# That made a package heuristic decide network semantics. Presence feeds degree,
# density, reference sets, isolate status and exposure, so the guess propagated
# into estimates — and the direction is the dangerous one: a genuinely complete
# dyadic table that falls just below the threshold has its zero rows silently
# promoted to ties.
#
# The refusal is deliberately narrow, and these tests pin both edges of it:
# it fires only where the mode was INFERRED and zeros are actually STORED, so an
# ordinary sparse edge list and an explicit user declaration are both untouched.

.zero_fixture <- function(tie_meta = NULL, values = c(3, 0)) {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"),
                     w = values)
  netis_data(nodes, ties, tie_meta = tie_meta)
}

test_that("the fixture really does infer row_presence, so the test is testing what it claims", {
  d <- netis_schema(.zero_fixture())$tie_descriptors$w
  expect_identical(d$presence_mode, "row_presence")
  expect_identical(d$ledger$presence_mode$state, "inferred")
  expect_true(d$stored_zeros)
})

test_that("an inferred row_presence network with stored zeros refuses presence resolution", {
  nd <- .zero_fixture()
  err <- tryCatch(netis_adjacency(nd, "w", presence = TRUE),
                  error = function(e) conditionMessage(e))
  expect_type(err, "character")
  # The message must name the ambiguity and offer the ways out, not just fail.
  expect_match(err, "will not guess")
  expect_match(err, "zero_meaning", fixed = TRUE)
  expect_match(err, "presence_rule", fixed = TRUE)
})

test_that("zero_meaning = 'measured_zero' resolves it: every row is a present tie", {
  nd <- .zero_fixture(list(w = list(zero_meaning = "measured_zero")))
  m <- netis_adjacency(nd, "w", presence = TRUE, drop = TRUE)
  expect_equal(m["a", "b"], 1)
  expect_equal(m["a", "c"], 1)
})

test_that("a value rule resolves it the other way: the zero row is not a tie", {
  nd <- .zero_fixture(list(w = list(
    presence_mode = "value_rule",
    presence_rule = function(value) value != 0
  )))
  m <- netis_adjacency(nd, "w", presence = TRUE, drop = TRUE)
  expect_equal(m["a", "b"], 1)
  expect_equal(m["a", "c"], 0)
})

test_that("zero_meaning = 'structural_absence' is a contradiction, and is named as one", {
  nd <- .zero_fixture(list(w = list(zero_meaning = "structural_absence")))
  expect_error(netis_adjacency(nd, "w", presence = TRUE), "contradicts")
})

test_that("an explicitly declared row_presence is taken at its word", {
  nd <- .zero_fixture(list(w = list(presence_mode = "row_presence")))
  m <- netis_adjacency(nd, "w", presence = TRUE, drop = TRUE)
  expect_equal(m["a", "c"], 1)
})

test_that("a sparse edge list with no stored zeros is unaffected", {
  nd <- .zero_fixture(values = c(3, 1))
  m <- netis_adjacency(nd, "w", presence = TRUE, drop = TRUE)
  expect_equal(m["a", "b"], 1)
  expect_equal(m["a", "c"], 1)
})

test_that("the refusal does not depend on which subset is being resolved", {
  # Determinacy is a property of the tie VARIABLE, not of whether one group's
  # rows happen to contain a zero. Group A has no zero and group B does; both
  # must refuse, or a per-group export would be internally inconsistent.
  nodes <- data.frame(group = rep(c("A", "B"), each = 3),
                      node = rep(c("a", "b", "c"), 2))
  ties <- data.frame(
    group  = c("A", "A", "B", "B"),
    source = c("a", "a", "a", "a"),
    target = c("b", "c", "b", "c"),
    w      = c(3, 1, 3, 0)          # only group B stores a zero
  )
  nd <- netis_data(nodes, ties)
  expect_identical(netis_schema(nd)$tie_descriptors$w$presence_mode, "row_presence")
  expect_error(netis_adjacency(nd, "w", presence = TRUE), "will not guess")
})
