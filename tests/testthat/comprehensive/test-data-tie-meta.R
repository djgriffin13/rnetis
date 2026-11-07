# Known-answer and integration tests for per-tie-variable schema descriptors:
# weighted/presence_mode inference, explicit tie_meta declarations, edge-
# presence resolution, schema migration, and the diagnose_data() zero_meaning
# check.

# ---- the test that proves the sparse/complete-dyadic distinction is honored --

test_that("a sparse edge list and an equivalent complete dyadic table give identical presence and weighted totals", {
  # Same logical network -- a -> b (weight 2), a -> c (weight 0), b -> c absent --
  # expressed two ways.
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))

  sparse_ties <- data.frame(
    group = 1,
    source = c("a", "a"),
    target = c("b", "c"),
    weight = c(2, 0)
  )
  # The sparse side declares what its stored zero means, exactly as the
  # complete side below declares its indicator column. Both halves of an
  # equivalence claim have to state their presence semantics; leaving the
  # sparse one to inference was asking rnetis to guess whether `weight = 0` was
  # an observed zero-strength tie or an absence, which it now refuses to do.
  nd_sparse <- netis_data(
    nodes, sparse_ties, self_ties = "prohibited",
    tie_meta = list(weight = list(zero_meaning = "measured_zero"))
  )

  complete_ties <- data.frame(
    group = 1,
    source = c("a", "a", "b", "b", "c", "c"),
    target = c("b", "c", "a", "c", "a", "b"),
    weight = c(2, 0, 0, 0, 0, 0),
    present = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  nd_complete <- netis_data(
    nodes, complete_ties, self_ties = "prohibited",
    tie_meta = list(weight = list(presence_mode = "indicator", presence_col = "present"))
  )

  m_sparse <- netis_adjacency(nd_sparse, "weight", presence = TRUE)
  m_complete <- netis_adjacency(nd_complete, "weight", presence = TRUE)
  expect_equal(m_sparse["a", "b"], 1)
  expect_equal(m_sparse["a", "c"], 1) # row exists -> present, even though weight is 0
  expect_equal(m_sparse, m_complete)

  w_sparse <- netis_adjacency(nd_sparse, "weight")
  w_complete <- netis_adjacency(nd_complete, "weight")
  expect_equal(w_sparse, w_complete)
})

test_that("a complete dyadic network with undeclared presence refuses presence operations", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  # Every ordered pair present as a row -> inferred complete_dyadic.
  ties <- expand.grid(source = c("a", "b", "c"), target = c("a", "b", "c"), stringsAsFactors = FALSE)
  ties <- ties[ties$source != ties$target, ]
  ties$group <- 1
  ties$weight <- 0

  nd <- netis_data(nodes, ties, self_ties = "prohibited")
  s <- netis_schema(nd)
  expect_true(s$tie_descriptors$weight$presence_mode_inferred)
  expect_equal(s$tie_descriptors$weight$presence_mode, "complete_dyadic")

  expect_error(
    netis_adjacency(nd, "weight", presence = TRUE),
    "undetermined",
    ignore.case = TRUE
  )
})

# ---- inference correctness --------------------------------------------------

test_that("an all-ones sparse tie variable is inferred binary, not weighted", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), tie = c(1, 1))
  nd <- netis_data(nodes, ties)
  d <- netis_schema(nd)$tie_descriptors$tie
  expect_false(d$weighted)
  expect_true(d$weighted_inferred)
})

test_that("a weighted variable that happens to contain only 0/1 in this sample can be overridden", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), freq = c(0, 1))
  nd_default <- netis_data(nodes, ties)
  expect_false(netis_schema(nd_default)$tie_descriptors$freq$weighted)

  nd_override <- netis_data(nodes, ties, tie_meta = list(freq = list(weighted = TRUE)))
  d <- netis_schema(nd_override)$tie_descriptors$freq
  expect_true(d$weighted)
  expect_false(d$weighted_inferred)
})

test_that("a sparse network (well below the completeness threshold) is inferred row_presence", {
  nodes <- data.frame(group = 1, node = as.character(1:10))
  ties <- data.frame(group = 1, source = c("1", "2"), target = c("2", "3"), tie = c(1, 1))
  nd <- netis_data(nodes, ties)
  d <- netis_schema(nd)$tie_descriptors$tie
  expect_equal(d$presence_mode, "row_presence")
  expect_true(d$presence_mode_inferred)
})

test_that("a near-complete dyadic table (a few missing dyads) still infers complete_dyadic", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c", "d"))
  ties <- expand.grid(source = c("a", "b", "c", "d"), target = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  ties <- ties[ties$source != ties$target, ]
  ties$group <- 1
  ties$weight <- 1
  # Drop one row: 11 of 12 possible ordered pairs remain (ratio ~0.917).
  ties <- ties[-1, ]

  nd <- netis_data(nodes, ties, self_ties = "prohibited")
  d <- netis_schema(nd)$tie_descriptors$weight
  expect_equal(d$presence_mode, "complete_dyadic")
})

# ---- tie_meta validation ----------------------------------------------------

test_that("tie_meta rejects an unknown tie-variable name", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  expect_error(
    netis_data(nodes, ties, tie_meta = list(not_a_column = list(weighted = TRUE))),
    "not_a_column"
  )
})

test_that("presence_mode = 'indicator' requires presence_col", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  expect_error(
    netis_data(nodes, ties, tie_meta = list(weight = list(presence_mode = "indicator"))),
    "presence_col"
  )
})

test_that("presence_mode = 'value_rule' requires presence_rule", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  expect_error(
    netis_data(nodes, ties, tie_meta = list(weight = list(presence_mode = "value_rule"))),
    "presence_rule"
  )
})

test_that("presence_rule must be a function", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  expect_error(
    netis_data(nodes, ties, tie_meta = list(weight = list(presence_mode = "value_rule", presence_rule = "not a function"))),
    "function"
  )
})

test_that("weight_semantics and zero_meaning validate against their enums", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  expect_error(
    netis_data(nodes, ties, tie_meta = list(weight = list(weight_semantics = "bogus"))),
    "weight_semantics"
  )
  expect_error(
    netis_data(nodes, ties, tie_meta = list(weight = list(zero_meaning = "bogus"))),
    "zero_meaning"
  )
})

test_that("a value_rule presence_rule resolves presence correctly", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), weight = c(3, -1))
  nd <- netis_data(
    nodes, ties,
    tie_meta = list(weight = list(presence_mode = "value_rule", presence_rule = function(w) w > 0))
  )
  m <- netis_adjacency(nd, "weight", presence = TRUE)
  expect_equal(m["a", "b"], 1)
  expect_equal(m["a", "c"], 0)
})

# ---- weight_semantics / direction_semantics / zero_meaning are recorded -----

test_that("declared weight_semantics, direction_semantics, and zero_meaning round-trip", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", freq = 3)
  nd <- netis_data(
    nodes, ties,
    tie_meta = list(freq = list(
      weight_semantics = "strength",
      direction_semantics = "source sends information to target",
      zero_meaning = "structural_absence"
    ))
  )
  d <- netis_schema(nd)$tie_descriptors$freq
  expect_equal(d$weight_semantics, "strength")
  expect_equal(d$direction_semantics, "source sends information to target")
  expect_equal(d$zero_meaning, "structural_absence")
})

test_that("undeclared weight_semantics defaults to unitless, not a guess", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", freq = 3)
  nd <- netis_data(nodes, ties)
  expect_equal(netis_schema(nd)$tie_descriptors$freq$weight_semantics, "unitless")
})

# ---- schema version ---------------------------------------------------------

test_that("an unrecognized future schema version errors clearly", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  nd <- netis_data(nodes, ties)
  nd$schema$schema_version <- "99"
  expect_error(netis_schema(nd), "schema_version")
  # No other version is read either: there is only one.
  nd$schema$schema_version <- "0"
  expect_error(netis_schema(nd), "does not understand")
})

# ---- print/format surfaces descriptors --------------------------------------

test_that("print.netis_data() reports tie network semantics", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  nd <- netis_data(nodes, ties)
  out <- format(nd)
  expect_true(any(grepl("tie network semantics", out)))
  expect_true(any(grepl("weight:.*binary.*row_presence", out)))
})

# ---- diagnose_data(): zero_meaning ambiguity --------------------------------

test_that("diagnose_data() notes an undeclared zero_meaning when zeros are present", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), weight = c(1, 0))
  nd <- netis_data(nodes, ties)
  diag <- diagnose_data(nd)
  notes <- filter_diagnostics(diag, level = "NOTE")
  codes <- vapply(notes, function(f) f$code, character(1))
  expect_true("zero_meaning_undeclared" %in% codes)
})

test_that("diagnose_data() does not flag zero_meaning when it was declared", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), weight = c(1, 0))
  nd <- netis_data(nodes, ties, tie_meta = list(weight = list(zero_meaning = "measured_zero")))
  diag <- diagnose_data(nd)
  codes <- vapply(diag, function(f) f$code, character(1))
  expect_false("zero_meaning_undeclared" %in% codes)
})

test_that("diagnose_data() does not flag zero_meaning when no zero values are present", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), weight = c(1, 2))
  nd <- netis_data(nodes, ties)
  diag <- diagnose_data(nd)
  codes <- vapply(diag, function(f) f$code, character(1))
  expect_false("zero_meaning_undeclared" %in% codes)
})

# ---- additional coverage: inference edge cases and malformed tie_meta -------

test_that(".infer_weighted() correctly identifies a genuinely weighted (continuous, negative) column", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = -2.5)
  nd <- netis_data(nodes, ties)
  d <- netis_schema(nd)$tie_descriptors$w
  expect_true(d$weighted)
  expect_true(d$weighted_inferred)
})

test_that(".infer_weighted() defaults an all-NA column to binary (the conservative reading)", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = NA_real_)
  nd <- netis_data(nodes, ties)
  expect_false(netis_schema(nd)$tie_descriptors$w$weighted)
})

test_that(".infer_presence_mode() handles every group having fewer than 2 nodes without error", {
  nodes <- data.frame(group = c(1, 2), node = c("a", "b")) # 1 node per group
  ties <- data.frame(group = 1, source = "a", target = "a", w = 1)[0, ] # zero tie rows
  nd <- netis_data(nodes, ties, self_ties = "permitted")
  d <- netis_schema(nd)$tie_descriptors$w
  expect_equal(d$presence_mode, "row_presence")
})

test_that("tie_meta rejects an unnamed list", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  expect_error(netis_data(nodes, ties, tie_meta = list(list(weighted = TRUE))), "named")
})

test_that("tie_meta rejects a non-list value", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  expect_error(netis_data(nodes, ties, tie_meta = "weight"), "named list")
})

test_that("a complete_dyadic network with an explicit presence_col does NOT error (the non-trigger case)", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- expand.grid(source = c("a", "b", "c"), target = c("a", "b", "c"), stringsAsFactors = FALSE)
  ties <- ties[ties$source != ties$target, ]
  ties$group <- 1
  ties$w <- 0
  ties$present <- FALSE
  ties$present[1] <- TRUE
  nd <- netis_data(
    nodes, ties, self_ties = "prohibited",
    tie_meta = list(w = list(presence_mode = "indicator", presence_col = "present"))
  )
  expect_no_error(netis_adjacency(nd, "w", presence = TRUE))
})
