# Known-answer and integration tests for netis_data().
#
# the data model scope only: structural validity (required columns, unique keys) and the
# static/dynamic storage split with exact round-trip reconstruction. Referential
# integrity, self-tie enforcement, and other design-level checks belong to
# diagnose_data() and are deliberately not tested here.

# ---- static-only construction, no wave dimension ----------------------------

test_that("static construction round-trips node and tie data exactly", {
  nodes <- data.frame(
    group = c(1, 1, 1),
    node  = c("a", "b", "c"),
    trait = c(0.2, -0.5, 1.1)
  )
  ties <- data.frame(
    group  = c(1, 1),
    source = c("a", "b"),
    target = c("b", "c"),
    weight = c(1, 0.5)
  )

  nd <- netis_data(nodes, ties)

  expect_s3_class(nd, "netis_data")

  out_nodes <- netis_nodes(nd)
  expect_setequal(names(out_nodes), c("group", "node", "trait"))
  expect_equal(out_nodes$trait[out_nodes$node == "b"], -0.5)
  expect_equal(nrow(out_nodes), 3L)

  out_ties <- netis_ties(nd)
  expect_equal(nrow(out_ties), 2L)
  expect_equal(out_ties$weight[out_ties$source == "a" & out_ties$target == "b"], 1)

  # Direction is preserved exactly as supplied -- source/target are never swapped.
  expect_true(all(out_ties$source %in% c("a", "b")))
  expect_true(all(out_ties$target %in% c("b", "c")))
})

test_that("no wave_col places everything in the static table with no dynamic table", {
  nodes <- data.frame(group = 1, node = c("a", "b"), trait = c(1, 2))
  ties  <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  nd <- netis_data(nodes, ties)

  expect_null(nd$nodes$dynamic)
  expect_null(nd$ties$dynamic)
  s <- netis_schema(nd)
  expect_null(s$wave_col)
  expect_setequal(s$node_vars$static, "trait")
  expect_length(s$node_vars$dynamic, 0)
})

# ---- static/dynamic split with waves ----------------------------------------

test_that("a genuinely constant variable is classified static even with a wave_col", {
  # 'trait' is identical across both waves for every node; 'mood' varies.
  nodes <- data.frame(
    group = rep(1, 4),
    node  = rep(c("a", "b"), 2),
    wave  = c(1, 1, 2, 2),
    trait = c(0.2, 0.2, 0.2, 0.2),  # constant per node across waves -> static
    mood  = c(3, 5, 4, 5)           # 'a' changes (3->4), 'b' constant (5->5)
  )
  ties <- data.frame(
    group = 1, source = "a", target = "b", wave = c(1, 2), tone = c("neg", "pos")
  )

  nd <- netis_data(nodes, ties, wave_col = "wave")
  s <- netis_schema(nd)

  # trait is static for every node even though wave_col was supplied at all.
  expect_true("trait" %in% s$node_vars$static)
  # mood varies for at least one node (a: 3 vs 4), so it must be classified dynamic
  # as a whole column -- static/dynamic status is per-variable, not per-node.
  expect_true("mood" %in% s$node_vars$dynamic)
  expect_false("mood" %in% s$node_vars$static)

  # Internal storage: trait held once per node (2 rows), not duplicated per wave.
  expect_equal(nrow(nd$nodes$static), 2L)
  expect_equal(nrow(nd$nodes$dynamic), 4L)

  # tone genuinely varies by wave for the one tie -> dynamic; tie has no static-only var.
  expect_true("tone" %in% s$tie_vars$dynamic)

  expect_equal(s$waves, c(1, 2))
})

test_that("static/dynamic split reconstructs the original logical table exactly", {
  nodes <- data.frame(
    group = rep(1, 4),
    node  = rep(c("a", "b"), 2),
    wave  = c(1, 1, 2, 2),
    trait = c(0.2, 0.2, 0.2, 0.2),
    mood  = c(3, 5, 4, 5)
  )
  ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2), tone = c("neg", "pos"))

  nd <- netis_data(nodes, ties, wave_col = "wave")
  out <- netis_nodes(nd)

  # Row-order-independent comparison: sort both by (node, wave) before comparing.
  expected <- nodes[order(nodes$node, nodes$wave), ]
  actual <- out[order(out$node, out$wave), c(names(expected))]
  rownames(expected) <- NULL
  rownames(actual) <- NULL
  expect_equal(actual, expected)
})

test_that("a fully dynamic variable with no static columns still reconstructs correctly", {
  nodes <- data.frame(group = 1, node = c("a", "b"), wave = c(1, 1))
  nodes <- rbind(nodes, data.frame(group = 1, node = c("a", "b"), wave = c(2, 2)))
  nodes$mood <- c(1, 2, 3, 4)  # every value distinct within its node -> fully dynamic

  ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2))

  nd <- netis_data(nodes, ties, wave_col = "wave")
  out <- netis_nodes(nd)
  expect_equal(sum(out$mood), sum(nodes$mood))
  expect_equal(nrow(out), 4L)
})

# ---- group data -------------------------------------------------------------

test_that("group_data is optional and netis_groups() is NULL when omitted", {
  nodes <- data.frame(group = 1, node = "a")
  ties  <- data.frame(group = 1, source = "a", target = "a")
  nd <- netis_data(nodes, ties, self_ties = "permitted")
  expect_null(netis_groups(nd))
})

test_that("group_data round-trips like node and tie data", {
  nodes <- data.frame(group = c(1, 2), node = c("a", "b"))
  ties  <- data.frame(group = c(1, 2), source = c("a", "b"), target = c("a", "b"))
  groups <- data.frame(group = c(1, 2), size = c(10, 20))

  nd <- netis_data(nodes, ties, group_data = groups, self_ties = "permitted")
  out <- netis_groups(nd)
  expect_equal(out$size[out$group == 2], 20)
})

# ---- structural validation --------------------------------------------------

test_that("missing required columns error with an informative message", {
  nodes <- data.frame(group = 1, node = "a")
  bad_ties <- data.frame(group = 1, from = "a", to = "a")  # wrong column names
  expect_error(netis_data(nodes, bad_ties), "tie_data is missing required column")
})

test_that("duplicate entity keys error rather than silently collapsing", {
  nodes <- data.frame(group = c(1, 1), node = c("a", "a"), trait = c(1, 2))
  ties <- data.frame(group = 1, source = "a", target = "a")
  expect_error(netis_data(nodes, ties, self_ties = "permitted"), "duplicate")
})

test_that("duplicate tie keys are only an error within the same wave", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(
    group = 1, source = "a", target = "b", wave = c(1, 2), weight = c(1, 2)
  )
  expect_silent(netis_data(nodes, ties, wave_col = "wave"))
})

test_that("wave_col need not be present in every input -- static levels stay static", {
  # ties have two waves; node data was only ever measured once and carries no
  # wave column at all. This must not be forced to add a redundant wave column:
  # static data is first-class, and a level with no repeated measures simply
  # has no wave dimension of its own.
  nodes <- data.frame(group = 1, node = c("a", "b"), trait = c(1, 2))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2), weight = c(1, 2))

  nd <- netis_data(nodes, ties, wave_col = "wave")

  expect_null(nd$nodes$dynamic)
  expect_equal(nrow(netis_nodes(nd)), 2L)
  expect_equal(nrow(netis_ties(nd)), 2L)
  expect_equal(netis_schema(nd)$waves, c(1, 2))
})

test_that("an invalid self_ties value is rejected", {
  nodes <- data.frame(group = 1, node = "a")
  ties <- data.frame(group = 1, source = "a", target = "a")
  expect_error(netis_data(nodes, ties, self_ties = "sometimes"), "self_ties")
})

test_that("non-data-frame input is rejected", {
  expect_error(netis_data(list(), data.frame(group = 1, source = 1, target = 1)),
               "node_data must be a data frame")
})

test_that("non-data-frame group_data is rejected", {
  nodes <- data.frame(group = 1, node = "a")
  ties <- data.frame(group = 1, source = "a", target = "a")
  expect_error(netis_data(nodes, ties, group_data = list(), self_ties = "permitted"), "group_data")
})

test_that("zero-row node_data (empty data) constructs without error", {
  nodes <- data.frame(group = numeric(0), node = character(0), trait = numeric(0))
  ties <- data.frame(group = numeric(0), source = character(0), target = character(0))
  nd <- netis_data(nodes, ties)
  expect_equal(nrow(netis_nodes(nd)), 0L)
})

# ---- .check_netis_data() guard ----------------------------------------------

test_that("every accessor rejects a non-netis_data object with the documented message", {
  expect_error(netis_nodes(list()), "x must be a netis_data object")
  expect_error(netis_ties(list()), "x must be a netis_data object")
  expect_error(netis_groups(list()), "x must be a netis_data object")
  expect_error(netis_schema(list()), "x must be a netis_data object")
})

# ---- print/format -----------------------------------------------------------

test_that("format.netis_data produces readable, non-erroring output", {
  nodes <- data.frame(group = 1, node = c("a", "b"), trait = c(1, 2))
  ties  <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  nd <- netis_data(nodes, ties)

  out <- format(nd)
  expect_type(out, "character")
  expect_true(any(grepl("netis_data", out, fixed = TRUE)))
  expect_true(any(grepl("2 nodes", out, fixed = TRUE)))
  expect_output(print(nd), "netis_data")
})

# ---- the ragged roster, at storage level --------------

test_that("a ragged roster survives storage even with no variables to carry it", {
  # `.split_static_dynamic()` classifies COLUMNS as static or dynamic. A table
  # whose only columns are its keys has none to classify, so the guard meant to
  # protect a ragged entity pattern had nothing to act on and the wave dimension
  # was dropped -- collapsing seven (node, wave) rows to three nodes and
  # silently asserting that every node was present at every wave.
  nodes <- data.frame(
    group = "A", node = c("a", "a", "a", "b", "b", "b", "c"),
    wave = c(1, 2, 3, 1, 2, 3, 3), stringsAsFactors = FALSE
  )
  ties <- data.frame(group = "A", source = "a", target = "b", wave = 1:3, w = 1,
                     stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties, wave_col = "wave")

  expect_identical(nrow(netis_nodes(nd)), 7L)
  n <- netis_nodes(nd)
  expect_identical(sort(n$node[n$wave == "1"]), c("a", "b"))
  expect_identical(sort(n$node[n$wave == "3"]), c("a", "b", "c"))
})

test_that("a COMPLETE roster still collapses, because the schema can rebuild it", {
  # The other half of the rule, pinned so the fix is a decision rather than an
  # accident: a complete entity-by-wave grid is reconstructible from
  # schema$waves, so collapsing it loses nothing and the storage optimisation is
  # kept. Only the irreversible case is preserved.
  nodes <- data.frame(
    group = "A", node = rep(c("a", "b"), each = 3),
    wave = rep(1:3, times = 2), stringsAsFactors = FALSE
  )
  ties <- data.frame(group = "A", source = "a", target = "b", wave = 1:3, w = 1,
                     stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_null(nd$nodes$dynamic)
  expect_identical(nrow(nd$nodes$static), 2L)
})

test_that("a ragged TIE table keeps its per-wave pattern too", {
  # A distinct defect of the same family as the roster case, one level down.
  #
  # Grid completeness is judged against the dataset's waves (`schema$waves`),
  # not against the waves present in that one table. A tie observed at waves 1
  # and 3 of a three-wave study has one dyad over its own two waves, which
  # looks complete; judged per table it would collapse to a static tie and
  # assert the tie also existed at wave 2, where it was not recorded. For a
  # sparse edge list that row pattern IS the network's time variation.
  nodes <- data.frame(
    group = "A", node = rep(c("a", "b"), each = 3),
    wave = rep(1:3, times = 2), stringsAsFactors = FALSE
  )
  ties <- data.frame(group = "A", source = "a", target = "b", wave = c(1, 3), w = 1,
                     stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  t <- netis_ties(nd)
  expect_identical(sort(as.character(t$wave)), c("1", "3"))
  expect_identical(nrow(t), 2L)
  # and the export at wave 2 holds no ties, not the wave-1 tie
  expect_warning(m2 <- netis_adjacency(nd, "w", wave = 2), "group A wave 2")
  expect_identical(sum(m2), 0)
  expect_identical(netis_adjacency(nd, "w", wave = 3)["a", "b"], 1)
})
