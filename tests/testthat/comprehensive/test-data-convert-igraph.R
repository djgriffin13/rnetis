# Known-answer and integration tests for igraph interoperability: as_igraph(),
# as_igraph_list(), netis_data_from_igraph(). Skipped entirely if igraph is not
# installed (Suggests-guarded).

skip_if_not_installed("igraph")

# ---- basic conversion, isolates, weight aliasing ----------------------------

test_that("as_igraph() preserves vertex names, node/tie attributes, and isolates", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c", "d"), trait = c(1, 2, 3, 4))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), weight = c(2, 1))
  nd <- netis_data(nodes, ties)

  g <- as_igraph(nd)
  expect_s3_class(g, "igraph")
  expect_setequal(igraph::V(g)$name, c("a", "b", "c", "d"))
  expect_true("d" %in% igraph::V(g)$name) # isolate survives as a vertex
  expect_equal(igraph::vcount(g), 4L)
  expect_equal(igraph::ecount(g), 2L)
  expect_equal(sort(igraph::E(g)$weight), c(1, 2))
  expect_true(igraph::is_directed(g))
})

test_that("as_igraph() attaches netis_meta with the resolved selection and descriptor", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", freq = 3)
  nd <- netis_data(nodes, ties, tie_meta = list(freq = list(weight_semantics = "strength")))
  g <- as_igraph(nd)
  meta <- igraph::graph_attr(g, "netis_meta")
  expect_equal(meta$group, "1")
  expect_null(meta$wave)
  expect_equal(meta$network, "freq")
  expect_equal(meta$weight_semantics, "strength")
})

test_that("the selected tie variable is aliased to the 'weight' edge attribute unless already named that", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", communication = 5)
  nd <- netis_data(nodes, ties)
  g <- as_igraph(nd)
  expect_setequal(igraph::edge_attr_names(g), c("communication", "weight"))
  expect_equal(igraph::E(g)$weight, 5)
  expect_equal(igraph::E(g)$communication, 5)

  nodes2 <- data.frame(group = 1, node = c("a", "b"))
  ties2 <- data.frame(group = 1, source = "a", target = "b", weight = 5)
  nd2 <- netis_data(nodes2, ties2)
  g2 <- as_igraph(nd2)
  expect_equal(igraph::edge_attr_names(g2), "weight") # no duplicate alias
})

test_that("undirected data converts without double-counting the single stored row", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  # Two nodes and one row is every possible undirected dyad, so the presence
  # mode would be inferred complete_dyadic (undetermined); declare it.
  nd <- netis_data(nodes, ties, directed = FALSE,
                   tie_meta = list(weight = list(presence_mode = "row_presence")))
  g <- as_igraph(nd)
  expect_false(igraph::is_directed(g))
  expect_equal(igraph::ecount(g), 1L)
})

# ---- selection: inference and ambiguity -------------------------------------

test_that("as_igraph() infers network/group/wave when uniquely determined", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  nd <- netis_data(nodes, ties)
  expect_s3_class(as_igraph(nd), "igraph") # no ambiguity anywhere -> infers silently
})

test_that("as_igraph() errors naming candidates when the network is ambiguous", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", communication = 1, advice = 0)
  nd <- netis_data(nodes, ties)
  expect_error(as_igraph(nd), "communication.*advice|advice.*communication")
})

test_that("as_igraph() errors naming candidates when the group is ambiguous", {
  nodes <- data.frame(group = c(1, 2), node = c("a", "c"))
  ties <- data.frame(group = c(1, 2), source = c("a", "c"), target = c("a", "c"), weight = c(1, 1))
  nd <- netis_data(nodes, ties, self_ties = "permitted")
  expect_error(as_igraph(nd), "2 groups")
})

test_that("as_igraph() errors naming candidates when the wave is ambiguous", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = rep(1:2, each = 2))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2), weight = c(1, 2))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(as_igraph(nd), "2 waves")
  expect_s3_class(as_igraph(nd, wave = 1), "igraph")
})

test_that("an unknown network/group/wave name errors listing the real candidates", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  nd <- netis_data(nodes, ties)
  expect_error(as_igraph(nd, network = "bogus"), "bogus")
  expect_error(as_igraph(nd, group = "bogus"), "bogus")
})

# ---- as_igraph_list() -------------------------------------------------------

test_that("as_igraph_list() names entries by group when there is no wave dimension", {
  nodes <- data.frame(group = c(1, 1, 2, 2), node = c("a", "b", "c", "d"))
  ties <- data.frame(group = c(1, 2), source = c("a", "c"), target = c("b", "d"), weight = c(1, 1))
  nd <- netis_data(nodes, ties)
  gl <- as_igraph_list(nd)
  expect_setequal(names(gl), c("1", "2"))
  expect_true(all(vapply(gl, inherits, logical(1), what = "igraph")))
})

test_that("as_igraph_list() names entries '<group>_w<wave>' when there is a wave dimension", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = rep(1:2, each = 2))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2), weight = c(1, 2))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  gl <- as_igraph_list(nd)
  expect_setequal(names(gl), c("1_w1", "1_w2"))
})

# ---- round trip: the direction-fidelity test --------------------------------

test_that("a deliberately asymmetric directed network round-trips exactly through igraph", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "a"), weight = c(10, 1))
  nd <- netis_data(nodes, ties, self_ties = "prohibited")

  g <- as_igraph(nd)
  nd2 <- netis_data_from_igraph(g)

  t1 <- netis_ties(nd)[order(netis_ties(nd)$source, netis_ties(nd)$target), ]
  t2 <- netis_ties(nd2)[order(netis_ties(nd2)$source, netis_ties(nd2)$target), ]
  expect_equal(t1$source, t2$source)
  expect_equal(t1$target, t2$target)
  expect_equal(t1$weight, t2$weight)
  # The critical assertion: a -> b (weight 10) is not confused with b -> a (weight 1).
  expect_equal(t2$weight[t2$source == "a" & t2$target == "b"], 10)
  expect_equal(t2$weight[t2$source == "b" & t2$target == "a"], 1)
})

test_that("round-trip preserves node data and does not reconstruct the weight alias as an extra tie variable", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), trait = c(0.2, -0.5, 1.1))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), communication = c(3, 0))
  # The stored zero is a measured zero-strength tie, so both rows are edges.
  nd <- netis_data(nodes, ties,
                   tie_meta = list(communication = list(zero_meaning = "measured_zero")))

  g <- as_igraph(nd)
  nd2 <- netis_data_from_igraph(g)

  n1 <- netis_nodes(nd)[order(netis_nodes(nd)$node), ]
  n2 <- netis_nodes(nd2)[order(netis_nodes(nd2)$node), ]
  expect_equal(n1$trait, n2$trait)

  tie_vars <- setdiff(names(netis_ties(nd2)), c("group", "source", "target"))
  expect_equal(tie_vars, "communication")
})

test_that("netis_data_from_igraph() infers directedness and self-tie policy for a foreign graph", {
  el <- data.frame(from = c("a", "b"), to = c("b", "c"))
  g <- igraph::graph_from_data_frame(el, directed = TRUE)
  nd <- netis_data_from_igraph(g)
  expect_true(netis_schema(nd)$directed)
  expect_equal(netis_schema(nd)$self_ties, "prohibited")
  expect_equal(netis_schema(nd)$group_col, "group")
  expect_equal(unique(netis_nodes(nd)$group), "1")
})

test_that("netis_data_from_igraph() infers self_ties = 'permitted' from a self-loop", {
  el <- data.frame(from = c("a", "a"), to = c("a", "b"))
  g <- igraph::graph_from_data_frame(el, directed = TRUE)
  nd <- netis_data_from_igraph(g, self_ties = "permitted")
  expect_equal(netis_schema(nd)$self_ties, "permitted")
})

test_that("netis_data_from_igraph() refuses a multigraph rather than silently collapsing it", {
  el <- data.frame(from = c("a", "a"), to = c("b", "b"))
  g <- igraph::graph_from_data_frame(el, directed = TRUE)
  expect_error(netis_data_from_igraph(g), "parallel edges|multigraph")
})

test_that("netis_data_from_igraph() errors on an unnamed-vertex graph", {
  g <- igraph::make_ring(3)
  expect_error(netis_data_from_igraph(g), "name")
})

test_that("netis_data_from_igraph() requires an igraph object", {
  expect_error(netis_data_from_igraph(list()), "igraph")
})

# ---- regression: declared tie-variable schema metadata was lost on round-trip --
# (guards against the case where as_igraph()'s netis_meta attribute records
# weight_semantics/direction_semantics/zero_meaning/presence_mode, but
# netis_data_from_igraph() never read them back into tie_meta, so a
# round-tripped object silently re-inferred everything from scratch.)

test_that("declared weight_semantics/direction_semantics/zero_meaning survive an as_igraph() round trip", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), freq = c(2, 1))
  nd <- netis_data(nodes, ties, tie_meta = list(freq = list(
    weight_semantics = "strength", direction_semantics = "sends info to", zero_meaning = "measured_zero"
  )))

  g <- as_igraph(nd)
  nd2 <- netis_data_from_igraph(g)
  d2 <- netis_schema(nd2)$tie_descriptors$freq

  expect_equal(d2$weight_semantics, "strength")
  expect_equal(d2$direction_semantics, "sends info to")
  expect_equal(d2$zero_meaning, "measured_zero")
})

test_that("presence_mode survives the round-trip, including 'indicator'", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1) # sparse -> row_presence
  nd <- netis_data(nodes, ties)
  g <- as_igraph(nd)
  nd2 <- netis_data_from_igraph(g)
  expect_equal(netis_schema(nd2)$tie_descriptors$w$presence_mode, "row_presence")

  nodes3 <- data.frame(group = 1, node = c("a", "b"))
  ties3 <- data.frame(group = 1, source = "a", target = "b", w = 1, present = TRUE)
  nd3 <- netis_data(nodes3, ties3, tie_meta = list(w = list(presence_mode = "indicator", presence_col = "present")))
  g3 <- as_igraph(nd3, network = "w")
  nd4 <- netis_data_from_igraph(g3)

  # CHANGED WHEN SCHEMA PROVENANCE LANDED. This previously asserted that
  # "indicator" fell back to fresh inference, alongside "value_rule". That was
  # a real limitation and it has been lifted rather than worked around: a
  # presence_col is a COLUMN NAME, which serializes, and the column itself
  # travels as an edge attribute -- so the declaration is recoverable and is
  # now restored. Only "value_rule" remains unrecoverable, because a rule is an
  # arbitrary closure, and exporting one raises the `presence_metadata` loss
  # rather than dropping it silently (see test-interop-igraph.R).
  d <- netis_schema(nd4)$tie_descriptors$w
  expect_equal(d$presence_mode, "indicator")
  expect_equal(d$presence_col, "present")
  expect_false(d$presence_mode_inferred)
})

test_that("a foreign (non-as_igraph()) graph gets no tie_meta restoration attempt", {
  el <- data.frame(from = c("a", "b"), to = c("b", "c"), weight = c(1, 2))
  g <- igraph::graph_from_data_frame(el, directed = TRUE)
  expect_no_error(netis_data_from_igraph(g))
})
