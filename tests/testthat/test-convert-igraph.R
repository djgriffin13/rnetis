# igraph round-out.
#
# The round-trip fixture is deliberately hostile (DF.9): directed, weighted,
# ASYMMETRIC so a transposition changes the answer, multi-group, multi-wave,
# with an isolate. A convenient fixture cannot tell "handles this" from "was
# never asked".

.ig_fixture <- function() {
  nodes <- data.frame(
    group = c(rep("A", 3), rep("B", 2)),
    node = c("a", "b", "z", "c", "d"),
    trait = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )
  # a -> b weight 2, b -> a weight 5: asymmetric, so a transpose is visible.
  # "z" has no ties at all: an isolate.
  ties <- data.frame(
    group = c("A", "A", "B"),
    source = c("a", "b", "c"),
    target = c("b", "a", "d"),
    w = c(2, 5, 7),
    stringsAsFactors = FALSE
  )
  netis_data(nodes, ties, tie_meta = list(w = list(
    weight_semantics = "strength", direction_semantics = "source advises target"
  )))
}

test_that("a list of graphs round-trips back to one object", {
  skip_if_not_installed("igraph")
  nd <- .ig_fixture()
  back <- netis_data_from_igraph_list(as_igraph_list(nd, network = "w"))

  expect_identical(nrow(netis_nodes(back)), nrow(netis_nodes(nd)))
  expect_identical(sort(unique(netis_nodes(back)$group)), c("A", "B"))
  expect_identical(nrow(netis_ties(back)), nrow(netis_ties(nd)))
})

test_that("the round trip preserves direction, so a transpose would be caught", {
  skip_if_not_installed("igraph")
  nd <- .ig_fixture()
  back <- netis_data_from_igraph_list(as_igraph_list(nd, network = "w"))

  before <- netis_adjacency(nd, "w", drop = FALSE)$A
  after <- netis_adjacency(back, "w", drop = FALSE)$A
  # The fixture is asymmetric: a[b] = 2 but b[a] = 5.
  expect_identical(before["a", "b"], 2)
  expect_identical(before["b", "a"], 5)
  expect_identical(after[rownames(before), colnames(before)], before)
})

test_that("isolates survive the list round trip, asserted by count", {
  skip_if_not_installed("igraph")
  nd <- .ig_fixture()
  back <- netis_data_from_igraph_list(as_igraph_list(nd, network = "w"))
  expect_true("z" %in% netis_nodes(back)$node)
  expect_identical(sum(netis_nodes(back)$group == "A"), 3L)
})

test_that("declared semantics survive the list round trip as declared", {
  skip_if_not_installed("igraph")
  nd <- .ig_fixture()
  back <- netis_data_from_igraph_list(as_igraph_list(nd, network = "w"))
  d <- netis_schema(back)$tie_descriptors$w
  expect_identical(d$weight_semantics, "strength")
  expect_identical(d$direction_semantics, "source advises target")

  # And the ledger says they were declared, not re-inferred on the way back.
  b <- netis_boundary(back)
  expect_identical(b$semantics$state[b$semantics$field == "weight_semantics"], "declared")
  expect_identical(b$semantics$state[b$semantics$field == "direction_semantics"], "declared")
})

test_that("a multi-wave collection round-trips with its waves intact", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(
    group = "A", node = rep(c("a", "b"), each = 2),
    wave = rep(1:2, times = 2), stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = "A", source = "a", target = "b", wave = 1:2, w = c(1, 2),
    stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")
  back <- netis_data_from_igraph_list(as_igraph_list(nd, network = "w"))
  expect_identical(as.character(netis_schema(back)$waves), c("1", "2"))
  expect_identical(nrow(netis_ties(back)), 2L)
})

test_that("group and wave come from netis_meta, not from parsing the list names", {
  skip_if_not_installed("igraph")
  # A group identifier containing "_w" would defeat any name-parsing scheme.
  nodes <- data.frame(group = "cohort_w2", node = c("a", "b"), stringsAsFactors = FALSE)
  ties <- data.frame(group = "cohort_w2", source = "a", target = "b", w = 1, stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties)
  back <- netis_data_from_igraph_list(as_igraph_list(nd, network = "w"))
  expect_identical(unique(netis_nodes(back)$group), "cohort_w2")
})

test_that("foreign graphs need explicit group labels rather than being numbered", {
  skip_if_not_installed("igraph")
  g1 <- igraph::make_graph(~ a - +b)
  g2 <- igraph::make_graph(~ c - +d)
  expect_error(netis_data_from_igraph_list(list(g1, g2)), "cannot tell which group")

  back <- netis_data_from_igraph_list(list(g1, g2), groups = c("A", "B"))
  expect_identical(sort(unique(netis_nodes(back)$group)), c("A", "B"))
  # Directedness came from the graphs' own structural flag, and the ledger says so.
  expect_true(netis_boundary(back)$directedness$source_encoded)
})

test_that("graphs that disagree on schema-level facts are refused", {
  skip_if_not_installed("igraph")
  g_dir <- igraph::make_graph(~ a - +b)
  g_und <- igraph::make_graph(~ c - d)
  expect_error(
    netis_data_from_igraph_list(list(g_dir, g_und), groups = c("A", "B")),
    "disagree on directed"
  )
})

test_that("a mixed list of rnetis-authored and foreign graphs is refused", {
  skip_if_not_installed("igraph")
  nd <- .ig_fixture()
  own <- as_igraph(nd, network = "w", group = "A")
  foreign <- igraph::make_graph(~ x - +y)
  expect_error(
    netis_data_from_igraph_list(list(own, foreign)),
    "carry a netis_meta record and some do not"
  )
})

test_that("parallel edges raise the collapse loss rather than being merged", {
  skip_if_not_installed("igraph")
  g <- igraph::graph_from_data_frame(
    data.frame(from = c("a", "a"), to = c("b", "b")),
    directed = TRUE
  )
  e <- tryCatch(
    netis_data_from_igraph_list(list(g), groups = "A"),
    netis_semantic_loss = function(e) e
  )
  expect_s3_class(e, "netis_semantic_loss")
  expect_identical(e$loss_id, "tie_value_collapse")
})

# ---- the presence-declaration gap, narrowed --------------------------------

test_that("an indicator-declared network now round-trips its declaration", {
  skip_if_not_installed("igraph")
  # Before the schema-provenance work, `indicator` fell back to fresh inference along with
  # `value_rule`. But a presence_col is a COLUMN NAME -- a string, which
  # serializes -- and the column travels as an edge attribute, so this one is
  # recoverable and no longer lumped in with the closure case.
  nodes <- data.frame(group = 1, node = c("a", "b"), stringsAsFactors = FALSE)
  ties <- data.frame(
    group = 1, source = c("a", "b"), target = c("b", "a"),
    w = c(1, 0), present = c(TRUE, FALSE), stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(
    presence_mode = "indicator", presence_col = "present"
  )))
  back <- netis_data_from_igraph(as_igraph(nd, network = "w"))
  d <- netis_schema(back)$tie_descriptors$w
  expect_identical(d$presence_mode, "indicator")
  expect_identical(d$presence_col, "present")
  # Declared, not re-inferred.
  expect_false(d$presence_mode_inferred)
})

test_that("a presence_rule closure announces its loss rather than vanishing", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(group = 1, node = c("a", "b"), stringsAsFactors = FALSE)
  ties <- data.frame(group = 1, source = "a", target = "b", w = 2, stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(
    presence_mode = "value_rule", presence_rule = function(v) v > 1
  )))

  w <- tryCatch(as_igraph(nd, network = "w"), netis_representational_loss = function(e) e)
  expect_s3_class(w, "netis_representational_loss")
  expect_identical(w$loss_id, "presence_metadata")

  # It is representational, not semantic, so it can be authorized by name.
  expect_silent(as_igraph(nd, network = "w", allow_loss = "presence_metadata"))
})

test_that("authorizing a different loss does not silence the presence one", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(group = 1, node = c("a", "b"), stringsAsFactors = FALSE)
  ties <- data.frame(group = 1, source = "a", target = "b", w = 2, stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(
    presence_mode = "value_rule", presence_rule = function(v) v > 1
  )))
  expect_warning(
    as_igraph(nd, network = "w", allow_loss = "foreign_attributes"),
    class = "netis_representational_loss"
  )
})

test_that("an unknown allow_loss value is refused at the converter boundary", {
  skip_if_not_installed("igraph")
  nd <- .ig_fixture()
  expect_error(as_igraph(nd, network = "w", group = "A", allow_loss = "nope"), "unknown loss identifier")
})

test_that("a derived (add_centering()/add_decomposition()) node column announces its loss on export", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), age = c(22, 41, 35))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(1, 1))
  nd <- add_centering(netis_data(nodes, ties), "age", method = "grand")

  w <- tryCatch(as_igraph(nd), netis_representational_loss = function(e) e)
  expect_s3_class(w, "netis_representational_loss")
  expect_identical(w$loss_id, "derived_provenance")
  expect_match(conditionMessage(w), "age_grand", fixed = TRUE)

  expect_silent(as_igraph(nd, allow_loss = "derived_provenance"))
  # The values themselves are exported correctly regardless -- only the
  # provenance record is what does not cross, and that is the whole point
  # of classifying this loss representational rather than semantic.
  g <- as_igraph(nd, allow_loss = "derived_provenance")
  expect_equal(igraph::vertex_attr(g, "age_grand"), c(22, 41, 35) - mean(c(22, 41, 35)))
})

test_that("a netis_data object with no derived columns exports silently", {
  skip_if_not_installed("igraph")
  nd <- .ig_fixture()
  expect_silent(as_igraph(nd, network = "w", group = "A"))
})
