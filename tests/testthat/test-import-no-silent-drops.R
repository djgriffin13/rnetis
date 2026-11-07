# Import paths keep what they are given, or refuse by name. Nothing is
# dropped without a word.

test_that("an edgelist matrix keeps its extra columns as tie variables", {
  el <- matrix(c("a", "b", "2.5",
                 "b", "c", "4"), ncol = 3, byrow = TRUE,
               dimnames = list(NULL, c("from", "to", "w")))
  nd <- netis_data_from_edgelist(el, nodes = c("a", "b", "c"))
  t1 <- netis_ties(nd)
  expect_true("w" %in% names(t1))
  expect_equal(t1$w[t1$source == "a" & t1$target == "b"], 2.5)
  expect_equal(t1$w[t1$source == "b" & t1$target == "c"], 4)
  # Unnamed numeric matrix: the third column becomes V3.
  el2 <- matrix(c(1, 2, 7, 2, 3, 9), ncol = 3, byrow = TRUE)
  t2 <- netis_ties(netis_data_from_edgelist(el2, nodes = 1:3))
  expect_equal(t2$V3[t2$source == "2" & t2$target == "3"], 9)
})

test_that("grouped edges with an ungrouped roster are refused", {
  edges <- data.frame(group = c("A", "B"), source = c("a", "c"), target = c("b", "d"))
  expect_error(netis_data_from_edgelist(edges, nodes = c("a", "b", "c", "d")),
               "roster does not")
  nodes <- data.frame(group = c("A", "A", "B", "B"), node = c("a", "b", "c", "d"))
  expect_s3_class(netis_data_from_edgelist(edges, nodes = nodes), "netis_data")
})

test_that("an entity only in a static table is refused, not dropped", {
  ns <- data.frame(group = 1, node = c("a", "b", "c"), sex = c(1, 0, 1))
  nd <- data.frame(group = 1, node = c("a", "b", "a", "b"), wave = c(1, 1, 2, 2),
                   y = c(1, 2, 3, 4))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  expect_error(
    netis_data_from_split(node_static = ns, node_dynamic = nd, tie_static = ties,
                          wave_col = "wave"),
    "1/c"
  )
})

test_that("a tie to a node outside the roster is refused by every network builder", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "zzz"), w = 1)
  nd <- netis_data(nodes, ties)
  expect_error(compute_centrality(nd, network = w, measure = "degree", mode = "out"), "zzz")
  expect_error(compute_network_stat(nd, network = w, measure = "density"), "zzz")
  expect_error(netis_adjacency(nd, "w"), "zzz")
  skip_if_not_installed("igraph")
  expect_error(as_igraph(nd), "zzz")
  skip_if_not_installed("network")
  expect_error(as_network(nd), "zzz")
})

test_that("weighted betweenness with a missing weight refuses clearly", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(group = 1, node = c("a", "b", "c", "d"))
  ties <- data.frame(group = 1, source = c("a", "b", "c"), target = c("b", "c", "d"),
                     w = c(1, NA, 2))
  nd <- netis_data(nodes, ties)
  expect_error(compute_centrality(nd, network = w, measure = "betweenness", weights = "weighted"),
               "missing tie weight")
})
