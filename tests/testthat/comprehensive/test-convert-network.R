# statnet `network` conversion.
#
# The fixture is the hostile one (DF.9): directed, weighted, ASYMMETRIC so a
# transposition changes the answer, multi-group, with an isolate.

.nw_fixture <- function() {
  nodes <- data.frame(
    group = c(rep("A", 3), rep("B", 2)),
    node = c("a", "b", "z", "c", "d"),
    trait = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )
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

test_that("export builds the vertex set from the roster, so isolates survive", {
  skip_if_not_installed("network")
  net <- as_network(.nw_fixture(), network = "w", group = "A")
  expect_equal(network::network.size(net), 3)
  expect_true("z" %in% as.character(network::network.vertex.names(net)))
  expect_equal(network::network.edgecount(net), 2)
})

test_that("EDGE VALUES LAND ON THE RIGHT DYADS, on an asymmetric fixture", {
  skip_if_not_installed("network")
  # The failure this guards against attaches every weight to the wrong dyad
  # while leaving edge count, density, and degree all correct.
  net <- as_network(.nw_fixture(), network = "w", group = "A")
  m <- network::as.matrix.network.adjacency(net, attrname = "w")
  expect_identical(m["a", "b"], 2)
  expect_identical(m["b", "a"], 5)
})

test_that("node and tie variables travel as vertex and edge attributes", {
  skip_if_not_installed("network")
  net <- as_network(.nw_fixture(), network = "w", group = "A")
  expect_true("trait" %in% network::list.vertex.attributes(net))
  expect_true("w" %in% network::list.edge.attributes(net))
  ord <- match(c("a", "b", "z"), as.character(network::network.vertex.names(net)))
  expect_identical(network::get.vertex.attribute(net, "trait")[ord], c(1, 2, 3))
})

test_that("directedness and the self-tie policy come from the schema", {
  skip_if_not_installed("network")
  nd <- .nw_fixture()
  expect_true(network::is.directed(as_network(nd, network = "w", group = "A")))

  nodes <- data.frame(group = 1, node = c("a", "b"), stringsAsFactors = FALSE)
  ties <- data.frame(group = 1, source = "a", target = "a", w = 1, stringsAsFactors = FALSE)
  loops <- netis_data(nodes, ties, self_ties = "permitted")
  expect_true(network::has.loops(as_network(loops, network = "w")))
})

test_that("a round trip preserves the roster, the values, and the direction", {
  skip_if_not_installed("network")
  nd <- .nw_fixture()
  back <- netis_data_from_network(as_network(nd, network = "w", group = "A"))

  expect_identical(sort(netis_nodes(back)$node), c("a", "b", "z"))
  before <- netis_adjacency(nd, "w", drop = FALSE)$A
  # `back` holds the single exported group, so drop = TRUE gives the matrix.
  after <- netis_adjacency(back, "w", drop = TRUE)
  expect_identical(after[rownames(before), colnames(before)], before)
})

test_that("declared semantics survive the round trip as declared", {
  skip_if_not_installed("network")
  back <- netis_data_from_network(as_network(.nw_fixture(), network = "w", group = "A"))
  d <- netis_schema(back)$tie_descriptors$w
  expect_identical(d$weight_semantics, "strength")
  expect_identical(d$direction_semantics, "source advises target")
  b <- netis_boundary(back)
  expect_identical(b$semantics$state[b$semantics$field == "weight_semantics"], "declared")
})

test_that("as_network_list() covers every group and wave", {
  skip_if_not_installed("network")
  l <- as_network_list(.nw_fixture(), network = "w")
  expect_identical(sort(names(l)), c("A", "B"))
  expect_equal(network::network.size(l$B), 2)
})

test_that("an undetermined complete_dyadic network refuses to export", {
  skip_if_not_installed("network")
  # A `network` edge exists or it does not, so exporting a network whose
  # presence is undetermined would have to invent the distinction the tie descriptor
  # exists to protect.
  nodes <- data.frame(group = 1, node = c("a", "b"), stringsAsFactors = FALSE)
  ties <- data.frame(
    group = 1, source = c("a", "b"), target = c("b", "a"), w = c(1, 0),
    stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(presence_mode = "complete_dyadic")))
  expect_error(as_network(nd, network = "w"), "undetermined")
})

test_that("a declared indicator resolves which rows become edges", {
  skip_if_not_installed("network")
  nodes <- data.frame(group = 1, node = c("a", "b"), stringsAsFactors = FALSE)
  ties <- data.frame(
    group = 1, source = c("a", "b"), target = c("b", "a"),
    w = c(1, 0), present = c(TRUE, FALSE), stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(
    presence_mode = "indicator", presence_col = "present"
  )))
  net <- as_network(nd, network = "w")
  # The measured-zero dyad is observed but not a tie, so it is not an edge.
  expect_equal(network::network.edgecount(net), 1)
})

test_that("a multiplex network is refused rather than collapsed", {
  skip_if_not_installed("network")
  mp <- network::network.initialize(2, directed = TRUE, multiple = TRUE)
  mp <- network::add.edges(mp, tail = c(1, 1), head = c(2, 2))
  e <- tryCatch(netis_data_from_network(mp), netis_semantic_loss = function(e) e)
  expect_s3_class(e, "netis_semantic_loss")
  expect_identical(e$loss_id, "tie_value_collapse")
})

test_that("a bipartite network is refused", {
  skip_if_not_installed("network")
  bp <- network::network.initialize(4, directed = FALSE, bipartite = 2)
  expect_error(netis_data_from_network(bp), "bipartite")
})

test_that("a foreign network object reads directedness from its own flag", {
  skip_if_not_installed("network")
  net <- network::network.initialize(3, directed = TRUE)
  network::network.vertex.names(net) <- c("a", "b", "c")
  net <- network::add.edges(net, tail = 1, head = 2)

  nd <- netis_data_from_network(net)
  expect_true(netis_schema(nd)$directed)
  # And the ledger records that the SOURCE encoded it, not the caller.
  expect_true(netis_boundary(nd)$directedness$source_encoded)
  expect_identical(netis_boundary(nd)$directedness$state, "declared")
  # Isolate `c` is still there.
  expect_identical(nrow(netis_nodes(nd)), 3L)
})

test_that("a foreign network leaves unknowable semantics undeclared", {
  skip_if_not_installed("network")
  net <- network::network.initialize(2, directed = TRUE)
  network::network.vertex.names(net) <- c("a", "b")
  net <- network::add.edges(net, tail = 1, head = 2)
  b <- netis_boundary(netis_data_from_network(net))
  # A network object cannot say what a tie MEANS.
  expect_identical(b$semantics$state[b$semantics$field == "direction_semantics"], "undeclared")
  expect_identical(b$semantics$state[b$semantics$field == "weight_semantics"], "undeclared")
})

test_that("as_netis_data() dispatches on the network class", {
  skip_if_not_installed("network")
  net <- as_network(.nw_fixture(), network = "w", group = "A")
  expect_s3_class(as_netis_data(net), "netis_data")
  expect_identical(netis_boundary(as_netis_data(net))$source$source_class, "network")
})

test_that("an NA tie weight stays distinguishable from an absent tie", {
  skip_if_not_installed("network")
  # a missing weight and a missing tie are different states, and a converter
  # that flattened one into the other would be losing exactly the distinction
  # the package exists to protect.
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), stringsAsFactors = FALSE)
  ties <- data.frame(
    group = 1, source = c("a", "b"), target = c("b", "c"), w = c(NA, 3),
    stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties)
  back <- netis_data_from_network(as_network(nd, network = "w"))
  t2 <- netis_ties(back)
  # Two ties, one of which has an unknown weight -- not one tie.
  expect_identical(nrow(t2), 2L)
  expect_true(any(is.na(t2$w)))
})

test_that("a derived (add_centering()/add_decomposition()) node column announces its loss on export", {
  skip_if_not_installed("network")
  nd <- add_centering(.nw_fixture(), "trait", method = "grand")
  w <- tryCatch(as_network(nd, network = "w", group = "A"),
               netis_representational_loss = function(e) e)
  expect_s3_class(w, "netis_representational_loss")
  expect_identical(w$loss_id, "derived_provenance")
  expect_silent(as_network(nd, network = "w", group = "A", allow_loss = "derived_provenance"))
})
