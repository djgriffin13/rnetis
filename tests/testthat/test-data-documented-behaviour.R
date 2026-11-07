# Behavior the data layer's help pages state, pinned on known cells.

test_that("diagnose_data() reads a stored zero on the diagonal as no tie", {
  # netis_data() documents that a stored zero on the diagonal of a complete
  # dyadic table is not a tie, and every network builder accepts it.
  dyads <- expand.grid(source = c("a", "b"), target = c("a", "b"),
                       stringsAsFactors = FALSE)
  dyads$group <- 1
  dyads$k <- c(0, 1, 1, 0)
  nd <- netis_data(
    data.frame(group = 1, node = c("a", "b")), dyads,
    tie_meta = list(k = list(presence_mode = "value_rule",
                             presence_rule = function(v) v != 0))
  )
  expect_silent(netis_adjacency(nd, "k"))
  codes <- vapply(diagnose_data(nd), function(f) f$code, character(1))
  expect_false("self_tie_despite_prohibited" %in% codes)
})

test_that("diagnose_data() still reports a self-tie that carries a value, and only that row", {
  ties <- data.frame(group = 1, source = c("a", "b", "a"), target = c("a", "b", "b"),
                     k = c(2, 0, 1))
  nd <- netis_data(data.frame(group = 1, node = c("a", "b")), ties)
  found <- filter_diagnostics(diagnose_data(nd), level = "ERROR")
  hit <- Filter(function(f) f$code == "self_tie_despite_prohibited", found)
  expect_length(hit, 1L)
  expect_identical(netis_ties(nd)$source[hit[[1]]$details$rows], "a")
  expect_match(hit[[1]]$message, "^1 self-tie")
})

test_that("diagnose_data() reports a self-tie row with no tie variable at all", {
  nd <- netis_data(data.frame(group = 1, node = "a"),
                   data.frame(group = 1, source = "a", target = "a"))
  codes <- vapply(diagnose_data(nd), function(f) f$code, character(1))
  expect_true("self_tie_despite_prohibited" %in% codes)
})

test_that("diagnose_data() counts a tie with two unknown endpoints as one tie", {
  nd <- netis_data(data.frame(group = 1, node = "a"),
                   data.frame(group = 1, source = "x", target = "y", k = 1))
  found <- Filter(function(f) f$code == "tie_references_unknown_node", diagnose_data(nd))
  expect_match(found[[1]]$message, "^1 tie\\(s\\)")
  expect_setequal(found[[1]]$details$nodes, c("x", "y"))
})

test_that("netis_data_from_igraph_list() names an unvalued foreign graph's tie variable", {
  skip_if_not_installed("igraph")
  g1 <- igraph::make_graph(c("a", "b"), directed = TRUE)
  g2 <- igraph::make_graph(c("b", "a"), directed = TRUE)

  named <- netis_data_from_igraph_list(list(g1, g2), groups = c("x", "y"), network = "w")
  ties <- netis_ties(named)
  expect_identical(ties$w[ties$group == "x"], 1)
  expect_identical(ties$w[ties$group == "y"], 1)

  default <- netis_data_from_igraph_list(list(g1, g2), groups = c("x", "y"))
  expect_true("weight" %in% names(netis_ties(default)))
})

test_that("netis_data_from_igraph_list() keeps a foreign graph's own edge attributes", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(c("a", "b"), directed = TRUE)
  igraph::E(g)$strength <- 3
  ties <- netis_ties(netis_data_from_igraph_list(list(g), groups = "x", network = "w"))
  expect_identical(ties$strength, 3)
  expect_false("w" %in% names(ties))
})

test_that("a series roster with no group column is refused for several groups", {
  m <- matrix(c(0, 1, 0, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  series <- list(A = list(`1` = m), B = list(`1` = m))
  expect_error(
    netis_data_from_adjacency_series(series, nodes = c("a", "b", "z")),
    "every node would be placed in group"
  )
  expect_error(
    netis_data_from_adjacency_series(series, nodes = data.frame(node = c("a", "b"))),
    "every node would be placed in group"
  )
  # A roster that says which group each node is in is what the message asks for.
  roster <- data.frame(group = c("A", "A", "A", "B", "B"),
                       node = c("a", "b", "z", "a", "b"))
  nd <- netis_data_from_adjacency_series(series, nodes = roster)
  expect_identical(sum(netis_nodes(nd)$group == "A"), 3L)
})

test_that("a vector roster is accepted for the one group of a flat series", {
  m <- matrix(c(0, 1, 0, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  nd <- netis_data_from_adjacency_series(list(`1` = m, `2` = m), nodes = c("a", "b", "z"))
  expect_setequal(netis_nodes(nd)$node, c("a", "b", "z"))
})

test_that("a derived-variable record prints only what it has", {
  nd <- netis_data(data.frame(group = 1, node = c("a", "b"), z = c(1, 3)),
                   data.frame(group = 1, source = "a", target = "b", k = 1))
  grand <- capture.output(print(netis_schema(add_centering(nd, "z"))$derived$z_grand))
  expect_false(any(grepl("held within \\)", grand)))
  expect_match(grand[1], "averaged over node", fixed = TRUE)

  const <- capture.output(
    print(netis_schema(add_centering(nd, "z", method = "constant", at = 1))$derived$z_c)
  )
  expect_false(any(grepl("averaged over ,", const, fixed = TRUE)))
  expect_match(const[1], "relative to a fixed reference", fixed = TRUE)
})

test_that("the refusal messages point at what exists", {
  nd <- netis_data(data.frame(group = 1, node = c("a", "b"), z = c(1, 3)),
                   data.frame(group = 1, source = "a", target = "b", k = 1))
  expect_error(add_decomposition(nd, "z", unit = "actor"), "?add_decomposition", fixed = TRUE)
  expect_error(as_netis_data(list()), "an igraph graph, a network object", fixed = TRUE)
  expect_error(
    fit_selection(k ~ z, data = nd),
    "sender(), receiver(), or dyad()", fixed = TRUE
  )
})
