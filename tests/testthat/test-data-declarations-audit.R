# Data-layer declarations and identifiers: known cells on asymmetric fixtures.

.decl_nodes <- function() data.frame(group = 1, node = c("a", "b", "c"))

test_that("a missing group or node identifier is refused by name", {
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  expect_error(
    netis_data(data.frame(group = c(1, 1, NA), node = c("a", "b", "c")), ties),
    "node_data has 1 row\\(s\\) with a missing group"
  )
  expect_error(
    netis_data(data.frame(group = 1, node = c("a", "b", NA)), ties),
    "node_data has 1 row\\(s\\) with a missing node"
  )
  expect_error(
    netis_data(.decl_nodes(), data.frame(group = 1, source = c("a", NA), target = "b", w = 1)),
    "tie_data has 1 row\\(s\\) with a missing source"
  )
  expect_error(
    netis_data(.decl_nodes(), ties, group_data = data.frame(group = NA, z = 1)),
    "group_data has 1 row\\(s\\) with a missing group"
  )
})

test_that("an identifier containing the key separator is refused at construction", {
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  expect_error(
    netis_data(data.frame(group = 1, node = c("a", "b\rc")), ties),
    "carriage return"
  )
  # The forged pair: distinct entities that would otherwise share one key.
  expect_error(
    netis_data(data.frame(group = c("x", "x\ry"), node = c("y\rz", "z")), ties),
    "carriage return"
  )
})

test_that("symmetrize('any'/'both') gives a binary network, whatever the source declared", {
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 4))
  nd <- netis_data(.decl_nodes(), ties,
                   tie_meta = list(w = list(weight_semantics = "strength")))
  anyr <- .resolve_network(symmetrize(w, rule = "any"), nd, "1", NULL)
  expect_false(anyr$weighted)
  expect_identical(anyr$weight_semantics, "unitless")
  expect_identical(anyr$M[["a", "b"]], 1)
  expect_identical(anyr$M[["b", "a"]], 1)
  both <- .resolve_network(symmetrize(w, rule = "both"), nd, "1", NULL)
  expect_false(both$weighted)
  expect_identical(both$M[["a", "b"]], 0)
  # A value-combining rule keeps the source's weighting.
  maxr <- .resolve_network(symmetrize(w, rule = "max"), nd, "1", NULL)
  expect_true(maxr$weighted)
  expect_identical(maxr$weight_semantics, "strength")
  expect_identical(maxr$M[["b", "a"]], 2)

  skip_if_not_installed("igraph")
  # The path a - b - c as a binary network: b lies on the one a-c path. Refused
  # before as a strength/distance mismatch.
  out <- compute_centrality(nd, symmetrize(w, rule = "any"), measure = "betweenness",
                            name = "bt")
  expect_identical(out$bt[match("b", out$node)], 1)
})

test_that("presence follows the declared mode, and contradictory declarations are refused", {
  nodes <- .decl_nodes()
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"),
                     w = c(2, 0), flag = c(0, 1))
  # The column and the rule disagree on both rows; each mode must read its own.
  rule <- function(v) v != 0
  by_rule <- netis_data(nodes, ties, tie_meta = list(
    w = list(presence_mode = "value_rule", presence_rule = rule)
  ))
  m <- netis_adjacency(by_rule, "w", presence = TRUE)
  expect_identical(m[["a", "b"]], 1)
  expect_identical(m[["b", "c"]], 0)
  by_col <- netis_data(nodes, ties, tie_meta = list(
    w = list(presence_mode = "indicator", presence_col = "flag")
  ))
  m <- netis_adjacency(by_col, "w", presence = TRUE)
  expect_identical(m[["a", "b"]], 0)
  expect_identical(m[["b", "c"]], 1)

  expect_error(
    netis_data(nodes, ties, tie_meta = list(w = list(
      presence_mode = "value_rule", presence_rule = rule, presence_col = "flag"))),
    "value_rule.*presence_col"
  )
  expect_error(
    netis_data(nodes, ties, tie_meta = list(w = list(
      presence_mode = "indicator", presence_rule = rule, presence_col = "flag"))),
    "indicator.*presence_rule"
  )
  expect_error(
    netis_data(nodes, ties, tie_meta = list(w = list(
      presence_mode = "row_presence", presence_col = "flag"))),
    "row_presence.*presence_col"
  )
  expect_error(
    netis_data(nodes, ties, tie_meta = list(w = list(
      presence_mode = "complete_dyadic", presence_rule = rule, presence_col = "flag"))),
    "complete_dyadic.*either"
  )
  expect_error(
    netis_data(nodes, ties, tie_meta = list(w = list(presence_col = "flag"))),
    "presence_col.*without.*presence_mode"
  )
})

test_that("tie_meta entries are validated", {
  nodes <- .decl_nodes()
  ties <- data.frame(group = 1, source = "a", target = "b", w = 2)
  expect_error(netis_data(nodes, ties, tie_meta = list(w = list(zero_meanin = "unknown"))),
               "unknown field.*zero_meanin")
  expect_error(netis_data(nodes, ties, tie_meta = list(w = list(weighted = "yes"))),
               "weighted must be TRUE or FALSE")
  expect_error(netis_data(nodes, ties, tie_meta = list(w = list(direction_semantics = c("a", "b")))),
               "direction_semantics must be a single string")
  expect_error(netis_data(nodes, ties, tie_meta = list(w = "strength")),
               "must be a list")
})

test_that("add_centering() and add_decomposition() refuse a column they cannot center", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), wave = 1,
                      f = factor(c("lo", "hi", "hi")), s = c("x", "y", "z"), age = c(1, 2, 6))
  nd2 <- netis_data(nodes, data.frame(group = 1, source = "a", target = "b", wave = 1, w = 1),
                    wave_col = "wave")
  expect_error(add_centering(nd2, "f"), "'f' is factor, not numeric")
  expect_error(add_centering(nd2, "s"), "'s' is character, not numeric")
  expect_error(add_centering(nd2, "age", method = "constant", at = NA_real_), "finite")
  expect_error(add_centering(nd2, "age", method = "constant", at = Inf), "finite")
  expect_error(add_decomposition(nd2, "f", unit = "context"), "'f' is factor, not numeric")
  # The arithmetic itself: mean(1, 2, 6) = 3.
  expect_identical(netis_nodes(add_centering(nd2, "age"))$age_grand[
    order(netis_nodes(add_centering(nd2, "age"))$node)], c(-2, -1, 3))
  expect_identical(netis_nodes(add_centering(nd2, "age", method = "constant", at = 2))$age_c[
    order(netis_nodes(nd2)$node)], c(-1, 0, 4))
})

test_that("a wave argument must be one wave", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = rep(1:2, each = 2), x = 1:4)
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = c(1, 3))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(netis_adjacency(nd, "w", wave = c(1, 2)), "wave must be a single")
  expect_error(compute_exposure(nd, "x", "w", wave = c(1, 2)), "wave must be a single")
  expect_error(compute_centrality(nd, w, measure = "degree", wave = c(1, 2)), "wave must be a single")
})

test_that("time_meta needs one known unit per group", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = rep(1:2, each = 2))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = 1)
  tm <- data.frame(wave = 1:2, time_value = c(0, 5))
  expect_error(
    netis_data(nodes, ties, wave_col = "wave", time_meta = cbind(tm, time_unit = c("day", NA))),
    "time_unit"
  )
  expect_error(
    netis_data(nodes, ties, wave_col = "wave", time_meta = tm, time_unit = c("day", "week")),
    "one time unit"
  )
  expect_error(
    netis_data(nodes, ties, wave_col = "wave", time_meta = cbind(tm, time_unit = c("day", "week"))),
    "one time unit"
  )
  ok <- netis_data(nodes, ties, wave_col = "wave", time_meta = tm, time_unit = "day")
  expect_identical(netis_time_index(ok)$time_unit, c("day", "day"))
})

test_that("compute_network_stat(augment = TRUE) returns the group table with the column added", {
  nodes <- data.frame(group = c(1, 1, 1, 2, 2), node = c("a", "b", "c", "d", "e"))
  ties <- data.frame(group = c(1, 2), source = c("a", "d"), target = c("b", "e"), w = 1)
  gd <- data.frame(group = c(2, 1), size = c(20, 10))
  nd <- netis_data(nodes, ties, group_data = gd)
  out <- compute_network_stat(nd, w, measure = "density", augment = TRUE, name = "dens")
  expect_identical(nrow(out), 2L)
  expect_setequal(names(out), c("group", "size", "dens"))
  expect_lt(abs(out$dens[out$group == 1] - 1 / 6), 1e-12)
  expect_identical(out$dens[out$group == 2], 0.5)
  expect_identical(out$size[out$group == 1], 10)
  # With no group table the group table is just the identifiers.
  out2 <- compute_network_stat(netis_data(nodes, ties), w, measure = "density",
                               augment = TRUE, name = "dens")
  expect_identical(names(out2), c("group", "dens"))
  # A node-level measure still augments the node table.
  cent <- compute_centrality(nd, w, measure = "degree", mode = "out", augment = TRUE, name = "od")
  expect_identical(nrow(cent), 5L)
  expect_identical(cent$od[cent$node == "a"], 1)
})

test_that("an adjacency series array with one node per slice converts", {
  arr <- array(0, dim = c(1L, 1L, 3L), dimnames = list("solo", "solo", NULL))
  nd <- netis_data_from_adjacency_series(arr, waves = c(9, 10, 11))
  expect_identical(netis_schema(nd)$waves, c(9, 10, 11))
  expect_identical(nrow(netis_ties(nd)), 0L)
  expect_identical(netis_nodes(nd)$node, "solo")
  # A 2 x 2 x 2 array keeps its dimnames and cells.
  a2 <- array(c(0, 1, 0, 0, 0, 0, 2, 0), dim = c(2, 2, 2),
              dimnames = list(c("a", "b"), c("a", "b"), NULL))
  nd2 <- netis_data_from_adjacency_series(a2, waves = 1:2)
  t2 <- netis_ties(nd2)
  expect_identical(t2$weight[t2$wave == 1 & t2$source == "b" & t2$target == "a"], 1)
  expect_identical(t2$weight[t2$wave == 2 & t2$source == "a" & t2$target == "b"], 2)
})

test_that("as_igraph_list() and as_network_list() build only observed group-waves", {
  nodes <- data.frame(group = c(1, 1, 1, 1, 2, 2), node = c("a", "b", "a", "b", "c", "d"),
                      wave = c(1, 1, 2, 2, 1, 1))
  ties <- data.frame(group = c(1, 1, 2), source = c("a", "b", "c"), target = c("b", "a", "d"),
                     wave = c(1, 2, 1), w = c(2, 3, 5))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  skip_if_not_installed("igraph")
  gl <- as_igraph_list(nd)
  expect_identical(names(gl), c("1_w1", "1_w2", "2_w1"))
  expect_identical(igraph::E(gl[["1_w2"]])$w, 3)
  expect_error(as_igraph(nd, group = "2", wave = 2), "group '2' has no node rows at wave 2")
  skip_if_not_installed("network")
  nl <- as_network_list(nd)
  expect_identical(names(nl), c("1_w1", "1_w2", "2_w1"))
  expect_error(as_network(nd, group = "2", wave = 2), "group '2' has no node rows at wave 2")
})

test_that("the weight-semantics guard runs in both directions", {
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 4))
  nd <- netis_data(.decl_nodes(), ties, tie_meta = list(w = list(weight_semantics = "distance")))
  expect_error(compute_centrality(nd, w, measure = "strength"), "distances.*larger value")
  # With the transform a distance reads as a strength: 1/2 and 1/4.
  out <- compute_centrality(nd, w, measure = "strength", mode = "out",
                            weight_transform = inverse(), name = "s")
  expect_identical(out$s[match(c("a", "b", "c"), out$node)], c(0.5, 0.25, 0))
  # A transform on a measure that reads no weights is not silently ignored.
  expect_error(compute_centrality(nd, w, measure = "degree", weight_transform = inverse()),
               "weight_transform has no effect")
  skip_if_not_installed("igraph")
  for (m in c("eigenvector", "pagerank", "constraint")) {
    expect_error(compute_centrality(nd, w, measure = m, weights = "weighted"),
                 "distances.*larger value", info = m)
  }
  # Betweenness reads distances, so a distance declaration passes unchanged.
  bt <- compute_centrality(nd, w, measure = "betweenness", weights = "weighted", name = "b")
  expect_identical(bt$b[match("b", bt$node)], 1)
})

test_that("a dense network is inferred complete_dyadic only where a row can be a non-tie", {
  ids <- c("a", "b", "c", "d")
  clique <- matrix(1, 4, 4, dimnames = list(ids, ids)); diag(clique) <- 0
  # The importer dropped zero cells and declared what they meant, so a stored
  # row is a tie however dense the matrix is.
  nd <- netis_data_from_adjacency(clique)
  expect_identical(netis_schema(nd)$tie_descriptors$weight$presence_mode, "row_presence")
  expect_identical(compute_centrality(nd, weight, measure = "degree", mode = "out", name = "d")$d,
                   c(3, 3, 3, 3))
  w <- clique; w[w == 1] <- 5
  expect_identical(netis_schema(netis_data_from_adjacency(w))$tie_descriptors$weight$presence_mode,
                   "row_presence")

  # A mutual dyad in a two-node group: every dyad has a row holding 1, so no
  # reading of it differs.
  pair <- netis_data(data.frame(group = 1, node = c("p", "q")),
                     data.frame(group = 1, source = c("p", "q"), target = c("q", "p"), w = 1))
  expect_identical(netis_schema(pair)$tie_descriptors$w$presence_mode, "row_presence")

  # Stored zeros, a missing row, or values with no declared meaning stay
  # undetermined.
  pairs <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$source != pairs$target, ]
  pairs$group <- 1
  nodes <- data.frame(group = 1, node = ids)
  zeros <- pairs; zeros$w <- c(0, rep(1, nrow(pairs) - 1L))
  expect_identical(netis_schema(netis_data(nodes, zeros))$tie_descriptors$w$presence_mode,
                   "complete_dyadic")
  near <- pairs[-1, ]; near$w <- 1
  expect_identical(netis_schema(netis_data(nodes, near))$tie_descriptors$w$presence_mode,
                   "complete_dyadic")
  rated <- pairs; rated$w <- seq_len(nrow(pairs))
  expect_identical(netis_schema(netis_data(nodes, rated))$tie_descriptors$w$presence_mode,
                   "complete_dyadic")
})

test_that("graph attributes and schema records a conversion drops are named", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(c("a", "b"), directed = TRUE)
  g <- igraph::set_graph_attr(g, "study", "pilot")
  expect_warning(netis_data_from_igraph(g), "study")
  expect_silent(netis_data_from_igraph(g, allow_loss = "foreign_attributes"))
  expect_true("foreign_attributes" %in% netis_losses()$loss_id)

  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1, v = 2)
  nd <- netis_data(nodes, ties, group_data = data.frame(group = 1, z = 1),
                   tie_meta = list(v = list(weight_semantics = "count")))
  expect_warning(as_igraph(nd, network = "w"), "group table")
  expect_warning(as_igraph(nd, network = "w"), "'v'")
  expect_silent(as_igraph(nd, network = "w", allow_loss = "schema_records"))
  skip_if_not_installed("network")
  expect_warning(as_network(nd, network = "w"), "group table")
  expect_silent(as_network(nd, network = "w", allow_loss = "schema_records"))
})

test_that("network features at an unobserved wave are NA with a warning, not zero", {
  nodes <- data.frame(group = c(rep(1, 6), rep(2, 4)),
                      node = c(rep(c("a", "b", "c"), 2), rep(c("d", "e"), 2)),
                      wave = c(rep(1:2, each = 3), rep(1:2, each = 2)))
  # Group 1's network is observed at wave 1 only; group 2 has ties at both.
  ties <- data.frame(group = c(1, 2, 2), source = c("a", "d", "d"), target = c("b", "e", "e"),
                     wave = c(1, 1, 2), w = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_warning(
    out <- compute_centrality(nd, w, measure = "degree", mode = "out", wave = 2, name = "d"),
    "group 1 wave 2"
  )
  expect_true(all(is.na(out$d[out$group == "1"])))
  expect_identical(out$d[out$node == "d"], 1)
  expect_warning(
    st <- compute_network_stat(nd, w, measure = "density", wave = 2, name = "dd"),
    "group 1 wave 2"
  )
  expect_true(is.na(st$dd[st$group == "1"]))
  expect_identical(st$dd[st$group == "2"], 0.5)
  # An observed wave is not disturbed.
  expect_silent(obs <- compute_centrality(nd, w, measure = "degree", mode = "out", wave = 1, name = "d"))
  expect_identical(obs$d[obs$node == "a"], 1)
  # A group with no ties anywhere has an empty network, not an absent one.
  nd0 <- netis_data(nodes, ties[ties$group == 2, ], wave_col = "wave")
  expect_silent(z <- compute_centrality(nd0, w, measure = "degree", mode = "out", wave = 2, name = "d"))
  expect_identical(z$d[z$node == "a"], 0)
})

test_that("ties to a node with no row at that wave are kept by network features", {
  # c is on the group's roster (wave 2) but has no row at wave 1; a -> c and
  # b -> a are the ties at wave 1.
  nodes <- data.frame(group = 1, node = c("a", "b", "a", "b", "c"), wave = c(1, 1, 2, 2, 2),
                      x = c(1, 2, 3, 4, 5))
  ties <- data.frame(group = 1, source = c("a", "b", "a"), target = c("c", "a", "b"),
                     wave = c(1, 1, 2), w = c(7, 3, 2))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  out <- compute_centrality(nd, w, measure = "strength", mode = "out", wave = 1, name = "s")
  # One row per node observed at the wave; a's strength counts its tie to c.
  expect_identical(out$node, c("a", "b"))
  expect_identical(out$s, c(7, 3))
  deg <- compute_centrality(nd, binarize(w, rule = nonzero()), measure = "degree",
                            mode = "out", wave = 1, name = "d")
  expect_identical(deg$d, c(1, 1))
  # Density is over the vertices in play at the wave: a, b, c, two ties of six.
  dens <- compute_network_stat(nd, w, measure = "density", wave = 1, name = "dd")
  expect_lt(abs(dens$dd - 2 / 6), 1e-12)
  # And the exposure engine reads the same network: c's value is missing at
  # wave 1, so an exposure that depends on it is NA (b influences a; a -> c).
  ex <- compute_exposure(nd, "x", "w", wave = 1, direction = "target_influences_source")
  expect_true(is.na(ex$ex_x_w[ex$node == "a"]))
  expect_identical(ex$ex_x_w[ex$node == "b"], 3 * 1)
})

test_that("a prohibited self-tie is refused when a network is built from it", {
  nodes <- .decl_nodes()
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("a", "b"), w = c(7, 2))
  nx <- netis_data(cbind(nodes, x = c(1, 10, 100)), ties)
  expect_error(compute_exposure(nx, "x", "w"), "self-tie")
  expect_error(compute_centrality(nx, w, measure = "degree"), "self-tie")
  expect_error(netis_adjacency(nx, "w"), "self-tie")
  permitted <- netis_data(cbind(nodes, x = c(1, 10, 100)), ties, self_ties = "permitted")
  # a's own value through its self-tie: 7 * 1.
  ex <- compute_exposure(permitted, "x", "w")
  expect_identical(ex$ex_x_w[ex$node == "a"], 7)
  # A complete dyadic table that stores the diagonal as zeros is not a self-tie.
  ids <- c("a", "b")
  full <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
  full$group <- 1
  full$w <- c(0, 1, 1, 0)
  nz <- netis_data(data.frame(group = 1, node = ids, x = c(1, 2)), full,
                   tie_meta = list(w = list(zero_meaning = "measured_zero")))
  expect_identical(netis_adjacency(nz, "w")[["a", "b"]], 1)
})
