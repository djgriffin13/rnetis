# Conversions and derive-and-attach helpers must not change what the object
# says about itself: edges are present ties, reserved names are not
# overwritten, and inferred or undeclared semantics stay inferred or
# undeclared.

.prov_states <- function(nd, var) {
  b <- netis_boundary(nd)$semantics
  b <- b[b$variable == var, ]
  stats::setNames(b$state, b$field)
}

test_that("as_igraph() exports present ties only, and refuses an undetermined network", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  # A complete dyadic table: 6 rows, 2 ties, the rest measured zeros.
  pairs <- subset(expand.grid(source = c("a", "b", "c"), target = c("a", "b", "c"),
                              stringsAsFactors = FALSE), source != target)
  pairs$w <- ifelse(pairs$source == "a" & pairs$target %in% c("b", "c"), 1, 0)
  undetermined <- netis_data(nodes, data.frame(group = 1, pairs))
  expect_error(as_igraph(undetermined), "undetermined")

  ruled <- netis_data(nodes, data.frame(group = 1, pairs), tie_meta = list(
    w = list(presence_mode = "value_rule", presence_rule = function(v) v != 0)))
  g <- suppressWarnings(as_igraph(ruled, allow_loss = "presence_metadata"))
  expect_equal(igraph::ecount(g), 2L)
  el <- igraph::as_edgelist(g)
  expect_setequal(paste(el[, 1], el[, 2]), c("a b", "a c"))
})

test_that("as_igraph() refuses to overwrite a 'weight' tie variable or a 'name' node variable", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", trust = 1, weight = 7)
  nd <- netis_data(nodes, ties)
  expect_error(as_igraph(nd, network = "trust"), "'weight'")
  expect_equal(igraph::E(as_igraph(nd, network = "weight"))$weight, 7)

  nodes2 <- data.frame(group = 1, node = c("a", "b"), name = c("Ann", "Bob"))
  nd2 <- netis_data(nodes2, data.frame(group = 1, source = "a", target = "b", w = 1))
  expect_error(as_igraph(nd2), "'name'")
})

test_that("netis_data_from_igraph() refuses vertex attributes that would re-key nodes", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(c("x", "y"), directed = TRUE)
  igraph::V(g)$group <- c("red", "blue")
  expect_error(netis_data_from_igraph(g), "'group'")
})

test_that("an edgeless graph converts in both directions", {
  skip_if_not_installed("igraph")
  nd <- netis_data(data.frame(group = 1, node = c("a", "b")),
                   data.frame(group = numeric(0), source = character(0), target = character(0),
                              w = numeric(0)))
  g <- as_igraph(nd)
  expect_equal(igraph::ecount(g), 0L)
  back <- netis_data_from_igraph(g)
  expect_equal(nrow(netis_ties(back)), 0L)
  expect_setequal(as.character(netis_nodes(back)$node), c("a", "b"))
})

test_that("an igraph round trip keeps inferred and undeclared semantics as they were", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(group = 1, node = c("a", "b", "c", "d"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 5))
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(weight_semantics = "strength")))
  before <- .prov_states(nd, "w")
  back <- netis_data_from_igraph(as_igraph(nd))
  after <- .prov_states(back, "w")
  expect_identical(after, before)
  expect_identical(unname(after["weight_semantics"]), "declared")
  expect_identical(unname(after["zero_meaning"]), "undeclared")
  expect_identical(unname(after["presence_mode"]), "inferred")
})

test_that("a stored zero under an inferred presence mode is not laundered by igraph", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(group = 1, node = letters[1:5])
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(0, 3))
  nd <- netis_data(nodes, ties)
  expect_error(netis_adjacency(nd, "w", presence = TRUE), "stored zero")
  # The export refuses for the same reason, so no round trip can switch the
  # refusal off.
  expect_error(as_igraph(nd), "stored zero")
})

test_that("add_centering() keeps the ledger, the stored-zero refusal, time and attributes", {
  nodes <- data.frame(group = 1, node = rep(letters[1:5], 2), wave = rep(1:2, each = 5),
                      y = c(1, 4, 2, 8, 5, 2, 3, 9, 1, 4))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), wave = 1,
                     w = c(0, 3))
  tm <- data.frame(group = 1, wave = 1:2, time_value = c(0, 30))
  nd <- netis_data(nodes, ties, wave_col = "wave", time_meta = tm, time_unit = "day")
  attr(nd, "truth") <- list(beta = 0.4)
  nd2 <- add_centering(nd, "y")

  expect_identical(.prov_states(nd2, "w"), .prov_states(nd, "w"))
  expect_identical(unname(.prov_states(nd2, "w")["zero_meaning"]), "undeclared")
  expect_error(netis_adjacency(nd2, "w", presence = TRUE, wave = 1), "stored zero")
  ti <- netis_time_index(nd2)
  expect_equal(ti$time_value[ti$wave_label == "2"], 30)
  expect_identical(attr(nd2, "truth"), list(beta = 0.4))

  nd3 <- add_baseline_provenance(nd, "y")
  expect_identical(.prov_states(nd3, "w"), .prov_states(nd, "w"))
  expect_equal(netis_time_index(nd3)$time_value, netis_time_index(nd)$time_value)
})
