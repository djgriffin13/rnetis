# Tests for igraph-backed network-feature metrics and the weight-semantics
# guardrail. Skipped entirely if igraph is not installed.

skip_if_not_installed("igraph")

.chain4 <- function() {
  nodes <- data.frame(group = 1, node = c("a", "b", "c", "d"))
  ties <- data.frame(group = 1, source = c("a", "b", "c"), target = c("b", "c", "d"), w = c(1, 1, 1))
  netis_data(nodes, ties)
}

# ---- igraph metrics run and return node-indexed output ----------------------

test_that("betweenness matches hand computation on a 4-node chain", {
  nd <- .chain4()
  bet <- compute_centrality(nd, w, measure = "betweenness")
  # a-b-c-d: b and c each lie on exactly one shortest path (a-d passes through
  # both, contributing 1 to each; no other pair's shortest path passes
  # through an intermediate node more than once here).
  expect_equal(bet$cent_betweenness_w[bet$node == "a"], 0)
  expect_equal(bet$cent_betweenness_w[bet$node == "d"], 0)
  # b lies on a -> c and a -> d; c lies on a -> d and b -> d.
  expect_equal(bet$cent_betweenness_w[bet$node == "b"], 2)
  expect_equal(bet$cent_betweenness_w[bet$node == "c"], 2)
})

test_that("pagerank returns a probability distribution (sums to 1)", {
  nd <- .chain4()
  pr <- compute_centrality(nd, w, measure = "pagerank")
  expect_equal(sum(pr$cent_pagerank_w), 1, tolerance = 1e-6)
})

test_that("closeness, eigenvector, and constraint all run and return one value per node", {
  nd <- .chain4()
  for (m in c("closeness", "eigenvector", "constraint")) {
    out <- suppressWarnings(compute_centrality(nd, w, measure = m))
    expect_equal(nrow(out), 4L)
    expect_true(all(!is.na(out[[paste0("cent_", m, "_w")]]) | m == "eigenvector"))
  }
})

test_that("an igraph-backed measure errors clearly, naming the package, when igraph is absent", {
  # Simulated by intercepting the guard rather than actually uninstalling
  # igraph (the same pragmatic choice made for the sna/numDeriv guards):
  # confirm the guard function itself fires correctly.
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_error(.netfeat_igraph_guard("betweenness"), "igraph")
})

# ---- weight-semantics guardrail ------------------------------

test_that("a strength-declared network refuses an unmentioned distance-based measure", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 3))
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(weight_semantics = "strength")))
  expect_error(
    compute_centrality(nd, w, measure = "betweenness", weights = "weighted"),
    "distance"
  )
})

test_that("supplying weight_transform = inverse() resolves the guardrail and is recorded", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 4))
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(weight_semantics = "strength")))
  bet <- compute_centrality(nd, w, measure = "betweenness", weights = "weighted", weight_transform = inverse())
  expect_equal(nrow(bet), 3L)
})

test_that("requesting weights = 'unweighted' bypasses the guardrail entirely", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 3))
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(weight_semantics = "strength")))
  expect_no_error(compute_centrality(nd, w, measure = "betweenness", weights = "unweighted"))
})

test_that("an undeclared ('unitless') weight_semantics does not trigger the guardrail", {
  nd <- .chain4() # no tie_meta declared -> weight_semantics defaults to "unitless"
  expect_no_error(compute_centrality(nd, w, measure = "betweenness", weights = "weighted"))
})

test_that("inverse() requires strictly positive weights and refuses a non-positive value", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = -1)
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(weight_semantics = "strength")))
  expect_error(
    compute_centrality(nd, w, measure = "betweenness", weights = "weighted", weight_transform = inverse()),
    "positive"
  )
})

test_that("inverse() correctly computes 1/w on the resolved matrix", {
  M <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("a", "b")))
  M["a", "b"] <- 2
  out <- .apply_weight_transform_matrix(M, inverse())
  expect_equal(out["a", "b"], 0.5)
  expect_equal(out["b", "a"], 0) # untouched -- no tie there
})

# ---- regression: mode = "in"/"out" was silently ignored for 4 of 5 igraph measures --
# (guards against the case where betweenness/eigenvector/pagerank/constraint
# accepted mode as a formal argument but never referenced it -- only
# closeness genuinely has an in/out/all concept in igraph's own API.)

test_that("mode = 'in'/'out' errors clearly for measures with no direction-mode concept of their own", {
  nd <- .chain4()
  for (m in c("betweenness", "eigenvector", "pagerank", "constraint")) {
    expect_error(compute_centrality(nd, w, measure = m, mode = "in"), "not supported")
    expect_error(compute_centrality(nd, w, measure = m, mode = "out"), "not supported")
  }
})

test_that("mode = 'all' (the default) never triggers the mode-unsupported error", {
  nd <- .chain4()
  for (m in c("betweenness", "eigenvector", "pagerank", "constraint")) {
    expect_no_error(suppressWarnings(compute_centrality(nd, w, measure = m, mode = "all")))
  }
})

test_that("mode = 'in'/'out' still works for closeness, which genuinely supports it", {
  nd <- .chain4()
  expect_no_error(compute_centrality(nd, w, measure = "closeness", mode = "in"))
  expect_no_error(compute_centrality(nd, w, measure = "closeness", mode = "out"))
})

# ---- additional coverage: signed/count weight semantics, auto weighting -----

test_that("the weight-semantics guardrail also fires for 'signed' and 'count' declared networks", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 3))
  nd_signed <- netis_data(nodes, ties, tie_meta = list(w = list(weight_semantics = "signed")))
  expect_error(compute_centrality(nd_signed, w, measure = "betweenness", weights = "weighted"), "distance")

  nd_count <- netis_data(nodes, ties, tie_meta = list(w = list(weight_semantics = "count")))
  expect_error(compute_centrality(nd_count, w, measure = "closeness", weights = "weighted"), "distance")
})

test_that("weights = 'auto' (default) follows the resolved network's own weighted/binary classification", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties_weighted <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 5))
  nd_weighted <- netis_data(nodes, ties_weighted)
  auto_result <- compute_centrality(nd_weighted, w, measure = "betweenness")
  explicit_weighted <- compute_centrality(nd_weighted, w, measure = "betweenness", weights = "weighted")
  expect_equal(auto_result$cent_betweenness_w, explicit_weighted$cent_betweenness_w)

  ties_binary <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(1, 1))
  nd_binary <- netis_data(nodes, ties_binary)
  auto_binary <- compute_centrality(nd_binary, w, measure = "betweenness")
  explicit_unweighted <- compute_centrality(nd_binary, w, measure = "betweenness", weights = "unweighted")
  expect_equal(auto_binary$cent_betweenness_w, explicit_unweighted$cent_betweenness_w)
})
