# The undirected convention: one stored row per unordered pair, mirrored once
# when any matrix is built, and read the same way by every consumer.
#
# Every fixture here is ASYMMETRIC in the stored rows -- each pair appears in
# one orientation only, and the node values differ by orders of magnitude -- so
# a consumer that reads the row one way, or symmetrizes by max, gives a
# different answer from the truth. None of these tests goes through the
# simulator, which shares the engine and so cannot detect a one-way reading.

.und_path <- function(flip = FALSE) {
  # Path a - b - c. With flip = TRUE the same pairs are stored in the other
  # column order, which must not change anything.
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), x = c(1, 10, 100))
  ties <- if (flip) {
    data.frame(group = 1, source = c("b", "c"), target = c("a", "b"), w = c(2, 5))
  } else {
    data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 5))
  }
  netis_data(nodes, ties, directed = FALSE)
}

test_that("undirected exposure reaches both endpoints of a tie (known answer)", {
  # W (symmetric) = [0 2 0; 2 0 5; 0 5 0]; x = (1, 10, 100).
  # E = W x = (20, 2 + 500, 50) = (20, 502, 50).
  for (flip in c(FALSE, TRUE)) {
    out <- compute_exposure(.und_path(flip), "x", "w")
    expect_equal(out$ex_x_w[out$node == "a"], 20)
    expect_equal(out$ex_x_w[out$node == "b"], 502)
    expect_equal(out$ex_x_w[out$node == "c"], 50)
  }
  # Row-normalised: b averages over both neighbors, weighted 2 and 5.
  out <- compute_exposure(.und_path(), "x", "w", normalize = "row")
  expect_equal(out$ex_x_w[out$node == "b"], (2 * 1 + 5 * 100) / 7)
  expect_equal(out$ex_x_w[out$node == "a"], 10)
  expect_equal(out$ex_x_w[out$node == "c"], 10)
})

test_that("the direction argument cannot change an undirected exposure", {
  a <- compute_exposure(.und_path(), "x", "w", direction = "source_influences_target")
  b <- compute_exposure(.und_path(), "x", "w", direction = "target_influences_source")
  expect_identical(a$ex_x_w, b$ex_x_w)
})

test_that("the canonical W every consumer shares is symmetric for undirected data", {
  nd <- .und_path()
  W <- .build_canonical_W(netis_ties(nd), c("a", "b", "c"), "source", "target",
                          "w", "source_influences_target", directed = FALSE)
  expect_equal(unname(W["a", "b"]), 2)
  expect_equal(unname(W["b", "a"]), 2)
  expect_equal(unname(W["c", "b"]), 5)
  expect_equal(unname(W["b", "c"]), 5)
  expect_equal(unname(W["a", "c"]), 0)
  # The endogenous (SAR) W reads the same matrix.
  tab <- data.frame(group = 1, node = c("c", "a", "b"))
  W1 <- .build_endogenous_w1(nd, netis_schema(nd), tab,
                             list(conduit = "w", direction = "source_influences_target"),
                             NULL)
  expect_equal(unname(W1["a", "b"]), 2)
  expect_equal(unname(W1["b", "a"]), 2)
  expect_equal(unname(W1["b", "c"]), 5)
})

test_that("a negative undirected weight is kept on both endpoints", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(-2, 3))
  nd <- netis_data(nodes, ties, directed = FALSE)
  st <- compute_centrality(nd, network = w, measure = "strength")
  expect_equal(st$cent_strength_w[st$node == "a"], -2)
  expect_equal(st$cent_strength_w[st$node == "b"], 1)
  expect_equal(st$cent_strength_w[st$node == "c"], 3)
  m <- netis_adjacency(nd, "w")
  expect_equal(unname(m["b", "a"]), -2)
  expect_equal(unname(m["a", "b"]), -2)
  expect_equal(unname(m["c", "b"]), 3)
})

test_that("storing both orientations of an undirected pair is refused", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "a"), w = c(2, 7))
  expect_error(netis_data(nodes, ties, directed = FALSE), "more than once")
  # The same rows are an ordinary reciprocated pair when directed.
  expect_s3_class(netis_data(nodes, ties, directed = TRUE), "netis_data")
  # A pair stored once per wave is not a duplicate.
  tw <- data.frame(group = 1, source = c("a", "b"), target = c("b", "a"),
                   wave = c(1, 2), w = c(2, 7))
  nw <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = rep(1:2, each = 2))
  expect_s3_class(netis_data(nw, tw, wave_col = "wave", directed = FALSE), "netis_data")
})

test_that("symmetrize() on undirected data sees both directions holding the value", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 3))
  nd <- netis_data(nodes, ties, directed = FALSE)
  st <- function(r) stats::setNames(r[[ncol(r)]], r$node)
  truth <- c(a = 2, b = 5, c = 3)
  expect_equal(st(compute_centrality(nd, network = symmetrize(w, rule = "min"),
                                     measure = "strength")), truth)
  expect_equal(st(compute_centrality(nd, network = symmetrize(w, rule = "max"),
                                     measure = "strength")), truth)
  expect_equal(st(compute_centrality(nd, network = symmetrize(w, rule = "mean"),
                                     measure = "strength")), truth)
  expect_equal(st(compute_centrality(nd, network = symmetrize(w, rule = "sum"),
                                     measure = "strength")), 2 * truth)
  dg <- compute_centrality(nd, network = symmetrize(w, rule = "both"), measure = "degree")
  expect_equal(stats::setNames(dg[[ncol(dg)]], dg$node), c(a = 1, b = 2, c = 1))
})

test_that("presence and value rules are mirrored for undirected data", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("b", "c"), target = c("a", "b"), w = c(1, 4))
  nd <- netis_data(nodes, ties, directed = FALSE)
  dg <- compute_centrality(nd, network = binarize(w, rule = threshold(2)), measure = "degree")
  expect_equal(stats::setNames(dg[[ncol(dg)]], dg$node), c(a = 0, b = 1, c = 1))
  pr <- netis_adjacency(nd, "w", presence = TRUE)
  expect_equal(unname(pr["a", "b"]), 1)
  expect_equal(unname(pr["b", "a"]), 1)
  expect_equal(unname(pr["a", "c"]), 0)
})

test_that("fit_selection() refuses undirected data fitted as directed", {
  nodes <- data.frame(group = 1, node = letters[1:6], x = c(1, 4, 2, 8, 5, 7))
  pairs <- subset(expand.grid(source = letters[1:6], target = letters[1:6],
                              stringsAsFactors = FALSE), source < target)
  pairs$w <- rep(c(0, 1, 1), length.out = nrow(pairs))
  nd <- netis_data(nodes, data.frame(group = 1, pairs), directed = FALSE,
                   tie_meta = list(w = list(zero_meaning = "measured_zero")))
  expect_error(fit_selection(w ~ sender(x), data = nd, nscan = 10, burn = 0, odens = 1),
               "symmetric = TRUE")
})
