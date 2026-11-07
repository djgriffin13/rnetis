# How latent() builds the network it hands to the stage-one selection model,
# and how the positions come back. The sampler itself is replaced by a stub
# that records what it was given, so each property is checked on the
# resolver's own output rather than through a stochastic fit.

.lat_nd <- function(directed = TRUE) {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c", "d"), 2), wave = rep(1:2, each = 4),
    y = c(1, 3, 2, 5, 2, 4, 1, 6), stringsAsFactors = FALSE
  )
  # Asymmetric: a sends three ties and receives none; one valued tie.
  ties <- data.frame(group = 1, source = c("a", "a", "a"), target = c("b", "c", "d"),
                     wave = 1, f = c(1, 2, 1))
  ties <- rbind(ties, transform(ties, wave = 2))
  netis_data(nodes, ties, wave_col = "wave", directed = directed)
}

.capture_stub <- function(env) {
  function(term, adjacency, node_attrs, directed = TRUE) {
    env$adj <- adjacency
    env$directed <- directed
    env$draw <- stats::runif(1)
    Z <- matrix(seq_len(nrow(adjacency) * term$d), ncol = term$d)
    rownames(Z) <- rownames(adjacency)
    list(positions = Z, formula = "stub", n_actors = nrow(adjacency))
  }
}

test_that("the selection model is handed the declared network, not its transpose", {
  env <- new.env()
  local_mocked_bindings(.latent_fit_positions = .capture_stub(env))
  nd <- .lat_nd()
  .resolve_latent_positions(latent(f, d = 1, seed = 1), nd, netis_schema(nd), 1)
  # a -> b, a -> c, a -> d: row a is the sender. Valued tie reduced to presence.
  expect_equal(unname(env$adj["a", c("b", "c", "d")]), c(1, 1, 1))
  expect_equal(unname(env$adj[c("b", "c", "d"), "a"]), c(0, 0, 0))
  expect_true(env$directed)
})

test_that("an undirected network reaches the selection model symmetric and undirected", {
  env <- new.env()
  local_mocked_bindings(.latent_fit_positions = .capture_stub(env))
  nd <- .lat_nd(directed = FALSE)
  .resolve_latent_positions(latent(f, d = 1, seed = 1), nd, netis_schema(nd), 1)
  expect_equal(unname(env$adj["b", "a"]), 1)
  expect_equal(unname(env$adj["a", "b"]), 1)
  expect_true(isSymmetric(unname(env$adj)))
  expect_false(env$directed)
})

test_that("a derived network resolves for latent(), as the documentation says", {
  env <- new.env()
  local_mocked_bindings(.latent_fit_positions = .capture_stub(env))
  nd <- .lat_nd()
  term <- latent(binarize(f, rule = threshold(1)), d = 1, seed = 1)
  .resolve_latent_positions(term, nd, netis_schema(nd), 1)
  # Only a -> c has a value above 1.
  expect_equal(unname(env$adj["a", c("b", "c", "d")]), c(0, 1, 0))
})

test_that("the stage-one seed is used, recorded, and leaves the caller's stream alone", {
  env <- new.env()
  local_mocked_bindings(.latent_fit_positions = .capture_stub(env))
  nd <- .lat_nd()
  s <- netis_schema(nd)

  set.seed(99)
  before <- .Random.seed
  r1 <- .resolve_latent_positions(latent(f, seed = 123), nd, s, 1)
  d1 <- env$draw
  expect_identical(.Random.seed, before)
  r2 <- .resolve_latent_positions(latent(f, seed = 123), nd, s, 1)
  expect_identical(env$draw, d1)
  expect_identical(r1$seed, 123L)
  set.seed(123)
  expect_identical(d1, stats::runif(1))

  # seed = NULL: one integer is drawn from the caller's stream and recorded.
  set.seed(7)
  expected_seed <- sample.int(.Machine$integer.max, 1L)
  after_one_draw <- .Random.seed
  set.seed(7)
  r3 <- .resolve_latent_positions(latent(f), nd, s, 1)
  expect_identical(r3$seed, expected_seed)
  expect_identical(.Random.seed, after_one_draw)
})

test_that("latent() validates seed, d and lag instead of truncating them", {
  expect_error(latent(f, seed = 1.5), "seed")
  expect_error(latent(f, d = 2.7), "positive integer")
  expect_error(latent(f, lag = 0.5), "whole number")
})

test_that("multi-group positions are centered per group, so a translation cannot move the fit", {
  sim <- simulate_influence(n_nodes = 25, n_groups = 4, n_waves = 2, normalize = "row",
                            beta = 0.3, seed = 3, group_sd = 0.5, density = 0.2)
  stub <- function(shift) {
    force(shift)
    function(term, adjacency, node_attrs, directed = TRUE) {
      ids <- rownames(adjacency)
      Z <- matrix(cos(seq_along(ids) * 1.7 + nrow(adjacency)), ncol = 1)
      rownames(Z) <- ids
      # A different translation for every group: an equally valid solution.
      Z <- Z + shift * sum(adjacency)
      list(positions = Z, formula = "stub", n_actors = length(ids))
    }
  }
  fit_with <- function(shift) {
    local_mocked_bindings(.latent_fit_positions = stub(shift))
    suppressMessages(suppressWarnings(fit_influence(
      y ~ ar() + exposure(y, via = tie, normalize = "row") + latent(tie, d = 1, seed = 1) +
        (1 | group),
      sim$data
    )))
  }
  f0 <- fit_with(0)
  f1 <- fit_with(0.3)
  expect_lt(abs(coef(f0)[["ex_y_tie"]] - coef(f1)[["ex_y_tie"]]), 1e-8)
  expect_lt(abs(coef(f0)[["y_lag1"]] - coef(f1)[["y_lag1"]]), 1e-8)
  # Each group's own column has mean zero over that group's actors.
  mf <- f0$model_frame
  for (g in unique(as.character(mf$group))) {
    col <- sprintf("lat_tie_1__%s", g)
    pos <- unique(mf[as.character(mf$group) == g, c("node", col)])
    expect_lt(abs(mean(pos[[col]])), 1e-10)
  }
  expect_identical(f0$resolved_latent[["lat_tie_1"]]$seed, 1L)
})

test_that("a node variable named in terms = reaches the selection model", {
  # `terms` is documented as passed through verbatim, nodecov('x') included. A
  # term can only read a vertex attribute the network carries.
  seen <- new.env()
  local_mocked_bindings(.latent_fit_positions = function(term, adjacency, node_attrs,
                                                         directed = TRUE) {
    seen$attrs <- node_attrs
    Z <- matrix(seq_len(nrow(adjacency)), ncol = 1L)
    rownames(Z) <- rownames(adjacency)
    list(positions = Z, formula = "stub", n_actors = nrow(adjacency))
  })
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c", "d"), 2), wave = rep(1:2, each = 4),
    y = c(1, 3, 2, 5, 2, 4, 1, 6), x = c(9, 8, 7, 6, 5, 4, 3, 2), unused = 1:8,
    stringsAsFactors = FALSE
  )
  ties <- data.frame(group = 1, source = c("a", "a", "a"), target = c("b", "c", "d"),
                     wave = 1, f = 1)
  nd <- netis_data(nodes, rbind(ties, transform(ties, wave = 2)), wave_col = "wave")

  term <- latent(f, d = 1, homophily = "y", terms = "nodecov('x')", seed = 1)
  .resolve_latent_positions(term, nd, netis_schema(nd), 1)
  expect_setequal(names(seen$attrs), c("y", "x"))
  expect_equal(seen$attrs$x, c(9, 8, 7, 6))

  # Double quotes, and a term naming no node variable, read the same way.
  .resolve_latent_positions(latent(f, d = 1, terms = c('nodeocov("x")', "edges"), seed = 1),
                            nd, netis_schema(nd), 1)
  expect_identical(names(seen$attrs), "x")
  .resolve_latent_positions(latent(f, d = 1, terms = "edges", seed = 1), nd, netis_schema(nd), 1)
  expect_length(seen$attrs, 0L)
})
