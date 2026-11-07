# The simulator's stability check and network errors use the exposure
# engine's orientation: W[i, j] is the weight with which j influences i.

# Ties 1 -> 2, 2 -> 1, 3 -> 1. Under source_influences_target, node 1 is
# influenced by 2 and 3, node 2 by 1, node 3 by nobody. Row-normalized:
#   W = [0 .5 .5; 1 0 0; 0 0 0]
# Companion 0.3 I + 0.75 W has eigenvalues 0.3 +/- sqrt(0.375 * 0.75) and 0.3,
# so its spectral radius is 0.3 + sqrt(0.28125) = 0.8303. Building W with rows
# as sources instead gives 0.3 + 0.75 = 1.05, which would flag this stable
# process as explosive.
.orient_ties <- data.frame(group = 1, source = c("1", "2", "3"),
                           target = c("2", "1", "1"), tie = 1,
                           stringsAsFactors = FALSE)

test_that("companion radius uses the canonical orientation", {
  r <- .companion_radius(.orient_ties, as.character(1:3), 1L, gamma = 0.3,
                         beta = 0.75, normalize = "row", network_change = FALSE)
  expect_lt(abs(r - (0.3 + sqrt(0.28125))), 1e-12)

  # The reverse case: 1 -> 2, 2 -> 1, 1 -> 3. Node 1 is influenced by 2 only,
  # node 2 by 1, node 3 by 1: W = [0 1 0; 1 0 0; 1 0 0], radius 0.3 + 0.75.
  rev <- data.frame(group = 1, source = c("1", "2", "1"), target = c("2", "1", "3"),
                    tie = 1, stringsAsFactors = FALSE)
  r2 <- .companion_radius(rev, as.character(1:3), 1L, 0.3, 0.75, "row", FALSE)
  expect_lt(abs(r2 - 1.05), 1e-12)
})

test_that("the simulator's W is the matrix the exposure engine applies", {
  ids <- as.character(1:3)
  W <- .sim_canonical_W(.orient_ties, ids, directed = TRUE, normalize = "row")
  x <- c(10, 100, 1000)
  nd <- netis_data(data.frame(group = 1, node = ids, x = x),
                   .orient_ties[, c("group", "source", "target", "tie")])
  ex <- compute_exposure(nd, content = "x", via = "tie", normalize = "row",
                         isolate_rule = "zero")
  ex <- ex$ex_x_tie[match(ids, ex$node)]
  expect_equal(as.numeric(W %*% x), ex)
  expect_equal(ex, c(550, 10, 0))
})

test_that("an undirected tie influences both endpoints in the radius", {
  # Path 1 - 2 stored once. W = [0 1; 1 0]; radius of 0.3 I + 0.5 W is 0.8.
  # Treated as one-way the matrix is triangular and the radius is |gamma|.
  ties <- data.frame(group = 1, source = "1", target = "2", tie = 1,
                     stringsAsFactors = FALSE)
  r <- .companion_radius(ties, c("1", "2"), 1L, 0.3, 0.5, "none", FALSE,
                         directed = FALSE)
  expect_lt(abs(r - 0.8), 1e-12)
})

test_that("network errors are (I - rho W)^-1 u with the canonical W", {
  ids <- as.character(1:3)
  set.seed(7)
  eps <- .simulate_errors(n_nodes = 3, group_ids = 1L, resid_sd = 1,
                          error_structure = "network", error_rho = 0.5,
                          ties = .orient_ties, ids = ids, wave = 1L,
                          network_change = FALSE)
  set.seed(7)
  u <- stats::rnorm(3)
  W <- rbind(c(0, 0.5, 0.5), c(1, 0, 0), c(0, 0, 0))
  expect_lt(max(abs(eps - solve(diag(3) - 0.5 * W, u))), 1e-12)
})
