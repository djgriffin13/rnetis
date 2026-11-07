# B-I paths through a node that is not on the focal-wave roster, the
# parameter domain of bi(), and add_baseline_provenance() on unbalanced panels.

.bi_roster_nd <- function(c_at_wave3 = TRUE) {
  # a -> c at wave 1 (weight 2), c -> b at wave 2 (weight 3); a shocks 10 at
  # wave 1. Non-unit weights so a product, a minimum and a maximum differ.
  waves <- list(`1` = c("a", "b", "c"), `2` = c("a", "b", "c"),
                `3` = if (c_at_wave3) c("a", "b", "c") else c("a", "b"))
  nodes <- do.call(rbind, lapply(names(waves), function(w) {
    data.frame(group = 1, node = waves[[w]], wave = as.integer(w),
               shock = ifelse(waves[[w]] == "a" & w == "1", 10, 0))
  }))
  ties <- data.frame(group = 1, source = c("a", "c"), target = c("c", "b"),
                     wave = c(1L, 2L), link = c(2, 3))
  netis_data(nodes, ties, wave_col = "wave")
}

test_that("a B-I path through a node absent at the focal wave still counts (known answer)", {
  # Path a -(2)-> c -(3)-> b, alpha = 1, gamma = 0: b receives 10 * 2 * 3 = 60.
  full <- compute_exposure(.bi_roster_nd(TRUE), "shock", "link", wave = 3,
                           mechanism = bi(delta = 0, alpha = 1, gamma = 0))
  expect_equal(full$ex_shock_link[full$node == "b"], 60)
  gone <- compute_exposure(.bi_roster_nd(FALSE), "shock", "link", wave = 3,
                           mechanism = bi(delta = 0, alpha = 1, gamma = 0))
  # c is not reported (it has no row at wave 3), and b keeps its 60.
  expect_setequal(gone$node, c("a", "b"))
  expect_equal(gone$ex_shock_link[gone$node == "b"], 60)
  expect_equal(gone$ex_shock_link[gone$node == "a"], 0)
})

test_that("bi() and bi_components() refuse negative or infinite path factors", {
  expect_error(bi(delta = 0, alpha = -0.5, gamma = 0), "nonnegative")
  expect_error(bi(delta = 0, alpha = 1, gamma = -1), "nonnegative")
  expect_error(bi(delta = 0, alpha = Inf, gamma = 0), "finite")
  expect_error(bi_components(alpha = -0.5, gamma = 0), "nonnegative")
  expect_error(bi_components(alpha = 1, gamma = NA_real_), "non-missing")
  expect_true(is.function(bi(delta = 0, alpha = 0, gamma = 0)))
})

test_that("add_baseline_provenance() uses each group's first wave and adds no rows", {
  # Group 1 observed at waves 1-3; group 2 only at waves 2-3; node "z" joins
  # group 1 at wave 2. 12 node rows in, 12 out.
  nodes <- rbind(
    data.frame(group = 1, node = rep(c("a", "b"), 3), wave = rep(1:3, each = 2),
               v = c(1, 3, 5, 5, 5, 5)),
    data.frame(group = 1, node = "z", wave = 2:3, v = c(9, 9)),
    data.frame(group = 2, node = rep(c("p", "q"), 2), wave = rep(2:3, each = 2),
               v = c(2, 6, 0, 0))
  )
  ties <- data.frame(group = c(1, 2), source = c("a", "p"), target = c("b", "q"),
                     wave = c(1, 2), w = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  out <- netis_nodes(add_baseline_provenance(nd, "v"))
  expect_equal(nrow(out), nrow(nodes))
  val <- function(g, n, w) out$v_baseline_provenance[out$group == g & out$node == n & out$wave == w]
  # Group 1 baseline is wave 1: a, b centered on their mean 2.
  expect_equal(val(1, "a", 1), -1)
  expect_equal(val(1, "b", 1), 1)
  expect_equal(val(1, "a", 2), 0)
  # z has no baseline row: 0 throughout, not NA, and no row at wave 1.
  expect_equal(val(1, "z", 2), 0)
  expect_length(val(1, "z", 1), 0L)
  # Group 2 baseline is its own first wave, 2: p, q centered on 4.
  expect_equal(val(2, "p", 2), -2)
  expect_equal(val(2, "q", 2), 2)
  expect_equal(val(2, "q", 3), 0)
  expect_length(val(2, "p", 1), 0L)
})
