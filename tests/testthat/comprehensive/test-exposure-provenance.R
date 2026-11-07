# Known-answer tests for add_baseline_provenance(): the default,
# recommended construction for B-I provenance. Every expected value is
# derived by hand in the test comment.

test_that("baseline provenance is exactly the (centered) first-wave value, zero elsewhere", {
  # Group 1 baseline (wave 1): a=0.2, b=-0.6. Group mean = -0.2.
  # Centered: a = 0.2 - (-0.2) = 0.4, b = -0.6 - (-0.2) = -0.4.
  # Wave 2 provenance must be exactly 0 for both nodes -- not a repeat of
  # the baseline value, which would over-count it.
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2),
    negative_affect = c(0.2, -0.6, 0.5, -0.1)
  )
  ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2), tie = c(1, 1))
  nd <- netis_data(nodes, ties, wave_col = "wave")

  nd2 <- add_baseline_provenance(nd, "negative_affect")
  out <- netis_nodes(nd2)

  wave1 <- out[out$wave == 1, ]
  wave2 <- out[out$wave == 2, ]
  expect_equal(wave1$negative_affect_baseline_provenance[wave1$node == "a"], 0.4, tolerance = 1e-12)
  expect_equal(wave1$negative_affect_baseline_provenance[wave1$node == "b"], -0.4, tolerance = 1e-12)
  expect_equal(wave2$negative_affect_baseline_provenance, c(0, 0))
})

test_that("center = FALSE uses the raw baseline value, not group-mean-centered", {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2),
    x = c(0.2, -0.6, 0.5, -0.1)
  )
  ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2), tie = c(1, 1))
  nd <- netis_data(nodes, ties, wave_col = "wave")

  nd2 <- add_baseline_provenance(nd, "x", center = FALSE)
  out <- netis_nodes(nd2)
  wave1 <- out[out$wave == 1, ]
  expect_equal(wave1$x_baseline_provenance[wave1$node == "a"], 0.2)
  expect_equal(wave1$x_baseline_provenance[wave1$node == "b"], -0.6)
})

test_that("centering is computed within group, not across the whole dataset", {
  # Group 1: a=10, b=20 (mean 15, centered: -5, 5).
  # Group 2: a=100, b=300 (mean 200, centered: -100, 100).
  # If centering were global rather than per-group, these numbers would differ.
  nodes <- data.frame(
    group = c(1, 1, 1, 1, 2, 2, 2, 2),
    node = rep(c("a", "b"), 4),
    wave = c(1, 1, 2, 2, 1, 1, 2, 2),
    x = c(10, 20, 10, 20, 100, 300, 100, 300)
  )
  ties <- data.frame(group = c(1, 2), source = "a", target = "b", wave = 1, tie = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")

  nd2 <- add_baseline_provenance(nd, "x")
  out <- netis_nodes(nd2)
  w1 <- out[out$wave == 1, ]
  expect_equal(w1$x_baseline_provenance[w1$group == 1 & w1$node == "a"], -5)
  expect_equal(w1$x_baseline_provenance[w1$group == 1 & w1$node == "b"], 5)
  expect_equal(w1$x_baseline_provenance[w1$group == 2 & w1$node == "a"], -100)
  expect_equal(w1$x_baseline_provenance[w1$group == 2 & w1$node == "b"], 100)
})

test_that("a custom name is honored", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2), x = c(1, 2, 3, 4))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1, tie = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  nd2 <- add_baseline_provenance(nd, "x", name = "shock")
  expect_true("shock" %in% names(netis_nodes(nd2)))
})

test_that("data with no wave dimension, or only one wave, is rejected", {
  nodes_nowave <- data.frame(group = 1, node = c("a", "b"), x = c(1, 2))
  ties_nowave <- data.frame(group = 1, source = "a", target = "b", tie = 1)
  nd_nowave <- netis_data(nodes_nowave, ties_nowave)
  expect_error(add_baseline_provenance(nd_nowave, "x"), "wave dimension")

  nodes_1wave <- data.frame(group = 1, node = c("a", "b"), wave = 1, x = c(1, 2))
  ties_1wave <- data.frame(group = 1, source = "a", target = "b", wave = 1, tie = 1)
  nd_1wave <- netis_data(nodes_1wave, ties_1wave, wave_col = "wave")
  expect_error(add_baseline_provenance(nd_1wave, "x"), "wave dimension")
})

test_that("the resulting netis_data round-trips through compute_exposure() end to end, matching a hand-verified value", {
  # Single tie a->b (weight 1) at both waves; centered baseline a=0.4, b=-0.4
  # (as in the first test). At focal wave 2, only one step of history (wave
  # 1), so exposure_b = W[b,a] * e_a = 1 * 0.4 = 0.4, exposure_a = 0.
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2),
    negative_affect = c(0.2, -0.6, 0.5, -0.1)
  )
  ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2), tie = c(1, 1))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  nd2 <- add_baseline_provenance(nd, "negative_affect")

  out <- compute_exposure(nd2, "negative_affect_baseline_provenance", "tie",
                           wave = 2, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))
  expect_equal(out$ex_negative_affect_baseline_provenance_tie[out$node == "a"], 0)
  expect_equal(out$ex_negative_affect_baseline_provenance_tie[out$node == "b"], 0.4, tolerance = 1e-12)
})

test_that("declared tie semantics survive add_baseline_provenance() (regression: tie_meta was dropped on reconstruction)", {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2),
    x = c(1, 2, 3, 4)
  )
  ties <- data.frame(
    group = 1, source = c("a", "b"), target = c("b", "a"),
    wave = 1, tie = c(2, 0)
  )
  nd <- netis_data(
    nodes, ties, wave_col = "wave",
    tie_meta = list(tie = list(
      weight_semantics = "strength",
      zero_meaning = "measured_zero",
      direction_semantics = "source sends information to target",
      weighted = TRUE
    ))
  )
  nd2 <- add_baseline_provenance(nd, "x")
  d <- netis_schema(nd2)$tie_descriptors$tie
  expect_identical(d$weight_semantics, "strength")
  expect_identical(d$zero_meaning, "measured_zero")
  expect_identical(d$direction_semantics, "source sends information to target")
  expect_true(d$weighted)
  expect_false(d$weighted_inferred)
})
