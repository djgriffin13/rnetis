# Distributional summaries of a node-level network feature: network_stat(x,
# measure = "degree", summary = "mean") etc.

.summary_fixture <- function() {
  nodes <- data.frame(group = 1, node = c("a", "b", "c", "d"))
  ties <- data.frame(group = 1, source = c("a", "b", "c"), target = c("b", "c", "d"), w = c(1, 1, 1))
  netis_data(nodes, ties)
}

test_that("network_stat(measure, summary = 'mean') equals the mean of the corresponding centrality() column", {
  nd <- .summary_fixture()
  deg <- compute_centrality(nd, w, measure = "degree", mode = "all")
  mean_deg <- compute_network_stat(nd, w, measure = "degree", summary = "mean")
  expect_equal(mean_deg$net_degree_w, mean(deg$cent_degree_w))
})

test_that("every documented summary computes the correct hand statistic", {
  nd <- .summary_fixture()
  deg <- compute_centrality(nd, w, measure = "degree", mode = "all")$cent_degree_w

  expect_equal(compute_network_stat(nd, w, measure = "degree", summary = "sd")$net_degree_w, stats::sd(deg))
  expect_equal(compute_network_stat(nd, w, measure = "degree", summary = "var")$net_degree_w, stats::var(deg))
  expect_equal(compute_network_stat(nd, w, measure = "degree", summary = "median")$net_degree_w, stats::median(deg))
  expect_equal(compute_network_stat(nd, w, measure = "degree", summary = "min")$net_degree_w, min(deg))
  expect_equal(compute_network_stat(nd, w, measure = "degree", summary = "max")$net_degree_w, max(deg))
  expect_equal(compute_network_stat(nd, w, measure = "degree", summary = "prop_zero")$net_degree_w, mean(deg == 0))
})

test_that("prop_zero is exactly proportion of isolates for measure = 'degree'", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c", "d")) # d is an isolate
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  nd <- netis_data(nodes, ties)
  prop <- compute_network_stat(nd, w, measure = "degree", summary = "prop_zero")
  expect_equal(prop$net_degree_w, 0.5) # c and d both have degree 0
})

test_that("summary = is rejected for an already group-level measure", {
  expect_error(network_stat(w, measure = "density", summary = "mean"), "group-level")
})

test_that("a node-level measure without summary = is rejected", {
  expect_error(network_stat(w, measure = "degree"), "node-level")
})

test_that("an invalid summary name is rejected", {
  expect_error(network_stat(w, measure = "degree", summary = "bogus"))
})

test_that("fit_influence()'s network_stat(summary=) matches compute_network_stat() (shared engine)", {
  nodes <- data.frame(
    group = rep(c(1, 2), each = 8), node = rep(rep(c("a", "b", "c", "d"), 2), 2), wave = rep(rep(1:2, each = 4), 2),
    y = c(1, 2, 3, 4, 2, 3, 4, 5, 1, 2, 3, 4, 2, 3, 3, 5)
  )
  ties <- rbind(
    data.frame(group = 1, source = c("a", "b", "c"), target = c("b", "c", "d"), w = c(1, 1, 1)),
    data.frame(group = 2, source = c("a", "b"), target = c("b", "c"), w = c(1, 1))
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  fit <- suppressWarnings(fit_influence(
    y ~ ar() + network_stat(w, measure = "degree", summary = "mean") + (1 | group),
    data = nd
  ))
  standalone <- compute_network_stat(nd, w, measure = "degree", summary = "mean", wave = 1)
  merged <- merge(fit$model_frame, standalone, by = "group", suffixes = c("_fit", "_standalone"))
  expect_equal(merged$net_degree_w_fit, merged$net_degree_w_standalone)
})
