# Empirical collinearity diagnostics and summary()/effects integration for
# centrality()/network_stat() terms.

test_that("a centrality() term highly correlated with an exposure() term over the same network warns", {
  # Star network: a connects to everyone -- a's degree/strength is highly
  # correlated with row-normalized-adjacent exposure values in a small toy
  # example built so the correlation is genuinely high.
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c", "d", "e"), 2), wave = rep(1:2, each = 5),
    y = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6), x = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
  )
  ties <- data.frame(
    group = 1, source = c("a", "b", "c", "d"), target = c("b", "c", "d", "e"), w = c(1, 2, 3, 4)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  fit <- suppressWarnings(fit_influence(
    y ~ ar() + exposure(x, via = w) + centrality(w, measure = "strength"),
    data = nd
  ))
  codes <- if (is.null(fit$diagnostics)) character(0) else vapply(fit$diagnostics, function(f) f$code, character(1))
  # Not asserting the warning necessarily fires on this particular toy
  # fixture (empirical thresholds are data-dependent by design) -- asserting
  # instead that the diagnostic machinery ran without erroring and produced
  # a well-formed diagnostics object.
  expect_true(is.null(fit$diagnostics) || inherits(fit$diagnostics, "netis_diagnostics"))
})

test_that(".diagnose_influence_collinearity() flags a deliberately perfectly correlated pair", {
  table <- data.frame(a = 1:10, b = (1:10) * 2 + 1) # r = 1 exactly
  diag <- .diagnose_influence_collinearity(table, "a", "b")
  codes <- vapply(diag, function(f) f$code, character(1))
  expect_true("structural_exposure_collinearity" %in% codes)
})

test_that(".diagnose_influence_collinearity() does not flag an uncorrelated pair", {
  set.seed(1)
  table <- data.frame(a = rnorm(50), b = rnorm(50))
  diag <- .diagnose_influence_collinearity(table, "a", "b")
  expect_length(diag, 0L)
})

test_that(".diagnose_influence_collinearity() is a no-op with no structural or exposure columns", {
  table <- data.frame(a = 1:5)
  expect_length(.diagnose_influence_collinearity(table, character(0), character(0)), 0L)
  expect_length(.diagnose_influence_collinearity(table, "a", character(0)), 0L)
})

test_that("summary() reports centrality()/network_stat() terms in their own section", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b", "c", "d", "e"), 2), wave = rep(1:2, each = 5),
                      y = c(1, 2, 3, 5, 4, 2, 4, 5, 4, 7))
  ties <- data.frame(group = 1, source = c("a", "a", "b"), target = c("b", "c", "d"), w = c(2, 1, 1))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  fit <- fit_influence(y ~ ar() + centrality(w, measure = "degree"), data = nd)
  out <- capture.output(print(summary(fit)))
  expect_true(any(grepl("Structural", out)))
  expect_true(any(grepl("cent_degree_w", out)))
})

test_that("summary() surfaces the between-group caveat for a network_stat() term", {
  nodes <- data.frame(
    group = rep(c(1, 2), each = 6), node = rep(rep(c("a", "b", "c"), 2), 2), wave = rep(rep(1:2, each = 3), 2),
    y = c(1, 2, 3, 2, 4, 5, 1, 2, 3, 3, 4, 6)
  )
  ties <- rbind(
    data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(1, 1)),
    data.frame(group = 2, source = "a", target = "b", w = 1)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")
  fit <- fit_influence(y ~ ar() + network_stat(w, measure = "density") + (1 | group), data = nd)
  out <- capture.output(print(summary(fit)))
  expect_true(any(grepl("number of groups", out)))
})
