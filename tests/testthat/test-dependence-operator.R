# The operator netis_dependence() reads residuals against is the one the fit
# applied. Row normalization divides by a row sum, and that sum depends on which
# actors the matrix holds when it is divided. An actor outside the estimating sample
# still counts in its alters' denominators on the pooled and mixed routes, which
# compute exposure before the sample is known, and does not on the simultaneous
# route, which normalizes W1 over the actors it retains.

.dop_ids <- sprintf("n%02d", 1:12)

# W[i, j] = "j influences i", with sources influencing targets: a tie source -> target
# is a one in row target, column source. Row sums run from one to three.
.dop_edges <- rbind(
  c("n02", "n01"), c("n12", "n01"),
  c("n03", "n02"), c("n12", "n02"), c("n01", "n02"),
  c("n04", "n03"),
  c("n05", "n04"), c("n01", "n04"),
  c("n12", "n05"),
  c("n07", "n06"), c("n12", "n06"),
  c("n08", "n07"),
  c("n09", "n08"), c("n03", "n08"), c("n12", "n08"),
  c("n10", "n09"),
  c("n11", "n10"), c("n02", "n10"),
  c("n01", "n11"), c("n12", "n11"),
  c("n06", "n12")
)

.dop_raw <- function(ids, edges = .dop_edges) {
  raw <- matrix(0, length(ids), length(ids), dimnames = list(ids, ids))
  for (k in seq_len(nrow(edges))) {
    if (all(edges[k, ] %in% ids)) raw[edges[k, 2], edges[k, 1]] <- 1
  }
  raw
}

.dop_nodes <- function() {
  set.seed(101)
  data.frame(group = 1, node = rep(.dop_ids, 2), wave = rep(1:2, each = 12),
             y = stats::rnorm(24), x = stats::rnorm(24), stringsAsFactors = FALSE)
}

.dop_ties <- function(edges = .dop_edges, waves = 1:2) {
  do.call(rbind, lapply(waves, function(w) {
    data.frame(group = 1, source = edges[, 1], target = edges[, 2], wave = w, tie = 1,
               stringsAsFactors = FALSE)
  }))
}

# `n12` has no outcome at wave 2, so it has no residual; its ties reach six actors.
.dop_data <- function() {
  nodes <- .dop_nodes()
  nodes$y[nodes$node == "n12" & nodes$wave == 2] <- NA
  netis_data(nodes, .dop_ties(), wave_col = "wave")
}

.dop_fit <- function(nd, isolate_rule = "zero") {
  suppressWarnings(fit_influence(
    stats::as.formula(sprintf(
      'y ~ ar() + exposure(x, via = tie, normalize = "row", isolate_rule = "%s")',
      isolate_rule
    )),
    data = nd
  ))
}

# The statistic by hand, from the residuals and a weight matrix given in full.
.dop_moran <- function(r, W) {
  z <- r - mean(r)
  (length(z) / sum(W)) * as.numeric(t(z) %*% W %*% z) / sum(z^2)
}

.dop_kept <- function(fit) {
  as.character(fit$model_frame$node[as.integer(names(fit$residuals))])
}

test_that("a dropped actor's ties stay in the row sums the statistic normalizes by", {
  nd <- .dop_data()
  fit <- .dop_fit(nd)
  term <- netis_dependence(fit, nd)$network_autocorrelation$per_term[[1]]
  expect_identical(term$status, "computed")

  raw <- .dop_raw(.dop_ids)
  W_fit <- raw / rowSums(raw)

  # That this IS the fit's operator, read off the fit's own exposure column.
  tbl <- fit$model_frame
  x1 <- stats::setNames(.dop_nodes()$x[1:12], .dop_ids)
  expect_equal(tbl[[names(fit$resolved_exposures)]],
               as.numeric(W_fit %*% x1)[match(tbl$node, .dop_ids)])

  r <- as.numeric(fit$residuals)
  keep <- .dop_kept(fit)
  expect_false("n12" %in% keep)
  expect_equal(term$median, .dop_moran(r, W_fit[keep, keep]))

  # And the fixture can tell the two apart: normalizing the retained actors alone
  # gives another number.
  survivors <- raw[keep, keep]
  W_wrong <- survivors / ifelse(rowSums(survivors) == 0, 1, rowSums(survivors))
  expect_gt(abs(term$median - .dop_moran(r, W_wrong)), 1e-3)
})

test_that("isolate_rule = 'error' does not refuse an actor the fit accepted", {
  # `n05`'s only tie comes from `n12`, whose outcome is missing. The fit has a
  # denominator for `n05`, so the diagnostic must not take it for an isolate.
  nd <- .dop_data()
  fit <- suppressWarnings(fit_influence(
    y ~ ar() + exposure(x, via = tie, normalize = "row", isolate_rule = "error"),
    data = nd
  ))
  term <- netis_dependence(fit, nd)$network_autocorrelation$per_term[[1]]
  expect_identical(term$status, "computed")
})

test_that("an actor with no row at the outcome wave still counts in the row sums", {
  # `n12` is observed at the content wave and has no row at all at the outcome wave,
  # so the estimating table never holds it. The exposure is resolved at the content
  # wave, where it is an ordinary alter.
  nodes <- .dop_nodes()
  nodes <- nodes[!(nodes$node == "n12" & nodes$wave == 2), ]
  nd <- netis_data(nodes, .dop_ties(), wave_col = "wave")
  fit <- .dop_fit(nd)
  term <- netis_dependence(fit, nd)$network_autocorrelation$per_term[[1]]

  raw <- .dop_raw(.dop_ids)
  W_fit <- raw / rowSums(raw)
  keep <- .dop_kept(fit)
  expect_false("n12" %in% keep)
  expect_equal(term$median, .dop_moran(as.numeric(fit$residuals), W_fit[keep, keep]))
})

test_that("on the simultaneous route the operator is normalized over the fitted actors", {
  # lnam() normalizes W1 after removing incomplete rows, so there the fitted
  # operator IS the retained actors' one, and the statistic reads that.
  nd <- .dop_data()
  fit <- suppressWarnings(fit_influence(
    y ~ exposure(y, via = tie, x_lag = 0, normalize = "row", isolate_rule = "zero") + x,
    data = nd
  ))
  expect_identical(fit$backend, "lnam")
  term <- netis_dependence(fit, nd)$network_autocorrelation$per_term[[1]]
  expect_identical(term$status, "computed")

  keep <- .dop_kept(fit)
  expect_false("n12" %in% keep)
  raw <- .dop_raw(sort(keep))
  W <- raw / ifelse(rowSums(raw) == 0, 1, rowSums(raw))
  expect_equal(term$median, .dop_moran(as.numeric(fit$residuals), W[keep, keep]))
})

test_that("the data argument is required", {
  nd <- .dop_data()
  fit <- .dop_fit(nd)
  # `nd` is the symbol in the fit's call and is in scope here, and the call still
  # refuses rather than reading it.
  expect_error(netis_dependence(fit), "needs the netis_data the fit was built from")
  expect_error(netis_dependence(fit, NULL), "netis_data")
  expect_false("env" %in% names(formals(netis_dependence)))
})

test_that("a history mechanism's operator can be rebuilt", {
  # bi() is resolved one wave later than its content, with every earlier wave's ties
  # in use; the operator is still built at the term's own lag.
  nodes <- do.call(rbind, lapply(1:3, function(w) {
    n <- .dop_nodes()[1:12, ]
    n$wave <- w
    n$y <- n$y + w
    n
  }))
  nd <- netis_data(nodes, .dop_ties(waves = 1:3), wave_col = "wave")
  fit <- suppressWarnings(fit_influence(
    y ~ ar() + exposure(x, via = tie, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0)),
    data = nd, waves = c(2, 3)
  ))
  term <- netis_dependence(fit, nd)$network_autocorrelation$per_term[[1]]
  expect_identical(term$status, "computed")
})

test_that("printing names a skipped snapshot on data with no wave dimension", {
  # A short chain: under the floor of ten non-isolated actors, with no wave to name.
  set.seed(4)
  nodes <- data.frame(group = 1, node = letters[1:6], y = stats::rnorm(6),
                      x = stats::rnorm(6), stringsAsFactors = FALSE)
  ties <- data.frame(group = 1, source = letters[1:5], target = letters[2:6], tie = 1,
                     stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties)
  fit <- suppressWarnings(fit_influence(y ~ x + exposure(x, via = tie, x_lag = 0), data = nd))
  dep <- netis_dependence(fit, nd)
  out <- format(dep)
  expect_true(any(grepl("group 1 (only 5 non-isolated actors", out, fixed = TRUE)))
})

test_that("row normalization does not confine the statistic to [-1, 1]", {
  # Two stars, each leaf reading only its hub. Every row sums to one, and residuals
  # of opposite sign on the two stars are centered, with the statistic above one.
  star <- function(hub, leaves, W) {
    W[leaves, hub] <- 1
    W[hub, leaves[1]] <- 1
    W
  }
  W <- matrix(0, 18, 18)
  W <- star(1, 2:9, W)
  W <- star(10, 11:18, W)
  expect_equal(rowSums(W), rep(1, 18))
  ev <- eigen((W + t(W)) / 2, symmetric = TRUE)
  P <- diag(18) - 1 / 18
  top <- eigen(P %*% ((W + t(W)) / 2) %*% P, symmetric = TRUE)$vectors[, 1]
  expect_gt(.moran_cross_product(top - mean(top), W), 1)
  # And the printed record says so for a row-normalized term.
  nd <- .dop_data()
  out <- format(netis_dependence(.dop_fit(nd), nd))
  expect_true(any(grepl("not confined to [-1, 1]", out, fixed = TRUE)))
})
