# `nobs` and "rows in the estimating sample" count what the backend fitted,
# not what was assembled. Each fixture drops a known number of rows or dyads
# and asserts the exact count.

.nobs_influence_data <- function(n = 12, na_x_at = c(2L, 5L, 9L), seed = 4) {
  set.seed(seed)
  ids <- sprintf("v%02d", c(7, 3, 11, 1, 9, 5, 12, 2, 10, 4, 8, 6)[seq_len(n)])
  nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = n),
                      y = stats::rnorm(2 * n), x = stats::rnorm(2 * n),
                      stringsAsFactors = FALSE)
  # Missing x at the focal wave only, so exactly these rows are dropped.
  nodes$x[n + na_x_at] <- NA
  ties <- data.frame(group = 1, source = ids[-n], target = ids[-1], tie = 1,
                     stringsAsFactors = FALSE)
  netis_data(nodes, rbind(cbind(ties, wave = 1), cbind(ties, wave = 2)),
             wave_col = "wave",
             tie_meta = list(tie = list(zero_meaning = "measured_zero")))
}

test_that("an OLS fit reports the rows lm() used", {
  nd <- .nobs_influence_data()
  fit <- fit_influence(y ~ ar() + x, data = nd)
  expect_identical(nrow(fit$model_frame), 12L)
  expect_identical(netis_glance(fit)$nobs, 9L)
  out <- paste(utils::capture.output(print(fit)), collapse = "\n")
  expect_match(out, "9 rows in the estimating sample (3 of 12 assembled rows dropped as incomplete)",
               fixed = TRUE)
})

test_that("an lnam fit reports the rows lnam used", {
  skip_if_not_installed("sna")
  nd <- .nobs_influence_data()
  fit <- fit_influence(y ~ x + exposure(y, via = tie, x_lag = 0), data = nd)
  expect_identical(netis_glance(fit)$nobs, 9L)
  expect_identical(length(fit$dropped_nodes), 3L)
  expect_identical(attr(logLik(fit), "nobs"), 9L)
  out <- paste(utils::capture.output(print(fit)), collapse = "\n")
  expect_match(out, "9 rows in the estimating sample", fixed = TRUE)
})

.nobs_selection_data <- function(directed = TRUE, n_na = 0L) {
  set.seed(3)
  ids <- sprintf("n%02d", 1:8)
  nodes <- data.frame(group = 1, node = ids, x = stats::rnorm(8))
  if (directed) {
    p <- subset(expand.grid(source = ids, target = ids, stringsAsFactors = FALSE),
                source != target)
  } else {
    pr <- t(utils::combn(ids, 2))
    p <- data.frame(source = pr[, 1], target = pr[, 2], stringsAsFactors = FALSE)
  }
  ties <- data.frame(group = 1, p, tie = stats::rbinom(nrow(p), 1, 0.4))
  if (n_na > 0L) ties$tie[seq_len(n_na)] <- NA
  netis_data(nodes, ties, directed = directed,
             tie_meta = list(tie = list(zero_meaning = "measured_zero")))
}

test_that("a directed Selection fit counts observed dyads, not the roster", {
  skip_if_not_installed("amen")
  nd <- .nobs_selection_data(directed = TRUE, n_na = 5L)
  fit <- suppressWarnings(fit_selection(tie ~ sender(x), data = nd, nscan = 100,
                                        burn = 20, odens = 5, seed = 1, R = 0))
  # 8 * 7 ordered pairs, five of them missing.
  expect_identical(netis_glance(fit)$nobs, 51L)
})

test_that("a symmetric Selection fit counts each unordered pair once", {
  skip_if_not_installed("amen")
  nd <- .nobs_selection_data(directed = FALSE, n_na = 3L)
  fit <- suppressWarnings(fit_selection(tie ~ sender(x), data = nd, symmetric = TRUE,
                                        nscan = 100, burn = 20, odens = 5, seed = 1, R = 0))
  # choose(8, 2) = 28 unordered pairs, three of them missing.
  expect_identical(netis_glance(fit)$nobs, 25L)
})
