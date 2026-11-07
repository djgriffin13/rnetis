# The exposure() options of a same-outcome contemporaneous (SAR) term.
#
# The term is fitted as rho * W1 y by sna::lnam(). Every option that describes
# W1 -- direction, w_lag, normalize, isolate_rule, a derived conduit -- must
# change the matrix lnam receives, and the reference fit is sna::lnam() on a
# W1 built by hand from the raw tie list, not from anything the package
# assembled. The network is DIRECTED and the node ids are UNSORTED, so a
# transposed, reordered or unnormalized W1 gives a different rho.

.sar_opt_fixture <- function(seed = 3, n = 40) {
  set.seed(seed)
  ids <- sprintf("n%02d", sample(n))
  A1 <- matrix(stats::rbinom(n * n, 1, 0.08), n, n); diag(A1) <- 0
  A2 <- matrix(stats::rbinom(n * n, 1, 0.08), n, n); diag(A2) <- 0
  # Two isolates in the wave-2 network (no ties sent or received), so the
  # source-influences-target matrix t(A2) has two all-zero rows.
  A2[1:2, ] <- 0; A2[, 1:2] <- 0
  dimnames(A1) <- dimnames(A2) <- list(ids, ids)
  # source influences target: W[i, j] = A[j, i]
  W2 <- t(A2)
  x <- stats::rnorm(n)
  y <- as.numeric(solve(diag(n) - 0.15 * W2, 0.5 + 0.8 * x + stats::rnorm(n)))
  to_ties <- function(A, wave) {
    i <- which(A == 1, arr.ind = TRUE)
    data.frame(group = "g", source = ids[i[, 1]], target = ids[i[, 2]],
               wave = wave, tie = 1, stringsAsFactors = FALSE)
  }
  nodes <- data.frame(group = "g", node = rep(ids, 2), wave = rep(1:2, each = n),
                      y = c(stats::rnorm(n), y), x = c(stats::rnorm(n), x),
                      stringsAsFactors = FALSE)
  list(
    data = netis_data(nodes, rbind(to_ties(A1, 1), to_ties(A2, 2)), wave_col = "wave",
                      tie_meta = list(tie = list(zero_meaning = "measured_zero"))),
    A1 = A1, A2 = A2, ids = ids, y = y, x = x, nodes = nodes
  )
}

.sar_ref_rho <- function(y, x, W) {
  unname(sna::lnam(y = y, x = cbind(1, x), W1 = W)$rho1)
}

test_that("normalize = 'row' row-normalizes W1, with isolates at zero", {
  skip_if_not_installed("sna")
  d <- .sar_opt_fixture()
  W <- t(d$A2)
  rs <- rowSums(W)
  Wr <- W / ifelse(rs == 0, 1, rs)
  expect_gte(sum(rs == 0), 2L)

  fit <- fit_influence(
    y ~ x + exposure(y, via = tie, x_lag = 0, normalize = "row", isolate_rule = "zero"),
    data = d$data
  )
  raw <- fit_influence(y ~ x + exposure(y, via = tie, x_lag = 0), data = d$data)
  expect_lt(abs(fit$rho - .sar_ref_rho(d$y, d$x, Wr)), 1e-6)
  expect_lt(abs(raw$rho - .sar_ref_rho(d$y, d$x, W)), 1e-6)
  # The two must actually differ on this fixture, or the test proves nothing.
  expect_gt(abs(fit$rho - raw$rho), 0.05)
})

test_that("normalize = 'row' with isolate_rule = 'na' or 'error' refuses when isolates exist", {
  skip_if_not_installed("sna")
  d <- .sar_opt_fixture()
  expect_error(
    fit_influence(y ~ x + exposure(y, via = tie, x_lag = 0, normalize = "row"),
                  data = d$data),
    "isolate_rule = \"zero\""
  )
  expect_error(
    fit_influence(y ~ x + exposure(y, via = tie, x_lag = 0, normalize = "row",
                                   isolate_rule = "error"),
                  data = d$data),
    "isolates"
  )
})

test_that("w_lag = 1 reads W1 from the prior wave", {
  skip_if_not_installed("sna")
  d <- .sar_opt_fixture()
  fit <- fit_influence(y ~ x + exposure(y, via = tie, x_lag = 0, w_lag = 1), data = d$data)
  expect_lt(abs(fit$rho - .sar_ref_rho(d$y, d$x, t(d$A1))), 1e-6)
  expect_gt(abs(fit$rho - .sar_ref_rho(d$y, d$x, t(d$A2))), 0.01)
  expect_identical(as.character(fit$resolved_exposures$ex_y_tie$conduit_wave), "1")
})

test_that("a derived conduit is resolved for the SAR term", {
  skip_if_not_installed("sna")
  d <- .sar_opt_fixture()
  fit <- fit_influence(y ~ x + exposure(y, via = binarize(tie), x_lag = 0), data = d$data)
  expect_lt(abs(fit$rho - .sar_ref_rho(d$y, d$x, t(d$A2))), 1e-6)
})

test_that("a non-linear mechanism on the SAR term is refused, not ignored", {
  skip_if_not_installed("sna")
  d <- .sar_opt_fixture()
  kernel <- function(context, spec) as.numeric(context$W %*% context$X)^2
  expect_error(
    fit_influence(y ~ x + exposure(y, via = tie, x_lag = 0, mechanism = kernel), data = d$data),
    "Only mechanism = \"linear\" is defined"
  )
  # A history mechanism has no reading at x_lag = 0 on any route.
  expect_error(
    fit_influence(y ~ x + exposure(y, via = tie, x_lag = 0,
                                   mechanism = bi(delta = 0.5, alpha = 1, gamma = 0)),
                  data = d$data),
    "x_lag = 0 has no reading"
  )
})

test_that("normalization is applied over the actors the fit retains", {
  skip_if_not_installed("sna")
  d <- .sar_opt_fixture()
  # Drop an actor with ties: its wave-2 x is missing.
  drop_id <- d$ids[which(rowSums(d$A2) > 0)[1]]
  nodes <- d$nodes
  nodes$x[nodes$wave == 2 & nodes$node == drop_id] <- NA
  nd <- netis_data(nodes, netis_ties(d$data), wave_col = "wave",
                   tie_meta = list(tie = list(zero_meaning = "measured_zero")))
  fit <- fit_influence(
    y ~ x + exposure(y, via = tie, x_lag = 0, normalize = "row", isolate_rule = "zero"),
    data = nd
  )
  keep <- setdiff(d$ids, drop_id)
  W <- t(d$A2)[keep, keep]
  rs <- rowSums(W)
  Wr <- W / ifelse(rs == 0, 1, rs)
  expect_lt(abs(fit$rho - .sar_ref_rho(d$y[d$ids %in% keep], d$x[d$ids %in% keep], Wr)), 1e-6)
  expect_identical(fit$dropped_nodes, drop_id)
})

test_that("moderation is refused on the SAR route", {
  skip_if_not_installed("sna")
  d <- .sar_opt_fixture()
  nodes <- d$nodes
  nodes$z <- seq_len(nrow(nodes)) / 10
  nd <- netis_data(nodes, netis_ties(d$data), wave_col = "wave",
                   tie_meta = list(tie = list(zero_meaning = "measured_zero")))
  expect_error(
    fit_influence(y ~ x * z + exposure(y, via = tie, x_lag = 0), data = nd),
    "cannot be combined with a same-outcome contemporaneous"
  )
  expect_error(
    fit_influence(y ~ x + exposure(y, via = tie, x_lag = 0) * z, data = nd),
    "cannot be combined with a same-outcome contemporaneous"
  )
})
