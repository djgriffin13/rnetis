# Wave order and wave lags.
#
# Every fixture here is ASYMMETRIC in the dimension under test: waves whose
# text order differs from their time order (9, 10), a transition whose `from`
# is not the wave before `to`, content and networks that differ at every wave,
# and directed networks whose transpose would give a different answer. Each
# assertion checks known cells against a value computed by hand from the raw
# inputs, never against the fit's own model frame.

# ---- the wave vocabulary ----------------------------------------------------

test_that("numeric text waves are ordered by value, not alphabetically", {
  expect_identical(.wave_vocabulary(list(a = c("10", "9", "11"))), c("9", "10", "11"))
  expect_identical(.wave_vocabulary(list(a = c(10, 9), b = c(11, 9))), c(9, 10, 11))
  # numbers in one table and numeric text in another are the same waves
  expect_identical(.wave_vocabulary(list(a = c(9, 10), b = c("10", "9"))), c(9, 10))
})

test_that("a factor declares the wave order through its levels", {
  f <- factor(c("pre", "post", "pre"), levels = c("pre", "post"))
  expect_identical(.wave_vocabulary(list(a = f, b = c("post"))), c("pre", "post"))
  g <- factor("post", levels = c("post", "pre"))
  expect_error(.wave_vocabulary(list(a = f, b = g)), "different orders")
})

test_that("text waves with no readable order, missing waves and mixed types are refused", {
  expect_error(.wave_vocabulary(list(a = c("pre", "post"))), "factor")
  expect_error(.wave_vocabulary(list(a = c(1, NA))), "missing wave label")
  expect_error(.wave_vocabulary(list(a = c(1, 2), b = c("x"))), "one type")
  # one text label has nothing to be out of order with
  expect_identical(.wave_vocabulary(list(a = "A")), "A")
})

test_that("netis_data() orders waves 9, 10 in time order, and lags follow it", {
  ids <- letters[1:6]
  y9 <- c(1, 4, 2, 8, 5, 7)
  y10 <- 2 * y9 + c(0.3, -0.1, 0.2, -0.4, 0.1, 0.05)
  nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(c("9", "10"), each = 6),
                      y = c(y9, y10))
  ties <- data.frame(group = 1, source = ids, target = ids[c(2:6, 1)], wave = "9", w = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_identical(netis_schema(nd)$waves, c("9", "10"))
  fit <- fit_influence(y ~ ar(), data = nd)
  # wave 10 regressed on wave 9 (slope near 2), not the reverse (near 0.5)
  ref <- stats::lm(y10 ~ y9)
  expect_lt(max(abs(unname(coef(fit)) - unname(coef(ref)))), 1e-10)
  expect_gt(unname(coef(fit)["y_lag1"]), 1.9)
})

# ---- importers keep the wave order ------------------------------------------

.wave_series_matrices <- function() {
  ids <- c("a", "b", "c")
  m9 <- matrix(0, 3, 3, dimnames = list(ids, ids)); m9["a", "b"] <- 1
  m10 <- matrix(0, 3, 3, dimnames = list(ids, ids)); m10["b", "c"] <- 1; m10["c", "a"] <- 1
  list(m9 = m9, m10 = m10)
}

test_that("an adjacency series with waves 9, 10 keeps them in time order", {
  m <- .wave_series_matrices()
  nd <- netis_data_from_adjacency_series(list(m$m9, m$m10), waves = c(9, 10))
  expect_identical(netis_schema(nd)$waves, c(9, 10))
  expect_identical(netis_adjacency(nd, "weight", wave = 9)["a", "b"], 1)
  expect_identical(netis_adjacency(nd, "weight", wave = 10)["c", "a"], 1)

  # names as labels, given out of order: the numbers decide
  nd2 <- netis_data_from_adjacency_series(list(`10` = m$m10, `9` = m$m9))
  expect_identical(as.character(netis_schema(nd2)$waves), c("9", "10"))
  expect_identical(netis_adjacency(nd2, "weight", wave = "9")["a", "b"], 1)

  # text labels: the order given
  nd3 <- netis_data_from_adjacency_series(list(pre = m$m9, post = m$m10))
  expect_identical(as.character(netis_schema(nd3)$waves), c("pre", "post"))
})

test_that("an igraph-list round trip keeps waves 9, 10, 11 in time order", {
  skip_if_not_installed("igraph")
  m <- .wave_series_matrices()
  nd <- netis_data_from_adjacency_series(list(m$m9, m$m10, m$m9), waves = c(9, 10, 11))
  back <- netis_data_from_igraph_list(as_igraph_list(nd))
  expect_identical(as.character(netis_schema(back)$waves), c("9", "10", "11"))
  # explicit labels that are text but numeric
  gl <- unname(as_igraph_list(nd))
  back2 <- netis_data_from_igraph_list(lapply(gl, function(g) {
    igraph::delete_graph_attr(g, "netis_meta")
  }), groups = c("1", "1", "1"), waves = c("9", "10", "11"))
  expect_identical(as.character(netis_schema(back2)$waves), c("9", "10", "11"))
})

# ---- a table covering only some waves is not fanned out ---------------------

test_that("a node variable measured at wave 2 only is not available at wave 1", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = rep(1:2, each = 2))
  groups <- data.frame(group = 1, wave = 2, climate = 5)
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = 1)
  nd <- netis_data(nodes, ties, group_data = groups, wave_col = "wave")
  g <- netis_groups(nd)
  expect_identical(nrow(g), 1L)
  expect_identical(as.numeric(g$wave), 2)
})

test_that("ties observed at wave 1 only describe wave 1, not wave 2", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b", "c"), 2), wave = rep(1:2, each = 3))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), wave = 1, w = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_identical(as.numeric(unique(netis_ties(nd)$wave)), 1)
  expect_identical(sum(netis_adjacency(nd, "w", wave = 1)), 2)
  expect_identical(nrow(netis_ties(nd)[netis_ties(nd)$wave == 2, ]), 0L)
})

# ---- a named transition fits `from -> to` -----------------------------------

.three_wave_fixture <- function() {
  set.seed(3)
  n <- 30
  ids <- sprintf("n%02d", 1:n)
  mk <- function() {
    A <- matrix(rbinom(n * n, 1, 0.12), n, n); diag(A) <- 0
    dimnames(A) <- list(ids, ids); A
  }
  A <- list(mk(), mk(), mk())
  Y <- cbind(rnorm(n), rnorm(n), rnorm(n))
  X <- cbind(rnorm(n), rnorm(n), rnorm(n))
  rownames(Y) <- rownames(X) <- ids
  el <- function(w) {
    i <- which(A[[w]] == 1, arr.ind = TRUE)
    data.frame(group = "g", source = ids[i[, 1]], target = ids[i[, 2]], wave = w, tie = 1)
  }
  nodes <- data.frame(group = "g", node = rep(ids, 3), wave = rep(1:3, each = n),
                      y = as.numeric(Y), x = as.numeric(X))
  ties <- do.call(rbind, lapply(1:3, el))
  list(nd = netis_data(nodes, ties, wave_col = "wave"), A = A, Y = Y, X = X, ids = ids)
}

test_that("waves = c(1, 3) regresses wave 3 on wave 1, as reported", {
  fx <- .three_wave_fixture()
  fit <- fit_influence(y ~ ar() + lagged(x) + exposure(y, via = tie), data = fx$nd,
                       waves = c(1, 3))
  # exposure source -> target: node i is exposed to the y of every j with a
  # tie j -> i, i.e. t(A) %*% y, all at wave 1
  ex1 <- as.numeric(t(fx$A[[1]]) %*% fx$Y[, 1])
  ref <- stats::lm(fx$Y[, 3] ~ fx$Y[, 1] + fx$X[, 1] + ex1)
  expect_lt(max(abs(unname(coef(fit)) - unname(coef(ref)))), 1e-10)
  expect_identical(as.character(fit$spec$waves_unused), "2")
  expect_identical(as.character(fit$resolved_exposures[[1]]$exposure_wave), "1")
  # and it is not the 2 -> 3 fit
  f23 <- fit_influence(y ~ ar() + lagged(x) + exposure(y, via = tie), data = fx$nd,
                       waves = c(2, 3))
  expect_false(isTRUE(all.equal(coef(fit), coef(f23))))
})

test_that("a lag deeper than the named transition allows is refused; one that fits is reported", {
  fx <- .three_wave_fixture()
  expect_error(
    fit_influence(y ~ ar(lag = 1:2), data = fx$nd, waves = c(1, 3)),
    "skips wave"
  )
  fit <- fit_influence(y ~ ar(lag = 1:2), data = fx$nd, waves = c(2, 3))
  ref <- stats::lm(fx$Y[, 3] ~ fx$Y[, 2] + fx$Y[, 1])
  expect_lt(max(abs(unname(coef(fit)) - unname(coef(ref)))), 1e-10)
  # wave 1 is read by ar(lag = 2), so it is not reported as unused
  expect_length(fit$spec$waves_unused, 0L)
})

test_that("a transition named with factor waves follows the levels, not the labels", {
  fx <- .three_wave_fixture()
  nodes <- netis_nodes(fx$nd); ties <- netis_ties(fx$nd)
  lv <- c("w9", "w10", "w11")
  nodes$wave <- factor(lv[nodes$wave], levels = lv)
  ties$wave <- factor(lv[ties$wave], levels = lv)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_identical(netis_schema(nd)$waves, lv)
  fit <- fit_influence(y ~ ar(), data = nd, waves = c("w9", "w11"))
  ref <- stats::lm(fx$Y[, 3] ~ fx$Y[, 1])
  expect_lt(max(abs(unname(coef(fit)) - unname(coef(ref)))), 1e-10)
  expect_error(fit_influence(y ~ ar(), data = nd, waves = c("w11", "w9")), "from < to")
})

test_that("Selection: waves = c(1, 3) reads wave 1 for prior_tie() and lagged covariates", {
  set.seed(5)
  n <- 6
  ids <- sprintf("a%02d", seq_len(n))
  nodes <- data.frame(group = "1", node = rep(ids, 3), wave = rep(1:3, each = n),
                      x = rnorm(3 * n))
  pr <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
  pr <- pr[pr$source != pr$target, ]
  ties <- do.call(rbind, lapply(1:3, function(w) {
    data.frame(group = "1", pr, wave = w, tie = rbinom(nrow(pr), 1, 0.4))
  }))
  nd <- netis_data(nodes, ties, wave_col = "wave",
                   tie_meta = list(tie = list(zero_meaning = "measured_zero")))
  spec <- .parse_selection_formula(tie ~ prior_tie() + sender(x, lag = 1))
  route <- .resolve_selection_waves(spec, netis_schema(nd), waves = c(1, 3))
  expect_identical(as.character(route$waves_analyzed), c("1", "3"))
  expect_identical(as.character(route$waves_unused), "2")
  asm <- .assemble_selection_group(spec, nd, netis_schema(nd), "1", route, NULL)
  W1 <- matrix(0, n, n, dimnames = list(ids, ids))
  t1 <- ties[ties$wave == 1, ]
  W1[cbind(t1$source, t1$target)] <- t1$tie
  pt <- asm$stacked$Xdyad[, , "prior_tie"]
  off <- row(W1) != col(W1)
  expect_identical(unname(pt[off]), unname(W1[off]))
  x1 <- nodes$x[nodes$wave == 1][match(ids, nodes$node[nodes$wave == 1])]
  expect_identical(unname(asm$stacked$Xrow[, 1]), x1)

  # lag 2 has no wave to read before `from` = 1
  spec2 <- .parse_selection_formula(tie ~ prior_tie(lag = 2))
  expect_error(.resolve_selection_waves(spec2, netis_schema(nd), waves = c(1, 3)),
               "only 1 earlier wave")
  # with `from` = 2 it reads wave 1, and wave 1 is reported as analyzed
  r23 <- .resolve_selection_waves(spec2, netis_schema(nd), waves = c(2, 3))
  expect_identical(as.character(r23$waves_analyzed), c("1", "3"))
  expect_identical(as.character(r23$waves_unused), "2")
})

# ---- bi() reads the transition into the outcome wave ------------------------

.bi_fixture <- function(n_waves) {
  ids <- c("a", "b", "c", "d")
  set.seed(9)
  shock <- matrix(c(3, 0, 1, 0,  0, 2, 0, 5,  1, 1, 0, 4), 4)[, seq_len(n_waves), drop = FALSE]
  nodes <- data.frame(group = 1, node = rep(ids, n_waves), wave = rep(seq_len(n_waves), each = 4),
                      shock = as.numeric(shock), y = rnorm(4 * n_waves))
  # a different directed network at every wave
  ties <- rbind(
    data.frame(group = 1, source = c("a", "a", "c"), target = c("b", "c", "d"), wave = 1, link = c(1, 2, 1)),
    data.frame(group = 1, source = c("b", "d"), target = c("d", "a"), wave = 2, link = c(1, 3)),
    data.frame(group = 1, source = c("c"), target = c("a"), wave = 3, link = 2)
  )
  ties <- ties[ties$wave <= n_waves, ]
  list(nd = netis_data(nodes, ties, wave_col = "wave"), shock = shock, ids = ids)
}

test_that("bi() at the default x_lag = 1 is the transition into the outcome wave (two waves)", {
  fx <- .bi_fixture(2)
  fit <- fit_influence(y ~ exposure(shock, via = link, mechanism = bi(0.5, 0.5, 0)), data = fx$nd)
  # One step: E = alpha * K_1 e_1, with K_1[i, j] the tie j -> i at wave 1.
  # Ties at wave 1: a->b (1), a->c (2), c->d (1); shock at wave 1 = (3, 0, 1, 0)
  # b receives 0.5 * 1 * 3 = 1.5; c receives 0.5 * 2 * 3 = 3; d 0.5 * 1 * 1 = 0.5
  mf <- fit$model_frame
  got <- setNames(mf$ex_shock_link, mf$node)[fx$ids]
  expect_lt(max(abs(unname(got) - c(0, 1.5, 3, 0.5))), 1e-12)
  expect_false(anyNA(coef(fit)))
  expect_identical(as.character(fit$resolved_exposures[[1]]$exposure_wave), "1")
})

test_that("bi() in a three-wave fit accumulates both transitions into wave 3", {
  fx <- .bi_fixture(3)
  fit <- fit_influence(y ~ exposure(shock, via = link, mechanism = bi(1, 1, 0)),
                       data = fx$nd, waves = c(2, 3))
  direct <- compute_exposure(fx$nd, "shock", "link", wave = 3, mechanism = bi(1, 1, 0))
  mf <- fit$model_frame
  expect_identical(setNames(mf$ex_shock_link, mf$node)[fx$ids],
                   setNames(direct$ex_shock_link, direct$node)[fx$ids])
  # by hand, delta = 1 (all paths), alpha = 1, gamma = 0:
  # one step from wave 2 (b->d 1, d->a 3; shock2 = (0, 2, 0, 5)): a 15, d 2
  # two steps, wave-1 shock through wave-1 then wave-2 ties: a->c->? none at
  # wave 2 from c; a->b->d: 3 * 1 * 1 = 3 to d; c->d->a: 1 * 1 * 3 = 3 to a
  expect_identical(unname(setNames(mf$ex_shock_link, mf$node)[fx$ids]), c(18, 0, 0, 5))
  expect_length(fit$spec$waves_unused, 0L)
})

test_that("bi() refuses timings it has no reading for", {
  fx <- .bi_fixture(3)
  expect_error(
    fit_influence(y ~ exposure(shock, via = link, x_lag = 0, mechanism = bi(0.5, 1, 0)) + ar(),
                  data = fx$nd, waves = c(2, 3)),
    "x_lag = 0 has no reading"
  )
  expect_error(
    fit_influence(y ~ exposure(shock, via = link, mechanism = bi(0.5, 1, 0)),
                  data = fx$nd, waves = c(1, 3)),
    "skips wave"
  )
})

# ---- lag arguments are validated --------------------------------------------

test_that("zero, negative and fractional lags are refused where they have no reading", {
  expect_error(ar(lag = 0), "at least 1")
  expect_error(ar(lag = -1), "at least 1")
  expect_error(ar(lag = 1.9), "whole number")
  expect_error(ar(lag = c(1, 1)), "same lag twice")
  expect_error(lagged(x, lag = 0), "at least 1")
  expect_error(lagged(x, lag = 0.5), "whole number")
  expect_error(exposure(x, via = w, x_lag = -1), "at least 0")
  expect_error(exposure(x, via = w, x_lag = 1.5), "whole number")
  expect_error(exposure(x, via = w, w_lag = -1), "at least 0")
  expect_error(centrality(w, measure = "degree", lag = -1), "at least 0")
  expect_error(network_stat(w, measure = "density", lag = 2.5), "whole number")
  expect_error(latent(w, lag = 1.5), "whole number")
  expect_error(latent(w, d = 2.7), "positive integer")
  expect_identical(ar(lag = 1:2)$lag, 1:2)
  expect_identical(exposure(x, via = w, x_lag = 0)$x_lag, 0L)
})

# ---- network timing compares values, not only rows --------------------------

test_that("a complete dyadic table whose values change is time-varying", {
  ids <- c("a", "b", "c", "d")
  pr <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
  pr <- pr[pr$source != pr$target, ]
  set.seed(2)
  nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = 4), y = rnorm(8))
  v1 <- rep(c(0, 1), length.out = nrow(pr))
  ties <- rbind(data.frame(group = 1, pr, wave = 1, w = v1),
                data.frame(group = 1, pr, wave = 2, w = rev(v1)))
  meta <- list(w = list(zero_meaning = "measured_zero"))
  nd <- netis_data(nodes, ties, wave_col = "wave", tie_meta = meta)
  fit <- fit_influence(y ~ ar() + exposure(y, via = w), data = nd)
  expect_identical(fit$assumptions$network$timing, "time_varying")

  ties_same <- rbind(data.frame(group = 1, pr, wave = 1, w = v1),
                     data.frame(group = 1, pr, wave = 2, w = v1))
  nd2 <- netis_data(nodes, ties_same, wave_col = "wave", tie_meta = meta)
  fit2 <- fit_influence(y ~ ar() + exposure(y, via = w), data = nd2)
  expect_identical(fit2$assumptions$network$timing, "fixed")
})

# ---- one occasion -----------------------------------------------------------

.one_occasion <- function(with_wave) {
  ids <- sprintf("n%02d", 1:12)
  set.seed(5)
  nodes <- data.frame(group = 1, node = ids, y = rnorm(12), x = rnorm(12))
  # a directed chain plus two chords, so in- and out-degree differ
  ties <- data.frame(group = 1, source = c(ids[1:11], "n01", "n05"),
                     target = c(ids[2:12], "n07", "n09"), tie = 1)
  if (with_wave) {
    nodes$wave <- 1
    ties$wave <- 1
    return(list(nd = netis_data(nodes, ties, wave_col = "wave"), nodes = nodes, ties = ties))
  }
  list(nd = netis_data(nodes, ties), nodes = nodes, ties = ties)
}

for (with_wave in c(FALSE, TRUE)) {
  test_that(sprintf("one-occasion data (%s): contemporaneous exposure and centrality fit",
                    if (with_wave) "one wave" else "no wave column"), {
    fx <- .one_occasion(with_wave)
    ids <- fx$nodes$node
    A <- matrix(0, 12, 12, dimnames = list(ids, ids))
    A[cbind(fx$ties$source, fx$ties$target)] <- 1
    ex <- as.numeric(t(A) %*% fx$nodes$x)
    indeg <- colSums(A)
    fit <- fit_influence(y ~ exposure(x, via = tie, x_lag = 0) +
                           centrality(tie, measure = "degree", mode = "in", lag = 0),
                         data = fx$nd)
    ref <- stats::lm(fx$nodes$y ~ ex + indeg)
    expect_lt(max(abs(unname(coef(fit)) - unname(coef(ref)))), 1e-10)
    expect_match(netis_estimand(fit)$description, "single cross-section")
    expect_s3_class(netis_dependence(fit, fx$nd), "netis_dependence")
    # a lagged network term has no earlier wave to read
    expect_error(fit_influence(y ~ centrality(tie, measure = "degree"), data = fx$nd),
                 "needs a prior wave")
  })
}

test_that("one-occasion SAR fit reports its estimand and dependence", {
  fx <- .one_occasion(FALSE)
  fit <- suppressWarnings(fit_influence(y ~ x + exposure(y, via = tie, x_lag = 0), data = fx$nd))
  expect_match(netis_estimand(fit)$description, "single cross-section")
  expect_length(netis_estimand(fit)$outcome_waves, 0L)
  expect_s3_class(netis_dependence(fit, fx$nd), "netis_dependence")
})

test_that("a group absent at the requested wave gets no node rows, not an error", {
  nodes <- data.frame(group = c(1, 1, 1, 1, 2, 2), node = c("a", "b", "a", "b", "c", "d"),
                      wave = c(1, 1, 2, 2, 1, 1), y = 1:6)
  ties <- data.frame(group = c(1, 1, 2), source = c("a", "a", "c"), target = c("b", "b", "d"),
                     wave = c(1, 2, 1), w = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  out <- compute_centrality(nd, w, measure = "degree", mode = "in", wave = 2)
  expect_identical(as.character(out$node), c("a", "b"))
  expect_identical(out$cent_degree_w, c(0, 1))
})
