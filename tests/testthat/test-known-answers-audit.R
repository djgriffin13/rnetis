# Known answers computed by hand from the raw inputs, never from a fit's own table.

# ---- centering and decomposition -------------------------------------------

test_that("add_centering() counts a static variable's nodes, not its node-waves", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b", "c"), 2), wave = rep(1:2, each = 3),
                      age = rep(c(20, 50, 80), 2), y = c(1, 2, 3, 2, 3, 5))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = c(1, 2))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  out <- add_centering(nd, "age")
  rec <- netis_schema(out)$derived$age_grand
  expect_identical(rec$basis, 50)
  expect_identical(rec$eligible_n, 3L)
  # A dynamic variable counts its rows.
  dyn <- netis_schema(add_centering(nd, "y"))$derived$y_grand
  expect_identical(dyn$eligible_n, 6L)
  expect_identical(dyn$basis, 16 / 6)
  n <- netis_nodes(out)
  expect_identical(unique(n$age_grand[n$node == "c"]), 30)
})

test_that("each decomposition unit gives its hand-computed means", {
  # Two groups, unequal sizes, two waves.
  nodes <- data.frame(
    group = c(1, 1, 1, 1, 2, 2),
    node = c("a", "b", "a", "b", "c", "c"),
    wave = c(1, 1, 2, 2, 1, 2),
    z = c(1, 3, 5, 11, 100, 200)
  )
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = c(1, 2))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  cell <- function(d, node, wave, col) d[[col]][d$node == node & d$wave == wave]

  actor <- netis_nodes(add_decomposition(nd, "z", unit = "actor"))
  expect_identical(cell(actor, "a", 1, "z_between"), 3)   # (1 + 5) / 2
  expect_identical(cell(actor, "b", 2, "z_within"), 4)    # 11 - 7
  expect_identical(cell(actor, "c", 1, "z_within"), -50)  # 100 - 150

  context <- netis_nodes(add_decomposition(nd, "z", unit = "context"))
  expect_identical(cell(context, "a", 1, "z_between"), 2) # (1 + 3) / 2
  expect_identical(cell(context, "b", 2, "z_between"), 8) # (5 + 11) / 2
  expect_identical(cell(context, "b", 2, "z_within"), 3)
  expect_identical(cell(context, "c", 2, "z_within"), 0)

  pooled <- netis_nodes(add_decomposition(nd, "z", unit = "group_pooled"))
  expect_identical(cell(pooled, "a", 1, "z_between"), 5)  # (1 + 3 + 5 + 11) / 4
  expect_identical(cell(pooled, "c", 1, "z_between"), 150)

  # A group-level variable over its group's waves.
  gd <- data.frame(group = c(1, 1, 2, 2), wave = c(1, 2, 1, 2), k = c(2, 4, 10, 40))
  ng <- netis_data(nodes, ties, group_data = gd, wave_col = "wave")
  gw <- netis_groups(add_decomposition(ng, "k", unit = "group_wave"))
  expect_identical(gw$k_between[gw$group == 2 & gw$wave == 1], 25)
  expect_identical(gw$k_within[gw$group == 1 & gw$wave == 2], 1)
  # Which unit a variable may take is checked.
  expect_error(add_decomposition(ng, "k", unit = "actor"))
  expect_error(add_decomposition(ng, "z", unit = "group_wave"))
})

test_that("a decomposition with no within-unit variation is refused", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = rep(1:2, each = 2), z = c(1, 2, 1, 2))
  nd <- netis_data(nodes, data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = 1:2),
                   wave_col = "wave")
  expect_error(add_decomposition(nd, "z", unit = "actor"))
})

# ---- time index, keys, assertions ------------------------------------------

test_that("time_meta refusals each name the problem", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = rep(1:2, each = 2))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = 1)
  mk <- function(tm, ...) netis_data(nodes, ties, wave_col = "wave", time_meta = tm, ...)
  expect_error(netis_data(nodes[1:2, c("group", "node")], data.frame(group = 1, source = "a", target = "b", w = 1),
                          time_meta = data.frame(wave = 1, time_value = 0), time_unit = "day"),
               "no wave dimension")
  expect_error(mk(list(1), time_unit = "day"), "must be a data frame")
  expect_error(mk(data.frame(wave = 9, time_value = 0), time_unit = "day"), "not observed")
  expect_error(mk(data.frame(group = 2, wave = 1, time_value = 0), time_unit = "day"), "not present")
  expect_error(mk(data.frame(wave = c(1, 1), time_value = 0:1), time_unit = "day"), "more than one row")
  expect_error(mk(data.frame(wave = 1:2, time_value = c(0, NA)), time_unit = "day"), "finite")
  expect_error(mk(data.frame(wave = 1:2, time_value = c(5, 1)), time_unit = "day"), "not increasing")
  expect_error(mk(data.frame(wave = 1:2, time_value = c(0, 1))), "time_unit")
})

test_that("the small assertions refuse what they say", {
  expect_error(.netis_unit_key("a\rb", "c"), "carriage return")
  expect_identical(.netis_unit_key(1, "c"), "1\rc")
  expect_error(.assert_count(1.5, "n"), "whole number")
  expect_error(.assert_count(0, "n", min = 1L), ">= 1")
  expect_true(.assert_count(5, "n"))
  expect_error(.assert_nonneg_scalar(-1, "s"), "non-negative")
  expect_error(.assert_nonneg_scalar(c(1, 2), "s"), "non-negative")
  expect_true(.assert_nonneg_scalar(0, "s"))
})

# ---- importers and ragged data ---------------------------------------------

test_that("netis_data_from_split() joins non-coincident tables by key", {
  node_static <- data.frame(group = 1, node = c("b", "a"), sex = c("m", "f"))
  node_dynamic <- data.frame(group = 1, node = c("a", "b", "a"), wave = c(1, 1, 2), y = c(1, 2, 3))
  tie_dynamic <- data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = c(4, 5))
  nd <- netis_data_from_split(node_static = node_static, node_dynamic = node_dynamic,
                              tie_dynamic = tie_dynamic, wave_col = "wave")
  n <- netis_nodes(nd)
  # b has no wave 2 row; the static column follows the key, not the position.
  expect_identical(nrow(n), 3L)
  expect_identical(n$sex[n$node == "a" & n$wave == 2], "f")
  expect_identical(n$sex[n$node == "b"], "m")
})

test_that("a grouped edge list keeps each tie in its group", {
  edges <- data.frame(group = c("A", "B"), source = c("x", "x"), target = c("y", "z"), w = c(2, 3))
  roster <- data.frame(group = c("A", "A", "B", "B"), node = c("x", "y", "x", "z"))
  nd <- netis_data_from_edgelist(edges, nodes = roster)
  m <- netis_adjacency(nd, "w", drop = FALSE)
  expect_identical(m$A[["x", "y"]], 2)
  expect_identical(m$B[["x", "z"]], 3)
  expect_false("z" %in% rownames(m$A))
})

test_that("a ragged group x wave table reconstructs its own rows", {
  nodes <- data.frame(group = c(1, 1, 1, 2, 2), node = c("a", "b", "a", "c", "d"),
                      wave = c(1, 1, 2, 2, 2))
  ties <- data.frame(group = c(1, 2), source = c("a", "c"), target = c("b", "d"), wave = c(1, 2), w = c(1, 2))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  n <- netis_nodes(nd)
  expect_identical(nrow(n), 5L)
  # Group 2 is not at wave 1, and b is not at wave 2.
  expect_false(any(n$group == 2 & n$wave == 1))
  expect_false(any(n$node == "b" & n$wave == 2))
  ti <- netis_time_index(nd)
  expect_false(any(ti$group == "2" & ti$wave_position == 1L))
})

test_that("the shipped multi-group demo's network is the one it documents", {
  t <- netis_ties(demo_influence_groups)
  expect_identical(unique(t$wave), 1L)
  expect_setequal(unique(as.character(t$group)), unique(as.character(netis_nodes(demo_influence_groups)$group)))
  # Tie endpoints are members of their own group, for every tie.
  n <- netis_nodes(demo_influence_groups)
  key <- paste(n$group, n$node)
  expect_true(all(paste(t$group, t$source) %in% key))
  expect_true(all(paste(t$group, t$target) %in% key))
})

# ---- network measures ------------------------------------------------------

test_that("closeness reads the direction it is asked for", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = 1)
  nd <- netis_data(nodes, ties)
  out <- compute_centrality(nd, w, measure = "closeness", mode = "out", name = "cl")
  # a reaches b at 1 and c at 2: 1 / 3 under igraph's reachable-vertices rule.
  expect_lt(abs(out$cl[out$node == "a"] - 1 / 3), 1e-12)
  expect_true(is.nan(out$cl[out$node == "c"]))
  inn <- compute_centrality(nd, w, measure = "closeness", mode = "in", name = "cl")
  expect_lt(abs(inn$cl[inn$node == "c"] - 1 / 3), 1e-12)
  expect_true(is.nan(inn$cl[inn$node == "a"]))
})

# ---- the fit path against a table assembled by hand ------------------------

.ka_world <- function(n = 14L, seed = 21L) {
  set.seed(seed)
  ids <- sprintf("n%02d", sample(n))
  # A different asymmetric network at each of three waves.
  draw <- function(w) {
    A <- matrix(stats::rbinom(n * n, 1, 0.2), n, n, dimnames = list(ids, ids))
    diag(A) <- 0
    idx <- which(A == 1, arr.ind = TRUE)
    list(A = A, ties = data.frame(group = 1, source = ids[idx[, 1]], target = ids[idx[, 2]],
                                  wave = w, tie = stats::runif(nrow(idx), 1, 3)))
  }
  nets <- lapply(1:3, draw)
  for (w in 1:3) {
    t <- nets[[w]]$ties
    M <- matrix(0, n, n, dimnames = list(ids, ids))
    M[cbind(t$source, t$target)] <- t$tie
    nets[[w]]$M <- M
  }
  values <- function() stats::setNames(stats::rnorm(n), ids)
  y <- lapply(1:3, function(w) values())
  x <- lapply(1:3, function(w) values())
  nodes <- do.call(rbind, lapply(1:3, function(w) {
    data.frame(group = 1, node = ids, wave = w, y = y[[w]], x = x[[w]], row.names = NULL)
  }))
  list(data = netis_data(nodes, do.call(rbind, lapply(nets, `[[`, "ties")), wave_col = "wave"),
       ids = ids, y = y, x = x, M = lapply(nets, `[[`, "M"))
}

test_that("x_lag and w_lag read the waves they name, on a changing asymmetric network", {
  w <- .ka_world()
  # Exposure at wave 3 to x at wave 2 through the network at wave 1: a source
  # influences its target, so the matrix multiplies transposed.
  fit <- fit_influence(y ~ ar() + exposure(x, via = tie, x_lag = 1, w_lag = 2), data = w$data,
                       waves = c(2, 3))
  by_hand <- as.numeric(t(w$M[[1]]) %*% w$x[[2]])
  names(by_hand) <- w$ids
  mf <- fit$model_frame
  expect_lt(max(abs(mf$ex_x_tie - by_hand[as.character(mf$node)])), 1e-12)
  expect_identical(fit$resolved_exposures$ex_x_tie$conduit_wave, 1L)
  # The coefficients against a regression written from the raw vectors.
  ref <- stats::lm(w$y[[3]] ~ w$y[[2]] + by_hand)
  expect_lt(max(abs(unname(coef(fit)) - unname(stats::coef(ref)))), 1e-10)
  # And the transposed network gives another answer, so the fixture can tell.
  swapped <- as.numeric(w$M[[1]] %*% w$x[[2]])
  expect_gt(max(abs(swapped - by_hand)), 0.5)
})

test_that("the simultaneous route reads an asymmetric W1 the right way round", {
  skip_if_not_installed("sna")
  w <- .ka_world()
  two <- netis_data(netis_nodes(w$data)[netis_nodes(w$data)$wave == 1, c("group", "node", "y", "x")],
                    transform(netis_ties(w$data)[netis_ties(w$data)$wave == 1, c("group", "source", "target", "tie")]))
  fit <- fit_influence(y ~ exposure(y, via = tie, x_lag = 0) + x, data = two)
  mf <- fit$model_frame
  ids <- as.character(mf$node)
  W <- t(w$M[[1]])[ids, ids]
  direct <- sna::lnam(y = mf$.outcome, x = cbind(`(Intercept)` = 1, x = mf$x), W1 = W)
  transposed <- sna::lnam(y = mf$.outcome, x = cbind(`(Intercept)` = 1, x = mf$x), W1 = t(W))
  expect_lt(abs(coef(fit)[["ex_y_tie"]] - unname(direct$rho1)), 1e-6)
  expect_gt(abs(unname(direct$rho1) - unname(transposed$rho1)), 1e-3)
})

test_that("the standard model methods agree with lm() on the same fit", {
  w <- .ka_world()
  fit <- fit_influence(y ~ ar() + x, data = w$data, waves = c(2, 3))
  ref <- stats::lm(w$y[[3]] ~ w$y[[2]] + w$x[[3]])
  expect_lt(abs(as.numeric(logLik(fit)) - as.numeric(stats::logLik(ref))), 1e-10)
  expect_lt(abs(AIC(fit) - stats::AIC(ref)), 1e-10)
  expect_lt(abs(BIC(fit) - stats::BIC(ref)), 1e-10)
  expect_lt(max(abs(unname(confint(fit)) - unname(stats::confint(ref)))), 1e-10)
  expect_lt(max(abs(sort(unname(predict(fit))) - sort(unname(stats::fitted(ref))))), 1e-10)
  # update() refits with the replaced argument.
  back <- update(fit, waves = c(1, 2))
  ref12 <- stats::lm(w$y[[2]] ~ w$y[[1]] + w$x[[2]])
  expect_lt(max(abs(unname(coef(back)) - unname(stats::coef(ref12)))), 1e-10)
})

test_that("a mixed fit drops the rows with a missing exposure, as a hand-built sample does", {
  skip_if_not_installed("lme4")
  set.seed(8)
  make <- function(g, n) {
    ids <- sprintf("g%d_%d", g, seq_len(n))
    list(nodes = data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = n),
                            y = stats::rnorm(2 * n) + g, x = stats::rnorm(2 * n)),
         ties = data.frame(group = g, source = ids[-n], target = ids[-1], tie = 1))
  }
  parts <- Map(make, 1:6, c(6, 7, 8, 9, 10, 11))
  nodes <- do.call(rbind, lapply(parts, `[[`, "nodes"))
  ties <- do.call(rbind, lapply(parts, `[[`, "ties"))
  # One alter's value missing at the lag wave: its one target loses its row.
  nodes$x[nodes$node == "g2_3" & nodes$wave == 1] <- NA
  nd <- netis_data(nodes, ties, wave_col = "wave")
  fit <- suppressWarnings(fit_influence(y ~ ar() + exposure(x, via = tie) + (1 | group), data = nd))
  w1 <- nodes[nodes$wave == 1, ]
  w2 <- nodes[nodes$wave == 2, ]
  d <- data.frame(group = w2$group, node = w2$node, y2 = w2$y,
                  y1 = w1$y[match(w2$node, w1$node)])
  src <- ties$source[match(d$node, ties$target)]
  d$ex <- 0
  d$ex[!is.na(src)] <- w1$x[match(src[!is.na(src)], w1$node)]
  d <- d[!is.na(d$ex), ]
  expect_identical(netis_glance(fit)$nobs, nrow(d))
  ref <- suppressWarnings(lme4::lmer(y2 ~ y1 + ex + (1 | group), data = d))
  expect_lt(max(abs(unname(coef(fit)) - unname(lme4::fixef(ref)))), 1e-6)
})

test_that("a function in the caller's scope cannot shadow a formula term", {
  set.seed(6)
  nodes <- data.frame(group = 1, node = rep(letters[1:8], 2), wave = rep(1:2, each = 8),
                      y = stats::rnorm(16), x = stats::rnorm(16))
  nd <- netis_data(nodes, data.frame(group = 1, source = letters[1:7], target = letters[2:8], tie = 1),
                   wave_col = "wave")
  baseline <- coef(fit_influence(y ~ ar() + exposure(x, via = tie), data = nd))
  local({
    ar <- function(...) stop("the caller's ar() was called")
    exposure <- function(...) stop("the caller's exposure() was called")
    got <- coef(fit_influence(y ~ ar() + exposure(x, via = tie), data = nd))
    expect_identical(got, baseline)
  })
})

# ---- surfaces nothing else exercises ---------------------------------------

test_that("netis_diagnostics() lists exactly the findings the fit recorded", {
  set.seed(3)
  nodes <- data.frame(group = 1, node = rep(letters[1:8], 2), wave = rep(1:2, each = 8),
                      y = stats::rnorm(16), x = stats::rnorm(16))
  nd <- netis_data(nodes, data.frame(group = 1, source = letters[1:7], target = letters[2:8], tie = 1),
                   wave_col = "wave")
  fit <- fit_influence(y ~ exposure(x, via = tie, x_lag = 0), data = nd)
  d <- netis_diagnostics(fit)
  expect_identical(names(d), c("family", "code", "level", "summary", "detail"))
  expect_setequal(d$code, c("no_autoregressive_term", "contemporaneous_exposure"))
  expect_identical(d$level[d$code == "contemporaneous_exposure"], "WARNING")
  expect_identical(netis_glance(fit)$n_warning, 1L)
  expect_identical(netis_glance(fit)$n_note, 1L)
  expect_identical(nrow(.diagnostics_as_rows(list())), 0L)
  expect_error(netis_diagnostics(1), "no method for an object of class numeric")
})

test_that("as_netis_data() takes an igraph graph", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(c("a", "b", "b", "c"), directed = TRUE)
  igraph::E(g)$w <- c(2, 5)
  nd <- as_netis_data(g, group = "z")
  t <- netis_ties(nd)
  expect_identical(t$w[t$source == "b"], 5)
  expect_identical(unique(t$group), "z")
})

test_that("a derived-variable record prints what it was computed from", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), age = c(1, 2, 6))
  nd <- netis_data(nodes, data.frame(group = 1, source = "a", target = "b", w = 1))
  rec <- netis_schema(add_centering(nd, "age"))$derived$age_grand
  out <- capture.output(print(rec))
  expect_match(out[1], "source: age", fixed = TRUE)
  expect_match(out[1], "method: grand", fixed = TRUE)
})

test_that("predict(newdata) reads the assembled columns on the least-squares and mixed routes", {
  set.seed(7)
  nodes <- data.frame(group = rep(1:3, each = 12), node = paste0(rep(1:3, each = 12), rep(letters[1:6], 6)),
                      wave = rep(rep(1:2, each = 6), 3), y = stats::rnorm(36) + rep(1:3, each = 12),
                      x = stats::rnorm(36))
  ties <- data.frame(group = 1:3, source = c("1a", "2a", "3a"), target = c("1b", "2b", "3b"), tie = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  one <- fit_influence(y ~ ar() + x, data = netis_data(nodes[nodes$group == 1, ], ties[1, ], wave_col = "wave"))
  new <- data.frame(y_lag1 = c(0, 1), x = c(2, 3))
  b <- coef(one)
  expect_lt(max(abs(unname(predict(one, newdata = new)) -
                      c(b[[1]] + 2 * b[["x"]], b[[1]] + b[["y_lag1"]] + 3 * b[["x"]]))), 1e-12)
  skip_if_not_installed("lme4")
  mixed <- suppressWarnings(fit_influence(y ~ ar() + x + (1 | group), data = nd))
  newm <- data.frame(y_lag1 = 0, x = 1, group = 2)
  re <- mixed$random_effects$group
  expected <- coef(mixed)[[1]] + coef(mixed)[["x"]] + re[["(Intercept)"]][rownames(re) == "2"]
  expect_lt(abs(unname(predict(mixed, newdata = newm)) - expected), 1e-8)
})
