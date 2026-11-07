# An observed empty network and an unobserved one are different states, and
# every route in and out of the package keeps them apart.

.euw_nodes <- function() {
  data.frame(
    group = "1", node = rep(c("a", "b", "c", "d"), times = 3),
    wave = rep(1:3, each = 4),
    y = c(1, 2, 4, 8, 3, 5, 7, 11, 2, 6, 3, 9)
  )
}

.euw_adj <- function(from, to, value, ids = c("a", "b", "c", "d")) {
  m <- matrix(0, length(ids), length(ids), dimnames = list(ids, ids))
  m[cbind(from, to)] <- value
  m
}

# Waves 1 and 3 hold ties; wave 2 is an all-zero matrix.
.euw_series <- function() {
  list(
    `1` = .euw_adj(c("a", "b"), c("b", "c"), c(2, 3)),
    `2` = .euw_adj(character(0), character(0), numeric(0)),
    `3` = .euw_adj(c("a", "c"), c("d", "a"), c(5, 7))
  )
}

test_that("an all-zero matrix in a series is an observed empty wave", {
  nd <- netis_data_from_adjacency_series(.euw_series(), nodes = .euw_nodes())
  expect_identical(netis_schema(nd)$waves, c(1, 2, 3))
  expect_identical(netis_schema(nd)$observed_tie_waves$wave, c("1", "3", "2"))

  # Hand-known: no alter at wave 2, so a weighted sum of nothing is 0.
  expect_silent(ex <- compute_exposure(nd, "y", "weight", wave = 2))
  expect_identical(ex$ex_y_weight, c(0, 0, 0, 0))

  # The isolate rule governs a normalized exposure there, as for any isolate.
  expect_silent(ex_na <- compute_exposure(nd, "y", "weight", wave = 2, normalize = "row"))
  expect_true(all(is.na(ex_na$ex_y_weight)))
  expect_silent(ex_zero <- compute_exposure(nd, "y", "weight", wave = 2, normalize = "row",
                                            isolate_rule = "zero"))
  expect_identical(ex_zero$ex_y_weight, c(0, 0, 0, 0))
  expect_error(
    compute_exposure(nd, "y", "weight", wave = 2, normalize = "row", isolate_rule = "error"),
    "isolates"
  )

  # And the waves with ties are the values their rows give: under the default
  # direction source influences target, so b receives a's y (1) at weight 2.
  ex1 <- compute_exposure(nd, "y", "weight", wave = 1)
  expect_identical(ex1$ex_y_weight, c(0, 2 * 1, 3 * 2, 0))
})

test_that("an empty wave a tie table leaves unrecorded reads as unobserved", {
  ties <- netis_ties(netis_data_from_adjacency_series(.euw_series(), nodes = .euw_nodes()))
  nd <- netis_data(.euw_nodes(), ties, wave_col = "wave",
                   tie_meta = list(weight = list(zero_meaning = "structural_absence")))
  expect_warning(ex <- compute_exposure(nd, "y", "weight", wave = 2), "group 1 wave 2")
  expect_true(all(is.na(ex$ex_y_weight)))
  # The record is the only difference, and it is the whole difference.
  declared <- netis_data(.euw_nodes(), ties, wave_col = "wave",
                         tie_meta = list(weight = list(zero_meaning = "structural_absence")),
                         observed_tie_waves = data.frame(group = "1", wave = 2))
  expect_silent(ex2 <- compute_exposure(declared, "y", "weight", wave = 2))
  expect_identical(ex2$ex_y_weight, c(0, 0, 0, 0))
})

test_that("a wave left out of a series is unobserved", {
  s <- .euw_series()
  s$`2` <- NULL
  nd <- netis_data_from_adjacency_series(s, nodes = .euw_nodes())
  expect_warning(ex <- compute_exposure(nd, "y", "weight", wave = 2), "group 1 wave 2")
  expect_true(all(is.na(ex$ex_y_weight)))
})

test_that("a series whose last wave is empty keeps that wave and stays time-varying", {
  # The wave has no tie row and the roster has no wave column, so only the
  # record carries it. Without it the wave disappears and the wave-1 ties collapse
  # to a network reused at every wave.
  s <- .euw_series()[1:2]
  nd <- netis_data_from_adjacency_series(s)
  expect_identical(as.character(netis_schema(nd)$waves), c("1", "2"))
  expect_identical(sum(netis_adjacency(nd, "weight", wave = 2)), 0)
  expect_identical(sum(netis_adjacency(nd, "weight", wave = 1)), 5)
})

test_that("a series with no ties at all keeps its waves and is empty at each", {
  zero <- .euw_adj(character(0), character(0), numeric(0))
  nd <- netis_data_from_adjacency_series(list(`1` = zero, `2` = zero), nodes = .euw_nodes())
  expect_silent(ex <- compute_exposure(nd, "y", "weight", wave = 1))
  expect_identical(ex$ex_y_weight, c(0, 0, 0, 0))
  # Declared observed, so not the "cannot tell" note.
  expect_false("group_without_any_ties" %in%
                 vapply(diagnose_data(nd), function(f) f$code, character(1)))
})

test_that("a group's empty matrix does not disturb another group's waves", {
  nodes <- rbind(transform(.euw_nodes(), group = "A"), transform(.euw_nodes(), group = "B"))
  s <- list(A = .euw_series(), B = .euw_series())
  s$B$`2` <- .euw_adj("a", "b", 4)
  s$A$`3` <- NULL
  nd <- netis_data_from_adjacency_series(s, nodes = nodes)
  ex <- suppressWarnings(compute_exposure(nd, "y", "weight", wave = 3))
  # Group A left wave 3 out, so it is unobserved there, and only there.
  expect_true(all(is.na(ex$ex_y_weight[ex$group == "A"])))
  expect_false(anyNA(ex$ex_y_weight[ex$group == "B"]))
  expect_warning(compute_exposure(nd, "y", "weight", wave = 3), "group A wave 3")
  expect_silent(ex2 <- compute_exposure(nd, "y", "weight", wave = 2))
  expect_identical(ex2$ex_y_weight[ex2$group == "A"], c(0, 0, 0, 0))
  # b receives a's y (3) at weight 4.
  expect_identical(ex2$ex_y_weight[ex2$group == "B"], c(0, 12, 0, 0))
})

test_that("a path-dependent history treats the two states differently", {
  mech <- bi(delta = 0.5, alpha = 1, gamma = 0)
  nd <- netis_data_from_adjacency_series(.euw_series(), nodes = .euw_nodes())
  # An empty history wave cuts paths through it, and is not missing.
  expect_silent(ex <- compute_exposure(nd, "y", "weight", wave = 3, mechanism = mech))
  expect_false(anyNA(ex[[ncol(ex)]]))
  s <- .euw_series()
  s$`2` <- NULL
  gap <- netis_data_from_adjacency_series(s, nodes = .euw_nodes())
  expect_warning(ex2 <- compute_exposure(gap, "y", "weight", wave = 3, mechanism = mech),
                 "history wave 2")
  expect_true(all(is.na(ex2[[ncol(ex2)]])))
})

test_that("network features read an observed empty wave as empty", {
  nd <- netis_data_from_adjacency_series(.euw_series(), nodes = .euw_nodes())
  expect_silent(deg <- compute_centrality(nd, "weight", measure = "degree", wave = 2))
  expect_identical(deg[[ncol(deg)]], c(0, 0, 0, 0))
  s <- .euw_series()
  s$`2` <- NULL
  gap <- netis_data_from_adjacency_series(s, nodes = .euw_nodes())
  expect_warning(deg2 <- compute_centrality(gap, "weight", measure = "degree", wave = 2),
                 "group 1 wave 2")
  expect_true(all(is.na(deg2[[ncol(deg2)]])))
})

# --- fit_influence, end to end ------------------------------------------------

.euw_fit_fixture <- function(observed_empty) {
  ids <- paste0("n", 1:6)
  nodes <- do.call(rbind, lapply(c("A", "B", "C"), function(g) {
    off <- c(A = 0, B = 3, C = -2)[[g]]
    data.frame(
      group = g, node = rep(ids, times = 2), wave = rep(1:2, each = 6),
      y = off + c(1.0, 2.5, 0.5, 3.0, 1.5, 4.0, 1.3, 2.0, 1.1, 3.9, 0.8, 4.4) +
        c(A = 0, B = 1, C = -1)[[g]] * rep(c(0.7, -0.4, 0.2, -0.9, 0.5, 0.1), times = 2)
    )
  }))
  ring <- .euw_adj(ids, ids[c(2:6, 1)], c(1, 2, 1, 3, 1, 2), ids = ids)
  zero <- .euw_adj(character(0), character(0), numeric(0), ids = ids)
  # Group A's wave-1 network is the empty one; a wave-2 network shows that group
  # A's network exists, so leaving wave 1 out makes it unobserved there.
  series <- list(
    A = if (observed_empty) list(`1` = zero, `2` = ring) else list(`2` = ring),
    B = list(`1` = ring, `2` = ring),
    C = list(`1` = t(ring), `2` = ring)
  )
  netis_data_from_adjacency_series(series, nodes = nodes)
}

test_that("fit_influence() keeps an observed empty wave's rows at exposure 0", {
  skip_if_not_installed("lme4")
  nd <- .euw_fit_fixture(observed_empty = TRUE)
  f <- y ~ ar() + exposure(y, via = weight) + (1 | group)
  expect_silent(ex <- compute_exposure(nd, "y", "weight", wave = 1))
  expect_identical(ex$ex_y_weight[ex$group == "A"], rep(0, 6))

  fit <- suppressMessages(fit_influence(f, data = nd))
  mf <- fit$model_frame
  expcol <- grep("^ex_", names(mf), value = TRUE)
  expect_identical(nrow(mf), 18L)
  expect_identical(mf[[expcol]][mf$group == "A"], rep(0, 6))
  expect_false(anyNA(mf[[expcol]]))

  # The same regression written out by hand from those values.
  nodes <- netis_nodes(nd)
  w1 <- nodes[nodes$wave == 1, ]
  w2 <- nodes[nodes$wave == 2, ]
  byhand <- merge(w2[c("group", "node", "y")],
                  transform(w1[c("group", "node", "y")], y1 = y, y = NULL),
                  by = c("group", "node"))
  byhand <- merge(byhand, ex, by = c("group", "node"))
  m <- lme4::lmer(y ~ y1 + ex_y_weight + (1 | group), data = byhand, REML = TRUE)
  est <- netis_estimates(fit)
  beta <- est$estimate[grepl("^ex_", est$term)]
  expect_lt(abs(beta - lme4::fixef(m)[["ex_y_weight"]]), 1e-4)
})

test_that("fit_influence() drops an unobserved wave's rows and reports them", {
  skip_if_not_installed("lme4")
  nd <- .euw_fit_fixture(observed_empty = FALSE)
  expect_warning(ex <- compute_exposure(nd, "y", "weight", wave = 1), "group A wave 1")
  expect_true(all(is.na(ex$ex_y_weight[ex$group == "A"])))

  f <- y ~ ar() + exposure(y, via = weight) + (1 | group)
  expect_warning(fit <- suppressMessages(fit_influence(f, data = nd)), "group A wave 1")
  mf <- fit$model_frame
  expcol <- grep("^ex_", names(mf), value = TRUE)
  expect_true(all(is.na(mf[[expcol]][mf$group == "A"])))
  codes <- vapply(fit$diagnostics, function(d) d$code, character(1))
  expect_true("exposure_missing_via_absent_network" %in% codes)
})

# --- Selection ----------------------------------------------------------------

test_that("Selection reads an observed empty wave as known zeros and an unobserved one as missing", {
  nodes <- .euw_nodes()
  s <- .euw_series()
  empty <- netis_data_from_adjacency_series(s, nodes = nodes)
  y2 <- .build_selection_y(empty, "weight", "1", 2)
  expect_identical(unname(y2$counts[c("observed", "missing", "structural")]), c(12L, 0L, 4L))
  expect_identical(sum(y2$Y, na.rm = TRUE), 0)

  s$`2` <- NULL
  gap <- netis_data_from_adjacency_series(s, nodes = nodes)
  y2 <- .build_selection_y(gap, "weight", "1", 2)
  expect_identical(unname(y2$counts[c("observed", "missing", "structural")]), c(0L, 12L, 4L))
  expect_true(all(is.na(y2$Y)))
})

# --- igraph and network routes -----------------------------------------------

.euw_unobserved <- function() {
  s <- .euw_series()
  s$`2` <- NULL
  netis_data_from_adjacency_series(s, nodes = .euw_nodes())
}

test_that("an edgeless graph in a list is an observed empty wave", {
  skip_if_not_installed("igraph")
  nodes <- .euw_nodes()
  mk <- function(w) {
    g <- igraph::graph_from_adjacency_matrix(.euw_series()[[w]], mode = "directed",
                                             weighted = "weight")
    igraph::V(g)$y <- nodes$y[nodes$wave == w]
    g
  }
  nd <- netis_data_from_igraph_list(lapply(1:3, mk), groups = rep("1", 3), waves = 1:3)
  expect_silent(ex <- compute_exposure(nd, "y", "weight", wave = 2))
  expect_identical(ex$ex_y_weight, c(0, 0, 0, 0))
  expect_identical(compute_exposure(nd, "y", "weight", wave = 1)$ex_y_weight, c(0, 2, 6, 0))
  # Leaving the wave out says it was not observed.
  gap <- netis_data_from_igraph_list(lapply(c(1, 3), mk), groups = rep("1", 2), waves = c(1, 3))
  expect_identical(as.numeric(netis_schema(gap)$waves), c(1, 3))
  expect_setequal(netis_schema(gap)$observed_tie_waves$wave, c("1", "3"))
})

test_that("an igraph list carries exposure to a known value through an edgeless wave", {
  skip_if_not_installed("igraph")
  nd0 <- netis_data_from_adjacency_series(.euw_series(), nodes = .euw_nodes())
  gs <- as_igraph_list(nd0)
  expect_identical(names(gs), c("1_w1", "1_w2", "1_w3"))
  back <- netis_data_from_igraph_list(gs)
  expect_silent(ex <- compute_exposure(back, "y", "weight", wave = 2))
  expect_identical(ex$ex_y_weight, c(0, 0, 0, 0))
  expect_identical(compute_exposure(back, "y", "weight", wave = 1)$ex_y_weight, c(0, 2, 6, 0))
})

test_that("as_igraph() warns for an unobserved wave and not for an observed empty one", {
  skip_if_not_installed("igraph")
  gap <- .euw_unobserved()
  expect_warning(g <- as_igraph(gap, wave = 2), "group 1 wave 2")
  expect_identical(igraph::ecount(g), 0)
  expect_identical(igraph::vcount(g), 4)
  empty <- netis_data_from_adjacency_series(.euw_series(), nodes = .euw_nodes())
  expect_silent(g2 <- as_igraph(empty, wave = 2))
  expect_identical(igraph::ecount(g2), 0)
  expect_silent(as_igraph(gap, wave = 1))
})

test_that("as_igraph_list() names every unobserved snapshot once, and the list round-trips", {
  skip_if_not_installed("igraph")
  gap <- .euw_unobserved()
  expect_warning(gs <- as_igraph_list(gap), "group 1 wave 2")
  expect_identical(names(gs), c("1_w1", "1_w2", "1_w3"))
  expect_silent(as_igraph_list(netis_data_from_adjacency_series(.euw_series(), nodes = .euw_nodes())))
  # The graph records it, so coming back it is still unobserved, not empty.
  back <- netis_data_from_igraph_list(gs)
  expect_warning(ex <- compute_exposure(back, "y", "weight", wave = 2), "group 1 wave 2")
  expect_true(all(is.na(ex$ex_y_weight)))
  # On its own the graph would read as an observed empty network, so it is refused.
  expect_error(netis_data_from_igraph(gs[["1_w2"]]), "unobserved")
})

test_that("as_network() and as_network_list() warn only for an unobserved wave", {
  skip_if_not_installed("network")
  gap <- .euw_unobserved()
  expect_warning(n <- as_network(gap, wave = 2), "group 1 wave 2")
  expect_identical(network::network.edgecount(n), 0L)
  expect_warning(l <- as_network_list(gap), "group 1 wave 2")
  expect_identical(names(l), c("1_w1", "1_w2", "1_w3"))
  expect_error(netis_data_from_network(n), "unobserved")
  empty <- netis_data_from_adjacency_series(.euw_series(), nodes = .euw_nodes())
  expect_silent(n2 <- as_network(empty, wave = 2))
  expect_identical(network::network.edgecount(n2), 0L)
  expect_silent(as_network_list(empty))
  # One edgeless object at a wave, with its own record of being observed, imports.
  back <- netis_data_from_network(n2)
  expect_identical(netis_schema(back)$observed_tie_waves$wave, "2")
})

test_that("netis_adjacency() warns only for an unobserved wave, naming group and wave", {
  gap <- .euw_unobserved()
  expect_warning(m <- netis_adjacency(gap, "weight", wave = 2), "group 1 wave 2")
  expect_identical(sum(m), 0)
  empty <- netis_data_from_adjacency_series(.euw_series(), nodes = .euw_nodes())
  expect_silent(m2 <- netis_adjacency(empty, "weight", wave = 2))
  expect_identical(sum(m2), 0)
  expect_silent(netis_adjacency(gap, "weight", wave = 1))
  expect_silent(netis_adjacency(empty, "weight", wave = 2, presence = TRUE))
})

# --- the record survives what rebuilds the object ------------------------------

test_that("add_centering(), add_decomposition() and add_baseline_provenance() keep the record", {
  empty <- netis_data_from_adjacency_series(.euw_series(), nodes = .euw_nodes())
  rec <- netis_schema(empty)$observed_tie_waves
  for (nd in list(
    add_centering(empty, "y"),
    add_decomposition(empty, "y", unit = "actor"),
    add_baseline_provenance(empty, "y")
  )) {
    expect_identical(netis_schema(nd)$observed_tie_waves[order(rec$wave), ], rec[order(rec$wave), ],
                     ignore_attr = TRUE)
    expect_silent(ex <- compute_exposure(nd, "y", "weight", wave = 2))
    expect_identical(ex$ex_y_weight, c(0, 0, 0, 0))
  }
  # A wave only the record holds also survives a rebuild.
  bare <- netis_data_from_adjacency_series(.euw_series()[1:2])
  expect_identical(as.character(netis_schema(add_baseline_provenance(
    netis_data(
      data.frame(group = "1", node = c("a", "b", "c", "d"), y = 1:4),
      netis_ties(bare), wave_col = "wave",
      tie_meta = list(weight = list(zero_meaning = "structural_absence")),
      observed_tie_waves = data.frame(group = "1", wave = "2")
    ), "y"
  ))$waves), c("1", "2"))
})

test_that("a declaration that cannot apply is refused", {
  nodes <- data.frame(group = 1, node = c("a", "b"), y = 1:2)
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  expect_error(netis_data(nodes, ties, wave_col = "wave",
                          observed_tie_waves = data.frame(group = 1, wave = 2)),
               "no 'wave' column")
  expect_error(netis_data(nodes, ties, observed_tie_waves = data.frame(group = 1, wave = 2)),
               "no wave_col")
  wties <- transform(ties, wave = 1)
  expect_error(netis_data(nodes, wties, wave_col = "wave",
                          observed_tie_waves = data.frame(group = 9, wave = 2)),
               "no node rows")
  expect_error(netis_data(nodes, wties, wave_col = "wave", observed_tie_waves = "2"),
               "must be a data frame")
})
