# Influence front end and estimation: known cells on asymmetric fixtures.

.ia_data <- function(n = 12L, seed = 3L, na_x = integer(0), groups = 1L) {
  set.seed(seed)
  frames <- lapply(seq_len(groups), function(g) {
    ids <- sprintf("g%dn%02d", g, seq_len(n))
    list(
      nodes = data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = n),
                         y = stats::rnorm(2 * n), x = stats::rnorm(2 * n),
                         z = stats::rnorm(2 * n)),
      ties = data.frame(group = g, source = rep(ids[-n], 2), target = rep(ids[-1], 2),
                        wave = rep(1:2, each = n - 1L), tie = 1)
    )
  })
  nodes <- do.call(rbind, lapply(frames, `[[`, "nodes"))
  ties <- do.call(rbind, lapply(frames, `[[`, "ties"))
  nodes$x[nodes$wave == 2][na_x] <- NA
  netis_data(nodes, ties, wave_col = "wave")
}

# ---- influence_effect() ----------------------------------------------------

test_that("influence_effect() contrasts are quantiles of the rows the fit used", {
  nd <- .ia_data(na_x = c(1, 2, 3, 4))
  fit <- fit_influence(y ~ ar() + exposure(y, via = tie) + x, data = nd)
  mf <- fit$model_frame
  used <- mf[stats::complete.cases(mf[c(".outcome", "y_lag1", "ex_y_tie", "x")]), ]
  expect_lt(nrow(used), nrow(mf))
  eff <- influence_effect(fit)$exposures$ex_y_tie
  expected <- stats::quantile(used$ex_y_tie, c(0.1, 0.9), names = FALSE)
  expect_identical(c(eff$contrast_low, eff$contrast_high), expected)
  # The dropped rows do matter on this fixture: all rows give another range.
  expect_false(isTRUE(all.equal(expected, stats::quantile(mf$ex_y_tie, c(0.1, 0.9), names = FALSE))))
})

test_that("a moderator's reported values are quantiles of the rows the fit used", {
  nd <- .ia_data(na_x = c(1, 2, 3))
  fit <- fit_influence(y ~ ar() * z + exposure(y, via = tie) * z + x, data = nd)
  mf <- fit$model_frame
  used <- mf[stats::complete.cases(mf[setdiff(names(mf), c("group", "node"))]), ]
  z_q <- stats::quantile(used$z, c(0.1, 0.9), names = FALSE)
  eff <- influence_effect(fit)
  expect_identical(eff$exposures$ex_y_tie$conditional$z, c(0, z_q))
  expect_identical(eff$ar$y_lag1$conditional$z, c(0, z_q))
})

test_that("influence_effect() on a simultaneous fit is labeled coefficient-scale", {
  skip_if_not_installed("sna")
  nd <- .ia_data(n = 15L)
  fit <- suppressWarnings(fit_influence(
    y ~ exposure(y, via = tie, x_lag = 0) + exposure(x, via = tie, name = "ex_x", x_lag = 0) + z,
    data = nd
  ))
  eff <- influence_effect(fit)
  # Beta times the range, under a name that does not read as an outcome change.
  e <- eff$exposures$ex_x
  expect_identical(e$coefficient_times_contrast,
                   unname(coef(fit)["ex_x"]) * (e$contrast_high - e$contrast_low))
  expect_null(e$predicted_change)
  expect_identical(e$scale, "coefficient")
  expect_identical(eff$scale, "coefficient")
  expect_true(isTRUE(eff$excludes_network_feedback))
  expect_match(eff$scope_note, "(I - rho * W1)^-1", fixed = TRUE)
  expect_match(eff$scope_note, "not the change in the outcome", fixed = TRUE)
  # The same-outcome term is rho, and has no entry.
  expect_identical(names(eff$exposures), "ex_x")

  out <- paste(capture.output(print(eff)), collapse = " ")
  expect_match(out, "excludes the network feedback", fixed = TRUE)
  expect_match(out, sprintf("coefficient times contrast %.4f", e$coefficient_times_contrast),
               fixed = TRUE)
  expect_false(grepl("predicted change|effect size|direct effect", out, ignore.case = TRUE))

  # The figure draws the same number and carries the same label.
  d <- plot_data(fit, what = "effects")
  expect_lt(abs(d$value[d$term == "ex_x"] - e$coefficient_times_contrast), 1e-12)
  caption <- paste(attr(d, "netis_caption"), collapse = " ")
  expect_match(caption, "excludes the network feedback", fixed = TRUE)
  expect_false(grepl("predicted change|direct effect", caption, ignore.case = TRUE))
  p <- plot(fit, what = "effects")
  expect_match(p$labels$x, "excludes network feedback", fixed = TRUE)
  expect_match(p$labels$caption, "not a change in the outcome", fixed = TRUE)

  # And nothing is said on a route with no feedback to exclude.
  fit_plain <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
  plain <- influence_effect(fit_plain)
  expect_false(isTRUE(plain$excludes_network_feedback))
  expect_null(plain$scope_note)
  expect_identical(plain$scale, "outcome")
  expect_true(is.numeric(plain$exposures$ex_x_tie$predicted_change))
  out <- capture.output(print(plain))
  expect_false(any(grepl("feedback", out)))
  expect_identical(plot(fit_plain, what = "effects")$labels$x, "predicted change in the outcome")
})

test_that("printing influence_effect() reports the numbers it holds", {
  nd <- .ia_data()
  fit <- fit_influence(y ~ ar() + exposure(x, via = tie) * z, data = nd)
  eff <- influence_effect(fit)
  out <- paste(capture.output(print(eff)), collapse = "\n")
  cond <- eff$exposures$ex_x_tie$conditional
  expect_match(out, sprintf("%.4f", cond$estimate[2]), fixed = TRUE)
  expect_match(out, "z = 0 (reference)", fixed = TRUE)
  expect_match(out, "Johnson-Neyman", fixed = TRUE)
  fit2 <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
  e2 <- influence_effect(fit2)
  out2 <- paste(capture.output(print(e2)), collapse = "\n")
  expect_match(out2, sprintf("%.4f", e2$exposures$ex_x_tie$predicted_change), fixed = TRUE)
})

# ---- the fit object --------------------------------------------------------

test_that("an Influence fit carries no always-empty fields", {
  fit <- fit_influence(y ~ ar() + exposure(x, via = tie), data = .ia_data())
  expect_false(any(c("effects", ".cache") %in% names(fit)))
  g <- netis_glance(fit)
  expect_false(any(c("reliability", "repeated_units") %in% names(g)))
  expect_identical(g$n_transitions, 1L)
  e <- netis_estimand(fit)
  expect_false(any(c("repeated_units", "dependence_handled_by") %in% names(e)))
  expect_false(any(c(".transition", ".from_wave", ".to_wave") %in% names(fit$model_frame)))
})

test_that("a cross-section reports no transition in netis_glance()", {
  nodes <- data.frame(group = 1, node = letters[1:8], y = stats::rnorm(8), x = stats::rnorm(8))
  nd <- netis_data(nodes, data.frame(group = 1, source = letters[1:7], target = letters[2:8], tie = 1))
  expect_identical(netis_glance(fit_influence(y ~ x, data = nd))$n_transitions, 0L)
})

# ---- residual dependence ---------------------------------------------------

test_that("within-unit serial correlation is a plain not-applicable on every route", {
  # Two waves, named waves of three, several groups: each gives one outcome row
  # per unit, so no sequence exists for the statistic to run along.
  nd <- .ia_data()
  fit <- fit_influence(y ~ ar() + exposure(y, via = tie), data = nd)
  expect_identical(anyDuplicated(fit$model_frame$node), 0L)
  dep <- netis_dependence(fit, nd)
  expect_identical(dep$serial$status, "not_applicable")
  expect_identical(names(dep$serial), c("status", "reason"))
  expect_true(any(grepl("serial correlation: not applicable", format(dep))))

  multi <- .ia_data(groups = 3L)
  fm <- suppressWarnings(fit_influence(y ~ ar() + exposure(y, via = tie) + (1 | group), data = multi))
  mf <- fm$model_frame
  expect_identical(anyDuplicated(paste(mf$group, mf$node)), 0L)
  expect_identical(netis_dependence(fm, multi)$serial$status, "not_applicable")

  set.seed(1)
  three <- netis_data(
    data.frame(group = 1, node = rep(letters[1:8], 3), wave = rep(1:3, each = 8), y = stats::rnorm(24)),
    data.frame(group = 1, source = rep(letters[1:7], 3), target = rep(letters[2:8], 3),
               wave = rep(1:3, each = 7), tie = 1),
    wave_col = "wave"
  )
  f3 <- fit_influence(y ~ ar() + exposure(y, via = tie), data = three, waves = c(1, 3))
  expect_identical(anyDuplicated(f3$model_frame$node), 0L)
  expect_identical(netis_dependence(f3, three)$serial$status, "not_applicable")
  expect_false(exists(".dependence_serial", envir = asNamespace("rnetis"), inherits = FALSE))
})

test_that("the network-exposure note names the term's actual timing", {
  skip_if_not_installed("sna")
  nd <- .ia_data(n = 15L)
  note <- function(f) {
    d <- netis_dependence(suppressWarnings(fit_influence(f, data = nd)), nd)$diagnostics
    vapply(Filter(function(x) identical(x$code, "network_exposure_route_present"), d),
           function(x) x$message %||% x$summary, character(1))
  }
  sar <- note(y ~ exposure(y, via = tie, x_lag = 0) + x)
  expect_match(sar, "^A contemporaneous exposure to the outcome")
  expect_false(grepl("lagged exposure", sar))
  lag <- note(y ~ ar() + exposure(y, via = tie))
  expect_match(lag, "^A lagged exposure to the outcome")
})

# ---- exposure --------------------------------------------------------------

test_that("an isolate under row normalization is NA whatever its alters' content", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), x = c(0, 0, 0))
  nd <- netis_data(nodes, data.frame(group = 1, source = "a", target = "b", w = 1))
  out <- compute_exposure(nd, "x", "w", normalize = "row")
  # b has one alter with content 0: exposure 0. a and c have no alters: undefined.
  expect_identical(out$ex_x_w[out$node == "b"], 0)
  expect_true(is.na(out$ex_x_w[out$node == "a"]))
  expect_true(is.na(out$ex_x_w[out$node == "c"]))
})

test_that("a row whose signed weights sum to zero is undefined, not an isolate", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c", "d"), x = c(1, 2, 4, 8))
  # c receives +1 from a and -1 from b; d receives nothing.
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("c", "c"), w = c(1, -1))
  nd <- netis_data(nodes, ties)
  zero <- compute_exposure(nd, "x", "w", normalize = "row", isolate_rule = "zero")
  expect_true(is.na(zero$ex_x_w[zero$node == "c"]))
  expect_identical(zero$ex_x_w[zero$node == "d"], 0)
  # "error" refuses isolates by name; c is not one of them.
  err <- tryCatch(compute_exposure(nd, "x", "w", normalize = "row", isolate_rule = "error"),
                  error = conditionMessage)
  expect_match(err, "2|3")
  expect_false(grepl(": .*\\bc\\b", err))
})

test_that("compute tables hold identifiers in the input's type", {
  nodes <- data.frame(group = c(1L, 1L, 2L, 2L), node = c(10, 9, 3, 4), x = c(1, 2, 3, 4))
  ties <- data.frame(group = c(1L, 2L), source = c(10, 3), target = c(9, 4), w = 1)
  nd <- netis_data(nodes, ties)
  ex <- compute_exposure(nd, "x", "w")
  expect_type(ex$group, "integer")
  expect_type(ex$node, "double")
  expect_identical(ex$ex_x_w[ex$node == 9], 1)
  cent <- compute_centrality(nd, w, measure = "degree", mode = "in", name = "d")
  expect_type(cent$group, "integer")
  expect_identical(cent$d[cent$node == 4], 1)
})

test_that("two multi-column mechanisms get their own columns, or are refused", {
  set.seed(9)
  ids <- letters[1:10]
  nodes <- data.frame(group = 1, node = rep(ids, 3), wave = rep(1:3, each = 10),
                      y = stats::rnorm(30), a = stats::rnorm(30), b = stats::rnorm(30))
  ties <- data.frame(group = 1, source = rep(ids[-10], 3), target = rep(ids[-1], 3),
                     wave = rep(1:3, each = 9), tie = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  m <- bi_components(alpha = 0.5, gamma = 0.2)
  expect_error(
    fit_influence(y ~ ar() + exposure(a, via = tie, mechanism = m) + exposure(b, via = tie, mechanism = m),
                  data = nd, waves = c(2, 3)),
    "same column name.*A_maxpath"
  )
  fit <- fit_influence(
    y ~ ar() + exposure(a, via = tie, mechanism = m, name = "ea") +
      exposure(b, via = tie, mechanism = m, name = "eb"),
    data = nd, waves = c(2, 3)
  )
  expect_true(all(c("ea_A_maxpath", "ea_B_redundant", "eb_A_maxpath", "eb_B_redundant") %in%
                    names(coef(fit))))
  # The columns are the computation's, under the requested names.
  direct <- compute_exposure(nd, "a", "tie", wave = 3, mechanism = m)
  mf <- fit$model_frame
  expect_identical(mf$ea_A_maxpath[match(direct$node, mf$node)], direct$A_maxpath)
  expect_error(
    fit_influence(y ~ ar() + exposure(a, via = tie, mechanism = m) * b, data = nd, waves = c(2, 3)),
    "several columns.*cannot be moderated"
  )
})

test_that("an exposure term cannot take the name of a key column", {
  nd <- .ia_data()
  for (nm in c("group", "node", ".outcome")) {
    expect_error(
      fit_influence(y ~ ar() + exposure(x, via = tie, name = nm), data = nd),
      "reserved",
      info = nm
    )
  }
})

test_that("a custom kernel bound to two names records no guessed name", {
  kern <- function(context, spec) as.numeric(context$W %*% context$X)
  alias <- kern
  res <- .resolve_mechanism(kern)
  expect_identical(res$provenance$type, "anonymous")
  only <- local({
    one <- function(context, spec) as.numeric(context$W %*% context$X)
    .resolve_mechanism(one)
  })
  expect_identical(only$provenance$name, "one")
})

test_that("public term constructors give direct errors", {
  expect_error(exposure(x), "needs `via`")
  nd <- .ia_data()
  expect_error(compute_exposure(nd, c("y", "x"), "tie", wave = 1), "single node variable name")
  # A quoted name is the same name.
  f1 <- fit_influence(y ~ ar() + exposure("x", via = "tie"), data = nd)
  f2 <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
  expect_identical(coef(f1), coef(f2))
})

test_that("summary() of a bi_components() term omits the delta it does not take", {
  set.seed(2)
  ids <- letters[1:8]
  nd <- netis_data(
    data.frame(group = 1, node = rep(ids, 3), wave = rep(1:3, each = 8), y = stats::rnorm(24),
               p = stats::rnorm(24)),
    data.frame(group = 1, source = rep(ids[-8], 3), target = rep(ids[-1], 3),
               wave = rep(1:3, each = 7), tie = 1),
    wave_col = "wave"
  )
  fit <- fit_influence(y ~ ar() + exposure(p, via = tie, mechanism = bi_components(alpha = 0.5, gamma = 0.2)),
                       data = nd, waves = c(2, 3))
  out <- capture.output(print(summary(fit)))
  expect_true(any(grepl("bi_components(alpha = 0.5, gamma = 0.2)", out, fixed = TRUE)))
  expect_false(any(grepl("delta = NA", out, fixed = TRUE)))
})

test_that("bi() with x_lag = 0 on the outcome gets the timing refusal", {
  nd <- .ia_data()
  expect_error(
    fit_influence(y ~ exposure(y, via = tie, x_lag = 0, mechanism = bi(0.5, 1, 0)) + x, data = nd),
    "x_lag = 0 has no reading"
  )
})

# ---- outcomes, sample and findings -----------------------------------------

test_that("a non-numeric outcome is refused and a 0/1 outcome is named", {
  nd <- .ia_data()
  nodes <- netis_nodes(nd)
  ties <- netis_ties(nd)
  fac <- nodes; fac$y <- factor(ifelse(fac$y > 0, "hi", "lo"))
  expect_error(fit_influence(y ~ ar() + x, data = netis_data(fac, ties, wave_col = "wave")),
               "outcome 'y' is factor")
  lgl <- nodes; lgl$y <- lgl$y > 0
  expect_error(fit_influence(y ~ ar() + x, data = netis_data(lgl, ties, wave_col = "wave")),
               "outcome 'y' is logical")
  bin <- nodes; bin$y <- as.numeric(bin$y > 0)
  fit <- fit_influence(y ~ ar() + x, data = netis_data(bin, ties, wave_col = "wave"))
  codes <- vapply(fit$diagnostics, function(f) f$code, character(1))
  expect_true("binary_outcome_fitted_as_gaussian" %in% codes)
  cont <- fit_influence(y ~ ar() + x, data = nd)
  expect_false("binary_outcome_fitted_as_gaussian" %in%
                 vapply(cont$diagnostics, function(f) f$code, character(1)))
})

test_that("a fit with no complete row says which column emptied it", {
  nodes <- data.frame(group = 1, node = rep(letters[1:6], 2), wave = rep(1:2, each = 6),
                      y = c(1:6, 2:7) + 0.5, x = 1:12)
  # The network is observed at wave 2 only, and the exposure reads wave 1.
  ties <- data.frame(group = 1, source = letters[1:5], target = letters[2:6], wave = 2, tie = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(
    suppressWarnings(fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)),
    "no row.*ex_x_tie"
  )
})

test_that("a group with no rows at the fitted waves is a recorded finding", {
  set.seed(5)
  mk <- function(g, waves, ids) {
    data.frame(group = g, node = rep(ids, length(waves)),
               wave = rep(waves, each = length(ids)), y = stats::rnorm(length(ids) * length(waves)))
  }
  nodes <- rbind(mk(1, 1:3, letters[1:6]), mk(2, 1:3, LETTERS[1:6]), mk(3, 1:2, letters[7:12]))
  ties <- rbind(
    data.frame(group = 1, source = letters[1:5], target = letters[2:6], tie = 1),
    data.frame(group = 2, source = LETTERS[1:5], target = LETTERS[2:6], tie = 1),
    data.frame(group = 3, source = letters[7:11], target = letters[8:12], tie = 1)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")
  fit <- suppressWarnings(fit_influence(y ~ ar() + (1 | group), data = nd, waves = c(2, 3)))
  f <- Filter(function(x) identical(x$code, "groups_absent_at_resolved_waves"), fit$diagnostics)
  expect_length(f, 1L)
  expect_identical(f[[1]]$details$absent, "3")
})

test_that("exposure lost to a missing alter value is a recorded finding", {
  nd <- .ia_data()
  nodes <- netis_nodes(nd)
  # g1n01 at wave 1 is the alter of g1n02 only.
  nodes$x[nodes$node == "g1n01" & nodes$wave == 1] <- NA
  nd2 <- netis_data(nodes, netis_ties(nd), wave_col = "wave")
  fit <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd2)
  f <- Filter(function(x) identical(x$code, "exposure_missing_via_alters"), fit$diagnostics)
  expect_length(f, 1L)
  expect_identical(f[[1]]$details$share_missing, 1 / 12)
})

test_that("lagging a static variable is a recorded note, as ?lagged says", {
  nd0 <- .ia_data()
  nodes <- netis_nodes(nd0)
  nodes$stat <- as.numeric(factor(nodes$node))
  nd <- netis_data(nodes, netis_ties(nd0), wave_col = "wave")
  fit <- fit_influence(y ~ ar() + lagged(stat), data = nd)
  codes <- vapply(fit$diagnostics, function(f) f$code, character(1))
  expect_true("lagged_static_variable" %in% codes)
})

test_that("network_stat() refusal no longer recommends fitting every transition", {
  nd <- .ia_data()
  err <- tryCatch(fit_influence(y ~ ar() + network_stat(tie, measure = "density", lag = 0), data = nd),
                  error = conditionMessage)
  expect_match(err, "no variation")
  expect_false(grepl("waves = \"all\"", err, fixed = TRUE))
})

# ---- robust variance -------------------------------------------------------

test_that("clustering by actor says what singleton clusters give", {
  skip_if_not_installed("clubSandwich")
  fit <- fit_influence(y ~ ar() + x, data = .ia_data())
  s <- influence_se_comparison(fit, by = "node", cr_type = "CR0")
  expect_match(attr(s, "addresses"), "one outcome row")
  expect_match(attr(s, "addresses"), "heteroskedasticity")
  expect_false(grepl("sequence", attr(s, "addresses")))
  # HC0, by hand.
  X <- stats::model.matrix(fit$backend_fit)
  e <- stats::residuals(fit$backend_fit)
  bread <- solve(crossprod(X))
  hc0 <- bread %*% crossprod(X * e) %*% bread
  expect_lt(max(abs(s$se_robust - sqrt(diag(hc0)))), 1e-12)
})

# ---- latent ----------------------------------------------------------------

test_that("latent() fits the selection model to the actors at the modeled wave", {
  # b has a row at wave 1 and c does not; d has no row at wave 1 but a tie there
  # reaches it, so it stays a vertex rather than the tie being dropped.
  nodes <- data.frame(group = 1, node = c("a", "b", "a", "b", "c", "d"),
                      wave = c(1, 1, 2, 2, 2, 2), y = c(1, 2, 3, 4, 6, 9))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("d", "b"), wave = c(1, 2), tie = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  seen <- NULL
  testthat::local_mocked_bindings(
    .latent_fit_positions = function(term, adjacency, node_attrs, directed = TRUE) {
      seen <<- adjacency
      Z <- matrix(seq_len(nrow(adjacency)), ncol = 1L, dimnames = list(rownames(adjacency), NULL))
      list(positions = Z, formula = "network ~ euclidean(d = 1)", n_actors = nrow(adjacency))
    },
    .package = "rnetis"
  )
  res <- .resolve_latent_positions(latent(tie, seed = 1), nd, netis_schema(nd), 1)
  expect_identical(rownames(seen), c("a", "b", "d"))
  expect_identical(seen[["a", "d"]], 1)
  expect_identical(res$n_actors, 3L)
  expect_false("c" %in% res$columns$node)
})

# ---- SAR W1 ----------------------------------------------------------------

test_that("a W1 tie to a node with no row at the wave is recorded as a removal", {
  skip_if_not_installed("sna")
  set.seed(4)
  ids <- sprintf("n%02d", 1:15)
  nodes <- rbind(
    data.frame(group = 1, node = ids, wave = 1, y = stats::rnorm(15), x = stats::rnorm(15)),
    data.frame(group = 1, node = ids[-15], wave = 2, y = stats::rnorm(14), x = stats::rnorm(14))
  )
  ties <- data.frame(group = 1, source = rep(ids[-15], 2), target = rep(ids[-1], 2),
                     wave = rep(1:2, each = 14), tie = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  fit <- suppressWarnings(fit_influence(y ~ exposure(y, via = tie, x_lag = 0) + x, data = nd))
  expect_identical(fit$dropped_nodes, "n15")
  expect_true(any(grepl("n15", capture.output(print(fit)), fixed = TRUE)))
})

test_that("a zero-sum row of W1 is refused rather than handed to lnam", {
  skip_if_not_installed("sna")
  W <- matrix(c(0, 1, -1, 0, 0, 0, 1, 1, 0), 3, 3, byrow = TRUE,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  expect_error(.lnam_normalize_w1(W, "row", "zero"), "sum to zero")
})

# ---- simulation ------------------------------------------------------------

test_that("simulate_influence() records a skipped burn-in", {
  expect_message(sim <- simulate_influence(n_nodes = 30, seed = 1), "no stationary distribution")
  expect_identical(sim$truth$burnin, 0L)
  expect_identical(sim$truth$burnin_requested, 50L)
  expect_gte(sim$truth$companion_radius, 1)
  ok <- simulate_influence(n_nodes = 20, beta = 0.1, normalize = "row", seed = 1)
  expect_identical(ok$truth$burnin, 50L)
  expect_lt(ok$truth$companion_radius, 1)
})

test_that("simulate_influence() records an empty drawn network as observed", {
  # At this density most waves draw few ties; some draw none.
  expect_silent(
    sim <- simulate_influence(n_nodes = 3, density = 0.05, beta = 0.1, n_waves = 12, normalize = "row",
                              isolate_rule = "zero", network_change = TRUE, seed = 2)
  )
  empty_wave <- setdiff(1:12, netis_ties(sim$data)$wave)[1]
  expect_false(is.na(empty_wave))
  # The fit reads what the generating process drew: no ties, so exposure 0.
  expect_silent(
    ex <- compute_exposure(sim$data, "y", "tie", wave = empty_wave, normalize = "row",
                           isolate_rule = "zero")
  )
  expect_identical(ex$ex_y_tie, c(0, 0, 0))
})

test_that("the covariate starts from its stationary distribution", {
  # Marginal variance of x is 1 (the level) + 1 (the AR(1) innovation) = 2 at
  # every wave, including the first, under a start with no burn-in.
  sim <- simulate_influence(n_nodes = 1500, density = 0.001, beta = 0.1, gamma = 0.2,
                            exposure_content = "covariate", initial_condition = "independent",
                            n_waves = 3, covariate_persistence = 0.5, seed = 11)
  nodes <- netis_nodes(sim$data)
  v <- tapply(nodes$x, nodes$wave, stats::var)
  expect_lt(abs(v[["1"]] - 2), 0.3)
  expect_lt(abs(v[["3"]] - 2), 0.3)
})
