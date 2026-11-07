# What a Selection design covers: which actors, which cells, which predictor
# types, and which sampler controls reach the fit.

.scope_data <- function(n = 6L, waves = 2L, seed = 1L, extra_nodes = NULL) {
  set.seed(seed)
  ids <- sprintf("n%02d", seq_len(n))
  nodes <- data.frame(group = "g", node = rep(ids, waves),
                      wave = rep(seq_len(waves), each = n),
                      x = stats::rnorm(n * waves), stringsAsFactors = FALSE)
  pairs <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$source != pairs$target, ]
  ties <- do.call(rbind, lapply(seq_len(waves), function(w) {
    data.frame(group = "g", pairs, wave = w,
               tie = stats::rbinom(nrow(pairs), 1, 0.35),
               w = stats::rnorm(nrow(pairs)), stringsAsFactors = FALSE)
  }))
  list(nodes = nodes, ties = ties, ids = ids)
}

.scope_nd <- function(d, ...) {
  netis_data(d$nodes, d$ties, wave_col = "wave",
             tie_meta = list(tie = list(zero_meaning = "measured_zero"),
                             w = list(zero_meaning = "measured_zero")), ...)
}

.scope_fit <- function(formula, nd, ...) {
  suppressWarnings(fit_selection(formula, nd, nscan = 40, burn = 10, odens = 5, ...))
}

# --- orientation --------------------------------------------------------------

test_that("every design block has the orientation its role declares, cell by cell", {
  nodes <- data.frame(group = "g", node = rep(c("a", "b", "c"), 2), wave = rep(1:2, each = 3),
                      x = c(1, 2, 3, 10, 20, 30), stringsAsFactors = FALSE)
  pairs <- data.frame(source = c("a", "a", "b", "b", "c", "c"),
                      target = c("b", "c", "a", "c", "a", "b"), stringsAsFactors = FALSE)
  ties <- rbind(
    data.frame(group = "g", pairs, wave = 1, tie = c(1, 0, 0, 0, 0, 0), w = 11:16),
    data.frame(group = "g", pairs, wave = 2, tie = c(0, 0, 0, 1, 1, 0), w = 21:26)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave",
                   tie_meta = list(tie = list(zero_meaning = "measured_zero"),
                                   w = list(zero_meaning = "measured_zero")))
  spec <- .parse_selection_formula(
    tie ~ sender(x) + receiver(x, lag = 1) + dyad(w) + prior_tie() + prior_reciprocity()
  )
  route <- .resolve_selection_waves(spec, netis_schema(nd))
  asm <- .assemble_selection_group(spec, nd, netis_schema(nd), "g", route, NULL)$stacked
  ids <- c("a", "b", "c")

  # The outcome is wave 2, row = source, column = target.
  expect_identical(asm$Y["b", "c"], 1)
  expect_identical(asm$Y["c", "b"], 0)
  expect_identical(asm$Y["c", "a"], 1)
  expect_identical(asm$Y["a", "c"], 0)
  # A sender column reads the outcome wave; a lagged receiver column reads wave 1.
  expect_identical(unname(asm$Xrow[ids, 1]), c(10, 20, 30))
  expect_identical(unname(asm$Xcol[ids, 1]), c(1, 2, 3))
  # A dyadic layer keeps source and target apart.
  expect_identical(asm$Xdyad["a", "b", "dyd_w"], 21)
  expect_identical(asm$Xdyad["b", "a", "dyd_w"], 23)
  expect_identical(asm$Xdyad["c", "b", "dyd_w"], 26)
  # prior_tie reads the same dyad at wave 1; reciprocity reads its reverse.
  rec <- spec$prior_reciprocity$name
  expect_identical(asm$Xdyad["a", "b", "prior_tie"], 1)
  expect_identical(asm$Xdyad["b", "a", "prior_tie"], 0)
  expect_identical(asm$Xdyad["b", "a", rec], 1)
  expect_identical(asm$Xdyad["a", "b", rec], 0)
})

# --- self-ties ---------------------------------------------------------------

test_that("permitted self-ties leave the fit, are reported, and are not counted", {
  skip_if_not_installed("amen")
  d <- .scope_data(n = 6L, waves = 2L, seed = 3L)
  with_self <- d
  # Self-ties at both waves, with values that differ from any off-diagonal rule.
  selfs <- do.call(rbind, lapply(1:2, function(w) {
    data.frame(group = "g", source = d$ids, target = d$ids, wave = w, tie = 1,
               w = 9, stringsAsFactors = FALSE)
  }))
  with_self$ties <- rbind(d$ties, selfs)
  f_self <- .scope_fit(tie ~ prior_tie() + dyad(w), .scope_nd(with_self, self_ties = "permitted"))
  f_none <- .scope_fit(tie ~ prior_tie() + dyad(w), .scope_nd(d))

  # The fit is the fit of the same data with no self-ties at all.
  expect_identical(f_self$per_group[[1]]$draws, f_none$per_group[[1]]$draws)
  expect_true(all(is.na(diag(f_self$designs[[1]]$Y))))
  expect_identical(unname(diag(f_self$designs[[1]]$Xdyad[, , "prior_tie"])), rep(0, 6))
  expect_identical(unname(diag(f_self$designs[[1]]$Xdyad[, , "dyd_w"])), rep(0, 6))

  # 6 actors: 30 dyads, not 36.
  expect_identical(selection_effect(f_self, "prior_tie")$n_dyads, 30L)
  expect_identical(netis_glance(f_self)$nobs, 30L)
  expect_identical(nobs(f_self), 30L)

  codes <- netis_diagnostics(f_self)$code
  expect_true("selection_self_ties_not_modeled" %in% codes)
  msg <- netis_diagnostics(f_self)$detail[codes == "selection_self_ties_not_modeled"]
  expect_match(msg, "^6 observed self-tie")
  expect_false("selection_self_ties_not_modeled" %in% netis_diagnostics(f_none)$code)
})

test_that("the small-group finding counts dyads without the diagonal", {
  d <- .scope_data(n = 5L, waves = 1L)
  selfs <- data.frame(group = "g", source = d$ids, target = d$ids, wave = 1, tie = 0,
                      w = 0, stringsAsFactors = FALSE)
  d$ties <- rbind(d$ties, selfs)
  found <- .check_selection_group_sizes(.scope_nd(d, self_ties = "permitted"),
                                        netis_schema(.scope_nd(d, self_ties = "permitted")))
  expect_match(found[[1]]$message, "g \\(5 actors, 20 dyads\\)")
})

# --- predictor types ---------------------------------------------------------

test_that("a factor or text covariate is refused by name, not used as codes", {
  d <- .scope_data(n = 6L, waves = 1L)
  d$nodes$f <- factor(rep(c("hi", "lo"), 3L))
  d$nodes$txt <- rep(c("a", "b", "c"), 2L)
  d$ties$kind <- factor(rep(c("u", "v"), length.out = nrow(d$ties)))
  nd <- netis_data(d$nodes, d$ties, wave_col = "wave",
                   tie_meta = list(tie = list(zero_meaning = "measured_zero"),
                                   kind = list(zero_meaning = "measured_zero")))
  expect_error(.scope_fit(tie ~ sender(f), nd), "sender\\(f\\) needs a numeric variable, but 'f' is a factor")
  expect_error(.scope_fit(tie ~ receiver(f), nd), "receiver\\(f\\) needs a numeric variable")
  expect_error(.scope_fit(tie ~ sender(txt), nd), "'txt' is of class 'character'")
  expect_error(.scope_fit(tie ~ dyad(kind), nd), "dyad\\(kind\\) needs a numeric variable")
  expect_error(.scope_fit(kind ~ sender(x), nd), "Selection outcome needs a numeric variable")
})

test_that("a logical covariate is a 0/1 indicator", {
  skip_if_not_installed("amen")
  d <- .scope_data(n = 6L, waves = 1L)
  d$nodes$flag <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  fit <- .scope_fit(tie ~ sender(flag), .scope_nd(d))
  expect_identical(unname(fit$designs[[1]]$Xrow[d$ids, "snd_flag"]), c(1, 0, 1, 1, 0, 0))
})

# --- the waves a fit reads ---------------------------------------------------

test_that("a roster change at a wave the fit does not read is not a fault of the fit", {
  skip_if_not_installed("amen")
  d <- .scope_data(n = 7L, waves = 3L, seed = 5L)
  gone <- "n07"
  # Absent at wave 1 only.
  a <- d
  a$nodes <- a$nodes[!(a$nodes$wave == 1 & a$nodes$node == gone), ]
  a$ties <- a$ties[!(a$ties$wave == 1 & (a$ties$source == gone | a$ties$target == gone)), ]
  fit <- .scope_fit(tie ~ prior_tie(), .scope_nd(a), waves = c(2, 3))
  expect_identical(fit$actor_ids[[1]], d$ids)
  # The same pair reaching back over the change is refused, naming the actor.
  expect_error(.scope_fit(tie ~ prior_tie(), .scope_nd(a), waves = c(1, 3)),
               "present at some of these waves but not others: n07")

  # Seen at wave 3 only: not on the roster of a fit over waves 1 and 2.
  b <- d
  b$nodes <- b$nodes[!(b$nodes$wave < 3 & b$nodes$node == gone), ]
  b$ties <- b$ties[!(b$ties$wave < 3 & (b$ties$source == gone | b$ties$target == gone)), ]
  fit2 <- .scope_fit(tie ~ prior_tie(), .scope_nd(b), waves = c(1, 2))
  expect_identical(fit2$actor_ids[[1]], setdiff(d$ids, gone))
  expect_identical(nrow(fit2$designs[[1]]$Y), 6L)
  expect_identical(netis_glance(fit2)$nobs, 30L)
})

test_that("a tie at a read wave for an actor with no node row there is refused", {
  d <- .scope_data(n = 6L, waves = 3L, seed = 8L)
  gone <- "n06"
  d$nodes <- d$nodes[!(d$nodes$wave < 3 & d$nodes$node == gone), ]
  # Ties at waves 1 and 2 still name n06.
  expect_error(.scope_fit(tie ~ prior_tie(), .scope_nd(d), waves = c(1, 2)),
               "no node row at the waves this fit reads: .*n06")
})

test_that("a relation identical at the two waves read is refused as its own predictor", {
  d <- .scope_data(n = 6L, waves = 3L, seed = 9L)
  d$ties$tie[d$ties$wave == 2] <- d$ties$tie[d$ties$wave == 1]
  expect_error(.scope_fit(tie ~ prior_tie(), .scope_nd(d), waves = c(1, 2)),
               "identical at waves '1' and '2'")
  # The other transition is fine, and so is delayed reciprocity on this one.
  skip_if_not_installed("amen")
  expect_s3_class(.scope_fit(tie ~ prior_tie(), .scope_nd(d), waves = c(2, 3)), "netis_selection")
  expect_s3_class(.scope_fit(tie ~ prior_reciprocity(), .scope_nd(d), waves = c(1, 2)),
                  "netis_selection")
})

# --- sampler controls --------------------------------------------------------

test_that("amen arguments that change the model are refused", {
  nd <- .scope_nd(.scope_data())
  for (a in list(list(rvar = FALSE), list(cvar = FALSE), list(dcor = FALSE),
                 list(nvar = FALSE), list(intercept = FALSE), list(prior = list(g = 1)),
                 list(print = TRUE), list(chains = 2))) {
    expect_error(
      do.call(fit_selection, c(list(tie ~ prior_tie(), nd, nscan = 40, burn = 10, odens = 5), a)),
      sprintf("given %s", names(a))
    )
  }
})

test_that("chain settings that keep too few draws are refused on both routes", {
  nd <- .scope_nd(.scope_data())
  expect_error(fit_selection(tie ~ prior_tie(), nd, nscan = 5, burn = 20, odens = 10),
               "keep 0 posterior draw")
  expect_error(fit_selection(tie ~ prior_tie(), nd, nscan = 0), "nscan must be a whole number")
  expect_error(fit_selection(tie ~ prior_tie(), nd, odens = 0.5), "odens must be a whole number")
  expect_error(fit_selection(tie ~ prior_tie(), nd, burn = -1), "burn must be a whole number")

  d <- .scope_data()
  d2 <- d
  d2$nodes$group <- "h"
  d2$ties$group <- "h"
  nd2 <- netis_data(rbind(d$nodes, d2$nodes), rbind(d$ties, d2$ties), wave_col = "wave",
                    tie_meta = list(tie = list(zero_meaning = "measured_zero"),
                                    w = list(zero_meaning = "measured_zero")))
  pool <- selection_pooling(random = "intercept")
  expect_error(fit_selection(tie ~ prior_tie(), nd2, pooling = pool, nscan = 5, burn = 20, odens = 10),
               "keep 0 posterior draw")
  expect_error(fit_selection(tie ~ prior_tie(), nd2, pooling = pool, chains = 0),
               "chains must be a whole number")
})

test_that("a constant moderator is refused however it is scaled", {
  z_one <- matrix(1, 3, 1, dimnames = list(c("a", "b", "c"), "flag"))
  expect_error(.selection_moderator_scale(z_one), "flag do not vary across groups")
  z_seven <- matrix(7, 3, 1, dimnames = list(c("a", "b", "c"), "size"))
  expect_error(.selection_moderator_scale(z_seven), "size do not vary")
  expect_error(.selection_moderator_scale(z_seven, declared = c(size = 2)), "size do not vary")
  z_ok <- matrix(c(0, 1, 0), 3, 1, dimnames = list(c("a", "b", "c"), "flag"))
  expect_identical(.selection_moderator_scale(z_ok)$delta_Z[["flag"]], 1)
})

# --- a dyadic correlation that would start at its bound -------------------------

.three_actor_y <- function(m) {
  Y <- matrix(NA_real_, 3, 3)
  Y[row(Y) != col(Y)] <- as.integer(intToBits(m))[1:6]
  Y
}

test_that("every three-actor network amen fails on with a bad rho is refused beforehand", {
  skip_if_not_installed("amen")
  x <- matrix(c(-1, 0.3, 0.9), ncol = 1, dimnames = list(NULL, "x"))
  crashed <- predicted <- logical(0)
  for (m in 1:62) {
    Y <- .three_actor_y(m)
    if (.amen_regular_degree(Y)) next
    design <- list(Y = Y, Xrow = x)
    out <- tryCatch({
      utils::capture.output(amen::ame(Y, Xrow = x, family = "bin", nscan = 10, burn = 0,
                                      odens = 5, print = FALSE, plot = FALSE, gof = FALSE))
      "ok"
    }, error = function(e) conditionMessage(e), warning = function(w) "warned")
    fail <- grepl("missing value", out)
    crashed <- c(crashed, fail)
    predicted <- c(predicted, .amen_start_rho_at_bound(design, "bin"))
  }
  expect_gt(sum(crashed), 0L)
  # No crash goes unpredicted.
  expect_false(any(crashed & !predicted))
  # It is a condition of some three-actor designs, not of all of them.
  expect_true(any(!predicted))
})

test_that("a sampler failure on rho says what is known, and others pass through", {
  m <- .selection_backend_failure("g", 3L, "missing value where TRUE/FALSE needed")
  expect_match(m, "^Fitting group 'g' failed: missing value")
  expect_match(m, "group of 3 actors")
  expect_match(m, "selection_pooling")
  expect_identical(.selection_backend_failure("g", 3L, "other"), "Fitting group 'g' failed: other")
})

test_that("a larger group's correlation does not start at the bound", {
  set.seed(4)
  Y <- matrix(stats::rbinom(36, 1, 0.4), 6, 6)
  diag(Y) <- NA
  expect_false(.amen_start_rho_at_bound(list(Y = Y), "bin"))
})

test_that("fit_selection() names the group and the way forward; the hierarchical sampler runs it", {
  skip_if_not_installed("amen")
  x <- c(-1, 0.3, 0.9)
  hit <- NULL
  for (m in 1:62) {
    Y <- .three_actor_y(m)
    if (.amen_regular_degree(Y)) next
    if (.amen_start_rho_at_bound(list(Y = Y, Xrow = matrix(x, ncol = 1)), "bin")) {
      hit <- Y
      break
    }
  }
  ids <- c("a", "b", "c")
  dimnames(hit) <- list(ids, ids)
  cells <- which(row(hit) != col(hit), arr.ind = TRUE)
  ties <- data.frame(group = "small", source = ids[cells[, 1]], target = ids[cells[, 2]],
                     tie = hit[cells], stringsAsFactors = FALSE)
  nodes <- data.frame(group = "small", node = ids, x = x, stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties, tie_meta = list(tie = list(zero_meaning = "measured_zero")))
  expect_error(.scope_fit(tie ~ sender(x), nd), "Group 'small' has 3 actors.*\\+/-1.*no seed")

  big <- .scope_data(n = 7L, waves = 1L, seed = 2L)
  nodes2 <- rbind(nodes, big$nodes[big$nodes$wave == 1, c("group", "node", "x")])
  ties2 <- rbind(ties, big$ties[big$ties$wave == 1, c("group", "source", "target", "tie")])
  nd2 <- netis_data(nodes2, ties2, tie_meta = list(tie = list(zero_meaning = "measured_zero")))
  fit <- .scope_fit(tie ~ sender(x), nd2, pooling = selection_pooling(random = "intercept"))
  expect_s3_class(fit, "netis_selection")
})

# --- the backend reference ---------------------------------------------------

test_that("fit_selection() on a symmetric relation reproduces amen::ame() draw for draw", {
  skip_if_not_installed("amen")
  d <- .scope_data(n = 7L, waves = 1L, seed = 12L)
  d$ties <- d$ties[d$ties$source < d$ties$target, ]
  nd <- netis_data(d$nodes, d$ties, wave_col = "wave", directed = FALSE,
                   tie_meta = list(tie = list(zero_meaning = "measured_zero"),
                                   w = list(zero_meaning = "measured_zero")))
  fit <- suppressWarnings(fit_selection(tie ~ sender(x), nd, symmetric = TRUE,
                                        nscan = 40, burn = 10, odens = 5, seed = 4))
  Y <- matrix(NA_real_, 7, 7, dimnames = list(d$ids, d$ids))
  Y[cbind(d$ties$source, d$ties$target)] <- d$ties$tie
  Y[cbind(d$ties$target, d$ties$source)] <- d$ties$tie
  x <- d$nodes$x[match(d$ids, d$nodes$node)]
  invisible(utils::capture.output(ref <- amen::ame(
    Y, Xrow = matrix(x, ncol = 1, dimnames = list(d$ids, "nod_x")),
    family = "bin", symmetric = TRUE, nscan = 40, burn = 10, odens = 5, seed = 4,
    print = FALSE, plot = FALSE
  )))
  expect_identical(unname(fit$per_group[[1]]$draws), unname(ref$BETA))
})
