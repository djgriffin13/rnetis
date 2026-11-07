# The multi-group latent() construction, checked through fit_influence() with
# known positions injected at the stage-one seam: the design and coefficients
# against a hand-built lmer()/lm(), invariance to each group's arbitrary frame,
# and the reduction to the single-network procedure.

.lmg_groups <- c(a = 14L, b = 19L, c = 11L, d = 25L, e = 16L)

# Unequal groups, directed ties, and a real group effect, so the fitted group
# variance is not on the boundary and the invariance is tested where the
# likelihood depends on it.
.lmg_frames <- function(seed = 11L) {
  set.seed(seed)
  shift <- c(a = -1.5, b = 0.5, c = 2, d = -0.5, e = 1)
  lapply(names(.lmg_groups), function(g) {
    n <- .lmg_groups[[g]]
    ids <- paste0(g, sprintf("%02d", seq_len(n)))
    A <- matrix(stats::rbinom(n^2, 1, 0.2), n, n)
    diag(A) <- 0L
    ij <- which(A == 1L, arr.ind = TRUE)
    list(
      nodes = data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = n),
                         y = stats::rnorm(2 * n) + rep(c(0, shift[[g]]), each = n),
                         x = stats::rnorm(2 * n), stringsAsFactors = FALSE),
      ties = data.frame(group = g, source = ids[ij[, 1]], target = ids[ij[, 2]],
                        wave = 1, tie = 1, stringsAsFactors = FALSE)
    )
  })
}

.lmg_data <- function(frames = .lmg_frames()) {
  netis_data(do.call(rbind, lapply(frames, `[[`, "nodes")),
             do.call(rbind, lapply(frames, `[[`, "ties")), wave_col = "wave")
}

# Known positions, different in every group and dimension, deliberately not centered.
.lmg_positions <- function(ids, d) {
  k <- as.numeric(sub("^[a-z]", "", ids))
  g <- utf8ToInt(substr(ids[1], 1, 1))
  Z <- vapply(seq_len(d), function(j) {
    sin(k * (1.3 + 0.7 * j) + g) + cos(k^2 / (3 + j) + j * g)
  }, numeric(length(ids)))
  Z <- matrix(Z, ncol = d)
  rownames(Z) <- ids
  Z
}

.lmg_fit <- function(d, transform = function(Z, g) Z, data = .lmg_data(), formula = NULL) {
  local_mocked_bindings(.latent_fit_positions = function(term, adjacency, node_attrs,
                                                         directed = TRUE) {
    ids <- rownames(adjacency)
    Z <- transform(.lmg_positions(ids, term$d), substr(ids[1], 1, 1))
    rownames(Z) <- ids
    list(positions = Z, formula = "stub", n_actors = length(ids))
  })
  f <- formula %||% stats::as.formula(sprintf(
    "y ~ ar() + x + exposure(y, via = tie, normalize = 'row') + latent(tie, d = %d, seed = 1) + (1 | group)",
    d
  ))
  suppressWarnings(suppressMessages(fit_influence(f, data = data)))
}

.lmg_colname <- function(d, k, g) {
  if (d == 1L) sprintf("lat_tie_1__%s", g) else sprintf("lat_tie_1_d%d__%s", k, g)
}

test_that("the stage-two design and fit equal a hand-built lmer on zero-padded centered columns", {
  nd <- .lmg_data()
  nodes <- netis_nodes(nd)
  ties <- netis_ties(nd)
  for (d in 1:2) {
    fit <- .lmg_fit(d, data = nd)
    mf <- fit$model_frame
    hand <- data.frame(group = mf$group, y = mf$.outcome, y1 = mf$y_lag1, x = mf$x,
                       stringsAsFactors = FALSE)
    # Row-normalized exposure by hand: the mean of the senders' wave-1 outcomes.
    hand$ex <- vapply(as.character(mf$node), function(i) {
      from <- ties$source[ties$target == i]
      v <- nodes$y[nodes$wave == 1 & nodes$node %in% from]
      if (length(v)) mean(v) else NA_real_
    }, numeric(1))
    expect_identical(is.na(hand$ex), is.na(mf$ex_y_tie))
    expect_lt(max(abs(hand$ex - mf$ex_y_tie), na.rm = TRUE), 1e-12)

    map <- c(`(Intercept)` = "(Intercept)", y_lag1 = "y1", x = "x", ex_y_tie = "ex")
    for (g in names(.lmg_groups)) {
      ids <- sort(unique(nodes$node[nodes$group == g]))
      Z <- .lmg_positions(ids, d)
      centered <- sweep(Z, 2, colMeans(Z))
      for (k in seq_len(d)) {
        hand_col <- sprintf("z%d_%s", k, g)
        hand[[hand_col]] <- ifelse(hand$group == g, centered[match(mf$node, ids), k], 0)
        pkg_col <- .lmg_colname(d, k, g)
        expect_lt(max(abs(hand[[hand_col]] - mf[[pkg_col]])), 1e-12)
        expect_true(all(mf[[pkg_col]][mf$group != g] == 0))
        map[pkg_col] <- hand_col
      }
    }
    rhs <- paste(setdiff(map, "(Intercept)"), collapse = " + ")
    reference <- suppressMessages(lme4::lmer(
      stats::as.formula(sprintf("y ~ %s + (1 | group)", rhs)), data = hand,
      REML = lme4::isREML(fit$backend_fit)
    ))
    expect_gt(fit$var_corr$sdcor[1], 0.1)
    expect_lt(max(abs(coef(fit)[names(map)] - lme4::fixef(reference)[map])), 1e-6)
    expect_lt(abs(sqrt(stats::vcov(fit)["ex_y_tie", "ex_y_tie"]) -
                    sqrt(as.matrix(stats::vcov(reference))["ex", "ex"])), 1e-6)
  }
})

test_that("the exposure coefficient ignores each group's translation, rotation and reflection", {
  nd <- .lmg_data()
  rotation <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  angle <- c(a = 0.3, b = 1.1, c = 2.2, d = -0.7, e = 4)
  shift <- c(a = 3, b = -7, c = 50, d = 0.1, e = -1000)
  same_as <- function(base, d, transform) {
    moved <- .lmg_fit(d, transform, data = nd)
    common <- c("(Intercept)", "y_lag1", "x", "ex_y_tie")
    expect_lt(max(abs(coef(moved)[common] - coef(base)[common])), 1e-5)
    expect_lt(abs(moved$var_corr$sdcor[1] - base$var_corr$sdcor[1]), 1e-5)
    moved
  }

  one <- .lmg_fit(1L, data = nd)
  expect_gt(abs(coef(one)[["ex_y_tie"]]), 0.01)
  same_as(one, 1L, function(Z, g) Z + shift[[g]])
  same_as(one, 1L, function(Z, g) if (g %in% c("b", "d")) -Z else Z)

  two <- .lmg_fit(2L, data = nd)
  same_as(two, 2L, function(Z, g) sweep(Z, 2, c(shift[[g]], -shift[[g]] / 2), "+"))
  rotated <- same_as(two, 2L, function(Z, g) Z %*% rotation(angle[[g]]))
  same_as(two, 2L, function(Z, g) if (g == "c") Z %*% diag(c(-1, 1)) else Z)
  same_as(two, 2L, function(Z, g) {
    sweep(Z %*% rotation(angle[[g]]) %*% diag(c(-1, 1)), 2, c(shift[[g]], 2), "+")
  })

  # The position coefficients themselves do move: they are the frame.
  expect_gt(abs(coef(rotated)[["lat_tie_1_d1__b"]] - coef(two)[["lat_tie_1_d1__b"]]), 1e-3)
})

test_that("one group is the single-network procedure, apart from the intercept", {
  frame <- .lmg_frames()[[4]]   # group d
  nd <- .lmg_data(list(frame))
  fit <- .lmg_fit(1L, data = nd, formula = y ~ ar() + x +
    exposure(y, via = tie, normalize = "row") + latent(tie, d = 1, seed = 1))
  mf <- fit$model_frame
  expect_identical(fit$backend, "stats")
  expect_true("lat_tie_1" %in% names(coef(fit)))

  ids <- sort(unique(frame$nodes$node))
  Z <- .lmg_positions(ids, 1L)
  raw <- Z[match(mf$node, ids), 1]
  expect_lt(max(abs(mf$lat_tie_1 - raw)), 1e-12)

  # The published procedure, with the positions as they come.
  xu <- stats::lm(mf$.outcome ~ mf$y_lag1 + mf$x + mf$ex_y_tie + raw)
  terms <- c("(Intercept)", "y_lag1", "x", "ex_y_tie", "lat_tie_1")
  expect_lt(max(abs(coef(fit)[terms] - stats::coef(xu))), 1e-10)

  # The multi-group form restricted to this group centers them. Every slope is
  # unchanged and the intercept moves by exactly the slope times the mean position.
  centered <- stats::lm(mf$.outcome ~ mf$y_lag1 + mf$x + mf$ex_y_tie + I(raw - mean(Z)))
  expect_lt(max(abs(stats::coef(centered)[-1] - stats::coef(xu)[-1])), 1e-10)
  expect_lt(abs(stats::coef(centered)[[1]] - stats::coef(xu)[[1]] -
                  stats::coef(xu)[[5]] * mean(Z)), 1e-10)
})

test_that("fitting latent() across several networks warns; one network does not", {
  stub <- function(term, adjacency, node_attrs, directed = TRUE) {
    ids <- rownames(adjacency)
    Z <- .lmg_positions(ids, term$d)
    list(positions = Z, formula = "stub", n_actors = length(ids))
  }
  local_mocked_bindings(.latent_fit_positions = stub)
  several <- y ~ ar() + exposure(y, via = tie, normalize = "row") +
    latent(tie, d = 1, seed = 1) + (1 | group)
  warnings_from <- function(expr) {
    seen <- character(0)
    withCallingHandlers(suppressMessages(expr), warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    seen
  }
  seen <- warnings_from(fit_influence(several, data = .lmg_data()))
  hit <- seen[grepl("latent()", seen, fixed = TRUE)]
  expect_length(hit, 1L)
  expect_match(hit, "experimental", fixed = TRUE)
  expect_match(hit, "single-network procedure", fixed = TRUE)
  expect_match(hit, "not validated for statistical performance", fixed = TRUE)
  expect_match(hit, "exposure coefficient is shared across networks", fixed = TRUE)
  expect_match(hit, "separate per-network fits", fixed = TRUE)

  one <- y ~ ar() + exposure(y, via = tie, normalize = "row") + latent(tie, d = 1, seed = 1)
  seen <- warnings_from(fit_influence(one, data = .lmg_data(.lmg_frames()[4])))
  expect_false(any(grepl("experimental|per-network", seen)))
})
