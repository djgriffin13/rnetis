# Random-number hygiene and the exact population estimand.
#
# A fit must leave the caller's random-number stream as it found it, every
# group must get its own seed, and the population estimand must not draw at
# all: it is an exact integral.

.repro_data <- function(G = 2, n = 7, waves = 2, seed = 11L, valued = FALSE,
                        identical_groups = FALSE) {
  set.seed(seed)
  gs <- as.character(seq_len(G))
  one <- function(g) {
    ids <- sprintf("g%s_%02d", g, seq_len(n))
    p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
    p <- p[p$source != p$target, ]
    list(
      nodes = data.frame(group = g, node = rep(ids, waves),
                         wave = rep(seq_len(waves), each = n),
                         x = stats::rnorm(n * waves), stringsAsFactors = FALSE),
      ties = do.call(rbind, lapply(seq_len(waves), function(w) {
        data.frame(group = g, p, wave = w,
                   tie = if (valued) sample(0:3, nrow(p), TRUE) else stats::rbinom(nrow(p), 1, 0.35),
                   stringsAsFactors = FALSE)
      }))
    )
  }
  parts <- lapply(gs, one)
  if (identical_groups) {
    # Group 2 is group 1 relabeled: the same data under another name.
    relabel <- function(df) {
      df$group <- "2"
      for (col in intersect(c("node", "source", "target"), names(df))) {
        df[[col]] <- sub("^g1_", "g2_", df[[col]])
      }
      df
    }
    parts[[2]] <- list(nodes = relabel(parts[[1]]$nodes), ties = relabel(parts[[1]]$ties))
  }
  netis_data(do.call(rbind, lapply(parts, `[[`, "nodes")),
             do.call(rbind, lapply(parts, `[[`, "ties")),
             wave_col = "wave",
             tie_meta = list(tie = list(zero_meaning = "measured_zero")))
}

test_that("a per-group fit leaves the caller's random-number stream unchanged", {
  skip_if_not_installed("amen")
  nd <- .repro_data(G = 1)
  set.seed(123); before <- .Random.seed; expected <- stats::runif(1)
  set.seed(123)
  suppressWarnings(fit_selection(tie ~ prior_tie(), nd, nscan = 30, burn = 10, odens = 10))
  expect_identical(.Random.seed, before)
  expect_identical(stats::runif(1), expected)
})

test_that("a hierarchical fit leaves the caller's random-number stream unchanged", {
  skip_if_not_installed("amen")
  nd <- .repro_data(G = 3)
  set.seed(77); before <- .Random.seed
  suppressWarnings(fit_selection(tie ~ prior_tie(), nd,
                                 pooling = selection_pooling(random = "intercept"),
                                 nscan = 30, burn = 10, odens = 10))
  expect_identical(.Random.seed, before)
})

test_that("a draw-data-fit loop draws different data on every replication", {
  skip_if_not_installed("amen")
  nd <- .repro_data(G = 1)
  set.seed(5)
  reps <- lapply(1:3, function(i) {
    z <- stats::rnorm(3)
    suppressWarnings(fit_selection(tie ~ prior_tie(), nd, nscan = 30, burn = 10, odens = 10))
    z
  })
  expect_false(identical(reps[[1]], reps[[2]]))
  expect_false(identical(reps[[2]], reps[[3]]))
  # And the stream is exactly the one an unfitted loop would have drawn.
  set.seed(5)
  expect_identical(reps, lapply(1:3, function(i) stats::rnorm(3)))
})

test_that("each group gets its own seed, derived from the fit seed and recorded", {
  skip_if_not_installed("amen")
  nd <- .repro_data(G = 2, identical_groups = TRUE)
  fit <- suppressWarnings(fit_selection(tie ~ prior_tie(), nd, pool_beta = FALSE,
                                        nscan = 40, burn = 10, odens = 10, seed = 3))
  expect_identical(fit$estimation_controls$group_seeds, c(`1` = 3, `2` = 3 + 7919))
  # Identical data, different seeds: the chains must differ.
  expect_false(isTRUE(all.equal(unname(fit$per_group[["1"]]$draws),
                                unname(fit$per_group[["2"]]$draws))))
})

test_that("a single-group fit uses the fit seed itself, as amen::ame() would", {
  skip_if_not_installed("amen")
  nd <- .repro_data(G = 1)
  fit <- suppressWarnings(fit_selection(tie ~ prior_tie(), nd, nscan = 40, burn = 10,
                                        odens = 10, seed = 9))
  d <- fit$designs[[1]]
  invisible(utils::capture.output(ref <- amen::ame(
    d$Y, Xdyad = d$Xdyad, family = "bin", R = 0, nscan = 40, burn = 10, odens = 10,
    seed = 9, print = FALSE, plot = FALSE)))
  expect_identical(unname(fit$per_group[[1]]$draws), unname(ref$BETA))
})

test_that("the closed form used by the population estimand is the normal integral", {
  # E[Phi(eta + o)], o ~ N(0, v), against numerical quadrature at known values.
  for (cfg in list(c(eta = 0.3, v = 1.3), c(eta = -1.2, v = 0.4), c(eta = 2, v = 3))) {
    quad <- stats::integrate(function(o) stats::pnorm(cfg[["eta"]] + o) *
                               stats::dnorm(o, 0, sqrt(cfg[["v"]])), -Inf, Inf)$value
    expect_lt(abs(quad - stats::pnorm(cfg[["eta"]] / sqrt(1 + cfg[["v"]]))), 1e-8)
  }
  # The variance a population dyad's offset has: va + vb, no cab term; 2 va on
  # a symmetric fit.
  expect_identical(.population_offset_variance(c(va = 0.8, cab = 0.6, vb = 0.5, rho = 0, ve = 1)), 1.3)
  expect_identical(.population_offset_variance(c(va = 0.8, cab = 0, rho = 0, ve = 1)), 1.6)
})

test_that("the population estimand is exact, deterministic and draws no random numbers", {
  skip_if_not_installed("amen")
  nd <- .repro_data(G = 1)
  fit <- suppressWarnings(fit_selection(tie ~ prior_tie() + sender(x), nd,
                                        nscan = 60, burn = 10, odens = 10))
  set.seed(1); before <- .Random.seed
  e1 <- selection_effect(fit, "prior_tie", type = "population")
  expect_identical(.Random.seed, before)
  e2 <- selection_effect(fit, "prior_tie", type = "population")
  expect_identical(e1$draws, e2$draws)

  ex <- fit$per_group[[1]]
  des <- fit$designs[[1]]
  X <- .effect_design_matrix(des, colnames(ex$draws))
  keep <- !is.na(as.vector(des$Y))
  hand <- vapply(seq_len(nrow(ex$draws)), function(s) {
    b <- ex$draws[s, ]
    sc <- sqrt(1 + ex$variance_components[s, "va"] + ex$variance_components[s, "vb"])
    # prior_tie is binary, so the contrast is 0 to 1 at every dyad.
    e0 <- as.vector(X[keep, ] %*% b) - X[keep, "prior_tie"] * b[["prior_tie"]]
    mean(stats::pnorm((e0 + b[["prior_tie"]]) / sc) - stats::pnorm(e0 / sc))
  }, numeric(1))
  expect_lt(max(abs(e1$draws - hand)), 1e-12)
  expect_match(e1$coverage, "closed form")
})

test_that("binarize applies to the lagged tie as well as the outcome", {
  skip_if_not_installed("amen")
  nd <- .repro_data(G = 1, valued = TRUE, seed = 4L)
  fit <- suppressWarnings(fit_selection(tie ~ prior_tie() + prior_reciprocity(), nd,
                                        binarize = threshold(1),
                                        nscan = 30, burn = 10, odens = 10))
  ties <- netis_ties(nd)
  w1 <- ties[ties$wave == 1, ]
  ids <- fit$actor_ids[[1]]
  lag <- matrix(NA_real_, length(ids), length(ids), dimnames = list(ids, ids))
  lag[cbind(match(w1$source, ids), match(w1$target, ids))] <- w1$tie
  X <- fit$designs[[1]]$Xdyad
  off <- row(lag) != col(lag)
  # Cell by cell: the persistence predictor is the binarized earlier tie, and
  # the reciprocity predictor is its transpose -- not the valued 0..3 count.
  expect_identical(X[, , "prior_tie"][off], as.numeric(lag > 1)[off])
  expect_identical(X[, , "prior_recip"][off], as.numeric(t(lag) > 1)[off])
  expect_true(any(lag[off] > 1) && any(lag[off] == 1))
})
