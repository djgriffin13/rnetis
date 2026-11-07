# Every sampler control fit_selection() accepts reaches the estimator, and what
# it refuses is refused. Observed on the fitted object and on the call the backend
# receives.

.capture_ame <- function(env) {
  real <- amen::ame
  function(...) {
    env$args <- list(...)
    env$fit <- real(...)
    env$fit
  }
}

test_that("nscan, burn, odens, seed and gof reach amen::ame() on the per-group route", {
  skip_if_not_installed("amen")
  d <- .selection_fixture_data("bin", n_actors = 8)
  env <- new.env()
  testthat::local_mocked_bindings(ame = .capture_ame(env), .package = "amen")
  fit <- fit_selection(tie ~ prior_tie() + sender(x), data = d, family = "bin",
                       nscan = 40, burn = 10, odens = 10, seed = 7, gof = FALSE)
  expect_identical(env$args$nscan, 40)
  expect_identical(env$args$burn, 10)
  expect_identical(env$args$odens, 10)
  expect_identical(env$args$seed, 7)
  expect_identical(env$args$gof, FALSE)
  expect_identical(env$args$family, "bin")
  expect_identical(env$args$R, 0)
  expect_null(env$args$symmetric)
  # (burn + nscan) %/% odens - burn %/% odens = 5 - 1 kept draws.
  expect_identical(nrow(fit$per_group[[1]]$draws), 4L)
})

test_that("the number of retained draws follows nscan, burn and odens", {
  skip_if_not_installed("amen")
  d <- .selection_fixture_data("bin", n_actors = 8)
  f <- function(...) {
    nrow(fit_selection(tie ~ prior_tie(), data = d, family = "bin", gof = FALSE, ...)$per_group[[1]]$draws)
  }
  expect_identical(f(nscan = 60, burn = 10, odens = 10), 6L)
  expect_identical(f(nscan = 60, burn = 10, odens = 5), 12L)
  # 25 %/% 10 = 2 of the 8 multiples of 10 up to 85 fall in burn-in, leaving 6.
  expect_identical(f(nscan = 60, burn = 25, odens = 10), 6L)
})

test_that("seed decides the draws, and gof decides the goodness-of-fit record", {
  skip_if_not_installed("amen")
  d <- .selection_fixture_data("bin", n_actors = 8)
  f <- function(seed, gof = FALSE) {
    fit_selection(tie ~ prior_tie(), data = d, family = "bin", nscan = 40, burn = 10,
                  odens = 10, seed = seed, gof = gof)
  }
  a <- f(3)
  expect_identical(a$per_group[[1]]$draws, f(3)$per_group[[1]]$draws)
  expect_false(identical(a$per_group[[1]]$draws, f(4)$per_group[[1]]$draws))
  # amen records the observed statistics in the first row, and one row per kept
  # draw only when gof is on.
  env <- new.env()
  testthat::local_mocked_bindings(ame = .capture_ame(env), .package = "amen")
  f(3, gof = TRUE)
  expect_identical(nrow(env$fit$GOF), 5L)
  f(3, gof = FALSE)
  expect_identical(nrow(env$fit$GOF), 1L)
})

test_that("R reaches the backend and changes what is fitted", {
  skip_if_not_installed("amen")
  d <- .selection_fixture_data("bin", n_actors = 8)
  env <- new.env()
  testthat::local_mocked_bindings(ame = .capture_ame(env), .package = "amen")
  fit_selection(tie ~ prior_tie(), data = d, family = "bin", R = 1,
                nscan = 40, burn = 10, odens = 10, gof = FALSE)
  expect_identical(env$args$R, 1)
  uv <- env$fit$UVPM
  expect_identical(dim(uv), c(8L, 8L))
  expect_gt(max(abs(uv)), 0)
})

test_that("family reaches the backend, with the values it models", {
  skip_if_not_installed("amen")
  d <- .selection_fixture_data("nrm", n_actors = 8)
  env <- new.env()
  testthat::local_mocked_bindings(ame = .capture_ame(env), .package = "amen")
  fit_selection(tie ~ sender(x), data = d, family = "nrm", waves = c(1, 2),
                nscan = 40, burn = 10, odens = 10, gof = FALSE)
  expect_identical(env$args$family, "nrm")
  ties <- netis_ties(d)
  one <- ties[ties$wave == 2, ]
  # Wave 2's own values, on a known asymmetric cell, not a recoding of them.
  cell <- one[1, ]
  expect_identical(env$args$Y[cell$source, cell$target], cell$tie)
  expect_true(all(is.na(diag(env$args$Y))))
})

test_that("binarize decides the outcome the backend receives", {
  skip_if_not_installed("amen")
  d <- .selection_fixture_data("nrm", n_actors = 8)
  env <- new.env()
  testthat::local_mocked_bindings(ame = .capture_ame(env), .package = "amen")
  fit_selection(tie ~ sender(x), data = d, family = "bin", binarize = threshold(0.5),
                waves = c(1, 2), nscan = 40, burn = 10, odens = 10, gof = FALSE)
  ties <- netis_ties(d)
  one <- ties[ties$wave == 2, ]
  expected <- matrix(NA_real_, 8, 8, dimnames = dimnames(env$args$Y))
  expected[cbind(one$source, one$target)] <- as.numeric(one$tie > 0.5)
  expect_identical(env$args$family, "bin")
  expect_identical(env$args$Y, expected)
})

test_that("symmetric reaches the backend and fits one additive effect", {
  skip_if_not_installed("amen")
  ids <- sprintf("n%02d", 1:8)
  set.seed(5)
  pairs <- t(utils::combn(ids, 2))
  ties <- data.frame(group = 1, source = pairs[, 1], target = pairs[, 2], w = stats::rnorm(nrow(pairs)))
  nodes <- data.frame(group = 1, node = ids, x = stats::rnorm(8))
  nd <- netis_data(nodes, ties, directed = FALSE,
                   tie_meta = list(w = list(zero_meaning = "measured_zero")))
  env <- new.env()
  testthat::local_mocked_bindings(ame = .capture_ame(env), .package = "amen")
  fit_selection(w ~ sender(x), data = nd, family = "nrm", symmetric = TRUE,
                nscan = 40, burn = 10, odens = 10, gof = FALSE)
  expect_identical(env$args$symmetric, TRUE)
  # A symmetric fit has no separate receiver effect.
  expect_null(env$fit$BPM)
  expect_true(isSymmetric(unname(env$args$Y)))
})

test_that("chains, nscan, burn, odens and seed reach the hierarchical sampler", {
  skip_if_not_installed("amen")
  d <- .selection_fixture_data("bin", n_groups = 3, n_actors = 8)
  f <- function(...) {
    fit_selection(tie ~ prior_tie() + sender(x), data = d, family = "bin",
                  pooling = selection_pooling(random = "snd_x"), ...)
  }
  two <- f(nscan = 40, burn = 10, odens = 10, chains = 2, seed = 11)
  # Per chain: (10 + 40) %/% 10 - 10 %/% 10 = 4 kept draws.
  expect_identical(nrow(two$draws$parameters), 8L)
  expect_identical(as.integer(table(two$draws$chain)), c(4L, 4L))
  expect_equal(two$draws$n_chains, 2)

  one <- f(nscan = 60, burn = 10, odens = 10, chains = 1, seed = 11)
  expect_identical(nrow(one$draws$parameters), 6L)
  expect_equal(one$draws$n_chains, 1)

  # The first chain runs under seed itself, so it repeats a one-chain fit at that
  # seed, and a different seed gives different draws.
  same <- f(nscan = 40, burn = 10, odens = 10, chains = 1, seed = 11)
  other <- f(nscan = 40, burn = 10, odens = 10, chains = 1, seed = 12)
  first <- two$draws$parameters[two$draws$chain == 1, , drop = FALSE]
  expect_identical(unname(first), unname(same$draws$parameters))
  expect_false(identical(unname(first), unname(other$draws$parameters)))
})

test_that("controls outside the route's list are refused by name", {
  d <- .selection_fixture_data("bin", n_actors = 8)
  for (ctrl in list(list(prior = list()), list(rvar = FALSE), list(cvar = FALSE),
                    list(dcor = FALSE), list(nvar = FALSE), list(intercept = FALSE))) {
    expect_error(
      do.call(fit_selection, c(list(tie ~ prior_tie(), data = d, family = "bin"), ctrl)),
      names(ctrl)
    )
  }
  expect_error(
    fit_selection(tie ~ prior_tie(), data = d, family = "bin", chains = 2),
    "chains"
  )
  dg <- .selection_fixture_data("bin", n_groups = 3, n_actors = 8)
  expect_error(
    fit_selection(tie ~ prior_tie() + sender(x), data = dg, family = "bin",
                  pooling = selection_pooling(random = "snd_x"), gof = FALSE),
    "gof"
  )
})

test_that("pool_beta beside pooling is refused, and only when actually supplied", {
  d <- .selection_fixture_data("bin", n_groups = 3, n_actors = 8)
  for (value in list(TRUE, FALSE)) {
    expect_error(
      fit_selection(tie ~ prior_tie() + sender(x), data = d, family = "bin", pool_beta = value,
                    pooling = selection_pooling(random = "snd_x"), nscan = 40, burn = 10, odens = 10),
      "pool_beta and pooling"
    )
  }
  # The default does not trigger it, and the fit goes ahead.
  skip_if_not_installed("amen")
  fit <- fit_selection(tie ~ prior_tie() + sender(x), data = d, family = "bin",
                       pooling = selection_pooling(random = "snd_x"),
                       nscan = 40, burn = 10, odens = 10)
  expect_s3_class(fit, "netis_selection")
})
