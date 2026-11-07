# Known answers for ICC(1)/ICC(2) and r_wg(j), computed by hand.

# Three UNEQUAL groups (4, 3, 5 members). Wave-2 outcomes are fixed numbers so
# the one-way ANOVA can be done on paper; member c05 has a missing wave-1
# outcome, so its row is in the assembled table but not in the estimating
# sample (its lag is missing). Reading the assembled table would include its
# wave-2 value of 10 and change every quantity below.
.icc_known_data <- function() {
  set.seed(11)
  y2 <- list(a = c(1, 2, 3, 4), b = c(5, 6, 7), c = c(2, 4, 6, 8, 10))
  nodes <- do.call(rbind, lapply(names(y2), function(g) {
    n <- length(y2[[g]])
    ids <- sprintf("%s%02d", g, seq_len(n))
    y1 <- stats::rnorm(n)
    if (g == "c") y1[5] <- NA
    data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = n),
               y = c(y1, y2[[g]]), x = stats::rnorm(2 * n),
               stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(names(y2), function(g) {
    n <- length(y2[[g]])
    ids <- sprintf("%s%02d", g, seq_len(n))
    data.frame(group = g, source = ids, target = c(ids[-1], ids[1]), wave = 1,
               tie = 1, stringsAsFactors = FALSE)
  }))
  netis_data(nodes, ties, wave_col = "wave")
}

.icc_known_fit <- function() {
  suppressWarnings(suppressMessages(fit_influence(
    y ~ ar() + exposure(x, via = tie, normalize = "row") + (1 | group),
    data = .icc_known_data(), waves = c(1, 2)
  )))
}

# Estimating sample: a = 1,2,3,4; b = 5,6,7; c = 2,4,6,8 (c05 dropped).
# N = 11, k = 3, grand mean 48/11.
# SSB = 4(2.5 - 48/11)^2 + 3(6 - 48/11)^2 + 4(5 - 48/11)^2 = 2849/121
# SSW = 5 + 2 + 20 = 27
# MSB = 2849/242, MSW = 27/8, n0 = (11 - 41/11) / 2 = 40/11
.msb <- 2849 / 242
.msw <- 27 / 8
.n0 <- 40 / 11

test_that("ICC(1) is Bliese's ANOVA formula on the estimating sample", {
  fit <- .icc_known_fit()
  res <- icc1(fit)
  truth <- (.msb - .msw) / (.msb + (.n0 - 1) * .msw)   # 0.40627...
  expect_lt(abs(res$value - truth), 1e-10)
  expect_identical(res$status, "ok")
  expect_identical(res$n_obs, 11L)
  expect_identical(res$n_groups, 3L)
  expect_lt(abs(res$n0 - .n0), 1e-12)
  expect_lt(abs(res$anova[["msb"]] - .msb), 1e-10)
  expect_lt(abs(res$anova[["msw"]] - .msw), 1e-10)
  expect_lt(abs(res$components[["between"]] - (.msb - .msw) / .n0), 1e-10)
})

test_that("ICC(2) overall is (MSB - MSW) / MSB and per-group n_j are estimating-sample sizes", {
  fit <- .icc_known_fit()
  res <- icc2(fit)
  expect_lt(abs(res$overall - (.msb - .msw) / .msb), 1e-10)    # 0.71332...
  expect_lt(abs(res$overall_n - .n0), 1e-12)
  expect_equal(res$by_group$group, c("a", "b", "c"))
  # c has five members on the roster but four rows in the estimating sample.
  expect_equal(res$by_group$n_j, c(4, 3, 4))
  rho <- res$icc1$value
  expect_lt(abs(res$by_group$reliability[2] - 3 * rho / (1 + 2 * rho)), 1e-12)
})

test_that("ICC(1) under MSB < MSW is the negative ANOVA estimate, flagged", {
  # Two groups with identical means: MSB = 0, so ICC(1) = -1 / (n0 - 1).
  set.seed(3)
  y2 <- list(a = c(1, 3, 5), b = c(2, 3, 4))
  nodes <- do.call(rbind, lapply(names(y2), function(g) {
    ids <- sprintf("%s%02d", g, 1:3)
    data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = 3),
               y = c(stats::rnorm(3), y2[[g]]), x = stats::rnorm(6))
  }))
  ties <- do.call(rbind, lapply(names(y2), function(g) {
    ids <- sprintf("%s%02d", g, 1:3)
    data.frame(group = g, source = ids, target = c(ids[-1], ids[1]), wave = 1, tie = 1)
  }))
  fit <- suppressWarnings(suppressMessages(fit_influence(
    y ~ ar() + exposure(x, via = tie, normalize = "row") + (1 | group),
    data = netis_data(nodes, ties, wave_col = "wave"), waves = c(1, 2)
  )))
  res <- icc1(fit)
  expect_identical(res$status, "boundary")
  expect_lt(abs(res$value - (-1 / 2)), 1e-12)
  # The negative value is reported with a note saying what it means.
  printed <- capture.output(print(res))
  expect_true(any(grepl("-0.5", printed, fixed = TRUE)))
  expect_true(any(grepl("status: boundary", printed, fixed = TRUE)))
  expect_true(any(grepl("no evidence of between-group variance",
                        paste(printed, collapse = " "), fixed = TRUE)))
})

# ---- r_wg(j) -----------------------------------------------------------------

# Two items, values on a 1..5 scale, uniform null variance 2.
# Group "agree":    item variances 0.3 and 0.3 -> ratio 0.15
# Group "polar":    item variances 16/3 each   -> ratio 8/3, above J/(J-1) = 2
.rwg_items <- function() {
  agree <- cbind(c(3, 3, 4, 3, 4), c(4, 3, 3, 3, 4))
  polar <- cbind(c(1, 5, 1, 5), c(5, 1, 5, 1))
  rbind(agree, polar)
}

test_that("rwg_j is zero, not above 1, for a group whose variance exceeds the null", {
  items <- .rwg_items()
  g <- c(rep("agree", 5), rep("polar", 4))
  res <- rwg_j(items, g, null = list(dist = "uniform", n_options = 5))
  bg <- res$by_group
  # agree: var(c(3,3,4,3,4)) = 0.3, var(c(4,3,3,3,4)) = 0.3; Sbar = 0.3
  r <- 0.3 / 2
  expect_lt(abs(bg$rwg[bg$group == "agree"] - (2 * (1 - r)) / (2 * (1 - r) + r)), 1e-12)
  # polar: var(c(1,5,1,5)) = 16/3 for both items; Sbar = 16/3, ratio 8/3
  expect_lt(abs(bg$observed_var[bg$group == "polar"] - 16 / 3), 1e-12)
  expect_lt(abs(bg$variance_ratio[bg$group == "polar"] - 8 / 3), 1e-12)
  expect_identical(bg$rwg[bg$group == "polar"], 0)
  # Untruncated, the formula is returned as computed: (2 * (1 - 8/3)) /
  # (2 * (1 - 8/3) + 8/3) = (-10/3) / (-2/3) = 5.
  raw <- rwg_j(items, g, null = 2, truncate = FALSE)$by_group
  expect_lt(abs(raw$rwg[raw$group == "polar"] - 5), 1e-12)
})

test_that("rwg_j at ratio J/(J-1) with truncation is 0, not -Inf", {
  M <- cbind(c(1, 5, 1, 5), c(1, 5, 5, 1))
  v <- mean(apply(M, 2, stats::var))          # 16/3
  res <- rwg_j(M, rep("g", 4), null = v / 2)  # ratio exactly 2 = J/(J-1)
  expect_identical(res$by_group$rwg, 0)
  expect_lt(abs(res$by_group$variance_ratio - 2), 1e-12)
  expect_lt(abs(res$by_group$observed_var - 16 / 3), 1e-12)
})

test_that("netis_reliability accepts a one-column data frame of ratings", {
  fit <- .icc_known_fit()
  x <- c(4, 4, 5, 3, 4, 1, 5, 1, 5, 3)
  g <- rep(c("a", "b"), each = 5)
  res <- netis_reliability(fit, ratings = data.frame(i1 = x), null = 2,
                           rating_group = g)
  # rwg = 1 - var / 2; var(c(4,4,5,3,4)) = 0.5 -> 0.75
  expect_lt(abs(res$agreement$by_group$rwg[1] - 0.75), 1e-12)
})
