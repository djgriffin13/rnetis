# Designs a Selection fit cannot estimate are refused by name, on both routes,
# before any sampler runs; and designs it can estimate are not refused.

.deg_group <- function(g, Y, x = NULL) {
  n <- nrow(Y)
  id <- sprintf("g%s_%02d", g, seq_len(n))
  pr <- expand.grid(source = id, target = id, stringsAsFactors = FALSE)
  pr <- pr[pr$source != pr$target, ]
  list(nodes = data.frame(group = g, node = id,
                          x = if (is.null(x)) seq_len(n) / n else x,
                          stringsAsFactors = FALSE),
       ties = data.frame(group = g, pr,
                         tie = Y[cbind(match(pr$source, id), match(pr$target, id))],
                         stringsAsFactors = FALSE))
}

.deg_data <- function(parts, ...) {
  netis_data(do.call(rbind, lapply(parts, `[[`, "nodes")),
             do.call(rbind, lapply(parts, `[[`, "ties")),
             tie_meta = list(tie = list(zero_meaning = "measured_zero")), ...)
}

.deg_random_Y <- function(n, p, seed) {
  set.seed(seed)
  Y <- matrix(stats::rbinom(n * n, 1, p), n, n)
  diag(Y) <- NA
  Y
}

.deg_cycle <- function(n) {
  Y <- matrix(0, n, n)
  for (i in seq_len(n)) Y[i, i %% n + 1] <- 1
  diag(Y) <- NA
  Y
}

.deg_base <- function() {
  list(.deg_group("1", .deg_random_Y(8, 0.35, 1)), .deg_group("2", .deg_random_Y(8, 0.4, 2)))
}

.deg_fit <- function(nd, ...) {
  suppressWarnings(fit_selection(tie ~ sender(x), nd, nscan = 30, burn = 10, odens = 10, ...))
}

test_that("a regular-degree group is refused by name on both routes", {
  skip_if_not_installed("amen")
  nd <- .deg_data(c(.deg_base(), list(.deg_group("3", .deg_cycle(6)))))
  expect_error(.deg_fit(nd), "Group '3' is a regular-degree network")
  expect_error(.deg_fit(nd, pooling = selection_pooling(random = "intercept")),
               "Group '3' is a regular-degree network")
  # The detector mirrors amen's prior: regular in one margin only is fine.
  Y <- .deg_cycle(6)
  Y[1, 3] <- 1
  expect_false(.amen_regular_degree(Y))
  expect_true(.amen_regular_degree(.deg_cycle(10)))
})

test_that("a group of two actors is refused by name on both routes", {
  skip_if_not_installed("amen")
  nd <- .deg_data(c(.deg_base(), list(.deg_group("3", matrix(c(NA, 0, 1, NA), 2, 2)))))
  expect_error(.deg_fit(nd), "Group '3' has 2 actor\\(s\\)")
  expect_error(.deg_fit(nd, pooling = selection_pooling(random = "intercept")),
               "Group '3' has 2 actor\\(s\\)")
})

test_that("a binary group without tie variation is refused per group, admitted hierarchically", {
  skip_if_not_installed("amen")
  Y0 <- matrix(0, 6, 6); diag(Y0) <- NA
  nd <- .deg_data(c(.deg_base(), list(.deg_group("3", Y0))))
  expect_error(.deg_fit(nd), "Group '3' has no variation in its binary ties")
  fit <- .deg_fit(nd, pooling = selection_pooling(random = "intercept"))
  expect_identical(unname(fit$tie_variation[["3"]]), "all0")
})

test_that("a predictor constant within one group is refused on the per-group route", {
  skip_if_not_installed("amen")
  parts <- .deg_base()
  parts[[2]] <- .deg_group("2", .deg_random_Y(8, 0.4, 2), x = rep(0.5, 8))
  nd <- .deg_data(parts)
  expect_error(.deg_fit(nd), "In group '2', 'snd_x' takes the same value")
  # Shared on the hierarchical route, the other group identifies it.
  expect_s3_class(.deg_fit(nd, pooling = selection_pooling(random = "intercept")),
                  "netis_selection")
  # Unpooled, it has neither data nor a proper prior in that group.
  expect_error(.deg_fit(nd, pooling = selection_pooling(random = "intercept", unpooled = "snd_x")),
               "'snd_x' is group-specific \\(unpooled\\), but it has no within-group variation in group\\(s\\) '2'")
})

test_that("a random slope with no within-group variation anywhere is refused by name", {
  skip_if_not_installed("amen")
  parts <- list(.deg_group("1", .deg_random_Y(8, 0.35, 1), x = rep(1, 8)),
                .deg_group("2", .deg_random_Y(8, 0.4, 2), x = rep(2, 8)),
                .deg_group("3", .deg_random_Y(8, 0.3, 3), x = rep(3, 8)))
  expect_error(.deg_fit(.deg_data(parts), pooling = selection_pooling(random = "snd_x")),
               "'snd_x' has no within-group variation in any of the 3 group\\(s\\)")
})

test_that("the hierarchical sampler fits an intercept-only model", {
  skip_if_not_installed("amen")
  nd <- .deg_data(c(.deg_base(), list(.deg_group("3", .deg_random_Y(8, 0.5, 3)))))
  fit <- suppressWarnings(fit_selection(tie ~ 1, nd,
                                        pooling = selection_pooling(random = "intercept"),
                                        nscan = 40, burn = 10, odens = 10))
  expect_identical(dim(fit$mgc_draws$theta), c(4L, 3L, 1L))
  expect_identical(dimnames(fit$mgc_draws$theta)[[2]], c("1", "2", "3"))
  means <- colMeans(fit$mgc_draws$theta[, , 1])
  expect_true(all(is.finite(means)))
})

test_that("undirected data must be fitted symmetric, and is refused on the hierarchical route", {
  skip_if_not_installed("amen")
  set.seed(3)
  ids <- sprintf("n%02d", 1:7)
  pr <- t(utils::combn(ids, 2))
  nodes <- rbind(data.frame(group = "1", node = ids, x = stats::rnorm(7)),
                 data.frame(group = "2", node = ids, x = stats::rnorm(7)))
  ties <- rbind(data.frame(group = "1", source = pr[, 1], target = pr[, 2],
                           tie = stats::rbinom(nrow(pr), 1, 0.45)),
                data.frame(group = "2", source = pr[, 1], target = pr[, 2],
                           tie = stats::rbinom(nrow(pr), 1, 0.45)))
  nd <- netis_data(nodes, ties, directed = FALSE,
                   tie_meta = list(tie = list(zero_meaning = "measured_zero")))
  expect_error(.deg_fit(nd), "declares its ties undirected.*symmetric = TRUE")
  expect_error(.deg_fit(nd, pooling = selection_pooling(random = "intercept")),
               "does not support in this version")
})

test_that("group() terms are refused on both routes", {
  skip_if_not_installed("amen")
  parts <- .deg_base()
  nd0 <- .deg_data(parts)
  nodes <- netis_nodes(nd0)
  ties <- netis_ties(nd0)
  # Varies across waves within each group, which does not make it estimable:
  # every fit reads one outcome wave.
  nodes2 <- rbind(transform(nodes, wave = 1), transform(nodes, wave = 2))
  ties2 <- rbind(transform(ties, wave = 1), transform(ties, wave = 2))
  grp <- data.frame(group = c("1", "2", "1", "2"), wave = c(1, 1, 2, 2), size = c(3, 5, 4, 9))
  nd <- netis_data(nodes2, ties2, group_data = grp, wave_col = "wave",
                   tie_meta = list(tie = list(zero_meaning = "measured_zero")))
  f <- tie ~ prior_tie() + group(size)
  expect_error(suppressWarnings(fit_selection(f, nd, nscan = 30, burn = 10, odens = 10)),
               "group\\(size\\) cannot be identified")
  expect_error(suppressWarnings(fit_selection(f, nd, pooling = selection_pooling(random = "intercept"),
                                              nscan = 30, burn = 10, odens = 10)),
               "group\\(size\\) cannot be identified")
})
