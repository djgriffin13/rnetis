# plot.netis_selection() across groups, hierarchical netis_estimates() rows, and
# netis_glance() on each kind of Selection fit, each checked against the draws.

.psg_data <- function(G = 3L, n = 7L, seed = 21L) {
  set.seed(seed)
  gs <- as.character(seq_len(G))
  rate <- c(0.2, 0.35, 0.5)[seq_len(G)]
  nodes <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("g%s_%02d", g, seq_len(n))
    data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = n),
               x = stats::rnorm(2 * n), stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(seq_along(gs), function(k) {
    ids <- sprintf("g%s_%02d", gs[k], seq_len(n))
    p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
    p <- p[p$source != p$target, ]
    do.call(rbind, lapply(1:2, function(w) {
      data.frame(group = gs[k], p, wave = w, tie = stats::rbinom(nrow(p), 1, rate[k]),
                 stringsAsFactors = FALSE)
    }))
  }))
  netis_data(nodes, ties, wave_col = "wave",
             tie_meta = list(tie = list(zero_meaning = "measured_zero")))
}

.psg_fit <- function(kind) {
  .cached_fixture(paste("psg", kind, sep = "_"), {
    args <- list(tie ~ prior_tie() + sender(x), .psg_data(), R = 1, nscan = 60, burn = 10,
                 odens = 10, seed = 1)
    extra <- switch(
      kind,
      pooled = list(),
      separate = list(pool_beta = FALSE),
      hierarchical = list(pooling = selection_pooling(random = "intercept", unpooled = "snd_x")),
      hierarchical2 = list(pooling = selection_pooling(random = "intercept", unpooled = "snd_x"),
                           chains = 2L)
    )
    suppressWarnings(do.call(fit_selection, c(args, extra)))
  })
}

.psg_draws_panel_matches <- function(fit, what, group, m) {
  d <- plot_data(fit, what = what, group = group)
  for (term in colnames(m)) {
    expect_identical(d$value[d$term == term], unname(m[, term]))
  }
  expect_setequal(unique(d$term), colnames(m))
  expect_s3_class(plot(fit, what = what, group = group), "ggplot")
}

test_that("groups estimated separately refuse a per-group panel without a group", {
  skip_if_not_installed("amen")
  for (kind in c("pooled", "separate")) {
    fit <- .psg_fit(kind)
    for (what in c("posterior", "trace", "mcmc", "actor_effects", "multiplicative")) {
      expect_error(plot(fit, what = what), "Name one", info = paste(kind, what))
      expect_s3_class(plot(fit, what = what, group = "2"), "ggplot")
    }
    expect_error(plot(fit, what = "posterior", group = "9"), "Group '9' is not in this fit")
    m <- fit$per_group[["2"]]$draws
    .psg_draws_panel_matches(fit, "posterior", "2", m)
    .psg_draws_panel_matches(fit, "trace", "2", m)

    actors <- plot_data(fit, what = "actor_effects", group = "2")
    expect_identical(actors$node, sprintf("g2_%02d", 1:7))
    expect_identical(actors$sender, as.numeric(fit$per_group[["2"]]$additive_effects$sender))
    expect_identical(nrow(plot_data(fit, what = "multiplicative", group = "2")), 42L)
    expect_identical(sort(unique(plot_data(fit, what = "mcmc", group = "2")$term)), sort(colnames(m)))
    # The variance components cover every group, whatever group is named.
    h <- plot_data(fit, what = "heterogeneity", group = "2")
    expect_identical(nrow(h), 18L)
    expect_identical(h, plot_data(fit, what = "heterogeneity"))
  }
})

test_that("the coefficient panel labels per-group rows, and names a group's own", {
  skip_if_not_installed("amen")
  fit <- .psg_fit("separate")
  all_rows <- plot_data(fit, what = "coefficients")
  expect_true(all(c("prior_tie [1]", "snd_x [2]", "intercept [3]") %in% all_rows$term))
  expect_identical(nrow(all_rows), 9L)
  two <- plot_data(fit, what = "coefficients", group = "2")
  expect_setequal(two$term, c("intercept", "snd_x", "prior_tie"))
  expect_identical(two$value[two$term == "snd_x"],
                   unname(fit$per_group[["2"]]$coefficients["snd_x"]))
  expect_s3_class(plot(fit, what = "groups"), "ggplot")
  g <- plot_data(fit, what = "groups")
  expect_identical(nrow(g), 9L)
  expect_identical(g$value[g$term == "snd_x" & g$group == "3"],
                   unname(fit$per_group[["3"]]$coefficients["snd_x"]))
})

test_that("a pooled fit has no per-group rows to show", {
  skip_if_not_installed("amen")
  fit <- .psg_fit("pooled")
  all_rows <- plot_data(fit, what = "coefficients")
  expect_setequal(all_rows$term, c("intercept", "snd_x", "prior_tie"))
  expect_error(plot(fit, what = "coefficients", group = "2"), "no per-group estimates")
  expect_error(plot(fit, what = "groups"), "pooled its coefficients")
})

test_that("a hierarchical fit shows population draws, or a named group's", {
  skip_if_not_installed("amen")
  fit <- .psg_fit("hierarchical")
  d <- fit$mgc_draws
  for (what in c("posterior", "trace")) {
    pop <- plot_data(fit, what = what)
    # snd_x varies by group with no population level, so it has no draws here.
    expect_setequal(unique(pop$term), c("intercept", "prior_tie"))
    expect_identical(pop$value[pop$term == "prior_tie"], unname(d$mu[, "prior_tie"]))
    expect_identical(pop$value[pop$term == "intercept"], unname(d$mu[, "intercept"]))
    expect_s3_class(plot(fit, what = what), "ggplot")
    .psg_draws_panel_matches(fit, what, "2", fit$per_group[["2"]]$draws)
  }
  # A group's coefficient draws are that group's theta, and the shared one its mu.
  g2 <- plot_data(fit, what = "posterior", group = "2")
  at <- match("2", as.character(d$groups))
  expect_identical(g2$value[g2$term == "intercept"], unname(d$theta[, at, "intercept"]))
  expect_identical(g2$value[g2$term == "snd_x"], unname(d$theta[, at, "snd_x"]))
  expect_identical(g2$value[g2$term == "prior_tie"], unname(d$mu[, "prior_tie"]))
  expect_setequal(unique(plot_data(fit, what = "mcmc")$term), c("intercept", "prior_tie"))
  expect_setequal(unique(plot_data(fit, what = "mcmc", group = "2")$term),
                  c("intercept", "snd_x", "prior_tie"))

  for (what in c("actor_effects", "multiplicative")) {
    expect_error(plot(fit, what = what), "Name one")
    expect_s3_class(plot(fit, what = what, group = "2"), "ggplot")
  }
  expect_error(plot(fit, what = "heterogeneity"), "not defined for a hierarchical")

  rows <- plot_data(fit, what = "coefficients")
  expect_true(all(c("intercept", "prior_tie", "intercept [2]", "snd_x [3]") %in% rows$term))
  two <- plot_data(fit, what = "coefficients", group = "2")
  # A shared coefficient has no row of its own for a group.
  expect_setequal(two$term, c("intercept", "snd_x"))
  expect_s3_class(plot(fit, what = "groups"), "ggplot")
  expect_identical(nrow(plot_data(fit, what = "groups")), 6L)
})

test_that("hierarchical netis_estimates() has both levels, computed from the draws", {
  skip_if_not_installed("amen")
  fit <- .psg_fit("hierarchical")
  d <- fit$mgc_draws
  est <- netis_estimates(fit)
  key <- paste(est$term, est$group)
  expect_identical(key, c("intercept NA", "prior_tie NA",
                          "intercept 1", "intercept 2", "intercept 3",
                          "snd_x 1", "snd_x 2", "snd_x 3"))
  row <- est[est$term == "snd_x" & est$group == "2", ]
  v <- d$theta[, match("2", as.character(d$groups)), "snd_x"]
  expect_identical(row$estimate, mean(v))
  expect_identical(row$std.error, stats::sd(v))
  expect_identical(row$conf.low, stats::quantile(v, 0.025, names = FALSE))
  expect_identical(row$conf.high, stats::quantile(v, 0.975, names = FALSE))
  expect_identical(row$role, "sender")
  expect_identical(row$scale, "latent probit")
  pop <- est[est$term == "prior_tie" & is.na(est$group), ]
  expect_identical(pop$estimate, mean(d$mu[, "prior_tie"]))
  # The level argument reaches the quantiles.
  narrow <- netis_estimates(fit, level = 0.5)
  expect_identical(narrow$conf.low[1], stats::quantile(d$mu[, "intercept"], 0.25, names = FALSE))
})

test_that("a moderated hierarchical fit adds one row per moderator coefficient", {
  skip_if_not_installed("amen")
  fit <- .moderated_fixture_fit()
  d <- fit$mgc_draws
  est <- netis_estimates(fit)
  mod <- est[est$role == "moderator", ]
  expect_identical(mod$term, "snd_x:size")
  expect_true(is.na(mod$group))
  expect_identical(mod$estimate, mean(d$gamma[, "gamma:snd_x:size"]))
  # The random coefficient's population row is its value at every moderator's zero.
  expect_true(any(est$term == "snd_x" & is.na(est$group)))
  expect_identical(sum(est$term == "snd_x" & !is.na(est$group)), 4L)
  expect_identical(est$estimate[est$term == "snd_x" & is.na(est$group)], mean(d$mu[, "snd_x"]))
})

test_that("netis_glance() on a Selection fit counts what the fit holds", {
  skip_if_not_installed("amen")
  expected <- list(
    pooled = list(estimand = "tie formation", pooled = TRUE, backend = "amen"),
    separate = list(estimand = "per-group tie formation", pooled = FALSE, backend = "amen"),
    hierarchical = list(estimand = "hierarchical multi-group tie formation", pooled = FALSE,
                        backend = "rnetis-mgc")
  )
  for (kind in names(expected)) {
    fit <- .psg_fit(kind)
    g <- netis_glance(fit)
    expect_identical(nrow(g), 1L)
    expect_identical(g$family, "selection")
    expect_identical(g$estimand, expected[[kind]]$estimand)
    expect_identical(g$estimand, netis_estimand(fit)$estimand)
    expect_identical(g$pooled, expected[[kind]]$pooled)
    expect_identical(g$backend, expected[[kind]]$backend)
    expect_identical(g$outcome_family, "bin")
    expect_identical(g$n_groups, 3L)
    expect_identical(g$latent_dims, 1L)
    # Seven actors, ordered pairs, three groups, every dyad observed.
    expect_identical(g$nobs, 126L)
    # nscan 60, odens 10.
    expect_identical(g$n_draws, 6L)
    expect_identical(g$n_warning, sum(vapply(fit$diagnostics, function(f) f$level == "WARNING", logical(1))))
    expect_false(any(c("r.squared", "sigma", "rmse", "estimator") %in% names(g)))
  }
  # Draws are counted over chains.
  two <- .psg_fit("hierarchical2")
  expect_identical(netis_glance(two)$n_draws, nrow(two$mgc_draws$mu))
  expect_identical(netis_glance(two)$n_draws, 12L)
})
