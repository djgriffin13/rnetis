# Accessors, effects and plots on a hierarchical multi-group fit, and the
# refusals for what is not defined there.

.hier_acc_data <- function(G = 3, n = 7, waves = 2, seed = 21L) {
  set.seed(seed)
  gs <- as.character(seq_len(G))
  # Unequal tie rates by group, so group-level quantities differ.
  rate <- c(0.2, 0.35, 0.5, 0.3, 0.4)[seq_len(G)]
  nodes <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("g%s_%02d", g, seq_len(n))
    data.frame(group = g, node = rep(ids, waves), wave = rep(seq_len(waves), each = n),
               x = stats::rnorm(n * waves), stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(seq_along(gs), function(k) {
    g <- gs[k]
    ids <- sprintf("g%s_%02d", g, seq_len(n))
    p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
    p <- p[p$source != p$target, ]
    do.call(rbind, lapply(seq_len(waves), function(w) {
      data.frame(group = g, p, wave = w, tie = stats::rbinom(nrow(p), 1, rate[k]),
                 stringsAsFactors = FALSE)
    }))
  }))
  netis_data(nodes, ties, wave_col = "wave",
             tie_meta = list(tie = list(zero_meaning = "measured_zero")))
}

.hier_acc_fit <- function(R = 0, chains = 1L) {
  .cached_fixture(paste("hier_acc", R, chains, sep = "_"), {
    suppressWarnings(fit_selection(
      tie ~ prior_tie() + sender(x), .hier_acc_data(), R = R,
      pooling = selection_pooling(random = "intercept", unpooled = "snd_x"),
      nscan = 40, burn = 10, odens = 10, chains = chains))
  })
}

test_that("netis_estimand() names a hierarchical fit and its regimes", {
  skip_if_not_installed("amen")
  e <- netis_estimand(.hier_acc_fit())
  expect_identical(e$estimand, "hierarchical multi-group tie formation")
  expect_identical(e$pooling, "hierarchical")
  expect_identical(e$regime, c(intercept = "random", snd_x = "group", prior_tie = "shared"))
  # Shared and random coefficients borrow across groups; the unpooled one does not.
  expect_identical(e$pooled_parameters, c("intercept", "prior_tie"))
  expect_identical(netis_glance(.hier_acc_fit())$estimand, e$estimand)
})

test_that("pseudo_r2() and netis_variance_components() refuse a hierarchical fit", {
  skip_if_not_installed("amen")
  fit <- .hier_acc_fit()
  expect_error(pseudo_r2(fit), "not defined for a hierarchical multi-group")
  expect_error(pseudo_r2(fit, group = "2"), "not defined for a hierarchical multi-group")
  expect_error(netis_variance_components(fit), "not defined for a hierarchical multi-group")
  expect_error(plot(fit, what = "heterogeneity"), "not defined for a hierarchical multi-group")
})

test_that("the population effect on a hierarchical fit integrates that group's own variances", {
  skip_if_not_installed("amen")
  fit <- .hier_acc_fit()
  g <- "3"
  pop <- selection_effect(fit, "prior_tie", type = "population", group = g)
  ref <- selection_effect(fit, "prior_tie", type = "reference", group = g)
  part <- fit$per_group[[g]]
  des <- fit$designs[[g]]
  X <- .effect_design_matrix(des, colnames(part$draws))
  keep <- !is.na(as.vector(des$Y))
  vc <- part$variance_components
  expect_identical(nrow(vc), nrow(part$draws))
  hand <- vapply(seq_len(nrow(part$draws)), function(s) {
    b <- part$draws[s, ]
    sc <- sqrt(1 + vc[s, "va"] + vc[s, "vb"])
    # prior_tie is binary, so the contrast is 0 to 1 at every dyad.
    e0 <- as.vector(X[keep, ] %*% b) - X[keep, "prior_tie"] * b[["prior_tie"]]
    mean(stats::pnorm((e0 + b[["prior_tie"]]) / sc) - stats::pnorm(e0 / sc))
  }, numeric(1))
  expect_lt(max(abs(pop$draws - hand)), 1e-12)
  # The reference estimand ignores the actor-effect distribution, so the two differ.
  expect_gt(max(abs(pop$draws - ref$draws)), 1e-6)
})

test_that("a multi-chain fit's posterior means average every chain", {
  # Two hand-built chains with different lengths and known means. The combined
  # posterior mean is the draw-weighted average, not chain 1's.
  run <- function(ch, S, a, uv) {
    list(version = .MGC_DRAWS_VERSION, chain = rep(ch, S), iteration = seq_len(S),
         theta = array(ch, c(S, 1, 1), dimnames = list(NULL, "g", "intercept")),
         random_data_precision = array(NA_real_, c(S, 1, 1)),
         mu = matrix(ch, S, 1), tau = matrix(NA_real_, S, 1),
         ame = list(va = matrix(ch, S, 1)),
         posterior_means = list(g = list(a = a, b = -a, uv = uv)),
         control = list(chains = 1L))
  }
  r1 <- run(1L, 2L, a = c(1, 2, 3), uv = matrix(1, 3, 3))
  r2 <- run(2L, 3L, a = c(4, 0, -2), uv = matrix(6, 3, 3))
  out <- .mgc_combine_chains(list(r1, r2))
  expect_lt(max(abs(out$posterior_means$g$a - c(2.8, 0.8, 0))), 1e-12)
  expect_lt(max(abs(out$posterior_means$g$b - c(-2.8, -0.8, 0))), 1e-12)
  expect_lt(abs(out$posterior_means$g$uv[2, 3] - 4), 1e-12)
})

test_that("a two-chain hierarchical fit reports effects from both chains", {
  skip_if_not_installed("amen")
  two <- .hier_acc_fit(chains = 2L)
  one <- .hier_acc_fit(chains = 1L)
  # Chain 1 of the two-chain fit is the one-chain fit (same seed), so if only
  # chain 1 were used the actor effects would be identical.
  expect_false(isTRUE(all.equal(two$per_group[["2"]]$additive_effects$sender,
                                one$per_group[["2"]]$additive_effects$sender)))
})

test_that("n_excluded counts the same dyads on both routes", {
  skip_if_not_installed("amen")
  nd <- .hier_acc_data()
  ties <- netis_ties(nd)
  # Drop one wave-2 outcome tie in group 1: a missing outcome, not an exclusion.
  drop <- ties$wave == 2 & ties$source == "g1_01" & ties$target == "g1_02"
  ties$seen <- TRUE
  ndm <- netis_data(netis_nodes(nd), ties[!drop, ], wave_col = "wave",
                    tie_meta = list(tie = list(presence_mode = "complete_dyadic",
                                               presence_col = "seen",
                                               zero_meaning = "measured_zero")))
  two <- suppressWarnings(fit_selection(tie ~ prior_tie(), ndm, nscan = 30, burn = 10, odens = 10))
  hie <- suppressWarnings(fit_selection(tie ~ prior_tie(), ndm,
                                        pooling = selection_pooling(random = "intercept"),
                                        nscan = 30, burn = 10, odens = 10))
  expect_identical(unname(hie$n_excluded), unname(two$n_excluded))
})

test_that("an R > 0 Selection fit refuses only the null comparison, with one note", {
  skip_if_not_installed("amen")
  nd <- .hier_acc_data(G = 1)
  fit <- suppressWarnings(fit_selection(tie ~ prior_tie() + sender(x), nd, R = 1,
                                        nscan = 40, burn = 10, odens = 10))
  r2 <- pseudo_r2(fit)
  expect_false(any(grepl("Null-comparison", r2$measures$measure)))
  expect_identical(sum(grepl("reported only for R = 0", r2$notes)), 1L)
  expect_true(any(grepl("biased upward", r2$notes)))
  # The variance share at a known draw, by hand: the multiplicative variance is
  # that of the posterior-mean product over the modeled dyads.
  ex <- fit$per_group[[1]]
  des <- fit$designs[[1]]
  mask <- .selection_dyad_mask(des)
  X <- .effect_design_matrix(des, colnames(ex$draws))[!is.na(as.vector(des$Y)), ]
  v_uv <- stats::var(ex$multiplicative[mask])
  share <- vapply(seq_len(nrow(ex$draws)), function(s) {
    vf <- stats::var(as.vector(X %*% ex$draws[s, ]))
    vc <- ex$variance_components[s, ]
    vf / (vf + vc[["va"]] + vc[["vb"]] + v_uv + vc[["ve"]])
  }, numeric(1))
  expect_lt(abs(r2$measures$estimate[1] - mean(share)), 1e-12)
})

test_that("Selection plot panels on multi-group fits: true refusals, and a group draws", {
  skip_if_not_installed("amen")
  skip_if_not_installed("ggplot2")
  fit <- .hier_acc_fit(R = 1)
  build <- function(p) ggplot2::ggplot_build(p)
  expect_error(plot(fit, what = "actor_effects"), "each has its own sender and receiver effects")
  p <- plot(fit, what = "actor_effects", group = "2")
  expect_s3_class(build(p), "ggplot_built")
  d <- plot_data(fit, what = "actor_effects", group = "2")
  expect_identical(d$sender, as.numeric(fit$per_group[["2"]]$additive_effects$sender))
  expect_identical(d$node, fit$actor_ids[["2"]])
  expect_s3_class(build(plot(fit, what = "multiplicative", group = "1")), "ggplot_built")
  # Population-level draws without a group; that group's coefficient draws with one.
  expect_s3_class(build(plot(fit, what = "posterior")), "ggplot_built")
  pd <- plot_data(fit, what = "trace", group = "3")
  expect_identical(pd$value[pd$term == "snd_x"],
                   unname(fit$per_group[["3"]]$draws[, "snd_x"]))
  expect_s3_class(build(plot(fit, what = "mcmc", group = "3")), "ggplot_built")
  expect_error(plot(fit, what = "posterior", group = "9"), "not in this fit")

  # Groups fitted separately: the panels refuse without a group, and draw with one.
  sep <- suppressWarnings(fit_selection(tie ~ prior_tie(), .hier_acc_data(), pool_beta = FALSE,
                                        nscan = 30, burn = 10, odens = 10))
  for (w in c("posterior", "trace", "mcmc", "actor_effects")) {
    expect_error(plot(sep, what = w), "covers 3 groups")
    expect_s3_class(build(plot(sep, what = w, group = "1")), "ggplot_built")
  }
  expect_error(plot(sep, what = "multiplicative", group = "1"), "R = 0")
  # Coefficients: one labeled row per group and term, no overplotting.
  cd <- plot_data(sep, what = "coefficients")
  expect_identical(anyDuplicated(cd$term), 0L)
  expect_true("prior_tie [2]" %in% cd$term)
})

test_that("a single-group actor-effects panel renders", {
  skip_if_not_installed("amen")
  skip_if_not_installed("ggplot2")
  fit <- suppressWarnings(fit_selection(tie ~ prior_tie(), .hier_acc_data(G = 1),
                                        nscan = 30, burn = 10, odens = 10))
  expect_s3_class(ggplot2::ggplot_build(plot(fit, what = "actor_effects")), "ggplot_built")
})
