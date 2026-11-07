# What a Selection fit reports: effect sizes, term roles, findings, the standard
# model methods, and every plot panel, for each kind of fit.

.surface_nd <- function(G = 1L, n = 8L, seed = 21L, directed = TRUE) {
  set.seed(seed)
  gs <- letters[seq_len(G)]
  nodes <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("%s%02d", g, seq_len(n))
    data.frame(group = g, node = rep(ids, 2L), wave = rep(1:2, each = n),
               x = stats::rnorm(2L * n), reciprocal_trust = stats::rnorm(2L * n),
               stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("%s%02d", g, seq_len(n))
    p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
    p <- if (directed) p[p$source != p$target, ] else p[p$source < p$target, ]
    do.call(rbind, lapply(1:2, function(w) {
      data.frame(group = g, p, wave = w, tie = stats::rbinom(nrow(p), 1, 0.4),
                 stringsAsFactors = FALSE)
    }))
  }))
  netis_data(nodes, ties, wave_col = "wave", directed = directed,
             tie_meta = list(tie = list(zero_meaning = "measured_zero")))
}

.surface_fit <- function(formula, nd, ...) {
  suppressWarnings(fit_selection(formula, nd, nscan = 60, burn = 20, odens = 5, ...))
}

# --- selection_effect() ------------------------------------------------------

test_that("a binary predictor's unit contrast is 0 to 1 at every dyad", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ prior_tie() + sender(x), .surface_nd())
  ex <- fit$per_group[[1]]
  des <- fit$designs[[1]]
  B <- ex$draws
  off <- outer(ex$additive_effects$sender, ex$additive_effects$receiver, "+") + ex$multiplicative
  keep <- !is.na(des$Y)
  x <- outer(des$Xrow[, "snd_x"], rep(1, nrow(des$Y)))
  hand <- vapply(seq_len(nrow(B)), function(s) {
    base <- B[s, "intercept"] + B[s, "snd_x"] * x + off
    mean((stats::pnorm(base + B[s, "prior_tie"]) - stats::pnorm(base))[keep])
  }, numeric(1))
  e <- selection_effect(fit, "prior_tie")
  expect_lt(max(abs(e$draws - hand)), 1e-12)
  expect_identical(e$contrast, "0 to 1 at every dyad")
  expect_output(print(e), "prior_tie \\(0 to 1 at every dyad\\)")
})

test_that("a continuous predictor moves from each dyad's observed value", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ prior_tie() + sender(x), .surface_nd())
  ex <- fit$per_group[[1]]
  des <- fit$designs[[1]]
  B <- ex$draws
  off <- outer(ex$additive_effects$sender, ex$additive_effects$receiver, "+") + ex$multiplicative
  keep <- !is.na(des$Y)
  x <- outer(des$Xrow[, "snd_x"], rep(1, nrow(des$Y)))
  prior <- des$Xdyad[, , "prior_tie"]
  hand <- vapply(seq_len(nrow(B)), function(s) {
    base <- B[s, "intercept"] + B[s, "snd_x"] * x + B[s, "prior_tie"] * prior + off
    mean((stats::pnorm(base + 0.5 * B[s, "snd_x"]) - stats::pnorm(base))[keep])
  }, numeric(1))
  e <- selection_effect(fit, "snd_x", delta = 0.5)
  expect_lt(max(abs(e$draws - hand)), 1e-12)
  expect_identical(e$contrast, "observed value + 0.5")
  # A delta other than 1 on a binary predictor is what it says: observed + delta.
  expect_identical(selection_effect(fit, "prior_tie", delta = 2)$contrast, "observed value + 2")
})

test_that("a pooled fit's contrast says it reads the group's own draws", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ prior_tie(), .surface_nd(G = 2L))
  expect_error(selection_effect(fit, "prior_tie"), "one group's own coefficient draws")
  e <- selection_effect(fit, "prior_tie", group = "a")
  expect_match(e$coverage, "this group's own, from before pooling")
  # The contrast really is from group a's own draws.
  own <- fit$per_group[["a"]]$draws
  e_ref <- selection_effect(fit, "prior_tie", group = "a", type = "reference")
  hand <- stats::pnorm(own[, "intercept"] + own[, "prior_tie"]) - stats::pnorm(own[, "intercept"])
  expect_lt(max(abs(e_ref$draws - hand)), 1e-12)
  # Unpooled and single-group fits carry no such sentence.
  e1 <- selection_effect(.surface_fit(tie ~ prior_tie(), .surface_nd()), "prior_tie")
  expect_false(grepl("before pooling", e1$coverage))
})

# --- term roles --------------------------------------------------------------

test_that("a term's role comes from its specification, not from text in its name", {
  spec <- .parse_selection_formula(
    tie ~ sender(reciprocal_trust) + receiver(prior_tie_score) + dyad(w) + prior_tie() +
      prior_reciprocity() + sender(x, name = "custom")
  )
  roles <- .selection_term_roles(
    c("intercept", "snd_reciprocal_trust", "rcv_prior_tie_score", "dyd_w", "prior_tie",
      spec$prior_reciprocity$name, "custom"),
    spec
  )
  expect_identical(roles, c("intercept", "sender", "receiver", "dyadic", "prior tie",
                            "reciprocity", "sender"))
  spec$symmetric <- TRUE
  expect_identical(.selection_term_roles("custom", spec), "node")
})

test_that("netis_estimates() labels a misleadingly named covariate by its role", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ sender(reciprocal_trust) + prior_tie(), .surface_nd())
  est <- netis_estimates(fit)
  expect_identical(est$role[est$term == "snd_reciprocal_trust"], "sender")
})

# --- findings ----------------------------------------------------------------

test_that("netis_diagnostics() returns the fit's findings as rows", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ sender(x) + prior_tie(), .surface_nd())
  d <- netis_diagnostics(fit)
  expect_identical(names(d), c("family", "code", "level", "summary", "detail"))
  expect_identical(d$code, vapply(unclass(fit$diagnostics), function(f) f$code, character(1)))
  expect_identical(sum(d$level == "WARNING"), netis_glance(fit)$n_warning)
  expect_error(netis_diagnostics(lm(dist ~ speed, cars)), "no method")
})

test_that("an empty fit record gives zero rows", {
  empty <- structure(list(diagnostics = NULL), class = c("netis_selection", "netis_fit"))
  d <- netis_diagnostics(empty)
  expect_identical(nrow(d), 0L)
  expect_identical(names(d), c("family", "code", "level", "summary", "detail"))
})

test_that("a one-line summary is not cut at an abbreviation", {
  expect_identical(.first_sentence("Check this, e.g. the wave. Second."), "Check this, e.g. the wave.")
  expect_identical(.first_sentence("Use a rule, i.e. nonzero()! Then fit."), "Use a rule, i.e. nonzero()!")
  expect_identical(.first_sentence("No terminator here"), "No terminator here")
  expect_identical(.first_sentence("One. Two."), "One.")
  expect_true(is.na(.first_sentence("")))
})

test_that("summary() says how many findings a fit recorded", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ sender(x) + prior_tie(), .surface_nd())
  n <- netis_glance(fit)$n_warning
  expect_output(print(summary(fit)), sprintf("Fit-time findings: %d WARNING", n))
})

test_that("a hierarchical fit keeps the data findings its designs raised", {
  skip_if_not_installed("amen")
  nd <- .surface_nd(G = 3L)
  nodes <- netis_nodes(nd)
  nodes$x[nodes$node == "b03" & nodes$wave == 2] <- NA
  nd2 <- netis_data(nodes, netis_ties(nd), wave_col = "wave",
                    tie_meta = list(tie = list(zero_meaning = "measured_zero")))
  fit <- .surface_fit(tie ~ sender(x) + prior_tie(), nd2,
                      pooling = selection_pooling(random = "intercept"))
  expect_true("selection_predictor_unresolvable" %in% netis_diagnostics(fit)$code)
  expect_identical(unname(fit$n_excluded["b"]), 7)
})

# --- netis_hierarchy() -------------------------------------------------------

test_that("a random intercept's information rule is stated, not NA", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ prior_tie(), .surface_nd(G = 3L),
                      pooling = selection_pooling(random = "intercept"))
  info <- netis_hierarchy(fit)$information
  expect_identical(info$informative_threshold, "at least one observed dyad")
  expect_false(grepl("NA", info$informative_threshold))
  # 8 actors: 56 dyads in each group.
  expect_identical(info$support_n, c(56, 56, 56))
})

# --- standard methods --------------------------------------------------------

test_that("the methods that have no valid answer stop and say why", {
  skip_if_not_installed("amen")
  fits <- list(
    single = .surface_fit(tie ~ prior_tie(), .surface_nd()),
    hier = .surface_fit(tie ~ prior_tie(), .surface_nd(G = 3L),
                        pooling = selection_pooling(random = "intercept"))
  )
  for (fit in fits) {
    expect_error(fitted(fit), "not defined for a Selection fit")
    expect_error(residuals(fit), "not defined for a Selection fit")
    expect_error(sigma(fit), "not defined for a Selection fit")
    expect_error(predict(fit), "not defined for a Selection fit")
    expect_error(logLik(fit), "no log-likelihood")
    expect_error(AIC(fit), "no log-likelihood")
    expect_error(BIC(fit), "no log-likelihood")
  }
})

test_that("confint() and the printed table give the accessor's interval", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ prior_tie() + sender(x), .surface_nd())
  est <- netis_estimates(fit)
  ci <- confint(fit)
  expect_identical(rownames(ci), est$term)
  expect_identical(colnames(ci), c("2.5 %", "97.5 %"))
  expect_identical(unname(ci[, 1]), est$conf.low)
  # A quantile interval of the draws, not mean +/- 1.96 SD.
  d <- fit$per_group[[1]]$draws
  expect_lt(abs(ci["prior_tie", 1] - stats::quantile(d[, "prior_tie"], 0.025, names = FALSE)), 1e-12)
  tab <- .selection_estimate_table(fit)
  expect_identical(
    tab[["95% interval"]][tab$Term == "prior_tie"],
    sprintf("[%s, %s]", format(round(est$conf.low[est$term == "prior_tie"], 4), trim = TRUE),
            format(round(est$conf.high[est$term == "prior_tie"], 4), trim = TRUE))
  )
  expect_identical(rownames(confint(fit, "snd_x")), "snd_x")
  expect_identical(colnames(confint(fit, level = 0.9)), c("5 %", "95 %"))
})

test_that("confint() on a Selection fit refuses a method it does not apply", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ prior_tie(), .surface_nd())
  expect_error(confint(fit, method = "profile"), "no `method` argument")
})

test_that("confint() reads pooled standard errors, and refuses uncombined groups", {
  skip_if_not_installed("amen")
  nd <- .surface_nd(G = 2L)
  fit <- .surface_fit(tie ~ prior_tie(), nd)
  se <- sqrt(fit$pooled_beta$var)[["prior_tie"]]
  expect_lt(abs(confint(fit)["prior_tie", 2] -
                  (coef(fit)[["prior_tie"]] + stats::qnorm(0.975) * se)), 1e-12)
  expect_error(confint(.surface_fit(tie ~ prior_tie(), nd, pool_beta = FALSE)),
               "one interval per group and term")
})

test_that("coef(), vcov() and confint() of a hierarchical fit share one set of names", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ prior_tie() + sender(x), .surface_nd(G = 3L),
                      pooling = selection_pooling(random = "intercept", unpooled = "snd_x"))
  nms <- c("intercept", "snd_x", "prior_tie")
  expect_identical(names(coef(fit)), nms)
  expect_identical(dimnames(vcov(fit)), list(nms, nms))
  expect_identical(rownames(confint(fit)), nms)
  # The unpooled coefficient has no population value anywhere.
  expect_true(is.na(coef(fit)[["snd_x"]]))
  expect_true(all(is.na(vcov(fit)["snd_x", ])))
  expect_true(all(is.na(confint(fit)["snd_x", ])))
  # The others are the population draws' own.
  mu <- fit$mgc_draws$mu
  expect_lt(abs(vcov(fit)["prior_tie", "intercept"] -
                  stats::cov(mu[, "prior_tie"], mu[, "intercept"])), 1e-12)
  expect_lt(abs(confint(fit)["prior_tie", 1] -
                  stats::quantile(mu[, "prior_tie"], 0.025, names = FALSE)), 1e-12)
})

# --- every panel, built --------------------------------------------------------

.panel_builds <- function(fit, what, group = NULL) {
  p <- plot(fit, what = what, group = group)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  invisible(plot_data(fit, what = what, group = group))
}

test_that("every panel of a single-group fit builds", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ prior_tie() + sender(x), .surface_nd(), R = 1)
  for (w in c("coefficients", "heterogeneity", "posterior", "trace", "actor_effects",
              "multiplicative", "mcmc")) {
    .panel_builds(fit, w)
  }
  expect_error(plot(fit, what = "groups"), "one estimate per term")
  d <- plot_data(fit, what = "coefficients")
  expect_match(attr(d, "netis_caption"), "95% equal-tailed posterior quantile", all = FALSE)
})

test_that("the multiplicative table has one row per ordered pair of different actors", {
  skip_if_not_installed("amen")
  fit <- .surface_fit(tie ~ prior_tie(), .surface_nd(n = 6L), R = 1)
  d <- plot_data(fit, what = "multiplicative")
  expect_identical(nrow(d), 30L)
  expect_false(any(d$sender == d$receiver))
  uv <- fit$per_group[[1]]$multiplicative
  ids <- fit$actor_ids[[1]]
  expect_lt(abs(d$value[d$sender == ids[2] & d$receiver == ids[5]] - uv[2, 5]), 1e-12)
  expect_lt(abs(d$value[d$sender == ids[5] & d$receiver == ids[2]] - uv[5, 2]), 1e-12)
})

test_that("a symmetric fit's actor table has one effect column", {
  skip_if_not_installed("amen")
  nd <- .surface_nd(directed = FALSE)
  nodes <- netis_nodes(nd)
  fit <- suppressWarnings(fit_selection(tie ~ sender(x) + prior_tie(), nd, symmetric = TRUE,
                                        nscan = 60, burn = 20, odens = 5))
  d <- plot_data(fit, what = "actor_effects")
  expect_true("effect" %in% names(d))
  expect_false(any(c("sender", "receiver") %in% names(d)))
  expect_equal(d$effect, as.numeric(fit$per_group[[1]]$additive_effects$sender))
  expect_s3_class(ggplot2::ggplot_build(plot(fit, what = "actor_effects")), "ggplot_built")
})

test_that("every panel of a pooled and an unpooled fit builds, with a group where needed", {
  skip_if_not_installed("amen")
  nd <- .surface_nd(G = 2L)
  for (fit in list(.surface_fit(tie ~ prior_tie() + sender(x), nd, R = 1),
                   .surface_fit(tie ~ prior_tie() + sender(x), nd, R = 1, pool_beta = FALSE))) {
    .panel_builds(fit, "coefficients")
    .panel_builds(fit, "heterogeneity")
    for (w in c("posterior", "trace", "actor_effects", "multiplicative", "mcmc")) {
      expect_error(plot(fit, what = w), "Name one")
      .panel_builds(fit, w, group = "b")
    }
  }
  pooled <- .surface_fit(tie ~ prior_tie(), nd)
  expect_match(attr(plot_data(pooled, what = "coefficients"), "netis_caption"),
               "normal intervals from the pooled standard errors", all = FALSE)
})

test_that("a dependence object has one panel and refuses another", {
  dep <- structure(list(), class = "netis_dependence")
  expect_error(plot(dep, what = "bogus"), "one panel")
  expect_error(plot_data(dep, what = "bogus"), "one panel")
})

# --- group identity in a hierarchical fit ---------------------------------------

test_that("a hierarchical fit keeps each group's estimate with that group's data", {
  skip_if_not_installed("amen")
  # Tie rates differ strongly and do not follow the groups' sorted order, so an
  # estimate attached to the wrong group would show.
  set.seed(31)
  rates <- c(a = 0.5, b = 0.1, c = 0.9, d = 0.3)
  nodes <- do.call(rbind, lapply(names(rates), function(g) {
    data.frame(group = g, node = paste0(g, 1:10), x = stats::rnorm(10), stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(names(rates), function(g) {
    ids <- paste0(g, 1:10)
    p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
    p <- p[p$source != p$target, ]
    data.frame(group = g, p, tie = stats::rbinom(nrow(p), 1, rates[[g]]), stringsAsFactors = FALSE)
  }))
  nd <- netis_data(nodes, ties, tie_meta = list(tie = list(zero_meaning = "measured_zero")))
  fit <- suppressWarnings(fit_selection(tie ~ sender(x), nd,
                                        pooling = selection_pooling(random = "intercept"),
                                        nscan = 200, burn = 100, odens = 5))
  est <- netis_estimates(fit)
  ints <- est[est$term == "intercept" & !is.na(est$group), ]
  got <- stats::setNames(ints$estimate, ints$group)
  expect_identical(names(sort(got)), c("b", "d", "a", "c"))
  # And each sits near its own probit scale, within the width of its interval.
  expect_lt(got[["b"]], -0.5)
  expect_gt(got[["c"]], 0.5)
})
