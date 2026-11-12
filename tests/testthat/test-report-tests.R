# The test columns, significance marks and fit lines print() and summary() add
# to a fit's coefficient table.
#
# What is pinned here is that each printed number is the backend's own -- the
# t and p of summary.lm(), lmerTest's Satterthwaite table, a tail probability
# counted from the retained draws -- and that the printed surfaces name the
# assumptions and the posterior predictive check without listing them.

.rt_data <- function(n_groups = 6L, n = 12L, seed = 11L) {
  set.seed(seed)
  ids <- sprintf("a%02d", seq_len(n))
  nodes <- do.call(rbind, lapply(seq_len(n_groups), function(g) {
    y1 <- stats::rnorm(n, g / 3)
    x <- stats::rnorm(n)
    data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = n),
               y = c(y1, 0.5 * y1 + 0.3 * x + stats::rnorm(n, sd = 0.6) + g / 4),
               x = rep(x, 2))
  }))
  ties <- do.call(rbind, lapply(seq_len(n_groups), function(g) {
    p <- subset(expand.grid(source = ids, target = ids, stringsAsFactors = FALSE),
                source != target)
    p <- p[stats::rbinom(nrow(p), 1, 0.25) == 1, ]
    data.frame(group = g, p, wave = 1, w = 1)
  }))
  netis_data(nodes, ties, wave_col = "wave")
}

.rt_ols <- function() {
  fit_influence(y ~ ar() + x + exposure(y, via = w), data = .rt_data(n_groups = 1L))
}

.rt_mixed <- function() {
  fit_influence(y ~ ar() + x + exposure(y, via = w) + (1 | group), data = .rt_data())
}

# Line breaks and the wrapping indent collapse to single spaces: what is pinned
# is what is said, not where an 88-column wrap happens to fall.
.rt_out <- function(x) {
  gsub("[ \n]+", " ", paste(utils::capture.output(print(x)), collapse = "\n"))
}

# ---- the marks ---------------------------------------------------------------

test_that("significance marks sit on lm's cutoffs, and NA gets none", {
  expect_identical(
    .netis_stars(c(0.0005, 0.001, 0.005, 0.03, 0.07, 0.5, NA)),
    c("***", "***", "**", "*", ".", "", "")
  )
})

test_that("a posterior tail probability cannot earn a mark it could not measure", {
  # 200 draws, none below zero: the count is 0, but all the chain can say is
  # that the tail probability is below 2/200. The mark comes from that bound.
  t <- .posterior_tail(seq(0.5, 1.5, length.out = 200))
  expect_identical(unname(t[["tail"]]), 0)
  expect_equal(unname(t[["floor"]]), 0.01)
  expect_identical(.netis_stars(max(t[["tail"]], t[["floor"]])), "**")
  expect_identical(.netis_format_p(t[["tail"]], floor = t[["floor"]]), "<0.010")
  # Twice the smaller side, counted.
  v <- c(rep(-1, 10), rep(1, 190))
  expect_equal(unname(.posterior_tail(v)[["tail"]]), 0.1)
})

# ---- Influence ---------------------------------------------------------------

test_that("a least-squares fit prints summary.lm()'s own t tests", {
  fit <- .rt_ols()
  tests <- .influence_coef_tests(fit)
  ref <- stats::coef(summary(fit$backend_fit))
  expect_identical(tests$kind, "t")
  expect_equal(tests$statistic, unname(ref[, "t value"]))
  expect_equal(tests$p, unname(ref[, "Pr(>|t|)"]))
  expect_true(all(tests$df == fit$backend_fit$df.residual))

  out <- .rt_out(fit)
  expect_match(out, "t value", fixed = TRUE)
  expect_match(out, "Pr(>|t|)", fixed = TRUE)
  expect_match(out, "Residual standard error: .* degrees of freedom")
  expect_match(out, sprintf("R-squared: %.4f, adjusted: %.4f",
                            fit$r_squared, fit$adj_r_squared), fixed = TRUE)
  f <- summary(fit$backend_fit)$fstatistic
  expect_match(.rt_out(summary(fit)),
               sprintf("F-statistic: %.4g on %d and %d DF", f[[1]],
                       as.integer(f[[2]]), as.integer(f[[3]])), fixed = TRUE)
})

test_that("a mixed fit prints lmerTest's Satterthwaite table, and says so", {
  fit <- .rt_mixed()
  # The fit is an lmerTest fit, with the estimates lme4 gives.
  expect_true(methods::is(fit$backend_fit, "lmerModLmerTest"))
  ref <- stats::coef(summary(fit$backend_fit))
  tests <- .influence_coef_tests(fit)
  expect_equal(tests$df, unname(ref[, "df"]))
  expect_equal(tests$statistic, unname(ref[, "t value"]))
  expect_equal(tests$p, unname(ref[, "Pr(>|t|)"]))
  plain <- lme4::lmer(stats::formula(fit$backend_fit),
                      data = stats::model.frame(fit$backend_fit))
  expect_equal(unname(lme4::fixef(plain)), unname(fit$coefficients), tolerance = 1e-8)

  out <- .rt_out(fit)
  expect_match(out, "Satterthwaite", fixed = TRUE)
  expect_match(out, "lmerTest", fixed = TRUE)
  expect_match(out, "R-squared (Nakagawa-Schielzeth): marginal", fixed = TRUE)
  r2 <- pseudo_r2(fit)$measures$estimate
  expect_match(out, sprintf("marginal %.4f, conditional %.4f", r2[1], r2[2]), fixed = TRUE)
  expect_match(.rt_out(summary(fit)), "Random effects: Groups Name Variance")
})

test_that("a mixed fit without lmerTest results prints t and invents no df", {
  fit <- .rt_mixed()
  fit$coef_tests <- NULL
  tests <- .influence_coef_tests(fit)
  expect_null(tests$df)
  expect_null(tests$p)
  out <- .rt_out(fit)
  expect_match(out, "t value", fixed = TRUE)
  expect_false(grepl("Pr(>|t|)", out, fixed = TRUE))
  # A current fit that lmerTest could not test is told that, and is not told
  # to refit; both absences say which quantile the intervals fall back to.
  expect_match(out, "lmerTest could not compute", fixed = TRUE)
  expect_false(grepl("Refit", out, fixed = TRUE))
  expect_match(out, "intervals use the normal quantile", fixed = TRUE)

  # A fit saved before the backend fitted through lmerTest holds a plain
  # lmerMod, and a refit is the right advice there.
  old <- fit
  old$backend_fit <- lme4::lmer(stats::formula(fit$backend_fit),
                                data = stats::model.frame(fit$backend_fit))
  old_out <- .rt_out(old)
  expect_match(old_out, "carries no lmerTest results", fixed = TRUE)
  expect_match(old_out, "Refit with the current rnetis", fixed = TRUE)
})

test_that("print() and summary() name the assumptions and do not list them", {
  fit <- .rt_ols()
  for (out in list(.rt_out(fit), .rt_out(summary(fit)))) {
    expect_match(out, sprintf("rests on %d assumptions", length(fit$assumptions$entries)),
                 fixed = TRUE)
    expect_match(out, "netis_assumptions(fit)", fixed = TRUE)
    # No ledger row, in either its full or its compact form.
    expect_false(grepl("[A4]", out, fixed = TRUE))
    expect_false(grepl("Assumptions carrying this fit", out, fixed = TRUE))
  }
  # The ledger itself is unchanged and both of its forms are still reachable.
  expect_match(.rt_out(netis_assumptions(fit)), "[A4]", fixed = TRUE)
  compact <- utils::capture.output(print(netis_assumptions(fit), compact = TRUE))
  expect_identical(compact, .format_assumptions_compact(fit$assumptions))
})

# ---- Selection ---------------------------------------------------------------

test_that("a Selection table's posterior columns are counted from its draws", {
  skip_if_not_installed("amen")
  fit <- .selection_fixture_fit()
  tab <- .selection_estimate_table(fit)
  expect_true(all(c("Post.SD", "Est/SD", "Tail prob.", " ") %in% names(tab)))
  d <- .selection_beta_draws(fit$draws, tab$Term)
  for (k in tab$Term) {
    t <- .posterior_tail(d[, k])
    i <- which(tab$Term == k)
    expect_identical(tab[["Est/SD"]][i], sprintf("%.3f", mean(d[, k]) / stats::sd(d[, k])))
    expect_identical(tab[["Tail prob."]][i], .netis_format_p(t[["tail"]], floor = t[["floor"]]))
    expect_identical(tab[[" "]][i], .netis_stars(max(t[["tail"]], t[["floor"]])))
  }
  # Labelled as a posterior, in the output a reader sees.
  out <- .rt_out(fit)
  expect_match(out, "Posterior summaries, not tests", fixed = TRUE)
  expect_false(grepl("Pr(>|", out, fixed = TRUE))
})

test_that("a Selection fit prints its pseudo R-squared and one line of PPC", {
  skip_if_not_installed("amen")
  fit <- .selection_fixture_fit()
  out <- .rt_out(fit)
  r2 <- pseudo_r2(fit, include_null_comparison = FALSE)$measures
  expect_match(out, sprintf("latent-scale variance share %.3f", r2$estimate[1]), fixed = TRUE)
  expect_match(out, "pseudo_r2(fit)", fixed = TRUE)

  g <- netis_gof(fit)
  inside <- sum(g$observed >= g$predicted_lower & g$observed <= g$predicted_upper)
  expect_match(out, sprintf("inside the simulated 95%% range on %d of %d statistics",
                            inside, nrow(g)))
  expect_match(out, "netis_gof(fit)", fixed = TRUE)
  expect_match(out, "plot(fit, what = \"gof\")", fixed = TRUE)
  # The table is summary()'s, not print()'s.
  expect_false(grepl("Simulated 95% range", out, fixed = TRUE))
  s <- .rt_out(summary(fit))
  expect_match(s, "Simulated 95% range", fixed = TRUE)
  for (st in g$statistic) expect_match(s, st, fixed = TRUE)
})

test_that("a fit with no simulated networks says so instead of failing to print", {
  skip_if_not_installed("amen")
  fit <- .selection_fixture_fit()
  fit$per_group[[1]]$gof <- fit$per_group[[1]]$gof[1, , drop = FALSE]
  expect_error(netis_gof(fit), "gof = FALSE")
  expect_match(.rt_out(fit), "no simulated networks were kept", fixed = TRUE)
})
