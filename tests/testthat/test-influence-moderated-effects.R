# The moderated branches of influence_effect(), by hand: the conditional table
# with its covariance term, the predicted change at each moderator value, the
# moderated ar() entry, and the Johnson-Neyman crossings.

.me_data <- function(n = 40L, seed = 3L, slope = 0.1, groups = 1L) {
  set.seed(seed)
  frames <- lapply(seq_len(groups), function(g) {
    ids <- sprintf("g%dn%02d", g, seq_len(n))
    source_a <- ids[c(2:n, 1)]
    source_b <- ids[c(5:n, 1:4)]
    x1 <- stats::rnorm(n)
    z <- stats::rnorm(n, 1, 2)
    y1 <- stats::rnorm(n)
    ex <- (x1[match(source_a, ids)] + x1[match(source_b, ids)])
    y2 <- 0.2 * y1 + slope * z * ex + stats::rnorm(n, sd = 0.3) + g / 2
    list(
      nodes = data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = n),
                         y = c(y1, y2), x = c(x1, stats::rnorm(n)), z = c(z, z)),
      ties = data.frame(group = g, source = c(source_a, source_b), target = rep(ids, 2),
                        wave = 1, tie = 1)
    )
  })
  netis_data(do.call(rbind, lapply(frames, `[[`, "nodes")),
             do.call(rbind, lapply(frames, `[[`, "ties")), wave_col = "wave")
}

test_that("a moderated exposure entry is the hand-computed conditional table", {
  nd <- .me_data()
  fit <- fit_influence(y ~ ar() + exposure(x, via = tie) * z, data = nd)
  e <- influence_effect(fit)$exposures$ex_x_tie
  expect_true(e$moderated)
  expect_identical(e$moderator, "z")
  expect_null(e$predicted_change)
  expect_null(e$coefficient)

  mf <- fit$model_frame
  z_q <- stats::quantile(mf$z, c(0.1, 0.9), names = FALSE)
  x_q <- stats::quantile(mf$ex_x_tie, c(0.1, 0.9), names = FALSE)
  expect_identical(e$conditional$z, c(0, z_q))
  expect_identical(c(e$contrast_low, e$contrast_high), x_q)

  # The same quantity from a plain lm(), recentered at each z: the coefficient on
  # the exposure there is the conditional effect and its interval is lm()'s own.
  for (i in 1:3) {
    z0 <- e$conditional$z[i]
    ref <- stats::lm(.outcome ~ y_lag1 + ex_x_tie * I(z - z0), data = mf)
    ci <- stats::confint(ref)["ex_x_tie", ]
    expect_lt(abs(e$conditional$estimate[i] - stats::coef(ref)[["ex_x_tie"]]), 1e-10)
    expect_lt(abs(e$conditional$se[i] - sqrt(stats::vcov(ref)["ex_x_tie", "ex_x_tie"])), 1e-10)
    expect_lt(max(abs(c(e$conditional$ci_low[i], e$conditional$ci_high[i]) - ci)), 1e-10)
    expect_lt(abs(e$conditional$predicted_change[i] -
                    stats::coef(ref)[["ex_x_tie"]] * (x_q[2] - x_q[1])), 1e-10)
  }
})

test_that("a moderated ar() entry has the same table, without the predicted change", {
  nd <- .me_data()
  fit <- fit_influence(y ~ ar() * z + exposure(x, via = tie), data = nd)
  eff <- influence_effect(fit)
  a <- eff$ar$y_lag1
  expect_identical(a$moderator, "z")
  expect_identical(a$coefficient_at_z0, unname(coef(fit)["y_lag1"]))
  expect_false("predicted_change" %in% names(a$conditional))
  mf <- fit$model_frame
  for (i in 1:3) {
    z0 <- a$conditional$z[i]
    ref <- stats::lm(.outcome ~ y_lag1 * I(z - z0) + ex_x_tie, data = mf)
    expect_lt(abs(a$conditional$estimate[i] - stats::coef(ref)[["y_lag1"]]), 1e-10)
    expect_lt(abs(a$conditional$se[i] - sqrt(stats::vcov(ref)["y_lag1", "y_lag1"])), 1e-10)
  }
  # An unmoderated exposure beside it keeps the scalar shape; an unmoderated ar()
  # has no entry.
  expect_false(eff$exposures$ex_x_tie$moderated)
  expect_true(is.numeric(eff$exposures$ex_x_tie$predicted_change))
  plain <- influence_effect(fit_influence(y ~ ar() + exposure(x, via = tie), data = nd))
  expect_length(plain$ar, 0L)
})

test_that("Johnson-Neyman boundaries are where the interval's endpoint is zero", {
  nd <- .me_data(slope = 0.1)
  fit <- fit_influence(y ~ ar() + exposure(x, via = tie) * z, data = nd)
  e <- influence_effect(fit)$exposures$ex_x_tie
  b <- e$jn_boundaries
  expect_true(e$jn_applicable)
  expect_gte(length(b), 1L)
  vc <- stats::vcov(fit)
  crit <- stats::qt(0.975, stats::df.residual(fit$backend_fit))
  beta <- coef(fit)[["ex_x_tie"]]
  inter <- grep(":", names(coef(fit)), value = TRUE)
  gamma <- coef(fit)[[inter]]
  at <- function(z) {
    se <- sqrt(vc["ex_x_tie", "ex_x_tie"] + z^2 * vc[inter, inter] +
                 2 * z * vc["ex_x_tie", inter])
    abs(beta + z * gamma) - crit * se
  }
  for (z in b) expect_lt(abs(at(z)), 1e-8)
  # And nowhere else on a scan: the sign of the gap changes only at the boundaries.
  grid <- seq(min(b) - 20, max(b) + 20, length.out = 4001)
  changes <- grid[-1][diff(sign(at(grid))) != 0]
  expect_identical(length(changes), length(b))
  expect_lt(max(abs(sort(changes) - sort(b))), 0.02)

  out <- paste(utils::capture.output(print(influence_effect(fit))), collapse = " ")
  expect_match(out, "the interval's relationship to zero changes at z =", fixed = TRUE)
  expect_false(grepl("significant", out, ignore.case = TRUE))

  # No crossing: the wording says so and reports the state at z = 0.
  noisy <- fit_influence(y ~ ar() + exposure(x, via = tie) * z, data = .me_data(n = 12L, slope = 0))
  region <- .describe_jn_region(list(jn_applicable = TRUE, jn_boundaries = numeric(0),
                                     conditional = data.frame(ci_low = -1, ci_high = 1)))
  expect_match(region, "includes zero at every value", fixed = TRUE)
  expect_s3_class(influence_effect(noisy), "netis_influence_effects")
  # Not applicable is a stated absence, not a crash.
  bad <- .johnson_neyman_boundaries(NaN, 1, 1, 1, 0, 1.96)
  expect_false(bad$applicable)
  expect_match(.describe_jn_region(list(jn_applicable = FALSE)), "not available", fixed = TRUE)
})

test_that("on an lme4 fit the conditional table uses the normal quantile, and composes with a random slope", {
  skip_if_not_installed("lme4")
  nd <- .me_data(n = 20L, groups = 6L)
  fit <- suppressWarnings(suppressMessages(
    fit_influence(y ~ ar() + exposure(x, via = tie) * z + (1 | group), data = nd)
  ))
  e <- influence_effect(fit)$exposures$ex_x_tie
  half <- (e$conditional$ci_high - e$conditional$ci_low) / 2
  expect_lt(max(abs(half - stats::qnorm(0.975) * e$conditional$se)), 1e-10)

  # A moderator and a random exposure slope are different claims and compose.
  both <- suppressWarnings(suppressMessages(
    fit_influence(y ~ ar() + exposure(x, via = tie) * z + (1 + ex_x_tie | group), data = nd)
  ))
  entry <- influence_effect(both)$exposures$ex_x_tie
  expect_true(entry$moderated)
  expect_true(is.numeric(entry$group_sd))
  expect_length(entry$group_range, 2L)
  ranef_slope <- lme4::ranef(both$backend_fit)$group[["ex_x_tie"]]
  expect_lt(max(abs(entry$group_range - (coef(both)[["ex_x_tie"]] + range(ranef_slope)))), 1e-8)

  # The figure refuses a moderated term by name, and draws the term it can.
  expect_error(plot(fit, what = "effects"), "does not plot a moderated term")
})
