# The default Wald interval uses the reference distribution each backend's own
# inference supports: t on the residual degrees of freedom for least squares, t
# on lmerTest's Satterthwaite degrees of freedom for lme4, normal for lnam. Each
# is compared with the backend's own numbers, computed from the backend object,
# or with the normal formula written out by hand.

test_that("an lme4 fit's default interval is the t interval on lmerTest's Satterthwaite df", {
  skip_if_not_installed("lme4")
  fit <- .vc_intercept_fit()
  # The reference is lmerTest's own table, read from the backend object: the
  # estimate, its standard error and its Satterthwaite degrees of freedom.
  tab <- stats::coef(summary(fit$backend_fit))
  half <- stats::qt(0.975, tab[, "df"]) * tab[, "Std. Error"]
  ref <- cbind(tab[, "Estimate"] - half, tab[, "Estimate"] + half)
  ours <- stats::confint(fit)
  expect_lt(max(abs(ours[rownames(ref), ] - ref)), 1e-10)

  # THE POINT OF IT: an interval excludes zero exactly when its printed test's
  # p-value is below 0.05, for every coefficient.
  excludes <- ours[rownames(ref), 1] > 0 | ours[rownames(ref), 2] < 0
  expect_identical(unname(excludes), unname(tab[, "Pr(>|t|)"] < 0.05))

  # lme4's own normal-quantile interval is still there, under its own name,
  # and it is the narrower of the two.
  wald <- stats::confint(fit, method = "Wald")[rownames(ours), ]
  expect_true(all(wald[, 2] - wald[, 1] < ours[, 2] - ours[, 1]))

  # A fit carrying no lmerTest results falls back to the normal quantile.
  bare <- fit
  bare$coef_tests <- NULL
  expect_lt(max(abs(stats::confint(bare) - wald)), 1e-10)

  # The printed table and netis_estimates() carry the same interval.
  est <- netis_estimates(fit)
  expect_lt(max(abs(est$conf.low - ours[est$term, 1])), 1e-10)
  frame <- .estimate_frame(fit)
  expect_lt(max(abs(frame$conf.high - ours[frame$term, 2])), 1e-10)

  # And the effect layer.
  eff <- influence_effect(fit)$exposures[["ex_x_tie"]]
  expect_lt(abs(eff$ci_low - ref["ex_x_tie", 1]), 1e-10)
  expect_lt(abs(eff$ci_high - ref["ex_x_tie", 2]), 1e-10)
})

test_that("an lnam fit's default interval is the asymptotic normal one", {
  skip_if_not_installed("sna")
  set.seed(8)
  n <- 25
  ids <- sprintf("m%02d", sample(n))
  nodes <- data.frame(group = 1, node = ids, y = stats::rnorm(n), x = stats::rnorm(n))
  ties <- data.frame(group = 1, source = ids[-n], target = ids[-1], tie = 1)
  fit <- fit_influence(y ~ x + exposure(y, via = tie, x_lag = 0),
                       data = netis_data(nodes, ties))
  est <- coef(fit)
  se <- sqrt(diag(vcov(fit)))[names(est)]
  z <- stats::qnorm(0.975)
  ci <- stats::confint(fit)
  expect_lt(max(abs(ci[, 1] - (est - z * se))), 1e-12)
  expect_lt(max(abs(ci[, 2] - (est + z * se))), 1e-12)
})

test_that("an OLS fit's default interval is still confint.lm()", {
  set.seed(9)
  ids <- letters[1:10]
  nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = 10),
                      y = stats::rnorm(20), x = stats::rnorm(20))
  ties <- data.frame(group = 1, source = ids[-10], target = ids[-1], wave = 1, tie = 1)
  fit <- fit_influence(y ~ ar() + exposure(x, via = tie),
                       data = netis_data(nodes, ties, wave_col = "wave"))
  ref <- stats::confint(fit$backend_fit)
  rownames(ref) <- gsub("`", "", rownames(ref))
  expect_lt(max(abs(stats::confint(fit) - ref[names(coef(fit)), ])), 1e-12)
})

test_that("confint() refuses a method the backend does not apply, and keeps those it does", {
  set.seed(9)
  ids <- letters[1:10]
  nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = 10),
                      y = stats::rnorm(20), x = stats::rnorm(20))
  ties <- data.frame(group = 1, source = ids[-10], target = ids[-1], wave = 1, tie = 1)
  ols <- fit_influence(y ~ ar() + exposure(x, via = tie),
                       data = netis_data(nodes, ties, wave_col = "wave"))
  for (m in c("profile", "boot", "Wald", "nonsense")) {
    expect_error(stats::confint(ols, method = m), "not available on this stats fit")
  }
  expect_error(stats::confint(ols, method = NULL), "not available")
  # What does work on least squares, each identical to confint.lm().
  ref <- stats::confint(ols$backend_fit)
  rownames(ref) <- gsub("`", "", rownames(ref))
  ref <- ref[names(coef(ols)), ]
  expect_lt(max(abs(stats::confint(ols) - ref)), 1e-12)
  expect_lt(max(abs(stats::confint(ols, method = "backend")[names(coef(ols)), ] - ref)), 1e-12)
})

test_that("on an lme4 fit each method is the one lme4 computes", {
  skip_if_not_installed("lme4")
  fit <- .vc_intercept_fit()
  term <- "ex_x_tie"
  expect_error(stats::confint(fit, method = "nonsense"), "not available on this lme4 fit")
  wald <- stats::confint(fit, parm = term)
  profile <- stats::confint(fit, parm = term, method = "profile")
  direct <- suppressMessages(stats::confint(fit$backend_fit, parm = term, method = "profile"))
  expect_lt(max(abs(profile - direct)), 1e-8)
  # A different interval from the default on this fit, so the method did apply.
  expect_gt(max(abs(profile - wald)), 1e-6)
  set.seed(1)
  # Twenty resamples is enough to compare the two calls, and boot warns about its tails.
  boot <- suppressWarnings(stats::confint(fit, parm = term, method = "boot", nsim = 20))
  set.seed(1)
  direct <- suppressWarnings(suppressMessages(
    stats::confint(fit$backend_fit, parm = term, method = "boot", nsim = 20)
  ))
  expect_lt(max(abs(boot - direct)), 1e-10)
})

test_that("an lnam fit offers the Wald interval alone", {
  skip_if_not_installed("sna")
  set.seed(4)
  ids <- letters[1:15]
  nd <- netis_data(
    data.frame(group = 1, node = ids, y = stats::rnorm(15), x = stats::rnorm(15)),
    data.frame(group = 1, source = ids[-15], target = ids[-1], tie = 1)
  )
  fit <- fit_influence(y ~ x + exposure(y, via = tie, x_lag = 0), data = nd)
  for (m in c("profile", "boot", "backend", "Wald")) {
    expect_error(stats::confint(fit, method = m), "not available on this lnam fit")
  }
  ci <- stats::confint(fit)
  se <- sqrt(diag(stats::vcov(fit)))
  expect_lt(max(abs(ci[, 2] - (coef(fit) + stats::qnorm(0.975) * se))), 1e-12)
})
