# Every documented panel of plot.netis_influence() draws.

.pip_fit <- function() {
  set.seed(5)
  n <- 12
  ids <- sprintf("n%02d", 1:n)
  nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = n),
                      y = stats::rnorm(2 * n), x = stats::rnorm(2 * n))
  ties <- data.frame(group = 1, source = ids, target = ids[c(2:n, 1)],
                     wave = 1, tie = 1)
  fit_influence(y ~ ar() + exposure(x, via = tie),
                data = netis_data(nodes, ties, wave_col = "wave"))
}

test_that("the effects panel draws, with influence_effect()'s numbers", {
  fit <- .pip_fit()
  d <- plot_data(fit, what = "effects")
  eff <- influence_effect(fit)$exposures[["ex_x_tie"]]
  # Hand check: predicted change is beta * (q90 - q10) over the term.
  beta <- unname(fit$coefficients["ex_x_tie"])
  q <- stats::quantile(fit$model_frame$ex_x_tie, c(0.1, 0.9), names = FALSE)
  expect_lt(abs(d$value[d$term == "ex_x_tie"] - beta * (q[2] - q[1])), 1e-12)
  expect_lt(abs(d$value[d$term == "ex_x_tie"] - eff$predicted_change), 1e-12)
  expect_lt(abs(d$contrast_low - q[1]), 1e-12)
  p <- plot(fit, what = "effects")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("the heterogeneity panel refuses on a fit without random effects", {
  fit <- .pip_fit()
  expect_error(plot(fit, what = "heterogeneity"), "no variance components")
})

test_that("dependence and groups are not Influence panels and say where to look", {
  fit <- .pip_fit()
  expect_error(plot(fit, what = "dependence"), "netis_dependence(fit, data)", fixed = TRUE)
  expect_error(plot_data(fit, what = "dependence"), "netis_dependence(fit, data)", fixed = TRUE)
  expect_error(plot(fit, what = "groups"), "one estimate per term", fixed = TRUE)
  expect_false(any(c("dependence", "groups") %in% .NETIS_INFLUENCE_PANELS))
})

test_that("the dependence table states serial correlation and reads residual eta-squared", {
  dep <- structure(list(
    serial = list(status = "not_applicable", reason = "one outcome row per unit"),
    network_autocorrelation = list(status = "not_applicable", reason = "none"),
    residual_eta_squared = list(status = "computed", eta_squared = 0.31, n_groups = 9L,
                                n_rows = 81L, expected_no_group_effect = 0.1)
  ), class = "netis_dependence")
  d <- plot_data(dep)
  expect_true(is.na(d$value[1]))
  expect_identical(d$status[1], "not_applicable")
  expect_identical(d$reason[1], "one outcome row per unit")
  expect_identical(d$term[2], "residual eta-squared on group")
  expect_equal(d$value[2], 0.31)
  expect_match(d$detail[2], "expectation with no group effect 0.100", fixed = TRUE)
  expect_identical(as.character(d$missing_state[2]), "observed")
})
