# What a hierarchical Selection fit SAYS about itself: backend, coefficient
# regimes, intervals, the own-data weight, moderators, correlations and
# convergence.
#
# Several of these guard against specific misreports: print() and summary()
# naming amen::ame() and "one observed network per group" for a model that
# runs one chain over all groups; netis_estimates() describing the fit as
# independently fitted groups; the pooling print calling an unstructured
# request diagonal; and the own-data weight being reported under an
# unstructured covariance, where its formula does not describe the update.

test_that("print() and summary() name the sampler that actually ran", {
  skip_if_not_installed("amen")
  fit <- .hierarchical_fixture_fit()
  out <- paste(utils::capture.output(print(fit)), collapse = "\n")
  expect_match(out, "rnetis hierarchical sampler", fixed = TRUE)
  expect_false(grepl("one observed network per group", out, fixed = TRUE))
  expect_match(out, "Varying across groups (random), diagonal between-group covariance: snd_x",
               fixed = TRUE)
  expect_match(out, "Shared across groups: intercept, prior_tie", fixed = TRUE)

  s <- paste(utils::capture.output(print(summary(fit))), collapse = "\n")
  expect_match(s, "rnetis hierarchical sampler", fixed = TRUE)
  expect_match(s, "Convergence:", fixed = TRUE)
  expect_match(s, "rank-normalized, folded", fixed = TRUE)
  expect_match(.selection_pooling_sentence(fit), "one hierarchical model", fixed = TRUE)
})

test_that("the printed interval is the posterior quantile interval, not mean +/- 1.96 SD", {
  skip_if_not_installed("amen")
  fit <- .hierarchical_fixture_fit()
  tab <- .selection_estimate_table(fit)
  v <- fit$mgc_draws$mu[, "snd_x"]
  q <- stats::quantile(v, c(0.025, 0.975), names = FALSE)
  expect_identical(tab[["95% interval"]][tab$Term == "snd_x"],
                   sprintf("[%s, %s]", format(round(q[1], 4), trim = TRUE),
                           format(round(q[2], 4), trim = TRUE)))
})

test_that("netis_estimates() reports both levels of a hierarchical fit", {
  skip_if_not_installed("amen")
  fit <- .hierarchical_fixture_fit()
  est <- netis_estimates(fit)
  pop <- est[is.na(est$group), ]
  expect_setequal(pop$term, c("intercept", "snd_x", "prior_tie"))
  grp <- est[!is.na(est$group), ]
  # Group rows only for the coefficient that varies across groups.
  expect_identical(unique(grp$term), "snd_x")
  expect_setequal(grp$group, as.character(fit$groups))
  # Every interval is the draws' own quantile interval.
  g1 <- grp[grp$group == fit$groups[1], ]
  q <- stats::quantile(fit$mgc_draws$theta[, 1, "snd_x"], c(0.025, 0.975), names = FALSE)
  expect_equal(c(g1$conf.low, g1$conf.high), q)
  expect_identical(netis_glance(fit)$estimand, "hierarchical multi-group tie formation")
})

test_that("the own-data weight is kept only where its conditional meaning holds", {
  skip_if_not_installed("amen")
  diag_fit <- netis_hierarchy(.hierarchical_fixture_fit())
  expect_true(all(diag_fit$groups$own_data_weight > 0 & diag_fit$groups$own_data_weight < 1))
  expect_match(diag_fit$information$own_data_weight, "conditional share")

  # Moderators move the conditional MEAN of a group's deviation, not its
  # conditional precision, so the conditional share is still defined.
  mod <- netis_hierarchy(.moderated_fixture_fit())
  expect_true(all(is.finite(mod$groups$own_data_weight)))

  # Under an unstructured covariance the conditional prior precision is
  # (Sigma^-1)[k, k], so the scalar formula does not describe the update.
  corr <- netis_hierarchy(.correlated_fixture_fit())
  expect_true(all(is.na(corr$groups$own_data_weight)))
  expect_true(all(is.na(corr$groups$shrinkage)))
  expect_match(corr$information[[1]]$own_data_weight, "not reported under an unstructured")
})

test_that("an unstructured fit reports its correlations, acceptance and prior", {
  skip_if_not_installed("amen")
  h <- netis_hierarchy(.correlated_fixture_fit())
  expect_identical(h$covariance, "unstructured")
  expect_identical(nrow(h$correlations), 1L)
  expect_true(all(h$correlations$lower <= h$correlations$estimate &
                    h$correlations$estimate <= h$correlations$upper))
  expect_setequal(h$acceptance$block, c("log tau", "correlations"))
  expect_true(all(c("warmup", "retained", "target", "final_step") %in% names(h$acceptance)))
  expect_identical(h$information[[1]]$correlation_prior, "LKJ(eta = 1)")
  out <- utils::capture.output(print(selection_pooling(random = c("a", "b"),
                                                       covariance = "unstructured")))
  expect_true(any(grepl("between-group covariance: unstructured", out, fixed = TRUE)))
  expect_false(any(grepl("diagonal", out, fixed = TRUE)))
})

test_that("with moderators, the population row is labeled as the value at z = 0", {
  skip_if_not_installed("amen")
  h <- netis_hierarchy(.moderated_fixture_fit())
  expect_identical(h$population$parameter, c("mu (value at z = 0)", "tau"))
  expect_true(all(c("rhat", "ess_bulk", "ess_tail") %in% names(h$moderators)))
})

test_that("netis_hierarchy() carries convergence for what it reports", {
  skip_if_not_installed("amen")
  h <- netis_hierarchy(.hierarchical_fixture_fit())
  expect_true(all(c("rhat", "ess_bulk", "ess_tail") %in% names(h$population)))
  expect_true(all(c("rhat", "ess_bulk", "ess_tail", "tie_variation") %in% names(h$groups)))
  expect_true(all(h$groups$tie_variation))
  expect_s3_class(h$convergence, "data.frame")
  expect_error(netis_hierarchy(.hierarchical_fixture_fit(), level = 1.5), "strictly between 0 and 1")
})

test_that("the convergence plot reports bulk and tail ESS beside R-hat", {
  skip_if_not_installed("amen")
  d <- .plot_data_mcmc(.hierarchical_fixture_fit())
  expect_setequal(unique(d$statistic), c("R-hat", "Bulk ESS", "Tail ESS"))
})
