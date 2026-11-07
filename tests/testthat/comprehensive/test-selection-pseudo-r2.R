# SHORT CHAINS, deliberately.
#
# Every tolerance in this file is either an exact structural value (a row
# count, a class, a field name) or an INTERNAL CONSISTENCY identity -- two
# quantities derived from the same fit that must agree with each other, such
# as shares summing to 1. Neither kind moves with chain length: the chain only
# has to be long enough to return a well-formed object. A tolerance against an
# externally known truth would need longer chains and a recorded margin.
#
# pseudo_r2().
#
# The assertions that matter: each measure is the quantity its name claims,
# every required caveat actually prints, and the measures that are
# deliberately excluded stay excluded.

test_that("a Selection fit reports both decompositions, each named for what it is", {
  # THE NAMING IS THE POINT. McKelvey-Zavoina would be the wrong name: MZ
  # define their R2 for a latent index with no random effects at all.
  # Nakagawa-Schielzeth's marginal R2_GLMM(m) is the right TRADITION and still
  # not a name rnetis may claim -- it is not defined over AME's bilinear u_i'v_j
  # term, and adding "-style" would make the novelty explicit without removing
  # it.
  #
  # What ships instead is two measures named for what they COMPUTE, reported
  # as members of the standard pseudo-R2 family and documented as rnetis's own
  # construction. No author is credited with defining either for this model.
  skip_if_not_installed("amen")
  r2 <- pseudo_r2(.selection_fixture_fit())
  expect_s3_class(r2, "netis_pseudo_r2")

  # Neither name is claimed.
  expect_false(any(grepl("McKelvey", r2$measures$measure)))
  expect_false(any(grepl("Nakagawa", r2$measures$measure)))
  expect_false(any(grepl("-style", r2$measures$measure, fixed = TRUE)))

  # Both decompositions are present, plus Tjur's D.
  expect_true(any(grepl("variance share", r2$measures$measure)))
  expect_true(any(grepl("Null-comparison", r2$measures$measure)))
  expect_true(any(grepl("Tjur", r2$measures$measure)))

  # EACH ROW CARRIES ITS SCALE, because they are not on a common one: a
  # latent-scale variance share and a probability-scale discrimination
  # coefficient are not comparable numbers, and a single `scale` field on the
  # object would have to lie about one of them.
  expect_true("scale" %in% names(r2$measures))
  expect_setequal(unique(r2$measures$scale), c("latent", "probability"))
  expect_identical(
    r2$measures$scale[grepl("Tjur", r2$measures$measure)], "probability"
  )
})

test_that("the variance share is a proportion with a posterior interval", {
  skip_if_not_installed("amen")
  r2 <- pseudo_r2(.selection_fixture_fit(), include_null_comparison = FALSE)
  vs <- r2$measures[grepl("variance share", r2$measures$measure), ]
  expect_equal(nrow(vs), 1L)
  expect_true(vs$estimate > 0 && vs$estimate < 1)
  expect_true(vs$lo <= vs$estimate && vs$estimate <= vs$hi)
})

test_that("the null comparison needs a refit, and can be turned off", {
  # It differences the full model against a refitted null, so it costs a
  # second MCMC run -- which is why it is an argument rather than always on
  # without comment.
  skip_if_not_installed("amen")
  fit <- .selection_fixture_fit()
  with_null <- pseudo_r2(fit)
  without <- pseudo_r2(fit, include_null_comparison = FALSE)

  expect_true(any(grepl("Null-comparison", with_null$measures$measure)))
  expect_false(any(grepl("Null-comparison", without$measures$measure)))
  # Everything else is unchanged by the choice.
  expect_equal(nrow(without$measures), nrow(with_null$measures) - 1L)
})

test_that("a negative null comparison is reported, not clipped", {
  # It CAN be negative: the full model may not reduce the defined unexplained
  # variance relative to the null, and on two separate MCMC fits a small
  # negative value can also be Monte Carlo error. Clipping it to zero would
  # hide both, so the value stands and a note says what it means.
  skip_if_not_installed("amen")
  r2 <- pseudo_r2(.selection_fixture_fit())
  nc <- r2$measures$estimate[grepl("Null-comparison", r2$measures$measure)]
  expect_length(nc, 1L)
  expect_false(is.na(nc))
  if (nc < 0) {
    expect_true(any(grepl("deliberately not clipped", r2$notes)))
  }
})

test_that("the probit caveat prints with the null comparison", {
  # A binary probit fixes the residual variance at 1 for identification, so
  # predictors cannot reduce it and the reduction can appear only in the
  # additive and multiplicative components. The number understates explained
  # variation as a result, and is not comparable with a linear-model R2 --
  # which has to travel WITH it rather than live in the manual.
  skip_if_not_installed("amen")
  r2 <- pseudo_r2(.selection_fixture_fit())
  expect_true(any(grepl("fixed at 1 for identification", r2$notes)))
  expect_true(any(grepl("not comparable with a linear-model", r2$notes)))
})

test_that("Tjur's D matches its definition against the fit's own predictions", {
  skip_if_not_installed("amen")
  fit <- .selection_fixture_fit()
  r2 <- pseudo_r2(fit)

  Y <- fit$designs[[1]]$Y
  obs <- !is.na(Y)
  p <- fit$per_group[[1]]$posterior_predictive[obs]
  y <- Y[obs]
  expect_equal(
    r2$measures$estimate[grepl("Tjur", r2$measures$measure)],
    mean(p[y == 1]) - mean(p[y == 0])
  )
})

# Snijders-Bosker R2_1 is not offered: it is the proportional reduction in
# LEVEL-1 residual variance, and a binary probit fixes that variance at 1 for
# identification, so the reduction it is built around cannot occur. A measure
# outside the scope its authors defined does not ship under their name.

test_that("the deliberately excluded likelihood measures stay excluded", {
  skip_if_not_installed("amen")
  r2 <- pseudo_r2(.selection_fixture_fit())
  expect_false(any(grepl("McFadden|Nagelkerke|Cox|Efron",
                         r2$measures$measure)))
})

.pseudo_r2_multigroup <- function(n_groups = 10, seed = 1) {
  set.seed(seed)
  nodes <- do.call(rbind, lapply(seq_len(n_groups), function(g) {
    data.frame(
      group = g, node = rep(c("a", "b", "c", "d"), 2), wave = rep(1:2, each = 4),
      y = round(stats::rnorm(8, mean = g, sd = 1), 2)
    )
  }))
  ties <- do.call(rbind, lapply(seq_len(n_groups), function(g) {
    data.frame(group = g, source = c("a", "b", "c"), target = c("b", "c", "d"),
               w = c(1, 1, 1))
  }))
  netis_data(nodes, ties, wave_col = "wave")
}

test_that("Influence pseudo R2 reports the Nakagawa-Schielzeth decomposition", {
  skip_if_not_installed("lme4")
  fit <- fit_influence(y ~ ar() + exposure(y, via = w) + (1 | group),
                       data = .pseudo_r2_multigroup())
  r2 <- pseudo_r2(fit)

  est <- r2$measures$estimate
  expect_equal(nrow(r2$measures), 2L)
  # Conditional adds the random variance to the numerator, so it can never
  # be below marginal, and both are proportions.
  expect_gte(est[2], est[1])
  expect_true(all(est >= 0 & est <= 1))
})

test_that("an OLS-backed Influence fit reports its ordinary R-squared", {
  # CHANGED in the MCMC-diagnostics work. This previously asserted that pseudo_r2()
  # refuses and redirects to summary(). That was a test of the behavior rather
  # than of the contract, and the behavior was wrong: glance() reported this
  # same fit's r.squared while pseudo_r2() errored on it, so two surfaces
  # disagreed about whether the quantity existed. Asking how much variance a
  # fit explains is a reasonable question with a correct answer here, so it is
  # answered -- under its own name, since an OLS R-squared is a real one and
  # not a pseudo-measure.
  nodes <- data.frame(group = 1, node = rep(c("a", "b", "c", "d"), 2),
                      wave = rep(1:2, each = 4), y = c(1, 2, 3, 4, 2, 4, 5, 7))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  fit <- fit_influence(y ~ ar(), data = netis_data(nodes, ties, wave_col = "wave"))

  r <- pseudo_r2(fit)
  expect_s3_class(r, "netis_pseudo_r2")
  expect_match(r$measures$measure[1], "ordinary least squares")
  expect_equal(r$measures$estimate[1], netis_glance(fit)$r.squared)
  expect_match(r$notes, "not a pseudo-R-squared")
})

test_that("a random-slope fit is refused with the reason, not approximated", {
  skip_if_not_installed("lme4")
  fit <- fit_influence(y ~ ar() + exposure(y, via = w) + (1 + ex_y_w | group),
                       data = .pseudo_r2_multigroup(n_groups = 12, seed = 3))
  expect_error(pseudo_r2(fit), "Johnson")
})

test_that("objects from other packages are refused", {
  expect_error(pseudo_r2(lm(dist ~ speed, cars)), "netis_selection")
})
