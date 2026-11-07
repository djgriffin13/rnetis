# One hierarchical random selection coefficient: specification, refusals, the
# between-group layer, and the hierarchy report.

# --- specification ---------------------------------------------------------

test_that("selection_pooling() accepts one or several random coefficients", {
  p <- selection_pooling(random = "intercept")
  expect_s3_class(p, "netis_selection_pooling")
  expect_equal(p$random, "intercept")

  # Several are accepted, varying independently.
  several <- selection_pooling(random = c("a", "b"))
  expect_equal(several$random, c("a", "b"))
  expect_error(selection_pooling(random = character()), "one or more coefficients")
  expect_error(selection_pooling(random = "x", unpooled = "x"), "one regime")
  expect_error(selection_pooling(A0 = 0), "positive number")
  expect_error(selection_pooling(delta = -1), "delta must be positive")
})

test_that("the regime vector is shared except where pooling says otherwise", {
  nm <- c("intercept", "snd_x", "prior_tie")
  r <- .selection_regime_vector(nm, selection_pooling(random = "snd_x",
                                                      unpooled = "intercept"))
  expect_equal(unname(r[nm]), c("group", "random", "shared"))
  expect_error(
    .selection_regime_vector(nm, selection_pooling(random = "nope")),
    "does not have"
  )
})

test_that("several random coefficients are accepted, with a diagonal covariance", {
  # Several random coefficients are supported with INDEPENDENT between-group
  # SDs by default, so the contract to guard is what
  # `covariance` promises -- a correlated block is still a different model and
  # nothing here silently fits one.
  reg <- .mgc_regime(c("intercept", "a", "b"),
                     c(intercept = "random", a = "random", b = "shared"))
  expect_equal(reg$random, c(1L, 2L))
  expect_equal(reg$covariance, "diagonal")
  # One is fine, wherever it sits.
  expect_silent(.mgc_regime(c("intercept", "a"), c(intercept = "shared", a = "random")))
  expect_silent(.mgc_regime(c("intercept", "a"), c(intercept = "random", a = "shared")))
})

# --- the fit ---------------------------------------------------------------

test_that("a hierarchical fit estimates mu, tau and the group coefficients", {
  skip_if_not_installed("amen")
  fit <- .hierarchical_fixture_fit()
  h <- netis_hierarchy(fit)

  expect_equal(h$coefficient, "snd_x")
  expect_equal(h$population$parameter, c("mu", "tau"))
  expect_true(all(is.finite(h$population$estimate)))
  expect_true(h$population$estimate[2] > 0)              # tau is an SD
  expect_equal(nrow(h$groups), length(fit$groups))
  expect_true(all(h$groups$lower <= h$groups$beta_g))
  expect_true(all(h$groups$beta_g <= h$groups$upper))
  # Aligned draws of all three quantities are retained, not just summaries.
  expect_equal(dim(fit$hierarchical$draws$theta)[2], length(fit$groups))
  expect_equal(nrow(fit$hierarchical$draws$mu), nrow(fit$hierarchical$draws$tau))
  expect_false(anyNA(fit$hierarchical$draws$mu[, "snd_x"]))
  expect_false(anyNA(fit$hierarchical$draws$tau[, "snd_x"]))
  # A shared coefficient has a population value and no between-group SD.
  expect_false(anyNA(fit$hierarchical$draws$mu[, "prior_tie"]))
  expect_true(all(is.na(fit$hierarchical$draws$tau[, "prior_tie"])))
})

test_that("group coefficients are shrunk toward the population mean", {
  skip_if_not_installed("amen")
  fit <- .hierarchical_fixture_fit()
  h <- netis_hierarchy(fit)
  expect_true(all(h$groups$own_data_weight > 0 & h$groups$own_data_weight < 1))
  expect_equal(h$groups$shrinkage, 1 - h$groups$own_data_weight)
  # Every group's coefficient lies within the span the population distribution
  # and its own data allow; with real shrinkage none of them sits far outside
  # the others.
  expect_lt(max(abs(h$groups$beta_g - h$population$estimate[1])),
            6 * h$population$estimate[2])
})

test_that("the heterogeneity prior is scaled by a reference predictor change", {
  skip_if_not_installed("amen")
  fit <- .hierarchical_fixture_fit()
  h <- netis_hierarchy(fit)
  expect_match(h$information$reference_rule, "median within-group SD")
  expect_equal(h$information$reference_change,
               stats::median(h$groups$within_sd[h$groups$informative]))

  # A binary predictor keeps the 0 to 1 contrast rather than an SD.
  binary <- .hierarchical_fixture_fit(random = "prior_tie")
  hb <- netis_hierarchy(binary)
  expect_equal(hb$information$reference_change, 1)
  expect_match(hb$information$reference_rule, "0 to 1 contrast")
})

test_that("the joint route refuses what it cannot fit, by name", {
  skip_if_not_installed("amen")
  d1 <- .selection_fixture_data("bin", n_groups = 1, n_actors = 8, n_waves = 2)
  expect_error(
    fit_selection(tie ~ prior_tie(), data = d1, family = "bin",
                  pooling = selection_pooling(random = "intercept"),
                  nscan = 20, burn = 10, odens = 5),
    "at least two groups"
  )
  d <- .selection_fixture_data("bin", n_groups = 2, n_actors = 8, n_waves = 2)
  expect_error(
    fit_selection(tie ~ prior_tie(), data = d, family = "bin",
                  pooling = selection_pooling(random = "intercept"),
                  nscan = 20, burn = 10, odens = 5, gof = FALSE),
    "does not call amen::ame"
  )
  expect_error(
    fit_selection(tie ~ prior_tie(), data = d, family = "bin", pooling = "random",
                  nscan = 20, burn = 10, odens = 5),
    "selection_pooling"
  )
})

# --- weak-information groups ------------------------------------------------

test_that("a group with no within-group variation is flagged, not refused", {
  skip_if_not_installed("amen")
  fit <- .hierarchical_fixture_fit(flat_group = "3")
  h <- netis_hierarchy(fit)

  expect_equal(nrow(h$groups), 3L)
  expect_false(h$groups$informative[h$groups$group == "3"])
  expect_true(all(h$groups$informative[h$groups$group != "3"]))
  expect_equal(h$information$n_informative, 2L)
  # Its coefficient comes from the population distribution: no own-data weight.
  expect_lt(h$groups$own_data_weight[h$groups$group == "3"],
            min(h$groups$own_data_weight[h$groups$group != "3"]))
  expect_lt(h$groups$own_data_weight[h$groups$group == "3"], 0.05)
  # And the reference change ignores it rather than counting a zero.
  expect_equal(h$information$reference_change,
               stats::median(h$groups$within_sd[h$groups$informative]))
})

test_that("a coefficient with no information anywhere is refused", {
  info_none <- list(informative = c(FALSE, FALSE, FALSE))
  expect_error(.assert_hierarchical_information(info_none, "snd_x", c("1", "2", "3")),
               "not identified anywhere")
  info_one <- list(informative = c(TRUE, FALSE, FALSE))
  expect_error(.assert_hierarchical_information(info_one, "snd_x", c("1", "2", "3")),
               "cannot be estimated from one informative group")
  expect_true(.assert_hierarchical_information(list(informative = c(TRUE, TRUE, FALSE)),
                                               "snd_x", c("1", "2", "3")))
})

test_that("netis_hierarchy() refuses a fit that has no hierarchical layer", {
  skip_if_not_installed("amen")
  expect_error(netis_hierarchy(.selection_fixture_fit("bin", n_actors = 6)),
               "no hierarchical layer")
})

# --- the tau prior's scale ------------------------------------------------------
#
# delta sets A, and A is the prior that governs tau's behavior, so these are
# deterministic checks on quantities rather than on fitted output.

.info_design <- function(x, n, miss = 0, seed = 1) {
  set.seed(seed)
  Y <- matrix(stats::rbinom(n * n, 1, 0.3), n)
  diag(Y) <- NA
  if (miss > 0) Y[sample(which(!is.na(Y)), miss)] <- NA
  list(Y = Y, Xrow = matrix(x, n, 1, dimnames = list(NULL, "snd_x")), Xcol = NULL,
       Xdyad = array(stats::rnorm(n * n), c(n, n, 1),
                     dimnames = list(NULL, NULL, "dyd")))
}

test_that("a nodal column's reference change is the ACTOR-level SD, not the dyad-level one", {
  # Reading a nodal covariate off the flattened design slice counts each
  # actor's value once per observed dyad in its row: the population SD at
  # complete data, and out-degree-weighted under missingness. Neither is the
  # actor-level SD the spec names.
  x <- c(-2, -1, 0, 1, 2, 3, 4, 5)                    # sd = 2.449
  d <- list(.info_design(x, 8), .info_design(x, 8, seed = 2))
  info <- .selection_predictor_information(d, c("intercept", "snd_x", "dyd"), "snd_x")

  expect_equal(info$role, "sender")
  expect_equal(info$predictor_sd[1], stats::sd(x), tolerance = 1e-12)
  # ASYMMETRIC MISSINGNESS: actor 1 loses most of its row. The actor-level SD
  # cannot move, because an actor contributes one value however many dyads it
  # has; a dyad-level SD would shift.
  holed <- d
  holed[[1]]$Y[1, 2:7] <- NA
  info2 <- .selection_predictor_information(holed, c("intercept", "snd_x", "dyd"), "snd_x")
  expect_equal(info2$predictor_sd[1], stats::sd(x), tolerance = 1e-12)
})

test_that("the informative threshold is relative, not an exact-zero test in disguise", {
  # Three groups at SD ~1 and three at SD ~0.001. Information about a slope
  # scales with SD^2, so the second three carry about 1e-6 of the first three's
  # and are not usable -- while `sds > 1e-8 * max(ref, 1)` called all six
  # informative.
  set.seed(3)
  big <- lapply(1:3, function(i) .info_design(stats::rnorm(12), 12, seed = i))
  tiny <- lapply(4:6, function(i) .info_design(stats::rnorm(12) * 0.001, 12, seed = i))
  info <- .selection_predictor_information(c(big, tiny),
                                           c("intercept", "snd_x", "dyd"), "snd_x")
  expect_equal(sum(info$informative), 3L)
  expect_true(all(info$informative[1:3]))
  expect_false(any(info$informative[4:6]))
  # And delta is the median over the INFORMATIVE groups, not over all nonzero ones.
  expect_equal(info$delta, stats::median(info$predictor_sd[1:3]), tolerance = 1e-12)
})

test_that("the pooled SD weights groups by support size", {
  # Spec quantity (c). One small group with a wide spread and one large group
  # with a narrow one: weighting by support pulls the pooled value toward the
  # large group, while an unweighted root-mean-square does not.
  set.seed(5)
  d <- list(.info_design(stats::rnorm(5) * 3, 5), .info_design(stats::rnorm(40), 40))
  info <- .selection_predictor_information(d, c("intercept", "snd_x", "dyd"), "snd_x")
  unweighted <- sqrt(mean(info$predictor_sd^2))

  expect_lt(info$pooled_sd, unweighted)              # the big group dominates
  expect_lt(abs(info$pooled_sd - info$predictor_sd[2]),
            abs(info$pooled_sd - info$predictor_sd[1]))
  # The two quantities are reported side by side so the difference is visible.
  expect_equal(info$support_n, c(5, 40))
})

test_that("the hierarchy report carries the between-group spread diagnostics", {
  skip_if_not_installed("amen")
  h <- netis_hierarchy(.hierarchical_fixture_fit())
  for (field in c("sd_max_min_ratio", "sd_iqr_ratio", "pooled_sd",
                  "median_to_pooled_ratio", "informative_threshold", "support_n")) {
    expect_false(is.null(h$information[[field]]), info = field)
  }
  expect_gte(h$information$sd_max_min_ratio, 1)
  # The mean stays the primary summary; the median rides alongside it.
  expect_true("estimate" %in% names(h$population))
  expect_true("median" %in% names(h$population))
})

test_that("A0 defaults to the weak-regularization setting, with its grid documented", {
  # A0 = 0.5 is a choice about how much regularization a between-group SD
  # carries on the standardized contrast scale -- not a value tuned to any
  # study's truth. The two sensitivity settings sit either side of it.
  expect_equal(selection_pooling(random = "intercept")$A0, 0.5)
  expect_equal(selection_pooling(random = "intercept", A0 = 0.25)$A0, 0.25)
  expect_equal(selection_pooling(random = "intercept", A0 = 1)$A0, 1)

  # A0 scales the prior linearly through A = A0 * outcome_scale / delta, so the
  # grid is a factor-of-two ladder in prior scale and nothing more subtle.
  pri <- function(a0, delta = 1) .mgc_tau_prior_scale(1, predictor_sd = delta, A0 = a0)
  expect_equal(pri(0.5) / pri(0.25), 2, tolerance = 1e-12)
  expect_equal(pri(1) / pri(0.5), 2, tolerance = 1e-12)
  # And delta divides it, which is what puts tau on the contrast scale: a
  # predictor with twice the within-group spread halves the prior scale.
  expect_equal(pri(0.5, delta = 2), pri(0.5) / 2, tolerance = 1e-12)
})

# --- several INDEPENDENT random coefficients --------------------------------

test_that("several random coefficients each get their own between-group SD", {
  skip_if_not_installed("amen")
  fit <- .multi_random_fixture_fit()
  h <- netis_hierarchy(fit)

  expect_equal(h$coefficient, c("snd_x", "prior_tie"))
  expect_equal(h$covariance, "diagonal")
  # Two labeled sets of rows rather than one merged summary.
  expect_equal(sort(unique(h$population$coefficient)), sort(c("snd_x", "prior_tie")))
  expect_equal(nrow(h$population), 4L)                    # mu and tau, twice
  expect_equal(nrow(h$groups), 2L * length(fit$groups))

  # Each has its own tau, drawn from its own conditional, and both are real SDs.
  taus <- h$population$estimate[h$population$parameter == "tau"]
  expect_true(all(taus > 0))
  expect_false(isTRUE(all.equal(taus[1], taus[2])))
  # ... and its own reference change: a continuous predictor's is a within-group
  # SD, a binary one's is the 0 to 1 contrast.
  expect_equal(h$information$prior_tie$reference_change, 1)
  expect_false(isTRUE(all.equal(h$information$snd_x$reference_change, 1)))
})

test_that("the draws carry one mu and one tau per random coefficient", {
  skip_if_not_installed("amen")
  fit <- .multi_random_fixture_fit()
  d <- fit$hierarchical$draws
  for (k in c("snd_x", "prior_tie")) {
    expect_false(anyNA(d$mu[, k]), info = k)
    expect_false(anyNA(d$tau[, k]), info = k)
  }
  # A shared coefficient still has a population value and no between-group SD,
  # so the random/shared distinction survives having several random columns.
  expect_true(all(is.na(d$tau[, "intercept"])))
  expect_false(anyNA(d$mu[, "intercept"]))
})

test_that("the hyperparameter update is independent per coefficient", {
  # Two columns with very different spreads: each tau must find its own, which a
  # single shared conditional could not do.
  set.seed(4)
  zeta <- cbind(stats::rnorm(10, 0, 0.15), stats::rnorm(10, 0, 2))
  st <- list(tau2 = c(1, 1), lambda = c(1, 1))
  for (i in 1:400) {
    st <- .mgc_update_random(zeta, st, list(nu = 3, A = c(0.5, 0.5)))
  }
  tau <- sqrt(st$tau2)
  expect_lt(tau[1], 0.6)
  expect_gt(tau[2], 1.0)
  expect_gt(tau[2] / tau[1], 3)
})

test_that("the specification refuses contradictory or duplicated regimes", {
  expect_error(selection_pooling(random = c("a", "a")), "more than once")
  expect_error(selection_pooling(random = c("a", "b"), unpooled = "b"), "one regime")
  expect_error(selection_pooling(random = c("a", "b"), delta = c(1, 2, 3)),
               "one per random coefficient")
  # One delta for all, or one each, are both legitimate.
  expect_s3_class(selection_pooling(random = c("a", "b"), delta = 1),
                  "netis_selection_pooling")
  expect_s3_class(selection_pooling(random = c("a", "b"), delta = c(1, 2)),
                  "netis_selection_pooling")
})

test_that("each random coefficient's reference change reaches the prior in design order", {
  # `random` is written in the caller's order; the sampler indexes its random
  # columns in DESIGN-COLUMN order. Handing it deltas in the caller's order
  # would attach one coefficient's scale to another's prior -- correct-looking
  # and wrong -- so the order is asserted rather than assumed.
  skip_if_not_installed("amen")
  fit <- .multi_random_fixture_fit(random = c("prior_tie", "snd_x"))  # reversed
  h <- netis_hierarchy(fit)
  # prior_tie is binary (delta 1); snd_x is continuous (delta != 1). Whichever
  # order the caller wrote them in, each keeps its own.
  expect_equal(h$information$prior_tie$reference_change, 1)
  expect_false(isTRUE(all.equal(h$information$snd_x$reference_change, 1)))
})

# --- group-level moderators -------------------------------------------------

test_that("the moderator table reports gamma with the scale it was priored on", {
  skip_if_not_installed("amen")
  fit <- .moderated_fixture_fit(binary = FALSE)
  h <- netis_hierarchy(fit)

  expect_identical(h$moderators$moderator, c("(Intercept)", "size"))
  # THE SCALE TRAVELS WITH THE ESTIMATE. gamma is interpretable only against
  # the reference change its prior was expressed per, and a continuous
  # moderator's is computed from the G group values -- so the flag saying so is
  # reported, not left for a reader to infer from the moderator's type.
  expect_true(h$moderators$scale_from_group_values[2])
  expect_match(h$moderators$reference_rule[2], "SD across the 4 group values")
  # The intercept row is not a moderator slope and has neither.
  expect_true(is.na(h$moderators$reference_change[1]))
  expect_true(is.na(h$moderators$scale_from_group_values[1]))

  # mu IS gamma's intercept: the coefficient at z = 0. Documented in the map
  # rather than silently reinterpreted, LABELLED as such in the population
  # table, and asserted here so the two reporting paths cannot
  # drift apart.
  expect_equal(h$population$estimate[h$population$parameter == "mu (value at z = 0)"],
               h$moderators$estimate[1])
})

test_that("a binary moderator takes the 0 to 1 contrast, not its own SD", {
  skip_if_not_installed("amen")
  fit <- .moderated_fixture_fit(binary = TRUE)
  h <- netis_hierarchy(fit)
  # EXACTLY THE RULE USED FOR BINARY PREDICTORS. A moderator true in two groups
  # of four has an SD near 0.58, and dividing by it would widen the prior by
  # three quarters for no reason except how common the moderator is. The
  # contrast is 1 by construction, so the scale is fixed rather than estimated
  # and nothing is flagged.
  expect_equal(h$moderators$reference_change[2], 1)
  expect_false(h$moderators$scale_from_group_values[2])
  expect_match(h$moderators$reference_rule[2], "0 to 1 contrast")
})

test_that("a fit without moderators reports none, rather than an empty table", {
  skip_if_not_installed("amen")
  h <- netis_hierarchy(.hierarchical_fixture_fit())
  # ABSENT, NOT EMPTY. A zero-row table reads like "the moderators were
  # estimated and came to nothing"; NULL says there were none.
  expect_null(h$moderators)
})

test_that("the between-group covariance is a named choice with a default", {
  # Both covariance models are implemented, so the contract is that the choice
  # is NAMED and defaults to the independent model -- a caller
  # who says nothing gets a diagonal and a fit that says so, rather than a
  # correlated model they did not ask for.
  p <- selection_pooling(random = "snd_x")
  expect_identical(p$covariance, "diagonal")
  q <- selection_pooling(random = c("snd_x", "prior_tie"), covariance = "unstructured")
  expect_identical(q$covariance, "unstructured")
  # A value naming neither model is still refused rather than coerced.
  expect_error(selection_pooling(random = "snd_x", covariance = "banded"),
               "should be one of")
})

test_that("a correlated fit does not describe itself as diagonal", {
  # THE BUG THIS PINS WAS REAL AND SILENT. netis_hierarchy() reported
  # covariance = "diagonal" as a literal in both of its branches, so an
  # unstructured fit -- correlations estimated, Omega retained -- described
  # itself as the independent model. Worse than saying nothing: a reader would
  # conclude the correlations had not been estimated and go looking for a
  # feature that was already there.
  skip_if_not_installed("amen")
  fit <- .correlated_fixture_fit()
  h <- netis_hierarchy(fit)

  expect_identical(fit$hierarchical$covariance, "unstructured")
  expect_identical(h$covariance, "unstructured")
  expect_equal(dim(fit$hierarchical$Omega)[2:3], c(2L, 2L))

  # A diagonal fit still says diagonal and carries no Omega -- absent, not an
  # identity matrix that would read as "estimated and came out zero".
  d <- netis_hierarchy(.hierarchical_fixture_fit())
  expect_identical(d$covariance, "diagonal")
  expect_null(d$Omega)
})

test_that("an unstructured covariance is refused at the public surface too", {
  # Refused where the user types it, not only deep in the core: a caller who
  # asks for correlated random coefficients with only one of them would
  # otherwise get a fit that runs and is the diagonal model.
  expect_error(selection_pooling(random = "snd_x", covariance = "unstructured"),
               "at least two random coefficients")
  expect_silent(selection_pooling(random = c("snd_x", "prior_tie"),
                                  covariance = "unstructured"))
})
