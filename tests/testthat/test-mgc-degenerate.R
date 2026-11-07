# Degenerate groups in the hierarchical sampler, and rnetis's admission rule for
# binary groups without tie variation (see ?hierarchical_selection).
#
# Three different kinds of degenerate group, kept apart:
#   * all observed ties 0 (or all 1)        -> admitted under a random intercept
#                                             with nothing unpooled, else refused
#   * no observed dyads at all              -> always refused
#   * a predictor without within-group variation -> flagged, not refused (tested
#                                             in test-selection-hierarchical.R)
#
# The admission and directional tests below fail if every binary group without
# tie variation is refused.

# Controlled groups: G groups of n actors, a sender covariate, intercept
# `alpha[g]`, slope 0.5. `flat` names groups whose observed ties are then all
# set to `flat_value`, with one dyad left missing so missingness is exercised.
.degenerate_groups <- function(G = 6, n = 8, seed = 7, alpha = rep(-0.4, G),
                               flat = NULL, flat_value = 0) {
  set.seed(seed)
  out <- lapply(seq_len(G), function(g) {
    x <- stats::rnorm(n)
    eta <- alpha[g] + 0.5 * matrix(x, n, n) +
      outer(stats::rnorm(n, 0, 0.3), stats::rnorm(n, 0, 0.3), "+")
    Y <- 1 * (matrix(stats::rnorm(n * n), n) < eta)
    diag(Y) <- NA
    list(Y = Y, Xrow = matrix(x, n, 1, dimnames = list(NULL, "x")))
  })
  names(out) <- letters[seq_len(G)]
  for (f in flat) {
    Y <- out[[f]]$Y
    Y[!is.na(Y)] <- flat_value
    Y[1, 2] <- NA
    out[[f]]$Y <- Y
  }
  out
}
.deg_names <- c("intercept", "snd_x")

# The posterior mean of a deviation, as a z-score against zero with its Monte
# Carlo SE from the draws' effective sample size. The DIRECTION of a tie-less
# group's intercept shift is definite (its full conditional is the
# hierarchical prior tilted by a likelihood monotone in the intercept); its
# SIZE is prior-sensitive, because the group's actor effects share the tie
# rate. So the assertion is on the sign of the mean, beyond Monte Carlo
# error, not on a posterior probability chosen in advance.
.dev_z <- function(dev) {
  ess <- .mcmc_ess_basic(matrix(dev, ncol = 1))
  mean(dev) / (stats::sd(dev) / sqrt(ess))
}

# --- preparation: what is recorded, what is refused -------------------------

test_that("groups without tie variation are recorded, and missing dyads stay missing", {
  skip_if_not_installed("amen")
  prep <- .mgc_prepare_groups(.degenerate_groups(flat = "b"), "bin", 0, .deg_names)
  expect_identical(prep$b$tie_variation, "all0")
  expect_true(is.na(prep$a$tie_variation))
  # The left-missing dyad is NOT an observed zero.
  expect_true(is.na(prep$b$Y[1, 2]))
  expect_false(prep$b$mask[1, 2])
  expect_identical(prep$b$n_observed, 8L * 7L - 1L)

  prep1 <- .mgc_prepare_groups(.degenerate_groups(flat = "c", flat_value = 1),
                               "bin", 0, .deg_names)
  expect_identical(prep1$c$tie_variation, "all1")
})

test_that("a group with no observed dyads is refused under every family", {
  skip_if_not_installed("amen")
  g <- .degenerate_groups(G = 3)
  g$b$Y[] <- NA
  expect_error(.mgc_prepare_groups(g, "bin", 0, .deg_names),
               "Group 'b' has no observed dyads")
  expect_error(.mgc_prepare_groups(g, "nrm", 0, .deg_names),
               "Group 'b' has no observed dyads")
})

test_that("a normal-model group with one tie value everywhere is refused", {
  skip_if_not_installed("amen")
  g <- .degenerate_groups(G = 3)
  g$a$Y[!is.na(g$a$Y)] <- 2
  expect_error(.mgc_prepare_groups(g, "nrm", 0, .deg_names),
               "Group 'a' has the same tie value")
})

# --- the admission rule ------------------------------------------------------

test_that("the rule admits such a group only under a random intercept with nothing unpooled", {
  skip_if_not_installed("amen")
  prep <- .mgc_prepare_groups(.degenerate_groups(flat = "b"), "bin", 0, .deg_names)
  ok <- .mgc_regime(.deg_names, c(intercept = "random", snd_x = "shared"))
  expect_silent(.mgc_assert_degenerate_admissible(prep, ok))
  ok2 <- .mgc_regime(.deg_names, c(intercept = "random", snd_x = "random"))
  expect_silent(.mgc_assert_degenerate_admissible(prep, ok2))

  shared_int <- .mgc_regime(.deg_names, c(intercept = "shared", snd_x = "random"))
  expect_error(.mgc_assert_degenerate_admissible(prep, shared_int),
               "'b' \\(all 0\\).*intercept is not random")
  unpooled <- .mgc_regime(.deg_names, c(intercept = "random", snd_x = "group"))
  expect_error(.mgc_assert_degenerate_admissible(prep, unpooled),
               "snd_x is group-specific")
})

# --- the start and the prior --------------------------------------------------

test_that("starting values are finite and on the truncation's side of zero for every m", {
  skip_if_not_installed("amen")
  # m = 1 is the case the first-proposed formula got wrong (it gave exactly 0).
  for (m in c(1L, 2L, 12L)) {
    for (val in c(0, 1)) {
      n <- 4L
      Y <- matrix(NA_real_, n, n)
      off <- which(row(Y) != col(Y))
      Y[off[seq_len(m)]] <- val
      g <- list(a = list(Y = Y, Xrow = matrix(c(0.1, -0.4, 0.3, 0.9), n, 1,
                                               dimnames = list(NULL, "x"))))
      prep <- .mgc_prepare_groups(g, "bin", 0, .deg_names)
      st <- .mgc_init_group(prep$a, "bin", 0)
      expect_true(all(is.finite(st$Z)), info = sprintf("m = %d, value %d", m, val))
      zo <- st$Z[prep$a$mask]
      if (val == 0) expect_true(all(zo < 0)) else expect_true(all(zo > 0))
      # Strictly inside (-1, 1): at +/-1 the dyadic covariance is singular.
      expect_true(is.finite(st$rho) && abs(st$rho) < 1)
    }
  }
})

test_that("such a group uses amen's fixed nuisance defaults, and says so", {
  skip_if_not_installed("amen")
  prep <- .mgc_prepare_groups(.degenerate_groups(flat = "b"), "bin", 0, .deg_names)
  np <- prep$b$nuisance_prior
  expect_identical(np$source, "amen fixed defaults (no tie variation)")
  # Absent entries are what make amen's own function defaults apply.
  expect_null(np$Sab0)
  expect_null(np$eta0)
  expect_identical(np$g, prep$b$n_observed)
  expect_identical(prep$a$nuisance_prior$source, "amen data-derived")
})

test_that("a fully observed all-0 group with only a sender covariate samples", {
  skip_if_not_installed("amen")
  # The case the first version missed: no missing dyad and a row-only design
  # leave the start's residual with no information, its two triangles
  # perfectly correlated, and amen's rho update then failed on a singular
  # dyadic covariance.
  g <- .degenerate_groups(G = 4)
  g$b$Y[!is.na(g$b$Y)] <- 0
  d <- .mgc_sample(g, regime = c(intercept = "random", snd_x = "shared"),
                   nscan = 20, burn = 5, odens = 5, coef_names = .deg_names)
  expect_true(all(is.finite(d$theta)))
  expect_true(all(abs(d$ame$rho) < 1))
})

# --- what the estimates do -------------------------------------------------------

# tau held fixed so the direction is a property of the group's data and the
# population prior, not of how well tau is estimated from a few groups. No fit
# holds tau fixed, so these tests replace the between-group update.
.hold_tau <- function(tau) {
  function(zeta_k, state, prior) {
    state$tau2 <- rep(tau^2, ncol(as.matrix(zeta_k)))
    state
  }
}

test_that("an all-0 group's intercept sits below its hierarchical mean", {
  skip_if_not_installed("amen")
  testthat::local_mocked_bindings(.mgc_update_random = .hold_tau(0.5))
  d <- .mgc_sample(.degenerate_groups(flat = "b"),
                   regime = c(intercept = "random", snd_x = "shared"),
                   nscan = 300, burn = 100, odens = 2, seed = 3,
                   coef_names = .deg_names)
  dev <- d$theta[, "b", "intercept"] - d$mu[, "intercept"]
  expect_lt(.dev_z(dev), -3)
  expect_identical(unname(d$tie_variation["b"]), "all0")
  expect_true(all(is.finite(d$theta)))
})

test_that("an all-1 group's intercept sits above its hierarchical mean", {
  skip_if_not_installed("amen")
  testthat::local_mocked_bindings(.mgc_update_random = .hold_tau(0.5))
  d <- .mgc_sample(.degenerate_groups(flat = "b", flat_value = 1),
                   regime = c(intercept = "random", snd_x = "shared"),
                   nscan = 300, burn = 100, odens = 2, seed = 3,
                   coef_names = .deg_names)
  dev <- d$theta[, "b", "intercept"] - d$mu[, "intercept"]
  expect_gt(.dev_z(dev), 3)
})

test_that("under moderators the comparison is with the group's OWN hierarchical mean", {
  skip_if_not_installed("amen")
  testthat::local_mocked_bindings(.mgc_update_random = .hold_tau(0.5))
  # z puts the flat group at z = 1, so its hierarchical mean is
  # gamma0 + gamma1, not gamma0 ("mu" at z = 0). Comparing with gamma0 would
  # test the wrong quantity whenever gamma1 is not zero.
  z <- seq(-1, 1, length.out = 6)
  grp <- .degenerate_groups(flat = "f", alpha = -0.4 + 0.6 * z)
  Z <- cbind("(Intercept)" = 1, z = z)
  rownames(Z) <- names(grp)
  d <- .mgc_sample(grp, regime = c(intercept = "random", snd_x = "shared"),
                   nscan = 300, burn = 100, odens = 2, seed = 5,
                   hyper = list(delta_Z = c(z = stats::sd(z))),
                   coef_names = .deg_names, moderators = Z)
  m_f <- d$gamma[, "gamma:intercept:(Intercept)"] + d$gamma[, "gamma:intercept:z"] * z[6]
  dev <- d$theta[, "f", "intercept"] - m_f
  expect_lt(.dev_z(dev), -3)
})

# --- end to end -----------------------------------------------------------------

test_that("fit_selection() admits an all-0 group under a random intercept and reports it", {
  skip_if_not_installed("amen")
  d <- .selection_fixture_data("bin", n_groups = 4, n_actors = 8, n_waves = 2)
  ties <- netis_ties(d)
  ties$tie[ties$group == "2" & ties$wave == 2] <- 0
  d <- netis_data(netis_nodes(d), ties, wave_col = "wave",
                  tie_meta = list(tie = list(zero_meaning = "measured_zero")))
  fit <- fit_selection(tie ~ prior_tie() + sender(x), data = d, family = "bin",
                       pooling = selection_pooling(random = "intercept"),
                       nscan = 150, burn = 50, odens = 5)
  h <- netis_hierarchy(fit)
  expect_identical(h$groups$tie_variation, h$groups$group != "2")
  expect_identical(h$information$groups_without_tie_variation, 1L)
  expect_output(print(fit), "1 group\\(s\\) admitted with no variation")

  expect_error(
    fit_selection(tie ~ prior_tie() + sender(x), data = d, family = "bin",
                  pooling = selection_pooling(random = "snd_x"),
                  nscan = 20, burn = 5, odens = 5),
    "'2' \\(all 0\\)"
  )
})

test_that("a group with n or fewer observed dyads still informs a random intercept", {
  # The informative count once subtracted the diagonal from a Y whose diagonal
  # was already NA, so it undercounted by n.
  n <- 4L
  Y <- matrix(NA_real_, n, n)
  off <- which(row(Y) != col(Y))
  Y[off[1:4]] <- c(0, 1, 0, 1)
  full <- matrix(stats::rbinom(n * n, 1, 0.4), n, n)
  diag(full) <- NA
  info <- .selection_predictor_information(list(a = list(Y = Y), b = list(Y = full)),
                                           c("intercept"), "intercept")
  expect_identical(info$informative, c(a = TRUE, b = TRUE))
})

test_that("a group with an all-zero random predictor does not break an unpooled coefficient", {
  # The documented behavior: a group with no variation in a random predictor is
  # flagged and partially pooled. That group's g-prior is singular along the
  # flat column, which is not the column the unpooled coefficient's marginal
  # prior is taken on.
  skip_if_not_installed("amen")
  P <- diag(c(2, 0, 4))
  expect_equal(.mgc_marginal_precision(P, c(1L, 3L)), diag(c(2, 4)))

  set.seed(11)
  gs <- lapply(c(a = 1, b = 2, c = 3), function(k) {
    n <- 7L
    Y <- matrix(stats::rbinom(n^2, 1, 0.35), n, n)
    diag(Y) <- NA
    list(Y = Y, Xrow = cbind(x = if (k == 1) rep(0, n) else stats::rnorm(n),
                             w = stats::rnorm(n)))
  })
  d <- .mgc_sample(gs, regime = c(intercept = "shared", x = "random", w = "group"),
                   nscan = 20, burn = 10, odens = 5, seed = 2,
                   hyper = list(predictor_sd = 1),
                   coef_names = c("intercept", "x", "w"))
  expect_true(all(is.finite(d$theta)))
})
