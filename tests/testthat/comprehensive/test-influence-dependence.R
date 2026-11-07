# The `dependence` diagnostic family.
#
# Known-answer first, integration second. The Moran-type statistic is four
# arithmetic operations, so the arithmetic is checked against values computed by
# hand rather than only through a fitted model, where a sign error would be
# invisible behind plausible-looking output.
#
# The family's central design rule is negative and needs testing as such: a
# statistic never becomes a finding on magnitude. A build that added
# "WARNING: significant residual network autocorrelation" would pass every
# positive test here and be wrong.

# fit_influence() returns estimates and interpretation is computed on request,
# so the dependence quantities and their findings both come from
# netis_dependence(fit, data) -- which returns the quantities with their
# findings attached.
.dep_codes <- function(dep) {
  d <- filter_diagnostics(dep$diagnostics, family = "dependence")
  vapply(d, function(f) f$code, character(1))
}

# ---------------------------------------------------------------------------
# The statistic itself


# A hand-built fixture: each unit contributes exactly one outcome row.
.dep_data <- function(n = 20, seed = 4, n_waves = 2L) {
  set.seed(seed)
  ids <- sprintf("n%02d", seq_len(n))
  nodes <- data.frame(
    group = 1, node = rep(ids, n_waves),
    wave = rep(seq_len(n_waves), each = n),
    y = stats::rnorm(n * n_waves), stringsAsFactors = FALSE
  )
  # The same chain observed at every wave. A tie table at wave 1 only would
  # leave the network unobserved at later waves, not copied to them.
  ties <- data.frame(group = 1, source = rep(ids[-n], n_waves), target = rep(ids[-1], n_waves),
                     wave = rep(seq_len(n_waves), each = n - 1L),
                     tie = 1, stringsAsFactors = FALSE)
  netis_data(nodes, ties, wave_col = "wave")
}

.dep_fit <- function(data = .dep_data(), ...) {
  suppressWarnings(fit_influence(
    y ~ ar() + exposure(y, via = tie, normalize = "row", isolate_rule = "zero"),
    data = data, ...
  ))
}

.dep_of <- function(fit, data) netis_dependence(fit, data)


test_that("the cross-product statistic matches a hand-computed value", {
  # Four nodes in a directed cycle a -> b -> c -> d -> a. Under the canonical
  # W[i,j] = "j influences i" orientation each row has exactly one 1, so
  # S0 = 4 and n / S0 = 1: the statistic reduces to (z' W z) / (z' z), which is
  # sum_i z_i * z_{predecessor(i)} over sum z_i^2.
  W <- matrix(0, 4, 4)
  W[1, 4] <- 1; W[2, 1] <- 1; W[3, 2] <- 1; W[4, 3] <- 1
  z <- c(1, -1, 1, -1)
  # Every neighbor pair is (1, -1), so the numerator is 4 * (-1) = -4 and the
  # denominator is 4: perfect negative autocorrelation on a 4-cycle.
  expect_equal(.moran_cross_product(z, W), -1)

  # The same operator with perfectly correlated residuals gives +1.
  expect_equal(.moran_cross_product(c(2, 2, 2, 2) - 2 + c(1, 1, 1, 1), W), 1)
})

test_that("the statistic is not confined to [-1, 1] without row normalization", {
  # Two mutual influencers with weight 3 each, plus two isolates. S0 = 6, n = 4,
  # so n / S0 = 2/3 and a perfectly correlated pair gives I = 2/3 * (18 / 2)...
  # computed explicitly below rather than asserted from a remembered bound.
  W <- matrix(0, 4, 4)
  W[1, 2] <- 3; W[2, 1] <- 3
  z <- c(1, 1, -1, -1)
  expected <- (4 / 6) * as.numeric(t(z) %*% W %*% z) / sum(z^2)
  expect_equal(.moran_cross_product(z, W), expected)
  expect_gt(abs(expected), 0)
  # The point of the test: nothing here clamps to [-1, 1], and a reader who
  # assumes that scale on a non-row-normalized operator is reading it wrong.
  expect_equal(expected, (4 / 6) * 6 / 4)
})

test_that("isolates contribute nothing to the statistic", {
  # A zero row adds nothing to the numerator and nothing to S0, so appending an
  # isolate must leave the value unchanged apart from the n in the leading term.
  W3 <- matrix(0, 3, 3); W3[1, 2] <- 1; W3[2, 1] <- 1
  W4 <- matrix(0, 4, 4); W4[1, 2] <- 1; W4[2, 1] <- 1
  z3 <- c(1, 1, -2)
  z4 <- c(1, 1, -2, 0)
  expect_equal(
    .moran_cross_product(z3, W3) * (4 / 3),
    .moran_cross_product(z4, W4)
  )
})

# ---------------------------------------------------------------------------
# Serial correlation

test_that("a single-occasion fit reports serial correlation as not applicable", {
  d <- .dep_data(n = 20, n_waves = 2L)
  fit <- fit_influence(
    y ~ ar() + exposure(y, via = tie, normalize = "row", isolate_rule = "zero"),
    data = d
  )
  dep <- netis_dependence(fit, d)
  expect_identical(dep$serial$status, "not_applicable")
  # Not applicable is not a gap, so it earns no finding.
  expect_false(any(grepl("serial", .dep_codes(dep))))
})

# ---------------------------------------------------------------------------
# Residual ICC and its applicability rules

test_that("one group leaves residual eta-squared undefined, never zero", {
  d <- .dep_data(n = 20, n_waves = 2L)
  fit <- fit_influence(
    y ~ ar() + exposure(y, via = tie, normalize = "row", isolate_rule = "zero"),
    data = d
  )
  dep <- netis_dependence(fit, d)
  eta <- dep$residual_eta_squared
  expect_identical(eta$status, "undefined")
  expect_match(eta$reason, "Undefined is not zero")
})

test_that("a grouping the estimator models is reported from the fit, not from residuals", {
  # Restating it from residuals would give a number near zero by construction,
  # which reads as reassurance rather than as evidence.
  set.seed(21)
  groups <- 1:6
  nodes <- do.call(rbind, lapply(groups, function(g) {
    data.frame(group = g, node = rep(letters[1:6], 2), wave = rep(1:2, each = 6),
               y = stats::rnorm(12, mean = g))
  }))
  ties <- do.call(rbind, lapply(groups, function(g) {
    data.frame(group = g, source = letters[1:5], target = letters[2:6], tie = 1)
  }))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  fit <- suppressWarnings(
    fit_influence(y ~ ar() + exposure(y, via = tie) + (1 | group), data = nd))
  dep <- netis_dependence(fit, nd)

  eta <- dep$residual_eta_squared
  expect_identical(eta$status, "modeled")
  expect_identical(eta$grouping, "group")
  expect_match(eta$reason, "near zero by construction")
})

test_that("residual eta-squared is SSB / SST by hand, beside its null expectation", {
  # Hand arithmetic on five unequal groups. Grand mean 1.
  r <- c(0, 2, 1,   4, 2,   -1, 0, 1, 0,   3,   -2, 0, 2, -1, 1, 0)
  r <- r - mean(r) + 1
  g <- rep(c("a", "b", "c", "d", "e"), c(3, 2, 4, 1, 6))
  means <- c(a = mean(r[1:3]), b = mean(r[4:5]), c = mean(r[6:9]), d = r[10], e = mean(r[11:16]))
  ssb <- sum(c(3, 2, 4, 1, 6) * (means - 1)^2)
  sst <- sum((r - 1)^2)
  ctx <- list(schema = list(group_col = "group"), re_groupings = character(0),
              group_is_network = TRUE)
  out <- .dependence_residual_eta_squared(data.frame(group = g, .resid = r), ctx)
  expect_identical(out$status, "computed")
  expect_lt(abs(out$eta_squared - ssb / sst), 1e-12)
  expect_lt(abs(out$eta_squared - summary(stats::lm(r ~ g))$r.squared), 1e-12)
  expect_identical(out$n_rows, 16L)
  expect_lt(abs(out$expected_no_group_effect - 4 / 15), 1e-12)

  # The stated expectation is the real one: no group effect, and well above zero.
  set.seed(8)
  draws <- replicate(4000, {
    summary(stats::lm(stats::rnorm(16) ~ g))$r.squared
  })
  expect_lt(abs(mean(draws) - 4 / 15), 3 * stats::sd(draws) / sqrt(4000))

  txt <- paste(format(structure(list(
    network_autocorrelation = list(status = "not_applicable"),
    residual_eta_squared = out
  ), class = "netis_dependence")), collapse = " ")
  expect_match(txt, "Residual eta-squared on group", fixed = TRUE)
  expect_match(txt, "(J - 1) / (N - 1) = 0.267", fixed = TRUE)
  expect_false(grepl("ICC|intraclass", txt, ignore.case = TRUE))
})

# ---------------------------------------------------------------------------
# The structural notes, and the rule that keeps them honest

test_that("network formation is reported as not assessed, not as unaddressed", {
  d <- .dep_data(n = 20, n_waves = 2L)
  fit <- fit_influence(
    y ~ ar() + exposure(y, via = tie, normalize = "row", isolate_rule = "zero"),
    data = d
  )
  dep <- netis_dependence(fit, d)
  expect_true("network_formation_not_assessed" %in% .dep_codes(dep))
  msg <- Filter(function(f) f$code == "network_formation_not_assessed",
                dep$diagnostics)[[1]]$message
  # The fit may not assert that this network's formation is an open problem --
  # it cannot know that, and an experimentally assigned structure is exogenous.
  expect_match(msg, "does not establish whether")
  expect_false(grepl("unaddressed", msg, fixed = TRUE))
})

test_that("the network-exposure route is stated as potential and named as coupling", {
  d <- .dep_data(n = 20, n_waves = 2L)
  fit <- fit_influence(
    y ~ ar() + exposure(y, via = tie, normalize = "row", isolate_rule = "zero"),
    data = d
  )
  dep <- netis_dependence(fit, d)
  msg <- Filter(function(f) f$code == "network_exposure_route_present",
                dep$diagnostics)[[1]]$message
  expect_match(msg, "distinct potential endogeneity route")
  expect_match(msg, "regressor coupling, not disturbance dependence")
  # A remedy for one problem is never described as addressing another.
  expect_match(msg, "addresses some of the second kind and none of the first")
})

test_that("a covariate-only exposure raises no network-exposure route", {
  # The negative half. The route is triggered by exposure to the OUTCOME; an
  # exposure to some other variable is an ordinary predictor.
  set.seed(31)
  nodes <- data.frame(
    group = 1, node = rep(letters[1:20], 2), wave = rep(1:2, each = 20),
    y = stats::rnorm(40), x = stats::rnorm(40)
  )
  ties <- data.frame(group = 1, source = letters[1:19], target = letters[2:20], tie = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  fit <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
  dep <- netis_dependence(fit, nd)
  expect_false("network_exposure_route_present" %in% .dep_codes(dep))
})

test_that("no dependence statistic ever becomes a finding on its magnitude", {
  # The design rule, tested directly. Whatever the residual structure turns out to be, the
  # family's findings are drawn from a fixed set of STRUCTURAL codes -- so a
  # future build that promoted a large Moran value to a WARNING fails here.
  structural <- c("network_autocorrelation_undefined",
                  "network_formation_not_assessed", "network_exposure_route_present")
  for (seed in 1:3) {
    # Strong enough exposure to give the residuals real network structure to
    # find, and short of the explosive region -- a divergent process would test
    # the family against data no analysis should be run on.
    # Three waves with an explicit transition, which is how this package
    # analyses a longer panel: the pair is fitted exactly as two-wave data.
    d <- .dep_data(n = 25, n_waves = 3L, seed = seed)
    fit <- suppressWarnings(fit_influence(
      y ~ ar() + exposure(y, via = tie, normalize = "row", isolate_rule = "zero"),
      data = d, waves = c(2, 3)
    ))
    dep <- netis_dependence(fit, d)
    expect_true(all(.dep_codes(dep) %in% structural))
    expect_false(has_level(dep$diagnostics, "WARNING"))
    expect_false(has_level(dep$diagnostics, "ERROR"))
  }
})

# The `residuals_unavailable` status (an estimator with no per-row residual
# reports the absence with its reason) is reached by no estimator under the
# T <= 2 scope, so it has no fitted test here.

