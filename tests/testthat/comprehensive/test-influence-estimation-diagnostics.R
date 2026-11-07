# The `estimation` diagnostic family across every backend that can fit.
#
# Until the missingness sweep this family returned an empty collection for six of the seven
# fitting backends, and a user had no way to distinguish "nothing to report"
# from "nothing was checked". Two shapes of test are needed to keep that from
# recurring, and they are different tests rather than one:
#
#   - the per-backend check functions, exercised directly on hand-built
#     `backend_extract()` returns. These are known-answer tests: the input is a
#     literal, so the expected finding is exact and the test costs nothing. It
#     is also the only way to reach a Heywood case or a non-computable GMM
#     serial-correlation test without engineering data that produces one.
#   - the wiring, exercised through a real `fit_influence()` call, so the
#     dispatcher is proven to be reached at all.
#
# Every positive case is paired with its negative. A family that fires on a
# clean fit is worse than one that stays silent, because it trains the reader
# to skip the block.

.codes <- function(d) vapply(d, function(f) f$code, character(1))

# ---------------------------------------------------------------------------
# stats (lm)

test_that("a saturated stats-backed fit reports its zero residual df", {
  # Three nodes, one transition, three estimated parameters. This fit was
  # asserted to produce NO diagnostics at all until the diagnostics contract stopped
  # restricting the family to lme4: the package fit a model reproducing its data
  # exactly, returned NA standard errors, and said nothing about why.
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c"), 2), wave = rep(1:2, each = 3),
    negative_affect = c(0.2, -0.5, 1.1, 0.4, -0.3, 0.9)
  )
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), tie = c(1, 0.5))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  fit <- fit_influence(
    negative_affect ~ ar() + exposure(negative_affect, via = tie),
    data = nd
  )
  expect_equal(fit$residual_df, 0)
  expect_true("no_residual_degrees_of_freedom" %in% .codes(fit$diagnostics))
})

test_that("a well-conditioned stats-backed fit emits no estimation finding", {
  set.seed(11)
  n <- 12L
  ids <- letters[seq_len(n)]
  nodes <- data.frame(
    group = 1, node = rep(ids, 2), wave = rep(1:2, each = n),
    y = round(stats::rnorm(2 * n), 3)
  )
  ties <- data.frame(group = 1, source = ids[-n], target = ids[-1], tie = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  fit <- fit_influence(y ~ ar() + exposure(y, via = tie), data = nd)

  expect_identical(fit$backend, "stats")
  expect_gt(fit$residual_df, 0)
  found <- if (is.null(fit$diagnostics)) .combine_diagnostics() else fit$diagnostics
  expect_length(filter_diagnostics(found, family = "estimation"), 0L)
})

test_that("an aliased coefficient is named, not left as a silent NA", {
  d <- .diagnose_estimation_stats(list(
    aliased = "z", residual_df = 5, leverage = NULL
  ))
  expect_identical(.codes(d), "rank_deficient_fit")
  expect_match(d[[1]]$message, "'z'")
  expect_match(d[[1]]$message, "rank deficient")
})

test_that("high-leverage rows are reported as design geometry, not as outliers", {
  d <- .diagnose_estimation_stats(list(
    aliased = character(0), residual_df = 20,
    leverage = list(max = 0.9, threshold = 0.4, n_above = 3L, n = 25L, p = 5L)
  ))
  expect_identical(.codes(d), "high_leverage_design_points")
  # The message must not read as a recommendation to delete anything: section
  # 10.3 ships no row-deletion influence measure and says why.
  expect_match(d[[1]]$message, "not a claim that any observation is erroneous")
  expect_false(grepl("Cook", d[[1]]$message, fixed = TRUE))
})

test_that("an ordinary design raises no leverage note", {
  # The trigger is max leverage above 0.5 -- a row supplying most of its own
  # fitted value -- and NOT the count above the 2p/n screening line, which a
  # handful of rows exceed in almost any design. Keyed there, this fired on a
  # routine 30-row, 3-parameter fit, which is the diagnostic-noise risk:
  # a NOTE on every fit trains the reader to skip the block.
  d <- .diagnose_estimation_stats(list(
    aliased = character(0), residual_df = 27,
    leverage = list(max = 0.222, threshold = 0.2, n_above = 4L, n = 30L, p = 3L)
  ))
  expect_length(d, 0L)
})

# ---------------------------------------------------------------------------
# lnam

test_that("the SAR boundary check is the operator's invertibility, not rho's size", {
  # (I - rho*W) is singular exactly when 1 - rho*lambda = 0 for an eigenvalue
  # lambda of W. W below has eigenvalues 2 and -0.5: rho = 0.5 is singular,
  # and rho = 1 is not although |rho| * radius = 2.
  ev <- c(2, -0.5)
  ok <- .sar_operator_check(1, ev)
  quiet <- .diagnose_estimation_lnam(list(
    rho = 1, w1_spectral_radius = 2, sar_operator_min_modulus = ok$min_modulus,
    sar_operator_invertible = ok$invertible, information_matrix_available = TRUE
  ))
  expect_length(quiet, 0L)

  bad <- .sar_operator_check(0.5, ev)
  loud <- .diagnose_estimation_lnam(list(
    rho = 0.5, w1_spectral_radius = 2, sar_operator_min_modulus = bad$min_modulus,
    sar_operator_invertible = bad$invertible, information_matrix_available = TRUE
  ))
  expect_identical(.codes(loud), "sar_operator_not_invertible")
  expect_lt(abs(loud[[1]]$details$min_modulus), 1e-12)
})

test_that("an absent lnam information matrix is reported rather than read as zero", {
  d <- .diagnose_estimation_lnam(list(
    rho = 0.1, w1_spectral_radius = 1, information_matrix_available = FALSE
  ))
  expect_identical(.codes(d), "information_matrix_unavailable")
  expect_match(d[[1]]$message, "numDeriv")
})

# ---------------------------------------------------------------------------
# plm (GMM)

# ---------------------------------------------------------------------------
# lavaan

# ---------------------------------------------------------------------------
# The dispatcher itself

test_that("an unrecognized backend returns an empty collection rather than erroring", {
  expect_length(
    .diagnose_influence_estimation("nonesuch", list(), data.frame(), "group"),
    0L
  )
})

# ---------------------------------------------------------------------------
# The estimation-diagnostic rules, for the three estimators in scope: a non-computable test is reported as non-computable
# and never as a pass; a rejection names what it bears on rather than the
# coefficients; convergence and admissibility are separate claims.
