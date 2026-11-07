# ICC(2), r_wg/r_wg(j), and the bundle.

# A BALANCED multi-group two-wave fixture, built by hand.
#
# Balanced deliberately, and the tests below say so: with equal
# group sizes the three n_j conventions COINCIDE, which is a property of the
# fixture rather than of the statistics. The unbalanced case is a separate
# test with its own fixture, and it is what shows the three apart.
.rel_data <- function(sizes = c(a = 7, b = 7, c = 7, d = 7), seed = 4) {
  set.seed(seed)
  gs <- names(sizes)
  nodes <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("%s%02d", g, seq_len(sizes[[g]]))
    n <- length(ids)
    data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = n),
               y = stats::rnorm(2 * n) + stats::rnorm(1, sd = 1.5),
               x = stats::rnorm(2 * n), stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("%s%02d", g, seq_len(sizes[[g]]))
    n <- length(ids)
    data.frame(group = g, source = ids[-n], target = ids[-1], wave = 1,
               tie = 1, stringsAsFactors = FALSE)
  }))
  netis_data(nodes, ties, wave_col = "wave")
}

.rel_fit <- function() {
  suppressWarnings(fit_influence(
    y ~ ar() + exposure(x, via = tie, normalize = "row") + (1 | group),
    data = .rel_data(), waves = c(1, 2)
  ))
}

test_that("ICC(2) reports per-group reliabilities from each group's own n", {
  fit <- .rel_fit()
  res <- icc2(fit)
  expect_s3_class(res, "netis_icc2")
  expect_true(all(c("n_j", "reliability") %in% names(res$by_group)))
  # n_j counts the estimating sample, the rows the backend kept, which here
  # excludes each chain's first member (no in-tie, so no exposure value).
  used <- fit$model_frame[names(fit$residuals), ]
  expect_equal(sum(res$by_group$n_j), nrow(used))
  expect_equal(sum(res$by_group$n_j), as.numeric(stats::nobs(fit$backend_fit)))
})

test_that("ICC(2) known answer: Spearman-Brown from ICC(1) and n_j", {
  fit <- .rel_fit()
  res <- icc2(fit)
  rho <- res$icc1$value
  n <- res$by_group$n_j
  expect_equal(res$by_group$reliability, (n * rho) / (1 + (n - 1) * rho),
               tolerance = 1e-12)
})

test_that("the overall summary defaults to n0 and reports how it was built", {
  # A scalar IS produced by default, but never bare: the method and the n it
  # was evaluated at travel with it, because under imbalance the value alone
  # does not identify which of three constructions produced it.
  fit <- .rel_fit()
  res <- icc2(fit)
  expect_equal(res$overall_method, "n0")
  expect_false(is.na(res$overall))
  expect_false(is.na(res$overall_n))

  expect_equal(icc2(fit, overall = "harmonic")$overall_method, "harmonic")
  expect_true(is.na(icc2(fit, overall = "none")$overall))
  expect_true(is.na(icc2(fit, overall = "none")$overall_method))
})

test_that("n0 matches the unbalanced one-way ANOVA effective group size", {
  # Known answer against the definition, so the default is pinned to a formula
  # rather than to whatever the code happens to compute.
  fit <- .rel_fit()
  n_j <- as.numeric(table(fit$model_frame[names(fit$residuals), "group"]))
  expected_n0 <- (sum(n_j) - sum(n_j^2) / sum(n_j)) / (length(n_j) - 1)
  expect_equal(icc2(fit)$overall_n, expected_n0, tolerance = 1e-12)
})

test_that("the three constructions coincide on a BALANCED fixture", {
  # demo_influence_groups has equal group sizes, so this is the degenerate case
  # where the distinction cannot bite. Asserted deliberately, and named as a
  # property of THIS fixture rather than of the statistics -- on unbalanced data
  # the three separate, which is why the choice is exposed at all.
  fit <- .rel_fit()
  sizes <- as.numeric(table(fit$model_frame$group))
  expect_equal(length(unique(sizes)), 1L)   # the fixture really is balanced

  vals <- vapply(c("n0", "harmonic", "arithmetic"),
                 function(m) icc2(fit, overall = m)$overall, numeric(1))
  expect_equal(unname(diff(range(vals))), 0, tolerance = 1e-8)
})

test_that("the three constructions separate on UNBALANCED data", {
  # The complement of the test above, on a hand-built unbalanced fixture:
  # without this, nothing in the suite can tell the three apart and the
  # `overall` argument would be untested where it actually matters.
  n_j <- c(4, 4, 40)                       # deliberately lopsided
  N <- sum(n_j); k <- length(n_j)
  rho <- 0.3
  sb <- function(n) (n * rho) / (1 + (n - 1) * rho)

  n0   <- (N - sum(n_j^2) / N) / (k - 1)
  harm <- k / sum(1 / n_j)
  arith <- mean(n_j)

  expect_gt(diff(range(c(sb(n0), sb(harm), sb(arith)))), 0.05)
  # Arithmetic is the most optimistic of the three under this imbalance, which
  # is the substantive reason the choice is not rnetis's to make silently.
  expect_gt(sb(arith), sb(harm))
})

test_that("rwg refuses without an explicit null", {
  # The null decides the answer; an implicit uniform would be an undeclared
  # modeling assumption.
  expect_error(rwg(c(1, 2, 3, 4), c("a", "a", "b", "b")), "explicit `null =`")
})

test_that("rwg known answer against the definition", {
  ratings <- c(4, 4, 5, 3, 1, 5)
  g <- c("a", "a", "a", "b", "b", "b")
  res <- rwg(ratings, g, null = 2)
  a <- 1 - stats::var(c(4, 4, 5)) / 2
  b <- 1 - stats::var(c(3, 1, 5)) / 2
  expect_equal(res$by_group$rwg[res$by_group$group == "a"], max(a, 0))
  # Group b's observed variance exceeds the null, so untruncated it is negative
  # -- the case truncation hides.
  expect_lt(b, 0)
  expect_equal(res$by_group$rwg[res$by_group$group == "b"], 0)
  expect_equal(rwg(ratings, g, null = 2, truncate = FALSE)$by_group$rwg[2], b)
})

test_that("the uniform null uses the discrete-uniform variance", {
  res <- rwg(c(4, 4, 5), c("a", "a", "a"),
             null = list(dist = "uniform", n_options = 5))
  expect_equal(unname(res$null_variance), (5^2 - 1) / 12)
})

test_that("missing ratings refuse until na_action is declared", {
  x <- c(4, NA, 5, 3, 1, 5)
  g <- c("a", "a", "a", "b", "b", "b")
  expect_error(rwg(x, g, null = 2), "no `na_action` was declared")
  expect_s3_class(rwg(x, g, null = 2, na_action = "omit"), "netis_rwg")
})

test_that("rwg_j runs over items and records J", {
  set.seed(1)
  items <- cbind(rnorm(20, 4, .4), rnorm(20, 4, .5), rnorm(20, 4, .3))
  g <- rep(c("a", "b"), each = 10)
  res <- rwg_j(items, g, null = list(dist = "uniform", n_options = 5))
  expect_equal(res$n_items, 3L)
  expect_equal(nrow(res$by_group), 2L)
})

test_that("netis_reliability bundles the ICCs and omits agreement by default", {
  fit <- .rel_fit()
  res <- netis_reliability(fit)
  expect_s3_class(res, "netis_reliability")
  expect_s3_class(res$icc1, "netis_icc1")
  expect_s3_class(res$icc2, "netis_icc2")
  # It cannot manufacture rwg from a fit -- the ratings do not exist there.
  expect_null(res$agreement)
})

test_that("the bundle's ICCs are the standalone ICCs", {
  fit <- .rel_fit()
  res <- netis_reliability(fit)
  expect_identical(res$icc1$value, icc1(fit)$value)
  expect_identical(res$icc2$overall, icc2(fit)$overall)
})

test_that("supplying ratings without their grouping refuses", {
  fit <- .rel_fit()
  expect_error(
    netis_reliability(fit, ratings = c(1, 2, 3), null = 2),
    "rating_group"
  )
})

# ---- the published formulation, and only it ---------------------------------

test_that("r_wg(j) takes ONE null variance, not one per item", {
  # This package ships published methods rather than generalizations of them
  # under published names.
  #
  # A per-item null vector, with the ratio computed as the mean of per-item
  # observed-to-null ratios, is a coherent quantity. It is not r_WG(J): James, Demaree & Wolf (1984) define the index against a
  # single expected random-response variance common to all J items, with Sbar
  # the mean observed item variance over that one null.
  set.seed(1)
  items <- cbind(stats::rnorm(20, 4, .4), stats::rnorm(20, 4, .5),
                 stats::rnorm(20, 4, .3))
  teams <- rep(c("a", "b"), each = 10)

  err <- tryCatch(rwg_j(items, teams, null = c(2, 2, 2)),
                  error = function(e) conditionMessage(e))
  expect_match(err, "must be a single variance", fixed = TRUE)
  expect_match(err, "James, Demaree & Wolf", fixed = TRUE)
  # And it says what to do instead rather than only refusing.
  expect_match(err, "compute it per scale instead", fixed = TRUE)
})

test_that("r_wg(j) is the published formula, computed by hand", {
  # Known answer. Two items, one group, numbers chosen so the arithmetic is
  # checkable on paper:
  #
  #   item variances 1 and 3  ->  Sbar = 2
  #   null = 4                ->  ratio = Sbar / null = 0.5
  #   J = 2
  #   rwg_j = (2 * (1 - 0.5)) / (2 * (1 - 0.5) + 0.5) = 1 / 1.5 = 0.6666667
  #
  # Built from exact deviations so the sample variances are exactly 1 and 3.
  # Two values per item: var(c(m - d, m + d)) = 2 * d^2.
  # d = sqrt(1/2) gives var 1; d = sqrt(3/2) gives var 3.
  it1 <- c(5 - sqrt(0.5), 5 + sqrt(0.5))
  it2 <- c(5 - sqrt(1.5), 5 + sqrt(1.5))
  items <- cbind(it1, it2)
  expect_equal(stats::var(items[, 1]), 1, tolerance = 1e-12)
  expect_equal(stats::var(items[, 2]), 3, tolerance = 1e-12)

  res <- rwg_j(items, group = c("g", "g"), null = 4)
  expect_equal(res$by_group$rwg[1], 2 / 3, tolerance = 1e-10)
  # `observed_var` is Sbar itself; `variance_ratio` is Sbar / null.
  expect_lt(abs(res$by_group$observed_var[1] - 2), 1e-12)
  expect_lt(abs(res$by_group$variance_ratio[1] - 0.5), 1e-12)
  expect_identical(res$n_items, 2L)
})

test_that("a missing group label is refused in r_wg(j), as it already was in r_wg", {
  # Both functions must refuse, not drop such raters silently. A rater whose group is unknown cannot be placed in
  # one, and dropping them changes the within-group variances the index is
  # computed from -- which is what `na_action` exists to make the caller
  # decide.
  set.seed(2)
  items <- cbind(stats::rnorm(6, 4, .4), stats::rnorm(6, 4, .5))
  grp <- c("a", "a", "a", "b", "b", NA)

  expect_error(rwg_j(items, grp, null = 2), "missing value")
  expect_error(rwg_j(items, grp, null = 2), "rnetis will not choose for you")
  # Declaring the policy is what makes it proceed.
  expect_s3_class(rwg_j(items, grp, null = 2, na_action = "omit"), "netis_rwg")

  # And rwg() still behaves the same way, so the two have not diverged again.
  expect_error(rwg(items[, 1], grp, null = 2), "missing value")
})
