# What a POOLED multi-group fit reports.
#
# THE BUG THIS PINS. `fit_selection()` computed the pooled coefficients
# correctly and then reported `extracts[[1]]` -- group 1's coefficients, group
# 1's draws, group 1's covariance -- under the pooled label. The pooled values
# were stashed on the object but never reached `coefficients`, so a two-group
# fit printed one group's estimates and called them pooled.
#
# Taking `extracts[[1]]` would be correct only if one sampler fitted every
# group at once, so that every group's extract carried the same coefficient
# vector. `pool_beta` fits the groups independently and combines them
# afterwards, which makes exactly that line of code wrong.

.two_group_data <- function(seed = 3, n = 10) {
  set.seed(seed)
  mk <- function(g) {
    ids <- sprintf("%s%02d", g, seq_len(n))
    nodes <- data.frame(group = g, node = ids, tenure = stats::rnorm(n),
                        stringsAsFactors = FALSE)
    p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
    p <- p[p$source != p$target, ]
    p$group <- g
    p$tie <- stats::rbinom(nrow(p), 1, 0.25)
    list(nodes = nodes, ties = p[, c("group", "source", "target", "tie")])
  }
  a <- mk("A")
  b <- mk("B")
  netis_data(rbind(a$nodes, b$nodes), rbind(a$ties, b$ties),
             tie_meta = list(tie = list(zero_meaning = "measured_zero")))
}

.fit_two_groups <- function(pool = TRUE) {
  fit_selection(tie ~ sender(tenure) + receiver(tenure),
                data = .two_group_data(), pool_beta = pool,
                nscan = 120, burn = 20, odens = 5, seed = 1)
}

test_that("a pooled fit reports the POOLED coefficients, not group 1's", {
  skip_if_not_installed("amen")
  fit <- .fit_two_groups()

  # The pooled vector, recomputed from the per-group draws the fit kept.
  expect_equal(unname(coef(fit)), unname(fit$pooled_beta$mean), tolerance = 1e-12)

  # And it is NOT group 1's. This is the assertion that would have caught the
  # bug: with two groups fitted on different data the two differ, and a fit
  # that silently reported group 1 would satisfy every other check here.
  g1 <- colMeans(fit$per_group[[1]]$draws)
  expect_false(isTRUE(all.equal(unname(coef(fit)), unname(g1), tolerance = 1e-8)))

  # A vector, not a matrix: one common beta was reported, so per-group rows
  # would be identical copies reading as estimates that happen to agree.
  expect_null(dim(coef(fit)))
})

test_that("a pooled fit's standard errors are the pooled ones", {
  skip_if_not_installed("amen")
  fit <- .fit_two_groups()
  tbl <- .selection_estimate_table(fit)
  expect_equal(tbl$SE, round(unname(sqrt(fit$pooled_beta$var)), 4),
               tolerance = 1e-9)
})

test_that("a pooled fit has no covariance matrix, and says why", {
  skip_if_not_installed("amen")
  fit <- .fit_two_groups()
  # Coefficient-wise pooling discards covariance; a matrix built from the
  # pooled marginal variances would assert zero covariances nobody estimated.
  expect_null(fit$vcov)
  expect_error(vcov(fit), "one coefficient at a time")
})

test_that("a pooled fit carries no pooled draws, because none exist", {
  skip_if_not_installed("amen")
  fit <- .fit_two_groups()
  # Precision pooling combines means and variances. It never produces a draw
  # set, and group 1's draws are not the fit's draws.
  expect_null(fit$draws)
  # The per-group draws remain reachable, which is where they belong.
  expect_true(is.matrix(fit$per_group[[1]]$draws))
})

test_that("pool_beta = FALSE keeps the groups apart", {
  skip_if_not_installed("amen")
  fit <- .fit_two_groups(pool = FALSE)
  expect_true(is.matrix(coef(fit)))
  expect_identical(rownames(coef(fit)), c("A", "B"))
  expect_null(fit$pooled_beta)
  expect_error(vcov(fit), "pool_beta = FALSE")
})

test_that("pooling is stated wherever a pooled fit is printed", {
  skip_if_not_installed("amen")
  fit <- .fit_two_groups()
  printed <- paste(utils::capture.output(print(fit)), collapse = " ")
  summed <- paste(utils::capture.output(print(summary(fit))), collapse = " ")
  for (txt in list(printed, summed)) {
    expect_match(txt, "pooled across 2 groups", fixed = TRUE)
    expect_match(txt, "COMMON across groups", fixed = TRUE)
  }
  # And the formula is reported rather than printed as NULL.
  expect_match(summed, "tie ~ sender(tenure)", fixed = TRUE)
})

test_that("a single-group fit is unaffected by the pooling default", {
  skip_if_not_installed("amen")
  # Pooling over one group is the identity, so the two calls must agree
  # exactly -- the K = 1 reduction, checked at the level of the fitted object
  # rather than the pooler alone.
  one <- .two_group_data()
  nodes <- netis_nodes(one)
  ties <- netis_ties(one)
  keep_n <- nodes[nodes$group == "A", ]
  keep_t <- ties[ties$group == "A", ]
  nd <- netis_data(keep_n, keep_t,
                   tie_meta = list(tie = list(zero_meaning = "measured_zero")))

  f <- function(pool) {
    fit_selection(tie ~ sender(tenure), data = nd, pool_beta = pool,
                  nscan = 120, burn = 20, odens = 5, seed = 1)
  }
  expect_equal(coef(f(TRUE)), coef(f(FALSE)), tolerance = 1e-12)
})
