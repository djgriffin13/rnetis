# SHORT CHAINS, deliberately.
#
# Every tolerance in this file is either an exact structural value (a row
# count, a class, a field name) or an INTERNAL CONSISTENCY identity -- two
# quantities derived from the same fit that must agree with each other, such
# as shares summing to 1. Neither kind moves with chain length: the chain only
# has to be long enough to return a well-formed object. A tolerance against an
# externally known truth would need longer chains and a recorded margin.
#
# Selection effect sizes. The assertions that matter are about honesty as much as arithmetic: which
# estimand was computed, and what the interval does and does not cover.

# Cached through helper-selection.R's memo: most of these calls pass the
# default arguments, and the tests assert different things about ONE fit, so
# refitting would buy nothing but check time.
.effect_fit <- function(n_groups = 1, family = "bin", pool_beta = TRUE) {
  .cached_fixture(paste("effect", family, n_groups, pool_beta, sep = "_"), {
    nd <- .selection_fixture_data(family = family, n_groups = n_groups,
                                  n_actors = 10, n_waves = 2, seed = 21L)
    fit_selection(tie ~ sender(x) + prior_tie(), data = nd, pool_beta = pool_beta,
                  family = family, nscan = 80, burn = 20, odens = 10)
  })
}

test_that("a probability contrast is returned on the probability scale", {
  skip_if_not_installed("amen")
  e <- selection_effect(.effect_fit(), "prior_tie")

  expect_s3_class(e, "netis_selection_effect")
  expect_equal(e$scale, "probability")
  # A probability difference cannot leave [-1, 1], whatever the latent scale did.
  expect_gte(e$estimate, -1)
  expect_lte(e$estimate, 1)
  expect_true(e$interval[1] <= e$estimate && e$estimate <= e$interval[2])
  # Persistence is strongly positive in the DGP, so this should be clearly so.
  expect_gt(e$estimate, 0)
})

test_that("the three estimands differ, and each says which it is", {
  skip_if_not_installed("amen")
  fit <- .effect_fit()
  cond <- selection_effect(fit, "prior_tie", type = "conditional")
  popn <- selection_effect(fit, "prior_tie", type = "population")
  ref  <- selection_effect(fit, "prior_tie", type = "reference")

  # Phi is nonlinear, so these are not rescalings of one another.
  expect_false(isTRUE(all.equal(cond$estimate, popn$estimate)))
  expect_false(isTRUE(all.equal(cond$estimate, ref$estimate)))

  expect_match(cond$coverage, "plug-in|posterior mean|not fully propagated|narrower")
  expect_false(grepl("coefficients and the actor effects jointly", cond$coverage,
                     fixed = TRUE))
  expect_match(popn$coverage, "uncertainty in the coefficients and the additive actor-effect")
  expect_match(ref$coverage, "hypothetical pair")
})

test_that("the conditional interval states its propagation honestly", {
  skip_if_not_installed("amen")
  # The printed sentence must reflect what this fit can actually support.
  # `amen::ame()` returns the additive actor effects as posterior means, so
  # the conditional interval is a plug-in one and must say so. An interval
  # claiming a joint propagation it did not have is the defect this assertion
  # exists to catch.
  out <- paste(capture.output(print(selection_effect(.effect_fit(), "prior_tie"))),
               collapse = " ")
  expect_match(out, "narrower than a fully propagated one")
  expect_false(grepl("retained actor effects", out, fixed = TRUE))
  expect_match(out, "percentage points")
})

test_that("the population estimand does not depend on the random-number stream", {
  skip_if_not_installed("amen")
  fit <- .effect_fit()
  # Integrated exactly, so it does not depend on the random-number stream.
  set.seed(1); a <- selection_effect(fit, "prior_tie", type = "population")
  set.seed(2); b <- selection_effect(fit, "prior_tie", type = "population")
  expect_identical(a$draws, b$draws)
  set.seed(1); c1 <- selection_effect(fit, "prior_tie", type = "conditional")
  set.seed(2); c2 <- selection_effect(fit, "prior_tie", type = "conditional")
  expect_equal(c1$estimate, c2$estimate)
})

test_that(".population_offset_variance() uses Var(a_i + b_j) = va + vb, not + 2*cab", {
  # For a real dyad (i, j), i != j: a_i and b_j belong to two different
  # actors drawn independently, so Cov(a_i, b_j) = 0 and Var(a_i + b_j) =
  # va + vb. cab = Cov(a_i, b_i) is a WITHIN-actor quantity and has no bearing
  # on a dyad between two different actors.
  expect_identical(
    .population_offset_variance(c(va = 2, vb = 3, cab = 1.4, rho = 0, ve = 1)), 5)
})

test_that("a valued outcome reports a value contrast, computed directly", {
  skip_if_not_installed("amen")
  fit <- .effect_fit(family = "nrm")
  e <- selection_effect(fit, "snd_x", delta = 2)

  expect_equal(e$scale, "outcome value")
  # A valued outcome is linear in the coefficient, so the contrast is exactly
  # delta * beta -- no dyad averaging can change it, and routing it through
  # the probability path would be a no-op in a heavier disguise.
  expect_equal(e$estimate, 2 * mean(fit$per_group[[1]]$draws[, "snd_x"]),
               tolerance = 1e-8)
})

test_that("an unknown term is refused with the available ones listed", {
  skip_if_not_installed("amen")
  err <- tryCatch(selection_effect(.effect_fit(), "nonsense"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "is not a term in this fit")
  expect_match(err, "prior_tie")
})

test_that("a multi-group fit refuses to average a contrast across groups", {
  skip_if_not_installed("amen")
  # Both routes refuse, each for its own stated reason. The unpooled assertion
  # once ran on the pooled DEFAULT and passed only because the refusal misread
  # a pooled fit as unpooled.
  unpooled <- .effect_fit(n_groups = 2, pool_beta = FALSE)
  expect_error(selection_effect(unpooled, "prior_tie"), "must name one")
  expect_error(selection_effect(unpooled, "prior_tie"), "imply a joint model")

  pooled <- .effect_fit(n_groups = 2)
  expect_error(selection_effect(pooled, "prior_tie"), "must name one")
  expect_error(selection_effect(pooled, "prior_tie"), "with pooled coefficients")

  e <- selection_effect(pooled, "prior_tie", group = "1")
  expect_equal(e$group, "1")
})

test_that("delta scales the contrast", {
  skip_if_not_installed("amen")
  fit <- .effect_fit()
  small <- selection_effect(fit, "snd_x", delta = 0.5)
  big <- selection_effect(fit, "snd_x", delta = 2)
  expect_gt(abs(big$estimate), abs(small$estimate))
})

test_that("the flattening is exact for every block type", {
  # A distinct value in every cell, so any permutation of the dyad layout shows.
  n <- 3
  ids <- c("a", "b", "c")
  Y <- matrix(0, n, n, dimnames = list(ids, ids))
  v <- c(10, 20, 30)
  Xrow <- matrix(v, n, 1, dimnames = list(ids, "snd_z"))
  Xcol <- matrix(v + 1, n, 1, dimnames = list(ids, "rcv_z"))
  D <- array(seq_len(n * n), c(n, n, 1), dimnames = list(ids, ids, "dyd_q"))

  design <- list(Y = Y, Xrow = Xrow, Xcol = Xcol, Xdyad = D)
  X <- .effect_design_matrix(design, c("intercept", "snd_z", "rcv_z", "dyd_q"))
  expect_equal(nrow(X), n * n)

  # Rebuild each column as the definition states it, cell by cell.
  want_snd <- want_rcv <- want_dyd <- matrix(NA_real_, n, n)
  for (i in 1:n) for (j in 1:n) {
    want_snd[i, j] <- v[i]         # sender covariate: actor i's value
    want_rcv[i, j] <- v[j] + 1     # receiver covariate: actor j's value
    want_dyd[i, j] <- D[i, j, 1]   # dyadic covariate: the cell itself
  }
  expect_equal(X[, "snd_z"], as.vector(want_snd))
  expect_equal(X[, "rcv_z"], as.vector(want_rcv))
  expect_equal(X[, "dyd_q"], as.vector(want_dyd))
  expect_true(all(X[, "intercept"] == 1))
})

test_that("the design matrix expands node covariates along the right margin", {
  skip_if_not_installed("amen")
  fit <- .effect_fit()
  design <- fit$designs[[1]]
  X <- .effect_design_matrix(design, colnames(fit$per_group[[1]]$draws))

  n <- dim(design$Y)[1]
  expect_equal(nrow(X), n * n)
  expect_true(all(X[, "intercept"] == 1))
  # A sender covariate is constant down a row of the dyad matrix: cell (i, j)
  # takes actor i's value regardless of j.
  snd <- matrix(X[, "snd_x"], n, n)
  expect_equal(snd[, 1], snd[, 2])
  expect_equal(as.numeric(snd[, 1]), as.numeric(design$Xrow[, "snd_x"]))
})
