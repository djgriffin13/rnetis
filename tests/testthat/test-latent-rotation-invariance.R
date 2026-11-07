# Rotation invariance of the multi-group latent adjustment.
#
# THE PROPERTY THAT MAKES A MULTI-GROUP LATENT ADJUSTMENT WELL DEFINED, and
# the reason latent() emits group-specific columns.
#
# Each group's latent space is fitted independently and is identified only up
# to translation, reflection and rotation. Group B's sampler could equally have
# returned Z_B R for any orthogonal R -- the same answer in a different frame.
# If both groups' coordinates shared ONE coefficient, that arbitrary choice
# would move the substantive influence estimate: a common slope on "dimension
# 1" asserts that the two groups' dimension 1 mean the same thing, and they do
# not.
#
# With per-group columns the adjustment is a projection onto each group's own
# column SPAN, and rotation preserves a span. So the fitted adjustment, the
# exposure coefficient, and every other coefficient are unchanged.
#
# TESTED ON THE DESIGN RATHER THAN THROUGH ergmm. The property is an algebraic
# fact about the estimator GIVEN positions; routing it through the sampler
# would make a deterministic claim depend on a stochastic fixture, and would
# hide which half had failed when it did.

.lat_frame <- function(n_per = 30, d = 2L, groups = c("A", "B"), seed = 7) {
  set.seed(seed)
  do.call(rbind, lapply(groups, function(g) {
    Z <- matrix(stats::rnorm(n_per * d), ncol = d)
    out <- data.frame(group = g, y = stats::rnorm(n_per),
                      expo = stats::rnorm(n_per), stringsAsFactors = FALSE)
    for (k in seq_len(d)) out[[sprintf("z%d", k)]] <- Z[, k]
    out
  }))
}

# The encoding latent() produces: every group's columns on every row, zero
# off-block.
.lat_blockify <- function(df, d = 2L, groups = c("A", "B")) {
  for (g in groups) {
    for (k in seq_len(d)) {
      df[[sprintf("z%d__%s", k, g)]] <-
        ifelse(df$group == g, df[[sprintf("z%d", k)]], 0)
    }
  }
  df
}

.lat_rotate_group <- function(df, g = "B", theta = 0.7) {
  R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2, 2)
  i <- df$group == g
  Z <- as.matrix(df[i, c("z1", "z2")]) %*% R
  df$z1[i] <- Z[, 1]
  df$z2[i] <- Z[, 2]
  df
}

test_that("rotating one group's latent space leaves the substantive estimate alone", {
  frame <- .lat_frame()
  base <- .lat_blockify(frame)
  rot <- .lat_blockify(.lat_rotate_group(frame))

  cols <- unlist(lapply(c("A", "B"), function(g) sprintf("z%d__%s", 1:2, g)))
  f <- stats::as.formula(paste("y ~ expo + group +", paste(cols, collapse = " + ")))
  m0 <- stats::lm(f, data = base)
  m1 <- stats::lm(f, data = rot)

  # THE SUBSTANTIVE COEFFICIENT IS UNCHANGED.
  expect_equal(unname(coef(m0)[["expo"]]), unname(coef(m1)[["expo"]]),
               tolerance = 1e-10)
  # And so is the whole fit: same fitted values, same residual scale.
  expect_equal(unname(stats::fitted(m0)), unname(stats::fitted(m1)),
               tolerance = 1e-10)
  expect_equal(stats::sigma(m0), stats::sigma(m1), tolerance = 1e-10)

  # The NUISANCE coefficients DO move -- they rotate with the coordinates,
  # which is how the invariance is achieved rather than a defect in it.
  expect_false(isTRUE(all.equal(unname(coef(m0)[["z1__B"]]),
                                unname(coef(m1)[["z1__B"]]))))
  # Group A's are untouched, since only B was rotated.
  expect_equal(unname(coef(m0)[["z1__A"]]), unname(coef(m1)[["z1__A"]]),
               tolerance = 1e-10)
})

test_that("reflection and translation of one group's space are also absorbed", {
  # The other two transformations the space is unidentified up to. Translation
  # is absorbed only because the design carries a group indicator -- which is
  # required for multi-group data anyway -- so this pins that dependence
  # rather than leaving it to be discovered.
  frame <- .lat_frame()
  base <- .lat_blockify(frame)

  flipped <- frame
  i <- flipped$group == "B"
  flipped$z1[i] <- -flipped$z1[i]
  flipped <- .lat_blockify(flipped)

  shifted <- frame
  shifted$z1[i] <- shifted$z1[i] + 3.5
  shifted$z2[i] <- shifted$z2[i] - 1.25
  shifted <- .lat_blockify(shifted)

  cols <- unlist(lapply(c("A", "B"), function(g) sprintf("z%d__%s", 1:2, g)))
  f <- stats::as.formula(paste("y ~ expo + group +", paste(cols, collapse = " + ")))
  e <- function(d) unname(coef(stats::lm(f, data = d))[["expo"]])

  expect_equal(e(base), e(flipped), tolerance = 1e-10)
  expect_equal(e(base), e(shifted), tolerance = 1e-10)
})

test_that("a POOLED latent column would NOT be invariant, which is why it is not used", {
  # The counterfactual, asserted so the reason for the per-group encoding
  # lives in the suite rather than only in a comment. One shared coefficient
  # across independently fitted spaces makes the substantive estimate depend
  # on each group's arbitrary orientation -- and nothing downstream could
  # detect that it had.
  frame <- .lat_frame()
  rot <- .lat_rotate_group(frame)

  pooled <- y ~ expo + group + z1 + z2
  e0 <- unname(coef(stats::lm(pooled, data = frame))[["expo"]])
  e1 <- unname(coef(stats::lm(pooled, data = rot))[["expo"]])
  expect_false(isTRUE(all.equal(e0, e1)))
})

test_that("one group keeps the plain column names", {
  # The common case is unaffected: with a single latent space there is no
  # second frame to disagree with, so no split is needed and the column reads
  # as one covariate.
  expect_identical(.latent_colnames(latent(w, d = 1), groups = "only"),
                   "lat_w_1")
  expect_identical(.latent_colnames(latent(w, d = 1)), "lat_w_1")
})
