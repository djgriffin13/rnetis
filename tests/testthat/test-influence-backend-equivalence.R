# Reference equivalence: rnetis must equal the estimator it wraps.
#
# THIS IS THE PACKAGE'S CENTRAL CORRECTNESS CLAIM. rnetis contributes a data
# structure, a grammar, and a diagnostics surface around published methods; it
# does not contribute estimators. So the thing that has to be true, and has to
# keep being true, is that calling `fit_influence()` gives exactly what calling
# `lm()` or `lmer()` on the same assembled data would give.
#
# Comparing against the assembled model frame rather than re-deriving one is
# deliberate. The claim under test is "rnetis hands the backend the right
# problem and reports its answer faithfully", not "rnetis assembles a table the
# way this test would" -- assembly is covered by the exposure and formula
# known-answer tests. Re-deriving the table here would test the test.

.eq_fixture <- function(n = 14) {
  set.seed(7)
  # One row per (group, node, wave), built by expansion rather than by
  # recycling: the key has to be unique and `rep()` arithmetic across three
  # factors is easy to get subtly wrong.
  grid <- expand.grid(
    node_i = seq_len(n), wave = 1:2, group = c("A", "B"),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  nodes <- data.frame(
    group = grid$group,
    node = sprintf("%s_n%02d", grid$group, grid$node_i),
    wave = grid$wave,
    y = rnorm(nrow(grid)),
    x = rnorm(nrow(grid)),
    stringsAsFactors = FALSE
  )
  pairs <- do.call(rbind, lapply(c("A", "B"), function(g) {
    ids <- sprintf("%s_n%02d", g, seq_len(n))
    p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
    p <- p[p$source != p$target, ]
    p <- p[sample(nrow(p), 30), ]
    data.frame(group = g, p, wave = 1, w = 1, stringsAsFactors = FALSE)
  }))
  netis_data(nodes, pairs, wave_col = "wave")
}

.eq_single_group <- function(nd) {
  netis_data(
    netis_nodes(nd)[netis_nodes(nd)$group == "A", ],
    netis_ties(nd)[netis_ties(nd)$group == "A", ],
    wave_col = "wave"
  )
}

# ---- pooled == stats::lm() --------------------------------------------------

test_that("a pooled fit equals stats::lm() on the same assembled table", {
  nd <- .eq_fixture()
  single <- .eq_single_group(nd)
  fit <- fit_influence(y ~ ar() + exposure(y, via = w) + x, data = single)

  mf <- fit$model_frame
  direct <- stats::lm(.outcome ~ y_lag1 + ex_y_w + x, data = mf)

  expect_equal(unname(coef(fit)), unname(stats::coef(direct)))
  expect_equal(unname(vcov(fit)), unname(stats::vcov(direct)))
  expect_equal(fit$sigma, summary(direct)$sigma)
  expect_equal(fit$residual_df, direct$df.residual)
})

test_that("the pooled fit reports the backend it actually used", {
  nd <- .eq_fixture()
  single <- .eq_single_group(nd)
  fit <- fit_influence(y ~ ar() + x, data = single)
  expect_identical(fit$backend, "stats")
  expect_identical(fit$spec$estimator, "pooled")
})

# ---- mixed == lme4::lmer() --------------------------------------------------

test_that("a mixed fit equals lme4::lmer() on the same assembled table", {
  skip_if_not_installed("lme4")
  nd <- .eq_fixture()
  fit <- fit_influence(y ~ ar() + exposure(y, via = w) + x + (1 | group), data = nd)

  mf <- fit$model_frame
  direct <- lme4::lmer(.outcome ~ y_lag1 + ex_y_w + x + (1 | group),
                       data = mf, REML = TRUE)

  expect_equal(unname(coef(fit)), unname(lme4::fixef(direct)))
  expect_equal(unname(as.matrix(vcov(fit))),
               unname(as.matrix(stats::vcov(direct))))
  expect_equal(fit$sigma, stats::sigma(direct))
})

test_that("the mixed fit reports the backend it actually used", {
  skip_if_not_installed("lme4")
  nd <- .eq_fixture()
  fit <- fit_influence(y ~ ar() + x + (1 | group), data = nd)
  expect_identical(fit$backend, "lme4")
  expect_identical(fit$spec$estimator, "mixed")
})

test_that("the random-effects level count is recorded, since variance components need it", {
  skip_if_not_installed("lme4")
  nd <- .eq_fixture()
  fit <- fit_influence(y ~ ar() + x + (1 | group), data = nd)
  expect_equal(unname(fit$re_levels[["group"]]), 2L)
})

# ---- the estimator is the formula's choice, not an argument -----------------

test_that("adding a random-effects term is what switches estimator and backend", {
  skip_if_not_installed("lme4")
  nd <- .eq_fixture()
  # The same data and the same fixed part: only the (1 | group) term differs,
  # and it is the whole of the choice. There is no `estimator =` to disagree
  # with the formula, which is why two ways to express one choice was removed.
  single <- .eq_single_group(nd)
  pooled <- fit_influence(y ~ ar() + x, data = single)
  mixed <- fit_influence(y ~ ar() + x + (1 | group), data = nd)

  expect_identical(pooled$spec$estimator, "pooled")
  expect_identical(mixed$spec$estimator, "mixed")
  expect_false(identical(pooled$backend, mixed$backend))
})

test_that("fit_influence() has no estimator argument to disagree with the formula", {
  # THE POINT OF PINNING THE WHOLE SIGNATURE: an estimator argument is not the
  # only way to end up with two ways to express one choice. Any argument added
  # here has to be justified against that, so the list is asserted whole and a
  # change to it has to be deliberate.
  expect_false("estimator" %in% names(formals(fit_influence)))
  expect_identical(
    names(formals(fit_influence)),
    c("formula", "data", "waves", "network_exogeneity",
      "exposure_orthogonal_to_actor_effect")
  )
})

test_that("the two declarations change what is reported, never what is fitted", {
  # This is what makes them admissible as arguments at all. They are claims a
  # user makes about a design, and rnetis cannot check either -- so they must
  # not be able to move an estimate.
  nodes <- data.frame(
    group = 1, node = rep(letters[1:6], 2), wave = rep(1:2, each = 6),
    y = c(1, 2, 3, 2, 1, 4, 2.3, 2.8, 4.4, 3.1, 1.7, 5.2), x = rep(c(1, 0, 1, 0, 1, 0), 2)
  )
  ties <- data.frame(group = 1, source = c("a", "b", "c", "d", "e"),
                     target = c("b", "c", "d", "e", "f"), wave = 1, tie = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")

  plain <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
  declared <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd,
                            network_exogeneity = "exogenous",
                            exposure_orthogonal_to_actor_effect = TRUE)

  expect_equal(coef(plain), coef(declared), tolerance = 1e-12)
  expect_equal(vcov(plain), vcov(declared), tolerance = 1e-12)
  # And the ledger DOES differ, which is the whole reason they exist.
  expect_false(identical(plain$assumptions, declared$assumptions))
})

test_that("center = is gone, and the data-layer surface that works is not", {
  # `center =` decomposed a predictor across the occasions a group is observed.
  # The estimating table holds one occasion under this package's contract, so
  # the within component is identically zero and there is no estimand for the
  # argument to have. It was unreachable in practice -- its own guard refused
  # every call -- rather than merely unused.
  expect_false("center" %in% names(formals(fit_influence)))
})
