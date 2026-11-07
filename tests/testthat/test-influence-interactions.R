# Moderation: assembly, the capability matrix, and the refusals that need a
# real fit to demonstrate. Parser-level refusals that need no fit at all live
# in test-guard-formula-completeness.R.
#
# LEVEL 1 THROUGHOUT. Every fit here is a single-transition `lm()`-equivalent
# on 6-10 nodes -- a known-answer contract, not a statement about a sampler.
# The two capability-matrix refusals fire before any
# backend runs, so they cost a parse and nothing more.

# .moderation_fixture() lives in helper-moderation.R, shared with
# test-influence-conditional-effects.R.

# ---- Known-answer: the product is the assembled column, not the raw one -----
#
# THE MOST IMPORTANT TEST IN THIS FILE. Constructed independently: a DGP with a
# KNOWN true interaction coefficient (-0.9), fit through fit_influence(), and
# compared against an lm() built by hand from the same two waves. Equal
# coefficients to 1e-9 is the only way to be sure the moderand's ASSEMBLED
# column (post-lag, post-exposure) was multiplied by the moderator's, rather
# than some other pairing -- a wrong column pairing would still produce A
# number, plausible-looking, and wrong.

test_that("covariate moderation matches a hand-built lm() exactly", {
  set.seed(99)
  actors <- sprintf("n%d", 1:10)
  xw1 <- stats::rnorm(10); xw2 <- stats::rnorm(10); yw1 <- stats::rnorm(10); zval <- stats::rnorm(10)
  yw2 <- 0.4 * yw1 + 0.3 * xw2 + 0.2 * zval - 0.9 * xw2 * zval + stats::rnorm(10, sd = 0.1)
  nodes <- rbind(
    data.frame(node = actors, wave = 1L, group = 1L, y = yw1, x = xw1, stringsAsFactors = FALSE),
    data.frame(node = actors, wave = 2L, group = 1L, y = yw2, x = xw2, stringsAsFactors = FALSE)
  )
  static <- data.frame(group = 1L, node = actors, z = zval, stringsAsFactors = FALSE)
  ties <- data.frame(group = 1L, source = actors, target = c(actors[-1], actors[1]),
                     tie = 1L, stringsAsFactors = FALSE)
  d <- netis_data(nodes, ties, wave_col = "wave")
  d$nodes$static <- merge(d$nodes$static, static, by = c("group", "node"), all.x = TRUE)
  d$schema$node_vars$static <- "z"

  fit <- fit_influence(y ~ ar() + x * z, data = d)
  manual <- stats::lm(y ~ ylag + x + z + xz,
                      data = data.frame(y = yw2, ylag = yw1, x = xw2, z = zval, xz = xw2 * zval))
  expect_equal(unname(coef(fit)), unname(coef(manual)), tolerance = 1e-9)
  expect_named(coef(fit), c("(Intercept)", "y_lag1", "x", "z", "x:z"))
})

test_that("exposure() moderation multiplies the EXPOSURE column, not the raw content", {
  d <- .moderation_fixture()
  fit <- fit_influence(y ~ ar() + exposure(y, via = tie) * z, data = d, waves = c(2, 3))
  expect_true("ex_y_tie:z" %in% names(coef(fit)))

  # Reconstruct the exposure column exactly as compute_exposure() would and
  # verify the interaction coefficient matches a hand-built lm() on it -- the
  # regression test for "moderating exposure() multiplied the wrong column",
  # which would still return a fit and a plausible-looking coefficient.
  # The transition is named explicitly: with three waves rnetis refuses to choose
  # one, so every fit here says which it means. That is the contract working,
  # not an inconvenience -- the old default silently took the latest.
  # with 3 waves that is wave 2 -> wave 3, so the reconstruction has to match:
  # focal wave 3, lag wave 2. AND exposure()'s own default is x_lag = 1, so
  # the exposure CONTENT is read at the wave BEFORE focal (wave 2), not at
  # focal itself -- using wave 3 here reconstructed the wrong content wave
  # and looked like a fit bug; it was a second fixture-mismatch bug.
  exp_col <- compute_exposure(d, content = "y", via = "tie", wave = 2L)
  tbl <- merge(exp_col, netis_nodes(d)[netis_nodes(d)$wave == 3L, c("group", "node", "y", "x")],
              by = c("group", "node"))
  tbl <- merge(tbl, netis_nodes(d)[netis_nodes(d)$wave == 2L, c("group", "node", "y")],
              by = c("group", "node"), suffixes = c("", "_lag1"))
  zvals <- unique(netis_nodes(d)[, c("group", "node", "z")])
  tbl <- merge(tbl, zvals, by = c("group", "node"))
  ex_col <- grep("^ex_", names(tbl), value = TRUE)[1]
  tbl$interaction <- tbl[[ex_col]] * tbl$z
  manual <- stats::lm(
    stats::as.formula(sprintf("y ~ y_lag1 + `%s` + z + interaction", ex_col)),
    data = tbl
  )
  expect_equal(unname(coef(fit)), unname(coef(manual)), tolerance = 1e-9)
})

test_that("ar() moderation multiplies the LAG column, not the focal-wave outcome", {
  d <- .moderation_fixture()
  fit <- fit_influence(y ~ ar() * z, data = d, waves = c(2, 3))
  expect_true("y_lag1:z" %in% names(coef(fit)))

  # Same focal/lag correction as the exposure test above: wave 3 is focal,
  # wave 2 is the lag, matching fit_influence()'s default latest-transition.
  nodes <- netis_nodes(d)
  tbl <- merge(nodes[nodes$wave == 3L, c("group", "node", "y")],
              nodes[nodes$wave == 2L, c("group", "node", "y")],
              by = c("group", "node"), suffixes = c("", "_lag1"))
  zvals <- unique(netis_nodes(d)[, c("group", "node", "z")])
  tbl <- merge(tbl, zvals, by = c("group", "node"))
  tbl$interaction <- tbl$y_lag1 * tbl$z
  manual <- stats::lm(y ~ y_lag1 + z + interaction, data = tbl)
  expect_equal(unname(coef(fit)), unname(coef(manual)), tolerance = 1e-9)
})

test_that("the formula-shadowing bug this file's mixed test was written against is caught", {
  # PROVES THE FIX, NOT JUST THE ANSWER. as.formula() parses a bare `x:z`
  # column name as ITS OWN interaction operator between covariates `x` and
  # `z`, silently ignoring a same-named column .assemble_influence_table()
  # built by hand -- true for every unsafe (non-backtick-quoted) formula
  # construction site this package had. Coefficients matched every
  # known-answer test above anyway, because for two plain numeric operands
  # R's own recomputation is mathematically identical to this package's --
  # so poisoning the assembled column and checking the fit is unaffected is
  # the only way this class of defect is visible at all. It stopped being
  # invisible for a factor moderator (test below), where the two mechanisms
  # differ.
  d <- .moderation_fixture()
  p <- .parse_influence_formula(y ~ ar() + x * z)
  tr <- .resolve_influence_transitions(netis_schema(d), c(2, 3), p)
  assembled <- .assemble_influence_table(p, d, tr[[length(tr)]])
  f <- .safe_predictor_formula(assembled$predictor_cols)
  poisoned <- assembled$table
  poisoned[["x:z"]] <- 999999999
  fit_normal <- stats::lm(f, data = assembled$table)
  fit_poisoned <- stats::lm(f, data = poisoned)
  expect_false(isTRUE(all.equal(unname(coef(fit_normal)), unname(coef(fit_poisoned)))))
})

test_that("a factor moderator is refused, not silently dummy-coded or dropped", {
  # This is where the shadowing bug stopped being invisible: model.matrix()'s
  # own `:` dummy-codes a factor operand (producing per-level contrast
  # columns like "x:zlo"), a DIFFERENT computation from the numeric product
  # this package deliberately controls -- and even with the shadowing fixed, a
  # factor operand's product is Ops.factor's all-NA output, which lm() would
  # silently drop as aliased, returning a result the package cannot stand
  # behind. .assert_numeric_interaction_operand() closes both:
  # a non-numeric operand refuses before either failure mode can occur.
  d <- .moderation_fixture()
  d$nodes$static$z <- factor(rep(c("lo", "hi"), length.out = nrow(d$nodes$static)))
  expect_error(fit_influence(y ~ ar() + x * z, data = d, waves = c(2, 3)), "not numeric")
})

test_that("a logical moderator is accepted (0/1-compatible, not refused)", {
  d <- .moderation_fixture()
  d$nodes$static$z <- rep(c(TRUE, FALSE), length.out = nrow(d$nodes$static))
  expect_no_error(fit_influence(y ~ ar() + x * z, data = d, waves = c(2, 3)))
})

# ---- Consuming the centering surface ----------------------------------------
#
# A "prepared representation" is never a distinct grammar construct (see
# .attach_interaction_terms()'s header): naming one is just referencing
# the column add_centering()/add_decomposition() produced, the same as any
# other moderator/moderand. These tests are the centering counterpart of the
# three known-answer tests above -- same hand-built-lm() discipline, plus
# the two things unique to a derived column: that its provenance is
# recorded (moderation_provenance), and that the SAME ar()/exposure()
# moderand cannot be moderated by two representations of the same source
# variable at once (a genuinely new refusal this surface makes reachable).

test_that("an add_decomposition() column moderating ar() matches a hand-built lm(), with provenance recorded", {
  d <- .moderation_fixture()
  d2 <- add_decomposition(d, "x", unit = "actor")
  fit <- fit_influence(y ~ ar() * x_within, data = d2, waves = c(2, 3))
  expect_true("y_lag1:x_within" %in% names(coef(fit)))

  nodes <- netis_nodes(d2)
  y_tbl <- merge(nodes[nodes$wave == 3L, c("group", "node", "y")],
                nodes[nodes$wave == 2L, c("group", "node", "y")],
                by = c("group", "node"), suffixes = c("", "_lag1"))
  xw_tbl <- nodes[nodes$wave == 3L, c("group", "node", "x_within")]
  tbl <- merge(y_tbl, xw_tbl, by = c("group", "node"))
  tbl$interaction <- tbl$y_lag1 * tbl$x_within
  manual <- stats::lm(y ~ y_lag1 + x_within + interaction, data = tbl)
  expect_equal(unname(coef(fit)), unname(coef(manual)), tolerance = 1e-9)

  expect_length(fit$moderation_provenance, 1L)
  expect_match(fit$moderation_provenance, "'x_within' is the within/centered component of 'x'", fixed = TRUE)
  expect_equal(influence_effect(fit)$ar[["y_lag1"]]$moderator, "x_within")
})

test_that("an add_centering() column moderating a covariate matches a hand-built lm(), with provenance recorded", {
  d <- .moderation_fixture()
  d2 <- add_centering(d, "z", method = "grand")
  fit <- fit_influence(y ~ ar() + x * z_grand, data = d2, waves = c(2, 3))

  nodes <- netis_nodes(d2)
  manual <- stats::lm(
    y ~ ylag + x + z_grand + xz,
    data = data.frame(
      y = nodes$y[nodes$wave == 3L], ylag = nodes$y[nodes$wave == 2L],
      x = nodes$x[nodes$wave == 3L],
      z_grand = unique(nodes[, c("node", "z_grand")])$z_grand,
      xz = nodes$x[nodes$wave == 3L] * unique(nodes[, c("node", "z_grand")])$z_grand
    )
  )
  expect_equal(unname(coef(fit)), unname(coef(manual)), tolerance = 1e-9)

  expect_length(fit$moderation_provenance, 1L)
  expect_match(fit$moderation_provenance, "'z_grand' is a centered representation of 'z'", fixed = TRUE)
})

test_that("a fit that never references a derived column reports empty moderation_provenance", {
  # The pre- case, exactly recovered: character(0), not NULL and not
  # a note about something the fit never touched.
  d <- .moderation_fixture()
  fit <- fit_influence(y ~ ar() + x * z, data = d, waves = c(2, 3))
  expect_identical(fit$moderation_provenance, character(0))
})

test_that("summary() states which derived representation a moderated term used", {
  d <- .moderation_fixture()
  d2 <- add_decomposition(d, "x", unit = "actor")
  fit <- fit_influence(y ~ ar() * x_within, data = d2, waves = c(2, 3))
  out <- capture.output(summary(fit))
  expect_true(any(grepl("x_within.*within/centered component of 'x'", out)))
})

test_that("the same ar() moderand cannot be moderated by two representations of one source variable at once", {
  d <- .moderation_fixture()
  d2 <- add_decomposition(d, "x", unit = "actor")
  expect_error(
    fit_influence(y ~ ar() * x_within + ar() * x_between, data = d2,
                  waves = c(2, 3)),
    "moderated by more than one moderator"
  )
})

test_that("the double-moderation refusal does not fire for an ordinary covariate x covariate pair", {
  # x_within and x_between used TOGETHER as a plain covariate interaction
  # (neither wrapped in ar()/exposure()) is not the defect the refusal above
  # exists for -- plain-covariate interactions are never effect-sized, so there is no conditional-effect computation to silently get
  # wrong. Both derived columns' provenance is recorded.
  d <- .moderation_fixture()
  d2 <- add_decomposition(d, "x", unit = "actor")
  fit <- expect_no_error(fit_influence(y ~ ar() + x_within * x_between, data = d2, waves = c(2, 3)))
  expect_length(fit$moderation_provenance, 2L)
})

# ---- Refusals that need assembly/estimator resolution to demonstrate --------

test_that("a hierarchy violation is refused even when the model would otherwise fit", {
  d <- .moderation_fixture()
  expect_error(fit_influence(y ~ ar() * z - z, data = d), "missing its main effect")
})

test_that("an unsupported moderand pairing is refused by name", {
  d <- .moderation_fixture()
  expect_error(
    fit_influence(y ~ ar() * exposure(y, via = tie), data = d),
    "not one of the moderands"
  )
})

test_that("a missing moderator drops exactly the rows it touches, via the interaction column", {
  d <- .moderation_fixture()
  d$nodes$static$z[1] <- NA_real_
  fit <- fit_influence(y ~ ar() + x * z, data = d, waves = c(2, 3))
  # One node's moderator is missing, so the product is NA only for that node
  # -- na.omit() drops one row from an otherwise n-node estimating sample.
  n_full <- length(unique(netis_nodes(d)$node))
  expect_equal(length(fitted(fit)), n_full - 1L)
})

test_that("naming collisions between an interaction column and an existing term are caught", {
  d <- .moderation_fixture()
  d$nodes$dynamic$`x:z` <- stats::rnorm(nrow(netis_nodes(d)))
  d$schema$node_vars$dynamic <- c(d$schema$node_vars$dynamic, "x:z")
  expect_error(
    fit_influence(y ~ ar() + x * z + `x:z`, data = d, waves = c(2, 3)),
    "same column name"
  )
})

test_that("summary() states that a moderated term's coefficient is conditional", {
  d <- .moderation_fixture()
  fit_ex <- fit_influence(y ~ ar() + exposure(x, via = tie) * z, data = d, waves = c(2, 3))
  out <- paste(capture.output(summary(fit_ex)), collapse = "\n")
  expect_match(out, "the coefficient on 'ex_x_tie' is its effect conditional on z = 0", fixed = TRUE)
  expect_match(out, "ex_x_tie + ex_x_tie:z * z", fixed = TRUE)

  fit_ar <- fit_influence(y ~ ar() * z, data = d, waves = c(2, 3))
  out_ar <- paste(capture.output(summary(fit_ar)), collapse = "\n")
  expect_match(out_ar, "the coefficient on 'y_lag1' is its effect conditional on z = 0", fixed = TRUE)
  expect_match(out_ar, "not an unconditional stability parameter", fixed = TRUE)

  # Nothing of the kind on an unmoderated fit.
  plain <- fit_influence(y ~ ar() + exposure(x, via = tie) + z, data = d, waves = c(2, 3))
  expect_false(any(grepl("Moderated term", capture.output(summary(plain)))))
})
