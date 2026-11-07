# The Selection wave contract: T = 1 or T = 2, and never a silent choice.
#
# THE BUG THIS PINS. Outcome-wave resolution must not be a by-product of
# choosing a backend route: with a single hardcoded route, an unresolved
# `wave = NULL` flows all the way to `.selection_lag_wave()` and dies there on
# `is.na(match(NULL, waves))`. Tests that use only single-wave data cannot see
# it; the package's documented example, under `R CMD check --run-donttest`,
# does.
#
# Which waves supply the outcome is a question about the DESIGN, not about
# which backend function gets called. It still needs an answer when there is
# only one backend function.

.wave_spec <- function(formula = tie ~ sender(x) + prior_tie()) {
  .parse_selection_formula(formula)
}

.wave_schema <- function(waves) {
  list(waves = waves)
}

# ---- the T <= 2 cases resolve without being asked ---------------------------

test_that("wave-free data is one cross-section", {
  r <- .resolve_selection_waves(.wave_spec(tie ~ sender(x)), .wave_schema(NULL))
  expect_length(r$outcome_waves, 0L)
  expect_length(r$waves_unused, 0L)
  expect_match(r$reason, "no wave dimension")
})

test_that("one wave is that wave", {
  r <- .resolve_selection_waves(.wave_spec(tie ~ sender(x)), .wave_schema(1))
  expect_equal(r$outcome_waves, 1)
  expect_null(r$initial_waves)
  expect_length(r$waves_unused, 0L)
})

test_that("two waves with prior_tie() is ONE transition, not two replicates", {
  # The distinction the T <= 2 scope depends on: a lagged relation consumes the earlier wave
  # as an initial condition, so two waves is one modeled outcome.
  r <- .resolve_selection_waves(.wave_spec(), .wave_schema(c(1, 2)))
  expect_equal(r$outcome_waves, 2)
  expect_equal(r$initial_waves, 1)
  expect_equal(r$waves_analyzed, c(1, 2))
  expect_length(r$waves_unused, 0L)
})

test_that("prior_reciprocity() consumes a wave the same way prior_tie() does", {
  r <- .resolve_selection_waves(
    .wave_spec(tie ~ sender(x) + prior_reciprocity()), .wave_schema(c(1, 2))
  )
  expect_equal(r$outcome_waves, 2)
  expect_equal(r$initial_waves, 1)
})

# ---- the refusals -----------------------------------------------------------

test_that("two waves with NO lagged relation is refused as replication", {
  # Both waves would supply an outcome. Fitting them together asserts that two
  # observations of the same actors are independent given the AME structure.
  err <- tryCatch(
    .resolve_selection_waves(.wave_spec(tie ~ sender(x)), .wave_schema(c(1, 2))),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "both waves would supply an outcome", fixed = TRUE)
  expect_match(err, "not", fixed = TRUE)
  # And it names BOTH ways forward rather than only refusing.
  expect_match(err, "prior_tie()", fixed = TRUE)
  expect_match(err, "waves = c(1, 2)", fixed = TRUE)
})

test_that("three or more waves is refused until the transition is named", {
  err <- tryCatch(
    .resolve_selection_waves(.wave_spec(), .wave_schema(1:10)),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "10 waves", fixed = TRUE)
  expect_match(err, "rnetis will not choose", fixed = TRUE)
  # The suggested pair is the LAST transition, and it is offered rather than
  # taken -- suggesting is not the same as defaulting.
  expect_match(err, "waves = c(9, 10)", fixed = TRUE)
})

test_that("a named pair out of ten waves is analyzed exactly as two-wave data", {
  r <- .resolve_selection_waves(.wave_spec(), .wave_schema(1:10),
                                waves = c(4, 5))
  expect_equal(r$outcome_waves, 5)
  expect_equal(r$initial_waves, 4)
  expect_equal(r$waves_analyzed, c(4, 5))
  expect_length(r$waves_unused, 8L)
})

test_that("naming waves borrows the Influence validation, not a second copy", {
  # Two functions that accept `waves = c(from, to)` must not drift into
  # accepting different things, so both go through .resolve_influence_waves().
  expect_error(.resolve_selection_waves(.wave_spec(), .wave_schema(1:5),
                                        waves = c(1, 2, 3)),
               "exactly two waves")
  expect_error(.resolve_selection_waves(.wave_spec(), .wave_schema(1:5),
                                        waves = c(5, 2)),
               "from < to")
  expect_error(.resolve_selection_waves(.wave_spec(), .wave_schema(1:5),
                                        waves = c(2, 9)),
               "not among the observed waves")
})

test_that("naming waves on wave-free data is refused rather than ignored", {
  expect_error(
    .resolve_selection_waves(.wave_spec(tie ~ sender(x)), .wave_schema(NULL),
                             waves = c(1, 2)),
    "no wave dimension"
  )
})

# ---- what gets reported -----------------------------------------------------

test_that("the wave line states how many waves were NOT used", {
  # A ten-wave dataset fitted over one transition must never print as though
  # ten waves were modeled. Stating the unused count is what makes the
  # omission visible without the reader comparing two numbers themselves.
  r <- .resolve_selection_waves(.wave_spec(), .wave_schema(1:10),
                                waves = c(4, 5))
  expect_identical(.format_selection_waves(r),
                   "Waves: analyzed 4 -> 5 of 10 present; 8 waves not used")
})

test_that("the wave line is singular when exactly one wave went unused", {
  r <- .resolve_selection_waves(.wave_spec(), .wave_schema(1:3), waves = c(2, 3))
  expect_match(.format_selection_waves(r), "1 wave not used", fixed = TRUE)
})

test_that("single-wave and wave-free fits say so rather than counting", {
  expect_match(.format_selection_waves(
    .resolve_selection_waves(.wave_spec(tie ~ sender(x)), .wave_schema(1))
  ), "one wave", fixed = TRUE)
  expect_match(.format_selection_waves(
    .resolve_selection_waves(.wave_spec(tie ~ sender(x)), .wave_schema(NULL))
  ), "none", fixed = TRUE)
})

# ---- end to end -------------------------------------------------------------

test_that("a two-wave transition fit runs and reports its waves", {
  skip_if_not_installed("amen")
  # This is the package's own documented example, which crashed before the
  # wave resolution was restored.
  nodes <- data.frame(
    group = 1, node = letters[1:6], wave = rep(1:2, each = 6),
    tenure = c(1:6, 2:7)
  )
  pairs <- subset(expand.grid(source = letters[1:6], target = letters[1:6],
                              stringsAsFactors = FALSE), source != target)
  ties <- rbind(
    data.frame(group = 1, pairs, wave = 1, friendship = 0:1),
    data.frame(group = 1, pairs, wave = 2, friendship = 1:0)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave",
                   tie_meta = list(friendship = list(zero_meaning = "measured_zero")))

  fit <- fit_selection(friendship ~ sender(tenure) + prior_tie(), data = nd,
                       nscan = 100, burn = 50, odens = 10)

  expect_equal(fit$waves$outcome_waves, 2)
  expect_equal(fit$waves$initial_waves, 1)
  txt <- paste(utils::capture.output(print(fit)), collapse = " ")
  expect_match(txt, "analyzed 1 -> 2 of 2 present; 0 waves not used", fixed = TRUE)
})
