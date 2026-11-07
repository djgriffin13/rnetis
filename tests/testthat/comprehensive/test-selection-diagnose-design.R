# Selection design diagnostics.

.selection_fixture <- function(n_actors = 10, n_waves = 2) {
  ids <- paste0("a", seq_len(n_actors))
  nodes <- expand.grid(node = ids, wave = seq_len(n_waves),
                       stringsAsFactors = FALSE)
  nodes$group <- 1
  nodes$tenure <- as.numeric(seq_len(nrow(nodes)))
  # friendship must vary across waves, or netis_data() stores it as a static
  # tie variable and every prior_tie() specification here is legitimately
  # refused by .check_selection_static_outcome().
  ties <- data.frame(
    group = 1,
    source = ids[1], target = ids[2],
    wave = seq_len(n_waves),
    friendship = seq_len(n_waves)
  )
  netis_data(nodes[c("group", "node", "wave", "tenure")], ties, wave_col = "wave")
}

test_that("a focal-wave covariate warns that temporal ordering is not established", {
  nd <- .selection_fixture()
  spec <- .parse_selection_formula(friendship ~ sender(tenure))
  d <- .diagnose_selection_design(spec, nd)

  w <- filter_diagnostics(d, level = "WARNING")
  codes <- vapply(w, function(f) f$code, character(1))
  expect_true("contemporaneous_selection_covariate" %in% codes)
  msg <- w[[which(codes == "contemporaneous_selection_covariate")[1]]]$message
  expect_match(msg, "same wave as the tie outcome")
  expect_match(msg, "sender\\(tenure, lag = 1\\)")
})

test_that("a lagged covariate does not trigger the contemporaneous warning", {
  nd <- .selection_fixture()
  spec <- .parse_selection_formula(friendship ~ sender(tenure, lag = 1))
  codes <- vapply(.diagnose_selection_design(spec, nd), function(f) f$code, character(1))
  expect_false("contemporaneous_selection_covariate" %in% codes)
})

test_that("the warning fires for every role, not just sender()", {
  nd <- .selection_fixture()
  spec <- .parse_selection_formula(friendship ~ sender(tenure) + receiver(tenure))
  d <- .diagnose_selection_design(spec, nd)
  contemp <- Filter(function(f) f$code == "contemporaneous_selection_covariate", d)
  expect_length(contemp, 2)
  roles <- vapply(contemp, function(f) f$details$role, character(1))
  expect_setequal(roles, c("sender", "receiver"))
})

test_that("prior_tie() on single-wave data is an ERROR, not a NOTE", {
  nd <- .selection_fixture(n_waves = 1)
  spec <- .parse_selection_formula(friendship ~ prior_tie())
  d <- .diagnose_selection_design(spec, nd)

  errs <- filter_diagnostics(d, level = "ERROR")
  expect_length(errs, 1)
  expect_equal(errs[[1]]$code, "insufficient_waves_for_selection_lag")
  expect_match(errs[[1]]$message, "transitions between waves")

  # The data-level check emits only a NOTE and cannot see the spec, so
  # it does not cover this case -- the reason this check exists separately.
  expect_false(has_level(diagnose_design(nd), "ERROR"))
})

test_that("a lag deeper than the data reaches is an ERROR", {
  nd <- .selection_fixture(n_waves = 2)
  spec <- .parse_selection_formula(friendship ~ prior_tie(lag = 2))
  errs <- filter_diagnostics(.diagnose_selection_design(spec, nd), level = "ERROR")
  expect_length(errs, 1)
  expect_match(errs[[1]]$message, "needs at least 3 waves")
})

test_that("a lag the data does support raises no error", {
  nd <- .selection_fixture(n_waves = 2)
  spec <- .parse_selection_formula(friendship ~ prior_tie())
  expect_false(has_level(.diagnose_selection_design(spec, nd), "ERROR"))
})

test_that("small groups are reported with their actor and dyad counts", {
  nd <- .selection_fixture(n_actors = 4)
  spec <- .parse_selection_formula(friendship ~ prior_tie())
  d <- .diagnose_selection_design(spec, nd)
  small <- Filter(function(f) f$code == "small_group_for_selection", d)
  expect_length(small, 1)
  expect_equal(small[[1]]$details$actors, 4L)
  expect_match(small[[1]]$message, "4 actors, 12 dyads")   # 4 * 3, self-ties excluded
  expect_match(small[[1]]$message, "does not refuse")      # reported, not verdicted
})

test_that("the small-group dyad count leaves out the diagonal even when self-ties are permitted", {
  # An AME model has no self-dyad, whatever the data permits.
  n_actors <- 4L
  ids <- paste0("a", seq_len(n_actors))
  nodes <- expand.grid(node = ids, wave = 1:2, stringsAsFactors = FALSE)
  nodes$group <- 1
  ties <- data.frame(group = 1, source = ids[1], target = ids[2],
                     wave = 1:2, friendship = 1:2)
  nd <- netis_data(nodes[c("group", "node", "wave")], ties, wave_col = "wave",
                   self_ties = "permitted")
  spec <- .parse_selection_formula(friendship ~ prior_tie())
  d <- .diagnose_selection_design(spec, nd)
  small <- Filter(function(f) f$code == "small_group_for_selection", d)
  expect_length(small, 1)
  expect_match(small[[1]]$message, "4 actors, 12 dyads")   # 4 * 3
})

test_that("an adequately sized group is not flagged", {
  nd <- .selection_fixture(n_actors = 12)
  spec <- .parse_selection_formula(friendship ~ prior_tie())
  codes <- vapply(.diagnose_selection_design(spec, nd), function(f) f$code, character(1))
  expect_false("small_group_for_selection" %in% codes)
})

test_that("the fittable assertion refuses on ERROR and passes otherwise", {
  nd_bad <- .selection_fixture(n_waves = 1)
  spec <- .parse_selection_formula(friendship ~ prior_tie())
  expect_error(
    .assert_selection_design_fittable(.diagnose_selection_design(spec, nd_bad)),
    "cannot be estimated as written"
  )

  nd_ok <- .selection_fixture(n_waves = 2)
  expect_true(.assert_selection_design_fittable(.diagnose_selection_design(spec, nd_ok)))
})

test_that("a warning-only specification is still fittable", {
  nd <- .selection_fixture(n_actors = 4)
  spec <- .parse_selection_formula(friendship ~ sender(tenure))
  d <- .diagnose_selection_design(spec, nd)
  expect_true(has_level(d, "WARNING"))
  expect_true(.assert_selection_design_fittable(d))
})

test_that("a lagged term on a static outcome relation is refused", {
  # Constant tie values across waves are stored as a static tie variable, so
  # the prior wave's relation IS the current one. Found while building the
  # lag-completeness fixtures: the failure is invisible in fitted output,
  # where it looks like extremely strong persistence.
  ids <- c("a", "b", "c")
  nodes <- expand.grid(node = ids, wave = 1:2, stringsAsFactors = FALSE)
  nodes$group <- 1
  ties <- rbind(
    data.frame(group = 1, source = "a", target = "b", wave = 1, friendship = 1),
    data.frame(group = 1, source = "a", target = "b", wave = 2, friendship = 1)
  )
  nd <- netis_data(nodes[c("group", "node", "wave")], ties, wave_col = "wave")
  expect_true("friendship" %in% netis_schema(nd)$tie_vars$static)

  spec <- .parse_selection_formula(friendship ~ prior_tie())
  errs <- filter_diagnostics(.diagnose_selection_design(spec, nd), level = "ERROR")
  expect_length(errs, 1)
  expect_equal(errs[[1]]$code, "static_outcome_relation")
  expect_match(errs[[1]]$message, "does not change across waves")

  # A varying relation is fine.
  ties2 <- ties
  ties2$friendship <- c(1, 2)
  nd2 <- netis_data(nodes[c("group", "node", "wave")], ties2, wave_col = "wave")
  expect_false(has_level(.diagnose_selection_design(spec, nd2), "ERROR"))
})

test_that("the static-outcome check only applies to lagged-relation terms", {
  ids <- c("a", "b", "c")
  nodes <- expand.grid(node = ids, wave = 1:2, stringsAsFactors = FALSE)
  nodes$group <- 1
  nodes$tenure <- seq_len(nrow(nodes))
  ties <- rbind(
    data.frame(group = 1, source = "a", target = "b", wave = 1, friendship = 1),
    data.frame(group = 1, source = "a", target = "b", wave = 2, friendship = 1)
  )
  nd <- netis_data(nodes[c("group", "node", "wave", "tenure")], ties, wave_col = "wave")

  # No prior_tie(): a static relation is a perfectly ordinary cross-sectional
  # outcome and must not be refused.
  spec <- .parse_selection_formula(friendship ~ sender(tenure))
  expect_false(has_level(.diagnose_selection_design(spec, nd), "ERROR"))
})

# Variable-level and lagged-static checks: the covariate counterparts to the
# static-outcome ERROR. A role term declares the level its variable must live
# at, which makes a mismatch checkable rather than merely surprising.

.level_fixture <- function() {
  ids <- c("a", "b", "c")
  nodes <- expand.grid(node = ids, wave = 1:2, stringsAsFactors = FALSE)
  nodes$group <- 1
  nodes$tenure <- seq_len(nrow(nodes))     # varies across waves -> dynamic
  nodes$cohort <- rep(c(1, 2, 3), times = 2)  # constant per actor -> static
  ties <- rbind(
    data.frame(group = 1, source = "a", target = "b", wave = 1, friendship = 1),
    data.frame(group = 1, source = "a", target = "b", wave = 2, friendship = 2)
  )
  netis_data(nodes[c("group", "node", "wave", "tenure", "cohort")], ties,
             wave_col = "wave")
}

test_that("a role term naming a variable at the wrong level is refused", {
  nd <- .level_fixture()
  spec <- .parse_selection_formula(friendship ~ sender(friendship))
  errs <- filter_diagnostics(.diagnose_selection_design(spec, nd), level = "ERROR")
  expect_length(errs, 1)
  expect_equal(errs[[1]]$code, "selection_variable_wrong_level")
  expect_match(errs[[1]]$message, "needs 'friendship' to be a node variable")
  expect_match(errs[[1]]$message, "dyad()", fixed = TRUE)   # names the right role
})

test_that("a role term naming a variable that does not exist lists the alternatives", {
  nd <- .level_fixture()
  spec <- .parse_selection_formula(friendship ~ sender(salary))
  errs <- filter_diagnostics(.diagnose_selection_design(spec, nd), level = "ERROR")
  expect_length(errs, 1)
  expect_match(errs[[1]]$message, "no variable of that name exists")
  expect_match(errs[[1]]$message, "tenure")      # the available node variables
})

test_that("a correctly levelled term raises no level error", {
  nd <- .level_fixture()
  spec <- .parse_selection_formula(friendship ~ sender(tenure) + dyad(friendship))
  codes <- vapply(.diagnose_selection_design(spec, nd), function(f) f$code, character(1))
  expect_false("selection_variable_wrong_level" %in% codes)
})

test_that("lagging a static covariate is a NOTE, not an error", {
  nd <- .level_fixture()
  spec <- .parse_selection_formula(friendship ~ sender(cohort, lag = 1))
  d <- .diagnose_selection_design(spec, nd)

  notes <- filter_diagnostics(d, level = "NOTE")
  codes <- vapply(notes, function(f) f$code, character(1))
  expect_true("selection_lagged_static_variable" %in% codes)
  expect_match(notes[[which(codes == "selection_lagged_static_variable")[1]]]$message,
               "lag has no effect")
  # Redundant, not unfittable -- unlike the static *outcome* case.
  expect_false(has_level(d, "ERROR"))
})

test_that("lagging a genuinely dynamic covariate raises nothing", {
  nd <- .level_fixture()
  spec <- .parse_selection_formula(friendship ~ sender(tenure, lag = 1))
  codes <- vapply(.diagnose_selection_design(spec, nd), function(f) f$code, character(1))
  expect_false("selection_lagged_static_variable" %in% codes)
})

test_that("an unlagged static covariate is not flagged", {
  nd <- .level_fixture()
  spec <- .parse_selection_formula(friendship ~ sender(cohort))
  codes <- vapply(.diagnose_selection_design(spec, nd), function(f) f$code, character(1))
  expect_false("selection_lagged_static_variable" %in% codes)
})
