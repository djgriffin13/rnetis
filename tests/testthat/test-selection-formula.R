# Selection formula grammar. The highest-value cases here are the refusals: a
# dyadic model has no defensible default role for a bare covariate, and
# prior_tie() must reject a variable argument the same way ar() does, so both
# are tested by the message the user would actually have to act on rather than
# by class alone.

test_that("every role term parses and carries a default name", {
  p <- .parse_selection_formula(
    friendship ~ sender(tenure) + receiver(tenure) + dyad(same_unit) +
      group(group_size) + prior_tie() + prior_reciprocity()
  )

  expect_equal(p$outcome, "friendship")
  expect_equal(p$senders[[1]]$variable, "tenure")
  expect_equal(p$senders[[1]]$name, "snd_tenure")
  expect_equal(p$receivers[[1]]$name, "rcv_tenure")
  expect_equal(p$dyads[[1]]$name, "dyd_same_unit")
  expect_equal(p$groups[[1]]$name, "grp_group_size")
  expect_equal(p$prior_tie$name, "prior_tie")
  expect_equal(p$prior_reciprocity$name, "prior_recip")
})

test_that("sender() and receiver() on one variable are distinct terms", {
  p <- .parse_selection_formula(friendship ~ sender(tenure) + receiver(tenure))
  expect_length(p$senders, 1)
  expect_length(p$receivers, 1)
  expect_false(identical(p$senders[[1]]$name, p$receivers[[1]]$name))
})

test_that("the lagged-relation terms inherit the outcome from the formula", {
  p <- .parse_selection_formula(advice ~ prior_tie() + prior_reciprocity(lag = 2))
  expect_equal(p$prior_tie$variable, "advice")
  expect_equal(p$prior_reciprocity$variable, "advice")
  expect_equal(p$prior_reciprocity$lag, 2L)
  expect_equal(p$prior_reciprocity$name, "prior_recip_lag2")
})

test_that("a bare covariate is refused and the message names the role wrappers", {
  expect_error(
    .parse_selection_formula(friendship ~ tenure),
    "bare term is not interpretable"
  )
  expect_error(
    .parse_selection_formula(friendship ~ tenure),
    "sender\\(tenure\\)"
  )
  # The refusal must survive alongside valid role terms, not only in isolation.
  expect_error(
    .parse_selection_formula(friendship ~ sender(x) + tenure),
    "tenure"
  )
})

test_that("prior_tie() rejects a variable argument, with a fix that fits the mistake", {
  # Naming the outcome relation is redundant.
  expect_error(
    .parse_selection_formula(friendship ~ prior_tie(friendship)),
    "takes no variable argument"
  )
  # Naming a different relation means the user wanted dyad().
  expect_error(
    .parse_selection_formula(friendship ~ prior_tie(advice)),
    "dyad\\(advice, lag = 1\\)"
  )
  expect_error(
    .parse_selection_formula(friendship ~ prior_reciprocity(advice)),
    "dyad\\(advice, lag = 1\\)"
  )
})

test_that("a positional or named lag still parses", {
  expect_equal(.parse_selection_formula(y ~ prior_tie(2))$prior_tie$lag, 2L)
  expect_equal(.parse_selection_formula(y ~ prior_tie(lag = 2))$prior_tie$lag, 2L)
})

test_that("a zero lag on the outcome relation is refused", {
  expect_error(prior_tie(lag = 0), "at least 1")
  expect_error(prior_reciprocity(lag = 0), "at least 1")
})

test_that("invalid lags are rejected with the offending value", {
  expect_error(sender(x, lag = -1), "non-negative whole number")
  expect_error(sender(x, lag = 1.5), "non-negative whole number")
  expect_error(sender(x, lag = c(1, 2)), "non-negative whole number")
})

test_that("a formula with no response is refused", {
  expect_error(
    .parse_selection_formula(~ sender(tenure)),
    "left-hand side"
  )
})

test_that("duplicate lagged-relation terms are refused", {
  expect_error(
    .parse_selection_formula(y ~ prior_tie() + prior_tie(lag = 2)),
    "Only one prior_tie\\(\\) term"
  )
  expect_error(
    .parse_selection_formula(y ~ prior_reciprocity() + prior_reciprocity(lag = 2)),
    "Only one prior_reciprocity\\(\\) term"
  )
})

test_that("a repeated identical term collapses rather than colliding", {
  # stats::terms() deduplicates identical terms, so this is one sender() term
  # by the time the parser sees it -- the same idempotence `y ~ x + x` has in
  # lm(). Nothing to refuse here, and asserting an error would be asserting an
  # assumption rather than correct behavior.
  p <- .parse_selection_formula(y ~ sender(tenure) + sender(tenure))
  expect_length(p$senders, 1)
  expect_equal(p$senders[[1]]$name, "snd_tenure")
})

test_that("name collisions are refused within a role and across roles", {
  # Distinct terms forced onto one name -- which terms() does not deduplicate,
  # since the calls differ.
  expect_error(
    .parse_selection_formula(y ~ sender(a, name = "shared") + sender(b, name = "shared")),
    "Multiple sender\\(\\) terms"
  )
  # Across roles the per-role checks cannot see the clash, but the assembled
  # design array is one namespace, so it must still be caught.
  expect_error(
    .parse_selection_formula(y ~ sender(a, name = "shared") + receiver(b, name = "shared")),
    "same design column name"
  )
})

test_that("the same variable at two lags does not collide", {
  p <- .parse_selection_formula(y ~ sender(tenure) + sender(tenure, lag = 1))
  expect_equal(p$senders[[1]]$name, "snd_tenure")
  expect_equal(p$senders[[2]]$name, "snd_tenure_lag1")
})

test_that("an explicit name overrides the default", {
  p <- .parse_selection_formula(y ~ sender(tenure, name = "seniority"))
  expect_equal(p$senders[[1]]$name, "seniority")
})

test_that("a non-formula first argument is refused", {
  expect_error(.parse_selection_formula("y ~ sender(x)"), "requires a formula")
})
