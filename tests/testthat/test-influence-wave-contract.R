# The wave contract: T = 1, T = 2, and the refusal at T > 2.
#
# THIS IS A PACKAGE-SCOPE BOUNDARY, NOT A CLAIM ABOUT THE ESTIMATORS. The models
# handle more waves perfectly well; this release does not fit them, and the
# refusal says so in those terms. What the contract prevents is the thing that
# makes a scope boundary dangerous: a ten-wave study silently analyzed through
# one transition, with nothing in the output saying which one or how much was
# left out.
#
# So the refusal has two halves, and both are tested. rnetis must not choose a
# transition; and once the user has chosen, the fit must state what it used AND
# what it did not.

.wave_fixture <- function(n_waves, n = 8) {
  set.seed(11)
  nodes <- data.frame(
    group = 1,
    node = rep(letters[seq_len(n)], n_waves),
    wave = rep(seq_len(n_waves), each = n),
    y = rnorm(n * n_waves),
    x = rnorm(n * n_waves)
  )
  pairs <- expand.grid(source = letters[seq_len(n)], target = letters[seq_len(n)],
                       stringsAsFactors = FALSE)
  pairs <- pairs[pairs$source != pairs$target, ]
  pairs <- pairs[sample(nrow(pairs), 20), ]
  ties <- do.call(rbind, lapply(seq_len(n_waves), function(w) {
    data.frame(group = 1, pairs, wave = w, w = 1)
  }))
  netis_data(nodes, ties, wave_col = "wave")
}

# ---- T = 2: the canonical case ----------------------------------------------

test_that("two waves fit without the user naming them", {
  fit <- fit_influence(y ~ ar() + exposure(y, via = w), data = .wave_fixture(2))
  expect_s3_class(fit, "netis_influence")
  expect_equal(as.character(fit$spec$waves), c("1", "2"))
  expect_length(fit$spec$waves_unused, 0L)
})

test_that("each unit contributes exactly one outcome row", {
  fit <- fit_influence(y ~ ar() + exposure(y, via = w), data = .wave_fixture(2))
  # The reason the T <= 2 contract is coherent rather than merely conservative:
  # with one outcome row per unit the repeated-unit dependence invariant is
  # satisfied structurally, not by assumption.
  key <- paste(fit$model_frame$group, fit$model_frame$node)
  expect_false(anyDuplicated(key) > 0L)
})

# ---- T > 2: rnetis will not choose -------------------------------------------

test_that("three or more waves without `waves =` is REFUSED, not resolved", {
  err <- tryCatch(
    fit_influence(y ~ ar() + exposure(y, via = w), data = .wave_fixture(5)),
    error = function(e) conditionMessage(e)
  )
  expect_type(err, "character")
  # The message has to do three things: refuse, name what is there, and show
  # the call that resolves it.
  expect_match(err, "5", fixed = TRUE)            # names the wave count
  expect_match(err, "waves = c(", fixed = TRUE)   # shows the fix
})

test_that("the refusal states the scope, not a defect in the model", {
  err <- tryCatch(
    fit_influence(y ~ ar() + exposure(y, via = w), data = .wave_fixture(4)),
    error = function(e) conditionMessage(e)
  )
  # A user must not read this as "these data cannot be analyzed" or "the model
  # cannot handle more waves". Both are false.
  expect_match(err, "this version", fixed = TRUE)
})

test_that("naming a transition fits it", {
  fit <- fit_influence(y ~ ar() + exposure(y, via = w),
                       data = .wave_fixture(5), waves = c(3, 4))
  expect_equal(as.character(fit$spec$waves), c("3", "4"))
})

test_that("a named transition is analyzed EXACTLY as a two-wave problem", {
  # The strongest statement the contract makes: selecting waves 3 -> 4 out of
  # five is the same computation as having only those two waves. Same code
  # path, same estimand, no special handling. If these ever diverge, the
  # contract is a fiction.
  five <- .wave_fixture(5)
  sub_nodes <- netis_nodes(five)[netis_nodes(five)$wave %in% c(3, 4), ]
  # THE TIE TABLE IS STATIC AND MUST NOT BE FILTERED BY WAVE. The fixture
  # supplies the same tie set at every wave, and rnetis stores that once rather
  # than repeating it -- `netis_ties()` returns 20 rows, not 100. Filtering
  # those rows on `wave %in% c(3, 4)` therefore yields NOTHING, and the
  # comparison fit would silently have an empty network and a zero exposure
  # column. That is a fixture error, not a package one, and it is written down
  # because it looks exactly like a real disagreement between the two fits.
  sub_ties <- netis_ties(five)
  two <- netis_data(sub_nodes, sub_ties, wave_col = "wave")

  f_named <- fit_influence(y ~ ar() + exposure(y, via = w), data = five, waves = c(3, 4))
  f_two <- fit_influence(y ~ ar() + exposure(y, via = w), data = two)

  expect_equal(unname(coef(f_named)), unname(coef(f_two)))
  expect_equal(unname(diag(vcov(f_named))), unname(diag(vcov(f_two))))
})

test_that("`waves =` must name exactly two waves", {
  d <- .wave_fixture(5)
  expect_error(fit_influence(y ~ ar(), data = d, waves = c(1, 2, 3)), "two")
  expect_error(fit_influence(y ~ ar(), data = d, waves = 2), "two")
})

test_that("a named wave that is not observed is refused, naming what is", {
  err <- tryCatch(
    fit_influence(y ~ ar(), data = .wave_fixture(3), waves = c(2, 9)),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "9", fixed = TRUE)
  expect_match(err, "1, 2, 3", fixed = TRUE)
})

test_that("the transition must run forwards", {
  expect_error(
    fit_influence(y ~ ar(), data = .wave_fixture(3), waves = c(3, 2)),
    "from < to"
  )
})

# ---- stacking is gone, and says so ------------------------------------------

test_that("waves = 'all' is refused, naming what replaced it", {
  err <- tryCatch(
    fit_influence(y ~ ar(), data = .wave_fixture(4), waves = "all"),
    error = function(e) conditionMessage(e)
  )
  # It used to stack every transition into one fit, which is a repeated-measures
  # estimand this package does not fit. The refusal must not read as a typo
  # report.
  expect_match(err, "one transition", fixed = TRUE)
})

test_that("a list of transitions is refused too", {
  expect_error(
    fit_influence(y ~ ar(), data = .wave_fixture(4), waves = list(c(1, 2), c(2, 3))),
    "one transition"
  )
})

# ---- the fit states both halves ---------------------------------------------

test_that("the fit records waves used AND waves not used", {
  fit <- fit_influence(y ~ ar() + exposure(y, via = w),
                       data = .wave_fixture(10), waves = c(4, 5))
  expect_equal(as.character(fit$spec$waves), c("4", "5"))
  expect_equal(length(fit$spec$waves_available), 10L)
  expect_equal(sort(fit$spec$waves_unused),
               sort(as.character(c(1, 2, 3, 6, 7, 8, 9, 10))))
})

test_that("print() says which waves were analyzed and how many were not", {
  fit <- fit_influence(y ~ ar() + exposure(y, via = w),
                       data = .wave_fixture(10), waves = c(4, 5))
  out <- paste(utils::capture.output(print(fit)), collapse = " ")
  expect_match(out, "4 -> 5", fixed = TRUE)
  expect_match(out, "of 10 present", fixed = TRUE)
  expect_match(out, "8 waves not used", fixed = TRUE)
})

test_that("summary() says it too, since that is where a reader looks", {
  fit <- fit_influence(y ~ ar() + exposure(y, via = w),
                       data = .wave_fixture(10), waves = c(4, 5))
  out <- paste(utils::capture.output(print(summary(fit))), collapse = " ")
  expect_match(out, "8 waves not used", fixed = TRUE)
})

# ---- T = 1: contemporaneous only --------------------------------------------

test_that("one wave fits a specification that needs no prior wave", {
  fit <- fit_influence(y ~ x, data = .wave_fixture(1))
  expect_s3_class(fit, "netis_influence")
  expect_equal(as.character(fit$spec$waves), "1")
})

test_that("one wave refuses ar(), naming what it needs", {
  err <- tryCatch(fit_influence(y ~ ar(), data = .wave_fixture(1)),
                  error = function(e) conditionMessage(e))
  expect_match(err, "prior wave", fixed = TRUE)
})

test_that("one wave refuses lagged(), for the same reason", {
  expect_error(fit_influence(y ~ lagged(x), data = .wave_fixture(1)), "prior wave")
})

# ---- the simulator does not widen the estimator's scope ---------------------

test_that("simulating many waves does not make fit_influence() model them", {
  # A SIMULATOR CAPABILITY IS NOT A SCOPE PROMISE. simulate_influence() can
  # generate as long a panel as is useful; the wave contract is unchanged, and
  # a six-wave simulation is still fitted one transition at a time.
  #
  # This is the guard against the quiet version of a scope change: tests held
  # for the simulator get restored, a fixture with many waves appears, and the
  # contract is relaxed to make them pass rather than the fixture named.
  sim <- simulate_influence(n_nodes = 12, n_waves = 6, beta = 0.3, seed = 2)

  # Unnamed: refused, naming the waves it can see.
  err <- tryCatch(fit_influence(y ~ ar() + exposure(y, via = tie), data = sim$data),
                  error = function(e) conditionMessage(e))
  expect_match(err, "6 waves", fixed = TRUE)
  expect_match(err, "rnetis will not choose", fixed = TRUE)

  # Named: fitted, and ONLY the named transition is analyzed.
  fit <- fit_influence(y ~ ar() + exposure(y, via = tie), data = sim$data,
                       waves = c(4, 5))
  expect_equal(unname(fit$spec$waves), c(4, 5))
  expect_length(fit$spec$waves_unused, 4L)

  # And it says so, which is what stops a six-wave study being read as though
  # six waves were modeled.
  out <- paste(utils::capture.output(print(summary(fit))), collapse = " ")
  expect_match(out, "analyzed 4 -> 5 of 6 present; 4 waves not used", fixed = TRUE)

  # One outcome row per unit, whatever the panel's length.
  expect_equal(nrow(fit$model_frame), 12L)
})

test_that("a simulated two-wave panel needs no waves argument", {
  # The other half: the contract asks for a decision only where there is one
  # to make.
  sim <- simulate_influence(n_nodes = 12, n_waves = 2, beta = 0.3, seed = 2)
  fit <- fit_influence(y ~ ar() + exposure(y, via = tie), data = sim$data)
  expect_equal(unname(fit$spec$waves), c(1, 2))
  expect_length(fit$spec$waves_unused, 0L)
})
