# Every term the user wrote must be accounted for.
#
# THE BUG THESE GUARD AGAINST. A formula parser that partitions only
# `attr(terms(), "variables")` sees only the VARIABLES a formula mentions and
# never an interaction label -- those live in `attr(terms(), "term.labels")`.
# So `y ~ x * z` would be parsed as `x` and `z`, the interaction never seen at
# all, and the fit would run to completion reporting main effects only. No
# error, no warning, nothing in summary(). A user writing a moderated model
# would get an unmoderated one and have no way to find out.
#
# That is why these live in `guard` rather than in the influence/selection
# feature groups: they encode the invariant that a specification the package
# cannot estimate must refuse, and they break in response to any change in
# either parser's partitioning logic rather than to one feature's.
#
# WHY THE CHECK IS COMPLETENESS AND NOT A COLON-GREP. Refusing `:` would
# address interactions and nothing else, while the actual hole is a
# residual-category parser with no completeness invariant -- it will swallow
# the next construction nobody anticipated in exactly the same silence. The
# tests therefore assert the invariant (nothing the user wrote is discarded),
# not the instance.
#
# Hierarchical two-way moderation is supported, so the tests below cover the
# completeness chain this file exists to guard: every term is either resolved
# into a supported interaction, or refused BY NAME -- never dropped. The full known-answer verification that a resolved interaction
# multiplies the right assembled columns lives in
# test-influence-interactions.R; what stays here is the narrower claim that
# parsing itself succeeds and produces an `interactions` entry.

# ---- Influence --------------------------------------------------------------

test_that("a hierarchical two-way interaction parses, in both spellings", {
  # `*` and a bare `:` with both main effects present resolve identically:
  # both are the same supported construction.
  for (f in list(y ~ ar() + x * z, y ~ ar() + x + z + x:z)) {
    p <- .parse_influence_formula(f)
    expect_length(p$interactions, 1L)
    expect_identical(p$interactions[[1]]$moderand_kind, "covariate")
  }
})

test_that("a moderated exposure() term now parses and is classified correctly", {
  p <- .parse_influence_formula(y ~ ar() + exposure(y, via = tie) * z)
  expect_length(p$interactions, 1L)
  expect_identical(p$interactions[[1]]$moderand_kind, "exposure")
  expect_identical(p$interactions[[1]]$moderand$name, "ex_y_tie")
})

test_that("a moderated ar() term now parses and is classified correctly", {
  p <- .parse_influence_formula(y ~ ar() * z)
  expect_length(p$interactions, 1L)
  expect_identical(p$interactions[[1]]$moderand_kind, "ar")
})

test_that("an unmoderated specification is not refused", {
  # The completeness check is stricter than anything the parser did before, so
  # the load-bearing assertion is that it refuses nothing it should not.
  #
  # ASSERTED AT THE PARSER, NOT THROUGH A FIT (the cheapest fixture that can
  # still fail for the defect in question). The defect is a parser refusing a
  # term it should have consumed, and it is fully visible at
  # .parse_influence_formula() -- completed lme4 fits add estimation cost and
  # no additional ability to fail. One real fit is kept below as an anti-drift
  # check on the whole path.
  #
  # WHICH EXPECTATION CATCHES WHAT, measured rather than asserted. Running the
  # original deparse-based comparison over these four formulas leaves the
  # LONG-ARGUMENT one unconsumed and the other three clean -- so the first
  # three guard the ordinary case and the fourth is the only one that would
  # have failed against the first implementation. Dropping it would leave the
  # regression it was written for unprotected.
  expect_no_error(.parse_influence_formula(y ~ ar() + x))
  expect_no_error(.parse_influence_formula(y ~ ar() + exposure(y, via = tie)))
  expect_no_error(.parse_influence_formula(y ~ ar() + exposure(y, via = tie) + x + z))
  # The long-argument form the simulation DSL emits: this is what the first
  # implementation refused, by comparing deparsed text whose continuation
  # indent had been folded back in. It must parse.
  expect_no_error(.parse_influence_formula(
    y ~ ar() + exposure(y, via = tie, x_lag = 1, w_lag = 1,
                        normalize = "none", isolate_rule = "na")
  ))
})

test_that("a three-way interaction is refused naming the full term", {
  # Order 3 is never resolved (two-way only), so this label stays unconsumed
  # and reaches the generic completeness refusal -- the two-way resolutions
  # above do not partially satisfy it.
  expect_error(.parse_influence_formula(y ~ ar() + x * z * w), "x:z:w", fixed = TRUE)
})

test_that("a two-way interaction missing a main effect is still refused", {
  # Hierarchy is enforced even though two-way moderation parses --
  # this is not a completeness gap, it is a NAMED refusal from
  # .resolve_influence_interactions() itself.
  expect_error(.parse_influence_formula(y ~ ar() * z - z), "missing its main effect")
})

test_that("an unsupported moderand pairing is still refused", {
  # Two special terms interacting (here, ar() and exposure()) is not one of
  # the three supported moderands, and is refused by name rather than
  # falling through to the generic completeness message.
  expect_error(
    .parse_influence_formula(y ~ ar() * exposure(y, via = tie)),
    "not one of the moderands"
  )
})

# ---- Selection --------------------------------------------------------------

test_that("the Selection parser refuses an interaction too", {
  # Same line, same defect: R/formula-selection-parse.R partitions the same
  # variables attribute. Selection gets the refusal, not the feature.
  spec <- tryCatch(
    .parse_selection_formula(tie ~ prior_tie() + sender(x) * receiver(x)),
    error = function(e) e
  )
  expect_s3_class(spec, "error")
  expect_match(conditionMessage(spec), "sender(x):receiver(x)", fixed = TRUE)
})

test_that("an unmoderated Selection specification still parses", {
  expect_no_error(.parse_selection_formula(tie ~ prior_tie() + sender(x) + receiver(x)))
})

# ---- Intercept suppression --------------------------------------------------
#
# The same class of defect as the dropped interaction: a parser that reads part
# of the terms object and ignores the rest. If `attr(tt, "intercept")` is 0 and
# nothing looks, `y ~ ar() + x - 1` fits WITH an intercept and returns
# coefficients identical to the formula without the `- 1`.

test_that("a suppressed intercept is refused rather than silently restored", {
  expect_error(.parse_influence_formula(y ~ ar() + x - 1), "suppresses the intercept")
  expect_error(.parse_influence_formula(y ~ ar() + 0 + x), "suppresses the intercept")
})

test_that("the intercept refusal fires for Selection too", {
  err <- tryCatch(
    .parse_selection_formula(tie ~ prior_tie() + sender(x) - 1),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "suppresses the intercept")
})

test_that("an ordinary formula fits end to end and keeps its intercept", {
  # THE ONE REAL FIT IN THIS FILE, and it does two jobs so there is only one.
  # The parser assertions above cannot see a regression that leaves parsing
  # intact and breaks assembly, and the intercept refusal needs a fitted object
  # to show the intercept survived rather than merely that parsing allowed it.
  d <- .completeness_fixture()
  # The fixture has three waves, so the transition is named -- the contract
  # applies to a guard test as much as to a user.
  fit <- fit_influence(y ~ ar() + exposure(y, via = tie) + x + z, data = d,
                       waves = c(2, 3))
  expect_true("(Intercept)" %in% names(coef(fit)))
})

test_that("package-provided mechanism kernels are reachable from a formula", {
  # THE BUG THIS GUARDS AGAINST. exposure()'s documented usage
  # `exposure(x, via = w, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))`
  # must work inside a fit_influence() formula, not fail with "could not find
  # function \"bi\"". The parser's term environment has to bind bi() and
  # bi_components(), or a mechanism is usable via compute_exposure() and
  # unusable via the formula. Guard, not feature: any spec_env edit can
  # silently reintroduce the split.
  p <- .parse_influence_formula(
    y ~ ar() + exposure(shock, via = tie, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))
  )
  prov <- attr(p$exposures[[1]]$mechanism, "mechanism_provenance")
  expect_identical(prov$name, "bi")
  expect_identical(prov$params$delta, 0.5)

  p2 <- .parse_influence_formula(
    y ~ ar() + exposure(shock, via = tie, mechanism = bi_components(alpha = 1, gamma = 0))
  )
  prov2 <- attr(p2$exposures[[1]]$mechanism, "mechanism_provenance")
  expect_identical(prov2$name, "bi_components")
})

# ---- the term-evaluation environment ---------------------------------------

test_that("a term argument may reference the caller's own values", {
  # Term evaluation used to be sandboxed on baseenv(), so a term argument
  # could not name ANY user object: `latent(control = my_settings)` and
  # `exposure(normalize = my_choice)` both failed with "object not found" for
  # a formula that reads exactly like every other formula in R. The parent is
  # now environment(formula), which is ordinary R semantics.
  my_lag <- 1L
  my_norm <- "row"
  p <- .parse_influence_formula(
    y ~ ar() + exposure(x, via = tie, x_lag = my_lag, normalize = my_norm)
  )
  expect_identical(p$exposures[[1]]$x_lag, 1L)
  expect_identical(p$exposures[[1]]$normalize, "row")
})

test_that("a user object cannot shadow a rnetis term constructor", {
  # WHAT THE SANDBOX ACTUALLY PROTECTED, preserved by binding the constructors
  # in the CHILD environment rather than by cutting the parent off. A caller
  # with an object called `exposure` in scope -- entirely ordinary, since it
  # is also a substantive noun -- must still get rnetis's term.
  exposure <- "a string that is not a function"
  ar <- 42
  latent <- NULL
  bi <- list(not = "a kernel")

  p <- .parse_influence_formula(y ~ ar() + exposure(x, via = tie))
  expect_length(p$ar, 1L)
  expect_length(p$exposures, 1L)
  expect_s3_class(p$exposures[[1]], "netis_exposure_term")

  # And the mechanism kernels too, which is the case that motivated binding
  # them explicitly in the first place.
  p2 <- .parse_influence_formula(
    y ~ ar() + exposure(x, via = tie,
                        mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))
  )
  prov <- attr(p2$exposures[[1]]$mechanism, "mechanism_provenance")
  expect_identical(prov$name, "bi")
})
