# The assumption ledger.
#
# Level 1: the schema's own contracts and the resolution rules. What this file
# is really guarding is that the ledger can REPRESENT its own logic, which a
# single status enum could not.

.ledger_ctx <- function(...) {
  utils::modifyList(
    # DEFAULTS THAT DESCRIBE A REACHABLE FIT. Under T <= 2 there is exactly one
    # transition and each unit
    # contributes one outcome row, so the fixture says that; a test needing
    # another shape passes it explicitly.
    list(estimator = "pooled", has_ar = TRUE,
         outcome_content_terms = "ex_y_tie", covariate_content_terms = character(0),
         network_timing = "fixed", network_exogeneity = "unknown",
         b2_declared = FALSE),
    list(...)
  )
}

.ledger_row <- function(led, id) {
  hit <- Filter(function(a) identical(a$id, id), led$entries)
  if (length(hit) == 0L) NULL else hit[[1]]
}

test_that("an assumption carries three separate fields, not one status", {
  a <- .assumption("B2", "s", "the exposure coefficient",
                   source = "declared", handling = "addressed_by_estimator",
                   diagnostic_evidence = "not_testable",
                   evidence_detail = "no statistic bears on it")
  expect_identical(a$source, "declared")
  expect_identical(a$handling, "addressed_by_estimator")
  expect_identical(a$diagnostic_evidence, "not_testable")

  expect_error(.assumption("X", "s", "sc", "invented", "unaddressed"), "source")
  expect_error(.assumption("X", "s", "sc", "declared", "sort_of"), "handling")
  expect_error(.assumption("X", "s", "sc", "declared", "unaddressed", "probably_fine"),
               "diagnostic_evidence")
})

test_that("a row marked not testable must say why", {
  expect_error(
    .assumption("A3", "s", "sc", "maintained", "unaddressed", "not_testable"),
    "not testable without saying why"
  )
  expect_silent(.assumption("A3", "s", "sc", "maintained", "unaddressed", "none"))
})

test_that("the evidence vocabulary holds only what the ledger can report", {
  # No diagnostic feeds the ledger, so it can say only that a row is not
  # testable or that nothing bears on it -- never that a row is supported.
  expect_identical(.assumption_evidence, c("none", "not_testable"))
  expect_error(.assumption("A3", "s", "sc", "maintained", "unaddressed", "concern"),
               "diagnostic_evidence")
})

test_that("network timing and exogeneity are separate axes", {
  # Fixedness is about time variation; a fixed network can still be formed by
  # latent homophily. "fixed" must therefore not be an exogeneity value.
  expect_false("fixed" %in% .network_exogeneity_values)
  expect_true(all(c("unknown", "exogenous", "sequentially_exogenous",
                    "endogeneity_suspected") %in% .network_exogeneity_values))

  led <- .build_assumption_ledger(.ledger_ctx(network_timing = "fixed",
                                              network_exogeneity = "endogeneity_suspected"))
  expect_identical(led$network$timing, "fixed")
  expect_identical(led$network$exogeneity, "endogeneity_suspected")
  # The fixed x endogeneity_suspected cell is representable AND says the
  # exposure coefficient may be biased.
  expect_match(.ledger_row(led, "C")$statement, "possibly endogenous", fixed = TRUE)
  expect_match(.ledger_row(led, "C")$statement, "robust standard errors do not repair it",
               fixed = TRUE)
})

test_that("an undeclared network is reported as undeclared, never assumed exogenous", {
  led <- .build_assumption_ledger(.ledger_ctx())
  cc <- .ledger_row(led, "C")
  expect_identical(cc$source, "default")
  expect_match(cc$statement, "has not been declared", fixed = TRUE)
  expect_identical(led$network$exogeneity_source, "not declared")
})

test_that("B2 appears only for outcome-content exposure, and records who declared it", {
  # Exposure to a covariate is not a predetermined regressor in this sense, so
  # there is no B2 to price.
  none <- .build_assumption_ledger(.ledger_ctx(outcome_content_terms = character(0)))
  expect_null(.ledger_row(none, "B2"))
  expect_null(.ledger_row(none, "C"))

  undeclared <- .build_assumption_ledger(.ledger_ctx())
  expect_identical(.ledger_row(undeclared, "B2")$source, "default")

  declared <- .build_assumption_ledger(.ledger_ctx(b2_declared = TRUE))
  expect_identical(.ledger_row(declared, "B2")$source, "declared")
  b2 <- .ledger_row(declared, "B2")$statement
  # Stated against what is fitted: the fitted terms are the conditioning set,
  # and no estimator here models the heterogeneity.
  expect_match(b2, "Conditional on the terms in the fitted model", fixed = TRUE)
  expect_match(b2, "No estimator in this package models or removes that heterogeneity",
               fixed = TRUE)
  expect_false(grepl("initial outcome", b2, fixed = TRUE))
})

test_that("the one-transition regime is flagged on the lag, not on exposure", {
  led <- .build_assumption_ledger(.ledger_ctx(n_transitions = 1L, repeated_units = FALSE))
  t2 <- .ledger_row(led, "T2")
  expect_identical(t2$scope, "the lagged-outcome coefficient")
  expect_match(t2$statement, "not separately identified", fixed = TRUE)
  # And it explicitly protects the exposure coefficient from that reading,
  # while naming B2 as what exposure does rest on.
  expect_match(t2$statement, "does not inherit this", fixed = TRUE)
  expect_match(t2$statement, "rest on B2", fixed = TRUE)

  # No ar() means no such composite to warn about.
  no_ar <- .build_assumption_ledger(.ledger_ctx(n_transitions = 1L, has_ar = FALSE,
                                                repeated_units = FALSE))
  expect_null(.ledger_row(no_ar, "T2"))
})

test_that("the heterogeneity row states what each estimator ASSUMES, not what it fixes", {
  # Under the T <= 2 scope no estimator here has a device that could make B
  # "addressed". All three estimators assume it.
  for (est in c("pooled", "mixed", "lnam")) {
    row <- .ledger_row(.build_assumption_ledger(.ledger_ctx(estimator = est)), "B")
    expect_identical(row$handling, "unaddressed")
  }

  # `mixed` is the one most easily misread, so its statement has to say
  # explicitly that fitting a random effect is not addressing the assumption --
  # the random effect's own validity condition IS the assumption.
  mx <- .ledger_row(.build_assumption_ledger(.ledger_ctx(estimator = "mixed")), "B")
  expect_match(mx$statement, "generally false", fixed = TRUE)
  expect_match(mx$statement, "does not repair that", fixed = TRUE)

  pooled <- .ledger_row(.build_assumption_ledger(.ledger_ctx(estimator = "pooled")), "B")
  expect_match(pooled$statement, "No actor effect is estimated", fixed = TRUE)

  # The SAR route models dependence among outcomes through the network, which
  # is a different thing from heterogeneity between actors. The row says so
  # rather than reusing the pooled sentence.
  ln <- .ledger_row(.build_assumption_ledger(.ledger_ctx(estimator = "lnam")), "B")
  expect_match(ln$statement, "not persistent heterogeneity between actors", fixed = TRUE)
})

test_that("no estimator here addresses sequential exogeneity, and A2 says so", {
  # Difference and system GMM would address A2, since their moment conditions
  # ARE this assumption operationalized. None of the three estimators in this
  # package does anything of the kind, so A2 is unaddressed for every one of
  # them -- which is the substantive point.
  for (est in c("pooled", "mixed", "lnam")) {
    a2 <- .ledger_row(.build_assumption_ledger(.ledger_ctx(estimator = est)), "A2")
    expect_identical(a2$handling, "unaddressed")
  }
})

test_that("every assumption row is present whatever the estimator", {
  # THE GUARD AGAINST OVER-CUTTING. A fit does not assume less because the
  # package offers fewer ways to address an assumption.
  led <- .build_assumption_ledger(.ledger_ctx(
    estimator = "mixed", has_ar = TRUE, outcome_content_terms = "ex_y_tie"
  ))
  ids <- vapply(led$entries, function(a) a$id, character(1))
  expect_setequal(ids, c("A1", "A2", "A3", "A4", "B", "B2", "C", "T2"))
})

test_that("A4 is reported as not testable rather than silently absent", {
  a4 <- .ledger_row(.build_assumption_ledger(.ledger_ctx()), "A4")
  expect_identical(a4$diagnostic_evidence, "not_testable")
  expect_false(is.null(a4$evidence_detail))
})

test_that("only A4 and T2 carry diagnostic evidence, and both are not testable", {
  led <- .build_assumption_ledger(.ledger_ctx())
  ev <- vapply(led$entries, function(a) a$diagnostic_evidence, character(1))
  names(ev) <- vapply(led$entries, function(a) a$id, character(1))
  expect_identical(sort(names(ev)[ev != "none"]), c("A4", "T2"))
  expect_true(all(ev[ev != "none"] == "not_testable"))
})

test_that("the ledger formats every row with its provenance", {
  out <- format(.build_assumption_ledger(.ledger_ctx()))
  expect_match(out, "Assumptions carrying this fit", all = FALSE)
  expect_match(out, "\\[A2\\]", all = FALSE)
  expect_match(out, "source: maintained", all = FALSE)
  expect_match(out, "timing fixed \\(measured", all = FALSE)
})

# ---- a declaration is attributed, never presented as a finding --------------

test_that("nothing in the ledger ever reports an assumption as satisfied", {
  # THE STANDING GUARANTEE. Two of these rows come from user declarations and
  # rnetis can test neither; the rest are maintained assumptions no estimator
  # here addresses. Nothing was checked, so no wording may suggest otherwise.
  # A line like "Network exogeneity: satisfied" is the failure to avoid.
  for (exo in c("unknown", "exogenous", "sequentially_exogenous",
                "endogeneity_suspected")) {
    for (b2 in c(TRUE, FALSE)) {
      led <- .build_assumption_ledger(.ledger_ctx(
        network_exogeneity = exo, b2_declared = b2,
        outcome_content_terms = "ex_y_tie", has_ar = TRUE
      ))
      txt <- paste(c(format(led), .format_assumptions_compact(led)),
                   collapse = " ")
      for (bad in c("satisfied", "verified", "established", "confirmed",
                    "holds", "supported", "passed", "valid")) {
        expect_false(grepl(bad, txt, ignore.case = TRUE),
                     info = sprintf("'%s' appeared with exogeneity = %s", bad, exo))
      }
    }
  }
})

test_that("a declared exogeneity is ATTRIBUTED to the user, not asserted", {
  led <- .build_assumption_ledger(.ledger_ctx(
    network_exogeneity = "exogenous", outcome_content_terms = "ex_y_tie"
  ))
  txt <- paste(.format_assumptions_compact(led), collapse = " ")
  expect_match(txt, "DECLARED BY USER", fixed = TRUE)
  expect_match(txt, "rnetis did not test it", fixed = TRUE)
})

test_that("the undeclared default says rnetis assumes nothing, not that it is fine", {
  led <- .build_assumption_ledger(.ledger_ctx(outcome_content_terms = "ex_y_tie"))
  txt <- paste(.format_assumptions_compact(led), collapse = " ")
  expect_match(txt, "NOT DECLARED", fixed = TRUE)
  expect_match(txt, "rnetis assumes nothing about it", fixed = TRUE)
})

test_that("the declaration defaults are 'not asserted', never TRUE", {
  # A default that silently asserted an untestable claim on the user's behalf
  # would be the worst version of this feature.
  f <- formals(fit_influence)
  expect_identical(f$network_exogeneity, "unknown")
  expect_identical(f$exposure_orthogonal_to_actor_effect, FALSE)
})

test_that("T2 states persistence carefully rather than denying autoregression", {
  # rho IS a lagged-outcome coefficient and it does estimate persistence
  # across the transition observed. What one transition cannot establish is
  # that it describes a STABLE autoregressive process over a longer series.
  # The row must say the second without denying the first.
  t2 <- .ledger_row(.build_assumption_ledger(.ledger_ctx(has_ar = TRUE)), "T2")
  expect_match(t2$statement, "estimates persistence across the single observed transition",
               fixed = TRUE)
  expect_match(t2$statement, "should not be read as establishing a stable autoregressive process",
               fixed = TRUE)
  expect_false(grepl("not an autoregression", t2$statement, fixed = TRUE))
})
