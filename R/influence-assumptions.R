# The assumption ledger.
#
# Every Influence fit carries what it is ASSUMING, resolved against the
# specification rather than printed as boilerplate.
#
# INFORMATION, NOT ROUTING. The formula decides the estimator; the ledger
# only reports. A fit's assumptions do not become less true because the
# package offers no estimator that addresses them, so an assumption that no
# available estimator addresses is reported as unaddressed -- which is more
# important to say, not less.
#
# The T2 row is the clearest case. A fit analyses one transition, so it applies to
# every lagged-outcome fit, and it says the thing a reader of such a fit most needs
# to know.
#
# STATED CAREFULLY, because it is easy to overstate in either direction. In
# y2 = a + rho*y1 + b*x1 + e, rho IS a lagged-outcome coefficient and it does
# estimate persistence across the transition that was observed -- it does not
# stop being autoregressive in form because there is only one transition. What
# a single transition cannot establish is that rho describes a STABLE
# autoregressive process governing a longer series: with one transition,
# persistence and the initial-condition relation with persistent actor
# heterogeneity are not separately identified, so what is estimated is a
# composite of the two. The row says that, and does not say the coefficient is
# "not autoregression".
#
# THREE FIELDS, NOT ONE STATUS. Where an assumption comes from and what in
# the fit handles it are different questions, and whether any statistic could
# bear on it is a third, so the record carries:
#
#   source              where the assumption comes from
#   handling            what, if anything, in the fit addresses it
#   diagnostic_evidence "not_testable" where no statistic computed from a fit
#                       can bear on the row, "none" otherwise. No diagnostic
#                       feeds the ledger, so no row reports evidence for or
#                       against itself.

#' @noRd
.assumption_sources <- c("maintained", "default", "declared")

#' @noRd
.assumption_handling <- c("addressed_by_estimator", "unaddressed", "not_applicable")

#' @noRd
.assumption_evidence <- c("none", "not_testable")

#' Values the user may declare for how the network relates to the outcome process
#'
#' TWO AXES, NOT ONE, and the split is the correction that makes the ledger
#' able to state its own logic. Fixedness is about TIME VARIATION; exogeneity is
#' about whether tie formation is related to the outcome's innovations. A
#' network that never changes during the observation window can still have been
#' formed by latent homophily -- exactly the case where exposure correlates with
#' persistent actor heterogeneity -- so "fixed" is not a kind of "exogenous".
#'
#' Timing is MEASURED from the data, never declared: rnetis can see whether the
#' resolved conduit varies across the estimating waves, and asking a user to
#' assert an observable fact only invites the assertion to contradict the data.
#'
#' @noRd
.network_exogeneity_values <- c("unknown", "exogenous", "sequentially_exogenous",
                                "endogeneity_suspected")

#' Construct one assumption-ledger entry
#'
#' @param id short stable identifier (`"A2"`, `"B2"`, ...), so downstream code
#'   and documentation can refer to a row without matching on prose.
#' @param statement what is being assumed, in one sentence, in the model's own
#'   terms.
#' @param scope which part of the fit it bears on (`"the exposure coefficient"`,
#'   `"all coefficients"`, `"the reported standard errors"`).
#' @param source one of `.assumption_sources`.
#' @param handling one of `.assumption_handling`.
#' @param diagnostic_evidence one of `.assumption_evidence`.
#' @param evidence_detail why, when `diagnostic_evidence` is `"not_testable"`,
#'   no statistic can bear on the row. Required in that case: a row that says
#'   it cannot be tested without saying why cannot be checked.
#' @noRd
.assumption <- function(id, statement, scope, source, handling,
                        diagnostic_evidence = "none", evidence_detail = NULL,
                        headline = NULL) {
  .assert_enum(source, .assumption_sources, "source")
  .assert_enum(handling, .assumption_handling, "handling")
  .assert_enum(diagnostic_evidence, .assumption_evidence, "diagnostic_evidence")
  if (!identical(diagnostic_evidence, "none") && is.null(evidence_detail)) {
    stop(
      sprintf("Internal error: assumption '%s' is marked not testable without saying why.", id),
      call. = FALSE
    )
  }
  list(id = id, statement = statement, scope = scope, source = source,
       handling = handling, diagnostic_evidence = diagnostic_evidence,
       evidence_detail = evidence_detail,
       # WHAT THE COMPACT SUMMARY MUST SAY OUT LOUD, for the entries whose
       # scope alone would be read the wrong way. Most rows are safe as
       # "<scope> -- assumed, not addressed": the reader already knows what the
       # scope means. T2 is not: "the lagged-outcome coefficient -- structural"
       # tells a reader nothing about the thing that matters, which is that the
       # number describes one transition and not a stable autoregressive process.
       # A headline is added only where its absence would
       # invite a misreading, never as decoration.
       headline = headline)
}

#' Is the network time-varying across the waves this fit draws on?
#'
#' MEASURED, not declared. A network is time-varying when its ties differ
#' between the waves the fit uses -- which dyads have rows, OR the value any
#' tie variable takes on them. Comparing only the set of rows would call a
#' complete dyadic table fixed however its values changed, since such a table
#' has a row for every dyad at every wave.
#'
#' Returns `"fixed"`, `"time_varying"`, or `"undetermined"`.
#'
#' Three cases give `"fixed"` rather than `"undetermined"`, and the distinction
#' matters because a stability reading is issued for a fixed network and
#' deliberately refused for a time-varying network:
#'
#' - a **static network** stored without a wave column at all. Static networks
#' are first-class in this package, and a network that has no
#'   wave dimension cannot vary along one (`netis_data()` stores a tie table
#'   that is identical at every wave this way);
#' - data with a single wave;
#' - several waves whose tie rows and values are identical.
#'
#' `"undetermined"` is reserved for the case where the question genuinely
#' cannot be answered: the fit draws on a single wave of a network that does
#' have a wave dimension -- including a tie table observed at only some of the
#' dataset's waves -- so whether it moves elsewhere in the panel is not
#' something this fit's own data reveals. It is not a synonym for fixed.
#'
#' @param waves_used the waves whose networks entered the fit. Empty means
#'   "every wave in the tie table", which is the right question for a fit that
#'   recorded no per-transition conduit resolution.
#' @param tie_vars the tie variables the fit's conduits read, whose values are
#'   compared across waves. Empty means every tie variable.
#' @noRd
.measure_network_timing <- function(data, schema, waves_used = character(0),
                                    tie_vars = character(0)) {
  ties <- netis_ties(data)
  if (is.null(ties) || nrow(ties) == 0L) {
    return("undetermined")
  }
  key <- c(schema$group_col, schema$source_col, schema$target_col)
  key <- key[key %in% names(ties)]
  if (length(key) == 0L) {
    return("undetermined")
  }
  # A network with no wave dimension is fixed by construction.
  if (is.null(schema$wave_col) || !(schema$wave_col %in% names(ties))) {
    return("fixed")
  }
  present <- unique(as.character(ties[[schema$wave_col]]))
  waves <- if (length(waves_used) > 0L) {
    intersect(present, unique(as.character(waves_used)))
  } else {
    present
  }
  if (length(waves) == 0L) return("undetermined")
  if (length(waves) == 1L) {
    # Data with one wave cannot vary over time; one wave USED out of several
    # is a fit that cannot see.
    return(if (length(schema$waves) <= 1L) "fixed" else "undetermined")
  }
  # Each wave's ties as one sorted vector of "key + every tie variable's
  # value" strings: two waves agree only when the same dyads carry the same
  # values.
  value_cols <- setdiff(names(ties), c(key, schema$wave_col))
  if (length(tie_vars) > 0L) value_cols <- intersect(value_cols, tie_vars)
  per_wave <- lapply(waves, function(w) {
    rows <- ties[as.character(ties[[schema$wave_col]]) == w, c(key, value_cols), drop = FALSE]
    sort(do.call(paste, c(lapply(rows, function(v) {
      if (is.numeric(v)) sprintf("%.15g", v) else as.character(v)
    }), sep = .NETIS_KEY_SEP)))
  })
  same <- all(vapply(per_wave[-1], function(x) identical(x, per_wave[[1]]), logical(1)))
  if (same) "fixed" else "time_varying"
}

#' Build the assumption ledger for one Influence fit
#'
#' @param ctx a list with `estimator`, `has_ar`,
#'   `outcome_content_terms` (names of exposure terms whose content is the
#'   outcome), `covariate_content_terms`, `network_timing`, `network_exogeneity`, and `b2_declared`.
#' @return a `netis_assumptions` object: a list of entries plus the resolved
#'   network declaration.
#' @noRd
.build_assumption_ledger <- function(ctx) {
  rows <- list()
  has_outcome_exposure <- length(ctx$outcome_content_terms) > 0L

  # --- A: standard model assumptions -----------------------------------------
  rows <- c(rows, list(.assumption(
    "A1",
    "The conditional mean is linear in the specified terms and the lag structure is deep enough that no omitted earlier state still predicts the outcome.",
    "all coefficients", "maintained", "not_applicable"
  )))

  rows <- c(rows, list(.assumption(
    "A2",
    "Sequential exogeneity: the current innovation is mean-independent of the outcome, exposure and covariate history -- E[e_t | F_{t-1}] = 0. This is an assumption about the innovations; no estimator manufactures it.",
    # ALWAYS UNADDRESSED HERE, and that is the substantive point rather than a
    # gap. Difference and system GMM estimators operationalize this assumption
    # as moment conditions; none of the three estimators in this package does
    # anything of the kind: ordinary least squares, a mixed model, and a
    # simultaneous autoregression all assume it and none tests it.
    "all coefficients", "maintained", "unaddressed"
  )))

  rows <- c(rows, list(.assumption(
    "A3",
    "The innovations are independent across actors given the model -- no common group-wave shock, no residual network dependence beyond what the exposure term already models.",
    "the reported standard errors", "maintained", "unaddressed"
  )))

  rows <- c(rows, list(.assumption(
    "A4",
    "No unmodeled time-varying process is related both to network position or exposure and to the outcome. This is a design question, not a testable one.",
    "the exposure coefficient", "maintained", "unaddressed",
    "not_testable", "no statistic computed from a fit can distinguish an omitted time-varying confounder from a genuine effect"
  )))

  # --- B: persistent heterogeneity -------------------------------------------
  if (isTRUE(ctx$has_ar)) {
    rows <- c(rows, list(.assumption(
      "B",
      .assumption_b_statement(ctx$estimator),
      "the lagged-outcome and exposure coefficients",
      "maintained",
      # None of the three estimators here carries a device for persistent actor
      # heterogeneity that would make this "addressed". `mixed` fits a random
      # intercept per network and no actor effect, so the heterogeneity is in its
      # error term as it is in the others'.
      "unaddressed",
      "none"
    )))
  }

  # B2 is stated against what is fitted: one transition, with the lagged
  # outcome (where ar() is in the formula) and the covariates as the only
  # conditioning terms, and no estimator that models or removes persistent
  # actor heterogeneity. It is reported where an exposure's content is the
  # outcome.
  if (has_outcome_exposure) {
    rows <- c(rows, list(.assumption(
      "B2",
      "Conditional on the terms in the fitted model -- the lagged outcome where ar() is in the formula, and the covariates -- the persistent actor heterogeneity left in the error term is uncorrelated with network exposure. No estimator in this package models or removes that heterogeneity, so the exposure coefficient rests on this assumption.",
      "the exposure coefficient",
      if (isTRUE(ctx$b2_declared)) "declared" else "default",
      "unaddressed"
    )))
  }

  # --- C: the tie assumptions ------------------------------------------------
  if (has_outcome_exposure) {
    rows <- c(rows, list(.assumption(
      "C",
      .assumption_c_statement(ctx$network_timing, ctx$network_exogeneity),
      "the exposure coefficient",
      if (identical(ctx$network_exogeneity, "unknown")) "default" else "declared",
      "unaddressed", "none"
    )))
  }

  # --- The one-transition regime ---------------------------------------------
  if (isTRUE(ctx$has_ar)) {
    rows <- c(rows, list(.assumption(
      "T2",
      "With two waves the lagged-outcome term adjusts for the baseline outcome and estimates persistence across the single observed transition. It should not be read as establishing a stable autoregressive process over time: across one transition, persistence and the initial-condition relation with persistent actor heterogeneity are not separately identified, so the coefficient is a composite of both. The exposure coefficient does not inherit this -- it is identified by cross-sectional variation in exposure among actors at the same baseline -- but it does rest on B2.",
      "the lagged-outcome coefficient", "maintained", "not_applicable",
      "not_testable",
      "one transition supplies no repeated within-actor information with which to separate persistence from stable heterogeneity",
      headline = "persistence across ONE transition; not a stable autoregressive process"
    )))
  }

  structure(
    list(
      entries = rows,
      network = list(timing = ctx$network_timing,
                     exogeneity = ctx$network_exogeneity,
                     timing_source = "measured from the resolved conduit",
                     exogeneity_source = if (identical(ctx$network_exogeneity, "unknown")) "not declared" else "declared")
    ),
    class = "netis_assumptions"
  )
}

#' What the fitted estimator assumes about persistent actor heterogeneity
#'
#' One statement per estimator this package has. None of the three carries a
#' device that ADDRESSES this assumption, so all three statements say what is
#' assumed, not what is handled -- which is the honest reading and, for
#' `mixed`, the one most easily misread.
#'
#' @noRd
.assumption_b_statement <- function(estimator) {
  switch(
    estimator %||% "mixed",
    mixed = "The random intercept is per network, and no actor effect is estimated, so persistent actor heterogeneity sits in the error term and is assumed unrelated to the regressors. With a lagged outcome in the model this is generally false, since the lagged outcome contains the actor effect -- a random network intercept does not repair that.",
    pooled = "No actor effect is estimated, so persistent actor heterogeneity sits in the error term and is assumed unrelated to the regressors.",
    lnam = "No actor effect is estimated. The simultaneous autoregression models dependence through the network among the outcomes themselves, not persistent heterogeneity between actors, so any stable actor-level difference sits in the error term and is assumed unrelated to the regressors.",
    "Persistent actor heterogeneity is assumed unrelated to the regressors."
  )
}

#' The tie assumption, resolved against the measured timing and declared exogeneity
#' @noRd
.assumption_c_statement <- function(timing, exogeneity) {
  head <- switch(
    timing,
    fixed = "The network does not change across the estimating waves.",
    time_varying = "The network changes across the estimating waves.",
    "Whether the network changes across the estimating waves could not be determined from this fit's data."
  )
  tail <- switch(
    exogeneity %||% "unknown",
    exogenous = "It is declared exogenous: tie formation is unrelated to the outcome's actor effects and innovations at every wave.",
    sequentially_exogenous = "It is declared sequentially exogenous: ties may respond to past outcomes and past ties, but not to information about the current innovation. Time variation is not itself a problem; changing on unobserved determinants of the next outcome is.",
    endogeneity_suspected = "Tie formation is declared possibly endogenous to the outcome process. No estimator here addresses that, and robust standard errors do not repair it -- the exposure coefficient may be substantially biased.",
    "How the network came about has not been declared, so rnetis assumes nothing about it and reports the exposure coefficient as resting on an undeclared condition. Declare it if you can defend one."
  )
  paste(head, tail)
}

#' Print or format an assumption ledger
#'
#' What the fit is assuming, resolved against the specification: each row's
#' identifier, its statement, where it comes from, what handles it, and whether any
#' statistic could bear on it.
#'
#' # Why three fields rather than a status
#'
#' Where an assumption comes from, what handles it, and whether any statistic
#' could bear on it are reported separately: `source`, `handling` and
#' `diagnostic_evidence`.
#'
#' @param x a `netis_assumptions` object, carried on a fitted
#'   [fit_influence()] model.
#' @param ... unused; present for S3 consistency with the generics.
#' @return `format()` a character vector, one element per line. `print()`
#'   returns `x` invisibly and is called for its output.
#' @name netis-assumptions-methods
#' @noRd
NULL

#' Say who is making the exogeneity claim, in words that cannot be misread
#'
#' `network_exogeneity` is a USER DECLARATION. rnetis cannot test it and does
#' not try, so the printed form has to attribute it rather than state it. The
#' failure to avoid is a line like "Network exogeneity: satisfied", which reads
#' as though something had been checked.
#'
#' @noRd
.format_exogeneity_clause <- function(exogeneity) {
  if (is.null(exogeneity) || identical(exogeneity, "unknown")) {
    return("NOT DECLARED (rnetis assumes nothing about it)")
  }
  sprintf("DECLARED BY USER as %s (a claim about the design; rnetis did not test it)",
          exogeneity)
}

#' @exportS3Method
format.netis_assumptions <- function(x, ...) {
  lines <- "Assumptions carrying this fit:"
  net <- x$network
  lines <- c(lines, sprintf(
    "  Network: timing %s (%s); exogeneity %s.",
    net$timing, net$timing_source, .format_exogeneity_clause(net$exogeneity)
  ))
  for (a in x$entries) {
    lines <- c(lines, sprintf("  [%s] %s", a$id, a$statement))
    tags <- c(sprintf("source: %s", a$source),
              sprintf("handling: %s", gsub("_", " ", a$handling, fixed = TRUE)))
    if (!identical(a$diagnostic_evidence, "none")) {
      tags <- c(tags, sprintf("evidence: %s",
                              gsub("_", " ", a$diagnostic_evidence, fixed = TRUE)))
    }
    lines <- c(lines, sprintf("      (%s; bears on %s)",
                              paste(tags, collapse = "; "), a$scope))
    if (!is.null(a$evidence_detail)) {
      lines <- c(lines, sprintf("      %s", a$evidence_detail))
    }
  }
  lines
}

#' The compact ledger for `summary()`
#'
#' One line per row: identifier, scope, and the three fields. The full
#' statements are reachable on the fit; printing every one of them on every
#' summary would bury the coefficient table under a page of prose that does not
#' change between fits.
#'
#' Rows are ordered so the ones a reader must act on come first: anything
#' declared (because a declaration is a claim the reader is making), then the
#' rows no statistic can test, then the rest.
#'
#' @noRd
.format_assumptions_compact <- function(x) {
  net <- x$network
  lines <- c(
    "Assumptions:",
    sprintf("  Network: %s timing (measured); exogeneity %s.",
            net$timing, .format_exogeneity_clause(net$exogeneity))
  )
  rank <- vapply(x$entries, function(a) {
    if (identical(a$source, "declared")) 1L
    else if (identical(a$diagnostic_evidence, "not_testable")) 2L
    else 3L
  }, integer(1))
  for (a in x$entries[order(rank)]) {
    flag <- if (identical(a$diagnostic_evidence, "not_testable")) " [not testable]" else ""
    lines <- c(lines, sprintf(
      "  [%s] %s -- %s%s",
      a$id, a$scope,
      # The headline replaces the generic handling word where one is set: for
      # those entries the handling word is true and useless, and the compact
      # block is the only place many users will read.
      a$headline %||% switch(
        a$handling,
        addressed_by_estimator = "addressed by the estimator",
        unaddressed = if (identical(a$source, "declared")) "declared, not addressed" else "assumed, not addressed",
        not_applicable = "structural"),
      flag
    ))
  }
  c(lines, "  Full statements: netis_assumptions(fit).")
}

#' @exportS3Method
print.netis_assumptions <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' What a fitted Influence model is assuming
#'
#' Returns the fit's assumption ledger: what the model takes for granted,
#' resolved against the specification that was fitted rather than printed as
#' boilerplate. `summary()` prints a compact form; this returns the full
#' statements, and the object it returns has its own `print()` method.
#'
#' The ledger's taxonomy -- the rows `A1` to `A4`, `B`, `B2`, `C` and `T2`, and the
#' `source`, `handling` and `diagnostic_evidence` fields -- is this package's own
#' construction, not a published classification. Each row states a standard
#' assumption of the models fitted.
#'
#' @section Three fields, not one status:
#' Where an assumption comes from (`source`: maintained by the model, rnetis's
#' default, or declared by you), what in the fit handles it (`handling`), and
#' whether any statistic could bear on it (`diagnostic_evidence`) are reported
#' separately. `diagnostic_evidence` is `"not_testable"`, with the reason, for
#' a row no statistic computed from a fit can bear on, and `"none"` otherwise:
#' no diagnostic feeds the ledger, so nothing here reports an assumption as
#' satisfied or as violated.
#'
#' @section What "unaddressed" means here:
#' Most rows read `unaddressed`, and that is a statement about this package's
#' scope rather than an omission. None of its three estimators carries a device
#' for sequential exogeneity or for persistent actor heterogeneity -- the
#' dynamic-panel estimators that do need more than the one transition a fit analyses.
#' An assumption no available estimator addresses is more important to report,
#' not less.
#'
#' @param fit a fitted model from [fit_influence()].
#' @return an object of class `netis_assumptions`: a list of `entries`, one per row,
#'   each with `id`, `statement`, `scope`, `source` (`"maintained"`, `"default"` or
#'   `"declared"`), `handling` (`"addressed_by_estimator"`, `"unaddressed"` or
#'   `"not_applicable"`), `diagnostic_evidence`, `evidence_detail` and `headline`;
#'   and `network`, with `timing` (`"fixed"`, `"time_varying"` or `"undetermined"`,
#'   measured from the data), `exogeneity` (the declaration), `timing_source` and
#'   `exogeneity_source`. Rows `B` and `T2` appear with an [ar()] term, and `B2` and
#'   `C` with an exposure to the outcome.
#' @seealso [fit_influence()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = rep(letters[1:5], 2), wave = rep(1:2, each = 5),
#'   y = c(1, 2, 3, 2, 1, 2.4, 2.8, 4.3, 3.1, 1.6), x = rep(c(1, 0, 1, 0, 1), 2)
#' )
#' ties <- data.frame(
#'   group = 1, source = c("a", "b", "c", "d"), target = c("b", "c", "d", "e"),
#'   wave = 1, tie = 1
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
#' netis_assumptions(fit)
#' @export
netis_assumptions <- function(fit) {
  if (!inherits(fit, "netis_influence")) {
    stop(
      "netis_assumptions() takes a fitted Influence model from fit_influence().",
      call. = FALSE
    )
  }
  if (is.null(fit$assumptions)) {
    stop(
      "This fit carries no assumption ledger.",
      call. = FALSE
    ) # nocov
  }
  fit$assumptions
}
