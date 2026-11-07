# Design diagnostics for fit_selection().
#
# Structured the way the rest of the package already splits this work: the
# diagnostic function *reports* findings at every level, including ERROR, so
# that a user can inspect a specification before committing to a fit
# (diagnose_data() and diagnose_bi_identifiability() do the same); a fit
# attempt then refuses outright via .assert_selection_design_fittable(), which
# turns any ERROR-level finding into a real error. Reporting and refusing are
# different jobs and the same check should not be written twice to do both.

# Below this many actors, an AME fit on a single group is estimating additive
# sender and receiver effects for every actor from n(n-1) directed dyads, and
# the prior does much of the work. This is a package convention chosen to catch
# the obviously under-powered case, not a statistical result, which is why the
# finding reports the actual actor and dyad counts and leaves the judgment to
# the user.
#' @noRd
.SELECTION_SMALL_N_ACTORS <- 8L

#' Run design diagnostics on a parsed Selection specification
#'
#' @param spec a parsed Selection formula (see `.parse_selection_formula()`).
#' @param data the [netis_data()] object the model would be fit to.
#' @param waves the waves the fit reads. The roster and group-size checks look
#'   only at these, so a change at a wave the fit leaves out is not reported as
#'   a fault of the fit. `NULL` means every wave.
#' @return a `netis_diagnostics` object; see [netis-diagnostics].
#' @noRd
.diagnose_selection_design <- function(spec, data, waves = NULL) {
  schema <- netis_schema(data)
  findings <- c(
    .check_selection_variable_levels(spec, schema),
    .check_selection_variable_types(spec, data, schema),
    .check_selection_contemporaneous(spec),
    .check_selection_wave_depth(spec, schema),
    .check_selection_static_outcome(spec, schema),
    .check_selection_lagged_static(spec, schema),
    .check_selection_fixed_roster(data, schema, waves),
    .check_selection_group_identified(spec, data, schema),
    .check_selection_group_sizes(data, schema, waves)
  )
  .combine_diagnostics(findings)
}

#' Refuse a predictor or outcome that is not a number
#'
#' The design arrays hold numbers. A factor reaching them would enter as its
#' integer codes, an order nobody chose, and a character column has no numeric
#' meaning at all. A logical column is a 0/1 indicator and is fine.
#'
#' @noRd
.check_selection_variable_types <- function(spec, data, schema) {
  is_number <- function(v) is.numeric(v) || is.logical(v)
  describe <- function(v) if (is.factor(v)) "a factor" else sprintf("of class '%s'", class(v)[1])
  nodes <- netis_nodes(data)
  ties <- netis_ties(data)
  findings <- list()
  add <- function(label, variable, v, code) {
    findings[[length(findings) + 1L]] <<- .finding(
      "ERROR", "design", code,
      sprintf(
        "%s needs a numeric variable, but '%s' is %s. A design array holds numbers, and %s. Encode it as one or more numeric indicator columns, so the reference category is your choice and is visible in the output.",
        label, variable, describe(v),
        if (is.factor(v)) "a factor would enter as its integer codes, an order nobody chose" else "text has no numeric meaning"
      ),
      details = list(variable = variable)
    )
  }
  for (field in c("senders", "receivers")) {
    fname <- .SELECTION_ROLE_LEVELS[[field]]$fname
    for (term in spec[[field]]) {
      v <- nodes[[term$variable]]
      if (!is.null(v) && !is_number(v)) {
        add(sprintf("%s(%s)", fname, term$variable), term$variable, v, "selection_predictor_not_numeric")
      }
    }
  }
  for (term in spec$dyads) {
    v <- ties[[term$variable]]
    if (!is.null(v) && !is_number(v)) {
      add(sprintf("dyad(%s)", term$variable), term$variable, v, "selection_predictor_not_numeric")
    }
  }
  v <- ties[[spec$outcome]]
  if (!is.null(v) && !is_number(v)) {
    add("The Selection outcome", spec$outcome, v, "selection_outcome_not_numeric")
  }
  findings
}

#' Warn on a covariate measured at the same wave as the tie outcome
#'
#' The Selection analogue of `.diagnose_influence_design()`'s contemporaneous
#' exposure warning, and deliberately the same treatment: permitted, fit as an
#' ordinary predictor, but flagged because the specification does not establish
#' that the covariate preceded the tie. An actor attribute measured in the same
#' wave as the relation may as easily be a consequence of that relation as a
#' cause of it.
#'
#' @noRd
.check_selection_contemporaneous <- function(spec) {
  roles <- list(
    senders = "sender", receivers = "receiver",
    dyads = "dyad", groups = "group"
  )
  findings <- list()
  for (field in names(roles)) {
    fname <- roles[[field]]
    for (term in spec[[field]]) {
      if (term$lag != 0L) {
        next
      }
      findings <- c(findings, list(.finding(
        "WARNING", "design", "contemporaneous_selection_covariate",
        sprintf(
          "%s(%s) is measured at the same wave as the tie outcome, not a prior one. It is fit as an ordinary predictor, but this specification does not establish that %s preceded the tie -- an attribute measured alongside a relation may be a consequence of it as easily as a cause. Consider %s(%s, lag = 1) if the prior wave's value is a more defensible predictor.",
          fname, term$variable, term$variable, fname, term$variable
        ),
        details = list(term = term$name, variable = term$variable, role = fname)
      )))
    }
  }
  findings
}

#' Refuse a specification whose lags reach past the observed waves
#'
#' Two distinct failures share this check. A `prior_tie()` term on single-wave
#' data has nothing to lag at all; a lag deeper than the data reaches is the
#' same problem one step further out.
#' Both are ERROR rather than WARNING because there is no defensible fit to
#' return.
#'
#' The data-level wave-adequacy check (`.check_wave_adequacy()`) does
#' not cover this: it emits a NOTE, and it sees only `netis_data`, so it cannot
#' know whether any term actually requested a lag. So this check is not
#' redundant with it, despite covering adjacent ground.
#'
#' @noRd
.check_selection_wave_depth <- function(spec, schema) {
  n_waves <- length(schema$waves)
  lagged_terms <- Filter(Negate(is.null), list(spec$prior_tie, spec$prior_reciprocity))
  covariate_terms <- c(spec$senders, spec$receivers, spec$dyads, spec$groups)
  all_terms <- c(lagged_terms, covariate_terms)

  findings <- list()
  for (term in all_terms) {
    if (term$lag == 0L) {
      next
    }
    # A lag of k needs k + 1 waves: the focal wave the outcome is measured at,
    # plus k earlier ones to reach back through.
    if (n_waves >= term$lag + 1L) {
      next
    }
    label <- .selection_term_label(class(term))
    findings <- c(findings, list(.finding(
      "ERROR", "design", "insufficient_waves_for_selection_lag",
      sprintf(
        "%s(lag = %d) needs at least %d waves (the outcome wave plus %d earlier), but this data has %d: %s. %s",
        label, term$lag, term$lag + 1L, term$lag, n_waves,
        if (n_waves == 0L) "none" else paste(schema$waves, collapse = ", "),
        if (label %in% c("prior_tie", "prior_reciprocity")) {
          "Tie persistence and delayed reciprocity are transitions between waves, so they cannot be estimated from a single observation of the network."
        } else {
          sprintf("Use %s(%s) at the focal wave, or supply data covering the earlier wave.", label, term$variable)
        }
      ),
      details = list(term = term$name, lag = term$lag, n_waves = n_waves)
    )))
  }
  findings
}

#' Refuse a lagged-relation term on an outcome that never changes
#'
#' `netis_data()` stores a tie variable whose values are identical across waves
#' as **static**: one row per dyad, no wave dimension, reused at every wave.
#' That is the right storage decision, and it has a consequence specific to
#' this model family. If the outcome relation is static then `y[i,j]` at the
#' prior wave *is* `y[i,j]` now, by construction rather than by estimate. The
#' persistence coefficient is not merely large, it is fitting a predictor
#' identical to the response.
#'
#' The Influence side treats a `lagged()` term on a static variable as a NOTE
#' ("lagging it has no effect"). Here the consequence is categorically worse
#' than no effect, so this is an ERROR: there is no defensible
#' estimate to return.
#'
#' Constant tie values silently produce identical focal and lagged matrices.
#' A test pins the refusal, since the failure is invisible in the fitted
#' output -- it looks like extremely strong persistence.
#'
#' @noRd
.check_selection_static_outcome <- function(spec, schema) {
  lagged_terms <- Filter(Negate(is.null), list(spec$prior_tie, spec$prior_reciprocity))
  if (length(lagged_terms) == 0L) {
    return(list())
  }
  # Staticness is only diagnosable, and only meaningful, with two or more
  # waves: a single-wave relation is trivially "unchanging", and the real
  # problem there is wave depth, already reported by its own check. Reporting
  # both would name two causes for one fault.
  if (length(schema$waves) < 2L) {
    return(list())
  }
  if (!spec$outcome %in% schema$tie_vars$static) {
    return(list())
  }
  lapply(lagged_terms, function(term) {
    label <- .selection_term_label(class(term))
    .finding(
      "ERROR", "design", "static_outcome_relation",
      sprintf(
        "%s() cannot be estimated because the relation '%s' does not change across waves in this data -- it is stored as a static tie variable, so its value at the prior wave is identical to its value now. The lagged predictor would be the response itself. Either supply tie data that varies across waves, or drop %s() and model the relation cross-sectionally.",
        label, spec$outcome, label
      ),
      details = list(term = term$name, outcome = spec$outcome)
    )
  })
}

#' The schema level each Selection role reads its variable from
#'
#' Unlike Influence's `lagged()`, which must resolve a variable's level from
#' the schema and can find it ambiguous, a Selection role *declares* the level:
#' a sender or receiver effect is a property of an actor, a dyadic effect of a
#' pair, a group effect of a group. That makes a mismatch checkable rather than
#' merely surprising.
#'
#' @noRd
.SELECTION_ROLE_LEVELS <- list(
  senders = list(fname = "sender", level = "node", field = "node_vars"),
  receivers = list(fname = "receiver", level = "node", field = "node_vars"),
  dyads = list(fname = "dyad", level = "tie", field = "tie_vars"),
  groups = list(fname = "group", level = "group", field = "group_vars")
)

#' Refuse a role term naming a variable that does not exist at its level
#'
#' `sender(x)` where `x` is a tie variable is not a subtle modeling choice, it
#' is a specification the data cannot supply. Caught here, where the role names
#' the expected level and the message can say which level the variable actually
#' lives at, rather than surfacing later as a missing column.
#'
#' @noRd
.check_selection_variable_levels <- function(spec, schema) {
  available <- function(field) {
    v <- schema[[field]]
    c(v$static, v$dynamic)
  }
  all_levels <- list(
    node = available("node_vars"),
    tie = available("tie_vars"),
    group = available("group_vars")
  )

  findings <- list()
  for (field in names(.SELECTION_ROLE_LEVELS)) {
    role <- .SELECTION_ROLE_LEVELS[[field]]
    expected <- available(role$field)
    for (term in spec[[field]]) {
      if (term$variable %in% expected) {
        next
      }
      found_at <- names(all_levels)[vapply(all_levels, function(v) term$variable %in% v, logical(1))]
      findings <- c(findings, list(.finding(
        "ERROR", "design", "selection_variable_wrong_level",
        sprintf(
          "%s(%s) needs '%s' to be a %s variable, but %s.",
          role$fname, term$variable, term$variable, role$level,
          if (length(found_at) == 0L) {
            sprintf(
              "no variable of that name exists in this data. Available %s variables: %s",
              role$level,
              if (length(expected) == 0L) "none" else paste(expected, collapse = ", ")
            )
          } else {
            sprintf(
              "it is a %s variable here. Use %s instead",
              paste(found_at, collapse = "/"),
              switch(
                found_at[[1]],
                node = "sender()/receiver()",
                tie = "dyad()",
                group = "group()",
                "the matching role term"
              )
            )
          }
        ),
        details = list(term = term$name, variable = term$variable,
                       expected_level = role$level, found_level = found_at)
      )))
    }
  }
  findings
}

#' Note a lag applied to a variable that does not vary across waves
#'
#' The covariate counterpart to `.check_selection_static_outcome()`. A static
#' variable holds the same value at every wave, so lagging it returns exactly
#' what the unlagged term would -- often intentional (a baseline covariate
#' written with an explicit lag for clarity), sometimes a sign that the wrong
#' variable was named.
#'
#' NOTE rather than ERROR, matching `.diagnose_influence_lagged()`'s treatment
#' of the same situation. The outcome-relation case is an ERROR because there
#' the static predictor *is* the response; here it is merely redundant, and
#' the two must not be given the same severity just because they share a cause.
#'
#' @noRd
.check_selection_lagged_static <- function(spec, schema) {
  findings <- list()
  for (field in names(.SELECTION_ROLE_LEVELS)) {
    role <- .SELECTION_ROLE_LEVELS[[field]]
    static_vars <- schema[[role$field]]$static
    for (term in spec[[field]]) {
      if (term$lag == 0L || !term$variable %in% static_vars) {
        next
      }
      findings <- c(findings, list(.finding(
        "NOTE", "design", "selection_lagged_static_variable",
        sprintf(
          "%s(%s, lag = %d) lags a variable that does not vary across waves in this data, so the lagged value is the same as the unlagged one and the lag has no effect. This is often intentional (a baseline covariate written with an explicit lag for clarity), and is surfaced in case it is not.",
          role$fname, term$variable, term$lag
        ),
        details = list(term = term$name, variable = term$variable, level = role$level)
      )))
    }
  }
  findings
}

#' Refuse a group whose actor roster changes across waves
#'
#' The additive sender and receiver effects are per actor, and a lagged term
#' needs the same actors at the waves it connects. Real groups have turnover, so
#' this will be met in practice. rnetis refuses it: quietly unioning rosters and
#' masking the absent would produce numbers whose meaning nobody could state.
#' The two resolutions offered are ones the user can apply themselves; rnetis
#' never drops actors or waves on their behalf.
#'
#' **Scope.** Only the waves the fit reads are compared. **Detection.** The
#' roster is stored per group, not per (group, wave), so a change has to be
#' detected rather than read off. When node data is dynamic, `netis_nodes()` reconstructs a wave
#' column and per-wave membership is exactly which actors have rows at each
#' wave. When node data is fully static there is no wave column at all, which
#' means every actor is declared present throughout -- the roster is then fixed
#' by construction and passes without a data scan. That covers the real cases
#' without requiring a schema addition.
#'
#' @noRd
.check_selection_fixed_roster <- function(data, schema, waves = NULL) {
  nodes <- netis_nodes(data)
  wave_col <- schema$wave_col
  read <- if (length(waves) > 0L) waves else schema$waves

  # No wave dimension in the node data: membership is not wave-specific, so
  # the roster cannot vary. This is a declaration, not an absence of evidence.
  if (is.null(wave_col) || !wave_col %in% names(nodes) || length(read) < 2L) {
    return(list())
  }

  groups <- unique(as.character(nodes[[schema$group_col]]))
  findings <- list()
  for (g in groups) {
    in_group <- as.character(nodes[[schema$group_col]]) == g
    rosters <- lapply(read, function(w) {
      sort(unique(as.character(nodes[[schema$node_col]][in_group & nodes[[wave_col]] == w])))
    })
    names(rosters) <- as.character(read)

    # Waves where this group is entirely absent are a different condition from
    # turnover within an observed group, and are not reported as roster change.
    observed <- vapply(rosters, function(r) length(r) > 0L, logical(1))
    rosters <- rosters[observed]
    if (length(rosters) < 2L) {
      next
    }

    reference <- rosters[[1]]
    changed <- !vapply(rosters, function(r) identical(r, reference), logical(1))
    if (!any(changed)) {
      next
    }

    everyone <- sort(unique(unlist(rosters, use.names = FALSE)))
    inconsistent <- everyone[!vapply(everyone, function(a) {
      all(vapply(rosters, function(r) a %in% r, logical(1)))
    }, logical(1))]
    stable <- setdiff(everyone, inconsistent)

    findings <- c(findings, list(.finding(
      "ERROR", "design", "selection_roster_changes",
      sprintf(
        "Group '%s' does not have the same actors at every wave this fit reads, which a Selection model over waves requires: sender and receiver effects are estimated per actor, so a wave with a different roster has no consistent set of effects to estimate. Actor(s) present at some of these waves but not others: %s. Waves read for this group: %s. Two resolutions, both of which you apply rather than rnetis: restrict the data to the %d actor(s) present at every one of these waves (%s), or name a transition (waves = c(from, to)) over which the roster is constant. rnetis does not drop actors or waves for you.",
        g,
        paste(inconsistent, collapse = ", "),
        paste(names(rosters), collapse = ", "),
        length(stable),
        if (length(stable) == 0L) "none" else paste(stable, collapse = ", ")
      ),
      details = list(group = g, inconsistent = inconsistent, stable = stable,
                     waves = names(rosters))
    )))
  }
  findings
}

#' Refuse a group-level predictor the fit cannot identify
#'
#' The precedent (`.assert_network_stat_identified()`) refuses a
#' group-level Influence predictor with no variation across the resolved data,
#' because a constant is perfectly collinear with the intercept rather than
#' merely weak. The same reasoning applies here, but the architecture makes the
#' condition sharper and it is worth stating exactly rather than reusing the
#' Influence wording.
#'
#' Multi-group Selection data is fitted as **independent per-group fits**, so
#' between-group variation in a `group()` predictor is not available to any
#' single fit -- each fit sees one group.
#'
#' POOLING DOES NOT RESCUE IT, and this is where two-stage pooling differs from
#' a model that estimates every group in one chain, where a between-group
#' contrast is genuinely available and a `group()` term can be identified. Two-stage
#' pooling fits each group SEPARATELY FIRST: a predictor constant within a
#' group is perfectly collinear with that group's intercept in every panel, so
#' no panel produces an estimate, and there is nothing for the pooling stage to
#' combine. The refusal therefore holds whatever `pool_beta` is set to.
#'
#' Variation across waves does not help either: a fit reads exactly one
#' outcome wave, at which the group variable has one value for the whole
#' group, so a time-varying group covariate is still a constant column in every
#' fitted design.
#'
#' So every `group()` term is refused, on both routes. On the hierarchical
#' route a group-level predictor enters as a moderator of a random intercept.
#'
#' @noRd
.check_selection_group_identified <- function(spec, data, schema) {
  if (length(spec$groups) == 0L) {
    return(list())
  }
  group_data <- netis_groups(data)
  findings <- list()

  for (term in spec$groups) {
    if (is.null(group_data) || !term$variable %in% names(group_data)) {
      next  # a missing variable is reported by .check_selection_variable_levels()
    }
    keys <- as.character(group_data[[schema$group_col]])
    n_groups <- length(unique(keys))

    # Refused on every route. On the per-group route every group is FITTED
    # separately, so no fit sees more than one value of a group-constant
    # predictor. The hierarchical route does see every group, and a
    # group-level predictor enters it as a MODERATOR of a random intercept
    # (selection_pooling(random = "intercept", moderators = ...)), where its
    # information is explicitly from the number of groups and its prior is on
    # a between-group contrast. A group() main-effect term is not mapped onto
    # that, so it is refused there too, with the route named.
    findings <- c(findings, list(.finding(
      "ERROR", "design", "group_predictor_not_identified",
      sprintf(
        "group(%s) cannot be identified as a term: each group's fit reads one outcome wave, where '%s' takes one value for the whole group, so within any one group it is perfectly collinear with that group's intercept. %s pool_beta does not rescue it: it combines coefficients AFTER each group is fitted. To estimate a group-level predictor across groups, drop the group() term and supply it as a moderator of a random intercept: pooling = selection_pooling(random = \"intercept\", moderators = <one row per group>).",
        term$variable, term$variable,
        if (n_groups > 1L) {
          sprintf("The variable does differ between the %d groups, but that variation is between fits rather than inside any one of them.", n_groups)
        } else {
          "There is only one group in this data."
        }
      ),
      details = list(term = term$name, variable = term$variable, n_groups = n_groups)
    )))
  }
  findings
}

#' Report group sizes that leave an AME fit thinly identified
#'
#' Reports the counts rather than pronouncing on them. A directed group of `n`
#' actors supplies `n(n - 1)` dyads to estimate `2n` additive effects plus the
#' regression terms, so the ratio degrades quickly at small `n`.
#'
#' @noRd
.check_selection_group_sizes <- function(data, schema, waves = NULL) {
  nodes <- netis_nodes(data)
  if (length(waves) > 0L && !is.null(schema$wave_col) && schema$wave_col %in% names(nodes)) {
    nodes <- nodes[as.character(nodes[[schema$wave_col]]) %in% as.character(waves), , drop = FALSE]
  }
  # Distinct actors, not node rows: netis_nodes() returns reconstructed long
  # form, so a group observed over w waves has w rows per actor and a row
  # count would overstate the roster by a factor of w.
  roster <- unique(data.frame(
    group = as.character(nodes[[schema$group_col]]),
    node = as.character(nodes[[schema$node_col]]),
    stringsAsFactors = FALSE
  ))
  sizes <- table(roster$group)
  small <- sizes[sizes < .SELECTION_SMALL_N_ACTORS]
  if (length(small) == 0L) {
    return(list())
  }
  # n(n - 1) whatever the data permits: an AME model has no self-dyad.
  detail <- paste(
    sprintf("%s (%d actors, %d dyads)", names(small), as.integer(small),
            as.integer(small) * (as.integer(small) - 1L)),
    collapse = "; "
  )
  list(.finding(
    "WARNING", "design", "small_group_for_selection",
    sprintf(
      "%d group(s) have fewer than %d actors: %s. An AME fit estimates a sender and a receiver effect for every actor, so at this size the dyads available per estimated effect are few and the prior carries a large share of the result. The counts are reported so you can judge whether the estimates are informative; rnetis does not refuse the fit.",
      length(small), .SELECTION_SMALL_N_ACTORS, detail
    ),
    details = list(groups = names(small), actors = as.integer(small))
  ))
}

#' Refuse to fit a specification carrying an ERROR-level design finding
#'
#' The seam between reporting and refusing. `fit_selection()` calls this so
#' that every ERROR-level design check is enforced exactly once, in one place,
#' rather than each check being written twice -- once to report and once to
#' stop.
#'
#' @param diagnostics a `netis_diagnostics` collection.
#' @return invisibly `TRUE`, or an error naming every blocking finding.
#' @noRd
.assert_selection_design_fittable <- function(diagnostics) {
  blocking <- filter_diagnostics(diagnostics, level = "ERROR")
  if (length(blocking) == 0L) {
    return(invisible(TRUE))
  }
  stop(
    paste0(
      "This Selection specification cannot be estimated as written:\n",
      paste0("  - ", vapply(blocking, function(f) f$message, character(1)), collapse = "\n")
    ),
    call. = FALSE
  )
}
