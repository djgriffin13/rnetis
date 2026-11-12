#' Fit an Influence model
#'
#' Fits the canonical Influence Model: a node characteristic at a later
#' wave, predicted by its own prior state ([ar()]) and network exposure to
#' alter characteristics ([exposure()]), plus ordinary covariates. A single
#' network is estimated by ordinary least squares. Multiple independent
#' networks are estimated by a mixed-effects backend ([lme4::lmer()]) when
#' the formula includes a random-effects term, `(1 | group)` -- the routing
#' is a property of what you write in the formula, not inferred from how
#' many groups the data happens to have.
#'
#' # Formula
#'
#' \preformatted{
#' outcome ~ ar() + lagged(x) + exposure(content, via = conduit) + covariates + (1 | group)
#' }
#'
#' [ar()] takes no variable argument, since the autoregressive variable is
#' always the outcome (read from the formula's left-hand side). It is
#' optional: a contemporaneous same-outcome specification routed to the
#' simultaneous estimator is a legitimate model with no lagged outcome in it.
#' A formula with [exposure()] but no [ar()] gets a design NOTE, since
#' without a prior-state control the exposure coefficient carries whatever
#' stable node differences also predict alters' values. [lagged()],
#' [exposure()] terms, and ordinary covariates are likewise optional; repeat
#' [exposure()] for multiple pathways. See
#' [ar()], [lagged()], and [exposure()] for their own arguments (lags,
#' mechanism, direction, normalization). [centrality()], [network_stat()] and
#' [latent()] add network-derived predictors in the same way.
#'
#' # Wave timing of ordinary predictors
#'
#' A bare covariate (`y ~ ar() + stress`) is measured at the **focal**
#' wave -- the same wave as the outcome. [lagged()] is how a predictor at
#' the prior wave is requested (`y ~ ar() + lagged(stress)`); both are
#' expressible and neither is implied by the other.
#'
#' # Moderation
#'
#' A plain covariate, an [exposure()] term, or an [ar()] term may be
#' moderated by exactly one plain covariate, written `*`: `exposure(x, via =
#' w) * z` or `ar() * z`. This is hierarchical, the same rule `lm()` itself
#' follows -- `x * z` expands to `x + z + x:z`, and a bare `x:z` missing a
#' main effect is refused by name rather than silently fit. The moderator
#' must be numeric or logical (0/1); a factor moderator is refused rather
#' than dummy-coded or silently dropped. The `pooled` and `mixed` routes fit all
#' three moderands, and the simultaneous (`lnam`) route refuses a moderated
#' term (see Contemporaneous exposure).
#'
#' A moderated term's effect size is a function of the moderator, not a
#' single number: [influence_effect()] reports, under
#' `$exposures[["ex_..."]]$conditional` (and `$ar` for a moderated [ar()]),
#' the conditional estimate and a standard error that includes the
#' covariance term the two-term shortcut omits, at the moderator's 0, 10th-
#' and 90th-percentile values, plus a Johnson-Neyman crossing reported as
#' *where the interval excludes/includes zero* rather than a significance
#' verdict. `summary()` states the conditionality: the coefficient printed for
#' the moderand is its effect **at the moderator's zero**, and for a moderated
#' [ar()] that it is not an unconditional stability parameter.
#'
#' Moderation and a random exposure slope (below) are different claims and
#' compose rather than substitute for each other: a random slope says
#' susceptibility varies *unexplained* across networks, moderation says it
#' varies *as a function of z*. A moderated coefficient inherits every
#' endogeneity caveat the unmoderated one carried (see [netis_assumptions()]);
#' moderation resolves none of them.
#'
#' A moderator (or, for a plain-covariate moderand, the moderand too) may be
#' any ordinary column -- including one produced by [add_centering()] or
#' [add_decomposition()]. There is no separate argument for this: which
#' representation a moderated term uses is entirely a matter of which column
#' name it references (`ar() * z` moderates by the raw scale, `ar() *
#' z_within` by [add_decomposition()]'s within/actor-centered component),
#' since that layer never enters a predictor on its own. `moderation_provenance` (below) is where the fit
#' reports which derived representation, if any, was actually used. The same
#' `exposure()`/`ar()` moderand may not be moderated by more than one
#' moderator at once (e.g. simultaneously by both `z_within` and
#' `z_between`) -- refused by name, because the conditional-effect and
#' Johnson-Neyman machinery below is built for a single moderator; fit each
#' as a separate specification instead.
#'
#' # Multiple networks
#'
#' Add `(1 | g)` to the formula, where `g` is the `netis_data` object's own
#' group column name, to fit a random intercept per network via `lme4`. A
#' `netis_data` object with more than one group and *no* `(1 | g)` term is
#' rejected rather than silently pooled into one OLS fit across unrelated
#' networks. One random-effects term is supported, grouped by the group
#' column. A node-level term, `(1 | node)` or `(1 | g/node)`, is refused: a
#' fit analyses one transition, so each node contributes exactly one outcome
#' row and a node-level variance is not separately identified from the
#' residual variance. A term's left-hand side
#' may include one predictor already present in the fixed-effects formula
#' for a random slope: `(1 + ex_y_w | g)` (correlated random intercept and
#' slope), `(0 + ex_y_w | g)` or `(ex_y_w - 1 | g)` (slope only). The slope
#' must name a column the formula produces; a misspelled one is refused.
#' Arbitrary grouping factors and more than one random-slope predictor per
#' term are not supported. See Moderation above for how this differs from
#' `* z`. A random network intercept on the Influence model is this package's own
#' specification, not a published form of that model; [lme4::lmer()] estimates it.
#'
#' A slope-only group term on multi-group data is a valid model with a
#' substantive assumption: every group shares one intercept, i.e. the groups
#' are assumed not to differ in baseline outcome level. rnetis refuses to pool
#' groups into one intercept *silently* (no group term at all), but a
#' slope-only term states the shared intercept explicitly, so it is fitted and
#' a design-diagnostic warning (`common_intercept_across_groups`) records the
#' assumption. Use `(1 + ex_y_w | g)` to estimate baseline differences instead.
#'
#' # Wave selection
#'
#' `waves = NULL` (default): with exactly two observed waves, both are used;
#' with one observed wave (or no wave column), the fit is a single
#' cross-section and only contemporaneous terms are available. With three or
#' more, rnetis refuses to choose a transition and `waves = c(from, to)` must
#' name one; one transition is fitted.
#'
#' The outcome is measured at `to`, and every lag counts waves back from `to`
#' by position: a lag of 1 reads `from`, also when `from` and `to` are not
#' adjacent, so `waves = c(1, 3)` regresses wave 3 on wave 1 and wave 2 is not
#' read. A deeper lag steps back from `from` through the earlier observed
#' waves, and is refused when there are none. The fit reports as unused every
#' wave no term reads. A history mechanism such as [bi()] accumulates through
#' every wave before the outcome, so it is refused on a transition that skips
#' waves.
#'
#' # Contemporaneous exposure
#'
#' `exposure(..., x_lag = 0)` measures exposure at the same wave as the
#' outcome rather than a prior one. Two cases are treated differently.
#' A contemporaneous exposure to
#' a *different* variable (`content` is not the outcome) is fit as an
#' ordinary predictor, with a design diagnostic noting that temporal
#' ordering is not established. A contemporaneous exposure to the *outcome
#' itself* -- `Y = rho * W*Y + X*beta + e`, an endogenous simultaneous-
#' autocorrelation term -- routes to [sna::lnam()] instead of ordinary
#' least squares or an unmodified mixed-effects fit, either of which would
#' be statistically invalid for this term. Requires the `sna` package.
#' Only one such term is supported (`lnam()` fits a single `W1` matrix),
#' and it cannot be combined with a `(1 | group)` random-effects term -- both are
#' refused with a clear error rather than silently approximated.
#'
#' The term's [exposure()] options describe `W1`, and each is applied to it:
#' `direction` orients it; `w_lag` reads the network that many waves before
#' the outcome wave (the outcome wave itself by default); a derived conduit
#' such as `binarize(tie)` is resolved as for any other exposure; and
#' `normalize = "row"` divides each row of `W1` by its sum. Normalization is
#' applied after any actor with an incomplete row has been removed, so the
#' rows sum to one over the actors actually fitted. An actor with no ties then
#' has an all-zero row: `isolate_rule = "zero"` keeps it so (the actor
#' receives no contemporaneous influence), `"error"` refuses, and the default
#' `"na"` is refused because `lnam()` cannot fit a missing row of `W1`.
#' `mechanism` must be `"linear"`: any other mechanism computes an exposure
#' value rather than a matrix multiplying the outcome, and is refused. A
#' formula with a moderated term (`*` or `:`) is also refused on this route,
#' because in a simultaneous model a covariate's effect runs through
#' `(I - rho * W1)^-1` and a moderated coefficient is not its conditional
#' effect. For the same reason [influence_effect()] reports an exposure term on this
#' route as a coefficient-scale quantity that excludes that feedback, and rnetis
#' computes no quantity that includes it.
#'
#' The fit reports whether `(I - rho * W1)` is invertible at the estimate.
#' The check is the eigenvalue condition: the operator is singular exactly
#' when `1 - rho * lambda = 0` for an eigenvalue `lambda` of `W1`, and
#' `summary()` prints the smallest modulus `min |1 - rho * lambda|`.
#'
#' # What is and is not identical to fitting by hand
#'
#' The exposure columns in the fitted model are computed by exactly the same
#' [compute_exposure()] engine documented there -- there is one exposure
#' resolution code path in the package, not two that could silently
#' disagree. Ordinary covariates are taken from the focal (`to`) wave (see
#' Wave timing above).
#'
#' # Thresholds in the findings
#'
#' A few findings in `diagnostics` fire at a number that is a package convention, not
#' a published cutoff: `too_few_groups_for_random_effects` below 5 groups for a random
#' intercept and 10 for a random slope, as a WARNING below half of that;
#' `near_zero_variance_component` when a component's standard deviation is under 1%
#' of the residual's; `high_leverage_design_points` at a hat value above 0.5;
#' `structural_exposure_collinearity` at an absolute correlation of 0.9 or more; and
#' `insufficient_within_group_exposure_variance` at a mean within-group variance under
#' `1e-8`.
#'
#' @param formula a model formula; see Formula above. The outcome must be numeric:
#'   the model is linear, and a factor, text or logical outcome is refused. A
#'   numeric outcome holding only 0 and 1 is fitted as given, a linear probability
#'   model on these routes, and a design finding says so.
#' @param data a [netis_data()] object. One wave (or no wave column) supports a
#'   contemporaneous specification; a term that reaches back needs two.
#' @param waves `NULL` (default) or `c(from, to)`, naming two observed waves
#'   with `from` earlier in the wave order. See Wave selection above.
#' @param network_exogeneity how tie formation relates to
#' the outcome process: `"unknown"` (the default), `"exogenous"`,
#' `"sequentially_exogenous"`, or `"endogeneity_suspected"`. A **declaration,
#' not an inference** -- nothing in the data can establish it, and rnetis will
#' not guess. It changes no estimate; it changes what the fit reports itself as
#' assuming. Whether the network *changes over time* is measured from the data
#' and never declared, because asking a user to assert an observable fact only
#' invites the assertion to contradict the data.
#' @param
#' exposure_orthogonal_to_actor_effect whether you are declaring the persistent
#' actor heterogeneity left in the error uncorrelated with network exposure,
#' conditional on the fitted terms (assumption B2; see [netis_assumptions()]).
#' `FALSE` (the default) records it as rnetis's default assumption rather than
#' your claim. Also a declaration: it changes no
#' estimate, only whose assumption the ledger says it is.
#' @return an object of
#' class `netis_influence` (and `netis_fit`). Effect sizes are not stored on
#' the fit: [influence_effect()] computes them on request -- for each [exposure()] term the coefficient, Wald interval and
#' predicted change across the exposure's observed 10th-to-90th percentile
#' (on the `lnam` route, the coefficient times that range, which excludes the network
#' feedback and is labeled as such),
#' and for a *moderated* term (see Moderation above) a `conditional` data
#' frame instead of a single coefficient.
#' `moderation_provenance` is a character vector, one note per unique
#' [add_centering()]/ [add_decomposition()] derived column a moderated term
#' referenced as its moderator or (for a plain-covariate moderand) moderand --
#' empty when no moderated term referenced one. `diagnostics` is a
#' `netis_diagnostics` collection, empty when there is nothing to report:
#' design-family findings (e.g. contemporaneous exposure) for any backend,
#' estimation-family findings (singular fits, too-few-groups, near-zero
#' variance components) for `lme4`-backed fits, and the invertibility and
#' information-matrix findings for `lnam`-backed fits. On the `lnam` route the
#' fit also carries `rho`, `w1_spectral_radius`, `sar_operator_min_modulus`
#' (the smallest modulus of `1 - rho * lambda` over the eigenvalues of `W1`),
#' `sar_operator_invertible` and `dropped_nodes`.
#' @references
#' Friedkin, N. E. and Johnsen, E. C. (1990) Social influence and opinions.
#' *Journal of Mathematical Sociology* **15**(3-4), 193-206.
#'
#' Marsden, P. V. and Friedkin, N. E. (1993) Network studies of social
#' influence. *Sociological Methods & Research* **22**(1), 127-151.
#'
#' Leenders, R. Th. A. J. (2002) Modeling social influence through network
#' autocorrelation: constructing the weight matrix. *Social Networks* **24**(1),
#' 21-47.
#'
#' Anselin, L. (1988) *Spatial Econometrics: Methods and Models*. Kluwer Academic
#' Publishers, Dordrecht. For the simultaneous network-autocorrelation model.
#'
#' Butts, C. T. (2008) Social network analysis with sna. *Journal of Statistical
#' Software* **24**(6), 1-51. For [sna::lnam()].
#'
#' Bates, D., Maechler, M., Bolker, B. and Walker, S. (2015) Fitting linear
#' mixed-effects models using lme4. *Journal of Statistical Software* **67**(1),
#' 1-48. For [lme4::lmer()].
#'
#' Johnson, P. O. and Neyman, J. (1936) Tests of certain linear hypotheses and
#' their application to some educational problems. *Statistical Research Memoirs*
#' **1**, 57-93. For the conditional-effect boundaries of a moderated term.
#' @seealso [ar()],
#' [exposure()], [compute_exposure()], [netis_data()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = rep(c("a", "b", "c", "d"), 2),
#'   wave = rep(1:2, each = 4),
#'   y = c(0.2, -0.6, 0.5, -0.1, 0.4, -0.3, 0.7, 0.0),
#'   x = c(1, 0, 1, 0, 1, 0, 1, 0)
#' )
#' ties <- data.frame(
#'   group = 1,
#'   source = c("a", "b", "c"), target = c("b", "c", "d"),
#'   wave = 1, tie = 1
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#'
#' # Two waves: the transition is unambiguous and need not be named.
#' fit <- fit_influence(y ~ ar() + exposure(y, via = tie), data = nd)
#' fit
#'
#' # With more than two waves rnetis refuses to choose, and `waves =` names the
#' # transition. The fit then reports both what it used and what it did not.
#' nodes3 <- rbind(nodes, transform(nodes[nodes$wave == 2, ], wave = 3, y = y + 0.1))
#' ties3 <- rbind(ties, transform(ties, wave = 2))
#' nd3 <- netis_data(nodes3, ties3, wave_col = "wave")
#' fit_influence(y ~ ar() + exposure(y, via = tie), data = nd3, waves = c(1, 2))
#' @export
fit_influence <- function(formula, data, waves = NULL,
                          network_exogeneity = "unknown",
                          exposure_orthogonal_to_actor_effect = FALSE) {
  call <- match.call()
  # NO ESTIMATOR ARGUMENT: the estimator is carried by the formula.
  #
  # The two DECLARATIONS. Whether the network arose endogenously, and whether
  # the residual actor effect is orthogonal to exposure, are untestable claims
  # that matter for reading a fit whatever estimator produced it. They are
  # stated about an analysis rather than passed to an estimator chooser, and
  # they change what the fit REPORTS, never what it estimates.
  #
  # NO `center` ARGUMENT, as a consequence of the scope. Decomposing a
  # predictor into a group mean and a deviation across the occasions a group
  # is observed has no estimand here: the estimating table holds one occasion, so
  # the within component is
  # identically zero. `add_centering()` and `add_decomposition()` operate on
  # the node table across every wave the data holds.
  .check_netis_data(data)
  if (!inherits(formula, "formula")) {
    stop("formula must be a formula, e.g. y ~ ar() + exposure(x, via = w).", call. = FALSE)
  }
  .assert_enum(network_exogeneity, .network_exogeneity_values, "network_exogeneity")
  if (!is.logical(exposure_orthogonal_to_actor_effect) ||
      length(exposure_orthogonal_to_actor_effect) != 1L ||
      is.na(exposure_orthogonal_to_actor_effect)) {
    stop("exposure_orthogonal_to_actor_effect must be TRUE or FALSE.", call. = FALSE)
  }

  s <- netis_schema(data)
  re <- .parse_random_effects(formula)
  has_random_effects <- length(re$random_terms) > 0L
  if (has_random_effects) {
    .check_random_effects_structure(re$random_terms, s, data)
  }
  # .check_single_group_influence() runs after estimator resolution,
  # alongside the repeated-unit check.

  parsed <- .parse_influence_formula(re$fixed_formula)
  # Which derived column(s), if any, a moderated term
  # referenced -- computed here (needs only `parsed$interactions` and the
  # schema's `derived` registry, both already available) and carried to the
  # final fit object and out to summary() (character(0) for a fit with no
  # moderated terms or none referencing a derived column).
  moderation_provenance <- .moderation_provenance_notes(parsed$interactions, s$derived)
  transitions <- .resolve_influence_transitions(s, waves, parsed)
  waves_resolved <- transitions[[1L]]

  endogenous_idx <- .find_endogenous_exposures(parsed$exposures, parsed$outcome)
  if (length(endogenous_idx) > 1L) {
    stop(
      sprintf(
        "Found %d same-outcome contemporaneous exposure() terms; at most one is supported (sna::lnam() fits a single W1 matrix). Combine them into one network first, or remove all but one.",
        length(endogenous_idx)
      ),
      call. = FALSE
    )
  }
  has_endogenous <- length(endogenous_idx) > 0L
  if (has_endogenous && has_random_effects) {
    stop(
      "Combining (1 | group) with a same-outcome contemporaneous exposure() term is not supported: the lme4 and lnam backends are not composed. Fit each aspect separately.",
      call. = FALSE
    )
  }
  parsed_for_assembly <- parsed
  if (has_endogenous) {
    endogenous_term <- parsed$exposures[[endogenous_idx]]
    .check_endogenous_term_options(endogenous_term)
    .check_lnam_unmoderated(parsed$interactions)
    parsed_for_assembly$exposures <- parsed$exposures[-endogenous_idx]
  }
  resolved_estimator <- .resolve_estimator(has_random_effects, has_endogenous)
  .assert_estimator_available(as.character(resolved_estimator))
  assembled <- .assemble_influence(parsed_for_assembly, data, waves_resolved)
  # A random slope must name a column the assembly actually produced. Checked
  # here because the names exist only after assembly.
  if (has_random_effects) {
    for (rt in re$random_terms) {
      .check_random_effects_slopes(rt, assembled$predictor_cols)
    }
  }

  # THE REFUSALS THAT PRECEDE FITTING are correctness checks: a specification
  # rnetis cannot estimate correctly must fail, not be reported on afterwards. The
  # substantive statements about a specification -- that a contemporaneous
  # exposure leaves temporal ordering unestablished, that an ar() term carries
  # the initial-conditions problem, that network formation is unaddressed -- are
  # facts about the user's specification rather than about which estimator runs,
  # and the assumptions surface computes them from the stored specification.
  if (!has_random_effects) {
    .check_single_group_influence(data, s)
  }

  # How many levels each random-effects grouping factor has, recorded on the
  # fit. Basic fit metadata, not a diagnostic: summary() states it and the
  # variance components are uninterpretable without it.
  re_levels <- if (has_random_effects) {
    vapply(
      re$random_terms,
      function(term) length(unique(assembled$table[[term$group]])),
      integer(1)
    )
  } else {
    NULL
  }
  if (!is.null(re_levels)) {
    names(re_levels) <- vapply(re$random_terms, function(term) term$group, character(1))
  }

  # BACKEND DISPATCH. Three routes, all deterministic optimizers.
  #
  # The endogenous route comes first because its specification is not estimable
  # by the other two at all: the outcome's own contemporaneous exposure is
  # determined jointly with the outcome, so a least-squares fit of it would be
  # biased by construction rather than merely inefficient. sna::lnam() fits the
  # simultaneous system directly.
  if (has_endogenous) {
    w1 <- .build_endogenous_w1(data, s, assembled$table, endogenous_term,
                               waves_resolved[length(waves_resolved)])
    # Recorded like any other resolved exposure so summary() reports it beside
    # the rest, distinguished by its mechanism rather than hidden in the
    # backend. The term is a predictor of the model, not an artifact of how it
    # happened to be estimated.
    assembled$resolved_exposures[[endogenous_term$name]] <- list(
      term = endogenous_term,
      exposure_wave = waves_resolved[length(waves_resolved)],
      conduit_wave = attr(w1, "conduit_wave"),
      mechanism = "sar_endogenous"
    )
    model_formula <- .safe_predictor_formula(assembled$predictor_cols)
    backend <- .new_backend("lnam")
    backend_fit_obj <- backend_fit(
      backend, model_formula, assembled$table,
      W1 = w1, endogenous_name = endogenous_term$name,
      normalize = endogenous_term$normalize,
      isolate_rule = endogenous_term$isolate_rule
    )
  } else if (has_random_effects) {
    # `.backtick_join()` quotes every fixed-effect predictor -- including a
    # moderated term's product column, whose name contains a colon -- so it is
    # referenced literally rather than reinterpreted by lme4's own formula
    # parser. The bars are lme4-specific syntax appended as plain text after
    # that, never passed through the same quoting: a bar is not a column name.
    bars <- vapply(
      re$random_terms,
      function(rt) sprintf("(%s | %s)", .format_bar_lhs(rt), rt$group),
      character(1)
    )
    model_formula <- stats::as.formula(paste(
      ".outcome ~", .backtick_join(assembled$predictor_cols),
      "+", paste(bars, collapse = " + ")
    ))
    backend <- .new_backend("lme4")
    backend_fit_obj <- backend_fit(backend, model_formula, assembled$table)
  } else {
    model_formula <- assembled$lm_formula
    backend <- .new_backend("stats")
    backend_fit_obj <- backend_fit(backend, model_formula, assembled$table)
  }

  extracted <- backend_extract(backend, backend_fit_obj)
  if (has_endogenous) {
    # A node W1 reaches that has no row at this wave is outside the fitted
    # network, the same removal as a node whose row was incomplete.
    extracted$dropped_nodes <- union(extracted$dropped_nodes, attr(w1, "absent_nodes"))
  }

  # DESIGN AND ESTIMATION FINDINGS ARE COMPUTED HERE, and dependence is not.
  # What stays out of the fit is eager INTERPRETATION -- effects, residual
  # dependence -- which is expensive and answers questions
  # a caller may not have asked. These are different: they are facts about
  # what was fitted, they cost nothing (they read the assembled table and the
  # extract already in hand), and some of them are things a user must not have
  # to ask for. A singular fit hidden behind an accessor is a silent one.
  #
  # Residual dependence stays on request, in netis_dependence(): it rebuilds
  # the conduit matrix from the data, which is the expensive part.
  design_findings <- .combine_diagnostics(c(
    unclass(.diagnose_influence_design(parsed$exposures, parsed$outcome, parsed$ar)),
    unclass(.diagnose_influence_outcome(assembled$table, parsed$outcome)),
    unclass(.diagnose_influence_lagged(parsed$lagged, s)),
    unclass(.diagnose_influence_balance(assembled$table, s, assembled$predictor_cols)),
    unclass(.diagnose_groups_absent(data, s, assembled$table, waves_resolved)),
    unclass(.diagnose_exposure_missingness(
      assembled$table, names(assembled$resolved_exposures),
      .absent_network_groups_by_term(assembled$resolved_exposures), s$group_col
    )),
    unclass(.diagnose_influence_collinearity(
      assembled$table,
      structural_cols = c(names(assembled$resolved_centralities),
                          names(assembled$resolved_network_stats)),
      exposure_cols = names(assembled$resolved_exposures)
    )),
    unclass(.diagnose_influence_estimation(
      backend$name, extracted, assembled$table, s$group_col,
      random_terms = if (has_random_effects) re$random_terms else list(),
      re_levels = re_levels
    ))
  ))
  # A term whose assembled column name needed backtick-quoting to survive
  # .safe_predictor_formula()/.backtick_join() (a moderated
  # term's product column, e.g. "x:z") comes back from the backend still
  # wearing those backticks -- `coef()`/`fixef()` report exactly the symbol
  # the fitted formula used. Stripped here, once, for every downstream
  # consumer (summary(), tidy(), netis_estimates(), the effects layer, plot
  # methods) rather than requiring each to know which names might carry
  # them. The backtick was purely a parsing device to stop `stats::as.formula()`
  # reinterpreting the colon as ITS OWN interaction operator; it was never
  # part of the term's identity, and a coefficient named "x:z" is what a
  # user who wrote `x * z` expects to see.
  names(extracted$coefficients) <- gsub("^`|`$", "", names(extracted$coefficients))
  dimnames(extracted$vcov) <- list(
    gsub("^`|`$", "", rownames(extracted$vcov)),
    gsub("^`|`$", "", colnames(extracted$vcov))
  )
  # ------------------------------------------------------------------
  # Everything above estimates the model. Nothing below it
  # interprets one.
  #
  # Diagnostics, dependence measurements, reliability and effect sizes are
  # not computed inline, so a fit can exist without the whole interpretation
  # subsystem. `fit_influence()` records what it did and what it did it to; `netis_diagnostics()`, the assumptions surface and `summary()`
  # compute the rest later, from this record.
  #
  # What is recorded is chosen so nothing downstream has to RECONSTRUCT a
  # modeling decision and risk disagreeing with what was actually fitted.
  # ------------------------------------------------------------------

  spec <- list(
    schema_version = "1",
    family = "influence",
    call = call,
    formula = formula,
    outcome = parsed$outcome,
    outcome_family = "gaussian",
    # THE WAVE RECORD. Both halves, deliberately: which waves were analyzed
    # AND which were present but not used. A ten-wave dataset analyzed through
    # one transition is exactly what requiring `waves =` exists to prevent, so
    # the fit states the omission rather than only the selection.
    waves = waves_resolved,
    waves_available = s$waves,
    # Unused means NOT READ by any term -- not merely outside the named pair:
    # a deeper lag or a history mechanism reads waves before `from`.
    waves_unused = setdiff(as.character(s$waves),
                           c(as.character(unlist(transitions)), as.character(assembled$waves_read))),
    transitions = transitions,
    # WHAT WAS ACTUALLY ESTIMATED. A fit over two waves estimates a
    # transition; a fit at one occasion estimates a cross-section, and calling
    # that a "transition" describes a comparison the data cannot support.
    estimand = if (length(waves_resolved) >= 2L) {
      "single transition"
    } else {
      "single cross-section"
    },
    estimator = as.character(resolved_estimator),
    estimator_reason = attr(resolved_estimator, "reason"),
    ar = parsed$ar,
    lagged = parsed$lagged,
    exposures = parsed$exposures,
    centralities = parsed$centralities,
    network_stats = parsed$network_stats,
    covariates = parsed$covariates,
    interactions = parsed$interactions,
    moderation_provenance = moderation_provenance,
    random_effects = if (has_random_effects) re$random_terms else NULL,
    data_schema = s,
    netis_version = as.character(utils::packageVersion("rnetis")),
    created_at = Sys.time()
  )

  # THE ASSUMPTION LEDGER. Built here because every input exists by this point
  # and nothing later changes one.
  assumptions <- .build_assumption_ledger(list(
    estimator = as.character(resolved_estimator),
    has_ar = length(parsed$ar) > 0L,
    # Exposure terms whose CONTENT is the outcome -- at any lag, not only the
    # contemporaneous one. B2 and C are about the exposure coefficient's
    # relationship to the outcome process, which a lagged same-outcome
    # exposure has just as much as a contemporaneous one.
    outcome_content_terms = vapply(
      Filter(function(e) identical(e$content, parsed$outcome), parsed$exposures),
      function(e) e$name %||% e$content, character(1)
    ),
    covariate_content_terms = vapply(
      Filter(function(e) !identical(e$content, parsed$outcome), parsed$exposures),
      function(e) e$name %||% e$content, character(1)
    ),
    network_timing = .measure_network_timing(
      data, s, as.character(waves_resolved),
      tie_vars = unique(unlist(lapply(parsed$exposures, function(e) .conduit_variables(e$conduit))))
    ),
    network_exogeneity = network_exogeneity,
    b2_declared = isTRUE(exposure_orthogonal_to_actor_effect)
  ))

  .new_netis_fit(
    "netis_influence",
    fields = list(
      spec = spec,
      # WHAT THIS FIT IS ASSUMING, resolved against the specification. Carried
      # on the object rather than recomputed, because the specification it was
      # resolved against is the one that was fitted.
      assumptions = assumptions,
      call = call,
      netis_version = spec$netis_version,
      # The RESOLVED term specifications, including each exposure's mechanism
      # provenance -- for a B-I term, its delta, alpha and gamma. summary()
      # reports them from here, so a fit can always say what produced it.
      resolved_exposures = assembled$resolved_exposures,
      resolved_centralities = assembled$resolved_centralities,
      # Xu's latent-space adjustment: which selection model was fitted, at
      # which wave, over how many actors. Carried because a position column
      # is meaningless without the model that produced it.
      resolved_latent = assembled$resolved_latent,
      resolved_network_stats = assembled$resolved_network_stats,
      backend = backend$name,
      backend_version = backend_info(backend)$version,
      estimation_controls = extracted$sampler,
      coefficients = extracted$coefficients,
      vcov = extracted$vcov,
      random_effects = extracted$random_effects,
      convergence = extracted$convergence,
      loglik = extracted$loglik,
      r_squared = extracted$r_squared,
      adj_r_squared = extracted$adj_r_squared,
      residual_df = extracted$residual_df,
      # Satterthwaite t tests on the lme4 route, NULL on every other: the other
      # backends' tests are arithmetic on the coefficients and their covariance.
      coef_tests = extracted$coef_tests,
      sigma = extracted$sigma,
      var_corr = extracted$var_corr,
      # THE ADAPTER'''S RESIDUALS, named by the rows the fit actually used.
      # Kept on the object rather than left to be re-read from backend_fit:
      # a backend that drops incomplete rows returns residuals shorter than
      # the estimating table and, for lnam, unnamed -- so pairing them with
      # rows by position is wrong exactly when rows were dropped. The adapter
      # is the only layer that still knows the correspondence.
      residuals = extracted$residuals,
      residual_type = extracted$residual_type,
      # SAR-ROUTE QUANTITIES, NULL on every other route. `rho` is the
      # autocorrelation parameter itself. Whether the fitted simultaneous
      # system (I - rho * W1) is invertible is read from the eigenvalues of
      # W1: its smallest eigenvalue modulus, min |1 - rho * lambda|, is
      # recorded with the verdict, and the spectral radius of W1 alongside.
      # `dropped_nodes` names actors removed from the NETWORK because their
      # row was incomplete -- recorded because nothing is dropped silently.
      rho = extracted$rho,
      w1_spectral_radius = extracted$w1_spectral_radius,
      sar_operator_min_modulus = extracted$sar_operator_min_modulus,
      sar_operator_invertible = extracted$sar_operator_invertible,
      information_matrix_available = extracted$information_matrix_available,
      dropped_nodes = extracted$dropped_nodes,
      n_groups = if (has_random_effects) unname(re_levels[1]) else NULL,
      re_levels = if (has_random_effects) re_levels else NULL,
      # Design-family and estimation-family findings, resolved against what was
      # actually fitted. `NULL` would mean "not computed"; an empty collection
      # means "computed, nothing to report", and those are different claims.
      diagnostics = design_findings,
      moderation_provenance = moderation_provenance,
      # The estimating sample as fitted. Every later diagnostic reads this
      # rather than re-assembling from `data`, which is what stops a
      # diagnostic describing a table the fit never saw.
      model_frame = assembled$table,
      backend_fit = backend_fit_obj
    )
  )
}

#' Validate a random-effects specification's structure (pre-assembly)
#'
#' The supported grouping level is the group column: one random intercept
#' (and optionally one random slope) per network. A node-level term,
#' `(1 | node)` or `(1 | group/node)`, is recognized so that it can be
#' refused with its reason: a fit analyses one transition, so every node
#' contributes exactly one outcome row, and a node-level variance is not
#' separately identified from the residual variance. Any other grouping
#' factor is refused as unsupported.
#'
#' `(1 | group/node)` reaches here already expanded by
#' `reformulas::findbars()` into a `node:group` term and a `group` term; the
#' node-level half is what is refused.
#'
#' At most one random-slope predictor is supported per term. What that
#' predictor's *name* must resolve to (a real fixed-effects column) can only
#' be checked after assembly -- see .check_random_effects_slopes().
#'
#' @noRd
.check_random_effects_structure <- function(random_terms, schema, data) {
  levels_seen <- character(0)
  n_groups <- length(unique(netis_nodes(data)[[schema$group_col]]))

  for (rt in random_terms) {
    if (length(rt$slopes) > 1L) {
      stop(
        sprintf(
          "fit_influence() supports at most one random-slope predictor per random-effects term; found %d: %s.",
          length(rt$slopes), paste(rt$slopes, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    if (length(rt$slopes) == 0L && !rt$intercept) {
      stop(
        sprintf(
          "(0 | %s) specifies neither a random intercept nor a random slope; there is nothing to estimate. Use (1 | %s) for a random intercept.",
          rt$group, rt$group
        ),
        call. = FALSE
      )
    }

    level <- .resolve_re_grouping_level(rt$group, schema)
    if (is.null(level)) {
      stop(
        sprintf(
          "The random-effects grouping variable '%s' is not one fit_influence() supports. The supported grouping factor is the network: (1 | %s), optionally with one random slope, e.g. (1 + ex_y_w | %s).",
          rt$group, schema$group_col, schema$group_col
        ),
        call. = FALSE
      )
    }
    if (level %in% c("node", "nested")) {
      stop(
        sprintf(
          "A node-level random effect (%s | %s) cannot be fitted: fit_influence() analyses one transition, so each node contributes exactly one outcome row, and a node-level variance is not separately identified from the residual variance. Use (1 | %s) for a network-level effect, or no random-effects term for a single network.",
          .format_bar_lhs(rt), .node_level_bar_label(rt$group, level, schema),
          schema$group_col
        ),
        call. = FALSE
      )
    }
    if (level %in% levels_seen) {
      stop(
        sprintf(
          "The formula specifies more than one random-effects term at the same grouping level ('%s'). Combine them into a single term.",
          rt$group
        ),
        call. = FALSE
      )
    }
    levels_seen <- c(levels_seen, level)

    if (n_groups < 2L) {
      stop(
        sprintf(
          "(%s | %s) requires more than one group; this netis_data object has %d. Fit a plain ar()/exposure() formula (no random-effects term) for a single network.",
          .format_bar_lhs(rt), rt$group, n_groups
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' The grouping a refused node-level term was written with
#'
#' `(1 | group/node)` arrives expanded as `node:group`; the message names the
#' spelling the user wrote.
#'
#' @noRd
.node_level_bar_label <- function(group_expr, level, schema) {
  if (identical(level, "nested")) {
    paste(schema$group_col, schema$node_col, sep = "/")
  } else {
    group_expr
  }
}

#' Classify a random-effects grouping expression against the data schema
#'
#' Returns `"group"`, `"node"`, `"nested"`, or `NULL` for an unsupported
#' expression. Only `"group"` can be fitted; the node-level levels are
#' recognized so that `.check_random_effects_structure()` can refuse them with
#' the reason rather than as an unknown grouping factor.
#'
#' @noRd
.resolve_re_grouping_level <- function(group_expr, schema) {
  expr <- gsub("\\s+", "", group_expr)
  if (identical(expr, schema$group_col)) {
    return("group")
  }
  if (identical(expr, schema$node_col)) {
    return("node")
  }
  # `(1 | group/node)` reaches here already expanded by
  # reformulas::findbars() into an interaction, whose deparsed order is not
  # something to rely on; the unexpanded spelling is accepted too in case a
  # caller constructs terms directly.
  nested_spellings <- c(
    paste(schema$group_col, schema$node_col, sep = "/"),
    paste(schema$group_col, schema$node_col, sep = ":"),
    paste(schema$node_col, schema$group_col, sep = ":")
  )
  if (expr %in% nested_spellings) {
    return("nested")
  }
  NULL
}

#' Validate a random slope's predictor against the assembled fixed effects
#'
#' the naming guarantee -- exposure() columns are named the same
#' way in the formula, in compute_exposure() output, and in the assembled
#' table -- is what makes this check meaningful: the random-slope predictor
#' in `(term | group)` must be one of the columns .assemble_influence_table()
#' actually produced, not an arbitrary string.
#'
#' @noRd
.check_random_effects_slopes <- function(random_term, predictor_cols) {
  if (length(random_term$slopes) == 0L) {
    return(invisible(TRUE))
  }
  slope_var <- random_term$slopes
  if (!(slope_var %in% predictor_cols)) {
    stop(
      sprintf(
        "The random-slope predictor '%s' in (%s | %s) is not one of this formula's fixed-effects columns (%s). It must match an ar()/exposure() term's column name or a covariate exactly, e.g. (1 + ex_y_w | %s) for an exposure() term named 'ex_y_w'.",
        slope_var, .format_bar_lhs(random_term), random_term$group,
        paste(predictor_cols, collapse = ", "), random_term$group
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Format a random-effects term's left-hand side back into formula text
#'
#' The inverse of the intercept/slopes split .parse_random_effects() does --
#' used both to build the lmer() formula and in error/print messages, so
#' there is one place that knows how a (term | group) bar's LHS is written,
#' not several that could drift apart.
#'
#' @noRd
.format_bar_lhs <- function(random_term) {
  if (length(random_term$slopes) == 0L) {
    return("1")
  }
  if (random_term$intercept) {
    paste("1 +", paste(random_term$slopes, collapse = " + "))
  } else {
    paste("0 +", paste(random_term$slopes, collapse = " + "))
  }
}

#' Find same-outcome contemporaneous ("endogenous WY") exposure() terms
#'
#' A term is endogenous when its content is the outcome itself and it is
#' measured at the same wave as the outcome (`x_lag = 0`). Used to route
#' such a formula to the `lnam` backend, since least squares on that term would
#' be invalid: the regressor is determined jointly with the outcome.
#'
#' @return integer indices into `exposure_terms`; a formula with more than
#'   one is rejected by the caller (`sna::lnam()` takes a single `W1`).
#' @noRd
.find_endogenous_exposures <- function(exposure_terms, outcome) {
  which(vapply(exposure_terms, function(e) {
    identical(e$content, outcome) && identical(as.integer(e$x_lag), 0L)
  }, logical(1)))
}

#' Refuse the exposure() options a simultaneous W1 term cannot honour
#'
#' The same-outcome contemporaneous term is fitted as `rho * W1 Y` by
#' `sna::lnam()`, which takes a single fixed matrix. Of the `exposure()`
#' options, `direction`, `w_lag`, `normalize` and `isolate_rule` describe
#' that matrix and are applied to it. A `mechanism` other than `"linear"`
#' computes an exposure VALUE from the network and the content rather than a
#' matrix that multiplies the outcome, so it has no meaning inside a
#' simultaneous system and is refused by name rather than ignored.
#'
#' @noRd
.check_endogenous_term_options <- function(endogenous_term) {
  # A history mechanism has a more basic objection than the route: it has no
  # reading at x_lag = 0 at all, on the outcome or any other content.
  if (.mechanism_reads_history(endogenous_term$mechanism)) {
    .assert_history_exposure_timing(
      endogenous_term, NULL, NULL,
      sprintf("exposure(%s, via = %s, x_lag = 0)", endogenous_term$content,
              .conduit_label(endogenous_term$conduit))
    )
  }
  if (!identical(endogenous_term$mechanism, "linear")) {
    stop(
      sprintf(
        "exposure(%s, via = %s, x_lag = 0) is a same-outcome contemporaneous term, fitted as rho * W1 %%*%% %s by sna::lnam(). Only mechanism = \"linear\" is defined for it: another mechanism computes an exposure value rather than a network matrix, and a simultaneous model needs the matrix. Use mechanism = \"linear\", or lag the content (x_lag = 1) to fit the mechanism as an ordinary predictor.",
        endogenous_term$content, .conduit_label(endogenous_term$conduit),
        endogenous_term$content
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Refuse moderation on the simultaneous (lnam) route
#'
#' Moderation is fitted on the `pooled` and `mixed` routes only. In a
#' simultaneous model a covariate's effect on the outcome runs through
#' `(I - rho W1)^-1`, so the conditional-coefficient and Johnson-Neyman layer,
#' which reads a moderated coefficient as the term's effect at a moderator
#' value, does not describe it. Refused rather than fitted and reported on a
#' basis that does not apply.
#'
#' @noRd
.check_lnam_unmoderated <- function(interactions) {
  if (length(interactions) == 0L) {
    return(invisible(TRUE))
  }
  stop(
    "A moderated term (an interaction written with * or :) cannot be combined with a same-outcome contemporaneous exposure() term. That term is fitted by sna::lnam() as a simultaneous system, in which a covariate's effect on the outcome runs through (I - rho * W1)^-1, so a moderated coefficient is not the term's conditional effect. Moderation is fitted by the pooled and mixed routes only: drop the moderation, or lag the exposure (x_lag = 1).",
    call. = FALSE
  )
}

#' Build the row/column-ordered W1 matrix for an endogenous exposure term
#'
#' Reuses .build_canonical_W() -- the same direction-resolution code path
#' the exposure engine uses for every other exposure computation -- rather
#' than a second, hand-rolled matrix construction. The result is reindexed
#' to match `table`'s own row order exactly (.build_canonical_W() dimnames
#' its result by node id, which is what makes this reindex safe: nothing
#' here depends on row position matching by construction, only by explicit
#' name lookup).
#'
#' The network is read at the term's conduit wave: `w_lag` waves before the
#' outcome wave (the outcome wave itself when `w_lag` is 0 or unset, since
#' `x_lag` is 0 for this term). The matrix is returned UNNORMALIZED:
#' row-normalization is applied by the lnam adapter after it has removed any
#' actor with an incomplete row, so that the normalized rows describe the
#' network that is actually fitted.
#'
#' `W1` has one row per estimating row, because `sna::lnam()` needs an outcome
#' for every actor in the system. A tie reaching a node of the group with no row
#' at this wave therefore cannot enter it, and that node is reported in
#' `dropped_nodes` as removed from the network.
#'
#' @return the W1 matrix, with the conduit wave in attribute `conduit_wave` and
#'   the nodes it could not include in attribute `absent_nodes`.
#' @noRd
.build_endogenous_w1 <- function(data, schema, table, endogenous_term, to_wave) {
  group_id <- as.character(table[[schema$group_col]])[1]
  w_lag <- endogenous_term$w_lag %||% 0L
  conduit_wave <- if (is.null(to_wave) || identical(as.integer(w_lag), 0L)) {
    to_wave
  } else {
    .influence_lag_wave(
      schema, to_wave, w_lag,
      sprintf("exposure(%s, via = %s, x_lag = 0, w_lag = %d)",
              endogenous_term$content, .conduit_label(endogenous_term$conduit),
              as.integer(w_lag))
    )
  }
  ties_all <- netis_ties(data)
  ties_focal <- .filter_group_wave(ties_all, schema, group_id, conduit_wave)
  if (.network_absent_at_wave(ties_all, ties_focal, schema, group_id, conduit_wave)) {
    stop(
      sprintf(
        "The network '%s' has no ties recorded at wave %s for group %s, although the group has a network recorded at other waves, so it is unobserved there rather than empty. A simultaneous term cannot be fitted on an unobserved network. Choose a w_lag that reaches an observed wave.",
        .conduit_label(endogenous_term$conduit), conduit_wave, group_id
      ),
      call. = FALSE
    )
  }
  ids_order <- as.character(table[[schema$node_col]])
  nodes_all <- netis_nodes(data)
  .assert_tie_endpoints_on_roster(
    ties_focal, schema, group_id,
    unique(as.character(nodes_all[[schema$node_col]][
      as.character(nodes_all[[schema$group_col]]) == group_id]))
  )
  absent_nodes <- setdiff(c(as.character(ties_focal[[schema$source_col]]),
                            as.character(ties_focal[[schema$target_col]])), ids_order)
  w1_raw <- .build_canonical_W(
    ties_focal, sort(unique(ids_order)),
    source_col = schema$source_col, target_col = schema$target_col,
    value_col = endogenous_term$conduit, direction = endogenous_term$direction,
    directed = isTRUE(schema$directed),
    data = data, group = group_id, wave = conduit_wave
  )
  out <- w1_raw[ids_order, ids_order, drop = FALSE]
  attr(out, "conduit_wave") <- conduit_wave
  attr(out, "absent_nodes") <- absent_nodes
  out
}


#' Refuse to silently pool independent networks into one OLS fit
#'
#' Called only when the formula has no random-effects term. Without this
#' check, a multi-group netis_data object would flow straight through
#' .assemble_influence_table() -- compute_exposure() already resolves
#' exposure correctly per group -- and land in a single stats::lm() call
#' that pools rows from unrelated networks with no group term, silently
#' exactly the "accidental pooling of independent networks" case the data
#' diagnostics warn about. Fail clearly instead: rnetis never returns a result
#' from a specification it cannot estimate correctly, because a wrong number
#' is worse than no number. Add `(1 | group)` to the formula to route to the lme4
#' backend instead of hitting this check.
#'
#' @noRd
.check_single_group_influence <- function(data, schema) {
  groups <- unique(netis_nodes(data)[[schema$group_col]])
  if (length(groups) <= 1L) {
    return(invisible(TRUE))
  }
  # TWO WAYS FORWARD, AND BOTH EXIST. There is no `estimator =` argument, so
  # the message offers only the two routes that do exist.
  stop(
    sprintf(
      "This netis_data object has %d groups, but the formula has no random-effects term, so the fit would pool rows from unrelated networks into one intercept. Two ways forward: add (1 | %s) to the formula, which fits one mixed-effects model across the networks and is usually what is wanted; or subset to a single group's rows and reconstruct a single-group netis_data(), which fits that network on its own by ordinary least squares.",
      length(groups), schema$group_col
    ),
    call. = FALSE
  )
}

#' Resolve which wave or transition a fit analyses
#'
#' THE WAVE SCOPE. A package-scope boundary, not a claim about what the estimators
#' can do:
#'
#' \itemize{
#'   \item **one wave** -- a contemporaneous specification, with no term that
#'     reaches back. `ar()`, `lagged()`, and a lagged exposure are refused,
#'     because the wave they need is not observed.
#'   \item **two waves** -- the ordinary longitudinal case, resolved without
#'     the user naming them.
#'   \item **more than two** -- rnetis neither uses every wave nor picks two.
#'     `waves = c(from, to)` is required, and names exactly two.
#' }
#'
#' WHY THE REFUSAL RATHER THAN A DEFAULT. Silently taking the latest transition
#' is the failure to stop: a ten-wave study analyzed through one transition, with nothing in the output
#' saying which one or how much was set aside. Choosing the transition is the
#' user's decision, and the fit reports both what it used and what it did not.
#'
#' @param schema the `netis_data` schema.
#' @param waves `NULL`, or `c(from, to)`.
#' @param parsed the parsed formula, used to tell a contemporaneous
#'   specification from one needing a prior wave.
#' @return a list holding one element: the wave pair `c(from, to)`, or a single
#'   wave for a contemporaneous fit. A list of one rather than a bare vector so
#'   that the assembly path has one shape to handle.
#' @noRd
.resolve_influence_transitions <- function(schema, waves, parsed = NULL) {
  # Already in time order (netis_data() builds it); sorting again would put
  # text labels back in alphabetical order.
  observed <- schema$waves
  n_obs <- length(observed)

  # Fitting several transitions at once is refused by name rather than reading as
  # a typo report: each unit would contribute several outcome rows, a
  # repeated-measures estimand this package does not fit.
  # A pair of text labels is an ordinary transition (wave labels may be text
  # or a factor); a single string such as "all" or a list of pairs is not.
  if (is.list(waves) || (is.character(waves) && length(waves) == 1L)) {
    stop(
      "waves must be NULL or c(from, to): rnetis fits one transition, and a specification that pools several is not available. `waves = \"all\"` and a list of pairs both mean fitting every transition at once, which makes each unit contribute several outcome rows -- a different estimand from the one rnetis fits here. Name the transition you want, e.g. waves = c(1, 2).",
      call. = FALSE
    )
  }

  needs_prior <- .spec_needs_prior_wave(parsed)

  # ---- no wave dimension at all: one cross-section --------------------------
  # Data with no wave column is a single occasion, which is the T = 1 case
  # written the other way round -- and the natural shape for a contemporaneous
  # specification, including the simultaneous network-autocorrelation route.
  # It is reachable from any `netis_data()` built without `wave_col`, and the
  # Selection side accepts the same input as "no wave dimension".
  #
  # `list(NULL)` rather than `list(observed)`: there is no wave label to carry,
  # and `.filter_group_wave()` treats a NULL wave as "no wave filtering", which
  # is exactly right when the data has no wave column to filter on.
  if (n_obs < 1L) {
    if (!is.null(waves)) {
      stop(
        "waves = ... was supplied, but this data has no wave dimension: there are no waves to name. Remove waves =, or supply data with a wave column.",
        call. = FALSE
      )
    }
    if (length(needs_prior) > 0L) {
      stop(
        sprintf(
          "This formula needs a prior wave -- %s -- but this data has no wave dimension at all. A contemporaneous specification fits a single cross-section; a term that reaches back needs waves to reach back through. Supply data with a wave column, or drop the term.",
          paste(needs_prior, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(list(NULL))
  }

  # ---- one wave: contemporaneous only ---------------------------------------
  if (n_obs == 1L) {
    if (!is.null(waves)) {
      stop(
        sprintf("waves = ... was supplied, but this data has a single wave (%s); there is no transition to choose.", observed),
        call. = FALSE
      )
    }
    if (length(needs_prior) > 0L) {
      stop(
        sprintf(
          "This formula needs a prior wave -- %s -- but the data has only one wave (%s). A contemporaneous specification fits at one wave; a term that reaches back does not. Supply data at an earlier wave, or drop the term.",
          paste(needs_prior, collapse = ", "), observed
        ),
        call. = FALSE
      )
    }
    return(list(observed))
  }

  # ---- exactly two waves: no choice to make ---------------------------------
  if (n_obs == 2L && is.null(waves)) {
    return(list(observed))
  }

  # ---- more than two, unnamed: refuse ---------------------------------------
  if (is.null(waves)) {
    stop(
      sprintf(
        "This data has %d waves (%s), and this version fits one transition at a time. rnetis will not choose which one for you: analysing a %d-wave study through a single transition is a decision about your design, and leaving it to a default is how it goes unnoticed. Name the transition, e.g. waves = c(%s, %s).",
        n_obs, paste(observed, collapse = ", "), n_obs,
        observed[n_obs - 1L], observed[n_obs]
      ),
      call. = FALSE
    )
  }

  # ---- named: exactly two, observed, forwards -------------------------------
  list(.resolve_influence_waves(schema, waves))
}

#' Which terms in a specification require a wave before the outcome's
#'
#' @param parsed the parsed formula, or `NULL`.
#' @return a character vector naming the offending term kinds, empty when the
#'   specification is contemporaneous. Callers must test its LENGTH: an empty
#'   character vector is not FALSE-y under `if`, and writing it that way is an
#'   error R only raises when the vector is empty -- that is, on exactly the
#'   contemporaneous specification the branch exists to allow.
#' @noRd
.spec_needs_prior_wave <- function(parsed) {
  if (is.null(parsed)) {
    return(character(0))
  }
  out <- character(0)
  if (length(parsed$ar) > 0L) out <- c(out, "ar()")
  if (length(parsed$lagged) > 0L) out <- c(out, "lagged()")
  # An exposure reaching back needs a prior wave; a contemporaneous one does
  # not, and the two are distinguished by the term's own lag rather than by
  # whether an exposure is present at all.
  # `w_lag` DEFAULTS TO `x_lag`, not to 1. exposure() stores NULL when the
  # caller does not name it, and the documented default is the term's own
  # x_lag, which is how the assembly reads it too. So
  # `exposure(y, via = w, x_lag = 0)` -- the contemporaneous same-outcome term the
  # SAR route exists to fit -- needs no prior wave, and fits on single-occasion data.
  lagged_exposure <- vapply(
    parsed$exposures,
    function(e) {
      x_lag <- e$x_lag %||% 1L
      w_lag <- e$w_lag %||% x_lag
      isTRUE(x_lag > 0L) || isTRUE(w_lag > 0L)
    },
    logical(1)
  )
  if (any(lagged_exposure)) out <- c(out, "a lagged exposure()")
  # Network-feature terms read their network `lag` waves back too; at lag 0
  # they read the outcome wave and need no earlier one.
  for (kind in list(list(terms = parsed$centralities, label = "a lagged centrality()"),
                    list(terms = parsed$network_stats, label = "a lagged network_stat()"),
                    list(terms = parsed$latent, label = "a lagged latent()"))) {
    if (any(vapply(kind$terms, function(t) isTRUE(t$lag > 0L), logical(1)))) {
      out <- c(out, kind$label)
    }
  }
  out
}

#' Validate a user-named transition
#'
#' @param schema the `netis_data` schema.
#' @param waves the user's `c(from, to)`.
#' @return the validated pair.
#' @noRd
.resolve_influence_waves <- function(schema, waves) {
  observed <- schema$waves
  if (length(waves) != 2L) {
    stop(
      sprintf(
        "waves must name exactly two waves, c(from, to); %d given. This version fits one transition, so a longer selection has no reading -- it would either pool transitions, which is a different estimand, or leave rnetis to choose among them, which is what naming them avoids.",
        length(waves)
      ),
      call. = FALSE
    )
  }
  if (!all(as.character(waves) %in% as.character(observed))) {
    stop(
      sprintf(
        "waves = %s names %s, which %s not among the observed waves: %s.",
        paste(waves, collapse = ", "),
        paste(setdiff(as.character(waves), as.character(observed)), collapse = ", "),
        if (length(setdiff(as.character(waves), as.character(observed))) == 1L) "is" else "are",
        paste(observed, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  # Compared by POSITION in the observed order, not by label: "10" < "9" as
  # text, and a factor's labels need not sort at all.
  pos <- match(as.character(waves), as.character(observed))
  if (!(pos[1] < pos[2])) {
    stop("waves = c(from, to) must have from < to (from earlier in the wave order): the outcome is measured at the later wave.", call. = FALSE)
  }
  # Returned in the schema's own label type, so later lookups compare like
  # with like.
  observed[pos]
}

#' Assemble the analysis table of one fit
#'
#' The single-transition assembler, plus the refusals that read the assembled
#' table as a whole.
#'
#' @noRd
.assemble_influence <- function(parsed, data, waves) {
  one <- .assemble_influence_table(parsed, data, waves)
  # A group-level feature resolved at one wave varies only across groups, so a
  # constant column is aliased with the intercept.
  for (net_term in parsed$network_stats) {
    .assert_network_stat_identified(one$table[[net_term$name]], net_term$name)
  }
  .assert_some_complete_row(one$table, one$predictor_cols)
  one
}

#' Refuse an estimating table with no complete row, naming what emptied it
#'
#' Every backend would otherwise stop with its own message about zero cases,
#' which does not say which term left nothing to fit.
#'
#' @noRd
.assert_some_complete_row <- function(table, predictor_cols) {
  used <- c(".outcome", predictor_cols)
  if (any(stats::complete.cases(table[used]))) {
    return(invisible(TRUE))
  }
  empty <- used[vapply(used, function(col) all(is.na(table[[col]])), logical(1))]
  stop(
    sprintf(
      "The estimating table has no row with every value present, so there is nothing to fit.%s Check that each term's variables and network are observed at the wave it reads.",
      if (length(empty) > 0L) {
        sprintf(" Missing in every row: %s.", paste(gsub("^\\.outcome$", "the outcome", empty), collapse = ", "))
      } else {
        ""
      }
    ),
    call. = FALSE
  )
}

#' Resolve the wave a rnetis-managed Influence lag points at
#'
#' Positional, matching Selection's `.selection_lag_wave()`: a lag of `k` steps
#' back `k` positions through the fit's wave *sequence*. Waves are discrete
#' ordered occasions, not numbers to subtract from -- with observed waves 1, 3,
#' 5, a lag of 1 from wave 5 is wave 3 (rnetis-managed lags are positional, not
#' elapsed time and not label arithmetic).
#'
#' The sequence is the fit's own (`.lag_sequence()`): the observed waves up to
#' and including `from`, then `to`. So for a named transition `c(from, to)` a
#' lag of 1 always reads `from`, also when the pair is not adjacent, and the
#' waves strictly between the two are never read.
#'
#' A lag of 0 is the outcome wave itself, returned as given -- including `NULL`
#' for data with no wave dimension, where a contemporaneous term has no wave to
#' look up.
#'
#' A lag reaching before the first wave of the sequence errors at the specific
#' term responsible, before assembly produces an empty table.
#'
#' @param schema the data schema.
#' @param wave the outcome wave.
#' @param lag the term's lag.
#' @param term_desc how the term is named in an error.
#' @param sequence the wave sequence lags step through; defaults to every
#'   observed wave.
#' @noRd
.influence_lag_wave <- function(schema, wave, lag, term_desc, sequence = schema$waves) {
  if (identical(as.integer(lag), 0L)) {
    return(wave)
  }
  pos <- match(as.character(wave), as.character(sequence))
  if (length(pos) != 1L || is.na(pos)) {
    stop(
      sprintf("%s needs a wave before the outcome wave, but %s.",
              term_desc,
              if (length(wave) == 0L) "this data has no wave dimension" else
                sprintf("wave '%s' is not one of the observed waves (%s)", wave,
                        paste(schema$waves, collapse = ", "))),
      call. = FALSE
    )
  }
  target <- pos - as.integer(lag)
  if (target < 1L) {
    skipped <- setdiff(as.character(schema$waves[seq_len(match(as.character(wave), as.character(schema$waves)))]),
                       as.character(sequence))
    stop(
      sprintf(
        "%s reaches %d wave(s) before wave %s, but only %d earlier wave(s) are available to this fit (%s).%s Reduce the lag, or supply data at earlier waves.",
        term_desc, as.integer(lag), wave, pos - 1L,
        paste(sequence, collapse = ", "),
        if (length(skipped) > 0L) {
          sprintf(" The named transition skips wave(s) %s, which a lag does not step through.",
                  paste(skipped, collapse = ", "))
        } else {
          ""
        }
      ),
      call. = FALSE
    )
  }
  sequence[[target]]
}

#' Build `.outcome ~ <predictors>` with every predictor name a LITERAL reference
#'
#' Guards a serious failure, not a moderation-specific one: `stats::as.formula(
#' paste(".outcome ~", paste(predictor_cols, collapse = " + ")))` treats a bare `:` inside a column name
#' as R's OWN interaction operator, not as a reference to a column literally
#' named that. For a moderated term this is catastrophic in a way that is
#' invisible on ordinary numeric data: `table[["x:z"]]`, built by
#' .assemble_influence_table()'s product loop, would be constructed correctly
#' and then SILENTLY IGNORED, because `lm()`'s own `model.matrix()` would
#' recompute `x:z` from the raw `x` and `z` columns instead. Proven, not inferred:
#' without backticks, replacing every value in `table[["x:z"]]` with garbage
#' leaves `coef(lm(...))` byte-identical.
#'
#' It is invisible on ordinary data because for two numeric operands, R's own `:` recomputation is mathematically identical
#' to the product this package builds by hand -- both compute `x * z` from
#' the same two columns, so a known-answer test comparing coefficients could
#' not tell the two mechanisms apart. It becomes visible the moment
#' a moderator is a factor: `model.matrix()`'s own `:` dummy-codes a factor
#' operand into per-level contrast columns (`x:zlo`), which is a DIFFERENT
#' computation from the numeric product this package's architecture
#' deliberately controls (so an unsupported moderator type can be refused by
#' name rather than silently reinterpreted by `model.matrix()`'s
#' own contrast machinery).
#'
#' Backticking every name is the fix, not a workaround: `` `x:z` ``, `` `x` ``
#' and `` `y_lag1` `` are all valid literal column references, and the
#' backtick is a no-op for a name that was already a valid symbol. There is
#' no "is this name safe" branch to get wrong, because every name is treated
#' identically.
#'
#' @param predictor_cols character vector of column names in the assembled table.
#' @param outcome_col the response column's name, default `.outcome`
#'   (the internal convention every assembly function uses).
#' @return a formula.
#' @noRd
.safe_predictor_formula <- function(predictor_cols, outcome_col = ".outcome") {
  stats::as.formula(paste(
    sprintf("`%s`", outcome_col), "~", .backtick_join(predictor_cols)
  ))
}

#' Backtick-quote and `+`-join a set of column names for use inside a formula string
#'
#' The half of `.safe_predictor_formula()` that a caller needs when the other
#' half is not an ordinary formula -- lme4's random-effects bar syntax
#' (`(1 | group)`) is not a column reference and must never be passed through
#' this quoting, so the mixed-effects call site joins this function's output
#' with its bars as plain text rather than asking `.safe_predictor_formula()`
#' to build a two-sided formula and disassembling it back into parts.
#'
#' @param names_ character vector of column names.
#' @return a single string, e.g. `` "`a` + `b` + `c:d`" ``.
#' @noRd
.backtick_join <- function(names_) {
  paste(paste0("`", gsub("`", "\\`", names_, fixed = TRUE), "`"), collapse = " + ")
}

#' Look up a node variable's values at one wave, keyed by (group, node)
#'
#' Static variables have no wave column in the reconstructed node table
#' -- the schema stores static columns once rather than at every wave. This
#' returns the same
#' value regardless of `wave` in that case, matching "static data is
#' first-class" rather than requiring the caller to special-case it.
#'
#' @noRd
.lookup_wave_values <- function(nodes, schema, var, wave) {
  .assert_columns_present(nodes, var, "node data")
  # A NULL wave_col means the data has no wave dimension at all, which takes
  # the same branch as a static variable: one value per unit, the same at every
  # occasion. Guarded explicitly because `NULL %in% x` is logical(0), not FALSE,
  # and `if` errors on it rather than falling through.
  if (!is.null(schema$wave_col) && schema$wave_col %in% names(nodes)) {
    out <- nodes[nodes[[schema$wave_col]] == wave, c(schema$group_col, schema$node_col, var)]
  } else {
    out <- nodes[, c(schema$group_col, schema$node_col, var)]
  }
  rownames(out) <- NULL
  out
}

#' Look up a group variable's values at one wave, keyed by group
#'
#' The group-level counterpart to .lookup_wave_values() -- used by
#' lagged()'s group-level materialization. Returned as one row per group;
#' the caller's merge(by = group_col) is what fans it out to every member
#' node, not this function.
#'
#' @noRd
.lookup_wave_values_group <- function(groups, schema, var, wave) {
  .assert_columns_present(groups, var, "group data")
  # A NULL wave_col means the data has no wave dimension at all, which takes
  # the same branch as a static variable: one value per unit, the same at every
  # occasion. Guarded explicitly because `NULL %in% x` is logical(0), not FALSE,
  # and `if` errors on it rather than falling through.
  if (!is.null(schema$wave_col) && schema$wave_col %in% names(groups)) {
    out <- groups[groups[[schema$wave_col]] == wave, c(schema$group_col, var)]
  } else {
    out <- groups[, c(schema$group_col, var)]
  }
  rownames(out) <- NULL
  out
}

#' Resolve whether a lagged() variable is node- or group-level
#'
#' netis_data's schema already records which variables are node-level and which
#' are group-level, so lagged() does not need separate
#' lagged_node()/lagged_group() spellings -- this is the single place that
#' answers "which one." Tie-level lagging is not supported and errors
#' clearly rather than falling through to a confusing downstream failure.
#'
#' Bare covariates resolve through the same function, so a group-level
#' covariate named directly in a formula is looked up on the group table just
#' as `lagged()` of the same name is. One level resolver, both spellings.
#'
#' @param context how the variable was written, for error messages --
#'   `"lagged(x)"` or just `"x"`.
#' @return `"node"` or `"group"`.
#' @noRd
.resolve_lagged_level <- function(schema, var, context = sprintf("lagged(%s)", var)) {
  in_node  <- var %in% c(schema$node_vars$static, schema$node_vars$dynamic)
  in_group <- var %in% c(schema$group_vars$static, schema$group_vars$dynamic)
  in_tie   <- var %in% c(schema$tie_vars$static, schema$tie_vars$dynamic)
  if (in_node && in_group) {
    stop(
      sprintf(
        "%s: '%s' exists as both a node variable and a group variable in this netis_data object, so rnetis cannot tell which level you mean. Rename one of them.",
        context, var
      ),
      call. = FALSE
    )
  }
  if (in_node) {
    return("node")
  }
  if (in_group) {
    return("group")
  }
  if (in_tie) {
    stop(
      sprintf(
        "%s: '%s' is a tie-level variable. Tie-level variables are not supported as model predictors.",
        context, var
      ),
      call. = FALSE
    )
  }
  stop(
    sprintf(
      "%s: '%s' was not found as a node or group variable in this netis_data object.",
      context, var
    ),
    call. = FALSE
  )
}

#' Assemble the analysis table for fit_influence() and build its lm() formula
#'
#' One row per (group, node) with the outcome at the focal wave, ar() lag
#' columns, lagged() columns (node- or group-level, resolved from the schema),
#' exposure() columns (via the same compute_exposure() engine used standalone),
#' covariates at the focal wave, and moderated-interaction product
#' columns, built last because they read from columns every earlier loop
#' produced. Rows with any missing predictor are handled by the backend's own
#' default missing-data behavior (stats::lm()'s na.omit), not silently imputed
#' here.
#'
#' @noRd
.assemble_influence_table <- function(parsed, data, waves) {
  s <- netis_schema(data)
  # The outcome wave is the LAST analyzed wave: `to` of a transition, the one
  # wave of a single-occasion fit, and NULL for data with no wave dimension.
  to_wave <- if (length(waves) > 0L) waves[[length(waves)]] else NULL
  # Every lag steps back from `to` through this sequence, so a lag of 1 reads
  # `from` of the named transition (see .lag_sequence()).
  lag_seq <- .lag_sequence(s$waves, waves)
  # Every wave a term reads, so the fit can report the waves it did not use.
  waves_read <- to_wave
  nodes <- netis_nodes(data)

  # Every column the assembled table will gain, computed up front so a name
  # collision (e.g. a covariate that happens to share a name with an ar() or
  # exposure() column) is caught with a clear message before any merge()
  # runs -- merge() itself would silently produce ".x"/".y"-suffixed columns
  # instead, which then fails confusingly downstream when lm_formula
  # references a name that no longer exists.
  #
  # ar()/lagged() naming is "<var>_lag<k>" unconditionally -- one convention
  # for single and multiple lags, which also cannot collide with a bare
  # focal-wave covariate of the same name.
  ar_colnames <- unlist(lapply(parsed$ar, function(a) paste0(a$variable, "_lag", a$lag)))
  lagged_colnames <- unlist(lapply(parsed$lagged, function(l) paste0(l$variable, "_lag", l$lag)))
  exposure_colnames <- vapply(parsed$exposures, function(e) e$name, character(1))
  centrality_colnames <- vapply(parsed$centralities, function(cc) cc$name, character(1))
  network_stat_colnames <- vapply(parsed$network_stats, function(nn) nn$name, character(1))
  # Computed the same way the product-construction loop below names its
  # columns, so a collision is caught before any merge runs rather than
  # surfacing as a silently overwritten column later.
  interaction_colnames <- vapply(parsed$interactions, .interaction_product_colname, character(1))
  all_colnames <- c(
    ar_colnames, lagged_colnames, exposure_colnames,
    centrality_colnames, network_stat_colnames, parsed$covariates, interaction_colnames
  )
  if (anyDuplicated(all_colnames) > 0L) {
    stop(
      sprintf(
        "Two or more terms would produce the same column name: %s. Rename the colliding ar()/lagged()/exposure()/latent()/centrality()/network_stat()/interaction term(s) or covariate.",
        paste(unique(all_colnames[duplicated(all_colnames)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  reserved <- intersect(all_colnames, c(s$group_col, s$node_col, ".outcome"))
  if (length(reserved) > 0L) {
    stop(
      sprintf(
        "The column name(s) %s are reserved: the estimating table uses them for its group, node and outcome columns, so a term with that name would be overwritten or lost. Give the term another name.",
        paste(sprintf("'%s'", reserved), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  outcome_tbl <- .lookup_wave_values(nodes, s, parsed$outcome, to_wave)
  if (!is.numeric(outcome_tbl[[3L]])) {
    stop(
      sprintf(
        "The outcome '%s' is %s, not numeric. fit_influence() fits a linear model of a numeric outcome. Convert it to a numeric column deliberately (a 0/1 column gives a linear probability model), or use fit_selection() for a tie outcome.",
        parsed$outcome, class(outcome_tbl[[3L]])[1L]
      ),
      call. = FALSE
    )
  }
  names(outcome_tbl)[3] <- ".outcome"
  table <- outcome_tbl

  for (ar_term in parsed$ar) {
    for (lag in ar_term$lag) {
      lag_wave <- .influence_lag_wave(
        s, to_wave, lag,
        sprintf("ar(lag = %d) (the outcome, '%s')", lag, ar_term$variable),
        sequence = lag_seq
      )
      waves_read <- c(waves_read, lag_wave)
      col_tbl <- .lookup_wave_values(nodes, s, ar_term$variable, lag_wave)
      colname <- paste0(ar_term$variable, "_lag", lag)
      names(col_tbl)[3] <- colname
      table <- merge(table, col_tbl, by = c(s$group_col, s$node_col), all.x = TRUE)
    }
  }

  for (lag_term in parsed$lagged) {
    level <- .resolve_lagged_level(s, lag_term$variable)
    for (lag in lag_term$lag) {
      lag_wave <- .influence_lag_wave(
        s, to_wave, lag, sprintf("lagged(%s, lag = %d)", lag_term$variable, lag),
        sequence = lag_seq
      )
      waves_read <- c(waves_read, lag_wave)
      colname <- paste0(lag_term$variable, "_lag", lag)
      if (identical(level, "node")) {
        col_tbl <- .lookup_wave_values(nodes, s, lag_term$variable, lag_wave)
        names(col_tbl)[3] <- colname
        table <- merge(table, col_tbl, by = c(s$group_col, s$node_col), all.x = TRUE)
      } else {
        groups_data <- netis_groups(data)
        if (is.null(groups_data)) {
          stop(
            sprintf(
              "lagged(%s): '%s' resolved as a group-level variable, but this netis_data object has no group_data to look it up in.",
              lag_term$variable, lag_term$variable
            ),
            call. = FALSE
          )
        }
        col_tbl <- .lookup_wave_values_group(groups_data, s, lag_term$variable, lag_wave)
        names(col_tbl)[2] <- colname
        # Merge by group_col alone (not node_col): this is the fan-out that
        # gives every member of the group the same group-level lagged value,
        # a many-to-one join rather than the one-to-one node lookups above.
        table <- merge(table, col_tbl, by = s$group_col, all.x = TRUE)
      }
    }
  }

  resolved_exposures <- list()
  for (exp_term in parsed$exposures) {
    # x_lag and w_lag resolve INDEPENDENTLY, each a positional step count
    # through the observed wave sequence, so unequal lags are supported.
    w_lag <- if (is.null(exp_term$w_lag)) exp_term$x_lag else exp_term$w_lag
    exp_desc <- sprintf("exposure(%s, via = %s, x_lag = %d)",
                        exp_term$content, .conduit_label(exp_term$conduit), exp_term$x_lag)
    exposure_wave <- .influence_lag_wave(s, to_wave, exp_term$x_lag, exp_desc, sequence = lag_seq)
    conduit_wave <- if (identical(w_lag, exp_term$x_lag)) {
      exposure_wave
    } else {
      .influence_lag_wave(
        s, to_wave, w_lag,
        sprintf("exposure(%s, via = %s, w_lag = %d)", exp_term$content, .conduit_label(exp_term$conduit), w_lag),
        sequence = lag_seq
      )
    }
    # Which wave compute_exposure() is resolved AT. For an ordinary mechanism
    # that is the content wave itself. A history mechanism such as bi() is
    # different: resolved at wave t it already reads only the waves BEFORE t
    # (content and networks at t-1, t-2, ...), so it is resolved one step
    # later than the content wave -- at the outcome wave for the default
    # x_lag = 1. Resolving it at the content wave would drop the most recent
    # transition and leave a two-wave fit with no history at all.
    resolve_wave <- exposure_wave
    resolve_conduit <- conduit_wave
    if (.mechanism_reads_history(exp_term$mechanism)) {
      .assert_history_exposure_timing(exp_term, s$waves, waves, exp_desc)
      resolve_wave <- .influence_lag_wave(
        s, to_wave, exp_term$x_lag - 1L, exp_desc, sequence = lag_seq
      )
      resolve_conduit <- NULL
      waves_read <- c(waves_read, .waves_before(s$waves, resolve_wave))
    } else {
      waves_read <- c(waves_read, exposure_wave, conduit_wave)
    }

    # A name the user wrote also prefixes a multi-column mechanism's columns; a
    # generated one is the same text and leaves the mechanism's own names alone.
    exp_result <- compute_exposure(
      data, content = exp_term$content, via = exp_term$conduit,
      name = if (isTRUE(exp_term$name_given)) exp_term$name else NULL,
      wave = resolve_wave, conduit_wave = resolve_conduit,
      mechanism = exp_term$mechanism, normalize = exp_term$normalize,
      direction = exp_term$direction, isolate_rule = exp_term$isolate_rule
    )
    resolved_exposures[[exp_term$name]] <- list(
      term = exp_term, exposure_wave = exposure_wave, conduit_wave = conduit_wave,
      # Which groups' exposures are NA because the NETWORK was unobserved,
      # rather than because an alter's value was. The two have different causes
      # and different fixes, and the balance diagnostic reports them
      # separately. Captured
      # here because merging the exposure result into the table drops its
      # attributes.
      absent_network_groups = attr(exp_result, "absent_network_groups"),
      absent_network_history_groups = attr(exp_result, "absent_network_history_groups")
    )
    # The column-name check above knows each term's `name`, and a multi-column
    # mechanism names its own columns, so two of them are caught here.
    clash <- intersect(setdiff(names(exp_result), c(s$group_col, s$node_col)), names(table))
    if (length(clash) > 0L) {
      stop(
        sprintf(
          "Two or more terms would produce the same column name: %s. A mechanism with several columns names them itself; give each such term a distinct name =, which prefixes its columns.",
          paste(clash, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    table <- merge(table, exp_result, by = c(s$group_col, s$node_col), all.x = TRUE)
  }


  resolved_centralities <- list()
  for (cent_term in parsed$centralities) {
    cent_wave <- .influence_lag_wave(
      s, to_wave, cent_term$lag,
      sprintf("centrality(measure = %s, lag = %d)", cent_term$measure, cent_term$lag),
      sequence = lag_seq
    )
    waves_read <- c(waves_read, cent_wave)
    cent_spec <- centrality_(
      cent_term$network, cent_term$measure, cent_term$mode, cent_term$weights,
      cent_term$weight_transform, cent_term$name, cent_term$lag
    )
    cent_result <- .resolve_netfeat(cent_spec, data, cent_wave)
    resolved_centralities[[cent_term$name]] <- list(term = cent_term, feature_wave = cent_wave)
    table <- merge(table, cent_result, by = c(s$group_col, s$node_col), all.x = TRUE)
  }

  # XU'S LATENT-SPACE ADJUSTMENT (R/latent-xu.R). Resolved beside the other
  # network-derived terms because that is what it is from the assembly's point
  # of view: a node-level column keyed by (group, node). What makes it
  # different is upstream -- the column comes from a fitted selection model
  # rather than from a graph computation -- and that provenance is recorded so
  # summary() can state it.
  resolved_latent <- list()
  for (lat_term in parsed$latent) {
    lat_wave <- .influence_lag_wave(
      s, to_wave, lat_term$lag,
      sprintf("latent(d = %d, lag = %d)", lat_term$d, lat_term$lag),
      sequence = lag_seq
    )
    waves_read <- c(waves_read, lat_wave)
    lat_result <- .resolve_latent_positions(lat_term, data, s, lat_wave)
    resolved_latent[[lat_term$name]] <- list(
      term = lat_term, feature_wave = lat_wave,
      selection_formula = lat_result$formula, n_actors = lat_result$n_actors,
      seed = lat_result$seed, edges = lat_result$edges
    )
    table <- merge(table, lat_result$columns, by = c(s$group_col, s$node_col),
                   all.x = TRUE)
  }
  if (any(vapply(resolved_latent, function(r) length(r$n_actors) > 1L, logical(1)))) {
    .warn_multigroup_latent()
  }

  resolved_network_stats <- list()
  for (net_term in parsed$network_stats) {
    net_wave <- .influence_lag_wave(
      s, to_wave, net_term$lag,
      sprintf("network_stat(measure = %s, lag = %d)", net_term$measure, net_term$lag),
      sequence = lag_seq
    )
    waves_read <- c(waves_read, net_wave)
    net_spec <- network_stat_(
      net_term$network, net_term$measure, net_term$weights,
      net_term$weight_transform, net_term$name, net_term$lag, summary = net_term$summary
    )
    net_result <- .resolve_netfeat(net_spec, data, net_wave)
    resolved_network_stats[[net_term$name]] <- list(term = net_term, feature_wave = net_wave)
    # Fan-out join: one row per group in net_result, merged onto every member
    # row by group_col alone -- the same many-to-one pattern lagged()'s
    # group-level branch uses, since network_stat() is a group-level
    # predictor by construction.
    table <- merge(table, net_result, by = s$group_col, all.x = TRUE)
  }

  # A bare covariate is measured at the focal (to) wave, matching the
  # outcome -- the "x at the same wave as y" reading. A prior-wave reading is
  # written lagged(x), so both readings are available in the formula.
  #
  # A covariate may live at either level, and which one is a fact the schema
  # already records -- so it is resolved rather than assumed, through the
  # same .resolve_lagged_level() that `lagged()` uses. Reading the node table
  # unconditionally would make a group-level covariate unusable as a bare
  # predictor. A group-level value is merged by group alone, the same many-to-one fan-out
  # the lagged() branch performs, so every member of a group receives its
  # group's value.
  for (cov in parsed$covariates) {
    level <- .resolve_lagged_level(s, cov, context = cov)
    if (identical(level, "node")) {
      col_tbl <- .lookup_wave_values(nodes, s, cov, to_wave)
      table <- merge(table, col_tbl, by = c(s$group_col, s$node_col), all.x = TRUE)
    } else {
      groups_data <- netis_groups(data)
      if (is.null(groups_data)) {
        stop(
          sprintf(
            "'%s' resolved as a group-level variable, but this netis_data object has no group data to look it up in.",
            cov
          ),
          call. = FALSE
        )
      }
      col_tbl <- .lookup_wave_values_group(groups_data, s, cov, to_wave)
      table <- merge(table, col_tbl, by = s$group_col, all.x = TRUE)
    }
  }

  # Moderation. THE PRODUCT IS FORMED HERE, AFTER EVERY MAIN-EFFECT LOOP,
  # deliberately: a moderated exposure() column does not exist in the input
  # data at all, only in `table` once the exposure loop above has built it, so
  # this cannot be expressed as an ordinary model.matrix() interaction over the
  # raw formula. The moderator is always an ordinary covariate and both
  # operands are guaranteed present by this point: hierarchy puts the
  # moderand's own main effect through its own loop above, and the moderator's
  # main effect through the covariates loop just above. Missingness needs no
  # special handling -- `NA * x` is `NA`, and this column becomes an ordinary
  # member of `predictor_cols`/`lm_formula` below, so the backend's own na.omit
  # drops the row exactly as it would for any other missing predictor.
  for (i in seq_along(parsed$interactions)) {
    inter <- parsed$interactions[[i]]
    moderand_col <- .interaction_moderand_colname(inter)
    if (identical(inter$moderand_kind, "exposure") && !(moderand_col %in% names(table))) {
      stop(
        sprintf(
          "The exposure term '%s' produces several columns (its mechanism has more than one), and a mechanism with several columns cannot be moderated: there is no one column for the moderator to multiply. Use a mechanism with one column, such as bi(), or add the product you want to the node data as a covariate.",
          moderand_col
        ),
        call. = FALSE
      )
    }
    .assert_numeric_interaction_operand(table[[moderand_col]], moderand_col, interaction_colnames[i])
    .assert_numeric_interaction_operand(table[[inter$moderator]], inter$moderator, interaction_colnames[i])
    table[[interaction_colnames[i]]] <- table[[moderand_col]] * table[[inter$moderator]]
  }

  predictor_cols <- setdiff(names(table), c(s$group_col, s$node_col, ".outcome"))
  if (length(predictor_cols) == 0L) {
    stop("fit_influence() found no usable predictor columns after assembly. A simultaneous exposure term (x_lag = 0 on the outcome) enters through its network, not as a column, so the formula also needs another predictor, such as ar() or a covariate.", call. = FALSE)
  }
  lm_formula <- .safe_predictor_formula(predictor_cols)

  list(
    table = table, lm_formula = lm_formula, predictor_cols = predictor_cols,
    resolved_exposures = resolved_exposures,
    resolved_centralities = resolved_centralities,
    resolved_latent = resolved_latent,
    resolved_network_stats = resolved_network_stats,
    waves_read = s$waves[as.character(s$waves) %in% as.character(waves_read)]
  )
}

#' Does an exposure mechanism read network and content history?
#' @noRd
.mechanism_reads_history <- function(mechanism) {
  is.function(mechanism) &&
    any(c("w_history", "x_history") %in% attr(mechanism, "requires"))
}

#' Refuse timings a history mechanism has no reading for
#'
#' A history mechanism (e.g. [bi()]) resolved for an outcome at wave `t` reads
#' content and networks at every observed wave before `t`. Three timings have
#' no reading under that definition and are refused rather than given one:
#' `x_lag = 0` (the mechanism never reads the outcome wave's content), a
#' separate `w_lag` (the mechanism decides which networks enter), and a named
#' transition that skips waves (the accumulation would pass through the skipped
#' waves the fit reports as unused).
#' @noRd
.assert_history_exposure_timing <- function(exp_term, observed, waves, exp_desc) {
  if (identical(as.integer(exp_term$x_lag), 0L)) {
    stop(
      sprintf(
        "%s: a history mechanism such as bi() accumulates content and networks from the waves BEFORE the outcome wave, so x_lag = 0 has no reading for it. Use x_lag = 1 (the default): the most recent content and network it reads are then one wave before the outcome, as in Frank and Fahrbach's equation (5).",
        exp_desc
      ),
      call. = FALSE
    )
  }
  if (!is.null(exp_term$w_lag) && !identical(as.integer(exp_term$w_lag), as.integer(exp_term$x_lag))) {
    stop(
      sprintf(
        "%s: a history mechanism such as bi() decides for itself which networks enter the accumulation, so a separate w_lag has no meaning for it. Remove w_lag.",
        exp_desc
      ),
      call. = FALSE
    )
  }
  if (!.waves_adjacent(observed, waves)) {
    pos <- match(as.character(waves), as.character(observed))
    skipped <- observed[seq(pos[1] + 1L, pos[2] - 1L)]
    stop(
      sprintf(
        "%s: a history mechanism such as bi() accumulates through every observed wave before the outcome, so it cannot be fitted over waves = c(%s, %s), a transition that skips wave(s) %s. Name an adjacent transition, or drop the skipped waves from the data.",
        exp_desc, waves[[1L]], waves[[2L]], paste(skipped, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Refuse a network_stat() column with no variation to identify against
#'
#' A group-level predictor resolved to a single constant value across the
#' assembled table is perfectly collinear with the intercept -- aliased, not
#' merely weak. This is a **general variation check on the resolved column**,
#' not a rule keyed to the number of groups: a single-group fit gives a
#' constant column and errors here; a multi-group fit with genuinely differing
#' group values passes.
#'
#' @noRd
.assert_network_stat_identified <- function(values, term_name) {
  vals <- values[!is.na(values)]
  if (length(vals) > 0L && length(unique(vals)) <= 1L) {
    stop(
      sprintf(
        "network_stat() term '%s' has no variation across the estimating sample (a single constant value) -- perfectly collinear with the intercept, not merely weak. A group-level predictor resolved at one wave is estimable only across several groups with differing values, so add groups or drop the term. Its inferential information is carried by the number of groups, not the number of rows.",
        term_name
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' The Wald critical value a fit's inference is based on
#'
#' Split out of `.compute_influence_effects()` so `.compute_ar_effects()` uses
#' the identical value rather than recomputing it and risking the two effect
#' layers reporting intervals on a different basis for the same fit.
#'
#' `extracted$residual_df` here is the INFERENCE degrees of freedom from
#' `.influence_inference_df()`, not the backend's residual degrees of freedom:
#' `NA` (a normal quantile) for the lme4 and lnam backends.
#'
#' @noRd
.effects_critical_value <- function(extracted) {
  .netis_wald_critical(extracted$residual_df, level = 0.95)
}

#' The degrees of freedom a fit's Wald interval uses
#'
#' Which reference distribution depends on what inference the backend itself
#' supports, not on a house convention:
#'
#' \itemize{
#'   \item `stats` (ordinary least squares): the exact t distribution on the
#'     residual degrees of freedom, as `stats::confint.lm()` uses.
#'   \item `lme4`: normal HERE, which is the basis for a linear combination of
#'     coefficients (a moderated term's conditional table and its
#'     Johnson-Neyman boundaries). A single coefficient's interval uses
#'     Satterthwaite's degrees of freedom instead: see `.influence_coef_df()`.
#'     lme4's `df.residual()` is the conditional `n - p`, which is not a
#'     degrees of freedom for a fixed-effect test.
#'   \item `lnam`: normal. `sna::lnam()` estimates by maximum likelihood and
#'     its inference is asymptotically normal (its `summary()` reports z
#'     values).
#' }
#'
#' @param backend the fit's backend name.
#' @param residual_df the backend's residual degrees of freedom.
#' @return a single number, `NA` meaning "use the normal quantile".
#' @noRd
.influence_inference_df <- function(backend, residual_df) {
  if (identical(backend, "stats") && length(residual_df) == 1L) {
    residual_df
  } else {
    NA_real_
  }
}

#' The degrees of freedom each coefficient's own interval and test use
#'
#' ONE PLACE, so a coefficient's printed interval and its printed test cannot
#' sit on different reference distributions:
#'
#' \itemize{
#'   \item `stats`: the residual degrees of freedom, the same for every
#'     coefficient.
#'   \item `lme4`: Satterthwaite's degrees of freedom, one per coefficient, as
#'     lmerTest reports them (`coef_tests`). They differ by coefficient -- a
#'     group-level predictor has roughly the number of groups behind it, a
#'     row-level one the number of rows -- which is why a single value cannot
#'     stand in. `NA` (the normal quantile) where the fit carries none.
#'   \item `lnam`: `NA`, the normal quantile.
#' }
#'
#' This is per COEFFICIENT. A linear combination of coefficients -- a moderated
#' term's conditional effect -- has its own Satterthwaite degrees of freedom,
#' which this does not supply; `.influence_inference_df()` remains the basis
#' there.
#'
#' @param backend the fit's backend name.
#' @param residual_df the backend's residual degrees of freedom.
#' @param coef_tests the fit's `coef_tests` (lme4 route), or `NULL`.
#' @param nms the coefficient names.
#' @return a numeric vector named by `nms`; `NA` means the normal quantile.
#' @noRd
.influence_coef_df <- function(backend, residual_df, coef_tests, nms) {
  out <- stats::setNames(rep(NA_real_, length(nms)), nms)
  if (identical(backend, "stats") && length(residual_df) == 1L) {
    out[] <- residual_df
  } else if (identical(backend, "lme4") && !is.null(coef_tests)) {
    out[] <- coef_tests$df[match(nms, coef_tests$term)]
  }
  out
}

#' `.netis_wald_critical()` over a vector of degrees of freedom
#' @noRd
.netis_wald_critical_each <- function(df, level = 0.95) {
  vapply(df, function(d) .netis_wald_critical(d, level), numeric(1))
}

#' The critical value for ONE coefficient's interval in the effects layer
#'
#' The unmoderated exposure entry reports the coefficient's own interval, so it
#' uses the coefficient's own degrees of freedom and agrees with `confint()`.
#' `extracted` is either a backend extraction (fit time) or the list
#' `influence_effect()` assembles; both carry `backend_name` and `coef_tests`
#' where the route has them, and fall back to the shared critical value.
#'
#' @noRd
.effects_coef_critical <- function(extracted, nm) {
  ct <- extracted$coef_tests
  if (!is.null(ct) && nm %in% ct$term) {
    return(.netis_wald_critical(ct$df[match(nm, ct$term)], level = 0.95))
  }
  .effects_critical_value(extracted)
}

#' The two-sided Wald critical value for a fit's own inference basis
#'
#' ONE IMPLEMENTATION, TWO CALLERS. The effect-size layer and
#' `confint.netis_influence()` both need this rule, and two copies could give
#' different intervals for the same coefficient on the same fit. Whichever quantile is right, it has to be the same one in both
#' places. A saturated fit (`residual_df == 0`) is the degenerate case:
#' `stats::qt()` returns `NaN` with a "NaNs produced" warning where plain
#' `NA` is what the situation means, so it is short-circuited.
#'
#' @param residual_df the inference degrees of freedom, or `NA` for a normal
#'   quantile.
#' @param level the confidence level.
#' @noRd
.netis_wald_critical <- function(residual_df, level = 0.95) {
  p <- 1 - (1 - level) / 2
  if (is.null(residual_df) || length(residual_df) != 1L || is.na(residual_df)) {
    stats::qnorm(p)
  } else if (residual_df > 0) {
    stats::qt(p, df = residual_df)
  } else {
    NA_real_
  }
}

