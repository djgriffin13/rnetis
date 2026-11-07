# fit_selection().
#
# The integration point: parse the formula, diagnose the design, resolve the
# route from the data, assemble one design per (group, outcome wave), fit
# through the adapter, and return an object with the fields every rnetis fit
# carries.
#
# With `pooling = NULL`, multi-group data is fitted ONE GROUP AT A TIME and the
# coefficients are then
# combined by inverse-variance weighting (see selection-pool-beta.R). Two
# stages, not one chain: no group's fit informs another's, and the combination
# happens entirely after the fact.
#
# Pooling is ON by default because a shared beta is what a multi-group
# specification usually means, and `pool_beta = FALSE` returns the per-group
# fits uncombined. Whichever is in effect is stated in plain language wherever
# results are printed -- a list of unpooled fits that looks like a pooled
# result, or a pooled result that looks like a joint model, are the two most
# likely ways for this code to mislead someone.

#' Fit a Selection model
#'
#' Models how ties form, persist, and dissolve as a function of sender,
#' receiver, dyadic, and prior-tie effects, using an additive and multiplicative
#' effects (AME) model (Hoff, 2005, 2021).
#'
#' @section Estimator:
#' One observed network per group, fitted by `amen::ame()`; or, with `pooling`,
#' every group in one chain by this package's hierarchical sampler (see
#' [hierarchical_selection]). This package does not fit replicated designs (see
#' the **Waves** section).
#'
#' Temporal dependence enters through [prior_tie()], [prior_reciprocity()],
#' and time-varying predictors. A lagged tie **controls persistence and some
#' sources of temporal dependence**; it does not by itself resolve endogeneity
#' in general, and nothing here claims it does.
#'
#' @section Multiple groups:
#' With `pooling = NULL` every group is fitted on its own. By default
#' (`pool_beta = TRUE`) the per-group coefficients are then combined into one
#' common coefficient vector by **two-stage common-beta precision pooling**: each
#' group contributes its posterior mean weighted by the inverse of its posterior
#' variance. This is fixed-effect inverse-variance meta-analysis under a normal
#' approximation (Cochran, 1954; Hedges & Olkin, 1985), and it reproduces the
#' combination rule of `lame::lame_multi()` (Dorff, Minhas & Salau, 2026). That
#' is a pooling algorithm, not an exact joint Bayesian posterior: each group's fit
#' carries its own prior, so treating the posteriors as independent likelihood
#' contributions would count that prior once per group. A
#' variance below `1e-8` is raised to `1e-8` to keep its inverse computable, a
#' package convention, and `summary()` names any coefficient this touches. A zero
#' or non-finite variance is refused.
#'
#' Two stages, not one chain. No group's fit informs another's; the groups are
#' combined only after all of them are estimated. Everything other than the
#' coefficients stays group-specific -- each group keeps its own
#' sender/receiver covariance, dyadic variance and correlation, and
#' multiplicative structure.
#'
#' Three consequences the caller has to know; `summary()` states the first two:
#'
#' * The common coefficient assumes beta **means the same thing in every
#'   group**. There is no between-group variance component, so heterogeneity in
#'   beta is assumed away rather than estimated. This is not hierarchical
#'   partial pooling, which would give each group its own coefficient shrunk
#'   toward a common distribution and is a different model.
#' * Pooling is **coefficient-wise**, so no covariance between coefficients
#'   survives it. Per-coefficient intervals are valid; a contrast, a
#'   multi-coefficient Wald test, or a joint credible region is not, and
#'   [vcov()] therefore has nothing to return for a pooled multi-group fit.
#' * The pooling is a sum over groups, so the **order the groups appear in the
#'   data cannot change the result**.
#'
#' `pool_beta = FALSE` leaves the groups uncombined: `coefficients` is a matrix
#' of per-group rows, which must not be read as though it came from a joint
#' model.
#'
#' @section Groups that cannot be fitted:
#' With `pooling = NULL` a group is refused, by name, when it has fewer than 3
#' actors; when its binary ties have no variation; when every actor sends the same
#' number of ties and receives the same number; when a predictor is constant
#' across the whole group; or when `amen::ame()` would start its dyadic correlation
#' at +/-1. Every one of these comes from how `amen::ame()` starts or runs. A group
#' with fewer than 8 actors is fitted with a WARNING reporting its actor and dyad
#' counts: 8 is a package convention, not a statistical result.
#'
#' @section Waves:
#' At most one wave ever supplies the outcome. Two or more outcome waves is
#' replication, which treats repeated observations of the same actors as
#' independent given the AME structure; they are not, and this package refuses
#' it rather than fitting it. That refusal is why `amen::ame_rep()` is not
#' used here.
#'
#' * **One wave** is fitted as a single cross-section.
#' * **Two waves** with [prior_tie()] or [prior_reciprocity()]: the earlier
#'   wave is the initial condition, the later one is the modeled outcome.
#'   Without a lagged relation term, both waves would supply an outcome, and
#'   that is refused by name unless `waves = c(from, to)` names the transition.
#' * **Three or more waves** is an error unless `waves = c(from, to)` names
#'   exactly two. `to` is the modeled outcome, and a lag of 1 reads `from`,
#'   also when the two are not adjacent: `waves = c(1, 3)` models wave 3 with
#'   wave 1 as its prior network, and wave 2 is not read. A deeper lag steps
#'   back from `from` through the earlier observed waves, and is refused when
#'   there are none. Every wave a term reads counts as analyzed.
#'
#' `summary()` reports which waves were analyzed **and how many were present
#' but not used**, so a fit over one transition of a ten-wave panel cannot
#' read as though ten waves were modeled.
#'
#' @section Missing data:
#' A dyad whose outcome or required predictors cannot be resolved is excluded
#' from the outcome rather than being given a filled-in value, and the counts
#' are reported. Excluded dyads need not be a random subset, so check whether
#' they concentrate in particular actors or waves.
#'
#' Self-ties are never part of an AME model. On data that permits them
#' (`netis_data(self_ties = "permitted")`), recorded self-ties are left out of
#' the fit, a WARNING reports how many, and nothing counts them as dyads.
#'
#' Design checks read the waves the fit reads: an actor who appears only at a
#' wave `waves =` leaves out is not on the fitted roster, and a roster that
#' changes only outside the named waves is not refused.
#'
#' @param formula a Selection formula: a tie variable on the left, and
#'   [sender()], [receiver()], [dyad()], [prior_tie()], or
#'   [prior_reciprocity()] terms on the right. Every predictor carries an
#'   explicit role; a bare covariate is an error. Predictors must be numeric or
#'   logical: a factor or character covariate is refused (encode it as
#'   indicator columns). A [group()] term is refused:
#'   it is constant within each group's fitted network. Enter a group-level
#'   predictor as a moderator of a random intercept instead (see
#'   [selection_pooling()]).
#' @param symmetric `TRUE` fits the relation as symmetric (see below). Data
#'   whose ties are declared undirected must be fitted with `symmetric = TRUE`;
#'   fitting them as directed is refused.
#' @param data a [netis_data()] object.
#' @param family `"bin"` for a binary tie (probit) or `"nrm"` for a valued one
#'   (normal). Other AME families are not supported in this version.
#' @param waves for data with more than two waves, `c(from, to)` naming
#'   exactly the two to analyze: `to` supplies the outcome and a lag of 1
#'   reads `from`, also when the two are not adjacent. A deeper lag steps back
#'   from `from` through the earlier observed waves; the waves between `from`
#'   and `to` are never read. `NULL` (the default) is correct for one- and
#'   two-wave data. rnetis will not choose a transition out of a longer panel
#'   on its own -- see the **Waves** section.
#' @param binarize a binarization rule ([nonzero()], [positive()],
#'   [threshold()], [abs_threshold()], or [custom_binarize_rule()]) applied to
#'   a valued outcome under `family = "bin"`. Required in that case and
#'   recorded in the fit: rnetis will not choose a cutpoint for you. A rule that
#'   computes its cutpoint from the data is refused. The same
#'   rule is applied to the earlier wave read by [prior_tie()] and
#'   [prior_reciprocity()].
#' @param R number of multiplicative (latent factor) dimensions. Default `0`.
#'   `R > 0` is an additional modeling assumption and is an explicit choice;
#'   rnetis never selects it from the data.
#' @param pool_beta whether to combine the per-group coefficients into one
#'   common vector by inverse-variance weighting. `TRUE` (the default) pools;
#'   `FALSE` returns the groups uncombined. Only the coefficients are ever
#'   pooled -- the actor and dyadic variance components stay group-specific
#'   either way. Has no effect on single-group data, where pooling is the
#'   identity. Supplying it together with `pooling`, which fits a different model,
#'   is an error. See the **Multiple groups** section for what pooling assumes.
#' @section Symmetric relations:
#' `symmetric = TRUE` fits the relation as symmetric: one additive actor
#'   effect instead of separate sender and receiver effects, and one
#'   coefficient per nodal covariate. Requires the data's ties to be declared
#'   undirected in [netis_data()] -- fitting a directed-declared relation
#'   symmetrically would silently discard its asymmetry. On a symmetric fit
#'   [sender()] declares the **nodal** covariate (there is no sender/receiver
#'   distinction; the assembled column is named `nod_<var>` and its role
#'   prints as `node`), and [receiver()] and [prior_reciprocity()] are
#'   refused by name -- the reverse tie *is* the tie. The hierarchical route
#'   (`pooling`) does not fit symmetric relations. Conversely, undirected data with `symmetric = FALSE` is an
#'   error on either route: the directed model would count each undirected tie
#'   as two dyads with separate sender and receiver effects.
#' @param pooling `NULL` (the default) fits each group on its own, with
#'   optional two-stage precision pooling through `pool_beta`. A
#'   [selection_pooling()] object instead fits **one hierarchical model over
#'   all groups in one chain**, in which each coefficient is shared,
#'   group-specific, or random -- drawn from a between-group distribution
#'   (random intercepts and random slopes, with a diagonal or unstructured
#'   between-group covariance and optional group-level moderators). The two
#'   routes answer different questions: `pool_beta` assumes heterogeneity away
#'   and combines afterwards, while a random coefficient estimates it. Each
#'   group keeps its own AME nuisance structure either way. See
#'   [hierarchical_selection] for the model. **Experimental interface.**
#' @param ... sampler controls: `nscan` (default `10000`), `burn` (`500`),
#'   `odens` (`25`) and `seed` (`1`), plus `gof` (`TRUE`) on the per-group route
#'   and `chains` (`1`) on the hierarchical one. Anything else
#'   is refused, including `amen::ame()` arguments such as `rvar`, `dcor` or
#'   `prior`, which would change the model the results describe. At least two
#'   draws must be kept. `seed` (default `1`) is the fit seed. On the per-group route each
#'   group is fitted with its own seed derived from it (`seed + 7919 * (k - 1)`
#'   for the `k`-th group in sorted order, so a single-group fit uses `seed`
#'   itself), recorded as `estimation_controls$group_seeds`. On the
#'   hierarchical route chain `k` uses `seed + k - 1`. Either way the caller's
#'   own random-number stream is left as it was: fitting does not reset it.
#' @return an object of class `netis_selection` (and `netis_fit`). Read it with
#'   [netis_estimates()], [netis_glance()] and [netis_diagnostics()]. Among its
#'   fields, `coefficients` and `vcov` are as [coef()] and [vcov()] return them,
#'   `pooled_beta` holds the pooled means, variances and precisions (`NULL` when
#'   nothing was pooled), `per_group` each group's coefficient draws and variance
#'   components, `convergence` each group's diagnostics (split R-hat and effective
#'   sample sizes; Vehtari et al., 2021), `waves` the waves read and
#'   left unread, and `estimation_controls` the settings the fit ran under. A
#'   hierarchical fit adds `hierarchical`, `convergence_table` and `tie_variation`.
#' @references
#' Cochran, W. G. (1954). The combination of estimates from different
#' experiments. *Biometrics*, 10(1), 101-129.
#'
#' Dorff, C., Minhas, S., & Salau, T. (2026). *lame: Longitudinal Additive and
#' Multiplicative Effects Models for Networks*. R package version 1.3.4.
#'
#' Hedges, L. V., & Olkin, I. (1985). *Statistical Methods for Meta-Analysis*.
#' Academic Press.
#'
#' Hoff, P. D. (2005). Bilinear mixed-effects models for dyadic data. *Journal of
#' the American Statistical Association*, 100(469), 286-295.
#'
#' Hoff, P. D. (2021). Additive and multiplicative effects network models.
#' *Statistical Science*, 36(1), 34-50.
#'
#' Hoff, P., Fosdick, B., & Volfovsky, A. (2024). *amen: Additive and
#' Multiplicative Effects Models for Networks and Relational Data*. R package
#' version 1.4.5.
#'
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C. (2021).
#' Rank-normalization, folding, and localization: An improved R-hat for
#' assessing convergence of MCMC. *Bayesian Analysis*, 16(2), 667-718.
#' @seealso [sender()], [prior_tie()], [netis_data()], [selection_pooling()],
#'   [hierarchical_selection], [selection_effect()], [pseudo_r2()],
#'   [fit_influence()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = letters[1:6], wave = rep(1:2, each = 6),
#'   tenure = c(1:6, 2:7)
#' )
#' pairs <- subset(expand.grid(source = letters[1:6], target = letters[1:6],
#'                             stringsAsFactors = FALSE), source != target)
#' ties <- rbind(
#'   data.frame(group = 1, pairs, wave = 1, friendship = 0:1),
#'   data.frame(group = 1, pairs, wave = 2, friendship = 1:0)
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave",
#'                  tie_meta = list(friendship = list(zero_meaning = "measured_zero")))
#' # Cost: an amen MCMC fit, over CRAN's example budget at any usable chain length.
#' \donttest{
#' fit <- fit_selection(friendship ~ sender(tenure) + prior_tie(), data = nd,
#'                      nscan = 100, burn = 50, odens = 10)
#' summary(fit)
#' }
#' @export
fit_selection <- function(formula, data, family = c("bin", "nrm"),
                          waves = NULL, binarize = NULL, R = 0,
                          pool_beta = TRUE, symmetric = FALSE,
                          pooling = NULL, ...) {
  call <- match.call()
  .check_netis_data(data)
  family <- match.arg(family)
  .assert_selection_R(R)
  # The two arguments choose between two different models. A `pool_beta` given
  # beside `pooling` would otherwise be ignored while the call reads as if it held.
  if (!missing(pool_beta) && !is.null(pooling)) {
    stop(
      "pool_beta and pooling were both supplied, and they select different models: pool_beta combines per-group fits afterwards, while pooling fits one hierarchical model over all groups. Remove pool_beta to fit the hierarchical model, or remove pooling to fit the groups separately.",
      call. = FALSE
    )
  }
  if (!is.logical(symmetric) || length(symmetric) != 1L || is.na(symmetric)) {
    stop("symmetric must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(binarize) && !identical(family, "bin")) {
    stop(
      "binarize is for turning a valued outcome into a tie under family = \"bin\" -- it has no defined meaning for family = \"nrm\", which models the outcome's own values. Either drop binarize and fit the values directly with family = \"nrm\", or set family = \"bin\" if you meant to model tie presence.",
      call. = FALSE
    )
  }

  spec <- .parse_selection_formula(formula)
  schema <- netis_schema(data)

  if (isTRUE(symmetric)) {
    spec <- .assert_symmetric_selection_spec(spec, schema)
  } else if (identical(schema$directed, FALSE)) {
    # An undirected relation stores each tie once. Fitting it as directed
    # would treat i -> j and j -> i as two observations of different ties.
    stop(
      if (is.null(pooling)) {
        "This data declares its ties undirected (netis_data(directed = FALSE)), so fit it with symmetric = TRUE. Fitting an undirected relation as directed would treat the two directions of one tie as two separate observations."
      } else {
        "This data declares its ties undirected (netis_data(directed = FALSE)). An undirected relation is fitted with symmetric = TRUE, which the hierarchical multi-group sampler (pooling = selection_pooling()) does not support in this version; fitting it as directed would treat the two directions of one tie as two separate observations. Fit the groups with pooling = NULL and symmetric = TRUE."
      },
      call. = FALSE
    )
  }

  all_groups <- sort(unique(as.character(netis_nodes(data)[[schema$group_col]])))

  # Which wave supplies the outcome is a property of the design, not of the
  # backend: every fit is one observed network per group through amen::ame(),
  # and the prior network enters as predictors. See selection-waves.R.
  route <- .resolve_selection_waves(spec, schema, waves)

  diagnostics <- .diagnose_selection_design(spec, data, route$waves_analyzed)
  .assert_selection_design_fittable(diagnostics)

  descriptor <- schema$tie_descriptors[[spec$outcome]]
  if (is.null(descriptor)) {
    stop(sprintf("'%s' is not a tie variable in this data.", spec$outcome), call. = FALSE)
  }
  .assert_binary_outcome_rule(descriptor, spec$outcome, binarize, family)

  groups <- all_groups

  # THE HIERARCHICAL ROUTE. `pooling` selects one model over all groups,
  # estimated in one chain, with each coefficient shared, group-specific or
  # random (see ?hierarchical_selection). It is a different model from
  # `pool_beta`, not a different implementation of it, so the two are never
  # combined silently.
  if (!is.null(pooling)) {
    return(.fit_selection_joint(
      spec = spec, call = call, data = data, schema = schema, groups = groups,
      route = route, family = family, binarize = binarize, R = R,
      pooling = pooling, symmetric = symmetric,
      diagnostics = diagnostics, dots = list(...)
    ))
  }

  .assert_selection_controls(list(...), c("nscan", "burn", "odens", "seed", "gof"))

  # ONE SEED PER GROUP, derived from the fit seed, so groups do not share a
  # stream. The first group uses the fit seed itself, so a single-group fit
  # reproduces amen::ame() called with the same seed.
  dots <- list(...)
  group_seeds <- .selection_group_seeds(dots$seed, groups)
  per_group <- lapply(groups, function(g) {
    group_dots <- dots
    group_dots$seed <- group_seeds[[g]]
    do.call(.fit_selection_one_group,
            c(list(spec, data, schema, g, route, family, binarize, R,
                   symmetric = symmetric), group_dots))
  })
  names(per_group) <- groups

  # POOLED BY DEFAULT, and the default is stated wherever it is in effect
  # (see .format_beta_pooling()). `pool_beta = FALSE` returns the per-group
  # fits unpooled. With one group the pooling is the identity, which is the
  # K = 1 reduction its tests pin.
  pooled_beta <- if (isTRUE(pool_beta) && length(groups) > 0L) {
    .pool_beta_precision(per_group)
  } else {
    NULL
  }

  findings <- unlist(lapply(per_group, function(x) x$findings), recursive = FALSE)
  diagnostics <- .combine_diagnostics(c(unclass(diagnostics), findings))

  .assemble_netis_selection(
    spec = spec, call = call, per_group = per_group, groups = groups,
    route = route, waves = route, family = family, binarize = binarize, R = R,
    pooled_beta = pooled_beta, diagnostics = diagnostics,
    estimation_controls = c(
      .selection_estimation_controls(dots, family = family, R = R),
      list(group_seeds = group_seeds)
    ),
    symmetric = symmetric
  )
}

#' Refuse what a symmetric relation cannot express, and rename what it can
#'
#' Three refusals and one rename, all at specification time. receiver() is
#' refused because a symmetric relation has no sender/receiver distinction --
#' amen itself would silently replace the column design with the row design,
#' which is a dropped term wearing a completed fit. prior_reciprocity() is
#' refused because the reverse tie IS the tie: its transpose-read would
#' duplicate prior_tie() exactly. Directed-declared data is refused because
#' fitting it symmetrically discards the asymmetry the user declared;
#' symmetrize deliberately, or declare the relation undirected.
#'
#' The rename: sender() terms keep their role in the grammar but their
#' assembled columns are the NODAL covariates, so default `snd_` names become
#' `nod_` names (a user-supplied name = is respected untouched), and
#' netis_estimates() prints the role as `node`.
#'
#' @noRd
.assert_symmetric_selection_spec <- function(spec, schema) {
  if (length(spec$receivers %||% list()) > 0L) {
    stop(
      "receiver() terms have no meaning on a symmetric relation -- there is no sender/receiver distinction to attach the covariate to. Declare each nodal covariate once with sender(x); on a symmetric fit it is the actor's (nodal) effect, reported as nod_<var> with role 'node'.",
      call. = FALSE
    )
  }
  if (!is.null(spec$prior_reciprocity)) {
    stop(
      "prior_reciprocity() has no meaning on a symmetric relation: the reverse tie is the same tie, so the term would duplicate prior_tie() exactly. Remove it.",
      call. = FALSE
    )
  }
  if (isTRUE(schema$directed)) {
    stop(
      "symmetric = TRUE, but this data declares its ties directed. Fitting a directed relation symmetrically would silently discard the asymmetry you declared. Either declare the relation undirected in netis_data(directed = FALSE), or build a symmetrized tie variable deliberately (symmetrize()) and model that.",
      call. = FALSE
    )
  }
  # No pooling boundary here. Two-stage pooling is a weighted mean taken over
  # already-completed per-group fits, and a weighted mean does not care
  # whether the effect structure was symmetric. (The hierarchical multi-group
  # sampler refuses symmetric relations separately.)
  for (i in seq_along(spec$senders %||% list())) {
    spec$senders[[i]]$name <- sub("^snd_", "nod_", spec$senders[[i]]$name)
  }
  spec$symmetric <- TRUE
  spec
}

#' Assemble one group's design, without fitting it
#'
#' Kept separate from fitting so that any route that needs a group's design
#' without fitting it -- a check across groups, or a fit that estimates several
#' groups at once -- uses exactly the same design construction rather than
#' growing a second, divergent one.
#'
#' @noRd
.assemble_selection_group <- function(spec, data, schema, group, route,
                                      binarize) {
  ids <- .selection_group_roster(netis_nodes(data), schema, group, route$waves_analyzed)
  waves <- route$outcome_waves
  if (length(waves) == 0L) {
    waves <- list(NULL)   # wave-free data: one cross-section
  }

  designs <- lapply(waves, function(w) {
    .assemble_selection_design(spec, data, group, w, ids, binarize = binarize,
                               lag_sequence = route$lag_sequence)
  })
  findings <- unlist(lapply(designs, function(d) d$exclusion_findings), recursive = FALSE)

  d <- designs[[1]]
  stacked <- list(Y = d$Y, Xrow = d$Xrow, Xcol = d$Xcol, Xdyad = d$Xdyad)
  if (!is.null(binarize)) {
    stacked$Y <- .apply_outcome_binarize(stacked$Y, binarize, spec$outcome)
  }

  list(
    stacked = stacked, findings = findings, ids = ids,
    n_actors = length(ids),
    n_excluded = sum(vapply(designs, function(d) d$n_excluded, numeric(1)))
  )
}

#' Fit one group, over however many outcome waves the route resolved
#'
#' @noRd
.fit_selection_one_group <- function(spec, data, schema, group, route, family,
                                     binarize, R, symmetric = FALSE,
                                     ...) {
  asm <- .assemble_selection_group(spec, data, schema, group, route, binarize)
  ids <- asm$ids
  stacked <- asm$stacked
  # The flag travels on the stored design because every design consumer -- the
  # effect design matrix above all -- must know that a nodal covariate enters
  # BOTH margins of a symmetric dyad (x_i + x_j), not the row margin alone.
  stacked$symmetric <- symmetric
  findings <- asm$findings

  .assert_amen_group_fittable(stacked, family, group)
  backend <- .new_backend("amen")
  # An error from inside the sampler is reported with the group it came from,
  # so a multi-group fit says which group failed.
  backend_fit_obj <- tryCatch(
    backend_fit(
      backend,
      c(stacked, list(family = family, R = R, symmetric = symmetric)),
      data = NULL, ...
    ),
    error = function(e) {
      stop(.selection_backend_failure(group, length(ids), conditionMessage(e)), call. = FALSE)
    }
  )
  extracted <- backend_extract(backend, backend_fit_obj)

  list(
    extracted = extracted,
    findings = findings,
    n_actors = length(ids),
    ids = ids,
    # Retained because a probability contrast has to re-evaluate the linear
    # predictor with one covariate moved, which needs the design that was
    # actually fitted rather than a reconstruction. This is the largest thing
    # in the fitted object -- n x n x p x T doubles -- but at typical network
    # sizes whole fits stay small, so it remains the right trade for making
    # effect sizes computable without the original data.
    design = stacked,
    n_excluded = asm$n_excluded,
    backend_fit = backend_fit_obj
  )
}


#' The actors a fit covers in one group
#'
#' Those with a node row at a wave the fit reads. An actor seen only at a wave
#' the fit does not read is not part of the fitted network; static node data has
#' no wave to filter on, so it names everyone.
#'
#' @param nodes `netis_nodes()` of the data.
#' @param schema the data's schema.
#' @param group the group ID.
#' @param waves the waves the fit reads (`NULL` or empty for wave-free data).
#' @return sorted actor IDs.
#' @noRd
.selection_group_roster <- function(nodes, schema, group, waves = NULL) {
  in_group <- as.character(nodes[[schema$group_col]]) == as.character(group)
  wave_col <- schema$wave_col
  if (length(waves) > 0L && !is.null(wave_col) && wave_col %in% names(nodes)) {
    in_group <- in_group & as.character(nodes[[wave_col]]) %in% as.character(waves)
  }
  sort(unique(as.character(nodes[[schema$node_col]][in_group])))
}

#' Apply a recorded binarization rule to a valued outcome
#'
#' Only rules whose cutpoint is fixed by the rule itself are applied here.
#' Data-derived rules (quantile, above_mean, above_median) carry a scope --
#' which pool of values the threshold is computed over -- and applying one to
#' an outcome without resolving that scope would silently pick a cut from
#' whatever happened to be in the matrix. Refused rather than guessed.
#'
#' `NA` cells stay `NA`: a masked dyad is not binarized into an observation.
#'
#' @noRd
.apply_outcome_binarize <- function(Y, rule, variable) {
  pred <- switch(
    rule$type,
    nonzero = function(v) v != 0,
    positive = function(v) v > 0,
    threshold = function(v) v > rule$c,
    abs_threshold = function(v) abs(v) > rule$c,
    custom = rule$fn,
    stop(
      sprintf(
        "binarize = %s() computes its cutpoint from the data, which needs a declared scope (which values the threshold is taken over). That is not resolved for a Selection outcome in this version. Use a rule with a fixed cutpoint -- nonzero(), positive(), threshold(c), abs_threshold(c) -- or derive the binary relation as a tie variable yourself and model it directly.",
        rule$type
      ),
      call. = FALSE
    )
  )
  out <- Y
  observed <- !is.na(Y)
  out[observed] <- as.numeric(pred(Y[observed]))
  out
}

#' Build the netis_selection object
#'
#' Single-group and multi-group fits return the same class. The difference is
#' visible in `pooled_beta` and in whether `coefficients` is a vector or a
#' matrix of per-group rows. `vcov` is `NULL` for every multi-group fit,
#' pooled or not: unpooled, there is no joint model to have a covariance;
#' pooled, the pooling is coefficient-wise and discards covariance. Reporting
#' one in either case would imply a joint model that was not fitted.
#'
#' @noRd
.assemble_netis_selection <- function(spec, call, per_group, groups, route,
                                      family, binarize, R, diagnostics,
                                      waves = NULL, pooled_beta = NULL,
                                      estimation_controls = NULL,
                                      symmetric = FALSE) {
  multi <- length(groups) > 1L
  extracts <- lapply(per_group, function(x) x$extracted)

  # With beta pooled there is ONE coefficient vector, not one per group. The
  # per-group rows would all be identical copies of it, and a matrix of
  # identical rows reads as per-group estimates that happen to agree, which is
  # the opposite of what was fitted. So a pooled fit reports coefficients the
  # same way a single-group fit does, and the pooling policy is what says they
  # are shared.
  beta_pooled <- !is.null(pooled_beta)
  per_group_coefs <- multi && !beta_pooled
  # A pooled MULTI-group fit reports the pooled vector. `extracts[[1]]` is
  # group 1 and nothing more -- the groups were fitted independently and
  # combined afterwards, so reading group 1's estimates as the fit's estimates
  # would report one group's answer under the pooled label.
  pooled_multi <- multi && beta_pooled

  coefs <- if (per_group_coefs) {
    do.call(rbind, lapply(extracts, function(e) e$coefficients))
  } else if (pooled_multi) {
    pooled_beta$mean
  } else {
    # Single group. Pooling over one group is the identity, so the group's own
    # extracted coefficients are the pooled ones; using them keeps a
    # single-group fit bit-identical whether or not pooling was requested.
    extracts[[1]]$coefficients
  }
  if (per_group_coefs) rownames(coefs) <- groups

  draws <- if (per_group_coefs) {
    NULL
  } else if (pooled_multi) {
    # NO DRAWS EXIST for a pooled multi-group fit. Precision pooling combines
    # per-group posterior MEANS and VARIANCES; it never produces a pooled draw
    # set, and the per-group draw sets are not exchangeable with one another.
    # Group 1's draws are the tempting thing to put here and are exactly wrong
    # -- they would be one group's posterior wearing the pooled label, which is
    # the same error the coefficient vector could make, one level down. The
    # per-group draws remain available in the per-group slot.
    NULL
  } else {
    # The single-group path: amen::ame() returns additive actor effects as
    # posterior means only, so an interval over them is not fully propagated
    # and the object says so rather than leaving it to be inferred.
    .new_netis_draws(
      parameters = extracts[[1]]$draws,
      summarized = list(
        sender_effects = extracts[[1]]$additive_effects$sender,
        receiver_effects = extracts[[1]]$additive_effects$receiver,
        multiplicative = extracts[[1]]$multiplicative
      ),
      n_chains = 1L,
      note = extracts[[1]]$convergence$note
    )
  }

  .new_netis_fit(
    "netis_selection",
    fields = list(
      spec = spec,
      call = call,
      netis_version = as.character(utils::packageVersion("rnetis")),
      backend = "amen",
      backend_version = backend_info(.new_backend("amen"))$version,
      estimation_controls = estimation_controls,
      coefficients = coefs,
      # NULL for any multi-group fit. Unpooled: no joint model to have a
      # covariance. Pooled: the pooling runs coefficient by coefficient, so no
      # between-coefficient covariance survives it -- the marginal variances
      # in `pooled_beta$var` are all that exist, and a matrix built from them
      # would assert a zero covariance that was never estimated.
      vcov = if (multi) NULL else extracts[[1]]$vcov,
      convergence = lapply(extracts, function(e) e$convergence),
      loglik = NA,
      diagnostics = diagnostics
    ),
    extra = list(
      pooled_beta = pooled_beta,
      # The resolved wave selection, kept on the object because `summary()`
      # must be able to say which waves were read AND how many were not.
      waves = waves,
      route = route,
      family = family,
      binarize = binarize,
      symmetric = symmetric,
      R = R,
      groups = groups,
      n_groups = length(groups),
      draws = draws,
      per_group = extracts,
      designs = lapply(per_group, function(x) x$design),
      actor_ids = lapply(per_group, function(x) x$ids),
      n_excluded = vapply(per_group, function(x) x$n_excluded, numeric(1)),
      n_actors = vapply(per_group, function(x) x$n_actors, numeric(1))
    )
  )
}

#' The resolved estimation regime a fit ran under
#'
#' Stored on the fit as `estimation_controls`. Internal refits --
#' `pseudo_r2()`'s null model, for instance -- inherit these, so that an
#' estimand built by *comparing* fits compares quantities carrying the same
#' Monte Carlo error rather than whatever the backend's defaults happen to be.
#'
#' **Resolved and normalized, deliberately, not the raw `...`.** Defaults are
#' filled in rather than left absent: a fit that named no controls still ran
#' under a definite regime, and "the user did not say" is not something a
#' months-old saved fit can act on. The defaults here mirror `amen::ame()`'s
#' own signature, which is what runs when rnetis passes nothing; if a later
#' amen release changes them, this table is the one place to update, and the
#' tests will not catch the drift, so it is called out here.
#'
#' The seed is recorded but is **not** what refits reuse -- see
#' `.derive_refit_seed()`.
#'
#' @param dots the caller's `...` from [fit_selection()].
#' @param family,R the resolved model settings.
#' @return a named list of the settings that affected estimation.
#' @noRd
.selection_estimation_controls <- function(dots, family, R) {
  amen_default <- function(name, value) {
    if (!is.null(dots[[name]])) dots[[name]] else value
  }
  list(
    nscan = amen_default("nscan", 10000),
    burn = amen_default("burn", 500),
    odens = amen_default("odens", 25),
    seed = amen_default("seed", 1),
    gof = amen_default("gof", TRUE),
    family = family,
    R = R
  )
}

#' The arguments a refit should pass to the backend
#'
#' Everything in the recorded regime that the backend accepts, with a derived
#' seed substituted for the parent's. `family` and `R` are supplied
#' by the caller's design list rather than here, because a refit may
#' deliberately change the design (the null model drops every predictor)
#' while keeping the estimation regime.
#'
#' @noRd
.refit_backend_args <- function(controls, operation) {
  if (is.null(controls)) {
    return(list())
  }
  list(nscan = controls$nscan, burn = controls$burn, odens = controls$odens,
       gof = controls$gof, seed = .derive_refit_seed(controls, operation))
}

#' A deterministic, operation-specific seed for an internal refit
#'
#' Inheriting computational effort is required; inheriting the *seed* is not,
#' and is sometimes wrong. A null model reusing the parent's seed replays the
#' parent's stream inside the very difference that defines the null
#' comparison, and several refits sharing one seed would restart the same
#' chain stream each time. So each refit gets its own stream, derived rather than random
#' so that a reported number stays reproducible.
#'
#' Derivation is a cheap string hash of the operation name mixed with the
#' parent seed. It needs to be deterministic, operation-distinct, and not
#' equal to the parent's -- not cryptographic.
#'
#' @param controls a fit's `estimation_controls`.
#' @param operation a short string naming the refit, e.g. `"null_model"` or
#'   `"refit_2"`.
#' @return a single numeric seed.
#' @noRd
.derive_refit_seed <- function(controls, operation) {
  parent <- if (is.null(controls$seed)) 1 else controls$seed
  chars <- utf8ToInt(operation)
  # Positional weighting so "fold_12" and "fold_21" do not collide.
  offset <- sum(chars * seq_along(chars))
  # The +1 keeps the result away from the parent seed even in the
  # (impossible for real operation names) case of a zero offset.
  parent + offset + 1
}

#' Distinct per-group seeds derived from one fit seed
#'
#' Group `k` (in the fit's sorted group order) uses `seed + 7919 * (k - 1)`:
#' deterministic, distinct for every group, and equal to the fit seed for the
#' first group.
#'
#' @param seed the fit seed (`NULL` means amen's default, 1).
#' @param groups the group IDs, in fit order.
#' @return a numeric vector named by group.
#' @noRd
.selection_group_seeds <- function(seed, groups) {
  if (is.null(seed)) seed <- 1
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("seed must be a single finite number.", call. = FALSE)
  }
  stats::setNames(seed + 7919 * (seq_along(groups) - 1L), groups)
}

#' Refuse sampler controls the route does not take, or cannot run with
#'
#' Only the listed controls reach a sampler. amen::ame() has others (`rvar`,
#' `cvar`, `dcor`, `nvar`, `intercept`, `prior`, ...) that change the model, and
#' a fit whose reporting assumes the model it documents would then describe a
#' different one, so they are refused rather than passed through.
#'
#' @param dots the caller's `...`.
#' @param allowed the control names this route takes.
#' @return invisibly `TRUE`, or an error naming the problem.
#' @noRd
.assert_selection_controls <- function(dots, allowed) {
  unknown <- setdiff(names(dots) %||% rep("", length(dots)), allowed)
  if (length(unknown)) {
    stop(sprintf(
      "fit_selection() takes the sampler controls %s here; it was given %s. Other amen::ame() arguments change the model that is fitted, which the package's reporting would then misdescribe.",
      paste(allowed, collapse = ", "),
      paste(ifelse(nzchar(unknown), unknown, "an unnamed argument"), collapse = ", ")
    ), call. = FALSE)
  }
  whole <- function(nm, minimum) {
    v <- dots[[nm]]
    if (is.null(v)) return(invisible(NULL))
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v != round(v) || v < minimum) {
      stop(sprintf("%s must be a whole number of at least %d.", nm, minimum), call. = FALSE)
    }
  }
  whole("nscan", 1L)
  whole("burn", 0L)
  whole("odens", 1L)
  whole("chains", 1L)
  nscan <- dots$nscan %||% 10000
  burn <- dots$burn %||% 500
  odens <- dots$odens %||% 25
  # A draw is kept at each scan past burn-in divisible by odens.
  kept <- (burn + nscan) %/% odens - burn %/% odens
  if (kept < 2L) {
    stop(sprintf(
      "nscan = %s, burn = %s and odens = %s keep %d posterior draw(s); a fit needs at least 2 to report any uncertainty. Increase nscan or reduce odens.",
      format(nscan), format(burn), format(odens), kept
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.assert_selection_R <- function(R) {
  if (length(R) != 1L || is.na(R) || !is.numeric(R) || R != as.integer(R) || R < 0) {
    stop(
      sprintf("R must be a single non-negative whole number; got %s.",
              paste(deparse(R), collapse = "")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
