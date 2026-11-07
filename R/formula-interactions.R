# Two-way interaction resolution for fit_influence().
#
# INFLUENCE ONLY. Selection refuses interactions unconditionally, so nothing here
# is shared with formula-selection-parse.R.
#
# STRUCTURAL REFERENCES, NEVER DEPARSED TEXT. Every composing variable is identified by its INDEX into the
# de-listed `variables` vector, read from attr(tt, "factors") -- never by
# comparing deparse() output, which can produce a false-positive refusal (a
# long exposure() call's wrapped deparse does not equal its own label). A term label's composing variables are the
# nonzero rows of its factors column; that is true regardless of whether the
# entry is 1 or 2 (the contrast-coding flag model.matrix() uses internally),
# so composition is read as "row != 0", not "row == 1".
#
# ORDER 2 ONLY, HIERARCHICAL ONLY. An order-2 term is resolved
# only when both its composing variables also have their own order-1 term.
# `stats::terms()` guarantees an order-2 column has exactly two nonzero
# rows -- order IS the composing-variable count -- so there is no
# not-exactly-two-composing-variables case to guard against.
#
# THREE-WAY-AND-UP IS NOT REFUSED HERE. An order >= 3 label is simply never
# added to `resolved`, so it stays in the unconsumed set
# .assert_terms_all_consumed() already reports on, with the message that
# already names it. Duplicating that refusal here would be two error paths
# for one defect.
#
# THE MODERAND, NOT THE MODERATOR, CARRIES THE INTERESTING TYPE. Exactly one
# composing variable must be an ordinary covariate (the moderator); the
# other -- the moderand -- must be a covariate, an exposure() term, or an
# ar() term. `lagged()`, `latent()`, `centrality()` and
# `network_stat()` are real, understood term kinds that are not expressible as
# a moderand, and a pairing involving one of them is refused
# by name here rather than falling through to the generic completeness
# message, which would not say why.

#' Classify one interaction's two composing variables into a moderand pairing
#'
#' @param kind_a,kind_b the family each composing variable belongs to
#'   (`"covariate"`, `"exposure"`, `"ar"`, `"lagged"`, `"latent"`,
#'   `"centrality"`, `"network_stat"`).
#' @return `NULL` if the pairing is not one of the three supported moderands;
#'   otherwise a list with `moderand_kind` and `moderand_is_a` (whether the
#'   first composing variable is the moderand rather than the moderator).
#' @noRd
.classify_moderand_pairing <- function(kind_a, kind_b) {
  supported_moderands <- c("covariate", "exposure", "ar")
  if (identical(kind_a, "covariate") && kind_b %in% supported_moderands) {
    return(list(moderand_kind = kind_b, moderand_is_a = FALSE))
  }
  if (identical(kind_b, "covariate") && kind_a %in% supported_moderands) {
    return(list(moderand_kind = kind_a, moderand_is_a = TRUE))
  }
  NULL
}

#' Which family a variable index belongs to
#' @noRd
.interaction_var_kind <- function(idx, families) {
  for (nm in names(families)) {
    if (idx %in% families[[nm]]) {
      return(nm)
    }
  }
  # Unreachable given how `families` is constructed in
  # .parse_influence_formula(): covariate_idx is defined as everything left
  # over after every special family is subtracted out, so every non-response
  # variable belongs to exactly one family. Not guarded further (trust internal
  # invariants rather than re-checking them).
  NA_character_
}

#' Resolve every order-2 interaction term into a moderation spec, or refuse it by name
#'
#' Two refusals happen here, immediately, because a generic "unsupported
#' construction" message would not say why: a missing main effect (hierarchy)
#' and an unsupported moderand pairing. Everything else --
#' order >= 3 chief among it -- is left unconsumed for
#' `.assert_terms_all_consumed()` to report with the message it already has.
#'
#' @param tt a `terms` object.
#' @param var_list the de-listed `variables` vector (`as.list(attr(tt,
#'   "variables"))[-1]`), for display-only deparsing in messages -- never for
#'   matching.
#' @param families named list of variable-index vectors, one per family
#'   (`"ar"`, `"lagged"`, `"exposure"`, `"latent"`, `"centrality"`,
#'   `"network_stat"`, `"covariate"`); together they must partition every
#'   non-response variable index exactly once.
#' @return a list with `resolved` (one entry per supported interaction:
#'   `label`, `moderand_kind`, `moderand_idx`, `moderator_idx`) and
#'   `consumed_labels` (their term labels, to add to the completeness check's
#'   accounted-for set).
#' @noRd
.resolve_influence_interactions <- function(tt, var_list, families) {
  factors <- attr(tt, "factors")
  if (length(factors) == 0L) {
    return(list(resolved = list(), consumed_labels = character(0)))
  }
  order <- attr(tt, "order")
  labels <- colnames(factors)

  # An order-1 term's sole composing variable, so hierarchy can ask "does
  # variable v have a main-effect term of its own" without a text lookup.
  # unname()'d throughout: `factors`' rows are named by variable text, so
  # which() on it returns indices carrying that text as a name, and every
  # downstream comparison (moderand_idx < moderator_idx for
  # .interaction_product_colname()'s ordering) would otherwise inherit a
  # stray name that carries no information and only risks confusing an
  # identical()-based comparison later.
  order1_cols <- which(order == 1L)
  main_effect_var <- vapply(order1_cols, function(j) unname(which(factors[, j] != 0L)[1]), integer(1))

  resolved <- list()
  consumed <- character(0)

  for (j in which(order == 2L)) {
    composing <- unname(which(factors[, j] != 0L))
    a <- composing[1]
    b <- composing[2]

    missing_main <- setdiff(c(a, b), main_effect_var)
    if (length(missing_main) > 0L) {
      missing_text <- vapply(missing_main, function(i) deparse(var_list[[i]]), character(1))
      full_text <- vapply(c(a, b), function(i) deparse(var_list[[i]]), character(1))
      stop(
        sprintf(
          paste(
            "The interaction '%s' is missing its main effect%s %s. rnetis requires",
            "hierarchical interactions: write '%s' rather than the bare colon form,",
            "which adds both main effects together with the product."
          ),
          labels[j],
          if (length(missing_text) > 1L) "s" else "",
          paste(sprintf("'%s'", missing_text), collapse = " and "),
          paste(full_text, collapse = " * ")
        ),
        call. = FALSE
      )
    }

    kind_a <- .interaction_var_kind(a, families)
    kind_b <- .interaction_var_kind(b, families)
    pairing <- .classify_moderand_pairing(kind_a, kind_b)
    if (is.null(pairing)) {
      stop(
        sprintf(
          paste(
            "'%s' moderates a %s term with a %s term, which is not one of the moderands",
            "rnetis supports: a plain covariate times a moderator, an exposure() term",
            "times a moderator, or ar() times a moderator -- in every case the second factor",
            "must be a plain covariate. lagged(), latent(), centrality() and network_stat()",
            "terms cannot be moderated."
          ),
          labels[j], kind_a, kind_b
        ),
        call. = FALSE
      )
    }

    resolved[[length(resolved) + 1L]] <- list(
      label = labels[j],
      moderand_kind = pairing$moderand_kind,
      moderand_idx = if (pairing$moderand_is_a) a else b,
      moderator_idx = if (pairing$moderand_is_a) b else a
    )
    consumed <- c(consumed, labels[j])
  }

  .assert_no_double_moderated_effect_sized_term(resolved, var_list)

  list(resolved = resolved, consumed_labels = consumed)
}

#' Refuse the same `exposure()`/`ar()` moderand moderated by more than one moderator
#'
#' This is easy to reach: a raw variable and its `add_centering()`/
#' `add_decomposition()` siblings are separate, independently referenceable
#' columns, so `ar() * z_within + ar() * z_between` -- using both
#' components -- is something a user can naturally
#' write. The FIT itself has no problem with it: both product columns are
#' ordinary assembled predictors and both coefficients are estimated and
#' reported normally (the fitting machinery does not care how many
#' interactions there are).
#'
#' What does NOT generalize is the conditional-effect layer this package
#' reports through `summary()`/`plot_data()`:
#' `.conditional_effect()` and `.johnson_neyman_boundaries()`
#' (`R/influence-conditional-effects.R`) are built for ONE moderator's
#' `beta_term + z * beta_inter`, a line in z, and their callers
#' (`.compute_influence_effects()`, `.compute_ar_effects()`) find "the"
#' interaction for a moderand by filtering `interactions` and taking the
#' first match -- silently dropping a second simultaneous moderator's
#' contribution rather than erroring. A second moderator turns the estimand
#' into a plane (or a Johnson-Neyman boundary CURVE, not up to two points),
#' which this package does not compute. Refused HERE, structurally and
#' by name, rather than letting the effects layer silently report only the
#' first-matched interaction's conditional table as though it were complete
#' -- a wrong number is worse than no number.
#'
#' Scoped to `"ar"`/`"exposure"` moderands only (never `"covariate"`): a
#' plain-covariate interaction is never effect-sized, so the problem cannot occur
#' for it, and a covariate moderated by two different covariates is an ordinary
#' specification.
#'
#' @param resolved the in-progress `resolved` list built by
#'   `.resolve_influence_interactions()`.
#' @param var_list the de-listed `variables` vector, for display-only
#'   deparsing.
#' @return invisibly `TRUE`, or an error.
#' @noRd
.assert_no_double_moderated_effect_sized_term <- function(resolved, var_list) {
  effect_sized <- Filter(function(r) r$moderand_kind %in% c("ar", "exposure"), resolved)
  if (length(effect_sized) < 2L) {
    return(invisible(TRUE))
  }
  moderand_idxs <- vapply(effect_sized, function(r) r$moderand_idx, integer(1))
  dup_idx <- unique(moderand_idxs[duplicated(moderand_idxs)])
  if (length(dup_idx) == 0L) {
    return(invisible(TRUE))
  }
  offending <- Filter(function(r) r$moderand_idx == dup_idx[1], effect_sized)
  moderand_text <- deparse(var_list[[dup_idx[1]]])
  moderator_texts <- vapply(offending, function(r) deparse(var_list[[r$moderator_idx]]), character(1))
  stop(
    sprintf(
      paste(
        "'%s' is moderated by more than one moderator at once (%s). rnetis's conditional-effect",
        "and Johnson-Neyman reporting is built for a single moderator per moderated",
        "term -- simultaneous moderation by, for instance, both a within/centered and a",
        "mean/between representation of the same source variable -- is not supported",
        "there. Fit each moderator as a separate specification instead."
      ),
      moderand_text, paste(sprintf("'%s'", moderator_texts), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' Attach resolved term objects to each interaction, once the term families exist
#'
#' Phase two of interaction resolution, run after `ar_terms`/`exposure_terms`
#' etc. are built and named. Kept separate from
#' `.resolve_influence_interactions()` because the completeness check (which
#' must run early -- everything downstream
#' reads the partition as complete) cannot wait for exposure()/ar() terms to be
#' evaluated and disambiguated first.
#'
#' The moderator is always a plain covariate name (a string). rnetis ships no
#' centering API of its own -- `add_centering()`/
#' `add_decomposition()` is the data layer that produces one, and that layer
#' never enters a predictor into a model. The consequence
#' for this file: a "prepared representation" is never a distinct grammar
#' construct here, just an ordinary column a user names in the formula like any
#' other covariate -- `ar() * z_within` selects `add_decomposition()`'s within
#' component exactly because `z_within` is the string this code resolves, the
#' same as any other moderator. `.moderation_provenance_notes()` below is the
#' other half: once a representation is picked by reference,
#' the fit reports which one, by looking the referenced column up in the
#' schema's derived-variable registry rather than requiring a second, separate
#' declaration. The moderand is the same string for a covariate pairing, or the
#' fully resolved, named term object for an exposure()/ar() moderand --
#' `match()` against the family's own index vector finds which position in
#' `ar_terms`/`exposure_terms` the moderand's variable index resolved to,
#' rather than re-deriving it.
#'
#' @param resolved output of `.resolve_influence_interactions()$resolved`.
#' @param var_list the de-listed `variables` vector.
#' @param ar_idx,exposure_idx the specials' variable-index vectors.
#' @param ar_terms,exposure_terms the resolved, named term lists in the same
#'   order as `ar_idx`/`exposure_idx`.
#' @return a list of interaction specs, each with `name` (the term label,
#'   already unique -- `stats::terms()` guarantees no two term labels
#'   collide), `moderand_kind`, `moderand`, and `moderator`.
#' @noRd
.attach_interaction_terms <- function(resolved, var_list, ar_idx, ar_terms,
                                      exposure_idx, exposure_terms) {
  lapply(resolved, function(r) {
    moderator <- deparse(var_list[[r$moderator_idx]])
    moderand <- switch(
      r$moderand_kind,
      covariate = deparse(var_list[[r$moderand_idx]]),
      ar = {
        term <- ar_terms[[match(r$moderand_idx, ar_idx)]]
        # A multi-lag ar() moderated by z is genuinely ambiguous -- the
        # single conditional stability effect gamma + theta*z presumes one
        # gamma. Which lag's coefficient theta moderates is undecided design
        # space, not a plumbing gap, so this refuses by name rather than
        # guessing. Checked here, at parse time, because
        # ar_term$lag is already fully known once ar() has been evaluated --
        # nothing downstream adds information this check needs.
        if (length(term$lag) > 1L) {
          stop(
            sprintf(
              paste(
                "'%s' moderates an ar() term with multiple lags (ar(lag = %s)), which rnetis",
                "does not support: it is undecided which lag's stability coefficient the",
                "moderator applies to. Fit a single-lag ar() term (e.g. ar(1) * %s), or fit",
                "each lag's moderation as a separate specification."
              ),
              r$label, paste(term$lag, collapse = ", "), moderator
            ),
            call. = FALSE
          )
        }
        term
      },
      exposure = exposure_terms[[match(r$moderand_idx, exposure_idx)]]
    )
    list(
      name = r$label,
      moderand_kind = r$moderand_kind,
      moderand = moderand,
      moderator = moderator,
      # Whether the MODERAND was the first-appearing (lower-index) composing
      # variable, preserved so the assembled product column can be named in
      # the same order `stats::terms()` itself would use -- see
      # .interaction_product_colname()'s header for why this matters. `a` is
      # always the smaller index (composing <- which(...) is ascending).
      moderand_first = r$moderand_idx < r$moderator_idx
    )
  })
}

#' The assembled table's column name for a resolved interaction's moderand
#'
#' Mirrors exactly how `.assemble_influence_table()` names each term kind's own
#' column, because the interaction's product is built FROM those columns after
#' they are merged -- a name that drifted from assembly's own convention would
#' look up a column that does not exist.
#'
#' @param term one entry of `parsed$interactions`.
#' @return a single string.
#' @noRd
.interaction_moderand_colname <- function(term) {
  switch(
    term$moderand_kind,
    covariate = term$moderand,
    exposure = term$moderand$name,
    ar = paste0(term$moderand$variable, "_lag", term$moderand$lag)
  )
}

#' The assembled table's column name for the interaction's product itself
#'
#' NOT simply "moderand:moderator" -- for two ordinary covariates
#' (`x * z`), `.classify_moderand_pairing()` assigns "moderand" and
#' "moderator" arbitrarily (the pairing is symmetric), so that construction would
#' give 'z:x' for a user-written `x * z`, and would not collide with an existing
#' column literally named "x:z". Preserving `moderand_first` keeps
#' `stats::terms()`'s own convention: the product is always named
#' `<first-appearing colname>:<second-appearing colname>`, matching what a
#' user who wrote `x * z` sees from `lm()` itself.
#'
#' @param term one entry of `parsed$interactions`.
#' @return a single string.
#' @noRd
.interaction_product_colname <- function(term) {
  moderand_col <- .interaction_moderand_colname(term)
  if (isTRUE(term$moderand_first)) {
    paste0(moderand_col, ":", term$moderator)
  } else {
    paste0(term$moderator, ":", moderand_col)
  }
}

#' Provenance notes for every derived variable referenced by a moderated term
#'
#' The other half of "the fit reports which representation was used"
#' (see `.attach_interaction_terms()`'s header): since a representation is
#' selected purely by which column a moderated term references, this is
#' where that reference is looked up against the schema's derived-variable
#' registry and turned into prose, once, for `fit_influence()` to store and
#' `summary()` to print -- rather than requiring a reader to already know
#' that `z_within` means "the actor-level within component of z" from the
#' name alone.
#'
#' Checks the moderator always (`ar()`/`exposure()`/covariate moderands all
#' have one, and it is always a plain column reference). Checks the
#' moderand too, but ONLY for a `"covariate"` pairing: an `exposure()`/`ar()`
#' moderand is a computed term (an assembled exposure column, an outcome
#' lag), never itself a row in the derived-variable registry, so looking it
#' up would only ever find nothing -- skipped rather than doing a lookup
#' that can never succeed.
#'
#' @param interactions `parsed$interactions` (`.attach_interaction_terms()`'s
#'   output); `list()` for a fit with no moderated terms.
#' @param derived a schema's `derived` registry (`netis_schema(data)$derived`,
#'   possibly empty).
#' @return a character vector, one note per unique derived column actually
#'   referenced; empty when no moderated term references one, or there are no
#'   moderated terms at all.
#' @noRd
.moderation_provenance_notes <- function(interactions, derived) {
  if (length(interactions) == 0L || length(derived) == 0L) {
    return(character(0))
  }
  referenced <- unique(unlist(lapply(interactions, function(inter) {
    c(inter$moderator, if (identical(inter$moderand_kind, "covariate")) inter$moderand)
  })))
  notes <- vapply(referenced, function(nm) {
    note <- .derived_provenance_note(derived, nm)
    if (is.null(note)) NA_character_ else note
  }, character(1))
  unname(notes[!is.na(notes)])
}

#' Refuse a non-numeric interaction operand rather than let R silently reinterpret it
#'
#' Moderators are numeric or binary only, and this enforces it in code. Two
#' failure modes are both closed by refusing here, before the multiplication
#' runs:
#'
#'   1. Without the formula-shadowing guard (`.safe_predictor_formula()`), a
#'      factor operand would let `model.matrix()`'s OWN `:` silently take over
#'      -- dummy-coding the factor into per-level contrast columns
#'      (`x:zlo`) computed a different way than this package's controlled
#'      numeric product, with no error and no message.
#'   2. With the guard, a factor operand instead hits `Ops.factor`'s "'*' not
#'      meaningful for factors" warning and produces an all-NA column, which
#'      `lm()` then quietly drops as aliased -- a silent failure of a
#'      different shape: the term the user asked to moderate silently
#'      vanishes from the fitted model instead of erroring.
#'
#' A 2-level factor moderator is refused too rather than dummy-coded here; a
#' user who wants one converts the column to 0/1 themselves before fitting.
#'
#' @param x the column's values.
#' @param colname the column's name, for the message.
#' @param interaction_name the interaction term's own name, for the message.
#' @return invisibly `TRUE`, or an error.
#' @noRd
.assert_numeric_interaction_operand <- function(x, colname, interaction_name) {
  if (is.numeric(x) || is.logical(x)) {
    return(invisible(TRUE))
  }
  stop(
    sprintf(
      paste(
        "The interaction '%s' involves '%s', which is %s, not numeric. rnetis supports",
        "numeric (including binary 0/1) moderators and moderands only -- a factor operand",
        "would either be silently dummy-coded by R's own formula machinery (a different",
        "computation than the controlled product this package builds) or produce an",
        "all-missing column lm() would then silently drop as aliased. Convert '%s' to a",
        "numeric or 0/1 column before fitting if you want it treated as binary."
      ),
      interaction_name, colname, paste(class(x), collapse = "/"), colname
    ),
    call. = FALSE
  )
}
