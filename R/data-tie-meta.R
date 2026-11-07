# Per-tie-variable schema descriptors: weighted or binary, what a zero means, and
# what makes a stored row a tie.
#
# The key distinction this file encodes: row
# presence is NOT universally tie presence. That holds only for a sparse edge
# list. A complete dyadic dataset -- a round-robin design, which this package
# explicitly supports -- carries a row for every dyad including zero-valued
# ones, where a row means the dyad was *observed*, not that a relationship
# exists. presence_mode names which reading applies, per tie variable, and an
# ambiguous case (complete_dyadic with no declared resolution) is undetermined
# -- callers that need presence must refuse rather than default to `!= 0`.

#' @noRd
.tie_presence_modes <- c("row_presence", "complete_dyadic", "indicator", "value_rule")

#' @noRd
.weight_semantics_values <- c("strength", "distance", "signed", "count", "unitless")

#' @noRd
.zero_meaning_values <- c("measured_zero", "structural_absence", "unknown")

# A dataset whose actual tie-row count reaches this fraction of the maximum
# possible dyad count (given the node roster, directedness, and self-tie
# policy) is inferred to be a complete dyadic table rather than a sparse
# edge list, unless no stored row can be a non-tie (.settle_inferred_presence()).
# This is a heuristic default, not a statistical result -- it exists so ordinary
# sparse or ordinary complete data infer correctly without the user declaring
# presence_mode by hand, and it is always overridable per tie variable via
# netis_data()'s tie_meta argument.
#
# THE HEURISTIC MAY SUGGEST METADATA; IT MAY NOT DECIDE TIE STATUS. Both sides
# of the threshold are closed against that:
#
#   above it, `complete_dyadic` presence is UNDETERMINED without an explicit
#     presence_col or presence_rule, and every consumer refuses;
#   below it, an inferred `row_presence` table that nevertheless STORES ZEROS
#     also refuses, via .assert_inferred_row_presence_unambiguous().
#
# The second is what stops a genuinely complete dyadic table falling just below
# the threshold from having its zero-valued rows silently promoted to present
# ties, and presence feeds degree, density, isolate status and exposure. A cutoff
# that cannot be cited is allowed to guess which structural story a table resembles; it is not
# allowed to settle whether a dyad is connected.
#' @noRd
.complete_dyadic_inference_threshold <- 0.9

#' Infer whether a tie variable's values are binary or weighted
#'
#' A tie variable taking only \{0, 1\} (ignoring `NA`) is treated as binary;
#' anything else -- including a constant nonzero value, negative values, or
#' a continuous range -- is weighted. An all-`NA` or empty column has
#' nothing to infer from and defaults to binary (`FALSE` weighted) as the
#' more conservative reading; this is expected to be rare in practice and
#' is always overridable.
#'
#' @noRd
.infer_weighted <- function(values) {
  vals <- unique(values[!is.na(values)])
  if (length(vals) == 0L) {
    return(FALSE)
  }
  !all(vals %in% c(0, 1))
}

#' Infer a dataset's dominant presence_mode from its row completeness
#'
#' Computes, per group, the maximum number of dyads possible given that
#' group's node roster, directedness, and self-tie policy, multiplied by
#' the number of distinct waves tie_data actually has rows for in that
#' group (1 if there is no wave dimension) -- then compares the sum of
#' those maxima against the actual total row count in tie_data. This is a
#' structural property of the *table* (which group/source/target/wave
#' combinations have rows at all), shared across every value column stored
#' in that table, so it is computed once and used as every tie variable's
#' inferred default rather than separately per column.
#'
#' @return a list with `mode` (`"row_presence"` or `"complete_dyadic"`) and
#'   `ratio` (the actual/expected fraction, `NA` if no group had 2+ nodes).
#' @noRd
.infer_presence_mode <- function(tie_data, node_data, group_col, node_col,
                                  wave_col, directed, self_ties) {
  groups <- unique(as.character(tie_data[[group_col]]))
  tie_has_wave <- !is.null(wave_col) && wave_col %in% names(tie_data)

  total_expected <- 0
  for (g in groups) {
    node_ids <- unique(as.character(node_data[[node_col]][as.character(node_data[[group_col]]) == g]))
    n <- length(node_ids)
    if (n < 2L) {
      next
    }
    max_dyads <- if (directed) {
      if (identical(self_ties, "permitted")) n * n else n * (n - 1)
    } else {
      if (identical(self_ties, "permitted")) n * (n + 1) / 2 else n * (n - 1) / 2
    }
    n_waves <- if (tie_has_wave) {
      length(unique(tie_data[[wave_col]][as.character(tie_data[[group_col]]) == g]))
    } else {
      1L
    }
    total_expected <- total_expected + max_dyads * max(n_waves, 1L)
  }

  if (total_expected == 0) {
    return(list(mode = "row_presence", ratio = NA_real_))
  }
  ratio <- nrow(tie_data) / total_expected
  mode <- if (ratio >= .complete_dyadic_inference_threshold) "complete_dyadic" else "row_presence"
  list(mode = mode, ratio = ratio)
}

#' Build the per-tie-variable descriptor list for a netis_data schema
#'
#' One descriptor per tie variable (every tie_data column other than the
#' key columns), combining structural inference (`.infer_weighted()`,
#' `.infer_presence_mode()`) with any user-declared overrides from
#' `netis_data()`'s `tie_meta` argument.
#'
#' `tie_meta` is a named list keyed by tie-variable name; each entry may set
#' any of `weighted`, `weight_semantics`, `direction_semantics`,
#' `zero_meaning`, `presence_mode`, `presence_col`, `presence_rule`. Unset
#' fields fall back to inference (`weighted`, `presence_mode`) or a
#' documented neutral default (`weight_semantics = "unitless"`,
#' `zero_meaning = "unknown"`, `direction_semantics = NA`) -- never a guess
#' at meaning, which is not inferable from values alone.
#'
#' @noRd
.build_tie_descriptors <- function(tie_data, node_data, group_col, node_col,
                                    source_col, target_col, wave_col,
                                    directed, self_ties, tie_meta) {
  key_cols <- c(group_col, source_col, target_col, if (!is.null(wave_col)) wave_col else NULL)
  tie_vars <- setdiff(names(tie_data), key_cols)

  if (!is.null(tie_meta)) {
    unknown <- setdiff(names(tie_meta), tie_vars)
    if (length(unknown) > 0L) {
      stop(
        sprintf(
          "tie_meta names variable(s) not present in tie_data: %s.",
          paste(unknown, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  presence_default <- .infer_presence_mode(
    tie_data, node_data, group_col, node_col, wave_col, directed, self_ties
  )

  descriptors <- list()
  for (v in tie_vars) {
    user <- tie_meta[[v]]
    .assert_tie_meta_entry(v, user)
    inferred <- .settle_inferred_presence(presence_default, tie_data[[v]], user)
    descriptors[[v]] <- .build_one_tie_descriptor(v, tie_data[[v]], tie_data, user, inferred)
  }
  descriptors
}

#' Refuse a `tie_meta` entry that would not mean what it says
#'
#' An unknown field (a misspelt `zero_meanin`) would otherwise leave the field it
#' meant undeclared while the user believes it is declared, and a `weighted` that
#' is not `TRUE`/`FALSE` would be read as `FALSE`.
#'
#' @noRd
.assert_tie_meta_entry <- function(var_name, user) {
  if (is.null(user)) {
    return(invisible(TRUE))
  }
  if (!is.list(user)) {
    stop(sprintf("tie_meta[[\"%s\"]] must be a list of declared fields.", var_name), call. = FALSE)
  }
  fields <- c("weighted", "weight_semantics", "direction_semantics", "zero_meaning",
              "presence_mode", "presence_col", "presence_rule")
  unknown <- setdiff(names(user), fields)
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "tie_meta[[\"%s\"]] has unknown field(s): %s. The fields are: %s.",
        var_name, paste(unknown, collapse = ", "), paste(fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!is.null(user$weighted) &&
      (!is.logical(user$weighted) || length(user$weighted) != 1L || is.na(user$weighted))) {
    stop(sprintf("tie_meta[[\"%s\"]]$weighted must be TRUE or FALSE.", var_name), call. = FALSE)
  }
  if (!is.null(user$direction_semantics) &&
      (!is.character(user$direction_semantics) || length(user$direction_semantics) != 1L)) {
    stop(sprintf("tie_meta[[\"%s\"]]$direction_semantics must be a single string.", var_name), call. = FALSE)
  }
  invisible(TRUE)
}

#' The inferred presence mode of one tie variable
#'
#' A table dense enough to read as `complete_dyadic` is undetermined only because
#' a stored row might not be a tie. Two tables have no such row, however dense:
#' one whose zeros are declared structural absences and which stores none (the
#' adjacency importers drop zero cells, so every stored row is a tie), and one
#' with a row for every possible dyad, each holding `1`, where reading a row as
#' present and reading a nonzero value as present agree on every dyad. A table
#' that stores zeros, misses some dyads, or holds values with no declared meaning
#' stays `complete_dyadic`.
#'
#' @param default the table's result from `.infer_presence_mode()`.
#' @param values the tie variable's values.
#' @param user the variable's `tie_meta` entry.
#' @noRd
.settle_inferred_presence <- function(default, values, user) {
  if (!identical(default$mode, "complete_dyadic")) {
    return(default$mode)
  }
  stores_zero <- any(values == 0, na.rm = TRUE)
  if (identical(user$zero_meaning, "structural_absence") && !stores_zero) {
    return("row_presence")
  }
  if (isTRUE(default$ratio == 1) && !anyNA(values) && length(values) > 0L && all(values == 1)) {
    return("row_presence")
  }
  default$mode
}

#' Refuse a presence declaration part of which would be ignored
#'
#' Each mode reads one thing: `row_presence` the row itself, `indicator` the
#' column, `value_rule` the rule, and `complete_dyadic` whichever one of the two
#' it is given. Anything else declared alongside would be recorded on the object
#' and never consulted.
#'
#' @noRd
.assert_presence_declaration <- function(var_name, user, presence_mode, presence_col, presence_rule) {
  label <- sprintf("tie_meta[[\"%s\"]]", var_name)
  if (is.null(user$presence_mode)) {
    if (!is.null(presence_col) || !is.null(presence_rule)) {
      stop(
        sprintf(
          "%s gives %s without a presence_mode, so nothing says it should be read. Declare presence_mode = \"indicator\" for a column, \"value_rule\" for a rule, or \"complete_dyadic\" for either.",
          label, if (!is.null(presence_col)) "presence_col" else "presence_rule"
        ),
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }
  conflict <- switch(
    presence_mode,
    row_presence = c(if (!is.null(presence_col)) "presence_col",
                     if (!is.null(presence_rule)) "presence_rule"),
    indicator = if (!is.null(presence_rule)) "presence_rule",
    value_rule = if (!is.null(presence_col)) "presence_col",
    NULL
  )
  if (length(conflict) > 0L) {
    stop(
      sprintf(
        "%s declares presence_mode = \"%s\" together with %s, which that mode does not read, so it would be ignored. Remove it, or declare the mode that reads it.",
        label, presence_mode, paste(conflict, collapse = " and ")
      ),
      call. = FALSE
    )
  }
  if (identical(presence_mode, "complete_dyadic") && !is.null(presence_col) && !is.null(presence_rule)) {
    stop(
      sprintf(
        "%s declares presence_mode = \"complete_dyadic\" with both a presence_col and a presence_rule. It reads either one, and the two can disagree. Keep one.",
        label
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @noRd
.build_one_tie_descriptor <- function(var_name, values, tie_data, user, inferred_presence_mode) {
  weighted_inferred <- is.null(user$weighted)
  weighted <- if (weighted_inferred) .infer_weighted(values) else isTRUE(user$weighted)

  presence_mode_inferred <- is.null(user$presence_mode)
  presence_mode <- if (presence_mode_inferred) inferred_presence_mode else user$presence_mode
  .assert_enum(presence_mode, .tie_presence_modes, sprintf("tie_meta[[\"%s\"]]$presence_mode", var_name))

  presence_col <- user$presence_col
  presence_rule <- user$presence_rule
  .assert_presence_declaration(var_name, user, presence_mode, presence_col, presence_rule)

  if (identical(presence_mode, "indicator") && is.null(presence_col)) {
    stop(
      sprintf(
        "tie_meta[[\"%s\"]]$presence_mode = \"indicator\" requires presence_col naming the indicator column.",
        var_name
      ),
      call. = FALSE
    )
  }
  if (identical(presence_mode, "value_rule") && is.null(presence_rule)) {
    stop(
      sprintf(
        "tie_meta[[\"%s\"]]$presence_mode = \"value_rule\" requires presence_rule, a function(value) that returns TRUE/FALSE per row.",
        var_name
      ),
      call. = FALSE
    )
  }
  if (!is.null(presence_col)) {
    .assert_columns_present(tie_data, presence_col, "tie_data")
  }
  if (!is.null(presence_rule) && !is.function(presence_rule)) {
    stop(sprintf("tie_meta[[\"%s\"]]$presence_rule must be a function.", var_name), call. = FALSE)
  }

  weight_semantics <- if (!is.null(user$weight_semantics)) {
    .assert_enum(user$weight_semantics, .weight_semantics_values, sprintf("tie_meta[[\"%s\"]]$weight_semantics", var_name))
    user$weight_semantics
  } else {
    "unitless"
  }

  zero_meaning <- if (!is.null(user$zero_meaning)) {
    .assert_enum(user$zero_meaning, .zero_meaning_values, sprintf("tie_meta[[\"%s\"]]$zero_meaning", var_name))
    user$zero_meaning
  } else {
    "unknown"
  }

  direction_semantics <- if (!is.null(user$direction_semantics)) as.character(user$direction_semantics) else NA_character_

  # The ledger (R/data-ledger.R) is built from the same `user` list that
  # produced the values above, in this one pass, so the recorded provenance
  # cannot disagree with what was stored.
  #
  # `weighted_inferred` and `presence_mode_inferred` are part of the descriptor
  # because `format.netis_data()` and
  # `.tie_meta_from_descriptors()` read them. They are DERIVED from the ledger
  # rather than computed alongside it: two independently-maintained records of
  # one fact is how a "(inferred)" marking eventually starts contradicting the
  # provenance table three lines below it.
  ledger <- .build_tie_ledger(user)

  list(
    weighted = weighted,
    weighted_inferred = identical(ledger$weighted$state, "inferred"),
    weight_semantics = weight_semantics,
    direction_semantics = direction_semantics,
    zero_meaning = zero_meaning,
    presence_mode = presence_mode,
    presence_mode_inferred = identical(ledger$presence_mode$state, "inferred"),
    presence_col = presence_col,
    presence_rule = presence_rule,
    # Whether the STORED VALUES contain a zero, computed once from the whole
    # tie table rather than from whichever rows a later caller happens to
    # resolve. This is the same reasoning `netis_adjacency()` gives for probing
    # determinacy up front: presence determinacy is a property of the tie
    # variable, not of whether one group's subset happens to contain a zero, so
    # a subset without one must not silently skip the refusal every other
    # subset gets. Consumed by `.resolve_tie_presence()`.
    stored_zeros = any(values == 0, na.rm = TRUE),
    ledger = ledger
  )
}

#' Rebuild a netis_data()-shaped tie_meta list from a schema's descriptors
#'
#' For internal code that reconstructs a netis_data object from an existing one
#' (e.g. `add_baseline_provenance()`, R/exposure-provenance.R): calling
#' `netis_data()` without `tie_meta` would silently drop every user-declared
#' tie semantic -- `zero_meaning`, `presence_mode`, `presence_col`,
#' `presence_rule`, `weight_semantics`, `direction_semantics`, a `weighted`
#' override -- and re-infer from values alone. This converts the resolved descriptors back into the declaration
#' form `netis_data()` accepts.
#'
#' ONLY DECLARED FIELDS ARE PASSED BACK. A field the ledger records as
#' inferred is omitted so reconstruction re-infers it the same way construction
#' did, and a field recorded as undeclared is omitted so it stays undeclared.
#' Passing a neutral default back (`weight_semantics = "unitless"`,
#' `zero_meaning = "unknown"`) would turn it into a declaration, which would
#' change what the ledger reports and switch off refusals that depend on a
#' field being undeclared or inferred.
#'
#' @param schema a current-version netis_data schema (from `netis_schema()`).
#' @return a named list suitable for `netis_data(tie_meta = )`, or `NULL` if
#'   there are no tie descriptors.
#' @noRd
.tie_meta_from_descriptors <- function(schema) {
  descriptors <- schema$tie_descriptors
  if (is.null(descriptors) || length(descriptors) == 0L) {
    return(NULL)
  }
  declared <- function(d, field) {
    identical(d$ledger[[field]]$state, "declared")
  }
  out <- lapply(descriptors, function(d) {
    meta <- list()
    for (f in c("weight_semantics", "zero_meaning", "direction_semantics",
                "weighted", "presence_mode")) {
      if (declared(d, f)) meta[[f]] <- d[[f]]
    }
    if (!is.null(d$presence_col)) meta$presence_col <- d$presence_col
    if (!is.null(d$presence_rule)) meta$presence_rule <- d$presence_rule
    meta
  })
  # netis_data() validates tie_meta as a named list; an entry with nothing
  # declared is simply left out.
  out <- out[lengths(out) > 0L]
  if (length(out) == 0L) NULL else out
}

#' Is a tie variable's edge presence determined, given its descriptor?
#'
#' `row_presence` is always determined (a row's existence *is* presence).
#' `indicator`/`value_rule` are determined once their required
#' `presence_col`/`presence_rule` is set (guaranteed by
#' `.build_one_tie_descriptor()`, which refuses to construct one without it).
#' `complete_dyadic` is determined only when a presence_col or presence_rule
#' has additionally been supplied -- row existence alone does not mean presence
#' there, and without an explicit rule there is no honest way to tell the two
#' apart.
#'
#' @noRd
.tie_presence_determined <- function(descriptor) {
  if (identical(descriptor$presence_mode, "complete_dyadic")) {
    return(!is.null(descriptor$presence_col) || !is.null(descriptor$presence_rule))
  }
  TRUE
}

#' Resolve a 0/1 edge-presence vector for a tie variable's rows
#'
#' The point-of-use consumer `.tie_presence_determined()` exists to guard:
#' operations needing presence call this, and this refuses outright on an
#' undetermined `complete_dyadic` network rather than silently reading
#' presence off `value != 0`. `netis_adjacency(..., presence = TRUE)`
#' (R/data-convert-out.R), `binarize(presence())` and the graph exports all share
#' this function.
#'
#' @param descriptor one entry from `netis_schema(x)$tie_descriptors`.
#' @param tie_data the tie table (or a subset of its rows) the descriptor
#'   was built from -- must contain `var_name` and, if applicable,
#'   `descriptor$presence_col`.
#' @param var_name the tie variable's own column name (used for
#'   `presence_rule`, which is applied to that column's values).
#' @return a logical vector, one element per row of `tie_data`, `TRUE`
#'   where the tie is present.
#' @noRd
.resolve_tie_presence <- function(descriptor, tie_data, var_name) {
  if (!.tie_presence_determined(descriptor)) {
    stop(
      sprintf(
        "Edge presence for '%s' is undetermined: this network is declared presence_mode = \"complete_dyadic\" (rows exist regardless of tie status), but no presence_col or presence_rule was supplied to say which rows are actually present. Say which in netis_data(). If a nonzero value means a tie, pass:\n  %s\nIf a column of your tie table says so, pass:\n  %s\nIf every row is a tie, whatever its value, pass:\n  %s",
        var_name,
        .tie_meta_example(var_name, "presence_mode = \"value_rule\", presence_rule = function(value) value != 0"),
        .tie_meta_example(var_name, "presence_mode = \"indicator\", presence_col = \"<column>\""),
        .tie_meta_example(var_name, "presence_mode = \"row_presence\"")
      ),
      call. = FALSE
    )
  }
  if (identical(descriptor$presence_mode, "row_presence")) {
    .assert_inferred_row_presence_unambiguous(descriptor, var_name)
    return(rep(TRUE, nrow(tie_data)))
  }
  # The declaration check at construction leaves each remaining mode with exactly
  # one of the two: "indicator" a column, "value_rule" a rule, "complete_dyadic"
  # whichever it was given.
  if (!identical(descriptor$presence_mode, "value_rule") && !is.null(descriptor$presence_col)) {
    return(as.logical(tie_data[[descriptor$presence_col]]))
  }
  as.logical(descriptor$presence_rule(tie_data[[var_name]]))
}

#' A `tie_meta` declaration a reader can paste into `netis_data()`
#'
#' @param var_name the tie variable.
#' @param fields the entry's fields, as R source.
#' @return one line of R source.
#' @noRd
.tie_meta_example <- function(var_name, fields) {
  key <- if (identical(make.names(var_name), var_name)) var_name else sprintf("`%s`", var_name)
  sprintf("tie_meta = list(%s = list(%s))", key, fields)
}

#' Refuse an INFERRED `row_presence` whose stored zeros have no settled meaning
#'
#' `row_presence` means "a stored row is an existing tie". That reading is
#' unambiguous for a genuine sparse edge list, where a zero-valued row does not
#' occur -- a zero-valued row in one is itself the anomaly. When rnetis has
#' *inferred* the mode from table shape and the values nevertheless contain
#' zeros, the package is being asked to decide something it cannot know: whether
#' a stored zero is an observed zero-strength relationship (present, with weight
#' zero) or an encoding of structural absence (not a tie at all).
#'
#' Guessing here is not a metadata default. Presence feeds degree, density,
#' reference sets, isolate status and exposure, so the guess propagates into
#' estimates. That is why this refuses rather than warns: the same rule that
#' makes an undetermined `complete_dyadic` network an ERROR applies: a
#' measured zero and a structurally absent edge are never automatically the same thing.
#'
#' The refusal is deliberately narrow. It applies only when the mode was
#' inferred; a user who *declares* `presence_mode = "row_presence"` has made the
#' call themselves and is taken at their word. And it applies only when zeros
#' are actually stored -- the ordinary sparse edge list, which is the case the
#' inference exists to serve, is untouched.
#'
#' @param descriptor one entry from `netis_schema(x)$tie_descriptors`.
#' @param var_name the tie variable's column name, for the message.
#' @return `TRUE` invisibly, or an error.
#' @noRd
.assert_inferred_row_presence_unambiguous <- function(descriptor, var_name) {
  inferred <- identical(descriptor$ledger$presence_mode$state, "inferred")
  if (!inferred || !isTRUE(descriptor$stored_zeros)) {
    return(invisible(TRUE))
  }
  # A declared measured zero settles it: the dyad was observed and its
  # relationship has zero magnitude, so the row is a present tie.
  if (identical(descriptor$zero_meaning, "measured_zero")) {
    return(invisible(TRUE))
  }

  common <- sprintf(
    "rnetis inferred presence_mode = \"row_presence\" for '%s' from the tie table's shape, which reads every stored row as an existing tie. But '%s' contains stored zero values, and a zero row is not what a sparse edge list looks like.",
    var_name, var_name
  )
  resolution <- sprintf(
    "Resolve it in netis_data(). If the zeros are observed zero-strength ties, so every row is present, pass:\n  %s\nIf the zeros mean no tie, pass:\n  %s\nIf a column of your tie table says so, pass:\n  %s\nIf every row counts as a tie regardless, pass:\n  %s",
    .tie_meta_example(var_name, "zero_meaning = \"measured_zero\""),
    .tie_meta_example(var_name, "presence_mode = \"value_rule\", presence_rule = function(value) value != 0"),
    .tie_meta_example(var_name, "presence_mode = \"indicator\", presence_col = \"<column>\""),
    .tie_meta_example(var_name, "presence_mode = \"row_presence\"")
  )

  if (identical(descriptor$zero_meaning, "structural_absence")) {
    stop(
      paste(
        common,
        sprintf("You declared zero_meaning = \"structural_absence\" for '%s', which says those zeros are NOT ties -- that contradicts the inferred row_presence reading rather than resolving it.", var_name),
        resolution
      ),
      call. = FALSE
    )
  }

  stop(
    paste(
      common,
      "rnetis will not guess whether they are observed zero-strength relationships or encodings of structural absence: presence feeds degree, density, isolate status and exposure, so the guess would propagate into your estimates.",
      resolution
    ),
    call. = FALSE
  )
}
