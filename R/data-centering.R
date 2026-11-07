# Centering in the data layer: add_centering() for the two one-column methods
# -- grand-mean and a user reference constant -- which have no basis-ambiguity
# question (that concern is specific to the mean-REMOVING methods
# add_decomposition() implements):
#
#   "none"        z              --
#   "grand"       z - z-bar      pure reparameterization, predictions invariant
#   user constant z - c          same, at a substantively chosen reference
#
# THE TRANSFORMATION LAYER NEVER ENTERS A PREDICTOR INTO A MODEL. This
# produces and registers a derived column; nothing here touches a formula or
# a fit. The raw variable is preserved unchanged -- the centered
# column is added, never substituted.
#
#' Add a centered representation of a node- or group-level variable
#'
#' Produces a new derived column -- `z` minus its grand mean, or minus a user-
#' supplied reference constant -- and registers it with its provenance.
#' Never enters the model: the returned object has one more column available to
#' a formula, and nothing else changes. The raw column is untouched.
#'
#' # Which method
#'
#' `method = "grand"` centers on the variable's own mean over every row it
#' appears in (every node-wave, for a dynamic node variable; every node, for
#' a static one; and the group-level equivalents). `method = "constant"`
#' centers on `at`, a value **you** choose -- a natural zero, a scale
#' midpoint, or any other substantively meaningful reference. Both are pure
#' reparameterizations: a model's fitted predictions are identical whichever
#' you pick (or neither); only the intercept and this term's own coefficient
#' change interpretation, being evaluated at the new zero instead of the raw
#' scale's zero. Contrast [add_decomposition()], whose mean-removing methods
#' do **not** have this property.
#'
#' @param data a [netis_data()] object.
#' @param var name of a numeric (or logical) node- or group-level variable,
#'   static or dynamic, to center. A factor or character variable has no mean
#'   and is refused, as is a dyadic (tie-level) variable.
#' @param method `"grand"` (default) or `"constant"`.
#' @param at reference value for `method = "constant"`: a single finite number.
#'   Required for that method; must be `NULL` for `"grand"`, where the basis is
#'   computed, not supplied.
#' @param name name for the new column. Defaults to `"<var>_grand"` or
#'   `"<var>_c"`.
#' @return a [netis_data()] object with the new variable added and its
#'   provenance registered (`netis_schema(data)$derived`).
#' @references
#' Enders, C. K. and Tofighi, D. (2007) Centering predictor variables in
#' cross-sectional multilevel models: a new look at an old issue.
#' *Psychological Methods* **12**(2), 121-138.
#' @seealso [add_decomposition()] for the mean-removing methods,
#'   [add_baseline_provenance()] for the same "derive and attach" idiom
#'   applied to B-I provenance.
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b", "c"), age = c(22, 41, 35))
#' ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"))
#' nd <- netis_data(nodes, ties)
#'
#' nd2 <- add_centering(nd, "age", method = "grand")
#' netis_nodes(nd2)$age_grand
#'
#' nd3 <- add_centering(nd, "age", method = "constant", at = 18)
#' netis_nodes(nd3)$age_c
#' @export
add_centering <- function(data, var, method = c("grand", "constant"), at = NULL,
                          name = NULL) {
  .check_netis_data(data)
  method <- match.arg(method)
  if (!is.character(var) || length(var) != 1L) {
    stop("var must be a single column name.", call. = FALSE)
  }
  if (identical(method, "constant")) {
    if (is.null(at) || !is.numeric(at) || length(at) != 1L || !is.finite(at)) {
      stop("method = \"constant\" requires at = <a single finite reference number>.",
           call. = FALSE)
    }
  } else if (!is.null(at)) {
    stop("at = is only meaningful for method = \"constant\"; method = \"grand\" ",
         "computes its own basis and does not take a reference value.",
         call. = FALSE)
  }

  loc <- .locate_derivable_var(data, var)
  s <- netis_schema(data)
  x <- loc$table[[var]]
  # The rows that count once each. A static variable holds one value per entity,
  # repeated at each wave the entity was observed, so averaging over the long
  # table would weight each entity by its number of waves.
  entity_cols <- if (identical(loc$level, "node")) c(s$group_col, s$node_col) else s$group_col
  once <- if ("wave" %in% loc$row_indices) {
    rep(TRUE, length(x))
  } else {
    !duplicated(loc$table[entity_cols])
  }
  eligible_n <- sum(!is.na(x[once]))
  if (eligible_n == 0L) {
    stop(sprintf("'%s' is missing in every row; there is nothing to center.", var),
         call. = FALSE)
  }

  if (identical(method, "grand")) {
    basis <- mean(x[once], na.rm = TRUE)
  } else {
    basis <- as.numeric(at)
  }
  centered <- x - basis

  if (is.null(name)) {
    name <- paste0(var, if (identical(method, "grand")) "_grand" else "_c")
  }
  if (name %in% names(loc$table)) {
    stop(sprintf(
      "add_centering('%s', ...) would produce a column named '%s', which already exists. Supply name = to choose a different one.",
      var, name), call. = FALSE)
  }

  record <- .new_derived_record(
    source = var, method = method,
    averaged_over = if (identical(method, "grand")) loc$row_indices else character(0),
    held = character(0), role = "centered", pair = NULL,
    basis = basis, eligible_n = eligible_n
  )

  .rebuild_with_derived_column(data, loc$level, name, centered, record)
}

#' Locate a node- or group-level variable and describe what indexes its rows
#'
#' @param data a `netis_data` object.
#' @param var the variable name.
#' @return a list with `level` (`"node"` or `"group"`), `table` (the full
#'   logical accessor's output), and `row_indices` (which of `"node"`,
#'   `"group"`, `"wave"` distinguish one row from another at this level --
#'   e.g. a dynamic node variable's rows are indexed by node AND wave, so
#'   centering "over every row" for it means averaging over both).
#' @noRd
.locate_derivable_var <- function(data, var) {
  s <- netis_schema(data)
  is_node <- var %in% c(s$node_vars$static, s$node_vars$dynamic)
  is_group <- var %in% c(s$group_vars$static, s$group_vars$dynamic)
  if (is_node && is_group) {
    # A node table and a group table may each hold a column of this name, and a
    # silent pick between the two levels would center the wrong variable.
    stop(
      sprintf("'%s' is both a node variable and a group variable in this netis_data object, so rnetis cannot tell which one you mean. Rename one of them.", var),
      call. = FALSE
    )
  }
  numeric_values <- function(tbl) {
    x <- tbl[[var]]
    if (!is.numeric(x) && !is.logical(x)) {
      stop(
        sprintf("'%s' is %s, not numeric, so it has no mean to center on. Convert it to a numeric column first.",
                var, class(x)[1L]),
        call. = FALSE
      )
    }
    tbl
  }
  if (is_node) {
    tbl <- numeric_values(netis_nodes(data))
    dynamic <- var %in% s$node_vars$dynamic
    return(list(level = "node", table = tbl,
                row_indices = if (dynamic) c("node", "wave") else "node"))
  }
  if (is_group) {
    tbl <- numeric_values(netis_groups(data))
    dynamic <- var %in% s$group_vars$dynamic
    return(list(level = "group", table = tbl,
                row_indices = if (dynamic) c("group", "wave") else "group"))
  }
  stop(
    sprintf(
      "'%s' is not a node- or group-level variable in this netis_data object. Available node variables: %s. Available group variables: %s. Dyadic (tie-level) variables are not supported by add_centering()/add_decomposition().",
      var,
      paste(c(s$node_vars$static, s$node_vars$dynamic), collapse = ", "),
      if (length(c(s$group_vars$static, s$group_vars$dynamic)) == 0L) "(none)"
      else paste(c(s$group_vars$static, s$group_vars$dynamic), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' Rebuild a netis_data object with one new derived column merged in
#'
#' Thin wrapper over `.rebuild_with_derived_columns()` (plural) for
#' `add_centering()`'s one-column methods.
#'
#' @noRd
.rebuild_with_derived_column <- function(data, level, col_name, values, record) {
  .rebuild_with_derived_columns(
    data, level,
    stats::setNames(list(values), col_name),
    stats::setNames(list(record), col_name)
  )
}

#' Rebuild a netis_data object with one or more new derived columns merged in
#'
#' Shared reconstruction path for `add_centering()` and `add_decomposition()`:
#' merges the derived column(s) into the node or group table, registers their
#' provenance, and reconstructs through `netis_data()` exactly the way
#' `add_baseline_provenance()` reconstructs after adding a provenance column.
#' Everything the original object carried survives: tie semantics with their
#' ledger states (`.tie_meta_from_descriptors()` passes back only what was
#' declared, so an inferred or undeclared field stays inferred or
#' undeclared), declared metric time (`.time_meta_from_index()`), the import
#' boundary record, and any attributes attached to the object
#' (`.carry_object_state()`).
#'
#' `add_decomposition()` needs BOTH of a mean-removing method's columns
#' merged and registered in ONE reconstruction, not two sequential calls --
#' two calls would mean the first reconstruction's `netis_data()` briefly
#' existed with only half a decomposition registered, and `add_decomposition()` can only ever
#' return both.
#'
#' @param data the original `netis_data`.
#' @param level `"node"` or `"group"`.
#' @param columns a named list of new column value vectors, aligned to
#'   `netis_nodes(data)`'s or `netis_groups(data)`'s own row order.
#' @param records a named list of `netis_derived_var` records, same names as
#'   `columns`.
#' @return the reconstructed `netis_data`.
#' @noRd
.rebuild_with_derived_columns <- function(data, level, columns, records) {
  s <- netis_schema(data)
  new_derived <- .add_derived_records(s$derived, records)

  nodes_new <- netis_nodes(data)
  groups_new <- netis_groups(data)
  for (nm in names(columns)) {
    if (identical(level, "node")) {
      nodes_new[[nm]] <- columns[[nm]]
    } else {
      groups_new[[nm]] <- columns[[nm]]
    }
  }

  out <- netis_data(
    nodes_new, netis_ties(data), group_data = groups_new,
    group_col = s$group_col, node_col = s$node_col, wave_col = s$wave_col,
    source_col = s$source_col, target_col = s$target_col,
    directed = s$directed, self_ties = s$self_ties,
    tie_meta = .tie_meta_from_descriptors(s),
    time_meta = .time_meta_from_index(s),
    observed_tie_waves = .observed_tie_waves_declaration(s, netis_ties(data)),
    derived_meta = new_derived
  )
  .carry_object_state(out, data)
}

#' Recover a `netis_data(time_meta = )` declaration from a schema's time index
#'
#' Only the rows whose `time_source` is `"declared"` are returned: an absent
#' observation time stays absent. `NULL` when nothing was declared.
#'
#' @noRd
.time_meta_from_index <- function(schema) {
  ti <- schema$time_index
  if (is.null(ti) || nrow(ti) == 0L || is.null(schema$wave_col)) {
    return(NULL)
  }
  ti <- ti[ti$time_source == "declared", , drop = FALSE]
  if (nrow(ti) == 0L) {
    return(NULL)
  }
  out <- data.frame(ti$group, ti$wave_label, ti$time_value, ti$time_unit,
                    stringsAsFactors = FALSE)
  names(out) <- c(schema$group_col, schema$wave_col, "time_value", "time_unit")
  out
}

#' Carry what a rebuilt object would otherwise lose from the original
#'
#' A derive-and-attach helper rebuilds the object through `netis_data()`,
#' which knows nothing about where the original came from or what a caller
#' attached to it. The import boundary record and every attribute other than
#' the object's own structure are copied back.
#'
#' @noRd
.carry_object_state <- function(new, old) {
  new$schema$boundary <- netis_schema(old)$boundary
  extra <- setdiff(names(attributes(old)), c("names", "class"))
  for (a in extra) {
    attr(new, a) <- attr(old, a)
  }
  new
}

# ---------------------------------------------------------------------------
# add_decomposition(): the four mean-removing averaging units
# ---------------------------------------------------------------------------
#
# Four units -- the averaging unit IS the
# representation, so the primary argument names it directly rather
# than a colloquial "group" that would re-encode the very ambiguity the
# averaging unit exists to remove:
#
#   unit           averages over   held fixed         applies to
#   "actor"        wave            node               node-dynamic
#   "group_wave"   wave            group               group-dynamic
#   "context"      node            group x wave        node-level
#   "group_pooled" node, wave      group               node-level
#
# add_decomposition() can ONLY EVER return both components together --
# there is no argument to ask for only one. The removed mean is always
# produced as its own column; whether a model uses it is a term-resolution
# question, never something this layer decides.

#' Add a within/mean decomposition of a node- or group-level variable
#'
#' Splits `var` into a deviation from a mean and the mean itself, computed over
#' the averaging unit you name. Unlike [add_centering()]'s
#' grand/constant methods, this is **not** a prediction- invariant
#' reparameterization: the deviation column carries only the variation within
#' the averaging unit, so a model using it (with or without the paired mean
#' column) asks a different question than a model using the raw variable, and
#' neither is more correct in general.
#'
#' # The four units
#'
#' rnetis carries three identifiers (node, group, wave) where the multilevel
#' literature's own vocabulary assumes two, so "the group mean" already
#' names several different quantities. `unit = ` says explicitly which one:
#'
#' \describe{
#'   \item{`"actor"`}{Averages over waves, holding node fixed. The
#'     person/actor mean: for a **node-dynamic** variable, `z_i.` is node
#'     `i`'s own mean across the waves it is observed, `z_within` is its
#'     deviation from that mean at each wave.}
#'   \item{`"group_wave"`}{Averages over waves, holding group fixed, for a
#'     **group-dynamic** variable.}
#'   \item{`"context"`}{Averages over nodes, holding group x wave fixed. The
#'     contextual/cluster mean for a **node-level** variable: how a node
#'     compares to its own group's other members AT THAT WAVE. What most
#'     applied researchers mean by "group-mean centering."}
#'   \item{`"group_pooled"`}{Averages over both nodes and waves, holding
#'     group fixed. A **node-level** variable's group pooled across its
#'     whole observed history -- distinct from `"context"`, and easy to
#'     confuse with it.}
#' }
#'
#' Splitting a variable into within and between parts is standard practice; this
#' four-way naming of the averaging unit is the package's own, because the
#' literature's two-level vocabulary does not distinguish them. A node variable
#' on data whose node table has no wave column has only one row per node, so
#' `"context"` and `"group_pooled"` then give the same numbers.
#'
#' A variable with no within-unit variation at all is refused, since the within
#' column would be identically zero. A unit observed at only one occasion, while
#' others have more, gets a message and a within value of zero.
#'
#' @param data a [netis_data()] object.
#' @param var name of the variable to decompose. A dyadic (tie-level) variable is
#'   refused.
#' @param unit one of `"actor"`, `"group_wave"`, `"context"`,
#'   `"group_pooled"`. Which units a given variable is eligible for is
#'   checked: `"group_wave"` applies to a dynamic group-level variable and to
#'   nothing else; `"actor"` to a dynamic node variable only; `"context"` and
#'   `"group_pooled"` to any node variable, static or dynamic.
#' @param name_within,name_between names for the two new columns. Default to
#'   `"<var>_within"`/`"<var>_between"`.
#' @return a [netis_data()] object with both new variables added and their
#'   provenance registered (`netis_schema(data)$derived`), each pointing at
#'   its sibling.
#' @references
#' Curran, P. J. and Bauer, D. J. (2011) The disaggregation of within-person and
#' between-person effects in longitudinal models of change. *Annual Review of
#' Psychology* **62**, 583-619.
#'
#' Enders, C. K. and Tofighi, D. (2007) Centering predictor variables in
#' cross-sectional multilevel models: a new look at an old issue.
#' *Psychological Methods* **12**(2), 121-138.
#' @seealso [add_centering()] for the prediction-invariant methods.
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = rep(c("a", "b"), each = 3), wave = rep(1:3, 2),
#'   mood = c(0.1, 0.4, -0.2, 1.1, 0.9, 1.4)
#' )
#' ties <- data.frame(group = 1, source = "a", target = "b", wave = 1:3)
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#'
#' # Each node's own deviation from ITS OWN average across waves.
#' nd2 <- add_decomposition(nd, "mood", unit = "actor")
#' netis_nodes(nd2)[c("node", "wave", "mood_within", "mood_between")]
#' @export
add_decomposition <- function(data, var,
                              unit = c("actor", "group_wave", "context", "group_pooled"),
                              name_within = NULL, name_between = NULL) {
  .check_netis_data(data)
  unit <- match.arg(unit)
  if (!is.character(var) || length(var) != 1L) {
    stop("var must be a single column name.", call. = FALSE)
  }
  loc <- .locate_derivable_var(data, var)
  .assert_decomposition_compatible(loc, unit, var)
  s <- netis_schema(data)

  dec <- switch(
    unit,
    actor        = .decompose_actor(loc, var, s),
    group_wave   = .decompose_group_wave(loc, var, s),
    context      = .decompose_context(loc, var, s),
    group_pooled = .decompose_group_pooled(loc, var, s)
  )

  # A whole-dataset zero-within-variance refusal, DISTINCT from and IN
  # ADDITION TO each .decompose_*()'s per-unit single-occasion diagnostic
  # above. A unit can have multiple occasions (so no single-occasion
  # warning fires) and STILL contribute zero within-variance if the raw
  # value simply never changes across them -- e.g. a group-level feature
  # fed by a static network, observed at every wave but constant at every
  # one. That case is aliased with nothing to estimate and is refused here.
  within_sd <- stats::sd(dec$within, na.rm = TRUE)
  if (is.na(within_sd) || within_sd == 0) {
    stop(
      sprintf(
        "add_decomposition('%s', unit = \"%s\") is not possible: it has no within-unit variation, so the within component is identically zero and only the between component would be estimable. Drop it, or use a variable/unit combination where it actually varies.",
        var, unit),
      call. = FALSE
    )
  }

  if (is.null(name_within)) name_within <- paste0(var, "_within")
  if (is.null(name_between)) name_between <- paste0(var, "_between")
  for (nm in c(name_within, name_between)) {
    if (nm %in% names(loc$table)) {
      stop(sprintf(
        "add_decomposition('%s', unit = \"%s\") would produce a column named '%s', which already exists. Supply name_within = / name_between = to choose different ones.",
        var, unit, nm), call. = FALSE)
    }
  }
  if (identical(name_within, name_between)) {
    stop("name_within and name_between must be different.", call. = FALSE)
  }

  within_record <- .new_derived_record(
    source = var, method = unit, averaged_over = dec$averaged_over,
    held = dec$held, role = "centered", pair = name_between,
    basis = dec$basis, eligible_n = dec$eligible_n
  )
  between_record <- .new_derived_record(
    source = var, method = unit, averaged_over = dec$averaged_over,
    held = dec$held, role = "mean", pair = name_within,
    basis = dec$basis, eligible_n = dec$eligible_n
  )

  .rebuild_with_derived_columns(
    data, loc$level,
    stats::setNames(list(dec$within, dec$between), c(name_within, name_between)),
    stats::setNames(list(within_record, between_record), c(name_within, name_between))
  )
}

#' The compatibility table: which units a variable's class is eligible for
#' @noRd
.assert_decomposition_compatible <- function(loc, unit, var) {
  dynamic <- length(loc$row_indices) > 1L  # has a wave index, i.e. varies over wave
  ok <- if (identical(loc$level, "group")) {
    identical(unit, "group_wave") && dynamic
  } else {
    switch(unit,
      actor = dynamic,
      group_wave = FALSE,  # group_wave is defined for GROUP-level variables only
      context = TRUE,      # applies to static or dynamic node variables (degenerates for static)
      group_pooled = TRUE
    )
  }
  if (ok) {
    return(invisible(TRUE))
  }
  reason <- if (identical(loc$level, "group") && !identical(unit, "group_wave")) {
    sprintf("'%s' is group-level; only unit = \"group_wave\" applies (it has no node dimension to average over).", var)
  } else if (identical(loc$level, "group") && !dynamic) {
    sprintf("'%s' is a STATIC group variable; unit = \"group_wave\" averages over waves, and there is no wave variation to average.", var)
  } else if (identical(unit, "actor") && !dynamic) {
    sprintf("'%s' is a STATIC node variable; unit = \"actor\" averages over waves, and there is no wave variation to average.", var)
  } else if (identical(unit, "group_wave")) {
    sprintf("'%s' is node-level; unit = \"group_wave\" is defined for group-level variables only (it averages over waves holding GROUP fixed, with no node dimension at all). Use unit = \"context\" or \"group_pooled\" for a node-level variable's group decomposition.", var)
  } else {
    sprintf("'%s' is not eligible for unit = \"%s\".", var, unit)
  }
  stop(paste(reason, "See ?add_decomposition for which units each variable kind supports."),
       call. = FALSE)
}

#' A per-unit occasion-count diagnostic for a mean-removing decomposition
#'
#' A unit (node, or group under `"group_wave"`) observed at only one occasion
#' contributes an identically-zero within component -- not missing, not
#' wrong, just uninformative. Under a ragged roster this is a per-unit
#' property, so the honest report is how many units are in that state
#' (the group-level-predictor reasoning, applied per-unit), not a
#' whole-dataset refusal UNLESS every unit is in it, in which case the
#' within column would be identically zero everywhere and there is nothing
#' to decompose at all.
#'
#' @param single_occasion logical vector, one per averaging cell (keyed by
#'   the unit being held fixed).
#' @param unit the unit name, for the message.
#' @param what what kind of unit these are (e.g. `"node"`, `"group"`), for
#'   the message.
#' @return invisibly `TRUE`; messages or errors as a side effect.
#' @noRd
.assert_decomposition_has_variation <- function(single_occasion, unit, what) {
  if (all(single_occasion)) {
    stop(
      sprintf(
        "add_decomposition(unit = \"%s\") is not possible: every %s in this data is observed at only one occasion, so the within component would be identically zero everywhere -- there is no within-%s-over-time variation to decompose.",
        unit, what, what),
      call. = FALSE
    )
  }
  if (any(single_occasion)) {
    message(sprintf(
      "add_decomposition(unit = \"%s\"): %d of %d %s(s) are observed at only one occasion, so their within component is identically zero (not missing, not informative).",
      unit, sum(single_occasion), length(single_occasion), what))
  }
  invisible(TRUE)
}

#' unit = "actor": averages over waves, holds node fixed
#' @noRd
.decompose_actor <- function(loc, var, s) {
  tbl <- loc$table
  key <- .netis_unit_key(tbl[[s$group_col]], tbl[[s$node_col]])
  z <- tbl[[var]]
  means <- tapply(z, key, mean, na.rm = TRUE)
  n_obs <- tapply(!is.na(z), key, sum)
  .assert_decomposition_has_variation(n_obs <= 1L, "actor", "node")

  between <- as.numeric(means[key])
  within <- z - between
  list(within = within, between = between, basis = means,
       averaged_over = "wave", held = "node", eligible_n = sum(!is.na(z)))
}

#' The "group_wave" estimand's core arithmetic: per-key means over ALREADY
#' one-row-per-key data, i.e. the occasion mean rather than the row mean
#'
#' Used by `.decompose_group_wave()`, reading directly from `netis_groups()`'s
#' own one-row-per-group-wave data.
#'
#' @param key a vector, one value per row, identifying which averaging cell
#'   each row belongs to -- for a table already deduplicated to one row per
#'   cell, this is simply the grouping key.
#' @param z the values to average, aligned to `key`.
#' @return a list with `means` (named by `key`'s levels) and `n_obs` (the
#'   non-`NA` count per cell, for the single-occasion diagnostic).
#' @noRd
.decomposition_group_means <- function(key, z) {
  list(means = tapply(z, key, mean, na.rm = TRUE),
       n_obs = tapply(!is.na(z), key, sum))
}

#' unit = "group_wave": averages over waves, holds group fixed
#' @noRd
.decompose_group_wave <- function(loc, var, s) {
  tbl <- loc$table
  key <- as.character(tbl[[s$group_col]])
  z <- tbl[[var]]
  gm <- .decomposition_group_means(key, z)
  .assert_decomposition_has_variation(gm$n_obs <= 1L, "group_wave", "group")

  between <- as.numeric(gm$means[key])
  within <- z - between
  list(within = within, between = between, basis = gm$means,
       averaged_over = "wave", held = "group", eligible_n = sum(!is.na(z)))
}

#' unit = "context": averages over nodes, holds group x wave fixed -- the
#' contextual/cluster mean
#' @noRd
.decompose_context <- function(loc, var, s) {
  tbl <- loc$table
  has_wave <- !is.null(s$wave_col) && s$wave_col %in% names(tbl)
  key <- if (has_wave) {
    paste(tbl[[s$group_col]], tbl[[s$wave_col]], sep = .NETIS_KEY_SEP)
  } else {
    # A node table with no wave column: the cell degenerates to "within group"
    # alone, which coincides numerically with group_pooled, as
    # add_decomposition()'s help page says.
    as.character(tbl[[s$group_col]])
  }
  z <- tbl[[var]]
  means <- tapply(z, key, mean, na.rm = TRUE)
  n_obs <- tapply(!is.na(z), key, sum)
  .assert_decomposition_has_variation(n_obs <= 1L, "context", if (has_wave) "group-wave cell" else "group")

  between <- as.numeric(means[key])
  within <- z - between
  list(within = within, between = between, basis = means,
       averaged_over = "node", held = if (has_wave) c("group", "wave") else "group",
       eligible_n = sum(!is.na(z)))
}

#' unit = "group_pooled": averages over both nodes and waves, holds group fixed
#' @noRd
.decompose_group_pooled <- function(loc, var, s) {
  tbl <- loc$table
  key <- as.character(tbl[[s$group_col]])
  z <- tbl[[var]]
  means <- tapply(z, key, mean, na.rm = TRUE)
  n_obs <- tapply(!is.na(z), key, sum)
  .assert_decomposition_has_variation(n_obs <= 1L, "group_pooled", "group")

  between <- as.numeric(means[key])
  within <- z - between
  list(within = within, between = between, basis = means,
       averaged_over = c("node", "wave"), held = "group", eligible_n = sum(!is.na(z)))
}
