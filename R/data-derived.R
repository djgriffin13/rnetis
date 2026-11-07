# Derived-variable provenance: the schema-level record that makes a
# centered/decomposed column's averaging unit first-class rather than a name.
#
# rnetis carries three identifiers -- node, group, wave -- where the
# multilevel-modeling literature's vocabulary assumes two, so "the group
# mean" already names at least three distinct quantities before any new
# method is added: averaged over waves within a group, averaged over nodes within a
# group x wave (the contextual/cluster mean most applied researchers mean by
# "group-mean centering"), and averaged over both nodes and waves within a
# group (pooling a group's members across its whole observed history). A
# user-facing method name is an ALIAS over the averaging unit, offered only
# where it maps to exactly one of these -- never the representation itself.
#
# One provenance record answers, for one derived column: what was it derived
# FROM, by AVERAGING OVER which indices while HOLDING which others fixed,
# and what basis values (means or a constant) resulted. `role` and `pair`
# exist because a mean-removing method always produces TWO columns together
# (`add_decomposition()` can only ever return both) -- the centered
# deviation and the mean/context component -- and each one's record points
# at its sibling, so a consumer holding one column's provenance can always
# find the other.

#' One derived-variable provenance record
#'
#' @param source the source column this was derived from.
#' @param method a short mnemonic name for the averaging unit (e.g.
#'   `"grand"`, `"constant"`, `"actor"`, `"group_wave"`, `"context"`,
#'   `"group_pooled"`) -- documentary only. `averaged_over`/`held` are what
#'   code reads: the alias is never the representation.
#' @param averaged_over character vector of index names (`"node"`, `"group"`,
#'   `"wave"`) the basis averages over.
#' @param held character vector of index names the basis holds fixed --
#'   together with `averaged_over`, every one of `"node"`, `"group"`,
#'   `"wave"` that applies to this variable's level must appear in exactly
#'   one of the two, so the unit is unambiguous by construction.
#' @param role `"centered"` (the deviation/recentered column) or `"mean"`
#'   (the removed mean/reference component). A one-column method (`"grand"`,
#'   `"constant"`) is `"centered"` with `pair = NULL`.
#' @param pair the name of the companion derived column, or `NULL` for a
#'   one-column method.
#' @param basis the computed basis: a single constant for `"grand"`/
#'   `"constant"`, or a named numeric vector (one value per averaging cell,
#'   e.g. per group or per node) for a mean-removing method.
#' @param eligible_n the number of rows/cells the basis was computed from --
#'   recorded so a later audit can see what "the declared eligible
#'   observations" actually were, without recomputing it.
#' @return a list of class `netis_derived_var`.
#' @noRd
.new_derived_record <- function(source, method, averaged_over, held, role,
                                pair, basis, eligible_n) {
  stopifnot(
    is.character(source), length(source) == 1L,
    is.character(method), length(method) == 1L,
    is.character(averaged_over), is.character(held),
    role %in% c("centered", "mean"),
    is.null(pair) || (is.character(pair) && length(pair) == 1L)
  )
  structure(
    list(source = source, method = method,
         unit = list(averaged_over = averaged_over, held = held),
         role = role, pair = pair, basis = basis, eligible_n = eligible_n),
    class = "netis_derived_var"
  )
}

#' Attach one or more derived-variable records to a schema's registry
#'
#' @param existing the schema's current `derived` list (possibly empty).
#' @param new_records a named list of new `netis_derived_var` records, keyed
#'   by derived-column name.
#' @return the merged registry.
#' @noRd
.add_derived_records <- function(existing, new_records) {
  collide <- intersect(names(existing), names(new_records))
  if (length(collide) > 0L) {
    stop(
      sprintf(
        "Derived column name%s %s already registered on this netis_data object. Rename the target, or drop the existing derived variable first.",
        if (length(collide) > 1L) "s" else "",
        paste(sprintf("'%s'", collide), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  c(existing, new_records)
}

#' One-line prose description of a derived column, or NULL if it is not one
#'
#' A consumer that needs to STATE what a column is, not just look up its
#' fields -- the "the fit reports which representation was used"
#' requirement for a moderated term's moderator/moderand -- shares this
#' rather than hand-formatting `netis_derived_var`'s fields itself.
#'
#' @param derived a schema's `derived` registry (`netis_schema(x)$derived`,
#'   a named list of `netis_derived_var`, possibly empty).
#' @param colname the column name to describe.
#' @return a single string, or `NULL` if `colname` is not a registered
#'   derived variable.
#' @noRd
.derived_provenance_note <- function(derived, colname) {
  rec <- derived[[colname]]
  if (is.null(rec)) {
    return(NULL)
  }
  # add_decomposition() always sets `pair` (it can only ever return
  # both components together); add_centering()'s one-column methods never
  # do. Reused for BOTH which family-word ("centering" vs "decomposition")
  # describes `rec$method`, and which prose the role gets -- one source of
  # truth rather than two conditions that could drift apart.
  is_decomposition <- !is.null(rec$pair)
  role_text <- if (identical(rec$role, "mean")) {
    "the mean/context component"
  } else if (is_decomposition) {
    "the within/centered component"
  } else {
    "a centered representation"
  }
  family_word <- if (is_decomposition) "decomposition" else "centering"
  unit_text <- if (length(rec$unit$averaged_over) == 0L) {
    # add_centering(method = "constant"): no basis to average, just a fixed
    # reference value the user supplied.
    sprintf("relative to a fixed reference (%s %s)", rec$method, family_word)
  } else if (length(rec$unit$held) == 0L) {
    # add_centering(method = "grand"): averages over every row the source
    # variable appears in, with nothing held fixed (there is only one cell).
    sprintf(
      "averaged over %s (%s %s)",
      paste(rec$unit$averaged_over, collapse = "+"), rec$method, family_word
    )
  } else {
    sprintf(
      "averaged over %s, held within %s (%s %s)",
      paste(rec$unit$averaged_over, collapse = "+"),
      paste(rec$unit$held, collapse = "+"), rec$method, family_word
    )
  }
  sprintf("'%s' is %s of '%s', %s.", colname, role_text, rec$source, unit_text)
}

#' @exportS3Method base::print
print.netis_derived_var <- function(x, ...) {
  # A centering on a supplied constant averages nothing, and the grand mean holds
  # nothing fixed; each says only what it has.
  unit_text <- if (length(x$unit$averaged_over) == 0L) {
    "relative to a fixed reference"
  } else if (length(x$unit$held) == 0L) {
    sprintf("averaged over %s", paste(x$unit$averaged_over, collapse = "+"))
  } else {
    sprintf("averaged over %s, held within %s",
            paste(x$unit$averaged_over, collapse = "+"),
            paste(x$unit$held, collapse = "+"))
  }
  cat(sprintf(
    "<derived variable> source: %s   method: %s (%s)\n",
    x$source, x$method, unit_text
  ))
  cat(sprintf("  role: %s%s\n", x$role,
              if (!is.null(x$pair)) sprintf(" (paired with '%s')", x$pair) else ""))
  invisible(x)
}
