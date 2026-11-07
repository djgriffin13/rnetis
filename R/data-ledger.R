# The semantic ledger.
#
# Every declaration a rnetis network carries is stored with WHERE IT CAME FROM,
# not just with its value. The distinction is computational, not documentary:
# `weight_semantics = "unitless"` is the recorded ABSENCE of a claim, and a
# consumer that cannot tell it apart from a user who typed "unitless" will
# eventually treat a neutral default as an assertion. The gate is
# written against these states, which is why they live beside the values rather
# than in a comment.
#
# Three states, and only three:
#
#   declared   -- the caller said so, or a rnetis-authored record on the source
#                 object said so
#   inferred   -- rnetis derived it from structure, by the NAMED rule recorded
#                 alongside it
#   undeclared -- rnetis does not know; the stored value is a neutral default
#
# `directedness` is deliberately not one of the per-variable fields. It is a
# property of the tie table's structure, not of a value column in it, so its
# provenance sits at schema level -- and it is the one field that may legally be
# read off a source object, because igraph and `network` genuinely encode it.
# It may never be `inferred`: directedness is not derived from matrix symmetry
# or edgelist reciprocity, and `.ledger_directedness()` below has
# no code path that produces that state.

#' @noRd
.ledger_states <- c("declared", "inferred", "undeclared")

#' The per-tie-variable fields the ledger tracks
#'
#' `directedness` is dataset-level (see this
#' file's header) and static-vs-dynamic is *computed* from the data every
#' time, never declared or inferred from a semantic, so it has no provenance
#' to record -- `.split_static_dynamic()` is a storage decision that
#' `.reconstruct_long()` undoes exactly.
#'
#' @noRd
.ledger_fields <- c(
  "weighted", "weight_semantics", "direction_semantics",
  "zero_meaning", "presence_mode"
)

#' Stable identifiers for the structural inferences rnetis makes
#'
#' Named rather than described, so a test can pin the rule that fired without
#' matching prose, and so a user reading the ledger can look the rule up. Adding
#' an inference means adding an identifier here; an inference that reports no
#' rule is a defect, and `.ledger_entry()` refuses to build one.
#'
#' @noRd
.ledger_rules <- c(
  values_in_0_1 = "every non-missing value is 0 or 1, so the network reads as binary",
  row_completeness_vs_roster = "the stored row count reaches the complete-dyad count implied by the node roster, directedness, and self-tie policy"
)

#' Build one ledger entry
#'
#' @param state one of `.ledger_states`.
#' @param rule the identifier of the inference rule, required when
#'   `state == "inferred"` and forbidden otherwise -- a declared value has no
#'   rule behind it, and an inference without one cannot be audited.
#' @noRd
.ledger_entry <- function(state, rule = NULL) {
  .assert_enum(state, .ledger_states, "ledger state")
  if (identical(state, "inferred")) {
    if (is.null(rule) || !(rule %in% names(.ledger_rules))) {
      stop(
        sprintf(
          "An inferred ledger entry must name a rule from .ledger_rules (%s); got %s.",
          paste(names(.ledger_rules), collapse = ", "),
          if (is.null(rule)) "NULL" else sprintf("'%s'", rule)
        ),
        call. = FALSE
      )
    }
  } else if (!is.null(rule)) {
    stop(
      sprintf("A '%s' ledger entry must not name an inference rule.", state),
      call. = FALSE
    ) # nocov
  }
  list(state = state, rule = rule)
}

#' Record provenance for one tie variable's five declarable fields
#'
#' Called from `.build_one_tie_descriptor()` (R/data-tie-meta.R) with the same
#' `user` declaration list that produced the values, so the ledger cannot
#' disagree with what was stored -- one pass, one source of truth.
#'
#' The `weight_semantics` / `zero_meaning` / `direction_semantics` rule is
#' "declared if the user set it, undeclared otherwise". There is no `inferred`
#' state for any of the three, and that is the point: what a larger weight
#' *means*, whether a stored zero was measured, and which way influence runs are
#' not recoverable from values, and a package that guessed them would be
#' inventing the semantics.
#'
#' @noRd
.build_tie_ledger <- function(user) {
  declared_or_not <- function(field) {
    if (is.null(user[[field]])) .ledger_entry("undeclared") else .ledger_entry("declared")
  }
  list(
    weighted = if (is.null(user$weighted)) {
      .ledger_entry("inferred", "values_in_0_1")
    } else {
      .ledger_entry("declared")
    },
    weight_semantics    = declared_or_not("weight_semantics"),
    direction_semantics = declared_or_not("direction_semantics"),
    zero_meaning        = declared_or_not("zero_meaning"),
    presence_mode = if (is.null(user$presence_mode)) {
      .ledger_entry("inferred", "row_completeness_vs_roster")
    } else {
      .ledger_entry("declared")
    }
  )
}

#' Provenance for the dataset-level directedness declaration
#'
#' `state` is `"declared"` in every path that exists, because a default argument
#' is still the caller asserting a value and a source object that
#' encodes directedness is asserting it too. `source_encoded` records which of
#' those two it was, so an audit can distinguish "the user chose this" from
#' "igraph told us", without inventing a fourth ledger state for a distinction
#' that is about the *speaker* rather than about confidence.
#'
#' There is deliberately no argument that produces `"inferred"`. Reading
#' directedness off matrix symmetry or edgelist reciprocity is forbidden,
#' and the cheapest way to keep a future importer from doing it
#' is to give this function no way to express it.
#'
#' @param source_encoded `TRUE` when read from a representation that genuinely
#'   encodes directedness (`igraph::is_directed()`, a `network` object's own
#'   flag); `FALSE` when the caller declared it, including via a default.
#' @noRd
.ledger_directedness <- function(source_encoded = FALSE) {
  .assert_flag(source_encoded, "source_encoded")
  c(.ledger_entry("declared"), list(source_encoded = source_encoded))
}

#' The empty boundary record carried by a hand-built netis_data object
#'
#' `source` is `NULL` rather than a placeholder: an object built directly by
#' `netis_data()` did not cross a foreign boundary, and saying so with an
#' absence is more honest than a row reading "source: none".
#'
#' @noRd
.new_boundary <- function(source = NULL, source_encoded_directedness = FALSE) {
  list(
    source = source,
    directedness = .ledger_directedness(source_encoded_directedness)
  )
}

#' Attach an import record to a freshly constructed netis_data object
#'
#' The single seam every importer goes through. An importer that builds a
#' `netis_data` object and returns it without calling this produces an object
#' that cannot say where it came from, which is the failure this seam exists to
#' make impossible: there is one function to remember, and it takes the
#' converter's own name so the record cannot drift from the function that wrote
#' it.
#'
#' @param x a `netis_data` object.
#' @param converter the importing function's name, e.g. `"netis_data_from_igraph"`.
#' @param source_class the class of the object converted, e.g. `"igraph"`.
#' @param source_encoded_directedness `TRUE` when directedness was read from the
#'   source's own structural encoding rather than declared by the caller.
#' @return `x`, with its schema's boundary record populated.
#' @noRd
.record_boundary <- function(x, converter, source_class,
                             source_encoded_directedness = FALSE) {
  .check_netis_data(x)
  x$schema$boundary <- .new_boundary(
    source = list(
      converter = converter,
      source_class = source_class
    ),
    source_encoded_directedness = source_encoded_directedness
  )
  x
}

#' Inspect where a netis_data object's declarations came from
#'
#' Every network semantic rnetis holds is stored with its provenance: whether
#' **you** declared it, whether rnetis **inferred** it from the data's structure,
#' or whether it remains **undeclared** and is sitting at a neutral default.
#' This is the function that answers "did rnetis decide this, or did I?" without
#' reading source.
#'
#' It matters because a neutral default is not an assertion. A network whose
#' `weight_semantics` is `"unitless"` because nobody said otherwise is in a
#' different state from one a user deliberately declared unitless, even though
#' the stored value is identical -- and operations distinguish the two: a
#' weight-reading measure is refused on a declared conflict and not on an
#' undeclared default, and reconstructing or converting an object restores only
#' what was declared.
#'
#' The three states and the named inference rules are the package's own
#' bookkeeping, not a published classification.
#'
#' # The three states
#'
#' \describe{
#'   \item{`declared`}{You said so, or a rnetis-authored record on an imported
#'     object said so.}
#'   \item{`inferred`}{rnetis derived it from the data's structure. The `rule`
#'     column names which inference fired; `netis_boundary_rules()` describes
#'     them.}
#'   \item{`undeclared`}{rnetis does not know. The stored value is a documented
#'     neutral default -- `weight_semantics = "unitless"`, `zero_meaning =
#'     "unknown"`, `direction_semantics = NA` -- never a guess at meaning.}
#' }
#'
#' Directedness is reported separately from the per-network rows because it is a
#' property of the tie table rather than of a value column in it. It is never
#' `inferred`: rnetis will not read directedness off matrix symmetry or edgelist
#' reciprocity, since a symmetric matrix may perfectly well be a directed
#' network whose ties happen to be reciprocal.
#'
#' @param x a [netis_data()] object.
#' @return an object of class `netis_boundary`: a list with
#'   \describe{
#'     \item{`semantics`}{a data frame with one row per tie variable and field,
#'       carrying `variable`, `field`, `value`, `state`, and `rule` (`NA` unless
#'       `state` is `"inferred"`).}
#'     \item{`directedness`}{a list with `state`, and `source_encoded` recording
#'       whether the value was read from a source representation that encodes it
#'       or declared by the caller.}
#'     \item{`source`}{`NULL` for an object built directly by [netis_data()], or
#'       a list naming the `converter` and `source_class` it was imported
#'       through.}
#'   }
#' @seealso [netis_schema()] for the declarations themselves rather than their
#'   provenance, [netis_data()]'s `tie_meta` argument to declare them,
#'   [netis_time_index()] for the parallel record on the time coordinate
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b", "c"))
#' ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 1))
#'
#' # Nothing declared: rnetis infers what it structurally can and leaves the
#' # rest undeclared rather than guessing.
#' netis_boundary(netis_data(nodes, ties))
#'
#' # Declaring one field moves exactly that row to "declared".
#' nd <- netis_data(nodes, ties, tie_meta = list(w = list(weight_semantics = "strength")))
#' netis_boundary(nd)$semantics
#' @export
netis_boundary <- function(x) {
  .check_netis_data(x)
  s <- netis_schema(x)
  b <- s$boundary

  rows <- lapply(names(s$tie_descriptors), function(v) {
    d <- s$tie_descriptors[[v]]
    do.call(rbind, lapply(.ledger_fields, function(f) {
      e <- d$ledger[[f]]
      data.frame(
        variable = v,
        field = f,
        value = .ledger_value_string(d, f),
        state = e$state,
        rule = if (is.null(e$rule)) NA_character_ else e$rule,
        stringsAsFactors = FALSE
      )
    }))
  })
  semantics <- if (length(rows) == 0L) {
    data.frame(
      variable = character(0), field = character(0), value = character(0),
      state = character(0), rule = character(0), stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  structure(
    list(semantics = semantics, directedness = b$directedness, source = b$source),
    class = "netis_boundary"
  )
}

#' Render a descriptor field's value as a single display string
#'
#' The ledger reports values alongside their provenance so the two are read
#' together; `presence_mode` additionally shows the column or rule resolving it,
#' since "complete_dyadic" alone does not say whether presence is determined.
#'
#' @noRd
.ledger_value_string <- function(d, field) {
  v <- d[[field]]
  if (identical(field, "presence_mode")) {
    if (!is.null(d$presence_col)) {
      return(sprintf("%s (via column '%s')", v, d$presence_col))
    }
    if (!is.null(d$presence_rule)) {
      return(sprintf("%s (via rule)", v))
    }
    if (!.tie_presence_determined(d)) {
      return(sprintf("%s (undetermined)", v))
    }
  }
  if (is.null(v) || length(v) != 1L) {
    return(NA_character_) # nocov
  }
  if (is.na(v)) {
    return(NA_character_)
  }
  as.character(v)
}

#' Describe rnetis's structural inference rules
#'
#' The `rule` column of [netis_boundary()]'s `semantics` table names an
#' inference; this returns what each one actually does. Rule identifiers are
#' stable, so code and tests can match on them while the descriptions stay free
#' to improve.
#'
#' @return a data frame with `rule` and `description`.
#' @seealso [netis_boundary()]
#' @examples
#' netis_boundary_rules()
#' @export
netis_boundary_rules <- function() {
  data.frame(
    rule = names(.ledger_rules),
    description = unname(.ledger_rules),
    stringsAsFactors = FALSE
  )
}

#' Print a boundary record
#'
#' @param x a `netis_boundary` object from [netis_boundary()].
#' @param ... unused; present for S3 consistency.
#' @return `x`, invisibly.
#' @seealso [netis_boundary()]
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b"))
#' ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
#' print(netis_boundary(netis_data(nodes, ties)))
#' @export
print.netis_boundary <- function(x, ...) {
  cat("<netis_boundary>\n")
  if (is.null(x$source)) {
    cat("  built directly by netis_data(); no conversion boundary was crossed\n")
  } else {
    cat(sprintf(
      "  imported by %s() from a %s object\n",
      x$source$converter, x$source$source_class
    ))
  }
  cat(sprintf(
    "  directedness: %s%s\n",
    x$directedness$state,
    if (isTRUE(x$directedness$source_encoded)) " (encoded by the source representation)" else " (by the caller)"
  ))
  if (nrow(x$semantics) == 0L) {
    cat("  no tie variables\n")
    return(invisible(x))
  }
  cat("  network semantics:\n")
  for (v in unique(x$semantics$variable)) {
    cat(sprintf("    %s:\n", v))
    sub <- x$semantics[x$semantics$variable == v, , drop = FALSE]
    for (i in seq_len(nrow(sub))) {
      cat(sprintf(
        "      %-20s %-24s %s%s\n",
        sub$field[i],
        if (is.na(sub$value[i])) "<none>" else sub$value[i],
        sub$state[i],
        if (is.na(sub$rule[i])) "" else sprintf(" [%s]", sub$rule[i])
      ))
    }
  }
  invisible(x)
}
