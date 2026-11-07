# Generic ERROR/WARNING/NOTE diagnostic framework.
#
# A single "finding" is a plain list, not a special class, so check functions stay
# simple to write and test. A "netis_diagnostics" object is an ordered collection
# of findings with its own class for printing and filtering. A fitted object stores
# it wholesale in its `diagnostics` field -- nothing here assumes it is only
# ever run standalone.

#' @noRd
.diagnostic_levels <- c("ERROR", "WARNING", "NOTE")

#' @noRd
.diagnostic_families <- c("data", "design", "estimation", "dependence", "robustness")

#' Construct a single diagnostic finding
#'
#' Not exported: check functions use this to build findings; users only ever see
#' the resulting `netis-diagnostics` collection (see [netis-diagnostics]).
#'
#' @param level one of `"ERROR"`, `"WARNING"`, `"NOTE"`.
#' @param family one of
#' `"data"`, `"design"`, `"estimation"`, `"dependence"`, `"robustness"`.
#' @param
#' code short machine-readable identifier, e.g.
#' `"tie_references_unknown_node"`.
#' @param message human-readable message: say what was wrong, what was expected,
#' and what to do.
#' @param details a list of supporting information (e.g. offending
#' IDs), or `NULL`. Never used to duplicate what `message` already states in
#' prose.
#' @noRd
.finding <- function(level, family, code, message, details = NULL) {
  .assert_enum(level, .diagnostic_levels, "level")
  .assert_enum(family, .diagnostic_families, "family")
  list(level = level, family = family, code = code, message = message, details = details)
}

#' Combine findings (and/or other diagnostics collections) into one collection
#'
#' @param ... `netis_diagnostics` objects and/or raw finding lists (as produced
#'   by individual check functions), in any mixture.
#' @return a `netis_diagnostics` object.
#' @noRd
.combine_diagnostics <- function(...) {
  inputs <- list(...)
  findings <- list()
  for (item in inputs) {
    if (inherits(item, "netis_diagnostics")) {
      findings <- c(findings, unclass(item))
    } else if (is.list(item) && !is.null(item$level)) {
      findings <- c(findings, list(item))
    } else if (is.list(item)) {
      findings <- c(findings, item)
    }
  }
  structure(findings, class = "netis_diagnostics")
}

#' Diagnostic findings from rnetis data validity and design checks
#'
#' `netis_diagnostics` objects hold zero or more findings, each with a severity
#' level (`"ERROR"`, `"WARNING"`, or `"NOTE"`), the diagnostic family it belongs
#' to, a machine-readable code, a human-readable message, and supporting details.
#' They are produced by [diagnose_data()] and related functions, never
#' constructed directly by users.
#'
#' The three levels have specific meanings, applied consistently across the
#' package: **ERROR** means the analysis is
#' not statistically interpretable or executable as specified; **WARNING** means
#' it can run, but there is a real inferential concern; **NOTE** means something
#' unusual or worth knowing, not necessarily a problem.
#'
#' @param x a `netis_diagnostics` object.
#' @param level restrict to one or more levels, e.g. `"ERROR"`.
#' @param family restrict to one or more diagnostic families, e.g. `"data"`.
#' @return for `has_level()`, a single logical. For `filter_diagnostics()`, a
#'   `netis_diagnostics` object containing only the matching findings.
#' @seealso [diagnose_data()] and [diagnose_design()], which produce these
#'   collections, and the print method documented at
#'   [netis-diagnostics-methods] for how a collection reads
#' @name netis-diagnostics
#' @examples
#' nodes <- data.frame(group = 1, node = "a")
#' ties <- data.frame(group = 1, source = "a", target = "b")
#' nd <- netis_data(nodes, ties, self_ties = "permitted")
#' d <- diagnose_data(nd)
#' print(d)
#' has_level(d, "ERROR")
#' filter_diagnostics(d, level = "WARNING")
NULL

#' @rdname netis-diagnostics
#' @export
filter_diagnostics <- function(x, level = NULL, family = NULL) {
  if (!inherits(x, "netis_diagnostics")) {
    stop("x must be a netis_diagnostics object.", call. = FALSE)
  }
  # Deliberately not implemented as a `[` method: `[` on a list-backed S3
  # object is called internally by base functions like Filter() with plain
  # positional index arguments, and giving `[` non-standard two-argument
  # named semantics here silently breaks any such caller -- their index
  # argument gets routed into `level` instead of used as a selector. A
  # dedicated function has no such ambiguity.
  plain <- unclass(x)
  keep <- rep(TRUE, length(plain))
  if (!is.null(level)) {
    keep <- keep & vapply(plain, function(f) f$level %in% level, logical(1))
  }
  if (!is.null(family)) {
    keep <- keep & vapply(plain, function(f) f$family %in% family, logical(1))
  }
  structure(plain[keep], class = "netis_diagnostics")
}

#' @rdname netis-diagnostics
#' @export
has_level <- function(x, level) {
  if (!inherits(x, "netis_diagnostics")) {
    stop("x must be a netis_diagnostics object.", call. = FALSE)
  }
  .assert_enum(level, .diagnostic_levels, "level")
  any(vapply(x, function(f) identical(f$level, level), logical(1)))
}

#' Print or format a diagnostics collection
#'
#' How a `netis_diagnostics` collection displays: a header counting findings by
#' level, then one line per finding as `[LEVEL/family] message`.
#'
#' # Reading the levels
#'
#' The three levels are a statement about *what rnetis did*, not a score:
#'
#' - **ERROR** — rnetis refused, or will refuse, to produce a result. Something
#'   about the specification or data makes the answer wrong rather than
#'   imprecise, and a wrong number is worse than no number.
#' - **WARNING** — rnetis produced a result and there is something about it you
#'   need to know before using it.
#' - **NOTE** — informational. Something was resolved in a way worth recording,
#'   such as a lag applied to a variable that turns out to be static.
#'
#' The `family` names which of the diagnostic families raised it (data, design,
#' estimation, dependence, robustness), which is usually enough to tell you
#' whether to look at your data, your formula, or your fit.
#'
#' # There is no overall verdict, deliberately
#'
#' An empty collection prints `no findings` and means exactly that: no check
#' rnetis ran had anything to say. It does **not** mean the model is right, that
#' the specification is identified in some broader sense, or that the design
#' supports the claim you want to make. Several important problems — network
#' formation being endogenous to the outcome, most notably — cannot be detected
#' from the data at all and are reported as unaddressed on every fit rather
#' than as findings here.
#'
#' @param x a `netis_diagnostics` collection, as returned by [diagnose_data()],
#'   [diagnose_design()], or carried on a fitted model.
#' @param ... unused; present for S3 consistency with the generics.
#' @return `format()` a character vector, one element per line. `print()`
#'   returns `x` invisibly and is called for its output.
#' @seealso [diagnose_data()], [diagnose_design()],
#'   [filter_diagnostics()] and [has_level()] for working with a collection
#'   programmatically rather than reading it
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b", "c"), y = c(1, 2, 3))
#' ties <- data.frame(group = 1, source = "a", target = "b", link = 1)
#' d <- diagnose_data(netis_data(nodes, ties))
#' d
#'
#' # Branch on severity rather than on the printed text.
#' has_level(d, "ERROR")
#' @name netis-diagnostics-methods
NULL

#' @rdname netis-diagnostics-methods
#' @export
format.netis_diagnostics <- function(x, ...) {
  if (length(x) == 0L) {
    return("<netis_diagnostics: no findings>")
  }
  counts <- table(factor(vapply(x, `[[`, character(1), "level"), levels = .diagnostic_levels))
  header <- sprintf(
    "<netis_diagnostics: %d ERROR, %d WARNING, %d NOTE>",
    counts["ERROR"], counts["WARNING"], counts["NOTE"]
  )
  lines <- vapply(x, function(f) sprintf("  [%s/%s] %s", f$level, f$family, f$message), character(1))
  c(header, lines)
}

#' @rdname netis-diagnostics-methods
#' @export
print.netis_diagnostics <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
