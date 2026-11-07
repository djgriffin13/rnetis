# The conversion-loss registry.
#
# TWO CLASSES OF LOSS, AND THEY ARE NOT THE SAME KIND OF PROBLEM.
#
#   semantic         can change the downstream numerical meaning of the
#                    represented network. REFUSES by default.
#   representational cannot change represented values under the requested
#                    conversion. May warn.
#
# Every loss raises a STRUCTURED condition carrying a stable `loss_id`, so a
# test pins the identifier rather than the prose and a caller can program
# against a specific loss, while the wording stays free to improve.
#
# AUTHORIZATION IS NAMED AND PER-CONDITION. There is deliberately no global
# `allow_lossy = TRUE`: one boolean cannot express "I have reviewed this
# particular sacrifice", and a user who set it to get past a presence question
# would also, invisibly, have authorized isolate loss and temporal collapse.
# An unauthorized required loss still refuses even when other losses are
# authorized.
#
# AND SOME LOSSES TAKE NO OVERRIDE AT ALL. Where authorizing the loss would
# leave rnetis unable to know WHAT NETWORK THE USER MEANT, no `allow_loss` value
# is offered and the resolution is to reshape or redeclare the input. An escape
# hatch may permit a known representational sacrifice; it may never permit
# semantic guessing.

#' The registry
#'
#' Each entry states what is lost, what that does downstream, and what the user
#' can do instead. `resolution` is not optional and is not decoration: a refusal
#' without a way forward is a dead end rather than a guardrail.
#'
#' @noRd
.loss_registry <- list(
  isolate_roster = list(
    severity = "semantic",
    overridable = FALSE,
    what_is_lost = "the node roster: nodes with no ties at all",
    consequence = "every centrality, degree, and density value would be computed over a smaller node set than the study actually has, silently changing each one",
    resolution = "supply the roster explicitly. If the data genuinely has no isolates, say so by passing the endpoints as the roster; rnetis will not assume it."
  ),
  tie_value_collapse = list(
    severity = "semantic",
    overridable = FALSE,
    what_is_lost = "distinct values on parallel edges between the same ordered pair",
    consequence = "rnetis has no multi-edge representation, so one edge's value would silently stand in for several",
    resolution = "collapse the parallel edges yourself with an explicit rule (sum, max, mean) before converting, so the rule is yours and is recorded."
  ),
  presence_metadata = list(
    severity = "representational",
    overridable = TRUE,
    what_is_lost = "a presence_rule closure, which has no stable serialized form",
    consequence = "the target representation carries the resolved presence but not the rule that produced it, so a round trip re-infers rather than restores it",
    resolution = "re-declare presence_rule on import, or use presence_col, whose column name does survive."
  ),
  foreign_attributes = list(
    severity = "representational",
    overridable = TRUE,
    what_is_lost = "graph-level attributes the source object carries that rnetis has no schema slot for",
    consequence = "none for computed values; the attributes are simply not present after conversion",
    resolution = "read them off the source object before converting if you need them."
  ),
  schema_records = list(
    severity = "representational",
    overridable = TRUE,
    what_is_lost = "records a single-network snapshot has no place for: a group table, declared metric time, declarations on the other tie variables",
    consequence = "none for the exported network's values; the records are simply absent from the graph, and from anything rebuilt from it",
    resolution = "read them off the netis_data object before converting if you need them, and re-declare them when rebuilding."
  ),
  derived_provenance = list(
    # REPRESENTATIONAL, not semantic, even though a derived variable becomes
    # "indistinguishable from a measured one" after conversion. Compare the two
    # semantic entries above: both of them
    # silently change a COMPUTED VALUE if allowed (isolate loss changes every
    # centrality/density number; parallel-edge collapse picks one value over
    # another). Losing add_centering()/add_decomposition()'s provenance record
    # changes neither -- the exported column's VALUES are exactly what they
    # were, and only the machine-readable tag saying "this is z's within-node
    # deviation, computed by averaging over waves" is gone. That is the same
    # shape of loss presence_metadata already catalogs as representational (the
    # resolved values survive; the rule that produced them does not), so it is
    # classified the same way here.
    severity = "representational",
    overridable = TRUE,
    what_is_lost = "the derived-variable provenance record (source column, averaging unit, basis) for any add_centering()/add_decomposition() column included in the export",
    consequence = "the exported column's values are unchanged, but on the far side (and on any round trip back into rnetis) it is an ordinary declared variable indistinguishable from independently measured data -- re-deriving a further decomposition from it, or re-averaging it, would silently treat a derived quantity as raw",
    resolution = "read netis_schema(x)$derived before converting if you need to know which columns are derived and how, or drop them from the exported table yourself."
  )
)

#' Raise a conversion loss
#'
#' @param loss_id an identifier present in `.loss_registry`.
#' @param allow_loss the caller's `allow_loss` argument -- a character vector of
#'   authorized `loss_id`s, or `NULL`.
#' @param detail optional extra sentence naming the specific instance (which
#'   dyad, which attribute), appended to the registry's generic wording. The
#'   registry says what class of thing was lost; this says which one.
#' @return invisibly `TRUE` if the loss was authorized, or if it is
#'   representational, in which case an unauthorized loss emits a warning;
#'   otherwise an error.
#' @noRd
.loss_raise <- function(loss_id, allow_loss = NULL, detail = NULL) {
  entry <- .loss_registry[[loss_id]]
  if (is.null(entry)) {
    stop(sprintf("Internal: unknown loss_id '%s'.", loss_id), call. = FALSE) # nocov
  }

  authorized <- !is.null(allow_loss) && loss_id %in% allow_loss
  if (authorized && !isTRUE(entry$overridable)) {
    # Refusing the authorization itself, rather than ignoring it. A user who
    # wrote allow_loss = "isolate_roster" believes they have permitted
    # something; letting the call proceed to a different refusal, or silently
    # dropping their argument, would both leave them guessing.
    stop(
      .loss_condition(
        loss_id, entry, detail,
        sprintf(
          "This loss cannot be authorized: allowing it would leave rnetis unable to determine what network you meant. %s",
          entry$resolution
        ),
        class = "netis_semantic_loss"
      )
    )
  }

  if (authorized) {
    return(invisible(TRUE))
  }

  msg_tail <- if (isTRUE(entry$overridable)) {
    sprintf(
      "%s To proceed anyway, pass allow_loss = \"%s\".",
      entry$resolution, loss_id
    )
  } else {
    entry$resolution
  }

  if (identical(entry$severity, "semantic")) {
    stop(.loss_condition(loss_id, entry, detail, msg_tail, "netis_semantic_loss"))
  }
  warning(.loss_condition(loss_id, entry, detail, msg_tail, "netis_representational_loss"))
  invisible(TRUE)
}

#' Build the structured condition object
#'
#' The fields travel on the condition, not only in its message, so that
#' `tryCatch(..., netis_semantic_loss = function(e) e$loss_id)` works and tests
#' assert on `loss_id` rather than on wording.
#'
#' @noRd
.loss_condition <- function(loss_id, entry, detail, msg_tail, class) {
  message <- sprintf(
    "This conversion would lose %s.%s %s %s",
    entry$what_is_lost,
    if (is.null(detail)) "" else paste0(" ", detail),
    sprintf("Consequence: %s.", entry$consequence),
    msg_tail
  )
  structure(
    class = c(class, "netis_conversion_loss", if (identical(class, "netis_semantic_loss")) "error" else "warning", "condition"),
    list(
      message = message,
      call = NULL,
      loss_id = loss_id,
      what_is_lost = entry$what_is_lost,
      consequence = entry$consequence,
      resolution = entry$resolution,
      severity = entry$severity,
      overridable = entry$overridable
    )
  )
}

#' Validate an `allow_loss` argument at a public boundary
#'
#' An unrecognized identifier is refused rather than ignored: a user who
#' misspells one has authorized nothing and would otherwise discover it only
#' through a refusal that looks like the flag was not working.
#'
#' @noRd
.check_allow_loss <- function(allow_loss) {
  if (is.null(allow_loss)) {
    return(invisible(NULL))
  }
  if (!is.character(allow_loss)) {
    stop("allow_loss must be a character vector of loss identifiers; see netis_losses().", call. = FALSE)
  }
  unknown <- setdiff(allow_loss, names(.loss_registry))
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "allow_loss names unknown loss identifier(s): %s. Known identifiers: %s. See netis_losses().",
        paste(unknown, collapse = ", "), paste(names(.loss_registry), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(allow_loss)
}

#' What a conversion can lose, and which losses you can authorize
#'
#' rnetis conversions never lose information silently. Where a conversion cannot
#' carry something, it says so with a stable identifier, and this is the table
#' of those identifiers.
#'
#' # Two severities
#'
#' \describe{
#'   \item{`semantic`}{The loss can change the downstream numerical meaning of
#'     the network: isolates disappearing because an edgelist has no roster, or
#'     one edge's value standing in for several parallel edges. These
#'     **refuse**.}
#'   \item{`representational`}{The loss cannot change any represented value
#'     under the requested conversion -- a foreign graph attribute with nowhere
#'     to go, a rule that has no serialized form, a group table a single graph
#'     has no place for, the record of how a derived column was made. These
#'     **warn**, or pass silently once authorized.}
#' }
#'
#' # Authorizing a loss
#'
#' Where a loss is `overridable`, pass its identifier to the converting
#' function's `allow_loss` argument:
#'
#' ```r
#' allow_loss = "presence_metadata"
#' allow_loss = c("presence_metadata", "foreign_attributes")
#' ```
#'
#' Authorization is **per loss**. Allowing one never allows another, and there
#' is deliberately no blanket "allow everything" switch: a single flag cannot
#' express that you reviewed a particular sacrifice.
#'
#' Some losses are **not overridable at all**. Where allowing the loss would
#' leave rnetis unable to determine what network you meant, no authorization is
#' accepted and the `resolution` column says what to do instead -- usually to
#' declare the missing piece, so the decision is recorded as yours.
#'
#' The two severities and this table are the package's own bookkeeping, not a
#' published classification.
#'
#' @return a data frame with `loss_id`, `severity`, `overridable`,
#'   `what_is_lost`, `consequence`, and `resolution`.
#' @seealso [netis_boundary()] for what a converted object records about its own
#'   provenance
#' @examples
#' netis_losses()
#'
#' # Which losses can be authorized at all
#' l <- netis_losses()
#' l[l$overridable, c("loss_id", "what_is_lost")]
#' @export
netis_losses <- function() {
  data.frame(
    loss_id = names(.loss_registry),
    severity = vapply(.loss_registry, function(e) e$severity, character(1)),
    overridable = vapply(.loss_registry, function(e) isTRUE(e$overridable), logical(1)),
    what_is_lost = vapply(.loss_registry, function(e) e$what_is_lost, character(1)),
    consequence = vapply(.loss_registry, function(e) e$consequence, character(1)),
    resolution = vapply(.loss_registry, function(e) e$resolution, character(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
