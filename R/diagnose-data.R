#' Run data-validity diagnostics on a netis_data object
#'
#' Checks properties that [netis_data()] construction deliberately does not
#' enforce, because they do not prevent the object from being well-formed --
#' only from supporting a sound analysis. See "What is and is not validated
#' here" on [netis_data()] for the boundary between structural construction
#' checks and these design-level diagnostics.
#'
#' `ERROR`, `WARNING` and `NOTE` below are **finding levels on the returned
#' object**, not R conditions. `diagnose_data()` reports; it does not refuse.
#' An `ERROR`-level finding means rnetis judges the analysis not estimable as
#' specified — the call itself still returns normally, and the refusal, if one
#' comes, comes later from the function that cannot proceed. Filter with
#' [filter_diagnostics()] or [has_level()].
#'
#' The checks are these (see `vignette("model-fit")` for how they fit into
#' a workflow). A node is a (group, node) unit: labels are unique only within a
#' group, and a node observed at several waves is counted once.
#' \itemize{
#'   \item ties referencing a node absent from node data in the same group
#'     (**ERROR** -- not estimable);
#'   \item self-tie rows carrying a nonzero value despite
#'     `self_ties = "prohibited"` in the schema (**ERROR** -- contradicts declared
#'     metadata). A stored zero on the diagonal is not a tie and is not reported;
#'   \item isolated nodes with no stored tie in either direction at any wave
#'     (**NOTE**);
#'   \item a group with no ties at any wave while other groups have some, and no
#'     record that its network was observed, which cannot be told apart from a
#'     network that was never measured (**NOTE**);
#'   \item groups with fewer than two distinct nodes, where no tie is possible
#'     (**WARNING**);
#'   \item a numeric node variable with zero variance across the entire dataset
#'     (**NOTE**);
#'   \item a tie variable containing zero values whose `zero_meaning` was not
#' declared at construction (**NOTE** -- a measured zero and a structural
#' zero must not be confused).
#' }
#'
#' @param x a [netis_data()] object.
#' @return a `netis_diagnostics` object; see [netis-diagnostics].
#' @seealso [diagnose_design()], [netis-diagnostics]
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b"))
#' ties <- data.frame(group = 1, source = "a", target = "c")  # 'c' does not exist
#' nd <- netis_data(nodes, ties, self_ties = "permitted")
#' diagnose_data(nd)
#' @export
diagnose_data <- function(x) {
  .check_netis_data(x)
  .combine_diagnostics(
    .check_tie_referential_integrity(x),
    .check_self_tie_policy(x),
    .check_isolates(x),
    .check_groups_without_ties(x),
    .check_group_size(x),
    .check_zero_variance(x),
    .check_zero_meaning_ambiguity(x)
  )
}

#' @noRd
.check_tie_referential_integrity <- function(x) {
  s <- netis_schema(x)
  nodes <- netis_nodes(x)
  ties <- netis_ties(x)

  node_key <- .netis_unit_key(nodes[[s$group_col]], nodes[[s$node_col]])
  src_key <- .netis_unit_key(ties[[s$group_col]], ties[[s$source_col]])
  tgt_key <- .netis_unit_key(ties[[s$group_col]], ties[[s$target_col]])

  bad_src <- !src_key %in% node_key
  bad_tgt <- !tgt_key %in% node_key

  findings <- list()
  if (any(bad_src) || any(bad_tgt)) {
    # Reported as (group, node) units: the same label can be missing from one
    # group and present in another.
    off <- unique(data.frame(
      group = as.character(c(ties[[s$group_col]][bad_src], ties[[s$group_col]][bad_tgt])),
      node = as.character(c(ties[[s$source_col]][bad_src], ties[[s$target_col]][bad_tgt])),
      stringsAsFactors = FALSE
    ))
    offending <- off$node
    multi <- length(unique(as.character(nodes[[s$group_col]]))) > 1L
    off_label <- if (multi) paste0(off$group, ": ", off$node) else off$node
    findings <- list(.finding(
      "ERROR", "data", "tie_references_unknown_node",
      sprintf(
        "%d tie(s) reference a node not present in node_data for the same group: %s. Expected every source/target in tie_data to have a matching (group, node) row in node_data. Add the missing node rows, or remove the offending ties.",
        sum(bad_src | bad_tgt),
        paste(utils::head(off_label, 5), collapse = ", ")
      ),
      details = list(nodes = offending, groups = off$group)
    ))
  }
  findings
}

#' @noRd
.check_self_tie_policy <- function(x) {
  s <- netis_schema(x)
  if (s$self_ties != "prohibited") {
    return(list())
  }
  ties <- netis_ties(x)
  # The rule the network builders refuse by: a stored zero on the diagonal of a
  # complete dyadic table is not a tie.
  is_self <- .self_tie_carries_value(ties, s)
  if (!any(is_self)) {
    return(list())
  }
  list(.finding(
    "ERROR", "data", "self_tie_despite_prohibited",
    sprintf(
      "%d self-tie(s) found (source == target) but the schema declares self_ties = \"prohibited\". Either the data contains an error, or self_ties should have been set to \"permitted\" at construction.",
      sum(is_self)
    ),
    details = list(rows = which(is_self))
  ))
}

#' The distinct (group, node) units of a node table
#'
#' `key` is the composite unit key; `label` names the group when the data has
#' more than one, so a message never lists two groups' "a" as one node.
#' @noRd
.data_units <- function(s, nodes) {
  u <- unique(data.frame(
    group = as.character(nodes[[s$group_col]]),
    node = as.character(nodes[[s$node_col]]),
    stringsAsFactors = FALSE
  ))
  u$key <- .netis_unit_key(u$group, u$node)
  u$label <- if (length(unique(u$group)) > 1L) paste0(u$group, ": ", u$node) else u$node
  rownames(u) <- NULL
  u
}

#' @noRd
.check_isolates <- function(x) {
  s <- netis_schema(x)
  nodes <- netis_nodes(x)
  ties <- netis_ties(x)

  # One entry per (group, node) unit, not per node-wave row: a node observed at
  # several waves is one node, and two groups' "a" are two nodes.
  units <- .data_units(s, nodes)
  tied_key <- unique(c(
    .netis_unit_key(ties[[s$group_col]], ties[[s$source_col]]),
    .netis_unit_key(ties[[s$group_col]], ties[[s$target_col]])
  ))
  isolate <- !units$key %in% tied_key
  if (!any(isolate)) {
    return(list())
  }
  list(.finding(
    "NOTE", "data", "isolated_nodes",
    sprintf(
      "%d node(s) have no ties in either direction at any wave: %s.",
      sum(isolate),
      paste(utils::head(units$label[isolate], 5), collapse = ", ")
    ),
    details = list(nodes = units$node[isolate], groups = units$group[isolate])
  ))
}

#' Report a group with no ties at any wave
#'
#' The exposure engine's absent-network rule is deliberately
#' narrow: it returns `NA` only where a group has an observed network at some waves
#' and no record of one at the requested wave, so the network demonstrably exists and this snapshot's
#' observation does not. A group with no ties at *any* wave, and no record that
#' its network was observed, has established nothing, and absence cannot be inferred from it -- so the engine leaves it
#' alone, and every node in it is treated as an isolate.
#'
#' It is reported rather than assumed. Inferring absence would be guessing; treating every node as an isolate
#' without saying so leaves the user unable to tell "this group genuinely has no
#' ties" from "nobody measured this group's network" -- exactly the
#' zero-versus-missing distinction rnetis does not collapse. Naming it is what
#' rnetis can honestly do: the fact is checkable, the interpretation is the
#' user's.
#'
#' A NOTE, not a WARNING. A tie-less control group is a perfectly ordinary thing
#' to have, and level discipline matters more here than emphasis.
#'
#' @noRd
.check_groups_without_ties <- function(x) {
  s <- netis_schema(x)
  nodes <- netis_nodes(x)
  ties <- netis_ties(x)
  node_groups <- unique(as.character(nodes[[s$group_col]]))
  if (length(node_groups) == 0L) {
    return(list())
  }
  tied_groups <- unique(as.character(ties[[s$group_col]]))
  # A group recorded as observed has an empty network, and that is settled.
  untied <- setdiff(node_groups, c(tied_groups, s$observed_tie_waves$group))
  # Only worth saying when some OTHER group does have ties. If no group has any,
  # the data has no network at all and that is a different conversation, not a
  # per-group anomaly.
  if (length(untied) == 0L || length(tied_groups) == 0L) {
    return(list())
  }
  list(.finding(
    "NOTE", "data", "group_without_any_ties",
    sprintf(
      "%d %s(s) have no ties at any wave, while others do: %s. rnetis cannot tell a genuinely tie-less group from one whose network was never measured, and does not guess: every node there is treated as an isolate, so its exposure follows the isolate_rule rather than becoming NA. If the network was measured and empty, that is correct as it stands, and netis_data(observed_tie_waves = ) records it when ties carry a wave column. If it was never measured, exclude those %s(s) from the fit -- otherwise they enter the estimand as observed zero exposure.",
      length(untied), s$group_col,
      paste(utils::head(untied, 5), collapse = ", "), s$group_col
    ),
    details = list(groups = untied)
  ))
}

#' @noRd
.check_group_size <- function(x) {
  s <- netis_schema(x)
  nodes <- netis_nodes(x)
  # Distinct nodes per group, not node-wave rows: one node observed at two
  # waves still cannot form a tie.
  units <- .data_units(s, nodes)
  counts <- table(units$group)
  small <- names(counts)[counts < 2L]
  if (length(small) == 0L) {
    return(list())
  }
  list(.finding(
    "WARNING", "data", "group_too_small_for_ties",
    sprintf(
      "%d group(s) have fewer than 2 nodes, so no tie is possible: %s.",
      length(small), paste(utils::head(small, 5), collapse = ", ")
    ),
    details = list(groups = small)
  ))
}

#' @noRd
.check_zero_variance <- function(x) {
  s <- netis_schema(x)
  nodes <- netis_nodes(x)
  candidate_vars <- c(s$node_vars$static, s$node_vars$dynamic)
  numeric_vars <- candidate_vars[vapply(candidate_vars, function(v) is.numeric(nodes[[v]]), logical(1))]

  flat <- numeric_vars[vapply(numeric_vars, function(v) {
    vals <- nodes[[v]]
    vals <- vals[!is.na(vals)]
    length(vals) > 0L && length(unique(vals)) == 1L
  }, logical(1))]

  if (length(flat) == 0L) {
    return(list())
  }
  list(.finding(
    "NOTE", "data", "zero_variance_node_variable",
    sprintf(
      "Numeric node variable(s) with no variance across the entire dataset: %s. Such a variable cannot contribute to any model that relies on variation to identify an effect.",
      paste(flat, collapse = ", ")
    ),
    details = list(variables = flat)
  ))
}

#' Flag tie variables containing zero values whose zero_meaning is undeclared
#'
#' A stored zero is ambiguous between a measured zero-
#' magnitude tie and an encoding of structural absence, and the package never
#' resolves that ambiguity on the user's behalf; this is a NOTE, not a WARNING,
#' because zero_meaning defaults to "unknown" for every dataset that has not
#' declared it and firing louder than NOTE would make an undeclared default
#' read as an error on ordinary data.
#'
#' @noRd
.check_zero_meaning_ambiguity <- function(x) {
  s <- netis_schema(x)
  ties <- netis_ties(x)

  ambiguous <- Filter(function(v) {
    d <- s$tie_descriptors[[v]]
    identical(d$zero_meaning, "unknown") && v %in% names(ties) && any(ties[[v]] == 0, na.rm = TRUE)
  }, names(s$tie_descriptors))

  if (length(ambiguous) == 0L) {
    return(list())
  }
  list(.finding(
    "NOTE", "data", "zero_meaning_undeclared",
    sprintf(
      "Tie variable(s) contain zero values with no declared zero_meaning: %s. A stored zero could be a measured zero-magnitude tie or an encoding of structural absence -- rnetis does not assume either. Declare zero_meaning = \"measured_zero\" or \"structural_absence\" via netis_data()'s tie_meta argument if this matters for your analysis.",
      paste(ambiguous, collapse = ", ")
    ),
    details = list(variables = ambiguous)
  ))
}
