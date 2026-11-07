# igraph interoperability. netis_data stays canonical -- we convert *out to* igraph for
# graph algorithms and interoperability; nothing in the fitting path routes
# through an igraph object. igraph stays in Suggests, guarded by
# requireNamespace() at every entry point, since the network-feature layer implements the common
# network features natively precisely so ordinary use never needs it.

#' Convert a netis_data object to an igraph graph
#'
#' Builds a single [igraph::graph_from_data_frame()] graph for one (group, wave, tie
#' variable) selection. Node IDs become vertex names (never positional indices); every
#' node variable at the selected wave becomes a vertex attribute; every tie variable at
#' the selected wave becomes an edge attribute, with `network`'s own values additionally
#' aliased to the edge attribute `weight` (igraph's own convention for weighted
#' algorithms) unless `network` is already named `"weight"`. Directedness is read from
#' the schema. A node with no ties still appears as an isolated vertex -- the vertex set
#' is built from node data, not inferred from edge endpoints, which would silently drop
#' isolates ([netis_adjacency()] builds its node set the same way).
#'
#' # Edges are the present ties
#'
#' An igraph edge either exists or it does not, so the edge set is
#' presence-resolved first, exactly as [as_network()] does: a `row_presence`
#' network exports every stored row; an `indicator` or `value_rule` network
#' exports only the rows its declaration marks present; a `complete_dyadic`
#' network with no declared presence column or rule is **refused**, because
#' its stored zero rows would otherwise become edges. A network whose presence
#' mode rnetis inferred and whose values include stored zeros is refused for the
#' same reason [netis_adjacency()]`(presence = TRUE)` refuses it.
#'
#' # Reserved names
#'
#' `name` is igraph's vertex identifier and `weight` is the edge attribute igraph's
#' weighted algorithms read. A node variable called `name`, or a tie variable called
#' `weight` when `network` is a different variable, would be overwritten by the
#' conversion, so either one is an error asking you to rename it.
#'
#' Network semantics recorded by [netis_data]'s `tie_meta` -- `weight_semantics`, `presence_mode`, `direction_semantics`,
#' `zero_meaning` -- are attached as graph-level attributes under `netis_meta`, along with
#' which `group`/`wave`/`network` were selected and **which of those fields were declared**
#' (see [netis_boundary()]). [netis_data_from_igraph()] reads this attribute back and
#' restores only the declared fields, so a value rnetis inferred, or a neutral default
#' standing for "undeclared", is not turned into a declaration by a round trip.
#'
#' # Selection
#'
#' `network`, `group`, and `wave` are inferred when uniquely determined and are an error
#' when ambiguous, as in [centrality()] and [network_stat()]: a single-network,
#' single-group, single-wave dataset converts with `as_igraph(x)` alone; anything ambiguous names the candidates and asks,
#' rather than guessing among them.
#'
#' @param x a [netis_data()] object.
#' @param network name of the tie variable to convert. Inferred if there is exactly one
#'   tie variable; an error naming the candidates otherwise.
#' @param group the group to convert. Inferred if there is exactly one group; an error
#'   naming the candidates otherwise. See [as_igraph_list()] to convert every group at
#'   once.
#' @param wave the wave to convert, or `NULL` if the data has no wave dimension. Inferred
#'   if there is exactly one observed wave; an error naming the candidates otherwise.
#' @param allow_loss character vector of conversion losses to authorize; see
#'   [netis_losses()]. Relevant here for `presence_metadata`, raised when a network
#'   resolves presence with a rule function, which has no serialized form;
#'   `schema_records`, raised when the object holds a group table, declared metric
#'   time, or declarations on tie variables other than `network`, none of which a
#'   graph carries; and `derived_provenance`, raised when a column from
#'   [add_centering()] or [add_decomposition()] becomes a vertex attribute.
#' @return an `igraph` object. A wave at which the group has no node rows is an
#'   error, and so is a tie reaching a node with no row at the selected wave. A wave
#'   whose network is unobserved (see Empty and unobserved waves on [netis_data()])
#'   gives an edgeless graph and a warning naming the group and wave; an observed
#'   empty network gives the same graph with no warning.
#' @references
#' Csardi, G. and Nepusz, T. (2006) The igraph software package for complex
#' network research. *InterJournal*, Complex Systems, 1695.
#' @seealso [as_igraph_list()] for every group (and wave) at once,
#'   [netis_data_from_igraph()] for the reverse conversion.
#' @examples
#' # Suggests dependency: igraph, without which the example cannot run.
#' \donttest{
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   nodes <- data.frame(group = 1, node = c("a", "b", "c"), trait = c(1, 2, 3))
#'   ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), weight = c(2, 1))
#'   nd <- netis_data(nodes, ties)
#'   g <- as_igraph(nd)
#'   g
#' }
#' }
#' @export
as_igraph <- function(x, network = NULL, group = NULL, wave = NULL, allow_loss = NULL) {
  .check_netis_data(x)
  .require_igraph("as_igraph()")
  .check_allow_loss(allow_loss)
  s <- netis_schema(x)
  network <- .resolve_igraph_network(s, network)
  group <- .resolve_igraph_group(x, s, group)
  wave <- .resolve_igraph_wave(s, wave)
  out <- .build_one_igraph(x, s, network, group, wave, allow_loss)
  .warn_unobserved_export(.unobserved_network_labels(netis_ties(x), s, group, wave), "graph")
  out
}

#' Convert each group and wave of a netis_data object to igraph graphs
#'
#' The list form of [as_igraph()], for when a dataset has more than one group and/or wave
#' and every snapshot is wanted at once. Deliberately a separate function rather than
#' having [as_igraph()] return either one graph or a list depending on how many groups
#' the data happens to have -- a converter whose return type depends on the data is
#' unpredictable to program against.
#'
#' @param x a [netis_data()] object.
#' @param network name of the tie variable to convert. Inferred if there is exactly one
#'   tie variable; an error naming the candidates otherwise.
#' @param allow_loss character vector of conversion losses to authorize; see
#'   [as_igraph()] and [netis_losses()].
#' @return a named list of `igraph` objects. Names are `"<group>"` when the data has no
#'   wave dimension, or `"<group>_w<wave>"` otherwise, one per wave at which the group
#'   has node rows: a group absent from a wave has no graph, not an empty one. A
#'   warning names every snapshot whose network is unobserved, as in [as_igraph()].
#' @references
#' Csardi, G. and Nepusz, T. (2006) The igraph software package for complex
#' network research. *InterJournal*, Complex Systems, 1695.
#' @seealso [as_igraph()] for a single graph.
#' @examples
#' # Suggests dependency: igraph, without which the example cannot run.
#' \donttest{
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   nodes <- data.frame(group = c(1, 1, 2, 2), node = c("a", "b", "c", "d"))
#'   ties <- data.frame(group = c(1, 2), source = c("a", "c"), target = c("b", "d"), weight = c(1, 2))
#'   nd <- netis_data(nodes, ties)
#'   graphs <- as_igraph_list(nd)
#'   names(graphs)
#' }
#' }
#' @export
as_igraph_list <- function(x, network = NULL, allow_loss = NULL) {
  .check_netis_data(x)
  .require_igraph("as_igraph_list()")
  .check_allow_loss(allow_loss)
  s <- netis_schema(x)
  network <- .resolve_igraph_network(s, network)
  groups <- sort(unique(as.character(netis_nodes(x)[[s$group_col]])))
  has_wave <- !is.null(s$wave_col) && length(s$waves) > 0L
  # Named once for the whole list rather than once per graph.
  allow_loss <- c(allow_loss, .raise_schema_records_loss(x, s, network, allow_loss))

  out <- list()
  for (g in groups) {
    if (has_wave) {
      for (w in .waves_observed_in_group(x, s, g)) {
        out[[sprintf("%s_w%s", g, w)]] <- .build_one_igraph(x, s, network, g, w, allow_loss)
      }
    } else {
      out[[g]] <- .build_one_igraph(x, s, network, g, NULL, allow_loss)
    }
  }
  .warn_unobserved_export(
    .unobserved_list_labels(out, function(g) igraph::graph_attr(g, "netis_meta")),
    "graph"
  )
  out
}

#' Name the snapshots of an exported list whose network is unobserved
#'
#' @param out the named list of exported objects.
#' @param meta_of reads the `netis_meta` record of one object.
#' @return labels such as `"group 1 wave 3"`, in list order.
#' @noRd
.unobserved_list_labels <- function(out, meta_of) {
  unlist(lapply(out, function(obj) {
    m <- meta_of(obj)
    if (isFALSE(m$network_observed)) sprintf("group %s wave %s", m$group, format(m$wave))
  }), use.names = FALSE) %||% character(0)
}

#' The waves at which a group has node rows, in wave order
#' @noRd
.waves_observed_in_group <- function(x, schema, group) {
  nodes <- netis_nodes(x)
  in_group <- as.character(nodes[[schema$group_col]]) == group
  if (!(schema$wave_col %in% names(nodes))) {
    return(schema$waves)
  }
  schema$waves[as.character(schema$waves) %in% as.character(nodes[[schema$wave_col]][in_group])]
}

#' Refuse to build a graph for an occasion the group was not observed at
#' @noRd
.assert_group_wave_observed <- function(nodes, group, wave) {
  if (nrow(nodes) == 0L) {
    stop(
      sprintf(
        "group '%s' has no node rows at wave %s, so it has no network to export at that occasion.",
        group, format(wave)
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Raise the loss of what a one-network, one-snapshot export cannot carry
#'
#' @return `"schema_records"`, invisibly, so a list-building caller can name the
#'   loss once and authorize the per-graph calls; `NULL` when nothing is lost.
#' @noRd
.raise_schema_records_loss <- function(x, schema, network, allow_loss) {
  ti <- schema$time_index
  other <- setdiff(names(schema$tie_descriptors), network)
  declared_other <- other[vapply(other, function(v) {
    length(.declared_tie_fields(schema$tie_descriptors[[v]])) > 0L
  }, logical(1))]
  lost <- c(
    if (!is.null(x$groups)) "the group table",
    if (!is.null(ti) && any(ti$time_source == "declared")) "declared metric time",
    if (length(declared_other) > 0L) {
      sprintf("the declarations on tie variable(s) %s",
              paste(sprintf("'%s'", declared_other), collapse = ", "))
    }
  )
  if (length(lost) == 0L) {
    return(invisible(NULL))
  }
  .loss_raise("schema_records", allow_loss = allow_loss,
              detail = sprintf("Not carried: %s.", paste(lost, collapse = ", ")))
  invisible("schema_records")
}

#' @noRd
.require_igraph <- function(fn_name) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop(
      sprintf("%s requires the 'igraph' package. Install it with install.packages(\"igraph\").", fn_name),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @noRd
.resolve_igraph_network <- function(schema, network) {
  candidates <- names(schema$tie_descriptors)
  if (!is.null(network)) {
    if (!(network %in% candidates)) {
      stop(
        sprintf("network = \"%s\" is not a tie variable in this netis_data object. Available: %s.", network, paste(candidates, collapse = ", ")),
        call. = FALSE
      )
    }
    return(network)
  }
  if (length(candidates) == 1L) {
    return(candidates)
  }
  if (length(candidates) == 0L) {
    stop("This netis_data object has no tie variables to convert.", call. = FALSE)
  }
  stop(
    sprintf(
      "This netis_data object has %d tie variables (%s); network = ... must name one.",
      length(candidates), paste(candidates, collapse = ", ")
    ),
    call. = FALSE
  )
}

#' @noRd
.resolve_igraph_group <- function(x, schema, group) {
  candidates <- sort(unique(as.character(netis_nodes(x)[[schema$group_col]])))
  if (!is.null(group)) {
    group <- as.character(group)
    if (!(group %in% candidates)) {
      stop(
        sprintf("group = \"%s\" is not a group in this netis_data object. Available: %s.", group, paste(candidates, collapse = ", ")),
        call. = FALSE
      )
    }
    return(group)
  }
  if (length(candidates) == 1L) {
    return(candidates)
  }
  stop(
    sprintf(
      "This netis_data object has %d groups (%s); group = ... must name one, or use as_igraph_list() to convert every group at once.",
      length(candidates), paste(utils::head(candidates, 5), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' @noRd
.resolve_igraph_wave <- function(schema, wave) {
  .resolve_wave(
    schema, wave,
    multi_hint = ", or use as_igraph_list() to convert every wave at once."
  )
}

#' Build one igraph graph for a resolved (network, group, wave) selection
#'
#' `group`/`wave` are already resolved (character/scalar or NULL); this function does no
#' further inference. The vertex data frame is built first, from node data alone, so that
#' isolates -- nodes with no matching edge in either direction -- are guaranteed to survive
#' as vertices: `igraph::graph_from_data_frame()`'s `vertices` argument, when supplied,
#' defines the vertex set exactly, rather than the vertex set being inferred from edge
#' endpoints (which would silently drop them).
#'
#' @noRd
.build_one_igraph <- function(x, schema, network, group, wave, allow_loss = NULL) {
  nodes <- .filter_group_wave(netis_nodes(x), schema, group, wave)
  ties <- .filter_group_wave(netis_ties(x), schema, group, wave)
  .assert_group_wave_observed(nodes, group, wave)
  .raise_schema_records_loss(x, schema, network, allow_loss)

  node_key_cols <- c(schema$group_col, schema$node_col, schema$wave_col)
  node_attr_cols <- setdiff(names(nodes), node_key_cols)
  # A derived column (add_centering()/add_decomposition()) becomes
  # an ordinary vertex attribute with no marker distinguishing it from a
  # measured one -- the loss is in the provenance record, not the values,
  # which is why this is representational rather than semantic (see
  # data-convert-loss.R's derived_provenance entry for the full reasoning).
  derived_here <- intersect(node_attr_cols, names(schema$derived))
  if (length(derived_here) > 0L) {
    .loss_raise(
      "derived_provenance", allow_loss = allow_loss,
      detail = sprintf("Column(s) %s.", paste(sprintf("'%s'", derived_here), collapse = ", "))
    )
  }
  if ("name" %in% node_attr_cols) {
    stop(
      "This netis_data object has a node variable called 'name', which is igraph's vertex identifier: as_igraph() would overwrite node identity with it. Rename the variable before converting.",
      call. = FALSE
    )
  }
  vertex_df <- cbind(
    data.frame(name = as.character(nodes[[schema$node_col]]), stringsAsFactors = FALSE),
    nodes[node_attr_cols]
  )
  # graph_from_data_frame() requires unique vertex names; netis_data()'s own
  # unique-key construction check already guarantees this for (group, node)
  # within one group, so no separate check is needed here.

  tie_key_cols <- c(schema$group_col, schema$source_col, schema$target_col, schema$wave_col)
  tie_attr_cols <- setdiff(names(ties), tie_key_cols)
  if ("weight" %in% tie_attr_cols && !identical(network, "weight")) {
    stop(
      sprintf(
        "This netis_data object has a tie variable called 'weight', but network = \"%s\" was selected. as_igraph() writes the selected network's values to the edge attribute 'weight' (the one igraph's weighted algorithms read), which would overwrite your 'weight' variable. Select network = \"weight\", or rename the variable before converting.",
        network
      ),
      call. = FALSE
    )
  }

  # Edges are the present ties. Presence is resolved BEFORE the edge set is
  # built, with the same primitive as.network() and netis_adjacency(presence =
  # TRUE) use: it refuses an undetermined complete_dyadic network and an
  # inferred row_presence network that stores zeros, rather than exporting
  # non-ties as edges.
  d <- schema$tie_descriptors[[network]]
  .assert_tie_endpoints_on_roster(
    ties, schema, group,
    unique(as.character(netis_nodes(x)[[schema$node_col]][
      as.character(netis_nodes(x)[[schema$group_col]]) == group]))
  )
  if (nrow(ties) > 0L) {
    present <- .resolve_tie_presence(d, ties, network)
    ties <- ties[!is.na(present) & present, , drop = FALSE]
  }
  missing_end <- !(as.character(ties[[schema$source_col]]) %in% vertex_df$name) |
    !(as.character(ties[[schema$target_col]]) %in% vertex_df$name)
  if (any(missing_end)) {
    stop(
      sprintf(
        "%d tie(s) in group '%s' at this wave reference node(s) with no node row at this wave, so they have no vertex to attach to. Add node rows for them at this wave, or remove the ties.",
        sum(missing_end), group
      ),
      call. = FALSE
    )
  }

  edge_df <- data.frame(
    from = as.character(ties[[schema$source_col]]),
    to = as.character(ties[[schema$target_col]]),
    stringsAsFactors = FALSE
  )
  if (length(tie_attr_cols) > 0L) {
    edge_df <- cbind(edge_df, ties[tie_attr_cols])
  }
  # igraph's own convention: the edge attribute named "weight" is what
  # weighted algorithms pick up automatically. Aliased from the selected
  # network's own values so that convention works without the caller
  # needing to know which column was selected -- skipped when the selected
  # variable is already named "weight" to avoid a pointless duplicate
  # column, and undone symmetrically by netis_data_from_igraph() so the
  # alias does not become a spurious extra tie variable on round-trip.
  if (!identical(network, "weight")) {
    edge_df$weight <- ties[[network]]
  }

  g <- igraph::graph_from_data_frame(edge_df, directed = schema$directed, vertices = vertex_df)

  # `presence_col` is a COLUMN NAME -- a string, which serializes -- and the
  # column itself travels as an ordinary edge attribute above, so an
  # indicator-declared network round-trips exactly. A `presence_rule` is a
  # closure with no stable serialized form, and that one genuinely cannot
  # survive; it raises the representational loss rather than being dropped in
  # silence.
  if (!is.null(d$presence_rule)) {
    .loss_raise(
      "presence_metadata", allow_loss = allow_loss,
      detail = sprintf("Network '%s' resolves presence with a rule function.", network)
    )
  }

  igraph::graph_attr(g, "netis_meta") <- list(
    group = group, wave = wave, network = network,
    # Whether this snapshot's network was observed: an edgeless graph cannot say,
    # and a graph list coming back in has to keep the difference.
    network_observed = length(.unobserved_network_labels(netis_ties(x), schema, group, wave)) == 0L,
    directed = schema$directed, self_ties = schema$self_ties,
    weight_semantics = d$weight_semantics, presence_mode = d$presence_mode,
    direction_semantics = d$direction_semantics, zero_meaning = d$zero_meaning,
    weighted = d$weighted,
    # Which of those the caller DECLARED. Only these are restored on the way
    # back; the others are re-inferred or stay undeclared.
    declared = .declared_tie_fields(d),
    # Carried only when the column actually made it onto the graph; a
    # presence_col naming a column that was not exported would restore a
    # declaration netis_data() then refuses to construct.
    presence_col = if (!is.null(d$presence_col) && d$presence_col %in% names(edge_df)) d$presence_col else NULL
  )
  g
}

#' The ledger fields of a tie descriptor whose state is "declared"
#'
#' @noRd
.declared_tie_fields <- function(d) {
  states <- vapply(.ledger_fields, function(f) {
    st <- d$ledger[[f]]$state
    if (is.null(st)) NA_character_ else st
  }, character(1))
  .ledger_fields[!is.na(states) & states == "declared"]
}

#' Refuse an exported object that stands for a network nobody observed
#'
#' @param meta the object's `netis_meta` record, or `NULL`.
#' @param what the argument name, for the message.
#' @param hint text naming the importer that keeps the difference, or `NULL`.
#' @noRd
.assert_export_was_observed <- function(meta, what, hint = NULL) {
  if (!isFALSE(meta$network_observed)) {
    return(invisible(TRUE))
  }
  stop(
    sprintf(
      "%s was exported from group '%s' wave %s, where the network is unobserved. It has no ties because none were observed, and on its own it would read as a network observed to be empty.%s",
      what, meta$group, format(meta$wave),
      if (is.null(hint)) "" else sprintf(" Use %s with the group's other waves, which keeps the difference.", hint)
    ),
    call. = FALSE
  )
}

#' The `observed_tie_waves` declaration of a one-snapshot import
#'
#' A graph or network object handed over at a wave is an observed network, edges
#' or none.
#'
#' @noRd
.observed_single_snapshot <- function(group, wave) {
  if (is.null(wave)) {
    return(NULL)
  }
  data.frame(group = group, wave = wave, stringsAsFactors = FALSE)
}

#' Convert an igraph graph back to a netis_data object
#'
#' The reverse of [as_igraph()]. When `g` carries the `netis_meta` graph attribute
#' [as_igraph()] attaches, `group`/`wave`/`directed`/`self_ties` default to the values
#' recorded there, and the `weight` edge attribute [as_igraph()] aliases from the selected
#' tie variable is dropped rather than reconstructed as a spurious extra tie variable
#' (round-tripping exactly). For a graph from elsewhere, sensible defaults are used
#' instead: `group = "1"` (a single implied group), no wave dimension, directedness from
#' [igraph::is_directed()], and `self_ties` inferred from whether any self-loop is
#' present (a loop's absence in one particular graph does not prove a policy of
#' `"prohibited"` -- override explicitly if that distinction matters). A graph
#' with no edge attributes comes back with no tie variable; give its edges an
#' attribute first if a formula is to name one.
#'
#' # What does not survive
#'
#' This is a lossy conversion in general, not just for a foreign graph:
#' \itemize{
#'   \item **Parallel edges are refused, not silently collapsed.** `netis_data` has no
#'     multi-edge representation; a graph with more than one edge between the same ordered
#'     pair errors, naming `igraph::simplify()` as the fix, rather than picking one edge
#'     or summing weights on the caller's behalf.
#'   \item **Graph-level attributes other than `netis_meta` are not kept.** Arbitrary
#'     custom graph attributes on a foreign graph have no place to go in `netis_data`'s
#'     schema, so they raise the `foreign_attributes` loss ([netis_losses()]): a warning
#'     naming them, unless `allow_loss` authorizes it.
#'   \item **Non-atomic vertex/edge attributes** (e.g. list-valued attributes) are not
#'     specially handled and may fail when `netis_data()`'s own data-frame construction is
#'     reached; this function does not coerce them.
#'   \item **One graph, one object.** Each call builds one single-group (or
#'     single-group-wave) `netis_data` object; [netis_data_from_igraph_list()]
#'     recombines a list produced by [as_igraph_list()].
#' \item **Only declared semantics are restored.** `weighted`, `weight_semantics`,
#'     `direction_semantics` and `zero_meaning` are restored from `netis_meta` when they
#'     were declared on the original object; a value rnetis had inferred is inferred
#'     again, and an undeclared one stays undeclared, so [netis_boundary()] reports the
#'     same provenance after the round trip.
#'   \item **Only present ties make the trip.** [as_igraph()] exports the present ties,
#'     so a declared `presence_mode` of `"row_presence"` is restored, an `"indicator"`
#'     network restores with its indicator column, and a `"complete_dyadic"` or
#'     `"value_rule"` network comes back without its non-present rows and with its
#'     presence mode inferred afresh. A `presence_rule` closure has no stable serialized
#'     form; exporting one raises the `presence_metadata` loss ([netis_losses()]).
#'   \item **A graph exported from an unobserved network is refused**: it has no
#'     edges because no one observed them, and on its own it would read as an observed
#'     empty network. [netis_data_from_igraph_list()] keeps the difference when the
#'     group's other waves come with it.
#'   \item **Reserved attribute names are refused.** A vertex attribute named `node`,
#'     `group` (or `wave`, when a wave is assigned), or an edge attribute named `source`,
#'     `target`, `group` (or `wave`) would be overwritten by the key columns this function
#'     builds, so it is an error.
#' }
#'
#' @param g an `igraph` object.
#' @param group group identifier to assign. Defaults to the value recorded in `g`'s
#'   `netis_meta` attribute if present, otherwise `"1"`.
#' @param wave wave identifier to assign, or `NULL` for no wave dimension. Defaults to the
#'   value recorded in `g`'s `netis_meta` attribute if present, otherwise `NULL`.
#' @param self_ties `"prohibited"` or `"permitted"`, or `NULL` to use the recorded/inferred
#'   default (see Details).
#' @param allow_loss character vector of conversion losses to authorize; see
#'   [netis_losses()]. Relevant here for `foreign_attributes`.
#' @return a [netis_data()] object.
#' @references
#' Csardi, G. and Nepusz, T. (2006) The igraph software package for complex
#' network research. *InterJournal*, Complex Systems, 1695.
#' @seealso [as_igraph()] for the forward conversion.
#' @examples
#' # Suggests dependency: igraph, without which the example cannot run.
#' \donttest{
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   nodes <- data.frame(group = 1, node = c("a", "b", "c"), trait = c(1, 2, 3))
#'   ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), weight = c(2, 1))
#'   nd <- netis_data(nodes, ties)
#'   g <- as_igraph(nd)
#'   nd2 <- netis_data_from_igraph(g)
#'   nd2
#' }
#' }
#' @export
netis_data_from_igraph <- function(g, group = NULL, wave = NULL, self_ties = NULL,
                                   allow_loss = NULL) {
  .require_igraph("netis_data_from_igraph()")
  if (!inherits(g, "igraph")) {
    stop("g must be an igraph object.", call. = FALSE)
  }
  .check_allow_loss(allow_loss)
  foreign <- setdiff(igraph::graph_attr_names(g), "netis_meta")
  if (length(foreign) > 0L) {
    .loss_raise("foreign_attributes", allow_loss = allow_loss,
                detail = sprintf("Graph attribute(s): %s.",
                                 paste(sprintf("'%s'", foreign), collapse = ", ")))
  }

  meta <- tryCatch(igraph::graph_attr(g, "netis_meta"), error = function(e) NULL)
  is_own <- !is.null(meta)
  .assert_export_was_observed(meta, "g", "netis_data_from_igraph_list()")

  group_val <- if (!is.null(group)) as.character(group) else if (is_own) meta$group else "1"
  wave_val <- if (!is.null(wave)) wave else if (is_own) meta$wave else NULL
  directed_val <- if (is_own) meta$directed else igraph::is_directed(g)
  self_ties_val <- if (!is.null(self_ties)) {
    self_ties
  } else if (is_own) {
    meta$self_ties
  } else {
    if (any(igraph::which_loop(g))) "permitted" else "prohibited"
  }

  vdf <- igraph::as_data_frame(g, what = "vertices")
  if (!"name" %in% names(vdf)) {
    stop("g has no vertex names (igraph::V(g)$name); netis_data_from_igraph() requires named vertices.", call. = FALSE)
  }
  # The key columns rnetis builds are reserved: a vertex attribute already
  # called one of them would be overwritten, re-keying or regrouping nodes.
  reserved_v <- intersect(names(vdf), c("node", "group", if (!is.null(wave_val)) "wave"))
  if (length(reserved_v) > 0L) {
    stop(
      sprintf(
        "g has vertex attribute(s) %s, which netis_data_from_igraph() uses as key columns (node identity, group, wave) and would overwrite. Rename or remove them first; use the group and wave arguments to set the group and wave.",
        paste(sprintf("'%s'", reserved_v), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  names(vdf)[names(vdf) == "name"] <- "node"
  vdf$group <- rep(group_val, nrow(vdf))
  if (!is.null(wave_val)) {
    vdf$wave <- rep(wave_val, nrow(vdf))
  }

  edf <- igraph::as_data_frame(g, what = "edges")
  reserved_e <- intersect(names(edf), c("source", "target", "group", if (!is.null(wave_val)) "wave"))
  if (length(reserved_e) > 0L) {
    stop(
      sprintf(
        "g has edge attribute(s) %s, which netis_data_from_igraph() uses as key columns and would overwrite. Rename or remove them first.",
        paste(sprintf("'%s'", reserved_e), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  names(edf)[names(edf) == "from"] <- "source"
  names(edf)[names(edf) == "to"] <- "target"
  if (is_own && !identical(meta$network, "weight") && "weight" %in% names(edf)) {
    edf$weight <- NULL
  }
  if (nrow(edf) > 0L) {
    dup_key <- paste(edf$source, edf$target, sep = .NETIS_KEY_SEP)
    if (anyDuplicated(dup_key) > 0L) {
      stop(
        "g has parallel edges (more than one edge between the same ordered pair); netis_data_from_igraph() does not support multigraphs. Simplify first, e.g. with igraph::simplify(g, edge.attr.comb = \"sum\").",
        call. = FALSE
      )
    }
  }
  edf$group <- rep(group_val, nrow(edf))
  if (!is.null(wave_val)) {
    edf$wave <- rep(wave_val, nrow(edf))
  }

  out <- netis_data(
    vdf, edf,
    group_col = "group", node_col = "node",
    wave_col = if (is.null(wave_val)) NULL else "wave",
    source_col = "source", target_col = "target",
    directed = directed_val, self_ties = self_ties_val,
    tie_meta = .netis_meta_to_tie_meta(meta, edf),
    observed_tie_waves = .observed_single_snapshot(group_val, wave_val)
  )
  # TRUE exactly when directedness came from the graph's own structural flag:
  # a foreign graph is read with igraph::is_directed(). When `netis_meta` is
  # present, `directed_val` came from rnetis's own earlier record of a caller's
  # declaration travelling back, which is not igraph asserting anything -- and
  # the ledger keeps those apart.
  .record_boundary(
    out, "netis_data_from_igraph", "igraph",
    source_encoded_directedness = !is_own
  )
}

#' Reconstruct a `tie_meta` argument from a graph's `netis_meta` attribute
#'
#' Without this, a declared `weight_semantics = "strength"` would come back
#' `"unitless"` after `as_igraph()` then `netis_data_from_igraph()`.
#'
#' ONLY DECLARED FIELDS ARE RESTORED, as listed in `meta$declared`. Restoring an
#' inferred value, or a neutral default standing for "undeclared", as a
#' declaration would change what [netis_boundary()] reports and switch off
#' refusals that apply to inferred or undeclared semantics (for example the
#' stored-zero refusal for an inferred `row_presence`). A graph whose
#' `netis_meta` has no `declared` record restores nothing.
#'
#' `presence_mode` is restored only where it still describes the graph's edges,
#' which are the present ties (`as_igraph()` exports nothing else): a declared
#' `row_presence`, and a declared `indicator` whose column made the trip. A
#' declared `complete_dyadic` or `value_rule` network has lost its non-present
#' rows on the way through, so its mode is not restored and is inferred afresh.
#'
#' @noRd
.netis_meta_to_tie_meta <- function(meta, edf) {
  if (is.null(meta) || !("network" %in% names(meta)) || !(meta$network %in% names(edf))) {
    return(NULL)
  }
  declared <- meta$declared
  if (is.null(declared)) {
    return(NULL)
  }
  entry <- list()
  for (f in intersect(c("weight_semantics", "direction_semantics", "zero_meaning", "weighted"),
                      declared)) {
    if (!is.null(meta[[f]]) && !is.na(meta[[f]])) {
      entry[[f]] <- meta[[f]]
    }
  }
  if ("presence_mode" %in% declared) {
    if (identical(meta$presence_mode, "row_presence")) {
      entry$presence_mode <- "row_presence"
    } else if (identical(meta$presence_mode, "indicator") &&
               !is.null(meta$presence_col) && meta$presence_col %in% names(edf)) {
      entry$presence_mode <- "indicator"
      entry$presence_col <- meta$presence_col
    }
  }
  if (length(entry) == 0L) {
    return(NULL)
  }
  stats::setNames(list(entry), meta$network)
}

#' @rdname as_netis_data
#' @export
as_netis_data.igraph <- function(x, ...) {
  netis_data_from_igraph(x, ...)
}
