# statnet `network` interoperability.
#
# The `network` class is the statnet ecosystem's data model -- what `ergm` and
# `sna` consume. rnetis builds no ERGM bridge, and instead makes leaving for one
# trivial.
#
# THREE THINGS THIS BOUNDARY HANDLES THAT igraph's DOES NOT.
#
# 1. A `network` edge either exists or it does not. There is no "row with a
#    value but no edge", so the edge set must be PRESENCE-RESOLVED before
#    export. For a row_presence network that is every row; for a
#    complete_dyadic one it is whatever the declared indicator or rule says;
#    and for an UNDETERMINED complete_dyadic network it is unknowable, so the
#    export refuses rather than inventing the distinction.
#
# 2. Values live in named edge attributes, and most statnet consumers ignore
#    them unless told (`ignore.eval = FALSE`). Exporting a weighted network
#    without saying so is how a valued analysis silently becomes a binary one,
#    so the attribute is named explicitly and the documentation says what a
#    consumer must pass.
#
# 3. `network` supports multiplex edges natively, so the multigraph refusal
#    igraph made rare is reachable here far more often.
#
# EDGE-ATTRIBUTE ALIGNMENT. `set.edge.attribute()` with a vector assigns by
# edge id, which equals insertion order for a network built in one pass and
# never edited. That holds here, and it is asserted rather than trusted
# (`.nw_assert_edge_alignment()`): a misalignment would attach every weight to
# the wrong dyad while leaving counts, density, and degree all correct.

#' @noRd
.require_network <- function(fn_name) {
  if (!requireNamespace("network", quietly = TRUE)) {
    stop(
      sprintf("%s requires the 'network' package. Install it with install.packages(\"network\").", fn_name),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Convert a netis_data object to a statnet `network` object
#'
#' Builds one [network::network()] object for a chosen (group, wave, tie
#' variable) selection -- the format `ergm`, `sna`, and the rest of the statnet
#' ecosystem consume.
#'
#' Node identifiers become vertex names (never positional indices), every node
#' variable at the selected wave becomes a vertex attribute, and every tie
#' variable becomes an edge attribute. A node with no ties still appears as an
#' isolated vertex, because the vertex set is built from the node rows at the
#' selected wave rather than inferred from edge endpoints; a tie reaching a node
#' with no row there is an error.
#'
#' # Valued networks need one extra argument downstream
#'
#' `network` stores tie values as **named edge attributes**, and most statnet
#' functions ignore them unless told to look. After exporting a weighted
#' network, pass the attribute name on:
#'
#' ```r
#' net <- as_network(nd, network = "w")
#' sna::degree(network::as.sociomatrix(net, "w"), ignore.eval = FALSE)
#' ```
#'
#' Omitting that silently analyses the binarized network instead, which is why
#' the value attribute is named for the tie variable rather than something
#' generic.
#'
#' # Edge presence must be resolvable
#'
#' A `network` edge either exists or it does not. For a `complete_dyadic`
#' network (see [netis_data()]'s `tie_meta`) with no declared indicator or rule,
#' edge presence is **undetermined**, and this refuses rather than treating
#' measured zeros as absences. Declare `presence_col` or `presence_rule` first.
#' A network whose `row_presence` was inferred and which stores zeros is refused
#' for the same reason, unless `zero_meaning = "measured_zero"` is declared.
#'
#' @param x a [netis_data()] object.
#' @param network name of the tie variable to convert. Inferred if there is
#'   exactly one; an error naming the candidates otherwise.
#' @param group the group to convert. Inferred if there is exactly one. See
#'   [as_network_list()] for every group at once.
#' @param wave the wave to convert, or `NULL` if the data has no wave dimension.
#'   Inferred if exactly one wave is observed.
#' @param allow_loss character vector of conversion losses to authorize; see
#'   [netis_losses()]. Relevant here for `presence_metadata`, `schema_records` and
#'   `derived_provenance`, as in [as_igraph()].
#' @return a `network` object. A wave whose network is unobserved (see Empty and
#'   unobserved waves on [netis_data()]) gives an edgeless object and a warning
#'   naming the group and wave; an observed empty network gives the same object with
#'   no warning.
#' @references
#' Butts, C. T. (2008) network: a package for managing relational data in R.
#' *Journal of Statistical Software* **24**(2).
#' @seealso [as_network_list()], [netis_data_from_network()], [as_igraph()]
#' @examples
#' # Suggests dependency: network, without which the example cannot run.
#' \donttest{
#' if (requireNamespace("network", quietly = TRUE)) {
#'   nodes <- data.frame(group = 1, node = c("a", "b", "z"), trait = c(1, 2, 3))
#'   ties <- data.frame(group = 1, source = "a", target = "b", w = 2)
#'   nd <- netis_data(nodes, ties)
#'   net <- as_network(nd)
#'   net
#' }
#' }
#' @export
as_network <- function(x, network = NULL, group = NULL, wave = NULL, allow_loss = NULL) {
  .check_netis_data(x)
  .require_network("as_network()")
  .check_allow_loss(allow_loss)
  s <- netis_schema(x)
  network <- .resolve_igraph_network(s, network)
  group <- .resolve_igraph_group(x, s, group)
  wave <- .resolve_wave(s, wave, multi_hint = ", or use as_network_list() to convert every wave at once.")
  out <- .build_one_network(x, s, network, group, wave, allow_loss)
  .warn_unobserved_export(.unobserved_network_labels(netis_ties(x), s, group, wave), "network object")
  out
}

#' Convert each group and wave of a netis_data object to network objects
#'
#' The list form of [as_network()]. A separate function rather than a return
#' type that changes with the data, for the same reason [as_igraph_list()] is.
#'
#' @param x a [netis_data()] object.
#' @param network name of the tie variable to convert.
#' @param allow_loss character vector of conversion losses to authorize; see
#'   [as_network()] and [netis_losses()].
#' @return a named list of `network` objects. Names are `"<group>"` without a
#'   wave dimension, `"<group>_w<wave>"` with one, one per wave at which the group
#'   has node rows. A warning names every snapshot whose network is unobserved, as
#'   in [as_network()].
#' @references
#' Butts, C. T. (2008) network: a package for managing relational data in R.
#' *Journal of Statistical Software* **24**(2).
#' @seealso [as_network()], [netis_data_from_network()]
#' @examples
#' # Suggests dependency: network, without which the example cannot run.
#' \donttest{
#' if (requireNamespace("network", quietly = TRUE)) {
#'   nodes <- data.frame(group = c(1, 1, 2, 2), node = c("a", "b", "c", "d"))
#'   ties <- data.frame(group = c(1, 2), source = c("a", "c"),
#'                      target = c("b", "d"), w = c(1, 2))
#'   names(as_network_list(netis_data(nodes, ties)))
#' }
#' }
#' @export
as_network_list <- function(x, network = NULL, allow_loss = NULL) {
  .check_netis_data(x)
  .require_network("as_network_list()")
  .check_allow_loss(allow_loss)
  s <- netis_schema(x)
  network <- .resolve_igraph_network(s, network)
  groups <- sort(unique(as.character(netis_nodes(x)[[s$group_col]])))
  has_wave <- !is.null(s$wave_col) && length(s$waves) > 0L
  # Named once for the whole list rather than once per object.
  allow_loss <- c(allow_loss, .raise_schema_records_loss(x, s, network, allow_loss))

  out <- list()
  for (g in groups) {
    if (has_wave) {
      for (w in .waves_observed_in_group(x, s, g)) {
        out[[sprintf("%s_w%s", g, w)]] <- .build_one_network(x, s, network, g, w, allow_loss)
      }
    } else {
      out[[g]] <- .build_one_network(x, s, network, g, NULL, allow_loss)
    }
  }
  .warn_unobserved_export(
    .unobserved_list_labels(out, function(n) network::get.network.attribute(n, "netis_meta")),
    "network object"
  )
  out
}

#' Build one `network` object for a resolved selection
#'
#' @noRd
.build_one_network <- function(x, schema, network, group, wave, allow_loss = NULL) {
  nodes <- .filter_group_wave(netis_nodes(x), schema, group, wave)
  ties <- .filter_group_wave(netis_ties(x), schema, group, wave)
  .assert_group_wave_observed(nodes, group, wave)
  .raise_schema_records_loss(x, schema, network, allow_loss)
  d <- schema$tie_descriptors[[network]]

  # A `network` edge exists or does not, so presence is resolved BEFORE the
  # edge set is built. `.resolve_tie_presence()` refuses on an undetermined
  # complete_dyadic network, which is exactly the right refusal here: there is
  # no representation in `network` for "a dyad observed to have value zero".
  if (nrow(ties) > 0L) {
    .assert_tie_endpoints_on_roster(
      ties, schema, group,
      unique(as.character(netis_nodes(x)[[schema$node_col]][
        as.character(netis_nodes(x)[[schema$group_col]]) == group]))
    )
    present <- .resolve_tie_presence(d, ties, network)
    ties <- ties[!is.na(present) & present, , drop = FALSE]
  }

  ids <- sort(unique(as.character(nodes[[schema$node_col]])))
  net <- network::network.initialize(
    length(ids),
    directed = schema$directed,
    loops = identical(schema$self_ties, "permitted")
  )
  network::network.vertex.names(net) <- ids

  node_attr_cols <- setdiff(names(nodes), c(schema$group_col, schema$node_col, schema$wave_col))
  # A derived column (add_centering()/add_decomposition()) becomes
  # an ordinary vertex attribute with no marker distinguishing it from a
  # measured one -- see data-convert-loss.R's derived_provenance entry.
  derived_here <- intersect(node_attr_cols, names(schema$derived))
  if (length(derived_here) > 0L) {
    .loss_raise(
      "derived_provenance", allow_loss = allow_loss,
      detail = sprintf("Column(s) %s.", paste(sprintf("'%s'", derived_here), collapse = ", "))
    )
  }
  # Name-based alignment, never positional: `nodes` arrives in whatever order
  # the storage layer produced, and `ids` is sorted.
  ord <- match(ids, as.character(nodes[[schema$node_col]]))
  for (col in node_attr_cols) {
    net <- network::set.vertex.attribute(net, col, nodes[[col]][ord])
  }

  if (nrow(ties) > 0L) {
    tail_i <- match(as.character(ties[[schema$source_col]]), ids)
    head_i <- match(as.character(ties[[schema$target_col]]), ids)
    if (anyNA(tail_i) || anyNA(head_i)) {
      off <- unique(c(as.character(ties[[schema$source_col]])[is.na(tail_i)],
                      as.character(ties[[schema$target_col]])[is.na(head_i)]))
      stop(
        sprintf(
          "%d tie(s) in group '%s' at this wave reference node(s) with no node row at this wave: %s. A network object's vertex set is the node roster at the selected wave, so those ties have no vertex to attach to. Add node rows for them at this wave, or remove the ties.",
          sum(is.na(tail_i) | is.na(head_i)), group, paste(utils::head(off, 5), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    net <- network::add.edges(net, tail = tail_i, head = head_i)
    .nw_assert_edge_alignment(net, tail_i, head_i, network)

    tie_attr_cols <- setdiff(
      names(ties),
      c(schema$group_col, schema$source_col, schema$target_col, schema$wave_col)
    )
    for (col in tie_attr_cols) {
      net <- network::set.edge.attribute(net, col, ties[[col]])
    }
  }

  if (!is.null(d$presence_rule)) {
    .loss_raise(
      "presence_metadata", allow_loss = allow_loss,
      detail = sprintf("Network '%s' resolves presence with a rule function.", network)
    )
  }

  net <- network::set.network.attribute(net, "netis_meta", list(
    group = group, wave = wave, network = network,
    # As on an exported graph: an edgeless object cannot say whether it was observed.
    network_observed = length(.unobserved_network_labels(netis_ties(x), schema, group, wave)) == 0L,
    directed = schema$directed, self_ties = schema$self_ties,
    weight_semantics = d$weight_semantics, presence_mode = d$presence_mode,
    direction_semantics = d$direction_semantics, zero_meaning = d$zero_meaning,
    weighted = d$weighted,
    # Which fields were declared: only these are restored on import.
    declared = .declared_tie_fields(d),
    presence_col = if (!is.null(d$presence_col) && d$presence_col %in% names(ties)) d$presence_col else NULL
  ))
  net
}

#' Assert that the constructed edge set is in the order the attributes assume
#'
#' `set.edge.attribute()` assigns by edge id. For a network built in one pass
#' that equals insertion order, but relying on it silently would be a poor
#' trade: a misalignment attaches every weight to the wrong dyad while leaving
#' edge count, density, and degree all correct, so nothing downstream looks
#' wrong. Verified rather than assumed, at a cost of one integer comparison per
#' edge.
#'
#' @noRd
.nw_assert_edge_alignment <- function(net, tail_i, head_i, network) {
  el <- network::as.matrix.network.edgelist(net)
  if (nrow(el) != length(tail_i) ||
    !identical(as.integer(el[, 1L]), as.integer(tail_i)) ||
    !identical(as.integer(el[, 2L]), as.integer(head_i))) {
    stop(
      sprintf(
        "Internal: the constructed 'network' edge order does not match the tie rows for '%s', so edge attributes would be attached to the wrong dyads. This is a rnetis bug; please report it.",
        network
      ),
      call. = FALSE
    ) # nocov
  }
  invisible(TRUE)
}

#' Convert a statnet `network` object to a netis_data object
#'
#' The reverse of [as_network()]. When the object carries the `netis_meta`
#' attribute [as_network()] attaches, the recorded group, wave, and network
#' semantics are restored. For an object from elsewhere, directedness and the
#' self-tie policy are read from the object's own structural flags -- which
#' `network` genuinely encodes -- and everything else is left undeclared rather
#' than guessed.
#'
#' # What does not survive
#'
#' \itemize{
#'   \item **Multiplex networks are refused, not collapsed.** `network` supports
#'     multiple edges between the same ordered pair; `netis_data` has no
#'     multi-edge representation, so this raises the `tie_value_collapse` loss
#'     rather than picking one edge or summing them on your behalf.
#'   \item **A `presence_rule` closure does not survive** in either direction;
#'     see [netis_losses()].
#'   \item **Network attributes rnetis has no slot for are not kept**: the
#'     `foreign_attributes` loss names them.
#'   \item **Bipartite networks are not supported** and are refused, since
#'     rnetis's node roster is one mode.
#'   \item **Missing edges are refused**: a dyad whose tie status is unknown has no
#'     representation in `netis_data`, and dropping it would record no tie.
#'   \item **An object exported from an unobserved network is refused**: it has no
#'     edges because no one observed them, and on its own it would read as an observed
#'     empty network.
#'   \item **Reserved attribute names are refused**, as in
#'     [netis_data_from_igraph()].
#' }
#'
#' @param net a `network` object.
#' @param group group identifier to assign. Defaults to the recorded value, else
#'   `"1"`.
#' @param wave wave identifier, or `NULL`. Defaults to the recorded value.
#' @param network the name given to a tie variable when the object has no edge
#'   attributes, which records each edge as `1`. Defaults to the name recorded in
#'   `netis_meta`, else `"weight"`. An object's own edge attributes keep their
#'   names.
#' @param self_ties `"prohibited"`/`"permitted"`, or `NULL` to use the recorded
#'   value or the object's own `loops` flag.
#' @param allow_loss character vector of conversion losses to authorize; see
#'   [netis_losses()]. Relevant here for `foreign_attributes`, raised for network
#'   attributes other than the object's structural flags and `netis_meta`.
#' @return a [netis_data()] object.
#' @references
#' Butts, C. T. (2008) network: a package for managing relational data in R.
#' *Journal of Statistical Software* **24**(2).
#' @seealso [as_network()], [netis_boundary()] to see what was restored versus
#'   left undeclared
#' @examples
#' # Suggests dependency: network, without which the example cannot run.
#' \donttest{
#' if (requireNamespace("network", quietly = TRUE)) {
#'   nodes <- data.frame(group = 1, node = c("a", "b", "z"), trait = c(1, 2, 3))
#'   ties <- data.frame(group = 1, source = "a", target = "b", w = 2)
#'   nd <- netis_data(nodes, ties)
#'   back <- netis_data_from_network(as_network(nd))
#'   netis_nodes(back)
#' }
#' }
#' @export
netis_data_from_network <- function(net, group = NULL, wave = NULL,
                                     network = NULL, self_ties = NULL, allow_loss = NULL) {
  .require_network("netis_data_from_network()")
  if (!inherits(net, "network")) {
    stop("net must be a network object (from the 'network' package).", call. = FALSE)
  }
  .check_allow_loss(allow_loss)
  foreign <- setdiff(network::list.network.attributes(net),
                     c("bipartite", "directed", "hyper", "loops", "mnext", "multiple", "n",
                       "netis_meta"))
  if (length(foreign) > 0L) {
    .loss_raise("foreign_attributes", allow_loss = allow_loss,
                detail = sprintf("Network attribute(s): %s.",
                                 paste(sprintf("'%s'", foreign), collapse = ", ")))
  }
  if (isTRUE(network::is.bipartite(net))) {
    stop(
      "This is a bipartite network. netis_data holds one node roster and one mode, so a bipartite structure has no faithful representation; convert the two modes separately, or project first.",
      call. = FALSE
    )
  }
  if (isTRUE(network::is.multiplex(net))) {
    .loss_raise(
      "tie_value_collapse",
      detail = "This network object permits multiple edges between the same ordered pair."
    )
  }

  meta <- tryCatch(network::get.network.attribute(net, "netis_meta"), error = function(e) NULL)
  is_own <- !is.null(meta)
  .assert_export_was_observed(meta, "net")

  group_val <- if (!is.null(group)) as.character(group) else if (is_own) meta$group else "1"
  wave_val <- if (!is.null(wave)) wave else if (is_own) meta$wave else NULL
  directed_val <- if (is_own) meta$directed else network::is.directed(net)
  self_val <- if (!is.null(self_ties)) {
    self_ties
  } else if (is_own) {
    meta$self_ties
  } else if (isTRUE(network::has.loops(net))) {
    "permitted"
  } else {
    "prohibited"
  }
  net_name <- if (!is.null(network)) network else if (is_own) meta$network else "weight"

  # A missing edge (na = TRUE) is a dyad whose tie status is unknown; the
  # edgelist below leaves it out, which would record it as no tie.
  n_na <- network::network.naedgecount(net)
  if (n_na > 0L) {
    stop(
      sprintf(
        "net has %d missing edge(s) (edge attribute na = TRUE). netis_data has no representation for a dyad whose tie status is unknown, and dropping them would record them as no tie. Resolve them (delete them if they mean no tie) before converting.",
        n_na
      ),
      call. = FALSE
    )
  }
  reserved_v <- intersect(network::list.vertex.attributes(net),
                          c("node", "group", if (!is.null(wave_val)) "wave"))
  reserved_e <- intersect(network::list.edge.attributes(net),
                          c("source", "target", "group", if (!is.null(wave_val)) "wave"))
  if (length(reserved_v) + length(reserved_e) > 0L) {
    stop(
      sprintf(
        "net has attribute(s) %s, which netis_data_from_network() uses as key columns (node identity, group, wave, tie endpoints) and would overwrite. Rename or remove them first; use the group and wave arguments to set the group and wave.",
        paste(sprintf("'%s'", c(reserved_v, reserved_e)), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  ids <- as.character(network::network.vertex.names(net))
  vdf <- data.frame(group = group_val, node = ids, stringsAsFactors = FALSE)
  for (a in setdiff(network::list.vertex.attributes(net), c("na", "vertex.names"))) {
    vdf[[a]] <- network::get.vertex.attribute(net, a)
  }
  if (!is.null(wave_val)) vdf$wave <- wave_val

  el <- network::as.matrix.network.edgelist(net)
  edf <- data.frame(
    group = rep(group_val, nrow(el)),
    source = ids[el[, 1L]],
    target = ids[el[, 2L]],
    stringsAsFactors = FALSE
  )
  edge_attrs <- setdiff(network::list.edge.attributes(net), "na")
  for (a in edge_attrs) {
    edf[[a]] <- network::get.edge.attribute(net, a)
  }
  # An unvalued network still needs a tie variable, or there is nothing for a
  # formula to name. `1` records presence, matching how a binary rnetis network
  # already looks.
  if (length(edge_attrs) == 0L) {
    edf[[net_name]] <- rep(1, nrow(edf))
  }
  if (!is.null(wave_val)) edf$wave <- rep(wave_val, nrow(edf))

  out <- netis_data(
    vdf, edf,
    group_col = "group", node_col = "node",
    wave_col = if (is.null(wave_val)) NULL else "wave",
    source_col = "source", target_col = "target",
    directed = directed_val, self_ties = self_val,
    tie_meta = .netis_meta_to_tie_meta(meta, edf),
    observed_tie_waves = .observed_single_snapshot(group_val, wave_val)
  )
  .record_boundary(
    out, "netis_data_from_network", "network",
    source_encoded_directedness = !is_own
  )
}

#' @rdname as_netis_data
#' @export
as_netis_data.network <- function(x, ...) {
  netis_data_from_network(x, ...)
}
