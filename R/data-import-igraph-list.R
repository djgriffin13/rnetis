# Recombining a list of igraph graphs into one netis_data.
#
# The inverse of `as_igraph_list()`. Making repeated `netis_data_from_igraph()`
# calls and `rbind()`ing the underlying frames works and is easy to get subtly
# wrong: the tie variable has to be the same in every graph, the
# directedness and self-tie policy have to agree, and the group/wave each graph
# belongs to has to be recovered from somewhere.
#
# WHERE GROUP AND WAVE COME FROM, AND WHY NOT FROM THE NAMES.
# `as_igraph_list()` names its elements "<group>" or "<group>_w<wave>". Parsing
# that back is guesswork the moment a group identifier contains "_w" -- and
# group identifiers are user data, so it eventually will. Each graph already
# carries its own `netis_meta` recording exactly which group and wave it is, so
# that is what this reads. List names are a convenience label for humans; the
# attribute is the record.

#' Recombine a list of igraph graphs into one netis_data object
#'
#' The inverse of [as_igraph_list()]: takes the graphs back to a single
#' multi-group and/or multi-wave [netis_data()] object, rather than one object
#' per graph.
#'
#' # Where group and wave come from
#'
#' For graphs produced by [as_igraph_list()], each carries a `netis_meta`
#' attribute recording which group and wave it is, and that is what this reads.
#' The list's **names are not parsed** -- `as_igraph_list()` names elements
#' `"<group>_w<wave>"`, and recovering the two halves from that string stops
#' being reliable the moment a group identifier happens to contain `"_w"`.
#'
#' For graphs from elsewhere, supply `groups` (and `waves` if the collection is
#' longitudinal), one entry per graph. rnetis will not fall back to positional
#' numbering: a group identifier is something you chose, not an index rnetis
#' should invent.
#'
#' # What must agree across the graphs
#'
#' Directedness, the self-tie policy, and the recorded tie variable must be the
#' same in every graph, because the result is one object with one schema. A
#' mismatch is an error naming the values that disagree, rather than a silent
#' choice of whichever came first; pass the matching argument to settle it.
#'
#' # Reserved names
#'
#' A vertex attribute named `node` or `group` (or `wave`, when `waves` assigns
#' one), or an edge attribute named `source`, `target` or `group` (or `wave`), would
#' be overwritten by the key columns this function builds, so it is an error, as in
#' [netis_data_from_igraph()].
#'
#' # Edgeless graphs
#'
#' With `waves`, an edgeless graph is a network observed to be empty at that wave,
#' not an unobserved one (see Empty and unobserved waves on [netis_data()]). Leave
#' a wave out of the list to say it was not observed. The exception is a graph
#' [as_igraph_list()] built for an unobserved network, which records that and comes
#' back unobserved.
#'
#' @param graphs a list of `igraph` objects.
#' @param groups optional character vector of group identifiers, one per graph.
#'   Required when the graphs carry no `netis_meta`.
#' @param waves optional vector of wave labels, one per graph. Omit for a
#'   collection with no wave dimension. Numeric labels (or text that reads as
#'   numbers) are ordered by value; other text labels are ordered as they first
#'   appear in `graphs`; a factor by its levels.
#' @param network for graphs from elsewhere, the name of the tie variable given to
#'   a graph with no edge attributes, which records each edge as `1`; defaults to
#'   `"weight"`. A graph's own edge attributes keep their names. For graphs
#'   carrying `netis_meta`, supplying it lets the list proceed when their records
#'   name different tie variables; every tie variable on the graphs is kept, and
#'   the declared semantics restored are those the first graph records.
#' @param directed,self_ties optional overrides; default to the recorded or
#'   structurally encoded values.
#' @param allow_loss character vector of conversion losses to authorize; see
#'   [netis_data_from_igraph()].
#' @return a [netis_data()] object.
#' @references
#' Csardi, G. and Nepusz, T. (2006) The igraph software package for complex
#' network research. *InterJournal*, Complex Systems, 1695.
#' @seealso [as_igraph_list()] for the forward conversion,
#'   [netis_data_from_igraph()] for a single graph
#' @examples
#' # Suggests dependency: igraph, without which the example cannot run.
#' \donttest{
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   nodes <- data.frame(group = c(1, 1, 2, 2), node = c("a", "b", "c", "d"))
#'   ties <- data.frame(group = c(1, 2), source = c("a", "c"),
#'                      target = c("b", "d"), weight = c(1, 2))
#'   nd <- netis_data(nodes, ties)
#'
#'   graphs <- as_igraph_list(nd)
#'   back <- netis_data_from_igraph_list(graphs)
#'   nrow(netis_nodes(back))
#' }
#' }
#' @export
netis_data_from_igraph_list <- function(graphs, groups = NULL, waves = NULL,
                                         network = NULL, directed = NULL,
                                         self_ties = NULL, allow_loss = NULL) {
  .require_igraph("netis_data_from_igraph_list()")
  .check_allow_loss(allow_loss)
  if (!is.list(graphs) || length(graphs) == 0L) {
    stop("graphs must be a non-empty list of igraph objects.", call. = FALSE)
  }
  bad <- !vapply(graphs, function(g) inherits(g, "igraph"), logical(1))
  if (any(bad)) {
    stop(
      sprintf("graphs must all be igraph objects; element(s) %s are not.", paste(which(bad), collapse = ", ")),
      call. = FALSE
    )
  }

  foreign <- unique(unlist(lapply(graphs, function(g) {
    setdiff(igraph::graph_attr_names(g), "netis_meta")
  })))
  if (length(foreign) > 0L) {
    .loss_raise("foreign_attributes", allow_loss = allow_loss,
                detail = sprintf("Graph attribute(s): %s.",
                                 paste(sprintf("'%s'", foreign), collapse = ", ")))
  }
  metas <- lapply(graphs, function(g) tryCatch(igraph::graph_attr(g, "netis_meta"), error = function(e) NULL))
  have_meta <- !vapply(metas, is.null, logical(1))
  if (any(have_meta) && !all(have_meta)) {
    stop(
      sprintf(
        "Some graphs carry a netis_meta record and some do not (missing on element(s) %s). Convert a homogeneous list, or supply groups/waves explicitly for all of them.",
        paste(which(!have_meta), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  n <- length(graphs)
  groups <- .igl_resolve_labels(groups, metas, have_meta, "group", n, required = TRUE)
  waves <- .igl_resolve_labels(waves, metas, have_meta, "wave", n, required = FALSE)

  network <- .igl_resolve_network(network, metas, have_meta)
  directed <- .igl_resolve_scalar(directed, metas, have_meta, "directed",
    fallback = function(g) igraph::is_directed(g), graphs = graphs
  )
  self_ties <- .igl_resolve_scalar(self_ties, metas, have_meta, "self_ties",
    fallback = function(g) if (any(igraph::which_loop(g))) "permitted" else "prohibited",
    graphs = graphs
  )

  pieces <- lapply(seq_len(n), function(i) {
    .igl_one_piece(graphs[[i]], metas[[i]], groups[[i]],
      if (is.null(waves)) NULL else waves[[i]], network
    )
  })

  vdf <- do.call(rbind, lapply(pieces, `[[`, "nodes"))
  edf <- do.call(rbind, lapply(pieces, `[[`, "edges"))
  rownames(vdf) <- NULL
  rownames(edf) <- NULL

  # The node roster is deduplicated across snapshots so a node present at
  # several waves is one node, not several -- but ONLY on the identity columns
  # plus wave, so a genuine per-wave attribute is preserved.
  vdf <- unique(vdf)

  out <- netis_data(
    vdf, edf,
    group_col = "group", node_col = "node",
    wave_col = if (is.null(waves)) NULL else "wave",
    source_col = "source", target_col = "target",
    directed = directed, self_ties = self_ties,
    tie_meta = if (any(have_meta)) .netis_meta_to_tie_meta(metas[[1L]], edf) else NULL,
    observed_tie_waves = if (!is.null(waves)) {
      observed <- !vapply(metas, function(m) isFALSE(m$network_observed), logical(1))
      data.frame(group = groups[observed], wave = waves[observed], stringsAsFactors = FALSE)
    }
  )
  .record_boundary(
    out, "netis_data_from_igraph_list", "list of igraph",
    source_encoded_directedness = !any(have_meta)
  )
}

#' Resolve per-graph group or wave labels
#'
#' Explicit argument wins; otherwise the recorded `netis_meta`. There is no
#' positional fallback: numbering the graphs 1..n would invent identifiers that
#' look like the user's own.
#'
#' @noRd
.igl_resolve_labels <- function(supplied, metas, have_meta, field, n, required) {
  if (!is.null(supplied)) {
    if (length(supplied) != n) {
      stop(
        sprintf("%ss has %d entr%s but there are %d graphs.", field, length(supplied),
                if (length(supplied) == 1L) "y" else "ies", n),
        call. = FALSE
      )
    }
    return(.igl_typed_labels(supplied, field))
  }
  if (!all(have_meta)) {
    if (!required) {
      return(NULL)
    }
    stop(
      sprintf(
        "These graphs carry no netis_meta record, so rnetis cannot tell which %s each belongs to. Supply %ss = , one entry per graph.",
        field, field
      ),
      call. = FALSE
    )
  }
  vals <- lapply(metas, function(m) m[[field]])
  if (all(vapply(vals, is.null, logical(1)))) {
    if (!required) {
      return(NULL)
    }
    stop(sprintf("The graphs' netis_meta records no %s; supply %ss = .", field, field), call. = FALSE) # nocov
  }
  if (any(vapply(vals, is.null, logical(1)))) {
    stop(
      sprintf(
        "Some graphs record a %s and some do not. A collection is either longitudinal or it is not; supply %ss = to say which.",
        field, field
      ),
      call. = FALSE
    )
  }
  .igl_typed_labels(unlist(vals), field)
}

#' Group labels are identifiers and stay text. Wave labels keep the type that
#' orders them (see `.import_wave_values()`): stringifying them would sort
#' waves 9, 10 as "10", "9".
#' @noRd
.igl_typed_labels <- function(vals, field) {
  if (identical(field, "wave")) .import_wave_values(vals) else as.character(vals)
}

#' @noRd
.igl_resolve_network <- function(network, metas, have_meta) {
  if (!is.null(network)) {
    return(network)
  }
  if (!all(have_meta)) {
    return("weight")
  }
  nets <- unique(unlist(lapply(metas, function(m) m$network)))
  if (length(nets) != 1L) {
    stop(
      sprintf(
        "The graphs record different tie variables (%s). One netis_data object has one schema, so they must agree; supply network = to proceed anyway, keeping every tie variable on the graphs with the declared semantics the first graph records.",
        paste(nets, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  nets
}

#' @noRd
.igl_resolve_scalar <- function(supplied, metas, have_meta, field, fallback, graphs) {
  if (!is.null(supplied)) {
    return(supplied)
  }
  vals <- if (all(have_meta)) {
    unique(unlist(lapply(metas, function(m) m[[field]])))
  } else {
    unique(unlist(lapply(graphs, fallback)))
  }
  if (length(vals) != 1L) {
    stop(
      sprintf(
        "The graphs disagree on %s (%s). One netis_data object has one schema, so this must be the same across the collection; supply %s = to settle it.",
        field, paste(vals, collapse = ", "), field
      ),
      call. = FALSE
    )
  }
  vals
}

#' Extract one graph's node and edge frames, tagged with its group and wave
#'
#' @noRd
.igl_one_piece <- function(g, meta, group, wave, network) {
  vdf <- igraph::as_data_frame(g, what = "vertices")
  if (!("name" %in% names(vdf))) {
    stop(
      "A graph has no vertex names (igraph::V(g)$name); netis_data_from_igraph_list() requires named vertices, since node identity is what links the snapshots together.",
      call. = FALSE
    )
  }
  # The key columns built below are reserved: an attribute already called one of
  # them would be overwritten, re-keying or regrouping nodes and ties.
  reserved_v <- intersect(names(vdf), c("node", "group", if (!is.null(wave)) "wave"))
  if (length(reserved_v) > 0L) {
    stop(
      sprintf(
        "The graph for group '%s' has vertex attribute(s) %s, which netis_data_from_igraph_list() uses as key columns (node identity, group, wave) and would overwrite. Rename or remove them first; use the groups and waves arguments to set the group and wave.",
        group, paste(sprintf("'%s'", reserved_v), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  names(vdf)[names(vdf) == "name"] <- "node"
  # `rep(x, nrow(df))` rather than a bare scalar: assigning a length-1 value to
  # a ZERO-ROW data frame is an error, and zero rows is the ordinary case here
  # -- a wave at which a group has no ties at all, which is a real snapshot and
  # not a malformed one.
  vdf$group <- rep(group, nrow(vdf))
  if (!is.null(wave)) vdf$wave <- rep(wave, nrow(vdf))

  edf <- igraph::as_data_frame(g, what = "edges")
  reserved_e <- intersect(names(edf), c("source", "target", "group", if (!is.null(wave)) "wave"))
  if (length(reserved_e) > 0L) {
    stop(
      sprintf(
        "The graph for group '%s' has edge attribute(s) %s, which netis_data_from_igraph_list() uses as key columns and would overwrite. Rename or remove them first.",
        group, paste(sprintf("'%s'", reserved_e), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  names(edf)[names(edf) == "from"] <- "source"
  names(edf)[names(edf) == "to"] <- "target"
  # The `weight` alias as_igraph() adds is dropped symmetrically, exactly as
  # netis_data_from_igraph() does, so it does not come back as a spurious extra
  # tie variable.
  if (!is.null(meta) && !identical(meta$network, "weight") && "weight" %in% names(edf)) {
    edf$weight <- NULL
  }
  # A foreign graph with no edge attributes still needs a tie variable, or there
  # is nothing for a formula to name. `1` records the edge, as in
  # netis_data_from_network().
  if (is.null(meta) && identical(names(edf), c("source", "target"))) {
    edf[[network]] <- rep(1, nrow(edf))
  }
  if (nrow(edf) > 0L && anyDuplicated(paste(edf$source, edf$target, sep = .NETIS_KEY_SEP)) > 0L) {
    .loss_raise(
      "tie_value_collapse",
      detail = sprintf("Graph for group '%s' has parallel edges.", group)
    )
  }
  edf$group <- rep(group, nrow(edf))
  if (!is.null(wave)) edf$wave <- rep(wave, nrow(edf))

  list(nodes = vdf, edges = edf)
}
