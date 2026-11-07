#' Export a tie variable from a netis_data object as an adjacency matrix
#'
#' Builds one square adjacency matrix per group from a chosen tie variable.
#'
#' By default (`presence = FALSE`) cells with no corresponding tie row are
#' filled with `fill` (default `0`); this is a "value if present, else
#' `fill`" export, not a claim about missingness. A stored `0` stays `0` and an
#' absent row reads as `fill`, so with the default `fill` the two look the same;
#' pass another `fill` to tell them apart.
#'
#' An adjacency matrix represents a network at **one point in time**. When the
#' tie data carries a wave dimension, `wave` selects the snapshot; it is
#' inferred when exactly one wave is observed and is an error naming the
#' candidates otherwise, because one matrix over several waves would correspond
#' to no observed wave. To export every wave, loop over `netis_schema(x)$waves`.
#' The rows and columns are the group's whole roster at every wave (see The node
#' roster on [netis_data()]), so matrices from two waves are conformable.
#'
#' `presence = TRUE` instead returns a 0/1 matrix of resolved edge presence,
#' honoring the tie variable's declared `presence_mode`: a `row_presence`
#' network reads every stored row as present; an `indicator`/`value_rule`
#' network resolves presence from the declared column/rule; a `complete_dyadic`
#' network with neither declared is **undetermined** and this refuses rather
#' than defaulting to `variable != 0`, since a measured zero and a structural
#' absence are different things. An inferred `row_presence` network that stores
#' zeros is refused for the same reason, unless `zero_meaning = "measured_zero"`
#' is declared. `fill` is not used in this mode.
#'
#' @param x a [netis_data()] object.
#' @param variable name of the tie variable to place in the matrix cells (or,
#'   under `presence = TRUE`, whose declared presence rule to resolve).
#' @param fill value used for cells with no corresponding tie, when `presence
#'   = FALSE`. Default `0`. Ignored when `presence = TRUE`.
#' @param drop if `TRUE` (default) and there is exactly one group, return a single
#'   matrix rather than a length-one list.
#' @param presence if `TRUE`, return a 0/1 edge-presence matrix instead of a
#'   value matrix. Default `FALSE`. See Details.
#' @param wave the wave to export, or `NULL` if the data has no wave dimension.
#'   Inferred if there is exactly one observed wave; an **error** naming the
#'   candidates otherwise. An adjacency matrix describes one snapshot, so a
#'   multi-wave object has no single correct matrix to return -- see Details.
#' @return a square numeric matrix (if one group and `drop = TRUE`), or a named
#'   list of matrices, one per group, otherwise. For an undirected `netis_data`
#'   object, the returned matrix is symmetric (see Undirected data on
#'   [netis_data()]). A tie whose endpoint is not in the group's node data is an
#'   error, not a dropped cell. A wave whose network is unobserved (see Empty and
#'   unobserved waves on [netis_data()]) gives an all-`fill` matrix and a warning
#'   naming the group and wave; an observed empty network gives the same matrix
#'   with no warning.
#' @seealso [netis_data_from_adjacency()] for the inverse conversion.
#' @examples
#' m <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3,
#'             dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
#' nd <- netis_data_from_adjacency(m)
#' netis_adjacency(nd, "weight")
#' @export
netis_adjacency <- function(x, variable, fill = 0, drop = TRUE, presence = FALSE,
                            wave = NULL) {
  .check_netis_data(x)
  .assert_flag(presence, "presence")
  s <- netis_schema(x)
  ties <- netis_ties(x)
  nodes <- netis_nodes(x)
  .assert_columns_present(ties, variable, "tie data")
  wave <- .resolve_wave(
    s, wave,
    multi_hint = ", since an adjacency matrix describes a single snapshot. To export every wave, loop over netis_schema(x)$waves."
  )

  descriptor <- s$tie_descriptors[[variable]]
  if (presence) {
    if (is.null(descriptor)) {
      stop(sprintf("No schema descriptor found for tie variable '%s'.", variable), call. = FALSE)
    }
    # Checked once, up front, rather than only inside the per-group loop:
    # determinacy is a property of the tie variable's declared presence_mode,
    # not of whether any particular group happens to have rows, so a group
    # with zero tie rows must not silently skip the same refusal every other
    # group gets.
    .resolve_tie_presence(descriptor, ties[0L, , drop = FALSE], variable)
  }

  # Groups come from NODE data, not tie data: a group with nodes but no ties
  # at all is legal (the group-size diagnostic flags it as a WARNING,
  # not an ERROR) and must still produce an all-`fill` matrix, not vanish
  # from the export silently.
  groups <- sort(unique(nodes[[s$group_col]]))
  mats <- lapply(groups, function(g) {
    # Ties are filtered by group AND wave, via the same helper every other
    # snapshot consumer uses. The node roster deliberately is not filtered by
    # wave: the matrix should keep one stable set of rows and columns across
    # waves, so that two waves of the same group are conformable. A node with
    # no rows at this wave is an isolate in this snapshot, not an actor who
    # ceases to exist.
    sub <- .filter_group_wave(ties, s, as.character(g), wave)
    ids <- sort(unique(nodes[[s$node_col]][nodes[[s$group_col]] == g]))
    fill_value <- if (presence) 0 else fill
    m <- matrix(fill_value, nrow = length(ids), ncol = length(ids), dimnames = list(ids, ids))
    if (nrow(sub) > 0L) {
      .assert_tie_endpoints_on_roster(sub, s, as.character(g), as.character(ids))
      cell_value <- if (presence) as.numeric(.resolve_tie_presence(descriptor, sub, variable)) else sub[[variable]]
      m <- .place_declared(m, as.character(sub[[s$source_col]]),
                           as.character(sub[[s$target_col]]), cell_value,
                           isTRUE(s$directed))
    }
    m
  })
  names(mats) <- as.character(groups)
  .warn_unobserved_export(
    .unobserved_network_labels(ties, s, as.character(groups), wave),
    "adjacency matrix"
  )

  if (drop && length(mats) == 1L) mats[[1]] else mats
}

#' @noRd
.maybe_tibble <- function(df, as) {
  as <- match.arg(as, c("data.frame", "tibble"))
  if (as == "data.frame") {
    return(df)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop(
      "as = \"tibble\" requires the tibble package. Install it with install.packages(\"tibble\"), or use as = \"data.frame\".",
      call. = FALSE
    )
  }
  tibble::as_tibble(df)
}
