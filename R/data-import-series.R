# Wave-series adjacency import, and split static/dynamic input.
#
# `netis_data_from_adjacency()` handles one matrix per group. Reshaping waved
# matrices to long tie data by hand is fiddly and easy to get wrong in ways that
# are invisible afterwards -- most obviously by losing the distinction between a
# node that is an isolate at a wave and one that is not in the roster at that
# wave.
#
# WHY A NAMED CONSTRUCTOR RATHER THAN AN as_netis_data() METHOD FOR `list`.
# A plain list could be this, a list of igraph graphs, or a node/edge table
# pair, and a length/class probe that picked one would be a guess.
# Once the caller has named THIS function, the layouts below are
# an unambiguous format rather than a family to be guessed at, and telling them
# apart by element type is parsing, not inference.

#' Build a netis_data object from a series of adjacency matrices
#'
#' Converts a group-by-wave collection of adjacency matrices into a
#' [netis_data()] object. [netis_data_from_adjacency()] handles one snapshot per
#' group.
#'
#' # Accepted layouts
#'
#' \describe{
#'   \item{nested list}{`list(<group> = list(<wave> = M, ...), ...)` -- the
#'     general case. Inner names become wave labels; outer names become groups.}
#'   \item{flat list}{`list(<wave> = M, ...)` -- one group, named by `group`.}
#'   \item{3-D array}{`M[, , k]` is wave `k`, one group.}
#'   \item{static networks}{with `static = TRUE`, each named element is one
#'     **group's** time-invariant network, reused at every wave in `waves`
#'     rather than duplicated. Static networks are first-class in rnetis and do
#'     not require repeated rows.}
#' }
#'
#' `static` is an argument rather than something inferred from the shape,
#' because the shape cannot say: `list(A = M)` is equally "group A's static
#' network" and "wave A of one group", and picking one by branch order would
#' make the answer depend on an implementation detail. It applies to the flat
#' list; a nested list and a 3-D array already say which matrix is which wave.
#'
#' Wave labels come from the inner names, or from `waves` when the layout has
#' none. They are **labels, not times**: rnetis treats waves as ordered
#' occasions, so labels `1, 3, 5` are three consecutive occasions and no
#' spacing is implied. Their order is the labels' own: numeric `waves` (or
#' names that all read as numbers, such as `"9"`, `"10"`) are ordered by value;
#' other text labels are ordered as given, first group first. To record actual observation times, pass `time_meta` --
#' it is retained as metadata and changes no estimate. See [netis_time_index()].
#'
#' # Rosters
#'
#' The node roster is the union of every matrix's dimnames unless `nodes` is
#' given. A node absent from one wave's matrix but present in another is an
#' **isolate at that wave**, not a node that left the study; pass `nodes` if you
#' need a roster that differs from the union.
#'
#' @param adjacency a nested list, flat list, or 3-D array -- see Details.
#' @param nodes optional node roster (vector of ids, or a data frame as in
#'   [netis_data()]). Defaults to the union of the matrices' dimnames. A roster
#'   with no group column puts every node in `group`, so with several groups it
#'   is an error unless it says which group each node is in.
#' @param waves wave labels, required when the layout does not name them (a 3-D
#'   array, or a list without names) and whenever `static = TRUE`.
#' @param static if `TRUE`, each element of `adjacency` is one group's
#'   time-invariant network, reused at every wave in `waves`. Default `FALSE`.
#' @param group group identifier for the single-group layouts. Default `"1"`.
#' @param value_name name for the tie-weight column. Default `"weight"`.
#' @param directed `TRUE` (default) or `FALSE`. **Declared, never inferred** from
#'   matrix symmetry.
#' @param drop_zero if `TRUE` (default), zero cells are treated as absent ties
#'   rather than stored as explicit zero-weight ties, an `NA` cell is an error,
#'   and the network's `zero_meaning` is recorded accordingly -- exactly as in
#'   [netis_data_from_adjacency()]. Either way a matrix is an observed network:
#'   an all-zero matrix is a network observed to be empty at that wave, not an
#'   unobserved one (see Empty and unobserved waves on [netis_data()]). Leave a
#'   wave out of the series to say it was not observed.
#' @param self_ties `"prohibited"` (default) or `"permitted"`.
#' @param time_meta,time_unit optional metric observation times, as in
#'   [netis_data()].
#' @param group_col,node_col,wave_col,source_col,target_col column names for the
#'   constructed data.
#' @return a [netis_data()] object.
#' @seealso [netis_data_from_adjacency()] for a single snapshot per group,
#'   [as_netis_data()], [netis_time_index()]
#' @examples
#' w1 <- matrix(c(0, 1, 0, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
#' w2 <- matrix(c(0, 1, 1, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
#'
#' nd <- netis_data_from_adjacency_series(list(`1` = w1, `2` = w2))
#' netis_ties(nd)
#'
#' # Two groups, each with its own two waves.
#' netis_data_from_adjacency_series(
#'   list(A = list(`1` = w1, `2` = w2), B = list(`1` = w2, `2` = w2))
#' )
#' @export
netis_data_from_adjacency_series <- function(adjacency, nodes = NULL, waves = NULL,
                                              static = FALSE,
                                              group = "1", value_name = "weight",
                                              directed = TRUE, drop_zero = TRUE,
                                              self_ties = "prohibited",
                                              time_meta = NULL, time_unit = NULL,
                                              group_col = "group", node_col = "node",
                                              wave_col = "wave",
                                              source_col = "source",
                                              target_col = "target") {
  .assert_flag(directed, "directed")
  .assert_flag(drop_zero, "drop_zero")
  .assert_enum(self_ties, c("prohibited", "permitted"), "self_ties")
  .assert_flag(static, "static")

  layout <- .series_normalize(adjacency, waves, group, static)

  # Wave labels are list names here, so text. The stored wave column takes the
  # type that keeps their order: the caller's own `waves` values when given
  # (numbers stay numbers), and otherwise the names, read as numbers when they
  # all are and as a factor in the order given when they are not. Storing the
  # names as text would sort waves 9, 10 as "10", "9".
  wave_labels <- unique(unlist(lapply(layout, names), use.names = FALSE))
  wave_values <- if (!is.null(waves)) {
    .import_wave_values(waves)[match(wave_labels, as.character(waves))]
  } else {
    .import_wave_values(wave_labels)
  }

  tie_pieces <- list()
  roster <- list()
  # Every matrix in the series is an observed network, whatever it holds.
  observed <- list()
  for (g in names(layout)) {
    for (w in names(layout[[g]])) {
      m <- layout[[g]][[w]]
      piece <- .adjacency_to_ties(
        m, g,
        value_name = value_name, directed = directed, drop_zero = drop_zero,
        self_ties = self_ties,
        cols = list(group = group_col, source = source_col, target = target_col)
      )
      piece[[wave_col]] <- rep(wave_values[match(w, wave_labels)], nrow(piece))
      tie_pieces[[length(tie_pieces) + 1L]] <- piece
      observed[[length(observed) + 1L]] <- stats::setNames(
        data.frame(g, wave_values[match(w, wave_labels)], stringsAsFactors = FALSE),
        c(group_col, wave_col)
      )
      roster[[length(roster) + 1L]] <- data.frame(
        g = g, n = .matrix_node_ids(m), stringsAsFactors = FALSE
      )
    }
  }
  tie_data <- do.call(rbind, tie_pieces)
  rownames(tie_data) <- NULL

  node_data <- if (is.null(nodes)) {
    # Union across waves, so a node that is an isolate at one wave keeps its
    # place in the roster instead of vanishing from that snapshot.
    r <- unique(do.call(rbind, roster))
    out <- data.frame(x = r$g, y = r$n, stringsAsFactors = FALSE)
    names(out) <- c(group_col, node_col)
    out[order(out[[group_col]], out[[node_col]]), , drop = FALSE]
  } else {
    # A roster that does not say which group a node is in places every node in
    # `group`; the ties in any other group would then have no nodes, and every
    # exposure there would be missing from the output with nothing reported.
    roster_has_group <- is.data.frame(nodes) && group_col %in% names(nodes)
    other <- setdiff(names(layout), as.character(group))
    if (!roster_has_group && length(other) > 0L) {
      stop(
        sprintf(
          "The series has groups %s but the node roster has no '%s' column, so every node would be placed in group '%s' and the ties in the other groups would have no nodes. Supply nodes as a data frame with a '%s' column.",
          paste(utils::head(names(layout), 5), collapse = ", "), group_col, group, group_col
        ),
        call. = FALSE
      )
    }
    .edgelist_roster(nodes, node_col, group_col, group)
  }
  rownames(node_data) <- NULL

  out <- netis_data(
    node_data, tie_data,
    group_col = group_col, node_col = node_col, wave_col = wave_col,
    source_col = source_col, target_col = target_col,
    directed = directed, self_ties = self_ties,
    tie_meta = stats::setNames(
      list(list(zero_meaning = if (drop_zero) "structural_absence" else "measured_zero")),
      value_name
    ),
    time_meta = time_meta, time_unit = time_unit,
    observed_tie_waves = do.call(rbind, observed)
  )
  .record_boundary(out, "netis_data_from_adjacency_series", "adjacency series")
}

#' Normalize the accepted layouts into `list(group = list(wave = matrix))`
#'
#' Element TYPE distinguishes the two list layouts -- a list of lists is
#' grouped, a list of matrices is one group's wave series -- and `static` says
#' which of the two readings a flat named list carries, because that one the
#' shape genuinely cannot answer. This is format parsing within a function the
#' caller named, not a guess at what family of input this is: nothing here
#' decides whether the input was an edgelist or an adjacency series, only how
#' this adjacency series is laid out.
#'
#' @noRd
.series_normalize <- function(adjacency, waves, group, static = FALSE) {
  if (is.array(adjacency) && length(dim(adjacency)) == 3L) {
    d <- dim(adjacency)
    if (d[1L] != d[2L]) {
      stop(
        sprintf("A 3-D adjacency array must be square in its first two dimensions; got %d x %d x %d.", d[1L], d[2L], d[3L]),
        call. = FALSE
      )
    }
    labels <- .series_wave_labels(dimnames(adjacency)[[3L]], waves, d[3L], "the array's third dimension")
    # array() rather than `adjacency[, , k]`: dropping the slice's dimensions
    # turns a 1 x 1 slice into a bare number.
    slices <- lapply(seq_len(d[3L]), function(k) {
      array(adjacency[, , k], dim = d[1:2], dimnames = dimnames(adjacency)[1:2])
    })
    names(slices) <- labels
    return(stats::setNames(list(slices), as.character(group)))
  }

  if (is.matrix(adjacency)) {
    stop(
      "netis_data_from_adjacency_series() expects a series; a single matrix is one snapshot. Use netis_data_from_adjacency() for that, or wrap it in a list to say which wave it is.",
      call. = FALSE
    )
  }
  if (!is.list(adjacency) || length(adjacency) == 0L) {
    stop(
      sprintf("adjacency must be a nested list, a list of matrices, or a 3-D array; got %s.", paste(class(adjacency), collapse = "/")),
      call. = FALSE
    )
  }

  if (all(vapply(adjacency, is.matrix, logical(1)))) {
    if (static) {
      # Each element is a GROUP's time-invariant network, reused at every wave.
      if (is.null(waves)) {
        stop(
          "static = TRUE declares one network per group reused across waves, so `waves` must say which waves those are.",
          call. = FALSE
        )
      }
      if (is.null(names(adjacency)) || any(!nzchar(names(adjacency)))) {
        stop("With static = TRUE the list must be named; the names become group identifiers.", call. = FALSE)
      }
      return(lapply(adjacency, function(m) {
        stats::setNames(rep(list(m), length(waves)), as.character(waves))
      }))
    }
    labels <- .series_wave_labels(names(adjacency), waves, length(adjacency), "the list")
    slices <- stats::setNames(adjacency, labels)
    return(stats::setNames(list(slices), as.character(group)))
  }

  if (is.null(names(adjacency)) || any(!nzchar(names(adjacency)))) {
    stop(
      "A grouped adjacency series must be a named list; the names become group identifiers.",
      call. = FALSE
    )
  }
  lapply(adjacency, function(entry) {
    if (is.matrix(entry)) {
      stop(
        "A group's entry is a single matrix. If that network is time-invariant, say so with static = TRUE and supply `waves`; rnetis will not infer a static network from the shape, since the same shape is also one group with one wave.",
        call. = FALSE
      )
    }
    if (!is.list(entry) || !all(vapply(entry, is.matrix, logical(1)))) {
      stop(
        "Each group's entry must be a list of adjacency matrices (one per wave), or a single matrix for a static network.",
        call. = FALSE
      )
    }
    stats::setNames(entry, .series_wave_labels(names(entry), waves, length(entry), "a group's list"))
  })
}

#' @noRd
.series_wave_labels <- function(nms, waves, n, where) {
  if (!is.null(waves)) {
    if (length(waves) != n) {
      stop(
        sprintf("waves has %d label(s) but %s has %d wave(s).", length(waves), where, n),
        call. = FALSE
      )
    }
    return(as.character(waves))
  }
  if (is.null(nms) || any(!nzchar(nms))) {
    stop(
      sprintf(
        "%s does not name its waves, so `waves` must supply the labels. rnetis will not number them itself: a wave label is an occasion identifier you chose, not a position rnetis should invent.",
        where
      ),
      call. = FALSE
    )
  }
  as.character(nms)
}

#' Build a netis_data object from separate static and dynamic tables
#'
#' Some pipelines hold node or tie data already split into a time-invariant part
#' and a time-varying part -- one row per node for the things that never change,
#' one row per node-wave for the things that do. `netis_data()` wants a single
#' long table per level; this joins them and calls it.
#'
#' rnetis stores data this way internally, so the split is a natural input shape
#' rather than an odd one. The join is on the entity keys, and a column present
#' in both tables is an error rather than a silent preference for one -- rnetis
#' does not know which copy you meant. An entity that appears in a static
#' table but has no row in the matching dynamic table is also an error: it has
#' no wave at which it was observed, and joining would drop it. An entity with
#' dynamic rows and no static row is kept, with its static variables missing.
#'
#' @param node_static one row per node (per group). May be `NULL`.
#' @param node_dynamic one row per node-wave. May be `NULL`.
#' @param tie_static one row per tie (per group). May be `NULL`.
#' @param tie_dynamic one row per tie-wave. May be `NULL`.
#' @param group_static,group_dynamic the same, at group level. May be `NULL`.
#' @param ... passed to [netis_data()], including `wave_col`, which is required
#'   whenever any dynamic table is supplied.
#' @return a [netis_data()] object.
#' @seealso [netis_data()]
#' @examples
#' node_static <- data.frame(group = 1, node = c("a", "b"), sex = c("f", "m"))
#' node_dynamic <- data.frame(
#'   group = 1, node = rep(c("a", "b"), each = 2),
#'   wave = rep(1:2, times = 2), y = c(1, 2, 3, 4)
#' )
#' tie_dynamic <- data.frame(
#'   group = 1, source = "a", target = "b", wave = 1:2, w = c(1, 2)
#' )
#' nd <- netis_data_from_split(
#'   node_static = node_static, node_dynamic = node_dynamic,
#'   tie_dynamic = tie_dynamic, wave_col = "wave"
#' )
#' netis_nodes(nd)
#' @export
netis_data_from_split <- function(node_static = NULL, node_dynamic = NULL,
                                   tie_static = NULL, tie_dynamic = NULL,
                                   group_static = NULL, group_dynamic = NULL,
                                   ...) {
  args <- list(...)
  group_col <- args$group_col %||% "group"
  node_col <- args$node_col %||% "node"
  source_col <- args$source_col %||% "source"
  target_col <- args$target_col %||% "target"
  wave_col <- args$wave_col

  node_data <- .join_split(node_static, node_dynamic, c(group_col, node_col), wave_col, "node")
  tie_data <- .join_split(tie_static, tie_dynamic, c(group_col, source_col, target_col), wave_col, "tie")
  group_data <- .join_split(group_static, group_dynamic, group_col, wave_col, "group")

  if (is.null(node_data) || is.null(tie_data)) {
    stop(
      "netis_data_from_split() needs node data and tie data: supply at least one of node_static/node_dynamic, and at least one of tie_static/tie_dynamic.",
      call. = FALSE
    )
  }

  out <- do.call(netis_data, c(
    list(node_data = node_data, tie_data = tie_data, group_data = group_data), args
  ))
  .record_boundary(out, "netis_data_from_split", "static/dynamic split tables")
}

#' @noRd
.join_split <- function(static, dynamic, key_cols, wave_col, level) {
  static <- .as_plain_data_frame(static)
  dynamic <- .as_plain_data_frame(dynamic)
  if (is.null(static) && is.null(dynamic)) {
    return(NULL)
  }
  if (is.null(dynamic)) {
    return(static)
  }
  if (is.null(wave_col)) {
    stop(
      sprintf("A dynamic %s table varies by wave, so wave_col must be supplied.", level),
      call. = FALSE
    )
  }
  .assert_columns_present(dynamic, c(key_cols, wave_col), sprintf("%s_dynamic", level))
  if (is.null(static)) {
    return(dynamic)
  }
  .assert_columns_present(static, key_cols, sprintf("%s_static", level))

  overlap <- setdiff(intersect(names(static), names(dynamic)), key_cols)
  if (length(overlap) > 0L) {
    stop(
      sprintf(
        "Column(s) %s appear in both the static and dynamic %s tables. rnetis will not choose between two values for one variable -- keep each variable in exactly one of the two.",
        paste(overlap, collapse = ", "), level
      ),
      call. = FALSE
    )
  }
  # An entity in the static table with no row in the dynamic table has no wave
  # at which it was observed, so it has no place in the long table. Dropping
  # it would remove a node (or group) from the study with nothing reported.
  skey <- do.call(paste, c(lapply(static[key_cols], as.character), sep = .NETIS_KEY_SEP))
  dkey <- do.call(paste, c(lapply(dynamic[key_cols], as.character), sep = .NETIS_KEY_SEP))
  static_only <- !(skey %in% dkey)
  if (any(static_only)) {
    shown <- do.call(paste, c(lapply(static[static_only, key_cols, drop = FALSE], as.character),
                              sep = "/"))
    stop(
      sprintf(
        "%d entit%s in the static %s table ha%s no row in the dynamic %s table, so %s no wave at which %s observed: %s. Add the dynamic rows (with missing values where nothing was measured), or remove %s from the static table.",
        sum(static_only), if (sum(static_only) == 1L) "y" else "ies", level,
        if (sum(static_only) == 1L) "s" else "ve", level,
        if (sum(static_only) == 1L) "it has" else "they have",
        if (sum(static_only) == 1L) "it was" else "they were",
        paste(utils::head(shown, 5), collapse = ", "),
        if (sum(static_only) == 1L) "it" else "them"
      ),
      call. = FALSE
    )
  }
  merge(static, dynamic, by = key_cols, all.y = TRUE, sort = FALSE)
}
