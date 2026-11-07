# Import: foreign representations into netis_data.
#
# DISPATCH DISCIPLINE IS THE POINT OF THIS FILE.
#
# `as_netis_data()` dispatches on class, and a class earns a method only when it
# identifies an input FAMILY on its own. `matrix`, `list`, and `data.frame` do
# not:
#
#   a two-column matrix   -> an edgelist, or a 2x2 adjacency matrix
#   a plain list          -> a group->wave adjacency series, a list of graphs,
#                            or a node/edge table pair
#   a data.frame          -> an edgelist, a node table, or ordinary long data
#
# Dispatching on those and then inspecting shape to work out which was meant
# would be a guess reached through a convenience API. So `list` gets no method at all, `data.frame` gets no method at
# all, and `matrix` gets one that means ADJACENCY, unequivocally: a non-square
# matrix is an error naming the edgelist constructor, never a reinterpretation.
#
# Everything ambiguous is reached through a named constructor. The caller says
# which family they have; rnetis never guesses it.

#' Convert an object to a netis_data
#'
#' The single entry point for turning something rnetis understands into the
#' canonical [netis_data()] structure. It is deliberately **not** a
#' guess-what-you-meant function: it dispatches on class, and only on classes
#' that say unambiguously what kind of input they are.
#'
#' # Why some inputs need a named function instead
#'
#' A two-column matrix is an edgelist or a 2x2 adjacency matrix. A plain list is
#' a group-by-wave adjacency series, a list of graphs, or a node/edge table pair.
#' A data frame is an edgelist, a node table, or ordinary long data. Nothing
#' about those classes says which, and a converter that inspected their shape to
#' decide would be guessing at your data's meaning -- exactly what rnetis refuses
#' to do everywhere else. So each has its own function, named for what it
#' expects:
#'
#' \describe{
#'   \item{edgelist}{[netis_data_from_edgelist()]}
#'   \item{node table + tie table}{[netis_tables()], then this function}
#'   \item{group-by-wave adjacency series}{[netis_data_from_adjacency_series()]}
#'   \item{a single adjacency matrix, with options}{[netis_data_from_adjacency()]}
#'   \item{an igraph graph}{[netis_data_from_igraph()], or this function}
#'   \item{a statnet `network` object}{[netis_data_from_network()], or this function}
#'   \item{a list of igraph graphs}{[netis_data_from_igraph_list()]}
#' }
#'
#' A bare `matrix` **does** get a method, and it means an adjacency matrix. A
#' non-square matrix is an error naming [netis_data_from_edgelist()] rather than
#' being re-read as an edgelist.
#'
#' @param x the object to convert.
#' @param ... passed to the method, and from there to the function it calls:
#'   [netis_data_from_adjacency()] for a matrix, [netis_data()] for a
#'   [netis_tables()] pair, [netis_data_from_igraph()] and
#'   [netis_data_from_network()] for the graph classes.
#' @return a [netis_data()] object.
#' @seealso [netis_boundary()] to see what the conversion declared, inferred, and
#'   left undeclared; [netis_losses()] for what a conversion can lose
#' @examples
#' m <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3,
#'             dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
#' as_netis_data(m)
#'
#' # A shape that does not say what it is gets routed, not guessed at.
#' try(as_netis_data(data.frame(from = "a", to = "b")))
#' @export
as_netis_data <- function(x, ...) {
  UseMethod("as_netis_data")
}

#' @rdname as_netis_data
#' @export
as_netis_data.default <- function(x, ...) {
  stop(
    sprintf(
      paste0(
        "as_netis_data() has no method for an object of class %s.\n",
        "It converts only classes that say unambiguously what they are: an igraph graph, a network object, a netis_tables() pair, or a square adjacency matrix.\n",
        "For a shape whose class does not identify it, name the function that matches your data:\n",
        "  - an edgelist (two columns of node ids)   -> netis_data_from_edgelist()\n",
        "  - a node table plus a tie table           -> netis_tables()\n",
        "  - a group-by-wave series of matrices      -> netis_data_from_adjacency_series()\n",
        "  - one adjacency matrix, with options      -> netis_data_from_adjacency()"
      ),
      paste(sprintf("'%s'", class(x)), collapse = "/")
    ),
    call. = FALSE
  )
}

#' @rdname as_netis_data
#' @param directed `TRUE` (default) or `FALSE`. **Declared, never inferred**:
#'   rnetis will not read directedness off matrix symmetry, because a symmetric
#'   matrix may perfectly well be a directed network whose ties happen to be
#'   reciprocal.
#' @export
as_netis_data.matrix <- function(x, directed = TRUE, ...) {
  # A character matrix is not a plausible adjacency matrix -- adjacency cells
  # are tie weights -- so this is a TYPE check, not a shape guess, and it
  # catches the case the squareness check cannot: a two-column edgelist with
  # exactly two rows is square, and no amount of inspecting dimensions will
  # distinguish it from a 2x2 adjacency matrix. That ambiguity is real and is
  # why this method has one meaning and says so, rather than probing.
  if (!is.numeric(x) && !is.logical(x)) {
    stop(
      sprintf(
        "as_netis_data() reads a matrix as an ADJACENCY matrix, whose cells are tie weights, but this matrix is %s. If it is an edgelist, use netis_data_from_edgelist(), which says so explicitly.",
        typeof(x)
      ),
      call. = FALSE
    )
  }
  if (nrow(x) != ncol(x)) {
    stop(
      sprintf(
        "as_netis_data() reads a matrix as an ADJACENCY matrix, which must be square; this one is %d x %d. If it is an edgelist, use netis_data_from_edgelist(), which says so explicitly.",
        nrow(x), ncol(x)
      ),
      call. = FALSE
    )
  }
  out <- netis_data_from_adjacency(x, directed = directed, ...)
  .record_boundary(out, "as_netis_data", "matrix")
}

#' Pair a node table with a tie table for conversion
#'
#' Two data frames -- one describing nodes, one describing ties -- are a common
#' way to hold a network, and they are also indistinguishable from any other two
#' data frames. `netis_tables()` is how you say which is which, so that
#' [as_netis_data()] can convert them without inspecting their columns and
#' guessing.
#'
#' This exists because the alternative is worse. A converter that took two data
#' frames and worked out which was the node table would be right most of the
#' time and silently wrong on, for example, a dyadic dataset whose node table
#' happens to carry `source` and `target` columns for another purpose.
#'
#' @param nodes a node-level data frame.
#' @param ties a tie-level data frame.
#' @return an object of class `netis_table_pair`, for [as_netis_data()].
#' @seealso [as_netis_data()], [netis_data()] to build the object directly when
#'   you already know your column names
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b", "c"), trait = c(1, 2, 3))
#' ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
#' as_netis_data(netis_tables(nodes, ties))
#' @export
netis_tables <- function(nodes, ties) {
  if (!is.data.frame(nodes)) stop("nodes must be a data frame.", call. = FALSE)
  if (!is.data.frame(ties)) stop("ties must be a data frame.", call. = FALSE)
  structure(list(nodes = nodes, ties = ties), class = "netis_table_pair")
}

#' @rdname as_netis_data
#' @export
as_netis_data.netis_table_pair <- function(x, ...) {
  out <- netis_data(x$nodes, x$ties, ...)
  .record_boundary(out, "as_netis_data", "netis_table_pair")
}

#' Build a netis_data object from an edgelist
#'
#' Converts a two-column edgelist -- each row one tie, naming its source and
#' target -- into a [netis_data()] object.
#'
#' # The roster is required, and this is the reason
#'
#' An edgelist names only the nodes that have ties. **Isolates are invisible in
#' it**, and deriving the node set from the endpoints would silently drop them,
#' changing every degree, centrality, and density value the data supports. rnetis
#' therefore refuses rather than assuming: pass `nodes`.
#'
#' If your data genuinely has no isolates, say so by passing the endpoints
#' yourself -- `nodes = unique(c(edges$from, edges$to))`. That is one line, and
#' it makes the claim yours and recorded, rather than rnetis's and silent.
#'
#' # Direction is declared, not observed
#'
#' `directed` is your declaration. rnetis will not infer it from whether the
#' edgelist happens to be reciprocated, because a fully reciprocated edgelist may
#' perfectly well be a directed network in which everyone reciprocated.
#'
#' @param edges a data frame or matrix of ties. For a data frame, `source_col`
#'   and `target_col` name the endpoint columns and every other column (apart
#'   from `group_col`/`wave_col`) becomes a tie variable. For a matrix, the
#'   first two columns are source and target and any further columns become
#'   tie variables, named by the matrix's column names (or `V3`, `V4`, ...),
#'   with numeric-looking values converted to numbers.
#' @param nodes the node roster: a character/numeric vector of node ids, or a
#'   data frame of node-level data containing `node_col`. Required -- see
#'   Details. When `edges` carries `group_col`, a roster without it (a vector,
#'   or a data frame lacking the column) would put every node in `group`, so
#'   it is an error unless every edge is in that group.
#' @param group the group identifier to assign, when neither `edges` nor `nodes`
#'   carries `group_col`. Default `"1"` (a single network).
#' @param directed `TRUE` (default) or `FALSE`. Declared, never inferred.
#' @param self_ties `"prohibited"` (default) or `"permitted"`.
#' @param source_col,target_col,node_col,group_col,wave_col column names, as in
#'   [netis_data()]. `wave_col` names a wave column present in `edges` (and, if
#'   the roster varies by wave, in `nodes`).
#' @param tie_meta per-network semantics, as in [netis_data()].
#' @param observed_tie_waves (group, wave) snapshots observed to have no ties, as
#'   in [netis_data()]. An edgelist has no row for such a snapshot, so without this
#'   it reads as unobserved.
#' @return a [netis_data()] object.
#' @seealso [as_netis_data()], [netis_data()], [netis_losses()] for the
#'   `isolate_roster` loss this refuses on
#' @examples
#' edges <- data.frame(from = c("a", "b"), to = c("b", "c"), w = c(2, 1))
#'
#' # `d` is an isolate: present in the study, absent from the edgelist.
#' nd <- netis_data_from_edgelist(edges, nodes = c("a", "b", "c", "d"),
#'                                 source_col = "from", target_col = "to")
#' nrow(netis_nodes(nd))
#'
#' # Without a roster rnetis refuses rather than dropping it.
#' try(netis_data_from_edgelist(edges, source_col = "from", target_col = "to"))
#' @export
netis_data_from_edgelist <- function(edges, nodes = NULL, group = "1",
                                      directed = TRUE, self_ties = "prohibited",
                                      source_col = "source", target_col = "target",
                                      node_col = "node", group_col = "group",
                                      wave_col = NULL, tie_meta = NULL,
                                      observed_tie_waves = NULL) {
  .assert_flag(directed, "directed")
  .assert_enum(self_ties, c("prohibited", "permitted"), "self_ties")

  edges <- .edgelist_as_frame(edges, source_col, target_col)
  .assert_columns_present(edges, c(source_col, target_col), "edges")

  if (is.null(nodes)) {
    # Not a warning. Dropping isolates changes every structural quantity the
    # data supports, which is a semantic loss. It is also not overridable: authorizing it would leave rnetis
    # unable to say which nodes the study contains.
    .loss_raise("isolate_roster", detail = "An edgelist names only nodes that have ties.")
  }

  roster_has_group <- is.data.frame(nodes) && group_col %in% names(nodes)
  node_data <- .edgelist_roster(nodes, node_col, group_col, group)
  if (!(group_col %in% names(edges))) {
    edges[[group_col]] <- rep(group, nrow(edges))
  } else if (!roster_has_group) {
    other <- setdiff(unique(as.character(edges[[group_col]])), as.character(group))
    if (length(other) > 0L) {
      stop(
        sprintf(
          "edges carries a '%s' column (groups: %s) but the node roster does not, so every node would be placed in group '%s' and the ties in other groups would have no nodes. Supply nodes as a data frame with a '%s' column.",
          group_col, paste(utils::head(unique(as.character(edges[[group_col]])), 5), collapse = ", "),
          group, group_col
        ),
        call. = FALSE
      )
    }
  }

  out <- netis_data(
    node_data, edges,
    group_col = group_col, node_col = node_col, wave_col = wave_col,
    source_col = source_col, target_col = target_col,
    directed = directed, self_ties = self_ties, tie_meta = tie_meta,
    observed_tie_waves = observed_tie_waves
  )
  .record_boundary(out, "netis_data_from_edgelist", "edgelist")
}

#' Normalize an edgelist argument to a data frame
#'
#' A matrix is accepted here specifically because the caller has already said, by
#' choosing this function, that they have an edgelist -- the ambiguity is in the
#' *class*, and naming the function resolves it.
#'
#' @noRd
.edgelist_as_frame <- function(edges, source_col, target_col) {
  if (is.matrix(edges)) {
    if (ncol(edges) < 2L) {
      stop(
        sprintf("An edgelist matrix needs at least 2 columns (source, target); this one has %d.", ncol(edges)),
        call. = FALSE
      )
    }
    out <- data.frame(
      a = as.character(edges[, 1L]), b = as.character(edges[, 2L]),
      stringsAsFactors = FALSE
    )
    names(out) <- c(source_col, target_col)
    # Further columns are tie variables, as documented, not dropped.
    if (ncol(edges) > 2L) {
      extra_names <- colnames(edges)
      if (is.null(extra_names)) extra_names <- paste0("V", seq_len(ncol(edges)))
      extra_names <- extra_names[-(1:2)]
      if (any(!nzchar(extra_names) | is.na(extra_names)) ||
          anyDuplicated(c(source_col, target_col, extra_names))) {
        stop(
          "An edgelist matrix's columns beyond the first two become tie variables and need distinct, non-empty column names that differ from the source and target columns.",
          call. = FALSE
        )
      }
      for (k in seq_along(extra_names)) {
        v <- edges[, k + 2L]
        out[[extra_names[k]]] <- if (is.character(v)) utils::type.convert(v, as.is = TRUE) else v
      }
    }
    return(out)
  }
  if (!is.data.frame(edges)) {
    stop(
      sprintf("edges must be a data frame or a matrix with at least two columns; got %s.", paste(class(edges), collapse = "/")),
      call. = FALSE
    )
  }
  .as_plain_data_frame(edges)
}

#' Build the node roster from a vector or data frame
#'
#' @noRd
.edgelist_roster <- function(nodes, node_col, group_col, group) {
  if (is.data.frame(nodes)) {
    nd <- .as_plain_data_frame(nodes)
    .assert_columns_present(nd, node_col, "nodes")
    if (!(group_col %in% names(nd))) {
      nd[[group_col]] <- group
    }
    return(nd)
  }
  if (!is.atomic(nodes) || length(nodes) == 0L) {
    stop(
      "nodes must be a vector of node ids or a data frame of node data containing the node column.",
      call. = FALSE
    )
  }
  out <- data.frame(x = group, y = as.character(nodes), stringsAsFactors = FALSE)
  names(out) <- c(group_col, node_col)
  out
}
