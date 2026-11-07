# Node-coordinate resolution for network displays.
#
# The governing constraint is that the DEFAULT must not depend on which
# optional packages happen to be installed. A layout silently swapped for a
# different algorithm makes the same network look like a different network on
# two machines, and rnetis does not transform anything silently. So: native deterministic layouts are
# the defaults, igraph algorithms are named explicitly, and an explicitly named
# one that is unavailable ERRORS rather than degrading.
#
# igraph is used for coordinate generation only. It never draws -- ggplot2 draws
# the network from the node and edge tables this file feeds.

.NETIS_NATIVE_LAYOUTS <- c("circle", "grid", "mds")

#' Resolve node coordinates
#'
#' @param ids Character vector of node identifiers, already in the order the
#' caller wants them laid out. Sorted as character upstream, per the package's
#' ID convention ("10" precedes "2"); this function does not re-sort, because a
#' layout that quietly reorders nodes breaks the caller's row alignment.
#' @param
#' edges Two-column data frame (`from`, `to`) of resolved edge endpoints.
#' **Already direction-resolved** by the semantic layer -- this function never
#' consults edge semantics and must never be given raw canonical `W` endpoints.
#' @param method One of `.NETIS_NATIVE_LAYOUTS`, or an igraph layout name.
#' @param seed seed for a stochastic (igraph) layout, or `NULL` to draw from
#' the session's random stream. A seeded layout restores the caller's stream
#' on exit, so drawing a plot never changes what the caller's next random
#' draw is. Native layouts are deterministic and ignore it.
#'
#' @return A data frame with `node`, `x`, `y`, carrying a `layout_method`
#' attribute recording what actually ran. The record is the point: it makes a
#' drawing auditable after the fact, the same argument `resolved_exposures`
#' makes for exposure decisions.
#' @noRd
.netis_layout <- function(ids, edges = NULL, method = "circle", seed = 1L) {
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("`layout` must be a single layout name.", call. = FALSE)
  }
  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) || seed != round(seed))) {
    stop("`seed` must be a single whole number, or NULL to use the session's random stream.",
         call. = FALSE)
  }
  ids <- as.character(ids)
  n <- length(ids)
  if (n == 0L) {
    out <- data.frame(node = character(0), x = numeric(0), y = numeric(0),
                      stringsAsFactors = FALSE)
    attr(out, "layout_method") <- method
    return(out)
  }

  coords <- switch(
    method,
    circle = .netis_layout_circle(n),
    grid   = .netis_layout_grid(n),
    mds    = .netis_layout_mds(ids, edges),
    .netis_layout_igraph(ids, edges, method, seed)
  )

  out <- data.frame(node = ids, x = coords$x, y = coords$y, stringsAsFactors = FALSE)
  attr(out, "layout_method") <- method
  # The seed is recorded only where it was used: native layouts draw nothing.
  attr(out, "layout_seed") <- if (method %in% .NETIS_NATIVE_LAYOUTS) NULL else seed
  # Distance provenance, recorded rather than left to be inferred. It matters
  # most on a WEIGHTED network, where a reader could reasonably assume the
  # picture used the weights and it does not -- see .netis_geodesic() for why
  # unweighted is the correct choice here and why inferring it from the layout
  # name would not be obvious to anyone.
  attr(out, "layout_distance") <- if (identical(method, "mds")) {
    "unweighted_geodesic"
  } else {
    NA_character_
  }
  out
}

#' Points evenly spaced on the unit circle
#'
#' Universal: defined for any node set including isolates, and deterministic, so
#' the default plot is byte-identical across machines and sessions.
#' @noRd
.netis_layout_circle <- function(n) {
  if (n == 1L) return(list(x = 0, y = 0))
  # Start at the top and go clockwise so node order reads the way a reader scans.
  theta <- pi / 2 - seq(0, 2 * pi, length.out = n + 1L)[seq_len(n)]
  list(x = cos(theta), y = sin(theta))
}

#' Row-major grid, as square as the node count allows
#' @noRd
.netis_layout_grid <- function(n) {
  ncol <- ceiling(sqrt(n))
  idx <- seq_len(n) - 1L
  # y descends so the first node is top-left, matching reading order rather than
  # plotting order.
  list(x = idx %% ncol, y = -(idx %/% ncol))
}

#' Classical MDS on the geodesic distance matrix
#'
#' Refuses on a disconnected network, for the reason in the body; this is the
#' one native layout that is not universal, and the refusal is deliberate.
#' @noRd
.netis_layout_mds <- function(ids, edges) {
  n <- length(ids)
  if (n <= 2L) {
    # Two nodes have nothing for MDS to arrange, connected or not, and the
    # degenerate solve is not worth the special-casing downstream.
    return(.netis_layout_circle(n))
  }

  d <- .netis_geodesic(ids, edges)

  # THE FINITENESS RULE.
  #
  # A disconnected network has infinite between-component distances and classical
  # MDS is undefined on a matrix containing Inf. Because this layer preserves
  # isolates -- and an isolate IS a disconnected component of size one -- this is
  # the ordinary case for real data, not an edge case.
  #
  # The tempting fix is to replace Inf with 2 * max(finite) or similar. It is
  # forbidden: that constant DETERMINES how far apart the components are drawn, so
  # a value chosen for computational convenience becomes a visual claim about the
  # distance between two unconnected parts of a network. Refusing costs the user
  # one argument change; filling costs them a picture that asserts something
  # nobody measured.
  if (any(!is.finite(d))) {
    comps <- .netis_components(ids, edges)
    n_comp <- length(unique(comps))
    n_iso <- sum(tabulate(match(comps, unique(comps))) == 1L)
    stop(
      sprintf(
        paste0(
          "layout = \"mds\" needs a finite distance matrix, and this network is disconnected: ",
          "%d components%s, so some node pairs have infinite geodesic distance.\n",
          "rnetis will not substitute a finite value for an infinite distance -- the ",
          "substituted constant would decide how far apart the components appear, making a ",
          "computational convenience into a visual claim about the data.\n",
          "Use layout = \"circle\" (the default) or layout = \"grid\", both of which are ",
          "defined for any network."
        ),
        n_comp,
        if (n_iso > 0L) sprintf(" including %d isolate(s)", n_iso) else ""
      ),
      call. = FALSE
    )
  }

  fit <- stats::cmdscale(d, k = 2L)
  # cmdscale can return fewer than k columns when the configuration is degenerate
  # (every node mutually equidistant, say). Pad rather than fail: the distances
  # were finite, so the user's request was well-posed and a one-dimensional answer
  # is a real answer.
  if (is.null(dim(fit)) || ncol(fit) < 2L) {
    fit <- cbind(as.numeric(fit), 0)
  }
  list(x = fit[, 1], y = fit[, 2])
}

#' Unweighted geodesic distances by breadth-first search
#'
#' Unweighted deliberately. A weighted shortest path reads tie weight as
#' distance, and a strength-like weight means the opposite -- a stronger tie is a
#' SHORTER path, not a longer one. Rather than resolve `weight_semantics` here and risk getting it
#' backwards, the layout treats every present edge as one step. A layout is a
#' picture of structure, not a measurement, so this costs nothing that matters.
#'
#' @return An `n x n` matrix of hop counts, `Inf` between components.
#' @noRd
.netis_geodesic <- function(ids, edges) {
  n <- length(ids)
  adj <- .netis_adjacency_list(ids, edges)

  d <- matrix(Inf, n, n, dimnames = list(ids, ids))
  diag(d) <- 0
  for (s in seq_len(n)) {
    # Plain BFS: unit edge weights make a queue sufficient and a priority queue
    # unnecessary.
    frontier <- s
    depth <- 0L
    seen <- rep(FALSE, n)
    seen[s] <- TRUE
    while (length(frontier) > 0L) {
      depth <- depth + 1L
      nxt <- unique(unlist(adj[frontier], use.names = FALSE))
      nxt <- nxt[!seen[nxt]]
      if (length(nxt) == 0L) break
      d[s, nxt] <- depth
      seen[nxt] <- TRUE
      frontier <- nxt
    }
  }
  d
}

#' Undirected adjacency list over node indices
#'
#' Symmetrized, because a layout places nodes in a plane and a plane has no
#' direction. This is a derived computational structure, never a mutation of
#' the canonical network, and it is used for geometry only -- arrowheads are drawn from the semantic layer's
#' resolved endpoints, not from this.
#' @noRd
.netis_adjacency_list <- function(ids, edges) {
  adj <- vector("list", length(ids))
  adj[] <- list(integer(0))
  if (is.null(edges) || nrow(edges) == 0L) return(adj)

  from <- match(as.character(edges[[1]]), ids)
  to   <- match(as.character(edges[[2]]), ids)
  keep <- !is.na(from) & !is.na(to) & from != to
  from <- from[keep]; to <- to[keep]

  for (i in seq_along(from)) {
    adj[[from[i]]] <- c(adj[[from[i]]], to[i])
    adj[[to[i]]]   <- c(adj[[to[i]]], from[i])
  }
  lapply(adj, unique)
}

#' Connected-component membership, for the MDS refusal message
#' @noRd
.netis_components <- function(ids, edges) {
  n <- length(ids)
  adj <- .netis_adjacency_list(ids, edges)
  comp <- integer(n)
  k <- 0L
  for (s in seq_len(n)) {
    if (comp[s] != 0L) next
    k <- k + 1L
    stack <- s
    while (length(stack) > 0L) {
      v <- stack[length(stack)]
      stack <- stack[-length(stack)]
      if (comp[v] != 0L) next
      comp[v] <- k
      stack <- c(stack, adj[[v]][comp[adj[[v]]] == 0L])
    }
  }
  comp
}

#' An igraph layout, named explicitly by the caller
#'
#' Never reached by default and never substituted for. If the user asked for `fr`
#' and igraph is absent, they get an error naming the native alternatives -- not a
#' circle layout labeled `fr`.
#'
#' Most igraph layouts are stochastic. With a `seed` the layout is drawn from
#' that seed and the caller's random stream is restored afterwards, so the same
#' call gives the same picture and leaves the session's next draw unchanged.
#' @noRd
.netis_layout_igraph <- function(ids, edges, method, seed = 1L) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop(
      sprintf(
        paste0(
          "layout = \"%s\" is an igraph layout and the igraph package is not installed.\n",
          "Install igraph, or use one of rnetis's native layouts: %s.\n",
          "rnetis does not silently substitute a different algorithm for the one you asked ",
          "for -- the same network would then look like a different network depending on ",
          "which packages happen to be installed."
        ),
        method, paste(.NETIS_NATIVE_LAYOUTS, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  fn_name <- paste0("layout_with_", method)
  fn <- tryCatch(getExportedValue("igraph", fn_name), error = function(e) NULL)
  if (is.null(fn)) {
    stop(
      sprintf(
        "Unknown layout \"%s\": it is neither a rnetis native layout (%s) nor an igraph `%s()`.",
        method, paste(.NETIS_NATIVE_LAYOUTS, collapse = ", "), fn_name
      ),
      call. = FALSE
    )
  }

  el <- if (is.null(edges) || nrow(edges) == 0L) {
    matrix(character(0), ncol = 2L)
  } else {
    cbind(as.character(edges[[1]]), as.character(edges[[2]]))
  }
  g <- igraph::graph_from_data_frame(
    d = as.data.frame(el, stringsAsFactors = FALSE),
    directed = FALSE,
    vertices = data.frame(name = ids, stringsAsFactors = FALSE)
  )
  if (!is.null(seed)) {
    .sim_restore_stream_on_exit(seed, parent = environment())
  }
  xy <- fn(g)
  list(x = xy[, 1], y = xy[, 2])
}
