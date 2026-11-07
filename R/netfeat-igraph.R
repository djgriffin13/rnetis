# igraph-backed network-feature metrics. Betweenness, closeness, eigenvector
# centrality, PageRank, and constraint reuse igraph's own mature
# implementations rather than reimplementing path/spectral algorithms --
# reimplementing them would buy nothing and carry real statistical and
# maintenance risk. rnetis owns
# data alignment, direction resolution, and weight-semantics validation; igraph
# owns the algorithm.
#
# Every function here builds its own igraph graph directly from the already-
# resolved declared-orientation matrix (.netfeat_build_igraph(), below)
# rather than accepting one from the caller, so direction and weight
# handling stay in exactly one place for every igraph-backed metric.

#' @noRd
.netfeat_igraph_guard <- function(measure) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop(
      sprintf("measure = \"%s\" requires the 'igraph' package. Install it with install.packages(\"igraph\").", measure),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' A weight transform: reciprocal (`w -> 1/w`)
#'
#' The standard conversion between strengths and distances: a stronger tie is a
#' shorter distance, and a shorter distance a stronger tie. It is the one weight
#' transform rnetis provides. Undefined at `w = 0` (division by zero) -- applied
#' only to the present (nonzero) cells of a resolved network, so zero-weight cells,
#' which represent no tie, are never transformed. Requires strictly positive
#' input; a negative weight has no principled "distance" reading and is
#' refused rather than silently producing a negative distance.
#'
#' @return an object of class `netis_weight_transform`.
#' @seealso [centrality()], [network_stat()]
#' @references
#' Opsahl, T., Agneessens, F. and Skvoretz, J. (2010) Node centrality in weighted
#' networks: generalizing degree and shortest paths. *Social Networks* **32**(3),
#' 245-251.
#' @examples
#' inverse()
#' # A "strength"-declared network (larger value = stronger tie) fed to a
#' # distance-based measure like betweenness needs this transform, or the
#' # weight-semantics guardrail refuses to run:
#' # Suggests dependency: igraph, which the "betweenness" measure requires.
#' \donttest{
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   nodes <- data.frame(group = 1, node = c("a", "b", "c"))
#'   ties <- data.frame(group = 1, source = c("a", "b"),
#'                      target = c("b", "c"), w = c(2, 4))
#'   nd <- netis_data(nodes, ties,
#'                    tie_meta = list(w = list(weight_semantics = "strength")))
#'   compute_centrality(nd, w, measure = "betweenness", weights = "weighted",
#'                      weight_transform = inverse())
#' }
#' }
#' @export
inverse <- function() {
  structure(list(type = "inverse"), class = "netis_weight_transform")
}

#' @noRd
.apply_weight_transform_matrix <- function(M, transform) {
  if (!inherits(transform, "netis_weight_transform")) {
    stop("weight_transform must be a netis_weight_transform, e.g. inverse().", call. = FALSE)
  }
  present <- !is.na(M) & M != 0
  if (identical(transform$type, "inverse")) {
    if (any(M[present] <= 0)) {
      stop(
        "weight_transform = inverse() requires strictly positive edge weights; found a non-positive value. A negative or zero weight has no principled 1/w distance reading.",
        call. = FALSE
      )
    }
    M[present] <- 1 / M[present]
    return(M)
  }
  stop(sprintf("Unknown weight_transform type '%s'.", transform$type), call. = FALSE) # nocov
}

#' The declared meaning of a resolved network's weights
#' @noRd
.netfeat_network_semantics <- function(resolved_net) {
  if (!is.null(resolved_net$descriptor)) {
    resolved_net$descriptor$weight_semantics
  } else {
    resolved_net$weight_semantics
  }
}

#' The weight-semantics guard, in both directions
#'
#' A metric reads a larger weight either as a shorter distance or as a stronger
#' tie. A network whose declared weights mean the other thing needs an explicit,
#' recorded transform: strength-, signed- or count-declared weights into a metric
#' reading distances, and distance-declared weights into a metric reading
#' strengths. `"unitless"` (the undeclared default) makes no claim either way and
#' is not flagged: the mismatch this guards against is a *declared* one.
#'
#' @param network_semantics the network's declared `weight_semantics`.
#' @param metric_semantics what the metric reads a weight as (`"distance"`,
#'   `"strength"`, or `NA`).
#' @param weight_transform the requested transform, or `NULL`.
#' @noRd
.assert_weight_semantics <- function(network_semantics, metric_semantics, weight_transform) {
  if (!is.null(weight_transform) || is.null(network_semantics)) {
    return(invisible(TRUE))
  }
  if (identical(metric_semantics, "distance") &&
      network_semantics %in% c("strength", "signed", "count")) {
    stop(
      sprintf(
        "This measure interprets edge weights as distances, but the network's weights are declared as \"%s\" (%s). Supply weight_transform = inverse(), or request weights = \"unweighted\".",
        network_semantics, "a larger value would be read as a shorter distance without one"
      ),
      call. = FALSE
    )
  }
  if (identical(metric_semantics, "strength") && identical(network_semantics, "distance")) {
    stop(
      "This measure interprets edge weights as tie strengths, but the network's weights are declared as distances (a larger value would be read as a stronger tie, when it means a longer distance). Supply weight_transform = inverse(), or request weights = \"unweighted\" where the measure allows it.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Build an igraph graph from a resolved network, applying the weight-
#' semantics guardrail and any weight transform before the graph is constructed
#' -- so every igraph-backed metric gets a consistently validated graph, never
#' a raw hand-off of declared-orientation weights whose meaning was never
#' checked against what the metric expects them to mean.
#'
#' @noRd
.netfeat_build_igraph <- function(resolved_net, weighted_use, weight_transform, metric_descriptor, data) {
  M <- resolved_net$M
  mode_ig <- if (isTRUE(resolved_net$directed)) "directed" else "undirected"

  if (!weighted_use) {
    P <- .netfeat_presence_matrix(resolved_net, data)
    return(igraph::graph_from_adjacency_matrix(P, mode = mode_ig, weighted = NULL, diag = FALSE))
  }

  .assert_weight_semantics(.netfeat_network_semantics(resolved_net),
                           metric_descriptor$weight_semantics, weight_transform)

  if (!is.null(weight_transform)) {
    M <- .apply_weight_transform_matrix(M, weight_transform)
  }

  # A missing tie weight makes a path-based measure undefined for every node
  # whose paths might run through it, which for these measures cannot be
  # narrowed to particular nodes. Refused by name rather than passed to igraph,
  # which either fails opaquely or treats the weight as something it is not.
  if (anyNA(M)) {
    stop(
      sprintf(
        "This network has %d missing tie weight(s) in this snapshot, so a weighted path-based measure is undefined. Resolve the missing weights, or request weights = \"unweighted\" to use tie presence only.",
        sum(is.na(M)) / if (isTRUE(resolved_net$directed)) 1 else 2
      ),
      call. = FALSE
    )
  }

  if (isTRUE(metric_descriptor$requires_positive_weights)) {
    present <- M != 0
    if (any(M[present] <= 0)) {
      stop(
        "This measure requires strictly positive edge weights (after any weight_transform); a non-positive value was found.",
        call. = FALSE
      )
    }
  }

  igraph::graph_from_adjacency_matrix(M, mode = mode_ig, weighted = TRUE, diag = FALSE)
}

#' @noRd
.netfeat_igraph_betweenness <- function(resolved_net, mode, weighted, weight_transform, spec, data) {
  .netfeat_igraph_guard("betweenness")
  d <- .netfeat_descriptor("betweenness")
  g <- .netfeat_build_igraph(resolved_net, weighted, weight_transform, d, data)
  w <- if (is.null(igraph::E(g)$weight)) NULL else igraph::E(g)$weight
  val <- igraph::betweenness(g, directed = isTRUE(resolved_net$directed), weights = w)
  stats::setNames(val, resolved_net$ids)
}

#' @noRd
.netfeat_igraph_closeness <- function(resolved_net, mode, weighted, weight_transform, spec, data) {
  .netfeat_igraph_guard("closeness")
  d <- .netfeat_descriptor("closeness")
  g <- .netfeat_build_igraph(resolved_net, weighted, weight_transform, d, data)
  w <- if (is.null(igraph::E(g)$weight)) NULL else igraph::E(g)$weight
  ig_mode <- if (!isTRUE(resolved_net$directed)) "all" else switch(mode %||% "all", out = "out", `in` = "in", "all")
  val <- igraph::closeness(g, mode = ig_mode, weights = w)
  stats::setNames(val, resolved_net$ids)
}

#' @noRd
.netfeat_igraph_eigenvector <- function(resolved_net, mode, weighted, weight_transform, spec, data) {
  .netfeat_igraph_guard("eigenvector")
  d <- .netfeat_descriptor("eigenvector")
  g <- .netfeat_build_igraph(resolved_net, weighted, weight_transform, d, data)
  w <- if (is.null(igraph::E(g)$weight)) NULL else igraph::E(g)$weight
  val <- igraph::eigen_centrality(g, directed = isTRUE(resolved_net$directed), weights = w)$vector
  stats::setNames(val, resolved_net$ids)
}

#' @noRd
.netfeat_igraph_pagerank <- function(resolved_net, mode, weighted, weight_transform, spec, data) {
  .netfeat_igraph_guard("pagerank")
  d <- .netfeat_descriptor("pagerank")
  g <- .netfeat_build_igraph(resolved_net, weighted, weight_transform, d, data)
  w <- if (is.null(igraph::E(g)$weight)) NULL else igraph::E(g)$weight
  val <- igraph::page_rank(g, directed = isTRUE(resolved_net$directed), weights = w)$vector
  stats::setNames(val, resolved_net$ids)
}

#' @noRd
.netfeat_igraph_constraint <- function(resolved_net, mode, weighted, weight_transform, spec, data) {
  .netfeat_igraph_guard("constraint")
  d <- .netfeat_descriptor("constraint")
  g <- .netfeat_build_igraph(resolved_net, weighted, weight_transform, d, data)
  w <- if (is.null(igraph::E(g)$weight)) NULL else igraph::E(g)$weight
  val <- igraph::constraint(g, weights = w)
  stats::setNames(val, resolved_net$ids)
}

