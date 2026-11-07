# Native (dependency-free) network-feature metrics. Degree, strength, density,
# and reciprocity are unambiguous matrix operations, implemented here so the
# most common centrality()/network_stat() terms never require igraph. Every
# function has signature function(resolved_net, mode, weighted,
# weight_transform, spec, data), matching .netfeat_registry()'s fn slot
# (R/netfeat-spec.R) exactly, even where a particular argument is unused --
# igraph-backed metrics (R/netfeat-igraph.R) need the full signature, and one
# shared shape is simpler than two.
#
# Transitivity is deliberately absent from this file (and from the
# registry): the name covers several distinct quantities (global vs. local,
# directed vs. undirected), and shipping one under an unqualified name would
# be exactly the silent substantive choice this package refuses elsewhere.
# Use igraph with the variant named explicitly.

#' @noRd
.netfeat_degree <- function(resolved_net, mode, weighted, weight_transform, spec, data) {
  P <- .netfeat_presence_matrix(resolved_net, data)
  .netfeat_mode_aggregate(P, mode, resolved_net$directed)
}

#' @noRd
.netfeat_strength <- function(resolved_net, mode, weighted, weight_transform, spec, data) {
  .assert_weight_semantics(.netfeat_network_semantics(resolved_net), "strength", weight_transform)
  M <- resolved_net$M
  if (!is.null(weight_transform)) {
    M <- .apply_weight_transform_matrix(M, weight_transform)
  }
  .netfeat_mode_aggregate(M, mode, resolved_net$directed)
}

#' Network density: actual ties / possible ties
#'
#' Possible ties accounts for directedness and the schema's self-tie policy
#' (mirroring the same formula the presence-mode inference uses for
#' its own possible-dyad count, `.infer_presence_mode()`, R/data-tie-meta.R
#' -- duplicated rather than shared because that function is private to a
#' different concern and this is a small, self-contained formula).
#'
#' @noRd
.netfeat_density <- function(resolved_net, mode, weighted, weight_transform, spec, data) {
  P <- .netfeat_presence_matrix(resolved_net, data)
  n <- length(resolved_net$ids)
  if (n < 2L) {
    return(NA_real_)
  }
  self_ties_permitted <- identical(netis_schema(data)$self_ties, "permitted")

  if (isTRUE(resolved_net$directed)) {
    possible <- if (self_ties_permitted) n * n else n * (n - 1)
    actual <- if (self_ties_permitted) sum(P) else sum(P) - sum(diag(P))
  } else {
    P <- .ensure_symmetric_if_undirected(P, FALSE)
    possible <- if (self_ties_permitted) n * (n + 1) / 2 else n * (n - 1) / 2
    offdiag <- (sum(P) - sum(diag(P))) / 2
    actual <- offdiag + if (self_ties_permitted) sum(diag(P)) else 0
  }
  actual / possible
}

#' Dyadic reciprocity: the fraction of present directed ties that are
#' reciprocated
#'
#' `sum(P * t(P)) / sum(P)` over off-diagonal cells -- among every ordered
#' pair with a present tie i -> j, what fraction also have j -> i present.
#' The standard "ratio" reciprocity measure (matching igraph's default
#' `reciprocity()` definition). Requires a directed network -- enforced by
#' the metric descriptor's `directed = "requires_directed"`
#' (`.resolve_netfeat_group()`, R/netfeat-spec.R), not re-checked here.
#'
#' @noRd
.netfeat_reciprocity <- function(resolved_net, mode, weighted, weight_transform, spec, data) {
  P <- .netfeat_presence_matrix(resolved_net, data)
  diag(P) <- 0
  total <- sum(P)
  if (total == 0) {
    return(NA_real_)
  }
  mutual <- sum(P * t(P))
  mutual / total
}
