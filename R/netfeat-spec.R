# Network-derived feature specification and metric descriptor registry.
#
# centrality() and network_stat() (R/formula-terms.R) construct a
# term specification and compute nothing -- the resolver here
# (.resolve_netfeat()) turns specs into columns, the pattern of
# netis_exposure_spec/exposure-engine.R and netis_latent_term/latent-xu.R. The two
# things this adds to those: a per-metric descriptor (metrics disagree about what their inputs
# mean, in a way exposure mechanisms and latent-space terms do not), and a `level`
# field (node vs. group output).

#' The metric descriptor registry
#'
#' Each entry: `level` (`"node"`/`"group"`), `directed` (how the metric
#' treats direction: `"supported"` takes a `mode`, `"requires_directed"`
#' errors on an undirected network, `"ignored"` computes the same regardless),
#' `weighted` (`"required"`/`"optional"`/`"unsupported"`),
#' `weight_semantics` (what the metric expects a larger value to mean, for
#' the weight-semantics guard, `.assert_weight_semantics()`; `NA` where not applicable),
#' `backend` (`"native"` or `"igraph"`), and `fn` (the computing function).
#'
#' This is a closed, explicit list, not an accumulation:
#' every ambiguous name -- transitivity above all -- must be resolved to a
#' precisely specified variant before it is added, not shipped under a
#' generic label.
#'
#' @noRd
.netfeat_registry <- function() {
  list(
    degree = list(
      level = "node", directed = "supported", weighted = "unsupported",
      weight_semantics = NA_character_, backend = "native", fn = .netfeat_degree
    ),
    strength = list(
      level = "node", directed = "supported", weighted = "required",
      weight_semantics = "strength", backend = "native", fn = .netfeat_strength
    ),
    density = list(
      level = "group", directed = "supported", weighted = "unsupported",
      weight_semantics = NA_character_, backend = "native", fn = .netfeat_density
    ),
    reciprocity = list(
      level = "group", directed = "requires_directed", weighted = "unsupported",
      weight_semantics = NA_character_, backend = "native", fn = .netfeat_reciprocity
    ),
    betweenness = list(
      level = "node", directed = "supported", weighted = "optional",
      weight_semantics = "distance", requires_positive_weights = TRUE,
      mode_supported = FALSE, backend = "igraph", fn = .netfeat_igraph_betweenness
    ),
    closeness = list(
      level = "node", directed = "supported", weighted = "optional",
      weight_semantics = "distance", requires_positive_weights = TRUE,
      mode_supported = TRUE, backend = "igraph", fn = .netfeat_igraph_closeness
    ),
    eigenvector = list(
      level = "node", directed = "supported", weighted = "optional",
      weight_semantics = "strength", mode_supported = FALSE,
      backend = "igraph", fn = .netfeat_igraph_eigenvector
    ),
    pagerank = list(
      level = "node", directed = "supported", weighted = "optional",
      weight_semantics = "strength", mode_supported = FALSE,
      backend = "igraph", fn = .netfeat_igraph_pagerank
    ),
    constraint = list(
      level = "node", directed = "supported", weighted = "optional",
      weight_semantics = "strength", mode_supported = FALSE,
      backend = "igraph", fn = .netfeat_igraph_constraint
    )
  )
}

#' @noRd
.netfeat_descriptor <- function(measure) {
  reg <- .netfeat_registry()
  if (!(measure %in% names(reg))) {
    stop(
      sprintf(
        "Unknown measure '%s'. Available: %s.",
        measure, paste(names(reg), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  reg[[measure]]
}

#' Infer the tie variable when a centrality()/network_stat() term omits `network`
#'
#' Same "infer only when unique, error naming candidates otherwise" policy
#' `as_igraph()`'s `.resolve_igraph_network()` (R/data-convert-igraph.R)
#' already applies -- duplicated rather than shared, since that function's
#' error wording is specific to conversion; the policy, not the code, is what
#' must stay consistent.
#'
#' @noRd
.resolve_netfeat_network_spec <- function(schema, net_spec) {
  if (!is.null(net_spec)) {
    return(net_spec)
  }
  candidates <- names(schema$tie_descriptors)
  if (length(candidates) == 1L) {
    return(.network_ref(candidates))
  }
  if (length(candidates) == 0L) {
    stop("This netis_data object has no tie variables to use.", call. = FALSE)
  }
  stop(
    sprintf(
      "This netis_data object has %d tie variables (%s); centrality()/network_stat() must name one.",
      length(candidates), paste(candidates, collapse = ", ")
    ),
    call. = FALSE
  )
}

#' Resolve a network-feature specification for one (group, wave)
#'
#' Parallel to `.resolve_exposure_group()`: resolves
#' the network via `.resolve_network()`, applies the metric, and
#' returns a data frame -- one row per node (`level = "node"`) or one row for
#' the group (`level = "group"`).
#'
#' @noRd
.resolve_netfeat_group <- function(spec, data, schema, group_id, wave) {
  d <- .netfeat_descriptor(spec$measure)
  net_spec <- .resolve_netfeat_network_spec(schema, spec$network)
  resolved_net <- .resolve_network(net_spec, data, group_id, wave)

  if (identical(d$directed, "requires_directed") && !resolved_net$directed) {
    stop(
      sprintf(
        "measure = \"%s\" requires a directed network; this network is undirected (or was symmetrized). %s is not a meaningful question for an inherently symmetric structure.",
        spec$measure, spec$measure
      ),
      call. = FALSE
    )
  }

  # mode = "in"/"out" is meaningless for several igraph algorithms that have
  # no such concept in their own definition (betweenness/eigenvector/
  # pagerank/constraint all take only a directed=TRUE/FALSE toggle, already
  # applied via the graph's own construction; igraph::closeness() is the
  # only one of the five with a genuine in/out/all mode argument). Silently
  # accepting and ignoring an explicit mode = "in"/"out" request here would
  # give a user a real, correctly-computed number that simply isn't what
  # they asked for, with no signal anything was ignored -- refuse instead.
  # "all" (the default, indistinguishable from "not specified") never
  # triggers this: only an explicit, meaningful-but-unsupported request does.
  if (!is.null(spec$mode) && !identical(spec$mode, "all") && identical(d$mode_supported, FALSE)) {
    stop(
      sprintf(
        "mode = \"%s\" is not supported for measure = \"%s\" -- this metric has no in/out/all direction concept of its own (only its overall directed/undirected treatment, which is already applied). Omit mode, or use a measure that supports it (e.g. \"degree\", \"strength\", \"closeness\").",
        spec$mode, spec$measure
      ),
      call. = FALSE
    )
  }

  weighted_use <- .resolve_netfeat_weighted(spec, d, resolved_net)
  # A transform is applied to edge weights, so a computation that reads none
  # would accept it, record it and not apply it.
  if (!is.null(spec$weight_transform) && !weighted_use) {
    stop(
      sprintf(
        "weight_transform has no effect here: measure = \"%s\" is being computed from tie presence, not edge weights. Remove it, or request weights = \"weighted\" if the measure supports them.",
        spec$measure
      ),
      call. = FALSE
    )
  }

  value <- d$fn(resolved_net, mode = spec$mode, weighted = weighted_use, weight_transform = spec$weight_transform, spec = spec, data = data)

  # The rows reported are the nodes OBSERVED at this wave. The network may also
  # hold a vertex with no node row here (a tie reaches a node of this group that
  # was not measured at this wave); it counts toward its alters' values, and has
  # no row of its own, as in the exposure engine.
  focal_ids <- sort(unique(as.character(
    .filter_group_wave(netis_nodes(data), schema, group_id, wave)[[schema$node_col]]
  )))

  # An unobserved network is not an empty one, the rule the exposure engine
  # applies: a group with an observed network at other waves and no record of one
  # here was not measured here, and a degree of 0 would claim that it was.
  ties_all <- netis_ties(data)
  # A group with no nodes at this wave was not there, so there is nothing to mark.
  absent <- length(focal_ids) > 0L && .network_absent_at_wave(
    ties_all, .filter_group_wave(ties_all, schema, group_id, wave), schema, group_id, wave
  )

  summary_type <- spec$summary
  produces_group_row <- identical(d$level, "group") || !is.null(summary_type)

  if (!produces_group_row) {
    # A group with no nodes at this wave (absent from that occasion) has no
    # node rows to report; `rep()` keeps the zero-row case a well-formed frame
    # instead of a length-1 group id against zero node ids.
    out <- data.frame(x = rep(group_id, length(focal_ids)), y = focal_ids,
                      stringsAsFactors = FALSE)
    names(out) <- c(schema$group_col, schema$node_col)
    out[[spec$name]] <- as.numeric(value[focal_ids])
  } else {
    out <- data.frame(x = group_id, stringsAsFactors = FALSE)
    names(out) <- schema$group_col
    out[[spec$name]] <- if (!is.null(summary_type)) {
      .apply_netfeat_summary(as.numeric(value[focal_ids]), summary_type)
    } else {
      as.numeric(value)
    }
  }
  if (absent) {
    out[[spec$name]] <- rep(NA_real_, nrow(out))
  }
  attr(out, "network_absent") <- absent
  attr(out, "backend") <- d$backend
  out
}

#' Distributional summaries of a node-level network feature
#'
#' `network_stat(x, measure = "degree", summary = "mean")` aggregates a
#' node-level feature into a single group-level value -- mean degree, SD of
#' betweenness, and so on -- reusing the same node-level resolution
#' `centrality()` uses (`.resolve_netfeat_group()`, above), never a parallel
#' calculation path. Aggregation happens within the same resolved (group,
#' wave) snapshot as the underlying node-level feature; there is no separate
#' cross-snapshot scope for this, unlike `binarize()`'s data-derived
#' thresholds.
#'
#' The surface syntax -- `measure` + `summary` on `network_stat()` itself,
#' rather than a literal nested `network_stat(centrality(...), summary = )`
#' call -- is a deliberate simplification: it delivers the same capability
#' (network specification -> node-level feature -> within-snapshot summary
#' -> group-level predictor) without requiring `stats::terms(specials = )` to
#' correctly distinguish an outer `network_stat()` call from an inner
#' `centrality()` call written inside it, which would need to be verified
#' empirically before being relied on.
#'
#' `"prop_zero"` reports the proportion of nodes with feature value exactly
#' `0` -- for `measure = "degree"`, this is exactly "proportion of
#' isolates"; for other measures it is the more general "proportion at the
#' measure's zero point," documented here rather than shipping a separately
#' named (and possibly differently defined) `"prop_isolates"`.
#'
#' @noRd
.netfeat_summary_types <- c("mean", "sd", "var", "median", "min", "max", "prop_zero")

#' @noRd
.apply_netfeat_summary <- function(values, summary_type) {
  v <- values[!is.na(values)]
  if (length(v) == 0L) {
    return(NA_real_)
  }
  if (identical(summary_type, "mean")) {
    return(mean(v))
  }
  if (identical(summary_type, "sd")) {
    return(stats::sd(v))
  }
  if (identical(summary_type, "var")) {
    return(stats::var(v))
  }
  if (identical(summary_type, "median")) {
    return(stats::median(v))
  }
  if (identical(summary_type, "min")) {
    return(min(v))
  }
  if (identical(summary_type, "max")) {
    return(max(v))
  }
  if (identical(summary_type, "prop_zero")) {
    return(mean(v == 0))
  }
  stop(sprintf("Unknown summary '%s'.", summary_type), call. = FALSE) # nocov
}

#' Decide whether a metric computation uses weights, given the spec's request
#' and the metric's own support
#'
#' `"auto"` (default) follows the resolved network's own `weighted` flag.
#' `"weighted"` and `"unweighted"` are explicit overrides, refused when the
#' metric does not support the requested mode (`weighted = "required"` metric
#' asked to run unweighted, or `weighted = "unsupported"` metric asked to run
#' weighted).
#'
#' @noRd
.resolve_netfeat_weighted <- function(spec, descriptor, resolved_net) {
  requested <- spec$weights
  if (identical(descriptor$weighted, "unsupported")) {
    if (identical(requested, "weighted")) {
      stop(sprintf("measure does not support a weighted calculation."), call. = FALSE)
    }
    return(FALSE)
  }
  if (identical(descriptor$weighted, "required")) {
    if (identical(requested, "unweighted")) {
      stop(sprintf("measure requires a weighted calculation; weights = \"unweighted\" is not supported for it."), call. = FALSE)
    }
    return(TRUE)
  }
  # "optional"
  if (identical(requested, "weighted")) {
    return(TRUE)
  }
  if (identical(requested, "unweighted")) {
    return(FALSE)
  }
  isTRUE(resolved_net$weighted)
}

#' Resolve edge presence for a network-feature computation
#'
#' A thin alias for `.resolve_component_presence()` (R/network-spec.R,
#' the network-construction layer) -- the same shared primitive `multiplex()`'s set-like combine
#' rules and `binarize(rule = presence())` use, not a second implementation.
#' Kept as its own name here since every native/igraph metric function in
#' this file already calls it as `.netfeat_presence_matrix()`.
#'
#' @noRd
.netfeat_presence_matrix <- function(resolved_net, data) {
  .resolve_component_presence(resolved_net, data)
}

#' Check that an undirected network's matrix is symmetric
#'
#' Undirected data is mirrored once, when its matrix is built
#' (`.build_declared_matrix()`, R/network-spec.R), so a matrix reaching a
#' network-feature computation for an undirected network is already symmetric
#' and is returned unchanged. No second symmetrization is applied here: an
#' elementwise `pmax(M, t(M))` would replace a negative tie weight by the zero
#' standing in for "no row", and would mask a construction defect. An
#' asymmetric matrix at this point is an internal error.
#'
#' @noRd
.ensure_symmetric_if_undirected <- function(M, directed) {
  if (isTRUE(directed)) {
    return(M)
  }
  if (!isSymmetric(unname(M))) {
    stop("Internal error: an undirected network reached a network-feature computation with an asymmetric matrix.", call. = FALSE) # nocov
  }
  M
}

#' Aggregate a declared-orientation matrix by direction mode
#'
#' `mode = "out"`: row sums (ties declared *from* each node, following the
#' raw declared direction exactly). `mode = "in"`: column sums (ties declared
#' *to* each node). `mode = "all"`: both combined. For undirected data the
#' matrix is already symmetric (`.ensure_symmetric_if_undirected()` checks) and
#' `mode` is moot (`out == in`, matching igraph's own convention).
#'
#' @noRd
.netfeat_mode_aggregate <- function(M, mode, directed) {
  M <- .ensure_symmetric_if_undirected(M, directed)
  if (!isTRUE(directed)) {
    return(rowSums(M))
  }
  mode <- if (is.null(mode)) "all" else mode
  if (identical(mode, "out")) {
    return(rowSums(M))
  }
  if (identical(mode, "in")) {
    return(colSums(M))
  }
  if (identical(mode, "all")) {
    return(rowSums(M) + colSums(M))
  }
  stop(sprintf("mode must be one of \"out\", \"in\", \"all\"; got \"%s\".", mode), call. = FALSE)
}

#' @noRd
.resolve_netfeat <- function(spec, data, wave) {
  s <- netis_schema(data)
  .assert_wave_argument(s, wave)
  nodes_all <- netis_nodes(data)
  groups <- sort(unique(as.character(nodes_all[[s$group_col]])))
  pieces <- lapply(groups, function(g) .resolve_netfeat_group(spec, data, s, g, wave))
  absent <- groups[vapply(pieces, function(p) isTRUE(attr(p, "network_absent")), logical(1))]
  .warn_absent_networks(
    if (length(absent) > 0L) sprintf("group %s wave %s", absent, wave) else character(0),
    .conduit_label(.resolve_netfeat_network_spec(s, spec$network)),
    quantity = "The network feature"
  )
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  .restore_key_types(out, nodes_all, s)
}
