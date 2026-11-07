#' Compute a node-level network-derived feature directly
#'
#' Calculates a structural quantity for every node -- degree, strength, or
#' (with the `igraph` package installed) betweenness, closeness, eigenvector
#' centrality, PageRank, or constraint -- without fitting any model. Useful
#' for inspection, teaching, debugging, and reproducibility: you can see
#' exactly what a [centrality()] term's control column looks like before (or
#' instead of) fitting anything.
#'
#' `compute_centrality()` resolves through the same engine [fit_influence()]
#' uses for `centrality()` terms. There is exactly one resolution code path
#' in the package; this function and model fitting cannot compute different
#' answers for the same specification, because they are the same computation.
#'
#' # Direction
#'
#' `mode = "in"`/`"out"` refer to the **declared** edge direction recorded in
#' [netis_data()] — how many ties point at this node under whatever semantic the
#' data declares — and deliberately **not** to the exposure engine's canonical
#' `W[i,j] = j influences i` orientation. The two are different questions, and
#' reusing the transposed convention here would silently reverse in-degree and
#' out-degree for any network whose declared direction runs the other way. See
#' [centrality()]'s "Direction is substantive, never computational" for the full
#' statement. `mode` has no effect on an undirected network.
#'
#' # Errors and refusals
#'
#' - **The metric reads weights one way and the network declares them the other.**
#'   `"betweenness"` and `"closeness"` treat a larger weight as a *longer* path,
#'   while a network declared `weight_semantics = "strength"` (or `"signed"`,
#'   `"count"`) means the opposite; `"strength"` and the other weight-reading
#'   measures treat a larger weight as a stronger tie, while one declared
#'   `"distance"` means the opposite. Passing those weights through would answer a
#'   different question silently, so this refuses. Resolve it with an explicit
#'   `weight_transform = inverse()`, or with `weights = "unweighted"` where the
#'   measure allows it — the transform is never applied automatically, because
#'   the scale interpretation is substantive. An undeclared `"unitless"` network
#'   makes no claim and is not refused.
#' - **Negative weights where the measure needs positive ones** (`"betweenness"`,
#'   `"closeness"`). Checked after any `weight_transform`. A zero cell reads as no
#'   tie.
#' - **A missing weight in a weighted `igraph`-backed measure.** It cannot be
#'   confined to particular nodes; use `weights = "unweighted"`.
#' - **`mode = "in"` or `"out"` on an `igraph`-backed measure other than
#'   `"closeness"`,** which has no such reading, and a `weights` setting the
#'   measure cannot honour: `"degree"` is never weighted and `"strength"` always
#'   is.
#' - **A `weight_transform` on a computation that reads no weights.**
#' - **An `igraph`-backed measure with `igraph` not installed.** Everything
#'   except `"degree"` and `"strength"` needs it; the error names the measure.
#' - **A group-level measure passed here**, e.g. `"density"`: use
#'   [compute_network_stat()] instead. The reverse mistake refuses too.
#' - **`wave` missing when the data has waves, or supplied when it has none.**
#'   A network that changes over time has no single answer, so rnetis will not
#'   pick a wave for you.
#'
#'
#' # Isolates
#'
#' A node with no ties is a **structural** zero, not a missing observation, and
#' which of those the output shows depends on the measure — so both halves are
#' stated here rather than left to be discovered. `"degree"`, `"strength"` and
#' `"betweenness"` return `0` for an isolate, which is the honest answer: it
#' has no ties, and it lies on no path. `"closeness"` returns `NaN`, because
#' closeness is built from distances to other nodes and there is no finite
#' distance from an isolate to anything — an undefined quantity, not a zero,
#' and rnetis does not coerce it to one. On the chain `a -> b -> c` plus an
#' isolate `d`, degree gives `1 2 1 0` and closeness gives `0.333 0.5 0.333 NaN`.
#'
#' A node of a group whose network was **not observed** at this wave (an observed
#' network at other waves, no record of one at this wave) is different again: its value is `NA` for every
#' measure, with a warning naming the snapshot. See "An unobserved network is not
#' an empty one" on [compute_exposure()].
#'
#' @param data a [netis_data()] object.
#' @param network a tie-variable name, or a [binarize()]/[symmetrize()]/
#'   [multiplex()] result. Inferred when omitted and there is exactly one tie
#'   variable in the data. See [centrality()].
#' @param measure one of `"degree"`, `"strength"`, `"betweenness"`,
#'   `"closeness"`, `"eigenvector"`, `"pagerank"`, `"constraint"`. Everything
#'   after `"strength"` requires the `igraph` package.
#' @param mode `"all"` (default), `"in"`, or `"out"`. Refers to the **declared**
#'   edge direction, not the exposure engine's canonical orientation — see
#'   Direction above, and [centrality()] for why the distinction is substantive.
#' @param weights `"auto"` (default), `"weighted"`, or `"unweighted"`.
#'   `"auto"` follows the resolved network's own weighted/binary classification.
#' @param weight_transform `NULL` (default), or [inverse()]. Never applied
#'   automatically: see Errors and refusals.
#' @param ... passed to methods; unused by the `netis_data` method.
#' @param wave which wave to compute the feature for, if `data` has a wave
#'   dimension. Required whenever `data` has any waves at all, and must be
#'   `NULL` when `data` has no wave dimension.
#' @param name name for the resulting column. Defaults as in [centrality()].
#' @param augment if `FALSE` (default), return a data frame with just the
#'   group, node, and feature columns. If `TRUE`, return the full node data
#'   (as from [netis_nodes()]) with the feature column added.
#' @return a data frame with one row per node observed at the wave, ordered by
#'   group then node with both sorted as character strings, so `"10"` precedes
#'   `"2"`. With
#'   `augment = FALSE` (default) the columns are the group identifier, the node
#'   identifier, and the computed feature under `name`; with `augment = TRUE`
#'   it is the full node table from [netis_nodes()] with that one column added.
#' @references
#' Bonacich, P. (1987) Power and centrality: a family of measures. *American
#' Journal of Sociology* **92**(5), 1170-1182. For `"eigenvector"`.
#'
#' Brin, S. and Page, L. (1998) The anatomy of a large-scale hypertextual Web
#' search engine. *Computer Networks and ISDN Systems* **30**, 107-117. For
#' `"pagerank"`.
#'
#' Burt, R. S. (2004) Structural holes and good ideas. *American Journal of
#' Sociology* **110**(2), 349-399. For `"constraint"`.
#'
#' Csardi, G. and Nepusz, T. (2006) The igraph software package for complex
#' network research. *InterJournal*, Complex Systems, 1695. The implementation of
#' every measure other than `"degree"` and `"strength"`.
#'
#' Freeman, L. C. (1978) Centrality in social networks: conceptual clarification.
#' *Social Networks* **1**(3), 215-239. For `"degree"`, `"betweenness"` and
#' `"closeness"`.
#' @seealso [centrality()] for the same computation as a model term, and for the
#'   full Direction and Weighting statements; [compute_network_stat()] for the
#'   group-level counterpart; [inverse()] for the weight transform the
#'   distance-metric refusal asks for; [netis_data()]
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b", "c"))
#' ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), tie = 1)
#' nd <- netis_data(nodes, ties)
#' compute_centrality(nd, tie, measure = "degree")
#' @export
compute_centrality <- function(data, network, measure, ...) {
  UseMethod("compute_centrality")
}

#' @rdname compute_centrality
#' @export
compute_centrality.netis_data <- function(data, network, measure, mode = c("all", "in", "out"),
                                weights = c("auto", "weighted", "unweighted"),
                                weight_transform = NULL, wave = NULL, name = NULL,
                                augment = FALSE, ...) {
  net_spec <- if (missing(network)) NULL else .capture_network_arg(substitute(network), parent.frame())
  .compute_centrality_resolved(
    data, net_spec, measure, mode = mode, weights = weights,
    weight_transform = weight_transform, wave = wave, name = name, augment = augment
  )
}

#' Compute a centrality from an ALREADY-RESOLVED network specification
#'
#' The shared body of `compute_centrality.netis_data()` and the foreign-object
#' method. The network argument is captured by NSE at whichever public entry the
#' user called, then handed here already resolved -- so there is one computation
#' path, and a foreign input cannot get a different answer from a `netis_data`
#' one.
#'
#' @noRd
.compute_centrality_resolved <- function(data, net_spec, measure,
                                          mode = c("all", "in", "out"),
                                          weights = c("auto", "weighted", "unweighted"),
                                          weight_transform = NULL, wave = NULL,
                                          name = NULL, augment = FALSE) {
  .check_netis_data(data)
  mode <- match.arg(mode)
  weights <- match.arg(weights)
  .assert_flag(augment, "augment")
  spec <- centrality_(net_spec, measure, mode, weights, weight_transform, name)
  spec <- .finalize_netfeat_name(spec, "cent")

  result <- .resolve_netfeat(spec, data, wave)
  .maybe_augment_netfeat(data, result, wave, augment)
}

#' Compute a group-level network-derived feature directly
#'
#' The group-level counterpart of [compute_centrality()]: density,
#' reciprocity, or a distributional summary of a node-level measure (`summary
#' = "mean"`, etc. -- see [network_stat()]).
#'
#' # Errors and refusals
#'
#' The weight-semantics, positive-weight, `igraph` and `wave` refusals are the
#' same ones [compute_centrality()] documents, because both resolve through the
#' same engine. Two are specific to this function:
#'
#' - **A node-level measure with no `summary`**, e.g. `"degree"`: it has one
#'   value per node and this function returns one row per group, so rnetis asks
#'   which summary you mean rather than picking one. Use
#'   [compute_centrality()] for the node-level values themselves.
#' - **`summary` supplied for a measure that is already group-level**, e.g.
#'   `"density"`: there is nothing to aggregate.
#'
#' **Computing a constant is not refused here.** A single group with one
#' resolved snapshot gives one density value, which is a perfectly good number
#' to look at. What is refused — by [fit_influence()], on a [network_stat()]
#' term — is *estimating a coefficient* on it, because a constant predictor is
#' perfectly collinear with the intercept. Inspection and estimation are
#' different questions, and only the second one is unidentified.
#'
#' @inheritParams compute_centrality
#' @param measure one of `"density"`, `"reciprocity"` (when `summary` is
#'   `NULL`), or a node-level measure such as `"degree"` (when `summary` is
#'   given). See [network_stat()].
#' @param ... passed to methods; unused by the `netis_data` method.
#' @param summary `NULL` (default), or one of `"mean"`, `"sd"`, `"var"`,
#'   `"median"`, `"min"`, `"max"`, `"prop_zero"`. See [network_stat()].
#' @return a data frame with one row per group, in group order (identifiers sort
#'   as character strings). With `augment = FALSE` (default) the columns are the
#'   group identifier and the computed feature under `name`; with
#'   `augment = TRUE` it is the group table with that column added. A group with
#'   nodes but no ties takes the measure's own structural value — a density of
#'   `0` — rather than `NA`, keeping "no ties were observed" distinct from "this
#'   was not measured". A group with an observed network at other waves and no
#'   record of one at this wave is the second case: its value is `NA`, with a
#'   warning.
#' @references
#' Wasserman, S. and Faust, K. (1994) *Social Network Analysis: Methods and
#' Applications*. Cambridge University Press. For `"density"` and
#' `"reciprocity"`, standard descriptive measures; reciprocity here is the share
#' of present directed ties that are reciprocated.
#'
#' Bonacich, P. (1987) Power and centrality: a family of measures. *American
#' Journal of Sociology* **92**(5), 1170-1182. For `"eigenvector"`.
#'
#' Brin, S. and Page, L. (1998) The anatomy of a large-scale hypertextual Web
#' search engine. *Computer Networks and ISDN Systems* **30**, 107-117. For
#' `"pagerank"`.
#'
#' Burt, R. S. (2004) Structural holes and good ideas. *American Journal of
#' Sociology* **110**(2), 349-399. For `"constraint"`.
#'
#' Csardi, G. and Nepusz, T. (2006) The igraph software package for complex
#' network research. *InterJournal*, Complex Systems, 1695. The implementation of
#' every measure other than `"degree"` and `"strength"`.
#'
#' Freeman, L. C. (1978) Centrality in social networks: conceptual clarification.
#' *Social Networks* **1**(3), 215-239. For `"degree"`, `"betweenness"` and
#' `"closeness"`.
#' @seealso [network_stat()] for the same computation as a model term, and for
#'   the Identification statement on what a group-level predictor needs;
#'   [compute_centrality()] for the node-level counterpart; [netis_data()]
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b", "c"))
#' ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), tie = 1)
#' nd <- netis_data(nodes, ties)
#' compute_network_stat(nd, tie, measure = "density")
#' compute_network_stat(nd, tie, measure = "degree", summary = "mean")
#' @export
compute_network_stat <- function(data, network, measure, ...) {
  UseMethod("compute_network_stat")
}

#' @rdname compute_network_stat
#' @export
compute_network_stat.netis_data <- function(data, network, measure, summary = NULL,
                                  weights = c("auto", "weighted", "unweighted"),
                                  weight_transform = NULL, wave = NULL, name = NULL,
                                  augment = FALSE, ...) {
  net_spec <- if (missing(network)) NULL else .capture_network_arg(substitute(network), parent.frame())
  .compute_network_stat_resolved(
    data, net_spec, measure, summary = summary, weights = weights,
    weight_transform = weight_transform, wave = wave, name = name, augment = augment
  )
}

#' Shared body, network already resolved -- see `.compute_centrality_resolved()`.
#' @noRd
.compute_network_stat_resolved <- function(data, net_spec, measure, summary = NULL,
                                            weights = c("auto", "weighted", "unweighted"),
                                            weight_transform = NULL, wave = NULL,
                                            name = NULL, augment = FALSE) {
  .check_netis_data(data)
  weights <- match.arg(weights)
  .assert_flag(augment, "augment")
  spec <- network_stat_(net_spec, measure, weights, weight_transform, name, summary = summary)
  spec <- .finalize_netfeat_name(spec, "net")

  result <- .resolve_netfeat(spec, data, wave)
  .maybe_augment_netfeat(data, result, wave, augment)
}

#' Build a netis_netfeat_spec directly from already-captured pieces
#'
#' `centrality()`/`network_stat()` (R/formula-terms.R) capture `network` via
#' NSE at the formula-term call site; `compute_centrality()`/
#' `compute_network_stat()` capture it themselves (they are the top-level
#' call, so NSE happens there instead) and hand the already-resolved network
#' spec to these, avoiding a second, slightly different NSE path for what is
#' otherwise identical spec construction.
#'
#' @noRd
centrality_ <- function(net_spec, measure, mode, weights, weight_transform, name, lag = 1L) {
  d <- .netfeat_descriptor(measure)
  if (!identical(d$level, "node")) {
    stop(sprintf("measure '%s' is group-level; use network_stat() instead.", measure), call. = FALSE)
  }
  structure(
    list(network = net_spec, measure = measure, mode = mode, weights = weights,
         weight_transform = weight_transform, lag = as.integer(lag), name = name),
    class = "netis_centrality_term"
  )
}

#' @noRd
network_stat_ <- function(net_spec, measure, weights, weight_transform, name, lag = 1L, summary = NULL) {
  d <- .netfeat_descriptor(measure)
  if (is.null(summary) && !identical(d$level, "group")) {
    stop(sprintf("measure '%s' is node-level; use centrality() instead, or pass summary = to aggregate it.", measure), call. = FALSE)
  }
  if (!is.null(summary) && !identical(d$level, "node")) {
    stop(sprintf("summary = is only meaningful for a node-level measure; '%s' is already group-level.", measure), call. = FALSE)
  }
  structure(
    list(network = net_spec, measure = measure, summary = summary, weights = weights,
         weight_transform = weight_transform, lag = as.integer(lag), name = name),
    class = "netis_network_stat_term"
  )
}

#' Fill in a netfeat spec's default name, the same way formula-parse.R does
#' for a formula-embedded term, since a standalone compute_*() call bypasses
#' that code path entirely
#'
#' @noRd
.finalize_netfeat_name <- function(spec, prefix) {
  if (!is.null(spec$name)) {
    return(spec)
  }
  net <- spec$network
  spec$name <- if (!is.null(net) && inherits(net, "netis_network_ref")) {
    paste0(prefix, "_", spec$measure, "_", net$variable)
  } else {
    paste0(prefix, "_", spec$measure)
  }
  spec
}

#' @noRd
.maybe_augment_netfeat <- function(data, result, wave, augment) {
  if (!augment) {
    return(result)
  }
  s <- netis_schema(data)
  # A feature with a node column augments the node table; a group-level feature
  # (one row per group) augments the group table, which is just the identifiers
  # when no group data was supplied.
  base <- if (s$node_col %in% names(result)) netis_nodes(data) else netis_groups(data)
  if (is.null(base)) {
    return(result)
  }
  if (!is.null(wave) && !is.null(s$wave_col) && s$wave_col %in% names(base)) {
    base <- base[base[[s$wave_col]] == wave, , drop = FALSE]
  }
  by_cols <- intersect(names(result), c(s$group_col, s$node_col))
  if (s$node_col %in% names(result)) {
    return(merge(base, result, by = by_cols, all.x = TRUE, sort = FALSE))
  }
  merge(base, result, by = by_cols, all = TRUE, sort = FALSE)
}
