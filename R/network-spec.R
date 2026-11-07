# Network construction layer: the specification object and resolver.
#
# "Which network, transformed how" is answered once, here, rather than every
# term that needs a network (centrality(), network_stat(), latent(),
# exposure()) re-deriving and re-guessing it.
#
# A network specification resolves to exactly one derived network per
# (group, wave): a bare column name is the degenerate case; binarize(),
# symmetrize(), and multiplex() (R/network-binarize.R, network-symmetrize.R,
# network-multiplex.R) compose on top of it. A spec still carries one
# conduit, and multiplex() is a *construction* collapsing several relations
# into one, not an exception to the "one network per term" rule.
#
# CRITICAL: this resolver's matrices are in DECLARED/SUBSTANTIVE orientation
# -- M[i, j] means "there is a tie recorded FROM i TO j", i.e. row = source,
# column = target, exactly matching netis_adjacency()'s export convention.
# This is deliberately NOT the exposure engine's canonical
# W[i,j] = "j influences i" orientation (.build_canonical_W(),
# R/exposure-engine.R). Reusing the exposure engine's transposed W for a
# network feature would silently reverse in-degree and
# out-degree for any network whose declared direction runs the other way.
# .build_declared_matrix() (below) is therefore a separate, un-transposed
# primitive, not a call to .build_canonical_W().

#' Binarize a tie network to presence/absence
#'
#' Used inside network-construction expressions ([binarize()], [symmetrize()],
#' [multiplex()]) and any term that takes a network ([exposure()], [centrality()],
#' [network_stat()], [latent()]) to specify a derived network built from another.
#' `network` may be a bare tie-variable column name (captured without evaluation,
#' like [exposure()]'s `via`) or the result of a nested network-construction call.
#'
#' A value rule ([nonzero()], [positive()], the thresholds, a custom rule) applied
#' to a tie variable sees only the dyads with a stored row, so a dyad with no row
#' never becomes a tie whatever the rule says about `0`. A stored `NA` stays `NA`
#' in the result. [presence()] reads the variable's declared presence instead.
#'
#' @param network a tie-variable name, or a [binarize()]/[symmetrize()]/
#'   [multiplex()] result to derive from.
#' @param rule a binarization rule: [presence()], [nonzero()], [positive()],
#'   [threshold()], [abs_threshold()], [quantile_threshold()], [above_mean()],
#'   [above_median()], or [custom_binarize_rule()]. Default [presence()].
#' @param name optional name for the derived network, used in place of the
#'   generated label when a term builds its default column name from it.
#' @return an object of class `netis_binarize_spec` (and `netis_network_spec`).
#' @seealso [symmetrize()], [multiplex()]
#' @examples
#' binarize(communication, rule = nonzero())
#' binarize(communication, rule = threshold(2))
#' @export
binarize <- function(network, rule = presence(), name = NULL) {
  inner <- .capture_network_arg(substitute(network), parent.frame())
  if (!inherits(rule, "netis_binarize_rule")) {
    stop(
      "rule must be one of presence()/nonzero()/positive()/threshold()/abs_threshold()/quantile_threshold()/above_mean()/above_median(), or custom_binarize_rule().",
      call. = FALSE
    )
  }
  structure(
    list(network = inner, rule = rule, name = name),
    class = c("netis_binarize_spec", "netis_network_spec")
  )
}

#' @noRd
.network_ref <- function(variable) {
  structure(list(variable = variable), class = c("netis_network_ref", "netis_network_spec"))
}

#' Capture a network-construction argument: a bare or quoted column name, not
#' evaluated, or a nested binarize()/symmetrize()/multiplex() call, evaluated
#' normally
#'
#' A symbol (`communication`) is always treated as a tie-variable name
#' reference and never evaluated -- the same NSE discipline every other
#' formula-term constructor in this package uses ([exposure()]'s `content`/
#' `via`, and the network-feature terms [centrality()] and [network_stat()]).
#' Anything else (a function call)
#' is evaluated in the caller's frame, because a nested construction call
#' must actually run to produce the spec object being composed. This
#' distinction, rather than a tryCatch-based "attempt to evaluate, see if it
#' errors" probe, is what makes nesting unambiguous: a bare name is never
#' accidentally evaluated against an unrelated same-named variable that
#' happens to exist in the caller's environment.
#'
#' @noRd
.capture_network_arg <- function(expr, env) {
  if (is.symbol(expr) || (is.character(expr) && length(expr) == 1L)) {
    return(.network_ref(as.character(expr)))
  }
  val <- eval(expr, envir = env)
  if (!inherits(val, "netis_network_spec")) {
    stop(
      "A network argument must be a tie-variable column name, or a binarize()/symmetrize()/multiplex() call.",
      call. = FALSE
    )
  }
  val
}

#' Build a declared-orientation adjacency matrix, row = source, column = target
#'
#' The one un-transposed matrix-construction primitive in the network
#' construction layer -- see this file's header comment for why it must stay
#' separate from `.build_canonical_W()`. Isolated/absent cells are `0`,
#' correct for weighted-sum-style consumers exactly as documented at
#' `.build_canonical_W()`'s own call site; presence-sensitive consumers
#' (`presence()`, the network-construction layer) resolve presence from the tie rows directly,
#' never by inspecting this matrix for nonzero cells.
#'
#' THE UNDIRECTED CONVENTION. Undirected data stores one row per unordered
#' pair (`netis_data()` refuses a pair stored twice), and every matrix built
#' from it is MIRRORED HERE, ONCE: the stored value is written to both `[i, j]`
#' and `[j, i]`. Every consumer -- exposure, network features, presence,
#' `symmetrize()`, `latent()` -- reads this symmetric matrix, so no consumer
#' needs, or may apply, a symmetrization of its own.
#'
#' @param directed `TRUE` for directed data; `FALSE` mirrors each stored row.
#' @noRd
.build_declared_matrix <- function(ties_g, ids, source_col, target_col, value_col,
                                   directed) {
  n <- length(ids)
  M <- matrix(0, nrow = n, ncol = n, dimnames = list(ids, ids))
  .assert_columns_present(ties_g, value_col, "tie data")
  if (nrow(ties_g) == 0L) {
    return(M)
  }
  src <- as.character(ties_g[[source_col]])
  tgt <- as.character(ties_g[[target_col]])
  val <- ties_g[[value_col]]
  keep <- src %in% ids & tgt %in% ids
  src <- src[keep]; tgt <- tgt[keep]; val <- val[keep]
  if (length(src) == 0L) {
    return(M)
  }
  .place_declared(M, src, tgt, val, directed)
}

#' Write stored tie rows into a declared-orientation matrix
#'
#' The single placement rule: `M[source, target] <- value`, and for undirected
#' data also `M[target, source] <- value`. Used by every matrix, mask and
#' presence builder so the undirected convention cannot differ between them.
#'
#' @noRd
.place_declared <- function(M, src, tgt, val, directed) {
  if (!is.logical(directed) || length(directed) != 1L || is.na(directed)) {
    stop("Internal error: `directed` must be TRUE or FALSE.", call. = FALSE) # nocov
  }
  M[cbind(src, tgt)] <- val
  if (!directed) {
    off <- src != tgt
    if (any(off)) {
      M[cbind(tgt[off], src[off])] <- if (length(val) == 1L) val else val[off]
    }
  }
  M
}

#' Resolve a network specification to one derived network for one (group, wave)
#'
#' Dispatches explicitly on the class of `spec` -- an ordinary if/else chain
#' rather than `UseMethod()`. Nothing outside this package will ever add a
#' new network-specification class, so this is internal control flow, not
#' genuinely open dispatch, and an explicit chain sidesteps a real
#' subtlety `UseMethod()` has for unexported generics called from outside
#' the package's own namespace environment (e.g. directly from a test file
#' under `devtools::load_all()`): method lookup can fail to find sibling
#' `.resolve_network.<class>` functions defined across several files
#' depending on the caller's environment, even though every function
#' involved is genuinely part of the same package. Each per-type resolver
#' below (`.resolve_network_ref()`, `.resolve_network_binarize()` in
#' `R/network-binarize.R`, `.resolve_network_symmetrize()` in
#' `R/network-symmetrize.R`, `.resolve_network_multiplex()` in
#' `R/network-multiplex.R`) is an ordinary named function, not a method,
#' avoiding the ambiguity entirely.
#'
#' Returns a list with `M` (the resolved declared-orientation matrix), `ids`
#' (node order), `directed`, `weighted`, `weight_semantics`, `variable` (the
#' single underlying tie variable, or `NULL` for a multiplex-derived network
#' with no single variable), `descriptor` (the tie descriptor, or
#' `NULL` if not applicable), `ties_snapshot` (the resolved tie rows, needed
#' by `presence()`), and `resolved` (provenance: every construction step
#' actually applied, recursively).
#'
#' @noRd
.resolve_network <- function(spec, data, group, wave) {
  if (is.character(spec)) {
    return(.resolve_network_ref(.network_ref(spec), data, group, wave))
  }
  if (inherits(spec, "netis_network_ref")) {
    return(.resolve_network_ref(spec, data, group, wave))
  }
  if (inherits(spec, "netis_binarize_spec")) {
    return(.resolve_network_binarize(spec, data, group, wave))
  }
  if (inherits(spec, "netis_symmetrize_spec")) {
    return(.resolve_network_symmetrize(spec, data, group, wave))
  }
  if (inherits(spec, "netis_multiplex_spec")) {
    return(.resolve_network_multiplex(spec, data, group, wave))
  }
  stop("spec must be a tie-variable name or a binarize()/symmetrize()/multiplex() result.", call. = FALSE) # nocov
}

#' @noRd
.resolve_network_ref <- function(spec, data, group, wave) {
  s <- netis_schema(data)
  if (!(spec$variable %in% names(s$tie_descriptors))) {
    stop(
      sprintf(
        "'%s' is not a tie variable in this netis_data object. Available: %s.",
        spec$variable, paste(names(s$tie_descriptors), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  nodes_all <- netis_nodes(data)
  nodes_g <- .filter_group_wave(nodes_all, s, group, wave)
  ties_g <- .filter_group_wave(netis_ties(data), s, group, wave)
  .assert_tie_endpoints_on_roster(
    ties_g, s, group,
    unique(as.character(nodes_all[[s$node_col]][as.character(nodes_all[[s$group_col]]) == group]))
  )
  # The vertices are the nodes observed at this wave plus every node a tie at this
  # wave reaches. A tie to a node of this group with no row at this wave is still
  # a tie, the exposure engine keeps it, and dropping it would change its other
  # endpoint's degree and strength with nothing reported.
  ids <- sort(unique(c(as.character(nodes_g[[s$node_col]]),
                       as.character(ties_g[[s$source_col]]),
                       as.character(ties_g[[s$target_col]]))))
  M <- .build_declared_matrix(ties_g, ids, s$source_col, s$target_col, spec$variable,
                              directed = s$directed)
  d <- s$tie_descriptors[[spec$variable]]
  list(
    M = M, ids = ids, directed = s$directed,
    weighted = d$weighted, weight_semantics = d$weight_semantics,
    variable = spec$variable, descriptor = d, ties_snapshot = ties_g,
    resolved = list(type = "reference", variable = spec$variable)
  )
}

#' Which declared-orientation cells correspond to an actual stored tie row
#'
#' `.build_declared_matrix()` fills every cell with `0` regardless of
#' whether that means "no row here" (the common `row_presence` case) or "a
#' row exists and its recorded value happens to be `0`." A value-based
#' predicate (`threshold()`, `abs_threshold()`, a custom rule) applied
#' directly to that matrix cannot tell the two apart, and a predicate that
#' happens to be satisfied at `v = 0` (e.g. `threshold(-1)`) would silently
#' turn every non-edge into a phantom edge. This mask lets a caller restrict
#' such a predicate's result to cells that genuinely have a row, independent
#' of the tie variable's declared `presence_mode` -- it answers "was this
#' cell actually observed," which is the narrower, always-safe question a
#' value rule needs, not "is this tie substantively present" (that is
#' `presence()`'s job, via `.resolve_component_presence()`).
#'
#' @noRd
.has_row_mask <- function(ties_snapshot, ids, source_col, target_col, directed) {
  n <- length(ids)
  mask <- matrix(FALSE, nrow = n, ncol = n, dimnames = list(ids, ids))
  if (nrow(ties_snapshot) == 0L) {
    return(mask)
  }
  src <- as.character(ties_snapshot[[source_col]])
  tgt <- as.character(ties_snapshot[[target_col]])
  keep <- src %in% ids & tgt %in% ids
  if (any(keep)) {
    mask <- .place_declared(mask, src[keep], tgt[keep], TRUE, directed)
  }
  mask
}

#' Resolve edge presence for an already-resolved network
#'
#' The single shared presence primitive every presence-needing consumer in
#' the package uses -- `binarize(rule = presence())`
#' (`.apply_presence_rule()`, R/network-binarize.R), `centrality()`/
#' `network_stat()`'s unweighted `degree`/`density`/`reciprocity`
#' (`.netfeat_presence_matrix()`, R/netfeat-spec.R, a thin alias for this
#' function), and `multiplex()`'s set-like combine rules
#' (`.resolve_network_multiplex()`, R/network-multiplex.R).
#'
#' A direct tie-variable reference (`resolved_net$descriptor` is set)
#' resolves via `.apply_presence_rule()` -- the correctly
#' `presence_mode`-aware logic, never a naive `value != 0`
#' check, which would misread a `complete_dyadic` network's measured zeros
#' as absent and ignore an `indicator`-declared presence column entirely.
#' A network that is already derived (a `binarize()`/`symmetrize()`/
#' `multiplex()` result, `descriptor` is `NULL`) has already been reduced to
#' its intended representation by its own construction step, so
#' `value != 0` is correct there -- there is no further schema-declared
#' presence semantic left to consult.
#'
#' Deliberately placed in this file, not `netfeat-spec.R`: `multiplex()` needs this and must not depend on the network-feature
#' layer built on top of it.
#'
#' @noRd
.resolve_component_presence <- function(resolved_net, data) {
  if (!is.null(resolved_net$descriptor)) {
    return(.apply_presence_rule(resolved_net, data)$M)
  }
  (resolved_net$M != 0) * 1
}
