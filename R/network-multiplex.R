# multiplex() (R/network-spec.R).
#
# A derived network built from several relations, resolving to exactly one
# network: this is a *network construction*, not an exception to the
# "one network per term" rule, because the spec it produces still carries
# one conduit.

#' Combine several networks into one derived network
#'
#' Used inside network-construction expressions and any term that takes a
#' network ([exposure()], [centrality()], [network_stat()], [latent()]), for
#' statements like "A and B are connected
#' if any of these relations exists." Each `...` argument is a tie-variable
#' name or a nested [binarize()]/[symmetrize()]/[multiplex()] result;
#' resolves to exactly one derived network.
#'
#' Set-like rules (`"union"`, `"intersection"`, `"k_of_n"`, `"count"`) operate
#' on each component's *presence*, resolved as [presence()] resolves it for a tie
#' variable, so a component whose presence is undetermined is refused. Value-combining
#' rules (`"sum"`, `"mean"`) are not automatically well-defined: summing two
#' binary networks is meaningful, but summing communication frequency with
#' friendship strength adds incommensurable units. Before combining by value,
#' every component's declared `weight_semantics` is checked for compatibility, and
#' mixing a weighted component with a binary one is refused -- both checks are
#' refusals with a fix named, not silent approximations. This check compares declared *categories* (e.g.
#' `"strength"` vs. `"distance"`), not numeric scale (two `"strength"`-declared
#' components measured on incompatible instruments still pass); binarize the
#' incompatible component first, or declare compatible semantics, if that finer
#' distinction matters.
#'
#' @param ... two or more tie-variable names or nested network-construction
#'   calls.
#' @param combine `"union"`/`"any"` (present in at least one component),
#'   `"intersection"`/`"all"` (present in every component), `"k_of_n"`
#'   (present in at least `k` components; requires `k`), `"count"` (weight is
#'   the number of components present), `"sum"` (component values added),
#'   or `"mean"` (component values averaged).
#' @param k for `combine = "k_of_n"`, the minimum number of components a tie
#'   must be present in.
#' @param name optional name for the derived network.
#' @return an object of class `netis_multiplex_spec` (and `netis_network_spec`).
#' @seealso [binarize()], [symmetrize()]
#' @examples
#' multiplex(communication, advice, combine = "union")
#' multiplex(communication, advice, friendship, combine = "k_of_n", k = 2)
#' @export
multiplex <- function(..., combine = c("union", "any", "intersection", "all", "k_of_n", "count", "sum", "mean"), k = NULL, name = NULL) {
  combine <- match.arg(combine)
  combine <- switch(combine, any = "union", all = "intersection", combine)

  dots <- as.list(substitute(list(...)))[-1]
  if (length(dots) < 2L) {
    stop("multiplex() requires at least two networks.", call. = FALSE)
  }
  env <- parent.frame()
  components <- lapply(dots, .capture_network_arg, env = env)

  if (identical(combine, "k_of_n")) {
    if (is.null(k) || !is.numeric(k) || length(k) != 1L || k < 1 || k > length(components)) {
      stop(
        sprintf("combine = \"k_of_n\" requires k, an integer between 1 and the number of networks (%d).", length(components)),
        call. = FALSE
      )
    }
  }

  structure(
    list(components = components, combine = combine, k = k, name = name),
    class = c("netis_multiplex_spec", "netis_network_spec")
  )
}

#' @noRd
.resolve_network_multiplex <- function(spec, data, group, wave) {
  resolved_components <- lapply(spec$components, .resolve_network, data = data, group = group, wave = wave)
  ids <- sort(unique(unlist(lapply(resolved_components, function(r) r$ids))))

  is_presence_combine <- spec$combine %in% c("union", "intersection", "k_of_n", "count")

  if (is_presence_combine) {
    # Presence-based rules: resolve each component's presence through the
    # same schema-aware primitive binarize(rule = presence()) uses
    # (.resolve_component_presence(), R/network-spec.R) -- NOT a naive
    # `value != 0` check on the raw resolved matrix, which would misread a
    # complete_dyadic component's measured zeros as absent and ignore an
    # indicator-declared presence column entirely; using `(m != 0) * 1` here
    # unconditionally would be wrong for exactly those components.
    mats <- lapply(resolved_components, function(r) {
      .reindex_matrix(.resolve_component_presence(r, data), ids)
    })
    M <- .combine_multiplex_presence(mats, spec$combine, spec$k)
    weight_semantics <- if (identical(spec$combine, "count")) "count" else "unitless"
  } else {
    # Value-combining rules ("sum"/"mean") operate on the components' actual
    # resolved values, not presence -- .check_multiplex_scale_compatibility()
    # below is what makes this safe.
    mats <- lapply(resolved_components, function(r) .reindex_matrix(r$M, ids))
    .check_multiplex_scale_compatibility(resolved_components, spec$combine)
    M <- .combine_multiplex_values(mats, spec$combine)
    # Propagate the semantics .check_multiplex_scale_compatibility() just
    # validated as shared across every component: "unitless" here would switch off
    # the network features' weight-semantics guard for every multiplex-summed
    # network, since that guard never fires on "unitless".
    weight_semantics <- .multiplex_combined_semantics(resolved_components)
  }

  dimnames(M) <- list(ids, ids)
  directed_any <- any(vapply(resolved_components, function(r) r$directed, logical(1)))

  list(
    M = M, ids = ids, directed = directed_any,
    weighted = spec$combine %in% c("count", "sum", "mean"),
    weight_semantics = weight_semantics,
    variable = NULL, descriptor = NULL, ties_snapshot = NULL,
    resolved = list(
      type = "multiplex", combine = spec$combine, k = spec$k,
      components = lapply(resolved_components, function(r) r$resolved)
    )
  )
}

#' Reindex a resolved matrix onto a (possibly larger) common node set
#'
#' Component networks in a multiplex() may have been resolved from different
#' node rosters (a node present in one relation's group data but absent from
#' another's, though within one netis_data group this is expected to be
#' rare). Missing rows/columns are filled with `0` -- absent, not present --
#' the correct default for combining into a union/intersection/etc.
#'
#' @noRd
.reindex_matrix <- function(M, ids) {
  out <- matrix(0, nrow = length(ids), ncol = length(ids), dimnames = list(ids, ids))
  common <- intersect(rownames(M), ids)
  if (length(common) > 0L) {
    out[common, common] <- M[common, common]
  }
  out
}

#' Combine presence-resolved (already-0/1) component matrices
#'
#' Each `mats[[i]]` is already a correctly-resolved presence matrix (from
#' `.resolve_component_presence()`); the `!= 0` here is just treating a 0/1
#' matrix as logical for the boolean combine ops, not re-deriving presence
#' from raw values.
#'
#' @noRd
.combine_multiplex_presence <- function(mats, combine, k) {
  if (identical(combine, "union")) {
    Reduce(function(a, b) ((a != 0) | (b != 0)) * 1, mats)
  } else if (identical(combine, "intersection")) {
    Reduce(function(a, b) ((a != 0) & (b != 0)) * 1, mats)
  } else if (identical(combine, "k_of_n")) {
    total <- Reduce(`+`, mats)
    (total >= k) * 1
  } else if (identical(combine, "count")) {
    Reduce(`+`, mats)
  } else {
    stop(sprintf("Unknown multiplex presence-combine rule '%s'.", combine), call. = FALSE) # nocov
  }
}

#' Combine component matrices by value ("sum"/"mean")
#'
#' @noRd
.combine_multiplex_values <- function(mats, combine) {
  if (identical(combine, "sum")) {
    Reduce(`+`, mats)
  } else if (identical(combine, "mean")) {
    Reduce(`+`, mats) / length(mats)
  } else {
    stop(sprintf("Unknown multiplex value-combine rule '%s'.", combine), call. = FALSE) # nocov
  }
}

#' The weight_semantics a validated value-combining multiplex() result carries
#'
#' Called only after `.check_multiplex_scale_compatibility()` has confirmed
#' every non-`"unitless"` component shares one semantics category, so there
#' is at most one such value to propagate; if every component happens to be
#' `"unitless"`, the result stays `"unitless"` honestly rather than guessing.
#'
#' @noRd
.multiplex_combined_semantics <- function(resolved_components) {
  semantics <- vapply(resolved_components, function(r) {
    if (is.null(r$weight_semantics)) "unitless" else r$weight_semantics
  }, character(1))
  distinct <- unique(semantics[semantics != "unitless"])
  if (length(distinct) == 1L) distinct else "unitless"
}

#' Refuse a value-combining multiplex() when components are not commensurable
#'
#' Two checks, both refusals naming the fix rather than silent
#' approximations: declared `weight_semantics` categories must agree across
#' every component that isn't `"unitless"`, and a weighted component may not
#' be combined by value with a binary one (a 0/1 presence vote summed with a
#' continuous strength score has no coherent interpretation).
#'
#' @noRd
.check_multiplex_scale_compatibility <- function(resolved_components, combine) {
  semantics <- vapply(resolved_components, function(r) {
    if (is.null(r$weight_semantics)) "unitless" else r$weight_semantics
  }, character(1))
  weighted_flags <- vapply(resolved_components, function(r) isTRUE(r$weighted), logical(1))

  distinct_semantics <- unique(semantics[semantics != "unitless"])
  if (length(distinct_semantics) > 1L) {
    stop(
      sprintf(
        "multiplex(..., combine = \"%s\") cannot combine networks with incompatible declared weight_semantics (%s) -- summing communication frequency with friendship strength, for example, adds incommensurable units. Binarize the incompatible component(s) first (e.g. binarize(x, rule = nonzero())), or declare compatible weight_semantics via netis_data()'s tie_meta.",
        combine, paste(distinct_semantics, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (any(weighted_flags) && any(!weighted_flags)) {
    stop(
      sprintf(
        "multiplex(..., combine = \"%s\") cannot combine a weighted network with a binary one -- a 0/1 presence value summed with a continuous strength score has no coherent interpretation. Binarize the weighted component(s) first, or use a presence-based combine rule (union/intersection/k_of_n/count).",
        combine
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
