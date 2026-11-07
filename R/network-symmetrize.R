# symmetrize() (R/network-spec.R).
#
# Builds a derived computational network; the canonical network in netis_data
# is never mutated.

#' Symmetrize a directed network into an undirected derived network
#'
#' Used inside network-construction expressions ([binarize()], [multiplex()])
#' and any term that takes a network ([exposure()], [centrality()],
#' [network_stat()], [latent()]), for a metric that requires (or the user
#' chooses) an undirected structure. Builds a **derived** matrix; it never
#' mutates the underlying `netis_data` object.
#'
#' `"any"`/`"both"` are presence-oriented and always produce a binary network,
#' with no weight semantics, regardless of whether the input is weighted. `"max"`/`"min"`/`"sum"`/
#' `"mean"` are value-combining and are substantively different operations on
#' a weighted network, and no one of them is "correct", so `rule` has no default:
#' leaving it out is an error listing the choices.
#'
#' # On an undirected network
#'
#' Undirected data stores one row per pair, and rnetis writes that row's value
#' to both `M[i, j]` and `M[j, i]` before any rule is applied (see
#' Undirected data on [netis_data()]). The two directions therefore always
#' agree: `"max"`, `"min"` and `"mean"` return the network unchanged, `"any"`
#' and `"both"` mark every tie present, and `"sum"` doubles every value,
#' because it adds the two (equal) directions.
#'
#' @param network a tie-variable name, or a [binarize()]/[symmetrize()]/
#'   [multiplex()] result to derive from.
#' @param rule how to combine `M[i,j]` and `M[j,i]`. Required. One of:
#'   \itemize{
#'     \item `"max"`: the larger of the two (a tie counts at its declared
#'       weight if either direction reports it).
#'     \item `"min"`: the smaller of the two.
#'     \item `"sum"`: the two added.
#'     \item `"mean"`: the two averaged -- on a directed network this halves
#'       the weight of any tie recorded in only one direction. Use
#'       deliberately, not by default.
#'     \item `"any"`: `1` if either direction is nonzero, else `0`.
#'     \item `"both"`: `1` only if both directions are nonzero (reciprocal
#'       ties only), else `0`.
#'   }
#' @param name optional name for the derived network.
#' @return an object of class `netis_symmetrize_spec` (and `netis_network_spec`).
#' @seealso [binarize()], [multiplex()]
#' @examples
#' symmetrize(communication, rule = "max")
#' symmetrize(communication, rule = "both")
#' @export
symmetrize <- function(network, rule, name = NULL) {
  choices <- c("max", "min", "sum", "mean", "any", "both")
  listed <- paste(sprintf("\"%s\"", choices), collapse = ", ")
  if (missing(rule)) {
    stop(
      sprintf(
        "symmetrize() requires rule =, one of %s. They are different operations on a weighted network, so none of them is assumed.",
        listed
      ),
      call. = FALSE
    )
  }
  if (!is.character(rule) || length(rule) != 1L || !(rule %in% choices)) {
    stop(sprintf("symmetrize(rule = ) must be one of %s.", listed), call. = FALSE)
  }
  inner <- .capture_network_arg(substitute(network), parent.frame())
  structure(
    list(network = inner, rule = rule, name = name),
    class = c("netis_symmetrize_spec", "netis_network_spec")
  )
}

#' @noRd
.resolve_network_symmetrize <- function(spec, data, group, wave) {
  inner <- .resolve_network(spec$network, data, group, wave)
  S <- .symmetrize_matrix(inner$M, spec$rule)
  # "any" and "both" return 0/1 cells, whose values say nothing about the
  # source's weights; keeping its `weighted` flag and semantics would send them
  # to a weighted computation under a declaration they no longer carry.
  binary <- spec$rule %in% c("any", "both")
  list(
    M = S, ids = inner$ids, directed = FALSE,
    weighted = if (binary) FALSE else inner$weighted,
    weight_semantics = if (binary) "unitless" else inner$weight_semantics,
    variable = inner$variable,
    # descriptor/ties_snapshot are deliberately NOT propagated: symmetrization
    # can populate a cell that had no row in either raw direction's snapshot
    # alone (its mirror did), so the raw one-directional tie rows do not
    # correspond cell-for-cell to the resolved matrix. presence()
    # must therefore refuse on a symmetrized network exactly as it
    # does on any other derived network -- descriptor = NULL is what makes
    # .apply_presence_rule() (R/network-binarize.R) refuse correctly here,
    # not just for multiplex(). `variable` stays propagated as provenance (a
    # symmetrized network is still "about" one underlying tie variable), but
    # note data-derived thresholds are refused on ANY derived network at every
    # scope -- .scoped_value_pool() keys its refusal on ties_snapshot, which
    # is NULL here -- because the resolved cells do not correspond
    # one-to-one to observed values.
    descriptor = NULL, ties_snapshot = NULL,
    resolved = list(type = "symmetrize", rule = spec$rule, inner = inner$resolved)
  )
}

#' @noRd
.symmetrize_matrix <- function(M, rule) {
  Mt <- t(M)
  S <- if (rule == "max") {
    pmax(M, Mt)
  } else if (rule == "min") {
    pmin(M, Mt)
  } else if (rule == "sum") {
    M + Mt
  } else if (rule == "mean") {
    (M + Mt) / 2
  } else if (rule == "any") {
    ((M != 0) | (Mt != 0)) * 1
  } else if (rule == "both") {
    ((M != 0) & (Mt != 0)) * 1
  } else {
    stop(sprintf("Unknown symmetrize rule '%s'.", rule), call. = FALSE) # nocov
  }
  dimnames(S) <- dimnames(M)
  S
}
