# The exposure conduit.
#
# `via =` accepts a tie-variable NAME or a network specification -- `binarize()`, `symmetrize()`,
# `multiplex()` -- which is what makes "exposure from either direction"
# expressible at all:
#
#     via = symmetrize(advice, rule = "any")
#
# WHY THIS AND NOT A THIRD `direction` VALUE. "Both directions" is a DERIVED
# NETWORK, not a third reading of the stored rows. Symmetrization is an explicit
# recorded choice, and "tie if either exists"
# differs from "tie only if both do" -- a distinction a direction flag has
# nowhere to put. Burying it in an argument that otherwise only declares what a
# row means would hide a substantive modeling decision in a spelling.
#
# WHY THE CONDUIT STAYS A CHARACTER FOR THE COMMON CASE. A plain tie-variable
# name is stored as a character string, and only a DERIVED conduit carries a
# specification object. Normalizing everything to a spec would be tidier and would
# give every consumer of `spec$conduit` -- the dependence diagnostic, several
# message templates -- a second type to handle, for no gain in the case that
# accounts for nearly all use. The helpers below are what the derived case costs.

#' Normalize a `via` argument into a stored conduit
#'
#' Returns a character string for a bare tie-variable name and a network-specification object for a derived
#' network.
#'
#' @noRd
.as_conduit <- function(via) {
  if (is.character(via)) {
    if (length(via) != 1L) {
      stop("A conduit given as a name must be a single tie-variable name.", call. = FALSE)
    }
    return(via)
  }
  if (inherits(via, "netis_network_ref")) {
    # A bare symbol captured by NSE is just a name; store it as one so nothing
    # downstream has to special-case the ordinary path.
    return(via$variable)
  }
  if (inherits(via, "netis_network_spec")) {
    return(via)
  }
  stop(
    "via must be a tie-variable name, or a binarize()/symmetrize()/multiplex() network specification.",
    call. = FALSE
  )
}

#' Is this conduit a derived network rather than a plain tie variable?
#' @noRd
.conduit_is_derived <- function(conduit) {
  inherits(conduit, "netis_network_spec") && !inherits(conduit, "netis_network_ref")
}

#' A short readable label for a conduit, for names and messages
#'
#' A derived conduit has no single variable name, so one is built from the
#' construction it describes. This is the answer to "what does
#' `ex_<content>_<conduit>` mean when the conduit is a `multiplex()` of three
#' networks" -- it must be DEFINED rather than defaulted, because a fallback
#' that happened to produce a string would produce a different one for each
#' construction and none of them stable.
#'
#' @noRd
.conduit_label <- function(conduit) {
  if (is.character(conduit)) {
    return(conduit)
  }
  if (inherits(conduit, "netis_network_ref")) {
    return(conduit$variable)
  }
  if (!is.null(conduit$name) && nzchar(conduit$name)) {
    return(conduit$name)
  }
  kind <- if (inherits(conduit, "netis_binarize_spec")) {
    "bin"
  } else if (inherits(conduit, "netis_symmetrize_spec")) {
    "sym"
  } else if (inherits(conduit, "netis_multiplex_spec")) {
    "mplex"
  } else {
    "net" # nocov
  }
  vars <- .conduit_variables(conduit)
  paste(c(kind, vars), collapse = "_")
}

#' Every tie variable a conduit ultimately reads
#'
#' Consumers that need to know which columns are involved (the network-timing
#' measurement, most importantly) ask this rather than assuming the conduit is
#' one name. A `multiplex()` conduit genuinely reads several.
#'
#' @noRd
.conduit_variables <- function(conduit) {
  if (is.character(conduit)) {
    return(conduit)
  }
  if (inherits(conduit, "netis_network_ref")) {
    return(conduit$variable)
  }
  # `binarize()`/`symmetrize()` nest a single network under `network`;
  # `multiplex()` holds several under `components`. Both field names are checked
  # by name rather than by guessing at the structure, and an unknown spec shape
  # returns nothing rather than silently reporting the wrong variables.
  inner <- conduit$network
  if (is.null(inner)) {
    inner <- conduit$components
  }
  if (is.null(inner)) {
    return(character(0)) # nocov
  }
  if (is.list(inner) && !inherits(inner, "netis_network_spec")) {
    return(unique(unlist(lapply(inner, .conduit_variables), use.names = FALSE)))
  }
  .conduit_variables(inner)
}

#' Build the conduit's DECLARED-orientation matrix for one (group, wave)
#'
#' Declared orientation means row = source, column = target -- the substantive
#' reading of the stored rows, before any exposure convention is applied. The
#' orientation step is separate and happens exactly once, in
#' `.orient_canonical()`.
#'
#' A plain name is built directly from the already-filtered tie rows. A derived
#' conduit goes through `.resolve_network()`, the same resolver
#' `centrality()`/`network_stat()` use -- so a `binarize()` conduit means the
#' same thing in an exposure term as it does in a centrality term, rather than
#' the two layers each having their own idea of what binarization is.
#'
#' @noRd
.conduit_declared_matrix <- function(conduit, ties_g, ids, source_col, target_col,
                                      data, group, wave, directed) {
  if (!.conduit_is_derived(conduit)) {
    return(.build_declared_matrix(
      ties_g, ids,
      source_col = source_col, target_col = target_col,
      value_col = .conduit_label(conduit), directed = directed
    ))
  }
  resolved <- .resolve_network(conduit, data, group, wave)
  # The construction layer resolves against its own node roster for the
  # (group, wave); the exposure engine has already fixed `ids`. Align by NAME,
  # never by position, since the two orderings need not agree.
  M <- matrix(0, nrow = length(ids), ncol = length(ids), dimnames = list(ids, ids))
  shared <- intersect(ids, resolved$ids)
  if (length(shared) > 0L) {
    M[shared, shared] <- resolved$M[shared, shared, drop = FALSE]
  }
  M
}

#' Orient a declared-orientation matrix into the exposure convention
#'
#' THE ONE TRANSPOSITION POINT IN THE PACKAGE. Every exposure
#' matrix, focal or historical, plain conduit or derived, passes through here
#' and nowhere else.
#'
#' `W[i, j]` is "the strength with which j influences i", so under
#' `source_influences_target` a stored row (source = A, target = B) means A
#' influences B and contributes to `W[B, A]` -- the transpose of the declared
#' matrix. The other reading needs no transposition.
#'
#' @noRd
.orient_canonical <- function(M, direction) {
  if (identical(direction, "source_influences_target")) t(M) else M
}
