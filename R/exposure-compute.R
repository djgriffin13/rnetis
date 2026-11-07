#' Compute a network exposure term directly
#'
#' Calculates network exposure -- how strongly each node is exposed to a
#' characteristic of the alters it is connected to -- without fitting any
#' model. This is useful for inspection, teaching, debugging, and
#' reproducibility: you can see exactly what an Influence model's exposure
#' predictor looks like before (or instead of) fitting anything.
#'
#' `compute_exposure()` builds the same internal exposure specification and
#' resolves it through the same engine [fit_influence()] uses for its
#' `exposure()` terms. There is exactly one exposure resolution code path in
#' the package; this function and model fitting cannot compute different
#' answers for the same specification, because they are the same
#' computation.
#'
#' # Default mathematics
#'
#' With `normalize = "none"` (the default), exposure for node `i` is
#' \deqn{E_i = \sum_j W_{ij} X_j}
#' the raw weighted sum over alters `j`. With `normalize = "row"`, this is
#' divided by \eqn{\sum_j W_{ij}}, giving average rather than total exposure.
#' These are different substantive quantities and are never interchanged
#' silently -- see the Direction and Isolates sections below.
#'
#' # Direction
#'
#' There is no single universally correct way to turn a `(source, target)` tie
#' into the exposure convention \eqn{W_{ij}} = "the strength with which `j`
#' influences `i`" -- it depends on what the tie means. `direction` makes this
#' explicit:
#' \itemize{
#'   \item `"source_influences_target"` (default): a row with `source = A,
#'     target = B` means A influences B. Appropriate for ties like "sends
#'     information to" or "informs".
#'   \item `"target_influences_source"`: the same row means B influences A.
#'     Appropriate for ties like "seeks advice from" or "receives
#'     communication from", where the tie's target is the one exerting
#'     influence.
#' }
#' Picking the wrong one silently reverses who is exposed to whom, so check
#' this against how your tie variable was actually defined.
#'
#' # Incoming, outgoing, and both
#'
#' `direction` declares what a stored row *means*, and that settles which
#' neighbors contribute:
#'
#' | Intent | How to say it |
#' |---|---|
#' | exposure from **incoming** ties | `direction = "source_influences_target"` |
#' | exposure from **outgoing** ties | `direction = "target_influences_source"` |
#' | exposure from ties in **either** direction | `via = symmetrize(<network>, rule = )` |
#'
#' "Both" is a **derived network**, not a third `direction` value. Symmetrizing
#' is a substantive choice -- "tied if either direction exists" and "tied only
#' if both do" are different graphs, and mean/sum/max of the two weights are
#' different weighted graphs again -- so it is made explicitly, with the rule
#' recorded, rather than hidden inside a flag that otherwise only declares a
#' reading of the data.
#'
#' # Derived conduits
#'
#' `via` accepts a tie-variable name, or a network built with [binarize()],
#' [symmetrize()], or [multiplex()]:
#'
#' ```r
#' compute_exposure(nd, "attitude", via = symmetrize(advice, rule = "any"))
#' compute_exposure(nd, "attitude", via = binarize(contact, rule = threshold(3)))
#' ```
#'
#' These resolve through the same construction layer [centrality()] and
#' [network_stat()] use, so a binarized network means the same thing in an
#' exposure term as it does anywhere else in the package. The resulting column
#' is named for the construction (`ex_attitude_sym_advice`); pass `name` to
#' choose your own.
#'
#' # Isolates
#'
#' A node with no reference-set members (no relevant ties) has undefined
#' average exposure under row normalization, since the denominator is zero.
#' `isolate_rule` controls what such a node gets: `"na"` (default, `NA`),
#' `"zero"`, or `"error"` (refuse to compute at all while isolates are
#' present). Under `normalize = "none"`, isolates always get `0` with no
#' ambiguity, since an empty sum is `0`. A row whose weights are partly missing,
#' or whose signed weights cancel to zero, is not an isolate: its denominator is
#' undefined and its exposure is `NA` whatever `isolate_rule` says.
#'
#' # An unobserved network is not an empty one
#'
#' An isolate is a node observed to have no ties. That is a different thing
#' from a network nobody recorded, and the two must not get the same answer:
#' exposure `0` for every node at a wave that was never measured would
#' attenuate an estimated exposure effect toward zero with nothing to show for
#' it.
#'
#' When the conduit is **dynamic** (its values vary by wave, so ties are
#' wave-indexed), a group with an observed network at some waves and no record of
#' one at the requested wave is treated as unobserved there: exposure is `NA` for
#' every node in that group, and one warning names every affected group-wave
#' snapshot. `NA` rather than an error, because the data is incomplete rather
#' than malformed -- [fit_influence()] then drops those rows and reports them
#' like any other missingness.
#'
#' A wave **recorded as observed** with no ties is an empty network, not an
#' unobserved one: every node there is an isolate, so exposure is `0` under
#' `normalize = "none"` and follows `isolate_rule` otherwise, with no warning.
#' An all-zero matrix in [netis_data_from_adjacency_series()] and an edgeless
#' graph in [netis_data_from_igraph_list()] are recorded this way, and for a tie
#' table `netis_data(observed_tie_waves = )` declares it. See Empty and unobserved
#' waves on [netis_data()].
#'
#' This is deliberately narrow. A **static** conduit is reused across waves
#' and so has no per-wave existence to be missing. A group with no observed network
#' at *any* wave has not established that its network exists at all, so absence
#' cannot be inferred there. An isolate inside an observed network still gets
#' `0`. None of those cases produces `NA`.
#'
#' A path-dependent mechanism such as [bi()] reads the *prior* waves, so a focal
#' wave with no ties of its own still has a well-defined exposure. The same test
#' applies to those prior waves instead: one with no ties, in a group with ties at
#' other waves, makes the exposure `NA` for every node in that group, and the
#' warning names that wave.
#'
#' # Self-ties
#'
#' A self-tie is an ordinary tie to this function, so a node's own value would
#' count as exposure to an alter. Under `self_ties = "prohibited"`, the default in
#' [netis_data()], a self-tie row with a nonzero value is refused. Under
#' `self_ties = "permitted"` it enters the sum, and the node is exposed to its own
#' value. [bi()] refuses a self-loop in either case, because `gamma` already
#' supplies that quantity.
#'
#' # Custom kernels
#'
#' `mechanism` may be a `function(context, spec)`, called once per group.
#' `context$W` is the group's weight matrix, oriented so that `W[i, j]` is the
#' strength with which `j` influences `i`, with any normalization already applied;
#' `context$X` is the content vector in the same node order. Either may hold `NA`.
#' The function returns a numeric vector of `length(context$X)`, or a matrix with
#' that many rows and named columns, one output column each.
#'
#' Two attributes on the function declare what it needs:
#' `attr(kernel, "requires")` is `"focal"` (the default), or
#' `c("focal", "w_history", "x_history")` to also receive `context$W_history` and
#' `context$X_history`, lists holding the waves before the requested one, most
#' recent first. Nothing else is accepted. `attr(kernel, "supports_normalize")` is
#' `"row"` to allow `normalize = "row"`, which is otherwise refused.
#'
#' @param data a [netis_data()] object.
#' @param content name of the node variable whose values flow through the
#'   network (the alter characteristic).
#' @param via the conduit: a tie-variable name, or a network specification
#'   built with [binarize()], [symmetrize()], or [multiplex()]. See Derived
#'   conduits below, and Incoming, outgoing, and both for how `symmetrize()`
#'   expresses exposure from either direction.
#' @param name name to give the resulting exposure column. Defaults to
#'   `"ex_<content>_<via>"`. A mechanism with several columns names its own; a
#'   `name` given here prefixes them.
#' @param wave which wave to compute exposure for, if `data` has a wave
#'   dimension. Required whenever `data` has any waves at all (even if the
#'   specific variables requested happen to be static), and must be `NULL`
#'   when `data` has no wave dimension. This is deliberately explicit rather
#'   than guessed. With `conduit_wave = NULL` (the default), both the content
#'   and the network are read at this wave.
#' @param conduit_wave the wave the NETWORK is read at, when it differs from
#'   the wave the content is read at — the direct form of
#'   `exposure(..., x_lag =, w_lag =)` with unequal lags in a
#'   [fit_influence()] formula. `NULL` (default) means the same wave as
#'   `wave`. The output keeps one row per node observed at `wave` (the
#'   content wave). A tie at `conduit_wave` reaching a node of the group with no
#'   row at `wave` is kept: that node's content is missing, so an exposure that
#'   depends on it is `NA`.
#'   Refused for history-accumulating mechanisms such as [bi()], which define
#'   for themselves which historical networks enter the accumulation. For a
#'   static conduit any value gives the same network, so the distinction only
#'   matters when the conduit varies by wave.
#' @param mechanism `"linear"` (default, the ordinary weighted sum), a kernel from
#'   [bi()] or [bi_components()], or a custom kernel function: see Custom kernels.
#' @param normalize `"none"` (default) or `"row"`.
#' @param direction `"source_influences_target"` (default) or
#'   `"target_influences_source"`. See Direction above.
#' @param isolate_rule `"na"` (default), `"zero"`, or `"error"`. Only relevant
#'   when `normalize = "row"`. See Isolates above.
#' @param augment if `FALSE` (default), return a data frame with just the
#'   group, node, and exposure columns. If `TRUE`, return the full node data
#'   (as from [netis_nodes()]) with the exposure column added.
#'
#' @return a data frame. See `augment`. The group and node columns have the type
#'   they have in the node data. Rows are ordered by group then node,
#'   with both sorted as **character strings** -- numeric-looking IDs sort
#'   lexicographically (`"10"` before `"2"`), not numerically. This affects
#'   row order only; every value is matched to its node by name internally,
#'   never by position, so this never changes which exposure value belongs to
#'   which node.
#' @references
#' Friedkin, N. E. and Johnsen, E. C. (1990) Social influence and opinions.
#' *Journal of Mathematical Sociology* **15**(3-4), 193-206.
#'
#' Marsden, P. V. and Friedkin, N. E. (1993) Network studies of social
#' influence. *Sociological Methods & Research* **22**(1), 127-151.
#'
#' Leenders, R. Th. A. J. (2002) Modeling social influence through network
#' autocorrelation: constructing the weight matrix. *Social Networks* **24**(1),
#' 21-47. For row normalization.
#' @seealso [netis_data()], [netis_nodes()], [fit_influence()], [exposure()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = c("a", "b", "c"), negative_affect = c(0.2, -0.5, 1.1)
#' )
#' ties <- data.frame(
#'   group = 1,
#'   source = c("a", "b"), target = c("b", "c"),
#'   info_receipt = c(1, 0.5)
#' )
#' nd <- netis_data(nodes, ties)
#'
#' # b's exposure = 1 * negative_affect[a] (a -> b, a influences b)
#' compute_exposure(nd, content = "negative_affect", via = "info_receipt")
#'
#' # same computation, merged onto the original node data
#' compute_exposure(nd, "negative_affect", "info_receipt", augment = TRUE)
#' @export
compute_exposure <- function(data, content, via, ...) {
  UseMethod("compute_exposure")
}

#' @rdname compute_exposure
#' @param ... passed to the method.
#' @export
compute_exposure.netis_data <- function(data, content, via, name = NULL, wave = NULL,
                              conduit_wave = NULL,
                              mechanism = "linear",
                              normalize = c("none", "row"),
                              direction = c("source_influences_target", "target_influences_source"),
                              isolate_rule = c("na", "zero", "error"),
                              augment = FALSE, ...) {
  .check_netis_data(data)
  normalize <- match.arg(normalize)
  direction <- match.arg(direction)
  isolate_rule <- match.arg(isolate_rule)
  .assert_flag(augment, "augment")

  spec <- .new_exposure_spec(
    content = content, conduit = via, name = name,
    normalize = normalize, mechanism = mechanism,
    isolate_rule = isolate_rule, direction = direction
  )

  result <- .resolve_exposure(spec, data, wave = wave, conduit_wave = conduit_wave)

  if (!augment) {
    return(result)
  }

  s <- netis_schema(data)
  nodes <- netis_nodes(data)
  if (!is.null(wave) && !is.null(s$wave_col) && s$wave_col %in% names(nodes)) {
    nodes <- nodes[nodes[[s$wave_col]] == wave, , drop = FALSE]
  }
  out <- merge(nodes, result, by = c(s$group_col, s$node_col), all.x = TRUE, sort = FALSE)
  out
}
