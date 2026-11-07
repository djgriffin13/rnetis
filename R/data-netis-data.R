#' Construct a canonical rnetis data object
#'
#' `netis_data()` builds the single data structure that both [fit_influence()]
#' and [fit_selection()] consume. It holds node, tie, and (optionally) group data at
#' their natural grain -- nodes, ties, and groups are never flattened into one
#' duplicated table -- and it never renames a node characteristic to "ego" or
#' "alter": which role a variable plays is decided by model terms, not by storage.
#'
#' # Static and dynamic variables
#'
#' If `wave_col` is supplied, a variable is treated as *dynamic* when its value
#' actually changes across waves within an entity, and *static* when it does not.
#' This classification only affects internal storage (static values are held once
#' per entity rather than duplicated at every wave); it never affects what
#' [netis_nodes()], [netis_ties()], or [netis_groups()] return, which is always the
#' full logical long-format table.
#'
#' # Wave order
#'
#' The order of the waves is read from the labels' type, once, when the object
#' is built:
#'
#' - numbers and dates run in increasing order;
#' - a factor runs in the order of its levels, and factor columns in several
#'   tables must agree on that order;
#' - text that reads as numbers (`"9"`, `"10"`) runs by numeric value, whether or
#'   not another table stores the same waves as numbers;
#' - text that does not read as numbers has no order rnetis can know, so two or
#'   more such labels are refused: supply them as a factor whose levels give the
#'   time order.
#'
#' Missing wave labels are refused. Every lag and history computation steps
#' through this order by position, never by subtracting labels, so labels 1, 3, 5
#' are three consecutive occasions and no spacing is implied.
#'
#' The waves are those found in any table. A table with no wave column holds at
#' every wave. A table with a wave column that covers only some of the waves is
#' not reused at the others: a tie table observed at wave 1 only describes the
#' network at wave 1, and the network at the other waves is unobserved.
#'
#' # Empty and unobserved waves
#'
#' A group's network at a wave is in one of three states when ties carry a wave
#' column:
#'
#' - **observed, with ties**: the group has tie rows at that wave;
#' - **observed and empty**: the group has no tie rows at that wave, and the
#'   wave is recorded as observed. An all-zero matrix given to
#'   [netis_data_from_adjacency_series()] and an edgeless graph given to
#'   [netis_data_from_igraph_list()] are recorded this way, and for a tie table
#'   `observed_tie_waves` says so. Every node there is an isolate, and exposure
#'   follows the isolate rule;
#' - **unobserved**: the group has no tie rows at that wave, is not recorded as
#'   observed there, and has an observed network at another wave. Exposure and
#'   network features there are `NA` with a warning, a Selection fit treats the
#'   dyads as missing, and the exports warn and return no ties.
#'
#' A group with no observed network at any wave has established nothing either
#' way, so its nodes are treated as isolates and [diagnose_data()] notes it. The
#' record of observed networks is `netis_schema(x)$observed_tie_waves`.
#'
#' # Undirected data
#'
#' Undirected tie data stores **one row per unordered pair** (per group, and per
#' wave when ties carry one), with the endpoints in either column order. Storing
#' both `a -> b` and `b -> a` is an error. Every network rnetis builds from
#' undirected data -- exposure, network features, adjacency export, graph export,
#' `latent()` -- reads that one row as a tie in both directions, with its stored
#' value (negative values included) on both. `mode` and the two `direction`
#' readings therefore make no difference on undirected data.
#'
#' # The node roster
#'
#' A node is a (group, node) pair: the same label in two groups is two nodes. A
#' group's **roster** is every node with a row for that group in `node_data` at
#' any wave, and a node table with no wave column puts every node on the roster
#' at every wave.
#'
#' A tie naming a node that is not on its group's roster is accepted here and
#' refused by every function that builds a network from it, because it cannot be
#' placed; [diagnose_data()] lists all of them. A node on the roster with no ties
#' is an isolate.
#'
#' A node on the roster may have no row at a particular wave. What then happens
#' depends on what is being built:
#'
#' - [compute_exposure()], [compute_centrality()] and [compute_network_stat()]
#'   report one row per node with a row at that wave. A tie at that wave that
#'   reaches a rostered node without a row is still a tie: the node stays in the
#'   network as a vertex, its alters' degree and strength count it, its content
#'   is missing, and an exposure that depends on that content is `NA`.
#' - [netis_adjacency()] keeps the whole roster at every wave, so that matrices
#'   from two waves of a group are conformable.
#' - [as_igraph()] and [as_network()] use the node rows at the selected wave as
#'   the vertex set, so a tie reaching a rostered node without a row there is an
#'   error.
#'
#' # Direction
#'
#' `netis_data()` preserves tie direction exactly as supplied: `source_col` and
#' `target_col` are never swapped or reinterpreted here. Converting to the
#' canonical exposure-mathematics convention (`W[i,j]` = the strength with which
#' node `j` influences node `i`) is the exposure engine's job, not this
#' constructor's -- see [compute_exposure()]'s Direction section.
#'
#' # What is and is not validated here
#'
#' This constructor checks that the object is *structurally* well formed: required
#' columns are present, no identifier is missing (a group, node, source or target
#' that is `NA` is refused), and no entity key (node, tie, or group -- crossed
#' with wave if supplied) is duplicated -- including, for undirected data, the same
#' unordered pair stored in both orientations. It does **not** check that tie
#' endpoints are on the roster (see The node roster), check the declared
#' `self_ties` policy (every function that builds a network refuses a nonzero
#' self-tie under `"prohibited"`), or flag isolates, missing waves, or other
#' design-level concerns. Those belong to [diagnose_data()] and
#' [diagnose_design()], run separately, because they are about whether an
#' otherwise well-formed dataset supports the analysis a user is attempting --
#' not about whether the object can be constructed at all.
#'
#' # Errors and refusals
#'
#' `netis_data()` itself refuses only on the shape of what it was handed:
#' `node_data` and `tie_data` must be data frames, `group_data` a data frame or
#' `NULL`, and `tie_meta` / `derived_meta` named lists keyed by variable name or
#' `NULL`. Each names the offending argument.
#'
#' It also refuses what would make the object's keys or declarations unreadable:
#' a missing or duplicated key, a wave column it cannot order, a `tie_meta` entry
#' that does not fit (see `tie_meta`), and a `time_meta` that contradicts the wave
#' order.
#'
#' **Undetermined edge presence is recorded here, not refused here**, and the
#' distinction is deliberate. A `complete_dyadic` network with neither
#' `presence_col` nor `presence_rule` is a perfectly constructible object: the
#' rows are observations of dyads, and nothing is wrong until something asks
#' which dyads are *tied*. The refusal therefore belongs to the operations that
#' need presence — [as_network()], [as_igraph()], `binarize(rule = presence())`,
#' the measures computed from presence such as degree and density, the network
#' plots, and adjacency extraction with `presence = TRUE` — each of which errors
#' naming the tie variable and the ways to resolve it (declare `presence_col` or
#' `presence_rule` here, or declare a `presence_mode` that fits the data).
#' Refusing at construction instead would reject data that is entirely usable
#' for every analysis that never asks about presence.
#'
#' @param node_data a data frame of node-level data in long format. Must contain
#'   `group_col` and `node_col`, and `wave_col` if supplied. One row per node (per
#'   wave, if `wave_col` is used).
#' @param tie_data a data frame of tie-level data in long format. Must contain
#'   `group_col`, `source_col`, and `target_col`, and `wave_col` if supplied. One
#'   row per tie (per wave, if `wave_col` is used). Two rows for the same ordered
#'   pair within one wave are refused: there is no multi-edge representation.
#' @param group_data optional data frame of group-level data in long format. Must
#'   contain `group_col`, and `wave_col` if supplied.
#' @param group_col name of the group/network identifier column, shared
#'   across `node_data`, `tie_data`, and `group_data`. Default `"group"`.
#' @param node_col name of the node identifier column in `node_data`. Default
#'   `"node"`.
#' @param wave_col name of the wave/time identifier column, shared across all
#'   supplied data frames, or `NULL` (the default) if the data has no wave
#'   dimension. See Wave order.
#' @param source_col,target_col names of the tie source and target columns in
#'   `tie_data`. Defaults `"source"` and `"target"`.
#' @param directed `TRUE` (default) if ties are directed, `FALSE` if undirected.
#'   See Undirected data.
#' @param self_ties one of `"prohibited"` (default) or `"permitted"`. Not checked
#'   against the data by this constructor; under `"prohibited"`,
#'   every function that builds a network from the ties refuses a self-tie row
#'   with a nonzero value, and [diagnose_data()] reports where the data and this
#'   policy disagree. A stored zero on the diagonal of a complete dyadic
#'   table is not a tie and is accepted.
#' @param tie_meta optional named list, keyed by tie-variable name (a column of
#'   `tie_data` other than the source/target/group/wave keys), declaring
#'   per-network semantics that cannot be safely inferred from values alone.
#'   Each entry may set
#'   any of:
#'   \itemize{
#'     \item `weight_semantics`: `"strength"`, `"distance"`, `"signed"`,
#'       `"count"`, or `"unitless"` (default) -- what a larger value means.
#'       Left undeclared, defaults to `"unitless"`, which makes no claim. A
#'       measure whose reading of a weight conflicts with a declared meaning
#'       is refused (see [compute_centrality()]); an undeclared one is not.
#'     \item `direction_semantics`: free text describing the substantive
#'       direction, e.g. `"source sends information to target"`.
#'     \item `zero_meaning`: `"measured_zero"`, `"structural_absence"`, or
#'       `"unknown"` (default).
#'     \item `presence_mode`: `"row_presence"` (a row means the tie exists;
#'       inferred automatically for data shaped like a sparse edge list),
#'       `"complete_dyadic"` (rows exist for most/all dyads regardless of tie
#'       status; inferred automatically for data shaped like a round-robin
#'       design -- row existence alone does **not** establish presence in
#'       this case), `"indicator"` (an explicit presence column is
#'       authoritative; requires `presence_col`), or `"value_rule"` (a
#'       declared value-based rule defines presence; requires
#'       `presence_rule`). Left undeclared, inferred from row completeness
#'       against the node roster.
#'     \item `presence_col`: column name of an explicit presence indicator,
#'       for `"indicator"` or `"complete_dyadic"`.
#'     \item `presence_rule`: `function(value)` returning `TRUE`/`FALSE` per
#'       row, for `"value_rule"` or `"complete_dyadic"`.
#'     \item `weighted`: override the inferred weighted/binary
#'       classification, if needed.
#'   }
#'   `presence_col` and `presence_rule` each belong to a presence mode that
#'   reads them, so a combination in which one of them would be ignored is an
#'   error, as is an unknown field or a `weighted` that is not `TRUE`/`FALSE`.
#'
#'   `weighted` and `presence_mode` are inferred automatically when not
#'   declared. A table covering at least 90% of the possible dyads (at the waves
#'   it has rows for) is inferred `complete_dyadic`, unless no row in it can be a
#'   non-tie: when `zero_meaning = "structural_absence"` is declared and no zero
#'   is stored (a stored row is then a tie, as in the adjacency importers), or
#'   when every possible dyad has a row holding `1`. The 90% cutoff is a package
#'   convention, as are the presence-mode names; neither comes from a published
#'   source. A `complete_dyadic` network with neither `presence_col` nor
#'   `presence_rule` has **undetermined** edge presence, and any operation that
#'   needs presence (e.g. `netis_adjacency(..., presence = TRUE)`) refuses rather
#'   than defaulting to `value != 0`. So does an inferred `row_presence` table
#'   that stores zeros, unless `zero_meaning = "measured_zero"` is declared.
#'   Below the cutoff the inference suggests a reading; it does not decide
#'   whether a stored zero is a tie.
#'
#'   Which of these you declared, which rnetis inferred, and which remain
#'   undeclared is recorded and readable with [netis_boundary()].
#' @param time_meta optional data frame declaring **metric observation times**,
#'   one row per group and wave, with columns `wave_col`, `time_value`, and
#'   `group_col` (which may be omitted when there is exactly one group). A
#'   `time_unit` column may be given instead of the `time_unit` argument.
#'
#' This is **metadata, not a modeling coordinate.** rnetis models time as
#' ordered wave *positions*: `lagged(x, 1)` is the previous observed occasion
#' whether the gap was a day or a decade, and supplying `time_meta` does not
#' change that or any estimate. It is recorded for interoperability,
#' provenance, and plotting. Times must increase across the observed wave order
#' within each group; one that runs backwards contradicts the wave sequence
#' rather than describing it, and is an error. See [netis_time_index()].
#' @param
#' time_unit unit for `time_meta$time_value` -- `"day"`, `"week"`, ... .
#' Required when `time_meta` is supplied without its own `time_unit` column,
#' because a number without a unit is not an observation time.
#' @param observed_tie_waves optional data frame with columns `group_col` and
#'   `wave_col`, one row per (group, wave) whose network was **observed to have no
#'   ties**, so that it reads as an empty network rather than an unobserved one.
#'   See Empty and unobserved waves. Requires a wave column in `tie_data`. A wave
#'   named here that no table holds joins the object's waves.
#' @param
#' derived_meta internal. The derived-variable provenance registry, keyed by
#' derived-column name. Not meant to be supplied by hand -- [add_centering()]
#' and [add_decomposition()] populate it when they reconstruct a `netis_data`
#' object.
#'
#' @return An object of class `netis_data`: a list with four elements, and the
#'   accessors below rather than `$` are the supported way to read it, since the
#'   internal layout is free to change while the accessors are not.
#'   \describe{
#'     \item{`nodes`, `ties`, `groups`}{the node, tie and group tables, each
#'       split by group and wave rather than held as one flat frame; `groups`
#'       is `NULL` when no group table was supplied.}
#'     \item{`schema`}{the identifier columns, the per-tie-variable descriptors
#'       (direction semantics, zero meaning, weighted, `presence_mode`), and
#'       the time and derived-column metadata.}
#'   }
#'   Identifiers are stored and sorted as **character strings**, so `"10"`
#'   precedes `"2"`. This affects the row order of anything keyed by them and is
#'   presentational — join on identifiers rather than relying on position.
#'   Which descriptor values you declared and which rnetis inferred is recorded
#'   and readable with [netis_boundary()]: an inferred value is rnetis's best
#'   structural guess, not a declaration, and the two are never conflated.
#'
#' @seealso [netis_nodes()], [netis_ties()], [netis_groups()], [netis_schema()] to
#'   inspect the logical content and metadata of a `netis_data` object;
#'   [netis_boundary()] for what was declared versus inferred.
#'
#' @examples
#' nodes <- data.frame(
#'   group = c(1, 1, 1),
#'   node  = c("a", "b", "c"),
#'   trait = c(0.2, -0.5, 1.1)
#' )
#' ties <- data.frame(
#'   group  = c(1, 1),
#'   source = c("a", "b"),
#'   target = c("b", "c"),
#'   weight = c(1, 0.5)
#' )
#' nd <- netis_data(nodes, ties)
#' nd
#'
#' @export
netis_data <- function(node_data,
                        tie_data,
                        group_data = NULL,
                        group_col = "group",
                        node_col = "node",
                        wave_col = NULL,
                        source_col = "source",
                        target_col = "target",
                        directed = TRUE,
                        self_ties = "prohibited",
                        tie_meta = NULL,
                        time_meta = NULL,
                        time_unit = NULL,
                        observed_tie_waves = NULL,
                        derived_meta = NULL) {
  .assert_enum(self_ties, c("prohibited", "permitted"), "self_ties")
  .assert_flag(directed, "directed")
  if (!is.null(tie_meta) && (!is.list(tie_meta) || is.null(names(tie_meta)) || any(names(tie_meta) == ""))) {
    stop("tie_meta must be a named list, keyed by tie-variable name, or NULL.", call. = FALSE)
  }

  if (!is.data.frame(node_data)) stop("node_data must be a data frame.", call. = FALSE)
  if (!is.data.frame(tie_data)) stop("tie_data must be a data frame.", call. = FALSE)
  if (!is.null(group_data) && !is.data.frame(group_data)) {
    stop("group_data must be a data frame or NULL.", call. = FALSE)
  }

  # A tibble is accepted and stored as a plain data frame, so that internal
  # storage has one representation and `as =` on the accessors is the only thing
  # deciding what comes back out. See `.as_plain_data_frame()`.
  node_data <- .as_plain_data_frame(node_data)
  tie_data <- .as_plain_data_frame(tie_data)
  group_data <- .as_plain_data_frame(group_data)

  # wave_col is required as a *column* only in the data frames that actually
  # carry it. A dataset can legitimately have waves for ties but genuinely
  # time-invariant, single-row-per-node data -- forcing every input to carry a
  # (redundant) wave column would contradict "static data is first-class".
  # Presence is therefore checked per data frame.
  base_node_key  <- c(group_col, node_col)
  base_tie_key   <- c(group_col, source_col, target_col)
  base_group_key <- group_col

  .assert_columns_present(node_data, base_node_key, "node_data")
  .assert_columns_present(tie_data, base_tie_key, "tie_data")
  if (!is.null(group_data)) {
    .assert_columns_present(group_data, base_group_key, "group_data")
  }

  has_wave <- function(df) !is.null(wave_col) && wave_col %in% names(df)
  node_wave  <- if (has_wave(node_data)) wave_col else NULL
  tie_wave   <- if (has_wave(tie_data)) wave_col else NULL
  group_wave <- if (!is.null(group_data) && has_wave(group_data)) wave_col else NULL

  .assert_identifier_columns(node_data, base_node_key, "node_data")
  .assert_identifier_columns(tie_data, base_tie_key, "tie_data")
  if (!is.null(group_data)) {
    .assert_identifier_columns(group_data, base_group_key, "group_data")
  }
  .assert_unique_key(node_data, c(base_node_key, node_wave), "node_data")
  .assert_unique_key(tie_data, c(base_tie_key, tie_wave), "tie_data")
  if (!directed) {
    .assert_undirected_pairs_unique(tie_data, group_col, source_col, target_col, tie_wave)
  }
  if (!is.null(group_data)) {
    .assert_unique_key(group_data, c(base_group_key, group_wave), "group_data")
  }

  observed_tie_waves <- .as_plain_data_frame(observed_tie_waves)
  .assert_observed_tie_waves(observed_tie_waves, node_data, group_col, wave_col, tie_wave)

  # The wave vocabulary comes FIRST, from every table at once, because the
  # static/dynamic split below judges each table's grid against the dataset's
  # waves: a table covering only some of them is not static, however complete
  # it looks over its own waves. Its order is the labels' own -- numeric,
  # factor levels, or numeric text read as numbers -- never `c()` of mixed
  # types sorted as text, which puts wave 10 before wave 9.
  waves <- if (is.null(wave_col)) {
    NULL
  } else {
    .wave_vocabulary(list(
      node_data = if (!is.null(node_wave)) node_data[[wave_col]],
      tie_data = if (!is.null(tie_wave)) tie_data[[wave_col]],
      group_data = if (!is.null(group_wave)) group_data[[wave_col]],
      observed_tie_waves = if (!is.null(observed_tie_waves)) observed_tie_waves[[wave_col]]
    ))
  }

  node_split  <- .split_static_dynamic(node_data, base_node_key, node_wave, waves)
  tie_split   <- .split_static_dynamic(tie_data, base_tie_key, tie_wave, waves)
  group_split <- if (is.null(group_data)) {
    NULL
  } else {
    .split_static_dynamic(group_data, base_group_key, group_wave, waves)
  }

  # Validated here, against the resolved wave and group vocabularies, rather
  # than inside .build_schema(): a bad time declaration should be refused
  # naming what the user typed, before any storage decision has been taken.
  time_meta_resolved <- .validate_time_meta(
    time_meta, group_col, wave_col, waves,
    sort(unique(as.character(node_data[[group_col]]))), time_unit
  )

  if (!is.null(derived_meta) && (!is.list(derived_meta) || is.null(names(derived_meta)) ||
                                 any(names(derived_meta) == ""))) {
    stop("derived_meta must be a named list, keyed by derived-column name, or NULL.",
         call. = FALSE)
  }

  schema <- .build_schema(
    node_col = node_col, group_col = group_col, wave_col = wave_col,
    source_col = source_col, target_col = target_col,
    directed = directed, self_ties = self_ties, waves = waves,
    node_split = node_split, tie_split = tie_split, group_split = group_split,
    tie_data = tie_data, node_data = node_data, group_data = group_data,
    tie_meta = tie_meta, time_meta = time_meta_resolved, derived_meta = derived_meta,
    # A tie table the split collapsed holds at every wave, as one with no wave
    # column does, so no wave of it can be unobserved.
    observed_tie_waves = if (!is.null(tie_wave) &&
                               (nrow(tie_data) == 0L || !is.null(tie_split$dynamic))) {
      .build_observed_tie_waves(observed_tie_waves, tie_data, group_col, wave_col)
    }
  )

  structure(
    list(
      nodes = node_split,
      ties = tie_split,
      groups = group_split,
      schema = schema
    ),
    class = "netis_data"
  )
}

#' Logical accessors for a netis_data object
#'
#' Return the full long-format logical view of the nodes, ties, or groups held in
#' a [netis_data()] object, regardless of how static and dynamic variables are
#' split internally (see the Static and dynamic variables section of
#' [netis_data()]). These are the functions ordinary code should use to get data
#' out of a `netis_data` object; the internal static/dynamic split is not part of
#' the public interface and may change without notice as storage is optimized.
#'
#' @param x a [netis_data()] object.
#' @param as `"data.frame"` (default) or `"tibble"`. `"tibble"` requires the
#'   optional \pkg{tibble} package; a base `data.frame` is always available with
#'   no additional dependency.
#' @return a data frame (or tibble, if requested). `netis_groups()` returns
#'   `NULL` if no `group_data` was supplied at construction.
#' @seealso [netis_data()] to build the object these read from,
#'   [netis_schema()] for its structural declarations rather than its rows,
#'   [netis_adjacency()] to get a tie variable as a matrix instead
#' @name netis-accessors
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b"), trait = c(1, 2))
#' ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
#' nd <- netis_data(nodes, ties)
#' netis_nodes(nd)
#' netis_ties(nd)
NULL

#' @rdname netis-accessors
#' @export
netis_nodes <- function(x, as = c("data.frame", "tibble")) {
  .check_netis_data(x)
  s <- x$schema
  out <- .reconstruct_long(x$nodes, c(s$group_col, s$node_col), s$wave_col)
  .maybe_tibble(out, as)
}

#' @rdname netis-accessors
#' @export
netis_ties <- function(x, as = c("data.frame", "tibble")) {
  .check_netis_data(x)
  s <- x$schema
  out <- .reconstruct_long(x$ties, c(s$group_col, s$source_col, s$target_col), s$wave_col)
  .maybe_tibble(out, as)
}

#' @rdname netis-accessors
#' @export
netis_groups <- function(x, as = c("data.frame", "tibble")) {
  .check_netis_data(x)
  if (is.null(x$groups)) {
    return(NULL)
  }
  s <- x$schema
  out <- .reconstruct_long(x$groups, s$group_col, s$wave_col)
  .maybe_tibble(out, as)
}

#' Inspect the schema of a netis_data object
#'
#' Returns the metadata recorded at construction: identifier column names, the
#' directed/undirected and self-tie declarations, per level (node/tie/group)
#' which variables are stored statically versus dynamically, and
#' `tie_descriptors` -- per-tie-variable weighted/weight_semantics/
#' direction_semantics/zero_meaning/presence_mode metadata.
#'
#' The schema carries a format version. A version this installation of rnetis
#' does not recognize is a hard error rather than a silent partial read.
#'
#' @param x a [netis_data()] object.
#' @return a list of the object's internal fields. One field is worth naming
#'   here because you will meet it directly: `derived` holds one
#'   `netis_derived_var` record per column that [add_centering()] or
#'   [add_decomposition()] created, each printing the source column, the method,
#'   and what it was averaged over. A column from [add_baseline_provenance()] is
#'   an ordinary node variable and has no record here; its name says what it is.
#'   `observed_tie_waves` lists the (group, wave) snapshots whose network was
#'   observed, with or without ties, as text; it is `NULL` when ties do not vary
#'   by wave. See Empty and unobserved waves on [netis_data()].
#' @seealso [netis_data()], [netis_nodes()], [netis_ties()], [netis_groups()],
#'   [add_centering()] and [add_decomposition()], which add the `derived`
#'   records described above
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b"), trait = c(1, 2))
#' ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
#' nd <- netis_data(nodes, ties)
#' s <- netis_schema(nd)
#' s$directed
#' s$tie_descriptors
#' @export
netis_schema <- function(x) {
  .check_netis_data(x)
  .current_schema(x)
}

#' @noRd
.check_netis_data <- function(x) {
  if (!inherits(x, "netis_data")) {
    stop("x must be a netis_data object; see netis_data().", call. = FALSE)
  }
  invisible(TRUE)
}

#' Print or format a netis_data object
#'
#' The compact structural summary printed when a [netis_data()] object is
#' shown. It is worth reading rather than skimming: it is the fastest way to
#' confirm that rnetis understood your data the way you meant it, and most
#' data-side mistakes are visible here before any model is fitted.
#'
#' # What the summary reports, and what to check it against
#'
#' - **Counts and structure** — nodes, groups, ties, waves, whether the network
#'   is directed, and the self-tie policy. A node observed at several waves is
#'   counted once, and so is a tie stored at several waves.
#' - **Which variables are static and which are dynamic.** A variable you
#'   expected to vary by wave appearing under `static:` means the wave column
#'   did not distinguish it, and every `lagged()` term on it will be a NOTE
#'   rather than a lag.
#' - **The tie network semantics**, one line per tie variable: whether it is
#'   weighted, and its `presence_mode`. Values rnetis *inferred* rather than
#'   read from your declaration are marked `(inferred)`. That marking is the
#'   important part — an inferred `presence_mode` is rnetis's best structural
#'   guess, and on a complete dyadic dataset the guess and the truth differ in
#'   a way that changes which dyads count as tied. If a line says `(inferred)`
#'   and you know better, declare it in [netis_data()]'s `tie_meta`.
#' - **Whether group data was supplied**, since a group-level covariate,
#'   a [lagged()] group variable and a [selection_pooling()] moderator read it.
#'
#' Edge direction is *not* summarized as "incoming" or "outgoing", because the
#' object stores your rows exactly as supplied and the substantive reading is
#' yours to declare at the point of use — see [compute_exposure()]'s
#' `direction` argument.
#'
#' @param x a [netis_data()] object.
#' @param ... unused; present for S3 consistency with the generics.
#' @return `format()` a character vector, one element per line, suitable for
#'   `cat(sep = "\n")`. `print()` returns `x` invisibly and is called for its
#'   output.
#' @seealso [netis_data()], [netis_nodes()], [netis_schema()] for the same
#'   information as a list rather than as text, [diagnose_data()] for the
#'   checks that go beyond structure
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = c("a", "b", "c"), wave = rep(1:2, each = 3),
#'   y = c(1, 2, 3, 2, 2, 4)
#' )
#' # A sparse edge list: a row means a tie exists.
#' ties <- data.frame(group = 1, source = "a", target = "b", wave = 1, link = 1)
#' sparse <- netis_data(nodes, ties, wave_col = "wave")
#' sparse
#'
#' # The same facts as data, when you need to test one rather than read it.
#' netis_schema(sparse)$tie_descriptors$link$presence_mode
#'
#' # A complete dyadic table has a row for every ordered pair, including the
#' # zero-valued ones, so a row means the dyad was *observed* rather than that a
#' # tie exists. Presence must then be declared, not inferred -- compare the
#' # "tie network semantics" lines of the two.
#' pairs <- expand.grid(
#'   source = c("a", "b", "c"), target = c("a", "b", "c"),
#'   stringsAsFactors = FALSE
#' )
#' pairs <- pairs[pairs$source != pairs$target, ]
#' complete <- netis_data(
#'   nodes[nodes$wave == 1, c("group", "node")],
#'   data.frame(group = 1, pairs, link = c(1, 0, 0, 0, 1, 0)),
#'   tie_meta = list(link = list(
#'     presence_mode = "value_rule",
#'     presence_rule = function(value) value != 0
#'   ))
#' )
#' complete
#' @name netis-data-methods
NULL

#' @rdname netis-data-methods
#' @export
format.netis_data <- function(x, ...) {
  s <- netis_schema(x)
  n_nodes  <- nrow(x$nodes$static)
  n_groups <- length(unique(x$nodes$static[[s$group_col]]))
  n_ties   <- nrow(x$ties$static)
  n_waves  <- if (is.null(s$wave_col)) NA_integer_ else length(s$waves)

  var_line <- function(label, vars) {
    if (length(vars$static) == 0L && length(vars$dynamic) == 0L) {
      return(sprintf("  %s: (none)", label))
    }
    parts <- character(0)
    if (length(vars$static) > 0L) {
      parts <- c(parts, sprintf("static: %s", paste(vars$static, collapse = ", ")))
    }
    if (length(vars$dynamic) > 0L) {
      parts <- c(parts, sprintf("dynamic: %s", paste(vars$dynamic, collapse = ", ")))
    }
    sprintf("  %s: %s", label, paste(parts, collapse = "; "))
  }

  descriptor_lines <- vapply(names(s$tie_descriptors), function(v) {
    d <- s$tie_descriptors[[v]]
    sprintf(
      "    %s: %s%s, %s%s",
      v,
      if (d$weighted) "weighted" else "binary",
      if (d$weighted_inferred) " (inferred)" else "",
      d$presence_mode,
      if (d$presence_mode_inferred) " (inferred)" else ""
    )
  }, character(1))

  c(
    sprintf("<netis_data>"),
    sprintf(
      "  %d node%s in %d group%s, %d tie%s, %s%s, self-ties %s",
      n_nodes, if (n_nodes == 1L) "" else "s",
      n_groups, if (n_groups == 1L) "" else "s",
      n_ties, if (n_ties == 1L) "" else "s",
      if (s$directed) "directed" else "undirected",
      if (!is.null(s$wave_col) && !is.na(n_waves)) sprintf(", %d waves", n_waves) else "",
      s$self_ties
    ),
    var_line("node variables", s$node_vars),
    var_line("tie variables", s$tie_vars),
    if (length(descriptor_lines) > 0L) c("  tie network semantics:", descriptor_lines) else character(0),
    if (!is.null(x$groups)) var_line("group variables", s$group_vars) else "  no group_data supplied"
  )
}

#' @rdname netis-data-methods
#' @export
print.netis_data <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
