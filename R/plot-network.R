# Panels over a netis_data object: the network itself, roster coverage, and the
# missingness map.
#
# Two constraints shape every function here.
#
# DIRECTION. netis_data preserves the user's raw edge direction exactly as
# supplied, and that is what an arrowhead must show. The exposure engine's
# canonical W -- where W[i, j] is the strength with which j influences i -- is
# a DERIVED orientation for exposure mathematics, and drawing arrows from it
# would silently reverse who points at whom on any dataset the engine
# transposed. This file therefore reads netis_ties() and never touches the
# exposure engine's matrices.
#
# PRESENCE. Which stored rows are actually ties is representation-dependent,
# so presence is resolved through the same .resolve_tie_presence()
# every other layer uses -- which errors on an undetermined complete_dyadic
# network rather than defaulting to != 0. Reusing it rather than reimplementing
# it is the point: one definition, one refusal.

# NO "latent" PANEL, and the reason is not that latent spaces are undrawable.
#
# Xu's estimated latent space IS a geometry, and its RELATIVE structure --
# distances above all, which is what the model is a model of -- is meaningful
# within a fitted network. What it has no canonical version of is orientation,
# reflection, translation, or which coordinate is "the first one", so an axis
# carries no interpretation and two groups' axes share no frame. Implementing
# Xu's adjustment does not require a visualization layer, and rnetis does not
# add one: the positions appear as ordinary coefficient rows in
# `plot_data(fit)`, with the selection model that produced them stated by
# `summary()`. A picture that made axes look interpretable would be a new
# claim, not a new convenience.
.NETIS_DATA_PANELS <- c("network", "coverage", "missing", "exposure", "centrality")

#' Plot a netis_data object
#'
#' Views of the data before any model touches it: the network as declared, which
#' units are on the roster at which waves, where the values are missing, and how
#' exposure and a centrality measure are distributed.
#'
#' # Direction
#'
#' Arrowheads show the edge direction **you declared** — from `source` to
#' `target` as supplied to [netis_data()]. They are never drawn from the exposure
#' engine's canonical orientation, which is a derived convention for exposure
#' arithmetic and is transposed relative to the raw declaration for some
#' datasets. If the arrows look backwards, the declaration is what to check.
#'
#' # Layouts
#'
#' `"circle"` and `"grid"` are native, deterministic, and defined for any network.
#' `"mds"` is native but needs a finite geodesic distance matrix, so it refuses on
#' a disconnected network — including one with an isolate — rather than
#' substituting a finite value for an infinite distance. igraph layouts are
#' available by name when igraph is installed and error actionably when it is not;
#' rnetis never silently substitutes one layout algorithm for another.
#'
#' Most igraph layouts are stochastic. They are drawn from `seed` (default
#' `1`), so the same call gives the same picture, and the session's random
#' stream is restored afterwards: drawing a plot does not change the caller's
#' next random number. `seed = NULL` draws from the session's stream instead,
#' advancing it like any other random draw. The seed used is recorded in the
#' `"network"` table's `layout_seed` attribute.
#'
#' # Missing values
#'
#' Nothing is dropped for being missing. Every node keeps its row and carries a
#' `missing_state`, so an **isolate** (present, no ties, a real zero) stays
#' distinguishable from a node whose value was never observed. Where the object
#' cannot answer a question — whether a network was observed at a given wave is
#' sometimes not recoverable — the state is `undetermined` rather than a guess.
#'
#' On the `"exposure"` panel this distinction is the whole point. Under the
#' default non-row-normalized sum an isolate's exposure is exactly `0`, because
#' summing over no alters is zero — a defined value, not a gap. A node one of
#' whose alters went unobserved has `NA`. The two are marked differently, and the
#' number of alters is computed rather than inferred from the value: a node *with*
#' alters can also total exactly zero when their values cancel, so reading
#' "0 means isolate" off the number would be wrong precisely where it matters.
#' Alters are the present ties of `variable`, so a stored row that the
#' network's presence declaration says is not a tie is not counted. On the
#' `"centrality"` panel, likewise, only an isolate's zero is marked
#' `structural_zero`; a connected node that scores zero is an observed value.
#'
#' # Multiple groups
#'
#' Node labels are unique only within a group. The `"coverage"`, `"missing"`,
#' `"exposure"` and `"centrality"` panels key every node by (group, node), and
#' label the axis by group and node when the data has several groups, so two
#' groups that both contain a node `"a"` are shown as two nodes.
#'
#' # Errors and refusals
#'
#' Drawing a network requires knowing which rows are ties. A network declared
#' `presence_mode = "complete_dyadic"` with no presence column and no value rule
#' has **undetermined** presence, and this refuses rather than treating every
#' non-zero row as a tie — a measured zero-weight tie and a structurally absent
#' one are not the same thing. Declare `presence_col` or `presence_rule` via
#' `tie_meta` in [netis_data()].
#'
#' A multi-group object refuses to draw a network without `group`: picking the
#' first one silently would look like a complete picture of the data.
#'
#' @param x A [netis_data()] object.
#' @param what Which panel. `"network"` (default) draws the network for one group
#'   and wave; `"coverage"` shows roster membership across waves; `"missing"` maps
#'   missingness in a node variable; `"exposure"` plots exposure against the
#'   number of alters; `"centrality"` shows a node-level feature's distribution.
#' @param variable The **tie** variable the panel is about, for `"network"`,
#'   `"exposure"` and `"centrality"`. Defaults to the first tie
#'   variable.
#' @param node_variable The **node** variable for `"missing"`. Defaults to the
#'   first node variable. This is a separate argument because `variable` names a
#'   tie variable on every other panel, and one name meaning two kinds of thing
#'   depending on `what` is a trap: supplying `variable` with
#'   `what = "missing"` is an error naming this argument, not a silent
#'   reinterpretation.
#' @param content For `"exposure"`, the node variable whose values ego is exposed
#'   to. Defaults to the first node variable.
#' @param measure For `"centrality"`, the measure to compute (`"degree"` by
#'   default); passed to [compute_centrality()].
#' @param group,wave `group` chooses the group for `"network"`, and is required when
#'   the data has several. `wave` chooses the wave for `"network"`, `"exposure"` and
#'   `"centrality"`, and is required when the data has more than one; it may be `NULL`
#'   on data with one wave or no wave dimension. The other panels ignore both.
#' @param layout Node layout: `"circle"` (default), `"grid"`, `"mds"`, or an
#'   igraph layout name such as `"fr"`. See *Layouts*.
#' @param seed Seed for a stochastic (igraph) layout: a whole number (default
#'   `1`), or `NULL` to draw from the session's random stream. A seeded layout
#'   leaves the session's random stream as it found it. Ignored by the native
#'   layouts, which are deterministic. See *Layouts*.
#' @param ... Ignored, present for S3 consistency.
#'
#' @return A ggplot object for `plot()`. For `plot_data()`: a data frame for
#'   `"coverage"` and `"missing"` (one row per (group, node) unit and wave,
#'   with `node`, `group`, `wave`, `unit` -- the axis label, which names the
#'   group when the data has several -- and `missing_state`; `"coverage"` adds
#'   `present`, and `"missing"` adds `value` and `variable`), for
#'   `"exposure"` and `"centrality"` one row per exposure or centrality value
#'   with `node`, `group`, `value` and `missing_state` (`"exposure"` adds
#'   `degree`, the number of present ties of `variable` into the node within
#'   its group, and `"centrality"` adds `measure`), and for `"network"` a **named
#'   list** of two data frames, `nodes` (with `node`, `group`, `x`, `y`, `degree`,
#'   `missing_state`, and `wave` where one was chosen) and `edges`
#'   (with `from`, `to`, `weight`, and the segment endpoints `x`/`y`/`xend`/`yend`).
#'   Two tables rather than one because an edge table alone cannot represent an
#'   isolate. The `"network"` result carries a `layout_method` attribute recording
#'   which layout actually ran, a `layout_distance` attribute
#'   (`"unweighted_geodesic"` for `"mds"`, otherwise `NA`), and for an igraph layout a
#'   `layout_seed` attribute recording the seed used.
#'
#' @seealso [plot_data()] for the underlying tables; [netis_ties()] and
#'   [netis_nodes()] for the raw logical view; [compute_exposure()] for the
#'   exposure quantities built on this network.
#'
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b", "c"), trait = c(1, 2, NA))
#' ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"),
#'                    w = c(1, 1))
#' nd <- netis_data(nodes, ties)
#'
#' plot(nd)
#'
#' # The arrow a -> b is the direction declared in `ties`, not a derived one.
#' d <- plot_data(nd, what = "network")
#' d$edges[, c("from", "to")]
#'
#' # Node c has a missing trait; on the missingness panel it keeps its row and
#' # is marked, not dropped.
#' plot_data(nd, what = "missing", node_variable = "trait")[
#'   , c("node", "value", "missing_state")]
#'
#' # Exposure: b is exposed to a's trait (1), c to b's (2), a to nobody (0).
#' plot_data(nd, what = "exposure", content = "trait",
#'           variable = "w")[, c("node", "value", "degree", "missing_state")]
#'
#' @export
plot.netis_data <- function(x, what = NULL, variable = NULL, group = NULL,
                            wave = NULL, layout = "circle",
                            content = NULL, measure = "degree",
                            node_variable = NULL, seed = 1L, ...) {
  what <- .netis_what(what, .NETIS_DATA_PANELS, "a netis_data object")
  d <- plot_data(x, what = what, variable = variable, group = group,
                 wave = wave, layout = layout, content = content,
                 measure = measure, node_variable = node_variable, seed = seed)

  p <- switch(
    what,
    network    = .netis_plot_network(d),
    coverage   = .netis_plot_coverage(d),
    missing    = .netis_plot_missing(d),
    exposure   = .netis_plot_exposure(d),
    centrality = .netis_plot_feature(d),
  )
  .netis_declare_references(p, .netis_references())
}

#' @rdname plot.netis_data
#' @export
plot_data.netis_data <- function(x, what = NULL, variable = NULL, group = NULL,
                                 wave = NULL, layout = "circle",
                                 content = NULL, measure = "degree",
                                 node_variable = NULL, seed = 1L, ...) {
  what <- .netis_what(what, .NETIS_DATA_PANELS, "a netis_data object")

  # `variable` names a TIE variable on every panel that takes one. The missing
  # panel maps a NODE variable, so it reads `node_variable` instead. Silently
  # accepting `variable` here would make one argument mean two kinds of thing
  # depending on `what`.
  if (identical(what, "missing") && !is.null(variable)) {
    stop("`variable` names a tie variable; the \"missing\" panel maps a node ",
         "variable.
Use `node_variable = \"", variable, "\"` instead.",
         call. = FALSE)
  }

  switch(
    what,
    network    = .plot_data_network(x, variable, group, wave, layout, seed),
    coverage   = .plot_data_coverage(x),
    missing    = .plot_data_missing(x, node_variable),
    exposure   = .plot_data_exposure(x, content, variable, wave),
    centrality = .plot_data_centrality(x, variable, measure, wave),
  )
}

# ---- network ----------------------------------------------------------------

#' Node and edge tables for one snapshot
#'
#' Returns a named list because one rectangle cannot carry both: an edge table
#' alone loses every isolate, which is the node this package least wants to
#' lose.
#' @noRd
.plot_data_network <- function(x, variable, group, wave, layout, seed = 1L) {
  .check_netis_data(x)
  s <- netis_schema(x)
  ties <- netis_ties(x)
  nodes <- netis_nodes(x)

  variable <- .netis_tie_variable(s, ties, variable)
  group <- .netis_one_group(s, nodes, group)
  wave <- .resolve_wave(
    s, wave,
    multi_hint = ", since a network drawing shows a single snapshot. To draw every wave, loop over netis_schema(x)$waves."
  )

  nodes_g <- nodes[as.character(nodes[[s$group_col]]) == group, , drop = FALSE]
  ties_g <- ties[as.character(ties[[s$group_col]]) == group, , drop = FALSE]
  if (!is.null(wave) && !is.null(s$wave_col)) {
    if (s$wave_col %in% names(nodes_g)) {
      nodes_g <- nodes_g[nodes_g[[s$wave_col]] == wave, , drop = FALSE]
    }
    if (s$wave_col %in% names(ties_g)) {
      ties_g <- ties_g[ties_g[[s$wave_col]] == wave, , drop = FALSE]
    }
  }

  # Presence through the shared primitive, which errors on an undetermined
  # complete_dyadic network rather than guessing. One definition of
  # presence in the package, not one per consumer.
  descriptor <- s$tie_descriptors[[variable]]
  present <- if (nrow(ties_g) == 0L) logical(0) else {
    .resolve_tie_presence(descriptor, ties_g, variable)
  }
  ties_g <- ties_g[!is.na(present) & present, , drop = FALSE]

  ids <- sort(unique(as.character(nodes_g[[s$node_col]])))

  # Raw declared direction. netis_ties() gives source and target exactly as the
  # user supplied them; nothing here consults the exposure engine.
  edges <- data.frame(
    from = as.character(ties_g[[s$source_col]]),
    to   = as.character(ties_g[[s$target_col]]),
    stringsAsFactors = FALSE
  )
  edges$weight <- if (nrow(ties_g) > 0L) ties_g[[variable]] else numeric(0)

  coords <- .netis_layout(ids, edges, method = layout, seed = seed)

  node_tbl <- data.frame(
    node = ids, group = group, x = coords$x, y = coords$y,
    stringsAsFactors = FALSE
  )
  if (!is.null(wave)) node_tbl$wave <- wave

  # An isolate is a structural zero, not a missing value: it has no ties, and
  # that is an observation rather than a gap, and the drawing keeps the two apart.
  degree <- as.integer(table(factor(c(edges$from, edges$to), levels = ids)))
  node_tbl$degree <- degree
  node_tbl$missing_state <- .netis_missing_state(
    length(ids),
    ifelse(degree == 0L, "structural_zero", "observed")
  )

  if (nrow(edges) > 0L) {
    edges$x    <- coords$x[match(edges$from, coords$node)]
    edges$y    <- coords$y[match(edges$from, coords$node)]
    edges$xend <- coords$x[match(edges$to, coords$node)]
    edges$yend <- coords$y[match(edges$to, coords$node)]
  } else {
    edges$x <- numeric(0); edges$y <- numeric(0)
    edges$xend <- numeric(0); edges$yend <- numeric(0)
  }

  out <- list(
    nodes = .netis_plot_table(node_tbl, required = c("node", "x", "y", "missing_state")),
    edges = edges
  )
  attr(out, "layout_method") <- attr(coords, "layout_method")
  attr(out, "layout_seed") <- attr(coords, "layout_seed")
  attr(out, "layout_distance") <- attr(coords, "layout_distance")
  attr(out, "netis_snapshot") <- list(group = group, wave = wave, variable = variable)
  out <- .netis_caption(
    out,
    sprintf("Network '%s', group %s%s. Arrows show the declared edge direction (source to target).",
            variable, group,
            if (is.null(wave)) "" else sprintf(", wave %s", wave))
  )
  if (identical(attr(coords, "layout_distance"), "unweighted_geodesic")) {
    out <- .netis_caption(
      out,
      paste0("Layout distances are unweighted hop counts; tie weights do not affect the ",
             "positions.")
    )
  }
  if (any(node_tbl$degree == 0L)) {
    out <- .netis_caption(
      out,
      sprintf("%d node(s) have no ties in this snapshot; they are drawn as isolates, not as missing.",
              sum(node_tbl$degree == 0L))
    )
  }
  out
}

#' @noRd
.netis_plot_network <- function(d) {
  nodes <- d$nodes
  edges <- d$edges

  p <- ggplot2::ggplot()
  if (nrow(edges) > 0L) {
    p <- p + ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      colour = "grey55", linewidth = 0.4,
      arrow = ggplot2::arrow(length = ggplot2::unit(6, "pt"), type = "closed"),
      na.rm = TRUE
    )
  }
  p +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = x, y = y, colour = missing_state, shape = missing_state),
      size = 3.5, na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = nodes, ggplot2::aes(x = x, y = y, label = node),
      vjust = -1.1, size = 3, colour = "grey20"
    ) +
    .netis_scale_missing() +
    .netis_scale_missing_shape() +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL) +
    .netis_theme() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    .netis_caption_lab(d)
}

# ---- coverage ---------------------------------------------------------------

#' Which units are on the roster at which waves
#'
#' A ragged roster is a distinct incompleteness form from an `NA` value and
#' needs its own display: the absence is a row that does not exist, which no
#' plot of the values themselves can show.
#' @noRd
.plot_data_coverage <- function(x) {
  .check_netis_data(x)
  s <- netis_schema(x)
  nodes <- netis_nodes(x)

  if (is.null(s$wave_col) || length(s$waves) == 0L) {
    stop(
      paste0(
        "The \"coverage\" panel shows roster membership across waves, and this data has no ",
        "wave dimension (no wave_col was supplied to netis_data()).\n",
        "Use what = \"network\" or what = \"missing\" instead."
      ),
      call. = FALSE
    )
  }

  # The full grid is the point: a node-wave that never appears must be a row here
  # marked absent, because that is exactly the state being displayed. A unit is
  # a (group, node) pair: node labels are unique only within a group, so two
  # groups that both have a node "a" have two units.
  grid <- .netis_unit_wave_grid(s, nodes, s$waves)
  key_obs <- paste(.netis_unit_key(nodes[[s$group_col]], nodes[[s$node_col]]),
                   nodes[[s$wave_col]], sep = .NETIS_KEY_SEP)
  key_grid <- paste(.netis_unit_key(grid$group, grid$node), grid$wave,
                    sep = .NETIS_KEY_SEP)
  grid$present <- key_grid %in% key_obs
  grid$missing_state <- .netis_missing_state(
    nrow(grid),
    ifelse(grid$present, "observed", "absent_row")
  )

  out <- .netis_plot_table(grid, required = c("node", "group", "unit", "wave", "missing_state"))
  n_absent <- sum(!grid$present)
  .netis_caption(
    out,
    if (n_absent == 0L) {
      "Every node in the data is on the roster at every wave."
    } else {
      sprintf(paste0("%d node-wave(s) are not on the roster. A node absent from a wave is a ",
                     "different state from one present with a missing value."),
              n_absent)
    }
  )
}

#' @noRd
.netis_plot_coverage <- function(d) {
  ggplot2::ggplot(d, ggplot2::aes(x = factor(wave), y = unit,
                                  colour = missing_state, shape = missing_state)) +
    ggplot2::geom_point(size = 3, na.rm = TRUE) +
    .netis_scale_missing() +
    .netis_scale_missing_shape() +
    ggplot2::labs(x = "wave", y = NULL) +
    .netis_theme() +
    ggplot2::theme(legend.position = "bottom") +
    .netis_caption_lab(d)
}

# ---- missingness ------------------------------------------------------------

#' Where a node variable is missing, and where it is merely absent
#'
#' Scoped by what the object can actually answer. Network-wave absence is
#' deliberately **not** shown here: it is not stored on `netis_data`, and
#' inferring it from a static/dynamic classification that is itself value-
#' driven would assert an observation nobody made.
#' @noRd
.plot_data_missing <- function(x, variable) {
  .check_netis_data(x)
  s <- netis_schema(x)
  nodes <- netis_nodes(x)
  variable <- .netis_node_variable(s, nodes, variable)

  has_waves <- !is.null(s$wave_col) && s$wave_col %in% names(nodes)
  waves <- if (has_waves) s$waves else NA

  # Keyed by (group, node): node labels are unique only within a group.
  grid <- .netis_unit_wave_grid(s, nodes, waves)
  key_obs <- .netis_unit_key(nodes[[s$group_col]], nodes[[s$node_col]])
  key_grid <- .netis_unit_key(grid$group, grid$node)
  if (has_waves) {
    key_obs <- paste(key_obs, nodes[[s$wave_col]], sep = .NETIS_KEY_SEP)
    key_grid <- paste(key_grid, grid$wave, sep = .NETIS_KEY_SEP)
  }
  idx <- match(key_grid, key_obs)
  grid$value <- nodes[[variable]][idx]

  # Three states, and they are three different things: off the roster entirely,
  # on it with an unobserved value, on it and observed.
  state <- ifelse(is.na(idx), "absent_row", ifelse(is.na(grid$value), "missing", "observed"))
  grid$missing_state <- .netis_missing_state(nrow(grid), state)
  grid$variable <- variable

  out <- .netis_plot_table(grid, required = c("node", "group", "unit", "value", "missing_state"))
  out <- .netis_caption(
    out,
    sprintf("Missingness in '%s'. Off-roster and observed-but-missing are different states.",
            variable)
  )
  # The honest limit, stated on the plot rather than only in the help page.
  .netis_caption(
    out,
    paste0("Whether a network was observed at a given wave is not recorded by netis_data ",
           "and is not shown here.")
  )
}

#' @noRd
.netis_plot_missing <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(y = unit, colour = missing_state, shape = missing_state))
  p <- if (all(is.na(d$wave))) {
    p + ggplot2::aes(x = "") + ggplot2::labs(x = NULL)
  } else {
    p + ggplot2::aes(x = factor(wave)) + ggplot2::labs(x = "wave")
  }
  p +
    ggplot2::geom_point(size = 3, na.rm = TRUE) +
    .netis_scale_missing() +
    .netis_scale_missing_shape() +
    ggplot2::labs(y = NULL) +
    .netis_theme() +
    ggplot2::theme(legend.position = "bottom") +
    .netis_caption_lab(d)
}

# ---- exposure patterns ------------------------------------------------------

#' Exposure values, with isolates kept distinct from unobserved alters
#'
#' The whole reason this panel needs care. Under the default non-row-normalized
#' sum, an **isolate's** exposure is exactly `0` — a real, defined quantity,
#' because summing over no alters is zero. A node whose alter went unobserved
#' has `NA`. Those two print as `0` and blank and mean completely different
#' things, and a `0 * NA` error in the exposure engine is what happens when a
#' layer stops distinguishing them.
#'
#' Degree is computed here rather than inferred from the exposure value, because
#' a non-isolate can also produce exactly `0` (alters whose values happen to sum
#' to zero, or a negative and a positive cancelling). Reading "0 means isolate"
#' off the number would be wrong for exactly the datasets where it matters.
#' @noRd
.plot_data_exposure <- function(x, content, via, wave) {
  .check_netis_data(x)
  s <- netis_schema(x)
  nodes <- netis_nodes(x)
  ties <- netis_ties(x)

  content <- .netis_node_variable(s, nodes, content)
  via <- .netis_tie_variable(s, ties, via)

  ex <- compute_exposure(x, content = content, via = via, wave = wave)
  value_col <- setdiff(names(ex), c(s$group_col, s$node_col, s$wave_col))[1]

  d <- data.frame(
    node  = as.character(ex[[s$node_col]]),
    group = as.character(ex[[s$group_col]]),
    value = ex[[value_col]],
    stringsAsFactors = FALSE
  )
  if (!is.null(s$wave_col) && s$wave_col %in% names(ex)) d$wave <- ex[[s$wave_col]]

  d$degree <- .netis_indegree(s, ties, via, ex, wave)
  # Structure first: a node with no alters is an isolate whatever its exposure
  # value turned out to be. Under `isolate_rule = "na"` that value IS `NA`, and
  # calling it "missing" would blame unobserved data for a rule the user
  # declared -- the misattribution described in .netis_classify_missing().
  d$missing_state <- .netis_classify_missing(
    d$value,
    structural = ifelse(is.na(d$degree), NA, d$degree == 0L)
  )

  out <- .netis_plot_table(d, required = c("node", "value", "degree", "missing_state"))
  out <- .netis_caption(
    out,
    sprintf("Exposure to '%s' through '%s' (non-row-normalized sum).", content, via)
  )
  n_iso <- sum(d$degree == 0L, na.rm = TRUE)
  n_na <- sum(is.na(d$value))
  if (n_iso > 0L) {
    out <- .netis_caption(
      out,
      sprintf(paste0("%d node(s) have no alters. Their exposure is exactly 0 by construction, ",
                     "which is a value rather than a gap."), n_iso)
    )
  }
  if (n_na > 0L) {
    out <- .netis_caption(
      out,
      sprintf(paste0("%d node(s) have undefined exposure because an alter's value was not ",
                     "observed. That is a different state from an isolate's zero."), n_na)
    )
  }
  out
}

#' In-degree under the declared direction, per exposure row
#'
#' "Alters" for exposure means the nodes pointing *into* ego under the resolved
#' convention, so this counts incoming ties as declared. Counting out-ties here
#' would produce a plot that looked fine and answered a different question.
#'
#' Only rows that are ties of the `via` network count, resolved through the
#' shared presence primitive: a stored row whose `via` value declares the tie
#' absent is not an alter. Ego is keyed by (group, node), and by wave where the
#' data has one, because node labels are unique only within a group.
#' @noRd
.netis_indegree <- function(s, ties, via, ex, wave) {
  if (nrow(ties) > 0L) {
    present <- .resolve_tie_presence(s$tie_descriptors[[via]], ties, via)
    ties <- ties[!is.na(present) & present, , drop = FALSE]
  }
  key_ties <- .netis_unit_key(ties[[s$group_col]], ties[[s$target_col]])
  key_ex <- .netis_unit_key(ex[[s$group_col]], ex[[s$node_col]])
  if (!is.null(s$wave_col) && s$wave_col %in% names(ties) && s$wave_col %in% names(ex)) {
    key_ties <- paste(key_ties, ties[[s$wave_col]], sep = .NETIS_KEY_SEP)
    key_ex <- paste(key_ex, ex[[s$wave_col]], sep = .NETIS_KEY_SEP)
  } else if (!is.null(wave) && !is.null(s$wave_col) && s$wave_col %in% names(ties)) {
    ties <- ties[ties[[s$wave_col]] == wave, , drop = FALSE]
    key_ties <- .netis_unit_key(ties[[s$group_col]], ties[[s$target_col]])
  }
  tab <- table(factor(key_ties, levels = unique(key_ex)))
  as.integer(tab[match(key_ex, names(tab))])
}

#' Whether each row of a per-node table is an isolate in a network
#'
#' An isolate has no present tie of `variable` as source or target, within
#' its own group and, where both tables carry one, its own wave.
#' @noRd
.netis_isolate <- function(s, ties, variable, tbl, wave) {
  if (nrow(ties) > 0L) {
    present <- .resolve_tie_presence(s$tie_descriptors[[variable]], ties, variable)
    ties <- ties[!is.na(present) & present, , drop = FALSE]
  }
  by_wave <- !is.null(s$wave_col) && s$wave_col %in% names(ties) &&
    s$wave_col %in% names(tbl)
  if (!by_wave && !is.null(wave) && !is.null(s$wave_col) && s$wave_col %in% names(ties)) {
    ties <- ties[ties[[s$wave_col]] == wave, , drop = FALSE]
  }
  key_tie <- function(col) {
    k <- .netis_unit_key(ties[[s$group_col]], ties[[col]])
    if (by_wave) paste(k, ties[[s$wave_col]], sep = .NETIS_KEY_SEP) else k
  }
  touched <- c(key_tie(s$source_col), key_tie(s$target_col))
  key_tbl <- .netis_unit_key(tbl[[s$group_col]], tbl[[s$node_col]])
  if (by_wave) key_tbl <- paste(key_tbl, tbl[[s$wave_col]], sep = .NETIS_KEY_SEP)
  !(key_tbl %in% touched)
}

#' Every (group, node) unit crossed with the given waves
#'
#' Units are the distinct (group, node) pairs of the node table, ordered by
#' group then node; `unit` is the display label, which names the group when
#' the data has more than one, so two groups' node "a" are two rows on the
#' axis.
#' @noRd
.netis_unit_wave_grid <- function(s, nodes, waves) {
  units <- unique(data.frame(
    group = as.character(nodes[[s$group_col]]),
    node = as.character(nodes[[s$node_col]]),
    stringsAsFactors = FALSE
  ))
  units <- units[order(units$group, units$node), , drop = FALSE]
  multi <- length(unique(units$group)) > 1L
  label <- if (multi) paste0(units$group, ": ", units$node) else units$node
  n_u <- nrow(units)
  grid <- data.frame(
    node = rep(units$node, times = length(waves)),
    wave = rep(waves, each = n_u),
    group = rep(units$group, times = length(waves)),
    stringsAsFactors = FALSE
  )
  grid$unit <- factor(rep(label, times = length(waves)), levels = rev(unique(label)))
  rownames(grid) <- NULL
  grid
}

#' @noRd
.netis_plot_exposure <- function(d) {
  has_waves <- "wave" %in% names(d) && !all(is.na(d$wave))
  p <- ggplot2::ggplot(
    d, ggplot2::aes(x = degree, y = value, colour = missing_state, shape = missing_state)
  ) +
    ggplot2::geom_point(size = 2.5, na.rm = TRUE) +
    .netis_scale_missing() +
    .netis_scale_missing_shape() +
    ggplot2::labs(x = "alters (in-degree)", y = "exposure") +
    .netis_theme() +
    ggplot2::theme(legend.position = "bottom")
  # Faceting by wave rather than overplotting: exposure at different waves is a
  # different quantity, and one cloud would hide a wave whose network changed.
  if (has_waves) p <- p + ggplot2::facet_wrap(~ wave)
  p + .netis_caption_lab(d)
}

# ---- network features -------------------------------------------------------

#' A node-level network feature, as a distribution
#' @noRd
.plot_data_centrality <- function(x, network, measure, wave) {
  .check_netis_data(x)
  s <- netis_schema(x)
  ties <- netis_ties(x)
  network <- .netis_tie_variable(s, ties, network)

  # compute_centrality() takes the network as a bare column name (it captures the
  # unevaluated argument to admit binarize()/symmetrize() calls), so the call is
  # built rather than passed a string.
  cl <- as.call(list(
    quote(compute_centrality),
    data = quote(x), network = as.name(network), measure = measure, wave = wave
  ))
  cen <- eval(cl)
  value_col <- setdiff(names(cen), c(s$group_col, s$node_col, s$wave_col))[1]

  d <- data.frame(
    node  = as.character(cen[[s$node_col]]),
    group = as.character(cen[[s$group_col]]),
    value = cen[[value_col]],
    stringsAsFactors = FALSE
  )
  if (!is.null(s$wave_col) && s$wave_col %in% names(cen)) d$wave <- cen[[s$wave_col]]
  d$measure <- measure
  # A centrality of zero is a real measurement, not an absence. Only an ISOLATE
  # (no tie of this network in either direction) is a structural zero; a
  # connected node can also score 0 (betweenness on a periphery, say), and that
  # is an ordinary observed value.
  isolate <- .netis_isolate(s, ties, network, cen, wave)
  d$missing_state <- .netis_classify_missing(
    d$value, structural = !is.na(d$value) & isolate
  )

  out <- .netis_plot_table(d, required = c("node", "value", "missing_state"))
  .netis_caption(out, sprintf("%s over network '%s'.", measure, network))
}

#' @noRd
.netis_plot_feature <- function(d) {
  has_waves <- "wave" %in% names(d) && !all(is.na(d$wave))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 15, fill = "grey70", colour = "white", na.rm = TRUE) +
    ggplot2::labs(x = unique(d$measure)[1], y = "nodes") +
    .netis_theme()
  if (has_waves) p <- p + ggplot2::facet_wrap(~ wave)
  p + .netis_caption_lab(d)
}


# ---- shared resolvers -------------------------------------------------------

#' @noRd
.netis_tie_variable <- function(s, ties, variable) {
  candidates <- setdiff(names(ties), c(s$group_col, s$source_col, s$target_col, s$wave_col))
  if (length(candidates) == 0L) {
    stop("This data has no tie variable to draw.", call. = FALSE)
  }
  if (is.null(variable)) return(candidates[1])
  if (!variable %in% candidates) {
    stop(
      sprintf("\"%s\" is not a tie variable.\nAvailable: %s.",
              variable, paste(candidates, collapse = ", ")),
      call. = FALSE
    )
  }
  variable
}

#' @noRd
.netis_node_variable <- function(s, nodes, variable) {
  candidates <- setdiff(names(nodes), c(s$group_col, s$node_col, s$wave_col))
  if (length(candidates) == 0L) {
    stop("This data has no node variable to map.", call. = FALSE)
  }
  if (is.null(variable)) return(candidates[1])
  if (!variable %in% candidates) {
    stop(
      sprintf("\"%s\" is not a node variable.\nAvailable: %s.",
              variable, paste(candidates, collapse = ", ")),
      call. = FALSE
    )
  }
  variable
}

#' Resolve which group to draw
#'
#' A network drawing shows one group. Refusing to pick silently matters because
#' the alternative -- drawing the first group and saying nothing -- looks like a
#' complete picture of the data.
#' @noRd
.netis_one_group <- function(s, nodes, group) {
  groups <- sort(unique(as.character(nodes[[s$group_col]])))
  if (is.null(group)) {
    if (length(groups) > 1L) {
      stop(
        sprintf(
          paste0("This data has %d groups (%s) and a network drawing shows one.\n",
                 "Supply group = to choose, or loop over the groups to draw each."),
          length(groups), paste(utils::head(groups, 5), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(groups[1])
  }
  group <- as.character(group)
  if (!group %in% groups) {
    stop(
      sprintf("group = \"%s\" is not one of: %s.", group, paste(groups, collapse = ", ")),
      call. = FALSE
    )
  }
  group
}
