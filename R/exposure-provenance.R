#' Construct baseline-only provenance for the B-I mechanism
#'
#' Adds a new dynamic node variable suitable as the `content` argument to
#' [bi()]/[bi_components()] under the **baseline-only** provenance
#' assumption: the earliest observed wave's value of `var` is treated as the
#' sole exogenous shock entering the system, and every later wave's
#' provenance is exactly zero.
#'
#' This helper is the package's own construction. Frank and Fahrbach mention the
#' case of setting all exogenous effects after the first occasion to zero (endnote
#' 10, pp. 272-273) and do not give a rule for building it from data; the
#' within-group centering and the per-group baseline wave below are this
#' package's choices.
#'
#' # Why this needs a helper, not just documentation
#'
#' The correct construction is easy to get wrong in a way that looks
#' plausible: repeating the *same* baseline value at every historical wave's
#' provenance slot does **not** implement "baseline-only" -- it makes the B-I
#' accumulation treat baseline as `t` independent fresh shocks, one per wave,
#' over-counting it by roughly the number of waves. The correct construction
#' is zero everywhere except the first wave. This function builds that
#' directly so the mistake isn't available to make.
#'
#' # What this assumes, stated plainly
#'
#' Baseline-only provenance is the one provenance construction rnetis ships,
#' chosen for tractability: it requires no iterative estimation and no
#' baseline-model specification, and reduces the general path-accumulation sum
#' to a single term. Nothing makes it a default: `content` is whatever you pass.
#' It is not a low-assumption or conservative choice -- it is a specific, often
#' strong substantive claim:
#'
#' \itemize{
#'   \item **It assumes nothing new enters the system after the first
#'     observed wave.** All later change is attributed entirely to network
#'     redistribution of differences that already existed at baseline. For
#'     any real multi-wave social process spanning weeks or months, this is
#'     usually false to some degree -- new information, personnel changes,
#'     and external events routinely occur. Use this approach when your
#'     substantive question is specifically about the propagation of
#'     *initial* differences (e.g., how baseline group composition plays out
#'     over time), not as a universal safe choice.
#'   \item **It will misattribute genuine later shocks as if they were
#'     baseline propagation**, biasing the exposure term in an a priori
#'     unknown direction if real post-baseline shocks exist and correlate
#'     with the network structure.
#'   \item **A single missing baseline value can contaminate many nodes'
#'     downstream exposure.** Because the mechanism propagates provenance
#'     through matrix multiplication, an `NA` at one node's baseline value
#'     produces `NA` for every node reachable from it, not just that node.
#' }
#'
#' # Which wave is the baseline, and which rows get a value
#'
#' The baseline is **each group's own first observed wave** -- the earliest
#' wave at which the group has node rows -- not the first wave of the whole
#' study, so a group that enters the study late is not given a missing
#' baseline at a wave it was never observed. The new column is added to the
#' node rows that exist and to no others: on an unbalanced panel no node-wave
#' row is created. A node first observed after its group's baseline wave has
#' no baseline shock, so its provenance is `0` at every wave (under the
#' baseline-only assumption nothing enters the system after the baseline).
#' When node data has no wave dimension at all, the variable is static and
#' every node is present at every wave, so one row per node and wave is
#' built, and the baseline is the study's first wave.
#'
#' Everything else the object carries -- tie semantics and their ledger,
#' declared metric time, the import record, attributes -- is preserved. The new
#' column is an ordinary node variable: it has no entry in
#' `netis_schema(data)$derived`.
#'
#' @param data a [netis_data()] object with a wave dimension.
#' @param var name of the node variable whose earliest-wave value supplies
#'   the baseline shock.
#' @param center if `TRUE` (default), center the baseline value within each
#'   group (subtract the group mean) before use, so the provenance reflects
#'   relative rather than absolute baseline standing.
#' @param name name for the new provenance variable. Defaults to
#'   `"<var>_baseline_provenance"`. A name the node data already holds is an error.
#'
#' @return a [netis_data()] object with the new dynamic node variable added.
#' @references
#' Frank, K. A. and Fahrbach, K. (1999) Organization culture as a complex system:
#' balance and information in models of influence and selection. *Organization
#' Science* **10**(3), 253-277.
#'
#' Frank, K. A. and Fahrbach, K. (1999) Erratum: Organizational culture as a
#' complex system: balance and information in models of influence and selection.
#' *Organization Science* **10**(4), 514-515.
#' @seealso [bi()], [bi_components()], [compute_exposure()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2),
#'   negative_affect = c(0.2, -0.6, 0.5, -0.1)
#' )
#' ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2), tie = c(1, 1))
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#'
#' nd2 <- add_baseline_provenance(nd, "negative_affect")
#' netis_nodes(nd2)
#'
#' compute_exposure(nd2, "negative_affect_baseline_provenance", "tie",
#'                   wave = 2, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))
#' @export
add_baseline_provenance <- function(data, var, center = TRUE, name = NULL) {
  .check_netis_data(data)
  .assert_flag(center, "center")
  s <- netis_schema(data)
  if (is.null(s$wave_col) || length(s$waves) < 2L) {
    stop(
      "data must have a wave dimension with at least two waves for baseline-only provenance to be meaningful (there would be nothing after baseline to propagate to).",
      call. = FALSE
    )
  }

  nodes <- netis_nodes(data)
  .assert_columns_present(nodes, var, "node data")
  if (is.null(name)) {
    name <- paste0(var, "_baseline_provenance")
  }
  if (name %in% names(nodes)) {
    stop(sprintf(
      "add_baseline_provenance('%s', ...) would produce a column named '%s', which already exists. Supply name = to choose a different one.",
      var, name), call. = FALSE)
  }

  has_wave_col <- s$wave_col %in% names(nodes)
  gkey <- as.character(nodes[[s$group_col]])
  nkey <- .netis_unit_key(nodes[[s$group_col]], nodes[[s$node_col]])

  if (has_wave_col) {
    # The node rows that exist, and nothing else: an unbalanced panel stays
    # unbalanced. The baseline is each group's own first observed wave.
    wpos <- match(nodes[[s$wave_col]], s$waves)
    first_pos <- stats::ave(wpos, gkey, FUN = min)
    at_base <- wpos == first_pos
    base_vals <- nodes[[var]][at_base]
    base_group <- gkey[at_base]
    if (center) {
      base_vals <- base_vals - stats::ave(base_vals, base_group,
                                          FUN = function(x) mean(x, na.rm = TRUE))
    }
    names(base_vals) <- nkey[at_base]
    nodes[[name]] <- ifelse(at_base, as.numeric(base_vals[nkey]), 0)
    nodes_new <- nodes
  } else {
    # `nodes` may carry no wave column, if every node variable so far is
    # constant across waves (netis_nodes() then returns a purely static
    # table). A static node is present at every wave, so the full
    # (group, node, wave) grid is the node roster and is built explicitly.
    vals <- nodes[[var]]
    if (center) {
      vals <- vals - stats::ave(vals, gkey, FUN = function(x) mean(x, na.rm = TRUE))
    }
    names(vals) <- nkey
    roster <- unique(nodes[c(s$group_col, s$node_col)])
    rownames(roster) <- NULL
    grid <- do.call(rbind, lapply(s$waves, function(w) {
      piece <- roster
      piece[[s$wave_col]] <- w
      piece
    }))
    rownames(grid) <- NULL
    grid_key <- .netis_unit_key(grid[[s$group_col]], grid[[s$node_col]])
    grid[[name]] <- ifelse(grid[[s$wave_col]] == s$waves[1L],
                           as.numeric(vals[grid_key]), 0)
    nodes_new <- merge(nodes, grid, by = c(s$group_col, s$node_col), all = TRUE)
  }

  # tie_meta is reconstructed from the existing schema's descriptors:
  # rebuilding without it would silently drop every declared tie semantic
  # (zero_meaning, presence_mode, presence_col, ...) and re-infer from values
  # alone.
  out <- netis_data(
    nodes_new, netis_ties(data), group_data = netis_groups(data),
    group_col = s$group_col, node_col = s$node_col, wave_col = s$wave_col,
    source_col = s$source_col, target_col = s$target_col,
    directed = s$directed, self_ties = s$self_ties,
    tie_meta = .tie_meta_from_descriptors(s),
    time_meta = .time_meta_from_index(s),
    observed_tie_waves = .observed_tie_waves_declaration(s, netis_ties(data)),
    derived_meta = if (length(s$derived)) s$derived else NULL
  )
  .carry_object_state(out, data)
}
