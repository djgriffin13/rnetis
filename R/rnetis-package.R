#' rnetis: Network Influence and Selection Models
#'
#' @description
#' \if{html}{\figure{logo.png}{options: style="float: right; margin: 0 0 1em 1em;" alt="rnetis logo" width="120"}}
#'
#' One consistent interface to two established literatures on social network
#' processes: **Influence** models, in which a node characteristic changes as a
#' function of its prior state and of network exposure to the characteristics of
#' connected others; and **Selection** models, in which a tie changes as a
#' function of sender, receiver, dyadic, and higher-order structural effects,
#' with group-level predictors through a hierarchical model.
#'
#' # What this package contributes
#'
#' Unification and usability, more than new methodology. `rnetis` implements,
#' integrates, and operationalizes published methods behind a single data
#' structure, a single formula grammar, and one diagnostics surface.
#'
#' Every published method is cited on the help page of the function that computes
#' or fits it; a formula term or option that only feeds such a function is covered
#' on that function's page. Where an implementation departs from its source, the
#' departure is named at the function.
#'
#' # What is this package's own
#'
#' These are not published methods. Each is declared on its own help page:
#'
#' - the hierarchical multi-group Selection model ([hierarchical_selection]);
#' - the latent-scale variance share and null comparison for AME fits
#'   ([pseudo_r2()], [netis_variance_components()]);
#' - multi-group [latent()], which is experimental: it extends a single-network
#'   procedure with per-group position coefficients and one shared exposure
#'   coefficient, and is not validated for statistical performance;
#' - [add_baseline_provenance()];
#' - the assumption ledger ([netis_assumptions()]);
#' - the numeric thresholds in the diagnostics, each stated as a package
#'   convention where it applies.
#'
#' # Scope
#'
#' - Two waves at most (`T = 1` or `T = 2`). With three or more waves present,
#'   `rnetis` requires the user to name the wave or the transition rather than
#'   choosing one on their behalf.
#' - Exactly one outcome occasion per unit per fit. Wave *depth* is
#'   unrestricted --- lags and exposure history may reach further back --- but
#'   repeated outcome rows per unit are not modeled.
#' - Gaussian-continuous Influence outcomes.
#'
#' Dynamic-panel estimators and distance-shell network autocorrelation are published methods that fall outside this scope.
#' They are absent because of the scope, not because of any doubt about them.
#'
#' # Functions by task
#'
#' The package is named `rnetis`; its functions and classes use the `netis_`
#' prefix or a plain name.
#'
#' **Build a data object:** [netis_data()], [as_netis_data()],
#' [netis_data_from_adjacency()], [netis_data_from_adjacency_series()],
#' [netis_data_from_edgelist()], [netis_data_from_igraph()],
#' [netis_data_from_igraph_list()], [netis_data_from_network()],
#' [netis_data_from_split()].
#'
#' **Read a data object:** [netis_nodes()], [netis_ties()], [netis_groups()],
#' [netis_tables()], [netis_adjacency()], [netis_schema()],
#' [netis_time_index()], [netis_boundary()], [netis_boundary_rules()],
#' [netis_losses()].
#'
#' **Export to other formats:** [as_igraph()], [as_igraph_list()],
#' [as_network()], [as_network_list()].
#'
#' **Derive variables:** [add_centering()], [add_decomposition()],
#' [add_baseline_provenance()].
#'
#' **Check data and design:** [diagnose_data()], [diagnose_design()],
#' [filter_diagnostics()], [has_level()].
#'
#' **Construct exposure:** [exposure()], [compute_exposure()], [bi()],
#' [bi_components()], [bi_sweep()].
#'
#' **Derive networks:** [binarize()], [symmetrize()], [multiplex()],
#' [inverse()], with the binarization rules [above_mean()], [above_median()],
#' [abs_threshold()], [quantile_threshold()], [threshold()], [nonzero()],
#' [positive()], [presence()], [custom_binarize_rule()].
#'
#' **Influence formula terms:** [ar()], [lagged()], [exposure()], [centrality()],
#' [network_stat()], [latent()].
#'
#' **Selection formula terms:** [sender()], [receiver()], [dyad()], [group()],
#' [prior_tie()], [prior_reciprocity()].
#'
#' **Fit and simulate:** [fit_influence()], [fit_selection()],
#' [selection_pooling()], [hierarchical_selection], [netis_hierarchy()],
#' [simulate_influence()].
#'
#' **Results as data:** [netis_estimates()], [netis_glance()], [tidy()],
#' [glance()], [netis_estimand()], [netis_diagnostics()],
#' [netis_variance_components()], [plot_data()].
#'
#' **Assumptions and dependence:** [netis_assumptions()], [netis_dependence()],
#' [influence_se_comparison()].
#'
#' **Effect sizes and explained variation:** [influence_effect()],
#' [selection_effect()], [pseudo_r2()].
#'
#' **Aggregation reliability and agreement:** [icc1()], [icc2()], [rwg()],
#' [rwg_j()], [netis_reliability()].
#'
#' **Network features outside a model:** [compute_centrality()],
#' [compute_network_stat()].
#'
#' **Example data:** [demo_influence], [demo_influence_groups], [demo_selection].
#'
#' @section Acknowledgments:
#' The author acknowledges the **T.A.C.T.I.C.S Lab** (Technology, Adaptation,
#' Coordination, and Training in Interdependent Complex Systems) at the
#' University of South Florida, and thanks Ken Frank for his support and
#' insights.
#'
#' @section Developed by:
#' Developed and owned by **Griffin Dynamics LLC** as part of its ongoing research
#' and development. Licensed under AGPL-3 with additional terms under its
#' section 7: see `LICENSE`, `LICENSE-AGPL-3.md` and `COPYRIGHTS` in the
#' installed package directory.
#'
#' @keywords internal
"_PACKAGE"
