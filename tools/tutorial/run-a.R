# Blocks for the data pages and the Influence path.
#   Rscript run-a.R <outdir>
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")

# ===========================================================================
# Getting started
# ===========================================================================

cap("g-check", R(
  'library(rnetis)',
  'packageVersion("rnetis")'
))

cap("g-optional", R(
  'for (p in c("amen", "latentnet", "sna", "clubSandwich", "igraph", "network")) {',
  '  cat(sprintf("%-14s %s\\n", p,',
  '      if (requireNamespace(p, quietly = TRUE)) "installed" else "not installed"))',
  '}'
))

# ===========================================================================
# The data object
# ===========================================================================

setup(R(
  'nodes <- data.frame(group = 1, node = c("a", "b", "c"), x = c(10, 20, 30))',
  'ties  <- data.frame(group = 1, source = c("b", "c"), target = c("a", "a"),',
  '                    w = c(1, 3))',
  '',
  'nd <- netis_data(nodes, ties)',
  'nd'
))

setup(R(
  'netis_nodes(nd)',
  'netis_ties(nd)'
))

setup(R(
  'nodes2 <- data.frame(group = 1, node = rep(c("a", "b", "c"), 2),',
  '                     wave  = rep(1:2, each = 3),',
  '                     x     = c(10, 20, 30, 12, 19, 34))',
  'ties2  <- data.frame(group = 1, source = c("b", "c", "b"), target = c("a", "a", "c"),',
  '                     wave = c(1, 1, 2), w = c(1, 3, 2))',
  '',
  'nd2 <- netis_data(nodes2, ties2, wave_col = "wave")',
  'nd2'
))

cap("d-time-index", R(
  'netis_time_index(nd2)'
))

setup(R(
  'nd3 <- netis_data(nodes2, ties2, wave_col = "wave", tie_meta = list(',
  '  w = list(weighted = TRUE, zero_meaning = "measured_zero",',
  '           weight_semantics = "count")',
  '))',
  'netis_boundary(nd3)'
))

setup(R(
  'netis_boundary(nd2)'
))

cap("d-losses", R(
  'netis_losses()[, c("loss_id", "severity", "overridable")]'
))

cap("d-edgelist-refuse", R(
  'e <- data.frame(source = c("a", "b"), target = c("b", "c"), w = 1)',
  'netis_data_from_edgelist(e)'
), expect_error = TRUE)

cap("d-edgelist-roster", R(
  'roster <- data.frame(node = c("a", "b", "c", "d"))',
  'netis_data_from_edgelist(e, nodes = roster)'
))

cap("d-adjacency", R(
  'm <- matrix(c(0, 1, 0,',
  '              1, 0, 1,',
  '              0, 0, 0), nrow = 3, byrow = TRUE,',
  '            dimnames = list(c("a", "b", "c"), c("a", "b", "c")))',
  'netis_data_from_adjacency(m)'
))

cap("d-adjacency-refuse", R(
  'diag(m) <- 1',
  'netis_data_from_adjacency(m)'
), expect_error = TRUE)

cap("d-demo", R(
  'demo_influence'
))

cap("d-diagnose-clean", R(
  'diagnose_data(demo_influence)'
))

cap("d-diagnose-findings", R(
  'rough_nodes <- data.frame(group = 1, node = letters[1:8],',
  '                          wave = rep(1:2, each = 8),',
  '                          y = c(0.4, -1.2, 0.9, 0.1, -0.6, 1.4, 0.2, -0.3,',
  '                                0.8, -0.9, 1.1, 0.5, -0.2, 1.8, NA, 0.1),',
  '                          x = 1)',
  'rough_ties <- data.frame(group = 1, source = c("a", "b", "c"),',
  '                         target = c("b", "c", "d"), tie = 1)',
  '',
  'rough <- netis_data(rough_nodes, rough_ties, wave_col = "wave")',
  'diagnose_data(rough)'
))

cap("d-filter", R(
  'filter_diagnostics(diagnose_data(rough), level = "NOTE")'
))

capfig("d-plot-network", R(
  'plot(demo_influence, wave = 1)'
), width = 6.6, height = 5.2)

capfig("d-plot-coverage", R(
  'plot(demo_influence, what = "coverage")'
), width = 7.2, height = 3.6)

cap("d-plot-data", R(
  'head(plot_data(demo_influence, what = "coverage"))'
))

# ===========================================================================
# Influence: exposure
# ===========================================================================

setup(R(
  'compute_exposure(nd, content = "x", via = "w")'
))

setup(R(
  'compute_exposure(nd, content = "x", via = "w", normalize = "row")'
))

setup(R(
  'compute_exposure(nd, content = "x", via = "w", normalize = "row",',
  '                 isolate_rule = "zero")'
))

setup(R(
  'compute_exposure(nd, content = "x", via = "w",',
  '                 direction = "target_influences_source")'
))

cap("a-exposure-demo", R(
  'ex <- compute_exposure(demo_influence, content = "y", via = "tie",',
  '                       normalize = "row", wave = 1)',
  'head(ex)',
  'summary(ex$ex_y_tie)'
))

# ===========================================================================
# Influence: fit
# ===========================================================================

cap("a-fit", R(
  'fit <- fit_influence(',
  '  y ~ ar() + exposure(y, via = tie, normalize = "row"),',
  '  data = demo_influence',
  ')',
  'fit'
))

cap("a-fit-covariates", R(
  'fit2 <- fit_influence(',
  '  y ~ ar() + exposure(y, via = tie, normalize = "row") + x + lagged(x),',
  '  data = demo_influence',
  ')',
  'netis_estimates(fit2)'
))

cap("a-fit-centrality", R(
  'fit3 <- fit_influence(',
  '  y ~ ar() + exposure(y, via = tie, normalize = "row") +',
  '    centrality(tie, "degree", mode = "in"),',
  '  data = demo_influence',
  ')',
  'netis_estimates(fit3)'
))

cap("a-fit-derived", R(
  'fit_influence(',
  '  y ~ ar() + exposure(y, via = symmetrize(tie, "max"), normalize = "row"),',
  '  data = demo_influence',
  ')'
))

cap("a-waves-refuse", R(
  'three <- simulate_influence(n_nodes = 20, beta = 0.3, normalize = "row",',
  '                            n_waves = 3, seed = 4)',
  'fit_influence(y ~ ar() + exposure(y, via = tie, normalize = "row"),',
  '              data = three$data)'
), expect_error = TRUE)

cap("a-waves-named", R(
  'fit_influence(y ~ ar() + exposure(y, via = tie, normalize = "row"),',
  '              data = three$data, waves = c(2, 3))'
))

# ===========================================================================
# Influence: read the results
# ===========================================================================

cap("a-summary", R(
  'summary(fit)'
))

cap("a-estimates", R(
  'netis_estimates(fit)'
))

cap("a-glance", R(
  'netis_glance(fit)'
))

cap("a-estimand", R(
  'netis_estimand(fit)'
))

cap("a-effect", R(
  'eff <- influence_effect(fit)',
  'eff'
))

cap("a-effect-fields", R(
  'eff$exposures[["ex_y_tie"]][c("coefficient", "contrast_low", "contrast_high",',
  '                             "predicted_change")]'
))

cap("a-effect-probs", R(
  'influence_effect(fit, probs = c(0.25, 0.75))$exposures[["ex_y_tie"]]$predicted_change'
))

setup(R(
  'pseudo_r2(fit)'
))

capfig("a-plot-coef", R(
  'plot(fit)'
), width = 6.8, height = 3.4)

capfig("a-plot-effects", R(
  'plot(fit, what = "effects")'
), width = 6.8, height = 3.8)

# ===========================================================================
# Influence: check the fit
# ===========================================================================

cap("a-assumptions", R(
  'netis_assumptions(fit)'
))

cap("a-declared", R(
  'declared <- fit_influence(',
  '  y ~ ar() + exposure(y, via = tie, normalize = "row"),',
  '  data = demo_influence,',
  '  network_exogeneity = "exogenous",',
  '  exposure_orthogonal_to_actor_effect = TRUE',
  ')',
  'netis_assumptions(declared)$network$exogeneity',
  'all.equal(coef(fit), coef(declared))'
))

cap("a-dependence", R(
  'netis_dependence(fit, demo_influence)'
))

capfig("a-plot-dependence", R(
  'plot(netis_dependence(fit, demo_influence))'
), width = 7.0, height = 3.8)

# ===========================================================================
# Influence: several networks
# ===========================================================================

cap("a-mg-refuse", R(
  'fit_influence(y ~ ar() + exposure(x, via = tie), data = demo_influence_groups)'
), expect_error = TRUE)

cap("a-mg", R(
  'mg <- fit_influence(',
  '  y ~ ar() + exposure(x, via = tie) + (1 | group),',
  '  data = demo_influence_groups',
  ')',
  'mg'
))

cap("a-mg-vc", R(
  'netis_variance_components(mg)'
))

cap("a-se", R(
  'influence_se_comparison(mg, by = "group")'
))

cap("a-se-addresses", R(
  'attr(influence_se_comparison(mg, by = "group"), "addresses")'
))

cap("a-se-refuse", R(
  'influence_se_comparison(fit, by = "group")'
), expect_error = TRUE)

cap("a-icc", R(
  'icc1(mg)',
  'icc2(mg)'
))

cap("a-rwg", R(
  'set.seed(1)',
  'ratings <- matrix(stats::rnorm(60, 4, 0.4), ncol = 3)',
  'teams   <- rep(c("a", "b"), each = 10)',
  '',
  'rwg(ratings[, 1], teams, null = list(dist = "uniform", n_options = 5))'
))

cap("a-rwg-j", R(
  'rwg_j(ratings, teams, null = list(dist = "uniform", n_options = 5))'
))

capfig("a-plot-heterogeneity", R(
  'plot(mg, what = "heterogeneity")'
), width = 6.8, height = 4.0)

# ===========================================================================
# Derived variables
# ===========================================================================

cap("v-centering", R(
  'centred <- add_centering(demo_influence, "x", method = "grand")',
  'centred'
))

cap("v-decomposition", R(
  'split_x <- add_decomposition(demo_influence_groups, "x", unit = "context")',
  'split_x'
))

cap("v-baseline", R(
  'bp <- add_baseline_provenance(demo_influence, "y")',
  'bp'
))

# The simulation block the tutorial shows is s-precision, in run-b.R: it holds
# mean degree constant across three sizes, which the earlier single-size
# versions did not, so their intervals differed through degree as much as
# through n. They are not captured here for that reason.

.log("run-a done")
