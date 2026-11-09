# Blocks for the Selection path, the hierarchical model, the latent-space
# adjustment, and the precision table.
#   Rscript run-b.R <outdir>
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")

# ===========================================================================
# Selection: the data and the grammar
# ===========================================================================

cap("b-demo", R(
  'demo_selection'
))

cap("b-bare-refuse", R(
  'fit_selection(tie ~ tenure, data = demo_selection)'
), expect_error = TRUE)

cap("b-prepare", R(
  'nodes <- netis_nodes(demo_selection)',
  'ties  <- netis_ties(demo_selection)',
  '',
  'unit <- rep(c("u1", "u2"), length.out = nrow(nodes))',
  'names(unit) <- nodes$node',
  'ties$same_unit <- as.numeric(unit[ties$source] == unit[ties$target])',
  '',
  'staff <- netis_data(nodes, ties, wave_col = "wave", tie_meta = list(',
  '  tie       = list(zero_meaning = "measured_zero"),',
  '  same_unit = list(zero_meaning = "measured_zero")',
  '))',
  'staff'
))

cap("b-waves-refuse", R(
  'fit_selection(tie ~ sender(tenure), data = demo_selection,',
  '              nscan = 50, burn = 10)'
), expect_error = TRUE)

# ===========================================================================
# Selection: fit
# ===========================================================================

cap("b-fit", R(
  'sel <- fit_selection(',
  '  tie ~ sender(tenure) + receiver(tenure) + dyad(same_unit) + prior_tie(),',
  '  data = staff,',
  '  nscan = 5000, burn = 1000, odens = 10, seed = 1',
  ')',
  'sel'
))

cap("b-summary", R(
  'summary(sel)'
))

cap("b-diagnostics", R(
  'netis_diagnostics(sel)[, c("family", "code", "level", "summary")]'
))

cap("b-diagnostics-detail", R(
  'netis_diagnostics(sel)$detail[[1]]'
))

cap("b-waves", R(
  'sel$waves$initial_waves',
  'sel$waves$outcome_waves'
))

cap("b-estimates", R(
  'netis_estimates(sel)'
))

cap("b-glance", R(
  'netis_glance(sel)'
))

cap("b-vc", R(
  'netis_variance_components(sel)'
))

cap("b-reciprocity", R(
  'recip <- fit_selection(',
  '  tie ~ sender(tenure) + prior_tie() + prior_reciprocity(),',
  '  data = staff,',
  '  nscan = 5000, burn = 1000, odens = 10, seed = 1',
  ')',
  'netis_estimates(recip)'
))

cap("b-fit-multiplicative", R(
  'ame2 <- fit_selection(',
  '  tie ~ sender(tenure) + receiver(tenure) + prior_tie(),',
  '  data = staff, R = 2,',
  '  nscan = 5000, burn = 1000, odens = 10, seed = 1',
  ')',
  'netis_estimates(ame2)'
))

# ===========================================================================
# Selection: effect sizes
# ===========================================================================

cap("b-effect-prior", R(
  'selection_effect(sel, "prior_tie")'
))

cap("b-effect-pop", R(
  'selection_effect(sel, "snd_tenure", delta = 1, type = "population")'
))

cap("b-effect-ref", R(
  'selection_effect(sel, "snd_tenure", delta = 1, type = "reference")'
))

cap("b-pseudo", R(
  'pseudo_r2(sel)'
))

capfig("b-plot-coef",      R('plot(sel)'),                        width = 6.8, height = 3.6)
capfig("b-plot-trace",     R('plot(sel, what = "trace")'),        width = 7.2, height = 4.4)
capfig("b-plot-posterior", R('plot(sel, what = "posterior")'),    width = 7.2, height = 4.0)
capfig("b-plot-actor",     R('plot(sel, what = "actor_effects")'), width = 6.6, height = 4.4)
capfig("b-plot-mcmc",      R('plot(sel, what = "mcmc")'),         width = 7.2, height = 4.0)
capfig("b-plot-mult",      R('plot(ame2, what = "multiplicative")'), width = 6.4, height = 5.0)

cap("b-plot-data-trace", R(
  'head(plot_data(sel, what = "trace"))'
))

# ===========================================================================
# Selection: several groups
# ===========================================================================

cap("b-two-groups", R(
  'two_groups <- netis_data(',
  '  node_data = rbind(transform(netis_nodes(demo_selection), group = "a"),',
  '                    transform(netis_nodes(demo_selection), group = "b")),',
  '  tie_data  = rbind(transform(netis_ties(demo_selection), group = "a"),',
  '                    transform(netis_ties(demo_selection), group = "b")),',
  '  group_data = data.frame(group = c("a", "b"), size = c(8, 30)),',
  '  wave_col = "wave",',
  '  tie_meta = list(tie = list(zero_meaning = "measured_zero"))',
  ')',
  'two_groups'
))

cap("b-pooled", R(
  'pooled <- fit_selection(tie ~ sender(tenure) + prior_tie(), data = two_groups,',
  '                        nscan = 5000, burn = 1000, odens = 10, seed = 1)',
  'pooled'
))

cap("b-pooled-effect", R(
  'selection_effect(pooled, "prior_tie", group = "a")'
))

cap("b-pooled-vcov-refuse", R(
  'vcov(pooled)'
), expect_error = TRUE)

cap("b-separate", R(
  'separate <- fit_selection(tie ~ sender(tenure) + prior_tie(), data = two_groups,',
  '                          pool_beta = FALSE,',
  '                          nscan = 5000, burn = 1000, odens = 10, seed = 1)',
  'coef(separate)'
))

cap("b-group-refuse", R(
  'fit_selection(tie ~ group(size) + prior_tie(), data = two_groups)'
), expect_error = TRUE)

capfig("b-plot-groups", R('plot(separate, what = "groups")'), width = 6.8, height = 3.8)

# ===========================================================================
# Selection: a hierarchical model over every group at once
# ===========================================================================

cap("b-teams", R(
  'set.seed(42)',
  'teams <- sprintf("t%02d", 1:12)',
  '',
  'team_nodes <- do.call(rbind, lapply(teams, function(g) {',
  '  data.frame(group = g, node = sprintf("%s_%d", g, 1:8),',
  '             wave = rep(1:2, each = 8),',
  '             extraversion = rep(stats::rnorm(8), 2))',
  '}))',
  '',
  'team_ties <- do.call(rbind, lapply(teams, function(g) {',
  '  ids <- sprintf("%s_%d", g, 1:8)',
  '  x <- team_nodes$extraversion[team_nodes$group == g & team_nodes$wave == 1]',
  '  p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)',
  '  p <- p[p$source != p$target, ]',
  '  eta <- stats::rnorm(1, -0.6, 0.4) +',
  '         stats::rnorm(1, 0.4, 0.25) * x[match(p$source, ids)]',
  '  do.call(rbind, lapply(1:2, function(w) {',
  '    data.frame(group = g, p, wave = w,',
  '               tie = stats::rbinom(nrow(p), 1, stats::pnorm(eta)))',
  '  }))',
  '}))',
  '',
  'teams_data <- netis_data(team_nodes, team_ties, wave_col = "wave",',
  '                         tie_meta = list(tie = list(zero_meaning = "measured_zero")))',
  'teams_data'
))

cap("b-pooling-spec", R(
  'selection_pooling(random = c("intercept", "snd_extraversion"))'
))

cap("b-hier", R(
  'hier <- fit_selection(',
  '  tie ~ sender(extraversion) + prior_tie(), data = teams_data,',
  '  pooling = selection_pooling(random = c("intercept", "snd_extraversion")),',
  '  nscan = 10000, burn = 1000, odens = 10, chains = 2',
  ')',
  'hier'
))

cap("b-hier-pop", R(
  'netis_hierarchy(hier)$population'
))

cap("b-hier-groups", R(
  'head(netis_hierarchy(hier)$groups, 8)'
))

cap("b-hier-conv", R(
  'netis_hierarchy(hier)$convergence'
))

capfig("b-plot-hier", R('plot(hier, what = "heterogeneity")'), width = 6.8, height = 4.6)

# ===========================================================================
# The confounding problem, and the latent-space adjustment
# ===========================================================================

cap("c-latent", R(
  'lat <- fit_influence(',
  '  y ~ ar() + exposure(y, via = tie, normalize = "row") +',
  '    latent(tie, d = 1, homophily = c("y", "x"),',
  '           control = list(sample.size = 5000, burnin = 20000, interval = 10)),',
  '  data = demo_influence',
  ')',
  'summary(lat)'
))

cap("c-latent-compare", R(
  'rbind(unadjusted = netis_estimates(fit_influence(',
  '        y ~ ar() + exposure(y, via = tie, normalize = "row"),',
  '        data = demo_influence))[3, c("estimate", "conf.low", "conf.high")],',
  '      adjusted   = netis_estimates(lat)[3, c("estimate", "conf.low", "conf.high")])'
))

cap("c-latent-mg", R(
  'per_group <- fit_influence(',
  '  y ~ ar() + exposure(x, via = tie) + latent(tie, d = 1, homophily = "x") +',
  '    (1 | group),',
  '  data = demo_influence_groups',
  ')',
  'netis_estimates(per_group)'
))

# ===========================================================================
# How precise is an exposure coefficient?
# ===========================================================================

cap("s-precision", R(
  'for (n in c(30, 120, 480)) {',
  '  s <- simulate_influence(n_nodes = n, density = 4.5 / (n - 1),',
  '                          gamma = 0.3, beta = 0.4,',
  '                          normalize = "row", seed = 7)',
  '  e <- netis_estimates(fit_influence(',
  '    y ~ ar() + exposure(y, via = tie, normalize = "row"), data = s$data))',
  '  cat(sprintf("%3d nodes:  exposure %5.2f, 95%% interval [%5.2f, %5.2f]\\n",',
  '              n, e$estimate[3], e$conf.low[3], e$conf.high[3]))',
  '}'
))

.log("run-b done")
