# Re-capture the hierarchical block as a unit, so its tables and its figure
# come from ONE chain. The first run left the figure missing because
# plot(what = "heterogeneity") is not defined for a hierarchical fit; redoing
# only the plot against a refitted chain would have put a picture next to
# tables from a different chain.
#   Rscript run-d.R <outdir>
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")

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
  '  nscan = 10000, burn = 1000, odens = 10, chains = 2, seed = 1',
  ')',
  'hier'
))

saveRDS(get("hier", envir = ENV), file.path(OUT, "hier.rds"))

cap("b-hier-pop",    R('netis_hierarchy(hier)$population'))
cap("b-hier-groups", R('head(netis_hierarchy(hier)$groups, 8)'))
cap("b-hier-conv",   R('netis_hierarchy(hier)$convergence'))

# The figure the page uses. A hierarchical fit also supports "coefficients",
# "trace", "posterior" and "mcmc"; "heterogeneity" is NOT defined for one, and
# asking for it is what left this figure missing on the first pass.
#
# Rendered here at a provisional size only so that run-d.R is self-contained;
# run-e.R re-renders it at publication width from out/hier.rds.
capfig("b-hier-panel-groups", 'plot(hier, what = "groups")',
       width = 7.0, height = 4.6)

.log("run-d done")
