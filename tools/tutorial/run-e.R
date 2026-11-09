# Re-render every figure at a width that fits the package's own legend and
# caption. The first pass cropped both: rnetis's plot theme carries a wide
# categorical legend and a multi-line caption, and ggsave clips rather than
# shrinking them.
#   Rscript run-e.R <outdir>
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")
W <- 11.5          # inches; displayed at ~700 px, so this is a 2x asset

# --- data and Influence figures -------------------------------------------
capfig("d-plot-network",      R('plot(demo_influence, wave = 1)'), width = W, height = 8.4)
capfig("d-plot-coverage",     R('plot(demo_influence, what = "coverage")'), width = W, height = 5.0)

.log("refitting influence models")
fit <- fit_influence(y ~ ar() + exposure(y, via = tie, normalize = "row"),
                     data = demo_influence)
mg <- fit_influence(y ~ ar() + exposure(x, via = tie) + (1 | group),
                    data = demo_influence_groups)
assign("fit", fit, envir = ENV); assign("mg", mg, envir = ENV)

capfig("a-plot-coef",          R('plot(fit)'), width = W, height = 5.0)
capfig("a-plot-effects",       R('plot(fit, what = "effects")'), width = W, height = 5.2)
capfig("a-plot-dependence",    R('plot(netis_dependence(fit, demo_influence))'), width = W, height = 5.4)
capfig("a-plot-heterogeneity", R('plot(mg, what = "heterogeneity")'), width = W, height = 5.6)

# --- Selection figures -----------------------------------------------------
.log("refitting selection models")
nodes <- netis_nodes(demo_selection)
ties  <- netis_ties(demo_selection)
unit  <- rep(c("u1", "u2"), length.out = nrow(nodes)); names(unit) <- nodes$node
ties$same_unit <- as.numeric(unit[ties$source] == unit[ties$target])
staff <- netis_data(nodes, ties, wave_col = "wave", tie_meta = list(
  tie = list(zero_meaning = "measured_zero"),
  same_unit = list(zero_meaning = "measured_zero")))

sel <- fit_selection(
  tie ~ sender(tenure) + receiver(tenure) + dyad(same_unit) + prior_tie(),
  data = staff, nscan = 5000, burn = 1000, odens = 10, seed = 1)
ame2 <- fit_selection(
  tie ~ sender(tenure) + receiver(tenure) + prior_tie(),
  data = staff, R = 2, nscan = 5000, burn = 1000, odens = 10, seed = 1)

two_groups <- netis_data(
  node_data = rbind(transform(netis_nodes(demo_selection), group = "a"),
                    transform(netis_nodes(demo_selection), group = "b")),
  tie_data  = rbind(transform(netis_ties(demo_selection), group = "a"),
                    transform(netis_ties(demo_selection), group = "b")),
  group_data = data.frame(group = c("a", "b"), size = c(8, 30)),
  wave_col = "wave",
  tie_meta = list(tie = list(zero_meaning = "measured_zero")))
separate <- fit_selection(tie ~ sender(tenure) + prior_tie(), data = two_groups,
                          pool_beta = FALSE, nscan = 5000, burn = 1000,
                          odens = 10, seed = 1)

assign("sel", sel, envir = ENV); assign("ame2", ame2, envir = ENV)
assign("separate", separate, envir = ENV)

capfig("b-plot-coef",      R('plot(sel)'), width = W, height = 5.0)
capfig("b-plot-trace",     R('plot(sel, what = "trace")'), width = W, height = 6.0)
capfig("b-plot-posterior", R('plot(sel, what = "posterior")'), width = W, height = 5.6)
capfig("b-plot-actor",     R('plot(sel, what = "actor_effects")'), width = W, height = 6.0)
capfig("b-plot-mcmc",      R('plot(sel, what = "mcmc")'), width = W, height = 5.4)
capfig("b-plot-mult",      R('plot(ame2, what = "multiplicative")'), width = W, height = 7.0)
capfig("b-plot-groups",    R('plot(separate, what = "groups")'), width = W, height = 5.2)

# --- the hierarchical panel, from the chain run-d already fitted -----------
rds <- file.path(OUT, "hier.rds")
if (file.exists(rds)) {
  assign("hier", readRDS(rds), envir = ENV)
  capfig("b-hier-panel-groups", 'plot(hier, what = "groups")',
         width = W, height = 6.0)
} else {
  .log("hier.rds not present; run run-d.R first")
}

.log("run-e done")
