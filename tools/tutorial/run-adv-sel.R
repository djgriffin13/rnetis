# Captures for the advanced Selection walkthrough.
#   Rscript run-adv-sel.R <outdir>                               (~20 min)
#
# THE SAME DATA AND THE SAME MODEL AS PAGES B1 TO B8, with one exception. The
# walkthrough goes back over each step of that sequence and shows the
# alternatives, so every block varies one thing about the model the reader has
# already fitted.
#
# THE EXCEPTION is the fit check. The team data has no clustering for latent
# factors to find, so pages B5 and B7 only ever show a check that passes. A
# reader who has only seen it pass has learned that it prints five numbers.
# The `offices` data below is built with three dense clusters so that the
# check fails, latent factors repair it, and the latent-coordinates plot has
# something to show.
#
# CHAIN LENGTHS. The reference fit runs the package defaults, as page B4 does.
# Everything else runs a shorter chain to keep the script near twenty minutes,
# and the page says so.
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")
W <- 11.5
F1 <- c(
  '  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +',
  '    prior_tie(lag = 1),'
)

.log("the reference fit (about two minutes)")
cap("xs-setup", R(
  'library(rnetis)',
  '',
  'team  <- demo_affect',
  'teams <- demo_affect_teams',
  '',
  'fit <- fit_selection(',
  F1,
  '  data = team,',
  '  nscan = 10000, burn = 500, odens = 25, seed = 1',
  ')',
  'round(coef(fit), 2)'
))

# ---- check the design -------------------------------------------------------
cap("xs-design", R('diagnose_design(team)'))
cap("xs-noprior-refuse", R(
  'fit_selection(confides ~ dyad(affect_gap, lag = 1), data = team)'
), expect_error = TRUE)
.log("no prior tie, waves named")
cap("xs-waves", R(
  'fit_level <- fit_selection(',
  '  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1),',
  '  data = team, waves = c(1, 2),',
  '  nscan = 3000, burn = 500, odens = 10, seed = 1',
  ')',
  'round(rbind("with prior_tie()" = coef(fit)[1:3], "without" = coef(fit_level)), 2)'
))

# ---- predictors -------------------------------------------------------------
.log("another tie as a predictor")
cap("xs-tie-predictor", R(
  'fit_advice <- fit_selection(',
  '  confides ~ dyad(advice, lag = 1) + dyad(affect_gap, lag = 1) +',
  '    dyad(same_project, lag = 1) + prior_tie(lag = 1),',
  '  data = team,',
  '  nscan = 3000, burn = 500, odens = 10, seed = 1',
  ')',
  'fit_advice'
))

# ---- binary or valued -------------------------------------------------------
.log("a valued outcome")
cap("xs-valued", R(
  'fit_talk <- fit_selection(',
  '  talk ~ dyad(same_project, lag = 1) + dyad(confides, lag = 1) +',
  '    sender(remote, lag = 1) + receiver(remote, lag = 1),',
  '  data = team, family = "nrm", waves = c(1, 2),',
  '  nscan = 3000, burn = 500, odens = 10, seed = 1',
  ')',
  'fit_talk'
))
.log("a valued outcome, binarized")
cap("xs-binarize", R(
  'fit_talk2 <- fit_selection(',
  '  talk ~ dyad(same_project, lag = 1) + dyad(confides, lag = 1) +',
  '    sender(remote, lag = 1) + receiver(remote, lag = 1),',
  '  data = team, binarize = threshold(2), waves = c(1, 2),',
  '  nscan = 3000, burn = 500, odens = 10, seed = 1',
  ')',
  'round(coef(fit_talk2), 2)'
))
cap("xs-binarize-refuse", R(
  'fit_selection(talk ~ dyad(same_project, lag = 1), data = team,',
  '              family = "nrm", binarize = threshold(2), waves = c(1, 2))'
), expect_error = TRUE)

# ---- directed or undirected -------------------------------------------------
cap("xs-undirected-data", R(
  'ties <- netis_ties(team)',
  '',
  '# A pair is "mutual" when confiding runs in both directions.',
  'reverse <- match(paste(ties$wave, ties$target, ties$source),',
  '                 paste(ties$wave, ties$source, ties$target))',
  'ties$mutual <- as.numeric(ties$confides == 1 & ties$confides[reverse] == 1)',
  '',
  '# An undirected tie table lists each pair once.',
  'once <- ties[ties$source < ties$target,',
  '             c("group", "wave", "source", "target", "mutual", "same_project", "affect_gap")]',
  '',
  'pairs_data <- netis_data(',
  '  netis_nodes(team), once, wave_col = "wave", directed = FALSE,',
  '  tie_meta = list(',
  '    mutual       = list(zero_meaning = "measured_zero"),',
  '    same_project = list(zero_meaning = "measured_zero"),',
  '    affect_gap   = list(zero_meaning = "measured_zero", weighted = TRUE)',
  '  )',
  ')',
  'pairs_data'
))
cap("xs-undirected-refuse", R(
  'fit_selection(mutual ~ dyad(affect_gap, lag = 1) + prior_tie(lag = 1),',
  '              data = pairs_data)'
), expect_error = TRUE)
.log("the symmetric fit")
cap("xs-symmetric", R(
  'fit_mutual <- fit_selection(',
  '  mutual ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +',
  '    prior_tie(lag = 1),',
  '  data = pairs_data, symmetric = TRUE,',
  '  nscan = 3000, burn = 500, odens = 10, seed = 1',
  ')',
  'fit_mutual'
))
cap("xs-symmetric-vc", R(
  'netis_variance_components(fit_mutual)[, c("term", "estimate")]'
))

# ---- effect sizes -----------------------------------------------------------
cap("xs-effect-types", R(
  'effect <- function(type) {',
  '  e <- selection_effect(fit, "dyd_same_project_lag1", type = type)',
  '  round(100 * c(estimate = e$estimate, lower = e$interval[[1]], upper = e$interval[[2]]), 1)',
  '}',
  'rbind(conditional = effect("conditional"),',
  '      population  = effect("population"),',
  '      reference   = effect("reference"))'
))
cap("xs-effect-delta", R(
  'selection_effect(fit, "dyd_affect_gap_lag1", delta = 0.5)'
))

# ---- the chain --------------------------------------------------------------
.log("a chain that is too short")
cap("xs-short", R(
  'fit_short <- fit_selection(',
  F1,
  '  data = team,',
  '  nscan = 300, burn = 20, odens = 5, seed = 1',
  ')',
  'convergence <- function(f) {',
  '  d <- plot_data(f, what = "mcmc")',
  '  wide <- reshape(d[, c("term", "statistic", "value")], idvar = "term",',
  '                  timevar = "statistic", direction = "wide")',
  '  names(wide) <- c("term", "R-hat", "bulk ESS", "tail ESS")',
  '  wide[, 2:4] <- round(wide[, 2:4], 2)',
  '  wide',
  '}',
  'convergence(fit_short)'
))
cap("xs-long", R('convergence(fit)'))
.log("a second seed")
cap("xs-seed", R(
  'fit_seed2 <- fit_selection(',
  F1,
  '  data = team,',
  '  nscan = 10000, burn = 500, odens = 25, seed = 2',
  ')',
  'round(rbind("seed 1" = coef(fit), "seed 2" = coef(fit_seed2)), 3)'
))

# ---- a fit check that fails -------------------------------------------------
cap("xs-offices", R(
  '# Twenty-four people in three offices of eight. People confide mostly',
  '# within their own office, and nothing in the data records the office.',
  'set.seed(11)',
  'id     <- sprintf("q%02d", 1:24)',
  'office <- rep(1:3, each = 8)',
  'pairs  <- expand.grid(source = id, target = id, stringsAsFactors = FALSE)',
  'pairs  <- pairs[pairs$source != pairs$target, ]',
  'same   <- office[match(pairs$source, id)] == office[match(pairs$target, id)]',
  '',
  'office_ties <- rbind(',
  '  data.frame(group = "firm", wave = 1, pairs,',
  '             confides = rbinom(nrow(pairs), 1, ifelse(same, 0.7, 0.05))),',
  '  data.frame(group = "firm", wave = 2, pairs,',
  '             confides = rbinom(nrow(pairs), 1, ifelse(same, 0.7, 0.05)))',
  ')',
  'office_nodes <- data.frame(group = "firm", node = rep(id, 2), wave = rep(1:2, each = 24))',
  '',
  'offices <- netis_data(office_nodes, office_ties, wave_col = "wave",',
  '  tie_meta = list(confides = list(zero_meaning = "measured_zero")))'
))
.log("offices: R = 0")
cap("xs-misfit", R(
  'fit_r0 <- fit_selection(confides ~ prior_tie(lag = 1), data = offices,',
  '                        nscan = 6000, burn = 1000, odens = 10, seed = 1)',
  'netis_gof(fit_r0)[, c("statistic", "observed", "predicted_lower", "predicted_upper", "ppp")]'
))
capfig("xs-misfit-plot", R('plot(fit_r0, what = "gof")'), width = W, height = 4.4)
.log("offices: R = 0 to 3")
cap("xs-choose-r", R(
  '# The clustering feature, at each R: what was observed, and the range the',
  '# fitted model simulates.',
  'check <- function(R) {',
  '  f <- fit_selection(confides ~ prior_tie(lag = 1), data = offices, R = R,',
  '                     nscan = 6000, burn = 1000, odens = 10, seed = 1)',
  '  g <- netis_gof(f)',
  '  g <- g[g$statistic == "trans.dep", ]',
  '  round(c(observed = g$observed, simulated_low = g$predicted_lower,',
  '          simulated_high = g$predicted_upper, ppp = g$ppp), 3)',
  '}',
  'rbind("R = 0" = check(0), "R = 1" = check(1), "R = 2" = check(2), "R = 3" = check(3))'
))
.log("offices: the R = 2 fit, for its picture")
cap("xs-offices-r2", R(
  'fit_r2 <- fit_selection(confides ~ prior_tie(lag = 1), data = offices, R = 2,',
  '                        nscan = 6000, burn = 1000, odens = 10, seed = 1)'
))
capfig("xs-offices-latent", R('plot(fit_r2, what = "latent")'), width = 10.5, height = 6.4)
.log("offices: the office, recorded")
cap("xs-office-known", R(
  '# Suppose the office had been recorded: add it to the tie table as a',
  '# characteristic of the pair.',
  'office_ties$same_office <- as.numeric(rep(same, 2))',
  'offices_known <- netis_data(office_nodes, office_ties, wave_col = "wave",',
  '  tie_meta = list(confides    = list(zero_meaning = "measured_zero"),',
  '                  same_office = list(zero_meaning = "measured_zero")))',
  '',
  'fit_known <- fit_selection(',
  '  confides ~ dyad(same_office, lag = 1) + prior_tie(lag = 1),',
  '  data = offices_known,',
  '  nscan = 6000, burn = 1000, odens = 10, seed = 1',
  ')',
  'netis_gof(fit_known)[, c("statistic", "observed", "predicted_lower", "predicted_upper", "ppp")]'
))

# ---- people's own tendencies ------------------------------------------------
cap("xs-actor", R(
  'actors <- plot_data(fit, what = "actor_effects")',
  'head(actors[order(-actors$receiver), c("node", "sender", "receiver")])'
))
cap("xs-vc", R('netis_variance_components(fit)[, c("term", "estimate")]'))

# ---- several teams ----------------------------------------------------------
.log("ten teams, hierarchical (several minutes)")
cap("xs-hier", R(
  'fit_hier <- fit_selection(',
  F1,
  '  data = teams,',
  '  pooling = selection_pooling(random = c("intercept", "dyd_affect_gap_lag1")),',
  '  nscan = 3000, burn = 500, odens = 10, chains = 2, seed = 1',
  ')',
  'h <- netis_hierarchy(fit_hier)',
  'h$population[, c("coefficient", "parameter", "estimate", "lower", "upper")]'
))
cap("xs-hier-groups", R(
  'own <- h$groups[h$groups$coefficient == "dyd_affect_gap_lag1",',
  '                c("group", "beta_g", "lower", "upper", "shrinkage")]',
  'head(own)'
))
cap("xs-pooling-refuse", R(
  'fit_selection(confides ~ prior_tie(lag = 1), data = teams, pool_beta = FALSE,',
  '              pooling = selection_pooling(random = "intercept"))'
), expect_error = TRUE)
.log("done")
