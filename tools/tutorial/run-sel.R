# Captures for the Selection sequence (pages B1 to B8).
#   Rscript run-sel.R <outdir>                                   (~25 min)
#
# THE SAME DATA AS THE INFLUENCE SEQUENCE. `demo_affect` and
# `demo_affect_teams` ship with the package and page A3 builds the first of
# them by hand; nothing is rebuilt here. The Influence pages ask whether affect
# spreads along confiding ties. These ask who comes to confide in whom.
#
# EVERY LAG IS WRITTEN OUT, as on the Influence pages, so the timing of each
# predictor can be read in the code. The node and tie covariates do not change
# between waves, so `lag = 1` selects the same values as leaving it out; what
# it changes is what the formula says, and rnetis records a NOTE to that
# effect in place of a WARNING about a predictor measured with the outcome.
#
# CHAIN LENGTHS. One team runs the package defaults, written out on page B4.
# The multi-team fits run shorter chains so the whole script stays near
# twenty-five minutes; the pages say so.
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")
W <- 11.5

F1 <- c(
  '  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +',
  '    prior_tie(lag = 1),'
)

# ===========================================================================
# B1 - run a model first
# ===========================================================================
.log("B1: default chain (about two minutes)")
cap("s1-run", R(
  'library(rnetis)',
  '',
  'fit <- fit_selection(',
  F1,
  '  data = demo_affect, seed = 1',
  ')',
  'summary(fit)'
))

# ===========================================================================
# B3 - the data, read for Selection
# ===========================================================================
cap("s3-object", R(
  'team <- demo_affect',
  'team'
))
cap("s3-ties", R(
  'ties <- netis_ties(team)',
  'head(ties[ties$wave == 2, ])'
))
cap("s3-density", R(
  '# How many of the 1,560 possible ties exist at each survey?',
  'tapply(ties$confides, ties$wave, sum)',
  '',
  '# What happened to the ties and the non-ties of the first survey?',
  'table(first = ties$confides[ties$wave == 1], second = ties$confides[ties$wave == 2])'
))

# ===========================================================================
# B4 - running the model
# ===========================================================================
.log("B4: the same model on `team`, chain settings written out")
cap("s4-fit", R(
  'fit <- fit_selection(',
  F1,
  '  data = team,',
  '  nscan = 10000, burn = 500, odens = 25, seed = 1',
  ')',
  'fit'
))
.log("B4: sender and receiver covariates")
cap("s4-covariates", R(
  'fit2 <- fit_selection(',
  '  confides ~ sender(extraversion, lag = 1) + receiver(extraversion, lag = 1) +',
  '    dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +',
  '    prior_tie(lag = 1),',
  '  data = team,',
  '  nscan = 10000, burn = 500, odens = 25, seed = 1',
  ')',
  'fit2'
))
.log("B4: reciprocity")
cap("s4-reciprocity", R(
  'fit_recip <- fit_selection(',
  '  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +',
  '    prior_tie(lag = 1) + prior_reciprocity(lag = 1),',
  '  data = team,',
  '  nscan = 10000, burn = 500, odens = 25, seed = 1',
  ')',
  'round(coef(fit_recip), 2)'
))
cap("s4-refuse-plain", R(
  'fit_selection(confides ~ extraversion + prior_tie(lag = 1), data = team)'
), expect_error = TRUE)

# ===========================================================================
# B5 - interpreting
# ===========================================================================
cap("s5-summary", R('summary(fit)'))
cap("s5-effect", R('selection_effect(fit, "dyd_same_project_lag1")'))
cap("s5-effect-gap", R('selection_effect(fit, "dyd_affect_gap_lag1", delta = 1)'))
cap("s5-gof", R(
  'gof <- netis_gof(fit)',
  'gof[, c("statistic", "observed", "predicted_lower", "predicted_upper", "ppp")]'
))
cap("s5-pseudo", R('pseudo_r2(fit)'))
cap("s5-mcmc", R(
  'conv <- plot_data(fit, what = "mcmc")',
  'reshape(conv[, c("term", "statistic", "value")], idvar = "term",',
  '        timevar = "statistic", direction = "wide")'
))
cap("s5-vc", R('netis_variance_components(fit)[, c("term", "estimate")]'))
cap("s5-diagnostics", R('netis_diagnostics(fit)[, c("level", "summary")]'))
cap("s5-loglik", R('AIC(fit)'), expect_error = TRUE)

fact("s-gap",     'sprintf("%.2f", coef(fit)[["dyd_affect_gap_lag1"]])')
fact("s-project", 'sprintf("%.2f", coef(fit)[["dyd_same_project_lag1"]])')
fact("s-prior",   'sprintf("%.2f", coef(fit)[["prior_tie"]])')
fact("s-gap-lo",  'sprintf("%.2f", netis_estimates(fit)$conf.low[netis_estimates(fit)$term == "dyd_affect_gap_lag1"])')
fact("s-gap-hi",  'sprintf("%.2f", netis_estimates(fit)$conf.high[netis_estimates(fit)$term == "dyd_affect_gap_lag1"])')
fact("s-eff-project", 'sprintf("%.1f", 100 * selection_effect(fit, "dyd_same_project_lag1")$estimate)')
fact("s-eff-gap",     'sprintf("%.1f", abs(100 * selection_effect(fit, "dyd_affect_gap_lag1", delta = 1)$estimate))')
fact("s-ties-1", 'format(sum(ties$confides[ties$wave == 1]))')
fact("s-ties-2", 'format(sum(ties$confides[ties$wave == 2]))')

# ===========================================================================
# B6 - visualizing
# ===========================================================================
capfig("s6-network", R('plot(team, variable = "confides", wave = 2, layout = "mds")'),
       width = W, height = 8.4)
capfig("s6-coef",  R('plot(fit)'), width = W, height = 4.6)
capfig("s6-gof",   R('plot(fit, what = "gof")'), width = W, height = 4.4)
capfig("s6-actor", R('plot(fit, what = "actor_effects")'), width = 8.5, height = 6.4)
capfig("s6-trace", R('plot(fit, what = "trace")'), width = W, height = 6.0)

# ===========================================================================
# B7 - latent factors
# ===========================================================================
.log("B7: R = 2")
cap("s7-fit", R(
  'fit_latent <- fit_selection(',
  F1,
  '  data = team, R = 2,',
  '  nscan = 10000, burn = 500, odens = 25, seed = 1',
  ')',
  'fit_latent'
))
cap("s7-compare", R(
  'round(rbind(additive = coef(fit), latent = coef(fit_latent)), 2)'
))
cap("s7-gof", R(
  'both <- merge(netis_gof(fit), netis_gof(fit_latent), by = "statistic",',
  '              suffixes = c(".additive", ".latent"))',
  'both[, c("statistic", "observed.additive", "ppp.additive", "ppp.latent")]'
))
capfig("s7-latent", R('plot(fit_latent, what = "latent")'), width = W, height = 6.4)
capfig("s7-mult",   R('plot(fit_latent, what = "multiplicative")'), width = 8.5, height = 7.4)
.log("B7: R = 1")
cap("s7-r1", R(
  'fit_latent1 <- fit_selection(',
  F1,
  '  data = team, R = 1,',
  '  nscan = 10000, burn = 500, odens = 25, seed = 1',
  ')',
  'round(coef(fit_latent1), 2)'
))

# ===========================================================================
# B8 - several teams
# ===========================================================================
.log("B8: ten teams, pooled (several minutes)")
cap("s8-fit", R(
  'fit_teams <- fit_selection(',
  F1,
  '  data = demo_affect_teams,',
  '  nscan = 3000, burn = 500, odens = 10, seed = 1',
  ')',
  'summary(fit_teams)'
))
.log("B8: ten teams, kept separate (several minutes)")
cap("s8-separate", R(
  'fit_separate <- fit_selection(',
  F1,
  '  data = demo_affect_teams, pool_beta = FALSE,',
  '  nscan = 3000, burn = 500, odens = 10, seed = 1',
  ')',
  'round(coef(fit_separate), 2)'
))
capfig("s8-groups", R('plot(fit_separate, what = "groups")'), width = W, height = 7.0)
.log("B8: ten teams, hierarchical (several minutes)")
cap("s8-hier", R(
  'fit_hier <- fit_selection(',
  F1,
  '  data = demo_affect_teams,',
  '  pooling = selection_pooling(random = c("intercept", "dyd_affect_gap_lag1")),',
  '  nscan = 3000, burn = 500, odens = 10, chains = 2, seed = 1',
  ')',
  'fit_hier'
))
fact("s8-gap",     'sprintf("%.2f", coef(fit_teams)[["dyd_affect_gap_lag1"]])')
fact("s8-gap-min", 'sprintf("%.2f", min(coef(fit_separate)[, "dyd_affect_gap_lag1"]))')
fact("s8-gap-max", 'sprintf("%.2f", max(coef(fit_separate)[, "dyd_affect_gap_lag1"]))')

.log("done")
