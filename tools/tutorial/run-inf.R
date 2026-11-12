# Captures for the Influence sequence (pages A1 to A8).
#   Rscript run-inf.R <outdir>                                   (~3 min)
#
# ONE DATASET, BUILT ONCE. Page A3 builds the team data from scratch and checks
# it against the shipped `demo_affect`; every later page uses that object. The
# build code here is the first block of data-raw/demo-affect.R, line for line,
# and `i3-identical` is the capture that fails if the two drift.
#
# FIGURES ARE RENDERED HERE AT PUBLICATION WIDTH, so this script does not have
# to be followed by run-e.R the way run-a.R and run-b.R do.
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")
W <- 11.5

# ===========================================================================
# A1 - run a model first
# ===========================================================================
cap("i1-run", R(
  'library(rnetis)',
  '',
  'fit <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row"),',
  '  data = demo_affect',
  ')',
  'summary(fit)'
))

# ===========================================================================
# A3 - the data
# ===========================================================================
cap("i3-people", R(
  'set.seed(5)',
  'n  <- 40',
  'id <- sprintf("p%02d", 1:n)',
  '',
  '# People: one row per person.',
  'people <- data.frame(',
  '  node         = id,',
  '  extraversion = round(rnorm(n), 2),',
  '  tenure       = round(runif(n, 0, 10), 1),',
  '  remote       = rbinom(n, 1, 0.3),',
  '  project      = sample(c("north", "south", "east"), n, replace = TRUE)',
  ')',
  'people$affect <- round(3 + 0.4 * people$extraversion + rnorm(n, sd = 0.6), 2)',
  '',
  'head(people)'
))

cap("i3-pairs", R(
  '# Pairs: one row per ordered pair of different people.',
  'pairs <- expand.grid(source = id, target = id, stringsAsFactors = FALSE)',
  'pairs <- pairs[pairs$source != pairs$target, ]',
  's <- match(pairs$source, id)',
  't <- match(pairs$target, id)',
  'pairs$same_project <- as.numeric(people$project[s] == people$project[t])',
  'pairs$affect_gap   <- round(abs(people$affect[s] - people$affect[t]), 2)',
  'pairs$confides <- rbinom(nrow(pairs), 1,',
  '                         plogis(-2.0 + 1.0 * pairs$same_project - 0.5 * pairs$affect_gap))',
  'pairs$advice   <- rbinom(nrow(pairs), 1,',
  '                         plogis(-2.6 + 0.15 * people$tenure[t] + 0.8 * pairs$same_project))',
  'pairs$talk     <- rpois(nrow(pairs),',
  '                        exp(-0.3 + 1.1 * pairs$confides + 0.5 * pairs$same_project))',
  '',
  'head(pairs)'
))

cap("i3-wave2", R(
  '# Wave 2 affect: part your own earlier affect, part your confiders\' earlier affect.',
  'confiders_affect <- tapply(people$affect[s][pairs$confides == 1],',
  '                           pairs$target[pairs$confides == 1], mean)[id]',
  'affect2 <- round(0.3 + 0.4 * people$affect + 0.5 * confiders_affect + rnorm(n, sd = 0.3), 2)',
  '',
  '# Wave 2 confiding: ties persist, and form between people whose affect was close.',
  'confides2 <- rbinom(nrow(pairs), 1,',
  '                    plogis(-1.9 + 2.4 * pairs$confides + 0.8 * pairs$same_project',
  '                           - 1.1 * pairs$affect_gap))',
  '',
  'nodes <- rbind(',
  '  data.frame(group = "team", wave = 1, people),',
  '  data.frame(group = "team", wave = 2, transform(people, affect = affect2))',
  ')',
  'ties <- rbind(',
  '  data.frame(group = "team", wave = 1, pairs),',
  '  data.frame(group = "team", wave = 2, transform(pairs, confides = confides2))',
  ')',
  '',
  'dim(nodes)',
  'dim(ties)'
))

cap("i3-object", R(
  '# A tie is present where its value is not zero; a zero is a measured non-tie.',
  'is_tie <- function(value) value != 0',
  'team <- netis_data(nodes, ties, wave_col = "wave", tie_meta = list(',
  '  confides     = list(zero_meaning = "measured_zero",',
  '                      presence_mode = "value_rule", presence_rule = is_tie),',
  '  advice       = list(zero_meaning = "measured_zero",',
  '                      presence_mode = "value_rule", presence_rule = is_tie),',
  '  talk         = list(zero_meaning = "measured_zero", weighted = TRUE,',
  '                      weight_semantics = "count",',
  '                      presence_mode = "value_rule", presence_rule = is_tie),',
  '  same_project = list(zero_meaning = "measured_zero"),',
  '  affect_gap   = list(zero_meaning = "measured_zero", weighted = TRUE)',
  '))',
  'team'
))

cap("i3-identical", R(
  'identical(netis_nodes(team), netis_nodes(demo_affect))',
  'identical(netis_ties(team),  netis_ties(demo_affect))'
))
stopifnot(identical(netis_nodes(ENV$team), netis_nodes(demo_affect)),
          identical(netis_ties(ENV$team), netis_ties(demo_affect)))

# ===========================================================================
# A4 - running the model
# ===========================================================================
cap("i4-fit", R(
  'fit <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row"),',
  '  data = team',
  ')',
  'fit'
))

cap("i4-covariates", R(
  'fit2 <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +',
  '    extraversion + remote,',
  '  data = team',
  ')',
  'fit2'
))

cap("i4-sum", R(
  'fit_sum <- fit_influence(',
  '  affect ~ ar() + exposure(affect, via = confides, x_lag = 1, w_lag = 1),',
  '  data = team',
  ')',
  'coef(fit_sum)'
))

cap("i4-direction", R(
  'fit_rev <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row",',
  '             direction = "target_influences_source"),',
  '  data = team',
  ')',
  'coef(fit_rev)'
))

cap("i4-other-tie", R(
  'fit_talk <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = talk, x_lag = 1, w_lag = 1, normalize = "row"),',
  '  data = team',
  ')',
  'coef(fit_talk)'
))

# ===========================================================================
# A5 - interpreting the model
# ===========================================================================
cap("i5-summary", R('summary(fit)'))
cap("i5-assumptions", R('netis_assumptions(fit)'))
cap("i5-effect", R('influence_effect(fit)'))
cap("i5-dependence", R('netis_dependence(fit, team)'))

fact("i-ar",   'sprintf("%.2f", coef(fit)[["affect_lag1"]])')
fact("i-ex",   'sprintf("%.2f", coef(fit)[["ex_affect_confides"]])')
fact("i-ex-lo", 'sprintf("%.2f", confint(fit)["ex_affect_confides", 1])')
fact("i-ex-hi", 'sprintf("%.2f", confint(fit)["ex_affect_confides", 2])')
fact("i-r2",   'sprintf("%.2f", fit$r_squared)')
fact("i-df",   'format(fit$residual_df)')
fact("i-ex-p", 'sprintf("%.3f", summary(fit$backend_fit)$coefficients["ex_affect_confides", 4])')

# ===========================================================================
# A6 - visualizing
# ===========================================================================
capfig("i6-network", R('plot(team, variable = "confides", wave = 1, layout = "mds")'),
       width = W, height = 8.4)
capfig("i6-coef",    R('plot(fit)'), width = W, height = 4.6)

# ===========================================================================
# A7 - the latent space
# ===========================================================================
.log("latent-space fit (about a minute and a half)")
cap("i7-fit", R(
  'fit_latent <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +',
  '    latent(confides, d = 2, seed = 1),',
  '  data = team',
  ')',
  'summary(fit_latent)'
))
cap("i7-compare", R(
  'rbind(basic  = coef(fit)[c("affect_lag1", "ex_affect_confides")],',
  '      latent = coef(fit_latent)[c("affect_lag1", "ex_affect_confides")])'
))
capfig("i7-latent", R('plot(fit_latent, what = "latent")'), width = 8.5, height = 7.0)
cap("i7-d1", R(
  'fit_latent1 <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +',
  '    latent(confides, d = 1, seed = 1),',
  '  data = team',
  ')',
  'coef(fit_latent1)'
))
.log("latent-space fits for the homophily box (several minutes)")
cap("i7-homophily", R(
  '# How much of extraversion do the two position columns carry?',
  'overlap <- function(fit) {',
  '  X <- model.matrix(fit$backend_fit)',
  '  positions <- X[, grep("^lat_", colnames(X))]',
  '  summary(lm(X[, "extraversion"] ~ positions))$r.squared',
  '}',
  '',
  'fit_plain <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +',
  '    extraversion + latent(confides, d = 2, seed = 1),',
  '  data = team',
  ')',
  'fit_homophily <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +',
  '    extraversion + latent(confides, d = 2, homophily = "extraversion", seed = 1),',
  '  data = team',
  ')',
  '',
  'round(c(without = overlap(fit_plain), with = overlap(fit_homophily)), 2)'
))
fact("i7-ov-without", 'sprintf("%.2f", overlap(fit_plain))')
fact("i7-ov-with",    'sprintf("%.2f", overlap(fit_homophily))')
fact("i7-ex", 'sprintf("%.2f", coef(fit_latent)[["ex_affect_confides"]])')

# ===========================================================================
# A8 - multilevel
# ===========================================================================
cap("i8-data", R('demo_affect_teams'))
cap("i8-refuse", R(
  'fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row"),',
  '  data = demo_affect_teams',
  ')'
), expect_error = TRUE)
cap("i8-fit", R(
  'fit_teams <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +',
  '    (1 | group),',
  '  data = demo_affect_teams',
  ')',
  'summary(fit_teams)'
))
capfig("i8-heterogeneity", R('plot(fit_teams, what = "heterogeneity")'), width = W, height = 4.6)
cap("i8-slope", R(
  'fit_slope <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +',
  '    (1 + ex_affect_confides | group),',
  '  data = demo_affect_teams',
  ')',
  'fit_slope'
))
fact("i8-ex", 'sprintf("%.2f", coef(fit_teams)[["ex_affect_confides"]])')
fact("i8-sd-team", 'sprintf("%.2f", fit_teams$var_corr$sdcor[fit_teams$var_corr$grp == "group"])')
fact("i8-sd-res",  'sprintf("%.2f", fit_teams$var_corr$sdcor[fit_teams$var_corr$grp == "Residual"])')

.log("done")
