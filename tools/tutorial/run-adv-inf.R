# Captures for the advanced Influence walkthrough.
#   Rscript run-adv-inf.R <outdir>                               (~10 min)
#
# THE SAME DATA AND THE SAME MODEL AS PAGES A1 TO A8. The walkthrough goes back
# over each step of that sequence and shows the alternatives the main pages
# left out, so every block here varies ONE thing about the model the reader
# has already fitted. Nothing here builds a second dataset.
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")
EX <- 'exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row")'

cap("xi-setup", R(
  'library(rnetis)',
  '',
  'team  <- demo_affect',
  'teams <- demo_affect_teams',
  '',
  'fit <- fit_influence(',
  '  affect ~ ar() +',
  sprintf('    %s,', EX),
  '  data = team',
  ')',
  'round(coef(fit), 4)'
))

# ---- the data --------------------------------------------------------------
cap("xi-diagnose", R('diagnose_data(team)'))

# ---- exposure, by hand -----------------------------------------------------
cap("xi-compute", R(
  'ex <- compute_exposure(team, content = "affect", via = "confides",',
  '                       normalize = "row", wave = 1)',
  'head(ex)'
))
cap("xi-lm", R(
  'people <- netis_nodes(team)',
  'by_hand <- data.frame(',
  '  affect_now    = people$affect[people$wave == 2],',
  '  affect_before = people$affect[people$wave == 1],',
  '  exposure      = ex$ex_affect_confides[match(people$node[people$wave == 2], ex$node)]',
  ')',
  'round(coef(lm(affect_now ~ affect_before + exposure, data = by_hand)), 4)'
))

# ---- average or total ------------------------------------------------------
cap("xi-sum", R(
  'total <- compute_exposure(team, content = "affect", via = "confides", wave = 1)',
  'compare <- data.frame(node = ex$node, average = round(ex$ex_affect_confides, 2),',
  '                      total = total$ex_affect_confides)',
  'head(compare)'
))

# ---- people with nobody to average over -------------------------------------
cap("xi-isolate-na", R(
  '# Count only pairs who talk four or more hours a week as tied.',
  'close <- compute_exposure(team, content = "affect",',
  '                          via = binarize(talk, threshold(4)),',
  '                          normalize = "row", wave = 1)',
  'sum(is.na(close$ex_affect_bin_talk))'
))
cap("xi-isolate-fit", R(
  'fit_close <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = binarize(talk, threshold(4)), x_lag = 1, w_lag = 1,',
  '             normalize = "row"),',
  '  data = team',
  ')',
  'fit_close'
))
cap("xi-isolate-zero", R(
  'fit_zero <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = binarize(talk, threshold(4)), x_lag = 1, w_lag = 1,',
  '             normalize = "row", isolate_rule = "zero"),',
  '  data = team',
  ')',
  'round(rbind(dropped = coef(fit_close), as_zero = coef(fit_zero)), 3)'
))
cap("xi-isolate-error", R(
  'fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = binarize(talk, threshold(4)), x_lag = 1, w_lag = 1,',
  '             normalize = "row", isolate_rule = "error"),',
  '  data = team',
  ')'
), expect_error = TRUE)

# ---- weighted or binary ----------------------------------------------------
cap("xi-weighted", R(
  'one <- function(via_term) {',
  '  f <- as.formula(sprintf(',
  '    "affect ~ ar() + exposure(affect, via = %s, x_lag = 1, w_lag = 1, normalize = \\"row\\")",',
  '    via_term))',
  '  est <- netis_estimates(fit_influence(f, data = team))',
  '  round(unlist(est[3, c("estimate", "std.error", "conf.low", "conf.high")]), 2)',
  '}',
  '',
  'rbind(',
  '  "talk, as hours"            = one("talk"),',
  '  "talk, any at all"          = one("binarize(talk, nonzero())"),',
  '  "talk, 2 hours or more"     = one("binarize(talk, threshold(2))"),',
  '  "talk, above the team mean" = one("binarize(talk, above_mean())")',
  ')'
))

# ---- direction, and several networks ----------------------------------------
cap("xi-direction", R(
  'rbind(',
  '  "from those who confide in me"   = one("confides"),',
  '  "either direction (symmetrized)" = one("symmetrize(confides, \\"max\\")"),',
  '  "confides or advice (union)"     = one("multiplex(confides, advice, combine = \\"union\\")")',
  ')'
))
cap("xi-two-exposures", R(
  'fit_two <- fit_influence(',
  '  affect ~ ar() +',
  sprintf('    %s +', EX),
  '    exposure(affect, via = advice, x_lag = 1, w_lag = 1, normalize = "row"),',
  '  data = team',
  ')',
  'fit_two'
))

# ---- timing ----------------------------------------------------------------
cap("xi-wlag", R(
  'fit_wlag <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 1, w_lag = 0, normalize = "row"),',
  '  data = team',
  ')',
  'round(rbind("network at wave 1 (w_lag = 1)" = coef(fit),',
  '            "network at wave 2 (w_lag = 0)" = coef(fit_wlag)), 3)'
))
cap("xi-sar", R(
  'fit_sar <- fit_influence(',
  '  affect ~ ar() +',
  '    exposure(affect, via = confides, x_lag = 0, w_lag = 0, normalize = "row"),',
  '  data = team',
  ')',
  'fit_sar'
))

# ---- covariates and moderation ----------------------------------------------
cap("xi-lagged", R(
  'fit_cov <- fit_influence(',
  '  affect ~ ar() +',
  sprintf('    %s +', EX),
  '    lagged(extraversion) + tenure,',
  '  data = team',
  ')',
  'fit_cov'
))
cap("xi-moderation", R(
  'fit_mod <- fit_influence(',
  '  affect ~ ar() +',
  sprintf('    %s * remote,', EX),
  '  data = team',
  ')',
  'fit_mod'
))
cap("xi-moderation-effect", R('influence_effect(fit_mod)'))
cap("xi-centrality", R(
  'fit_cent <- fit_influence(',
  '  affect ~ ar() +',
  sprintf('    %s +', EX),
  '    centrality(confides, "degree", mode = "in"),',
  '  data = team',
  ')',
  'round(coef(fit_cent), 3)'
))

# ---- comparing models -------------------------------------------------------
cap("xi-compare", R(
  'fit_ar <- fit_influence(affect ~ ar(), data = team)',
  '',
  'data.frame(',
  '  model     = c("ar() only", "ar() + exposure()"),',
  '  rows      = c(netis_glance(fit_ar)$nobs, netis_glance(fit)$nobs),',
  '  r.squared = round(c(netis_glance(fit_ar)$r.squared, netis_glance(fit)$r.squared), 3),',
  '  AIC       = round(c(AIC(fit_ar), AIC(fit)), 1),',
  '  BIC       = round(c(BIC(fit_ar), BIC(fit)), 1)',
  ')'
))
cap("xi-effect-probs", R('influence_effect(fit, probs = c(0.25, 0.75))'))
cap("xi-estimates", R('netis_estimates(fit)'))

# ---- the latent space -------------------------------------------------------
.log("latent space: d = 1, 2, 3 (several minutes)")
cap("xi-latent-d", R(
  'latent_fit <- function(d, seed = 1) {',
  '  f <- as.formula(sprintf(paste(',
  '    "affect ~ ar() +",',
  '    "exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = \\"row\\") +",',
  '    "latent(confides, d = %d, seed = %d)"), d, seed))',
  '  est <- netis_estimates(fit_influence(f, data = team))',
  '  round(unlist(est[est$term == "ex_affect_confides",',
  '                   c("estimate", "std.error", "conf.low", "conf.high")]), 2)',
  '}',
  '',
  'rbind("no latent space" = one("confides"),',
  '      "d = 1" = latent_fit(1), "d = 2" = latent_fit(2), "d = 3" = latent_fit(3))'
))
.log("latent space: two more seeds at d = 2")
cap("xi-latent-seed", R(
  'rbind("seed 1" = latent_fit(2, seed = 1),',
  '      "seed 2" = latent_fit(2, seed = 2),',
  '      "seed 3" = latent_fit(2, seed = 3))'
))

# ---- several teams ----------------------------------------------------------
cap("xi-mg", R(
  'fit_teams <- fit_influence(',
  '  affect ~ ar() +',
  sprintf('    %s +', EX),
  '    (1 | group),',
  '  data = teams',
  ')',
  'netis_variance_components(fit_teams)[, c("grouping", "term", "variance", "sd")]'
))
cap("xi-icc", R('icc1(fit_teams)'))
cap("xi-icc2", R('icc2(fit_teams)$overall'))
cap("xi-se", R('influence_se_comparison(fit_teams, by = "group")'))
cap("xi-se-refuse", R('influence_se_comparison(fit)'), expect_error = TRUE)

# ---- planning a study -------------------------------------------------------
cap("xi-simulate", R(
  'for (n in c(40, 160, 640)) {',
  '  sim <- simulate_influence(n_nodes = n, density = 5 / (n - 1),',
  '                            gamma = 0.4, beta = 0.5, normalize = "row", seed = 7)',
  '  est <- netis_estimates(fit_influence(',
  '    y ~ ar() + exposure(y, via = tie, x_lag = 1, w_lag = 1, normalize = "row"),',
  '    data = sim$data))',
  '  cat(sprintf("%3d people: exposure %5.2f, 95%% interval [%5.2f, %5.2f]\\n",',
  '              n, est$estimate[3], est$conf.low[3], est$conf.high[3]))',
  '}'
))
.log("done")
