fit_separate <- fit_selection(
  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +
    prior_tie(lag = 1),
  data = demo_affect_teams, pool_beta = FALSE,
  nscan = 3000, burn = 500, odens = 10, seed = 1
)
round(coef(fit_separate), 2)
