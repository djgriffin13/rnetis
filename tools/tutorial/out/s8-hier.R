fit_hier <- fit_selection(
  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +
    prior_tie(lag = 1),
  data = demo_affect_teams,
  pooling = selection_pooling(random = c("intercept", "dyd_affect_gap_lag1")),
  nscan = 3000, burn = 500, odens = 10, chains = 2, seed = 1
)
fit_hier
