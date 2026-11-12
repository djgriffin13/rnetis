fit_teams <- fit_selection(
  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +
    prior_tie(lag = 1),
  data = demo_affect_teams,
  nscan = 3000, burn = 500, odens = 10, seed = 1
)
summary(fit_teams)
