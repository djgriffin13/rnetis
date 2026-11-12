fit_advice <- fit_selection(
  confides ~ dyad(advice, lag = 1) + dyad(affect_gap, lag = 1) +
    dyad(same_project, lag = 1) + prior_tie(lag = 1),
  data = team,
  nscan = 3000, burn = 500, odens = 10, seed = 1
)
fit_advice
