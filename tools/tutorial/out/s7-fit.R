fit_latent <- fit_selection(
  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +
    prior_tie(lag = 1),
  data = team, R = 2,
  nscan = 10000, burn = 500, odens = 25, seed = 1
)
fit_latent
