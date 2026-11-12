fit_latent1 <- fit_selection(
  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +
    prior_tie(lag = 1),
  data = team, R = 1,
  nscan = 10000, burn = 500, odens = 25, seed = 1
)
round(coef(fit_latent1), 2)
