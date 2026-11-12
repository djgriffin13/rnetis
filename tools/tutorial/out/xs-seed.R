fit_seed2 <- fit_selection(
  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +
    prior_tie(lag = 1),
  data = team,
  nscan = 10000, burn = 500, odens = 25, seed = 2
)
round(rbind("seed 1" = coef(fit), "seed 2" = coef(fit_seed2)), 3)
