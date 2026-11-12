fit_recip <- fit_selection(
  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +
    prior_tie(lag = 1) + prior_reciprocity(lag = 1),
  data = team,
  nscan = 10000, burn = 500, odens = 25, seed = 1
)
round(coef(fit_recip), 2)
