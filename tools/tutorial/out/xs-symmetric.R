fit_mutual <- fit_selection(
  mutual ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +
    prior_tie(lag = 1),
  data = pairs_data, symmetric = TRUE,
  nscan = 3000, burn = 500, odens = 10, seed = 1
)
fit_mutual
