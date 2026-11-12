fit2 <- fit_selection(
  confides ~ sender(extraversion, lag = 1) + receiver(extraversion, lag = 1) +
    dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +
    prior_tie(lag = 1),
  data = team,
  nscan = 10000, burn = 500, odens = 25, seed = 1
)
fit2
