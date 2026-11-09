hier <- fit_selection(
  tie ~ sender(extraversion) + prior_tie(), data = teams_data,
  pooling = selection_pooling(random = c("intercept", "snd_extraversion")),
  nscan = 10000, burn = 1000, odens = 10, chains = 2, seed = 1
)
hier
