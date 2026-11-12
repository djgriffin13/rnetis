fit_cent <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +
    centrality(confides, "degree", mode = "in"),
  data = team
)
round(coef(fit_cent), 3)
