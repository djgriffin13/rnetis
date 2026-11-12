fit_cov <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +
    lagged(extraversion) + tenure,
  data = team
)
fit_cov
