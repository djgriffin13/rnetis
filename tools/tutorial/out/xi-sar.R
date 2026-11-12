fit_sar <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 0, w_lag = 0, normalize = "row"),
  data = team
)
fit_sar
