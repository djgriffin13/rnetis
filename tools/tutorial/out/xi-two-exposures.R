fit_two <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +
    exposure(affect, via = advice, x_lag = 1, w_lag = 1, normalize = "row"),
  data = team
)
fit_two
