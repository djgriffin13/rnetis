fit_talk <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = talk, x_lag = 1, w_lag = 1, normalize = "row"),
  data = team
)
coef(fit_talk)
