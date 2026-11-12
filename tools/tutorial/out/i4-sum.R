fit_sum <- fit_influence(
  affect ~ ar() + exposure(affect, via = confides, x_lag = 1, w_lag = 1),
  data = team
)
coef(fit_sum)
