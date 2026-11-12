fit_rev <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row",
             direction = "target_influences_source"),
  data = team
)
coef(fit_rev)
