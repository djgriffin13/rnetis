fit2 <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +
    extraversion + remote,
  data = team
)
fit2
