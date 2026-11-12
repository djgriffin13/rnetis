fit_close <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = binarize(talk, threshold(4)), x_lag = 1, w_lag = 1,
             normalize = "row"),
  data = team
)
fit_close
