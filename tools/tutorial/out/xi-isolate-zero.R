fit_zero <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = binarize(talk, threshold(4)), x_lag = 1, w_lag = 1,
             normalize = "row", isolate_rule = "zero"),
  data = team
)
round(rbind(dropped = coef(fit_close), as_zero = coef(fit_zero)), 3)
