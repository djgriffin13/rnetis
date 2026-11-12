fit_wlag <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 0, normalize = "row"),
  data = team
)
round(rbind("network at wave 1 (w_lag = 1)" = coef(fit),
            "network at wave 2 (w_lag = 0)" = coef(fit_wlag)), 3)
