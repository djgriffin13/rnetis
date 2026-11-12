library(rnetis)

team  <- demo_affect
teams <- demo_affect_teams

fit <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row"),
  data = team
)
round(coef(fit), 4)
