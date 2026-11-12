fit_slope <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +
    (1 + ex_affect_confides | group),
  data = demo_affect_teams
)
fit_slope
