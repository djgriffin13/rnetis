fit_slope <- fit_influence(
  affect ~ ar() + exposure(affect, via = confides, normalize = "row") +
    (1 + ex_affect_confides | group),
  data = demo_affect_teams
)
fit_slope
