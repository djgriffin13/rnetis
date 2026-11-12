fit_teams <- fit_influence(
  affect ~ ar() + exposure(affect, via = confides, normalize = "row") + (1 | group),
  data = demo_affect_teams
)
summary(fit_teams)
