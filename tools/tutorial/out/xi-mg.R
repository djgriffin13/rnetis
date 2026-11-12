fit_teams <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +
    (1 | group),
  data = teams
)
netis_variance_components(fit_teams)[, c("grouping", "term", "variance", "sd")]
