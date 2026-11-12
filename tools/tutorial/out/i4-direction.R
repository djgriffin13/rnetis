fit_rev <- fit_influence(
  affect ~ ar() + exposure(affect, via = confides, normalize = "row",
                           direction = "target_influences_source"),
  data = team
)
coef(fit_rev)
