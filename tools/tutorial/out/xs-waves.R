fit_level <- fit_selection(
  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1),
  data = team, waves = c(1, 2),
  nscan = 3000, burn = 500, odens = 10, seed = 1
)
round(rbind("with prior_tie()" = coef(fit)[1:3], "without" = coef(fit_level)), 2)
