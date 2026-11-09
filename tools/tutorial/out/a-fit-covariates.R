fit2 <- fit_influence(
  y ~ ar() + exposure(y, via = tie, normalize = "row") + x + lagged(x),
  data = demo_influence
)
netis_estimates(fit2)
