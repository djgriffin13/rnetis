fit <- fit_influence(
  y ~ ar() + exposure(y, via = tie, normalize = "row"),
  data = demo_influence
)
fit
