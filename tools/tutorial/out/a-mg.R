mg <- fit_influence(
  y ~ ar() + exposure(x, via = tie) + (1 | group),
  data = demo_influence_groups
)
mg
