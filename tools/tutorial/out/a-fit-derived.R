fit_influence(
  y ~ ar() + exposure(y, via = symmetrize(tie, "max"), normalize = "row"),
  data = demo_influence
)
