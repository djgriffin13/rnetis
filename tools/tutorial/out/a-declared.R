declared <- fit_influence(
  y ~ ar() + exposure(y, via = tie, normalize = "row"),
  data = demo_influence,
  network_exogeneity = "exogenous",
  exposure_orthogonal_to_actor_effect = TRUE
)
netis_assumptions(declared)$network$exogeneity
all.equal(coef(fit), coef(declared))
