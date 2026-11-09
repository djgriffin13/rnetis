per_group <- fit_influence(
  y ~ ar() + exposure(x, via = tie) + latent(tie, d = 1, homophily = "x") +
    (1 | group),
  data = demo_influence_groups
)
netis_estimates(per_group)
