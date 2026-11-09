fit3 <- fit_influence(
  y ~ ar() + exposure(y, via = tie, normalize = "row") +
    centrality(tie, "degree", mode = "in"),
  data = demo_influence
)
netis_estimates(fit3)
