ame2 <- fit_selection(
  tie ~ sender(tenure) + receiver(tenure) + prior_tie(),
  data = staff, R = 2,
  nscan = 5000, burn = 1000, odens = 10, seed = 1
)
netis_estimates(ame2)
