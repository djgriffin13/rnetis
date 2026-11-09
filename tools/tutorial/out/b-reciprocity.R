recip <- fit_selection(
  tie ~ sender(tenure) + prior_tie() + prior_reciprocity(),
  data = staff,
  nscan = 5000, burn = 1000, odens = 10, seed = 1
)
netis_estimates(recip)
