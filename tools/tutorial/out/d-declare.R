nd3 <- netis_data(nodes2, ties2, wave_col = "wave", tie_meta = list(
  w = list(weighted = TRUE, zero_meaning = "measured_zero",
           weight_semantics = "count")
))
netis_boundary(nd3)
