strength_ties <- transform(co_ties, closeness = contact)
st <- netis_data(co_nodes, strength_ties, wave_col = "wave", tie_meta = list(
  closeness = list(weighted = TRUE, weight_semantics = "strength",
                   zero_meaning = "measured_zero")
))
compute_centrality(st, network = "closeness", measure = "betweenness",
                   weights = "weighted", wave = 1)
