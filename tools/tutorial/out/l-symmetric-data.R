# An undirected measurement: each pair recorded once, and the object told
# so. "6 nodes, 8 ties, undirected" is the object agreeing.
pair  <- paste(pmin(co_ties$source, co_ties$target),
               pmax(co_ties$source, co_ties$target))
un_ties <- co_ties[!duplicated(pair), ]

undirected <- netis_data(co_nodes, un_ties, wave_col = "wave",
  directed = FALSE,
  tie_meta = list(contact = list(weighted = TRUE,
                                 weight_semantics = "count",
                                 zero_meaning = "measured_zero")))
undirected
