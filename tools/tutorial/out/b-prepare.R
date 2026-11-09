nodes <- netis_nodes(demo_selection)
ties  <- netis_ties(demo_selection)

unit <- rep(c("u1", "u2"), length.out = nrow(nodes))
names(unit) <- nodes$node
ties$same_unit <- as.numeric(unit[ties$source] == unit[ties$target])

staff <- netis_data(nodes, ties, wave_col = "wave", tie_meta = list(
  tie       = list(zero_meaning = "measured_zero"),
  same_unit = list(zero_meaning = "measured_zero")
))
staff
