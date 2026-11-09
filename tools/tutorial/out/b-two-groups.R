two_groups <- netis_data(
  node_data = rbind(transform(netis_nodes(demo_selection), group = "a"),
                    transform(netis_nodes(demo_selection), group = "b")),
  tie_data  = rbind(transform(netis_ties(demo_selection), group = "a"),
                    transform(netis_ties(demo_selection), group = "b")),
  group_data = data.frame(group = c("a", "b"), size = c(8, 30)),
  wave_col = "wave",
  tie_meta = list(tie = list(zero_meaning = "measured_zero"))
)
two_groups
