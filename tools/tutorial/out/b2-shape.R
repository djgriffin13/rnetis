# 20 nodes make 380 ordered pairs, and both waves were observed, so the
# tie table has a row for every pair at every wave -- zeros included.
nrow(netis_nodes(demo_selection))
nrow(netis_ties(demo_selection))
table(netis_ties(demo_selection)$wave)
table(netis_ties(demo_selection)$tie)
