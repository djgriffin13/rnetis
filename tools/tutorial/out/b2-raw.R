# demo_selection is two plain data frames, like any other netis_data.
# The node table is static -- tenure does not change -- so it has no
# wave column at all.
head(netis_nodes(demo_selection), 3)
head(netis_ties(demo_selection), 4)
