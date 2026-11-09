# A pair attribute is a TIE variable: one value per ordered pair, not a
# property of either person on their own. Here, whether the two are in
# the same unit of the organisation.
nodes <- netis_nodes(demo_selection)
ties  <- netis_ties(demo_selection)

unit <- rep(c("u1", "u2"), length.out = nrow(nodes))
names(unit) <- nodes$node
ties$same_unit <- as.numeric(unit[ties$source] == unit[ties$target])

head(ties[, c("source", "target", "wave", "tie", "same_unit")], 4)
