# An igraph graph has edges, so exporting one means deciding which rows
# of a complete-dyadic table are edges. The declaration we made says a
# zero is a measured absence; it does not yet say a zero is not an edge.
as_igraph(staff, "tie", wave = 2)
