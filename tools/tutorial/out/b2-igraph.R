# Say it, and the round trip works. This is the same declaration the
# fits on the following pages rely on.
staff <- netis_data(
  node_data = nodes,
  tie_data  = ties,
  wave_col  = "wave",
  tie_meta  = list(
    tie = list(zero_meaning  = "measured_zero",
               presence_mode = "value_rule",
               presence_rule = function(value) value != 0),
    same_unit = list(zero_meaning = "measured_zero"))
)

g <- as_igraph(staff, "tie", wave = 2)
g

netis_data_from_igraph(g)
