# The same six rows under the two readings. "row_presence" means a row
# IS a tie, so the four zeros are counted as ties and the network looks
# complete. "value_rule" says a zero row is an observed non-tie.
rows <- netis_data(who, full,
  tie_meta = list(r = list(presence_mode = "row_presence")))
vals <- netis_data(who, full,
  tie_meta = list(r = list(presence_mode = "value_rule",
                           presence_rule = function(value) value != 0)))

compute_network_stat(rows, "r", "density")
compute_network_stat(vals, "r", "density")
