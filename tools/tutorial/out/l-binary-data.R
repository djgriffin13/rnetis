# The conversion done once, in the tie table. `met` is a binary variable
# in its own right, declared as one, and every later call reads it
# without a wrapper.
co_ties$met <- as.numeric(co_ties$contact >= 3)

binary <- netis_data(co_nodes, co_ties, wave_col = "wave",
  tie_meta = list(met = list(weighted = FALSE,
                             zero_meaning = "measured_zero")))
binary
