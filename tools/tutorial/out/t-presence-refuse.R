# Saying every pair was observed is not enough on its own: rnetis still
# needs to know which of those rows counts as a tie. The object builds,
# and the question is put to you the moment something needs the answer.
cd <- netis_data(who, full,
                 tie_meta = list(r = list(presence_mode = "complete_dyadic")))
compute_network_stat(cd, "r", "density")
