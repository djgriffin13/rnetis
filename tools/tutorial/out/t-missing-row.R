# And now the third case: the pair is not in the table at all. Under
# "row_presence" that is simply not a tie. Under "complete_dyadic" the
# claim is that it WAS observed, so an absent pair is a measured zero.
# Neither of those is "we do not know", which is what a genuinely
# unobserved pair is -- and that is why the declaration is yours.
thin <- full[full$r == 1, ]
thin

compute_centrality(
  netis_data(who, thin,
    tie_meta = list(r = list(presence_mode = "row_presence"))),
  "r", "degree", mode = "in")
