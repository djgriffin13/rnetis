# Of the nine ties, how many are matched by one going the other way?
compute_network_stat(team, "strength", "reciprocity", wave = 1)

# Ada -> Ben and Ben -> Ada are both present, so that dyad is mutual.
# Cyd -> Ada is not matched by Ada -> Cyd, so that one is asymmetric.
A <- netis_adjacency(team, "strength", wave = 1)
A["Ada", "Ben"]; A["Ben", "Ada"]
A["Cyd", "Ada"]; A["Ada", "Cyd"]
