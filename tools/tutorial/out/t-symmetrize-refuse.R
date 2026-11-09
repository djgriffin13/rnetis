# Collapsing the two directions into one is several different operations
# on a weighted network, so rnetis will not pick one.
compute_exposure(team, content = "wellbeing",
                 via = symmetrize("strength"), wave = 1)
