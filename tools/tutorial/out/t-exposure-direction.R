# The default reads a stored tie as "source influences target". Reversing
# it asks a different question of the same data: not what reaches Ada
# through the people who advise her, but what reaches her through the
# people she advises.
compute_exposure(team, content = "wellbeing", via = "strength",
                 direction = "target_influences_source", wave = 1)
