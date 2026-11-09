# Three occasions of the same relation. A fit models one transition, and
# will not pick which.
three <- netis_data(
  nodes,
  rbind(ties, transform(ties[ties$wave == 2, ], wave = 3)),
  wave_col = "wave",
  tie_meta = list(tie = list(zero_meaning = "measured_zero"),
                  same_unit = list(zero_meaning = "measured_zero")))

fit_selection(tie ~ sender(tenure) + prior_tie(), data = three,
              nscan = 500, burn = 100, odens = 10, seed = 1)
