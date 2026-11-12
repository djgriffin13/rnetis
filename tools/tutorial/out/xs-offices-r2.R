fit_r2 <- fit_selection(confides ~ prior_tie(lag = 1), data = offices, R = 2,
                        nscan = 6000, burn = 1000, odens = 10, seed = 1)
