pooled <- fit_selection(tie ~ sender(tenure) + prior_tie(), data = two_groups,
                        nscan = 5000, burn = 1000, odens = 10, seed = 1)
pooled
