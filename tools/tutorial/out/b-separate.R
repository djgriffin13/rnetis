separate <- fit_selection(tie ~ sender(tenure) + prior_tie(), data = two_groups,
                          pool_beta = FALSE,
                          nscan = 5000, burn = 1000, odens = 10, seed = 1)
coef(separate)
