sel <- fit_selection(tie ~ sender(tenure) + receiver(tenure) +
                       dyad(same_unit) + prior_tie(),
                     data = staff, nscan = 5000, burn = 1000,
                     odens = 10, seed = 1)
logLik(sel)
