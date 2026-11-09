# Naming the transition is how you proceed -- but here waves 2 and 3 were
# built as copies of each other, and a lagged relation identical to the
# response is not a predictor.
fit_selection(tie ~ sender(tenure) + prior_tie(), data = three,
              waves = c(2, 3), nscan = 500, burn = 100, odens = 10, seed = 1)
