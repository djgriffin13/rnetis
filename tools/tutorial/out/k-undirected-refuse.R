# The relation is declared undirected. Fitting it as a directed model
# would read one tie as two observations.
fit_selection(tie ~ sender(tenure) + prior_tie(), data = undirected,
              nscan = 500, burn = 100, odens = 10, seed = 1)
