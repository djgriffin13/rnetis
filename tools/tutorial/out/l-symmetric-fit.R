# No symmetrize() and no rule = : the relation is undirected, so there
# are no two directions left to choose between.
fit_influence(y ~ ar() + exposure(y, via = contact), data = undirected)
