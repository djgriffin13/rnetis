# No binarize() anywhere: the network is already what the model needs.
fit_influence(y ~ ar() + exposure(y, via = met), data = binary)
