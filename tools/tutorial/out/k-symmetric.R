# Same data, fitted as what it is. One additive effect per actor, not a
# sender effect and a receiver effect.
sym <- fit_selection(tie ~ sender(tenure) + prior_tie(),
                     data = undirected, symmetric = TRUE,
                     nscan = 3000, burn = 1000, odens = 10, seed = 1)
netis_variance_components(sym)[, c("term", "estimate", "residual_variance_share")]
