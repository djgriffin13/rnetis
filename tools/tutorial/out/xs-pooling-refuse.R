fit_selection(confides ~ prior_tie(lag = 1), data = teams, pool_beta = FALSE,
              pooling = selection_pooling(random = "intercept"))
