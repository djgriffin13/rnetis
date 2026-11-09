three <- simulate_influence(n_nodes = 20, beta = 0.3, normalize = "row",
                            n_waves = 3, seed = 4)
fit_influence(y ~ ar() + exposure(y, via = tie, normalize = "row"),
              data = three$data)
