fit_influence(y ~ ar() + exposure(y, via = tie, normalize = "row"),
              data = three$data, waves = c(2, 3))
