for (n in c(40, 160, 640)) {
  sim <- simulate_influence(n_nodes = n, density = 5 / (n - 1),
                            gamma = 0.4, beta = 0.5, normalize = "row", seed = 7)
  est <- netis_estimates(fit_influence(
    y ~ ar() + exposure(y, via = tie, x_lag = 1, w_lag = 1, normalize = "row"),
    data = sim$data))
  cat(sprintf("%3d people: exposure %5.2f, 95%% interval [%5.2f, %5.2f]\n",
              n, est$estimate[3], est$conf.low[3], est$conf.high[3]))
}
