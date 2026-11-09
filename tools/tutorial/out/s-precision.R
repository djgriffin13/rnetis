for (n in c(30, 120, 480)) {
  s <- simulate_influence(n_nodes = n, density = 4.5 / (n - 1),
                          gamma = 0.3, beta = 0.4,
                          normalize = "row", seed = 7)
  e <- netis_estimates(fit_influence(
    y ~ ar() + exposure(y, via = tie, normalize = "row"), data = s$data))
  cat(sprintf("%3d nodes:  exposure %5.2f, 95%% interval [%5.2f, %5.2f]\n",
              n, e$estimate[3], e$conf.low[3], e$conf.high[3]))
}
