# The clustering feature, at each R: what was observed, and the range the
# fitted model simulates.
check <- function(R) {
  f <- fit_selection(confides ~ prior_tie(lag = 1), data = offices, R = R,
                     nscan = 6000, burn = 1000, odens = 10, seed = 1)
  g <- netis_gof(f)
  g <- g[g$statistic == "trans.dep", ]
  round(c(observed = g$observed, simulated_low = g$predicted_lower,
          simulated_high = g$predicted_upper, ppp = g$ppp), 3)
}
rbind("R = 0" = check(0), "R = 1" = check(1), "R = 2" = check(2), "R = 3" = check(3))
