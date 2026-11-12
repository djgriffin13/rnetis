fit_r0 <- fit_selection(confides ~ prior_tie(lag = 1), data = offices,
                        nscan = 6000, burn = 1000, odens = 10, seed = 1)
netis_gof(fit_r0)[, c("statistic", "observed", "predicted_lower", "predicted_upper", "ppp")]
