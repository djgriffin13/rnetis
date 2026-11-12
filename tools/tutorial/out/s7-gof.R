both <- merge(netis_gof(fit), netis_gof(fit_latent), by = "statistic",
              suffixes = c(".additive", ".latent"))
both[, c("statistic", "observed.additive", "ppp.additive", "ppp.latent")]
