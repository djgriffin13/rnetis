rbind(basic  = coef(fit)[c("affect_lag1", "ex_affect_confides")],
      latent = coef(fit_latent)[c("affect_lag1", "ex_affect_confides")])
