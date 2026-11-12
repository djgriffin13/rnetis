fit_latent1 <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +
    latent(confides, d = 1, seed = 1),
  data = team
)
coef(fit_latent1)
