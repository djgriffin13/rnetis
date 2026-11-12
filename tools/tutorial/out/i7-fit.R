fit_latent <- fit_influence(
  affect ~ ar() + exposure(affect, via = confides, normalize = "row") +
    latent(confides, d = 2, seed = 1),
  data = team
)
summary(fit_latent)
