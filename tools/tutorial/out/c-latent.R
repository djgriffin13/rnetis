lat <- fit_influence(
  y ~ ar() + exposure(y, via = tie, normalize = "row") +
    latent(tie, d = 1, homophily = c("y", "x"),
           control = list(sample.size = 5000, burnin = 20000, interval = 10)),
  data = demo_influence
)
summary(lat)
