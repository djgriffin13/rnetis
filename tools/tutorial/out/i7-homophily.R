# How much of extraversion do the two position columns carry?
overlap <- function(fit) {
  X <- model.matrix(fit$backend_fit)
  positions <- X[, grep("^lat_", colnames(X))]
  summary(lm(X[, "extraversion"] ~ positions))$r.squared
}

fit_plain <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +
    extraversion + latent(confides, d = 2, seed = 1),
  data = team
)
fit_homophily <- fit_influence(
  affect ~ ar() +
    exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = "row") +
    extraversion + latent(confides, d = 2, homophily = "extraversion", seed = 1),
  data = team
)

round(c(without = overlap(fit_plain), with = overlap(fit_homophily)), 2)
