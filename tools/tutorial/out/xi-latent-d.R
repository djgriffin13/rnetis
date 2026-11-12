latent_fit <- function(d, seed = 1) {
  f <- as.formula(sprintf(paste(
    "affect ~ ar() +",
    "exposure(affect, via = confides, x_lag = 1, w_lag = 1, normalize = \"row\") +",
    "latent(confides, d = %d, seed = %d)"), d, seed))
  est <- netis_estimates(fit_influence(f, data = team))
  round(unlist(est[est$term == "ex_affect_confides",
                   c("estimate", "std.error", "conf.low", "conf.high")]), 2)
}

rbind("no latent space" = one("confides"),
      "d = 1" = latent_fit(1), "d = 2" = latent_fit(2), "d = 3" = latent_fit(3))
