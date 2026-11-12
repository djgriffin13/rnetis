one <- function(via_term) {
  f <- as.formula(sprintf(
    "affect ~ ar() + exposure(affect, via = %s, x_lag = 1, w_lag = 1, normalize = \"row\")",
    via_term))
  est <- netis_estimates(fit_influence(f, data = team))
  round(unlist(est[3, c("estimate", "std.error", "conf.low", "conf.high")]), 2)
}

rbind(
  "talk, as hours"            = one("talk"),
  "talk, any at all"          = one("binarize(talk, nonzero())"),
  "talk, 2 hours or more"     = one("binarize(talk, threshold(2))"),
  "talk, above the team mean" = one("binarize(talk, above_mean())")
)
