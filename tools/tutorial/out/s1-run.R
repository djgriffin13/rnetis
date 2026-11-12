library(rnetis)

fit <- fit_selection(
  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +
    prior_tie(lag = 1),
  data = demo_affect, seed = 1
)
summary(fit)
