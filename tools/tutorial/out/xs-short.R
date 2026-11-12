fit_short <- fit_selection(
  confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +
    prior_tie(lag = 1),
  data = team,
  nscan = 300, burn = 20, odens = 5, seed = 1
)
convergence <- function(f) {
  d <- plot_data(f, what = "mcmc")
  wide <- reshape(d[, c("term", "statistic", "value")], idvar = "term",
                  timevar = "statistic", direction = "wide")
  names(wide) <- c("term", "R-hat", "bulk ESS", "tail ESS")
  wide[, 2:4] <- round(wide[, 2:4], 2)
  wide
}
convergence(fit_short)
