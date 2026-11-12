# Suppose the office had been recorded: add it to the tie table as a
# characteristic of the pair.
office_ties$same_office <- as.numeric(rep(same, 2))
offices_known <- netis_data(office_nodes, office_ties, wave_col = "wave",
  tie_meta = list(confides    = list(zero_meaning = "measured_zero"),
                  same_office = list(zero_meaning = "measured_zero")))

fit_known <- fit_selection(
  confides ~ dyad(same_office, lag = 1) + prior_tie(lag = 1),
  data = offices_known,
  nscan = 6000, burn = 1000, odens = 10, seed = 1
)
netis_gof(fit_known)[, c("statistic", "observed", "predicted_lower", "predicted_upper", "ppp")]
