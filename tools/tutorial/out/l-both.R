# Both declarations at once, and the model reads as if the network had
# always been this shape.
un_ties$met <- as.numeric(un_ties$contact >= 3)
plain <- netis_data(co_nodes, un_ties, wave_col = "wave",
  directed = FALSE,
  tie_meta = list(met = list(weighted = FALSE,
                             zero_meaning = "measured_zero")))

summary(fit_influence(y ~ ar() + exposure(y, via = met), data = plain))
