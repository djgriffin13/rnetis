own <- h$groups[h$groups$coefficient == "dyd_affect_gap_lag1",
                c("group", "beta_g", "lower", "upper", "shrinkage")]
head(own)
