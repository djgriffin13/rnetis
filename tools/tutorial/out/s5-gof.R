gof <- netis_gof(fit)
gof[, c("statistic", "observed", "predicted_lower", "predicted_upper", "ppp")]
