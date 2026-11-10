# Five statistics of the observed network, each beside the range the
# fitted model actually generates. Sender and receiver effects are what
# explain unequal sending and unequal popularity; nothing additive
# explains closure.
netis_gof(sel)[, c("statistic", "observed", "predicted_lower",
                   "predicted_upper", "ppp", "z")]
