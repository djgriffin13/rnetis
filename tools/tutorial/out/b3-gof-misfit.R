# Three dense blocks, so the network is full of closed triads. The same
# additive model as before -- sender and receiver effects, no latent
# dimensions -- fitted to it.
netis_gof(cl_fit)[, c("statistic", "observed", "predicted_lower",
                      "predicted_upper", "ppp", "z")]
