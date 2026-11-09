rbind(unadjusted = netis_estimates(fit_influence(
        y ~ ar() + exposure(y, via = tie, normalize = "row"),
        data = demo_influence))[3, c("estimate", "conf.low", "conf.high")],
      adjusted   = netis_estimates(lat)[3, c("estimate", "conf.low", "conf.high")])
