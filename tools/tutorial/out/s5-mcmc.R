conv <- plot_data(fit, what = "mcmc")
reshape(conv[, c("term", "statistic", "value")], idvar = "term",
        timevar = "statistic", direction = "wide")
