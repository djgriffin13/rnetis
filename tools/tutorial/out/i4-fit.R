fit <- fit_influence(affect ~ ar() + exposure(affect, via = confides, normalize = "row"),
                     data = team)
fit
