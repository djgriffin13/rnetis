library(rnetis)

fit <- fit_influence(affect ~ ar() + exposure(affect, via = confides, normalize = "row"),
                     data = demo_affect)
summary(fit)
