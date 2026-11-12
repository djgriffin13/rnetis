fit_sum <- fit_influence(affect ~ ar() + exposure(affect, via = confides),
                         data = team)
coef(fit_sum)
