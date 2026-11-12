fit_talk <- fit_influence(affect ~ ar() + exposure(affect, via = talk, normalize = "row"),
                          data = team)
coef(fit_talk)
