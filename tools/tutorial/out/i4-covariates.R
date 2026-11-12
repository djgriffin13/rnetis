fit2 <- fit_influence(
  affect ~ ar() + exposure(affect, via = confides, normalize = "row") +
    extraversion + remote,
  data = team
)
fit2
