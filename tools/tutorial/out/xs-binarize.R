fit_talk2 <- fit_selection(
  talk ~ dyad(same_project, lag = 1) + dyad(confides, lag = 1) +
    sender(remote, lag = 1) + receiver(remote, lag = 1),
  data = team, binarize = threshold(2), waves = c(1, 2),
  nscan = 3000, burn = 500, odens = 10, seed = 1
)
round(coef(fit_talk2), 2)
