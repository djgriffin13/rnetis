fit_selection(talk ~ dyad(same_project, lag = 1), data = team,
              family = "nrm", binarize = threshold(2), waves = c(1, 2))
