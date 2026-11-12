total <- compute_exposure(team, content = "affect", via = "confides", wave = 1)
compare <- data.frame(node = ex$node, average = round(ex$ex_affect_confides, 2),
                      total = total$ex_affect_confides)
head(compare)
