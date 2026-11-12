# Count only pairs who talk four or more hours a week as tied.
close <- compute_exposure(team, content = "affect",
                          via = binarize(talk, threshold(4)),
                          normalize = "row", wave = 1)
sum(is.na(close$ex_affect_bin_talk))
