ex <- compute_exposure(demo_influence, content = "y", via = "tie",
                       normalize = "row", wave = 1)
head(ex)
summary(ex$ex_y_tie)
