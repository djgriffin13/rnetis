# "zero" says an isolate belongs at zero on the scale. Here wellbeing
# runs from 10 to 60, so zero is not a neutral value -- it is off the
# bottom of the observed range, and Fay would be the most extreme case
# in the dataset purely because nobody advises her.
compute_exposure(team, content = "wellbeing", via = "strength",
                 normalize = "row", isolate_rule = "zero", wave = 1)
