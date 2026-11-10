# binarize = is for turning a valued outcome INTO a tie. It has no
# meaning when the values themselves are being modelled.
fit_selection(contacts ~ sender(tenure), data = valued, family = "nrm",
              binarize = nonzero(), nscan = 500, burn = 100, seed = 1)
