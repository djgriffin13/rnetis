# family = "nrm" models the counts themselves. The coefficients are on
# the outcome's own scale -- contacts -- not on a latent probit scale.
val <- fit_selection(contacts ~ sender(tenure) + receiver(tenure),
                     waves = c(1, 2),
                     data = valued, family = "nrm",
                     nscan = 3000, burn = 1000, odens = 10, seed = 1)
val
