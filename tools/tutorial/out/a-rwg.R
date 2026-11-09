set.seed(1)
ratings <- matrix(stats::rnorm(60, 4, 0.4), ncol = 3)
teams   <- rep(c("a", "b"), each = 10)

rwg(ratings[, 1], teams, null = list(dist = "uniform", n_options = 5))
