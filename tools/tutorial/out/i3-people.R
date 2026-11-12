set.seed(5)
n  <- 40
id <- sprintf("p%02d", 1:n)

# People: one row per person.
people <- data.frame(
  node         = id,
  extraversion = round(rnorm(n), 2),
  tenure       = round(runif(n, 0, 10), 1),
  remote       = rbinom(n, 1, 0.3),
  project      = sample(c("north", "south", "east"), n, replace = TRUE)
)
people$affect <- round(3 + 0.4 * people$extraversion + rnorm(n, sd = 0.6), 2)

head(people)
