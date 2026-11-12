# Pairs: one row per ordered pair of different people.
pairs <- expand.grid(source = id, target = id, stringsAsFactors = FALSE)
pairs <- pairs[pairs$source != pairs$target, ]
s <- match(pairs$source, id)
t <- match(pairs$target, id)
pairs$same_project <- as.numeric(people$project[s] == people$project[t])
pairs$affect_gap   <- round(abs(people$affect[s] - people$affect[t]), 2)
pairs$confides <- rbinom(nrow(pairs), 1,
                         plogis(-2.0 + 1.0 * pairs$same_project - 0.5 * pairs$affect_gap))
pairs$advice   <- rbinom(nrow(pairs), 1,
                         plogis(-2.6 + 0.15 * people$tenure[t] + 0.8 * pairs$same_project))
pairs$talk     <- rpois(nrow(pairs),
                        exp(-0.3 + 1.1 * pairs$confides + 0.5 * pairs$same_project))

head(pairs)
