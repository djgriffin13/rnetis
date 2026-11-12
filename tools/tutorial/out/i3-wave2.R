# Wave 2 affect: part your own earlier affect, part your confiders' earlier affect.
confiders_affect <- tapply(people$affect[s][pairs$confides == 1],
                           pairs$target[pairs$confides == 1], mean)[id]
affect2 <- round(0.3 + 0.4 * people$affect + 0.5 * confiders_affect + rnorm(n, sd = 0.3), 2)

# Wave 2 confiding: ties persist, and form between people whose affect was close.
confides2 <- rbinom(nrow(pairs), 1,
                    plogis(-1.9 + 2.4 * pairs$confides + 0.8 * pairs$same_project
                           - 1.1 * pairs$affect_gap))

nodes <- rbind(
  data.frame(group = "team", wave = 1, people),
  data.frame(group = "team", wave = 2, transform(people, affect = affect2))
)
ties <- rbind(
  data.frame(group = "team", wave = 1, pairs),
  data.frame(group = "team", wave = 2, transform(pairs, confides = confides2))
)

dim(nodes)
dim(ties)
