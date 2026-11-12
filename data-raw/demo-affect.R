# Generates the two affect-contagion example datasets the tutorial is built on.
#
#   demo_affect        one team of 40, two waves
#   demo_affect_teams  ten teams of 20, two waves
#
# ONE DATASET FOR BOTH MODEL FAMILIES. An Influence model asks whether a
# person's affect moves toward the affect of the people who confide in them; a
# Selection model asks who comes to confide in whom. Both questions are about
# the same team, so both are answered from the same object, and a reader who
# has built it once does not build a second one.
#
# THE FIRST BLOCK IS THE TUTORIAL'S DATA PAGE, LINE FOR LINE. The page builds
# the one-team data from scratch with data.frame(), rnorm() and rbinom() and
# then checks that what it built is identical to `demo_affect`. That check is
# only true while this block and the page's code are the same code; the
# tutorial's capture script fails if they drift.
#
# BOTH PROCESSES ARE PLANTED, with known sizes, and recorded as `truth`:
#   - contagion: affect at wave 2 is 0.4 of the person's own wave-1 affect plus
#     0.5 of the mean wave-1 affect of the people who confide in them;
#   - selection: a confiding tie at wave 2 is likelier where one existed, where
#     the two share a project, and where their wave-1 affect was close.
#
# Regenerate with:
#   Rscript data-raw/demo-affect.R
#
# RUN IT THAT WAY, at top level. `is_tie` below is stored inside the objects,
# and a function defined at top level serializes as a reference to the global
# environment; sourced into a private environment it would drag that whole
# environment into the .rda with it.

library(rnetis)

# ---------------------------------------------------------------------------
# demo_affect -- the Data page's code
# ---------------------------------------------------------------------------
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

# A tie is present where its value is not zero; a zero is a measured non-tie.
is_tie <- function(value) value != 0
team <- netis_data(nodes, ties, wave_col = "wave", tie_meta = list(
  confides     = list(zero_meaning = "measured_zero",
                      presence_mode = "value_rule", presence_rule = is_tie),
  advice       = list(zero_meaning = "measured_zero",
                      presence_mode = "value_rule", presence_rule = is_tie),
  talk         = list(zero_meaning = "measured_zero", weighted = TRUE,
                      weight_semantics = "count",
                      presence_mode = "value_rule", presence_rule = is_tie),
  same_project = list(zero_meaning = "measured_zero"),
  affect_gap   = list(zero_meaning = "measured_zero", weighted = TRUE)
))
# ---------------------------------------------------------------------------
# end of the Data page's code
# ---------------------------------------------------------------------------

# Every person has at least one confider, so the row-normalized exposure the
# tutorial's first model uses is defined for all forty. Asserted, because the
# seed was chosen for it and a changed generator would silently break page one.
stopifnot(!anyNA(confiders_affect))

.affect_truth <- list(
  influence = list(ar = 0.4, exposure = 0.5, residual_sd = 0.3,
                   exposure_term = 'exposure(affect, via = confides, normalize = "row")'),
  selection = list(scale = "logit", intercept = -1.9, prior_tie = 2.4,
                   same_project = 0.8, affect_gap = -1.1)
)
demo_affect <- team
attr(demo_affect, "truth") <- .affect_truth

# ---------------------------------------------------------------------------
# demo_affect_teams -- ten teams, the same processes, a team-level shift
#
# The same generator run once per team, with two differences: a team-level
# offset on affect (so there is between-team variance for a random intercept
# to estimate), and a redraw of any team in which someone has no confider (so
# the row-normalized exposure is defined for every person in every team).
# ---------------------------------------------------------------------------
.one_team <- function(team_id, n, offset) {
  repeat {
    id <- sprintf("%s_p%02d", team_id, seq_len(n))
    people <- data.frame(
      node         = id,
      extraversion = round(rnorm(n), 2),
      tenure       = round(runif(n, 0, 10), 1),
      remote       = rbinom(n, 1, 0.3),
      project      = sample(c("north", "south", "east"), n, replace = TRUE)
    )
    people$affect <- round(3 + offset + 0.4 * people$extraversion + rnorm(n, sd = 0.6), 2)
    pairs <- expand.grid(source = id, target = id, stringsAsFactors = FALSE)
    pairs <- pairs[pairs$source != pairs$target, ]
    s <- match(pairs$source, id)
    t <- match(pairs$target, id)
    pairs$same_project <- as.numeric(people$project[s] == people$project[t])
    pairs$affect_gap   <- round(abs(people$affect[s] - people$affect[t]), 2)
    # A smaller team needs a denser network for everyone to have a confider.
    pairs$confides <- rbinom(nrow(pairs), 1,
                             plogis(-1.4 + 1.0 * pairs$same_project - 0.5 * pairs$affect_gap))
    pairs$advice   <- rbinom(nrow(pairs), 1,
                             plogis(-2.6 + 0.15 * people$tenure[t] + 0.8 * pairs$same_project))
    pairs$talk     <- rpois(nrow(pairs),
                            exp(-0.3 + 1.1 * pairs$confides + 0.5 * pairs$same_project))
    confiders_affect <- tapply(people$affect[s][pairs$confides == 1],
                               pairs$target[pairs$confides == 1], mean)[id]
    if (!anyNA(confiders_affect)) break
  }
  affect2 <- round(0.3 + 0.6 * offset + 0.4 * people$affect + 0.5 * confiders_affect +
                     rnorm(n, sd = 0.3), 2)
  confides2 <- rbinom(nrow(pairs), 1,
                      plogis(-1.9 + 2.4 * pairs$confides + 0.8 * pairs$same_project
                             - 1.1 * pairs$affect_gap))
  list(
    nodes = rbind(
      data.frame(group = team_id, wave = 1, people),
      data.frame(group = team_id, wave = 2, transform(people, affect = affect2))
    ),
    ties = rbind(
      data.frame(group = team_id, wave = 1, pairs),
      data.frame(group = team_id, wave = 2, transform(pairs, confides = confides2))
    )
  )
}

set.seed(20261112)
team_ids <- sprintf("team%02d", 1:10)
offsets <- round(rnorm(length(team_ids), sd = 0.4), 2)
built <- Map(.one_team, team_ids, 20L, offsets)

demo_affect_teams <- netis_data(
  do.call(rbind, lapply(built, `[[`, "nodes")),
  do.call(rbind, lapply(built, `[[`, "ties")),
  wave_col = "wave", tie_meta = list(
    confides     = list(zero_meaning = "measured_zero",
                        presence_mode = "value_rule", presence_rule = is_tie),
    advice       = list(zero_meaning = "measured_zero",
                        presence_mode = "value_rule", presence_rule = is_tie),
    talk         = list(zero_meaning = "measured_zero", weighted = TRUE,
                        weight_semantics = "count",
                        presence_mode = "value_rule", presence_rule = is_tie),
    same_project = list(zero_meaning = "measured_zero"),
    affect_gap   = list(zero_meaning = "measured_zero", weighted = TRUE)
  )
)
attr(demo_affect_teams, "truth") <- c(
  .affect_truth, list(team_offset_sd = 0.4, team_offsets = stats::setNames(offsets, team_ids))
)

usethis::use_data(demo_affect, demo_affect_teams, overwrite = TRUE, compress = "xz")
