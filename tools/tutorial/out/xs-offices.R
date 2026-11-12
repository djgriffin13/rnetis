# Twenty-four people in three offices of eight. People confide mostly
# within their own office, and nothing in the data records the office.
set.seed(11)
id     <- sprintf("q%02d", 1:24)
office <- rep(1:3, each = 8)
pairs  <- expand.grid(source = id, target = id, stringsAsFactors = FALSE)
pairs  <- pairs[pairs$source != pairs$target, ]
same   <- office[match(pairs$source, id)] == office[match(pairs$target, id)]

office_ties <- rbind(
  data.frame(group = "firm", wave = 1, pairs,
             confides = rbinom(nrow(pairs), 1, ifelse(same, 0.7, 0.05))),
  data.frame(group = "firm", wave = 2, pairs,
             confides = rbinom(nrow(pairs), 1, ifelse(same, 0.7, 0.05)))
)
office_nodes <- data.frame(group = "firm", node = rep(id, 2), wave = rep(1:2, each = 24))

offices <- netis_data(office_nodes, office_ties, wave_col = "wave",
  tie_meta = list(confides = list(zero_meaning = "measured_zero")))
