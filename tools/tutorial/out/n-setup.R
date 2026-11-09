co_nodes <- data.frame(group = 1, node = rep(letters[1:6], 2),
                       wave = rep(1:2, each = 6),
                       y = c(2.1, 3.4, 1.8, 2.9, 3.1, 2.2,
                             2.6, 3.1, 2.4, 3.3, 3.0, 2.7))
co_ties <- data.frame(
  group  = 1,
  source = c("a", "b", "c", "d", "e", "a", "c", "f"),
  target = c("b", "a", "a", "c", "d", "c", "b", "e"),
  wave   = 1,
  contact = c(1, 7, 2, 9, 4, 3, 6, 1),
  advice  = c(1, 1, 0, 1, 0, 0, 1, 1)
)

co <- netis_data(co_nodes, co_ties, wave_col = "wave", tie_meta = list(
  contact = list(weighted = TRUE, zero_meaning = "measured_zero",
                 weight_semantics = "count"),
  advice  = list(weighted = FALSE, zero_meaning = "measured_zero")
))
co
