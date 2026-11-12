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
team
