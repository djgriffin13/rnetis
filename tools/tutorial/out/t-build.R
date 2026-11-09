# Two data frames in, one object out. `wave_col` names the occasion
# column; `tie_meta` is where you say what a tie value means.
team <- netis_data(
  node_data = people,
  tie_data  = advice,
  wave_col  = "wave",
  tie_meta  = list(strength = list(
    weighted         = TRUE,
    weight_semantics = "strength",
    zero_meaning     = "measured_zero"))
)
team
