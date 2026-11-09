staff <- netis_data(
  node_data = nodes,
  tie_data  = ties,
  wave_col  = "wave",
  tie_meta  = list(
    tie       = list(zero_meaning = "measured_zero"),
    same_unit = list(zero_meaning = "measured_zero"))
)
staff
