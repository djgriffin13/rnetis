ties <- netis_ties(team)

# A pair is "mutual" when confiding runs in both directions.
reverse <- match(paste(ties$wave, ties$target, ties$source),
                 paste(ties$wave, ties$source, ties$target))
ties$mutual <- as.numeric(ties$confides == 1 & ties$confides[reverse] == 1)

# An undirected tie table lists each pair once.
once <- ties[ties$source < ties$target,
             c("group", "wave", "source", "target", "mutual", "same_project", "affect_gap")]

pairs_data <- netis_data(
  netis_nodes(team), once, wave_col = "wave", directed = FALSE,
  tie_meta = list(
    mutual       = list(zero_meaning = "measured_zero"),
    same_project = list(zero_meaning = "measured_zero"),
    affect_gap   = list(zero_meaning = "measured_zero", weighted = TRUE)
  )
)
pairs_data
