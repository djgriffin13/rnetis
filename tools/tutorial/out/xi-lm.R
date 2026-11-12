people <- netis_nodes(team)
by_hand <- data.frame(
  affect_now    = people$affect[people$wave == 2],
  affect_before = people$affect[people$wave == 1],
  exposure      = ex$ex_affect_confides[match(people$node[people$wave == 2], ex$node)]
)
round(coef(lm(affect_now ~ affect_before + exposure, data = by_hand)), 4)
