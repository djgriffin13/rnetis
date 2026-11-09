conv <- netis_hierarchy(hier)$convergence

# the rows you read first: the shared coefficient, the population means,
# and the between-group standard deviations
subset(conv, !family %in% c("group_coefficient", "nuisance"),
       select = c(family, parameter, rhat, ess_bulk, ess_tail))
