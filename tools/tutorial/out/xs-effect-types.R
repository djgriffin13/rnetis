effect <- function(type) {
  e <- selection_effect(fit, "dyd_same_project_lag1", type = type)
  round(100 * c(estimate = e$estimate, lower = e$interval[[1]], upper = e$interval[[2]]), 1)
}
rbind(conditional = effect("conditional"),
      population  = effect("population"),
      reference   = effect("reference"))
