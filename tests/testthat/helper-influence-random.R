# Random-effects Influence fixtures, shared by the variance-component guard
# (test-guard-variance-components.R) and the effect-size tests
# (test-influence-conditional-effects.R), so both files use the same fits.

# A random SLOPE is what both defects need; the random-intercept fit below is
# the positive control that the correction does not over-reach. The singular
# warning on this fixture is incidental to everything being asserted.
# Built by hand. Four groups so a group-level random effect has something to vary
# over, two waves so `ar()` has a prior wave to reach back to.
.vc_data <- function(seed = 4) {
  set.seed(seed)
  gs <- c("a", "b", "c", "d")
  nodes <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("%s%02d", g, 1:7)
    data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = 7),
               y = stats::rnorm(14), x = stats::rnorm(14),
               stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("%s%02d", g, 1:7)
    data.frame(group = g, source = ids[-7], target = ids[-1], wave = 1,
               tie = 1, stringsAsFactors = FALSE)
  }))
  netis_data(nodes, ties, wave_col = "wave")
}

.vc_slope_fit <- function() {
  suppressWarnings(fit_influence(
    y ~ ar() + exposure(x, via = tie, normalize = "row") + (1 + ex_x_tie | group),
    data = .vc_data()
  ))
}

.vc_intercept_fit <- function() {
  suppressWarnings(fit_influence(
    y ~ ar() + exposure(x, via = tie, normalize = "row") + (1 | group),
    data = .vc_data()
  ))
}
