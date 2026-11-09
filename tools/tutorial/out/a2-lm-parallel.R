# rnetis builds a design matrix and hands it to stats::lm. Doing the same
# thing by hand gives the same numbers -- which is the point: `ar()` and
# `exposure()` are shorthand for columns, not a different estimator.
ex <- compute_exposure(demo_influence, content = "y", via = "tie",
                       normalize = "row", wave = 1)
nd <- netis_nodes(demo_influence)

panel <- data.frame(
  y_now  = nd$y[nd$wave == 2],
  y_prev = nd$y[nd$wave == 1],
  ex     = ex$ex_y_tie[match(nd$node[nd$wave == 2], ex$node)]
)

round(coef(lm(y_now ~ y_prev + ex, data = panel)), 6)
