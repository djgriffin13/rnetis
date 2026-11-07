# Shapes the help pages document, on the routes they apply to.

.irs_mixed <- function() {
  suppressWarnings(fit_influence(
    y ~ ar() + exposure(x, via = tie) + (1 | group), data = demo_influence_groups
  ))
}

test_that("confint() forwards to lme4 with a term named as coef() names it", {
  fit <- .irs_mixed()
  ci <- confint(fit, parm = "ex_x_tie", method = "Wald")
  expect_identical(rownames(ci), "ex_x_tie")
  # lme4's Wald interval is the package's default interval.
  expect_equal(unname(ci[1, ]), unname(confint(fit, parm = "ex_x_tie")[1, ]))
})

test_that("confint() forwards a term with a backticked backend name", {
  nodes <- netis_nodes(demo_influence_groups)
  nodes$z <- as.numeric(nodes$x > 0)
  nd <- netis_data(nodes, netis_ties(demo_influence_groups), wave_col = "wave")
  fit <- suppressWarnings(fit_influence(
    y ~ ar() + exposure(x, via = tie) * z + (1 | group), data = nd
  ))
  term <- grep(":", names(coef(fit)), value = TRUE, fixed = TRUE)
  expect_identical(rownames(confint(fit, parm = term, method = "Wald")), term)
})

test_that("pseudo_r2() names the scale of every measure, on both Influence routes", {
  documented <- c("measure", "scale", "estimate", "lo", "hi")
  expect_named(pseudo_r2(.irs_mixed())$measures, documented)
  ols <- fit_influence(y ~ ar() + exposure(x, via = tie), data = demo_influence)
  m <- pseudo_r2(ols)$measures
  expect_named(m, documented)
  expect_identical(unique(m$scale), "observed")
  expect_equal(m$estimate[1], netis_glance(ols)$r.squared)
})
