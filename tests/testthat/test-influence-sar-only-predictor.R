test_that("a simultaneous term with no other predictor is refused, and says why", {
  err <- tryCatch(
    fit_influence(
      y ~ exposure(y, via = tie, x_lag = 0, normalize = "row", isolate_rule = "zero"),
      data = demo_influence
    ),
    error = conditionMessage
  )
  expect_match(err, "no usable predictor columns", fixed = TRUE)
  expect_match(err, "enters through its network", fixed = TRUE)
  expect_match(err, "ar() or a covariate", fixed = TRUE)
})
