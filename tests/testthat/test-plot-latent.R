# The latent-positions panel of an Influence fit.
#
# The positions are what a latent() term entered into the model, so the panel
# is checked against the model's own estimating table: the figure draws the
# numbers the coefficients were estimated from, and nothing else.

.pl_fit <- function(d = 2L) {
  skip_if_not_installed("latentnet")
  skip_if_not_installed("network")
  .cached_fixture(paste0("plot_latent_d", d), {
    if (d == 2L) {
      fit_influence(
        y ~ ar() + exposure(x, via = tie, normalize = "row") + latent(tie, d = 2, seed = 1),
        data = demo_influence
      )
    } else {
      fit_influence(
        y ~ ar() + exposure(x, via = tie, normalize = "row") + latent(tie, d = 1, seed = 1),
        data = demo_influence
      )
    }
  })
}

test_that("the panel draws each actor at the position the model used", {
  fit <- .pl_fit(2L)
  d <- plot_data(fit, what = "latent")
  mf <- fit$model_frame
  expect_identical(d$node, as.character(mf[[2]]))
  expect_identical(d$x, mf[["lat_tie_1_d1"]])
  expect_identical(d$y, mf[["lat_tie_1_d2"]])
  expect_identical(d$outcome, mf[[".outcome"]])

  p <- plot(fit, what = "latent")
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$data[[1]]$x, d$x)
  expect_equal(built$data[[1]]$y, d$y)
  # A map of distances: both axes in the same units.
  expect_equal(p$coordinates$ratio, 1)
})

test_that("the caption says the axes mean nothing and the distances do", {
  d <- plot_data(.pl_fit(2L), what = "latent")
  caption <- paste(attr(d, "netis_caption"), collapse = " ")
  expect_match(caption, "Read the distances, not the axes", fixed = TRUE)
  expect_match(caption, "rotation, reflection and shift", fixed = TRUE)
  expect_match(caption, "their uncertainty is not drawn", fixed = TRUE)
})

test_that("one dimension is drawn against the outcome", {
  fit <- .pl_fit(1L)
  d <- plot_data(fit, what = "latent")
  expect_identical(d$x, fit$model_frame[["lat_tie_1"]])
  expect_identical(d$y, fit$model_frame[[".outcome"]])
  p <- plot(fit, what = "latent")
  expect_null(p$coordinates$ratio)
  expect_identical(p$labels$y, "outcome")
})

test_that("a fit with no latent() term is refused, and told what would give one", {
  fit <- fit_influence(y ~ ar() + exposure(x, via = tie, normalize = "row"),
                       data = demo_influence)
  expect_error(plot(fit, what = "latent"), "has no latent() term", fixed = TRUE)
  expect_error(plot_data(fit, what = "latent"), "latent(<network>, d = 2)", fixed = TRUE)
})
