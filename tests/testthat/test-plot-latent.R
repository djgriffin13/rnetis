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
  expect_equal(built$data[[2]]$x, d$x)
  expect_equal(built$data[[2]]$y, d$y)
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

# ---- a Selection fit's latent coordinates -----------------------------------

test_that("a Selection fit's coordinates reproduce the product they came from", {
  skip_if_not_installed("amen")
  fit <- .cached_fixture("plot_selection_latent_r1", fit_selection(
    tie ~ sender(tenure) + receiver(tenure) + prior_tie(), data = demo_selection,
    nscan = 200, burn = 50, odens = 10, seed = 1, R = 1
  ))
  d <- plot_data(fit, what = "latent")
  uv <- fit$per_group[[1]]$multiplicative
  expect_identical(nrow(d), nrow(uv))
  expect_true(all(d$dims == 1L))
  # One dimension: sending coordinate times receiving coordinate is the rank-one
  # approximation of the posterior mean product.
  approx <- outer(d$x, d$y)
  sv <- svd(uv)
  expect_equal(approx, sv$d[1] * tcrossprod(sv$u[, 1], sv$v[, 1]),
               tolerance = 1e-8, ignore_attr = TRUE)
  expect_s3_class(plot(fit, what = "latent"), "ggplot")
})

test_that("two dimensions place every actor twice, in one frame", {
  skip_if_not_installed("amen")
  fit <- .cached_fixture("plot_selection_latent_r2", fit_selection(
    tie ~ sender(tenure) + receiver(tenure) + prior_tie(), data = demo_selection,
    nscan = 200, burn = 50, odens = 10, seed = 1, R = 2
  ))
  d <- plot_data(fit, what = "latent")
  uv <- fit$per_group[[1]]$multiplicative
  expect_identical(nrow(d), 2L * nrow(uv))
  expect_setequal(d$role, c("as sender", "as receiver"))
  snd <- as.matrix(d[d$role == "as sender", c("x", "y")])
  rcv <- as.matrix(d[d$role == "as receiver", c("x", "y")])
  sv <- svd(uv)
  rank2 <- sv$u[, 1:2] %*% diag(sv$d[1:2]) %*% t(sv$v[, 1:2])
  expect_equal(snd %*% t(rcv), rank2, tolerance = 1e-8, ignore_attr = TRUE)
  # The sign rule: the largest sending coordinate on each dimension is positive.
  expect_gt(snd[which.max(abs(snd[, 1])), 1], 0)
  expect_gt(snd[which.max(abs(snd[, 2])), 2], 0)
  expect_match(paste(attr(d, "netis_caption"), collapse = " "),
               "only the product is identified", fixed = TRUE)
  p <- plot(fit, what = "latent")
  expect_s3_class(p, "ggplot")
  expect_identical(p$coordinates$ratio, 1)
  # One panel: the two roles are overlaid, not faceted.
  expect_s3_class(p$facet, "FacetNull")
  # One segment per observed tie, from a sending point to a receiving point.
  segs <- attr(d, "netis_edges")
  y_obs <- fit$designs[[1]]$Y
  expect_identical(nrow(segs), sum(y_obs != 0, na.rm = TRUE))
  at <- which(!is.na(y_obs) & y_obs != 0, arr.ind = TRUE)[1, ]
  expect_equal(unlist(segs[1, ], use.names = FALSE),
               c(snd[at[[1]], ], rcv[at[[2]], ]), ignore_attr = TRUE)
})

test_that("an Influence fit's positions carry the ties they were fitted to", {
  fit <- .pl_fit(2L)
  d <- plot_data(fit, what = "latent")
  segs <- attr(d, "netis_edges")
  edges <- fit$resolved_latent[[1]]$edges
  expect_gt(nrow(edges), 0L)
  expect_identical(nrow(segs), nrow(edges))
  first <- edges[1, ]
  expect_equal(c(segs$x[1], segs$y[1]),
               unlist(d[d$node == first$source, c("x", "y")], use.names = FALSE))
  expect_equal(c(segs$xend[1], segs$yend[1]),
               unlist(d[d$node == first$target, c("x", "y")], use.names = FALSE))
  expect_match(paste(attr(d, "netis_caption"), collapse = " "),
               "observed ties", fixed = TRUE)
})

test_that("a fit with R = 0 is refused, and told what would give coordinates", {
  skip_if_not_installed("amen")
  fit <- .cached_fixture("plot_selection_latent_r0", fit_selection(
    tie ~ sender(tenure) + prior_tie(), data = demo_selection,
    nscan = 100, burn = 20, odens = 10, seed = 1
  ))
  expect_error(plot(fit, what = "latent"), "fit with R = 0", fixed = TRUE)
  expect_error(plot_data(fit, what = "latent"), "R = 2", fixed = TRUE)
})
