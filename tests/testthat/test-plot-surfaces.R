# The plot surfaces: what each one draws, and what it refuses to draw.
#
# The composition, layer and semantic tests cover the helpers underneath
# plot.netis_influence()/plot.netis_data(). These blocks cover the entry points
# themselves, and especially
# the two places where the plot layer must not paper over a decision made
# elsewhere: a network drawing needs one snapshot, and residual dependence
# moved off the fitted object at the estimation/interpretation seam.

.ps_data <- function(n = 10, seed = 1) {
  set.seed(seed)
  ids <- letters[seq_len(n)]
  nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = n),
                      y = stats::rnorm(2 * n), x = stats::rnorm(2 * n),
                      stringsAsFactors = FALSE)
  ties <- data.frame(group = 1, source = ids[-n], target = ids[-1], wave = 1,
                     tie = 1, stringsAsFactors = FALSE)
  netis_data(nodes, ties, wave_col = "wave")
}

.ps_fit <- function(nd = .ps_data()) {
  fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
}

# ---- a network drawing is a snapshot, so it needs a wave --------------------

test_that("drawing a multi-wave network without naming a wave is refused", {
  # A network picture shows ONE state. Drawing several waves superimposed
  # would produce a graph that exists at no point in time, and picking one
  # silently would be rnetis choosing which. The refusal names both ways out.
  nd <- .ps_data()
  err <- tryCatch(plot(nd), error = function(e) conditionMessage(e))
  expect_match(err, "must name one", fixed = TRUE)
  expect_match(err, "single snapshot", fixed = TRUE)
  expect_match(err, "netis_schema(x)$waves", fixed = TRUE)
})

test_that("naming the wave draws it", {
  nd <- .ps_data()
  expect_s3_class(plot(nd, wave = 1), "ggplot")
  # And the table behind it is available without the drawing.
  expect_type(plot_data(nd, wave = 1), "list")
})

# ---- a fit draws its estimates ----------------------------------------------

test_that("an Influence fit plots, and the numbers behind it are reachable", {
  fit <- .ps_fit()
  expect_s3_class(plot(fit), "ggplot")

  d <- plot_data(fit)
  expect_s3_class(d, "data.frame")
  # THE COLUMN CONTRACT. Downstream code and the vignettes both read these by
  # name, and `missing_state` is the one that carries whether an estimate has
  # an interval at all -- an absent interval must be visible as absent rather
  # than drawn as zero.
  expect_true(all(c("term", "value", "lower", "upper", "missing_state") %in% names(d)))
  expect_true(all(names(coef(fit)) %in% d$term))
})

test_that("a diagnostics collection plots, and an empty one says why it does not", {
  fit <- .ps_fit()
  expect_gt(length(fit$diagnostics), 0L)
  p <- plot(fit$diagnostics)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  d <- plot_data(fit$diagnostics)
  expect_identical(sum(d$n), length(fit$diagnostics))
  expect_error(plot(.combine_diagnostics(list())), "collection is empty")
})

# ---- dependence moved, and the plot layer says where ------------------------

test_that("asking a fit for its dependence panel says where the record lives", {
  # THE SEAM, one layer over. fit_influence() returns estimates; residual
  # dependence is computed on request because it rebuilds the conduit matrix
  # from the data. A panel that just reported "no dependence diagnostics"
  # would read as a gap in the fit rather than as a deliberate boundary.
  fit <- .ps_fit()
  err <- tryCatch(plot(fit, what = "dependence"), error = function(e) conditionMessage(e))
  expect_match(err, "computed on request", fixed = TRUE)
  expect_match(err, "netis_dependence(fit, data)", fixed = TRUE)
})

test_that("the dependence record draws directly", {
  nd <- .ps_data()
  fit <- .ps_fit(nd)
  dep <- netis_dependence(fit, nd)
  expect_s3_class(plot(dep), "ggplot")

  d <- plot_data(dep)
  expect_s3_class(d, "data.frame")
  expect_true(all(c("term", "status", "value", "reason") %in% names(d)))
})

test_that("a component with nothing to report becomes a row saying so", {
  # THE BUG THIS PINS. A component that was not computed can carry
  # `character(0)` where a reason would be -- `not_applicable` with nothing
  # further to say is the ORDINARY case under a two-wave contract, since each
  # unit contributes one outcome row and serial correlation cannot exist.
  # `%||%` does not catch a zero-length value, only NULL, so data.frame()
  # failed with "differing number of rows" on the commonest input there is.
  nd <- .ps_data()
  dep <- netis_dependence(.ps_fit(nd), nd)
  d <- plot_data(dep)

  expect_gt(nrow(d), 0L)
  # Every row is one row: no field silently collapsed or recycled.
  expect_false(anyNA(d$term))
  expect_false(anyNA(d$status))
  # Serial correlation is present as a ROW with a status, not missing from the
  # table -- unmeasured and absent are different claims.
  expect_true(any(grepl("serial", d$term)))
})
