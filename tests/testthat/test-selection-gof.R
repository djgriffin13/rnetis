# netis_gof(): the posterior predictive check on a Selection fit.
#
# SHORT CHAINS, deliberately, with ONE exception. Most assertions here are
# structural (a column set, a row count, a refusal) or internal-consistency
# identities (z against observed, predicted_mean and predicted_sd), and
# neither kind moves with chain length.
#
# The exception is "an additive model misses closure", which is the assertion
# the whole function exists for, and it needs a chain long enough that the
# predictive range is a range rather than a handful of points. It gets one,
# and it is still the cheapest fit that can carry the claim: the separation it
# asserts is roughly five predictive standard deviations, so it does not sit
# anywhere near a tolerance.

# ---- fixtures ---------------------------------------------------------------

#' A network with real closure, or one without, from otherwise identical code
#'
#' ASYMMETRIC UNDER THE FAILURE MODE, which is the only reason this fixture
#' exists rather than reusing the shared one. `clustered = TRUE` puts three
#' dense blocks in the network, so transitive triads are everywhere; with
#' `FALSE` the same actors, the same waves and the same covariate get ties at a
#' constant probability and no closure at all. A `netis_gof()` that reported
#' the same thing for both -- by comparing the observed value against itself,
#' say, or by summarizing the wrong matrix row -- would pass on either network
#' alone and fail here.
#'
#' Blocks at p = 0.85 rather than 1. A perfectly blocked network makes
#' amen::ame() singular (the additive effects have nothing to vary over), so
#' the fixture would fail for a reason that has nothing to do with the thing
#' under test.
#' @noRd
.gof_fixture_data <- function(clustered, n_per_block = 5, n_blocks = 3,
                              seed = 11L) {
  set.seed(seed)
  n <- n_per_block * n_blocks
  ids <- sprintf("n%02d", seq_len(n))
  block <- rep(seq_len(n_blocks), each = n_per_block)
  nodes <- data.frame(
    group = "1", node = rep(ids, 2), wave = rep(1:2, each = n),
    x = stats::rnorm(2 * n), stringsAsFactors = FALSE
  )
  p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
  p <- p[p$source != p$target, ]
  same <- block[match(p$source, ids)] == block[match(p$target, ids)]
  ties <- do.call(rbind, lapply(1:2, function(w) {
    pr <- if (clustered) ifelse(same, 0.85, 0.05) else 0.30
    data.frame(group = "1", p, wave = w,
               tie = stats::rbinom(nrow(p), 1, pr), stringsAsFactors = FALSE)
  }))
  netis_data(nodes, ties, wave_col = "wave",
             tie_meta = list(tie = list(zero_meaning = "measured_zero")))
}

#' A cheap Influence fit, for the wrong-family refusal
#'
#' NOT `helper-plot.R`'s `.plot_fixture_influence()`, which passes
#' `waves = list(c(1, 2))` and has been refused by `fit_influence()` since the
#' wave specification tightened to one transition. Nothing called it, so
#' nothing noticed. Built here against the current API rather than reaching for
#' it, so this file does not depend on a fixture that is already broken.
#' @noRd
.gof_fixture_influence <- function() {
  .cached_fixture("gof_influence", {
    fit_influence(y ~ ar() + exposure(x, via = tie, normalize = "row"),
                  data = demo_influence, waves = c(1, 2))
  })
}

#' @noRd
.gof_fixture_fit <- function(clustered, seed = 1L, nscan = 500L, gof = TRUE,
                             R = 0L) {
  key <- paste("gof", clustered, seed, nscan, gof, R, sep = "_")
  .cached_fixture(key, {
    fit_selection(
      tie ~ prior_tie() + sender(x) + receiver(x),
      data = .gof_fixture_data(clustered), family = "bin", R = R,
      nscan = nscan, burn = 100, odens = 10, seed = seed, gof = gof
    )
  })
}

# ---- the table --------------------------------------------------------------

test_that("netis_gof() reports every statistic amen simulated, with its own distribution", {
  g <- netis_gof(.gof_fixture_fit(clustered = FALSE))

  expect_s3_class(g, "data.frame")
  expect_identical(
    names(g),
    c("grouping", "statistic", "feature", "observed", "predicted_mean",
      "predicted_sd", "predicted_lower", "predicted_upper", "ppp", "z",
      "n_draws")
  )
  # amen's five, in amen's order. Asserted rather than counted, because the
  # order is what the plot facets by and what a reader works through.
  expect_identical(
    g$statistic,
    c("sd.rowmean", "sd.colmean", "dyad.dep", "cycle.dep", "trans.dep")
  )
  # Every statistic carries a plain-language label. An amen release adding a
  # sixth would report it with an NA feature rather than erroring, which is
  # why this is asserted on the five that exist rather than on all().
  expect_false(anyNA(g$feature))
  expect_true(all(g$ppp >= 0 & g$ppp <= 1))
  expect_true(all(g$predicted_lower <= g$predicted_upper))
})

test_that("n_draws is the number of networks actually simulated, not the scan count", {
  fit <- .gof_fixture_fit(clustered = FALSE)
  g <- netis_gof(fit)
  # nscan = 500, burn = 100, odens = 10: amen keeps (500 - 100) / 10 = 40.
  # Read from the fit rather than recomputed from the controls, so this tests
  # the summary against the draws it summarized.
  kept <- nrow(fit$per_group[[1]]$draws)
  expect_identical(unique(g$n_draws), as.integer(kept))
  expect_gt(kept, 1L)
})

test_that("z is the observed value's distance from the predictive mean, in predictive SDs", {
  g <- netis_gof(.gof_fixture_fit(clustered = FALSE))
  # An internal-consistency identity: whatever the chain did, these three
  # columns must agree with the fourth, or one of them is summarizing a
  # different thing than its name says.
  expect_equal(g$z, (g$observed - g$predicted_mean) / g$predicted_sd,
               tolerance = 1e-12)
})

test_that("the observed statistics are a property of the network, not of the chain", {
  # THE TEST THAT CATCHES A ROW MIX-UP. amen puts the observed network in row 1
  # and the simulated ones below it. Summarizing the whole matrix, or taking
  # the wrong row as observed, would make `observed` drift with the seed --
  # because the draws do. It must not: the observed network is the same network
  # either way.
  a <- netis_gof(.gof_fixture_fit(clustered = FALSE, seed = 1L))
  b <- netis_gof(.gof_fixture_fit(clustered = FALSE, seed = 2L))

  expect_identical(a$observed, b$observed)
  # And the other half of the claim: the predictive side DID move, so the two
  # fits really were different chains and the identity above is not vacuous.
  expect_false(isTRUE(all.equal(a$predicted_mean, b$predicted_mean)))
})

# ---- the check it exists to make -------------------------------------------

test_that("an additive fit reproduces the degree statistics and misses closure", {
  # The substantive claim, and the reason R > 0 exists: sender and receiver
  # effects are what account for unequal sending and unequal popularity, and
  # nothing additive accounts for transitive triads. So the SAME model fitted
  # to a clustered network and to an unclustered one must come apart on
  # trans.dep and not on the degree statistics.
  random <- netis_gof(.gof_fixture_fit(clustered = FALSE))
  blocked <- netis_gof(.gof_fixture_fit(clustered = TRUE))

  trans_r <- random[random$statistic == "trans.dep", ]
  trans_b <- blocked[blocked$statistic == "trans.dep", ]

  # Without closure, the observed value sits inside the predictive range.
  expect_gt(trans_r$observed, trans_r$predicted_lower)
  expect_lt(trans_r$observed, trans_r$predicted_upper)

  # With closure, it is outside it, and far: no simulated network reached the
  # observed value, so the one-sided ppp is exactly 0.
  expect_gt(trans_b$observed, trans_b$predicted_upper)
  expect_identical(trans_b$ppp, 0)
  # Stated as a gap rather than a threshold. The separation measured when this
  # test was written was about 5 predictive SDs against about 1; asserting 3
  # leaves room for chain noise without letting a regression through.
  expect_gt(abs(trans_b$z) - abs(trans_r$z), 3)

  # And the degree statistics do NOT come apart the same way: the additive
  # terms are there for exactly these, so neither network should push them as
  # far out as closure goes.
  deg_b <- blocked[blocked$statistic == "sd.rowmean", ]
  expect_lt(abs(deg_b$z), abs(trans_b$z))
})

# ---- refusals ---------------------------------------------------------------

test_that("a fit run with gof = FALSE is refused rather than compared against nothing", {
  # amen returns its matrix with the observed row ONLY in this case. A
  # summary of it would compare the observed value against an empty
  # distribution and report ppp = NaN, which looks like a number.
  fit <- .gof_fixture_fit(clustered = FALSE, nscan = 200L, gof = FALSE)
  expect_error(netis_gof(fit), "gof = FALSE")
  expect_error(netis_gof(fit), "Refit with gof = TRUE")
})

test_that("a fit that never recorded the statistics says so, and says to refit", {
  # A fit saved by a version of rnetis whose amen adapter did not carry the
  # matrix at all. There is no way to produce one from the current package, so
  # it is simulated by removing the element -- which is precisely what such a
  # fit looks like from here. The two absences need different instructions:
  # telling someone to re-run with gof = TRUE when the problem is the version
  # they saved under sends them round a loop.
  fit <- .gof_fixture_fit(clustered = FALSE)
  stale <- fit
  stale$per_group[[1]]$gof <- NULL

  expect_error(netis_gof(stale), "holds no posterior predictive network statistics")
  expect_error(netis_gof(stale), "Refit with the current version")
})

test_that("a hierarchical fit is refused, and names what to do instead", {
  # Its sampler does not simulate a network per scan, deliberately: the
  # simulation does not affect the posterior. The refusal has to say that
  # rather than imply the check failed.
  fit <- .hierarchical_fixture_fit()
  expect_error(netis_gof(fit), "not defined for a hierarchical multi-group Selection fit")
  expect_error(netis_gof(fit), "Fit the groups separately")
})

test_that("an Influence fit is refused and pointed at netis_dependence()", {
  expect_error(
    netis_gof(.gof_fixture_influence()),
    "defined for netis_selection fits"
  )
  expect_error(netis_gof(.gof_fixture_influence()), "netis_dependence")
})

test_that("anything else is refused by class", {
  expect_error(netis_gof(data.frame(a = 1)), "defined for netis_selection fits")
})

test_that("naming a group the fit does not have is refused with the list", {
  fit <- .gof_fixture_fit(clustered = FALSE)
  expect_error(netis_gof(fit, group = "nope"), "is not in this fit")
})

# ---- several groups ---------------------------------------------------------

test_that("every group of a multi-group fit gets its own statistics", {
  # Each group was fitted to its own network, so each has its own observed
  # statistics AND its own simulated ones. There is no fit-level GOF to fall
  # back on, which is why this panel needs no group argument.
  fit <- .selection_fixture_fit(n_groups = 2, n_actors = 10)
  g <- netis_gof(fit)

  expect_identical(sort(unique(g$grouping)), sort(as.character(fit$groups)))
  expect_identical(nrow(g), 5L * length(fit$groups))

  one <- netis_gof(fit, group = fit$groups[[1]])
  expect_identical(nrow(one), 5L)
  expect_identical(unique(one$grouping), as.character(fit$groups[[1]]))
  # The same rows either way: narrowing selects, it does not recompute.
  expect_equal(one$observed, g$observed[g$grouping == as.character(fit$groups[[1]])])
})

# ---- the panel --------------------------------------------------------------

test_that("the table carries the simulated draws it was reduced from", {
  fit <- .gof_fixture_fit(clustered = FALSE)
  g <- netis_gof(fit)
  draws <- attr(g, "netis_draws")
  expect_named(draws, c("grouping", "statistic", "value"))
  # One row per simulated network per statistic: the histogram has exactly the
  # observations the row's n_draws says its summaries were computed from.
  expect_identical(nrow(draws), sum(g$n_draws))
  for (s in g$statistic) {
    v <- draws$value[draws$statistic == s]
    row <- g[g$statistic == s, ]
    expect_equal(mean(v), row$predicted_mean)
    expect_equal(unname(stats::quantile(v, 0.025)), row$predicted_lower)
    expect_equal(unname(stats::quantile(v, 0.975)), row$predicted_upper)
    expect_equal(mean(v >= row$observed), row$ppp)
  }
})

test_that("the gof panel is a histogram with the 95% band and the observed line", {
  fit <- .gof_fixture_fit(clustered = FALSE)
  p <- plot(fit, what = "gof")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomBar" %in% geoms)      # geom_histogram()
  expect_true("GeomRect" %in% geoms)     # the central 95% band
  expect_true("GeomSegment" %in% geoms)  # the observed network
  built <- ggplot2::ggplot_build(p)
  # One panel per statistic, and every simulated network is in a bar.
  hist <- built$data[[which(geoms == "GeomBar")]]
  g <- netis_gof(fit)
  expect_identical(length(unique(hist$PANEL)), nrow(g))
  expect_equal(sum(hist$count), sum(g$n_draws))
  # The band is drawn at the table's quantiles and the line at its observed
  # value: the figure and the table cannot disagree.
  band <- built$data[[which(geoms == "GeomRect")]]
  expect_equal(band$xmin, g$predicted_lower)
  expect_equal(band$xmax, g$predicted_upper)
  expect_equal(built$data[[which(geoms == "GeomSegment")]]$x, g$observed)
})

test_that("the gof panel draws exactly the table netis_gof() returned", {
  fit <- .gof_fixture_fit(clustered = FALSE)
  d <- plot_data(fit, what = "gof")
  # Identical, not merely similar: a second summary here could disagree with
  # the first, and then the picture and the number would tell different stories.
  expect_equal(d, netis_gof(fit))

  p <- plot(fit, what = "gof")
  expect_s3_class(p, "ggplot")
  # NO REFERENCE ANNOTATION AT ALL. There is no threshold in a posterior
  # predictive check, and a line through the middle of every predictive range
  # would be read as a target the observed value is supposed to hit.
  expect_length(.netis_references_of(d), 0L)
  expect_length(.netis_drawn_references(p), 0L)
})

test_that("the panel's caption states what the marks mean and refuses a verdict", {
  d <- plot_data(.gof_fixture_fit(clustered = FALSE), what = "gof")
  caption <- paste(attr(d, "netis_caption"), collapse = " ")
  # The RELATIONSHIP between the marks, not just their names: the legend
  # identifies the point and the bar, and the caption has to say what it
  # means for one to be inside the other.
  expect_match(caption, "Line inside the shaded band", fixed = TRUE)
  expect_match(caption, "Line outside: it cannot", fixed = TRUE)
  expect_match(caption, "Each histogram is the statistic across", fixed = TRUE)
  expect_match(caption, "simulated from the fitted model", fixed = TRUE)
  expect_match(caption, "own x scale", fixed = TRUE)
  # Both ends are misfit, and the reader has to be told so: a one-sided
  # probability read as a p-value would call ppp = 0.98 a good fit.
  expect_match(caption, "0 and 1 are both misfit")
  expect_match(caption, "No threshold is applied")
})

test_that("the caption's triadic line follows the model, not the statistic list", {
  # THE SAME STATISTICS, A DIFFERENT SENTENCE. "Nothing additive accounts for
  # the triadic statistics" excuses a misfit an AME fit has no excuse for: at
  # R > 0 those statistics are precisely what the multiplicative effects were
  # added to account for, so a reader who saw the additive caption on an AME
  # fit would wave away the one result they should be weighing.
  # The same data and the same formula, so R is the only thing that differs --
  # which is the point: the statistic list is identical and the sentence is not.
  # Not helper-plot.R's `.plot_fixture_selection()`, which is stale the same way
  # its influence sibling is: it omits prior_tie() on two-wave data, which
  # fit_selection() has refused since it stopped pooling transitions.
  additive <- plot_data(.gof_fixture_fit(clustered = FALSE), what = "gof")
  multiplicative <- plot_data(
    .gof_fixture_fit(clustered = FALSE, nscan = 300L, R = 1L), what = "gof"
  )

  # fixed = TRUE: the sentence under test contains parentheses, and the point
  # is the sentence, not a pattern that happens to match it.
  expect_match(paste(attr(additive, "netis_caption"), collapse = " "),
               "Nothing in this additive (R = 0) model", fixed = TRUE)
  expect_match(paste(attr(multiplicative, "netis_caption"), collapse = " "),
               "what the multiplicative effects (R = 1) are there to account for",
               fixed = TRUE)
  # And the additive sentence does not appear on the AME fit.
  expect_false(grepl("Nothing in this additive",
                     paste(attr(multiplicative, "netis_caption"), collapse = " ")))
})
