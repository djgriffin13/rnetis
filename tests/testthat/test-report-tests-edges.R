# The printed tables at their edges: the cases an independent review found the
# first version wrong or silent on. Every expected value here is written out by
# hand or read from the backend, never produced by the helper under test.

.rte_out <- function(x) {
  gsub("[ \n]+", " ", paste(utils::capture.output(print(x)), collapse = "\n"))
}

# ---- degrees of freedom ------------------------------------------------------

test_that("a whole df less floating-point noise prints as that whole number", {
  # THE REGRESSION. A boundary fit's Satterthwaite df comes back as
  # 20.9999999997; an integer format truncates that to 20, beside an interval
  # and a p-value computed on 21.
  expect_identical(.netis_format_df(c(20.9999999997, 21.0000000002, NA)),
                   c("21", "21", ""))
  # Fractional df keep one decimal, for every row.
  expect_identical(.netis_format_df(c(85.24, 7)), c("85.2", "7.0"))
})

test_that("the printed df column is lmerTest's df", {
  skip_if_not_installed("lme4")
  fit <- .vc_intercept_fit()
  ref <- stats::coef(summary(fit$backend_fit))[, "df"]
  tab <- .add_influence_tests(.format_estimate_table(.estimate_frame(fit)),
                              .influence_coef_tests(fit))
  shown <- as.numeric(tab[["df"]])
  # To the precision printed: whole numbers exactly, otherwise one decimal.
  expect_lt(max(abs(shown - unname(ref))), 0.05 + 1e-8)
  expect_identical(nrow(tab), length(ref))
})

# ---- one table, one block ----------------------------------------------------

test_that("a coefficient table does not wrap its marks away at an 80-column console", {
  skip_if_not_installed("amen")
  old <- options(width = 80)
  on.exit(options(old), add = TRUE)
  fit <- .hierarchical_fixture_fit()
  out <- utils::capture.output(print(fit))
  header <- grep("^ *Term ", out)
  expect_length(header, 1L)
  # Every column in the one header line, the posterior columns included.
  expect_match(out[header], "Regime +Estimate +Post\\.SD +95% interval +Est/SD +Tail prob\\.")
  # ONE BLOCK: exactly one line per coefficient between the header and the
  # legend. A wrapped table prints the mark column as a second block there, one
  # more line per coefficient plus its own header.
  legend <- grep("^---$", out)
  expect_length(legend, 1L)
  n_coef <- nrow(.selection_estimate_table(fit))
  expect_identical(legend - header - 1L, n_coef)
  # And the marks are on the rows: the table has one, and its row ends in it.
  marks <- .selection_estimate_table(fit)[[" "]]
  expect_true(any(nzchar(marks)))
  rows <- out[header + seq_len(n_coef)]
  expect_identical(nzchar(marks), grepl("[*.]$", rows))
  # The option is restored: printing a fit changes nothing for the caller.
  expect_identical(getOption("width"), 80L)

  # The summary's predictive table is one block too, at a width it would
  # otherwise wrap at: its last column stays on the rows it describes.
  options(width = 50)
  s <- utils::capture.output(print(summary(.selection_fixture_fit())))
  expect_length(grep("^ *Statistic .* Counted *$", s), 1L)
  expect_identical(getOption("width"), 50L)
})

# ---- the posterior columns, by hand ------------------------------------------

test_that("the tail probability and its mark are what the draws say, computed by hand", {
  skip_if_not_installed("amen")
  fit <- .selection_fixture_fit()
  tab <- .selection_estimate_table(fit)
  d <- .selection_beta_draws(fit$draws, tab$Term)
  for (k in tab$Term) {
    v <- d[, k]
    s <- length(v)
    tail <- 2 * min(sum(v > 0), sum(v < 0)) / s
    bound <- 2 / s
    expect_shown <- if (tail < bound) sprintf("<%.3f", bound) else sprintf("%.3f", tail)
    p <- max(tail, bound)
    expect_mark <- if (p <= 0.001) "***" else if (p <= 0.01) "**" else
      if (p <= 0.05) "*" else if (p <= 0.1) "." else ""
    i <- which(tab$Term == k)
    expect_identical(tab[["Tail prob."]][i], expect_shown)
    expect_identical(tab[[" "]][i], expect_mark)
  }
})

test_that("draws with no spread earn no ratio, no tail probability and no mark", {
  t <- .posterior_tail(rep(0.7, 50))
  expect_true(all(is.na(t)))
  tab <- .add_posterior_columns(data.frame(Term = "a"), list(t))
  expect_identical(tab[["Est/SD"]], "")
  expect_identical(tab[["Tail prob."]], "")
  expect_identical(tab[[" "]], "")
})

# ---- what the posterior predictive line counts -------------------------------

# One fitted object, its simulated statistics rewritten so each case is known.
.rte_gof_fit <- function(edit) {
  fit <- .selection_fixture_fit()
  set.seed(3)
  m <- fit$per_group[[1]]$gof
  # Five unrelated, well-spread columns with the observed value at the centre.
  for (j in seq_len(ncol(m))) {
    m[-1, j] <- stats::rnorm(nrow(m) - 1L, mean = j, sd = 0.1 * j)
    m[1, j] <- j
  }
  fit$per_group[[1]]$gof <- edit(m)
  fit
}

test_that("every statistic inside its range is counted, and nothing is flagged", {
  skip_if_not_installed("amen")
  out <- .rte_out(.rte_gof_fit(identity))
  expect_match(out, "inside the simulated 95% range on 5 of 5 statistics.", fixed = TRUE)
  expect_false(grepl("Outside:", out, fixed = TRUE))
  expect_false(grepl("Not counted:", out, fixed = TRUE))
})

test_that("a statistic outside its range is counted out and named with its ppp", {
  skip_if_not_installed("amen")
  fit <- .rte_gof_fit(function(m) { m[1, "trans.dep"] <- 1e6; m })
  out <- .rte_out(fit)
  expect_match(out, "on 4 of 5 statistics.", fixed = TRUE)
  # Observed above every simulated value: no draw is at least as large.
  expect_match(out, "Outside: trans.dep (ppp 0.00).", fixed = TRUE)
})

test_that("a statistic that could not be checked is said to be, not dropped", {
  skip_if_not_installed("amen")
  fit <- .rte_gof_fit(function(m) { m[-1, "trans.dep"] <- NaN; m })
  out <- .rte_out(fit)
  expect_match(out, "on 4 of 4 statistics.", fixed = TRUE)
  expect_match(out, "Not counted: 1 with no usable simulated distribution.", fixed = TRUE)
  s <- .rte_out(summary(fit))
  expect_match(s, "trans.dep transitive triads [0-9.]+ not available +no: not available")
})

test_that("a constant statistic and a repeated one are not counted as checks", {
  skip_if_not_installed("amen")
  # What a symmetric fit produces: dyad.dep identically 1, the in-degree spread
  # equal to the out-degree spread, the two triadic statistics equal.
  fit <- .rte_gof_fit(function(m) {
    m[, "dyad.dep"] <- 1
    m[, "sd.colmean"] <- m[, "sd.rowmean"]
    m[, "trans.dep"] <- m[, "cycle.dep"]
    m
  })
  out <- .rte_out(fit)
  expect_match(out, "on 2 of 2 statistics.", fixed = TRUE)
  expect_match(out, "Not counted: 1 constant under the model, 2 identical to another statistic.",
               fixed = TRUE)
})

test_that("a model that generates one value where the network shows another has failed that check", {
  skip_if_not_installed("amen")
  # Constant under the model, and NOT what was observed: a misfit, and counted
  # as one. Only a constant the network also shows is left out of the count.
  fit <- .rte_gof_fit(function(m) { m[-1, "dyad.dep"] <- 0.5; m[1, "dyad.dep"] <- -0.3; m })
  out <- .rte_out(fit)
  expect_match(out, "on 4 of 5 statistics.", fixed = TRUE)
  expect_match(out, "Outside: dyad.dep", fixed = TRUE)
  expect_false(grepl("constant under the model", out, fixed = TRUE))
})

test_that("an observed value that is missing is said to be unchecked", {
  skip_if_not_installed("amen")
  fit <- .rte_gof_fit(function(m) { m[1, "cycle.dep"] <- NA; m })
  out <- .rte_out(fit)
  expect_match(out, "on 4 of 4 statistics.", fixed = TRUE)
  expect_match(out, "Not counted: 1 with no usable simulated distribution.", fixed = TRUE)
})

test_that("a fit with no standard errors prints no test columns and says why", {
  fit <- fit_influence(y ~ ar() + exposure(x, via = tie, normalize = "row"),
                       data = demo_influence, waves = c(1, 2))
  # What sna::lnam leaves when its information matrix is unavailable.
  fit$backend <- "lnam"
  fit$vcov[] <- NA_real_
  tests <- .influence_coef_tests(fit)
  expect_null(tests$p)
  tab <- .add_influence_tests(.format_estimate_table(.estimate_frame(fit)), tests)
  expect_false(any(c("z value", "Pr(>|z|)", " ") %in% names(tab)))
  expect_identical(attr(tab, "netis_notes"), "No z tests: this fit has no standard errors.")
})

test_that("a multi-group fit counts group-by-statistic checks and names where they fail", {
  skip_if_not_installed("amen")
  fit <- .selection_fixture_fit(n_groups = 3)
  set.seed(5)
  for (g in seq_along(fit$per_group)) {
    m <- fit$per_group[[g]]$gof
    for (j in seq_len(ncol(m))) {
      m[-1, j] <- stats::rnorm(nrow(m) - 1L, mean = j, sd = 0.1 * j)
      m[1, j] <- j
    }
    # Groups 1 and 3 miss closure; nothing else misses.
    if (g != 2L) m[1, "trans.dep"] <- -1e6
    fit$per_group[[g]]$gof <- m
  }
  out <- .rte_out(fit)
  expect_match(out, "on 13 of 15 group-by-statistic checks.", fixed = TRUE)
  expect_match(out, "Outside: trans.dep (2 groups).", fixed = TRUE)
  s <- .rte_out(summary(fit))
  expect_match(s, "trans.dep transitive triads 1 of 3", fixed = TRUE)
  expect_match(s, "sd.rowmean out-degree spread 3 of 3", fixed = TRUE)
})

# ---- the other tables --------------------------------------------------------

test_that("a two-stage pooled fit prints z tests from its pooled SEs", {
  skip_if_not_installed("amen")
  fit <- .selection_fixture_fit(n_groups = 3)
  skip_if_not(.selection_is_pooled(fit))
  tab <- .selection_estimate_table(fit)
  est <- fit$pooled_beta$mean %||% fit$coefficients
  se <- sqrt(fit$pooled_beta$var)
  z <- unname(est[tab$Term] / se[tab$Term])
  expect_identical(tab[["z value"]], sprintf("%.3f", z))
  p <- 2 * stats::pnorm(-abs(z))
  expect_identical(tab[["Pr(>|z|)"]],
                   ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
  expect_true("SE" %in% names(tab))
  expect_false("Post.SD" %in% names(tab))
})

test_that("the F line carries summary.lm()'s p-value", {
  nd <- netis_data(
    data.frame(group = 1, node = rep(sprintf("a%02d", 1:20), 2), wave = rep(1:2, each = 20),
               y = c(seq(-1, 1, length.out = 20), seq(-1, 1, length.out = 20) * 0.6 +
                       rep(c(0.3, -0.2, 0.1, -0.4), 5)),
               x = rep(c(1, 0, 0, 1, 1), 8)),
    data.frame(group = 1, source = sprintf("a%02d", 1:19), target = sprintf("a%02d", 2:20),
               wave = 1, w = 1),
    wave_col = "wave"
  )
  fit <- fit_influence(y ~ ar() + x + exposure(y, via = w), data = nd)
  f <- summary(fit$backend_fit)$fstatistic
  p <- stats::pf(f[[1]], f[[2]], f[[3]], lower.tail = FALSE)
  expect_match(.rte_out(summary(fit)),
               sprintf("DF, p-value: %s", format.pval(p, digits = 4)), fixed = TRUE)
})

test_that("a 0/1 moderator's reference row is printed once", {
  fit <- fit_influence(
    affect ~ ar() + exposure(affect, via = confides, normalize = "row") * remote,
    data = demo_affect
  )
  out <- utils::capture.output(print(influence_effect(fit)))
  expect_identical(sum(grepl("remote = 0 (reference)", out, fixed = TRUE)), 1L)
  expect_true(any(grepl("remote = 1:", out, fixed = TRUE)))
})
