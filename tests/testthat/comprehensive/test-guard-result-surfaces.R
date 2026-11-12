# SHORT CHAINS, deliberately.
#
# Every tolerance in this file is either an exact structural value (a row
# count, a class, a field name) or an INTERNAL CONSISTENCY identity -- two
# quantities derived from the same fit that must agree with each other, such
# as shares summing to 1. Neither kind moves with chain length: the chain only
# has to be long enough to return a well-formed object. A tolerance against an
# externally known truth would need longer chains and a recorded margin.
# The user-facing result surfaces.
#
# GUARD TIER, because these tests break in response to changes anywhere rather
# than in one feature. These accessors read the fitted
# object's layout for BOTH families, so a field renamed in the Influence
# assembler, the Selection assembler, or any backend adapter surfaces here
# rather than in whichever feature file happens to touch it. They are also fast,
# and carry no fixture anything else depends on.
#
# WHAT THESE TESTS ARE FOR. Headline quantities must be reachable in one
# obvious call and visible in default printing. Both halves are
# asserted: the accessor returns the right shape, AND the printed output
# actually contains the quantity. A surface that requires reading the source to
# use is the thing being fixed.

# Fits are built once and reused: refitting the same model in a dozen tests
# would be almost all repeated MCMC runs of two fixtures whose values none of
# these tests depend on.
.rs_cache <- new.env(parent = emptyenv())

.rs_memo <- function(key, build) {
  if (is.null(.rs_cache[[key]])) .rs_cache[[key]] <- build()
  .rs_cache[[key]]
}

# Hand-built rather than simulated: the simulation layer is not in this package
# yet, and these are surface-contract tests -- what print()/summary() say and
# what the accessors return -- which a fixed two-wave fixture exercises exactly
# as well as a generated one.
.rs_influence_data <- function(n = 30, seed = 3) {
  set.seed(seed)
  ids <- sprintf("n%02d", seq_len(n))
  nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = n),
                      y = stats::rnorm(2 * n), stringsAsFactors = FALSE)
  ties <- data.frame(group = 1, source = ids[-n], target = ids[-1], wave = 1,
                     tie = 1, stringsAsFactors = FALSE)
  netis_data(nodes, ties, wave_col = "wave")
}

.rs_influence <- function() {
  .rs_memo("influence", function() {
    fit_influence(
      y ~ ar() + exposure(y, via = tie, normalize = "row", isolate_rule = "zero"),
      data = .rs_influence_data()
    )
  })
}

.rs_selection <- function(n_groups = 1, pool_beta = TRUE) {
  .rs_memo(paste0("selection", n_groups, "_pool", pool_beta), function() {
    set.seed(9)
    n <- 8L
    ids <- letters[seq_len(n)]
    nodes <- do.call(rbind, lapply(seq_len(n_groups), function(g) {
      data.frame(group = g, node = ids, tenure = stats::rnorm(n), stringsAsFactors = FALSE)
    }))
    pairs <- subset(expand.grid(source = ids, target = ids, stringsAsFactors = FALSE),
                    source != target)
    ties <- do.call(rbind, lapply(seq_len(n_groups), function(g) {
      data.frame(group = g, pairs,
                 friendship = stats::rbinom(nrow(pairs), 1, 0.3), stringsAsFactors = FALSE)
    }))
    nd <- netis_data(nodes, ties,
                     tie_meta = list(friendship = list(zero_meaning = "measured_zero")))
    # A short chain deliberately: nothing here asserts a posterior VALUE, only
    # the shape and provenance of what comes back, so chain length buys nothing.
    fit_selection(friendship ~ sender(tenure), data = nd, pool_beta = pool_beta,
                  nscan = 60, burn = 20, odens = 10)
  })
}

.ESTIMATE_COLS <- c("term", "group", "estimate", "std.error", "conf.low",
                    "conf.high", "role", "scale")

# ---------------------------------------------------------------------------
# One shape, both families

test_that("netis_estimates() returns the same columns for both families", {
  skip_if_not_installed("amen")
  inf <- netis_estimates(.rs_influence())
  sel <- netis_estimates(.rs_selection())

  expect_identical(names(inf), .ESTIMATE_COLS)
  expect_identical(names(sel), .ESTIMATE_COLS)
  # The whole point of the accessor: code written against one family's results
  # runs unchanged against the other's.
  expect_identical(vapply(inf, class, character(1)),
                   vapply(sel, class, character(1)))
})

test_that("every estimate carries an interval and a named scale", {
  skip_if_not_installed("amen")
  for (fit in list(.rs_influence(), .rs_selection())) {
    est <- netis_estimates(fit)
    expect_gt(nrow(est), 0L)
    expect_false(anyNA(est$estimate))
    expect_false(anyNA(est$conf.low))
    expect_false(anyNA(est$conf.high))
    expect_true(all(est$conf.low <= est$estimate))
    expect_true(all(est$estimate <= est$conf.high))
    # A column of numbers with no scale invites being read as an odds ratio, a
    # probability, or outcome units interchangeably.
    expect_false(anyNA(est$scale))
    expect_true(all(nzchar(est$scale)))
  }
})

test_that("Influence terms are labeled in the framework's own vocabulary", {
  est <- netis_estimates(.rs_influence())
  expect_identical(est$role[est$term == "y_lag1"], "stability")
  expect_identical(est$role[est$term == "ex_y_tie"], "exposure")
  expect_identical(est$role[est$term == "(Intercept)"], "direct")
})

test_that("the interval level is honoured and widens with it", {
  fit <- .rs_influence()
  narrow <- netis_estimates(fit, level = 0.80)
  wide <- netis_estimates(fit, level = 0.99)
  expect_true(all(wide$conf.high - wide$conf.low > narrow$conf.high - narrow$conf.low))
})

test_that("an unpooled multi-group Selection fit reports one row per group", {
  skip_if_not_installed("amen")
  # pool_beta = FALSE explicitly. This test once used the pooled DEFAULT and
  # passed only because every accessor misread a pooled fit as unpooled -- it
  # encoded the defect rather than the contract.
  est <- netis_estimates(.rs_selection(n_groups = 3, pool_beta = FALSE))
  # Not collapsed into one set: independently fitted groups are not a joint
  # model and stacking them without the group column would imply one.
  expect_setequal(unique(est$group), c("1", "2", "3"))
  expect_equal(nrow(est), 3L * length(unique(est$term)))
})

test_that("a pooled multi-group Selection fit reports the pooled coefficients once", {
  skip_if_not_installed("amen")
  fit <- .rs_selection(n_groups = 3)
  est <- netis_estimates(fit)
  expect_true(all(is.na(est$group)))
  expect_equal(est$estimate, unname(coef(fit)), tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Absent, not fabricated

test_that("a fit with no variance components says so instead of returning NAs", {
  vc <- netis_variance_components(.rs_influence())
  expect_equal(nrow(vc), 0L)
  # A zero-row frame with a reason, NOT a row of NAs: an OLS fit does not have
  # a near-zero ICC, it has none, and the two must not print the same way.
  expect_match(attr(vc, "reason"), "absence, not a value near zero")
})

test_that("variance components separate variances from correlations", {
  skip_if_not_installed("amen")
  vc <- netis_variance_components(.rs_selection())
  expect_true(all(c("grouping", "term", "kind", "variance", "sd", "residual_variance_share") %in% names(vc)))
  # A correlation has no variance and no SD, and sqrt() of one is not a
  # standard deviation of anything -- but it is still reported rather than
  # dropped, so nothing the model estimated goes unmentioned.
  corr <- vc[vc$kind != "variance", , drop = FALSE]
  expect_gt(nrow(corr), 0L)
  expect_true(all(is.na(corr$variance)))
  expect_true(all(is.na(corr$residual_variance_share)))
  # And the shares over the variance-like components are a partition.
  shares <- vc$residual_variance_share[vc$kind == "variance"]
  expect_equal(sum(shares), 1, tolerance = 1e-8)
})

test_that("an lme4-backed fit reports its components with shares that sum to one", {
  nodes <- do.call(rbind, lapply(1:6, function(g) {
    data.frame(group = g, node = rep(letters[1:5], 2), wave = rep(1:2, each = 5),
               y = (seq_len(10) %% 4) + g / 2, stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(1:6, function(g) {
    data.frame(group = g, source = letters[1:4], target = letters[2:5], tie = 1)
  }))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  fit <- fit_influence(y ~ ar() + exposure(y, via = tie) + (1 | group), data = nd)

  vc <- netis_variance_components(fit)
  expect_gt(nrow(vc), 0L)
  expect_true("(residual)" %in% vc$term || "Residual" %in% vc$grouping)
  expect_equal(sum(vc$residual_variance_share), 1, tolerance = 1e-8)
})

test_that("the accessors refuse an object that is not a rnetis fit, by name", {
  expect_error(netis_estimates(1:3), "no method for an object of class")
  expect_error(netis_glance(lm(1 ~ 1)), "no method for an object of class")
  expect_error(netis_variance_components("x"), "no method for an object of class")
  expect_error(netis_estimand(NULL), "no method for an object of class")
})

# ---------------------------------------------------------------------------
# glance and estimand

test_that("netis_glance() is one row and names the estimand and the routing", {
  skip_if_not_installed("amen")
  for (fit in list(.rs_influence(), .rs_selection())) {
    g <- netis_glance(fit)
    expect_equal(nrow(g), 1L)
    # `nobs` and `r.squared` follow the glance convention; `estimand`,
    # `n_groups` and the diagnostic counts are rnetis's own, because the
    # convention has no counterpart for them.
    expect_true(all(c("family", "estimand", "backend", "nobs", "n_groups",
                      "n_error", "n_warning", "n_note", "netis_version") %in% names(g)))
    expect_false(is.na(g$estimand))
  }
})

test_that("netis_estimand() carries what summary() states, as data", {
  fit <- .rs_influence()
  e <- netis_estimand(fit)
  expect_identical(e$family, "influence")
  expect_identical(e$estimand, fit$spec$estimand)
  # Two fits are comparable only if these match, which is exactly why the
  # statement has to be machine-readable and not only narrated.
  expect_true(all(c("transitions", "outcome_waves", "estimator", "estimator_reason") %in% names(e)))
  expect_false(any(grepl("endogeneity", names(e))))
  # An assumption no estimator addresses is reported by the assumption ledger,
  # which reports every such row. Row C is the temporal-endogeneity row.
  ids <- vapply(netis_assumptions(.rs_influence())$entries,
                function(a) a$id, character(1))
  expect_true("C" %in% ids)
})

# ---------------------------------------------------------------------------
# Default printing shows it

test_that("print() shows uncertainty, not a bare coefficient vector", {
  skip_if_not_installed("amen")
  for (fit in list(.rs_influence(), .rs_selection())) {
    out <- paste(utils::capture.output(print(fit)), collapse = "\n")
    expect_match(out, "Estimate")
    # A Selection fit's spread is a posterior SD and is labelled as one.
    expect_match(out, if (inherits(fit, "netis_selection")) " Post\\.SD " else " SE ")
    # THE MASS IS NAMED, not left to be assumed. The column used to read
    # "Interval", which invites the reader to supply "95%" from habit; an
    # interval whose coverage is assumed rather than stated is the kind of
    # number that gets quoted with the wrong label.
    expect_match(out, "interval")
    expect_match(out, "9[05]% interval")
    # The specific regression: both families used to print a named numeric with
    # no standard error and no interval at all.
    expect_match(out, "\\[-?[0-9.]+, -?[0-9.]+\\]")
  }
})

test_that("summary() shows intervals beside the tests", {
  out <- paste(utils::capture.output(print(summary(.rs_influence()))), collapse = "\n")
  expect_match(out, "95% interval")
  expect_match(out, "Estimand:")
  # And it points at the accessors rather than leaving the reader to find them.
  expect_match(out, "netis_estimates\\(\\)")
  expect_match(out, "netis_glance\\(\\)")
})

test_that("summary.netis_selection() returns an object rather than printing", {
  skip_if_not_installed("amen")
  fit <- .rs_selection()
  # Printing as a side effect and returning the fit invisibly would make
  # `s <- summary(fit)` give back the fit, and code handling both families
  # uniformly could not.
  s <- expect_silent(summary(fit))
  expect_s3_class(s, "summary.netis_selection")
  out <- paste(utils::capture.output(print(s)), collapse = "\n")
  expect_match(out, "Coefficients")
  # Variance components are NOT printed here. The Selection summary
  # states what was fitted plus the coefficient table; variance components are
  # an interpretation surface, reachable as data rather than printed at every
  # summary.
  vc <- netis_variance_components(fit)
  expect_true(is.data.frame(vc) || is.null(vc))
})

test_that("the dependence quantities are reported by their own accessor", {
  # fit_influence() returns estimates, and interpretation is computed on
  # request, so dependence is reached through its own accessor rather than
  # through summary().
  out <- paste(utils::capture.output(
    print(netis_dependence(.rs_influence(), .rs_influence_data()))
  ), collapse = "\n")
  expect_match(out, "Residual dependence structure")
  expect_match(out, "Residual network autocorrelation")
  # Reported as a quantity with the absence of calibration stated, never as a
  # verdict.
  expect_match(out, "no reference distribution")
  expect_false(grepl("p-value = ", out, fixed = TRUE))

  # And summary() does NOT carry it, which is the seam being kept rather than
  # quietly re-crossed.
  s_out <- paste(utils::capture.output(print(summary(.rs_influence()))),
                 collapse = "\n")
  expect_false(grepl("Residual dependence structure", s_out, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# The tidy()/glance() bridge

test_that("tidy() and glance() return exactly what the netis_ accessors do", {
  skip_if_not_installed("amen")
  for (fit in list(.rs_influence(), .rs_selection())) {
    # Identical, not merely similar. They are forwarders, and the whole reason
    # to keep them thin is that two spellings must not become two answers.
    expect_identical(tidy(fit), netis_estimates(fit))
    expect_identical(glance(fit), netis_glance(fit))
  }
})

test_that("tidy() passes its arguments through", {
  fit <- .rs_influence()
  expect_identical(tidy(fit, level = 0.8), netis_estimates(fit, level = 0.8))
})

test_that("the tidy shape uses the convention's column names", {
  # The reason these are dot-separated rather than snake_case: downstream tools
  # index them by name, so this is an interop contract and not a style lapse.
  est <- tidy(.rs_influence())
  expect_true(all(c("term", "estimate", "std.error", "conf.low", "conf.high") %in% names(est)))
  expect_false(any(c("std_error", "conf_low", "conf_high") %in% names(est)))
  expect_true("nobs" %in% names(glance(.rs_influence())))
})

test_that("a Selection fit gets no statistic and no p.value column", {
  # A posterior has neither. The convention tolerates an absent column, and
  # completing the shape for symmetry would manufacture inference the model did
  # not perform.
  skip_if_not_installed("amen")
  est <- tidy(.rs_selection())
  expect_false("p.value" %in% names(est))
  expect_false("statistic" %in% names(est))
})

test_that("the generics are re-exported, so tidy() needs no broom", {
  # `library(rnetis); tidy(fit)` must work on its own -- the point of the bridge
  # is the user who types tidy() out of habit and should not have to know which
  # package owns the verb.
  expect_true("tidy" %in% getNamespaceExports("rnetis"))
  expect_true("glance" %in% getNamespaceExports("rnetis"))
  # And it is the SAME generic object generics owns, which is why attaching
  # broom alongside is a cosmetic masking message rather than a conflict.
  expect_identical(rnetis::tidy, generics::tidy)
  expect_identical(rnetis::glance, generics::glance)
})
