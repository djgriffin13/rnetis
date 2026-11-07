# Clustered variance and the SE-ratio report (network clustering and the SE comparison).
#
# Level 1: the refusal contracts, reference-equivalence of the sandwich against
# a hand-built one, and the reporting rules. Most of these are about what the
# layer will NOT do -- switch estimators for you, cluster on one cluster, apply
# an OLS sandwich to a GLS estimator, or stack a second correction on a backend
# that already carries one.

# A single-transition fit, which is the shape that reaches the OLS backend:
# a stacked fit repeats units and the repeated-unit invariant then requires a
# random effect, which routes to lme4 -- where an OLS sandwich would be the
# wrong estimator (see the refusal test below).
#
# Built by hand: these tests are about the variance layer's contracts, which a
# hand-built two-wave fixture exercises exactly.
.rv_fit <- function(n_nodes = 24, n_groups = 1L) {
  set.seed(1)
  gs <- sprintf("g%02d", seq_len(n_groups))
  nodes <- do.call(rbind, lapply(gs, function(g) {
    a <- sprintf("%s_n%03d", g, seq_len(n_nodes))
    data.frame(group = g, node = rep(a, 2), wave = rep(1:2, each = n_nodes),
               y = stats::rnorm(2 * n_nodes), stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(gs, function(g) {
    a <- sprintf("%s_n%03d", g, seq_len(n_nodes))
    data.frame(group = g, source = a[-n_nodes], target = a[-1], wave = 1,
               tie = 1, stringsAsFactors = FALSE)
  }))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  suppressWarnings(fit_influence(y ~ ar() + exposure(y, via = tie, normalize = "row"),
                                 data = nd))
}

# A synthetic OLS-backed fit with SEVERAL networks.
#
# Needed because the network-clustered case is currently unreachable through
# fit_influence(): multi-group data requires a random-effects term, which
# routes to lme4, where the OLS sandwich is refused as the wrong estimator.
# Testing the group-clustered arithmetic therefore has to address the sandwich
# directly rather than wait for a route that does not yet exist -- and it must
# be tested, because it is the estimator this file is about.
.rv_synthetic <- function(n_groups = 40, per_group = 5, seed = 2) {
  set.seed(seed)
  n <- n_groups * per_group
  d <- data.frame(
    grp = rep(sprintf("g%02d", seq_len(n_groups)), each = per_group),
    nd = sprintf("n%03d", seq_len(n)),
    x = rnorm(n)
  )
  # A group-level shock, so residuals genuinely covary within network and the
  # clustered sandwich has something to find.
  d$y <- 0.5 * d$x + rep(rnorm(n_groups), each = per_group) + rnorm(n)
  m <- stats::lm(y ~ x, data = d)
  structure(
    list(backend = "stats", backend_fit = m, vcov = stats::vcov(m),
         model_frame = d,
         spec = list(data_schema = list(group_col = "grp", node_col = "nd"))),
    class = c("netis_influence", "netis_fit")
  )
}

.rv_hand_sandwich <- function(m, cl) {
  X <- stats::model.matrix(m)
  e <- as.numeric(stats::residuals(m))
  bread <- solve(crossprod(X))
  meat <- matrix(0, ncol(X), ncol(X))
  for (k in unique(cl)) {
    i <- which(cl == k)
    meat <- meat + tcrossprod(crossprod(X[i, , drop = FALSE], e[i]))
  }
  bread %*% meat %*% bread
}

test_that("the NETWORK-clustered sandwich matches a hand-built one exactly", {
  # Reference equivalence against the formula written out by hand. This is the
  # only thing that establishes the arithmetic, and it must not be checked only
  # through the wrapper that produced it.
  fit <- .rv_synthetic()
  # cr_type pinned: this is the known-answer test for the DEPENDENCY-FREE CR0
  # arithmetic rnetis computes itself. `auto` prefers CR2 where clubSandwich is
  # installed, which is a different (better) estimator and a different number.
  V <- .influence_vcov(fit, "cluster", by = "group", cr_type = "CR0")
  expected <- .rv_hand_sandwich(fit$backend_fit, fit$model_frame$grp)

  expect_equal(matrix(as.numeric(V), nrow(V)),
               matrix(as.numeric(expected), nrow(V)), tolerance = 1e-10)
  expect_identical(attr(V, "netis_cluster_by"), "group")
  expect_equal(attr(V, "netis_n_clusters"), 40L)
  # What it permits is recorded on the object, so nothing downstream has to
  # infer the estimand from the level name.
  expect_match(attr(V, "netis_addresses"), "within each network")
  expect_match(attr(V, "netis_addresses"), "independence between networks")
})

test_that("the actor-clustered sandwich matches a hand-built one exactly", {
  fit <- .rv_synthetic()
  V <- .influence_vcov(fit, "cluster", by = "node", cr_type = "CR0")
  expected <- .rv_hand_sandwich(fit$backend_fit, fit$model_frame$nd)
  expect_equal(matrix(as.numeric(V), nrow(V)),
               matrix(as.numeric(expected), nrow(V)), tolerance = 1e-10)
  expect_identical(attr(V, "netis_cluster_by"), "node")
})

test_that("node clustering is never labeled network-robust", {
  # THE ESTIMAND GUARD. Clustering on actors assumes actors are independent
  # clusters, which is exactly what residual network dependence violates. If
  # this ever starts describing itself as handling network dependence, the two
  # corrections have been conflated and the label is making a false claim.
  V <- .influence_vcov(.rv_synthetic(), "cluster", by = "node")
  addresses <- attr(V, "netis_addresses")
  expect_match(addresses, "NOT network-robust")
  expect_match(addresses, "singletons")
  expect_false(grepl("within each network", addresses, fixed = TRUE))
})

test_that("a small number of networks is flagged rather than reported bare", {
  # CR0's outer variance comes from variation across clusters, so with few of
  # them it understates uncertainty. The count and the flag travel with the
  # matrix; a ratio without its cluster count is not interpretable.
  few <- .influence_vcov(.rv_synthetic(n_groups = 6, per_group = 8), "cluster", by = "group")
  expect_true(attr(few, "netis_small_g"))
  expect_equal(attr(few, "netis_n_clusters"), 6L)

  many <- .influence_vcov(.rv_synthetic(n_groups = 40), "cluster", by = "group")
  expect_false(attr(many, "netis_small_g"))
})

test_that("vcov() defaults to the model variance and never switches on its own", {
  fit <- .rv_fit()
  expect_identical(vcov(fit), fit$vcov)
  # The robust matrix must differ, or the equivalence test above proves
  # nothing -- and it is reachable only by asking for it.
  expect_false(isTRUE(all.equal(
    unname(vcov(fit)),
    unname(as.matrix(.influence_vcov(fit, "cluster", by = "node")))
  )))
})

test_that("the OLS sandwich is never applied to a mixed fit", {
  # THE CORRECTNESS RULE, now enforced by ROUTING rather than by refusal.
  # lme4 solves X'V^-1 (y - Xb) = 0, so its sandwich needs bread
  # (X'V^-1 X)^-1; the ordinary (X'X)^-1 form would report the variance of an
  # estimator that did not produce those coefficients. The mixed route
  # therefore goes through clubSandwich and must never resolve to the
  # dependency-free CR0 path, which is OLS-only.
  expect_identical(.resolve_cr_type("auto", "stats", 40L),
                   if (requireNamespace("clubSandwich", quietly = TRUE)) "CR2" else "CR0")
  if (requireNamespace("clubSandwich", quietly = TRUE)) {
    expect_identical(.resolve_cr_type("auto", "lme4", 40L), "CR2")
  } else {
    # Without the package there is no correct mixed route at all, and the
    # refusal says why rather than silently downgrading to the wrong bread.
    expect_error(.resolve_cr_type("auto", "lme4", 40L), "clubSandwich")
  }
})

test_that("a backend whose residuals belong to another equation refuses", {
  # `lnam` needs this refusal because its residuals are from the FILTERED
  # equation, (I - rho W) y - X b, so a sandwich built on an ordinary least-squares
  # bread would combine two estimating equations and belong to neither.
  #
  # Keyed on the backend name and fires before any residual is touched, so no
  # real SAR fit is needed here.
  fake_sar <- structure(list(backend = "lnam", vcov = matrix(1)),
                        class = c("netis_influence", "netis_fit"))
  expect_error(.influence_vcov(fake_sar, "cluster"), "filtered equation")
  expect_error(.influence_vcov(fake_sar, "cluster"), "belong to neither")
  # And it must NOT offer a substitute it cannot justify.
  err <- tryCatch(.influence_vcov(fake_sar, "cluster"), error = conditionMessage)
  expect_match(err, "rnetis will not substitute one", fixed = TRUE)
})

test_that("cluster-robust vcov on a single network refuses, and says why", {
  # THE DEFINING REFUSAL. With one network there is no across-cluster
  # variation for the outer variance, and the fallback must NOT be node
  # clustering: that assumes actors are independent, which is precisely the
  # assumption residual network dependence violates. Offering it here would
  # answer a different question while appearing to answer this one.
  fit <- .rv_fit()
  expect_error(.influence_vcov(fit, "cluster", by = "group"),
               "needs more than one group")
  # It says it cannot compute this, rather than quietly returning a
  # clustered number that answers a different question.
  expect_error(.influence_vcov(fit, "cluster", by = "group"),
               "cannot compute this")
  expect_error(.influence_vcov(fit, "cluster", by = "group"),
               "Do NOT substitute by = \"node\"")
})

test_that("a single actor refuses too, with its own reason", {
  one_actor <- .rv_synthetic(n_groups = 1, per_group = 6)
  one_actor$model_frame$nd <- "n001"
  expect_error(.influence_vcov(one_actor, "cluster", by = "node"),
               "needs more than one node")
  # The node-level refusal must not borrow the single-network refusal: it is a
  # different correction with a different remedy.
  expect_error(.influence_vcov(one_actor, "cluster", by = "node"),
               "its own cluster")
})

test_that("unknown types and cluster levels are refused by name", {
  fit <- .rv_fit()
  expect_error(.influence_vcov(fit, "sandwich"), "vcov")
  expect_error(.influence_vcov(fit, "cluster", by = "dyad"), "cluster_by")
  expect_error(vcov(fit, "sandwich"), "arg")
  # "network_hac" is not a supported type, so it is refused by the type
  # assertion like any other name that is not a type.
  expect_error(.influence_vcov(fit, "network_hac"), "must be one of")
})

test_that("the SE comparison carries its cluster provenance", {
  # A ratio without its cluster count and level is not interpretable, so the
  # provenance rides on the result rather than being left to be remembered.
  cmp <- influence_se_comparison(.rv_synthetic(n_groups = 6, per_group = 8),
                                 by = "group")
  expect_identical(attr(cmp, "cluster_by"), "group")
  expect_equal(attr(cmp, "n_clusters"), 6L)
  expect_true(attr(cmp, "small_g"))
  expect_match(attr(cmp, "addresses"), "within each network")
})

test_that("the SE comparison reports a ratio and percent change, both directions allowed", {
  fit <- .rv_synthetic()
  cmp <- influence_se_comparison(fit, by = "group")

  expect_true(all(c("term", "se_model", "se_robust", "ratio", "pct_change") %in%
                    names(cmp)))
  expect_equal(cmp$ratio, cmp$se_robust / cmp$se_model, tolerance = 1e-12)
  expect_equal(cmp$pct_change, 100 * (cmp$ratio - 1), tolerance = 1e-12)
  # A ratio below 1 is a legitimate outcome; "attenuation" would name only one
  # direction, which is why the reported quantity is a ratio.
  expect_true(all(is.finite(cmp$ratio)))
  expect_true(all(cmp$se_model > 0))
})

test_that("comparing model against model gives a ratio of exactly 1", {
  cmp <- influence_se_comparison(.rv_fit(), type = "model")
  expect_equal(cmp$ratio, rep(1, nrow(cmp)), tolerance = 1e-12)
  expect_equal(cmp$pct_change, rep(0, nrow(cmp)), tolerance = 1e-12)
})

test_that("influence_se_comparison() refuses a non-Influence object", {
  expect_error(influence_se_comparison(list()), "netis_influence")
})

test_that("cluster ids follow row NAMES, not positions", {
  # model.matrix() drops incomplete rows. A positional join would misalign
  # every row after the first drop, and every cluster sum would be formed from
  # mismatched pairs while looking perfectly healthy.
  tbl <- data.frame(group = c("a", "a", "b", "b", "c"),
                    node = c("n1", "n2", "n3", "n4", "n5"),
                    stringsAsFactors = FALSE)
  X <- matrix(0, 3, 2, dimnames = list(c("2", "4", "5"), c("i", "x")))
  s <- list(group_col = "group", node_col = "node")

  expect_identical(.cluster_ids_for_rows(tbl, X, s), c("a", "b", "c"))
  expect_identical(.cluster_ids_for_rows(tbl, X, s, "node"), c("n2", "n4", "n5"))

  # Out-of-range names are refused rather than silently recycled.
  bad <- matrix(0, 2, 2, dimnames = list(c("1", "99"), c("i", "x")))
  expect_null(.cluster_ids_for_rows(tbl, bad, s))
})

# ---------------------------------------------------------------------------
# 55.6c -- the mixed-fit route, via clubSandwich
# ---------------------------------------------------------------------------

.rv_mixed <- function(n_groups = 10, n_nodes = 8, seed = 3) {
  # Hand-built for the same reason as .rv_fit(): the simulation layer is not
  # in this package yet, and what these tests need is several networks with a
  # random effect, which this produces directly.
  set.seed(seed)
  gs <- sprintf("g%02d", seq_len(n_groups))
  nodes <- do.call(rbind, lapply(gs, function(g) {
    a <- sprintf("%s_n%02d", g, seq_len(n_nodes))
    data.frame(group = g, node = rep(a, 2), wave = rep(1:2, each = n_nodes),
               y = stats::rnorm(2 * n_nodes), stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(gs, function(g) {
    a <- sprintf("%s_n%02d", g, seq_len(n_nodes))
    data.frame(group = g, source = a[-n_nodes], target = a[-1], wave = 1,
               tie = 1, stringsAsFactors = FALSE)
  }))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  suppressWarnings(fit_influence(
    y ~ ar() + exposure(y, via = tie, normalize = "row") + (1 | group),
    data = nd))
}

test_that("network clustering is reachable on a mixed fit, which is the ordinary rnetis shape", {
  # THE CASE THAT MAKES NETWORK CLUSTERING USEFUL RATHER THAN MERELY CORRECT. Multi-group data
  # requires a random effect, so every multi-network fit is mixed; before this
  # route existed the network-clustering correction could not be reached
  # through fit_influence() at all.
  skip_if_not_installed("clubSandwich")
  fit <- .rv_mixed()
  expect_identical(fit$backend, "lme4")

  V <- .influence_vcov(fit, "cluster", by = "group")
  expect_identical(attr(V, "netis_cluster_by"), "group")
  expect_equal(attr(V, "netis_n_clusters"), 10L)
  expect_true(all(is.finite(diag(V))))
  expect_true(all(diag(V) > 0))
})

test_that("the cluster vector is passed deliberately, never inferred", {
  # clubSandwich can infer a clustering from the model itself, and for an
  # lmerMod that inference is the random-effects grouping factor. rnetis's
  # clustering variable is NOT necessarily that factor -- a fit may carry
  # (1 | node) while the question is dependence within a network -- so letting
  # the inference stand would silently answer a different question.
  skip_if_not_installed("clubSandwich")
  fit <- .rv_mixed()
  tbl <- fit$model_frame
  s <- fit$spec$data_schema
  X <- .cluster_design_matrix(fit)

  by_group <- .influence_vcov(fit, "cluster", by = "group")
  reference <- clubSandwich::vcovCR(
    fit$backend_fit, type = "CR2",
    cluster = .cluster_ids_for_rows(tbl, X, s, s$group_col))
  expect_equal(matrix(as.numeric(by_group), nrow(by_group)),
               matrix(as.numeric(as.matrix(reference)), nrow(by_group)),
               tolerance = 1e-10)

  # And a DIFFERENT cluster vector gives a different matrix -- otherwise the
  # equivalence above would hold no matter which vector was handed over, and
  # the test would prove nothing about deliberate passing.
  # A COARSER clustering: pairs of networks merged. Still nested with the
  # random-effects grouping (so clubSandwich accepts it), but a different
  # partition, so it must give a different matrix.
  gid <- .cluster_ids_for_rows(tbl, X, s, s$group_col)
  coarser <- paste0("pair", match(gid, sort(unique(gid))) %/% 2L)
  other <- clubSandwich::vcovCR(fit$backend_fit, type = "CR2", cluster = coarser)
  expect_false(isTRUE(all.equal(
    matrix(as.numeric(by_group), nrow(by_group)),
    matrix(as.numeric(as.matrix(other)), nrow(by_group)))))
})

test_that("clustering finer than the random-effects grouping is refused in rnetis terms", {
  # A REAL RESTRICTION, not a package quirk: clusters finer than the modeled
  # grouping would split a random effect across clusters, and the correction
  # is undefined there. The message must say that rather than passing through
  # backend text a user cannot act on.
  skip_if_not_installed("clubSandwich")
  fit <- .rv_mixed()          # carries (1 | group)
  expect_error(.influence_vcov(fit, "cluster", by = "node"),
               "finer than the random-effects grouping")
  expect_error(.influence_vcov(fit, "cluster", by = "node"),
               "Cluster at or above")
})

test_that("auto prefers CR2, and the explicit types are honoured", {
  skip_if_not_installed("clubSandwich")
  fit <- .rv_mixed()
  expect_identical(attr(.influence_vcov(fit, "cluster"), "netis_cr_type"), "CR2")
  for (ty in c("CR0", "CR1", "CR2")) {
    V <- .influence_vcov(fit, "cluster", cr_type = ty)
    expect_identical(attr(V, "netis_cr_type"), ty)
  }
  # CR2 is the bias-reduced correction, so it must not coincide with the
  # uncorrected one -- if it did, the correction is not being applied.
  expect_false(isTRUE(all.equal(
    as.numeric(.influence_vcov(fit, "cluster", cr_type = "CR0")),
    as.numeric(.influence_vcov(fit, "cluster", cr_type = "CR2")))))
})

test_that("Satterthwaite df are reported and can fall below the cluster count", {
  # The other half of the few-clusters problem: CR2 repairs the variance
  # estimate, but a normal reference distribution would give back what it
  # bought. With ten networks the effective df is materially smaller.
  skip_if_not_installed("clubSandwich")
  cmp <- influence_se_comparison(.rv_mixed(), by = "group")
  expect_true("df_satterthwaite" %in% names(cmp))
  expect_true(all(is.finite(cmp$df_satterthwaite)))
  expect_true(all(cmp$df_satterthwaite < attr(cmp, "n_clusters")))
  expect_identical(attr(cmp, "cr_type"), "CR2")
})

test_that("the small-cluster flag survives the correction", {
  # CR2 improves the estimator; it does not turn ten networks into many. A
  # flag that switched off once the correction was applied would read as
  # reassurance that nothing had been established.
  skip_if_not_installed("clubSandwich")
  V <- .influence_vcov(.rv_mixed(n_groups = 10), "cluster", by = "group")
  expect_true(attr(V, "netis_small_g"))
  expect_identical(attr(V, "netis_cr_type"), "CR2")
})

test_that("a single-network MIXED fit is unreachable, so the refusal rests on the pooled one", {
  # The single-network refusal cannot be tested on a mixed fit, because the
  # case is unreachable:
  #
  #   * `(1 | group)` on one group is refused upstream -- a random intercept
  #     over a single level has nothing to vary across.
  #   * `(1 | node)` under the two-wave contract gives one observation per
  #     level, which is not identified either.
  #
  # So the defining refusal is exercised on the pooled route, where a
  # single-network fit genuinely lives, and the unreachability is pinned here
  # rather than left as a silently missing case.
  fit <- .rv_fit()
  expect_identical(fit$backend, "stats")
  expect_error(.influence_vcov(fit, "cluster", by = "group"),
               "needs more than one group")

  expect_error(.rv_mixed(n_groups = 1L, n_nodes = 12),
               "requires more than one group")
})

test_that("a missing clubSandwich refuses informatively rather than failing", {
  # The message must name the install, name the fallback that exists, and say
  # WHY a mixed fit has no dependency-free route -- otherwise the refusal
  # reads as arbitrary.
  msg_mixed <- .cr_missing_package_message("CR2", "lme4")
  expect_match(msg_mixed, "install.packages")
  expect_match(msg_mixed, "generalized-least-squares")
  msg_ols <- .cr_missing_package_message("CR2", "stats")
  expect_match(msg_ols, "cr_type = \"CR0\"")

  # CR0 on an OLS fit is the dependency-free floor and must not consult the
  # package at all.
  expect_identical(.resolve_cr_type("CR0", "stats", 10L), "CR0")
})

test_that("an unknown correction type is refused by name", {
  fit <- .rv_fit()
  expect_error(.influence_vcov(fit, "cluster", cr_type = "CR9"), "cr_type")
  expect_error(influence_se_comparison(fit, cr_type = "HC3"), "cr_type")
})


