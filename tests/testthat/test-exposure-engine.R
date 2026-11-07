# Known-answer tests for the exposure engine. Every expected value below is
# worked out by hand in the test comment, not derived from the code under test.
#
# The two "direction" tests are the highest-priority tests in this file: a
# deliberately asymmetric network where getting the transposition backwards
# changes the answer.

.exposure_fixture <- function() {
  nodes <- data.frame(
    group = 1, node = c("a", "b", "c"),
    negative_affect = c(0.2, -0.5, 1.1)
  )
  ties <- data.frame(
    group = 1,
    source = c("a", "b"), target = c("b", "c"),
    info_receipt = c(1, 0.5)
  )
  netis_data(nodes, ties)
}

# ---- direction: the highest-priority tests in this file ---------------------

test_that("source_influences_target computes the hand-verified exposure values", {
  # a -> b (value 1): a influences b.  b -> c (value 0.5): b influences c.
  # W[b,a]=1, W[c,b]=0.5, everything else 0. X = (a=0.2, b=-0.5, c=1.1).
  # E_a = 0 (no incoming influence)
  # E_b = W[b,a]*X_a = 1 * 0.2 = 0.2
  # E_c = W[c,b]*X_b = 0.5 * -0.5 = -0.25
  nd <- .exposure_fixture()
  out <- compute_exposure(nd, "negative_affect", "info_receipt")
  out <- out[order(out$node), ]
  expect_equal(out$ex_negative_affect_info_receipt, c(0, 0.2, -0.25), tolerance = 1e-12)
})

test_that("target_influences_source gives the OPPOSITE, independently correct answer", {
  # Same raw ties, opposite semantic: a -> b (value 1) now means b influences a.
  # b -> c (value 0.5) now means c influences b.
  # W[a,b]=1, W[b,c]=0.5. X = (a=0.2, b=-0.5, c=1.1).
  # E_a = W[a,b]*X_b = 1 * -0.5 = -0.5
  # E_b = W[b,c]*X_c = 0.5 * 1.1 = 0.55
  # E_c = 0
  nd <- .exposure_fixture()
  out <- compute_exposure(nd, "negative_affect", "info_receipt",
                           direction = "target_influences_source")
  out <- out[order(out$node), ]
  expect_equal(out$ex_negative_affect_info_receipt, c(-0.5, 0.55, 0), tolerance = 1e-12)
})

test_that("the two directions genuinely disagree on this asymmetric network", {
  nd <- .exposure_fixture()
  fwd <- compute_exposure(nd, "negative_affect", "info_receipt")
  rev <- compute_exposure(nd, "negative_affect", "info_receipt",
                           direction = "target_influences_source")
  fwd <- fwd[order(fwd$node), ]
  rev <- rev[order(rev$node), ]
  expect_false(isTRUE(all.equal(
    fwd$ex_negative_affect_info_receipt,
    rev$ex_negative_affect_info_receipt
  )))
})

# ---- normalization and isolates ---------------------------------------------

test_that("row normalization gives the hand-verified average exposure", {
  # Row a: no ties -> isolate. Row b: W[b,a]=1, sum=1, normalized weight 1 -> E_b = X_a = 0.2.
  # Row c: W[c,b]=0.5, sum=0.5, normalized weight 1 -> E_c = X_b = -0.5.
  nd <- .exposure_fixture()
  out <- compute_exposure(nd, "negative_affect", "info_receipt", normalize = "row")
  out <- out[order(out$node), ]
  expect_true(is.na(out$ex_negative_affect_info_receipt[out$node == "a"]))
  expect_equal(out$ex_negative_affect_info_receipt[out$node == "b"], 0.2, tolerance = 1e-12)
  expect_equal(out$ex_negative_affect_info_receipt[out$node == "c"], -0.5, tolerance = 1e-12)
})

test_that("isolate_rule = 'zero' gives 0 instead of NA for isolates under row normalization", {
  nd <- .exposure_fixture()
  out <- compute_exposure(nd, "negative_affect", "info_receipt",
                           normalize = "row", isolate_rule = "zero")
  expect_equal(out$ex_negative_affect_info_receipt[out$node == "a"], 0)
})

test_that("isolate_rule = 'error' refuses to compute when isolates are present", {
  nd <- .exposure_fixture()
  expect_error(
    compute_exposure(nd, "negative_affect", "info_receipt",
                      normalize = "row", isolate_rule = "error"),
    "isolate"
  )
})

test_that("normalize = 'none' gives isolates exactly 0 with no ambiguity", {
  nd <- .exposure_fixture()
  out <- compute_exposure(nd, "negative_affect", "info_receipt")
  expect_equal(out$ex_negative_affect_info_receipt[out$node == "a"], 0)
})

test_that("row normalization with a node having multiple alters at UNEQUAL weights is the true weighted average", {
  # d is influenced by both a (weight 2) and b (weight 1) -- a denominator
  # of 3, not the trivial 1 every other row-normalization test in this file
  # exercises. A transposed division (dividing by the wrong axis's sum)
  # would give a different, wrong answer here but pass every other test.
  nodes <- data.frame(group = 1, node = c("a", "b", "d"), x = c(3, 6, 0))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("d", "d"), w = c(2, 1))
  nd <- netis_data(nodes, ties, self_ties = "prohibited")
  out <- compute_exposure(nd, "x", "w", normalize = "row")
  # E_d = (2/3)*3 + (1/3)*6 = 2 + 2 = 4 (equivalently: raw 2*3+1*6=12, / row sum 3 = 4)
  expect_equal(out$ex_x_w[out$node == "d"], 4, tolerance = 1e-12)
})

test_that("a node with literally no ties in the whole group is 0 under normalize = 'none'", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), trait = c(1, 2, 3))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)  # c has no tie at all
  nd <- netis_data(nodes, ties)
  out <- compute_exposure(nd, "trait", "w")
  expect_equal(out$ex_trait_w[out$node == "c"], 0)
})

# ---- multi-group independence -----------------------------------------------

test_that("exposure is computed independently per group with no cross-group leakage", {
  # Groups share node ids "a","b" but have opposite tie directions and
  # different content values -- if group boundaries leaked, these would
  # contaminate each other.
  nodes <- data.frame(
    group = c(1, 1, 2, 2),
    node = c("a", "b", "a", "b"),
    x = c(1, 10, 100, 5)
  )
  ties <- data.frame(
    group = c(1, 2),
    source = c("a", "b"),
    target = c("b", "a"),
    w = c(2, 3)
  )
  nd <- netis_data(nodes, ties)
  out <- compute_exposure(nd, "x", "w")

  # Group 1: a->b (value 2) => W[b,a]=2, X_a=1 => E_b = 2*1 = 2, E_a = 0.
  g1 <- out[out$group == 1, ]
  expect_equal(g1$ex_x_w[g1$node == "b"], 2)
  expect_equal(g1$ex_x_w[g1$node == "a"], 0)

  # Group 2: b->a (value 3) => W[a,b]=3, X_b=5 => E_a = 3*5 = 15, E_b = 0.
  g2 <- out[out$group == 2, ]
  expect_equal(g2$ex_x_w[g2$node == "a"], 15)
  expect_equal(g2$ex_x_w[g2$node == "b"], 0)
})

# ---- augment ----------------------------------------------------------------

test_that("augment = TRUE returns the original node data with the exposure column added", {
  nd <- .exposure_fixture()
  out <- compute_exposure(nd, "negative_affect", "info_receipt", augment = TRUE)
  expect_setequal(names(out), c("group", "node", "negative_affect", "ex_negative_affect_info_receipt"))
  expect_equal(nrow(out), 3L)
  out <- out[order(out$node), ]
  expect_equal(out$ex_negative_affect_info_receipt, c(0, 0.2, -0.25), tolerance = 1e-12)
})

test_that("a custom exposure column name is honored", {
  nd <- .exposure_fixture()
  out <- compute_exposure(nd, "negative_affect", "info_receipt", name = "my_exposure")
  expect_true("my_exposure" %in% names(out))
})

# ---- wave handling ----------------------------------------------------------

test_that("a wave-containing dataset requires an explicit wave argument", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2), x = c(1, 2, 3, 4))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2), w = c(1, 1))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(compute_exposure(nd, "x", "w"), "wave")
})

test_that("supplying wave for a dataset with no wave dimension errors", {
  nd <- .exposure_fixture()
  expect_error(compute_exposure(nd, "negative_affect", "info_receipt", wave = 1), "no wave dimension")
})

test_that("an unobserved wave value errors", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2), x = c(1, 2, 3, 4))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2), w = c(1, 1))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(compute_exposure(nd, "x", "w", wave = 99), "not one of the observed waves")
})

test_that("selecting a specific wave uses only that wave's data", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2), x = c(1, 2, 100, 200))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = c(1, 2), w = c(1, 1))
  nd <- netis_data(nodes, ties, wave_col = "wave")

  out1 <- compute_exposure(nd, "x", "w", wave = 1)
  out2 <- compute_exposure(nd, "x", "w", wave = 2)
  # wave 1: X_a = 1 -> E_b = 1. wave 2: X_a = 100 -> E_b = 100.
  expect_equal(out1$ex_x_w[out1$node == "b"], 1)
  expect_equal(out2$ex_x_w[out2$node == "b"], 100)
})

# ---- custom kernels ---------------------------------------------------------

test_that("a custom kernel function is used through the same engine, defaulting to requires = 'focal'", {
  # Ignores X entirely; just returns each node's total incoming tie weight.
  my_kernel <- function(context, spec) rowSums(context$W)
  nd <- .exposure_fixture()
  out <- compute_exposure(nd, "negative_affect", "info_receipt", mechanism = my_kernel)
  out <- out[order(out$node), ]
  # W[b,a]=1, W[c,b]=0.5 (default direction) -> row sums: a=0, b=1, c=0.5.
  expect_equal(out$ex_negative_affect_info_receipt, c(0, 1, 0.5), tolerance = 1e-12)
})

test_that("a kernel requiring a context component outside the vocabulary errors clearly", {
  # The vocabulary is focal, w_history and x_history, all materialized.
  my_kernel <- function(context, spec) rep(0, length(context$X))
  attr(my_kernel, "requires") <- c("focal", "provenance")
  nd <- .exposure_fixture()
  expect_error(
    compute_exposure(nd, "negative_affect", "info_receipt", mechanism = my_kernel),
    "Unknown requires component(s): provenance. The requires vocabulary is closed: focal, w_history, x_history.",
    fixed = TRUE
  )
})

test_that("a kernel requiring w_history/x_history with no prior waves available gets empty history, not an error", {
  # Single-wave data: there is nothing to materialize, and that is a valid,
  # unsurprising state (the mechanism decides what "no history" means), not
  # a failure.
  my_kernel <- function(context, spec) {
    expect_length(context$W_history, 0L)
    expect_length(context$X_history, 0L)
    rep(0, length(context$X))
  }
  attr(my_kernel, "requires") <- c("focal", "w_history", "x_history")
  nd <- .exposure_fixture()
  out <- compute_exposure(nd, "negative_affect", "info_receipt", mechanism = my_kernel)
  expect_equal(out$ex_negative_affect_info_receipt, c(0, 0, 0))
})

test_that("mechanism must be \"linear\" or a function", {
  nd <- .exposure_fixture()
  expect_error(compute_exposure(nd, "negative_affect", "info_receipt", mechanism = "bogus"), "linear")
})

# ---- spec provenance (internal, but a governing requirement -- the
# specification-provenance requirement) --

test_that("a named custom kernel is recorded as portable with its name", {
  my_named_kernel <- function(context, spec) context$X
  spec <- .new_exposure_spec(content = "x", conduit = "w", mechanism = my_named_kernel)
  expect_true(spec$portable)
  expect_equal(spec$mechanism$provenance$type, "named_function")
  expect_equal(spec$mechanism$provenance$name, "my_named_kernel")
})

test_that("an anonymous custom kernel is recorded as not portable", {
  spec <- .new_exposure_spec(content = "x", conduit = "w",
                              mechanism = function(context, spec) context$X)
  expect_false(spec$portable)
  expect_equal(spec$mechanism$provenance$type, "anonymous")
})

test_that("an unknown requires component is rejected", {
  bad_kernel <- function(context, spec) context$X
  attr(bad_kernel, "requires") <- "not_a_real_component"
  expect_error(
    .new_exposure_spec(content = "x", conduit = "w", mechanism = bad_kernel),
    "Unknown requires"
  )
})

test_that("a nonexistent conduit column errors even when the affected group has zero ties", {
  # Regression test: the column-existence check must not be skippable by
  # constructing a group with no tie rows -- otherwise a typo'd `via` silently
  # produces all-zero exposure instead of an error, and whether it's caught
  # would depend on incidental row counts rather than on argument validity.
  nodes <- data.frame(group = 1, node = c("a", "b"), x = c(1, 2))
  ties <- data.frame(group = numeric(0), source = character(0), target = character(0), w = numeric(0))
  nd <- netis_data(nodes, ties)
  expect_error(compute_exposure(nd, "x", "this_column_does_not_exist"), "missing required column")
})

# ---- referential integrity does not crash the engine ------------------------

test_that("a tie referencing a node absent from this group is refused, not dropped", {
  # Dropping the tie would change a's exposure with nothing reported; the
  # engine refuses and names the missing node instead.
  nodes <- data.frame(group = 1, node = c("a", "b"), x = c(1, 2))
  ties <- data.frame(group = 1, source = "a", target = "ghost", w = 1)
  nd <- netis_data(nodes, ties, self_ties = "permitted")
  expect_error(compute_exposure(nd, "x", "w"), "ghost")
  expect_error(compute_centrality(nd, network = w, measure = "degree"), "ghost")
  expect_error(netis_adjacency(nd, "w"), "ghost")
})

# ---------------------------------------------------------------------------
# Missing content values: NA must propagate along ties, and only
# along ties.
# ---------------------------------------------------------------------------

test_that("a missing alter value contaminates only the nodes tied to it", {
  # Known-answer. Chain a -> b -> c under source_influences_target, so
  # W[b,a] and W[c,b] are the only nonzero entries. With x_b missing:
  #   a has no alters      -> exposure 0
  #   b's alter is a       -> exposure x_a = 2, unaffected
  #   c's alter is b       -> exposure NA
  #
  # The plain matrix product W %*% X gives NA for all three, because
  # `0 * NA` is NA in R, so a structural zero contributes NA rather than
  # nothing. One missing value would wipe out the whole column.
  nodes <- data.frame(
    group = 1, node = c("a", "b", "c"), x = c(2, NA, 5),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = 1, source = c("a", "b"), target = c("b", "c"), w = c(1, 1),
    stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties)

  ex <- compute_exposure(nd, content = "x", via = "w")
  ex <- ex[match(c("a", "b", "c"), ex$node), ]
  expect_equal(ex$ex_x_w, c(0, 2, NA_real_))
})

test_that("row-normalized exposure does not renormalize over the observed alters", {
  # b has two alters, a (observed) and c (missing). Averaging over just a
  # would be imputation and would silently change the estimand, so the
  # answer is NA, not 4.
  nodes <- data.frame(
    group = 1, node = c("a", "b", "c"), x = c(4, 0, NA),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = 1, source = c("a", "c"), target = c("b", "b"), w = c(1, 1),
    stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties)

  ex <- compute_exposure(nd, content = "x", via = "w", normalize = "row",
                         isolate_rule = "zero")
  expect_true(is.na(ex$ex_x_w[ex$node == "b"]))
})

test_that("no missing values leaves exposure exactly as before", {
  nodes <- data.frame(
    group = 1, node = c("a", "b", "c"), x = c(2, 3, 5),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = 1, source = c("a", "b"), target = c("b", "c"), w = c(1, 1),
    stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties)
  ex <- compute_exposure(nd, content = "x", via = "w")
  ex <- ex[match(c("a", "b", "c"), ex$node), ]
  expect_equal(ex$ex_x_w, c(0, 2, 3))
})

test_that("a missing tie WEIGHT makes only the receiving node's exposure undefined", {
  # Distinct from a missing content value (tested above): here the alter's
  # value is known but the strength of one tie is not. Chain a -> b -> c under
  # source_influences_target: w(a->b) is NA, w(b->c) = 1.
  #   a has no inbound ties          -> exposure 0
  #   b's one inbound weight is NA   -> exposure NA (undefined contribution)
  #   c's inbound tie is known       -> exposure x_b = 3, unaffected
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), x = c(2, 3, 5))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"),
                     w = c(NA, 1))
  nd <- netis_data(nodes, ties)
  ex <- compute_exposure(nd, content = "x", via = "w")
  ex <- ex[match(c("a", "b", "c"), ex$node), ]
  expect_equal(ex$ex_x_w, c(0, NA_real_, 3))
})

# ---- separately lagged content and conduit ( follow-up) --------------

# A DYNAMIC network whose ties differ by wave, and content values that differ
# by wave, so a wave swap on EITHER side changes the answer. Hand-worked:
#   wave 1 ties: a -> b (1)   =>  W1[b,a] = 1
#   wave 2 ties: b -> c (1)   =>  W2[c,b] = 1
#   x at wave 1: (a=1, b=2, c=4);  x at wave 2: (a=10, b=20, c=40)
.conduit_wave_fixture <- function() {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c"), 2), wave = rep(1:2, each = 3),
    x = c(1, 2, 4, 10, 20, 40)
  )
  ties <- data.frame(
    group = 1, wave = c(1, 2),
    source = c("a", "b"), target = c("b", "c"), tie = c(1, 1)
  )
  netis_data(nodes, ties, wave_col = "wave")
}

test_that("known answer: content at wave 2, network at wave 1", {
  # W from wave 1 (a influences b), X from wave 2:
  #   E_b = W1[b,a] * x2_a = 1 * 10 = 10;  E_a = E_c = 0.
  nd <- .conduit_wave_fixture()
  out <- compute_exposure(nd, "x", "tie", wave = 2, conduit_wave = 1)
  out <- out[order(out$node), ]
  expect_equal(out$ex_x_tie, c(0, 10, 0), tolerance = 1e-12)
})

test_that("known answer: content at wave 1, network at wave 2 — the mirror answer", {
  # W from wave 2 (b influences c), X from wave 1:
  #   E_c = W2[c,b] * x1_b = 1 * 2 = 2;  E_a = E_b = 0.
  # Distinct from both same-wave answers, so a swap in either direction fails.
  nd <- .conduit_wave_fixture()
  out <- compute_exposure(nd, "x", "tie", wave = 1, conduit_wave = 2)
  out <- out[order(out$node), ]
  expect_equal(out$ex_x_tie, c(0, 0, 2), tolerance = 1e-12)
})

test_that("conduit_wave = NULL is the pre-existing same-wave behavior, exactly", {
  nd <- .conduit_wave_fixture()
  same <- compute_exposure(nd, "x", "tie", wave = 2)
  # Same-wave at 2: W2 (b influences c), X at 2: E_c = 20.
  same <- same[order(same$node), ]
  expect_equal(same$ex_x_tie, c(0, 0, 20), tolerance = 1e-12)
  expect_identical(same, {
    exp2 <- compute_exposure(nd, "x", "tie", wave = 2, conduit_wave = 2)
    exp2[order(exp2$node), ]
  })
})

test_that("conduit_wave is validated like wave, and refused for history mechanisms", {
  nd <- .conduit_wave_fixture()
  expect_error(compute_exposure(nd, "x", "tie", wave = 2, conduit_wave = 9),
               "not one of the observed waves")
  # A path-dependent mechanism decides for itself which historical networks
  # enter the accumulation; "the conduit, but shifted" has no defined meaning
  # inside it.
  expect_error(
    compute_exposure(nd, "x", "tie", wave = 2, conduit_wave = 1,
                     mechanism = bi(delta = 0, alpha = 1, gamma = 0)),
    "no defined")
})

test_that("a network absent at the CONDUIT wave is named at that wave, not the content wave", {
  # Ties at waves 1 and 3 with DIFFERENT values keep the network demonstrably
  # dynamic -- identical rows would trip the value-driven static collapse
  # (a separate behavior, which hides wave-absence and is not what this test
  # is about). Reading the conduit at the tie-less
  # wave 2 is then an unobserved network: NA for every node, with the warning
  # naming wave 2 -- the wave the NETWORK was read at, not the content's
  # wave 1, which exists and would misdirect the user.
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b"), 3), wave = rep(1:3, each = 2),
    x = c(1, 2, 10, 20, 100, 200)
  )
  ties <- data.frame(group = 1, wave = c(1, 3), source = "a", target = "b",
                     tie = c(1, 2))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_warning(
    out <- compute_exposure(nd, "x", "tie", wave = 1, conduit_wave = 2),
    "wave 2")
  expect_true(all(is.na(out$ex_x_tie)))
})

# ---- A missing TIE WEIGHT, as against a missing content value ---------------
#
# A masking rule written only for a missing alter VALUE can fail on a missing
# alter WEIGHT in two different ways, and these tests guard against both:
#
#   * under normalize = "row", `rowSums(W)` is NA for the affected row, so
#     `row_sums == 0` is NA and `if (any(zero_rows))` fails with "missing
#     value where TRUE/FALSE needed" -- but only when no genuine isolate is
#     present, because an isolate makes `any()` TRUE first, so fixtures that
#     mostly have isolates cannot see it.
#   * with a missing value AND a missing weight, the contamination index itself
#     goes NA and the assignment fails with "NAs are not allowed in
#     subscripted assignments".
#
# Both are the same fact: an unknown weight makes that node's exposure
# undefined. It is NOT an isolate (that node has alters; their weights are
# partly unknown) and the denominator is not renormalized over the observed
# alters, which would be imputation and would change the estimand.

.exposure_na_weight_fixture <- function() {
  # A complete triangle so that NO node is an isolate -- the configuration the
  # crash needed, and the one no other fixture in this file supplies.
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), x = c(1, 2, 4))
  ties <- data.frame(
    group = 1,
    source = c("a", "b", "c", "a", "b", "c"),
    target = c("b", "c", "a", "c", "a", "b"),
    w = c(1, 1, NA, 1, 1, 1)
  )
  netis_data(nodes, ties)
}

test_that("a missing tie weight leaves that node's exposure undefined, not an error", {
  nd <- .exposure_na_weight_fixture()

  out <- compute_exposure(nd, "x", "w", normalize = "row")
  out <- out[order(out$node), ]
  vals <- out$ex_x_w

  # c -> a carries the unknown weight, so under the default direction
  # (source influences target) it is a's row that is undefined.
  expect_true(is.na(vals[[1]]))
  # b and c keep defined exposures: their own incoming weights are all known.
  # E_b = (1*x_a + 1*x_c) / 2 = (1 + 4) / 2 = 2.5
  # E_c = (1*x_b + 1*x_a) / 2 = (2 + 1) / 2 = 1.5
  expect_equal(vals[[2]], 2.5, tolerance = 1e-12)
  expect_equal(vals[[3]], 1.5, tolerance = 1e-12)
})

test_that("an unnormalized exposure masks the same row, by the same rule", {
  nd <- .exposure_na_weight_fixture()
  out <- compute_exposure(nd, "x", "w", normalize = "none")
  out <- out[order(out$node), ]
  # E_b = 1*x_a + 1*x_c = 1 + 4 = 5 ; E_c = 1*x_b + 1*x_a = 2 + 1 = 3
  expect_true(is.na(out$ex_x_w[[1]]))
  expect_equal(out$ex_x_w[[2]], 5, tolerance = 1e-12)
  expect_equal(out$ex_x_w[[3]], 3, tolerance = 1e-12)
})

test_that("a missing weight and a missing value together mask, rather than erroring", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), x = c(1, NA, 4))
  ties <- data.frame(
    group = 1,
    source = c("a", "b", "c", "a", "b", "c"),
    target = c("b", "c", "a", "c", "a", "b"),
    w = c(1, 1, NA, 1, 1, 1)
  )
  nd <- netis_data(nodes, ties)

  out <- compute_exposure(nd, "x", "w", normalize = "none")
  out <- out[order(out$node), ]
  # a: unknown weight from c            -> NA
  # b: tied to a (known) and c (known), but x_c is known and x_a is known,
  #    so E_b = 1*1 + 1*4 = 5
  # c: tied to b, whose x is missing    -> NA
  expect_true(is.na(out$ex_x_w[[1]]))
  expect_equal(out$ex_x_w[[2]], 5, tolerance = 1e-12)
  expect_true(is.na(out$ex_x_w[[3]]))
})

test_that("complete inputs are untouched by the masking path", {
  # The known answers at the top of this file go through .mask_matvec() now
  # whenever anything is missing; with nothing missing the fast path runs and
  # the two must agree exactly, or the change moved a validated number.
  nd <- .exposure_fixture()
  direct <- compute_exposure(nd, "negative_affect", "info_receipt")
  W <- matrix(c(0, 0, 0, 1, 0, 0, 0, 0.5, 0), nrow = 3, byrow = TRUE)
  x <- c(0.2, -0.5, 1.1)
  expect_equal(sort(direct$ex_negative_affect_info_receipt),
               sort(.mask_matvec(W, x)), tolerance = 1e-12)
})
