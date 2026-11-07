# Known-answer tests for the dependency-free half of the network-derived
# feature subsystem: degree, strength, density and reciprocity, the direction
# guardrail, network_stat()'s variation/identification check, and the
# compute_*()/fit_influence() equivalence guarantee.

# ---- fixtures ---------------------------------------------------------------

# a -> b (w=1), a -> c (w=1), b -> a (w=5): a has out-degree 2, in-degree 1;
# b has out-degree 1, in-degree 1; c has out-degree 0, in-degree 1.
.directed_fixture <- function() {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a", "b"), target = c("b", "c", "a"), w = c(1, 1, 5))
  netis_data(nodes, ties, self_ties = "prohibited")
}

# ---- degree/strength: hand-computed, both directions ------------------------

test_that("degree counts ties by declared direction (out/in/all), hand-verified", {
  nd <- .directed_fixture()
  deg_out <- compute_centrality(nd, w, measure = "degree", mode = "out")
  deg_in <- compute_centrality(nd, w, measure = "degree", mode = "in")
  deg_all <- compute_centrality(nd, w, measure = "degree", mode = "all")

  expect_equal(deg_out$cent_degree_w[deg_out$node == "a"], 2) # a->b, a->c
  expect_equal(deg_out$cent_degree_w[deg_out$node == "b"], 1) # b->a
  expect_equal(deg_out$cent_degree_w[deg_out$node == "c"], 0)

  expect_equal(deg_in$cent_degree_w[deg_in$node == "a"], 1) # b->a
  expect_equal(deg_in$cent_degree_w[deg_in$node == "b"], 1) # a->b
  expect_equal(deg_in$cent_degree_w[deg_in$node == "c"], 1) # a->c

  expect_equal(deg_all$cent_degree_w[deg_all$node == "a"], 3)
  expect_equal(deg_all$cent_degree_w[deg_all$node == "b"], 2)
  expect_equal(deg_all$cent_degree_w[deg_all$node == "c"], 1)
})

test_that("strength sums the actual weight values, not tie counts", {
  nd <- .directed_fixture()
  str_out <- compute_centrality(nd, w, measure = "strength", mode = "out")
  expect_equal(str_out$cent_strength_w[str_out$node == "a"], 2) # 1 + 1
  expect_equal(str_out$cent_strength_w[str_out$node == "b"], 5)
})

test_that("degree on an undirected network ignores mode and does not double-count", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  nd <- netis_data(nodes, ties, directed = FALSE)
  deg <- compute_centrality(nd, w, measure = "degree")
  expect_equal(deg$cent_degree_w[deg$node == "a"], 1)
  expect_equal(deg$cent_degree_w[deg$node == "b"], 1)
  expect_equal(deg$cent_degree_w[deg$node == "c"], 0)
})

# ---- the direction guardrail: proven against a fixture where a transposition
# would change the answer (the own discipline, applied here) --------

test_that("mode = 'in'/'out' use the DECLARED direction, never the exposure engine's transposed W", {
  # a declares "a -> b" (a sends to b). Under rnetis's exposure convention
  # W[i,j] = j influences i, this same row would transpose to W[b,a] -- if
  # centrality() naively reused that canonical matrix, "in-degree of a"
  # would come back 1 (wrong) instead of 0 (correct, under the declared
  # semantic that a is the sender).
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 10)
  nd <- netis_data(nodes, ties)

  deg_out <- compute_centrality(nd, w, measure = "degree", mode = "out")
  deg_in <- compute_centrality(nd, w, measure = "degree", mode = "in")

  expect_equal(deg_out$cent_degree_w[deg_out$node == "a"], 1) # a sends
  expect_equal(deg_out$cent_degree_w[deg_out$node == "b"], 0)
  expect_equal(deg_in$cent_degree_w[deg_in$node == "a"], 0)
  expect_equal(deg_in$cent_degree_w[deg_in$node == "b"], 1) # b receives

  # The exposure engine's own canonical W would give the opposite answer for
  # "in" -- confirm the two genuinely disagree, so this test could actually
  # catch a regression that silently swapped them.
  W_canonical <- .build_canonical_W(
    netis_ties(nd), c("a", "b"),
    source_col = "source", target_col = "target", value_col = "w",
    direction = "source_influences_target", directed = TRUE
  )
  expect_false(identical(unname(rowSums(W_canonical)), unname(deg_in$cent_degree_w)))
})

# ---- degree correctly resolves presence, not value != 0 ---------------------

# This block previously asserted the opposite: that an INFERRED row_presence
# network containing a stored zero counted that row as a present tie, giving
# degree 2. It was pinning a guess. rnetis cannot know whether a stored zero is
# an observed zero-strength relationship or an encoding of structural absence,
# and presence feeds degree, density, isolate status and exposure -- so the
# guess propagated into estimates. It now refuses, and the three blocks below
# pin the refusal together with both ways out of it.

test_that("degree refuses when an INFERRED row_presence network stores zeros", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(3, 0))
  nd <- netis_data(nodes, ties)
  expect_error(
    compute_centrality(nd, w, measure = "degree"),
    "will not guess"
  )
})

test_that("declaring zero_meaning = 'measured_zero' resolves it, and the zero row is present", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(3, 0))
  nd <- netis_data(nodes, ties,
                   tie_meta = list(w = list(zero_meaning = "measured_zero")))
  deg <- compute_centrality(nd, w, measure = "degree")
  # An observed zero-strength relationship is a tie: a has two of them.
  expect_equal(deg$cent_degree_w[deg$node == "a"], 2)
})

test_that("declaring the zeros as non-ties resolves it the other way", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(3, 0))
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(
    presence_mode = "value_rule",
    presence_rule = function(value) value != 0
  )))
  deg <- compute_centrality(nd, w, measure = "degree")
  expect_equal(deg$cent_degree_w[deg$node == "a"], 1)
})

test_that("an EXPLICITLY declared row_presence is taken at its word, zeros and all", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(3, 0))
  nd <- netis_data(nodes, ties,
                   tie_meta = list(w = list(presence_mode = "row_presence")))
  deg <- compute_centrality(nd, w, measure = "degree")
  # The refusal is narrow by design: it fires only where rnetis INFERRED the
  # mode. A user who declares it has made the call themselves.
  expect_equal(deg$cent_degree_w[deg$node == "a"], 2)
})

test_that("a declared structural_absence contradicts row_presence and says so", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(3, 0))
  nd <- netis_data(nodes, ties,
                   tie_meta = list(w = list(zero_meaning = "structural_absence")))
  expect_error(
    compute_centrality(nd, w, measure = "degree"),
    "contradicts"
  )
})

test_that("an ordinary sparse edge list with no stored zeros is untouched", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(3, 1))
  nd <- netis_data(nodes, ties)
  deg <- compute_centrality(nd, w, measure = "degree")
  # The case the inference exists to serve must not have become harder to use.
  expect_equal(deg$cent_degree_w[deg$node == "a"], 2)
})

test_that("degree on a complete_dyadic network with undetermined presence refuses", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- expand.grid(source = c("a", "b", "c"), target = c("a", "b", "c"), stringsAsFactors = FALSE)
  ties <- ties[ties$source != ties$target, ]
  ties$group <- 1
  ties$w <- 0
  nd <- netis_data(nodes, ties, self_ties = "prohibited")
  expect_error(compute_centrality(nd, w, measure = "degree"), "undetermined")
})

# ---- density: hand-computed, directed and undirected ------------------------

test_that("density: directed, hand-computed against actual/possible ties", {
  nd <- .directed_fixture() # 3 present directed ties, self_ties prohibited, n=3
  den <- compute_network_stat(nd, w, measure = "density")
  expect_equal(den$net_density_w, 3 / (3 * 2)) # 3 * (3-1) = 6 possible
})

test_that("density: undirected, hand-computed without double-counting", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  nd <- netis_data(nodes, ties, directed = FALSE)
  den <- compute_network_stat(nd, w, measure = "density")
  expect_equal(den$net_density_w, 1 / 3) # 1 of 3 possible unordered pairs
})

# ---- reciprocity: hand-computed, requires directed --------------------------

test_that("reciprocity: hand-computed ratio of mutual to total present ties", {
  # a->b, b->a (mutual), a->c (not mutual): 2 of 3 present ties are mutual.
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b", "a"), target = c("b", "a", "c"), w = c(1, 1, 1))
  nd <- netis_data(nodes, ties, self_ties = "prohibited")
  rec <- compute_network_stat(nd, w, measure = "reciprocity")
  expect_equal(rec$net_reciprocity_w, 2 / 3)
})

test_that("reciprocity requires a directed network", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  nd <- netis_data(nodes, ties, directed = FALSE)
  expect_error(compute_network_stat(nd, w, measure = "reciprocity"), "directed")
})

# ---- weighted/unweighted validation -----------------------------------------

test_that("degree refuses an explicit weighted request (unsupported by the metric)", {
  nd <- .directed_fixture()
  expect_error(compute_centrality(nd, w, measure = "degree", weights = "weighted"), "weighted")
})

test_that("strength refuses an explicit unweighted request (required by the metric)", {
  nd <- .directed_fixture()
  expect_error(compute_centrality(nd, w, measure = "strength", weights = "unweighted"), "weighted")
})

# ---- level mismatches -------------------------------------------------------

test_that("centrality() rejects a group-level measure", {
  expect_error(centrality(w, measure = "density"), "group-level")
})

test_that("network_stat() rejects a node-level measure", {
  expect_error(network_stat(w, measure = "degree"), "node-level")
})

test_that("an unknown measure name errors listing the real candidates", {
  expect_error(compute_centrality(.directed_fixture(), w, measure = "bogus"), "Unknown measure")
})

# ---- inference and ambiguity ------------------------------------------------

test_that("centrality()/compute_centrality() infer the network when there is exactly one", {
  nd <- .directed_fixture()
  deg <- compute_centrality(nd, measure = "degree")
  expect_true("cent_degree" %in% names(deg))
})

test_that("an ambiguous network (multiple tie variables) errors naming the candidates", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", w1 = 1, w2 = 1)
  nd <- netis_data(nodes, ties)
  expect_error(compute_centrality(nd, measure = "degree"), "w1.*w2|w2.*w1")
})

# ---- fit_influence() integration and the shared-engine guarantee ------------

test_that("fit_influence()'s centrality() columns exactly match compute_centrality()", {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c"), 2), wave = rep(1:2, each = 3),
    y = c(1, 2, 3, 2, 4, 5)
  )
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(2, 1))
  nd <- netis_data(nodes, ties, wave_col = "wave")

  fit <- fit_influence(y ~ ar() + centrality(w, measure = "degree"), data = nd)
  standalone <- compute_centrality(nd, w, measure = "degree", wave = 1)
  merged <- merge(fit$model_frame, standalone, by = "node", suffixes = c("_fit", "_standalone"))
  expect_equal(merged$cent_degree_w_fit, merged$cent_degree_w_standalone)
})

test_that("fit_influence()'s network_stat() columns exactly match compute_network_stat() (fanned out)", {
  nodes <- data.frame(
    group = rep(c(1, 2), each = 6), node = rep(rep(c("a", "b", "c"), 2), 2), wave = rep(rep(1:2, each = 3), 2),
    y = c(1, 2, 3, 2, 4, 5, 1, 2, 3, 3, 4, 6)
  )
  ties <- rbind(
    data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(1, 1)),
    data.frame(group = 2, source = "a", target = "b", w = 1)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  fit <- fit_influence(y ~ ar() + network_stat(w, measure = "density") + (1 | group), data = nd)
  standalone <- compute_network_stat(nd, w, measure = "density", wave = 1)
  merged <- merge(fit$model_frame, standalone, by = "group", suffixes = c("_fit", "_standalone"))
  expect_equal(merged$net_density_w_fit, merged$net_density_w_standalone)
})

# ---- network_stat() variation/identification check -----------

test_that("network_stat() on a single group (no variation) is refused, not aliased", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b", "c"), 2), wave = rep(1:2, each = 3), y = c(1, 2, 3, 2, 4, 5))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(2, 1))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(
    fit_influence(y ~ ar() + network_stat(w, measure = "density"), data = nd),
    "no variation"
  )
})

test_that("network_stat() across multiple groups with genuinely differing density fits", {
  nodes <- data.frame(
    group = rep(c(1, 2), each = 6), node = rep(rep(c("a", "b", "c"), 2), 2), wave = rep(rep(1:2, each = 3), 2),
    y = c(1, 2, 3, 2, 4, 5, 1, 2, 3, 3, 4, 6)
  )
  ties <- rbind(
    data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(1, 1)),
    data.frame(group = 2, source = "a", target = "b", w = 1)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")
  fit <- fit_influence(y ~ ar() + network_stat(w, measure = "density") + (1 | group), data = nd)
  expect_s3_class(fit, "netis_influence")
  expect_true("net_density_w" %in% names(coef(fit)))
})

# ---- naming and collisions --------------------------------------------------

test_that("centrality()/network_stat() default names use the network's variable name", {
  spec <- centrality(w, measure = "degree")
  expect_null(spec$name) # filled in by formula-parse.R, not the constructor
  nd <- .directed_fixture()
  deg <- compute_centrality(nd, w, measure = "degree")
  expect_true("cent_degree_w" %in% names(deg))
  den <- compute_network_stat(nd, w, measure = "density")
  expect_true("net_density_w" %in% names(den))
})

test_that("colliding centrality() terms with explicit identical names are rejected", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b", "c"), 2), wave = rep(1:2, each = 3), y = c(1, 2, 3, 2, 4, 5))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(2, 1))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(
    fit_influence(
      y ~ ar() + centrality(w, measure = "degree", name = "x") + centrality(w, measure = "strength", name = "x"),
      data = nd
    ),
    "column name"
  )
})

# ---- additional coverage: self-ties and negative weights --------------------

test_that("density with self_ties = 'permitted' includes the diagonal in both actual and possible counts", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("a", "b"), w = c(1, 1))
  nd <- netis_data(nodes, ties, self_ties = "permitted")
  den <- compute_network_stat(nd, w, measure = "density")
  # actual: a->a and a->b present = 2. possible with self-ties: n*n = 4.
  expect_equal(den$net_density_w, 2 / 4)
})

test_that("reciprocity excludes self-ties from both the numerator and denominator", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = c("a", "a", "b"), target = c("a", "b", "a"), w = c(1, 1, 1))
  nd <- netis_data(nodes, ties, self_ties = "permitted")
  rec <- compute_network_stat(nd, w, measure = "reciprocity")
  # a->a (self, excluded), a->b and b->a (mutual pair). 2 of 2 present ties are mutual.
  expect_equal(rec$net_reciprocity_w, 1)
})

test_that("strength correctly sums a negative weight, not its absolute value", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("b", "c"), w = c(3, -5))
  nd <- netis_data(nodes, ties)
  str <- compute_centrality(nd, w, measure = "strength", mode = "out")
  expect_equal(str$cent_strength_w[str$node == "a"], -2) # 3 + (-5)
})
