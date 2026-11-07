# Known-answer tests for the Frank-Fahrbach B-I mechanism (bi()).
#
# The primary test below is deliberately constructed with genuine path
# branching (two distinct 2-step paths reaching the same node), because a
# single-path network cannot distinguish all-paths (Q) accumulation from
# maximum-path (MP) accumulation -- Q and MP are mathematically identical
# whenever there is only one path. Every expected value is derived by hand in
# the comments, independent of the implementation under test.

test_that("delta interpolates linearly between max-path and all-path totals on a network with genuine parallel paths", {
  # Two 2-step paths carry the wave-1 shock at 'a' to 'b': a->c->b and a->d->b,
  # each with weight 1*1 = 1. This is exactly the "parallel effect" redundancy
  # Frank & Fahrbach (1999) describe in their Figure 3 discussion.
  #
  # direction = "source_influences_target" (default): row (source=A,target=B)
  # means A influences B, i.e. W[B,A] = value.
  #
  # Ties at wave 1 (drives the 1->2 transition): a->c (1), a->d (1)
  #   => W_wave1[c,a] = 1, W_wave1[d,a] = 1
  # Ties at wave 2 (drives the 2->3 transition): c->b (1), d->b (1)
  #   => W_wave2[b,c] = 1, W_wave2[b,d] = 1
  #
  # For focal wave 3, history is ordered most-recent-first:
  #   K[[1]] = W_wave2 (the 2->3 network), K[[2]] = W_wave1 (the 1->2 network)
  #   e[[1]] = shock at wave 2 = all 0 (isolates the v=2 step's contribution)
  #   e[[2]] = shock at wave 1 = (a=10, b=0, c=0, d=0)
  #
  # v=1: Q=MP=K[[1]]; e[[1]]=0 everywhere -> contributes exactly 0.
  # v=2: Q  = K[[1]] %*% K[[2]]  -> Q[b,a]  = W2[b,c]*W1[c,a] + W2[b,d]*W1[d,a] = 1*1 + 1*1 = 2
  #      MP = maxtimes(K[[1]], K[[2]]) -> MP[b,a] = max(1*1, 1*1) = 1
  #      contribution at b = ((1-delta)*MP[b,a] + delta*Q[b,a]) * e[[2]][a]
  #                        = ((1-delta)*1 + delta*2) * 10 = (1 + delta) * 10
  # Total[b] = 0 + (1 + delta) * 10 = 10 + 10*delta. All other nodes: exactly 0.

  nodes <- data.frame(
    group = 1,
    node = rep(c("a", "b", "c", "d"), 3),
    wave = rep(1:3, each = 4),
    shock = c(10, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0)
  )
  ties <- data.frame(
    group = 1,
    source = c("a", "a", "c", "d"),
    target = c("c", "d", "b", "b"),
    wave = c(1, 1, 2, 2),
    link = c(1, 1, 1, 1)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  get_b <- function(delta) {
    out <- compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = delta, alpha = 1, gamma = 0))
    out$ex_shock_link[out$node == "b"]
  }

  expect_equal(get_b(0), 10, tolerance = 1e-12)    # pure informational: single non-redundant unit
  expect_equal(get_b(0.5), 15, tolerance = 1e-12)  # linear interpolation
  expect_equal(get_b(1), 20, tolerance = 1e-12)    # pure normative: full redundant double-count

  # And every other node gets exactly 0 in every case.
  out0 <- compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 0, alpha = 1, gamma = 0))
  expect_equal(out0$ex_shock_link[out0$node != "b"], c(0, 0, 0))
})

test_that("bi_components(alpha = 1, gamma = 0) gives A=10, B=10 on the parallel-path network, matching bi()'s endpoints exactly", {
  # Same fixture and hand derivation as the delta-interpolation test above:
  # A (pure max-path) = bi(delta=0) = 10 at node b; C (pure all-path) =
  # bi(delta=1) = 20 at node b; B_redundant = C - A = 10.
  nodes <- data.frame(
    group = 1,
    node = rep(c("a", "b", "c", "d"), 3),
    wave = rep(1:3, each = 4),
    shock = c(10, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0)
  )
  ties <- data.frame(
    group = 1,
    source = c("a", "a", "c", "d"),
    target = c("c", "d", "b", "b"),
    wave = c(1, 1, 2, 2),
    link = c(1, 1, 1, 1)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  out <- compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi_components(alpha = 1, gamma = 0))
  expect_setequal(names(out), c("group", "node", "A_maxpath", "B_redundant"))

  b_row <- out[out$node == "b", ]
  expect_equal(b_row$A_maxpath, 10, tolerance = 1e-12)
  expect_equal(b_row$B_redundant, 10, tolerance = 1e-12)

  # Cross-check against bi()'s own endpoints -- must be exactly consistent,
  # not just individually plausible, since both call the same accumulator.
  a0 <- compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 0, alpha = 1, gamma = 0))
  a1 <- compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 1, alpha = 1, gamma = 0))
  out <- out[order(out$node), ]; a0 <- a0[order(a0$node), ]; a1 <- a1[order(a1$node), ]
  expect_equal(out$A_maxpath, a0$ex_shock_link, tolerance = 1e-12)
  expect_equal(out$A_maxpath + out$B_redundant, a1$ex_shock_link, tolerance = 1e-12)
})

test_that("bi_components(alpha = 1, gamma = 0) output augments correctly with two columns at once", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2), x = c(3, 7, 5, 5))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1, w = 2)
  nd <- netis_data(nodes, ties, wave_col = "wave")

  out <- compute_exposure(nd, "x", "w", wave = 2, mechanism = bi_components(alpha = 1, gamma = 0), augment = TRUE)
  expect_true(all(c("A_maxpath", "B_redundant", "x") %in% names(out)))
})

test_that("with only one prior wave available, bi() reduces exactly to a single-step weighted sum, matching the linear kernel", {
  # With exactly one step, Q_1 = MP_1 = K_1 by definition -- there is only one
  # path, so all-paths and maximum-path coincide regardless of delta. This
  # cross-checks bi() against the independently-tested linear kernel rather
  # than against a second hand derivation.
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2), x = c(3, 7, 99, 99))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1, w = 2)
  nd <- netis_data(nodes, ties, wave_col = "wave")

  linear_snapshot <- compute_exposure(nd, "x", "w", wave = 1, mechanism = "linear")
  bi_one_step <- compute_exposure(nd, "x", "w", wave = 2, mechanism = bi(delta = 0.37, alpha = 1, gamma = 0))

  linear_snapshot <- linear_snapshot[order(linear_snapshot$node), ]
  bi_one_step <- bi_one_step[order(bi_one_step$node), ]
  expect_equal(bi_one_step$ex_x_w, linear_snapshot$ex_x_w, tolerance = 1e-12)
})

test_that("a focal wave with no prior waves available (the first wave) yields exactly 0 for every node", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2), x = c(3, 7, 5, 5))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1, w = 2)
  nd <- netis_data(nodes, ties, wave_col = "wave")

  out <- compute_exposure(nd, "x", "w", wave = 1, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))
  expect_equal(out$ex_x_w, c(0, 0))
})

test_that("delta must be a single non-missing numeric value", {
  expect_error(bi(delta = "a", alpha = 1, gamma = 0), "numeric")
  expect_error(bi(delta = c(0.1, 0.2), alpha = 1, gamma = 0), "single")
  expect_error(bi(delta = NA_real_, alpha = 1, gamma = 0), "non-missing")
})

test_that("row normalization is rejected for bi() -- not yet defined for a path-accumulated mechanism", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2), x = c(3, 7, 5, 5))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1, w = 2)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(
    compute_exposure(nd, "x", "w", wave = 2, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0), normalize = "row"),
    "normalize"
  )
})

test_that("row normalization is rejected for bi_components(alpha = 1, gamma = 0) too, not just bi()", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2), x = c(3, 7, 5, 5))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1, w = 2)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(
    compute_exposure(nd, "x", "w", wave = 2, mechanism = bi_components(alpha = 1, gamma = 0), normalize = "row"),
    "normalize"
  )
})

test_that("a node absent from a historical wave contaminates downstream exposure with NA, not a silent 0", {
  # Same parallel-path fixture as the delta-interpolation test above, but
  # node 'a' has no row at wave 1 (the shock-carrying wave) -- its content
  # value there is unresolvable. That NA must propagate through the
  # matrix-multiplication chain to node 'b' at the focal wave, not silently
  # become 0 (which a mishandled NA -> 0 coercion could produce and which
  # would look like a plausible-but-wrong number rather than a clear signal
  # something upstream is missing).
  nodes <- data.frame(
    group = 1,
    node = c("b", "c", "d", "a", "b", "c", "d", "a", "b", "c", "d"), # 'a' omitted at wave 1
    wave = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
    shock = c(0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0)
  )
  ties <- data.frame(
    group = 1,
    source = c("a", "a", "c", "d"),
    target = c("c", "d", "b", "b"),
    wave = c(1, 1, 2, 2),
    link = c(1, 1, 1, 1)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  out <- compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))
  expect_true(is.na(out$ex_shock_link[out$node == "b"]))
})

test_that("bi() is recorded as portable, package-provenanced provenance with its delta", {
  spec <- .new_exposure_spec(content = "x", conduit = "w", mechanism = bi(delta = 0.42, alpha = 1, gamma = 0))
  expect_true(spec$portable)
  expect_equal(spec$mechanism$provenance$type, "package")
  expect_equal(spec$mechanism$provenance$name, "bi")
  expect_equal(spec$mechanism$provenance$params$delta, 0.42)
})

test_that(".maxtimes_matrix computes max_a(A[i,a]*B[a,j]) correctly on a hand-checkable case", {
  A <- matrix(c(2, 0, 0, 3), nrow = 2)  # A[1,1]=2, A[2,2]=3 (col-major fill)
  B <- matrix(c(5, 0, 0, 4), nrow = 2)  # B[1,1]=5, B[2,2]=4
  # out[1,1] = max(A[1,1]*B[1,1], A[1,2]*B[2,1]) = max(2*5, 0*0) = 10
  # out[2,2] = max(A[2,1]*B[1,2], A[2,2]*B[2,2]) = max(0*0, 3*4) = 12
  out <- .maxtimes_matrix(A, B)
  expect_equal(out[1, 1], 10)
  expect_equal(out[2, 2], 12)
  expect_equal(out[1, 2], 0)
  expect_equal(out[2, 1], 0)
})

test_that("bi() refuses a conduit history containing negative weights", {
  # A negative edge makes the stepwise max-of-products recursion invalid (the
  # strongest prefix before a negative edge is the smallest product, not the
  # largest), so this must refuse rather than accumulate a wrong quantity.
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c"), 3), wave = rep(1:3, each = 3),
    shock = c(10, 0, 0,  0, 0, 0,  0, 0, 0)
  )
  ties <- data.frame(
    group = 1, source = c("a", "b"), target = c("b", "c"),
    wave = c(1, 2), link = c(1, -2)
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(
    compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0)),
    "nonnegative conduit weights"
  )
  expect_error(
    compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi_components(alpha = 1, gamma = 0)),
    "nonnegative conduit weights"
  )
  # The all-nonnegative version of the same history still works.
  ties_ok <- transform(ties, link = abs(link))
  nd_ok <- netis_data(nodes, ties_ok, wave_col = "wave")
  out <- compute_exposure(nd_ok, "shock", "link", wave = 3, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))
  expect_true(is.numeric(out$ex_shock_link))
})

# --- missingness known-answers for the path accumulation ---------------------
#
# Regression for the masked-accumulation fix: the plain `%*%`/max-times
# products turned ONE missing content value at ONE historical wave into NA for
# EVERY node's B-I exposure (0 * NA is NA in R), including nodes with no path
# whatsoever to the missing one -- the same silent-total-data-loss class 8.1
# records for the pre-multi-group support linear kernel. The rule now: NA exactly where a
# missing quantity lies on some path that reaches the node, exact values
# everywhere else, and never a fabricated zero.
#
# Shared fixture (the parallel-path network bi()'s own examples use):
#   wave 1 ties a->c, a->d; wave 2 ties c->b, d->b; focal wave 3.
#   shock: a = 10 at wave 1, all else 0.
#   Complete-data truth: E_b(delta) = 10 + 10*delta, all other nodes 0.

.bi_miss_fixture <- function(nodes_mutate = identity, ties_mutate = identity) {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c", "d"), 3), wave = rep(1:3, each = 4),
    shock = c(10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
  ties <- data.frame(
    group = 1, source = c("a", "a", "c", "d"), target = c("c", "d", "b", "b"),
    wave = c(1, 1, 2, 2), link = 1
  )
  netis_data(nodes_mutate(nodes), ties_mutate(ties), wave_col = "wave")
}

test_that("a missing shock on a node with no outgoing paths leaves every other node's bi() exposure exactly intact", {
  # b is a pure sink (no out-ties at any wave), so b's own missing wave-1
  # shock can reach nobody. Under the old accumulation every node came back
  # NA; the correct answer is: b's value is still computable (its exposure
  # comes from a's shock, which is observed), and nothing else moves.
  nd <- .bi_miss_fixture(nodes_mutate = function(n) {
    n$shock[n$node == "b" & n$wave == 1] <- NA_real_
    n
  })
  out <- compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))
  val <- out$ex_shock_link
  names(val) <- out$node
  expect_equal(unname(val["b"]), 15, tolerance = 1e-12)  # 10 + 10 * 0.5
  expect_equal(unname(val[c("a", "c", "d")]), c(0, 0, 0))
})

test_that("a missing shock is NA for exactly the nodes its paths reach, with unreachable nodes' values exact", {
  # c's wave-2 shock is missing. Its only out-tie is c->b at wave 2, so the
  # one-step term contaminates b and nothing else; a, c, d keep exact values.
  nd <- .bi_miss_fixture(nodes_mutate = function(n) {
    n$shock[n$node == "c" & n$wave == 2] <- NA_real_
    n
  })
  out <- compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 1, alpha = 1, gamma = 0))
  val <- out$ex_shock_link
  names(val) <- out$node
  expect_true(is.na(val["b"]))
  expect_equal(unname(val[c("a", "c", "d")]), c(0, 0, 0))
})

test_that("a missing tie weight contaminates only the paths through it, and is never read as a substantive zero", {
  # The c->b wave-2 weight is missing. Every path into b runs through wave 2
  # (b's only in-ties), and the a->c->b chain's product is unknown, so b is
  # NA -- not 10 + 10*delta computed as though the missing tie were absent
  # (weight 0), which would silently understate b's exposure. a, c, d have no
  # path through the missing edge and keep exact values.
  nd <- .bi_miss_fixture(ties_mutate = function(t) {
    t$link[t$source == "c" & t$target == "b"] <- NA_real_
    t
  })
  out <- compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))
  val <- out$ex_shock_link
  names(val) <- out$node
  expect_true(is.na(val["b"]))
  expect_equal(unname(val[c("a", "c", "d")]), c(0, 0, 0))
})

test_that("bi_components(alpha = 1, gamma = 0) masks the same way, and complete data is bit-identical to the unmasked computation", {
  # A/B decomposition under the c-shock-missing fixture: same contamination
  # set as bi() (they share .bi_accumulate()).
  nd_miss <- .bi_miss_fixture(nodes_mutate = function(n) {
    n$shock[n$node == "c" & n$wave == 2] <- NA_real_
    n
  })
  comp <- compute_exposure(nd_miss, "shock", "link", wave = 3, mechanism = bi_components(alpha = 1, gamma = 0))
  expect_true(is.na(comp$A_maxpath[comp$node == "b"]))
  expect_true(is.na(comp$B_redundant[comp$node == "b"]))
  expect_equal(comp$A_maxpath[comp$node != "b"], c(0, 0, 0))

  # Complete data: the masks are all-FALSE and the computation must reduce
  # exactly (identical arithmetic, not merely close) to the known answer.
  nd_full <- .bi_miss_fixture()
  full <- compute_exposure(nd_full, "shock", "link", wave = 3, mechanism = bi_components(alpha = 1, gamma = 0))
  expect_identical(full$A_maxpath[full$node == "b"], 10)
  expect_identical(full$B_redundant[full$node == "b"], 10)
})
