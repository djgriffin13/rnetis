# Derived conduits for exposure.
#
# THE CROSS-PRODUCT: {2 directions} x {reference, binarize, symmetrize,
# multiplex}, all hand-computed on one small network. This is the known-answer
# layer, and a derived conduit is a NEW exposure form -- so it
# needs one, in the same way every other exposure form does.
#
# THE FIXTURE, a directed 3-cycle plus a second relation:
#
#   w:  a --2--> b --3--> c --5--> a          x = (a=1, b=10, c=100)
#   v:  a --7--> c
#
# Weights and contents are chosen so no two products collide, so a transposed
# or mis-constructed matrix cannot produce a right answer by coincidence.

.dc_data <- function() {
  nodes <- data.frame(
    group = 1, node = c("a", "b", "c"), x = c(1, 10, 100),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = 1,
    source = c("a", "b", "c", "a"),
    target = c("b", "c", "a", "c"),
    w = c(2, 3, 5, 0),
    v = c(0, 0, 0, 7),
    stringsAsFactors = FALSE
  )
  netis_data(nodes, ties)
}

.dc_vals <- function(res) {
  col <- setdiff(names(res), c("group", "node"))
  stats::setNames(res[[col]], res$node)
}

# ---- reference conduit, both directions (the baseline the rest is read against)

test_that("KNOWN ANSWER: a bare reference gives the plain-conduit answer", {
  nd <- .dc_data()
  expect_equal(.dc_vals(compute_exposure(nd, "x", "w")), c(a = 500, b = 2, c = 30))
  expect_equal(
    .dc_vals(compute_exposure(nd, "x", "w", direction = "target_influences_source")),
    c(a = 20, b = 300, c = 5)
  )
})

# ---- binarize ---------------------------------------------------------------

test_that("KNOWN ANSWER: a binarized conduit weights every present tie as 1", {
  # w's present ties are a->b, b->c, c->a (the 0-valued a->c row is a stored
  # zero, not a tie). So each node has exactly one in-neighbor at weight 1.
  nd <- .dc_data()
  expect_equal(
    .dc_vals(compute_exposure(nd, "x", binarize(w, rule = nonzero()))),
    c(a = 100, b = 1, c = 10)
  )
  expect_equal(
    .dc_vals(compute_exposure(nd, "x", binarize(w, rule = nonzero()),
                              direction = "target_influences_source")),
    c(a = 10, b = 100, c = 1)
  )
})

# ---- symmetrize: this is what "both directions" means -----------------------

test_that("KNOWN ANSWER: symmetrize() is how exposure from EITHER direction is expressed", {
  # max-symmetrized w: {a,b} = 2, {b,c} = 3, {c,a} = 5.
  #   E_a = 2*x_b + 5*x_c =  20 + 500 = 520
  #   E_b = 2*x_a + 3*x_c =   2 + 300 = 302
  #   E_c = 3*x_b + 5*x_a =  30 +   5 =  35
  nd <- .dc_data()
  expect_equal(
    .dc_vals(compute_exposure(nd, "x", symmetrize(w, rule = "max"))),
    c(a = 520, b = 302, c = 35)
  )
})

test_that("a symmetric conduit gives the SAME answer in both directions", {
  # The structural check behind the incoming/outgoing/both table: once the
  # network is symmetric there is no longer a direction to choose, which is
  # exactly why "both" belongs in the network rather than in `direction`.
  nd <- .dc_data()
  a <- .dc_vals(compute_exposure(nd, "x", symmetrize(w, rule = "max")))
  b <- .dc_vals(compute_exposure(nd, "x", symmetrize(w, rule = "max"),
                                 direction = "target_influences_source"))
  expect_equal(a, b)
})

test_that("KNOWN ANSWER: the symmetrization RULE changes the answer, and is recorded", {
  # "tie if either exists" and "tie only if both do" are different graphs.
  # w has no reciprocated pair at all, so `both` empties it.
  nd <- .dc_data()
  expect_equal(
    .dc_vals(compute_exposure(nd, "x", symmetrize(binarize(w, rule = nonzero()), rule = "both"))),
    c(a = 0, b = 0, c = 0)
  )
  # a is tied to both b and c once symmetrized, so E_a = x_b + x_c = 110;
  # E_b = x_a + x_c = 101; E_c = x_a + x_b = 11. (An earlier draft of this
  # expectation had the three values rotated -- computed per node correctly and
  # then mislabeled, which is precisely the error a hand-computed fixture with
  # non-colliding values is designed to surface.)
  expect_equal(
    .dc_vals(compute_exposure(nd, "x", symmetrize(binarize(w, rule = nonzero()), rule = "any"))),
    c(a = 110, b = 101, c = 11)
  )
})

# ---- multiplex --------------------------------------------------------------

test_that("KNOWN ANSWER: a multiplex conduit combines two relations", {
  # union of w and v as presence: a->b, b->c, c->a (from w) plus a->c (from v).
  #   in-neighbors: a <- c ; b <- a ; c <- b, a
  #   E_a = x_c = 100 ; E_b = x_a = 1 ; E_c = x_b + x_a = 11
  nd <- .dc_data()
  expect_equal(
    .dc_vals(compute_exposure(nd, "x", multiplex(binarize(w, rule = nonzero()),
                                                 binarize(v, rule = nonzero()),
                                                 combine = "union"))),
    c(a = 100, b = 1, c = 11)
  )
})

test_that("a multiplex conduit reports every variable it reads", {
  # The forecast known-at-origin check asks per underlying variable, so this
  # must not answer with one name. multiplex() stores its networks under
  # `components`, not `network` -- a helper that only knew the latter would
  # have returned nothing here and quietly passed every check.
  spec <- multiplex(binarize(w, rule = nonzero()), binarize(v, rule = nonzero()),
                    combine = "union")
  expect_setequal(.conduit_variables(spec), c("w", "v"))
  expect_setequal(.conduit_variables(symmetrize(w, rule = "max")), "w")
  expect_setequal(.conduit_variables("w"), "w")
})

# ---- naming and provenance --------------------------------------------------

test_that("a derived conduit gets a defined column name, not an improvised one", {
  nd <- .dc_data()
  expect_true("ex_x_sym_w" %in% names(compute_exposure(nd, "x", symmetrize(w, rule = "max"))))
  expect_true("ex_x_bin_w" %in% names(compute_exposure(nd, "x", binarize(w, rule = nonzero()))))
  # A multiplex has no single variable, so the label names every one it reads.
  nm <- names(compute_exposure(nd, "x", multiplex(binarize(w, rule = nonzero()),
                                                  binarize(v, rule = nonzero()),
                                                  combine = "union")))
  expect_true(any(grepl("^ex_x_mplex", nm)))
  # And an explicit name still wins.
  expect_true("mine" %in% names(
    compute_exposure(nd, "x", symmetrize(w, rule = "max"), name = "mine")
  ))
})

# ---- the formula path, not just the direct one ------------------------------

test_that("exposure() in a formula accepts a derived conduit", {
  # Capturing `via` with deparse(substitute()) would make a spec the literal
  # TEXT of the call, which then fails as a missing column name. The
  # formula and direct paths must reach the same engine, or the package has two
  # answers for one specification.
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c"), each = 2),
    wave = rep(1:2, times = 3), y = c(1, 2, 3, 4, 5, 6), stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = 1, source = c("a", "b"), target = c("b", "c"),
    wave = c(1, 1), w = c(2, 3), stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  term <- exposure(y, via = symmetrize(w, rule = "max"))
  expect_true(.conduit_is_derived(term$conduit))
  expect_identical(.conduit_label(term$conduit), "sym_w")

  # A bare name still stores a plain character, exactly as before.
  plain <- exposure(y, via = w)
  expect_identical(plain$conduit, "w")
  expect_false(.conduit_is_derived(plain$conduit))
})

test_that("a derived conduit fits, and agrees with the direct computation", {
  nodes <- data.frame(
    group = 1, node = rep(c("a", "b", "c", "d"), each = 2),
    wave = rep(1:2, times = 4),
    y = c(1, 1.4, 2, 2.3, 3, 3.6, 4, 4.1), stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = 1, source = c("a", "b", "c"), target = c("b", "c", "d"),
    wave = 1, w = c(2, 3, 5), stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  fit <- fit_influence(y ~ exposure(y, via = symmetrize(w, rule = "max")), data = nd)
  expect_s3_class(fit, "netis_influence")
  # The fitted term's name is the defined label, so a user can find it.
  expect_true(any(grepl("sym_w", names(coef(fit)))))
})

# ---- an unusable conduit ----------------------------------------------------

test_that("an unusable conduit is refused actionably, not with an internal message", {
  # Guards against a message like:
  #   "tie data is missing required column(s): list(variable = \"w\"), max, NULL"
  # which leaks a spec object into a column-name complaint and tells the user
  # nothing they could act on.
  nd <- .dc_data()
  msg <- tryCatch(
    compute_exposure(nd, "x", binarize(nope, rule = nonzero())),
    error = conditionMessage
  )
  expect_false(grepl("missing required column", msg))
  expect_match(msg, "nope")
})

test_that("a conduit that is neither a name nor a network specification is refused", {
  nd <- .dc_data()
  expect_error(compute_exposure(nd, "x", 42), "network specification")
})
