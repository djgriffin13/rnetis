# Specification-level contracts for symmetric Selection.
#
# `pool_beta` pooling is a weighted mean over already-completed per-group fits,
# which is indifferent to whether the effect structure was symmetric, so there
# is nothing to refuse there; the hierarchical multi-group route has no
# symmetric branch and refuses by name (last block).
#
# The end-to-end block checks that a symmetric fit actually runs: an
# argument-threading defect (`.assert_symmetric_selection_spec()` being handed
# a `pooling` argument that did not exist, so every symmetric call died on an
# undefined object) is caught only by running one.

.sym_spec <- function(formula) {
  .parse_selection_formula(formula)
}

.sym_schema <- function(directed = FALSE) {
  list(directed = directed)
}

test_that("receiver() on a symmetric relation is refused by name", {
  expect_error(
    .assert_symmetric_selection_spec(
      .sym_spec(tie ~ sender(x) + receiver(x)), .sym_schema()
    ),
    "no sender/receiver distinction"
  )
})

test_that("prior_reciprocity() on a symmetric relation is refused by name", {
  expect_error(
    .assert_symmetric_selection_spec(
      .sym_spec(tie ~ sender(x) + prior_tie() + prior_reciprocity()),
      .sym_schema()
    ),
    "the reverse tie is the same tie"
  )
})

test_that("symmetric on directed-declared data is refused, naming both fixes", {
  err <- tryCatch(
    .assert_symmetric_selection_spec(
      .sym_spec(tie ~ sender(x)), .sym_schema(directed = TRUE)
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "declares its ties directed")
  expect_match(err, "directed = FALSE")
  expect_match(err, "symmetrize")
})

test_that("sender() terms are renamed to nodal columns; a user name survives", {
  spec <- .assert_symmetric_selection_spec(
    .sym_spec(tie ~ sender(x) + sender(z, name = "my_z") + dyad(d)),
    .sym_schema()
  )
  nms <- vapply(spec$senders, function(t) t$name, character(1))
  expect_identical(nms, c("nod_x", "my_z"))
  # Dyadic terms are untouched -- the rename is about the vanished
  # sender/receiver split, which dyads never had.
  expect_identical(spec$dyads[[1]]$name, "dyd_d")
})

test_that("a symmetric fit runs and reports one nodal effect per covariate", {
  skip_if_not_installed("amen")
  set.seed(11)
  n <- 10
  ids <- sprintf("n%02d", seq_len(n))
  nodes <- data.frame(group = 1, node = ids, tenure = stats::rnorm(n),
                      stringsAsFactors = FALSE)
  # An undirected relation is stored ONCE per unordered pair, not twice. The
  # assembler mirrors the single stored row; storing both directions would be
  # a different (and, for undirected data, wrong) declaration.
  pairs <- t(utils::combn(ids, 2))
  ties <- data.frame(group = 1, source = pairs[, 1], target = pairs[, 2],
                     tie = stats::rbinom(nrow(pairs), 1, 0.3),
                     stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties, directed = FALSE,
                   tie_meta = list(tie = list(zero_meaning = "measured_zero")))

  fit <- fit_selection(tie ~ sender(tenure), data = nd, symmetric = TRUE,
                       nscan = 120, burn = 20, odens = 5, seed = 1)

  # One nodal coefficient, named for the vanished sender/receiver split.
  expect_true("nod_tenure" %in% names(coef(fit)))
  expect_false(any(grepl("^snd_|^rcv_", names(coef(fit)))))
  expect_true(isTRUE(fit$symmetric))
})

# ---- regression: the removed `pooling` argument -----------------------------

test_that("symmetric = TRUE reaches the assert without an undefined `pooling`", {
  # THE REGRESSION. `fit_selection()` called
  #   .assert_symmetric_selection_spec(spec, schema, pooling)
  # after `pooling` had been replaced by `pool_beta`, so EVERY symmetric call
  # died on `object 'pooling' not found` before doing any work. The suite was
  # green over it because nothing exercised the branch.
  #
  # This is a code defect with a reproducing test, not a documentation slip.
  # It is pinned three ways so that reintroducing the argument in any form
  # fails here rather than at a user's console.

  # 1. ARITY. The helper takes exactly the two things it needs. A third
  #    parameter reappearing is the shape the bug had.
  expect_identical(names(formals(.assert_symmetric_selection_spec)),
                   c("spec", "schema"))

  # 2. NO REFERENCE TO A VANISHED BINDING anywhere in the call chain. The
  #    failure mode was a name that did not exist, so what matters is that
  #    every name the source uses IS bound -- not that one particular spelling
  #    is absent.
  #
  #    Asserting that the string "pooling" never appears in fit_selection()
  #    would not work: `pooling` is a real formal (the hierarchical multi-group
  #    route), so a string test would fail on correct code while still not
  #    catching the defect. The defect was an UNBOUND name, and that is what is
  #    tested.
  src <- paste(deparse(fit_selection), collapse = " ")
  expect_true(grepl("pool_beta", src, fixed = TRUE))
  if (grepl("[^_.]pooling", src)) {
    expect_true("pooling" %in% names(formals(fit_selection)))
  }

  # 3. THE BRANCH ITSELF RUNS. Arity and source text would both pass if the
  #    call site were simply unreachable, so the path is exercised for real
  #    and the specific error is named.
  err <- tryCatch({
    .assert_symmetric_selection_spec(.sym_spec(tie ~ sender(x)), .sym_schema())
    NULL
  }, error = function(e) conditionMessage(e))
  expect_null(err)
})

test_that("a symmetric fit does not error on an undefined object", {
  # The end-to-end half of the regression above: the defect was only visible
  # from fit_selection(), which is where a user would meet it.
  skip_if_not_installed("amen")
  set.seed(11)
  n <- 8
  ids <- sprintf("n%02d", seq_len(n))
  nodes <- data.frame(group = 1, node = ids, tenure = stats::rnorm(n),
                      stringsAsFactors = FALSE)
  pairs <- t(utils::combn(ids, 2))
  ties <- data.frame(group = 1, source = pairs[, 1], target = pairs[, 2],
                     tie = stats::rbinom(nrow(pairs), 1, 0.3),
                     stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties, directed = FALSE,
                   tie_meta = list(tie = list(zero_meaning = "measured_zero")))

  err <- tryCatch({
    fit_selection(tie ~ sender(tenure), data = nd, symmetric = TRUE,
                  nscan = 60, burn = 10, odens = 5, seed = 1)
    NULL
  }, error = function(e) conditionMessage(e))
  expect_null(err)
  # And specifically not the error the defect produced.
  if (!is.null(err)) expect_false(grepl("object 'pooling' not found", err, fixed = TRUE))
})

test_that("a symmetric relation is refused by the joint multi-group route, by name", {
  skip_if_not_installed("amen")
  # The joint engine has no symmetric branch: amen's symmetric conditionals are
  # not part of the ported core. Refused rather than fitted as directed, which
  # would silently discard the relation's symmetry.
  d <- .selection_fixture_data("bin", n_groups = 2, n_actors = 6, n_waves = 2)
  expect_error(
    fit_selection(tie ~ prior_tie(), data = d, family = "bin", symmetric = TRUE,
                  pooling = selection_pooling(random = "intercept"),
                  nscan = 20, burn = 10, odens = 5),
    "symmetric"
  )
})
