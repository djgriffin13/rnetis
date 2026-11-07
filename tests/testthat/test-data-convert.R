# Known-answer tests for adjacency matrix <-> netis_data conversion.
# Hand-verified small matrices; expected tie rows worked out by hand, not by
# re-deriving them from the conversion code under test.

test_that("a directed matrix converts to exactly the expected tie rows", {
  m <- matrix(
    c(0, 1, 0,
      0, 0, 1,
      1, 0, 0),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
  )
  # By hand: a->b = 1, b->c = 1, c->a = 1. Everything else is 0 and dropped.
  nd <- netis_data_from_adjacency(m)
  ties <- netis_ties(nd)
  ties <- ties[order(ties$source, ties$target), ]

  expect_equal(nrow(ties), 3L)
  expect_equal(ties$weight[ties$source == "a" & ties$target == "b"], 1)
  expect_equal(ties$weight[ties$source == "b" & ties$target == "c"], 1)
  expect_equal(ties$weight[ties$source == "c" & ties$target == "a"], 1)

  # Nodes were derived from dimnames when node_data was not supplied.
  expect_setequal(netis_nodes(nd)$node, c("a", "b", "c"))
})

test_that("an undirected symmetric matrix stores one row per unordered pair", {
  m <- matrix(
    c(0, 1, 0,
      1, 0, 1,
      0, 1, 0),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
  )
  # By hand, upper triangle only: (a,b)=1, (a,c)=0 (dropped), (b,c)=1.
  nd <- netis_data_from_adjacency(m, directed = FALSE)
  ties <- netis_ties(nd)

  expect_equal(nrow(ties), 2L)
  expect_true(all(paste(ties$source, ties$target) %in% c("a b", "b c")))
  expect_false(any(paste(ties$source, ties$target) %in% c("b a", "c b")))
})

test_that("an asymmetric matrix passed as undirected warns and keeps only the upper triangle", {
  m <- matrix(
    c(0, 1, 0,
      0, 0, 1,
      1, 0, 0),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
  )
  expect_warning(
    nd <- netis_data_from_adjacency(m, directed = FALSE),
    "not symmetric"
  )
  ties <- netis_ties(nd)
  # Upper triangle only: (a,b)=1 kept; (a,c)=0 dropped; (b,c)=1 kept.
  # The lower-triangle value at (c,a)=1 is discarded, which is what the warning is about.
  expect_equal(nrow(ties), 2L)
})

test_that("nonzero diagonal with self_ties = 'prohibited' errors rather than silently dropping data", {
  m <- matrix(c(0, 1, 0, 5), nrow = 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_error(netis_data_from_adjacency(m), "self_ties")
})

test_that("nonzero diagonal with self_ties = 'permitted' is kept as a self-tie row", {
  m <- matrix(c(0, 1, 0, 5), nrow = 2, dimnames = list(c("a", "b"), c("a", "b")))
  nd <- netis_data_from_adjacency(m, self_ties = "permitted")
  ties <- netis_ties(nd)
  expect_equal(ties$weight[ties$source == "b" & ties$target == "b"], 5)
})

test_that("drop_zero = FALSE keeps explicit zero-weight ties", {
  m <- matrix(c(0, 1, 0, 0), nrow = 2, byrow = TRUE,
              dimnames = list(c("a", "b"), c("a", "b")))
  nd <- netis_data_from_adjacency(m, drop_zero = FALSE)
  ties <- netis_ties(nd)
  # Both off-diagonal cells kept: a->b = 1, b->a = 0.
  expect_equal(nrow(ties), 2L)
  expect_equal(ties$weight[ties$source == "b" & ties$target == "a"], 0)
})

# ---- regression: drop_zero silently dropped data with no trace in the schema
# -- (guards against the case where drop_zero = TRUE, the default, discarded
# 0/NA off-diagonal cells with no record of that choice anywhere in the
# resulting object: nothing is silently transformed, and data is never
# silently dropped.)

test_that("drop_zero = TRUE (default) records zero_meaning = 'structural_absence'", {
  m <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  nd <- netis_data_from_adjacency(m)
  expect_equal(netis_schema(nd)$tie_descriptors$weight$zero_meaning, "structural_absence")
})

test_that("drop_zero = FALSE records zero_meaning = 'measured_zero'", {
  m <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  nd <- netis_data_from_adjacency(m, drop_zero = FALSE)
  expect_equal(netis_schema(nd)$tie_descriptors$weight$zero_meaning, "measured_zero")
})

test_that("a named list of matrices produces multiple groups", {
  m1 <- matrix(c(0, 1, 0, 0), nrow = 2, dimnames = list(c("a", "b"), c("a", "b")))
  m2 <- matrix(c(0, 0, 1, 0), nrow = 2, dimnames = list(c("x", "y"), c("x", "y")))
  nd <- netis_data_from_adjacency(list(g1 = m1, g2 = m2))

  ties <- netis_ties(nd)
  expect_setequal(unique(ties$group), c("g1", "g2"))
  expect_equal(nrow(ties[ties$group == "g1", ]), 1L)
  expect_equal(nrow(ties[ties$group == "g2", ]), 1L)
})

test_that("an unnamed list of matrices is rejected", {
  m <- matrix(c(0, 1, 0, 0), nrow = 2)
  expect_error(netis_data_from_adjacency(list(m, m)), "named")
})

# ---- export: netis_adjacency() ----------------------------------------------

test_that("netis_adjacency() exactly round-trips a directed matrix with no dropped zeros", {
  m <- matrix(
    c(0, 1, 2,
      0, 0, 0,
      3, 0, 0),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
  )
  nd <- netis_data_from_adjacency(m, drop_zero = FALSE)
  out <- netis_adjacency(nd, "weight")
  # Reorder to match the original dimname order before comparing.
  out <- out[rownames(m), colnames(m)]
  expect_equal(unname(out), unname(m))
})

test_that("netis_adjacency() symmetrizes an undirected network on export", {
  m <- matrix(
    c(0, 1, 0,
      1, 0, 1,
      0, 1, 0),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
  )
  nd <- netis_data_from_adjacency(m, directed = FALSE)
  out <- netis_adjacency(nd, "weight")
  out <- out[c("a", "b", "c"), c("a", "b", "c")]
  expect_equal(out["a", "b"], out["b", "a"])
  expect_equal(unname(out["a", "b"]), 1)
})

test_that("netis_adjacency() includes a group that has nodes but zero ties, not silently dropping it", {
  # Regression test: group 2 has nodes but no rows in tie_data at all. This is
  # a legal configuration (the group-size diagnostic flags fewer than 2
  # nodes as a WARNING, but "nodes present, zero ties" is not itself an
  # error), and it must still appear in the export as an all-fill matrix.
  nodes <- data.frame(group = c(1, 1, 2, 2), node = c("a", "b", "x", "y"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  nd <- netis_data(nodes, ties)

  out <- netis_adjacency(nd, "weight", drop = FALSE)
  expect_setequal(names(out), c("1", "2"))
  expect_equal(dim(out[["2"]]), c(2L, 2L))
  expect_true(all(out[["2"]] == 0))
})

test_that("netis_adjacency() returns a named list when there are multiple groups", {
  m1 <- matrix(c(0, 1, 0, 0), nrow = 2, dimnames = list(c("a", "b"), c("a", "b")))
  m2 <- matrix(c(0, 0, 1, 0), nrow = 2, dimnames = list(c("x", "y"), c("x", "y")))
  nd <- netis_data_from_adjacency(list(g1 = m1, g2 = m2))
  out <- netis_adjacency(nd, "weight")
  expect_type(out, "list")
  expect_setequal(names(out), c("g1", "g2"))
})

# ---- tibble output ----------------------------------------------------------

test_that("as = 'tibble' returns a tibble when the package is available", {
  skip_if_not_installed("tibble")
  nodes <- data.frame(group = 1, node = c("a", "b"), trait = c(1, 2))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  nd <- netis_data(nodes, ties)
  out <- netis_nodes(nd, as = "tibble")
  expect_s3_class(out, "tbl_df")
})

test_that("default accessor output is a plain data.frame, not a tibble", {
  nodes <- data.frame(group = 1, node = c("a", "b"), trait = c(1, 2))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  nd <- netis_data(nodes, ties)
  expect_false(inherits(netis_nodes(nd), "tbl_df"))
})

test_that("a non-square adjacency matrix is rejected", {
  m <- matrix(1:6, nrow = 2, dimnames = list(c("a", "b"), c("x", "y", "z")))
  expect_error(netis_data_from_adjacency(m), "square")
})

test_that("netis_adjacency() honors a custom fill value for absent cells", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 5)
  nd <- netis_data(nodes, ties)
  m <- netis_adjacency(nd, "weight", fill = NA_real_)
  expect_true(is.na(m["a", "c"]))
  expect_equal(m["a", "b"], 5)
  m2 <- netis_adjacency(nd, "weight", fill = -1)
  expect_equal(m2["a", "c"], -1)
})

# Wave handling in netis_adjacency(). Before the wave
# argument existed, every wave's rows were written into one matrix and the
# last row for a cell won -- a silent collapse. These fixtures are built so
# that the collapsed answer and the correct answer differ, which is what
# makes them regression tests rather than restatements of behavior.

.two_wave_fixture <- function() {
  nodes <- data.frame(
    group = 1,
    node = rep(c("a", "b", "c"), each = 2),
    wave = rep(1:2, times = 3),
    trait = c(1, 2, 3, 4, 5, 6)
  )
  # a->b flips value between waves; a->c exists only at wave 2. Under the old
  # collapse both waves' rows landed in one matrix, so a->b took wave 2's
  # value and a->c appeared at wave 1 as well.
  ties <- data.frame(
    group = 1,
    source = c("a", "a", "a"),
    target = c("b", "b", "c"),
    wave = c(1, 2, 2),
    weight = c(10, 99, 7)
  )
  netis_data(nodes, ties, wave_col = "wave")
}

test_that("netis_adjacency() returns the requested wave, not a collapse of all waves", {
  nd <- .two_wave_fixture()

  w1 <- netis_adjacency(nd, "weight", wave = 1)
  expect_equal(w1["a", "b"], 10)   # not 99, which is wave 2's value
  expect_equal(w1["a", "c"], 0)    # the a->c tie does not exist yet at wave 1

  w2 <- netis_adjacency(nd, "weight", wave = 2)
  expect_equal(w2["a", "b"], 99)
  expect_equal(w2["a", "c"], 7)
})

test_that("netis_adjacency() errors on a multi-wave object when wave is not named", {
  nd <- .two_wave_fixture()
  expect_error(netis_adjacency(nd, "weight"), "2 waves")
  expect_error(netis_adjacency(nd, "weight"), "wave = ")
})

test_that("netis_adjacency() infers the wave when only one is observed", {
  nodes <- data.frame(group = 1, node = c("a", "b"), wave = c(1, 1), trait = c(1, 2))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1, weight = 4)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_equal(netis_adjacency(nd, "weight")["a", "b"], 4)
})

test_that("netis_adjacency() rejects an unobserved wave and a wave on wave-free data", {
  nd <- .two_wave_fixture()
  expect_error(netis_adjacency(nd, "weight", wave = 3), "not one of the observed waves")

  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", weight = 1)
  flat <- netis_data(nodes, ties)
  expect_error(netis_adjacency(flat, "weight", wave = 1), "no wave dimension")
})

test_that("netis_adjacency() keeps a stable roster across waves", {
  nd <- .two_wave_fixture()
  expect_identical(dimnames(netis_adjacency(nd, "weight", wave = 1)),
                   dimnames(netis_adjacency(nd, "weight", wave = 2)))
})
