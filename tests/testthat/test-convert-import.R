# Import: foreign representations into netis_data.
#
# The heaviest group of tests here assert REFUSALS. A guess can only be tested
# by its absence, so "a two-column matrix is never silently read as an edgelist"
# is a test that something errors -- and it is the test that would fail first if
# someone added a convenience shape probe to as_netis_data().

# ---- dispatch discipline (DF.12, the conversion ledger) ------------------------------

test_that("an ambiguous class gets no method and is routed to a named constructor", {
  # data.frame: an edgelist, a node table, or ordinary long data.
  expect_error(as_netis_data(data.frame(from = "a", to = "b")), "no method")
  # list: an adjacency series, a list of graphs, or a table pair.
  expect_error(as_netis_data(list(1, 2)), "no method")

  # And the refusal names the way forward rather than just refusing.
  msg <- tryCatch(as_netis_data(data.frame(a = 1)), error = conditionMessage)
  expect_match(msg, "netis_data_from_edgelist\\(\\)")
  expect_match(msg, "netis_tables\\(\\)")
  expect_match(msg, "netis_data_from_adjacency_series\\(\\)")
})

test_that("a matrix means adjacency, unequivocally", {
  m <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0),
    nrow = 3,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
  )
  nd <- as_netis_data(m)
  expect_s3_class(nd, "netis_data")
  expect_identical(nrow(netis_nodes(nd)), 3L)

  # A non-square NUMERIC matrix cannot be adjacency, and is refused naming the
  # edgelist constructor rather than being re-read as one.
  expect_error(as_netis_data(matrix(1:6, ncol = 2)), "must be square")
  expect_error(as_netis_data(matrix(1:6, ncol = 2)), "netis_data_from_edgelist")

  # The harder case, and the one that shows why the contract picks a meaning
  # rather than probing: a two-column edgelist with exactly two rows IS square,
  # so no dimension check can tell it from a 2x2 adjacency matrix. What settles
  # it is TYPE, not shape -- adjacency cells are tie weights, so a character
  # matrix is refused and routed.
  square_el <- matrix(c("a", "b", "b", "c"), ncol = 2)
  expect_true(nrow(square_el) == ncol(square_el))
  expect_error(as_netis_data(square_el), "tie weights")
  expect_error(as_netis_data(square_el), "netis_data_from_edgelist")

  # A numeric 2x2 is adjacency, per the stated contract, with no hedging.
  expect_s3_class(as_netis_data(matrix(c(0, 1, 1, 0), 2, 2)), "netis_data")
})

test_that("a table pair converts only once it has been named as one", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), trait = c(1, 2, 3))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  nd <- as_netis_data(netis_tables(nodes, ties))
  expect_identical(nrow(netis_nodes(nd)), 3L)
  expect_identical(netis_boundary(nd)$source$source_class, "netis_table_pair")
})


# ---- edgelists: the isolate trap --------------------------------------------

test_that("an edgelist without a roster refuses, naming the loss", {
  edges <- data.frame(source = c("a", "b"), target = c("b", "c"))
  e <- tryCatch(netis_data_from_edgelist(edges), netis_semantic_loss = function(e) e)
  expect_s3_class(e, "netis_semantic_loss")
  expect_identical(e$loss_id, "isolate_roster")
})

test_that("the roster refusal cannot be waved through with allow_loss", {
  # The conversion ledger: an escape hatch may permit a known representational sacrifice,
  # never semantic guessing. Without a roster rnetis does not know the node set.
  edges <- data.frame(source = c("a", "b"), target = c("b", "c"))
  expect_error(
    netis_data_from_edgelist(edges, nodes = NULL),
    class = "netis_semantic_loss"
  )
  expect_error(
    .loss_raise("isolate_roster", allow_loss = "isolate_roster"),
    "cannot be authorized"
  )
})

test_that("isolates survive when the roster is supplied", {
  edges <- data.frame(source = c("a", "b"), target = c("b", "c"), w = c(2, 1))
  nd <- netis_data_from_edgelist(edges, nodes = c("a", "b", "c", "d"))
  # Asserted by count, not by inspection -- the failure mode is silent.
  expect_identical(nrow(netis_nodes(nd)), 4L)
  expect_true("d" %in% netis_nodes(nd)$node)
  expect_identical(nrow(netis_ties(nd)), 2L)
})

test_that("an edgelist accepts a node data frame as its roster", {
  edges <- data.frame(source = "a", target = "b")
  nodes <- data.frame(node = c("a", "b", "c"), trait = c(1, 2, 3))
  nd <- netis_data_from_edgelist(edges, nodes = nodes)
  expect_identical(nrow(netis_nodes(nd)), 3L)
  expect_true("trait" %in% names(netis_nodes(nd)))
})

test_that("edgelist directedness is declared, never read off reciprocity", {
  # Fully reciprocated, and still directed, because that is what was declared.
  edges <- data.frame(source = c("a", "b"), target = c("b", "a"))
  nd <- netis_data_from_edgelist(edges, nodes = c("a", "b"), directed = TRUE)
  expect_true(netis_schema(nd)$directed)
  expect_identical(netis_boundary(nd)$directedness$state, "declared")
  expect_false(netis_boundary(nd)$directedness$source_encoded)
})

test_that("an edgelist matrix is accepted once the function has been named", {
  el <- matrix(c("a", "b", "b", "c"), ncol = 2)
  nd <- netis_data_from_edgelist(el, nodes = c("a", "b", "c"))
  expect_identical(nrow(netis_ties(nd)), 2L)
})

# ---- adjacency series -------------------------------------------------------

.w1 <- matrix(c(0, 1, 0, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
.w2 <- matrix(c(0, 1, 1, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))

test_that("a flat wave series builds one group across waves", {
  nd <- netis_data_from_adjacency_series(list(`1` = .w1, `2` = .w2))
  expect_identical(as.character(netis_schema(nd)$waves), c("1", "2"))
  ties <- netis_ties(nd)
  expect_identical(nrow(ties[ties$wave == "1", ]), 1L)
  expect_identical(nrow(ties[ties$wave == "2", ]), 2L)
})

test_that("a nested list builds groups and waves", {
  nd <- netis_data_from_adjacency_series(
    list(A = list(`1` = .w1, `2` = .w2), B = list(`1` = .w2, `2` = .w2))
  )
  expect_identical(sort(unique(netis_nodes(nd)$group)), c("A", "B"))
  expect_identical(nrow(netis_nodes(nd)), 4L)
})

test_that("a 3-D array is a wave series for one group", {
  arr <- array(0, dim = c(2, 2, 2), dimnames = list(c("a", "b"), c("a", "b"), NULL))
  arr[, , 1] <- .w1
  arr[, , 2] <- .w2
  nd <- netis_data_from_adjacency_series(arr, waves = c(1, 2))
  expect_identical(as.character(netis_schema(nd)$waves), c("1", "2"))
})

test_that("a static network is reused across waves rather than duplicated", {
  nd <- netis_data_from_adjacency_series(list(A = .w1), static = TRUE, waves = c(1, 2, 3))

  # The dataset has three waves ...
  expect_identical(as.character(netis_schema(nd)$waves), c("1", "2", "3"))
  expect_identical(unique(netis_nodes(nd)$group), "A")

  # ... and the network is STORED ONCE, which is what "reused rather than
  # duplicated" means. rnetis's static/dynamic split recognizes a tie whose
  # value never varies and holds it once per entity, so the logical tie view is
  # one row, not three. Asserting three rows here would be asserting the
  # duplication this argument exists to avoid.
  expect_true("weight" %in% netis_schema(nd)$tie_vars$static)
  expect_identical(nrow(netis_ties(nd)), 1L)

  # The behavioral claim -- the same network is in force at every wave -- is
  # checked where it is observable, at the resolved snapshot.
  snaps <- lapply(c("1", "2", "3"), function(w) netis_adjacency(nd, "weight", wave = w))
  expect_identical(snaps[[2]], snaps[[1]])
  expect_identical(snaps[[3]], snaps[[1]])
  expect_identical(sum(snaps[[1]]), 1)
})

test_that("static reuse is declared, not inferred from the shape", {
  # `list(A = M)` is equally "group A's static network" and "wave A of one
  # group". Without static = TRUE it reads as the latter, and a group entry
  # that is a bare matrix under the nested layout refuses rather than guessing.
  quiet <- netis_data_from_adjacency_series(list(A = .w1))
  expect_identical(as.character(netis_schema(quiet)$waves), "A")

  expect_error(
    netis_data_from_adjacency_series(list(A = .w1), static = TRUE),
    "`waves` must say"
  )
  expect_error(
    netis_data_from_adjacency_series(list(A = list(`1` = .w1), B = .w2)),
    "static = TRUE"
  )
})

test_that("unnamed waves are refused rather than numbered by rnetis", {
  # A wave label is an occasion identifier the user chose. Inventing one would
  # be rnetis deciding what its own data is called.
  expect_error(
    netis_data_from_adjacency_series(list(.w1, .w2)),
    "must supply the labels"
  )
  expect_error(
    netis_data_from_adjacency_series(list(.w1, .w2), waves = c(1, 2, 3)),
    "3 label"
  )
})

test_that("a single matrix is not a series", {
  expect_error(netis_data_from_adjacency_series(.w1), "netis_data_from_adjacency\\(\\)")
})

test_that("the roster is the union across waves, so a wave-absent node is an isolate", {
  a <- matrix(c(0, 1, 1, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  b <- matrix(c(0, 1, 1, 0), 2, 2, dimnames = list(c("b", "c"), c("b", "c")))
  nd <- netis_data_from_adjacency_series(list(`1` = a, `2` = b))
  expect_identical(sort(netis_nodes(nd)$node), c("a", "b", "c"))
})

test_that("an adjacency series carries declared observation times", {
  nd <- netis_data_from_adjacency_series(
    list(`1` = .w1, `2` = .w2),
    time_meta = data.frame(wave = c(1, 2), time_value = c(0, 14)), time_unit = "day"
  )
  ti <- netis_time_index(nd)
  expect_identical(ti$time_value, c(0, 14))
  expect_identical(ti$time_source, rep("declared", 2))
})

# ---- static/dynamic split input ---------------------------------------------

test_that("split static and dynamic tables join into one object", {
  node_static <- data.frame(group = 1, node = c("a", "b"), sex = c("f", "m"))
  node_dynamic <- data.frame(
    group = 1, node = rep(c("a", "b"), each = 2),
    wave = rep(1:2, times = 2), y = c(1, 2, 3, 4)
  )
  tie_dynamic <- data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = c(1, 2))
  nd <- netis_data_from_split(
    node_static = node_static, node_dynamic = node_dynamic,
    tie_dynamic = tie_dynamic, wave_col = "wave"
  )
  nodes <- netis_nodes(nd)
  expect_identical(nrow(nodes), 4L)
  expect_true(all(c("sex", "y") %in% names(nodes)))
  # `sex` was supplied once per node and is stored as static.
  expect_true("sex" %in% netis_schema(nd)$node_vars$static)
})

test_that("a variable in both split tables is refused rather than silently preferred", {
  node_static <- data.frame(group = 1, node = "a", y = 99)
  node_dynamic <- data.frame(group = 1, node = "a", wave = 1:2, y = c(1, 2))
  tie_dynamic <- data.frame(group = 1, source = "a", target = "a", wave = 1:2, w = 1)
  expect_error(
    netis_data_from_split(
      node_static = node_static, node_dynamic = node_dynamic,
      tie_dynamic = tie_dynamic, wave_col = "wave", self_ties = "permitted"
    ),
    "appear in both"
  )
})

test_that("a dynamic table without wave_col is refused", {
  expect_error(
    netis_data_from_split(
      node_dynamic = data.frame(group = 1, node = "a", wave = 1, y = 1),
      tie_dynamic = data.frame(group = 1, source = "a", target = "a", wave = 1, w = 1)
    ),
    "wave_col must be supplied"
  )
})

# ---- the tibble boundary ----------------------------------------------------

test_that("a tibble in comes back as the type `as =` asks for, not the type it went in as", {
  skip_if_not_installed("tibble")
  nodes <- tibble::tibble(group = 1, node = c("a", "b"), trait = c(1, 2))
  ties <- tibble::tibble(group = 1, source = "a", target = "b", w = 1)
  nd <- netis_data(nodes, ties)

  # Before the boundary existed, a tibble survived into storage and came back
  # out of `as = "data.frame"` still a tibble -- the documented return type and
  # the actual one disagreed.
  expect_identical(class(netis_nodes(nd)), "data.frame")
  expect_identical(class(netis_ties(nd)), "data.frame")
  expect_s3_class(netis_nodes(nd, as = "tibble"), "tbl_df")

  # And no value was lost on the way in.
  expect_identical(netis_nodes(nd)$trait, c(1, 2))
})

# ---- every importer records where it came from -----------------------------

test_that("every importer records its converter and source class", {
  edges <- data.frame(source = "a", target = "b")
  cases <- list(
    netis_data_from_edgelist(edges, nodes = c("a", "b")),
    netis_data_from_adjacency_series(list(`1` = .w1)),
    as_netis_data(.w1),
    as_netis_data(netis_tables(
      data.frame(group = 1, node = c("a", "b")),
      data.frame(group = 1, source = "a", target = "b", w = 1)
    ))
  )
  for (nd in cases) {
    src <- netis_boundary(nd)$source
    expect_false(is.null(src))
    expect_true(nzchar(src$converter))
    expect_true(nzchar(src$source_class))
  }
})
