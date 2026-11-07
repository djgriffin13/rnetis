# Adjacency-matrix import: dimnames, missing cells, empty networks.

test_that("column names label the receivers, even when ordered differently from the rows", {
  # Rows a, b, c; columns c, b, a. The only tie is in row b, column "c":
  # b -> c. Reading both dimensions from rownames would give b -> a.
  m <- matrix(0, 3, 3, dimnames = list(c("a", "b", "c"), c("c", "b", "a")))
  m["b", "c"] <- 4
  t1 <- netis_ties(netis_data_from_adjacency(m))
  expect_equal(nrow(t1), 1L)
  expect_identical(as.character(t1$source), "b")
  expect_identical(as.character(t1$target), "c")
  expect_equal(t1$weight, 4)
  # And the round trip puts the value back in the same named cell.
  back <- netis_adjacency(netis_data_from_adjacency(m), "weight")
  expect_equal(unname(back["b", "c"]), 4)
  expect_equal(unname(back["b", "a"]), 0)
})

test_that("a matrix with only column names uses them as the node ids", {
  m <- matrix(0, 3, 3, dimnames = list(NULL, c("x", "y", "z")))
  m[1, 3] <- 1
  nd <- netis_data_from_adjacency(m)
  expect_setequal(as.character(netis_nodes(nd)$node), c("x", "y", "z"))
  expect_identical(as.character(netis_ties(nd)$source), "x")
  expect_identical(as.character(netis_ties(nd)$target), "z")
})

test_that("row and column names that are different node sets are refused", {
  m <- matrix(0, 2, 2, dimnames = list(c("a", "b"), c("a", "q")))
  expect_error(netis_data_from_adjacency(m), "not the same set")
})

test_that("an NA cell is not silently read as no tie", {
  m <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  m["a", "b"] <- 1
  m["b", "c"] <- NA
  expect_error(netis_data_from_adjacency(m), "b -> c")
  # Kept as an NA-valued row when zeros are kept.
  t2 <- netis_ties(netis_data_from_adjacency(m, drop_zero = FALSE))
  expect_true(is.na(t2$weight[t2$source == "b" & t2$target == "c"]))
})

test_that("an undirected cell missing in one direction only is warned about", {
  m <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  m["a", "b"] <- NA
  m["b", "a"] <- 1
  expect_warning(netis_data_from_adjacency(m, directed = FALSE, drop_zero = FALSE),
                 "missing in one direction")
})

test_that("an all-zero matrix gives a network of isolates", {
  z <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  nd <- netis_data_from_adjacency(z)
  expect_equal(nrow(netis_ties(nd)), 0L)
  expect_equal(nrow(netis_nodes(nd)), 3L)
  expect_equal(unname(netis_adjacency(nd, "weight")), matrix(0, 3, 3))
})
