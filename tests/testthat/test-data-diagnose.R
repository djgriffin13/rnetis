# Known-answer tests for the diagnostic framework (ERROR/WARNING/NOTE) and each
# concrete check. Every check is exercised once with a hand-built dataset where
# the correct finding (or absence of one) is known in advance.

# ---- generic framework ------------------------------------------------------

test_that("an empty diagnostics collection has no findings and no errors", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b")
  nd <- netis_data(nodes, ties)

  d <- diagnose_data(nd)
  expect_s3_class(d, "netis_diagnostics")
  expect_length(d, 0L)
  expect_false(has_level(d, "ERROR"))
})

test_that(".finding() rejects an invalid level or family rather than accepting a typo silently", {
  expect_error(.finding("BOGUS", "data", "x", "msg"), "level")
  expect_error(.finding("ERROR", "bogus_family", "x", "msg"), "family")
})

test_that("filter_diagnostics() filters by level and family", {
  nodes <- data.frame(group = 1, node = "a")  # group of 1 -> WARNING (too small)
  ties <- data.frame(group = 1, source = "a", target = "b")  # 'b' unknown -> ERROR
  nd <- netis_data(nodes, ties, self_ties = "permitted")

  d <- diagnose_data(nd)
  expect_true(has_level(d, "ERROR"))
  expect_true(has_level(d, "WARNING"))

  errors_only <- filter_diagnostics(d, level = "ERROR")
  expect_true(all(vapply(errors_only, function(f) f$level == "ERROR", logical(1))))
  expect_true(length(errors_only) < length(d))

  data_only <- filter_diagnostics(d, family = "data")
  expect_true(all(vapply(data_only, function(f) f$family == "data", logical(1))))
})

test_that("Filter() and other base list operations still work on a netis_diagnostics object", {
  # This is exactly the case that broke when `[` had non-standard two-argument
  # semantics: Filter() calls x[selector] internally.
  nodes <- data.frame(group = 1, node = "a")
  ties <- data.frame(group = 1, source = "a", target = "b")
  nd <- netis_data(nodes, ties, self_ties = "permitted")
  d <- diagnose_data(nd)

  errs <- Filter(function(f) f$level == "ERROR", d)
  expect_true(length(errs) >= 1L)
  expect_true(all(vapply(errs, function(f) f$level == "ERROR", logical(1))))
})

test_that("print.netis_diagnostics does not error and reports counts", {
  nodes <- data.frame(group = 1, node = "a")
  ties <- data.frame(group = 1, source = "a", target = "b")
  nd <- netis_data(nodes, ties, self_ties = "permitted")
  d <- diagnose_data(nd)
  expect_output(print(d), "ERROR")
})

# ---- data diagnostics -------------------------------------------------------

test_that("a tie referencing an unknown node is an ERROR naming the missing node", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "c")  # 'c' does not exist
  nd <- netis_data(nodes, ties, self_ties = "permitted")

  d <- diagnose_data(nd)
  errs <- filter_diagnostics(d, "ERROR", "data")
  expect_length(errs, 1L)
  expect_equal(errs[[1]]$code, "tie_references_unknown_node")
  expect_true("c" %in% errs[[1]]$details$nodes)
})

test_that("a fully valid tie set produces no referential-integrity error", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b")
  nd <- netis_data(nodes, ties)
  d <- diagnose_data(nd)
  expect_false(any(vapply(d, function(f) f$code == "tie_references_unknown_node", logical(1))))
})

test_that("a self-tie despite self_ties = 'prohibited' is an ERROR", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("a", "b"))
  nd <- netis_data(nodes, ties, self_ties = "prohibited")

  d <- diagnose_data(nd)
  errs <- filter_diagnostics(d, "ERROR", "data")
  expect_true(any(vapply(errs, function(f) f$code == "self_tie_despite_prohibited", logical(1))))
})

test_that("a self-tie with self_ties = 'permitted' produces no finding for it", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = c("a", "a"), target = c("a", "b"))
  nd <- netis_data(nodes, ties, self_ties = "permitted")
  d <- diagnose_data(nd)
  expect_false(any(vapply(d, function(f) f$code == "self_tie_despite_prohibited", logical(1))))
})

test_that("an isolated node with no ties is a NOTE naming it", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = "a", target = "b")  # 'c' has no tie at all
  nd <- netis_data(nodes, ties)

  d <- diagnose_data(nd)
  notes <- filter_diagnostics(d, "NOTE", "data")
  isolate_finding <- Filter(function(f) f$code == "isolated_nodes", notes)
  expect_length(isolate_finding, 1L)
  expect_equal(isolate_finding[[1]]$details$nodes, "c")
})

test_that("a group with fewer than 2 nodes is a WARNING", {
  nodes <- data.frame(group = c(1, 2, 2), node = c("a", "b", "c"))
  ties <- data.frame(group = 2, source = "b", target = "c")
  nd <- netis_data(nodes, ties)

  d <- diagnose_data(nd)
  warns <- Filter(function(f) f$code == "group_too_small_for_ties", d)
  expect_length(warns, 1L)
  expect_equal(warns[[1]]$level, "WARNING")
  expect_equal(warns[[1]]$details$groups, "1")
})

test_that("a numeric node variable with zero variance is a NOTE naming it", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), trait = c(5, 5, 5), other = c(1, 2, 3))
  ties <- data.frame(group = 1, source = "a", target = "b")
  nd <- netis_data(nodes, ties)

  d <- diagnose_data(nd)
  flat <- Filter(function(f) f$code == "zero_variance_node_variable", d)
  expect_length(flat, 1L)
  expect_equal(flat[[1]]$details$variables, "trait")
})

test_that("varying numeric node variables produce no zero-variance finding", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), trait = c(1, 2, 3))
  ties <- data.frame(group = 1, source = "a", target = "b")
  nd <- netis_data(nodes, ties)
  d <- diagnose_data(nd)
  expect_false(any(vapply(d, function(f) f$code == "zero_variance_node_variable", logical(1))))
})

test_that("an all-NA numeric node variable is excluded, not false-flagged as zero variance", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"), trait = c(NA_real_, NA_real_, NA_real_))
  ties <- data.frame(group = 1, source = "a", target = "b")
  nd <- netis_data(nodes, ties)
  d <- diagnose_data(nd)
  expect_false(any(vapply(d, function(f) f$code == "zero_variance_node_variable", logical(1))))
})

# ---- design diagnostics -----------------------------------------------------

test_that("a single-wave (or no-wave) dataset produces a NOTE about lagged terms", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b")
  nd <- netis_data(nodes, ties)

  d <- diagnose_design(nd)
  expect_length(d, 1L)
  expect_equal(d[[1]]$code, "insufficient_waves_for_lags")
  expect_equal(d[[1]]$level, "NOTE")
})

test_that("two or more waves produce no wave-adequacy finding", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2))
  ties <- data.frame(group = 1, source = "a", target = "b")
  nd <- netis_data(nodes, ties, wave_col = "wave")

  d <- diagnose_design(nd)
  expect_length(d, 0L)
})
