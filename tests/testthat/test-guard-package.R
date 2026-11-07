# The guard group is the cross-cutting always-run set. This file exists from
# the first commit so the suite is never empty, and so a broken NAMESPACE or
# DESCRIPTION fails here rather than at R CMD check.

test_that("the package loads and declares the scope it documents", {
  expect_true(requireNamespace("rnetis", quietly = TRUE))

  desc <- utils::packageDescription("rnetis")
  expect_identical(desc$Package, "rnetis")

  # The R floor is load-bearing: CI is meant to test this exact version, not
  # just oldrel, so a silent bump here would quietly widen what we claim to
  # support. Compared as a fixed substring to keep the assertion readable.
  expect_true(grepl("R (>= 4.1)", desc$Depends, fixed = TRUE))
})
