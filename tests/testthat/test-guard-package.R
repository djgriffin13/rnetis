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
  #
  # 4.2 RATHER THAN 4.1, AND NOT BECAUSE OF OUR OWN CODE. rnetis runs on 4.1
  # and so does every required package. The floor is set by an optional one:
  # latentnet, which latent() needs, depends on ergm, and ergm requires
  # R >= 4.2. Declaring 4.2 means an install that succeeds can run every
  # capability rather than all but one. Raise this only with the CI matrix in
  # .github/workflows/R-CMD-check.yaml, which pins the same version.
  expect_true(grepl("R (>= 4.2)", desc$Depends, fixed = TRUE))
})
