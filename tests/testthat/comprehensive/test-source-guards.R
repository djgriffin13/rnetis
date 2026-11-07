# Guards that read the package source, so they live in this tier: it runs from
# a source checkout and is not part of the built package.

test_that("the package page lists every exported function by task", {
  root <- test_path("..", "..", "..")
  expect_true(file.exists(file.path(root, "NAMESPACE")))
  ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  exports <- gsub("[\"`]", "", sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE)))
  page <- paste(readLines(file.path(root, "R", "rnetis-package.R"), warn = FALSE), collapse = " ")
  listed <- vapply(exports, function(f) grepl(paste0("[", f, "()]"), page, fixed = TRUE), logical(1))
  expect_identical(names(listed)[!listed], character(0))
})

test_that("the package defines the %||% operator exactly once", {
  # The defect is structural rather than behavioral, so the guard is too: a
  # second definition anywhere in R/ introduces a collation dependency whatever
  # its semantics.
  r_dir <- test_path("..", "..", "..", "R")
  expect_true(dir.exists(r_dir))
  hits <- unlist(lapply(list.files(r_dir, pattern = "[.]R$", full.names = TRUE),
                        function(f) grep("^`%[|][|]%` *<-", readLines(f, warn = FALSE),
                                         value = TRUE)))
  expect_length(hits, 1L)
})
