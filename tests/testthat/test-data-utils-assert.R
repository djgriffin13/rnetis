# Dedicated tests for the shared assertion helpers (R/utils-assert.R). Every
# other test file exercises these only incidentally (e.g. via a malformed
# `directed` argument), never with the actual boundary cases (NA, length > 1,
# NULL, multiple missing columns) these guards exist to catch. A regression
# in any of these would silently corrupt schema flags across the package.
#
# NOT IN `guard`. These are unit tests for one source file: they break when
# R/utils-assert.R changes and not otherwise, which is the property the guard
# group is explicitly NOT for. Guard is the cross-cutting always-run set -- a
# statistical invariant, a known regression, or something that breaks in
# response to changes anywhere.

# ---- .assert_columns_present ------------------------------------------------

test_that(".assert_columns_present() passes when every column is present", {
  df <- data.frame(a = 1, b = 2)
  expect_true(.assert_columns_present(df, c("a", "b"), "test data"))
})

test_that(".assert_columns_present() errors naming every missing column", {
  df <- data.frame(a = 1)
  expect_error(.assert_columns_present(df, c("a", "b", "c"), "test data"), "b, c")
})

# ---- .assert_unique_key -----------------------------------------------------

test_that(".assert_unique_key() passes for a genuinely unique key", {
  df <- data.frame(id = c("a", "b", "c"))
  expect_true(.assert_unique_key(df, "id", "test data"))
})

test_that(".assert_unique_key() errors on a duplicated key, naming the count", {
  df <- data.frame(id = c("a", "a", "b"))
  expect_error(.assert_unique_key(df, "id", "test data"), "duplicate")
})

test_that(".assert_unique_key() checks the composite key, not each column independently", {
  # (1,a) and (2,a) share column "b" but are a unique composite key.
  df <- data.frame(a = c(1, 2), b = c("x", "x"))
  expect_true(.assert_unique_key(df, c("a", "b"), "test data"))
})

# ---- .assert_enum -----------------------------------------------------------

test_that(".assert_enum() passes for a valid single value", {
  expect_true(.assert_enum("a", c("a", "b"), "arg"))
})

test_that(".assert_enum() rejects a value not in the choice set", {
  expect_error(.assert_enum("z", c("a", "b"), "arg"), "arg")
})

test_that(".assert_enum() rejects NULL", {
  expect_error(.assert_enum(NULL, c("a", "b"), "arg"))
})

test_that(".assert_enum() rejects a length > 1 vector even if every element is valid", {
  expect_error(.assert_enum(c("a", "b"), c("a", "b"), "arg"))
})

test_that(".assert_enum() rejects a non-character value", {
  expect_error(.assert_enum(1, c("a", "b"), "arg"))
})

# ---- .assert_flag -----------------------------------------------------------

test_that(".assert_flag() passes for TRUE and FALSE", {
  expect_true(.assert_flag(TRUE, "arg"))
  expect_true(.assert_flag(FALSE, "arg"))
})

test_that(".assert_flag() rejects NA", {
  expect_error(.assert_flag(NA, "arg"), "arg")
})

test_that(".assert_flag() rejects a length > 1 logical vector", {
  expect_error(.assert_flag(c(TRUE, FALSE), "arg"))
})

test_that(".assert_flag() rejects a non-logical value", {
  expect_error(.assert_flag(1, "arg"))
  expect_error(.assert_flag("TRUE", "arg"))
})

# --- The null-coalescing operator has ONE definition -------------------------
#
# It was defined three times, and the third (R/study-summarize.R) also fell
# through on a length-1 NA. With no Collate field in DESCRIPTION, R sources R/
# alphabetically, so that last definition was the one the whole package bound:
# every `x %||% default` in rnetis treated a scalar NA as absent. Which
# definition applied was decided by a filename, which is why R/study-runner.R
# had a call written out longhand to avoid depending on it.

test_that("`%||%` falls back on NULL only, and NA is a value", {
  expect_identical(NULL %||% "fallback", "fallback")
  expect_identical("value" %||% "fallback", "value")

  # THE ASSERTION THAT MATTERS. NA means "measured, and not known"; it is not
  # the same as "not recorded", and a statistic that came back NA must not be
  # silently replaced by a default.
  expect_true(is.na(NA %||% "fallback"))
  expect_true(is.na(NA_real_ %||% 0))
  expect_identical(c(NA, 1) %||% "fallback", c(NA, 1))
})
