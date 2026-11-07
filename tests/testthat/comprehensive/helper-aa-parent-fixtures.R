# Make the tier-1 fixture helpers visible to the comprehensive tier.
#
# testthat sources `helper*.R` from the directory it is running, and it does
# not recurse. tests/testthat/helper-*.R are therefore invisible here, even
# though most of this directory's files were written against them
# (.selection_fixture_fit, .plot_fixture_*, .cached_fixture, fit_uncorrected).
# Copying them would create two divergent definitions of the same fixture --
# the failure mode where a test passes in one tier and not the other for a
# reason nobody can see -- so this sources the one copy instead.
#
# THE PATH IS RESOLVED BY SEARCH, NOT BY COUNTING `..`. This has bitten before:
# a helper here located its target with a fixed number of `..`, the directory
# depth changed, the path stopped resolving, and its guard turned 35 tests
# into skips that still reported green. A search either finds the directory
# or fails loudly, and cannot silently resolve to the wrong one.

.parent_helper_files <- function(dir) {
  f <- list.files(dir, pattern = "^helper-", full.names = TRUE)
  sort(f[endsWith(f, ".R")])
}

.parent_testthat_dir <- local({
  candidates <- c(
    testthat::test_path(".."),
    file.path(getwd(), ".."),
    "tests/testthat"
  )
  found <- NULL
  for (cand in candidates) {
    if (dir.exists(cand) && length(.parent_helper_files(cand))) {
      found <- normalizePath(cand, winslash = "/")
      break
    }
  }
  if (is.null(found)) {
    stop(
      "comprehensive/: cannot locate tests/testthat/ to load the shared fixture ",
      "helpers from. Looked in: ", paste(candidates, collapse = ", "), ". ",
      "This is a hard error on purpose -- skipping instead would hide every ",
      "test in this directory that depends on a fixture.",
      call. = FALSE
    )
  }
  found
})

for (.f in .parent_helper_files(.parent_testthat_dir)) {
  source(.f, local = TRUE)
}
rm(.f)
