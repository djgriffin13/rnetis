# Shared fixtures for the `plot` feature group.
#
# testthat sources helper-*.R once per run, before any test file, so a fit
# cached here is fit once for the whole group rather than once per file, so
# several files do not each fit the same two models.
#
# Fits are lazy: a filtered run that touches none of them pays nothing.

.plot_fixture_influence <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- fit_influence(
        y ~ ar() + exposure(x, via = tie, normalize = "row"),
        data = demo_influence, waves = list(c(1, 2))
      )
    }
    cached
  }
})

# R = 1 so the multiplicative panel has latent factors to report. The additive
# panels do not care, so one fit serves both.
.plot_fixture_selection <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- fit_selection(
        tie ~ sender(x) + receiver(x), data = demo_selection,
        nscan = 200, burn = 50, odens = 10, seed = 1, R = 1
      )
    }
    cached
  }
})

# Derived fixtures, hoisted here for the same reason as the fits: more than one
# test file needs them, and selection_profile() generates posterior-predictive
# replicates, which is not cheap.

.plot_fixture_profile <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) cached <<- selection_profile(.plot_fixture_selection())
    cached
  }
})

.plot_fixture_r2 <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) cached <<- pseudo_r2(.plot_fixture_selection())
    cached
  }
})

# Read the references a plot table declared. Half of the check on references: what
# a table declared is compared against what the built plot drew.
.netis_references_of <- function(x) {
  out <- attr(x, "netis_references")
  if (is.null(out)) .netis_references() else out
}

# The reference annotations a built plot actually contains, read from the
# constructed ggplot rather than trusted from the code that built it. Returns the
# intercepts drawn, in layer order.
.netis_drawn_references <- function(p) {
  if (!inherits(p, "ggplot")) {
    stop("`p` must be a ggplot object.", call. = FALSE)
  }
  out <- numeric(0)
  for (lyr in p$layers) {
    geom <- class(lyr$geom)[1]
    if (!geom %in% c("GeomHline", "GeomVline", "GeomAbline")) next
    d <- lyr$data
    # Each reference geom names its position differently -- hline/vline carry
    # y/xintercept, abline carries `intercept`. Missing one of them would make
    # this reader return NA for a reference that IS drawn.
    field <- intersect(c("yintercept", "xintercept", "intercept"), names(d))
    v <- if (is.data.frame(d) && length(field) > 0L) d[[field[1]]] else NA_real_
    out <- c(out, as.numeric(v))
  }
  out
}
