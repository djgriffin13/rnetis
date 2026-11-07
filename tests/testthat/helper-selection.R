# Shared, memoized Selection fixtures.
#
# Every Selection test needs a fitted model, and an MCMC fit is the most
# expensive thing in the suite. Fitting one per test would make the suite slow
# for no gain: the tests assert different things about the SAME fit, so
# refitting buys nothing but check time.
#
# Memoized per session rather than per test. testthat runs a file's tests in
# one process, so a fit computed for the first test that needs it is reused by
# the rest. Keys include every argument that changes the fit, so two tests
# asking for different models still get different models.
#
# BUILT BY HAND. Nothing in the tests that use these fixtures asserts a
# recovered parameter -- they check structure, internal consistency, and
# which estimand was computed -- so a fixed dataset serves exactly as well.

.netis_fixture_cache <- new.env(parent = emptyenv())

#' Fit once per session, reuse thereafter
#'
#' @param key a string identifying the fit; must vary with every argument that
#'   changes it.
#' @param expr the expression producing the fit, evaluated at most once.
#' @noRd
.cached_fixture <- function(key, expr) {
  if (!exists(key, envir = .netis_fixture_cache, inherits = FALSE)) {
    assign(key, force(expr), envir = .netis_fixture_cache)
  }
  get(key, envir = .netis_fixture_cache, inherits = FALSE)
}

#' The `netis_data` those fixtures fit
#'
#' Two waves so `prior_tie()` has an initial condition to reach back to, and a
#' node covariate `x` for `sender()`. `family` decides whether the tie variable
#' is binary or valued -- a probit needs a 0/1 outcome, and a normal fit needs
#' something with genuine spread.
#'
#' @noRd
.selection_fixture_data <- function(family = "bin", n_groups = 1,
                                    n_actors = 12, n_waves = 2, seed = 31L) {
  set.seed(seed)
  # Groups labeled 1..n, so tests can name a group by number.
  gs <- as.character(seq_len(n_groups))
  nodes <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("g%s_%02d", g, seq_len(n_actors))
    data.frame(group = g, node = rep(ids, n_waves),
               wave = rep(seq_len(n_waves), each = n_actors),
               x = stats::rnorm(n_actors * n_waves),
               stringsAsFactors = FALSE)
  }))
  ties <- do.call(rbind, lapply(gs, function(g) {
    ids <- sprintf("g%s_%02d", g, seq_len(n_actors))
    p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
    p <- p[p$source != p$target, ]
    do.call(rbind, lapply(seq_len(n_waves), function(w) {
      data.frame(
        group = g, p, wave = w,
        tie = if (identical(family, "nrm")) {
          stats::rnorm(nrow(p))
        } else {
          stats::rbinom(nrow(p), 1, 0.25)
        },
        stringsAsFactors = FALSE
      )
    }))
  }))
  netis_data(nodes, ties, wave_col = "wave",
             tie_meta = list(tie = list(zero_meaning = "measured_zero")))
}

#' A small fitted Selection model, cached
#'
#' Chains are short on purpose. These tests exercise wiring, reporting, and
#' arithmetic -- none of them assert anything a longer chain would change.
#'
#' @noRd
.selection_fixture_fit <- function(family = "bin", n_groups = 1, n_actors = 12,
                                   n_waves = 2, formula = NULL) {
  key <- paste("fit", family, n_groups, n_actors, n_waves,
               if (is.null(formula)) "default" else deparse(formula),
               sep = "_")
  .cached_fixture(key, {
    fit_selection(if (is.null(formula)) tie ~ prior_tie() else formula,
                  data = .selection_fixture_data(family, n_groups, n_actors,
                                                 n_waves),
                  family = family, nscan = 200, burn = 50, odens = 10)
  })
}
