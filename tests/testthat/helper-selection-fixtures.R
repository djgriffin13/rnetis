# Fixtures for the Selection tests.
#
# Fits are cached because most of these tests ask about the same small fit from
# different angles, and refitting per test dominates the suite's runtime.

#' A small hierarchical fit with one random coefficient, cached
#'
#' `flat_group` makes one group's sender covariate constant, which is the
#' weak-information case: that group can say nothing about its own coefficient
#' directly, and partial pooling has to carry it.
#'
#' @noRd
.hierarchical_fixture_fit <- function(random = "snd_x", flat_group = NULL,
                                      n_groups = 3, n_actors = 10) {
  key <- paste("hier", random, flat_group %||% "none", n_groups, n_actors,
               sep = "_")
  .cached_fixture(key, {
    d <- .selection_fixture_data("bin", n_groups = n_groups, n_actors = n_actors,
                                 n_waves = 2)
    if (!is.null(flat_group)) {
      nodes <- netis_nodes(d)
      nodes$x[nodes$group == flat_group] <- 0
      d <- netis_data(nodes, netis_ties(d), wave_col = "wave",
                      tie_meta = list(tie = list(zero_meaning = "measured_zero")))
    }
    fit_selection(tie ~ prior_tie() + sender(x), data = d, family = "bin", R = 1,
                  pooling = selection_pooling(random = random),
                  nscan = 200, burn = 50, odens = 10)
  })
}

#' A fit with several independent random coefficients, cached
#'
#' @noRd
.multi_random_fixture_fit <- function(random = c("snd_x", "prior_tie"),
                                      n_groups = 4, n_actors = 10) {
  key <- paste("multi", paste(random, collapse = "+"), n_groups, n_actors, sep = "_")
  .cached_fixture(key, {
    fit_selection(
      tie ~ prior_tie() + sender(x),
      data = .selection_fixture_data("bin", n_groups = n_groups,
                                     n_actors = n_actors, n_waves = 2),
      family = "bin", R = 1, pooling = selection_pooling(random = random),
      nscan = 200, burn = 50, odens = 10
    )
  })
}

#' Groups whose random sender slope is a known function of a group-level moderator
#'
#' `slope(z)` gives group g's true coefficient on its sender covariate, as a
#' function of that group's moderator value. The moderator is returned as
#' `attr(., "z")`, so a test asserts against the value that generated the data
#' rather than re-deriving it.
#'
#' @noRd
.mgc_moderator_groups <- function(G = 3, n = 8, seed = 1,
                                  slope = function(z) 0.5, alpha = -0.3) {
  set.seed(seed)
  gids <- letters[seq_len(G)]
  # Spread over [-1, 1] deterministically: a moderator drawn at random would
  # make the between-group contrast differ run to run, and at these group
  # counts that is the dominant source of variation in what the test sees.
  z <- if (G == 1L) 0 else seq(-1, 1, length.out = G)
  names(z) <- gids
  out <- stats::setNames(lapply(seq_len(G), function(g) {
    x <- stats::rnorm(n)
    eta <- alpha + slope(z[g]) * matrix(x, n, n) +
      outer(stats::rnorm(n, 0, 0.3), stats::rnorm(n, 0, 0.3), "+")
    Y <- 1 * (matrix(stats::rnorm(n * n), n) < eta)
    diag(Y) <- NA
    list(Y = Y, Xrow = matrix(x, n, 1, dimnames = list(NULL, "x")))
  }), gids)
  attr(out, "z") <- z
  out
}

#' A hierarchical fit with a group-level moderator, cached
#'
#' `binary` picks whether the moderator is an indicator (reference change 1 by
#' rule) or continuous (reference change from the group values, and flagged).
#'
#' @noRd
.moderated_fixture_fit <- function(binary = FALSE, n_groups = 4, n_actors = 8) {
  key <- paste("mod", binary, n_groups, n_actors, sep = "_")
  .cached_fixture(key, {
    d <- .selection_fixture_data("bin", n_groups = n_groups, n_actors = n_actors,
                                 n_waves = 2)
    gs <- sort(unique(netis_nodes(d)$group))
    z <- if (binary) rep(c(0, 1), length.out = length(gs)) else seq_along(gs) - 1
    mod <- data.frame(group = gs, size = as.numeric(z), stringsAsFactors = FALSE)
    fit_selection(tie ~ prior_tie() + sender(x), data = d, family = "bin", R = 0,
                  pooling = selection_pooling(random = "snd_x", moderators = mod),
                  nscan = 150, burn = 50, odens = 10, seed = 4)
  })
}

#' A fit with a correlated between-group covariance, cached
#'
#' @noRd
.correlated_fixture_fit <- function(n_groups = 4, n_actors = 8) {
  .cached_fixture(paste("corr", n_groups, n_actors, sep = "_"), {
    d <- .selection_fixture_data("bin", n_groups = n_groups, n_actors = n_actors,
                                 n_waves = 2)
    fit_selection(tie ~ prior_tie() + sender(x), data = d, family = "bin", R = 0,
                  pooling = selection_pooling(random = c("snd_x", "prior_tie"),
                                              covariance = "unstructured"),
                  nscan = 150, burn = 50, odens = 10, seed = 3)
  })
}
