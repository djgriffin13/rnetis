# Fixture for the formula-completeness guard.
#
# Deliberately tiny and deliberately FITTABLE. Every assertion in
# test-guard-formula-completeness.R is about whether the parser refuses, so
# the fixture has to be one where an unmoderated model succeeds -- otherwise a
# passing `expect_error()` proves nothing about the interaction and a failing
# `expect_no_error()` cannot be distinguished from a data problem.
#
# Two moderators (`z` static, `w` dynamic) because the three-way case needs a
# third variable, and because a static and a dynamic moderator resolve through
# different schema paths; if the completeness check ever became level-aware it
# should be visible here first.

#' Small two-group, three-wave data with two moderators
#' @noRd
.completeness_fixture <- function(seed = 41L) {
  set.seed(seed)
  actors <- sprintf("n%02d", 1:8)
  waves <- 1:3

  nodes <- expand.grid(node = actors, wave = waves, stringsAsFactors = FALSE)
  nodes$group <- 1L
  nodes <- nodes[order(nodes$wave, nodes$node), ]
  nodes$y <- stats::rnorm(nrow(nodes))
  nodes$x <- stats::rnorm(nrow(nodes))
  nodes$w <- stats::rnorm(nrow(nodes))

  static <- data.frame(
    group = 1L, node = actors,
    z = stats::rnorm(length(actors)),
    stringsAsFactors = FALSE
  )

  # A ring, so every actor has exactly one alter and no exposure is undefined.
  ties <- data.frame(
    group = 1L,
    source = actors,
    target = c(actors[-1], actors[1]),
    tie = 1L,
    stringsAsFactors = FALSE
  )

  d <- netis_data(nodes, ties, wave_col = "wave")
  d$nodes$static <- merge(d$nodes$static, static,
                          by = c("group", "node"), all.x = TRUE)
  d$schema$node_vars$static <- "z"
  d
}
