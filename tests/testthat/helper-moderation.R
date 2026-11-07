# Shared fixture for the moderation tests. Lives in a helper-*.R file
# (not inline in one test-*.R file) specifically so it is visible regardless
# of which test file testthat runs first -- test-influence-conditional-effects.R
# sorts alphabetically before test-influence-interactions.R, so defining
# .moderation_fixture() inside the latter left the former unable to find it
# outside a full-suite run in a fortunate order.
#
# Memoized: built once per session, not once per test_that() block. Every
# call site assigns the result to a local `d` and any mutation (e.g. a
# missingness test's NA injection) copies-on-write under R's usual
# semantics, so sharing the cached object across tests is safe -- and it is
# what turns many netis_data() constructions into one: fewer, better-shared
# fixtures, because the suite's cost is fixture overhead rather than any single
# expensive test.
.moderation_cache <- new.env(parent = emptyenv())

.moderation_fixture <- function(n = 8L, seed = 41L) {
  key <- paste(n, seed)
  if (!is.null(.moderation_cache[[key]])) {
    return(.moderation_cache[[key]])
  }
  set.seed(seed)
  actors <- sprintf("n%02d", seq_len(n))
  waves <- 1:3
  nodes <- expand.grid(node = actors, wave = waves, stringsAsFactors = FALSE)
  nodes$group <- 1L
  nodes <- nodes[order(nodes$wave, nodes$node), ]
  nodes$y <- stats::rnorm(nrow(nodes))
  nodes$x <- stats::rnorm(nrow(nodes))
  static <- data.frame(group = 1L, node = actors, z = stats::rnorm(n), stringsAsFactors = FALSE)
  ties <- data.frame(group = 1L, source = actors, target = c(actors[-1], actors[1]),
                     tie = 1L, stringsAsFactors = FALSE)
  d <- netis_data(nodes, ties, wave_col = "wave")
  d$nodes$static <- merge(d$nodes$static, static, by = c("group", "node"), all.x = TRUE)
  d$schema$node_vars$static <- "z"
  .moderation_cache[[key]] <- d
  d
}
