# RNG hygiene for seeded draws.
#
# ONE FUNCTION, because one is all that is needed. `simulate_influence()`
# draws throughout its body rather than inside a wrapped expression, so it
# needs the restoration registered against its own frame. A seeded igraph
# layout, the amen backend and the hierarchical sampler use it the same way,
# so no call leaves the caller's stream reset.

#' Seed the stream for the rest of a call, and restore it when that call ends
#'
#' An expression wrapper is right when the drawing happens in one place;
#' `simulate_influence()` draws throughout its body, so it needs the
#' restoration registered against ITS frame rather than a wrapper's.
#'
#' WHY A NAMED HELPER. Keeping this logic in one definition means there is
#' only one place deciding whether a seed argument is scoped or global, so
#' no two simulators can disagree about it.
#'
#' @param seed the seed to set.
#' @param parent the frame whose exit should restore the stream.
#' @return `NULL`, invisibly. Called for its effect on the RNG stream.
#' @noRd
.sim_restore_stream_on_exit <- function(seed, parent = parent.frame()) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  do.call(
    base::on.exit,
    list(
      substitute({
        if (had_) {
          assign(".Random.seed", old_, envir = globalenv())
        } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
          rm(".Random.seed", envir = globalenv())
        }
      }, list(had_ = had, old_ = old)),
      add = TRUE
    ),
    envir = parent
  )
  set.seed(seed)
  invisible(NULL)
}
