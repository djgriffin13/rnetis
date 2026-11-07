#' Evaluate B-I exposure across a grid of its parameters
#'
#' `delta`, `alpha` and `gamma` are supplied to [bi()], never estimated from
#' data — Frank and Fahrbach specify them. The appropriate
#' empirical practice is therefore to compute exposure at several values and
#' report how conclusions move, which is what this does: it calls
#' [compute_exposure()] once per combination and stacks the results.
#'
#' This is sensitivity analysis over a published model's own parameters, not a
#' method. **It deliberately offers no way to choose a "best" combination.**
#' Ranking the grid by fit would be a data-driven estimator for parameters the
#' source specifies, and rnetis provides none. The sweep itself is this package's
#' convenience; the paper does not prescribe one.
#'
#' @param data a [netis_data()] object.
#' @param content the node variable carrying the exogenous input, `e` in the
#'   published equations. See [bi()] on what this must be.
#' @param network the conduit: a tie-variable name, or a network specification, as
#'   [compute_exposure()]'s `via`.
#' @param alpha,gamma,delta numeric vectors of values to evaluate. Every
#'   combination of the three is computed. `alpha` and `gamma` have no default;
#'   `delta` defaults to `c(0, 0.5, 1)`.
#' @param ... passed to [compute_exposure()] — `wave`, for instance.
#' @return a data frame with `delta`, `alpha` and `gamma` recording which
#'   combination produced each row, followed by the columns [compute_exposure()]
#'   returns, stacked over the grid.
#' @references
#' Frank, K. A. and Fahrbach, K. (1999) Organization culture as a complex system:
#' balance and information in models of influence and selection. *Organization
#' Science* **10**(3), 253-277.
#'
#' Frank, K. A. and Fahrbach, K. (1999) Erratum: Organizational culture as a
#' complex system: balance and information in models of influence and selection.
#' *Organization Science* **10**(4), 514-515.
#' @seealso [bi()], [bi_components()], [compute_exposure()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = rep(c("a", "b", "c", "d"), 3), wave = rep(1:3, each = 4),
#'   shock = c(10, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0)
#' )
#' ties <- data.frame(
#'   group = 1, source = c("a", "a", "c", "d"), target = c("c", "d", "b", "b"),
#'   wave = c(1, 1, 2, 2), link = 1
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#'
#' # How much does b's exposure depend on the assumed mix of redundant paths?
#' sweep <- bi_sweep(nd, "shock", "link", alpha = 1, gamma = 0,
#'                   delta = c(0, 0.5, 1), wave = 3)
#' sweep[sweep$node == "b", c("delta", "alpha", "gamma", "ex_shock_link")]
#' @export
bi_sweep <- function(data, content, network, alpha, gamma,
                     delta = c(0, 0.5, 1), ...) {
  .check_netis_data(data)
  for (nm in c("delta", "alpha", "gamma")) {
    v <- get(nm)
    if (!is.numeric(v) || length(v) < 1L || anyNA(v)) {
      stop(sprintf("%s must be a non-missing numeric vector of one or more values.", nm),
           call. = FALSE)
    }
  }

  grid <- expand.grid(delta = delta, alpha = alpha, gamma = gamma,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  pieces <- lapply(seq_len(nrow(grid)), function(i) {
    out <- compute_exposure(
      data, content, network,
      mechanism = bi(delta = grid$delta[i], alpha = grid$alpha[i],
                     gamma = grid$gamma[i]),
      ...
    )
    # The grid columns go first so a printed result reads as "at these
    # assumptions, this exposure" rather than the other way round.
    cbind(grid[rep(i, nrow(out)), , drop = FALSE], out, row.names = NULL)
  })

  do.call(rbind, pieces)
}
