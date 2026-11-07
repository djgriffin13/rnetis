#' Run design diagnostics on a netis_data object
#'
#' Checks whether the data's temporal structure can support the kinds of models
#' `rnetis` is built around, independent of any particular model specification.
#' Most design diagnostics require a resolved model specification --
#' contemporaneous-versus-lagged exposure, identification of the requested
#' effect -- and are reported by the fit, on its `diagnostics`. This function
#' checks the one design property that depends on `netis_data` alone: whether
#' the data has at least two waves.
#'
#' @param x a [netis_data()] object.
#' @return a `netis_diagnostics` object; see [netis-diagnostics]. Its `level`
#'   values (`ERROR`, `WARNING`, `NOTE`) are severities recorded on findings,
#'   not R conditions: this function reports and does not refuse.
#' @seealso [diagnose_data()], [netis-diagnostics]
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b"))
#' ties <- data.frame(group = 1, source = "a", target = "b")
#' nd <- netis_data(nodes, ties)
#' diagnose_design(nd)  # single wave -> a NOTE about lagged terms
#' @export
diagnose_design <- function(x) {
  .check_netis_data(x)
  .combine_diagnostics(
    .check_wave_adequacy(x)
  )
}

#' @noRd
.check_wave_adequacy <- function(x) {
  s <- netis_schema(x)
  n_waves <- length(s$waves)
  if (n_waves >= 2L) {
    return(list())
  }
  list(.finding(
    "NOTE", "design", "insufficient_waves_for_lags",
    sprintf(
      "Only %d distinct wave%s present in this data. Autoregressive (ar()) and lagged exposure() terms require at least two waves to estimate; models that only use static or contemporaneous terms are unaffected.",
      n_waves, if (n_waves == 1L) "" else "s"
    ),
    details = list(waves = s$waves)
  ))
}
