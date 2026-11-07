# Empirical collinearity diagnostics for centrality()/network_stat() terms
# sharing a network with an exposure() term.
#
# Deliberately NOT specification-based: sharing a network between a
# centrality() and an exposure() term is not itself a defect (for a binary
# network, degree *is* the denominator of row-normalized exposure; for a
# weighted network, strength is -- but plenty of sound models legitimately
# use both), so warning on the specification alone would fire constantly and
# train users to ignore warnings. This runs on the *assembled* table and
# reports the measured Pearson correlation between column pairs, only
# flagging what is empirically high.

#' A package convention, not a published cutoff.
#' @noRd
.collinearity_warn_threshold <- 0.9

#' Flag empirically high correlation between centrality()/network_stat()
#' columns and exposure() columns in the assembled design table
#'
#' @param table the assembled analysis table (`.assemble_influence_table()`'s
#'   `table`).
#' @param structural_cols names of `centrality()`/`network_stat()` columns.
#' @param exposure_cols names of `exposure()` columns.
#' @noRd
.diagnose_influence_collinearity <- function(table, structural_cols, exposure_cols) {
  findings <- list()
  if (length(structural_cols) == 0L || length(exposure_cols) == 0L) {
    return(.combine_diagnostics(findings))
  }
  for (sc in structural_cols) {
    for (ec in exposure_cols) {
      x <- table[[sc]]
      y <- table[[ec]]
      if (is.null(x) || is.null(y)) {
        next
      }
      complete <- stats::complete.cases(x, y)
      if (sum(complete) < 3L || stats::sd(x[complete]) == 0 || stats::sd(y[complete]) == 0) {
        next
      }
      r <- stats::cor(x[complete], y[complete])
      if (!is.na(r) && abs(r) >= .collinearity_warn_threshold) {
        findings <- c(findings, list(.finding(
          "WARNING", "design", "structural_exposure_collinearity",
          sprintf(
            "'%s' and '%s' are highly correlated in the assembled data (r = %.3f). This is common when a centrality()/network_stat() term shares a network with an exposure() term (degree is the denominator of row-normalized exposure for a binary network; strength for a weighted one) -- sharing a network is not itself a problem, but coefficients on these two terms may be difficult to interpret separately here. Consider dropping one, or interpreting them jointly with this in mind.",
            sc, ec, r
          ),
          details = list(term_a = sc, term_b = ec, correlation = r)
        )))
      }
    }
  }
  .combine_diagnostics(findings)
}
