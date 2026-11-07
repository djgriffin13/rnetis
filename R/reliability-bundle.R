# netis_reliability(): the ICC/agreement family in one call.
#
# The bundle exists because these quantities are read together, and it derives
# both ICCs from ONE one-way ANOVA of the fit's estimating sample. The agreement section is conditional on being handed ratings: a
# fitted model's variance components cannot reconstruct item-level agreement, so
# netis_reliability(fit) alone cannot manufacture rwg and does not pretend to.

#' Multilevel reliability and agreement for a fit
#'
#' Collects the one-way ANOVA reliability statistics — [icc1()] and [icc2()] — and,
#' when raw item ratings are supplied, the within-group agreement index
#' [rwg()]/[rwg_j()], in one object.
#'
#' # The three quantities, and why they are not interchangeable
#'
#' \describe{
#'   \item{ICC(1)}{Unconditional clustering: how much of the outcome's variance
#'     is associated with group membership *before* the substantive predictors
#'     explain anything. From a one-way ANOVA of the fit's estimating sample.}
#'   \item{ICC(2)}{Reliability of the group means, from the same ANOVA.}
#'   \item{r_wg}{Within-group *agreement* among raw ratings — a different
#'     question from either ICC, needing data no fitted model retains, and
#'     therefore only computed when you pass `ratings`.}
#' }
#'
#' None of these is the share reported by [netis_variance_components()], which is
#' the *conditional* partition of variance the fitted model did not explain. A
#' large ICC(1) beside a small conditional share is informative: it says the
#' predictors absorbed much of the between-group heterogeneity.
#'
#' Together they are evidence relevant to whether aggregating to the group level
#' is defensible. They do not settle it — theory and construct definition still
#' bear on that, and high agreement can coexist with low differentiation.
#'
#' @param fit a `netis_influence` fit.
#' @param group the grouping factor; see [icc1()].
#' @param overall passed to [icc2()]; `"n0"` by default, Bliese's
#'   `(MSB - MSW) / MSB`. The method and the `n` used are always reported
#'   alongside the value.
#' @param ratings optional raw item responses for the agreement section: a
#'   vector or one-column matrix or data frame for [rwg()], several columns
#'   for [rwg_j()]. Requires `null` and `rating_group`.
#' @param null the declared random-response variance for the agreement index.
#'   Required whenever `ratings` is supplied; see [rwg()].
#' @param rating_group grouping vector for `ratings`, one entry per rating
#'   (row). Required whenever `ratings` is supplied: the ratings are separate
#'   data and rnetis will not assume they align row-for-row with the
#'   estimating sample.
#' @param ... passed to [rwg()]/[rwg_j()] (e.g. `truncate`, `na_action`).
#' @return a `netis_reliability` object with `icc1`, `icc2`, `agreement`
#'   (`NULL` when no ratings were supplied) and `group`.
#' @references
#' Bliese, P. D. (2000). Within-group agreement, non-independence, and
#' reliability: Implications for data aggregation and analysis. In K. J. Klein
#' & S. W. J. Kozlowski (Eds.), *Multilevel Theory, Research, and Methods in
#' Organizations* (pp. 349-381). Jossey-Bass.
#'
#' James, L. R., Demaree, R. G., & Wolf, G. (1984). Estimating within-group
#' interrater reliability with and without response bias. *Journal of Applied
#' Psychology*, 69(1), 85-98.
#' @seealso [icc1()], [icc2()], [rwg()], [netis_variance_components()].
#' @examples
#' # Four groups of seven, two waves.
#' set.seed(4)
#' gs <- c("a", "b", "c", "d")
#' nodes <- do.call(rbind, lapply(gs, function(g) {
#'   ids <- sprintf("%s%02d", g, 1:7)
#'   data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = 7),
#'              y = stats::rnorm(14), x = stats::rnorm(14))
#' }))
#' ties <- do.call(rbind, lapply(gs, function(g) {
#'   ids <- sprintf("%s%02d", g, 1:7)
#'   data.frame(group = g, source = ids[-7], target = ids[-1], wave = 1, tie = 1)
#' }))
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(
#'   y ~ ar() + exposure(x, via = tie, normalize = "row") + (1 | group),
#'   data = nd
#' )
#' netis_reliability(fit)
#' @export
netis_reliability <- function(fit, group = NULL, overall = "n0",
                              ratings = NULL, null = NULL,
                              rating_group = NULL, ...) {
  # icc2() calls icc1() internally, so both come from one ANOVA.
  two <- icc2(fit, group = group, overall = overall)
  one <- two$icc1

  agreement <- NULL
  if (!is.null(ratings)) {
    if (is.null(rating_group)) {
      stop(
        "netis_reliability() needs `rating_group` alongside `ratings`: the ratings are separate data from the estimating sample, and rnetis will not assume they align row-for-row with it.",
        call. = FALSE
      )
    }
    agreement <- if (is.null(dim(ratings)) || NCOL(ratings) == 1L) {
      rwg(as.numeric(unlist(ratings, use.names = FALSE)), rating_group,
          null = null, ...)
    } else {
      rwg_j(ratings, rating_group, null = null, ...)
    }
  }

  structure(
    list(icc1 = one, icc2 = two, agreement = agreement, group = two$group),
    class = "netis_reliability"
  )
}

#' @export
print.netis_reliability <- function(x, ...) {
  cat(sprintf("Multilevel reliability for '%s'\n\n", x$group))
  print(x$icc1)
  cat("\n")
  print(x$icc2)
  if (is.null(x$agreement)) {
    cat("\nAgreement: not computed. r_wg needs raw item ratings and a declared\n")
    cat("  null variance, neither recoverable from a fitted model -- pass\n")
    cat("  ratings = and null = to include it.\n")
  } else {
    cat("\n")
    print(x$agreement)
  }
  invisible(x)
}
