# Within-group AGREEMENT: r_wg and r_wg(j) (James, Demaree & Wolf 1984).
#
# Agreement is not reliability, and this file is deliberately separate from
# reliability-icc.R to keep that visible. ICC(1)/ICC(2) ask whether groups are
# reliably DIFFERENTIATED from one another; rwg asks whether the members of one
# group AGREE with each other. They can disagree in both directions -- high rwg
# with low ICC(1) is an ordinary, interpretable pattern -- so neither substitutes
# for the other and rnetis must not present them as alternatives to one another.
#
# These take raw item responses rather than a fit: agreement is a property of
# the ratings, and nothing in a fitted model's variance components can
# reconstruct it.

#' Resolve the declared null variance
#'
#' The null is the whole assumption in an agreement index -- rwg is
#' `1 - observed_variance / null_variance`, so the null decides the answer.
#' It therefore has NO default: an implicit uniform null is exactly the
#' kind of silent modeling choice this package refuses to make on a user's
#' behalf.
#'
#' @noRd
.resolve_rwg_null <- function(null) {
  if (is.null(null)) {
    stop(
      "rwg()/rwg_j() need an explicit `null =`: the expected variance of a random response. There is no default, because the null decides the answer and a hidden one would be an undeclared modeling assumption. Supply a numeric variance, or list(dist = \"uniform\", n_options = A) for the rectangular null over A response options.",
      call. = FALSE
    )
  }
  if (is.numeric(null)) {
    if (any(!is.finite(null)) || any(null <= 0)) {
      stop("A numeric `null =` must be a positive, finite variance.", call. = FALSE)
    }
    # ONE VARIANCE, NOT ONE PER ITEM. Accepting a vector would compute the
    # r_wg(j) ratio as the mean of per-item observed-to-null ratios. That is a
    # coherent quantity and it is NOT the published index: James, Demaree &
    # Wolf (1984) define r_WG(J) against a single expected random-response
    # variance sigma^2_EU common to all J items, and the Sbar term is the mean
    # observed item variance over that one null. Restricted, because shipping
    # a generalization under the published name is what this package exists
    # not to do.
    if (length(null) != 1L) {
      stop(sprintf(
        "`null =` must be a single variance; %d values were given. r_wg and r_wg(j) are defined against ONE expected random-response variance common to every item (James, Demaree & Wolf 1984), so a per-item null computes a different index than the one this function is named for. If the items genuinely have different response scales, they do not share a null and the index does not apply to them jointly -- compute it per scale instead.",
        length(null)), call. = FALSE)
    }
    return(null)
  }
  if (is.list(null) && identical(null$dist, "uniform")) {
    A <- null$n_options
    if (is.null(A) || !is.numeric(A) || length(A) != 1L || A < 2) {
      stop("A uniform null needs n_options: list(dist = \"uniform\", n_options = A) with A >= 2 response options.", call. = FALSE)
    }
    # Variance of a discrete uniform on A equally-spaced options.
    return((A^2 - 1) / 12)
  }
  stop(
    "`null =` must be a single positive numeric variance, or list(dist = \"uniform\", n_options = A).",
    call. = FALSE
  )
}

#' @noRd
.rwg_handle_missing <- function(x, na_action) {
  if (!anyNA(x)) return(x)
  if (is.null(na_action)) {
    stop(sprintf(
      "The ratings contain %d missing value(s) and no `na_action` was declared. Whether an incomplete respondent is dropped changes the observed variance and therefore the agreement index, so rnetis will not choose for you: pass na_action = \"omit\" to drop incomplete cases, or handle them before calling.",
      sum(is.na(x))), call. = FALSE)
  }
  if (!identical(na_action, "omit")) {
    stop("na_action must be \"omit\", or NULL to be told how many are missing.", call. = FALSE)
  }
  x
}

#' Within-group agreement r_wg (single item)
#'
#' `rwg = 1 - observed_group_variance / null_variance`, per group.
#'
#' # This is agreement, not reliability
#'
#' High agreement within groups and low differentiation *between* them can
#' coexist, so `rwg` is not an alternative reading of [icc1()]/[icc2()] and
#' neither implies the other.
#'
#' # The null is yours to declare
#'
#' There is no default `null`. The index is a ratio against it, so an implicit
#' null would silently decide the result.
#'
#' # Truncation
#'
#' `truncate = TRUE` (the convention) floors negative values at zero, so
#' published numbers reproduce. It is recorded on the result because it is a
#' real choice: an observed variance *exceeding* the null is a substantive
#' signal — systematic disagreement — and truncating discards it while biasing
#' the average agreement upward. Pass `truncate = FALSE` to see it.
#'
#' @param ratings numeric vector of one item's responses.
#' @param group grouping vector, the same length as `ratings`.
#' @param null the expected random-response variance: a positive number, or
#'   `list(dist = "uniform", n_options = A)`. Required.
#' @param truncate floor negative values at zero. Recorded either way.
#' @param na_action `"omit"`, or `NULL` to be refused with a count when any
#'   response is missing.
#' @return a `netis_rwg` object with a `by_group` data frame (`group`, `n`,
#'   `observed_var`, `rwg`) and the settings used.
#' @references James, L. R., Demaree, R. G., & Wolf, G. (1984). Estimating
#'   within-group interrater reliability with and without response bias.
#'   *Journal of Applied Psychology*, 69(1), 85-98.
#' @seealso [rwg_j()] for multi-item scales, [icc1()] for the reliability side.
#' @examples
#' set.seed(1)
#' ratings <- c(rnorm(10, 4, 0.3), rnorm(10, 3, 1.4))
#' teams <- rep(c("a", "b"), each = 10)
#' rwg(ratings, teams, null = list(dist = "uniform", n_options = 5))
#' @export
rwg <- function(ratings, group, null = NULL, truncate = TRUE, na_action = NULL) {
  if (!is.numeric(ratings)) stop("ratings must be numeric.", call. = FALSE)
  if (length(group) != length(ratings)) {
    stop("group must be the same length as ratings.", call. = FALSE)
  }
  .assert_flag(truncate, "truncate")
  null_var <- .resolve_rwg_null(null)

  ratings <- .rwg_handle_missing(ratings, na_action)
  # A MISSING GROUP LABEL GOES THROUGH THE SAME POLICY as a missing rating.
  # A rater whose group is unknown cannot be placed in one, and
  # dropping them changes the within-group variance the index is computed
  # from -- which is exactly what `na_action` exists to make the caller decide.
  if (anyNA(group) && !identical(na_action, "omit")) {
    stop(sprintf(
      "The group labels contain %d missing value(s) and no `na_action` was declared. A rater whose group is unknown cannot be placed in one, and dropping them changes the within-group variance the agreement index is computed from, so rnetis will not choose for you: pass na_action = \"omit\" to drop them, or resolve the labels before calling.",
      sum(is.na(group))), call. = FALSE)
  }
  keep <- !is.na(ratings) & !is.na(group)
  ratings <- ratings[keep]
  group <- group[keep]

  parts <- split(ratings, group)
  out <- do.call(rbind, lapply(names(parts), function(g) {
    v <- parts[[g]]
    # A single respondent has no within-group variance to compare against the
    # null: the index is undefined rather than perfect agreement, which is what
    # var() returning NA already says and is left to say.
    obs <- if (length(v) < 2L) NA_real_ else stats::var(v)
    value <- 1 - obs / null_var
    if (truncate && is.finite(value)) value <- max(value, 0)
    data.frame(group = g, n = length(v), observed_var = obs, rwg = value,
               stringsAsFactors = FALSE)
  }))

  structure(
    list(by_group = out, null_variance = null_var, truncate = truncate,
         n_items = 1L),
    class = "netis_rwg"
  )
}

#' Within-group agreement r_wg(j) for a multi-item scale
#'
#' The James, Demaree & Wolf (1984) multi-item extension:
#' `rwg_j = (J * (1 - Sbar/nullvar)) / (J * (1 - Sbar/nullvar) + Sbar/nullvar)`
#' where `Sbar` is the mean observed item variance within the group and `J` the
#' number of items.
#'
#' # Truncation
#'
#' With `truncate = TRUE` (the convention), a group whose mean observed item
#' variance is at or above the null (`Sbar / nullvar >= 1`) is assigned
#' `rwg_j = 0`: the observed variance is no smaller than the random-response
#' variance, so the group shows no agreement beyond it. The rule is applied to
#' the variance ratio, not to the formula's output: for `Sbar / nullvar` above
#' 1 the formula is negative, undefined at `J / (J - 1)`, and greater than 1
#' beyond it, so flooring its output would report a polarised group as
#' near-perfect agreement.
#'
#' With `truncate = FALSE` the formula is returned as computed. Whenever
#' `variance_ratio > 1` that value lies outside `[0, 1]` (it can be negative,
#' infinite, or above 1) and is not an agreement index; read `variance_ratio`
#' to see how far the observed variance exceeds the null.
#'
#' Lindell's `r*wg(j)` — which addresses this index's tendency to rise with
#' item count — is **not implemented**. It is a different estimand under a
#' similar name, and substituting it silently for the index the caller asked
#' for is precisely what this package does not do.
#'
#' @param ratings numeric matrix or data frame, one column per item.
#' @param group grouping vector with one entry per row of `ratings`.
#' @param null expected random-response variance: a single positive number, or
#'   `list(dist = "uniform", n_options = A)` for the rectangular null over `A`
#'   response options. Required -- there is no default, because the null
#'   decides the answer. **One value, not one per item**: the index is defined
#'   against a single expected random-response variance common to every item,
#'   and a per-item null computes a different quantity.
#' @param truncate set `rwg_j` to zero when the mean observed item variance is
#'   at or above the null (see Truncation). Recorded either way.
#' @param na_action `"omit"`, or `NULL` to be refused with a count.
#' @return a `netis_rwg` object with a `by_group` data frame (`group`, `n`,
#'   `observed_var` -- the mean observed item variance `Sbar` --,
#'   `variance_ratio` -- `Sbar / nullvar` --, and `rwg`) and the settings
#'   used; `n_items` records `J`.
#' @references James, L. R., Demaree, R. G., & Wolf, G. (1984). Estimating
#'   within-group interrater reliability with and without response bias.
#'   *Journal of Applied Psychology*, 69(1), 85-98.
#'
#'   Lindell, M. K., Brandt, C. J., & Whitney, D. J. (1999). A revised index of
#'   interrater agreement for multi-item ratings of a single target. *Applied
#'   Psychological Measurement*, 23(2), 127-135. For `r*wg(j)`.
#' @seealso [rwg()], [icc1()].
#' @examples
#' set.seed(1)
#' items <- cbind(rnorm(20, 4, .4), rnorm(20, 4, .5), rnorm(20, 4, .3))
#' teams <- rep(c("a", "b"), each = 10)
#' rwg_j(items, teams, null = list(dist = "uniform", n_options = 5))
#' @export
rwg_j <- function(ratings, group, null = NULL, truncate = TRUE, na_action = NULL) {
  ratings <- as.matrix(ratings)
  if (!is.numeric(ratings)) stop("ratings must be numeric.", call. = FALSE)
  if (length(group) != nrow(ratings)) {
    stop("group must have one entry per row of ratings.", call. = FALSE)
  }
  .assert_flag(truncate, "truncate")
  J <- ncol(ratings)
  null_var <- .resolve_rwg_null(null)

  ratings <- .rwg_handle_missing(ratings, na_action)
  # THE SAME POLICY rwg() APPLIES: a missing group label is routed through
  # `na_action` here exactly as in rwg(), never dropped silently.
  if (anyNA(group) && !identical(na_action, "omit")) {
    stop(sprintf(
      "The group labels contain %d missing value(s) and no `na_action` was declared. A rater whose group is unknown cannot be placed in one, and dropping them changes the within-group variances the agreement index is computed from, so rnetis will not choose for you: pass na_action = \"omit\" to drop them, or resolve the labels before calling.",
      sum(is.na(group))), call. = FALSE)
  }
  keep <- stats::complete.cases(ratings) & !is.na(group)
  ratings <- ratings[keep, , drop = FALSE]
  group <- group[keep]

  idx <- split(seq_len(nrow(ratings)), group)
  out <- do.call(rbind, lapply(names(idx), function(g) {
    block <- ratings[idx[[g]], , drop = FALSE]
    if (nrow(block) < 2L) {
      return(data.frame(group = g, n = nrow(block), observed_var = NA_real_,
                        variance_ratio = NA_real_, rwg = NA_real_,
                        stringsAsFactors = FALSE))
    }
    # Sbar / sigma^2_EU exactly as published: Sbar is the MEAN OBSERVED ITEM
    # VARIANCE within the group, divided by the one expected random-response
    # variance. Identical to averaging per-item ratios only because the null
    # is a single number, which is the restriction that makes this the
    # published index rather than a generalization of it.
    sbar <- mean(apply(block, 2, stats::var))
    ratio <- sbar / null_var
    # THE TRUNCATION RULE IS ON THE VARIANCE, NOT ON THE INDEX. When the mean
    # observed item variance reaches the null (ratio >= 1) the group shows no
    # agreement beyond random responding, and the index is set to zero. Flooring the formula's output at zero is not the same
    # rule: for 1 < ratio < J/(J-1) the formula is negative, at J/(J-1) its
    # denominator is zero, and above it the formula turns positive again and
    # exceeds 1, so a polarised group would read as near-perfect agreement.
    value <- if (truncate && ratio >= 1) {
      0
    } else {
      (J * (1 - ratio)) / (J * (1 - ratio) + ratio)
    }
    data.frame(group = g, n = nrow(block), observed_var = sbar,
               variance_ratio = ratio, rwg = value, stringsAsFactors = FALSE)
  }))

  structure(
    list(by_group = out, null_variance = null_var, truncate = truncate,
         n_items = J),
    class = "netis_rwg"
  )
}

#' @export
print.netis_rwg <- function(x, ...) {
  cat(sprintf("Within-group agreement r_wg%s   [%d item(s)]\n",
              if (x$n_items > 1L) "(j)" else "", x$n_items))
  cat(sprintf("  null variance: %s%s\n",
              paste(format(unique(x$null_variance), digits = 4), collapse = ", "),
              if (x$truncate) "   (negative values truncated at 0)" else "   (untruncated)"))
  cat("\n")
  print(x$by_group, row.names = FALSE)
  cat("\n  Agreement WITHIN groups. Not reliability of differentiation BETWEEN\n")
  cat("  them -- see icc1()/icc2(); the two can point different ways.\n")
  invisible(x)
}
