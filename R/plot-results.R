# Estimate and variance-component tables, shared by both model families.
#
# These read the public accessors -- netis_estimates(), netis_variance_components()
# -- rather than a fit's internals, as code outside the package would have to.

#' The sentence naming the interval an estimates panel draws
#'
#' @noRd
.estimate_interval_note <- function(fit) {
  if (inherits(fit, "netis_selection")) {
    if (.selection_is_pooled(fit)) {
      return("Intervals are 95% normal intervals from the pooled standard errors; a pooled fit has no pooled draws.")
    }
    return("Intervals are 95% equal-tailed posterior quantile intervals.")
  }
  sprintf("Intervals are 95%% %s confidence intervals.",
          if (identical(fit$backend, "stats")) "t-based" else "normal (Wald)")
}

#' Estimates as a plot table
#'
#' @param fit A `netis_influence` or `netis_selection` fit.
#' @param terms Optional character vector selecting terms.
#' @noRd
.plot_data_estimates <- function(fit, terms = NULL) {
  est <- netis_estimates(fit)
  if (nrow(est) == 0L) {
    stop("This fit reports no estimates, so there is nothing to plot.", call. = FALSE)
  }
  if (!is.null(terms)) {
    keep <- est$term %in% terms
    if (!any(keep)) {
      stop(
        sprintf("None of the requested term(s) are in this fit.\nAvailable: %s.",
                paste(unique(est$term), collapse = ", ")),
        call. = FALSE
      )
    }
    est <- est[keep, , drop = FALSE]
  }

  d <- data.frame(
    term  = as.character(est$term),
    value = est$estimate,
    lower = est$conf.low,
    upper = est$conf.high,
    std.error = est$std.error,
    role  = as.character(est$role),
    scale = as.character(est$scale),
    stringsAsFactors = FALSE
  )
  if ("group" %in% names(est)) d$group <- as.character(est$group)

  # An estimate with no interval is expected, not a gap: some backends cannot
  # produce uncertainty (an lnam fit without numDeriv, a rank-deficient lm) and
  # the fit's diagnostics say why. Marking it keeps a bare point from reading as
  # a suspiciously precise one.
  no_interval <- is.na(d$lower) & is.na(d$upper) & !is.na(d$value)
  d$missing_state <- .netis_missing_state(
    nrow(d),
    ifelse(is.na(d$value), "missing",
           ifelse(no_interval, "undetermined", "observed"))
  )

  out <- .netis_plot_table(
    d, required = c("term", "value", "lower", "upper", "missing_state")
  )

  # Scale is named on the plot, every time. A probit coefficient is not an odds
  # ratio and not a probability, and a column of numbers with no scale invites
  # all three readings -- the same argument netis_estimates() makes for carrying
  # the column at all.
  scales <- unique(d$scale[!is.na(d$scale)])
  if (length(scales) > 0L) {
    out <- .netis_caption(
      out, sprintf("Estimates on the %s scale.", paste(scales, collapse = " / "))
    )
  }
  if (any(no_interval)) {
    out <- .netis_caption(
      out,
      sprintf(paste0("%d estimate(s) have no interval because the backend could not produce ",
                     "one; see the fit's diagnostics for why. They are not more precise than ",
                     "the others."), sum(no_interval))
    )
  }
  # Which interval this is, stated rather than left to habit.
  out <- .netis_caption(out, .estimate_interval_note(fit))
  # Unstandardized estimates are primary. Saying so stops a reader
  # comparing bar lengths across terms measured in different units.
  .netis_caption(
    out,
    "Coefficients are unstandardized, so term magnitudes are not comparable across different units."
  )
}

#' Per-group estimates, where the fit actually produced them
#'
#' Refusal-first, because the common case is that there is nothing to compare.
#' A fit with a group-level random intercept **pools** the fixed effects: there
#' is one coefficient per term shared across groups by construction, and
#' `netis_estimates()` reports `group = NA` to say so. Drawing four identical
#' points labeled by group would invent a per-group estimate the model never
#' produced -- a fabricated quantity with a legend attached.
#'
#' The quantity that *does* describe between-group variation in that design is
#' the random-effect standard deviation, so the refusal names it.
#' @noRd
.plot_data_groups <- function(fit) {
  est <- netis_estimates(fit)
  if (!"group" %in% names(est) || all(is.na(est$group))) {
    # A pooled Selection fit reaches here too, and the Influence advice below
    # would send its reader to the AME variance components, which describe
    # variation WITHIN each network rather than between groups.
    if (inherits(fit, "netis_selection") && isTRUE(fit$n_groups > 1L)) {
      stop(
        paste0(
          "This fit pooled its coefficients across groups (two-stage precision weighting), ",
          "so it reports one estimate per term and there is nothing to compare across groups.\n",
          "Refit with pool_beta = FALSE to obtain and compare the per-group estimates."
        ),
        call. = FALSE
      )
    }
    stop(
      paste0(
        "This fit reports one estimate per term, not one per group, so there is nothing to ",
        "compare across groups.\n",
        "A group-level random intercept pools the fixed effects by construction -- the ",
        "coefficients are shared, and rnetis reports group = NA rather than repeating one ",
        "number under each group's name.\n",
        "For between-group variation in this design use what = \"heterogeneity\", or ",
        "netis_variance_components() for the numbers."
      ),
      call. = FALSE
    )
  }

  est <- est[!is.na(est$group), , drop = FALSE]
  n_groups <- length(unique(est$group))
  if (n_groups < 2L) {
    stop(
      sprintf("This fit has per-group estimates for only %d group, so there is nothing to compare.",
              n_groups),
      call. = FALSE
    )
  }

  d <- data.frame(
    term  = as.character(est$term),
    group = as.character(est$group),
    value = est$estimate,
    lower = est$conf.low,
    upper = est$conf.high,
    role  = as.character(est$role),
    scale = as.character(est$scale),
    stringsAsFactors = FALSE
  )
  no_interval <- is.na(d$lower) & is.na(d$upper) & !is.na(d$value)
  d$missing_state <- .netis_missing_state(
    nrow(d),
    ifelse(is.na(d$value), "missing",
           ifelse(no_interval, "undetermined", "observed"))
  )

  out <- .netis_plot_table(d, required = c("term", "group", "value", "missing_state"))
  out <- .netis_caption(
    out,
    sprintf("Per-group estimates across %d groups. The spread between groups is the display; no pooled summary is drawn.",
            n_groups)
  )
  # The inferential point about between-group quantities, on the plot rather
  # than only in the docs: what carries
  # the information here is the NUMBER OF GROUPS, not the number of rows behind
  # them. Four groups of a thousand people is four observations of a between-
  # group difference, and a reader looking at tight intervals should know which
  # n they are reading.
  .netis_caption(
    out,
    sprintf("Between-group comparisons carry information proportional to the number of groups (%d), not the number of observations within them.",
            n_groups)
  )
}

#' @noRd
.netis_groups_panel <- function(d) {
  refs <- .netis_references(.netis_reference("null", 0, "no effect"))
  d$group <- factor(d$group, levels = rev(sort(unique(d$group))))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = group,
                                       colour = missing_state, shape = missing_state)) +
    .netis_reference_layer(refs, orientation = "v") +
    .netis_interval_layer(horizontal = TRUE) +
    # One panel per term with free x: terms are on different scales, and the
    # comparison being made is between GROUPS within a term, never across terms.
    ggplot2::facet_wrap(~ term, scales = "free_x") +
    .netis_scale_missing() +
    .netis_scale_missing_shape() +
    ggplot2::labs(x = NULL, y = NULL) +
    .netis_theme() +
    ggplot2::theme(legend.position = "bottom") +
    .netis_caption_lab(d, refs)
  # No pooled line across the groups: the spread is the result, and a summary
  # through it reintroduces the rollup as a picture.
  .netis_declare_references(p, refs)
}

#' Variance components as a plot table
#' @noRd
.plot_data_variance <- function(fit) {
  vc <- netis_variance_components(fit)
  if (nrow(vc) == 0L) {
    reason <- attr(vc, "reason")
    stop(
      sprintf(
        "This fit has no variance components to plot%s.",
        if (is.null(reason)) "" else paste0(": ", reason)
      ),
      call. = FALSE
    )
  }

  # Only variances have a magnitude to draw. A covariance or a correlation is a
  # different quantity on a different scale, and netis_variance_components()
  # already sets variance/sd/icc to NA for them rather than computing something.
  # Dropping them silently would under-report what the model estimated, so they
  # are kept and marked.
  d <- data.frame(
    term  = paste(vc$grouping, vc$term, sep = ": "),
    value = vc$sd,
    lower = NA_real_,
    upper = NA_real_,
    kind  = as.character(vc$kind),
    # The plot column has the short name `icc`; the accessor's column is
    # `residual_variance_share`, the same numbers.
    icc   = vc$residual_variance_share,
    stringsAsFactors = FALSE
  )
  d$missing_state <- .netis_missing_state(
    nrow(d),
    ifelse(d$kind != "variance", "undetermined",
           ifelse(is.na(d$value), "missing", "observed"))
  )

  out <- .netis_plot_table(d, required = c("term", "value", "missing_state"))
  out <- .netis_caption(out, "Random-effect standard deviations.")
  n_other <- sum(d$kind != "variance")
  if (n_other > 0L) {
    out <- .netis_caption(
      out,
      sprintf(paste0("%d entr(y/ies) are covariances or correlations rather than variances. ",
                     "They are listed because the model estimated them, but they are not on ",
                     "this scale and no magnitude is drawn for them."), n_other)
    )
  }
  out
}

#' The estimates panel, shared by both model families
#'
#' One implementation for Influence and Selection, because `netis_estimates()`
#' already speaks one vocabulary across them and two drawings of one table is how
#' two families start disagreeing about what an interval means.
#'
#' @param d A table from `netis_estimates()`, already shaped by
#'   `.plot_data_estimates()`.
#' @param by Optional facet variable (`"role"` groups terms by what they do).
#' @noRd
.netis_estimates_panel <- function(d, by = NULL, title = NULL) {
  # Zero is a mathematically defined null on a coefficient scale, one of the
  # approved reference types. It is not a significance threshold and the label
  # says "no effect", not "significant".
  refs <- .netis_references(.netis_reference("null", 0, "no effect"))

  # Terms read top-to-bottom in the order the model states them, so the axis is
  # reversed rather than left in factor order.
  d$term <- factor(d$term, levels = rev(unique(d$term)))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = term,
                                       colour = missing_state, shape = missing_state)) +
    .netis_reference_layer(refs, orientation = "v") +
    .netis_interval_layer(horizontal = TRUE) +
    .netis_scale_missing() +
    .netis_scale_missing_shape() +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    .netis_theme() +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(by) && by %in% names(d) && length(unique(d[[by]])) > 1L) {
    # Free y so a facet shows only its own terms; the x scale stays shared
    # because every term here is on one scale, which the `scale` column asserts
    # and .plot_data_estimates() has already checked.
    #
    # A formula rather than vars(): the facet variable arrives as a string, and
    # wrapping a helper call in vars() builds a plot that constructs fine and
    # fails at build time.
    p <- p + ggplot2::facet_grid(
      stats::as.formula(paste(by, "~ .")),
      scales = "free_y", space = "free_y"
    )
  }
  p <- p + .netis_caption_lab(d, refs)
  .netis_declare_references(p, refs)
}
