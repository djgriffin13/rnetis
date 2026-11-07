# Diagnostic panels: MCMC convergence, and the findings collection.
#
# Two constraints govern this file and neither is negotiable.
#
# NO CONVERGENCE THRESHOLD. The conventional R-hat cutoffs are conventions, not
# mathematically defined nulls, so none is an approved reference type and none is
# drawn. The user-facing text does not name the specific values either: rnetis does
# not report a number it declines to draw, or the caption would reintroduce in prose
# exactly what the missing line was refused for. The panel reports each parameter's
# value and leaves the judgement to the reader.
#
# LEVELS ARE NOT A GRADIENT. ERROR / WARNING / NOTE are categories, not points on
# a severity scale. A NOTE rendered on a red-to-green ramp reads as a small
# failure, and rnetis never lets a statistic become a finding on magnitude -- the
# visual form of that is a colour ramp implying degree.

#' Plot a diagnostics collection
#'
#' Findings by family and level, so a reader can see what rnetis checked and what
#' it found without reading every message.
#'
#' # Levels are categories, not a scale
#'
#' ERROR, WARNING, and NOTE are distinct kinds of statement, not three points on
#' one severity axis. A NOTE is often an ordinary fact about a legitimate design
#' — a tie-less control group, a lag on a static variable — and colouring it on a
#' ramp toward red would turn a fact into an alarm. The palette here is
#' categorical for that reason, and no ordering is implied by position or hue.
#'
#' # What the counts do not say
#'
#' A fit with more NOTEs is not a worse fit. The families differ in how many
#' checks they run, so counts are not comparable across families either. This
#' panel is a map of what was checked, not a score.
#'
#' @param x A `netis_diagnostics` object.
#' @param what `"findings"` (default), the only panel.
#' @param ... Ignored, present for S3 consistency.
#'
#' @return A ggplot object; [plot_data()] returns a table with `family`,
#'   `level`, `n`, `value` (the same count) and `missing_state`, and refuses an empty
#'   collection, which has nothing to draw.
#'
#' @seealso [diagnose_data()], [diagnose_design()], [filter_diagnostics()].
#'
#' @examples
#' nodes <- data.frame(group = 1, node = c("a", "b", "c"), x = c(1, 2, 3))
#' ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = 1)
#' d <- diagnose_data(netis_data(nodes, ties))
#' if (length(d) > 0) plot(d)
#'
#' @export
plot.netis_diagnostics <- function(x, what = NULL, ...) {
  what <- .netis_what(what, "findings", "a netis_diagnostics object")
  d <- plot_data(x, what = what)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = n, y = family, fill = level)) +
    ggplot2::geom_col(width = 0.6) +
    # Categorical, deliberately. Okabe-Ito assigns three unrelated hues rather
    # than a ramp, so nothing about the colours implies an ordering.
    ggplot2::scale_fill_manual(
      values = stats::setNames(.netis_palette(3)[c(3, 2, 1)],
                               c("ERROR", "WARNING", "NOTE")),
      drop = FALSE, name = NULL
    ) +
    ggplot2::labs(x = "findings", y = NULL) +
    .netis_theme() +
    ggplot2::theme(legend.position = "bottom") +
    .netis_caption_lab(d)
  .netis_declare_references(p, .netis_references())
}

#' @rdname plot.netis_diagnostics
#' @export
plot_data.netis_diagnostics <- function(x, what = NULL, ...) {
  .netis_what(what, "findings", "a netis_diagnostics object")
  if (length(x) == 0L) {
    stop(
      paste0(
        "This diagnostics collection is empty, so there is nothing to plot.\n",
        "An empty collection means no check fired -- it is not a clean bill of health, ",
        "and rnetis does not draw one."
      ),
      call. = FALSE
    )
  }

  fam <- vapply(x, function(f) f$family, character(1))
  lev <- vapply(x, function(f) f$level, character(1))
  code <- vapply(x, function(f) f$code, character(1))

  tab <- as.data.frame(table(family = fam, level = lev), stringsAsFactors = FALSE)
  names(tab)[names(tab) == "Freq"] <- "n"
  tab <- tab[tab$n > 0L, , drop = FALSE]
  # Fixed level order so two collections render comparably, and so the legend
  # does not reorder itself according to what a particular fit happened to emit.
  tab$level <- factor(tab$level, levels = c("ERROR", "WARNING", "NOTE"))
  tab$value <- tab$n
  tab$missing_state <- .netis_missing_state(nrow(tab), "observed")
  rownames(tab) <- NULL

  out <- .netis_plot_table(tab, required = c("family", "level", "n", "missing_state"))
  out <- .netis_caption(
    out,
    sprintf("%d finding(s) across %d famil(y/ies): %s.",
            length(x), length(unique(fam)),
            paste(sort(unique(code)), collapse = ", "))
  )
  .netis_caption(
    out,
    "Levels are categories, not degrees. More findings is not a worse model, and counts are not comparable across families."
  )
}

# ---- MCMC convergence -------------------------------------------------------

#' Per-parameter convergence diagnostics as a plot table
#'
#' Per-parameter reporting exists because a single worst-case R-hat
#' says something moved, not what.
#' @noRd
.plot_data_mcmc <- function(fit) {
  draws <- fit$draws
  if (is.null(draws) || is.null(draws$parameters) || nrow(draws$parameters) == 0L) {
    stop(
      paste0(
        "This fit retained no posterior draws, so no convergence diagnostic can be computed.\n",
        "Use what = \"coefficients\" for the estimates."
      ),
      call. = FALSE
    )
  }
  # Chain identity, when the fit recorded it: stacked chains diagnosed as one
  # would never be compared with each other.
  m <- .netis_mcmc_diagnostics(draws$parameters, chain = draws$chain)

  d <- rbind(
    data.frame(term = names(m$rhat), statistic = "R-hat",
               value = as.numeric(m$rhat), stringsAsFactors = FALSE),
    data.frame(term = names(m$ess_bulk), statistic = "Bulk ESS",
               value = as.numeric(m$ess_bulk), stringsAsFactors = FALSE),
    data.frame(term = names(m$ess_tail), statistic = "Tail ESS",
               value = as.numeric(m$ess_tail), stringsAsFactors = FALSE)
  )
  # An NA here is not a missing measurement but an undefined one -- too few
  # draws to compute, or a constant parameter with no variance to compare. Both
  # are answers, and "undetermined" is the state that says so.
  d$missing_state <- .netis_missing_state(
    nrow(d), ifelse(is.na(d$value), "undetermined", "observed")
  )

  out <- .netis_plot_table(d, required = c("term", "statistic", "value", "missing_state"))
  out <- .netis_caption(
    out,
    sprintf("%d chain(s), %d retained draw(s) per chain.", m$n_chains, m$n_draws)
  )
  # The engine's own note explains what this configuration can and cannot
  # establish -- a one-chain R-hat detects drift within the run and cannot detect
  # a chain stuck in the wrong place. Carrying it verbatim beats paraphrasing it.
  if (!is.null(m$note)) out <- .netis_caption(out, m$note)
  # Says WHY no line is drawn without naming the conventional values. Printing
  # them would make rnetis the source that puts a specific cutoff in the reader's
  # head: the caption would be doing exactly what the missing line was refused for.
  .netis_caption(
    out,
    paste0("No convergence cutoff is drawn. Conventional thresholds are not defined nulls, ",
           "so rnetis reports each parameter's value and leaves the judgement to you.")
  )
}

#' @noRd
.netis_plot_mcmc <- function(d) {
  d$term <- factor(d$term, levels = rev(sort(unique(d$term))))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = term,
                                       colour = missing_state, shape = missing_state)) +
    ggplot2::geom_point(size = 2.5, na.rm = TRUE) +
    # Free x because R-hat and ESS are on entirely different scales; sharing one
    # axis would compress R-hat to a point against an ESS in the hundreds.
    ggplot2::facet_wrap(~ statistic, scales = "free_x") +
    .netis_scale_missing() +
    .netis_scale_missing_shape() +
    ggplot2::labs(x = NULL, y = NULL) +
    .netis_theme() +
    ggplot2::theme(legend.position = "bottom") +
    .netis_caption_lab(d)
  # Deliberately no reference layer. R-hat 1.01 has no approved type: it is a
  # convention, not a mathematically defined null.
  .netis_declare_references(p, .netis_references())
}
