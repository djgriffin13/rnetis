# The latent-space positions behind a latent() adjustment, drawn.
#
# WHY THIS MAY BE DRAWN, when a Selection fit's `U` and `V` may not. The
# positions a latent() term enters into an Influence model are a single point
# estimate -- the minimum Kullback-Leibler positions of latentnet's latent
# DISTANCE model -- and what that model identifies is the distances between
# actors. A scatter of the positions shows exactly those distances. What it
# does not identify is the frame: the configuration can be rotated, reflected
# and shifted without changing a single distance, so the axes mean nothing and
# the caption says so. That is a different situation from averaging a
# bilinear model's factors over draws that each sit in their own rotation,
# which destroys the structure being summarized.

#' The table behind the latent-positions panel
#'
#' @param fit a `netis_influence` fit with a `latent()` term.
#' @param group which group, on a multi-group fit.
#' @return a plot table: `node`, `group`, `x`, `y`, `outcome`, `dims`.
#' @noRd
.plot_data_latent <- function(fit, group = NULL) {
  resolved <- fit$resolved_latent %||% list()
  if (length(resolved) == 0L) {
    stop(
      "This fit has no latent-space positions to plot: its formula has no latent() term. Add one -- for example latent(<network>, d = 2) -- to fit the latent-space adjusted model, whose positions this panel draws.",
      call. = FALSE
    )
  }
  nm <- names(resolved)[[1]]
  term <- resolved[[nm]]$term
  mf <- fit$model_frame
  group_col <- names(mf)[[1]]
  node_col <- names(mf)[[2]]
  groups <- unique(as.character(mf[[group_col]]))

  if (length(groups) > 1L) {
    if (is.null(group)) {
      stop(sprintf(
        "This fit covers %d groups, and each has its own latent space, fitted separately and not comparable across groups. Name one: plot(fit, what = \"latent\", group = \"%s\").",
        length(groups), groups[[1]]
      ), call. = FALSE)
    }
    group <- as.character(group)
    if (!group %in% groups) {
      stop(sprintf("Group '%s' is not in this fit. Available: %s.",
                   group, paste(groups, collapse = ", ")), call. = FALSE)
    }
  } else {
    group <- groups[[1]]
  }

  cols <- .latent_colnames(term, groups, group = if (length(groups) > 1L) group else NULL)
  missing_cols <- setdiff(cols, names(mf))
  if (length(missing_cols)) {
    stop(sprintf(
      "This fit's estimating table does not hold the position column(s) %s, so there is nothing to draw. Refit with the current rnetis.",
      paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
  }
  rows <- as.character(mf[[group_col]]) == group
  d <- data.frame(
    node = as.character(mf[[node_col]][rows]),
    group = group,
    x = mf[[cols[[1]]]][rows],
    # One dimension has no second axis to draw. The outcome goes there
    # instead, which is the question the adjustment exists to answer: does
    # where an actor sits in the latent space go with the outcome?
    y = if (length(cols) >= 2L) mf[[cols[[2]]]][rows] else mf[[".outcome"]][rows],
    outcome = mf[[".outcome"]][rows],
    dims = length(cols),
    stringsAsFactors = FALSE
  )
  out <- .netis_plot_table(d, required = c("node", "x", "y", "outcome", "dims"))
  out <- .netis_caption(out, sprintf(
    "Each point is an actor at its estimated position in the latent space fitted to %s (wave %s)%s.",
    resolved[[nm]]$selection_formula, as.character(resolved[[nm]]$feature_wave),
    if (length(cols) > 2L) sprintf("; the first two of %d dimensions are drawn", length(cols)) else ""
  ))
  out <- .netis_caption(
    out,
    if (length(cols) >= 2L) {
      c("Read the distances, not the axes: the configuration is identified only up to rotation, reflection and shift.",
        "Actors close together are those the selection model found likely to be tied.")
    } else {
      "Read the spacing along the position axis, not its direction or origin: a one-dimensional latent space is identified only up to reflection and shift."
    }
  )
  .netis_caption(
    out,
    "Positions are point estimates from the stage-one model; their uncertainty is not drawn."
  )
}

#' @noRd
.netis_plot_latent <- function(d) {
  two <- isTRUE(d$dims[[1]] >= 2L)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(ggplot2::aes(colour = outcome), size = 3, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = node), size = 2.6, vjust = -1.1,
                       colour = "grey30", na.rm = TRUE) +
    ggplot2::scale_colour_gradient(low = "grey82", high = "#0072B2", name = "outcome") +
    ggplot2::labs(
      x = if (two) "latent dimension 1" else "latent position",
      y = if (two) "latent dimension 2" else "outcome"
    ) +
    .netis_theme() +
    .netis_caption_lab(d)
  # Equal units on both axes where both are latent dimensions: the figure is a
  # map of distances, and a stretched axis would misstate them.
  if (two) p <- p + ggplot2::coord_equal()
  .netis_declare_references(p, .netis_references())
}
