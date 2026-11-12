# Latent positions, drawn: an Influence fit's latent() adjustment, and a
# Selection fit's multiplicative effects.
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

# ---- a Selection fit's multiplicative effects, as coordinates ---------------
#
# WHY THIS IS NOT AN AVERAGE OF `U` AND `V`. Each retained draw's factors sit in
# their own rotation, so their separate posterior means shrink toward nothing.
# The PRODUCT `U V'` is invariant, and its posterior mean is what the fit keeps.
# The coordinates drawn here are a decomposition of that one matrix -- its
# singular value decomposition, each side scaled by the root of the singular
# value -- so their inner products reproduce the best rank-two approximation of
# the posterior mean product. They are one frame for an identified quantity, not
# an estimate of the unidentified factors, and the caption says which.

#' The table behind a Selection fit's latent-coordinates panel
#'
#' @param fit a `netis_selection` fit with `R > 0`.
#' @param group which group, on a multi-group fit.
#' @return a plot table: `node`, `group`, `role`, `x`, `y`, `dims`.
#' @noRd
.plot_data_selection_latent <- function(fit, group = NULL) {
  if (!isTRUE(fit$R > 0)) {
    stop(
      "This fit has no latent coordinates to plot: it was fit with R = 0 (no multiplicative effects). Refit with R = 1 or more -- for example fit_selection(..., R = 2) -- to draw this panel, or use what = \"actor_effects\" for the additive sender and receiver effects.",
      call. = FALSE
    )
  }
  if (isTRUE(fit$symmetric)) {
    stop(
      "The latent-coordinates panel is defined for a directed fit, where each actor has a sending and a receiving position. A symmetric fit has one position per actor with signed dimensions, which this panel does not draw. Use what = \"multiplicative\" for the product itself.",
      call. = FALSE
    )
  }
  g <- .selection_plot_group(fit, group, "multiplicative")
  uv <- fit$per_group[[g]]$multiplicative
  if (is.null(uv) || !is.matrix(uv) || !nrow(uv) || anyNA(uv)) {
    stop(
      "This fit holds no posterior mean product U V' for that group, so there are no coordinates to derive. Use what = \"actor_effects\" for the additive sender and receiver effects.",
      call. = FALSE
    )
  }
  actors <- rownames(uv)
  if (is.null(actors)) actors <- as.character(fit$actor_ids[[g]])
  if (length(actors) != nrow(uv)) actors <- as.character(seq_len(nrow(uv)))

  sv <- svd(uv)
  k <- min(2L, as.integer(fit$R), length(sv$d))
  keep <- seq_len(k)
  root <- sqrt(sv$d[keep])
  snd <- sweep(sv$u[, keep, drop = FALSE], 2L, root, `*`)
  rcv <- sweep(sv$v[, keep, drop = FALSE], 2L, root, `*`)
  # A singular vector's sign is arbitrary. Fix it, so the same fit always draws
  # the same picture: the largest sending coordinate on each dimension is positive.
  for (j in keep) {
    if (snd[which.max(abs(snd[, j])), j] < 0) {
      snd[, j] <- -snd[, j]
      rcv[, j] <- -rcv[, j]
    }
  }
  total <- sum(sv$d^2)
  share <- if (total > 0) sum(sv$d[keep]^2) / total else NA_real_

  if (k >= 2L) {
    d <- data.frame(
      node = rep(actors, 2L), group = g,
      role = rep(c("as sender", "as receiver"), each = length(actors)),
      x = c(snd[, 1L], rcv[, 1L]), y = c(snd[, 2L], rcv[, 2L]),
      dims = k, stringsAsFactors = FALSE
    )
  } else {
    # One dimension: a sending and a receiving coordinate per actor are the two
    # axes of one panel.
    d <- data.frame(
      node = actors, group = g, role = "actor",
      x = snd[, 1L], y = rcv[, 1L], dims = k, stringsAsFactors = FALSE
    )
  }
  out <- .netis_plot_table(d, required = c("node", "role", "x", "y", "dims"))
  out <- .netis_caption(out, sprintf(
    "Coordinates from the posterior mean product U V' (R = %d): %s, which carry %.0f%% of it.",
    as.integer(fit$R),
    if (k >= 2L) "its first two dimensions" else "its one dimension",
    100 * share
  ))
  out <- .netis_caption(
    out,
    if (k >= 2L) {
      c("A sender is likelier to have a tie to a receiver whose point lies in the same direction from the centre, and less likely where it lies opposite; points near the centre add little either way.",
        "Read directions and distances from the centre across the two panels, not the axes: only the product is identified, and this is one frame for it.")
    } else {
      "The multiplicative effect for a pair is the sender's horizontal value times the receiver's vertical value: same signs make a tie likelier, opposite signs less likely. Only that product is identified."
    }
  )
  .netis_caption(
    out,
    "Posterior means; their uncertainty is not drawn. The additive sender and receiver effects are separate: see what = \"actor_effects\"."
  )
}

#' @noRd
.netis_plot_selection_latent <- function(d) {
  two <- isTRUE(d$dims[[1]] >= 2L)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(colour = "#0072B2", size = 2.6, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = node), size = 2.6, vjust = -1.1,
                       colour = "grey30", na.rm = TRUE) +
    ggplot2::labs(
      x = if (two) "dimension 1" else "sending coordinate",
      y = if (two) "dimension 2" else "receiving coordinate"
    ) +
    .netis_theme() +
    .netis_caption_lab(d)
  if (two) {
    d$role <- factor(d$role, levels = c("as sender", "as receiver"))
    p$data <- d
    # One frame for both panels: a direction means the same thing in each.
    p <- p + ggplot2::facet_wrap(~role) + ggplot2::coord_equal()
  }
  .netis_declare_references(p, .netis_references())
}
