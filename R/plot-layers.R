# rnetis-specific ggplot2 layers, scales, and theme.
#
# What is deliberately NOT here: faceting, legends, shared axes, panel spacing,
# scale coordination. Those are ggplot2's, and rnetis deliberately neither
# reimplements them nor maintains a general-purpose panel manager.
#
# What IS here is the small set of layers carrying rnetis meaning, each with one
# implementation so two displays of the same quantity cannot drift apart.

#' The rnetis visual identity
#'
#' One theme across every rnetis plot, so a coefficient panel and a fit panel read
#' as the same package. Deliberately plain: this layer's job is to not editorialize.
#' @noRd
.netis_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      plot.title = ggplot2::element_text(face = "bold"),
      # Captions carry the honesty sentences, so they are left
      # aligned and full width rather than a small right-hand afterthought -- a
      # limitation nobody reads is a limitation that was not stated.
      plot.caption = ggplot2::element_text(hjust = 0, size = ggplot2::rel(0.85)),
      plot.caption.position = "plot",
      plot.title.position = "plot"
    )
}

# Okabe-Ito, colourblind-safe, in grDevices since R 4.0 and therefore free at
# rnetis's R (>= 4.2) floor. Categorical only: a SEQUENTIAL palette on a discrete
# scale reads as an ordering, and on ERROR/WARNING/NOTE that ordering reads as
# severity, which rnetis never implies.
.netis_palette <- function(n) {
  grDevices::palette.colors(n, palette = "Okabe-Ito", recycle = TRUE)
}

#' Scale for the missingness-state factor
#'
#' The visual counterpart of a structural zero and a missing value get
#' different marks, always the same different marks. Grey for undetermined
#' rather than a warning colour -- "the object cannot say" is an honest state,
#' not a problem with the user's data.
#' @noRd
.netis_scale_missing <- function(...) {
  values <- c(
    observed        = "#000000",
    structural_zero = "#0072B2",
    missing         = "#D55E00",
    absent_row      = "#CC79A7",
    undetermined    = "#999999"
  )
  labels <- c(
    observed        = "observed",
    structural_zero = "structural zero",
    missing         = "missing",
    absent_row      = "not on roster",
    undetermined    = "undetermined"
  )
  # drop = FALSE keeps every level in the legend whether or not this dataset
  # happens to contain it, so two plots of the same quantity stay comparable.
  ggplot2::scale_colour_manual(
    values = values, labels = labels, drop = FALSE,
    name = "state", na.value = values[["missing"]], ...
  )
}

#' Shape counterpart of the missingness scale
#'
#' Colour alone is not enough: these distinctions must survive a greyscale print
#' and a colourblind reader, and the isolate-versus-unobserved distinction is
#' exactly the one nobody can afford to lose to a bad projector.
#' @noRd
.netis_scale_missing_shape <- function(...) {
  # THE SAME LABELS AS THE COLOUR SCALE, and not a cosmetic duplication.
  # ggplot2 merges a colour guide and a shape guide into one legend only when
  # their name, breaks AND labels all match. Leaving these unset let the shape
  # scale fall back to the raw factor levels, so the two guides disagreed
  # ("structural zero" against "structural_zero") and could not merge -- every
  # network plot drew the same five states twice, side by side, once prettily
  # and once in snake_case. The point of the shape scale is that the
  # distinction survives greyscale, and a reader who sees two legends cannot
  # tell whether they encode two different things.
  ggplot2::scale_shape_manual(
    values = c(observed = 16, structural_zero = 15, missing = 4,
               absent_row = 3, undetermined = 1),
    labels = c(observed = "observed", structural_zero = "structural zero",
               missing = "missing", absent_row = "not on roster",
               undetermined = "undetermined"),
    drop = FALSE, name = "state", ...
  )
}

#' The point-and-interval glyph
#'
#' One implementation, used by every uncertainty display in the package. That is
#' the point: a coefficient interval and a sensitivity interval must not come to
#' mean different things because two files drew them differently.
#'
#' @param horizontal Intervals along x (terms on y) or along y.
#' @noRd
.netis_interval_layer <- function(horizontal = TRUE, size = 2, linewidth = 0.6) {
  # Bare column names rather than rlang's .data pronoun: .data would mean taking
  # rlang as a declared dependency for one piece of syntax, and the column names
  # (`lower`, `upper`) are declared to R CMD check in plot-globals.R's
  # globalVariables() call instead.
  if (horizontal) {
    list(
      ggplot2::geom_linerange(
        ggplot2::aes(xmin = lower, xmax = upper),
        linewidth = linewidth, na.rm = TRUE
      ),
      ggplot2::geom_point(size = size, na.rm = TRUE)
    )
  } else {
    list(
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = lower, ymax = upper),
        linewidth = linewidth, na.rm = TRUE
      ),
      ggplot2::geom_point(size = size, na.rm = TRUE)
    )
  }
}

#' Draw approved references, and nothing else
#'
#' **The only function in this package permitted to emit a reference
#' annotation**. It accepts only `netis_reference` objects, which can only
#' carry an approved type, so a cutoff line has no route into a plot: it cannot
#' be constructed, and if it were, it could not be drawn here.
#'
#' What is enforced is that *no reference annotation bypasses this function*, not
#' that these geoms are banned. `geom_ribbon()`, `geom_rect()` and `annotate()` stay freely
#' available elsewhere for uncertainty bands and predictive intervals, which are
#' ordinary displays with nothing to do with thresholds.
#'
#' @param references A `netis_references` set.
#' @param orientation `"h"` draws horizontal lines (a reference on the y scale),
#'   `"v"` vertical, `"identity"` the line `y = x + value` — the defined null for
#'   a calibration display, where predicted equalling observed is exactly what
#'   perfect calibration means. `"identity"` lives here rather than in the
#'   calling panel so that no reference bypasses this function.
#' @noRd
.netis_reference_layer <- function(references, orientation = c("h", "v", "identity")) {
  orientation <- match.arg(orientation)
  if (is.null(references) || length(references) == 0L) return(NULL)
  if (!inherits(references, "netis_references")) {
    stop(
      paste0(
        "Reference layers accept only a set built by .netis_references().\n",
        "This is what keeps an unapproved reference out of a plot."
      ),
      call. = FALSE
    )
  }

  vals <- vapply(references, function(r) r$value, numeric(1))
  # Linetype by type, not colour: a coloured reference invites reading the colour
  # as a judgement, and a dashed grey line is the most a reference should assert.
  ltys <- vapply(references, function(r) {
    switch(r$type,
           null = "dashed", chance = "dotted",
           baseline = "dotdash", identity = "dashed",
           nominal = "twodash")
  }, character(1))

  lapply(seq_along(vals), function(i) {
    switch(
      orientation,
      h = ggplot2::geom_hline(yintercept = vals[i], linetype = ltys[i],
                              colour = "grey40", linewidth = 0.4),
      v = ggplot2::geom_vline(xintercept = vals[i], linetype = ltys[i],
                              colour = "grey40", linewidth = 0.4),
      identity = ggplot2::geom_abline(slope = 1, intercept = vals[i],
                                      linetype = ltys[i], colour = "grey40",
                                      linewidth = 0.4)
    )
  })
}

#' Render a table's caption lines, plus reference labels
#'
#' Limitations render in the plot, not in the help page, because the plot is what
#' ends up in a slide deck. Reference labels ride
#' along so a dashed line is always identified -- an unlabeled line is an
#' assertion whose meaning the reader has to guess, and "that must be the cutoff"
#' is the misreading to prevent: rnetis draws no cutoffs.
#' @noRd
.netis_caption_lab <- function(x, references = NULL) {
  lines <- .netis_caption_of(x)
  if (!is.null(references) && length(references) > 0L) {
    ref_txt <- vapply(references, function(r) {
      src <- if (is.null(r$source)) "" else sprintf(" (%s)", r$source)
      sprintf("%s = %s%s", r$label, format(r$value, trim = TRUE), src)
    }, character(1))
    lines <- c(lines, paste0("Reference: ", paste(ref_txt, collapse = "; "), "."))
  }
  if (length(lines) == 0L) return(NULL)
  ggplot2::labs(caption = paste(lines, collapse = "\n"))
}

#' Attach the declared reference set to a built plot
#'
#' Records what the semantic layer authorized, so a test can compare it against
#' what the plot actually drew. A lexical search of the source catches an obvious
#' bypass of `.netis_reference_layer()`; that comparison catches the rest.
#' @noRd
.netis_declare_references <- function(p, references) {
  attr(p, "netis_references") <- references
  p
}
