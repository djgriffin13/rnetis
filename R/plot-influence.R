# Panels over a fitted Influence model.

.NETIS_INFLUENCE_PANELS <- c("coefficients", "effects", "heterogeneity")

#' Panels an Influence fit does not draw, and where the quantity lives instead
#'
#' Residual dependence is computed on request from the data, so it is drawn
#' from a `netis_dependence` object. An Influence fit reports one estimate per
#' term, never one per group, so there is no per-group panel.
#' @noRd
.influence_redirect_panel <- function(what) {
  if (identical(what, "dependence")) {
    stop(
      "A fit carries no dependence record: residual dependence is computed on request, not at fit time, because it rebuilds the conduit matrix from the data. Get it with netis_dependence(fit, data) and draw that: plot(netis_dependence(fit, data)).",
      call. = FALSE
    )
  }
  if (identical(what, "groups")) {
    stop(
      paste0(
        "An Influence fit reports one estimate per term, not one per group, so there is no ",
        "per-group panel. A group-level random intercept pools the fixed effects by ",
        "construction.\n",
        "For between-group variation use what = \"heterogeneity\", or ",
        "netis_variance_components() for the numbers."
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Plot a fitted Influence model
#'
#' Draws one panel of a fitted Influence model: its coefficients, the predicted
#' change across a stated exposure contrast, or the random-effect standard
#' deviations. Use [plot_data()] for the table behind any of them. Residual
#' dependence is drawn from [netis_dependence()]:
#' `plot(netis_dependence(fit, data))`.
#'
#' # What the reference line is, and is not
#'
#' The dashed vertical line is zero — a mathematically defined null on a
#' coefficient scale. It is **not** a significance threshold, and nothing on the
#' plot marks a term as significant or not. An interval that excludes zero is
#' shown as an interval that excludes zero; what to conclude from that is yours.
#'
#' # Scale
#'
#' Coefficients are unstandardized, so the length of one term's interval is not
#' comparable to another's unless they share units. The caption names the scale
#' the estimates are on, outcome units here.
#'
#' # Effect sizes carry no interval, deliberately
#'
#' `"effects"` shows the predicted change in the outcome when exposure moves
#' across a stated contrast — the framing that communicates magnitude in the
#' units a reader cares about. The values are those [influence_effect()]
#' reports at its default contrast (the observed 10th to 90th percentile of
#' each exposure term). It is drawn as a point with **no interval**,
#' because the coefficient's confidence interval does not carry over to this
#' quantity and rnetis will not rescale one into the other. That would manufacture
#' uncertainty it never computed. A moderated term has no single predicted change,
#' so the panel refuses it and names `influence_effect()$exposures[[term]]$conditional`.
#'
#' On a simultaneous (`sna::lnam`) fit the panel does not show a change in the outcome.
#' Each value is the coefficient times the contrast, the coefficient-scale quantity
#' [influence_effect()] reports as `coefficient_times_contrast`, and the axis and
#' caption say that it excludes the feedback through `(I - rho * W1)^-1`.
#'
#' # Missing values
#'
#' An estimate whose backend could not produce an interval is drawn as a point
#' and marked `undetermined`, not as a bare point that would read as unusually
#' precise. The fit's diagnostics say why the interval is absent.
#'
#' @param x A `netis_influence` fit.
#' @param what Which panel. `"coefficients"` (default) draws each term's estimate
#'   and interval; `"effects"` the predicted outcome change across a meaningful
#'   exposure contrast, on the outcome's own scale (on a simultaneous fit, the
#'   coefficient times that contrast; see above); `"heterogeneity"` the
#'   random-effect standard deviations (fits with random effects only).
#'   `"dependence"` and `"groups"` are not panels of an Influence fit and
#'   refuse with a message saying where the quantity lives.
#' @param terms Optional character vector selecting which terms to show, on
#'   `"coefficients"` and `"effects"`. `"heterogeneity"` ignores it.
#' @param ... Ignored, present for S3 consistency.
#'
#' @return A ggplot object. [plot_data()] returns the table behind it, with columns
#'   `term`, `value`, `lower`, `upper` and `missing_state`, and also `std.error`,
#'   `role`, `scale` and `group` on `"coefficients"`, `coefficient`, `contrast_low`
#'   and `contrast_high` on `"effects"`, and `kind` and `icc` (the
#'   `residual_variance_share` of [netis_variance_components()]) on
#'   `"heterogeneity"`.
#'
#' @seealso [netis_estimates()] for the numbers without the drawing;
#'   [netis_variance_components()]; [plot_data()].
#'
#' @examples
#' set.seed(1)
#' ids <- letters[1:8]
#' nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = 8),
#'                     y = stats::rnorm(16), x = stats::rnorm(16))
#' ties <- data.frame(group = 1, source = ids[-8], target = ids[-1],
#'                    wave = 1, tie = 1)
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
#' plot(fit)
#'
#' # The numbers behind the figure, including which estimates lack an interval.
#' plot_data(fit)[, c("term", "value", "lower", "upper", "missing_state")]
#'
#' @export
plot.netis_influence <- function(x, what = NULL, terms = NULL, ...) {
  .influence_redirect_panel(what)
  what <- .netis_what(what, .NETIS_INFLUENCE_PANELS, "a netis_influence fit")
  d <- plot_data(x, what = what, terms = terms)
  switch(
    what,
    coefficients  = .netis_estimates_panel(d, by = "role"),
    effects       = .netis_effects_panel(d, simultaneous = identical(x$backend, "lnam")),
    heterogeneity = .netis_variance_panel(d)
  )
}

#' @rdname plot.netis_influence
#' @export
plot_data.netis_influence <- function(x, what = NULL, terms = NULL, ...) {
  .influence_redirect_panel(what)
  what <- .netis_what(what, .NETIS_INFLUENCE_PANELS, "a netis_influence fit")
  switch(
    what,
    coefficients  = .plot_data_estimates(x, terms),
    effects       = .plot_data_effects(x, terms),
    heterogeneity = .plot_data_variance(x)
  )
}

# ---- effect sizes -----------------------------------------------------------

#' Predicted outcome change across a meaningful exposure contrast
#'
#' *Moving from low to high exposure through this network predicts this much change
#' in the outcome*. It is drawn on the outcome's own scale, so a reader gets a
#' magnitude in the units they care about. A simultaneous fit has no such number
#' here: its rows hold the coefficient times the contrast, and the caption says so.
#' @noRd
.plot_data_effects <- function(fit, terms = NULL) {
  eff <- influence_effect(fit)$exposures
  simultaneous <- identical(fit$backend, "lnam")
  if (is.null(eff) || length(eff) == 0L) {
    stop(
      paste0(
        "This fit reports no effect sizes. They are computed for exposure terms; a model ",
        "with none has nothing to contrast.\nUse what = \"coefficients\" for the estimates."
      ),
      call. = FALSE
    )
  }
  # This function reads eff$coefficient/predicted_change as SINGLE numbers,
  # which assumes linearity -- a moderated term has no such scalar (its
  # effect is a function of the moderator, in eff$conditional instead). Left
  # unguarded, num() below would return NA_real_ for a moderated term
  # silently rather than refusing. This plot does not draw the conditional
  # surface.
  #
  # A REFUSAL, NEVER A SILENT PARTIAL PLOT. If any term this call would draw
  # -- the explicit `terms =` request, or every exposure term when `terms`
  # is unset -- is moderated, the whole call refuses naming it, rather than
  # quietly narrowing to whichever terms happen to be unmoderated. A plot
  # that dropped a requested term without saying so fabricates a result in a
  # different shape from a plotted NA.
  in_scope <- if (is.null(terms)) names(eff) else intersect(terms, names(eff))
  moderated_in_scope <- intersect(in_scope, names(Filter(function(z) isTRUE(z$moderated), eff)))
  if (length(moderated_in_scope) > 0L) {
    stop(
      sprintf(
        paste(
          "what = \"effects\" does not plot a moderated term's conditional effect (%s)",
          "-- it is a function of the moderator, not a single value. Read",
          "influence_effect(fit)$exposures[[\"%s\"]]$conditional directly, or use summary(fit). Request an",
          "unmoderated term with terms = , or omit the moderated one."
        ),
        paste(moderated_in_scope, collapse = ", "), moderated_in_scope[1]
      ),
      call. = FALSE
    )
  }
  if (!is.null(terms)) {
    eff <- eff[names(eff) %in% terms]
    if (length(eff) == 0L) {
      stop(
        sprintf("None of the requested term(s) have effect sizes.\nAvailable: %s.",
                paste(names(influence_effect(fit)$exposures), collapse = ", ")),
        call. = FALSE
      )
    }
  }

  num <- function(z, f) if (is.null(z[[f]])) NA_real_ else as.numeric(z[[f]])
  d <- data.frame(
    term  = vapply(eff, function(z) as.character(z$term), character(1)),
    value = vapply(eff, num, numeric(1),
                   if (simultaneous) "coefficient_times_contrast" else "predicted_change"),
    coefficient = vapply(eff, num, numeric(1), "coefficient"),
    lower = vapply(eff, num, numeric(1), "ci_low"),
    upper = vapply(eff, num, numeric(1), "ci_high"),
    contrast_low  = vapply(eff, num, numeric(1), "contrast_low"),
    contrast_high = vapply(eff, num, numeric(1), "contrast_high"),
    stringsAsFactors = FALSE
  )
  rownames(d) <- NULL

  # The interval on the COEFFICIENT is not an interval on the predicted change:
  # it is not rescaled and pretending otherwise would manufacture uncertainty
  # rnetis did not compute. The predicted change is therefore drawn as a point
  # with its state saying the interval is undetermined, not as a bare point that
  # would read as exact.
  d$lower <- NA_real_
  d$upper <- NA_real_
  d$missing_state <- .netis_classify_missing(
    d$value, structural = ifelse(is.na(d$value), NA, FALSE)
  )

  out <- .netis_plot_table(
    d, required = c("term", "value", "contrast_low", "contrast_high", "missing_state")
  )
  out <- .netis_caption(
    out,
    if (simultaneous) {
      paste0("Coefficient times the contrast shown: a coefficient-scale quantity that excludes the ",
             "network feedback through (I - rho * W1)^-1, and not a change in the outcome.")
    } else {
      paste0("Predicted change in the outcome when exposure moves across the contrast shown, ",
             "on the outcome's own scale.")
    }
  )
  out <- .netis_caption(
    out,
    sprintf("Contrast: exposure from %s to %s.",
            paste(format(d$contrast_low, digits = 3), collapse = " / "),
            paste(format(d$contrast_high, digits = 3), collapse = " / "))
  )
  # Stated because the obvious inference is wrong: the coefficient's interval
  # does not transfer to this quantity, so no interval is shown rather than a
  # rescaled one that would look computed.
  .netis_caption(
    out,
    sprintf("No interval is shown: the coefficient's confidence interval does not carry over to the %s, and rnetis does not rescale one into the other.",
            if (simultaneous) "product with the contrast" else "predicted change")
  )
}

#' @noRd
.netis_effects_panel <- function(d, simultaneous = FALSE) {
  d$term <- factor(d$term, levels = rev(unique(d$term)))
  zero <- .netis_references(
    .netis_reference("null", 0, if (simultaneous) "zero coefficient" else "no change")
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = term,
                                       colour = missing_state, shape = missing_state)) +
    # Zero is a defined null on either scale.
    .netis_reference_layer(zero, orientation = "v") +
    ggplot2::geom_point(size = 3, na.rm = TRUE) +
    .netis_scale_missing() +
    .netis_scale_missing_shape() +
    ggplot2::labs(
      x = if (simultaneous) {
        "coefficient times contrast (excludes network feedback)"
      } else {
        "predicted change in the outcome"
      },
      y = NULL
    ) +
    .netis_theme() +
    ggplot2::theme(legend.position = "bottom") +
    .netis_caption_lab(d, zero)
  .netis_declare_references(p, zero)
}

# ---- dependence ------------------------------

#' Residual dependence, reported as quantities and never as remedies
#'
#' This family reports what it measured and stops. A component that could
#' not be computed renders as a **stated absence carrying its own reason** rather
#' than an empty panel or a zero — `undefined` is not zero, and the object's
#' `reason` strings say so in their own words.
#' @noRd
.plot_data_dependence <- function(dep) {
  .netis_dependence_table(dep)
}

#' Turn a netis_dependence object into a plot table
#'
#' Split out from `.plot_data_dependence()` so that any caller holding a
#' `netis_dependence` object reads the same builder rather than a second one
#' that could drift from it. One definition of what a dependence component
#' means on a plot.
#' @noRd
.netis_dependence_table <- function(dep) {

  rows <- list()
  # LENGTH-ZERO IS NOT NULL, and `%||%` only catches the second. A dependence
  # component that was not computed can carry `character(0)` where a reason
  # would be -- `not_applicable` with nothing further to say is the ordinary
  # case, since each unit has one outcome row -- and data.frame() then fails with
  # "differing number of rows" rather than drawing an absent value as absent.
  # Every field is coerced to exactly one element here so that a component
  # with nothing to report becomes a row saying so.
  one <- function(x, empty = NA_character_) {
    if (is.null(x) || length(x) == 0L) empty else x[[1L]]
  }
  add <- function(component, status, value, reason, detail = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.frame(
      term = one(component), status = one(status),
      value = one(value, NA_real_),
      reason = one(reason), detail = one(detail), stringsAsFactors = FALSE
    )
  }

  # The value fields are the ones netis_dependence() stores: eta-squared of the
  # residuals on group (SSB / SST). Serial correlation is a stated not-applicable,
  # with no value to draw.
  s <- dep$serial
  if (!is.null(s)) {
    add("serial residual correlation", s$status, NA_real_, s$reason)
  }
  eta <- dep$residual_eta_squared
  if (!is.null(eta)) {
    add("residual eta-squared on group",
        eta$status %||% NA_character_,
        if (identical(eta$status, "computed")) as.numeric(eta$eta_squared %||% NA) else NA_real_,
        eta$reason %||% NA_character_,
        if (identical(eta$status, "computed")) {
          sprintf("%d groups, %d rows; expectation with no group effect %.3f",
                  eta$n_groups, eta$n_rows, eta$expected_no_group_effect)
        } else {
          NA_character_
        })
  }
  na <- dep$network_autocorrelation
  if (!is.null(na)) {
    if (identical(na$status, "computed") && length(na$per_term) > 0L) {
      for (nm in names(na$per_term)) {
        pt <- na$per_term[[nm]]
        add(sprintf("network autocorrelation: %s", nm),
            pt$status %||% NA_character_,
            as.numeric(pt$median %||% NA),
            NA_character_,
            sprintf("via %s, %s-normalized, %d snapshot(s)",
                    pt$conduit, pt$normalize, pt$n_snapshots %||% NA))
      }
    } else {
      add("network autocorrelation", na$status %||% NA_character_, NA_real_,
          na$reason %||% NA_character_)
    }
  }
  if (length(rows) == 0L) {
    stop("This fit's dependence diagnostics contain no components.", call. = FALSE)
  }
  d <- do.call(rbind, rows)
  rownames(d) <- NULL

  # A component that is `undefined` or `not_applicable` is not a missing
  # measurement and certainly not zero -- it is a question this design does not
  # pose. `undetermined` is the state that says so (absent, never fabricated).
  d$missing_state <- .netis_classify_missing(
    d$value,
    structural = ifelse(d$status == "computed", FALSE, NA)
  )

  out <- .netis_plot_table(d, required = c("term", "status", "value", "missing_state"))
  # Measurements, not remedies -- stated in the output, not only in the docs.
  out <- .netis_caption(
    out,
    "These are measurements of residual structure, not recommendations. rnetis reports what it found and does not prescribe a remedy."
  )
  reasons <- d$reason[!is.na(d$reason)]
  for (r in reasons) out <- .netis_caption(out, r)
  out
}

#' @noRd
.netis_dependence_panel <- function(d) {
  d$term <- factor(d$term, levels = rev(unique(d$term)))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = term,
                                       colour = missing_state, shape = missing_state)) +
    ggplot2::geom_point(size = 3, na.rm = TRUE) +
    .netis_scale_missing() +
    .netis_scale_missing_shape() +
    ggplot2::labs(x = "measured dependence", y = NULL) +
    .netis_theme() +
    ggplot2::theme(legend.position = "bottom") +
    .netis_caption_lab(d)
  # No reference. Zero residual dependence is neither a null the design implies
  # nor a target, and a line there would read as "distance from zero is the
  # problem" -- a magnitude becoming a finding, drawn.
  .netis_declare_references(p, .netis_references())
}

#' @noRd
.netis_variance_panel <- function(d) {
  d$term <- factor(d$term, levels = rev(unique(d$term)))
  # No reference line: there is no defined null for a standard deviation that is
  # not itself a boundary case, and a line at zero would invite reading "distance
  # from zero" as evidence -- which is a verdict.
  p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = term,
                                       colour = missing_state, shape = missing_state)) +
    ggplot2::geom_point(size = 2.5, na.rm = TRUE) +
    .netis_scale_missing() +
    .netis_scale_missing_shape() +
    ggplot2::labs(x = "standard deviation", y = NULL) +
    .netis_theme() +
    ggplot2::theme(legend.position = "bottom") +
    .netis_caption_lab(d)
  .netis_declare_references(p, .netis_references())
}

#' Draw a residual-dependence record
#'
#' `fit_influence()` returns estimates, and residual dependence is computed on
#' request because it rebuilds the conduit matrix from the data. So the thing to draw
#' is what [netis_dependence()] returns, and these methods take it directly.
#'
#' Everything the panel shows is a QUANTITY. No cutoff decides whether a value
#' is drawn in an alarming colour, and there is no reference line for a
#' threshold nobody chose.
#'
#' @param x a `netis_dependence` object from [netis_dependence()].
#' @param what `NULL` or `"dependence"`, the one panel; anything else is refused.
#' @param ... unused.
#' @return `plot()` a ggplot object; `plot_data()` the table behind it, with one row
#'   per component and columns `term`, `status`, `value`, `reason`, `detail` and
#'   `missing_state`. The `"residual eta-squared on group"` row's value is the
#'   between-group over total sum of squares of the residuals, and its `detail` gives
#'   the expectation with no group effect, `(J - 1) / (N - 1)`.
#' @seealso [netis_dependence()], [plot_data()]
#' @examples
#' set.seed(1)
#' ids <- letters[1:12]
#' nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = 12),
#'                     y = stats::rnorm(24), x = stats::rnorm(24))
#' ties <- data.frame(group = 1, source = ids[-12], target = ids[-1],
#'                    wave = 1, tie = 1)
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
#' plot(netis_dependence(fit, nd))
#' @name netis-dependence-plot
#' @export
plot.netis_dependence <- function(x, what = NULL, ...) {
  .assert_dependence_panel(what)
  .netis_dependence_panel(.plot_data_dependence(x))
}

#' @rdname netis-dependence-plot
#' @export
plot_data.netis_dependence <- function(x, what = NULL, ...) {
  .assert_dependence_panel(what)
  .plot_data_dependence(x)
}

#' A dependence object has one panel
#'
#' @noRd
.assert_dependence_panel <- function(what) {
  if (!is.null(what) && !identical(what, "dependence")) {
    stop(sprintf("A netis_dependence object has one panel, \"dependence\"; got what = %s.",
                 paste(deparse(what), collapse = "")), call. = FALSE)
  }
  invisible(TRUE)
}
