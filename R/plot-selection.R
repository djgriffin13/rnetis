# Panels over a fitted Selection model.

.NETIS_SELECTION_PANELS <- c("coefficients", "heterogeneity", "posterior", "trace",
                             "actor_effects", "multiplicative", "mcmc", "groups",
                             "gof")

#' Plot a fitted Selection model
#'
#' Draws one panel of a fitted Selection model: its coefficients, the posterior
#' for a parameter, chain traces, the actor effects, the rotation-invariant
#' multiplicative structure, MCMC diagnostics, or the per-group estimates. Use
#' [plot_data()] for the table behind any of them.
#'
#' # Scale, and why it is named
#'
#' A Selection coefficient is on the **latent probit scale** unless the fit says
#' otherwise, and the caption says which. It is not an odds ratio and not a
#' probability. For a quantity an applied reader can act on, use
#' [selection_effect()], whose default is an average marginal probability
#' contrast rather than a coefficient.
#'
#' # What is not drawn
#'
#' Latent factors `U` and `V` are identified only up to rotation, reflection,
#' and scaling, so their coordinates carry no meaning of their own and are not
#' plotted. Only the rotation-invariant product `U V'` is ever reportable, and
#' it is averaged over draws **as a product** — averaging `U` and `V`
#' separately would average across each draw's arbitrary rotation and shrink
#' the structure toward nothing. Nothing here marks a term as significant: the
#' interval is drawn and the reading is yours.
#'
#' # Refusals rather than approximations
#'
#' `"posterior"` and `"trace"` need retained draws. A fit that kept none refuses
#' rather than drawing a normal curve built from the estimate and its standard
#' error — the reason to look at a posterior is that it need not be normal.
#' `"actor_effects"` draws posterior means without intervals, because the
#' sampler reports actor effects only as posterior means; the caption says so.
#' `"gof"` needs networks simulated from the posterior, so it refuses a
#' hierarchical fit, whose sampler does not simulate any, and a fit run with
#' `gof = FALSE`; [netis_gof()] states both refusals in full.
#'
#' Chain length is reported as a count, never judged: the caption states how many
#' draws were retained and that Monte Carlo error scales with that number, and
#' leaves the adequacy question to you.
#'
#' # Multi-group fits
#'
#' Every group of a multi-group fit has its own posterior draws, actor effects
#' and multiplicative structure, so `"posterior"`, `"trace"`, `"mcmc"`,
#' `"actor_effects"` and `"multiplicative"` take a `group`. On a fit with
#' groups estimated separately (`pooling = NULL`, pooled or not) they refuse
#' without one. On a hierarchical fit (`pooling = selection_pooling()`)
#' `"posterior"`, `"trace"` and `"mcmc"` show the population-level draws when
#' `group` is `NULL` and that group's coefficient draws when it is named;
#' `"actor_effects"` and `"multiplicative"` need a group. A coefficient with no
#' population level (`unpooled`) has no population-level draws.
#'
#' `"coefficients"` shows one group's rows when `group` is named, and otherwise
#' labels each per-group row with its group. A two-stage pooled fit has one row per
#' term and none per group, so naming a group there is refused, as is `"groups"`. On
#' a hierarchical fit a group's rows are its random and unpooled coefficients; a shared
#' coefficient has only its population row. `"heterogeneity"` reads
#' [netis_variance_components()], which refuses a hierarchical fit.
#'
#' `"gof"` is the one panel that needs no `group` on a multi-group fit: every
#' group was fitted to its own network and so has its own observed statistics
#' and its own simulated ones, and the panel shows them all, one row of histograms per
#' group and one column per statistic. Naming a group narrows it to that group.
#'
#' @param x A `netis_selection` fit.
#' @param what Which panel. `"coefficients"` (default) draws each term's
#'   posterior estimate and interval; `"heterogeneity"` the variance components;
#'   `"posterior"` the retained draws as densities; `"trace"` the draws in
#'   sequence; `"actor_effects"` each actor's sender against receiver effect (on
#'   a symmetric fit, each actor's one additive effect);
#'   `"multiplicative"` the rotation-invariant product `U V'` per ordered pair of
#'   different actors; `"mcmc"` per-parameter R-hat and effective sample size;
#'   `"groups"` the per-group estimates (see **Multi-group fits**); `"gof"` the
#'   posterior predictive check — for each of five network statistics, a
#'   histogram of its value across the networks simulated from the posterior,
#'   with the central 95% of them shaded and the observed network's value drawn
#'   as a line, via [netis_gof()].
#' @param terms Optional character vector selecting which terms to show, on
#'   `"coefficients"`, `"posterior"` and `"trace"`. The other panels ignore it.
#' @param group Optional group ID, for a multi-group fit (see **Multi-group
#'   fits**). Ignored by `"heterogeneity"` and `"groups"`.
#' @param ... Ignored, present for S3 consistency.
#'
#' @return A ggplot object; [plot_data()] returns the table behind it.
#'
#' @references
#' Hoff, P. D. (2021) Additive and multiplicative effects network models.
#' *Statistical Science* **36**(1), 34-50.
#'
#' Hoff, P., Fosdick, B. and Volfovsky, A. (2024) *amen: Additive and Multiplicative
#' Effects Models for Networks and Relational Data*. R package version 1.4.5.
#'
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B. and Burkner, P.-C. (2021)
#' Rank-normalization, folding, and localization: an improved R-hat for assessing
#' convergence of MCMC. *Bayesian Analysis* **16**(2), 667-718. For `"mcmc"`.
#'
#' @seealso [selection_effect()] for probability-scale quantities,
#'   [pseudo_r2()] for variance accounted for, [netis_estimates()],
#'   [plot_data()].
#'
#' @examples
#' # Cost: an amen MCMC fit, over CRAN's example budget at any usable chain length.
#' \donttest{
#' # An MCMC fit at CRAN-safe scale; a real analysis runs a far longer chain.
#' set.seed(1)
#' ids <- sprintf("n%02d", 1:10)
#' nodes <- data.frame(group = 1, node = ids, x = stats::rnorm(10))
#' pairs <- subset(expand.grid(source = ids, target = ids,
#'                             stringsAsFactors = FALSE), source != target)
#' ties <- data.frame(group = 1, pairs,
#'                    tie = stats::rbinom(nrow(pairs), 1, 0.25))
#' nd <- netis_data(nodes, ties,
#'                  tie_meta = list(tie = list(zero_meaning = "measured_zero")))
#' fit <- fit_selection(
#'   tie ~ sender(x) + receiver(x), data = nd,
#'   nscan = 200, burn = 50, odens = 10, seed = 1
#' )
#' plot(fit)
#' }
#'
#' @export
plot.netis_selection <- function(x, what = NULL, terms = NULL, group = NULL, ...) {
  what <- .netis_what(what, .NETIS_SELECTION_PANELS, "a netis_selection fit")
  d <- plot_data(x, what = what, terms = terms, group = group)
  switch(
    what,
    coefficients   = .netis_estimates_panel(d, by = "role"),
    heterogeneity  = .netis_variance_panel(d),
    posterior      = .netis_plot_posterior(d),
    trace          = .netis_plot_trace(d),
    actor_effects  = .netis_plot_actor_effects(d),
    multiplicative = .netis_plot_multiplicative(d),
    mcmc           = .netis_plot_mcmc(d),
    groups         = .netis_groups_panel(d),
    gof            = .netis_plot_gof(d)
  )
}

#' @rdname plot.netis_selection
#' @export
plot_data.netis_selection <- function(x, what = NULL, terms = NULL, group = NULL, ...) {
  what <- .netis_what(what, .NETIS_SELECTION_PANELS, "a netis_selection fit")
  if (!is.null(group) && !as.character(group) %in% as.character(x$groups)) {
    stop(sprintf("Group '%s' is not in this fit. Available: %s.",
                 group, paste(x$groups, collapse = ", ")), call. = FALSE)
  }
  switch(
    what,
    coefficients   = .plot_data_selection_estimates(x, terms, group),
    heterogeneity  = .plot_data_variance(x),
    posterior      = .plot_data_draws(.selection_plot_draws_source(x, group, what), terms),
    trace          = .plot_data_draws(.selection_plot_draws_source(x, group, what), terms),
    actor_effects  = .plot_data_actor_effects(x, group),
    multiplicative = .plot_data_multiplicative(x, group),
    mcmc           = .plot_data_mcmc(.selection_plot_draws_source(x, group, what)),
    groups         = .plot_data_groups(x),
    # Straight through: netis_gof() already owns every reporting decision in
    # this table, and a second summary here could disagree with the first.
    gof            = netis_gof(x, group = group)
  )
}

#' Which group a per-group Selection panel describes
#'
#' `NULL` means "the fit as a whole", which exists for a single-group fit and,
#' for draws, for a hierarchical fit (its population-level draws). Otherwise a
#' multi-group fit needs a named group, and the refusal says so.
#'
#' @noRd
.selection_plot_group <- function(fit, group, what, whole_fit_ok = FALSE) {
  if (!is.null(group)) return(as.character(group))
  if (!isTRUE(fit$n_groups > 1L)) return(as.character(fit$groups[[1]]))
  if (whole_fit_ok) return(NULL)
  stop(sprintf(
    "This fit covers %d groups, and each has its own %s. Name one: plot(fit, what = \"%s\", group = \"%s\").",
    fit$n_groups,
    switch(what, actor_effects = "sender and receiver effects",
           multiplicative = "multiplicative structure", "posterior draws"),
    what, fit$groups[[1]]
  ), call. = FALSE)
}

#' The draws a posterior, trace or mcmc panel reads, as a draws object
#'
#' @noRd
.selection_plot_draws_source <- function(fit, group, what) {
  hierarchical <- .selection_is_hierarchical(fit)
  g <- .selection_plot_group(fit, group, what, whole_fit_ok = hierarchical)
  if (is.null(g)) {
    return(fit)   # the hierarchical fit's population-level draws
  }
  if (!isTRUE(fit$n_groups > 1L) && is.null(group)) {
    return(fit)
  }
  part <- fit$per_group[[g]]
  m <- part$draws
  if (is.null(m) || !is.matrix(m) || nrow(m) == 0L) {
    return(list(draws = NULL))
  }
  list(draws = .new_netis_draws(
    parameters = m, summarized = list(),
    n_chains = if (hierarchical) length(unique(fit$chain)) else 1L,
    note = sprintf("Coefficient draws for group %s.", g),
    chain = if (hierarchical) fit$chain else NULL
  ))
}

#' Coefficient estimates for a Selection fit, by group where there are several
#'
#' @noRd
.plot_data_selection_estimates <- function(fit, terms, group) {
  out <- .plot_data_estimates(fit, terms)
  if (!"group" %in% names(out)) return(out)
  per_group <- !is.na(out$group)
  if (!is.null(group)) {
    keep <- per_group & out$group == as.character(group)
    if (!any(keep)) {
      stop(sprintf("This fit reports no per-group estimates for group '%s'.", group),
           call. = FALSE)
    }
    kept <- out[keep, , drop = FALSE]
    attr(kept, "netis_caption") <- attr(out, "netis_caption")
    attr(kept, "netis_references") <- attr(out, "netis_references")
    out <- kept
    return(.netis_caption(out, sprintf("Estimates for group %s.", group)))
  }
  if (length(unique(out$group[per_group])) > 1L) {
    # One row per group and term: label each row by its group, so groups do
    # not draw on top of one another.
    out$term[per_group] <- sprintf("%s [%s]", out$term[per_group], out$group[per_group])
  }
  out
}

# ---- posterior draws --------------------------------------------------------

#' Retained parameter draws, long format
#'
#' Refuses rather than approximating when no draws were retained: a density drawn
#' from a posterior mean and a standard error is a normal approximation wearing a
#' posterior's clothes, and the whole reason to show a posterior is that it need
#' not be normal.
#' @noRd
.plot_data_draws <- function(fit, terms = NULL) {
  # `fit` is a fit, or a list carrying one group's draws in `draws`.
  draws <- fit$draws
  if (is.null(draws) || is.null(draws$parameters) || nrow(draws$parameters) == 0L) {
    stop(
      paste0(
        "This fit retained no posterior draws, so there is no posterior to plot.\n",
        "rnetis will not substitute a normal approximation built from the estimate and its ",
        "standard error -- the reason to look at a posterior is that it need not be normal.\n",
        "Use what = \"coefficients\" for the estimates and their intervals."
      ),
      call. = FALSE
    )
  }
  m <- draws$parameters
  keep <- colnames(m)
  if (!is.null(terms)) {
    keep <- intersect(keep, terms)
    if (length(keep) == 0L) {
      stop(
        sprintf("None of the requested term(s) were retained.\nAvailable: %s.",
                paste(colnames(m), collapse = ", ")),
        call. = FALSE
      )
    }
  }

  d <- data.frame(
    term  = rep(keep, each = nrow(m)),
    draw  = rep(seq_len(nrow(m)), times = length(keep)),
    value = as.numeric(m[, keep, drop = FALSE]),
    stringsAsFactors = FALSE
  )
  d$missing_state <- .netis_classify_missing(d$value)

  out <- .netis_plot_table(d, required = c("term", "draw", "value", "missing_state"))
  # Factual and threshold-free. Saying "few draws" would smuggle in a cutoff;
  # stating the count and that Monte Carlo error scales with it is true at every
  # chain length and lets the reader judge.
  out <- .netis_caption(
    out,
    sprintf(paste0("Posterior summaries are based on %d retained draw(s); Monte Carlo error ",
                   "scales with that count."), nrow(m))
  )
  .netis_caption(out, .describe_draw_coverage(draws))
}

#' @noRd
.netis_plot_posterior <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = value)) +
    ggplot2::geom_density(fill = "grey80", colour = "grey30", na.rm = TRUE) +
    ggplot2::facet_wrap(~ term, scales = "free") +
    ggplot2::labs(x = NULL, y = "density") +
    .netis_theme() +
    .netis_caption_lab(d)
  # Free scales because each parameter is on its own scale; a shared axis would
  # assert a comparability the parameters do not have.
  .netis_declare_references(p, .netis_references())
}

#' @noRd
.netis_plot_trace <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = draw, y = value)) +
    ggplot2::geom_line(colour = "grey30", linewidth = 0.4, na.rm = TRUE) +
    ggplot2::facet_wrap(~ term, scales = "free_y") +
    ggplot2::labs(x = "retained draw", y = NULL) +
    .netis_theme() +
    .netis_caption_lab(d)
  .netis_declare_references(p, .netis_references())
}

# ---- additive actor effects -------------------------------------------------

#' Sender and receiver effects, per actor
#'
#' POSTERIOR MEANS, WITH NO INTERVAL, and the caption says so. `amen::ame()`
#' reduces the additive actor effects to posterior means, so there is no
#' per-actor interval to draw. Drawing a point without stating that would let
#' it read as a fully propagated estimate, which is the failure this caption
#' exists to prevent -- a mean and a mean-with-suppressed-interval look
#' identical on a plot.
#' @noRd
.plot_data_actor_effects <- function(fit, group = NULL) {
  g <- .selection_plot_group(fit, group, "actor_effects")
  eff <- fit$per_group[[g]]$additive_effects
  a <- eff$sender
  b <- eff$receiver
  if (is.null(a) || is.null(b)) {
    stop("This fit carries no sender and receiver effects to plot.", call. = FALSE)
  }

  actors <- names(a)
  if (is.null(actors)) actors <- as.character(fit$actor_ids[[g]])
  if (length(actors) != length(a)) actors <- as.character(seq_along(a))
  # A symmetric relation has one additive effect per actor and no sender/receiver
  # split, so the table has one effect column and no such distinction to imply.
  if (isTRUE(fit$symmetric)) {
    d <- data.frame(node = actors, effect = as.numeric(a), stringsAsFactors = FALSE)
    d$value <- d$effect
    d$missing_state <- .netis_classify_missing(d$effect)
    out <- .netis_plot_table(d, required = c("node", "effect", "missing_state"))
    return(.netis_caption(out, sprintf(
      "Posterior mean additive effect for %d actor(s)%s. No interval is shown: only posterior means are retained, so per-actor uncertainty is not available. On a symmetric relation one effect is shared by both roles.",
      nrow(d), if (isTRUE(fit$n_groups > 1L)) paste0(" in group ", g) else "")))
  }
  d <- data.frame(
    node     = actors,
    sender   = as.numeric(a),
    receiver = as.numeric(b),
    stringsAsFactors = FALSE
  )
  d$value <- d$sender
  d$missing_state <- .netis_classify_missing(d$sender)

  out <- .netis_plot_table(d, required = c("node", "sender", "receiver", "missing_state"))
  out <- .netis_caption(
    out,
    sprintf(paste0("Posterior mean sender and receiver effects for %d actor(s)%s. ",
                   "No interval is shown: only posterior means are retained, ",
                   "so per-actor uncertainty is not available."), nrow(d),
            if (isTRUE(fit$n_groups > 1L)) paste0(" in group ", g) else "")
  )
  .netis_caption(
    out,
    "A sender effect is that actor's overall tendency to send ties; a receiver effect, to attract them. They are not a ranking."
  )
}

#' @noRd
.netis_plot_actor_effects <- function(d) {
  if ("effect" %in% names(d)) {
    # A symmetric relation: one effect per actor.
    d$node <- factor(d$node, levels = d$node[order(d$effect)])
    p <- ggplot2::ggplot(d, ggplot2::aes(x = effect, y = node)) +
      ggplot2::geom_point(size = 2, colour = "grey20", na.rm = TRUE) +
      ggplot2::labs(x = "additive effect", y = NULL) +
      .netis_theme() +
      .netis_caption_lab(d)
    return(.netis_declare_references(p, .netis_references()))
  }
  # Points only: the table carries posterior means and no interval.
  p <- ggplot2::ggplot(d, ggplot2::aes(x = sender, y = receiver)) +
    ggplot2::geom_point(size = 2, colour = "grey20", na.rm = TRUE) +
    ggplot2::labs(x = "sender effect", y = "receiver effect") +
    .netis_theme() +
    .netis_caption_lab(d)
  # No reference cross at the origin: actor effects are centered by construction,
  # so a line at zero marks the mean rather than a null, and would invite reading
  # "above average" as a finding.
  .netis_declare_references(p, .netis_references())
}

# ---- multiplicative structure -----------------------------------------------

#' The rotation-invariant product `U V'`
#'
#' **Only the product is ever reportable.** `U` and `V` are identified up to
#' rotation, reflection, and scaling, so their coordinates carry no meaning of
#' their own. `U V'` is invariant under those
#' transformations and is therefore the quantity, which is why this returns a
#' dyad-level table rather than actor coordinates.
#'
#' Taken from amen's own `UVPM`, which is the posterior mean of the PRODUCT.
#' Averaging `U` and `V` separately would average across each draw's arbitrary
#' rotation and shrink the structure toward nothing -- a plausible-looking
#' computation that destroys the quantity it claims to summarize. Using amen's
#' product avoids that by construction.
#' @noRd
.plot_data_multiplicative <- function(fit, group = NULL) {
  # At R = 0 amen still returns a matrix (of zeros), so the refusal is decided
  # by R, not by whether a matrix exists.
  if (!isTRUE(fit$R > 0)) {
    stop(
      paste0("This fit has no multiplicative structure to plot: it was fit with R = 0 ",
             "(no latent factors).\n",
             "Use what = \"actor_effects\" for the additive sender and receiver effects."),
      call. = FALSE
    )
  }
  g <- .selection_plot_group(fit, group, "multiplicative")
  uv <- fit$per_group[[g]]$multiplicative
  if (is.null(uv) || !is.matrix(uv) || !nrow(uv)) {
    stop(
      paste0("This fit has no multiplicative structure to plot: it was fit with R = 0 ",
             "(no latent factors).\n",
             "Use what = \"actor_effects\" for the additive sender and receiver effects."),
      call. = FALSE
    )
  }

  actors <- rownames(uv)
  if (is.null(actors)) actors <- as.character(fit$actor_ids[[g]])
  if (length(actors) != nrow(uv)) actors <- as.character(seq_len(nrow(uv)))
  d <- expand.grid(sender = actors, receiver = actors, stringsAsFactors = FALSE)
  d$value <- as.numeric(uv)
  # The diagonal is not a dyad, and an AME model says nothing about it.
  d <- d[d$sender != d$receiver, , drop = FALSE]
  rownames(d) <- NULL
  d$missing_state <- .netis_classify_missing(d$value)

  out <- .netis_plot_table(d, required = c("sender", "receiver", "value", "missing_state"))
  out <- .netis_caption(
    out,
    "Posterior mean multiplicative effect U V' for each ordered pair."
  )
  .netis_caption(
    out,
    "Only the product is shown: U and V are identified up to rotation, reflection, and scaling, so their coordinates carry no meaning on their own."
  )
}

#' @noRd
.netis_plot_multiplicative <- function(d) {
  # A diverging scale, centered at zero, because the quantity is signed and zero
  # is its meaningful midpoint. This is a scale choice, not a verdict: neither
  # end is labeled good or bad.
  lim <- max(abs(d$value), na.rm = TRUE)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = receiver, y = sender, fill = value)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.2, na.rm = TRUE) +
    ggplot2::scale_fill_gradient2(
      low = "#0072B2", mid = "grey95", high = "#D55E00",
      midpoint = 0, limits = c(-lim, lim), name = "U V'"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "receiver", y = "sender") +
    .netis_theme() +
    .netis_caption_lab(d)
  .netis_declare_references(p, .netis_references())
}

# ---- posterior predictive check ---------------------------------------------

#' The observed network against the networks the posterior generates
#'
#' A HISTOGRAM PER STATISTIC, because the question is where the observed value
#' sits in a distribution and a distribution reduced to two quantiles hides its
#' shape -- `dyad.dep` in particular need not be symmetric, and a bar from the
#' 2.5% to the 97.5% quantile cannot show that. The histogram is every network
#' the fitted model simulated; the shaded band is the central 95% of them; the
#' line is the network that was observed.
#'
#' THREE DIFFERENT QUANTITIES, DRAWN AS THREE DIFFERENT MARKS. None of this is
#' an estimate with its uncertainty, which is why `.netis_interval_layer()` is
#' deliberately not reused here -- its point sits at the centre of its interval
#' by construction, and reusing it would make a misfit look like a wide
#' confidence interval.
#'
#' FREE X SCALES, one column per statistic. The five statistics are five
#' different quantities -- a standard deviation of row means and a triadic
#' correlation -- and putting them on one axis would invite comparing their
#' magnitudes, which means nothing. Each is read against its own predictive
#' distribution and nothing else. A multi-group fit gets one row of panels per
#' group; within a column the groups share the x axis, so they can be compared.
#'
#' No reference line at the predictive mean. The question is whether the line
#' is inside the band, and a second line through the middle of every histogram
#' would be read as a target the observed value is supposed to hit.
#' @noRd
.netis_plot_gof <- function(d) {
  draws <- attr(d, "netis_draws")
  # amen's own column order, not alphabetical: the two degree statistics, the
  # dyadic one, then the two triadic ones. That is the order in which a reader
  # works through them, because it is the order in which the model stops
  # accounting for them.
  stat_levels <- unique(d$statistic)
  group_levels <- unique(d$grouping)
  d$statistic <- factor(d$statistic, levels = stat_levels)
  d$grouping <- factor(d$grouping, levels = group_levels)
  draws$statistic <- factor(draws$statistic, levels = stat_levels)
  draws$grouping <- factor(draws$grouping, levels = group_levels)
  d$ppp_label <- ifelse(is.na(d$ppp), "", sprintf("ppp = %.2f", d$ppp))

  # WHAT THE STATISTIC MEASURES, above amen's name for it. `sd.rowmean` does
  # not tell an applied reader what was checked; the name stays because it is
  # the key into netis_gof()'s table.
  short <- sub(" \\(.*$", "", .NETIS_GOF_FEATURES[stat_levels])
  strip <- stats::setNames(
    ifelse(is.na(short), stat_levels, paste0(short, "\n", stat_levels)),
    stat_levels
  )

  # Bins from the number of simulated networks, so a short chain is not drawn
  # as thirty one-network spikes.
  n_sim <- suppressWarnings(max(d$n_draws, na.rm = TRUE))
  bins <- if (is.finite(n_sim)) as.integer(min(30, max(8, ceiling(sqrt(n_sim))))) else 15L

  # A LEGEND, NOT A CAPTION ALONE. The marks are different quantities, and a
  # reader who has to go to the caption to learn which is which has been given
  # a puzzle rather than a figure. Mapping each layer's aesthetic to a literal
  # string is what makes ggplot2 emit a guide for marks that are otherwise
  # unmapped; the strings are the labels.
  obs_lab <- "observed network"
  sim_lab <- "networks the model generates"
  band_lab <- "central 95% of them"
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = d,
      ggplot2::aes(xmin = predicted_lower, xmax = predicted_upper,
                   ymin = -Inf, ymax = Inf, fill = band_lab),
      na.rm = TRUE
    ) +
    ggplot2::geom_histogram(
      data = draws,
      ggplot2::aes(x = value, fill = sim_lab),
      bins = bins, colour = "white", linewidth = 0.2, na.rm = TRUE
    ) +
    # Ink for the data: the observed network is the thing being judged and
    # should read as foreground. A SEGMENT, NOT A VLINE: the observed value is
    # a datum, and the reference geoms are reserved for declared references
    # (`.netis_reference_layer()`), of which this panel has none.
    ggplot2::geom_segment(
      data = d,
      ggplot2::aes(x = observed, xend = observed, y = -Inf, yend = Inf,
                   colour = obs_lab),
      linewidth = 1, na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = d,
      ggplot2::aes(x = Inf, y = Inf, label = ppp_label),
      hjust = 1.08, vjust = 1.6, size = 3, colour = "grey25"
    ) +
    ggplot2::scale_fill_manual(
      name = NULL,
      breaks = c(sim_lab, band_lab),
      values = stats::setNames(c("grey55", "grey88"), c(sim_lab, band_lab))
    ) +
    ggplot2::scale_colour_manual(
      name = NULL, values = stats::setNames("grey10", obs_lab)
    ) +
    # Few breaks and room between the panels. With free scales and five
    # panels in a row, the last label of one panel and the first of the next
    # are drawn a few pixels apart and overprint each other -- which looks
    # like a rendering fault rather than two numbers on two different axes.
    ggplot2::scale_x_continuous(n.breaks = 3) +
    # Headroom above the tallest bar, so the ppp label sits over the histogram
    # and not on it.
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ggplot2::labs(x = "value of the statistic", y = "simulated networks") +
    .netis_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.spacing.x = ggplot2::unit(14, "pt")
    )

  # A SINGLE-GROUP FIT HAS NO ROWS TO LABEL. One row of panels, and no strip
  # reading "1" that invites a reader to look for a second. A multi-group fit
  # has one row per group and labels each.
  lab <- ggplot2::labeller(statistic = ggplot2::as_labeller(strip))
  p <- p + if (length(group_levels) == 1L) {
    ggplot2::facet_wrap(~ statistic, scales = "free_x", nrow = 1, labeller = lab)
  } else {
    ggplot2::facet_grid(grouping ~ statistic, scales = "free_x", labeller = lab)
  }

  p <- p + .netis_caption_lab(d)
  .netis_declare_references(p, .netis_references())
}
