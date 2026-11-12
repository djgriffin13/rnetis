# Posterior predictive checks on a fitted Selection model.
#
# amen simulates a network from the posterior at every retained scan and
# records five statistics of it. Those statistics are the only route this
# package has to the question a coefficient table cannot answer: does a model
# that fits the dyads reproduce the *network*? A sender/receiver model can put
# every coefficient in the right place and still generate networks with none of
# the observed closure, and nothing in the estimate table says so.
#
# WHAT IS SUMMARIZED HERE AND NOT IN THE ADAPTER. backend_extract() carries
# amen's matrix through raw. Every reduction of it -- a tail probability, a
# standardized distance, an interval -- is a reporting decision, and reporting
# decisions live where they can be documented, refused and tested.

#' The five statistics amen simulates, in amen's own column order
#'
#' The names are amen's. The labels are this package's, and exist because
#' `sd.rowmean` does not tell an applied reader what was checked.
#' @noRd
.NETIS_GOF_FEATURES <- c(
  sd.rowmean = "out-degree spread (how unequally actors send)",
  sd.colmean = "in-degree spread (how unequally actors are chosen)",
  dyad.dep   = "dyadic dependence (reciprocation)",
  cycle.dep  = "cyclic triads (generalized exchange)",
  trans.dep  = "transitive triads (closure)"
)

#' Posterior predictive check on a fitted Selection model
#'
#' For each of five network statistics, the value in the observed network
#' beside the distribution of that statistic across networks simulated from the
#' posterior. This is the check that asks whether the fitted model generates
#' networks that look like the one it was fitted to — a question the coefficient
#' table does not address and cannot.
#'
#' # The five statistics
#'
#' The names are `amen::gofstats()`'s. Which part of the model is supposed to
#' account for each one is worth knowing before reading the table, because a
#' statistic no term addresses is not evidence against the terms you fitted:
#'
#' \describe{
#'   \item{`sd.rowmean`}{Spread of out-degree: how unequally actors send.
#'     Sender effects are what account for this.}
#'   \item{`sd.colmean`}{Spread of in-degree: how unequally actors are chosen.
#'     Receiver effects account for this.}
#'   \item{`dyad.dep`}{Dyadic dependence: reciprocation beyond what the
#'     covariates imply. `prior_reciprocity()` and the fitted dyadic
#'     correlation account for this.}
#'   \item{`cycle.dep`}{Cyclic triads — generalized exchange. **Nothing in an
#'     additive model accounts for this**; it is what `R > 0` is for.}
#'   \item{`trans.dep`}{Transitive triads — closure and clustering. Again
#'     nothing additive accounts for it.}
#' }
#'
#' So an additive fit (`R = 0`) reproducing the two degree statistics and
#' missing the two triadic ones is the expected result rather than a defect,
#' and it is the reason to consider multiplicative effects. Whether raising `R`
#' actually recovers them is a question for this function, run twice.
#'
#' # Reading the columns
#'
#' `ppp` is the posterior predictive probability, the share of simulated
#' networks whose statistic was at least as large as the observed one. It is
#' **one-sided by construction**, so both ends are misfit: near 0 the observed
#' network is more extreme than almost every replicate, near 1 less extreme
#' than almost every replicate, and near 0.5 the model reproduces the feature.
#' Its granularity is `1 / n_draws`, so a short chain cannot distinguish 0 from
#' 0.02, and no threshold is applied to it here or anywhere else.
#'
#' `z` is the observed value's distance from the predictive mean in predictive
#' standard deviations. It is comparable across statistics, which `ppp` is not
#' once `ppp` saturates at 0 or 1, and it **assumes a roughly symmetric
#' predictive distribution**, which `dyad.dep` in particular need not have.
#' Read `ppp` first and `z` as the tie-breaker when `ppp` is 0 or 1.
#' `predicted_lower` and `predicted_upper` are the 2.5% and 97.5% quantiles of
#' the predictive draws, which make no symmetry assumption at all.
#'
#' # Refusals rather than approximations
#'
#' A hierarchical fit (`pooling = selection_pooling()`) has no posterior
#' predictive draws and is refused. The native sampler behind that route does
#' not simulate a network per scan, deliberately — the simulation does not
#' affect the posterior. Fit the groups separately to check them.
#'
#' A fit run with `gof = FALSE` kept the observed statistics and no draws, and
#' is refused rather than compared against an empty distribution. So is a fit
#' saved by a version of rnetis that did not retain them.
#'
#' @param object A `netis_selection` fit.
#' @param group Optional group ID. A multi-group fit returns every group's
#'   statistics by default, one block per group; naming a group returns that
#'   group's alone.
#' @param ... Ignored, present for S3 consistency.
#'
#' @return A data frame with one row per statistic per group: `grouping`,
#'   `statistic` (amen's name), `feature` (what it measures, in words),
#'   `observed`, `predicted_mean`, `predicted_sd`, `predicted_lower`,
#'   `predicted_upper`, `ppp`, `z` and `n_draws`. The attribute `netis_draws`
#'   holds the simulated values the rows were reduced from, one row per
#'   simulated network per statistic (`grouping`, `statistic`, `value`): the
#'   histograms of `plot(fit, what = "gof")` are drawn from it.
#'
#' @references
#' Gelman, A., Meng, X.-L. and Stern, H. (1996) Posterior predictive assessment
#' of model fitness via realized discrepancies. *Statistica Sinica* **6**(4),
#' 733-760.
#'
#' Hoff, P. D. (2021) Additive and multiplicative effects network models.
#' *Statistical Science* **36**(1), 34-50.
#'
#' Hoff, P., Fosdick, B. and Volfovsky, A. (2024) *amen: Additive and
#' Multiplicative Effects Models for Networks and Relational Data*. R package
#' version 1.4.5.
#'
#' @seealso [pseudo_r2()] for Tjur's D, which is also a posterior predictive
#'   quantity but a dyad-level one; [netis_glance()] for the fit's shape;
#'   [plot.netis_selection()] with `what = "gof"` for the picture.
#'
#' @examples
#' # Cost: an amen MCMC fit, over CRAN's example budget at any usable chain length.
#' \donttest{
#' set.seed(1)
#' ids <- sprintf("n%02d", 1:10)
#' nodes <- data.frame(group = 1, node = ids, x = stats::rnorm(10))
#' pairs <- subset(expand.grid(source = ids, target = ids,
#'                             stringsAsFactors = FALSE), source != target)
#' ties <- data.frame(group = 1, pairs,
#'                    tie = stats::rbinom(nrow(pairs), 1, 0.25))
#' nd <- netis_data(nodes, ties,
#'                  tie_meta = list(tie = list(zero_meaning = "measured_zero")))
#' fit <- fit_selection(tie ~ sender(x), data = nd,
#'                      nscan = 200, burn = 50, odens = 10, seed = 1)
#' netis_gof(fit)
#' }
#'
#' @export
netis_gof <- function(object, ...) {
  UseMethod("netis_gof")
}

#' @rdname netis_gof
#' @export
netis_gof.default <- function(object, ...) {
  stop(
    "netis_gof() is defined for netis_selection fits. It reads the networks amen simulated from the posterior, which only a Selection fit has.",
    call. = FALSE
  )
}

#' @rdname netis_gof
#' @export
netis_gof.netis_influence <- function(object, ...) {
  stop(
    "netis_gof() is defined for netis_selection fits. An Influence fit models a node outcome and simulates no networks, so it has no posterior predictive network statistics. For the dependence left in an Influence fit's residuals, use netis_dependence(fit, data).",
    call. = FALSE
  )
}

#' @rdname netis_gof
#' @export
netis_gof.netis_selection <- function(object, group = NULL, ...) {
  if (.selection_is_hierarchical(object)) {
    stop(
      "netis_gof() is not defined for a hierarchical multi-group Selection fit (pooling = selection_pooling()). Its sampler does not simulate a network per scan -- the simulation does not affect the posterior, so it is not run -- and there are therefore no posterior predictive statistics to compare the observed networks against. Fit the groups separately (pooling = NULL) to check them, or use netis_hierarchy() for the between-group distribution.",
      call. = FALSE
    )
  }

  parts <- object$per_group
  groups <- as.character(object$groups)
  if (!is.null(group)) {
    group <- as.character(group)
    if (!group %in% groups) {
      stop(sprintf("Group '%s' is not in this fit. Available: %s.",
                   group, paste(groups, collapse = ", ")), call. = FALSE)
    }
    keep <- which(groups == group)
    parts <- parts[keep]
    groups <- groups[keep]
  }

  .assert_gof_draws(parts, groups, object)

  rows <- lapply(seq_along(parts), function(i) {
    .gof_table_one(parts[[i]]$gof, groups[i])
  })
  out <- do.call(rbind, rows)
  attr(out, "netis_caption") <- .gof_caption(out, object$R %||% 0L)
  # THE DRAWS THE TABLE WAS REDUCED FROM, carried with it. The histogram panel
  # draws each distribution rather than its two quantiles, and reading the
  # draws from the same object the table came from is what keeps the picture
  # and the numbers from being computed twice.
  attr(out, "netis_draws") <- do.call(rbind, lapply(seq_along(parts), function(i) {
    .gof_draws_one(parts[[i]]$gof, groups[i])
  }))
  out
}

#' One group's simulated statistics, one row per simulated network per statistic
#'
#' The same draws `.gof_table_one()` summarizes, with the same non-finite
#' values removed, so a histogram of them has exactly `n_draws` observations.
#'
#' @param gof amen's GOF matrix: observed network in row 1, simulated in the rest.
#' @param grouping the group label for the rows.
#' @return a data frame with `grouping`, `statistic` and `value`.
#' @noRd
.gof_draws_one <- function(gof, grouping) {
  draws <- gof[-1, , drop = FALSE]
  out <- data.frame(
    grouping = as.character(grouping),
    statistic = rep(colnames(gof), each = nrow(draws)),
    value = as.vector(draws),
    stringsAsFactors = FALSE, row.names = NULL
  )
  out[is.finite(out$value), , drop = FALSE]
}

#' Refuse a fit whose posterior predictive draws were never kept
#'
#' TWO DIFFERENT ABSENCES, and they need different instructions. A fit run with
#' `gof = FALSE` has amen's matrix with its observed row and nothing else; a
#' fit saved by an older rnetis has no matrix at all, because the adapter did
#' not carry one. Telling someone to re-run with `gof = TRUE` when the real
#' problem is the version they saved under would send them round a loop.
#'
#' Checked across every group and refused if ANY group lacks draws, rather than
#' returning a table with silent NA blocks in it. `gof` is a fit-level control,
#' so in practice this is all-or-nothing; where it is not, a refusal naming the
#' groups is more use than a frame the reader has to audit.
#' @noRd
.assert_gof_draws <- function(parts, groups, object) {
  # A matrix WITH NAMED COLUMNS, because the statistic names are what every row
  # of the table is keyed by. An unnamed matrix would otherwise reach
  # data.frame() with a zero-length `statistic` beside a length-one `grouping`
  # and fail on differing row counts -- an obscure error for a clear cause.
  present <- vapply(parts, function(p) {
    !is.null(p$gof) && is.matrix(p$gof) && !is.null(colnames(p$gof)) &&
      ncol(p$gof) > 0L
  }, logical(1))
  if (!all(present)) {
    stop(
      sprintf(
        "This Selection fit holds no posterior predictive network statistics, so there is nothing to compare the observed network against (group%s %s). It was fitted by rnetis %s, which did not retain them. Refit with the current version.",
        if (sum(!present) > 1L) "s" else "",
        paste(groups[!present], collapse = ", "),
        object$netis_version %||% "an earlier version"
      ),
      call. = FALSE
    )
  }
  # Row 1 is the observed network; rows 2+ are the simulated ones. A matrix
  # with one row is amen's record of `gof = FALSE`: the statistics of the
  # observed network were computed, and no network was ever simulated.
  drawn <- vapply(parts, function(p) nrow(p$gof) > 1L, logical(1))
  if (!all(drawn)) {
    stop(
      sprintf(
        "This Selection fit was run with gof = FALSE, so no network was simulated from the posterior and the observed statistics have no distribution to sit in (group%s %s). Refit with gof = TRUE, which is the default.",
        if (sum(!drawn) > 1L) "s" else "",
        paste(groups[!drawn], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' One group's observed statistics against their predictive distributions
#'
#' @param gof amen's GOF matrix: observed network in row 1, simulated networks
#'   in the rest, one column per statistic.
#' @param grouping the group label for the rows.
#' @noRd
.gof_table_one <- function(gof, grouping) {
  observed <- gof[1, ]
  draws <- gof[-1, , drop = FALSE]
  stat_names <- colnames(gof)

  summarized <- lapply(seq_along(stat_names), function(j) {
    d <- draws[, j]
    obs <- observed[[j]]
    # COUNTED PER STATISTIC, not per fit. A degenerate simulated network can
    # leave one statistic undefined while the others are fine, and n_draws has
    # to be the number this row was actually computed from or it misdescribes
    # its own ppp.
    ok <- is.finite(d)
    n <- sum(ok)
    if (n < 2L || !is.finite(obs)) {
      return(c(observed = obs, predicted_mean = NA_real_, predicted_sd = NA_real_,
               predicted_lower = NA_real_, predicted_upper = NA_real_,
               ppp = NA_real_, z = NA_real_, n_draws = n))
    }
    d <- d[ok]
    m <- mean(d)
    s <- stats::sd(d)
    q <- stats::quantile(d, c(0.025, 0.975), names = FALSE)
    c(
      observed = obs,
      predicted_mean = m,
      predicted_sd = s,
      predicted_lower = q[[1]],
      predicted_upper = q[[2]],
      # One-sided, and documented as such: the share of simulated networks at
      # least as large as the observed one.
      ppp = mean(d >= obs),
      # A zero predictive spread makes this undefined rather than infinite.
      # Dividing anyway would report an arbitrarily large distance from a
      # distribution that has no width to measure distance in.
      z = if (is.finite(s) && s > 0) (obs - m) / s else NA_real_,
      n_draws = n
    )
  })

  m <- do.call(rbind, summarized)
  data.frame(
    grouping = as.character(grouping),
    statistic = stat_names,
    # An amen release adding a statistic should still report, with the name it
    # came with, rather than dropping a row or erroring on a missing label.
    feature = unname(.NETIS_GOF_FEATURES[stat_names]),
    observed = unname(m[, "observed"]),
    predicted_mean = unname(m[, "predicted_mean"]),
    predicted_sd = unname(m[, "predicted_sd"]),
    predicted_lower = unname(m[, "predicted_lower"]),
    predicted_upper = unname(m[, "predicted_upper"]),
    ppp = unname(m[, "ppp"]),
    z = unname(m[, "z"]),
    n_draws = as.integer(unname(m[, "n_draws"])),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' What the table cannot say for itself
#'
#' Rendered into the plot rather than left to the help page, for the same
#' reason every other caption in this package is: the plot is what ends up in a
#' slide deck.
#'
#' THE TRIADIC LINE DEPENDS ON THE MODEL, so `R` is passed in rather than
#' inferred from the table. "Nothing additive accounts for the triadic
#' statistics" is true of an additive model and actively misleading on a fit
#' with multiplicative effects, where those statistics are the ones `R > 0`
#' was added to account for -- and where a reader who saw that caption would
#' excuse exactly the misfit they should be weighing.
#' @noRd
.gof_caption <- function(tbl, R = 0L) {
  n <- unique(tbl$n_draws[is.finite(tbl$n_draws)])
  lines <- c(
    # HOW TO READ IT, FIRST. The legend says what each mark is; this says what
    # their relationship means, which is the whole content of the figure and
    # the one thing a reader cannot work out from the marks themselves.
    "Line inside the shaded band: the model reproduces that feature of the network. Line outside: it cannot.",
    sprintf(
      "Each histogram is the statistic across %s networks simulated from the fitted model; the shaded band spans its 2.5%%-97.5%% quantiles.",
      if (length(n) == 1L) format(n[[1]]) else "the retained"
    ),
    sprintf(
      "ppp is one-sided, so 0 and 1 are both misfit and 0.5 is agreement; its granularity is 1/%s.",
      if (length(n) == 1L) format(n[[1]]) else "n_draws"
    ),
    "Each panel has its own x scale: the five statistics are different quantities and their magnitudes are not comparable.",
    "No threshold is applied to any column."
  )
  triadic <- any(tbl$statistic %in% c("cycle.dep", "trans.dep"))
  if (triadic && R == 0L) {
    lines <- c(lines, "Nothing in this additive (R = 0) model accounts for the triadic statistics.")
  } else if (triadic) {
    lines <- c(lines, sprintf(
      "The triadic statistics are what the multiplicative effects (R = %s) are there to account for.",
      format(R)
    ))
  }
  lines
}
