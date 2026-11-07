# Uniform result extraction across both model families.
#
# The problem this solves is that the headline quantities of a rnetis fit would
# otherwise be reachable only by knowing which list element to reach into, and
# the element differs between families: an Influence fit keeps its uncertainty in `vcov`,
# a Selection fit keeps a posterior in `draws`, and a multi-group Selection fit
# keeps one set per group in `per_group`. A user comparing two fits would have to learn
# the internals of both.
#
# THREE RULES, and the third is the one that constrains the design.
#
# 1. One shape per question. `netis_estimates()` answers "what did you estimate
# and how uncertain is it", one row per term, identically for both families.
# `netis_glance()` answers "what kind of fit is this", one row. Those are the
# two shapes the tidy conventions established and they are the right two.
#
# 2. The column names are chosen for interoperability. `std.error`, `conf.low`,
# `conf.high`, `r.squared` and `nobs` are dot-separated because that is what the
# `tidy()`/`glance()` convention calls them and what downstream tools index by
# name, so do not change them to snake_case. Where the convention has no
# counterpart -- `group`, `role`, `scale`, `estimand`, `n_groups` and the rest --
# rnetis keeps its own names: match where there is something to match.
#
# 3. A quantity a fit does not have is ABSENT, never fabricated. An OLS fit has no
# variance components; a multi-group Selection fit has no joint covariance. Each
# accessor returns a zero-row frame carrying a `reason` attribute, or refuses
# with that reason -- and never an NA column that looks like a measured missing
# value.
#
#    This binds the tidy methods below too: neither family has a `statistic` or a
#    `p.value` column. A posterior has neither, the Influence table reports
#    intervals, and the convention tolerates an absent column.
#
# ON THE DEPENDENCY. `generics` is in Imports purely to
# own the `tidy`/`glance` generics, and it is about the cheapest dependency in
# R: `Depends: R (>= 3.6)`, `Imports: methods`, nothing else, so it adds no
# recursive burden and sits below rnetis's own R floor. Importing it does NOT
# pull in `broom`, which would drag dplyr, tibble and purrr behind it.
#
# The generics are RE-EXPORTED, so `library(rnetis); tidy(fit)` works without
# broom installed -- the point of the exercise is the user who types `tidy(fit)`
# out of habit, and they should not have to know which package owns the verb.
# Attaching broom as well prints a masking message; it is cosmetic, since both
# packages re-export the same generic object from `generics` and dispatch is
# identical either way.

#' Extract estimates and their uncertainty from a rnetis fit
#'
#' One row per estimated term, in the same shape for both model families, so
#' Influence and Selection results can be handled by the same downstream code.
#' This is the accessor to reach for rather than indexing into the fitted
#' object: the internal layout differs between families and between single- and
#' multi-group fits, and it is not part of the public API.
#'
#' @param object a `netis_influence` or `netis_selection` fit.
#' @param level confidence (Influence) or credible (Selection) interval level.
#'   Default `0.95`.
#' @param ... unused, present for method consistency.
#' @return a `data.frame` with columns `term`, `group` (`NA` for a single-group
#'   or pooled fit, and for a population-level row of a hierarchical fit), `estimate`, `std.error`, `conf.low`, `conf.high`, `role`
#'   (for Influence `"stability"`, `"exposure"`, `"lagged"`, `"structural"` or
#'   `"direct"`; for Selection `"intercept"`, `"sender"`, `"receiver"`, `"node"`,
#'   `"dyadic"`, `"prior tie"`, `"reciprocity"`, `"moderator"` or `"other"`), and
#'   `scale`, which names what the estimate is on: `"outcome units"` for Influence,
#'   and `"latent probit"` or `"tie values"` for Selection. `scale` matters: a probit
#'   coefficient is not an odds ratio and not a probability, and a column of numbers
#'   with no scale invites all three readings.
#'
#'   Neither family has a `statistic` or `p.value` column. Influence intervals are
#'   Wald intervals, the same as [confint()][netis-influence-methods]. Selection
#'   intervals are equal-tailed posterior quantile intervals, except on a two-stage
#'   pooled fit, which has no pooled draws and reports normal intervals from the
#'   pooled standard errors. `std.error` on a Selection fit is the posterior standard
#'   deviation, or the pooled standard error on a pooled fit.
#'
#'   A hierarchical Selection fit reports both levels: one row with `group = NA` for
#'   each shared coefficient and each random coefficient's hierarchical mean (with
#'   moderators, its value at every moderator's zero), one such row per moderator
#'   coefficient (`<coefficient>:<moderator>`, role `"moderator"`), and one row per group
#'   for each random or unpooled coefficient.
#'
#'   Interval columns are `NA` when the backend could not produce uncertainty
#'   (an `lnam` fit without `numDeriv`, a rank-deficient `lm`) -- and the fit's
#'   diagnostics say why, rather than the absence being left to be noticed.
#' @seealso [netis_glance()] for a one-row model summary,
#'   [netis_variance_components()] for the random-effects side, [pseudo_r2()].
#' @examples
#' set.seed(1)
#' ids <- letters[1:8]
#' nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = 8),
#'                     y = stats::rnorm(16), x = stats::rnorm(16))
#' ties <- data.frame(group = 1, source = ids[-8], target = ids[-1],
#'                    wave = 1, tie = 1)
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(y ~ ar() + exposure(x, via = tie), data = nd)
#' # One row per term, with its uncertainty, its role in the Influence
#' # vocabulary, and -- importantly -- the scale the estimate is on.
#' netis_estimates(fit)
#' # A Selection fit returns these same columns, which is what lets code
#' # handling both families avoid branching on which one it was given.
#' names(netis_estimates(fit))
#' @export
netis_estimates <- function(object, level = 0.95, ...) {
  UseMethod("netis_estimates")
}

#' @rdname netis_estimates
#' @export
netis_estimates.default <- function(object, level = 0.95, ...) {
  stop(
    sprintf("netis_estimates() has no method for an object of class %s. It is defined for netis_influence and netis_selection fits.",
            paste(sprintf("'%s'", class(object)), collapse = "/")),
    call. = FALSE
  )
}

#' Build the estimates frame
#'
#' Argument names match the column names, dots and all, so a caller cannot pass
#' the interval to the standard-error slot by getting the convention half right.
#'
#' @noRd
.estimates_frame <- function(term, group, estimate, std.error, conf.low, conf.high,
                             role, scale) {
  data.frame(
    term = term, group = group, estimate = estimate, std.error = std.error,
    conf.low = conf.low, conf.high = conf.high, role = role, scale = scale,
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' @rdname netis_estimates
#' @export
netis_estimates.netis_influence <- function(object, level = 0.95, ...) {
  est <- object$coefficients
  se <- sqrt(diag(object$vcov))[names(est)]
  # The interval is taken from confint(), not reconstructed here, so that this
  # accessor and a user's own confint() call cannot disagree -- the backend
  # decides whether its own inference is normal-quantile (lnam) or t-based, and
  # duplicating that choice is how two answers to one question appear.
  ci <- tryCatch(
    suppressWarnings(stats::confint(object, level = level)),
    error = function(e) NULL
  )
  lo <- hi <- stats::setNames(rep(NA_real_, length(est)), names(est))
  if (!is.null(ci) && is.matrix(ci)) {
    keep <- intersect(names(est), rownames(ci))
    lo[keep] <- ci[keep, 1]
    hi[keep] <- ci[keep, 2]
  }
  .estimates_frame(
    term = names(est), group = NA_character_, estimate = unname(est),
    std.error = unname(se), conf.low = unname(lo), conf.high = unname(hi),
    role = .influence_term_roles(names(est), object$spec),
    scale = "outcome units"
  )
}

#' Classify Influence terms into the framework's own vocabulary
#'
#' The same partition `summary()` prints under its section headings, computed
#' once and used by both, so a term cannot be filed under "exposure" in one
#' place and "direct" in the other.
#'
#' @noRd
.influence_term_roles <- function(nms, spec) {
  ar_names <- unlist(lapply(spec$ar, function(a) paste0(a$variable, "_lag", a$lag)))
  lagged_names <- unlist(lapply(spec$lagged, function(l) paste0(l$variable, "_lag", l$lag)))
  exposure_names <- vapply(spec$exposures, function(e) e$name, character(1))
  structural_names <- c(
    vapply(spec$centralities, function(cc) cc$name, character(1)),
    vapply(spec$network_stats, function(nn) nn$name, character(1))
  )
  out <- rep("direct", length(nms))
  out[nms %in% ar_names] <- "stability"
  out[nms %in% lagged_names] <- "lagged"
  out[nms %in% exposure_names] <- "exposure"
  out[nms %in% structural_names] <- "structural"
  out
}

#' @rdname netis_estimates
#' @export
netis_estimates.netis_selection <- function(object, level = 0.95, ...) {
  probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
  scale <- if (identical(object$family, "bin")) "latent probit" else "tie values"

  # A HIERARCHICAL fit is one model with two levels, and both are reported: a
  # population row per coefficient that has one, and a group row per group for
  # every coefficient that varies. Sending it down the unpooled branch below
  # described it as independently fitted groups, which it is not.
  if (.selection_is_hierarchical(object)) {
    return(.selection_estimates_hierarchical(object, probs, scale))
  }

  # A multi-group unpooled fit is a LIST of fits, and its estimates are one set
  # per group. Stacking them into one frame with a `group` column is the honest
  # representation: it makes the per-group structure visible instead of
  # implying a joint model that was not fitted.
  pooled <- .selection_is_pooled(object)
  if (object$n_groups > 1L && !pooled) {
    rows <- lapply(seq_len(object$n_groups), function(i) {
      .selection_estimates_one(object$per_group[[i]], probs, scale,
                               group = as.character(object$groups[i]), spec = object$spec)
    })
    return(do.call(rbind, rows))
  }
  # A POOLED multi-group fit has no covariance matrix and no pooled draws --
  # two-stage pooling combines per-group means and marginal variances and
  # nothing else -- so its standard errors are the pooled ones, supplied
  # directly, and its interval is the normal interval they imply. That is the
  # same SE summary() prints, so the two tables cannot disagree.
  .selection_estimates_one(
    list(coefficients = object$coefficients, vcov = object$vcov,
         draws = object$draws,
         se = if (pooled) sqrt(object$pooled_beta$var) else NULL),
    probs, scale, group = NA_character_, spec = object$spec
  )
}

#' Estimates for a hierarchical Selection fit, at both levels
#'
#' Population rows (`group = NA`) for shared coefficients and for each random
#' coefficient's hierarchical mean -- with moderators, its value at every
#' moderator equal to zero, and one row per moderator coefficient
#' (`<coefficient>:<moderator>`, role `"moderator"`). Group rows for every
#' random or unpooled coefficient. Every interval is an equal-tailed posterior
#' quantile interval from the retained draws; `std.error` is the posterior SD.
#'
#' @noRd
.selection_estimates_hierarchical <- function(object, probs, scale) {
  d <- object$mgc_draws
  reg <- d$regime
  one <- function(term, group, v, role) {
    q <- stats::quantile(v, probs, names = FALSE)
    .estimates_frame(term, group, mean(v), stats::sd(v), q[1], q[2], role, scale)
  }
  rows <- list()
  for (k in names(reg)[reg != "group"]) {
    rows[[length(rows) + 1L]] <- one(k, NA_character_, d$mu[, k], .selection_term_roles(k, object$spec))
  }
  if (!is.null(d$gamma)) {
    for (cn in colnames(d$gamma)) {
      if (endsWith(cn, ":(Intercept)")) next
      term <- sub("^gamma:", "", cn)
      rows[[length(rows) + 1L]] <- one(term, NA_character_, d$gamma[, cn], "moderator")
    }
  }
  for (k in names(reg)[reg != "shared"]) {
    for (gi in seq_along(d$groups)) {
      rows[[length(rows) + 1L]] <- one(k, as.character(d$groups[gi]), d$theta[, gi, k],
                                       .selection_term_roles(k, object$spec))
    }
  }
  do.call(rbind, rows)
}

#' Estimates for one Selection posterior
#'
#' Intervals come from the retained draws where they exist, because a posterior
#' interval and a normal interval built from the posterior SD are different
#' quantities and the first is the one the model produced. The normal fallback
#' is used only where draws were not retained, and it is labeled by the absence
#' of draws rather than silently substituted.
#'
#' @noRd
.selection_estimates_one <- function(part, probs, scale, group, spec) {
  est <- part$coefficients
  if (is.null(est) || length(est) == 0L) {
    return(.estimates_frame(character(0), character(0), numeric(0), numeric(0),
                            numeric(0), numeric(0), character(0), character(0)))
  }
  se <- if (!is.null(part$se)) {
    part$se[names(est)]
  } else if (!is.null(part$vcov)) {
    sqrt(diag(part$vcov))[names(est)]
  } else {
    stats::setNames(rep(NA_real_, length(est)), names(est))
  }
  beta_draws <- .selection_beta_draws(part$draws, names(est))
  if (!is.null(beta_draws)) {
    q <- apply(beta_draws, 2L, stats::quantile, probs = probs, na.rm = TRUE)
    lo <- q[1, names(est)]
    hi <- q[2, names(est)]
  } else {
    crit <- stats::qnorm(probs[2])
    lo <- est - crit * se
    hi <- est + crit * se
  }
  .estimates_frame(
    term = names(est), group = group, estimate = unname(est),
    std.error = unname(se), conf.low = unname(lo), conf.high = unname(hi),
    role = .selection_term_roles(names(est), spec), scale = scale
  )
}

#' Locate the coefficient draws inside a netis_draws object
#'
#' Returns `NULL` rather than guessing when the layout does not carry them, so
#' the caller falls back to a normal interval deliberately.
#'
#' @noRd
.selection_beta_draws <- function(draws, nms) {
  if (is.null(draws)) {
    return(NULL)
  }
  # Two shapes reach here and both are legitimate: a whole-fit `netis_draws`
  # object keeps its coefficient draws in `$parameters`, while a per-group entry
  # holds the bare matrix. Handling both here rather than at each call site
  # keeps the shape knowledge in one place.
  beta <- if (is.matrix(draws)) draws else draws$parameters
  if (is.null(beta) || !is.matrix(beta) || !all(nms %in% colnames(beta))) {
    return(NULL)
  }
  beta
}

#' A Selection term's role, read from the specification that named it
#'
#' Looked up by the term's name, not by matching text inside it: a sender
#' covariate called `reciprocal_trust` is a sender term, and a term given its own
#' `name =` keeps its role.
#'
#' @param nms coefficient names.
#' @param spec the fit's parsed Selection formula.
#' @noRd
.selection_term_roles <- function(nms, spec) {
  names_of <- function(terms) vapply(terms, function(t) t$name, character(1))
  out <- rep("other", length(nms))
  out[nms == "intercept"] <- "intercept"
  # A symmetric fit's nodal covariates are sender() terms; the relation has no
  # sender/receiver split, so their role says so.
  out[nms %in% names_of(spec$senders)] <- if (isTRUE(spec$symmetric)) "node" else "sender"
  out[nms %in% names_of(spec$receivers)] <- "receiver"
  out[nms %in% names_of(spec$dyads)] <- "dyadic"
  if (!is.null(spec$prior_tie)) out[nms == spec$prior_tie$name] <- "prior tie"
  if (!is.null(spec$prior_reciprocity)) out[nms == spec$prior_reciprocity$name] <- "reciprocity"
  out
}

#' One-row summary of what kind of fit this is
#'
#' The model-level counterpart to [netis_estimates()]: the metadata a reader
#' needs to know what was fit and what it targets, without opening the object.
#' Deliberately carries the **estimand** and the **routing** rather than only
#' goodness-of-fit numbers -- for these model families, which question was
#' answered is more consequential than how well.
#'
#' @param object a `netis_influence` or `netis_selection` fit.
#' @param ... unused.
#' @return a one-row `data.frame`. Columns common to both families: `family`
#'   (`"influence"`/`"selection"`), `estimand`, `nobs`, `n_groups`, `backend`,
#'   `netis_version`, and counts of ERROR/WARNING/NOTE diagnostics. Influence
#'   adds `estimator`, `n_transitions` (`1`, or `0` for a single cross-section),
#'   `r.squared`, `sigma`, `rmse`; Selection adds `outcome_family`, `pooled` (`TRUE` for
#'   two-stage combination across groups and `FALSE` otherwise, a hierarchical fit
#'   included; [netis_estimand()] names that as its `pooling`), `latent_dims`, and
#'   `n_draws` (retained draws, summed over chains). A column a fit cannot fill is `NA`, never a substituted zero:
#'   `n_groups` on an Influence fit with no random-effects term, and `r.squared` on an
#'   `lme4` or `sna::lnam` fit, for which [pseudo_r2()] says what is available.
#'
#'   `nobs` counts what the backend fitted, not what was assembled. For
#'   Influence it is the rows left after the backend dropped incomplete ones.
#'   For Selection it is the observed dyads: a dyad whose outcome is missing, or
#'   was masked because a predictor could not be resolved, is not counted, a
#'   self-tie is never counted, and in a symmetric fit each unordered pair counts
#'   once.
#'
#'   Influence's `rmse` is the in-sample root mean squared error, and is `NA`
#'   for a backend that exposes no residuals. It is in sample and therefore
#'   optimistic. Only cheap fit quantities appear here: nothing that needs
#'   replicate generation or refits is computed by `netis_glance()`.
#' @seealso [netis_estimates()], [netis_estimand()], [pseudo_r2()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = rep(letters[1:12], 2), wave = rep(1:2, each = 12),
#'   y = c(2.1, 0.4, -1.2, 0.8, 1.5, -0.3, 0.9, -1.1, 0.2, 1.8, -0.7, 0.5,
#'         1.9, 0.1, -0.9, 1.1, 1.2, -0.1, 1.3, -0.8, 0.6, 1.4, -0.4, 0.7)
#' )
#' ties <- data.frame(
#'   group = 1, source = letters[1:11], target = letters[2:12], tie = 1
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' netis_glance(fit_influence(y ~ ar() + exposure(y, via = tie), data = nd))
#' @export
netis_glance <- function(object, ...) {
  UseMethod("netis_glance")
}

#' @rdname netis_glance
#' @export
netis_glance.default <- function(object, ...) {
  stop(
    sprintf("netis_glance() has no method for an object of class %s. It is defined for netis_influence and netis_selection fits.",
            paste(sprintf("'%s'", class(object)), collapse = "/")),
    call. = FALSE
  )
}

#' @noRd
.diagnostic_counts <- function(d) {
  if (is.null(d) || length(d) == 0L) {
    return(c(n_error = 0L, n_warning = 0L, n_note = 0L))
  }
  lev <- vapply(d, function(f) f$level, character(1))
  c(n_error = sum(lev == "ERROR"), n_warning = sum(lev == "WARNING"),
    n_note = sum(lev == "NOTE"))
}

#' @rdname netis_glance
#' @export
netis_glance.netis_influence <- function(object, ...) {
  counts <- .diagnostic_counts(object$diagnostics)
  n_obs <- .influence_nobs(object)
  data.frame(
    family = "influence",
    estimand = object$spec$estimand %||% NA_character_,
    estimator = object$spec$estimator %||% NA_character_,
    backend = object$backend,
    nobs = as.integer(n_obs),
    n_groups = if (is.null(object$n_groups)) NA_integer_ else as.integer(object$n_groups),
    n_transitions = as.integer(length(object$spec$waves) >= 2L),
    # NA where the backend defines no single R-squared -- a mixed fit has a
    # marginal and a conditional one and they answer different questions, so
    # putting either here under a bare name would pick a side silently. Use
    # pseudo_r2() there.
    r.squared = if (is.null(object$r_squared)) NA_real_ else object$r_squared,
    sigma = if (is.null(object$sigma)) NA_real_ else object$sigma,
    # In-sample root mean squared error. Cheap -- it is arithmetic on residuals
    # the fit already holds -- which is why it belongs in glance(). Anything
    # needing replicate generation or refits does not: a one-row summary must
    # not quietly cost either.
    # NA where the backend exposes no residuals, documented as such rather than
    # substituted.
    rmse = {
      r <- tryCatch(stats::residuals(object$backend_fit), error = function(e) NULL)
      if (is.null(r) || !length(r)) NA_real_ else sqrt(mean(r^2, na.rm = TRUE))
    },
    n_error = counts[["n_error"]], n_warning = counts[["n_warning"]],
    n_note = counts[["n_note"]],
    netis_version = object$netis_version,
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' @rdname netis_glance
#' @export
netis_glance.netis_selection <- function(object, ...) {
  counts <- .diagnostic_counts(object$diagnostics)
  # TOTAL retained draws: the convergence record counts draws per chain.
  n_draws <- if (length(object$convergence) && !is.null(object$convergence[[1]])) {
    object$convergence[[1]]$n_draws * (object$convergence[[1]]$n_chains %||% 1L)
  } else {
    NA_integer_
  }
  pooled <- .selection_is_pooled(object)
  data.frame(
    family = "selection",
    estimand = if (.selection_is_hierarchical(object)) {
      "hierarchical multi-group tie formation"
    } else if (object$n_groups > 1L && !pooled) {
      "per-group tie formation"
    } else {
      "tie formation"
    },
    outcome_family = object$family,
    backend = object$backend,
    nobs = .selection_nobs(object),
    n_groups = as.integer(object$n_groups),
    pooled = pooled,
    latent_dims = as.integer(object$R),
    n_draws = as.integer(n_draws),
    n_error = counts[["n_error"]], n_warning = counts[["n_warning"]],
    n_note = counts[["n_note"]],
    netis_version = object$netis_version,
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' The number of observed dyads a Selection fit's likelihood used
#'
#' Counted from the outcome arrays that were fitted, not from the roster: a
#' dyad whose outcome is missing (unobserved, or masked because a predictor
#' could not be resolved) enters the sampler as missing data and contributes
#' no observation, and the diagonal is never an observation. In a symmetric
#' fit each unordered pair is one observation, so only the upper triangle is
#' counted.
#'
#' @param object a `netis_selection` fit.
#' @return a single integer.
#' @noRd
.selection_nobs <- function(object) {
  count_one <- function(Y) {
    keep <- if (isTRUE(object$symmetric)) upper.tri(Y) else row(Y) != col(Y)
    sum(!is.na(Y[keep]))
  }
  as.integer(sum(vapply(object$designs, function(d) count_one(d$Y), numeric(1))))
}

#' Variance components and residual variance shares
#'
#' The random-effects side of a fit, in one shape. Returns a zero-row frame
#' with a stated reason where the fit has no variance components rather than a
#' table of `NA`s -- an OLS fit does not have a near-zero component, it has none,
#' and the two must not print the same way.
#'
#' `residual_variance_share` is a share of the **fitted** variance components: a
#' level's variance over their total. It is a different quantity from the between-group
#' share of residuals [netis_dependence()] reports, which is computed from
#' residuals and exists to describe structure the model did **not** absorb.
#'
#' # Two ways a quantity can be absent, and why they differ
#'
#' rnetis has two distinct responses to a quantity a fit does not have, and the
#' difference is deliberate rather than inconsistent:
#'
#' - **This function returns an empty frame with the documented columns** and a
#'   `reason` attribute. Variance components are a *component* of a result:
#'   asking for them is reasonable on any fit, the answer is legitimately "there
#'   are none", and downstream code binding several fits' components together
#'   should not have to special-case the ones without. An empty frame of the
#'   right shape composes; an error does not.
#' - **[pseudo_r2()] errors instead.** There, the absence is the whole answer to
#'   the question asked — a caller asking for variance explained by a fit that
#'   has no agreed decomposition is asking something with no correct return
#'   value, and a zero-row frame would read as "explained nothing."
#'
#' Both give an absence rather than a fabricated value, and neither returns a table
#' of `NA`s.
#'
#' @param object a `netis_influence` or `netis_selection` fit.
#' @param ... unused.
#' @return a `data.frame` with `grouping`, `term`, `kind`, `estimate`,
#'   `variance`, `sd`, and `residual_variance_share`. Zero rows, with a
#'   `reason` attribute, where the fit has none.
#'
#'   The share column is deliberately not called `icc`: that name would invite
#'   confusion with the unconditional ICC(1) which [icc1()] reports, and the
#'   two are different quantities (see below).
#'
#'   `estimate` is the generic value: the variance on a variance row, the
#'   covariance on a covariance row. `kind` distinguishes a `"variance"` from a
#'   `"covariance"` or `"correlation"`, and `variance`, `sd` and
#'   `residual_variance_share` are `NA` for the entries that are not variances
#'   rather than being computed anyway.
#'   Every parameter the random-effects structure estimated appears, so nothing
#'   the model estimated goes unreported.
#'
#' @section What `residual_variance_share` is, and when it is absent:
#'
#'   It is a **conditional** variance share: of the variance left unexplained
#'   *after* conditioning on this model's predictors, the proportion at each
#'   level. It is **not** the unconditional ICC(1) of the aggregation literature
#'   (Bliese 2000; Kozlowski & Klein 2000) — that is [icc1()], computed from a
#'   null model with no predictors, and it answers a different question. The two
#'   can differ substantially, and the comparison is informative: a large
#'   [icc1()] beside a small `residual_variance_share` says the predictors
#'   account for much of the original between-group heterogeneity.
#'
#'   **It is `NA` for every row when the model has a random slope**, with the
#'   reason recorded in the returned object's `icc_reason` attribute. Between-
#'   group variance is then `tau00 + 2*x*tau01 + x^2*tau11` — a function of the
#'   slope predictor — so no scalar partitions the variance at any row. The
#'   variance and covariance estimates themselves are still reported; only the
#'   share is withheld, because there is no single value it could take.
#'
#'   For a binary (probit) Selection fit the dyadic residual variance is fixed
#'   at 1 by the identification constraint, not estimated. It still enters the
#'   share denominator, because it is genuinely part of the total variance the
#'   shares are shares of -- but a reader should not interpret it as a finding.
#'
#' @section Selection fits: shares of one dyad's latent variance:
#'
#'   This decomposition of a dyad's latent variance is this package's own
#'   construction, not a published statistic. For a Selection fit the share is each
#'   component's contribution to the latent variance of a single dyad (i, j),
#'   conditional on the predictors:
#'   `va + vb + V_uv + s2` for a directed relation and `2 va + V_uv + s2` for a
#'   symmetric one, where `V_uv` is the variance of the multiplicative term.
#'   There is **no `2 cab` term**: `a_i` and `b_j` belong to different actors
#'   and are independent. The sender-receiver covariance `cab` and the
#'   within-dyad correlation `rho` are reported as rows but carry no share.
#'
#'   With `R > 0` a `multiplicative (latent-factor) variance` row is added and
#'   enters the denominator. It is
#'   the variance of the posterior-mean product, which understates it; the
#'   returned frame then carries a `multiplicative_note` attribute saying so.
#'   On a symmetric fit the actor variance row's share is `2 va` over the total,
#'   because each dyad involves two independently drawn actors.
#'
#'   A hierarchical multi-group Selection fit (`pooling = selection_pooling()`)
#'   is refused with an error naming [netis_hierarchy()] and the fit's
#'   `convergence_table`, which carry its between-group and within-network
#'   quantities.
#' @references
#' Bliese, P. D. (2000) Within-group agreement, non-independence, and reliability:
#' implications for data aggregation and analysis. In K. J. Klein and S. W. J.
#' Kozlowski (eds), *Multilevel Theory, Research, and Methods in Organizations*,
#' 349-381. Jossey-Bass, San Francisco.
#'
#' Kozlowski, S. W. J. and Klein, K. J. (2000) A multilevel approach to theory and
#' research in organizations: contextual, temporal, and emergent processes. In K. J.
#' Klein and S. W. J. Kozlowski (eds), *Multilevel Theory, Research, and Methods in
#' Organizations*. Jossey-Bass, San Francisco.
#'
#' Bates, D., Maechler, M., Bolker, B. and Walker, S. (2015) Fitting linear
#' mixed-effects models using lme4. *Journal of Statistical Software* **67**(1), 1-48.
#'
#' Hoff, P. D. (2021) Additive and multiplicative effects network models.
#' *Statistical Science* **36**(1), 34-50.
#' @seealso [pseudo_r2()], [netis_glance()]
#' @examples
#' set.seed(1)
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
#' fit <- suppressWarnings(fit_influence(
#'   y ~ ar() + exposure(x, via = tie) + (1 | group), data = nd))
#' # A fit with a group random intercept.
#' netis_variance_components(fit)
#' # And the absent case: an OLS fit has no variance components, so this is an
#' # empty frame WITH the documented
#' # columns -- not an error, and not a row of NAs.
#' # One group, so no random-effects term is admissible: this is the OLS route.
#' one <- netis_data(nodes[nodes$group == "a", ], ties[ties$group == "a", ],
#'                   wave_col = "wave")
#' ols <- fit_influence(y ~ ar() + exposure(x, via = tie), data = one)
#' vc <- netis_variance_components(ols)
#' nrow(vc)
#' names(vc)
#' attr(vc, "reason")
#' @export
netis_variance_components <- function(object, ...) {
  UseMethod("netis_variance_components")
}

#' @rdname netis_variance_components
#' @export
netis_variance_components.default <- function(object, ...) {
  stop(
    sprintf("netis_variance_components() has no method for an object of class %s. It is defined for netis_influence and netis_selection fits.",
            paste(sprintf("'%s'", class(object)), collapse = "/")),
    call. = FALSE
  )
}

#' @noRd
.empty_variance_components <- function(reason) {
  structure(
    data.frame(grouping = character(0), term = character(0), kind = character(0),
               estimate = numeric(0), variance = numeric(0), sd = numeric(0),
               residual_variance_share = numeric(0),
               stringsAsFactors = FALSE),
    reason = reason
  )
}

#' Which AME components are variances, and which are not
#'
#' `amen`'s covariance block carries two correlation-like entries alongside the
#' three variances, and averaging them into a column called `variance` would
#' produce numbers with no meaning. They are kept, labeled by `kind`, and
#' excluded from the share's denominator -- dropping them silently would be the
#' silent failure, and mixing them in would fabricate a share.
#'
#' @noRd
.ame_component_kind <- function(nms) {
  ifelse(nms %in% c("va", "vb", "ve", "s2"), "variance",
         ifelse(nms == "cab", "covariance", "correlation"))
}

#' @noRd
.ame_component_label <- function(nms) {
  labels <- c(va = "sender variance", vb = "receiver variance",
              ve = "dyadic residual variance", s2 = "dyadic residual variance", cab = "sender-receiver covariance",
              rho = "within-dyad residual correlation")
  out <- unname(labels[nms])
  out[is.na(out)] <- nms[is.na(out)]
  out
}

#' @rdname netis_variance_components
#' @export
netis_variance_components.netis_influence <- function(object, ...) {
  vc <- object$var_corr
  if (is.null(vc)) {
    return(.empty_variance_components(sprintf(
      "This fit was estimated by '%s', which produces no random-effects variance components. That is an absence, not a value near zero.",
      object$spec$estimator %||% object$backend
    )))
  }
  # Covariance rows (var2 non-NA) are KEPT, labeled by `kind`, with their
  # value in `estimate` -- they are not variances, so `variance`/`sd`/`icc`
  # stay NA rather than carrying a mislabeled number. Filtering them out
  # would drop the intercept-slope covariance entirely, while this function's
  # own documentation promises nothing the model estimated goes unreported.
  is_variance <- is.na(vc$var2)

  # A SCALAR variance share exists only when every random term is an intercept.
  # With a random slope the between-group variance is tau00 + 2*x*tau01 +
  # x^2*tau11 -- a function of the slope predictor -- so no single number
  # partitions the variance, and that holds for EVERY row rather than only the
  # slope's own: the intercept row's denominator would carry tau11
  # unmultiplied, which is not the total variance at any x. Returning
  # component/total throughout would sum to exactly 1 and so read as a
  # coherent decomposition while being a ratio with no estimand.
  #
  # The test is "any non-intercept random term", which catches an uncorrelated
  # slope (`(1 + x || g)`, `(0 + x | g)`) as well as a correlated one -- those
  # produce no covariance row but leave the share just as undefined.
  has_random_slope <- any(is_variance & vc$grp != "Residual" & vc$var1 != "(Intercept)")

  share <- rep(NA_real_, nrow(vc))
  if (!has_random_slope) {
    total <- sum(vc$vcov[is_variance], na.rm = TRUE)
    if (is.finite(total) && total > 0) {
      share[is_variance] <- vc$vcov[is_variance] / total
    }
  }

  out <- data.frame(
    grouping = vc$grp,
    term = ifelse(is_variance, vc$var1, paste(vc$var1, vc$var2, sep = "-")),
    kind = ifelse(is_variance, "variance", "covariance"),
    # The generic value column: the variance on a variance row, the covariance
    # on a covariance row. Carrying the covariance ESTIMATE -- not merely a
    # labeled row with every numeric field NA -- is the point.
    estimate = vc$vcov,
    variance = ifelse(is_variance, vc$vcov, NA_real_),
    sd = ifelse(is_variance, vc$sdcor, NA_real_),
    # The name states what the quantity is: the share of the variance this
    # model left UNEXPLAINED, per level. It is deliberately not called `icc`,
    # which would invite confusion with the unconditional ICC(1) that icc1()
    # reports.
    residual_variance_share = share,
    stringsAsFactors = FALSE, row.names = NULL
  )
  out$term[is.na(out$term)] <- "(residual)"

  if (has_random_slope) {
    attr(out, "icc_reason") <- paste(
      "A scalar variance share is undefined for a model with a random slope:",
      "between-group variance depends on the value of the random-slope",
      "predictor (tau00 + 2*x*tau01 + x^2*tau11), so no single share exists at",
      "any row. The variance and covariance estimates themselves are reported."
    )
  }
  out
}

#' @rdname netis_variance_components
#' @export
netis_variance_components.netis_selection <- function(object, ...) {
  if (.selection_is_hierarchical(object)) {
    stop(
      "netis_variance_components() is not defined for a hierarchical multi-group Selection fit (pooling = selection_pooling()). Its shares are defined for per-group amen::ame() fits. Use netis_hierarchy() for the between-group distribution; each group's within-network quantities (va, cab, vb, rho, and uv_realized with R > 0) and their convergence diagnostics are in fit$convergence_table.",
      call. = FALSE
    )
  }
  parts <- object$per_group
  symmetric <- isTRUE(object$symmetric)
  R <- object$R %||% 0L
  used_posterior_mean_uv <- FALSE

  rows <- lapply(seq_along(parts), function(i) {
    vcomp <- parts[[i]]$variance_components
    # A draws x component MATRIX, not a vector of point estimates: these are
    # posterior draws of the covariance block. Summarizing to the posterior mean
    # is the reduction, and it has to be done here rather than by unlist(),
    # which would flatten S draws of 5 components into 5S unnamed numbers.
    if (is.null(vcomp) || !is.matrix(vcomp) || ncol(vcomp) == 0L) {
      return(NULL)
    }
    v <- colMeans(vcomp, na.rm = TRUE)
    kind <- .ame_component_kind(colnames(vcomp))
    is_var <- kind == "variance"
    term_labels <- .ame_component_label(colnames(vcomp))
    # A symmetric fit's va is not a SENDER variance -- the model has one
    # additive effect per actor with no sender/receiver split, and the label
    # must not reintroduce the distinction the model removed.
    if (symmetric) {
      term_labels[colnames(vcomp) == "va"] <- "actor variance (symmetric: one additive effect)"
    }

    # Exactly zero at R = 0. At R > 0 amen reports the latent factors only as
    # a posterior mean, so the variance of the posterior-mean product is used
    # and the frame says it understates the component.
    v_uv <- 0
    if (R > 0L) {
      mask <- .selection_dyad_mask(object$designs[[i]])
      v_uv <- stats::var(parts[[i]]$multiplicative[mask])
      used_posterior_mean_uv <<- TRUE
    }

    dyad <- .ame_dyad_variance_parts(
      va = v[["va"]],
      vb = if ("vb" %in% names(v)) v[["vb"]] else NULL,
      v_uv = v_uv,
      s2 = v[["ve"]],
      symmetric = symmetric
    )
    total <- dyad[["total"]]
    # What each ROW contributes to a dyad's variance, which is not always the
    # row's own estimate: a symmetric fit's actor variance enters every dyad
    # twice (a_i + a_j over two independent actors), so its contribution is
    # 2 * va, counted the same way pseudo_r2() counts it so the two agree
    # about the same fit.
    contribution <- rep(NA_real_, length(v))
    contribution[colnames(vcomp) == "va"] <- if (symmetric) dyad[["actor"]] else dyad[["sender"]]
    contribution[colnames(vcomp) == "vb"] <- if (symmetric) NA_real_ else dyad[["receiver"]]
    contribution[colnames(vcomp) %in% c("ve", "s2")] <- dyad[["residual"]]

    share_ok <- is.finite(total) && total > 0
    out <- data.frame(
      grouping = as.character(object$groups[i]),
      term = term_labels, kind = kind,
      # Only a variance has a variance and an SD. A correlation has neither, and
      # sqrt() of one is not a standard deviation of anything.
      estimate = unname(v),
      variance = ifelse(is_var, unname(v), NA_real_),
      sd = ifelse(is_var, sqrt(pmax(unname(v), 0)), NA_real_),
      # Both families return one schema, so code handling both does not branch
      # -- the same reason `netis_estimates()` shares its columns across them.
      residual_variance_share = if (share_ok) {
        ifelse(is_var, contribution / total, NA_real_)
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE, row.names = NULL
    )
    if (R > 0L) {
      out <- rbind(out, data.frame(
        grouping = as.character(object$groups[i]),
        term = "multiplicative (latent-factor) variance", kind = "variance",
        estimate = v_uv, variance = v_uv, sd = sqrt(max(v_uv, 0)),
        residual_variance_share = if (share_ok) v_uv / total else NA_real_,
        stringsAsFactors = FALSE, row.names = NULL
      ))
    }
    out
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(.empty_variance_components(
      "This Selection fit records no variance components. Sender, receiver and dyadic variation are carried by the additive-effects covariance instead; see the fit's per-group results."
    ))
  }
  out <- do.call(rbind, rows)
  if (used_posterior_mean_uv) {
    attr(out, "multiplicative_note") <- paste(
      "The multiplicative (latent-factor) variance is the variance of the",
      "posterior-mean U V' over dyads, because amen reports the latent factors",
      "only as a posterior mean. That understates the component, so the other shares are",
      "overstated by a corresponding amount."
    )
  }
  out
}

#' What a fit targets, and how it was routed there
#'
#' The estimand and the routing decisions as data rather than as printed prose,
#' so a report, a comparison table, or a study harness can read the same
#' statement `summary()` prints. Two fits are comparable only if these match,
#' which is precisely why the quantity needs to be machine-readable and not only
#' narrated.
#'
#' @param object a `netis_influence` or `netis_selection` fit.
#' @param ... unused.
#' @return a list with `family`, `estimand`, `description` (one sentence in the
#'   package's own vocabulary), and family-specific routing fields: for
#'   Influence, `transitions`, `outcome_waves`, `estimator`,
#'   and `estimator_reason` ([netis_assumptions()] reports what no estimator
#'   addresses); for
#'   Selection, `route`, `pooling` (`"two_stage"` when the coefficients were
#'   combined across groups by precision weighting, `"hierarchical"` for one
#'   hierarchical model over all groups fitted with [selection_pooling()],
#'   otherwise `"none"`), `pooled_parameters` (the names of the combined
#'   coefficients -- on a hierarchical fit, the shared and random ones; empty
#'   when nothing was pooled), `regime` (hierarchical fits only: each
#'   coefficient's regime, `"shared"`, `"random"` or `"group"`; `NULL`
#'   otherwise), `groups`, `outcome_family` and `latent_dims`.
#' @seealso [netis_glance()], [netis_estimates()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = rep(letters[1:12], 2), wave = rep(1:2, each = 12),
#'   y = c(2.1, 0.4, -1.2, 0.8, 1.5, -0.3, 0.9, -1.1, 0.2, 1.8, -0.7, 0.5,
#'         1.9, 0.1, -0.9, 1.1, 1.2, -0.1, 1.3, -0.8, 0.6, 1.4, -0.4, 0.7)
#' )
#' ties <- data.frame(
#'   group = 1, source = letters[1:11], target = letters[2:12], tie = 1
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' netis_estimand(fit_influence(y ~ ar() + exposure(y, via = tie), data = nd))
#' @export
netis_estimand <- function(object, ...) {
  UseMethod("netis_estimand")
}

#' @rdname netis_estimand
#' @export
netis_estimand.default <- function(object, ...) {
  stop(
    sprintf("netis_estimand() has no method for an object of class %s. It is defined for netis_influence and netis_selection fits.",
            paste(sprintf("'%s'", class(object)), collapse = "/")),
    call. = FALSE
  )
}

#' @rdname netis_estimand
#' @export
netis_estimand.netis_influence <- function(object, ...) {
  spec <- object$spec
  list(
    family = "influence",
    estimand = spec$estimand,
    description = if (length(spec$waves) >= 2L) {
      sprintf("A single transition, %s -> %s.", spec$waves[1], spec$waves[2])
    } else {
      "A single cross-section: one occasion, with no transition to compare across."
    },
    transitions = spec$transitions,
    # The outcome wave is the LAST analyzed wave: `to` of a pair, the one wave of
    # a single-occasion fit, and none at all for data with no wave dimension.
    outcome_waves = if (length(spec$waves) == 0L) {
      character(0)
    } else {
      as.character(spec$waves[[length(spec$waves)]])
    },
    estimator = spec$estimator,
    estimator_reason = spec$estimator_reason
  )
}

#' @rdname netis_estimand
#' @export
netis_estimand.netis_selection <- function(object, ...) {
  pooled <- .selection_is_pooled(object)
  hierarchical <- .selection_is_hierarchical(object)
  regime <- if (hierarchical) object$mgc_draws$regime else NULL
  list(
    family = "selection",
    estimand = if (hierarchical) {
      "hierarchical multi-group tie formation"
    } else if (object$n_groups > 1L && !pooled) {
      "per-group tie formation"
    } else {
      "tie formation"
    },
    description = .selection_pooling_sentence(object),
    route = object$route,
    # A label for the combination actually performed: two-stage precision
    # weighting of the coefficients, one hierarchical model estimated in one
    # chain, or none.
    pooling = if (hierarchical) "hierarchical" else if (pooled) "two_stage" else "none",
    # Hierarchical: the coefficients that borrow strength across groups --
    # shared ones (one common value) and random ones (a between-group
    # distribution); `regime` says which is which.
    pooled_parameters = if (hierarchical) {
      names(regime)[regime != "group"]
    } else if (pooled) {
      names(object$pooled_beta$mean)
    } else {
      character(0)
    },
    regime = if (hierarchical) regime else NULL,
    groups = as.character(object$groups),
    outcome_family = object$family,
    latent_dims = object$R
  )
}

# ---------------------------------------------------------------------------
# The tidy()/glance() bridge
#
# Thin forwarders, deliberately. The implementations live in the netis_*
# accessors and these exist so a user who types `tidy(fit)` out of habit gets an
# answer; keeping the logic in one place is what stops the two spellings
# from drifting into two answers.
#
# The first argument is `x`, not `object`, because generics::tidy and
# generics::glance are `function(x, ...)` and R CMD check's S3 generic/method
# consistency check compares argument names. The netis_* accessors keep
# `object`, which reads better on a standalone call.

#' Tidy and glance methods for rnetis fits
#'
#' `tidy()` and `glance()` are the widely-used conventions for "model results as
#' a data frame" and "one-row model summary". rnetis implements them by
#' forwarding to [netis_estimates()] and [netis_glance()], which are the same
#' functions under names that do not require knowing which package owns the
#' verb.
#'
#' The generics come from `generics` — a dependency with nothing behind it
#' (`Imports: methods` and no more) — and are re-exported, so `tidy(fit)` works
#' after `library(rnetis)` with no `broom` installed. If `broom` is attached as
#' well, R reports the name as masked; that is cosmetic, because both packages
#' re-export the same generic and dispatch is identical.
#'
#' **Neither family has a `statistic` or a `p.value` column.** A posterior has
#' neither, the Influence table reports intervals, and the convention tolerates an
#' absent column.
#'
#' @param x a `netis_influence` or `netis_selection` fit.
#' @param ... passed to the underlying accessor — notably `level` for `tidy()`.
#' @return for `tidy()`, the [netis_estimates()] data frame; for `glance()`, the
#'   [netis_glance()] one-row data frame.
#' @seealso [netis_estimates()], [netis_glance()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = rep(letters[1:12], 2), wave = rep(1:2, each = 12),
#'   y = c(2.1, 0.4, -1.2, 0.8, 1.5, -0.3, 0.9, -1.1, 0.2, 1.8, -0.7, 0.5,
#'         1.9, 0.1, -0.9, 1.1, 1.2, -0.1, 1.3, -0.8, 0.6, 1.4, -0.4, 0.7)
#' )
#' ties <- data.frame(
#'   group = 1, source = letters[1:11], target = letters[2:12], tie = 1
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(y ~ ar() + exposure(y, via = tie), data = nd)
#' tidy(fit)
#' glance(fit)
#' @name netis-tidiers
NULL

#' @rdname netis-tidiers
#' @importFrom generics tidy
#' @export
generics::tidy

#' @rdname netis-tidiers
#' @importFrom generics glance
#' @export
generics::glance

#' @rdname netis-tidiers
#' @exportS3Method generics::tidy
tidy.netis_influence <- function(x, ...) netis_estimates(x, ...)

#' @rdname netis-tidiers
#' @exportS3Method generics::tidy
tidy.netis_selection <- function(x, ...) netis_estimates(x, ...)

#' @rdname netis-tidiers
#' @exportS3Method generics::glance
glance.netis_influence <- function(x, ...) netis_glance(x, ...)

#' @rdname netis-tidiers
#' @exportS3Method generics::glance
glance.netis_selection <- function(x, ...) netis_glance(x, ...)
