# Backend adapter for amen, used by fit_selection(), which fits one network
# per group through amen::ame().
#
# amen objects never escape this file. Everything downstream consumes only
# what backend_extract() returns, as with the lme4 and stats adapters.
#
# Two behaviors of this backend, both verified against amen 1.4.5 rather than
# assumed, shape the adapter and are enforced here:
#
#   design_array() silently replaces NA with zero. rnetis therefore never
#   passes an incomplete design array; .assert_design_complete() refuses at
#   this boundary rather than letting a warning buried in MCMC output be the
#   user's only notice that a predictor was invented.
#
#   Coefficient names do not survive: a single named dyadic covariate came
#   back as ".dyad". The adapter reattaches the names rnetis assembled, so
#   summary() speaks in the user's terms rather than the backend's.
#
# A third, verified rather than assumed: `setTimeLimit()` DOES interrupt
# `amen::ame()`, including with latent factors (`R > 0`). The sampler is an
# R-level loop over `nscan` iterations calling compiled linear algebra PER
# ITERATION, not one monolithic C call, so R's evaluator regains control
# between scans. A time limit set around a Selection fit is therefore an
# effective bound, though it does not cover a process that is killed outright
# (a SIGKILL or an out-of-memory kill), or an amen release that restructures
# the sampler.

#' @exportS3Method
backend_info.netis_backend_amen <- function(backend) {
  list(
    name = "amen",
    package = "amen",
    version = as.character(utils::packageVersion("amen")),
    citation = "Hoff, P., Fosdick, B., and Volfovsky, A. (2024). amen: Additive and Multiplicative Effects Models for Networks and Relational Data."
  )
}

#' Fit an AME model
#'
#' `formula` in the shared generic's signature is, for this backend, the
#' assembled design: a list carrying `Y`, `Xrow`, `Xcol`, `Xdyad` and the
#' outcome family. The generic's parameter name is shared by every backend and
#' is not renamed for one; what varies is what a backend needs handed to it.
#'
#' @noRd
#' @exportS3Method
backend_fit.netis_backend_amen <- function(backend, formula, data, ...) {
  design <- formula
  .assert_amen_available()
  .assert_amen_design(design)

  dots <- list(...)
  symmetric <- isTRUE(design$symmetric)
  base_args <- list(
    Y = design$Y,
    Xdyad = design$Xdyad,
    Xrow = design$Xrow,
    Xcol = design$Xcol,
    family = design$family,
    R = design$R %||% 0
  )
  # Added only when TRUE, so a directed fit passes amen no symmetric argument.
  if (symmetric) {
    base_args$symmetric <- TRUE
  }

  # ONE SAMPLER: amen's own. amen::ame() summarizes the additive effects and
  # latent factors to posterior means and discards the per-draw values, so a
  # rnetis fit reports what amen reports. Where a quantity would need those
  # per-draw values, the affected summary says so and uses the posterior-mean
  # plug-in rather than implying an uncertainty it did not propagate.

  # amen prints burn-in progress to stdout even under print = FALSE, which
  # would spray "20 pct burnin complete" across a user's console on every fit.
  # capture.output() takes only printed text -- warnings and errors are
  # conditions and still propagate, so nothing diagnostic is swallowed.
  # amen::ame() calls set.seed(seed) itself (default 1). The caller's global
  # RNG state is saved here and restored when this call ends, so fitting does
  # not reset the stream of a simulation loop that calls it.
  if (is.null(dots$seed)) dots$seed <- 1
  .sim_restore_stream_on_exit(dots$seed)
  fitted <- NULL
  utils::capture.output(
    fitted <- do.call(amen::ame, c(base_args, list(print = FALSE, plot = FALSE), dots))
  )

  # The names rnetis assembled are reattached immediately, so nothing
  # downstream ever sees amen's own naming. Done here rather than in
  # backend_extract() so that even a debugging look at the raw fit is honest.
  structure(
    list(fit = fitted,
         coef_names = .amen_coef_names(design), symmetric = symmetric),
    class = "netis_amen_fit"
  )
}

#' Extract the fields every backend reports from an AME fit
#'
#' Three fields need explanation, because the honest answer is not the
#' obvious one.
#'
#' `loglik` is `NA`. amen returns no log-likelihood, and reconstructing one
#' from posterior draws would be a different quantity wearing the same name.
#' `AIC`/`BIC` are therefore undefined for this backend.
#'
#' `vcov` is the posterior covariance of the retained coefficient draws, not a
#' sampling covariance matrix. It is the right object for Wald-style intervals
#' only in the sense that it summarizes posterior spread; quantile intervals
#' from the draws themselves are preferable and are what the draws are
#' retained for.
#'
#' `convergence` reports rather than verdicts. amen runs a single
#' chain; split R-hat and per-coefficient ESS are computed from it via the
#' shared estimator (`.amen_convergence()`, R/mcmc-diagnostics.R), with a note
#' stating what one chain cannot establish -- a sampler that settled quickly
#' into the wrong region is invisible to any within-chain statistic.
#'
#' @noRd
#' @exportS3Method
backend_extract.netis_backend_amen <- function(backend, fit) {
  draws <- fit$fit$BETA
  # amen::design_array() silently OMITS the intercept column whenever any
  # design column has zero variance across the assembled n x n array (a
  # small group with a constant sender/receiver/dyad covariate is a
  # realistic way to hit this, and this package's own masking/zero-fill
  # logic can produce an all-constant column too). .amen_coef_names()
  # always assumes an intercept was fitted. Silently proceeding would
  # either error confusingly inside colnames<- (length mismatch) or, if the
  # lengths happened to coincide, attach the wrong name to every
  # coefficient -- a silently wrong number, and
  # worse than an error because it looks like a normal result. Checked here
  # rather than by reimplementing amen's zero-variance detection, so the
  # guard survives amen changing that internal rule.
  if (ncol(draws) != length(fit$coef_names)) {
    stop(
      sprintf(
        "Internal error: the fitted model has %d coefficient(s) but rnetis assembled %d name(s) for it (%s). This happens when a design column is constant across every dyad -- amen::design_array() drops the intercept term in that case, which rnetis's coefficient naming did not anticipate. If a sender(), receiver(), dyad(), prior_tie(), or prior_reciprocity() term has no variation in this group, check for a small or highly masked network before refitting.",
        ncol(draws), length(fit$coef_names), paste(fit$coef_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  colnames(draws) <- fit$coef_names

  symmetric <- isTRUE(fit$symmetric)
  list(
    coefficients = colMeans(draws),
    vcov = stats::cov(draws),
    draws = draws,
    # amen saves the variance components at every scan it keeps, in the same
    # row order as BETA, so these draws need no matching step.
    variance_components = fit$fit$VC,
    # A symmetric fit has ONE additive effect (amen returns APM and no BPM);
    # reporting it on both margins is the mathematical truth of that model
    # (a_i enters every dyad the actor touches, on either side), and it is
    # what keeps every outer(a, b, "+") consumer correct without a branch.
    additive_effects = if (symmetric) {
      list(sender = fit$fit$APM, receiver = fit$fit$APM)
    } else {
      list(sender = fit$fit$APM, receiver = fit$fit$BPM)
    },
    # UVPM is the posterior mean of U V', which is rotation-invariant. U and V
    # individually are a single draw and carry no cross-fit meaning, so they
    # are deliberately not passed on. The symmetric fit's
    # ULUPM is the same posterior-mean multiplicative matrix under its
    # eigen-parameterization.
    multiplicative = if (symmetric) fit$fit$ULUPM else fit$fit$UVPM,
    posterior_predictive = fit$fit$YPM,
    convergence = .amen_convergence(draws),
    loglik = NA,
    symmetric = symmetric
  )
}

#' @noRd
#' @exportS3Method
backend_predict.netis_backend_amen <- function(backend, fit, newdata) {
  stop(
    "predict() is not defined for a Selection fit in this version. An AME model predicts a tie probability for a dyad among the actors it was fitted on, which is not the same operation as predicting from new data, and the two must not share a method. Use selection_effect() for probability-scale contrasts.",
    call. = FALSE
  )
}

#' Summarize what a single MCMC chain can and cannot establish
#'
#' Per-coefficient rank-normalized, folded split R-hat and bulk and tail
#' effective sample sizes (Vehtari et al. 2021) from the retained draws, through
#' the shared estimator in `R/mcmc-diagnostics.R`. No threshold is applied and
#' no verdict is printed.
#'
#' amen runs one chain, but split R-hat halves the chain and compares the
#' halves, so it is computable from a single run and detects drift within it. What one
#' chain cannot do is reveal a sampler that settled quickly into the wrong
#' region, and the note says exactly that rather than leaving the number to
#' imply more than it supports.
#'
#' @noRd
.amen_convergence <- function(draws) {
  d <- .netis_mcmc_diagnostics(draws)
  list(
    n_draws = d$n_draws,
    ess = d$ess,
    ess_bulk = d$ess_bulk,
    ess_tail = d$ess_tail,
    rhat = d$rhat,
    rhat_bulk = d$rhat_bulk,
    rhat_folded = d$rhat_folded,
    status = d$status,
    table = d$table,
    ess_min = d$ess_min,
    ess_tail_min = d$ess_tail_min,
    rhat_max = d$rhat_max,
    ess_worst = d$ess_worst,
    rhat_worst = d$rhat_worst,
    n_chains = d$n_chains,
    scope = d$scope,
    note = d$note
  )
}

#' Reattach the column names rnetis assembled
#'
#' amen's returned BETA columns are `intercept` plus its own derived names,
#' which do not reliably carry the supplied dimnames through.
#'
#' @noRd
.amen_coef_names <- function(design) {
  c(
    "intercept",
    if (!is.null(design$Xrow)) colnames(design$Xrow),
    if (!is.null(design$Xcol)) colnames(design$Xcol),
    if (!is.null(design$Xdyad)) dimnames(design$Xdyad)[[3]]
  )
}

#' Refuse an incomplete or malformed design before calling the backend
#'
#' @noRd
.assert_amen_design <- function(design) {
  if (is.null(design$Y)) {
    stop("Internal error: the assembled design has no outcome matrix.", call. = FALSE)
  }
  for (block in list(list(x = design$Xrow, what = "sender terms"),
                     list(x = design$Xcol, what = "receiver terms"),
                     list(x = design$Xdyad, what = "dyadic terms"))) {
    if (!is.null(block$x)) {
      .assert_design_complete(block$x, block$what)
    }
  }
  invisible(TRUE)
}

#' Whether a binary network is regular in degree, as amen's prior sees it
#'
#' Mirrors the data-derived prior in `amen::ame()` for a binary outcome: the
#' row and column means of the standardized ties. When both are constant --
#' every actor sends the same number of ties and receives the same number --
#' amen's prior for the sender-receiver covariance is the zero matrix and its
#' sampler fails on a singular system.
#'
#' @param Y an n x n binary matrix (diagonal ignored) with tie variation.
#' @return `TRUE` or `FALSE`.
#' @noRd
.amen_regular_degree <- function(Y) {
  Y <- as.matrix(Y)
  ydist <- table(Y)
  ymode <- as.numeric(names(ydist)[ydist == max(ydist)])[1]
  YB <- 1 * (Y != ymode)
  ybar <- mean(YB, na.rm = TRUE)
  if (!is.finite(ybar) || ybar <= 0 || ybar >= 1) return(FALSE)
  E <- (YB - ybar) / stats::dnorm(stats::qnorm(ybar))
  diag(E) <- 0
  a <- rowMeans(E, na.rm = TRUE); a[is.na(a)] <- 0
  b <- colMeans(E, na.rm = TRUE); b[is.na(b)] <- 0
  flat <- function(v) max(abs(v - mean(v))) <= 1e-10 * max(1, abs(v))
  flat(a) && flat(b)
}

#' @noRd
.selection_small_group_message <- function(group, n) {
  sprintf(
    "Group '%s' has %d actor(s). amen::ame() cannot fit a network of fewer than 3 actors (its sampler fails on it), so the group cannot be fitted. Remove the group.",
    group, n
  )
}

#' @noRd
.selection_regular_degree_message <- function(group) {
  sprintf(
    "Group '%s' is a regular-degree network: every actor sends the same number of ties and receives the same number. amen's data-derived prior for the sender and receiver variances is then the zero matrix, and its sampler fails on a singular system; this is a property of amen::ame() that rnetis cannot change. Remove the group, or check that its ties were recorded as intended.",
    group
  )
}

#' Refuse a group amen::ame() cannot fit, naming the group
#'
#' Checked before the backend runs, so a degenerate group is reported by name
#' rather than as an error from inside the sampler: fewer than three actors; a
#' binary group with no tie variation (all observed ties 0, or all 1); a binary
#' group that is regular in degree; a predictor that is constant over the
#' group's whole design, which is perfectly collinear with the intercept; and a
#' group whose dyadic correlation would start at +/-1.
#'
#' @param design the stacked design for one group.
#' @param family the outcome family.
#' @param group the group ID.
#' @noRd
.assert_amen_group_fittable <- function(design, family, group) {
  Y <- as.matrix(design$Y)
  n <- nrow(Y)
  if (n < 3L) {
    stop(.selection_small_group_message(group, n), call. = FALSE)
  }
  if (identical(family, "bin")) {
    off <- Y
    diag(off) <- NA
    obs <- off[!is.na(off)]
    if (length(obs) && (all(obs == 0) || all(obs == 1))) {
      stop(sprintf(
        "Group '%s' has no variation in its binary ties: every observed tie is %d. amen::ame() cannot fit such a network (its starting values are undefined). Remove the group, or fit every group together with a random intercept, pooling = selection_pooling(random = \"intercept\"), which admits such a group.",
        group, as.integer(obs[1])
      ), call. = FALSE)
    }
    if (.amen_regular_degree(off)) {
      stop(.selection_regular_degree_message(group), call. = FALSE)
    }
  }
  off_diag <- row(Y) != col(Y)
  constant <- character(0)
  for (block in list(design$Xrow, design$Xcol)) {
    if (is.null(block)) next
    for (nm in colnames(block)) {
      if (length(unique(block[, nm])) < 2L) constant <- c(constant, nm)
    }
  }
  if (!is.null(design$Xdyad)) {
    for (nm in dimnames(design$Xdyad)[[3]]) {
      if (length(unique(design$Xdyad[, , nm][off_diag])) < 2L) constant <- c(constant, nm)
    }
  }
  if (length(constant)) {
    stop(sprintf(
      "In group '%s', %s %s the same value for every actor or dyad, so within that group's fit it is perfectly collinear with the intercept and cannot be estimated. Remove the term, or fit every group together (pooling = selection_pooling()) with the coefficient shared, so that the other groups identify it.",
      group, paste(sprintf("'%s'", constant), collapse = ", "),
      if (length(constant) == 1L) "takes" else "take"
    ), call. = FALSE)
  }
  # A symmetric fit has no dyadic correlation to start.
  if (!isTRUE(design$symmetric) && .amen_start_rho_at_bound(design, family)) {
    stop(.selection_rho_bound_message(group, n), call. = FALSE)
  }
  invisible(TRUE)
}

#' Whether amen::ame() would start a group's dyadic correlation at +/-1
#'
#' amen's start for `rho` is the correlation of the two triangles of the residual
#' after its additive start. With very few actors (in practice, three) that
#' residual can have the two directions of every pair perfectly correlated. When
#' the computed start is exactly +/-1 the dyadic covariance is singular and
#' `amen::rrho_mh()` fails at the first scan with "missing value where TRUE/FALSE
#' needed". A start that rounding leaves just inside the bound runs, so this tests
#' the exact condition, as a tolerance would refuse designs amen fits. The start
#' depends only on the ties and the predictors, not on the seed. Rounding here
#' can differ from amen's in the last bit, so a failure this misses is explained
#' by `.selection_backend_failure()`.
#'
#' @param design the stacked design for one group.
#' @param family the outcome family.
#' @return `TRUE` or `FALSE`.
#' @noRd
.amen_start_rho_at_bound <- function(design, family) {
  Y <- as.matrix(design$Y)
  n <- nrow(Y)
  diag(Y) <- NA
  X <- .mgc_design(design$Xrow, design$Xcol, design$Xdyad, n)
  rho <- .mgc_start_from_latent(.mgc_start_latent(Y, n, family), X)$rho
  !is.finite(rho) || rho^2 >= 1
}

#' The error reported when the sampler itself fails on one group
#'
#' Names the group. amen's "missing value where TRUE/FALSE needed" is what its
#' update of the dyadic correlation raises on a non-finite value, so that message
#' also says what is known about it.
#'
#' @param group the group ID.
#' @param n the group's number of actors.
#' @param message the backend's error message.
#' @return the message to raise.
#' @noRd
.selection_backend_failure <- function(group, n, message) {
  out <- sprintf("Fitting group '%s' failed: %s", group, message)
  if (grepl("missing value where TRUE/FALSE needed", message, fixed = TRUE)) {
    out <- paste0(out, sprintf(
      ". amen::ame() raises this when its update of the dyadic correlation meets a non-finite value. In a group of %d actors, few dyads inform that correlation and the additive effects, and this can follow from the data rather than from the chain settings. Check the group's ties and predictors, or fit every group together with pooling = selection_pooling(), whose sampler starts this correlation at 0.",
      n
    ))
  }
  out
}

#' @noRd
.selection_rho_bound_message <- function(group, n) {
  sprintf(
    "Group '%s' has %d actors, and with these ties and predictors amen::ame() would start its dyadic correlation at +/-1: after its additive start, the two directions of every pair are perfectly correlated. Its sampler then fails on a singular covariance, and no seed changes that. Add actors, remove the group, or fit every group together with pooling = selection_pooling(), whose sampler starts this correlation at 0.",
    group, n
  )
}

#' @noRd
.assert_amen_available <- function() {
  if (!requireNamespace("amen", quietly = TRUE)) {
    stop(
      "Selection models require the amen package. Install it with install.packages(\"amen\").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
