# Hierarchical multi-group Selection: the interface to the portable core
# (R/mgc-*.R). The model is specified in full at ?hierarchical_selection
# (R/selection-hierarchical-model.R).
#
# WHAT THIS ADDS. Each selection coefficient is shared, group-specific, or
# random -- drawn from a between-group distribution
#
#     beta_kg = m_kg + zeta_kg,   zeta_g ~ N(0, Sigma)
#
# with m_kg a population mean, or z_g' gamma_k with group-level moderators, and
# Sigma diagonal or unstructured (LKJ on the correlations). Everything is
# estimated jointly with every group's own AME structure in ONE chain over all
# groups.
#
# WHAT IT DELIBERATELY DOES NOT POOL. The nuisance AME structure stays
# group-specific: each group keeps its own sender and receiver effects, their
# covariance, its dyadic correlation and residual variance, and its own latent
# factors. Only the selection coefficients get the hierarchical layer. Pooling
# nuisance structure would change what every other estimand in the package
# means, so it is not part of this model.
#
# HOW THIS DIFFERS FROM `pool_beta = TRUE`. That route fits each group on its
# own and combines the coefficient estimates afterwards by inverse-variance
# weighting: two stages, no between-group variance component, heterogeneity
# assumed away. This route is one model: the groups are estimated together, the
# between-group SD is a parameter, and a group's coefficient is shrunk toward
# the population mean by how much information that group actually carries. They
# answer different questions and are not interchangeable.

#' Declare how Selection coefficients pool across groups
#'
#' Passed to [fit_selection()] as `pooling =`. Supplying it selects the
#' hierarchical multi-group sampler, which fits every group in one model and
#' one chain; leaving it `NULL` keeps the per-group fits with optional
#' two-stage precision pooling (`pool_beta`). The model, its priors and its
#' algorithm are specified at [hierarchical_selection].
#'
#' **Teach the diagonal model first.** Start with a random intercept
#' (`random = "intercept"`), add random slopes with the default diagonal
#' covariance, and only then consider `covariance = "unstructured"`, which
#' needs enough groups to inform the correlations.
#'
#' **Experimental interface.** This is the smallest specification that
#' expresses the three coefficient regimes (shared, unpooled, random); code
#' written against it should expect it may change.
#'
#' @param random names of the coefficients whose effects vary across groups,
#'   `beta_kg ~ N(mu_k, tau_k^2)`; or `NULL` for none. Use `"intercept"` for
#'   the group baseline. Several may be named: by default they vary
#'   **independently**, each with its own between-group SD drawn from its own
#'   conditional. `covariance = "unstructured"` estimates the correlations
#'   between them instead.
#' @param moderators optional group-level predictors of the random
#'   coefficients: `theta_kg = z_g' gamma_k + zeta_kg`. A data frame with one
#'   row per group, naming the group in a `group` column or in its row names,
#'   plus one numeric column per moderator. Rows are matched to groups **by
#'   name**, never by position, and must cover the fit's groups exactly. Encode
#'   a categorical moderator as indicator columns yourself, so the reference
#'   category is your choice and visible in the output. A missing value, a
#'   non-numeric column, or a column constant across groups is refused.
#' @param A0_Z the scale of the normal prior on a moderator coefficient, in
#'   units of the effect of a reference change in the predictor per reference
#'   change in the moderator. The default 0.5 is weak regularization on that
#'   standardized contrast, matching `A0`.
#' @param delta_Z the reference change each moderator's prior is expressed per,
#'   named for the moderators it describes, and an error without `moderators`.
#'   `NULL` (the default) derives it:
#'   `1` for a binary moderator (the 0 to 1 contrast, with no inflation for a
#'   rare one), otherwise the SD across the group values. **That default is a
#'   between-group quantity computed from `G` numbers** and is flagged as such
#'   on the fit, because a moderator is group-constant by definition and has no
#'   within-group variation to supply a reference change.
#' @param covariance the between-group covariance of the random coefficients.
#'   `"diagonal"` (the default) gives each its own independent SD.
#'   `"unstructured"` estimates the full covariance
#'   `Sigma = diag(tau) Omega diag(tau)` with an LKJ prior on `Omega`, so a
#'   group with little information on one coefficient borrows from what its
#'   others say. The two are **different models**, not a refinement: a diagonal
#'   fitted where the truth is correlated assumes the correlations away rather
#'   than approximating them. Requires at least two random coefficients.
#' @param eta the shape of the LKJ prior on the between-group correlation
#'   matrix, used only when `covariance = "unstructured"`. `1` (the default) is
#'   uniform over correlation matrices; larger values concentrate the prior
#'   toward no correlation (`2` is the sensitivity setting). Any value other
#'   than `1` under a diagonal covariance is an error, as there are no
#'   correlations for it to act on. With
#'   `q` random coefficients each correlation's marginal prior is
#'   Beta\eqn{(\eta - 1 + q/2, \eta - 1 + q/2)} on (-1, 1), so `eta = 1` gives
#'   uniform pairwise correlations only when `q = 2`.
#' @param unpooled names of coefficients estimated separately in each group,
#'   with no between-group structure at all. Everything not named here or in
#'   `random` is shared across groups.
#' @param A0 the scale of the half-t prior on `tau`, in units of the effect of
#'   a **reference change** in the predictor (see **The heterogeneity prior**).
#'   The default `0.5` is a **weak-regularization** choice on the standardized
#'   contrast scale; `0.25` and `1.0` are the documented sensitivity settings.
#' @param nu the degrees of freedom of that half-t prior. The default `3` is a
#'   package convention.
#' @param delta the reference predictor change `tau` is expressed per: one value,
#'   or one per coefficient in the order of `random`. `NULL` (the default) derives
#'   it: `1` for the intercept and for a binary predictor (the 0 to 1 contrast),
#'   and otherwise the median of the informative groups' within-group SDs.
#' @return an object of class `netis_selection_pooling`.
#' @section The heterogeneity prior:
#' The prior is placed on the heterogeneity of the *effect of a meaningful
#' predictor change*, not on the raw coefficient: `tau_k * delta_k ~
#' half-t(nu, A0)`. A raw-coefficient prior would mean something different for
#' a predictor measured in years than for the same predictor measured in
#' decades; this one does not. `A0` is in latent tie-propensity units under the
#' probit, and in the outcome's own scale for `family = "nrm"`.
#'
#' **Why `A0 = 0.5`.** Once `tau` is expressed per reference change the prior
#' lives on a standardized contrast scale, and the question becomes how much
#' regularization a between-group SD should carry by default there. `0.5` is a
#' weak-regularization choice: it neither concentrates `tau` near zero nor
#' leaves it effectively unconstrained. It is NOT chosen for sitting near any
#' particular true value, and no simulation selected it -- choosing a prior for
#' what it recovers in one study is how a prior stops being a prior. The grid
#' is stated so a user can move along it deliberately:
#'
#' | `A0` | role |
#' |---|---|
#' | `0.25` | stronger-regularization sensitivity |
#' | **`0.5`** | **default, weakly informative** |
#' | `1.0` | diffuse sensitivity |
#'
#' Sensitivity to `A0` depends on the data at hand; refitting at the two
#' sensitivity settings is how to check it for a given analysis.
#'
#' Within-group variation is what identifies a group-specific slope, so the
#' reference change is a within-group one. A group with no within-group
#' variation in the predictor is **not** refused: it contributes no direct
#' information about its own coefficient, is flagged as such, and its estimate
#' comes from partial pooling toward the population distribution. See
#' [netis_hierarchy()].
#'
#' These defaults, the reference-change rules and the 1% threshold for a group
#' to count as informative are this package's own constructions; every value is
#' listed at [hierarchical_selection].
#' @references
#' Gelman, A. (2006). Prior distributions for variance parameters in
#' hierarchical models. *Bayesian Analysis*, 1(3), 515-534.
#'
#' Lewandowski, D., Kurowicka, D., & Joe, H. (2009). Generating random
#' correlation matrices based on vines and extended onion method. *Journal of
#' Multivariate Analysis*, 100(9), 1989-2001.
#' @seealso [hierarchical_selection], [netis_hierarchy()], [fit_selection()]
#' @export
#' @examples
#' selection_pooling(random = "intercept")
selection_pooling <- function(random = NULL, unpooled = character(),
                              A0 = 0.5, nu = 3, delta = NULL,
                              moderators = NULL, A0_Z = 0.5, delta_Z = NULL,
                              covariance = c("diagonal", "unstructured"),
                              eta = 1) {
  covariance <- match.arg(covariance)
  if (!is.numeric(eta) || length(eta) != 1L || !is.finite(eta) || eta <= 0) {
    stop("eta must be a single finite positive number: the shape of the LKJ prior on the between-group correlations.",
         call. = FALSE)
  }
  if (identical(covariance, "diagonal") && eta != 1) {
    # The same rule as delta_Z without moderators: a setting for a part of the
    # model that is not being fitted is a mistake to report, not to ignore.
    stop("eta is the LKJ shape for the between-group CORRELATIONS, which only covariance = \"unstructured\" estimates. A diagonal covariance has no correlations, so eta would be silently ignored.",
         call. = FALSE)
  }
  # TWO DIFFERENT MODELS, NAMED. "diagonal" gives each random coefficient its
  # own independent between-group SD; "unstructured" estimates the full
  # between-group covariance Sigma = diag(tau) Omega diag(tau), so a group
  # with little information on one coefficient borrows from what its OTHER
  # coefficients say. A diagonal is not an approximation to a correlated
  # Sigma -- it assumes the correlations away -- which is why this is a named
  # choice rather than an implementation detail, and why `covariance` is
  # declared on every fit.
  if (identical(covariance, "unstructured") && length(random %||% character()) < 2L) {
    stop("An unstructured between-group covariance needs at least two random coefficients; with one there is no correlation to estimate and the covariance is just its variance.",
         call. = FALSE)
  }
  if (!is.null(random) && (!is.character(random) || !length(random) || anyNA(random))) {
    stop("random must name one or more coefficients, or be NULL.", call. = FALSE)
  }
  if (!is.null(random) && anyDuplicated(random)) {
    stop(sprintf("random names the same coefficient more than once: %s.",
                 paste(unique(random[duplicated(random)]), collapse = ", ")),
         call. = FALSE)
  }
  if (!is.character(unpooled) || anyNA(unpooled)) {
    stop("unpooled must be a character vector of coefficient names.", call. = FALSE)
  }
  clash <- intersect(random %||% character(), unpooled)
  if (length(clash)) {
    stop(sprintf("%s named both random and unpooled; a coefficient has one regime.",
                 paste(sprintf("'%s' is", clash), collapse = "; ")),
         call. = FALSE)
  }
  for (nm in c("A0", "nu")) {
    v <- get(nm)
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || v <= 0) {
      stop(sprintf("%s must be a single positive number.", nm), call. = FALSE)
    }
  }
  if (!is.null(delta)) {
    # One reference change, or one per random coefficient. A predictor that
    # varies widely everywhere and one that barely varies do not share a delta
    # merely because they are random in the same fit.
    n_random <- length(random %||% 1)
    ok <- is.numeric(delta) && !anyNA(delta) && all(delta > 0) &&
      length(delta) %in% c(1L, n_random)
    if (!ok) {
      stop(sprintf("delta must be positive, and either one value or one per random coefficient (%d given for %d).",
                   length(delta), n_random), call. = FALSE)
    }
  }
  if (!is.null(moderators)) {
    if (!length(random)) {
      stop("moderators describe how a RANDOM coefficient varies across groups, so at least one coefficient must be random. With none, every coefficient is either shared -- identical in every group -- or group-specific and already free, and a group-level predictor has nothing to explain.",
           call. = FALSE)
    }
    moderators <- .selection_moderator_frame(moderators)
  }
  if (!is.numeric(A0_Z) || length(A0_Z) != 1L || is.na(A0_Z) || A0_Z <= 0) {
    stop("A0_Z must be a single positive number.", call. = FALSE)
  }
  if (!is.null(delta_Z)) {
    if (is.null(moderators)) {
      stop("delta_Z is the reference change for a MODERATOR, so it means nothing without moderators.", call. = FALSE)
    }
    mods <- setdiff(names(moderators), ".group")
    if (is.null(names(delta_Z)) || !all(names(delta_Z) %in% mods)) {
      stop(sprintf("delta_Z must be named for the moderators it describes (%s). An unnamed reference change cannot be matched to a column, and matching by position would attach one moderator's scale to another.",
                   paste(mods, collapse = ", ")), call. = FALSE)
    }
    if (!is.numeric(delta_Z) || anyNA(delta_Z) || any(delta_Z <= 0)) {
      stop("delta_Z must be positive and non-missing.", call. = FALSE)
    }
  }
  structure(list(random = random, unpooled = unpooled, A0 = A0, nu = nu, delta = delta,
                 covariance = covariance, eta = as.numeric(eta),
                 moderators = moderators, A0_Z = A0_Z, delta_Z = delta_Z),
            class = "netis_selection_pooling")
}

#' @export
print.netis_selection_pooling <- function(x, ...) {
  cat("<netis_selection_pooling>\n")
  cat(sprintf("  random:   %s\n",
              if (length(x$random)) paste(x$random, collapse = ", ") else "none"))
  cat(sprintf("  unpooled: %s\n",
              if (length(x$unpooled)) paste(x$unpooled, collapse = ", ") else "none"))
  cat(sprintf("  everything else: shared across groups\n"))
  if (length(x$random)) {
    cat(sprintf("  tau prior: half-t(nu = %g, A0 = %g) per %s\n", x$nu, x$A0,
                if (is.null(x$delta)) {
                  "derived reference change"
                } else {
                  sprintf("delta = %s", paste(signif(x$delta, 4), collapse = ", "))
                }))
    if (length(x$random) > 1L) {
      # Stated wherever the specification is printed, and read off the
      # specification, so an unstructured request is never described as
      # independent.
      if (identical(x$covariance, "unstructured")) {
        cat(sprintf("  between-group covariance: unstructured (correlations estimated, LKJ(eta = %g) prior)\n",
                    x$eta %||% 1))
      } else {
        cat("  between-group covariance: diagonal (coefficients vary independently)\n")
      }
    }
    if (!is.null(x$moderators)) {
      cat(sprintf("  moderators: %s (enter the mean of every random coefficient)\n",
                  paste(setdiff(names(x$moderators), ".group"), collapse = ", ")))
    }
  }
  invisible(x)
}

#' The between-group layer of a hierarchical Selection fit
#'
#' Reports the three quantities the hierarchical layer estimates -- the
#' population mean `mu`, the between-group SD `tau`, and the partially pooled
#' group-specific coefficients -- together with how much direct information
#' each group carries about its own coefficient.
#'
#' @param object a `netis_selection` fit made with a `pooling` that names a
#'   random coefficient.
#' @param level credible-interval level. Intervals are equal-tailed posterior
#'   quantiles.
#' @return a list with:
#'   * `coefficient` and `covariance`;
#'   * `population`: the hierarchical mean (`mu`; with moderators, its value at
#'     every moderator equal to zero) and the between-group SD `tau`, each with
#'     posterior mean, quantile interval, median, and convergence diagnostics.
#'     With several random coefficients this table and `groups` gain a
#'     `coefficient` column;
#'   * `groups`: one row per group -- the posterior mean and interval of its
#'     coefficient `beta_g`, its within-group predictor SD, whether it is
#'     informative, whether its binary ties vary (`tie_variation`), the
#'     conditional own-data weight and its complement `shrinkage` (see
#'     Details), and convergence diagnostics;
#'   * `moderators`, when the fit has them, with the reference change and rule
#'     behind each moderator's prior;
#'   * `correlations` and `acceptance`, under an unstructured covariance;
#'   * `information`: the prior's reference change, scale and settings, the
#'     informative-group count, and the spread of the groups' SDs; with several
#'     random coefficients, a list of these named by coefficient;
#'   * `convergence`: rank-normalized, folded split R-hat and bulk and tail ESS
#'     for every sampled quantity the fit reports, with a `status` column saying
#'     why a diagnostic is `NA` (see [hierarchical_selection]).
#' @details
#' **`own_data_weight`** is defined exactly as a conditional quantity: in each
#' draw, the share of the full-conditional precision of the group's deviation
#' contributed by the group's own data, with every other coefficient, the
#' hierarchical parameters and the group's nuisance state held fixed,
#' averaged over draws. It is not a marginal pooling proportion. It is reported
#' under a diagonal covariance (with or without moderators) and is `NA` under an
#' unstructured one, where the conditional prior precision is
#' \eqn{(\Sigma^{-1})_{kk}} rather than \eqn{1/\tau_k^2}. The weight is this
#' package's own construction.
#'
#' No convergence threshold is applied and no verdict is printed.
#' @references
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C. (2021).
#' Rank-normalization, folding, and localization: An improved R-hat for
#' assessing convergence of MCMC. *Bayesian Analysis*, 16(2), 667-718.
#' @seealso [hierarchical_selection] for the model, [selection_pooling()]
#' @examples
#' # Cost: an MCMC fit, over CRAN's example budget at any usable chain length.
#' \donttest{
#' set.seed(1)
#' teams <- c("a", "b", "c")
#' nodes <- do.call(rbind, lapply(teams, function(g) {
#'   data.frame(group = g, node = paste0(g, 1:6), x = stats::rnorm(6))
#' }))
#' ties <- do.call(rbind, lapply(teams, function(g) {
#'   p <- expand.grid(source = paste0(g, 1:6), target = paste0(g, 1:6),
#'                    stringsAsFactors = FALSE)
#'   p <- p[p$source != p$target, ]
#'   data.frame(group = g, p, tie = stats::rbinom(nrow(p), 1, 0.3))
#' }))
#' nd <- netis_data(nodes, ties,
#'                  tie_meta = list(tie = list(zero_meaning = "measured_zero")))
#' fit <- fit_selection(tie ~ sender(x), data = nd,
#'                      pooling = selection_pooling(random = "intercept"),
#'                      nscan = 200, burn = 50, odens = 10)
#' netis_hierarchy(fit)$population
#' }
#' @export
netis_hierarchy <- function(object, level = 0.95) {
  h <- object$hierarchical
  if (is.null(h)) {
    stop("This fit has no hierarchical layer. Fit with pooling = selection_pooling(random = <coefficient>) to estimate a between-group distribution for at least one coefficient.",
         call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) {
    stop("level must be a single number strictly between 0 and 1.", call. = FALSE)
  }
  a <- (1 - level) / 2
  qs <- function(x) stats::quantile(x, c(a, 1 - a), names = FALSE)
  # SEVERAL RANDOM COEFFICIENTS report as several sets of rows, each labeled.
  # Under a diagonal covariance they vary independently across groups and no
  # cross-coefficient quantity exists; under an unstructured one the
  # correlations are reported in their own table.
  if (length(h$random) > 1L) {
    per <- lapply(h$random, function(k) {
      netis_hierarchy(.netis_hierarchy_one(object, k), level = level)
    })
    names(per) <- h$random
    label <- function(field) {
      do.call(rbind, lapply(h$random, function(k) {
        d <- per[[k]][[field]]
        cbind(coefficient = k, d, stringsAsFactors = FALSE)
      }))
    }
    return(list(
      coefficient = h$random,
      covariance = h$covariance %||% "diagonal",
      population = label("population"),
      groups = label("groups"),
      moderators = if (is.null(per[[1]]$moderators)) NULL else label("moderators"),
      correlations = .netis_hierarchy_correlations(object, qs),
      acceptance = .netis_hierarchy_acceptance(h),
      information = stats::setNames(lapply(h$random, function(k) per[[k]]$information),
                                    h$random),
      convergence = object$convergence_table
    ))
  }
  k <- h$random
  mu <- h$draws$mu[, k]
  tau <- h$draws$tau[, k]
  theta <- h$draws$theta[, , k, drop = TRUE]
  if (is.null(dim(theta))) theta <- matrix(theta, ncol = 1L)
  has_mod <- length(h$moderators) > 0L
  # With moderators the hierarchical mean is z_g' gamma_k, and the sampler's
  # `mu` is gamma_k0: the value at z = 0. Labeled as such, because it equals a
  # population mean only when every moderator is centered.
  mu_label <- if (has_mod) "mu (value at z = 0)" else "mu"
  mu_param <- if (has_mod) sprintf("gamma:%s:(Intercept)", k) else paste0("mu:", k)
  conv <- .netis_hierarchy_conv(object, c(mu_param, paste0("tau:", k)))
  tv <- h$tie_variation %||% rep(NA_character_, length(h$groups))
  gconv <- .netis_hierarchy_conv(object, sprintf("%s[%s]", k, h$groups))

  groups <- data.frame(
    group = h$groups,
    beta_g = colMeans(theta),
    lower = apply(theta, 2, function(z) qs(z)[1]),
    upper = apply(theta, 2, function(z) qs(z)[2]),
    within_sd = h$predictor_sd,
    informative = h$informative,
    tie_variation = is.na(tv),
    own_data_weight = h$own_data_weight,
    shrinkage = 1 - h$own_data_weight,
    rhat = gconv$rhat, ess_bulk = gconv$ess_bulk, ess_tail = gconv$ess_tail,
    stringsAsFactors = FALSE, row.names = NULL
  )
  list(
    coefficient = k,
    covariance = h$covariance %||% "diagonal",
    # `estimate` is the posterior MEAN, and stays the primary summary. The
    # median is reported beside it as a secondary diagnostic -- useful because
    # tau's posterior is right-skewed at small G -- and NOT a substitute:
    # choosing whichever summary looks better would be choosing the friendliest
    # number.
    population = data.frame(
      parameter = c(mu_label, "tau"),
      estimate = c(mean(mu), mean(tau)),
      lower = c(qs(mu)[1], qs(tau)[1]),
      upper = c(qs(mu)[2], qs(tau)[2]),
      median = c(stats::median(mu), stats::median(tau)),
      rhat = conv$rhat, ess_bulk = conv$ess_bulk, ess_tail = conv$ess_tail,
      stringsAsFactors = FALSE, row.names = NULL
    ),
    groups = groups,
    # GROUP-LEVEL MODERATORS, when the fit has them. ABSENT otherwise -- a
    # fit without moderators has no gamma, and an empty table invites a
    # reader to conclude the moderators were estimated at zero.
    moderators = .netis_hierarchy_moderators(h, k, qs, object),
    information = .netis_hierarchy_information(h),
    convergence = object$convergence_table
  )
}

#' Convergence columns for named quantities, from the fit's table
#'
#' @noRd
.netis_hierarchy_conv <- function(object, params) {
  tab <- object$convergence_table
  pick <- function(col) {
    if (is.null(tab)) return(rep(NA_real_, length(params)))
    unname(tab[[col]][match(params, tab$parameter)])
  }
  list(rhat = pick("rhat"), ess_bulk = pick("ess_bulk"), ess_tail = pick("ess_tail"))
}

#' The between-group correlations of an unstructured fit, as a table
#'
#' @noRd
.netis_hierarchy_correlations <- function(object, qs) {
  Om <- object$hierarchical$Omega
  if (is.null(Om)) return(NULL)
  rn <- dimnames(Om)[[2]]
  q <- length(rn)
  rows <- list()
  for (j in seq_len(q - 1L)) for (i in (j + 1L):q) {
    v <- Om[, i, j]
    nm <- sprintf("cor:%s,%s", rn[j], rn[i])
    cv <- .netis_hierarchy_conv(object, nm)
    rows[[length(rows) + 1L]] <- data.frame(
      first = rn[j], second = rn[i], estimate = mean(v),
      lower = qs(v)[1], upper = qs(v)[2], median = stats::median(v),
      rhat = cv$rhat, ess_bulk = cv$ess_bulk, ess_tail = cv$ess_tail,
      stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Metropolis acceptance of an unstructured fit, warm-up and retained
#'
#' @noRd
.netis_hierarchy_acceptance <- function(h) {
  acc <- h$acceptance
  if (is.null(acc)) return(NULL)
  data.frame(
    block = c("log tau", "correlations"),
    warmup = c(acc$warmup$tau %||% NA_real_, acc$warmup$omega %||% NA_real_),
    retained = c(acc$tau, acc$omega),
    target = c(acc$steps$target_tau %||% NA_real_, acc$steps$target_omega %||% NA_real_),
    final_step = c(paste(signif(acc$steps$tau, 3), collapse = ", "),
                   paste(signif(acc$steps$omega, 3), collapse = ", ")),
    adapted = isTRUE(acc$steps$adapted),
    stringsAsFactors = FALSE
  )
}

#' The moderator coefficients gamma_k, as a reporting table
#'
#' One row per moderator, plus the intercept row. `estimate` is the posterior
#' MEAN, as everywhere else in this layer.
#'
#' @param h the hierarchical layer; `k` the coefficient; `qs` the interval fn.
#' @return a data frame, or NULL when the fit had no moderators.
#' @noRd
.netis_hierarchy_moderators <- function(h, k, qs, object = NULL) {
  g <- h$draws$gamma
  if (is.null(g) || !length(h$moderators)) {
    return(NULL)
  }
  # Columns are named gamma:<coefficient>:<moderator>, so THIS coefficient's
  # are selected by name. With several random coefficients the matrix holds
  # every one of them side by side, and taking a positional slice would
  # report one coefficient's moderation under another's label.
  want <- paste0("gamma:", k, ":", c("(Intercept)", h$moderators))
  miss <- setdiff(want, colnames(g))
  if (length(miss)) {
    stop(sprintf("Internal error: the fit has no gamma column(s) %s.",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  gk <- g[, want, drop = FALSE]
  sc <- h$moderator_scale
  conv <- if (is.null(object)) {
    list(rhat = NA_real_, ess_bulk = NA_real_, ess_tail = NA_real_)
  } else {
    .netis_hierarchy_conv(object, want)
  }
  data.frame(
    # For the intercept these are team-level main effects of the moderators;
    # for a slope, cross-level interactions. "(Intercept)" is the hierarchical
    # mean at every moderator equal to zero.
    moderator = c("(Intercept)", h$moderators),
    estimate = colMeans(gk),
    lower = apply(gk, 2, function(z) qs(z)[1]),
    upper = apply(gk, 2, function(z) qs(z)[2]),
    median = apply(gk, 2, stats::median),
    rhat = conv$rhat, ess_bulk = conv$ess_bulk, ess_tail = conv$ess_tail,
    # The reference change this coefficient's prior was expressed per, and
    # whether that scale rests on the G group values. The intercept is not a
    # moderator slope and has neither.
    reference_change = c(NA_real_, unname(sc$delta_Z[h$moderators])),
    reference_rule = c(NA_character_, unname(sc$rule[h$moderators])),
    scale_from_group_values = c(NA, unname(sc$weak[h$moderators])),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# ---------------------------------------------------------------------------
# The route
# ---------------------------------------------------------------------------

#' Fit every group together in one hierarchical model
#'
#' @param spec,data,schema,route,family,binarize,R as in [fit_selection()].
#' @param groups the group ids.
#' @param pooling a `netis_selection_pooling`.
#' @param dots the caller's estimation controls.
#' @return the pieces `.assemble_netis_selection()` needs, plus the
#'   hierarchical layer.
#' @noRd
.fit_selection_hierarchical <- function(spec, data, schema, groups, route, family,
                                        binarize, R, pooling, dots) {
  asm <- lapply(groups, function(g) {
    .assemble_selection_group(spec, data, schema, g, route, binarize)
  })
  names(asm) <- groups
  designs <- lapply(asm, function(x) x$stacked)
  actor_ids <- lapply(asm, function(x) x$ids)

  coef_names <- .amen_coef_names(designs[[1]])
  for (g in groups) {
    nm <- .amen_coef_names(designs[[g]])
    if (!identical(nm, coef_names)) {
      stop(sprintf(
        "Group '%s' has a different design from group '%s' (%s against %s). One model over all groups needs the same coefficients in every group.",
        g, groups[[1]], paste(nm, collapse = ", "), paste(coef_names, collapse = ", ")
      ), call. = FALSE)
    }
  }
  regime <- .selection_regime_vector(coef_names, pooling)
  info <- .selection_predictor_information(designs, coef_names, pooling$random)
  .assert_hierarchical_information(info, pooling$random, groups)
  .assert_unpooled_information(designs, coef_names, setdiff(pooling$unpooled, "intercept"),
                               groups)

  hyper <- list(nu = pooling$nu, A0 = pooling$A0, eta = pooling$eta %||% 1)
  if (length(pooling$random)) {
    # ONE REFERENCE CHANGE PER RANDOM COEFFICIENT, in the core's own order.
    # `pooling$random` is the order the caller wrote; the sampler indexes its
    # random columns by DESIGN-COLUMN order, and handing it a vector in the
    # caller's order would attach one coefficient's scale to another's prior --
    # correct-looking and wrong.
    random_in_design_order <- coef_names[sort(match(pooling$random, coef_names))]
    per <- .netis_information_list(info, pooling$random)
    declared <- pooling$delta
    hyper$predictor_sd <- vapply(random_in_design_order, function(k) {
      if (!is.null(declared)) {
        if (length(declared) == 1L) declared else declared[[match(k, pooling$random)]]
      } else {
        per[[k]]$delta
      }
    }, numeric(1))
  }
  # GROUP-LEVEL MODERATORS. The moderator table is reindexed to the
  # fit's own group order BY NAME. Its rows were written by hand, away from
  # this fit, so nothing makes their order match; taking them positionally
  # would attach one group's moderators to another and the fit would look
  # entirely normal.
  mod_scale <- NULL
  Zmat <- NULL
  if (!is.null(pooling$moderators)) {
    md <- pooling$moderators
    missing_g <- setdiff(groups, md$.group)
    extra_g <- setdiff(md$.group, groups)
    if (length(missing_g) || length(extra_g)) {
      stop(sprintf("The moderator table does not describe this fit's groups. Missing: %s. Unknown: %s.",
                   if (length(missing_g)) paste(missing_g, collapse = ", ") else "none",
                   if (length(extra_g)) paste(extra_g, collapse = ", ") else "none"),
           call. = FALSE)
    }
    md <- md[match(groups, md$.group), , drop = FALSE]
    Zmat <- as.matrix(md[, setdiff(names(md), ".group"), drop = FALSE])
    rownames(Zmat) <- groups
    mod_scale <- .selection_moderator_scale(Zmat, pooling$delta_Z)
    hyper$delta_Z <- mod_scale$delta_Z
    hyper$A0_Z <- pooling$A0_Z
  }
  ctrl <- .mgc_controls(dots)
  draws <- .mgc_sample(
    groups = lapply(designs, function(d) list(Y = d$Y, Xrow = d$Xrow, Xcol = d$Xcol,
                                              Xdyad = d$Xdyad)),
    regime = regime, family = family, R = R,
    nscan = ctrl$nscan, burn = ctrl$burn, odens = ctrl$odens, seed = ctrl$seed,
    chains = ctrl$chains, hyper = hyper, coef_names = coef_names,
    covariance = pooling$covariance %||% "diagonal",
    moderators = if (is.null(Zmat)) NULL else cbind("(Intercept)" = 1, Zmat)
  )
  list(draws = draws, designs = designs, actor_ids = actor_ids,
       findings = unlist(lapply(asm, function(x) x$findings), recursive = FALSE),
       n_excluded = vapply(asm, function(x) x$n_excluded, numeric(1)),
       coef_names = coef_names, regime = regime, information = info,
       moderator_scale = mod_scale, moderators = if (is.null(Zmat)) NULL else colnames(Zmat))
}

#' The per-coefficient regime vector a pooling specification declares
#'
#' @noRd
.selection_regime_vector <- function(coef_names, pooling) {
  unknown <- setdiff(c(pooling$random, pooling$unpooled), coef_names)
  if (length(unknown)) {
    stop(sprintf(
      "pooling names coefficient(s) this model does not have: %s. The model's coefficients are: %s.",
      paste(unknown, collapse = ", "), paste(coef_names, collapse = ", ")
    ), call. = FALSE)
  }
  regime <- stats::setNames(rep("shared", length(coef_names)), coef_names)
  regime[pooling$unpooled] <- "group"
  if (length(pooling$random)) regime[pooling$random] <- "random"
  regime
}

#' How much within-group information each group carries about one predictor
#'
#' The reference change `delta` for the heterogeneity prior, and the flag that
#' says which groups can inform their own coefficient directly. A group with no
#' within-group variation is FLAGGED, never dropped: partial pooling still
#' estimates its coefficient, which is what a hierarchical model is for.
#'
#' @noRd
.selection_predictor_information <- function(designs, coef_names, random) {
  # ONE RECORD PER RANDOM COEFFICIENT. Each has its own reference change, its
  # own informative groups and its own prior scale: a predictor that varies
  # widely in every group and one that barely varies anywhere do not share a
  # delta just because they are random in the same fit.
  if (length(random) > 1L) {
    per <- lapply(random, function(k) {
      .selection_predictor_information(designs, coef_names, k)
    })
    names(per) <- random
    return(structure(per, class = "netis_predictor_information_set"))
  }
  if (is.null(random) || !length(random)) {
    return(list(predictor_sd = NULL, informative = NULL, delta = NULL,
                delta_rule = NULL, binary = FALSE))
  }
  if (identical(random, "intercept")) {
    # The intercept's "predictor" is the constant 1, so within-group variation
    # is the wrong question: a group informs its own baseline by having dyads
    # at all. Its reference change is one latent unit.
    # Counted on the off-diagonal directly, whatever the diagonal holds: the
    # assembled Y already carries an NA diagonal, so subtracting n would remove
    # it a second time and flag a group with n or fewer observed dyads as
    # uninformative.
    n_dyads <- vapply(designs, function(d) {
      obs <- !is.na(d$Y)
      diag(obs) <- FALSE
      sum(obs)
    }, numeric(1))
    return(list(predictor_sd = rep(1, length(designs)),
                informative = n_dyads > 0,
                delta = 1,
                delta_rule = "1 (the intercept's predictor is the constant 1)",
                binary = FALSE, pooled_sd = 1,
                support_n = unname(n_dyads), role = "intercept"))
  }
  k <- match(random, coef_names)
  role <- .selection_random_role(designs[[1]], k)
  # Built through .mgc_design(), not amen::design_array() directly: amen DROPS
  # the intercept slice whenever a covariate is constant, which is exactly the
  # weak-information case here, and the slice index would then point at the
  # wrong covariate for precisely the group being assessed. .mgc_design()
  # restores the intercept, and is what the sampler itself uses.
  #
  # THE SUPPORT DEPENDS ON THE COLUMN'S ROLE: actors
  # for a nodal column, observed off-diagonal dyads for a dyadic one. Reading a
  # nodal covariate off the dyad-level slice counts each actor's value once per
  # observed dyad in its row, which is the population SD at complete data
  # (a factor sqrt((n-1)/n)) and an OUT-DEGREE-WEIGHTED quantity under
  # missingness -- not the actor-level SD the reference change is defined on.
  values <- function(d) {
    n <- nrow(d$Y)
    obs <- !is.na(d$Y)
    diag(obs) <- FALSE
    if (identical(role, "dyadic")) {
      x <- .mgc_design(d$Xrow, d$Xcol, d$Xdyad, n)[, , k]
      return(x[obs])
    }
    # Nodal: one value per actor, over actors the likelihood sees at all.
    block <- if (identical(role, "sender")) d$Xrow else d$Xcol
    j <- k - 1L - if (identical(role, "receiver")) ncol(d$Xrow %||% matrix(0, n, 0)) else 0L
    in_support <- rowSums(obs) > 0 | colSums(obs) > 0
    block[in_support, j]
  }
  sds <- vapply(designs, function(d) stats::sd(values(d)), numeric(1))
  counts <- vapply(designs, function(d) length(values(d)), numeric(1))
  binary <- all(vapply(designs, function(d) {
    v <- unique(values(d))
    length(v) <= 2L && all(v %in% c(0, 1))
  }, logical(1)))

  # A RELATIVE threshold: a group carries usable within-group information
  # when its SD is not negligible against the typical group's. The constant is
  # a package convention and is reported rather than buried: information
  # about a slope scales with SD^2, so a group at 1% of the median carries
  # about 1e-4 of the typical group's, which is not usable. An absolute test
  # mixing scales (e.g. `sds > 1e-8 * max(ref, 1)`) would be an exact-zero
  # test in disguise, counting groups at SD 1.0 and 0.001 alike as
  # informative.
  tol <- 0.01
  positive <- is.finite(sds) & sds > 0
  # With no group varying at all there is no reference SD, and every group is
  # uninformative; the caller refuses that case by name.
  ref0 <- if (any(positive)) stats::median(sds[positive]) else NA_real_
  informative <- positive & sds >= tol * ref0
  # The reference change is the median over the INFORMATIVE groups, not over
  # every group with a nonzero SD.
  ref <- stats::median(sds[informative], na.rm = TRUE)
  delta_rule <- if (binary) {
    "1 (the 0 to 1 contrast of a binary predictor)"
  } else {
    sprintf("median within-group SD over the %d informative group(s)", sum(informative))
  }
  delta <- if (binary) 1 else ref
  # The POOLED within-group SD, weighted by support size -- groups contribute
  # in proportion to the observations they carry, which for a dyadic
  # covariate is n_g^2 rather than n_g. An unweighted root-mean-square would
  # be a different quantity under unequal group sizes.
  ss <- vapply(designs, function(d) {
    v <- values(d)
    sum((v - mean(v))^2)
  }, numeric(1))
  df <- pmax(counts - 1, 0)
  pooled <- if (sum(df[informative]) > 0) {
    sqrt(sum(ss[informative]) / sum(df[informative]))
  } else {
    NA_real_
  }
  list(predictor_sd = unname(sds), informative = unname(informative),
       delta = delta, delta_rule = delta_rule, binary = binary,
       pooled_sd = pooled, support_n = unname(counts), role = role,
       informative_tol = tol)
}

#' Which design block a coefficient belongs to
#'
#' Read from the fitted design's block widths, in `amen::design_array()`'s
#' layout: intercept, row block, column block, dyadic block.
#'
#' @noRd
.selection_random_role <- function(design, k) {
  p_row <- if (is.null(design$Xrow)) 0L else ncol(design$Xrow)
  p_col <- if (is.null(design$Xcol)) 0L else ncol(design$Xcol)
  if (k == 1L) return("intercept")
  if (k <= 1L + p_row) return("sender")
  if (k <= 1L + p_row + p_col) return("receiver")
  "dyadic"
}

#' Refuse only what is unidentified over the whole dataset
#'
#' @noRd
.assert_hierarchical_information <- function(info, random, groups) {
  if (is.null(random) || !length(random)) {
    return(invisible(TRUE))
  }
  # EACH RANDOM COEFFICIENT IS JUDGED ON ITS OWN. One predictor being
  # well-identified across groups says nothing about another's, so a set is
  # checked coefficient by coefficient and the refusal names which one failed.
  if (length(random) > 1L) {
    per <- .netis_information_list(info, random)
    for (k in random) {
      .assert_hierarchical_information(per[[k]], k, groups)
    }
    return(invisible(TRUE))
  }
  n_inf <- sum(info$informative)
  if (n_inf == 0L) {
    stop(sprintf(
      "'%s' has no within-group variation in any of the %d group(s), so a coefficient that varies across groups is not identified anywhere in this dataset. Fit it as a shared coefficient; a predictor that is constant within groups enters as a moderator of a random intercept instead.",
      random, length(groups)
    ), call. = FALSE)
  }
  if (n_inf < 2L) {
    stop(sprintf(
      "Only 1 of %d group(s) ('%s') has within-group variation in '%s'. A between-group SD cannot be estimated from one informative group -- tau would be whatever the prior says. Fit '%s' as shared, or as unpooled if that one group is the question.",
      length(groups), groups[info$informative][1], random, random
    ), call. = FALSE)
  }
  # Two informative groups identify tau only weakly. How many are really
  # needed depends on the design, not on a constant to settle here, so the
  # fit proceeds and the count is reported by netis_hierarchy().
  invisible(TRUE)
}

#' Refuse a group-specific coefficient that a group's data cannot inform
#'
#' An unpooled coefficient is estimated in each group under that group's own
#' amen g-prior, which is built from the group's design. A predictor with no
#' within-group variation gives that prior no curvature in that direction, so
#' the coefficient has neither data nor a proper prior in that group.
#'
#' @noRd
.assert_unpooled_information <- function(designs, coef_names, unpooled, groups) {
  for (k in unpooled) {
    info <- .selection_predictor_information(designs, coef_names, k)
    flat <- groups[!info$informative]
    if (length(flat)) {
      stop(sprintf(
        "'%s' is group-specific (unpooled), but it has no within-group variation in group(s) %s. A group-specific coefficient is estimated from that group's own data under a prior built from that group's design, so in such a group it has neither. Make '%s' random, so those groups borrow from the others, or shared, or remove the group(s).",
        k, paste(sprintf("'%s'", flat), collapse = ", "), k
      ), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' The MCMC controls the hierarchical engine takes from `...`
#'
#' @noRd
.mgc_controls <- function(dots) {
  pick <- function(nm, default) if (!is.null(dots[[nm]])) dots[[nm]] else default
  unknown <- setdiff(names(dots), c("nscan", "burn", "odens", "seed", "chains"))
  if (length(unknown)) {
    stop(sprintf(
      "The hierarchical multi-group sampler takes nscan, burn, odens, seed and chains; it was given %s. Those are amen::ame() options, and this route does not call amen::ame() -- it runs one chain over all groups using amen's own conditionals.",
      paste(unknown, collapse = ", ")
    ), call. = FALSE)
  }
  .assert_selection_controls(dots, c("nscan", "burn", "odens", "seed", "chains"))
  list(nscan = pick("nscan", 10000), burn = pick("burn", 500),
       odens = pick("odens", 25), seed = pick("seed", 1),
       chains = pick("chains", 1L))
}

#' The within-group information report
#'
#' Every random slope carries a report of how meaningful
#' one reference change is across groups: the per-group SDs, their spread, and
#' how many groups carry usable information. A single `delta` applied to groups
#' whose SDs differ by an order of magnitude is not equally meaningful in each,
#' and `delta` sets `A`, which sets the prior -- so this is the diagnostic that
#' says how much the prior scale can be trusted.
#'
#' @noRd
.netis_hierarchy_information <- function(h) {
  sds <- h$predictor_sd
  inf <- h$informative
  usable <- sds[inf & is.finite(sds) & sds > 0]
  ratio <- if (length(usable) > 1L) max(usable) / min(usable) else 1
  iqr <- if (length(usable) > 1L) {
    qq <- stats::quantile(usable, c(0.25, 0.75), names = FALSE)
    if (qq[1] > 0) qq[2] / qq[1] else NA_real_
  } else {
    1
  }
  list(
    reference_change = h$delta,
    reference_rule = h$delta_rule,
    # A for THIS coefficient, A0 x outcome scale / delta. The fit-level `A` is a
    # vector over every random coefficient, so it is recomputed here from this
    # coefficient's own reference change rather than indexed.
    tau_prior = sprintf("half-t(nu = %g, A = %g); tau * delta ~ half-t(nu = %g, A0 = %g x outcome scale %g)",
                        h$nu, h$A0 * (h$outcome_scale %||% 1) / h$delta, h$nu, h$A0,
                        h$outcome_scale %||% 1),
    correlation_prior = if (identical(h$covariance, "unstructured")) {
      sprintf("LKJ(eta = %g)", h$eta %||% 1)
    } else {
      "none (diagonal covariance)"
    },
    own_data_weight = h$own_data_weight_note,
    groups_without_tie_variation = sum(!is.na(h$tie_variation %||% NA_character_)),
    n_informative = sum(inf),
    n_groups = length(h$groups),
    # The spread report. A large ratio means one
    # reference change means different things in different groups.
    sd_max_min_ratio = ratio,
    sd_iqr_ratio = iqr,
    # (b) the median over informative groups, against (c) the support-weighted
    # pooled SD. They differ when groups differ in size; (c) is the
    # sensitivity alternative rather than the default.
    pooled_sd = h$pooled_sd,
    median_to_pooled_ratio = if (is.finite(h$pooled_sd) && h$pooled_sd > 0) {
      h$delta / h$pooled_sd
    } else {
      NA_real_
    },
    # A group informs its own intercept by having observed dyads; the SD rule
    # applies to a slope.
    informative_threshold = if (is.null(h$informative_tol)) {
      "at least one observed dyad"
    } else {
      sprintf(
        "SD >= %g x the median of the groups' nonzero SDs (a package convention, not an established minimum-information rule)",
        h$informative_tol
      )
    },
    support_n = h$support_n
  )
}

#' One information record per random coefficient, however many there are
#'
#' `.selection_predictor_information()` returns a single record for one
#' coefficient and a named set for several. This normalizes both to a named
#' list, so callers never branch on how many there are.
#'
#' @param info what that function returned.
#' @param random the random coefficient names.
#' @noRd
.netis_information_list <- function(info, random) {
  if (inherits(info, "netis_predictor_information_set")) {
    return(info[random])
  }
  stats::setNames(list(info), random)
}

#' A view of a multi-coefficient fit restricted to one random coefficient
#'
#' The reporting path for several random coefficients is the single-coefficient
#' path run once per coefficient, so there is one implementation of what a
#' between-group layer looks like rather than two that must agree.
#'
#' @noRd
.netis_hierarchy_one <- function(object, k) {
  h <- object$hierarchical
  one <- h
  one$random <- k
  # Per-coefficient fields are stored keyed by coefficient when there are
  # several; a single-coefficient fit stores them bare.
  for (field in c("predictor_sd", "informative", "delta", "delta_rule",
                  "pooled_sd", "support_n", "role", "informative_tol",
                  "own_data_weight")) {
    v <- h[[field]]
    if (is.list(v) && !is.null(names(v)) && k %in% names(v)) one[[field]] <- v[[k]]
  }
  # The moderator columns and their scales are properties of the FIT, not of
  # one coefficient, so they pass through unchanged; the gamma columns are
  # then selected by coefficient name downstream.
  one$moderators <- h$moderators
  one$covariance <- h$covariance
  one$moderator_scale <- h$moderator_scale
  object$hierarchical <- one
  object
}
# ---------------------------------------------------------------------------
# Group-level moderators
# ---------------------------------------------------------------------------

#' Normalize a moderator specification to a frame keyed by group
#'
#' Accepts a data frame with a group column, or a matrix / data frame whose ROW
#' NAMES are the group ids. Returns a data frame whose first column is `.group`.
#'
#' ROW ORDER IS NEVER THE IDENTITY. A moderator table is written by hand, away
#' from the fit, and nothing makes its row order match the fit's group order.
#' Matching by position would attach one group's moderators to another and
#' produce a fit that looks entirely normal, so the group is named or the table
#' is refused.
#'
#' @noRd
.selection_moderator_frame <- function(x) {
  if (is.matrix(x)) {
    if (is.null(rownames(x))) {
      stop("A moderator matrix must have row names giving the group ids. Without them its rows can only be matched to groups by position, and a mis-ordered table would attach one group's moderators to another with nothing in the fit to show for it.",
           call. = FALSE)
    }
    x <- data.frame(.group = rownames(x), as.data.frame(x, stringsAsFactors = FALSE),
                    stringsAsFactors = FALSE, check.names = FALSE)
  } else if (is.data.frame(x)) {
    gcol <- intersect(c(".group", "group"), names(x))
    if (!length(gcol)) {
      if (is.integer(attr(x, "row.names"))) {
        stop("A moderator data frame must name its groups, either in a `group` column or in its row names.",
             call. = FALSE)
      }
      x <- data.frame(.group = rownames(x), x, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      names(x)[names(x) == gcol[1]] <- ".group"
      x <- x[, c(".group", setdiff(names(x), ".group")), drop = FALSE]
    }
  } else {
    stop("moderators must be a data frame or a matrix with one row per group.", call. = FALSE)
  }
  x$.group <- as.character(x$.group)
  if (anyDuplicated(x$.group)) {
    stop(sprintf("moderators names the same group more than once: %s.",
                 paste(unique(x$.group[duplicated(x$.group)]), collapse = ", ")),
         call. = FALSE)
  }
  mods <- setdiff(names(x), ".group")
  if (!length(mods)) {
    stop("moderators has no moderator columns beside the group id.", call. = FALSE)
  }
  for (m in mods) {
    if (!is.numeric(x[[m]])) {
      stop(sprintf("Moderator '%s' is not numeric. Encode a categorical moderator as indicator columns yourself, so that the reference category is your choice and is visible in the output rather than chosen silently here.", m),
           call. = FALSE)
    }
    if (anyNA(x[[m]])) {
      stop(sprintf("Moderator '%s' is missing for at least one group. A group with an unknown moderator has no defined population mean, and rnetis will not impute one.", m),
           call. = FALSE)
    }
  }
  x
}

#' The reference change each moderator's prior is expressed per
#'
#' `delta_Z` is a BETWEEN-group
#' quantity and is never derived from `delta`, which is within-group: a
#' moderator is group-constant by definition, so it HAS no within-group
#' variation to supply one.
#'
#' @param Z the moderator matrix (groups x M), without the intercept column.
#' @param declared optional named reference changes, overriding the rules.
#' @return a list: `delta_Z` (named), `rule` (named), `weak` (named logical,
#'   TRUE where the scale rests on the G group values rather than on a contrast
#'   fixed by construction), and `n_groups`.
#' @noRd
.selection_moderator_scale <- function(Z, declared = NULL) {
  mods <- colnames(Z)
  G <- nrow(Z)
  is_binary <- function(m) all(Z[, m] %in% c(0, 1))
  was_declared <- function(m) !is.null(declared) && m %in% names(declared)

  # Checked first, for every rule: a constant column is collinear with the
  # hierarchical intercept however its prior is scaled, and a constant 0/1
  # column would otherwise pass as "binary".
  constant <- vapply(mods, function(m) length(unique(Z[, m])) < 2L, logical(1))
  if (any(constant)) {
    stop(sprintf(
      "Moderator(s) %s do not vary across groups, so they carry no contrast: a group-constant column shifts every group's coefficient by the same amount, which that coefficient's own intercept already absorbs. Drop them.",
      paste(mods[constant], collapse = ", ")), call. = FALSE)
  }

  out <- vapply(mods, function(m) {
    if (was_declared(m)) return(as.numeric(declared[[m]]))
    # Exactly the binary rule used for predictors: the 0 -> 1 contrast, ALWAYS
    # 1, with no inflation for a rare moderator. A moderator true in one group
    # of six has an SD near 0.4, and dividing by it would widen the prior by
    # more than double for no reason except rarity.
    if (is_binary(m)) return(1)
    stats::sd(Z[, m])
  }, numeric(1))

  rule <- vapply(mods, function(m) {
    if (was_declared(m)) "declared by the caller"
    else if (is_binary(m)) "1 (the 0 to 1 contrast of a binary moderator)"
    else sprintf("SD across the %d group values", G)
  }, character(1))

  # FLAGGED, NOT HIDDEN. A continuous moderator's scale is estimated from G
  # numbers -- often only a handful -- so it is weakest exactly where pooling
  # matters most. The flag travels to the fit so a reader sees
  # which scales rest on the group values and which are fixed by construction.
  weak <- vapply(mods, function(m) !was_declared(m) && !is_binary(m), logical(1))

  names(out) <- mods; names(rule) <- mods; names(weak) <- mods
  list(delta_Z = out, rule = rule, weak = weak, n_groups = G)
}

