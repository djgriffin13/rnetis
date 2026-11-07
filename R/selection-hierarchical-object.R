# The fitted object for a joint multi-group Selection fit.
#
# ONE SHAPE FOR BOTH ROUTES. A reader of a fitted object should not have to ask
# which sampler produced it, so the joint route fills the same per-group fields
# the two-stage route does -- with each group's own coefficient draws, where
# theta[, g, ] is the full coefficient vector that group's likelihood used,
# shared columns included. The hierarchical layer travels in its own
# `hierarchical` field rather than being mixed into the coefficient table,
# because a between-group SD is not a coefficient and printing it as one is how
# a pooled table gets misread.
#
# THE ROUTE IS RECORDED HERE. One chain produced every group, so the object
# records a single multi-group model (`engine` in its estimation controls).
# That is a fact about how the fit was made, recorded at the point it is known
# rather than inferred later from the draw indices happening to agree, which
# they would also do by coincidence.

#' Assemble a joint multi-group Selection fit
#'
#' @noRd
.fit_selection_joint <- function(spec, call, data, schema, groups, route, family,
                                 binarize, R, pooling, symmetric,
                                 diagnostics, dots) {
  if (!inherits(pooling, "netis_selection_pooling")) {
    stop("pooling must be a selection_pooling() object, or NULL for the per-group route.",
         call. = FALSE)
  }
  if (isTRUE(symmetric)) {
    stop("A symmetric relation is not supported by the hierarchical multi-group sampler in this version. Fit the groups separately, or fit the relation as directed.",
         call. = FALSE)
  }
  .assert_amen_available()
  if (length(groups) < 2L) {
    stop(sprintf(
      "The hierarchical multi-group sampler needs at least two groups; this data has %d. With one group there is nothing to pool, and a between-group SD has nothing to be estimated from.",
      length(groups)
    ), call. = FALSE)
  }

  fitted <- .fit_selection_hierarchical(
    spec = spec, data = data, schema = schema, groups = groups, route = route,
    family = family, binarize = binarize, R = R, pooling = pooling,
    dots = dots
  )
  draws <- fitted$draws
  coef_names <- fitted$coef_names
  diagnostics <- .combine_diagnostics(c(unclass(diagnostics), fitted$findings))
  S <- length(draws$chain)
  starts <- .mgc_starts_label(draws)
  convergence_table <- .mgc_convergence_table(draws, starts)

  per_group <- lapply(seq_along(groups), function(gi) {
    g <- groups[[gi]]
    theta_g <- matrix(draws$theta[, gi, ], nrow = S,
                      dimnames = list(NULL, coef_names))
    list(
      coefficients = colMeans(theta_g),
      vcov = stats::cov(theta_g),
      draws = theta_g,
      additive_effects = list(sender = draws$posterior_means[[g]]$a,
                              receiver = draws$posterior_means[[g]]$b),
      multiplicative = draws$posterior_means[[g]]$uv,
      # Named as on the per-group route, so every per-group consumer (the
      # population estimand above all) reads the same field.
      variance_components = cbind(va = draws$ame$va[, gi], cab = draws$ame$cab[, gi],
                             vb = draws$ame$vb[, gi], rho = draws$ame$rho[, gi],
                             ve = draws$ame$s2[, gi]),
      convergence = .mgc_convergence(theta_g, draws$chain, starts),
      symmetric = FALSE
    )
  })
  names(per_group) <- groups

  random <- pooling$random
  hierarchical <- if (!length(random)) NULL else {
    multi <- length(random) > 1L
    # PER-COEFFICIENT FIELDS are keyed by coefficient when there are several
    # and stored bare for one, so the single-coefficient shape is unchanged and
    # the reporting path can restrict a multi-coefficient fit to one at a time.
    info_list <- .netis_information_list(fitted$information, random)
    by_coef <- function(fun) {
      if (multi) stats::setNames(lapply(random, fun), random) else fun(random)
    }
    # OWN-DATA WEIGHT, kept only where its exact meaning holds. In one draw the
    # full conditional of the deviation zeta_kg -- given every other
    # coefficient, the hierarchical parameters and the group's nuisance
    # state -- is normal with precision Q_g[k, k] + (prior precision). Under a
    # DIAGONAL covariance that prior precision is 1 / tau_k^2, with or without
    # moderators (they move the conditional mean, not its precision), and
    # tau^2 Q / (1 + tau^2 Q) is the data's share of it, averaged over draws.
    # It is a conditional share with everything else held fixed, NOT a
    # marginal pooling proportion: predictors correlated within a group couple
    # their coefficients through the likelihood, and conditioning removes that.
    # Under an UNSTRUCTURED covariance the conditional prior precision is
    # (Sigma^-1)[k, k], so the scalar formula does not describe the update and
    # NA is returned, with the reason on the fit.
    weight_for <- function(k) {
      if (identical(draws$covariance, "unstructured")) {
        return(rep(NA_real_, length(groups)))
      }
      Q <- matrix(draws$random_data_precision[, , k], nrow = S)
      tau2 <- draws$tau[, k]^2
      colMeans(tau2 * Q / (1 + tau2 * Q))
    }
    list(
      random = random,
      groups = groups,
      # gamma travels with mu, tau and theta rather than beside them: it is a
      # posterior draw of the same state, and a consumer iterating the draws
      # list would otherwise silently miss it. ABSENT, not zero-filled, when
      # the fit had no moderators -- a fit without them has no gamma to
      # report, and an all-zero column reads like an estimate of no effect.
      draws = c(list(mu = draws$mu, tau = draws$tau, theta = draws$theta),
                if (is.null(draws$gamma)) list() else list(gamma = draws$gamma)),
      # The moderator columns, and the reference change each one's prior is
      # expressed per -- with the flag saying which of those scales rest on
      # the G group values rather than on a contrast fixed by construction.
      moderators = fitted$moderators,
      moderator_scale = fitted$moderator_scale,
      # The between-group covariance this fit ESTIMATED, read off the fit
      # rather than asserted. Under "diagonal" each coefficient's tau comes
      # from its own conditional and no cross-coefficient term exists; under
      # "unstructured" the correlations are estimated and travel in `Omega`.
      # Hardcoding the word here would have made a correlated fit describe
      # itself as diagonal.
      covariance = draws$covariance %||% "diagonal",
      Omega = draws$Omega,
      acceptance = draws$acceptance,
      # The LKJ shape the correlations were fitted under; recorded for every
      # fit so a reader never has to infer the default.
      eta = pooling$eta %||% 1,
      own_data_weight_note = if (identical(draws$covariance, "unstructured")) {
        "not reported under an unstructured covariance: the conditional prior precision of a group deviation is (Sigma^-1)[k, k], not 1 / tau^2, so the scalar share does not describe the update"
      } else {
        "the data's share of the full-conditional precision of the group's deviation, with every other quantity held fixed, averaged over draws; a conditional share, not a marginal pooling proportion"
      },
      tie_variation = draws$tie_variation,
      # SHRINKAGE, from what the sampler actually conditioned on rather than
      # from a second fit. In the conditional draw of beta_kg the group's own
      # data enters with precision Q_g and the population distribution with
      # precision 1/tau^2, so the share of beta_kg driven by this group's own
      # data is tau^2 Q_g / (1 + tau^2 Q_g), averaged over draws. A group
      # carrying no within-group variation has Q_g at zero: weight 0, and its
      # coefficient is the population distribution.
      own_data_weight = by_coef(weight_for),
      predictor_sd = by_coef(function(k) info_list[[k]]$predictor_sd),
      informative = by_coef(function(k) info_list[[k]]$informative),
      delta = by_coef(function(k) info_list[[k]]$delta),
      delta_rule = by_coef(function(k) info_list[[k]]$delta_rule),
      pooled_sd = by_coef(function(k) info_list[[k]]$pooled_sd),
      support_n = by_coef(function(k) info_list[[k]]$support_n),
      informative_tol = by_coef(function(k) info_list[[k]]$informative_tol),
      role = by_coef(function(k) info_list[[k]]$role),
      nu = pooling$nu, A0 = pooling$A0,
      outcome_scale = draws$priors$random$outcome_scale,
      regime = fitted$regime
    )
  }

  # The population-level coefficient vector: the shared value for a shared
  # column, mu for the random one. A group column has no population value and
  # is NA rather than an average of things that were never one quantity.
  coefs <- colMeans(draws$mu)
  names(coefs) <- coef_names

  .new_netis_fit(
    "netis_selection",
    fields = list(
      spec = spec, call = call,
      netis_version = as.character(utils::packageVersion("rnetis")),
      backend = "rnetis-mgc",
      backend_version = as.character(utils::packageVersion("rnetis")),
      estimation_controls = c(.mgc_controls(dots),
                              list(family = family, R = R,
                                   engine = "hierarchical_multigroup")),
      coefficients = coefs,
      vcov = .mgc_population_vcov(draws$mu, fitted$regime),
      convergence = lapply(per_group, function(e) e$convergence),
      loglik = NA, diagnostics = diagnostics
    ),
    extra = list(
      pooled_beta = NULL, waves = route, route = route, family = family,
      binarize = binarize, symmetric = FALSE, R = R, groups = groups,
      n_groups = length(groups),
      # Population-level draws only: an unpooled coefficient has no population
      # value, and an all-NA column would be diagnosed as missing data. The
      # chain vector travels with them so diagnostics keep chain identity.
      draws = .new_netis_draws(
        parameters = draws$mu[, fitted$regime != "group", drop = FALSE],
        summarized = list(), n_chains = draws$control$chains,
        note = "Population-level coefficient draws from the hierarchical multi-group sampler (all groups in one chain).",
        chain = draws$chain
      ),
      convergence_table = convergence_table,
      tie_variation = draws$tie_variation,
      per_group = per_group,
      designs = fitted$designs,
      actor_ids = fitted$actor_ids,
      # Dyads excluded for an unresolvable predictor or lag, counted exactly
      # as on the per-group route (a missing outcome is not an exclusion).
      n_excluded = fitted$n_excluded,
      n_actors = vapply(fitted$actor_ids, length, numeric(1)),
      hierarchical = hierarchical,
      pooling = pooling,
      chain = draws$chain,
      mgc_draws = draws
    )
  )
}

#' The population-level coefficient covariance of a hierarchical fit
#'
#' Over the same names as `coef()`. A group-specific coefficient has no
#' population value, so its row and column are `NA` like its `coef()` entry.
#'
#' @noRd
.mgc_population_vcov <- function(mu, regime) {
  nms <- colnames(mu)
  out <- matrix(NA_real_, length(nms), length(nms), dimnames = list(nms, nms))
  pop <- regime != "group"
  out[pop, pop] <- stats::cov(mu[, pop, drop = FALSE])
  out
}

#' Convergence diagnostics for one group's coefficient draws
#'
#' Chain identity is passed through: the stacked draws hold every chain, and
#' diagnosing the stack as one chain would never compare the chains.
#'
#' @noRd
.mgc_convergence <- function(theta_g, chain, starts = NULL) {
  .netis_mcmc_diagnostics(theta_g, chain = chain, starts = starts)
}

#' How the chains of a hierarchical fit were started, for the diagnostic note
#'
#' @noRd
.mgc_starts_label <- function(draws) {
  st <- draws$starts
  if (length(unique(draws$chain)) < 2L) return(NULL)
  if (any(st %in% "dispersed")) "dispersed" else "common"
}

#' Convergence diagnostics for every sampled quantity a hierarchical fit reports
#'
#' One row per quantity, with its family:
#'
#' * `shared`: a coefficient shared by every group;
#' * `population_mean` (`mu:<k>`), or `moderator` (`gamma:<k>:<term>`) with
#'   moderators;
#' * `between_group_sd` (`tau:<k>`);
#' * `correlation` (`cor:<k1>,<k2>`), under an unstructured covariance;
#' * `group_coefficient` (`<k>[<group>]`), for every unpooled or random
#'   coefficient in every group;
#' * `nuisance`: each group's identifiable AME quantities -- sender and receiver
#'   variances and covariance (`va`, `vb`, `cab`), the dyadic correlation
#'   (`rho`), the residual variance under `"nrm"` (`s2`), and with latent
#'   factors the variance of `u'v` over observed dyads (`uv_realized`). Factor
#'   coordinates are not identified and are not diagnosed.
#'
#' @return the `.netis_mcmc_diagnostics()` table with `family` and `group`
#'   columns added, plus attributes `scope` and `note`.
#' @noRd
.mgc_convergence_table <- function(draws, starts = NULL) {
  cols <- list()
  info <- list()
  add <- function(name, v, family, group = NA_character_) {
    cols[[name]] <<- v
    info[[name]] <<- c(family = family, group = group)
  }
  reg <- draws$regime
  nm <- draws$coef_names
  for (k in which(reg == "shared")) add(nm[k], draws$mu[, k], "shared")
  rand <- which(reg == "random")
  if (is.null(draws$gamma)) {
    for (k in rand) add(paste0("mu:", nm[k]), draws$mu[, k], "population_mean")
  } else {
    for (cn in colnames(draws$gamma)) add(cn, draws$gamma[, cn], "moderator")
  }
  for (k in rand) add(paste0("tau:", nm[k]), draws$tau[, k], "between_group_sd")
  if (!is.null(draws$Omega)) {
    rn <- dimnames(draws$Omega)[[2]]
    q <- length(rn)
    for (j in seq_len(q - 1L)) for (i in (j + 1L):q) {
      add(sprintf("cor:%s,%s", rn[j], rn[i]), draws$Omega[, i, j], "correlation")
    }
  }
  for (k in which(reg != "shared")) {
    for (g in seq_along(draws$groups)) {
      add(sprintf("%s[%s]", nm[k], draws$groups[g]), draws$theta[, g, k],
          "group_coefficient", draws$groups[g])
    }
  }
  nuis <- c("va", "cab", "vb", "rho")
  if (identical(draws$family, "nrm")) nuis <- c(nuis, "s2")
  if (isTRUE(draws$R > 0)) nuis <- c(nuis, "uv_realized")
  for (q_nm in nuis) {
    for (g in seq_along(draws$groups)) {
      add(sprintf("%s[%s]", q_nm, draws$groups[g]), draws$ame[[q_nm]][, g],
          "nuisance", draws$groups[g])
    }
  }
  mat <- do.call(cbind, cols)
  colnames(mat) <- names(cols)
  d <- .netis_mcmc_diagnostics(mat, chain = draws$chain, starts = starts)
  meta <- do.call(rbind, info)
  out <- cbind(family = unname(meta[, "family"]), group = unname(meta[, "group"]),
               d$table, stringsAsFactors = FALSE)
  attr(out, "scope") <- d$scope
  attr(out, "note") <- d$note
  attr(out, "n_chains") <- d$n_chains
  attr(out, "n_draws") <- d$n_draws
  out
}
