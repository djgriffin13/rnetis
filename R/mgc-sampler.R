# Parts of this file derive from the amen package (Hoff, Fosdick, Volfovsky and
# He; GPL-3), modified in 2026 for the hierarchical multi-group model. See
# inst/COPYRIGHTS.
# PORTABLE MULTI-GROUP CORE (mgc) -- the Gibbs driver.
# See mgc-regime.R for what this core may depend on.
#
# ONE SAMPLER FOR EVERY REGIME. Per scan:
#
#   1. per group, amen's nuisance conditionals in amen::ame()'s own order:
#      Z | . ; s2 | . (nrm) ; rho | .
#   2. every group's data summaries (Q_g, l_g), then ONE joint draw of all
#      coefficients through the kernel (mgc-kernel.R) -- shared, group and
#      random columns together, marginal over the additive effects;
#   3. the between-group parameters of any random coefficient (mgc-hyper.R);
#   4. per group, (a, b) | theta_g, then U, V (R > 0), then Sab (and amen's
#      binary a/Sab step), again in amen's order.
#
# Within a group this is amen::ame()'s scan with the coefficient step replaced,
# which is why a no-pooling run targets exactly amen's posterior. The draws are
# NOT the same stream as amen's -- the coefficient draw consumes the RNG
# differently -- and are not meant to be: agreement with amen means posterior
# equivalence within Monte Carlo error, not identical draws. This package tests the sampler's
# chains, starts, degenerate groups and reporting (test-mgc-chains.R,
# test-mgc-degenerate.R) and its hyperparameter layer
# (comprehensive/test-mgc-hyper-invariance.R); it does not test posterior
# equivalence with amen.
#
# The posterior-predictive simulation and GOF statistics amen computes each
# scan are not computed here: they do not affect the posterior, and would
# consume RNG only to keep amen's stream aligned, which this sampler does not
# attempt.
#
# NOT SUPPORTED HERE, refused rather than approximated: symmetric relations,
# families other than bin/nrm, replicated (multi-wave outcome) designs.

#' Validate and prepare the groups for sampling
#'
#' @param groups a named list over groups, each with `Y` (n x n) and optional
#'   `Xrow` (n x pr), `Xcol` (n x pc), `Xdyad` (n x n x pd).
#' @param family `"bin"` or `"nrm"`.
#' @param R multiplicative dimension.
#' @param coef_names optional names for the design columns (replacing amen's).
#' THREE DIFFERENT KINDS OF DEGENERATE GROUP, handled differently:
#'
#' * **No observed dyads** (every off-diagonal dyad missing): the group
#'   contributes no data, so it is refused under every family and regime. It is
#'   never dropped silently.
#' * **No variation in the outcome.** Binary: all observed ties 0, or all 1.
#'   Recorded as `tie_variation` and judged later, once the regime is known
#'   (`.mgc_assert_degenerate_admissible()`), because whether such a group is
#'   usable depends on its intercept's regime. Normal: every observed tie value
#'   equal. Refused, because `amen`'s g-prior for the group is scaled by its
#'   tie variance, which is then zero.
#' * **A predictor with no within-group variation** is not a property of the
#'   outcome and is not handled here: see `.selection_predictor_information()`.
#'
#' Missing dyads stay `NA` throughout. They are never counted as observed and
#' never become observed zeros.
#'
#' @return a named list over groups: `Y`, `X`, `n`, `mask`, `nuisance_prior`,
#'   `tie_variation` (`NA` when the outcome varies; `"all0"` or `"all1"` for a
#'   binary group with none), and `n_observed` (observed off-diagonal dyads).
#' @noRd
.mgc_prepare_groups <- function(groups, family, R = 0L, coef_names = NULL) {
  if (!is.list(groups) || is.null(names(groups)) || any(!nzchar(names(groups))) ||
      anyDuplicated(names(groups))) {
    stop("groups must be a list named by unique group IDs.", call. = FALSE)
  }
  if (!family %in% c("bin", "nrm")) stop("family must be \"bin\" or \"nrm\".", call. = FALSE)
  prep <- lapply(names(groups), function(gid) {
    gr <- groups[[gid]]
    Y <- as.matrix(gr$Y)
    n <- nrow(Y)
    if (ncol(Y) != n) stop(sprintf("group '%s': Y must be square.", gid), call. = FALSE)
    diag(Y) <- NA
    n_observed <- sum(!is.na(Y))
    if (n_observed == 0L) {
      stop(sprintf(
        "Group '%s' has no observed dyads: every tie between its actors is missing, so it contributes no data to a Selection model. Remove the group, or supply its observed ties.",
        gid), call. = FALSE)
    }
    tie_variation <- NA_character_
    if (identical(family, "bin")) {
      Y <- 1 * (Y > 0)
      obs <- Y[!is.na(Y)]
      if (all(obs == 0)) tie_variation <- "all0"
      if (all(obs == 1)) tie_variation <- "all1"
    } else {
      obs <- Y[!is.na(Y)]
      if (length(unique(obs)) < 2L) {
        stop(sprintf(
          "Group '%s' has the same tie value on all %d observed dyad(s). A normal Selection model scales each group's coefficient prior by its tie variance, which is zero here, so the group cannot be fitted. Remove the group, or model a binary version of the tie.",
          gid, length(obs)), call. = FALSE)
      }
    }
    X <- .mgc_design(gr$Xrow, gr$Xcol, gr$Xdyad, n)
    if (!is.null(coef_names)) {
      if (length(coef_names) != dim(X)[3]) {
        stop(sprintf("group '%s': %d coef_names for %d design columns.", gid,
                     length(coef_names), dim(X)[3]), call. = FALSE)
      }
      dn <- dimnames(X)
      dn[[3]] <- coef_names
      dimnames(X) <- dn
    }
    mask <- !is.na(Y)
    diag(mask) <- FALSE
    if (n < 3L) {
      stop(.selection_small_group_message(gid, n), call. = FALSE)
    }
    if (identical(family, "bin") && is.na(tie_variation) && .amen_regular_degree(Y)) {
      stop(.selection_regular_degree_message(gid), call. = FALSE)
    }
    np <- .mgc_amen_nuisance_prior(Y, family, R)
    list(Y = Y, X = X, n = n, mask = mask, nuisance_prior = np,
         tie_variation = tie_variation, n_observed = n_observed)
  })
  names(prep) <- names(groups)

  ref <- dimnames(prep[[1]]$X)[[3]]
  for (gid in names(prep)) {
    if (!identical(dimnames(prep[[gid]]$X)[[3]], ref)) {
      stop(sprintf(
        "Design columns differ between groups: '%s' has %s; '%s' has %s. A coefficient that is shared or random across groups must mean the same thing in each.",
        names(prep)[1], paste(ref, collapse = ", "), gid,
        paste(dimnames(prep[[gid]]$X)[[3]], collapse = ", ")), call. = FALSE)
    }
  }
  prep
}

#' Refuse a regime whose shared columns cannot be separated across groups
#'
#' @noRd
.mgc_assert_identified <- function(prep, regime) {
  s <- regime$shared
  if (!length(s)) return(invisible(TRUE))
  M <- do.call(rbind, lapply(prep, function(gr) attributes(gr$X)$mX[, s, drop = FALSE]))
  qrM <- qr(M)
  if (qrM$rank < length(s)) {
    bad <- regime$coef_names[s][qrM$pivot[(qrM$rank + 1L):length(s)]]
    stop(sprintf("The shared coefficients are not identified even with every group combined: %s cannot be separated from the others.",
                 paste(bad, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Refuse binary groups without tie variation unless the regime can use them
#'
#' rnetis's supported admission rule. A group whose observed
#' ties are all 0 (or all 1) is admitted if and only if the intercept is random
#' and no coefficient is group-specific.
#'
#' WHY THOSE TWO CONDITIONS. Such a group's data say one thing: its baseline
#' propensity is low (high) enough that no tie formed (every tie did). Its
#' likelihood is bounded and monotone in its intercept, so a proper prior on
#' that intercept gives a proper posterior.
#'
#' * With a RANDOM intercept the prior is the between-group distribution, and
#'   the group's intercept is pulled below (above) its hierarchical mean: in
#'   every draw its full conditional is that prior tilted by a likelihood
#'   monotone in the intercept, so the shift has a definite sign. How large it
#'   is depends on how the low (high) tie rate is SPLIT between the intercept
#'   and the group's own sender and receiver effects, and that split is
#'   prior-sensitive: such a group's actor-effect prior is amen's diffuse fixed
#'   default (see .mgc_amen_nuisance_prior()), so in practice the actor effects
#'   can carry much of it, with that group's actor-effect variance inflated
#'   well above the other groups'.
#' * With a SHARED intercept there is no group-level parameter to carry the
#'   group's low (high) tie rate, so it is absorbed by the group's own actor
#'   effects and their covariance -- "every member is unusually unsociable",
#'   an attribution supplied by the prior rather than the data.
#' * A GROUP-SPECIFIC coefficient in such a group is informed by its own prior
#'   alone once the linear predictor can be pushed below (above) zero
#'   everywhere.
#'
#' This is the package's rule for what it will report, not a claim that no
#' Bayesian model could use such a group.
#'
#' @param prep prepared groups; `reg` the regime.
#' @noRd
.mgc_assert_degenerate_admissible <- function(prep, reg) {
  tv <- vapply(prep, function(gr) .mgc_or(gr$tie_variation, NA_character_), character(1))
  flat <- names(prep)[!is.na(tv)]
  if (!length(flat)) {
    return(invisible(TRUE))
  }
  intercept_random <- identical(unname(reg$regime["intercept"]), "random")
  unpooled <- reg$coef_names[reg$regime == "group"]
  if (intercept_random && !length(unpooled)) {
    return(invisible(TRUE))
  }
  which_teams <- paste(sprintf("'%s' (all %s)", flat, ifelse(tv[flat] == "all0", "0", "1")),
                       collapse = ", ")
  reasons <- c(
    if (!intercept_random) {
      "the intercept is not random, so such a group's tie rate has no group-level parameter to go to and would be attributed to its actors' sender and receiver effects by the prior alone"
    },
    if (length(unpooled)) {
      sprintf("%s %s group-specific, and in such a group a group-specific coefficient is informed by its prior alone",
              paste(unpooled, collapse = ", "), if (length(unpooled) == 1L) "is" else "are")
    }
  )
  stop(sprintf(
    "Group(s) %s have no variation in their binary ties. rnetis admits such a group only when the intercept is random and no coefficient is group-specific; here %s. Fit with selection_pooling(random = \"intercept\", ...) and no `unpooled` coefficients, or remove the group(s).",
    which_teams, paste(reasons, collapse = "; and ")), call. = FALSE)
}

#' amen::ame()'s initial state for one group
#'
#' A binary group without tie variation cannot use amen's start: its threshold
#' is the midpoint between the largest score among 0s and the smallest among 1s,
#' and one of those sets is empty (`Inf`, and every latent value becomes
#' `-Inf`). Such a group starts every observed latent value at
#' `qnorm(0.5 / (m + 1))` -- negative for an all-0 group, its negation for an
#' all-1 group -- where `m >= 1` is the number of observed dyads.
#' `0.5 / (m + 1)` is the Jeffreys posterior mean of the tie probability with no
#' tie in `m` trials. It is strictly inside (0, 1/2) for every `m >= 1`, so the
#' start is finite and strictly on the side of zero the truncation requires.
#' (`m = 0` cannot reach here: a group with no observed dyads is refused.)
#' Missing dyads are left unconstrained, exactly as for any other group.
#'
#' @noRd
.mgc_init_group <- function(gr, family, R) {
  Y <- gr$Y
  n <- gr$n
  X <- gr$X
  no_variation <- FALSE
  if (identical(family, "bin")) {
    obs <- !is.na(Y)
    yo <- Y[obs]
    no_variation <- all(yo == 0) || all(yo == 1)
    if (no_variation) {
      z0 <- stats::qnorm(0.5 / (sum(obs) + 1))
      Z <- matrix(NA_real_, n, n)
      Z[obs] <- if (all(yo == 0)) z0 else -z0
    } else {
      Z <- .mgc_start_latent(Y, n, family)
    }
  } else {
    Z <- .mgc_start_latent(Y, n, family)
  }
  start <- .mgc_start_from_latent(Z, X)
  Z <- start$Z
  theta <- start$theta
  a <- start$a
  b <- start$b
  E <- start$E
  Sab <- start$Sab
  s2 <- if (identical(family, "nrm")) mean(E^2) else 1
  # rho MUST START STRICTLY INSIDE (-1, 1): at +/-1 the dyadic covariance is
  # singular and amen's rho update fails ("missing value where TRUE/FALSE
  # needed"). amen's start is the correlation of the residual's two triangles,
  # which is undefined (zero variance) or exactly +/-1 when the residual carries
  # no information -- always the case for a group without tie variation, whose
  # latent start is constant. Such a start is replaced by 0, the center of
  # rho's range and of its prior. For every group where amen's start is
  # defined and inside (-0.99, 0.99) it is unchanged.
  rho <- start$rho
  if (no_variation || !is.finite(rho) || abs(rho) >= 0.99) rho <- 0
  U <- V <- matrix(0, n, R)
  if (R > 0) {
    sE <- svd(E)
    U <- sE$u[, 1:R, drop = FALSE] %*% diag(sqrt(sE$d[1:R]), nrow = R)
    V <- sE$v[, 1:R, drop = FALSE] %*% diag(sqrt(sE$d[1:R]), nrow = R)
  }
  list(Z = Z, theta = theta, a = a, b = b, Sab = Sab, s2 = s2, rho = rho, U = U, V = V,
       Suv = NULL)
}

#' amen::ame()'s starting latent values for a group whose ties vary
#'
#' @noRd
.mgc_start_latent <- function(Y, n, family) {
  if (!identical(family, "bin")) return(Y)
  Z <- matrix(amen::zscores(Y), n, n)
  Z - 0.5 * (max(Z[Y == 0], na.rm = TRUE) + min(Z[Y == 1], na.rm = TRUE))
}

#' amen::ame()'s starting state, given the starting latent values
#'
#' @return `Z` (with missing cells filled), `theta`, `a`, `b`, `Sab`, the residual
#'   `E`, and `rho`, the correlation of `E`'s two triangles.
#' @noRd
.mgc_start_from_latent <- function(Z, X) {
  mu <- mean(Z, na.rm = TRUE)
  a <- rowMeans(Z, na.rm = TRUE); b <- colMeans(Z, na.rm = TRUE)
  a[is.na(a)] <- 0; b[is.na(b)] <- 0
  ZA <- mu + outer(a, b, "+")
  Z[is.na(Z)] <- ZA[is.na(Z)]
  p <- dim(X)[3]
  theta <- c(solve(attributes(X)$XX + diag(p)) %*% crossprod(attributes(X)$mX, c(Z)))
  E <- Z - amen::Xbeta(X, theta)
  a <- rowMeans(E, na.rm = TRUE); b <- colMeans(E, na.rm = TRUE)
  a[is.na(a)] <- 0; b[is.na(b)] <- 0
  Sab <- stats::cov(cbind(a, b))
  E <- E - outer(a, b, "+")
  rho <- suppressWarnings(stats::cor(c(E[upper.tri(E)]), c(t(E)[upper.tri(E)])))
  list(Z = Z, theta = theta, a = a, b = b, Sab = Sab, E = E, rho = rho)
}

#' Fit a multi-group AME model under a coefficient pooling regime
#'
#' @param groups see `.mgc_prepare_groups()`.
#' @param regime a regime vector (see `.mgc_regime()`).
#' @param family `"bin"` or `"nrm"`.
#' @param R multiplicative dimension.
#' @param nscan,burn,odens as in `amen::ame()`.
#' @param seed base seed; chain k uses `seed + k - 1`.
#' @param chains number of chains, run in sequence.
#' @param hyper between-group prior settings (see `.mgc_resolve_priors()`).
#' @param coef_names optional design-column names.
#' @return a canonical draws object (mgc-draws.R).
#' @noRd
.mgc_sample <- function(groups, regime, family = "bin", R = 0L,
                        nscan = 5000, burn = 500, odens = 25, seed = 1, chains = 1L,
                        hyper = list(), coef_names = NULL,
                        moderators = NULL, covariance = "diagonal") {
  # Each chain seeds its own stream (seed + chain - 1); the caller's global
  # RNG state is restored when this call ends, so a fit inside a simulation
  # loop does not reset the loop's stream.
  .sim_restore_stream_on_exit(seed)
  prep <- .mgc_prepare_groups(groups, family, R, coef_names)
  reg <- .mgc_regime(dimnames(prep[[1]]$X)[[3]], regime, covariance = covariance)
  .mgc_assert_degenerate_admissible(prep, reg)
  .mgc_assert_identified(prep, reg)
  if (!is.null(moderators)) {
    moderators <- as.matrix(moderators)
    if (nrow(moderators) != length(prep)) {
      stop(sprintf("The moderator matrix has %d row(s) for %d group(s). It carries one row per group, in the groups' own order.",
                   nrow(moderators), length(prep)), call. = FALSE)
    }
    # ROW ORDER IS AN IDENTITY, NOT A CONVENIENCE. If the rows are labeled,
    # they are matched to the groups BY NAME and a mismatch is refused;
    # silently trusting position would attach one group's moderators to
    # another, which no fit diagnostic would show.
    rn <- rownames(moderators)
    if (!is.null(rn)) {
      if (!setequal(rn, names(prep))) {
        stop(sprintf("The moderator rows name groups the fit does not have, or omit ones it does. Missing: %s. Unknown: %s.",
                     paste(setdiff(names(prep), rn), collapse = ", ") ,
                     paste(setdiff(rn, names(prep)), collapse = ", ")), call. = FALSE)
      }
      moderators <- moderators[names(prep), , drop = FALSE]
    }
  }
  pri <- .mgc_resolve_priors(prep, reg, family, hyper)
  runs <- lapply(seq_len(chains), function(ch) {
    .mgc_run_chain(prep, reg, pri, family, R, nscan, burn, odens, seed + ch - 1L, ch,
                   moderators = moderators)
  })
  out <- .mgc_combine_chains(runs)
  out$control <- list(nscan = nscan, burn = burn, odens = odens, seed = seed, chains = chains,
                      chain_seeds = seed + seq_len(chains) - 1L)
  out$moderators <- if (is.null(moderators)) NULL else colnames(moderators)[-1]
  out$covariance <- reg$covariance
  # Per group, not per draw: which groups were admitted without tie variation.
  out$tie_variation <- vapply(prep, function(gr) .mgc_or(gr$tie_variation, NA_character_),
                              character(1))
  # THE JOINT CLAIM, ASSERTED RATHER THAN DOCUMENTED. A shared coefficient must
  # hold the SAME value in every group within a draw -- that is what makes the
  # draw one state of one posterior rather than per-group chains wearing a
  # shared label.
  .mgc_validate_draws(out)
  out
}

#' One chain of the multi-group sampler
#'
#' @noRd
.mgc_run_chain <- function(prep, reg, pri, family, R, nscan, burn, odens, seed, chain,
                           moderators = NULL) {
  set.seed(seed)
  G <- length(prep)
  gids <- names(prep)
  p <- length(reg$coef_names)
  states <- lapply(prep, .mgc_init_group, family = family, R = R)
  theta <- matrix(vapply(states, `[[`, numeric(p), "theta"), nrow = G, ncol = p,
                  byrow = TRUE, dimnames = list(gids, reg$coef_names))

  rs <- NULL
  if (length(reg$random)) {
    # One (tau2, lambda) per random coefficient: the between-group SDs are
    # independent, so each starts from its own spread across groups.
    k <- reg$random
    A <- rep(pri$random$A, length.out = length(k))
    start_sd <- vapply(seq_along(k), function(j) {
      max(stats::sd(theta[, k[j]]), 0.1 * pri$random$outcome_scale)
    }, numeric(1))
    rs <- list(tau2 = start_sd^2, lambda = A^2)
    if (identical(reg$covariance, "unstructured")) {
      # Started at zero correlation -- the diagonal model -- so that any
      # correlation reported came from the data rather than from where the
      # chain was placed.
      rs$omega_y <- rep(0, length(k) * (length(k) - 1L) / 2L)
      rs$Omega <- diag(length(k))
      rs$omega_accept <- 0; rs$tau_accept <- 0; rs$n_sweeps <- 0
    }
  }
  # DISPERSED STARTS for every chain after the first. Chain 1 keeps amen's own
  # start and consumes no extra random numbers, so a one-chain fit is
  # unchanged; later chains are moved away from it so that several chains can
  # disagree about where the posterior is, which is what R-hat looks for.
  if (chain > 1L) {
    disp <- .mgc_disperse_start(theta, rs, prep, family)
    theta <- disp$theta
    rs <- disp$rs
  }
  # PROPOSAL-SCALE ADAPTATION for the correlated covariance's Metropolis steps,
  # during burn-in only (see .mgc_adapt_step()).
  adapt <- .mgc_adaptation_plan(reg, burn)
  if (!is.null(adapt)) {
    rs$step_tau <- adapt$initial_tau
    rs$step_omega <- adapt$initial_omega
  }
  warmup_acceptance <- NULL
  map <- .mgc_expanded_map(reg, Z = moderators)
  shared_prior <- .mgc_expanded_shared_prior(pri, reg, map)

  n_save <- (burn + nscan) %/% odens - burn %/% odens
  dn3 <- list(NULL, gids, reg$coef_names)
  out_theta <- array(NA_real_, c(n_save, G, p), dimnames = dn3)
  out_rdp <- array(NA_real_, c(n_save, G, p), dimnames = dn3)
  out_mu <- matrix(NA_real_, n_save, p, dimnames = list(NULL, reg$coef_names))
  out_tau <- matrix(NA_real_, n_save, p, dimnames = list(NULL, reg$coef_names))
  qk <- length(reg$random)
  out_omega <- if (identical(reg$covariance, "unstructured")) {
    array(NA_real_, c(n_save, qk, qk),
          dimnames = list(NULL, reg$coef_names[reg$random], reg$coef_names[reg$random]))
  } else NULL
  gam_names <- unlist(map$gamma_pos)
  out_gamma <- if (length(map$moderators)) {
    matrix(NA_real_, n_save, length(gam_names),
           dimnames = list(NULL, map$regime$coef_names[gam_names]))
  } else NULL
  ame_names <- c("va", "cab", "vb", "rho", "s2", "uv_realized")
  out_ame <- lapply(stats::setNames(ame_names, ame_names), function(nm) {
    matrix(NA_real_, n_save, G, dimnames = list(NULL, gids))
  })
  sums <- lapply(prep, function(gr) list(a = rep(0, gr$n), b = rep(0, gr$n), uv = matrix(0, gr$n, gr$n)))
  i_save <- 0L

  for (s in seq_len(nscan + burn)) {
    for (g in seq_len(G)) {
      st <- states[[g]]
      np <- prep[[g]]$nuisance_prior
      EZ <- amen::Xbeta(prep[[g]]$X, theta[g, ]) + outer(st$a, st$b, "+") + st$U %*% t(st$V)
      if (identical(family, "nrm")) {
        st$Z <- amen::rZ_nrm_fc(st$Z, EZ, st$rho, st$s2, prep[[g]]$Y)
        st$s2 <- amen::rs2_fc(st$Z - EZ, st$rho, nu0 = np$nu0, s20 = np$s20)
      } else {
        st$Z <- amen::rZ_bin_fc(st$Z, EZ, st$rho, prep[[g]]$Y)
      }
      st$rho <- amen::rrho_mh(st$Z - EZ, st$rho, st$s2, asp = np$asp)
      states[[g]] <- st
    }

    stats_g <- lapply(seq_len(G), function(g) {
      st <- states[[g]]
      .mgc_group_stats(st$Z - st$U %*% t(st$V), st$Sab, st$rho, st$s2, prep[[g]]$X)
    })
    names(stats_g) <- gids
    Sigma_now <- if (identical(reg$covariance, "unstructured")) {
      .mgc_sigma(rs, length(reg$random))
    } else NULL
    block_priors <- lapply(seq_len(G), function(g) {
      .mgc_block_prior(pri$group_static[[g]], reg, tau2 = rs$tau2, Sigma = Sigma_now)
    })
    # One joint draw in the expanded parameterization (shared coefficients and
    # population means; group coefficients and deviations), then back to each
    # group's own coefficient vector theta_g = T phi_g.
    # EACH GROUP CARRIES ITS OWN MAP when moderators are present: row k of
    # group g's T holds z_g', so expanding the stats and coming back out of
    # the expanded parameterization are both per group. Applying one group's
    # T to another's coefficients would attribute that group's moderator
    # values to every group -- a plausible number with the wrong covariates
    # behind it.
    draw <- .mgc_draw_coefficients(
      lapply(seq_len(G), function(g) .mgc_expand_stats(stats_g[[g]], map, g)),
      map$regime, shared_prior, block_priors)
    # matrix(byrow = TRUE) rather than t(): with one coefficient vapply()
    # returns a plain vector, and t() of it is 1 x G, not G x 1.
    theta <- matrix(vapply(seq_len(G), function(g) {
      c(.mgc_group_T(map, g) %*% draw$theta[g, ])
    }, numeric(p)), nrow = G, ncol = p, byrow = TRUE,
    dimnames = list(gids, reg$coef_names))
    if (!is.null(rs)) {
      # DISPATCH, not a branch inside the update: the diagonal path is the
      # exact Gibbs step and stays untouched, and at q = 1 the correlated
      # entry point hands back to it, so an unstructured fit with one random
      # coefficient is bit-identical to a diagonal one.
      rs <- if (identical(reg$covariance, "unstructured")) {
        .mgc_update_random_corr(draw$theta[, map$zeta_pos, drop = FALSE], rs, pri$random)
      } else {
        .mgc_update_random(draw$theta[, map$zeta_pos, drop = FALSE], rs, pri$random)
      }
      rs$mu <- draw$theta[1, map$mu_pos]
      if (length(map$moderators)) {
        # gamma is SHARED, so every row of draw$theta holds the same value;
        # row 1 is taken for that reason, not as a representative group.
        rs$gamma <- draw$theta[1, unlist(map$gamma_pos), drop = FALSE]
      }
      if (!is.null(adapt) && s <= burn) {
        rs <- .mgc_adapt_step(rs, adapt, s)
      }
      if (identical(reg$covariance, "unstructured") && s == burn) {
        # The end of burn-in: record warm-up acceptance, then count retained
        # acceptance from zero, so the rate reported for the retained draws
        # describes the frozen kernel that produced them.
        warmup_acceptance <- list(
          tau = .mgc_or(rs$tau_accept, 0) / max(1, .mgc_or(rs$n_sweeps, 1)),
          omega = .mgc_or(rs$omega_accept, 0) / max(1, .mgc_or(rs$n_sweeps, 1)),
          sweeps = .mgc_or(rs$n_sweeps, 0))
        rs$tau_accept <- 0
        rs$omega_accept <- 0
        rs$n_sweeps <- 0
      }
    }

    for (g in seq_len(G)) {
      st <- states[[g]]
      np <- prep[[g]]$nuisance_prior
      X <- prep[[g]]$X
      ab <- .mgc_draw_ab(stats_g[[g]]$ctx, theta[g, ])
      st$a <- ab$a
      st$b <- ab$b
      if (R > 0) {
        E <- st$Z - (amen::Xbeta(X, theta[g, ]) + outer(st$a, st$b, "+"))
        st$Suv <- amen::rSuv_fc(st$U, st$V, Suv0 = np$Suv0, kappa0 = np$kappa0)
        UV <- amen::rUV_fc(E, st$U, st$V, st$Suv, st$rho, st$s2)
        st$U <- UV$U
        st$V <- UV$V
      }
      st$Sab <- amen::rSab_fc(st$a, st$b, Sab0 = np$Sab0, eta0 = np$eta0)
      if (identical(family, "bin")) {
        tmp <- amen::raSab_bin_fc(st$Z, prep[[g]]$Y, st$a, st$b, st$Sab, Sab0 = np$Sab0, eta0 = np$eta0)
        st$Z <- tmp$Z
        st$Sab <- tmp$Sab
        st$a <- tmp$a
      }
      states[[g]] <- st
    }

    if (s %% odens == 0 && s > burn) {
      i_save <- i_save + 1L
      out_theta[i_save, , ] <- theta
      shared <- reg$shared
      if (length(shared)) out_mu[i_save, shared] <- theta[1, shared]
      if (!is.null(rs)) {
        out_mu[i_save, reg$random] <- rs$mu
        out_tau[i_save, reg$random] <- sqrt(rs$tau2)
        if (!is.null(out_gamma)) out_gamma[i_save, ] <- rs$gamma
        if (!is.null(out_omega)) out_omega[i_save, , ] <- rs$Omega
        out_rdp[i_save, , reg$random] <- draw$block_precision[, paste0("zeta:", names(map$zeta_pos))]
      }
      for (g in seq_len(G)) {
        st <- states[[g]]
        out_ame$va[i_save, g] <- st$Sab[1, 1]
        out_ame$cab[i_save, g] <- st$Sab[1, 2]
        out_ame$vb[i_save, g] <- st$Sab[2, 2]
        out_ame$rho[i_save, g] <- st$rho
        out_ame$s2[i_save, g] <- st$s2
        uvm <- st$U %*% t(st$V)
        out_ame$uv_realized[i_save, g] <- if (R > 0) stats::var(uvm[prep[[g]]$mask]) else 0
        sums[[g]]$a <- sums[[g]]$a + st$a
        sums[[g]]$b <- sums[[g]]$b + st$b
        sums[[g]]$uv <- sums[[g]]$uv + uvm
      }
    }
  }

  list(
    version = .MGC_DRAWS_VERSION,
    family = family, R = R,
    groups = gids, coef_names = reg$coef_names, regime = reg$regime,
    n_actors = vapply(prep, `[[`, integer(1), "n"),
    chain = rep(as.integer(chain), n_save),
    # METROPOLIS ACCEPTANCE, carried out of the chain because it cannot be
    # recovered from the draws. A step that never accepts returns the
    # correlation it started from: the chain looks stable, the posterior
    # looks tight, and the number is the initial value. Nothing in the
    # retained draws distinguishes that from a genuinely concentrated
    # posterior, so the counts have to travel.
    # `omega`, `tau` and `sweeps` describe the RETAINED phase (after burn-in),
    # under the frozen proposal; `warmup` the burn-in phase, during which the
    # proposal scales were being adapted; `steps` the scales used after burn-in.
    acceptance = if (identical(reg$covariance, "unstructured")) {
      list(omega = .mgc_or(rs$omega_accept, 0) / max(1, .mgc_or(rs$n_sweeps, 1)),
           tau = .mgc_or(rs$tau_accept, 0) / max(1, .mgc_or(rs$n_sweeps, 1)),
           sweeps = .mgc_or(rs$n_sweeps, 0),
           warmup = .mgc_or(warmup_acceptance,
                            list(tau = NA_real_, omega = NA_real_, sweeps = 0)),
           steps = list(tau = .mgc_or(rs$step_tau, NA_real_),
                        omega = .mgc_or(rs$step_omega, NA_real_),
                        adapted = isTRUE(adapt$active),
                        target_tau = .mgc_or(adapt$target_tau, NA_real_),
                        target_omega = .mgc_or(adapt$target_omega, NA_real_)))
    } else NULL,
    starts = if (chain > 1L) "dispersed" else "amen",
    theta = out_theta, mu = out_mu, tau = out_tau, Omega = out_omega, gamma = out_gamma,
    ame = out_ame, random_data_precision = out_rdp,
    posterior_means = lapply(sums, function(x) lapply(x, function(v) v / n_save)),
    priors = list(shared = pri$shared, group_static = pri$group_static, random = pri$random),
    control = list(nscan = nscan, burn = burn, odens = odens, seed = seed, chains = 1L)
  )
}
