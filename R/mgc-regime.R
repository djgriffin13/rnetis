# PORTABLE MULTI-GROUP CORE (mgc) -- coefficient pooling regimes.
#
# The mgc-*.R files are the statistical core of multi-group AME Selection. They
# are self-contained: base R plus amen's EXPORTED functions only, no rnetis
# classes, no formula parsing, no reporting, and every name prefixed `.mgc_`
# so nothing collides with the package's other internal helpers. Interface
# code (formulas, fit objects, summaries) lives elsewhere and calls in;
# nothing here calls out.
#
# THE UNIT OF POOLING IS THE COEFFICIENT, not the model. Each design column is
#
#   "group"   theta_kg estimated separately in each group, under its own prior
#   "shared"  theta_kg = beta_k, one value for every group
#   "random"  theta_kg = mu_k + zeta_kg,  zeta_g ~ N(0, Sigma), across groups
#
# and the familiar models are combinations: no pooling is every column
# "group"; complete pooling is every column "shared"; shared slopes with
# group intercepts, a random intercept, random slopes, selective partial
# pooling -- all are one representation and one sampler (mgc-kernel.R).
#
# THE INTERCEPT is an ordinary coefficient here. There is exactly one intercept
# per group in the linear predictor -- the coefficient of the design's
# intercept column -- and its regime decides whether it is group-specific,
# shared, or random. Nothing adds a separate group term beside it, which is
# what keeps a random intercept from being a second, unidentified intercept.

#' @noRd
.MGC_REGIMES <- c("group", "shared", "random")

#' Build and validate a coefficient pooling regime
#'
#' @param coef_names the design's coefficient names, in design-column order.
#'   Must include `"intercept"` as the first column.
#' @param regime a character vector of `.MGC_REGIMES`, one per coefficient
#'   (named or in `coef_names` order).
#' @return a list: `coef_names`, `regime` (named), `shared`, `block` (the
#'   group-varying columns, `group` then `random`, as integer indices),
#'   `random` (integer indices), `covariance` (`"diagonal"` or
#'   `"unstructured"`).
#' @noRd
.mgc_regime <- function(coef_names, regime, covariance = "diagonal") {
  if (!is.character(coef_names) || anyNA(coef_names) || anyDuplicated(coef_names)) {
    stop("coef_names must be unique, non-missing names.", call. = FALSE)
  }
  if (!identical(coef_names[1], "intercept")) {
    stop("The first design column must be the intercept: the regime of the group intercept is part of every model.",
         call. = FALSE)
  }
  p <- length(coef_names)
  if (!is.null(names(regime))) {
    if (!setequal(names(regime), coef_names)) {
      stop(sprintf("regime names must match coef_names exactly. Missing: %s. Unknown: %s.",
                   paste(setdiff(coef_names, names(regime)), collapse = ", "),
                   paste(setdiff(names(regime), coef_names), collapse = ", ")),
           call. = FALSE)
    }
    regime <- regime[coef_names]
  }
  if (length(regime) != p || !all(regime %in% .MGC_REGIMES)) {
    stop(sprintf("regime must give one of %s for each of the %d coefficients.",
                 paste(.MGC_REGIMES, collapse = "/"), p), call. = FALSE)
  }
  names(regime) <- coef_names

  random <- unname(which(regime == "random"))
  # TWO COVARIANCE MODELS for several random coefficients. "diagonal": each
  # tau_k from its own exact Gibbs conditional, no cross-coefficient term.
  # "unstructured": Sigma = diag(tau) Omega diag(tau) with an LKJ prior on
  # Omega (mgc-hyper-correlated.R). A diagonal fitted where the truth is
  # correlated does not approximate it -- it assumes the correlations away --
  # so the choice is a declared field every consumer can read.

  if (!covariance %in% c("diagonal", "unstructured")) {
    stop("covariance must be \"diagonal\" or \"unstructured\".", call. = FALSE)
  }
  if (identical(covariance, "unstructured") && length(random) < 2L) {
    # A CORRELATION NEEDS TWO THINGS TO CORRELATE. With one random
    # coefficient an unstructured covariance is the diagonal one -- Omega is
    # 1x1 with no free parameter -- so accepting the word here would let a
    # caller believe they had asked for something they did not get.
    stop("An unstructured between-group covariance needs at least two random coefficients; with one there is no correlation to estimate and the covariance is its variance.",
         call. = FALSE)
  }

  list(
    coef_names = coef_names,
    regime = regime,
    shared = unname(which(regime == "shared")),
    block = c(unname(which(regime == "group")), random),
    random = random,
    covariance = covariance
  )
}

#' `x` unless it is NULL, then `y`
#'
#' The core's own copy: rnetis's `%||%` is package-local, base R only gained one
#' in 4.4, and the core must not depend on either.
#'
#' @noRd
.mgc_or <- function(x, y) if (is.null(x)) y else x

#' The kernel's parameterization of a regime: shared parameters and group blocks
#'
#' The kernel (mgc-kernel.R) draws a shared vector phi and one block psi_g per
#' group jointly. This map says how they make up each group's coefficients:
#'
#'     theta_g = T_s phi + T_r psi_g
#'
#' phi holds the "shared" coefficients followed by the population mean mu_k of
#' each "random" coefficient; psi_g holds the "group" coefficients followed by
#' the deviations zeta_kg. A random coefficient therefore appears twice -- its
#' mean in phi, its deviation in psi_g -- and the data summaries are carried
#' into this parameterization as Q~ = T' Q_g T and l~ = T' l_g.
#'
#' WHY mu IS A SHARED PARAMETER. Putting mu_k in phi means mu, the shared slopes
#' and every zeta_g come from one exact joint Gaussian draw. Updating mu
#' separately from the current theta (the centered Gibbs step) couples mu and
#' theta so tightly at small tau that the chain stops moving; see mgc-hyper.R.
#'
#' GROUP-LEVEL MODERATORS are this same map with a group-specific
#' T_s. For each random coefficient k the population mean becomes a regression
#' on group-level predictors,
#'
#'     theta_kg = z_g' gamma_k + zeta_kg,   z_g = (1, z_g1, ..., z_gM)'
#'
#' so gamma_k is SHARED -- it joins phi exactly as mu_k did -- and row k of
#' group g's T carries z_g' across gamma_k's columns. The block-arrow kernel is
#' untouched. At M = 0, z_g = 1 and gamma_k0 IS mu_k, so the map, its names and
#' its positions are identical to the no-moderator case; that reduction is
#' asserted by test rather than asserted here.
#'
#' WHY THE NAMES CHANGE ONLY WHEN M > 0. With no moderators the column is
#' `mu:<k>` and means the population mean. With moderators it is
#' `gamma:<k>:(Intercept)` and means the coefficient AT z = 0, which is a
#' different quantity unless the moderators are centered, so it does not carry
#' the name `mu`.
#'
#' @param reg a `.mgc_regime()` object.
#' @return a list: `T` (p x (s' + r)), `regime` (an expanded regime the kernel
#'   accepts: `coef_names`, `shared`, `block`), `mu_pos` and `zeta_pos` (the
#'   positions of each random coefficient's mean and deviation in the expanded
#'   vector), named by coefficient.
#' @noRd
.mgc_expanded_map <- function(reg, Z = NULL) {
  nm <- reg$coef_names
  p <- length(nm)
  S <- reg$shared
  Fc <- setdiff(reg$block, reg$random)
  K <- reg$random

  if (!is.null(Z)) {
    Z <- as.matrix(Z)
    if (!length(K)) {
      stop("Moderators describe how a RANDOM coefficient varies across groups, so they need at least one random coefficient. With none, every coefficient is either shared (identical in every group) or group-specific (already free), and a group-level predictor has nothing to explain.",
           call. = FALSE)
    }
    if (!ncol(Z) || any(Z[, 1] != 1)) {
      stop("The moderator matrix must carry an intercept as its first column: gamma_k0 is the coefficient at z = 0, and dropping it forces every group's random coefficient through the origin.",
           call. = FALSE)
    }
    if (anyNA(Z)) {
      stop("The moderator matrix contains missing values. A group with an unknown moderator has no defined population mean, and rnetis will not impute one.",
           call. = FALSE)
    }
  }
  M <- if (is.null(Z)) 0L else ncol(Z) - 1L
  mod_names <- if (M) colnames(Z)[-1] else character(0)
  if (M && (is.null(colnames(Z)) || !all(nzchar(mod_names)))) {
    stop("Every moderator column must be named: gamma is reported per moderator, and an unnamed column cannot be reported.", call. = FALSE)
  }

  # gamma_k occupies (1 + M) shared columns. At M = 0 that is one column and
  # it is `mu:<k>`, so the no-moderator map is reproduced exactly rather than
  # approximated -- same names, same positions, same T.
  width <- 1L + M
  gam_names <- if (!length(K)) character(0) else if (M == 0L) {
    paste0("mu:", nm[K])
  } else {
    unlist(lapply(nm[K], function(k) paste0("gamma:", k, ":", c("(Intercept)", mod_names))))
  }
  s_names <- c(nm[S], gam_names)
  r_names <- c(nm[Fc], if (length(K)) paste0("zeta:", nm[K]))
  off <- length(s_names)

  build <- function(z) {
    Tm <- matrix(0, p, length(s_names) + length(r_names),
                 dimnames = list(nm, c(s_names, r_names)))
    for (i in seq_along(S)) Tm[S[i], i] <- 1
    # Row k picks up z_g' across gamma_k's columns. This is the ONLY place the
    # map depends on the group.
    for (j in seq_along(K)) {
      cols <- length(S) + (j - 1L) * width + seq_len(width)
      Tm[K[j], cols] <- z
    }
    for (i in seq_along(Fc)) Tm[Fc[i], off + i] <- 1
    for (j in seq_along(K)) Tm[K[j], off + length(Fc) + j] <- 1
    Tm
  }

  gamma_pos <- if (!length(K)) list() else stats::setNames(
    lapply(seq_along(K), function(j) length(S) + (j - 1L) * width + seq_len(width)),
    nm[K])

  list(
    # T is the common map when there are no moderators; T_list is the
    # per-group one when there are. Exactly one of them is non-NULL, so a
    # caller cannot silently apply a single T to groups that need their own.
    T = if (is.null(Z)) build(1) else NULL,
    T_list = if (is.null(Z)) NULL else lapply(seq_len(nrow(Z)), function(g) build(Z[g, ])),
    moderators = mod_names,
    regime = list(coef_names = c(s_names, r_names),
                  shared = seq_along(s_names),
                  block = off + seq_along(r_names)),
    gamma_pos = gamma_pos,
    # mu_pos is gamma_k's INTERCEPT column in both cases: the coefficient at
    # z = 0, which is the population mean only when M = 0 or the moderators
    # are centered.
    mu_pos = stats::setNames(
      if (!length(K)) integer(0) else length(S) + (seq_along(K) - 1L) * width + 1L,
      nm[K]),
    zeta_pos = stats::setNames(off + length(Fc) + seq_along(K), nm[K])
  )
}

#' The group's own map: its T_list entry, or the common T
#'
#' @noRd
.mgc_group_T <- function(map, g) {
  if (is.null(map$T_list)) map$T else map$T_list[[g]]
}

#' Carry one group's data summaries into the expanded parameterization
#'
#' @noRd
.mgc_expand_stats <- function(st, map, g = 1L) {
  Tm <- .mgc_group_T(map, g)
  Q <- t(Tm) %*% st$Q %*% Tm
  list(Q = (Q + t(Q)) / 2, l = stats::setNames(c(t(Tm) %*% st$l), colnames(Tm)), ctx = st$ctx)
}
