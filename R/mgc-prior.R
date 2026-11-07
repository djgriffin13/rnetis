# Parts of this file derive from the amen package (Hoff, Fosdick, Volfovsky and
# He; GPL-3), modified in 2026 for the hierarchical multi-group model. See
# inst/COPYRIGHTS.
# PORTABLE MULTI-GROUP CORE (mgc) -- priors.
# See mgc-regime.R for what this core may depend on.
#
# THREE KINDS OF PRIOR, kept apart because they answer different questions.
#
# 1. AME NUISANCE PRIORS (Sab, s2, rho, U/V), one list per group. The default
#    reproduces `amen::ame()`'s own construction for that group's data, so that
#    a no-pooling fit targets exactly the posterior amen targets. The
#    nuisance structure is group-specific in every regime, so its prior is too.
#
# 2. COEFFICIENT PRIORS for "group" and "shared" columns: amen's g-prior. For a
#    set of columns J the prior is the MARGINAL of the g-prior on J, i.e.
#    precision solve(V0[J, J]) with V0 the inverse of amen's iV0. For J = all
#    columns of one group this is amen's prior exactly (no pooling reproduces
#    amen). For shared columns the g-prior is built from the POOLED design --
#    pooled X'X, pooled g, and pooled cell count -- which, for J = every column,
#    is exactly the g-prior of a completely pooled specification. It is added
#    ONCE for the shared block.
#
# 3. BETWEEN-GROUP PRIORS for a random coefficient: mu ~ N(0, mu_sd^2) and a
#    half-t(nu, A) prior on the between-group SD tau.
#
#    THE SCALE A. A between-group SD is in the coefficient's own units, which
#    are outcome units per predictor unit. Rescaling a predictor x -> c x
#    rescales its coefficient beta -> beta / c, so tau -> tau / |c|, and a
#    scale-equivariant prior must satisfy A -> A / |c|. Setting
#    A = A0 * outcome_scale / sd(x) does exactly that (.mgc_tau_prior_scale).
#    For the intercept the predictor is the constant 1 and A = A0 *
#    outcome_scale. outcome_scale is 1 under the probit (the latent residual
#    SD is fixed at 1) and the SD of the observed ties under family = "nrm".
#
#    Defaults, stated so they can be printed: nu = 3, A0 = 0.5 (the same
#    default selection_pooling() passes, with 0.25 and 1 as the documented
#    sensitivity settings), mu_sd = 10 * outcome_scale. For the
#    intercept on the latent probit scale, half-t(3, 0.5) puts about 80% of its
#    mass on between-group SDs below 0.82 and about 90% below 1.18 -- weakly
#    informative for how far group baseline tie propensities can differ,
#    without the near-zero spike of an inverse-gamma(0.001, 0.001) that is
#    known to dominate at small G (Gelman 2006). The scale mixture used to
#    sample it is in mgc-hyper.R.

#' amen's own nuisance prior for one group's data
#'
#' Reproduces the prior block at the top of `amen::ame()` (amen 1.4.5).
#'
#' @param Y the group's sociomatrix (diagonal NA; 0/1 for `bin`).
#' @param family `"bin"` or `"nrm"`.
#' @param R the multiplicative dimension.
#' A BINARY GROUP WITHOUT TIE VARIATION has no data-derived prior: amen's
#' construction takes the probit of the observed tie rate (infinite at 0 or 1)
#' and scales by the spread of the rows' and columns' tie rates (zero), giving
#' `Sab0 = 0` and `eta0 = Inf`, under which the inverse-Wishart update returns
#' `NaN`. `amen::ame()` never meets this case because it cannot fit such a
#' network at all. For such a group the data-derived entries are left absent,
#' so amen's own fixed function defaults apply (`Sab0 = I`, `eta0 = 4`, and the
#' latent-factor equivalents), and `source` records that it happened. Only `g`,
#' the number of observed dyads, is still taken from the data.
#'
#' @return a list with any of `g`, `Sab0`, `eta0`, `Suv0`, `kappa0` amen would
#'   set; entries amen leaves `NULL` are absent, so amen's function defaults
#'   apply downstream exactly as they do inside `ame()`. `source` names which
#'   construction was used.
#' @noRd
.mgc_amen_nuisance_prior <- function(Y, family, R = 0L) {
  prior <- list()
  if (identical(family, "nrm")) {
    prior$g <- sum(!is.na(Y)) * stats::var(c(Y), na.rm = TRUE)
    prior$source <- "amen data-derived"
    return(prior)
  }
  obs <- Y[!is.na(Y)]
  if (length(obs) && (all(obs == 0) || all(obs == 1))) {
    prior$g <- length(obs)
    prior$source <- "amen fixed defaults (no tie variation)"
    return(prior)
  }
  ydist <- table(Y)
  ymode <- as.numeric(names(ydist)[ydist == max(ydist)])[1]
  YB <- 1 * (Y != ymode)
  ybar <- mean(YB, na.rm = TRUE)
  mu <- stats::qnorm(ybar)
  E <- (YB - ybar) / stats::dnorm(stats::qnorm(ybar))
  diag(E) <- 0
  a <- rowMeans(E, na.rm = TRUE); a[is.na(a)] <- 0
  b <- colMeans(E, na.rm = TRUE); b[is.na(b)] <- 0
  vscale <- mean(diag(stats::cov(cbind(a, b))))
  PHAT <- stats::pnorm(mu + outer(a, b, "+"))
  vdfmlt <- 0.25 / mean(PHAT * (1 - PHAT))
  prior$Sab0 <- diag(2) * vscale
  prior$Suv0 <- diag(2 * R) * vscale
  prior$eta0 <- round(4 * vdfmlt)
  prior$kappa0 <- round((2 * R + 2) * vdfmlt)
  prior$g <- sum(!is.na(Y))
  prior$source <- "amen data-derived"
  prior
}

#' amen's g-prior precision for a design
#'
#' The `is.null(iV0)` branch of `amen::rbeta_ab_fc()`, including its intercept
#' adjustment. With several groups, pass pooled `XX`, pooled `g`, and the total
#' cell count; with one group this is amen's expression exactly.
#'
#' @param XX a p x p cross-product of the design.
#' @param g the g-prior constant.
#' @param n_cells total design cells (`n^2` for one group).
#' @param intercept_first whether column 1 is the intercept.
#' @return the p x p prior precision.
#' @noRd
.mgc_gprior_precision <- function(XX, g, n_cells, intercept_first = TRUE) {
  iV0 <- XX / g + diag(diag(XX), nrow = nrow(XX)) / g^2
  if (intercept_first) {
    # RANK DEFICIENCY IS EXPECTED HERE, not exceptional. A covariate that does
    # not vary within a group -- the weak-information case a hierarchical model
    # exists to handle -- makes that group's cross-product singular, and amen's
    # expression inverts it twice. A pseudo-inverse keeps a flat direction flat
    # (zero data-derived precision, which is the truth: that group's data says
    # nothing about that column) instead of failing or inventing curvature.
    # Where the design has full rank this is exactly amen's expression.
    V0 <- .mgc_pinv(iV0)
    V0[1, 1] <- V0[1, 1] + sqrt(g) - g / n_cells
    iV0 <- .mgc_pinv(V0)
  }
  iV0
}

#' Moore-Penrose pseudo-inverse of a symmetric matrix
#'
#' @param A a symmetric matrix.
#' @param tol relative singular-value tolerance.
#' @noRd
.mgc_pinv <- function(A, tol = sqrt(.Machine$double.eps)) {
  s <- svd(A)
  keep <- s$d > tol * max(s$d, 0)
  if (!any(keep)) {
    return(matrix(0, nrow(A), ncol(A)))
  }
  s$v[, keep, drop = FALSE] %*% ((1 / s$d[keep]) * t(s$u[, keep, drop = FALSE]))
}

#' The marginal of a Gaussian prior on a subset of columns, as a precision
#'
#' A group's g-prior precision is singular when a column of its design is all
#' zero, which is admitted for a random coefficient (the group is flagged and
#' partially pooled). That column has no prior precision of its own and is not
#' among the columns this marginal is taken on, so the pseudo-inverse is used in
#' that case; at full rank this is the ordinary inverse.
#'
#' @noRd
.mgc_marginal_precision <- function(P, J) {
  if (length(J) == 0L) {
    return(matrix(0, 0, 0))
  }
  V <- if (qr(P)$rank < nrow(P)) .mgc_pinv(P) else solve(P)
  solve(V[J, J, drop = FALSE])
}

#' Scale of the half-t prior on a between-group SD
#'
#' @param outcome_scale 1 under the probit; the SD of observed ties for `nrm`.
#' @param predictor_sd the SD of the predictor whose coefficient varies (1 for
#'   the intercept).
#' @param A0 the scale in standardized units.
#' @return the half-t scale `A` in the coefficient's own units.
#' @noRd
.mgc_tau_prior_scale <- function(outcome_scale, predictor_sd = 1, A0 = 1) {
  A0 * outcome_scale / predictor_sd
}

#' The outcome scale that between-group priors are expressed in
#'
#' @param Y_list the groups' sociomatrices.
#' @param family `"bin"` or `"nrm"`.
#' @noRd
.mgc_outcome_scale <- function(Y_list, family) {
  if (identical(family, "bin")) {
    return(1)
  }
  stats::sd(unlist(lapply(Y_list, function(Y) Y[!is.na(Y)])))
}

#' Resolve every coefficient and between-group prior for a fit
#'
#' @param prep the prepared groups from `.mgc_prepare_groups()`.
#' @param regime a `.mgc_regime()` object.
#' @param family `"bin"` or `"nrm"`.
#' @param hyper a list of between-group prior settings (`nu`, `A0`,
#'   `predictor_sd`, `delta_Z`, `A0_Z`, `eta`).
#' @return a list with `shared` (P, m), `group_static` (per group, the
#'   precision and mean of the "group" columns), and `random` (the resolved
#'   between-group prior).
#' @noRd
.mgc_resolve_priors <- function(prep, regime, family, hyper = list()) {
  s <- regime$shared
  grp_cols <- which(regime$regime == "group")
  G <- length(prep)

  gpriors <- lapply(prep, function(gr) {
    .mgc_gprior_precision(attributes(gr$X)$XX, gr$nuisance_prior$g, gr$n^2)
  })

  shared <- NULL
  if (length(s)) {
    XX_pool <- Reduce(`+`, lapply(prep, function(gr) attributes(gr$X)$XX))
    g_pool <- sum(vapply(prep, function(gr) gr$nuisance_prior$g, numeric(1)))
    cells <- sum(vapply(prep, function(gr) gr$n^2, numeric(1)))
    P_pool <- .mgc_gprior_precision(XX_pool, g_pool, cells)
    shared <- list(
      P = .mgc_marginal_precision(P_pool, s),
      m = rep(0, length(s))
    )
  }

  group_static <- lapply(seq_len(G), function(g) {
    list(P = .mgc_marginal_precision(gpriors[[g]], grp_cols),
         m = rep(0, length(grp_cols)))
  })
  names(group_static) <- names(prep)

  random <- NULL
  if (length(regime$random)) {
    outcome_scale <- .mgc_outcome_scale(lapply(prep, `[[`, "Y"), family)
    nu <- .mgc_or(hyper$nu, 3)
    # 0.5, matching the public default, so a direct call to the core and a fit
    # through selection_pooling() carry the same prior.
    A0 <- .mgc_or(hyper$A0, 0.5)
    # The reference change the prior is expressed per, ONE PER RANDOM
    # COEFFICIENT. 1 is the intercept's predictor and a binary contrast; a
    # continuous predictor's is a within-group SD, supplied by the caller
    # because deciding it needs the design, not just the sociomatrices.
    # Recycled when a single value is given, so a
    # one-coefficient model is unchanged.
    predictor_sd <- rep(.mgc_or(hyper$predictor_sd, 1),
                        length.out = length(regime$random))
    random <- list(
      nu = nu,
      A0 = A0,
      predictor_sd = predictor_sd,
      A = .mgc_tau_prior_scale(outcome_scale, predictor_sd = predictor_sd, A0 = A0),
      mu_mean = 0,
      mu_sd = 10 * outcome_scale,
      # THE MODERATOR SLOPE'S SCALE, on the same contrast logic as
      # A but with a SECOND reference change. gamma_km is interpretable only as
      #
      #     gamma_km * delta_k * delta^Z_m
      #
      # -- the change in (the effect of a reference change in predictor k) per
      # reference change delta^Z_m in moderator m. So the prior SD in the
      # coefficient's own units is A0_Z * outcome_scale / (delta_k * delta^Z_m).
      #
      # delta^Z IS NOT delta. delta_k is a WITHIN-group reference change, and a
      # moderator is group-constant by definition, so it has no within-group
      # variation to supply one. Reusing delta_k here would put a prior on a
      # quantity that does not exist. delta^Z comes from the caller
      # (.selection_moderator_scale()), which computes it BETWEEN groups and
      # flags that it rests on G observations.
      #
      # A q x M MATRIX, and the shape matters. predictor_sd is indexed by
      # RANDOM COEFFICIENT (length q); delta_Z is indexed by MODERATOR (length
      # M). They are different axes, so multiplying the two vectors would
      # recycle one against the other and hand every coefficient a scale built
      # from the wrong moderator -- a real prior on the wrong parameter, with
      # nothing in the output to show it. The outer product is explicit.
      gamma_sd = if (is.null(hyper$delta_Z)) NULL else {
        A0_Z <- .mgc_or(hyper$A0_Z, 0.5)
        m <- outer(predictor_sd, as.numeric(hyper$delta_Z))   # q x M
        out <- A0_Z * outcome_scale / m
        dimnames(out) <- list(names(predictor_sd), names(hyper$delta_Z))
        out
      },
      A0_Z = .mgc_or(hyper$A0_Z, 0.5),
      delta_Z = hyper$delta_Z,
      # The LKJ shape on the between-group correlations (unstructured
      # covariance only). 1 is uniform over correlation matrices.
      eta = .mgc_or(hyper$eta, 1),
      outcome_scale = outcome_scale
    )
  }
  list(shared = shared, group_static = group_static, random = random)
}

#' Assemble one group's block prior for the kernel's expanded parameterization
#'
#' Block order is the "group" coefficients followed by the deviations zeta_k of
#' the random coefficients (`.mgc_expanded_map()`). A deviation's prior is
#' N(0, tau_k^2): its mean is zero because the population mean mu_k is a shared
#' parameter, not part of the block.
#'
#' @noRd
.mgc_block_prior <- function(static, regime, tau2 = NULL, Sigma = NULL) {
  q <- length(regime$random)
  f <- length(regime$block) - q
  if (length(regime$block) == 0L) {
    return(NULL)
  }
  P <- matrix(0, f + q, f + q)
  m <- numeric(f + q)
  if (f > 0L) {
    P[seq_len(f), seq_len(f)] <- static$P
    m[seq_len(f)] <- static$m
  }
  if (q > 0L) {
    # THE DEVIATION BLOCK'S PRIOR PRECISION. Diagonal when the between-group
    # covariance is diagonal, and the DENSE inverse of Sigma when it is not.
    # The kernel already treats this block as a dense matrix, so a correlated
    # covariance changes what is put here and nothing about the draw itself.
    P[f + seq_len(q), f + seq_len(q)] <- if (is.null(Sigma)) {
      diag(1 / tau2, nrow = q)
    } else {
      Si <- chol2inv(chol(Sigma))
      (Si + t(Si)) / 2
    }
  }
  list(P = P, m = m)
}

#' The shared prior in the expanded parameterization
#'
#' The "shared" coefficients' prior followed by N(mu_mean, mu_sd^2) for each
#' random coefficient's population mean, independent of the rest.
#'
#' @noRd
.mgc_expanded_shared_prior <- function(pri, regime, map = NULL) {
  q <- length(regime$random)
  s <- length(regime$shared)
  # WITH MODERATORS each random coefficient's gamma_k is (1 + M) wide, not 1,
  # so the prior block's size comes from the MAP rather than from a count of
  # random coefficients. Sized from q while the map is wider, the matrix would
  # be too small and the shared prior would land on the wrong columns.
  width <- if (is.null(map) || !length(map$gamma_pos)) 1L else length(map$gamma_pos[[1]])
  gw <- q * width
  if (s + gw == 0L) {
    return(NULL)
  }
  P <- matrix(0, s + gw, s + gw)
  m <- numeric(s + gw)
  if (s > 0L) {
    P[seq_len(s), seq_len(s)] <- pri$shared$P
    m[seq_len(s)] <- pri$shared$m
  }
  if (q > 0L) {
    # Column 1 of each gamma_k is the coefficient at z = 0 and keeps the
    # no-moderator `mu` prior exactly, so the M = 0 reduction is exact. The
    # moderator slopes take their own scale (gamma_sd), never mu's: they are
    # a different quantity in different units -- a change in the effect per
    # reference change in the moderator, not the effect itself.
    # gamma_sd is q x M: ROW j is random coefficient j's scale across the
    # moderators, so it is indexed by row and never recycled as a vector.
    gsd <- pri$random$gamma_sd
    for (j in seq_len(q)) {
      cols <- s + (j - 1L) * width + seq_len(width)
      # WIDTH FIRST. With no moderators there are no moderator slopes at all,
      # so gamma_sd is irrelevant even when a prior happens to carry one --
      # checking its shape before checking whether it is wanted refuses a
      # perfectly well-formed no-moderator call.
      row_sd <- if (width == 1L) {
        numeric(0)
      } else if (is.null(gsd)) {
        rep(pri$random$mu_sd, width - 1L)
      } else {
        stopifnot(nrow(gsd) >= j, ncol(gsd) == width - 1L)
        gsd[j, ]
      }
      sds <- c(pri$random$mu_sd, row_sd)
      P[cols, cols] <- diag(1 / sds^2, nrow = width)
      m[cols] <- c(pri$random$mu_mean, rep(0, width - 1L))
    }
  }
  list(P = P, m = m)
}
