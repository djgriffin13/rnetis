# PORTABLE MULTI-GROUP CORE (mgc) -- the canonical draw representation.
# See mgc-regime.R for what this core may depend on.
#
# ONE SELF-DESCRIBING SHAPE FOR A MULTI-GROUP FIT. Plain lists and base
# arrays, no classes, so any consumer can read it and two fits can be
# compared field by field. S is the number of retained draws (all chains stacked), G the number
# of groups, p the number of coefficients.
#
#   version            "mgc-draws/1"
#   family, R          outcome family and multiplicative dimension
#   groups             character G: group IDs, in the order of every G axis
#   coef_names         character p: coefficient names, in design-column order
#   regime             named character p: "group" / "shared" / "random"
#   n_actors           named integer G
#   chain              integer S: which chain each draw is from
#
#   theta              S x G x p   group-specific coefficients beta[g, k].
#                                  Shared columns repeat the shared value in
#                                  every group, so theta is always the full
#                                  coefficient vector each group's likelihood
#                                  used.
#   mu                 S x p       population level: the shared value for a
#                                  "shared" column, the population mean for a
#                                  "random" column, NA for a "group" column
#                                  (no population quantity exists).
#   tau                S x p       between-group SD for "random" columns; NA
#                                  elsewhere.
#   Omega              S x q x q   between-group correlations of the random
#                                  coefficients under an unstructured
#                                  covariance; NULL under a diagonal one.
#   gamma              S x (q(1+M)) moderator coefficients, columns
#                                  gamma:<coef>:<moderator>; NULL without
#                                  moderators.
#   acceptance         Metropolis acceptance (unstructured covariance only):
#                      retained-phase rates, `warmup` rates, and the
#                      proposal `steps` each chain used after burn-in.
#   tie_variation      character G: NA, or "all0"/"all1" for a binary group
#                      admitted without tie variation.
#   starts             character, one per chain: "amen" or "dispersed".
#   ame                list of S x G matrices, WITHIN-NETWORK structure:
#                      va, cab, vb, rho, s2 (s2 = 1 under the probit), and
#                      uv_realized (variance of u_i'v_j over the group's
#                      observed dyads; 0 when R = 0)
#   random_data_precision
#                      S x G x p   the data precision Q_g[k, k] of each random
#                                  coefficient at that draw (NA elsewhere) --
#                                  what shrinkage weights are computed from.
#   posterior_means    list over groups: a, b (length n); uv (n x n)
#   priors, control    what the fit was run with
#
# The hyperparameters of the between-group distribution (mu, tau, Omega
# and gamma) and the within-network AME quantities (ame) are separate fields
# on purpose: they answer different questions and must never be reported in
# one table.

#' @noRd
.MGC_DRAWS_VERSION <- "mgc-draws/1"

#' Check that an object has the canonical draw shape
#'
#' @param x a candidate draws object.
#' @return `TRUE` invisibly; errors naming the first violation.
#' @noRd
.mgc_validate_draws <- function(x) {
  need <- c("version", "family", "R", "groups", "coef_names", "regime",
            "chain", "theta", "mu", "tau", "ame")
  miss <- setdiff(need, names(x))
  if (length(miss)) stop(sprintf("draws missing field(s): %s", paste(miss, collapse = ", ")), call. = FALSE)
  if (!identical(x$version, .MGC_DRAWS_VERSION)) stop("unknown draws version", call. = FALSE)
  S <- length(x$chain)
  G <- length(x$groups)
  p <- length(x$coef_names)
  if (!identical(dim(x$theta), c(S, G, p))) stop("theta must be S x G x p", call. = FALSE)
  if (!identical(dim(x$mu), c(S, p)) || !identical(dim(x$tau), c(S, p))) {
    stop("mu and tau must be S x p", call. = FALSE)
  }
  if (!identical(names(x$regime), x$coef_names)) stop("regime must be named by coef_names", call. = FALSE)
  for (nm in names(x$ame)) {
    if (!identical(dim(x$ame[[nm]]), c(S, G))) stop(sprintf("ame$%s must be S x G", nm), call. = FALSE)
  }
  # EVERY PER-DRAW FIELD HAS S ROWS -- the optional per-draw fields as well
  # as the required ones. This guards against a combiner that starts from
  # runs[[1]] and updates named fields: every field it did not name (gamma,
  # Omega) would then describe the first chain only, silently, beside every
  # chain of everything else.
  for (nm in c("gamma", "Omega")) {
    v <- x[[nm]]
    if (!is.null(v) && !identical(dim(v)[1], S)) {
      stop(sprintf("%s has %d draw(s) for %d retained draw(s). A per-draw field that disagrees with the chain vector means some draws were dropped or a chain was not combined -- the values are real and the shape is wrong, which no downstream diagnostic reports as an error.",
                   nm, dim(v)[1], S), call. = FALSE)
    }
  }
  # Row order carries group identity: theta's rownames are reassigned
  # unconditionally by the sampler, so nothing but this check stands between a
  # reordered prep list and group 2's prior being applied to group 1's data --
  # relabeled correctly, and silently.
  if (!is.null(dimnames(x$theta)[[2]]) && !identical(dimnames(x$theta)[[2]], x$groups)) {
    stop("theta's group margin is not in the groups' own order", call. = FALSE)
  }
  shared <- x$regime == "shared"
  if (any(shared)) {
    spread <- apply(x$theta[, , shared, drop = FALSE], c(1, 3), function(v) diff(range(v)))
    if (any(spread > 0)) stop("a shared coefficient differs across groups within a draw", call. = FALSE)
  }
  invisible(TRUE)
}

#' Stack several chains' draws into one object
#'
#' @param runs a list of draws objects from single chains of the same model.
#' @noRd
.mgc_combine_chains <- function(runs) {
  if (length(runs) == 1L) {
    return(runs[[1]])
  }
  bind3 <- function(field) {
    arrs <- lapply(runs, `[[`, field)
    out <- array(NA_real_, c(sum(vapply(arrs, function(a) dim(a)[1], 1)), dim(arrs[[1]])[-1]),
                 dimnames = c(list(NULL), dimnames(arrs[[1]])[-1]))
    at <- 0L
    for (a in arrs) {
      idx <- at + seq_len(dim(a)[1])
      out[idx, , ] <- a
      at <- max(idx)
    }
    out
  }
  out <- runs[[1]]
  out$chain <- unlist(lapply(runs, `[[`, "chain"))
  out$theta <- bind3("theta")
  out$random_data_precision <- bind3("random_data_precision")
  out$mu <- do.call(rbind, lapply(runs, `[[`, "mu"))
  out$tau <- do.call(rbind, lapply(runs, `[[`, "tau"))
  if (!is.null(out$gamma)) {
    out$gamma <- do.call(rbind, lapply(runs, `[[`, "gamma"))
  }
  # OMEGA MUST BE COMBINED LIKE EVERYTHING ELSE. Left uncombined, a
  # multi-chain fit would return chain 1's correlations beside every chain
  # of every other parameter, and a convergence diagnostic would then split
  # one chain's draws by the full chain vector and return NA or nonsense.
  if (!is.null(out$Omega)) {
    oms <- lapply(runs, `[[`, "Omega")
    q <- dim(oms[[1]])[2]
    out$Omega <- array(NA_real_, c(sum(vapply(oms, function(a) dim(a)[1], 1L)), q, q),
                       dimnames = dimnames(oms[[1]]))
    at <- 0L
    for (a in oms) {
      idx <- at + seq_len(dim(a)[1])
      out$Omega[idx, , ] <- a
      at <- max(idx)
    }
  }
  # ACCEPTANCE ACROSS ALL CHAINS, not chain 1's. runs[[1]] seeds `out`, so
  # every unreduced field silently describes the first chain only -- and an
  # acceptance rate is exactly the kind of number a reader assumes covers
  # the whole fit.
  if (!is.null(out$acceptance)) {
    accs <- lapply(runs, `[[`, "acceptance")
    pool <- function(phase) {
      parts <- lapply(accs, function(a) if (identical(phase, "retained")) a else a$warmup)
      sw <- vapply(parts, function(a) .mgc_or(a$sweeps, 0), numeric(1))
      rate <- function(nm) {
        v <- vapply(parts, function(a) .mgc_or(a[[nm]], NA_real_), numeric(1))
        if (sum(sw) > 0) sum(v * sw, na.rm = TRUE) / sum(sw) else NA_real_
      }
      list(omega = rate("omega"), tau = rate("tau"), sweeps = sum(sw))
    }
    retained <- pool("retained")
    out$acceptance <- c(
      retained,
      list(warmup = pool("warmup"), chains = length(runs),
           # The scales are per chain: each chain adapts its own during burn-in.
           steps = list(
             tau = vapply(accs, function(a) .mgc_or(a$steps$tau, NA_real_), numeric(1)),
             omega = vapply(accs, function(a) .mgc_or(a$steps$omega, NA_real_), numeric(1)),
             adapted = isTRUE(accs[[1]]$steps$adapted),
             target_tau = .mgc_or(accs[[1]]$steps$target_tau, NA_real_),
             target_omega = .mgc_or(accs[[1]]$steps$target_omega, NA_real_))))
  }
  out$starts <- vapply(runs, function(r) .mgc_or(r$starts, NA_character_), character(1))
  out$ame <- lapply(stats::setNames(names(out$ame), names(out$ame)), function(nm) {
    do.call(rbind, lapply(runs, function(r) r$ame[[nm]]))
  })
  # Posterior means over every retained draw of every chain: each chain's mean
  # weighted by its number of retained draws.
  n_per <- vapply(runs, function(r) length(r$chain), numeric(1))
  w <- n_per / sum(n_per)
  gids <- names(out$posterior_means)
  out$posterior_means <- lapply(stats::setNames(gids, gids), function(g) {
    fields <- names(runs[[1]]$posterior_means[[g]])
    lapply(stats::setNames(fields, fields), function(f) {
      Reduce(`+`, Map(function(r, wk) wk * r$posterior_means[[g]][[f]], runs, w))
    })
  })
  out$control$chains <- length(runs)
  out
}
