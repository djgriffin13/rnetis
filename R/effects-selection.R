# Selection effect sizes.
#
# The default is a probability contrast, not a coefficient, because a probit
# coefficient answers no question an applied reader asked. "A one-unit
# increase in tenure corresponds to a 7.3 percentage-point increase in tie
# probability" is the sentence; the latent-scale coefficient is the
# intermediate quantity that produces it.
#
# THREE ESTIMANDS, and the package names which one it computed every time,
# because they differ materially at the group sizes this package targets:
#
#   reference    Phi(mu + x'beta) with actor effects at zero. A hypothetical
#                pair with exactly average everything. Rarely what is meant.
#   conditional  averaged over the observed dyads, using the fitted actor
#                effects. "For this network, as observed."
#   population   integrated over the DISTRIBUTION of actor effects. "For a
#                randomly drawn dyad from the modeled population."
#
# Because Phi is nonlinear, these are not rescalings of one another, and the
# marginal quantity is pulled toward 0.5 relative to the conditional one.
#
# WHAT THE ESTIMATOR ALLOWS. A fit keeps the additive and multiplicative actor
# effects only as posterior means, so the CONDITIONAL estimand
# is a plug-in: it propagates coefficient uncertainty with the actor effects
# held fixed at those means, and its interval is therefore narrower
# than one that integrated over them. The POPULATION estimand does integrate
# over the additive actor-effect distribution at each posterior draw, using
# that draw's own additive variances, and does so exactly: for o ~ N(0, v),
# E[Phi(eta + o)] = Phi(eta / sqrt(1 + v)) (derived on the help page below). No
# random numbers are drawn.
#
# The object carries a sentence saying which it computed and what the interval
# covers. That sentence exists so a narrower interval is never read as a full
# one -- the failure here is silent, because a plug-in interval looks exactly
# like a propagated one.

#' Average marginal probability contrast for a Selection term
#'
#' Reports the change in tie probability associated with a change in one
#' predictor, averaged over dyads and summarized across posterior draws.
#'
#' @section Which estimand:
#' `type = "conditional"` (default) averages over the dyads actually observed,
#' using the fitted actor effects: *for this network, as observed*.
#' `type = "population"` integrates over the distribution of actor effects:
#' *for a randomly drawn dyad from the modeled population*.
#' `type = "reference"` sets actor effects to zero. These are different
#' quantities, not rescalings, and the returned object records which was used.
#'
#' @section What the interval covers:
#' Intervals are posterior quantiles over the draws. For
#' `type = "conditional"` the actor effects are held at their posterior means,
#' because a fit keeps them only as posterior means: the interval
#' reflects uncertainty in the coefficients but **not** in the actor effects,
#' so it is **narrower than a fully propagated one**. `type = "population"`
#' does integrate over the additive actor-effect distribution. Under
#' `family = "nrm"` the contrast is exactly `delta` times the coefficient for every
#' `type`, with no actor effects involved. The object
#' carries a sentence stating what its interval covers; print it rather than
#' paraphrasing.
#'
#' @section How the population estimand is integrated:
#' Under the probit, a dyad's latent index is `eta + a_i + b_j + e_ij` with
#' `e_ij` of unit variance. For two actors drawn independently from the
#' population, `a_i + b_j ~ N(0, va + vb)` (no `cab` term: `cab` is a
#' within-actor covariance), and the population tie probability is the
#' standard closed form
#' \deqn{E[\Phi(\eta + o)] = \Phi\left(\eta / \sqrt{1 + v}\right), \quad o \sim N(0, v)}{E[Phi(eta + o)] = Phi(eta / sqrt(1 + v)), o ~ N(0, v)}
#' which follows in one line: for `Z ~ N(0, 1)` independent of `o`,
#' `Phi(eta + o) = P(Z <= eta + o | o)`, so the expectation is
#' `P(Z - o <= eta)`, and `Z - o ~ N(0, 1 + v)`. See Owen (1980) for tables of
#' integrals of this kind. Here `v = va + vb` (`2 va` on a symmetric fit), evaluated at every posterior
#' draw with that draw's own variances. The integral is exact, so the result is
#' deterministic and the caller's random-number stream is not touched. The
#' multiplicative term (`R > 0`) is not integrated over; the coverage sentence
#' says so.
#'
#' On a hierarchical multi-group fit (`pooling = selection_pooling()`) every
#' estimand is computed for the named group from that group's own coefficient
#' draws and additive variance draws.
#'
#' @section Pooled multi-group fits:
#' Two-stage pooling (`pool_beta = TRUE`) produces a pooled mean and variance
#' per coefficient and no pooled draws. A contrast is therefore computed from
#' the named group's **own** coefficient draws, before pooling, with that
#' group's actor effects. It can differ from the pooled coefficient, even in
#' sign, and the coverage sentence says so.
#'
#' @references
#' Owen, D. B. (1980). A table of normal integrals. *Communications in
#' Statistics -- Simulation and Computation*, 9(4), 389--419.
#'
#' @param object a [fit_selection()] fit.
#' @param term the design column to move, e.g. `"snd_tenure"` or
#'   `"prior_tie"`.
#' @param delta the change in that predictor. Default `1`. By default the
#'   predictor moves from each dyad's observed value `x` to `x + delta`. When
#'   every observed value is 0 or 1 (`prior_tie`, an indicator covariate) and
#'   `delta = 1`, the contrast is presence against absence instead: 0 to 1 at
#'   every dyad, including dyads observed at 1. The returned `contrast` field
#'   says which was computed.
#' @param type which estimand; see **Which estimand**.
#' @param level credible-interval mass. Default `0.95`.
#' @param group which group to compute for. Required when the fit covers
#'   several.
#' @param x a `netis_selection_effect` object, for the `print()` method.
#' @param ... unused; present for S3 consistency with `print()`.
#' @return an object of class `netis_selection_effect`: a list with `term`,
#'   `delta`, `type`, `group`, `family`, `contrast`, `estimate` (the posterior
#'   mean), `interval`, `level`, `n_dyads`, `draws` (the contrast at each posterior
#'   draw), `coverage` (the sentence above) and `scale`.
#' @seealso [fit_selection()], [hierarchical_selection]
#' @examples
#' # Cost: an amen MCMC fit, over CRAN's example budget at any usable chain length.
#' \donttest{
#' set.seed(1)
#' ids <- sprintf("n%02d", 1:10)
#' nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = 10),
#'                     x = stats::rnorm(20))
#' pairs <- subset(expand.grid(source = ids, target = ids,
#'                             stringsAsFactors = FALSE), source != target)
#' ties <- rbind(
#'   data.frame(group = 1, pairs, wave = 1,
#'              tie = stats::rbinom(nrow(pairs), 1, 0.25)),
#'   data.frame(group = 1, pairs, wave = 2,
#'              tie = stats::rbinom(nrow(pairs), 1, 0.25))
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave",
#'                  tie_meta = list(tie = list(zero_meaning = "measured_zero")))
#' fit <- fit_selection(tie ~ sender(x) + prior_tie(), data = nd,
#'                      nscan = 200, burn = 50, odens = 10)
#' selection_effect(fit, "prior_tie")
#' }
#' @export
selection_effect <- function(object, term, delta = 1,
                             type = c("conditional", "population", "reference"),
                             level = 0.95, group = NULL) {
  if (!inherits(object, "netis_selection")) {
    stop("selection_effect() needs a fit_selection() result.", call. = FALSE)
  }
  type <- match.arg(type)
  g <- .resolve_effect_group(object, group)
  extract <- object$per_group[[g]]
  design <- object$designs[[g]]

  draws <- extract$draws
  if (!term %in% colnames(draws)) {
    stop(
      sprintf(
        "'%s' is not a term in this fit. Available: %s.",
        term, paste(setdiff(colnames(draws), "intercept"), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  X <- .effect_design_matrix(design, colnames(draws))
  keep <- !is.na(as.vector(design$Y))
  if (!any(keep)) {
    stop("No observed dyads remain, so no contrast can be computed.", call. = FALSE)
  }

  offset <- .effect_actor_offset(extract, design, type)
  # A binary predictor's unit contrast is presence against absence at every
  # dyad. Adding 1 to the observed value would move dyads already at 1 to 2.
  zero_to_one <- identical(as.numeric(delta), 1) && all(X[keep, term] %in% c(0, 1))
  contrasts <- .effect_draw_contrasts(
    draws, X, keep, term, delta, offset, extract, object$family, zero_to_one
  )

  alpha <- (1 - level) / 2
  structure(
    list(
      term = term, delta = delta, type = type, group = g,
      family = object$family,
      contrast = if (zero_to_one) "0 to 1 at every dyad" else sprintf("observed value + %s", format(delta)),
      estimate = mean(contrasts),
      interval = unname(stats::quantile(contrasts, c(alpha, 1 - alpha))),
      level = level,
      n_dyads = sum(keep),
      draws = contrasts,
      coverage = .effect_coverage_note(
        type, object$family,
        # `extract$multiplicative` (UVPM) is a matrix even at R = 0 -- U/V
        # are n-by-0 there, and their product is an n x n zero matrix, not
        # NULL -- so a not-null check would wrongly attach the R > 0 caveat
        # to every fit. object$R is the real signal.
        has_multiplicative = object$R > 0,
        pooled = .selection_is_pooled(object)
      ),
      scale = if (identical(object$family, "bin")) "probability" else "outcome value"
    ),
    class = "netis_selection_effect"
  )
}

#' @noRd
.resolve_effect_group <- function(object, group) {
  # CHECKED BEFORE the single-group shortcut, not after. The shortcut returns
  # group 1 for ANY value of `group`, so on a one-group fit a mistyped
  # name would silently produce a result about a group the caller had not asked for
  # -- the failure mode is that nothing looks wrong. A name that does not exist
  # is an error whatever the group count.
  if (!is.null(group) && !as.character(group) %in% object$groups) {
    stop(sprintf("Group '%s' is not in this fit. Available: %s.",
                 group, paste(object$groups, collapse = ", ")), call. = FALSE)
  }
  if (object$n_groups == 1L) {
    return(object$groups[[1]])
  }
  if (is.null(group)) {
    # Both routes need a named group, for different reasons, and saying the
    # wrong one is worse than saying nothing. Unpooled: there is no joint model
    # at all. Pooled: no pooled draw set exists, so a contrast reads one group's
    # own draws. Averaging across groups would need a choice of weights and of
    # population that rnetis does not make for the caller.
    pooled <- .selection_is_pooled(object)
    stop(
      sprintf(
        "This fit covers %d groups %s, so a contrast must name one: group = \"%s\", for example. %s",
        object$n_groups,
        if (pooled) "with pooled coefficients" else "estimated independently",
        object$groups[[1]],
        if (pooled) {
          "Pooling produces a pooled mean and variance per coefficient and no pooled draws, so a contrast is computed from one group's own coefficient draws and actor effects, and can differ from the pooled coefficient."
        } else {
          "Averaging across them would imply a joint model that was not fitted."
        }
      ),
      call. = FALSE
    )
  }
  as.character(group)
}

#' Flatten the fitted design into a dyad-by-parameter matrix
#'
#' Column order follows the coefficient names, so a draw's coefficient vector
#' multiplies the right columns. The intercept is a column of ones rather than
#' a special case, which keeps the contrast arithmetic uniform.
#'
#' @noRd
.effect_design_matrix <- function(design, coef_names) {
  n <- nrow(design$Y)
  X <- matrix(0, n * n, length(coef_names), dimnames = list(NULL, coef_names))
  X[, "intercept"] <- 1

  fill_node <- function(block, margin) {
    if (is.null(block)) {
      return(invisible(NULL))
    }
    for (nm in colnames(block)) {
      v <- block[, nm]
      # A node covariate varies along one margin of the dyad matrix. A symmetric
      # fit's nodal covariate enters BOTH: the sampler fitted it on both margins
      # and the reported coefficient is the average of the two, so the
      # model-consistent column is x_i + x_j.
      m <- switch(
        margin,
        row = outer(v, rep(1, n)),
        col = outer(rep(1, n), v),
        both = outer(v, v, "+")
      )
      X[, nm] <<- as.vector(m)
    }
  }
  fill_node(design$Xrow, if (isTRUE(design$symmetric)) "both" else "row")
  fill_node(design$Xcol, "col")

  if (!is.null(design$Xdyad)) {
    dn <- dimnames(design$Xdyad)[[3]]
    for (i in seq_along(dn)) {
      X[, dn[i]] <- as.vector(design$Xdyad[, , i])
    }
  }
  X
}

#' The actor-effect offset each estimand uses
#'
#' Returns a fixed per-cell offset vector, or `NULL` for the population
#' estimand, which integrates over the offset at each posterior draw instead.
#'
#' @noRd
.effect_actor_offset <- function(extract, design, type) {
  n <- nrow(design$Y)

  if (identical(type, "reference")) {
    return(rep(0, n * n))
  }
  if (identical(type, "population")) {
    return(NULL)   # integrated per posterior draw instead
  }
  # PLUG-IN, and the coverage note says so. A fit keeps the actor effects only
  # as posterior means, so the conditional estimand holds them fixed
  # at those means rather than integrating over them. That understates the
  # interval, which is why the object states it rather than leaving a reader
  # to assume the actor effects were marginalized.
  a <- extract$additive_effects$sender
  b <- extract$additive_effects$receiver
  uv <- extract$multiplicative
  base <- outer(a, b, "+")
  if (!is.null(uv)) base <- base + uv
  as.vector(base)
}

#' Contrast at each posterior draw
#'
#' @noRd
.effect_draw_contrasts <- function(draws, X, keep, term, delta, offset,
                                   extract, family, zero_to_one = FALSE) {
  n_draws <- nrow(draws)
  Xk <- X[keep, , drop = FALSE]
  off <- if (is.null(offset)) NULL else offset[keep]

  # A valued outcome is linear in the coefficient, so the contrast is exactly
  # delta * beta and averaging over dyads adds nothing. Computed directly
  # rather than through the probability path, which would be a no-op wearing
  # a heavier disguise.
  if (!identical(family, "bin")) {
    return(draws[, term] * delta)
  }

  vc <- extract$variance_components
  if (is.null(off) && (is.null(vc) || nrow(vc) != n_draws)) {
    stop("Internal error: the population estimand needs one row of additive variance components per coefficient draw, and this fit does not carry them.",
         call. = FALSE)
  }
  out <- numeric(n_draws)
  for (s in seq_len(n_draws)) {
    beta <- draws[s, ]
    eta0 <- as.vector(Xk %*% beta)
    if (zero_to_one) eta0 <- eta0 - Xk[, term] * beta[[term]]
    eta1 <- eta0 + delta * beta[[term]]
    if (!is.null(off)) {
      out[s] <- mean(stats::pnorm(eta1 + off) - stats::pnorm(eta0 + off))
    } else {
      # Population estimand, integrated exactly over this draw's additive
      # actor-effect distribution: E[Phi(eta + o)] = Phi(eta / sqrt(1 + v)).
      scale <- sqrt(1 + .population_offset_variance(vc[s, ]))
      out[s] <- mean(stats::pnorm(eta1 / scale) - stats::pnorm(eta0 / scale))
    }
  }
  out
}

#' The variance of a population dyad's additive offset at one draw
#'
#' A population dyad is (sender `i`, receiver `j`) for two actors drawn
#' independently from the population, `i != j`. `cab = Cov(a_i, b_i)` is a
#' *within-actor* covariance -- how one person's own sending and receiving
#' tendencies co-vary -- and has no bearing on two different actors' effects.
#' A real dyad's offset is therefore `a_i + b_j` with `a_i` and `b_j`
#' independent (different actors, no shared randomness), giving variance
#' `va + vb` with no cross term. `va + vb + 2*cab` is the formula for
#' `Var(a_i + b_i)` -- the same actor sending to themselves -- which is not the
#' dyad this estimand describes; using it would bias every
#' `type = "population"` interval by however far `cab` is from zero.
#'
#' @noRd
.population_offset_variance <- function(vc_row) {
  va <- vc_row[["va"]]
  # A symmetric fit has one additive effect and its VC block has no vb: the
  # dyad's offset is a_i + a_j over two independent actors, so the second
  # variance is va again -- Var(a_i + a_j) = 2 * va, the exact analogue of the
  # directed va + vb.
  vb <- if ("vb" %in% names(vc_row)) vc_row[["vb"]] else va
  max(va + vb, 0)
}

#' The sentence that must accompany the interval
#'
#' The sentence is chosen by what was actually computed.
#'
#' A valued (`nrm`) outcome takes the direct linear path in
#' `.effect_draw_contrasts()` regardless of `type` -- no actor-effect offset
#' is ever computed, so the object still records the requested `type` (a
#' user genuinely asked for one) but the note must say plainly that none of
#' the `type`-specific machinery ran, rather than describing marginalization
#' or a plug-in that never happened.
#'
#' `type = "population"` with `R > 0` is real but incomplete: the additive
#' part is genuinely marginalized (integrated over each draw's `va`/`vb`),
#' but the multiplicative term `u_i'v_j` is not -- there is no per-draw
#' `Suv` to draw a population latent position from, only the fitted
#' actors' own `U`/`V`, which describe *these* actors, not a random pair from
#' the population. The note says the interval still understates uncertainty in
#' that case.
#'
#' @noRd
.effect_coverage_note <- function(type, family = "bin",
                                  has_multiplicative = FALSE, pooled = FALSE) {
  pooled_note <- if (pooled) {
    paste(
      "The coefficient draws are this group's own, from before pooling: a pooled",
      "fit has a pooled mean and variance per coefficient but no pooled draws, so",
      "this contrast can differ from the pooled coefficient, even in sign."
    )
  }
  if (!identical(family, "bin")) {
    return(paste(c(
      "The outcome is on its own (valued) scale, so the contrast is exactly",
      "delta times the coefficient and no actor-effect offset is computed for",
      "any type -- this interval reflects uncertainty in the coefficient alone.",
      pooled_note
    ), collapse = " "))
  }
  note <- switch(
    type,
    population = paste(
      "Additive sender and receiver effects were integrated over their",
      "estimated distribution at each posterior draw (exactly, in closed form),",
      "so this interval reflects uncertainty in the coefficients and the",
      "additive actor-effect variances.",
      if (has_multiplicative) {
        paste(
          "This fit also has a multiplicative (R > 0) term; its population",
          "variance is not integrated over (no per-draw distribution for a new",
          "actor's latent position is available), so the interval still",
          "understates uncertainty for this fit."
        )
      }
    ),
    conditional = {
      paste(
        "Actor effects are held at their posterior means rather than integrated",
        "over, because the fit keeps them only as posterior means. This interval",
        "reflects uncertainty in the coefficients only and is narrower than a",
        "fully propagated one."
      )
    },
    reference = "Actor effects are set to zero, so this is a hypothetical pair with exactly average sender and receiver tendencies, not an average over actual dyads."
  )
  paste(c(note, pooled_note), collapse = " ")
}

#' @rdname selection_effect
#' @export
print.netis_selection_effect <- function(x, ...) {
  cat("Average marginal ", x$scale, " contrast\n\n", sep = "")
  cat("Term:      ", x$term, " (", x$contrast, ")\n", sep = "")
  cat("Estimand:  ", switch(
    x$type,
    conditional = "conditional on the observed network",
    population  = "population-averaged over actor effects",
    reference   = "reference dyad, actor effects at zero"
  ), "\n", sep = "")
  cat("Group:     ", x$group, "\n", sep = "")
  cat("Dyads:     ", x$n_dyads, "\n\n", sep = "")

  if (identical(x$family, "bin")) {
    cat(sprintf("  %+.1f percentage points  [%.1f, %.1f] (%.0f%%)\n",
                100 * x$estimate, 100 * x$interval[1], 100 * x$interval[2],
                100 * x$level))
  } else {
    cat(sprintf("  %+.4f  [%.4f, %.4f] (%.0f%%)\n",
                x$estimate, x$interval[1], x$interval[2], 100 * x$level))
  }

  cat("\n", x$coverage, "\n", sep = "")
  invisible(x)
}
