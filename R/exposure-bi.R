# The Frank-Fahrbach balance-information (B-I) exposure mechanism.
#
# SOURCE. Frank, K. A. and Fahrbach, K. (1999) "Organization Culture as a
# Complex System: Balance and Information in Models of Influence and Selection",
# Organization Science 10(3), 253-277, TOGETHER WITH the authors' erratum,
# Organization Science 10(4), 514-515, which reformats equations (5) (printed
# p. 259; erratum p. 514), (6) (p. 260; pp. 514-515) and (8) (p. 260; p. 515).
# This file reads the erratum wherever the two differ, and (6) and (8) are exactly
# the equations implemented here. Equations (9), (10) and (11) are on p. 261.
# The erratum's running title has "Organizational"; the article's has
# "Organization".
#
# THE PARAMETERS ARE THE PAPER'S, ALL THREE OF THEM. The model carries two
# structural parameters and one mixing parameter:
#
#   alpha  the interpersonal transmission factor, applied at every step of a
#          path that passes between two different actors;
#   gamma  the self-retention factor, applied at every step in which an actor
#          holds information from one occasion to the next;
#   delta  the weight on non-maximal (redundant) paths relative to the maximal
#          informational path, equation (11).
#
# Frank & Fahrbach fold the first two into one symbol -- "let lambda_i' = gamma
# if i' = i, and lambda_i' = alpha if i' != i" (p. 259) -- purely to write the
# path expansions compactly. lambda is notation, not a third parameter, and
# there is no depth-damping parameter anywhere in the published equations.
#
# The expansions print lambda_i' on every factor. This file reads the subscript
# per step -- gamma where a step stays with one actor, alpha where it moves
# between two -- which is what Figure 3 (p. 261) shows on the two routes of a
# redundant path.
#
# THE DIAGONAL, AND WHAT THE SOURCE ACTUALLY SAYS. Equation (5), as corrected,
# reads
#
#   y_it = sum_{i'=1}^{N} lambda_i' k_{ii',t-1->t} y_{i',t-1} + e_it
#
# with the index running over ALL N actors, i' = i included, so paths do include
# self-retention steps. For that to reduce to the paper's Model (2) -- alpha
# times the sum over i' != i, plus gamma y_i -- the self position is effectively
# the identity, weighted by gamma; Model (1) (p. 257) says the influence of an
# actor upon itself is captured through k_ii. The internal transition matrix
# therefore has a gamma diagonal:
#
#   K'[i, i] = gamma,   K'[i, j] = alpha * k[i, j]
#
# BE PRECISE ABOUT WHOSE CONVENTION THIS IS. Frank & Fahrbach do not define
# k_ii = 0; Models (2) and (3) put gamma on the self position, which in (5) acts
# as the identity. What is rnetis's choice is the INPUT REPRESENTATION: `bi()` takes the
# interpersonal conduit's off-diagonal, and self-retention is supplied
# separately as `gamma`. Given that split, a user-supplied self-loop would be a
# second, redundant representation of the same quantity -- the internal diagonal
# would become gamma + alpha * k_ii -- so self-loops are refused to prevent
# self-retention being represented twice.
#
# The refusal rather than a silent hollowing matters because zeroing would
# change the user's data, and because no hollow-conduit fixture could catch the
# double count: every such fixture agrees with the correct implementation.
#
# WHY ar() IS NOT gamma. Equation (6)'s intermediate indices each run over all N,
# so a path may pass through a = i and be weighted gamma at that interior step:
# an actor receives, holds, and transmits later. rnetis's ar() is an additive
# lagged-outcome term applied once at the outermost level of the regression. It
# contributes no interior step and cannot reproduce that path weighting, so
# gamma lives in this kernel and ar() is not substituted for it.

#' Frank-Fahrbach balance-information (B-I) exposure mechanism
#'
#' Constructs the path-dependent B-I exposure kernel of Frank and Fahrbach
#' (1999) and its erratum, which combines the informational effect, carried by
#' the single maximal path from each source (their Model 10, p. 261), with the
#' normative or balance effect, carried by the remaining non-maximal paths and
#' weighted by `delta` (their Model 11, the "B-I model", p. 261; `delta = 1` is
#' the all-path Model 3, p. 257). Pass the result as `mechanism` to [compute_exposure()].
#'
#' # What this computes
#'
#' For the conduit network observed at each wave, form the combined per-wave
#' interaction matrix of the paper's equation (5) (p. 259; erratum p. 514),
#' \deqn{K'_w[i, j] = \gamma \textrm{ if } i = j, \quad \alpha\, k_w[i, j]
#'   \textrm{ otherwise,}}
#' where \eqn{k_w} is the conduit at wave \eqn{w} in this package's canonical
#' direction convention (see [compute_exposure()]'s Direction section). The paper
#' writes \eqn{k_{t-1 \to t}}, the interaction between two occasions; the network
#' observed at wave \eqn{w} stands for the interaction from \eqn{w} to the next
#' wave, which is this package's reading of observed data.
#'
#' For a focal wave \eqn{t} with prior waves \eqn{t-1, \ldots, t-V} available,
#' \eqn{Q_v = K'_{t-1} K'_{t-2} \cdots K'_{t-v}} accumulates influence through
#' **all** \eqn{v}-step paths (equation 6; p. 260, erratum pp. 514-515), and
#' \eqn{MP_v} is the same recursion with the sum over the intermediate actor
#' replaced by a maximum (equation 8; p. 260, erratum p. 515), accumulating only
#' the single strongest path. The exposure is equation (11) (p. 261):
#'
#' \deqn{E_i = \sum_{v=1}^{V} \left[ MP_v + \delta (Q_v - MP_v) \right]
#'   e_{(t-v)}}
#'
#' equivalently \eqn{(1-\delta) MP_v + \delta Q_v}. `delta = 0` is the paper's
#' Model (10), pure informational influence with redundant transmission removed;
#' `delta = 1` is Model (3), full redundant accumulation, which the authors note
#' permits sentiment to explode; a midrange value anchors the system to the
#' information that entered it (their Figure 4, p. 262, uses 0.8). The paper associates
#' the maximal path with information and the non-maximal paths with the
#' normative (balance) effect, which grows as `delta` approaches 1.
#'
#' Resolved at wave \eqn{t}, the mechanism reads only the waves before
#' \eqn{t}. So `compute_exposure(wave = t)` is the exposure that predicts the
#' outcome at \eqn{t}, and in a [fit_influence()] formula
#' `exposure(..., mechanism = bi(...))` with the default `x_lag = 1` is
#' resolved at the outcome wave: its most recent content and network are one
#' wave before the outcome, as in the paper's equation (5). `x_lag = k` resolves
#' it `k - 1` waves before the outcome; `x_lag = 0`, a separate `w_lag`, and a
#' transition that skips waves have no reading for it and are refused.
#'
#' Influence therefore needs history. At the first wave there is no earlier wave
#' for anything to have travelled through, and every exposure is 0. After that it
#' moves one tie per wave: on a chain `a -> b -> c`, `a`'s value reaches `b` at
#' the second wave and `c` at the third. At that third wave `c` is exposed under
#' this mechanism and not under the linear form, which reads direct ties only. A
#' node's own earlier value also counts along a path when `gamma > 0`.
#'
#' # The parameters
#'
#' All three are **supplied, never estimated**. Results are conditional on the
#' values given, which are recorded in the fitted object and reported by
#' `summary()`. Sweeping a range with [bi_sweep()] and reporting how conclusions
#' move is the appropriate practice; rnetis provides no procedure for choosing
#' them from data, because the source specifies them and does not estimate them.
#'
#' `alpha` down-weights each step between actors, so it controls how far influence
#' travels. `gamma` is the weight on an actor's own prior value along a path.
#'
#' `alpha`'s overall scale is absorbed by whatever coefficient a regression puts
#' on the exposure column, but its **depth profile is not**: a `v`-step path
#' carries \eqn{\alpha^v}, and because the exposure sums over depths no single
#' fitted coefficient can compensate for a wrong `alpha`.
#'
#' # What `content` means for this mechanism
#'
#' For the default linear mechanism, `content` is an alter characteristic. Here
#' it plays a different, mechanism-specific role: it is the \eqn{e} of the
#' published equations, the **exogenous input entering the system at each
#' wave** -- not an ordinary repeated observation. This is generally not
#' recoverable from raw outcome data; supply it explicitly, or see
#' [add_baseline_provenance()] for the one construction rnetis ships.
#'
#' # Requirements and refusals
#'
#' - **The conduit is the interpersonal off-diagonal only.** Self-retention is
#'   supplied separately as `gamma`, which becomes the transition matrix's
#'   diagonal internally. A conduit carrying self-loops would represent the same
#'   quantity twice, so it is refused rather than silently hollowed -- hollowing
#'   would change your data. This holds under `self_ties = "permitted"` as well.
#'   Equation (5) sums over all actors including `i` itself (erratum, p. 514), so
#'   the self-term in the paper is `gamma`. Frank and Fahrbach do not define
#'   \eqn{k_{ii} = 0}: the off-diagonal conduit is this package's input format, not
#'   part of their model.
#' - **Conduit weights must be nonnegative.** The maximal-path recursion extends
#'   the strongest prefix at each step, which finds the true maximum only when
#'   no later factor can reverse the ordering; a negative weight does exactly
#'   that, and the zeros standing in for absent ties would mask an all-negative
#'   path.
#' - **Row normalization is not defined for this mechanism** and is refused
#'   rather than applied after the fact: there is no single natural denominator
#'   for a path-accumulated quantity, and the correct stage at which to apply
#'   one would have to be derived. Frank and Fahrbach raise the same problem for
#'   networks that change over time (endnote 7, p. 272).
#' - **A wave whose network is unobserved** makes the exposure of every node in
#'   the group `NA`, with a warning naming it, when that wave is in the history:
#'   a zero matrix there would cut every path through it. See
#'   [compute_exposure()], "An unobserved network is not an empty one".
#' - **Missing data is masked along paths.** A node's exposure is `NA` exactly
#'   when a missing content value or tie weight lies on some path reaching it
#'   within the available history; every node no such path reaches keeps its
#'   exactly computed value. A missing quantity is never replaced by a
#'   substantive zero, and a known zero never turns a missing value elsewhere
#'   into missingness here.
#'
#' The published model does not treat these cases. The refusals, the masking rule,
#' and the split of input into a conduit and `gamma` are this package's own.
#'
#' @param delta the weight on non-maximal paths, in `[0, 1]`. `0` reproduces
#'   the paper's Model (10) and `1` its Model (3). **Required.**
#' @param alpha the interpersonal transmission factor applied at each
#'   between-actor path step, a finite nonnegative number (a negative factor
#'   would reverse which path is maximal, so it is refused).
#'   **Required, with no default**: `alpha = 1` says
#'   transmission is unattenuated beyond the conduit weights, which is a
#'   legitimate special case but not one Frank and Fahrbach establish as
#'   neutral.
#' @param gamma the self-retention factor applied at each within-actor path
#'   step, a finite nonnegative number, refused otherwise for the same reason
#'   as `alpha`. **Required, with no default**: `gamma = 0` deletes self-retention
#'   paths from the mechanism entirely, and `ar()` does not replace them (see
#'   Details), so defaulting to it would silently drop a published component
#'   of the model.
#' @return a kernel function suitable for `compute_exposure(..., mechanism = )`.
#' @references
#' Frank, K. A. and Fahrbach, K. (1999) Organization culture as a complex system:
#' balance and information in models of influence and selection. *Organization
#' Science* **10**(3), 253-277.
#'
#' Frank, K. A. and Fahrbach, K. (1999) Erratum: Organizational culture as a
#' complex system: balance and information in models of influence and selection.
#' *Organization Science* **10**(4), 514-515.
#' @seealso [compute_exposure()], [bi_components()], [bi_sweep()],
#'   [add_baseline_provenance()]
#' @examples
#' # Two 2-step paths carry a's wave-1 shock to b: a->c->b and a->d->b.
#' nodes <- data.frame(
#'   group = 1, node = rep(c("a", "b", "c", "d"), 3), wave = rep(1:3, each = 4),
#'   shock = c(10, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0)
#' )
#' ties <- data.frame(
#'   group = 1, source = c("a", "a", "c", "d"), target = c("c", "d", "b", "b"),
#'   wave = c(1, 1, 2, 2), link = 1
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#'
#' # delta = 0: informational only -- the strongest single path.
#' compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 0, alpha = 1, gamma = 0))
#'
#' # delta = 1: both parallel paths accumulate.
#' compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi(delta = 1, alpha = 1, gamma = 0))
#'
#' # Halving interpersonal transmission halves each step of a 2-step path.
#' compute_exposure(nd, "shock", "link", wave = 3,
#'                  mechanism = bi(delta = 0, alpha = 0.5, gamma = 0))
#' @export
bi <- function(delta, alpha, gamma) {
  .bi_check_scalar(delta, "delta")
  .bi_check_path_factor(alpha, "alpha")
  .bi_check_path_factor(gamma, "gamma")
  if (delta < 0 || delta > 1) {
    stop(
      sprintf(
        "delta must lie in [0, 1]; got %s. Frank & Fahrbach define delta as the weight on non-maximal paths relative to the maximal one: 0 is their Model (10) (informational only) and 1 is their Model (3) (all paths). Values outside that range are not an extrapolation of the published model.",
        format(delta)
      ),
      call. = FALSE
    )
  }

  kernel <- function(context, spec) {
    acc <- .bi_accumulate(context$W_history, context$X_history,
                          length(context$X), alpha = alpha, gamma = gamma)
    # E = MP + delta (Q - MP), equation (11), written as the equivalent blend.
    (1 - delta) * acc$A + delta * acc$C
  }
  attr(kernel, "requires") <- c("focal", "w_history", "x_history")
  attr(kernel, "supports_normalize") <- character(0)
  attr(kernel, "mechanism_provenance") <- list(
    type = "package", package = "rnetis", name = "bi",
    params = list(delta = delta, alpha = alpha, gamma = gamma)
  )
  kernel
}

#' Maximal-path and redundant-path components of the B-I mechanism
#'
#' Returns the two quantities [bi()] blends, as two columns rather than one:
#' `A_maxpath`, the maximal-path accumulation \eqn{\sum_v MP_v e_{(t-v)}} of
#' equation (8) (p. 260; erratum p. 515) -- equivalent to `bi(delta = 0, ...)` -- and `B_redundant`, the
#' non-maximal remainder \eqn{\sum_v (Q_v - MP_v) e_{(t-v)}}, which is what
#' `delta` weights in equation (11) (p. 261).
#'
#' Both are published quantities, and this exposes them so that the two
#' components of influence the paper distinguishes can be examined separately
#' rather than only in their blended form.
#'
#' **This is not a way to estimate `delta`.** Frank and Fahrbach specify `delta`;
#' rnetis provides no procedure for estimating it, and none is
#' implied by this function.
#'
#' @inheritParams bi
#' @return a kernel function suitable for `compute_exposure(..., mechanism = )`,
#'   whose result has two columns, `A_maxpath` and `B_redundant`.
#' @references
#' Frank, K. A. and Fahrbach, K. (1999) Organization culture as a complex system:
#' balance and information in models of influence and selection. *Organization
#' Science* **10**(3), 253-277.
#'
#' Frank, K. A. and Fahrbach, K. (1999) Erratum: Organizational culture as a
#' complex system: balance and information in models of influence and selection.
#' *Organization Science* **10**(4), 514-515.
#' @seealso [bi()], [compute_exposure()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = rep(c("a", "b", "c", "d"), 3), wave = rep(1:3, each = 4),
#'   shock = c(10, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0)
#' )
#' ties <- data.frame(
#'   group = 1, source = c("a", "a", "c", "d"), target = c("c", "d", "b", "b"),
#'   wave = c(1, 1, 2, 2), link = 1
#' )
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' compute_exposure(nd, "shock", "link", wave = 3, mechanism = bi_components(alpha = 1, gamma = 0))
#' @export
bi_components <- function(alpha, gamma) {
  .bi_check_path_factor(alpha, "alpha")
  .bi_check_path_factor(gamma, "gamma")

  kernel <- function(context, spec) {
    acc <- .bi_accumulate(context$W_history, context$X_history,
                          length(context$X), alpha = alpha, gamma = gamma)
    cbind(A_maxpath = acc$A, B_redundant = acc$C - acc$A)
  }
  attr(kernel, "requires") <- c("focal", "w_history", "x_history")
  attr(kernel, "supports_normalize") <- character(0)
  attr(kernel, "mechanism_provenance") <- list(
    type = "package", package = "rnetis", name = "bi_components",
    params = list(alpha = alpha, gamma = gamma)
  )
  kernel
}

#' @noRd
.bi_check_scalar <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
    stop(sprintf("%s must be a single, non-missing numeric value.", name),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Check a per-step path factor (`alpha` or `gamma`)
#'
#' Each step of a path is weighted by `alpha` (between actors) or `gamma`
#' (within an actor), and the maximal path of equation (8) is the path with the
#' largest product of those weights. The recursion that finds it (the updating
#' formula, equation (9), p. 261, runs the same recursion from the other end)
#' extends the strongest prefix one step at a time, which finds the true maximum
#' only when no later factor can reverse the ordering of two prefixes
#' -- that is, when every factor is nonnegative. Every example Frank & Fahrbach
#' work uses a nonnegative `alpha` and `gamma` for influence (Figure 1: gamma = 1,
#' alpha = 0.1, p. 257) and the paper defines no meaning for a negative one; an
#' infinite one makes every path value undefined. Both are refused, for the
#' same reason negative conduit weights are.
#'
#' @noRd
.bi_check_path_factor <- function(value, name) {
  .bi_check_scalar(value, name)
  if (!is.finite(value) || value < 0) {
    stop(
      sprintf(
        "%s must be a finite, nonnegative number; got %s. It weights each step of a path, and the maximal-path accumulation (Frank & Fahrbach's equation 8) is defined by extending the strongest path one step at a time, which is only the true maximum when no step factor is negative -- a negative factor reverses which path is strongest. Frank & Fahrbach use nonnegative transmission and retention rates throughout.",
        name, format(value)
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Build the combined per-wave interaction matrix of equation (5)
#'
#' `K'[i, i] = gamma` and `K'[i, j] = alpha * k[i, j]`. The diagonal is
#' **replaced**, not added to, because the self position in (5) acts as the
#' identity and `gamma` is the only free quantity on it (see this file's header).
#'
#' @param k one wave's conduit matrix.
#' @param alpha,gamma the published path-step parameters.
#' @param v how many waves before the focal wave this conduit is, for messages.
#' @return the combined matrix.
#' @noRd
.bi_combined_matrix <- function(k, alpha, gamma, v) {
  diag_k <- diag(k)
  if (any(diag_k != 0, na.rm = TRUE)) {
    offenders <- rownames(k)[which(diag_k != 0)]
    stop(
      sprintf(
        "bi() takes the interpersonal conduit's off-diagonal only, but the network %d wave(s) before the focal wave has self-loop(s) on: %s. Self-retention is supplied separately, as gamma, and it becomes the diagonal of the transition matrix internally -- so a self-loop here would represent the same quantity twice and be counted twice. Remove the self-loops from the conduit, or use a network that excludes self-interaction; set gamma to say how much an actor retains between occasions.",
        v, paste(utils::head(offenders, 5), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (any(k < 0, na.rm = TRUE)) {
    stop(
      sprintf(
        "The bi() mechanism requires nonnegative conduit weights, but the network %d wave(s) before the focal wave contains negative values. The maximal-path accumulation extends the strongest prefix at each step, which finds the true maximum only when no later factor can reverse the ordering -- a negative weight does exactly that, and the zeros standing in for absent ties would mask an all-negative path. Recode the conduit to a nonnegative scale (binarize(), or an explicit magnitude transform) before using bi()/bi_components().",
        v
      ),
      call. = FALSE
    )
  }

  out <- alpha * k
  diag(out) <- gamma
  out
}

#' Accumulate all-path (Q) and maximal-path (MP) exposure totals across history
#'
#' Implements equations (6) and (8) as corrected by the erratum. Both are the
#' same recursion over the combined matrices of equation (5), differing only in
#' whether the intermediate actor is summed or maximised over:
#'
#'   `Q_v  = K'_{t-1} K'_{t-2} ... K'_{t-v}`   (ordinary matrix product)
#'   `MP_v = K'_{t-1} (x) K'_{t-2} ... `       (max-times product)
#'
#' with the most recent wave leftmost, matching the erratum's ordering
#' (`k_{ia,t-1->t}` first, erratum p. 514). Returns `list(A = sum_v MP_v e_v, C = sum_v Q_v e_v)`;
#' callers blend them (`bi()`) or keep them apart (`bi_components()`).
#'
#' Missing values propagate by masked accumulation (`R/exposure-mask.R`): a
#' node's total is `NA` exactly when an unknown lies on some path reaching it at
#' some depth, and `NA + x` in the running sums is what makes "undefined at any
#' depth" mean "undefined in total".
#'
#' @noRd
.bi_accumulate <- function(K_hist, e_hist, n, alpha, gamma) {
  if (length(K_hist) == 0L) {
    return(list(A = numeric(n), C = numeric(n)))  # no history: the empty sum
  }

  K <- lapply(seq_along(K_hist), function(v) {
    .bi_combined_matrix(K_hist[[v]], alpha = alpha, gamma = gamma, v = v)
  })

  # v = 1: a single step, so there is exactly one path and the all-path and
  # maximal-path accumulations necessarily coincide.
  Q <- K[[1]]
  MP <- K[[1]]
  A <- .mask_matvec(MP, e_hist[[1]])
  C <- .mask_matvec(Q, e_hist[[1]])

  if (length(K) > 1L) {
    for (v in 2:length(K)) {
      # Extend the chain by appending the next-older matrix on the right, so the
      # most recent factor stays leftmost as the depth grows.
      Q <- .mask_matmul(Q, K[[v]])
      MP <- .mask_maxtimes(MP, K[[v]])
      A <- A + .mask_matvec(MP, e_hist[[v]])
      C <- C + .mask_matvec(Q, e_hist[[v]])
    }
  }
  list(A = A, C = C)
}
