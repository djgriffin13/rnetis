# MCMC convergence diagnostics, shared by every sampler rnetis runs.
#
# THE DEFINITIONS are those of Vehtari, Gelman, Simpson, Carpenter & Burkner
# (2021), "Rank-normalization, folding, and localization: an improved R-hat",
# statement for statement with the `posterior` package (1.7.0), against which
# they are cross-checked outside this package (posterior is not a dependency):
#
#   R-hat       max(bulk R-hat, folded R-hat). Each is the classic split R-hat
#               computed on RANK-NORMALIZED split chains. Bulk detects chains
#               that disagree in location; folded -- computed on
#               |x - median(x)| -- detects chains that agree in location and
#               disagree in scale, which bulk R-hat cannot see.
#   bulk ESS    effective sample size of the rank-normalized split chains.
#   tail ESS    the smaller of the effective sample sizes of the indicators
#               x <= q05 and x <= q95: how well the 5% and 95% quantiles, and
#               so a 90% interval's ends, are estimated.
#
# CHAIN IDENTITY IS AN INPUT. A stacked draws matrix does not say where one
# chain ends and the next begins, and computing R-hat on the stack treats K
# chains as one -- the comparison R-hat exists to make is then never made. Callers
# with several chains pass the chain vector (or a list of per-chain matrices).
#
# AN UNAVAILABLE DIAGNOSTIC IS REPORTED AS UNAVAILABLE, with the reason, rather
# than as a number: non-finite draws, a constant parameter (nothing to
# diagnose), or too few draws per half-chain. A single chain is reported as
# such, because its split R-hat compares the chain with itself.

#' Convergence diagnostics for one or several chains
#'
#' @param draws a numeric matrix of draws (iterations x parameters), a numeric
#'   vector (one parameter), or a list of per-chain matrices with the same
#'   columns and length.
#' @param chain optional chain identifier per row of a matrix `draws`. Rows are
#'   grouped by chain in order of first appearance; within a chain, row order is
#'   iteration order. `NULL` treats the matrix as one chain.
#' @param min_half the shortest usable half-chain, 4 by package convention.
#'   Below it the diagnostics are `NA` with status `"too_few_draws"`.
#' @param starts how the chains were started, for the note: `"dispersed"`,
#'   `"common"`, or `NULL` when unknown.
#' @return a list with:
#'   * `table`: one row per parameter -- `parameter`, `rhat`, `rhat_bulk`,
#'     `rhat_folded`, `ess_bulk`, `ess_tail`, `status`;
#'   * the same columns as named vectors (`rhat`, `rhat_bulk`, `rhat_folded`,
#'     `ess_bulk`, `ess_tail`, `status`), plus `ess` (= `ess_bulk`, kept for
#'     callers that read one ESS);
#'   * `rhat_max`, `ess_min` (bulk), `ess_tail_min`, and the parameters they
#'     belong to (`rhat_worst`, `ess_worst`);
#'   * `n_chains`, `n_draws` (per chain), `scope`, and `note`.
#' @noRd
.netis_mcmc_diagnostics <- function(draws, chain = NULL, min_half = 4L, starts = NULL) {
  chains <- .mcmc_as_chains(draws, chain)
  par_names <- colnames(chains[[1]])
  n_par <- ncol(chains[[1]])
  if (is.null(par_names)) par_names <- paste0("par", seq_len(n_par))
  n_chains <- length(chains)
  n_draws <- nrow(chains[[1]])

  rows <- lapply(seq_len(n_par), function(p) {
    x <- vapply(chains, function(m) m[, p], numeric(n_draws))
    if (is.null(dim(x))) x <- matrix(x, nrow = n_draws)
    .mcmc_one_parameter(x, min_half)
  })
  pick <- function(field) stats::setNames(vapply(rows, `[[`, numeric(1), field), par_names)
  status <- stats::setNames(vapply(rows, `[[`, character(1), "status"), par_names)
  rhat <- pick("rhat")
  ess_bulk <- pick("ess_bulk")
  ess_tail <- pick("ess_tail")

  finite_max <- function(v) {
    v <- v[is.finite(v)]
    if (length(v)) max(v) else NA_real_
  }
  finite_min <- function(v) {
    v <- v[is.finite(v)]
    if (length(v)) min(v) else NA_real_
  }
  worst <- function(v, fun) {
    ok <- is.finite(v)
    if (!any(ok)) NA_character_ else names(v)[ok][fun(v[ok])]
  }

  list(
    table = data.frame(parameter = par_names, rhat = unname(rhat),
                       rhat_bulk = unname(pick("rhat_bulk")),
                       rhat_folded = unname(pick("rhat_folded")),
                       ess_bulk = unname(ess_bulk), ess_tail = unname(ess_tail),
                       status = unname(status), stringsAsFactors = FALSE),
    rhat = rhat, rhat_bulk = pick("rhat_bulk"), rhat_folded = pick("rhat_folded"),
    ess_bulk = ess_bulk, ess_tail = ess_tail, ess = ess_bulk, status = status,
    rhat_max = finite_max(rhat), ess_min = finite_min(ess_bulk),
    ess_tail_min = finite_min(ess_tail),
    rhat_worst = worst(rhat, which.max), ess_worst = worst(ess_bulk, which.min),
    n_chains = n_chains, n_draws = n_draws,
    scope = if (n_chains > 1L) "between and within chains" else "within one chain",
    note = .mcmc_diagnostic_note(n_chains, starts)
  )
}

#' Normalize draws to a list of per-chain matrices
#'
#' @noRd
.mcmc_as_chains <- function(draws, chain = NULL) {
  if (is.list(draws) && !is.data.frame(draws)) {
    chains <- lapply(draws, as.matrix)
    if (!is.null(chain)) {
      stop("Internal error: give chain identity as a list of chains OR a chain vector, not both.",
           call. = FALSE)
    }
  } else {
    m <- if (is.null(dim(draws))) matrix(draws, ncol = 1L) else as.matrix(draws)
    if (is.null(chain)) {
      chains <- list(m)
    } else {
      if (length(chain) != nrow(m)) {
        stop(sprintf("Internal error: %d chain labels for %d draws.", length(chain), nrow(m)),
             call. = FALSE)
      }
      ids <- unique(chain)
      chains <- lapply(ids, function(id) m[chain == id, , drop = FALSE])
    }
  }
  lens <- vapply(chains, nrow, integer(1))
  if (length(unique(lens)) > 1L) {
    # R-hat's between-chain variance is defined for equal-length chains. The
    # sampler always produces them; unequal lengths mean draws were lost, which
    # is a defect to report, not a shape to accommodate.
    stop(sprintf("Internal error: chains have unequal lengths (%s).", paste(lens, collapse = ", ")),
         call. = FALSE)
  }
  widths <- vapply(chains, ncol, integer(1))
  if (length(unique(widths)) > 1L) {
    stop("Internal error: chains carry different parameters.", call. = FALSE)
  }
  chains
}

#' Every diagnostic for one parameter, or the reason it is unavailable
#'
#' @param x draws for one parameter, iterations x chains.
#' @noRd
.mcmc_one_parameter <- function(x, min_half) {
  na_row <- function(status) {
    list(rhat = NA_real_, rhat_bulk = NA_real_, rhat_folded = NA_real_,
         ess_bulk = NA_real_, ess_tail = NA_real_, status = status)
  }
  if (anyNA(x) || any(!is.finite(x))) return(na_row("nonfinite"))
  if (floor(nrow(x) / 2) < min_half) return(na_row("too_few_draws"))
  if (.mcmc_is_constant(x)) return(na_row("constant"))

  split_x <- .mcmc_split_chains(x)
  rhat_bulk <- .mcmc_rhat_basic(.mcmc_z_scale(split_x))
  rhat_folded <- .mcmc_rhat_basic(.mcmc_z_scale(.mcmc_split_chains(abs(x - stats::median(x)))))
  ess_bulk <- .mcmc_ess_basic(.mcmc_z_scale(split_x))
  ess_tail <- min(.mcmc_ess_quantile(x, 0.05), .mcmc_ess_quantile(x, 0.95))
  list(rhat = max(rhat_bulk, rhat_folded), rhat_bulk = rhat_bulk,
       rhat_folded = rhat_folded, ess_bulk = ess_bulk, ess_tail = ess_tail,
       status = "ok")
}

#' Whether every draw is the same value
#'
#' The `posterior` package's test, an absolute tolerance of machine epsilon.
#' @noRd
.mcmc_is_constant <- function(x, tol = .Machine$double.eps) {
  abs(max(x) - min(x)) < tol
}

#' Split each chain into its first and second halves
#'
#' An odd-length chain loses its middle draw, so both halves have the same
#' length.
#' @noRd
.mcmc_split_chains <- function(x) {
  x <- as.matrix(x)
  n <- nrow(x)
  if (n == 1L) return(x)
  half <- n / 2
  cbind(x[seq_len(floor(half)), , drop = FALSE], x[ceiling(half + 1):n, , drop = FALSE])
}

#' Rank-normalize draws pooled across every chain
#'
#' One monotone relabelling of the whole sample: ranking within chains would
#' destroy the between-chain information R-hat needs. Ties take average ranks,
#' so a sampler that repeats a value is not credited with variation it lacks.
#' The Blom offset c = 3/8 gives the denominator S - 2c + 1 = S + 1/4.
#' @noRd
.mcmc_z_scale <- function(x) {
  r <- rank(as.vector(x), ties.method = "average")
  z <- stats::qnorm((r - 3 / 8) / (length(r) - 2 * 3 / 8 + 1))
  matrix(z, nrow = nrow(x), ncol = ncol(x))
}

#' Classic split R-hat on already-split (and normalized) chains
#'
#' `NA` when the input is non-finite or constant.
#' @noRd
.mcmc_rhat_basic <- function(x) {
  x <- as.matrix(x)
  if (anyNA(x) || any(!is.finite(x)) || .mcmc_is_constant(x)) return(NA_real_)
  n <- nrow(x)
  chain_mean <- colMeans(x)
  chain_var <- apply(x, 2, stats::var)
  var_between <- n * stats::var(chain_mean)
  var_within <- mean(chain_var)
  sqrt((var_between / var_within + n - 1) / n)
}

#' Effective sample size of the indicator x <= quantile(x, prob)
#'
#' @noRd
.mcmc_ess_quantile <- function(x, prob) {
  I <- x <= stats::quantile(x, prob, names = FALSE)
  storage.mode(I) <- "double"
  .mcmc_ess_basic(.mcmc_split_chains(I))
}

#' Effective sample size of already-split chains
#'
#' Geyer's (1992, Practical Markov chain Monte Carlo, Statistical Science 7(4),
#' 473-483) initial positive sequence on paired autocorrelations, then his
#' initial monotone sequence, with autocorrelations combined across chains as
#' Vehtari et al. (2021) define them. `NA` for fewer than three iterations, or a
#' non-finite or constant input.
#'
#' @param x draws, iterations x (split) chains.
#' @noRd
.mcmc_ess_basic <- function(x) {
  x <- as.matrix(x)
  m <- ncol(x)
  n <- nrow(x)
  if (n < 3L || anyNA(x) || any(!is.finite(x)) || .mcmc_is_constant(x)) return(NA_real_)

  # Per-chain autocovariance, FFT-based and normalized by n. A chain that is constant on its own -- possible
  # for an indicator -- has zero autocovariance at every lag.
  acov <- vapply(seq_len(m), function(j) .autocovariance(x[, j]), numeric(n))
  if (is.null(dim(acov))) acov <- matrix(acov, nrow = n)
  acov_means <- rowMeans(acov)
  mean_var <- acov_means[1] * n / (n - 1)
  var_plus <- mean_var * (n - 1) / n
  if (m > 1L) var_plus <- var_plus + stats::var(colMeans(x))

  rho <- numeric(n)
  t <- 0L
  rho_even <- 1
  rho[1] <- rho_even
  rho_odd <- 1 - (mean_var - acov_means[2]) / var_plus
  rho[2] <- rho_odd
  while (t < n - 5L && !is.nan(rho_even + rho_odd) && (rho_even + rho_odd > 0)) {
    t <- t + 2L
    rho_even <- 1 - (mean_var - acov_means[t + 1L]) / var_plus
    rho_odd <- 1 - (mean_var - acov_means[t + 2L]) / var_plus
    if ((rho_even + rho_odd) >= 0) {
      rho[t + 1L] <- rho_even
      rho[t + 2L] <- rho_odd
    }
  }
  max_t <- t
  if (rho_even > 0) rho[max_t + 1L] <- rho_even

  # The true sequence is decreasing, so a pair sum that rises above its
  # predecessor is estimation noise; capping it keeps the autocorrelation from
  # being understated.
  t <- 0L
  while (t <= max_t - 4L) {
    t <- t + 2L
    if (rho[t + 1L] + rho[t + 2L] > rho[t - 1L] + rho[t]) {
      rho[t + 1L] <- (rho[t - 1L] + rho[t]) / 2
      rho[t + 2L] <- rho[t + 1L]
    }
  }
  s <- m * n
  # `rho[1:max_t]` exactly as the reference implementation writes it. At
  # max_t = 0 -- chains of five or fewer draws, where the loop never runs --
  # 1:0 selects rho[1], and seq_len(0) would select nothing; the two differ, and
  # the reference's choice is kept so the numbers agree with it.
  tau <- -1 + 2 * sum(rho[1:max_t]) + rho[max_t + 1L]
  # The floor keeps an antithetic chain from reporting an effective sample
  # larger than any finite sequence of draws could support.
  tau <- max(tau, 1 / log10(s))
  s / tau
}

#' FFT autocovariance of one chain, normalized by its length
#'
#' Zero-padded to a power of two of at least twice the length, so the circular
#' correlation the FFT computes is the linear one this needs; a shorter pad
#' wraps the series onto itself and contaminates long lags with short ones.
#'
#' @param x a numeric vector.
#' @return autocovariances at lags `0..length(x) - 1`, divided by `length(x)`.
#' @noRd
.autocovariance <- function(x) {
  n <- length(x)
  if (stats::var(x) == 0) return(rep(0, n))
  nfft <- 2^ceiling(log2(2 * n))
  y <- c(x - mean(x), rep(0, nfft - n))
  f <- stats::fft(y)
  ac <- Re(stats::fft(f * Conj(f), inverse = TRUE)) / nfft
  ac[seq_len(n)] / n
}

#' What a given chain configuration can and cannot establish
#'
#' Stated per fit rather than left to the reader, because the difference between
#' one chain and several is not a matter of precision: split R-hat on one chain
#' detects drift within it, and only several chains -- started apart -- can
#' disagree about where the posterior is.
#'
#' @param n_chains the number of chains.
#' @param starts `"dispersed"`, `"common"`, or `NULL` when unknown.
#' @noRd
.mcmc_diagnostic_note <- function(n_chains, starts = NULL) {
  start_text <- if (identical(starts, "dispersed")) {
    "Chains after the first were started from dispersed values; dispersion widens what R-hat can detect but does not guarantee the starts bracket the posterior."
  } else if (identical(starts, "common")) {
    "The chains were started from the same values, which limits what R-hat can detect."
  } else {
    NULL
  }
  if (n_chains > 1L) {
    paste(c(
      sprintf("R-hat (rank-normalized and folded) compares %d chains, each split in half, in location and in scale.", n_chains),
      start_text,
      "It detects chains that disagree; agreement does not establish that they found the posterior."
    ), collapse = " ")
  } else {
    paste(
      "One chain: R-hat compares the two halves of the chain, which detects drift",
      "within the run. It cannot detect a chain that settled quickly into the",
      "wrong region -- that needs several chains from dispersed starting values.",
      "Judging these values is left to you; rnetis prints no pass/fail verdict."
    )
  }
}
