# Backend adapter for sna::lnam(), the simultaneous network-autocorrelation
# (SAR) route: Y = rho1 * W1 %*% Y + X*beta + e.
#
# Reached only when fit_influence()'s formula puts the outcome's OWN
# contemporaneous network exposure on the right-hand side. That specification
# is not estimable by least squares -- the regressor is determined jointly with
# the outcome -- so it is routed here rather than fitted wrongly or refused.
#
# lnam objects never escape this file. fit_influence() only ever touches what
# backend_extract() returns, as with every other adapter.
#
# W1 IS THE CALLER'S MATRIX. It arrives already built by .build_endogenous_w1()
# through .build_canonical_W() -- the one direction-resolution code path the
# exposure engine itself uses -- so this file has no second, potentially
# disagreeing direction logic of its own.
#
# ---------------------------------------------------------------------------
# THREE PROPERTIES OF sna::lnam() THIS ADAPTER HAS TO HANDLE, each measured
# rather than assumed:
#
#   1. A fitted lnam object has no `convergence` field of any kind (checked
#      with `names()`), so convergence cannot simply be declared TRUE: that
#      would be a claim about something the optimizer never reported, and it
#      would grade every SAR fit "clean" whatever happened.
#   2. `names(fit$residuals)` is NULL, so residual naming has to come from the
#      adapter -- and positional naming (`seq_along()`) is correct only when no
#      row was dropped, silently wrong otherwise. Residuals are named by the
#      rows the model frame kept.
#   3. `model.frame()` drops incomplete rows while W1 is built for ALL rows,
#      so without alignment any NA crashes the fit with "Order of W1 must
#      match length of y." See .lnam_align_w1() below for what the alignment
#      does and does not claim.
# ---------------------------------------------------------------------------

#' @exportS3Method
backend_info.netis_backend_lnam <- function(backend) {
  list(name = "lnam", package = "sna", version = as.character(utils::packageVersion("sna")))
}

#' Restrict W1 to the rows the model frame kept
#'
#' WHY THIS IS NOT ORDINARY LISTWISE DELETION. In a least-squares fit, dropping
#' an incomplete row removes an OBSERVATION and leaves every other row's
#' predictors untouched. In a simultaneous autoregressive fit it removes a NODE
#' FROM THE NETWORK: the retained actors lose whatever influence reached them
#' through the dropped one, and `rho1` is then estimated on a different network
#' from the one the user supplied. The arithmetic is right either way; the
#' estimand is not the same.
#'
#' So the subset is performed -- refusing outright would make the model
#' unusable on any dataset with a single missing covariate, and the fit on the
#' retained subnetwork is a correct fit of a well-defined model -- but the
#' removal is recorded in the fitted object and reported rather than absorbed.
#'
#' Subsetting is safe here specifically because `.build_canonical_W()` returns
#' the RAW oriented adjacency with no row normalization. Ties to a removed node
#' simply vanish, which is exactly what rebuilding the matrix over the retained
#' node set would produce. Row-normalization (`normalize = "row"`) is
#' therefore applied AFTER this subset, by `.lnam_normalize_w1()`, so the
#' normalized rows sum to one over the actors the fit retains.
#'
#' WHICH LABELS ARE WHICH, because there are two sets and they are not the same.
#' `W1` is dimnamed by NODE ID, but built with one row per ASSEMBLED TABLE ROW,
#' in that table's row order (see `.build_endogenous_w1()`). `model.frame()`
#' reports the table's own row names, which are positional labels, not node
#' ids. So the join runs table-row-name to table-row-name -- the only labels
#' the two sides share -- and the node ids are read off afterwards for
#' reporting. Matching `rownames(mf)` against `rownames(W1)` directly would
#' compare row indices to node ids and find nothing.
#'
#' @param W1 the caller's conduit matrix, one row per table row, dimnamed by
#'   node id.
#' @param keep the row names the model frame retained.
#' @param table_rows the assembled table's row names, in its own order.
#' @return a list with `W1` (restricted) and `dropped` (the node ids removed).
#' @noRd
.lnam_align_w1 <- function(W1, keep, table_rows) {
  ids <- rownames(W1)
  if (is.null(ids) || is.null(colnames(W1))) {
    stop(
      "Internal error: the conduit matrix reaching the lnam adapter has no dimnames, so rnetis cannot report which actor a dropped row belonged to.",
      call. = FALSE
    ) # nocov
  }
  if (nrow(W1) != length(table_rows)) {
    stop(
      sprintf(
        "Internal error: the conduit matrix has %d rows but the assembled table has %d. .build_endogenous_w1() builds one W1 row per table row, so these cannot disagree through the supported path.",
        nrow(W1), length(table_rows)
      ),
      call. = FALSE
    ) # nocov
  }
  if (identical(as.character(keep), as.character(table_rows))) {
    return(list(W1 = W1, dropped = character(0)))
  }

  idx <- match(as.character(keep), as.character(table_rows))
  if (anyNA(idx)) {
    stop(
      sprintf(
        "Internal error: the model frame retained %d row(s) with no matching row in the assembled table.",
        sum(is.na(idx))
      ),
      call. = FALSE
    ) # nocov
  }

  list(W1 = W1[idx, idx, drop = FALSE], dropped = ids[-idx])
}

#' Apply the term's normalization to the fitted W1
#'
#' `normalize = "none"` leaves W1 as built. `normalize = "row"` divides each
#' row by its sum, using the exposure engine's own `.apply_normalization()`,
#' so `isolate_rule = "error"` refuses an actor with no ties and `"zero"`
#' leaves its row at zero (it receives no contemporaneous influence). The
#' default `isolate_rule = "na"` would put a missing row into W1, which
#' `sna::lnam()` cannot fit, so it is refused here with the two rules that can
#' be. A tie value that is itself missing is refused for the same reason.
#'
#' @noRd
.lnam_normalize_w1 <- function(W1, normalize = "none", isolate_rule = "na") {
  if (anyNA(W1)) {
    stop(
      "The network for the same-outcome contemporaneous exposure() term has missing tie values among the actors being fitted. sna::lnam() needs every cell of W1, so the term cannot be fitted on this network. Resolve the missing tie values first.",
      call. = FALSE
    )
  }
  if (identical(normalize, "row") && identical(isolate_rule, "na")) {
    isolates <- rownames(W1)[rowSums(W1) == 0]
    if (length(isolates) > 0L) {
      stop(
        sprintf(
          "Row-normalization was requested for the same-outcome contemporaneous exposure() term, but %d actor%s no ties in W1 (%s), and isolate_rule = \"na\" would leave %s row of W1 undefined, which sna::lnam() cannot fit. Use isolate_rule = \"zero\" (an isolate receives no contemporaneous influence) or isolate_rule = \"error\".",
          length(isolates), if (length(isolates) == 1L) " has" else "s have",
          paste(utils::head(isolates, 5), collapse = ", "),
          if (length(isolates) == 1L) "its" else "their"
        ),
        call. = FALSE
      )
    }
  }
  out <- .apply_normalization(W1, normalize, isolate_rule)
  if (anyNA(out)) {
    stop(
      "Row-normalization was requested for the same-outcome contemporaneous exposure() term, but an actor's signed tie weights sum to zero, so that row of W1 has no denominator and sna::lnam() cannot fit it. Use normalize = \"none\", or a network whose rows do not cancel.",
      call. = FALSE
    )
  }
  out
}

#' @exportS3Method
backend_fit.netis_backend_lnam <- function(backend, formula, data, W1, endogenous_name,
                                           normalize = "none", isolate_rule = "na", ...) {
  mf <- stats::model.frame(formula, data = data)
  y <- stats::model.response(mf)
  x <- stats::model.matrix(formula, data = mf)

  # WHICH ASSEMBLED ROWS THE FIT ACTUALLY USED. model.frame() drops incomplete
  # rows, and lnam takes matrices rather than a model frame, so nothing in the
  # returned object records which rows survived. This is the only place the
  # correspondence still exists, so it is captured here and carried on the fit.
  row_names <- rownames(mf)
  aligned <- .lnam_align_w1(W1, row_names, rownames(data))
  w1_fitted <- .lnam_normalize_w1(aligned$W1, normalize, isolate_rule)

  fit <- sna::lnam(y = y, x = x, W1 = w1_fitted, ...)
  attr(fit, "endogenous_name") <- endogenous_name
  attr(fit, "netis_row_names") <- row_names
  attr(fit, "netis_dropped_nodes") <- aligned$dropped

  # The eigenvalues of the conduit ACTUALLY FITTED -- computed from the
  # aligned, normalized matrix, not the caller's, because they describe the
  # system that was estimated. (I - rho1 * W1) is singular exactly when
  # 1 - rho1 * lambda = 0 for some eigenvalue lambda of W1, so these are what
  # backend_extract() needs to report whether the fitted operator is
  # invertible.
  attr(fit, "netis_w1_eigenvalues") <- tryCatch(
    eigen(w1_fitted, only.values = TRUE)$values,
    error = function(e) NULL
  )
  fit
}

#' Is the fitted simultaneous operator (I - rho * W1) invertible?
#'
#' Its eigenvalues are `1 - rho * lambda_i` over the eigenvalues `lambda_i` of
#' W1, so it is singular exactly when one of them is zero. The smallest
#' modulus is returned so the check can be reported as what it is. "Zero" is
#' the numerical zero of the eigenvalue computation: a modulus below
#' `sqrt(.Machine$double.eps)` times the operator's own scale.
#'
#' @param rho the fitted autocorrelation parameter.
#' @param eigenvalues the eigenvalues of the fitted W1 (possibly complex).
#' @return a list with `min_modulus` (`NA` when either input is unavailable)
#'   and `invertible` (`NA`, `TRUE` or `FALSE`).
#' @noRd
.sar_operator_check <- function(rho, eigenvalues) {
  if (length(rho) != 1L || !is.finite(rho) || length(eigenvalues) == 0L ||
      any(!is.finite(Mod(eigenvalues)))) {
    return(list(min_modulus = NA_real_, invertible = NA))
  }
  moduli <- Mod(1 - rho * eigenvalues)
  scale <- max(1, abs(rho) * max(Mod(eigenvalues)))
  min_modulus <- min(moduli)
  list(min_modulus = min_modulus,
       invertible = min_modulus > sqrt(.Machine$double.eps) * scale)
}

#' Extract fields from an lnam fit
#'
#' `fit$acvm` is the joint asymptotic covariance matrix for (beta..., rho1,
#' sigmasq), in that order -- verified against `fit$beta.se`/`fit$rho1.se`
#' rather than assumed. The leading `length(beta) + 1` rows and columns are
#' exactly (beta..., rho1), which is `coefficients`' own order, so no
#' reordering is needed, only dropping the trailing sigmasq row and column.
#'
#' rho1's coefficient is named after the `exposure()` term's own resolved name
#' rather than a generic "rho1", consistent with every other exposure term
#' being identified by its own column name -- and what makes it appear under
#' `summary()`'s exposure reporting like any other.
#'
#' `sna::lnam` objects have no `nobs`/`vcov`/`logLik` S3 methods (verified by
#' inspection: `methods(class = "lnam")` lists only `coef`/`plot`/`print`/
#' `summary`), so the `tryCatch(stats::logLik(fit), ...)` the other adapters
#' use would simply always fail. `fit$lnlik.model`/`fit$df.model`/
#' `fit$df.total` are the same numbers `summary.lnam()` itself prints its AIC
#' and BIC from, assembled here into a properly classed `logLik` object.
#'
#' @noRd
#' @exportS3Method
backend_extract.netis_backend_lnam <- function(backend, fit) {
  beta <- as.vector(fit$beta)
  names(beta) <- colnames(fit$x)
  coefs <- c(beta, stats::setNames(unname(fit$rho1), attr(fit, "endogenous_name")))
  k <- length(coefs)

  # `acvm` IS OPTIONAL. sna::lnam builds its asymptotic covariance from a
  # numerically differentiated information matrix, and that step needs
  # numDeriv -- a transitive Suggests, declared in rnetis's own Suggests so a
  # fresh install does not silently lack it. When it is absent lnam still
  # returns estimates and simply omits acvm, so indexing it unconditionally
  # turns a reportable gap into an error. Missing standard errors are reported
  # as missing; they are never quietly zero.
  information_available <- is.matrix(fit$acvm) &&
    all(dim(fit$acvm) >= k) && any(is.finite(fit$acvm[seq_len(k), seq_len(k)]))
  vcov_mat <- if (information_available) {
    fit$acvm[seq_len(k), seq_len(k), drop = FALSE]
  } else {
    matrix(NA_real_, k, k)
  }
  dimnames(vcov_mat) <- list(names(coefs), names(coefs))
  loglik <- structure(fit$lnlik.model, df = fit$df.model, nobs = fit$df.total,
                      class = "logLik")

  ev <- attr(fit, "netis_w1_eigenvalues")
  sr <- if (length(ev)) max(Mod(ev)) else NA_real_
  operator <- .sar_operator_check(unname(fit$rho1), ev)
  dropped <- attr(fit, "netis_dropped_nodes") %||% character(0)

  list(
    coefficients = coefs,
    vcov = vcov_mat,
    random_effects = NULL,
    var_corr = NULL,
    # REPORTED, NOT ASSERTED. sna::lnam returns no optimizer status of any kind:
    # a fitted object has no `convergence` field, so `converged = TRUE` would be a
    # claim about something never observed. What CAN be known from the returned object is whether it produced finite
    # estimates, and the absence of anything better is stated rather than
    # papered over.
    convergence = list(
      converged = all(is.finite(coefs)),
      singular = FALSE,
      messages = c(
        if (information_available) {
          character(0)
        } else {
          "sna::lnam returned no information matrix, so standard errors are unavailable; it reports no optimizer convergence status either way"
        },
        if (length(dropped) > 0L) {
          sprintf(
            "%d node%s removed from the network before fitting because %s incomplete (%s). In a simultaneous autoregressive model this removes the node from the network, not merely an observation from the sample: the retained actors lose whatever influence reached them through it, and rho1 describes the remaining subnetwork.",
            length(dropped), if (length(dropped) == 1L) "" else "s",
            if (length(dropped) == 1L) "its row was" else "their rows were",
            paste(utils::head(dropped, 10), collapse = ", ")
          )
        }
      )
    ),
    loglik = loglik,
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    residual_df = fit$df.residual,
    sigma = fit$sigma,
    # lnam's residuals are from the FILTERED equation, (I - rho*W)y - Xb, which
    # is what a simultaneous autoregressive fit leaves unexplained. Named by
    # the assembled row labels the model frame retained, not by position:
    # measured, `names(fit$residuals)` is NULL, and a positional label pairs
    # every row after the first dropped one with the wrong actor.
    residuals = tryCatch({
      rn <- attr(fit, "netis_row_names")
      r <- as.vector(fit$residuals)
      if (is.null(rn) || length(rn) != length(r)) NULL else stats::setNames(r, rn)
    }, error = function(e) NULL),
    residual_type = "filtered",
    rho = unname(fit$rho1),
    w1_spectral_radius = sr,
    sar_operator_min_modulus = operator$min_modulus,
    sar_operator_invertible = operator$invertible,
    information_matrix_available = information_available,
    dropped_nodes = dropped,
    warnings = character(0)
  )
}

#' @exportS3Method
backend_predict.netis_backend_lnam <- function(backend, fit, newdata) {
  stop(
    "predict() is not supported for an lnam-backed (endogenous contemporaneous exposure) fit: a prediction from a simultaneous autoregressive model requires solving (I - rho1 * W1)^-1 for the new data's own network, which is not implemented.",
    call. = FALSE
  )
}
