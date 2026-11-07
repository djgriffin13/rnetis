# Robust variance for Influence fits.
#
# THE PREAMBLE IS THE DESIGN, not decoration:
#
#   model what you understand;
#   use robust variance for residual dependence you cannot or do not want to
#     parameterize;
#   NO CORRECTION HERE FIXES A BIASED COEFFICIENT.
#
# That third line is why this file refuses to be reached automatically. If
# residual dependence reflects an omitted variable correlated with exposure,
# a sandwich puts a more honest interval around a wrong number, which is a
# worse outcome than a narrow interval around the same wrong number because it
# looks like it was dealt with.
#
# NO PRE-TEST SWITCHING. rnetis never inspects a diagnostic and swaps the
# variance estimator on the user's behalf. Choosing the estimator by looking at
# the data makes the reported interval's coverage a function of that choice,
# and the resulting procedure has neither the model-based nor the robust
# estimator's properties. rnetis reports, recommends, and records what was
# asked for.

#' Robust-variance options for an Influence fit
#'
#' `"model"` is the estimator's own variance. `"cluster"` is a clustered
#' sandwich, whose MEANING depends entirely on what it clusters on -- see
#' `.influence_cluster_levels` below, where the distinction is load-bearing
#' rather than a convenience.
#'
#' @noRd
# TWO TYPES. Neither models residual dependence INSIDE one network, so the
# single-network refusal below says that rnetis cannot compute it rather than
# pointing at an estimator.
.influence_vcov_types <- c("model", "cluster")

#' The two clustering levels answer DIFFERENT questions, and must never be
#' presented as alternatives to one another
#'
#' `"group"` is the network-level correction. Clusters are independent networks: arbitrary
#' residual dependence is permitted WITHIN each network, independence is
#' assumed BETWEEN them. This is the estimator that addresses residual network
#' dependence, and it is the reason a single network must refuse rather than
#' fall back to something else.
#'
#' `"node"` clusters on the actor. A fit gives each actor one outcome row, so its
#' clusters are singletons, and the sandwich reduces to the heteroskedasticity-
#' consistent one (HC0 at `CR0`, HC2 at `CR2`): it permits each row its own
#' variance and no covariance between any two rows. It assumes ACTORS ARE
#' INDEPENDENT, which is precisely the assumption residual network dependence
#' violates. So node clustering is NOT network-robust and is NOT a single-network
#' substitute for `"group"`.
#'
#' @noRd
.influence_cluster_levels <- c("group", "node")

#' Below this many clusters, CR0 badly overstates precision
#'
#' 30 is a package convention, not a published cutoff.
#'
#' The conventional sandwich's outer variance is estimated from variation
#' across clusters, so with few of them it is downward-biased -- often
#' severely. Two things happen below this count rather than one: `auto`
#' selects CR2, the bias-reduced correction, and the result is FLAGGED so the
#' caller can say the design is thin. The flag survives the correction on
#' purpose -- CR2 improves the estimator, it does not turn six networks into
#' many, and the Satterthwaite degrees of freedom are the other half of that
#' story.
#'
#' @noRd
.CLUSTER_SMALL_G <- 30L

#' Compute a requested variance matrix for an Influence fit
#'
#' @param object a `netis_influence` fit.
#' @param type one of `.influence_vcov_types`.
#' @return a variance matrix with the fit's own coefficient names, carrying a
#'   `netis_vcov_type` attribute and, for a robust type, the cluster count.
#' @noRd
.influence_vcov <- function(object, type = "model", by = "group", cr_type = "auto") {
  .assert_enum(type, .influence_vcov_types, "vcov")
  if (identical(type, "model")) {
    return(structure(object$vcov, netis_vcov_type = "model"))
  }
  .influence_vcov_cluster(object, by = by, cr_type = cr_type)
}

#' Clustered sandwich variance
#'
#' Routed by what produced the coefficients, because the bread has to match
#' the estimating equations. An OLS fit's CR0 is
#' `(X'X)^-1 (sum_g X_g' e_g e_g' X_g) (X'X)^-1` and rnetis computes it itself;
#' a mixed fit solves a generalized-least-squares system whose bread is
#' `(X'V^-1 X)^-1`, and that route goes through `clubSandwich`, which
#' implements it correctly rather than rnetis reimplementing it.
#'
#' Two refusals rather than a number:
#'
#' - **one cluster.** A single network supplies one term in the outer sum and
#'   the estimator is not defined; the honest pointer is a method that models
#'   dependence within a network rather than across replicates of it.
#' - **an estimator whose residuals belong to another equation.** The
#'   simultaneous network-autocorrelation route's residuals are from the
#'   filtered equation, so a sandwich built on an ordinary least-squares bread
#'   would combine two estimating equations and belong to neither.
#'
#' The **few-cluster** caveat is reported rather than silently corrected: with
#' a small number of networks the sandwich is downward-biased, and the count is
#' returned so the caller can say so.
#'
#' @noRd
.influence_vcov_cluster <- function(object, by = "group", cr_type = "auto") {
  .assert_enum(by, .influence_cluster_levels, "cluster_by")
  .assert_enum(cr_type, .influence_cr_types, "cr_type")
  backend <- object$backend
  # `lnam` needs its own refusal: its residuals are from the FILTERED equation, (I - rho W) y - X b, and the
  # bread of a clustered sandwich would have to be the simultaneous system's,
  # not (X'X)^-1. Clustering the filtered residuals with an ordinary bread
  # would produce a number belonging to neither estimator.
  if (identical(backend, "lnam")) {
    stop(
      "vcov = \"cluster\" is not available for the simultaneous network-autocorrelation route. Its residuals are from the filtered equation, (I - rho * W1) y - X beta, so a clustered sandwich built on an ordinary least-squares bread would combine two different estimating equations and belong to neither. The fit's own asymptotic covariance already accounts for the simultaneity it models; for residual dependence beyond that, no estimator here is correct and rnetis will not substitute one.",
      call. = FALSE
    )
  }

  s <- .fit_data_schema(object)
  tbl <- object$model_frame
  col <- if (identical(by, "group")) s$group_col else s$node_col
  groups <- .cluster_ids_for_rows(tbl, .cluster_design_matrix(object), s, col)
  if (is.null(groups)) {
    stop(
      sprintf("vcov = \"cluster\" could not align the estimating rows to their %s. The model-based variance is what this fit can report.", by),
      call. = FALSE
    )
  }
  ug <- unique(groups)
  if (length(ug) < 2L) {
    # THE ONE-NETWORK CASE IS THE DEFINING REFUSAL, and what it must NOT do is
    # offer node clustering as a substitute. Clustering on actors assumes
    # actors are independent clusters, which is exactly the assumption residual
    # network dependence violates -- it would answer a different question while
    # appearing to answer this one. No estimator in rnetis models dependence
    # inside a single network, so the honest answer is that rnetis cannot
    # compute this, not a number from a different model.
    stop(
      sprintf(
        "vcov = \"cluster\" with by = \"%s\" needs more than one %s to cluster on, and this fit has one. A clustered sandwich estimates its outer variance from variation ACROSS clusters, so a single cluster supplies one term and no estimate.%s",
        by, by,
        if (identical(by, "group")) {
          " No variance estimator in rnetis models residual dependence inside a SINGLE network, so rnetis reports that it cannot compute this rather than returning a number from a different model. Do NOT substitute by = \"node\": clustering on actors assumes actors are independent clusters, which is precisely what residual network dependence violates, so it answers a different question rather than a weaker version of this one."
        } else {
          " by = \"node\" treats each actor's one outcome row as its own cluster, so it needs more than one actor in the estimating sample."
        }
      ),
      call. = FALSE
    )
  }

  resolved <- .resolve_cr_type(cr_type, backend, length(ug))
  V <- if (identical(resolved, "CR0") && identical(backend, "stats")) {
    .cluster_sandwich_ols(object, groups)
  } else {
    .cluster_sandwich_club(object, groups, resolved, backend, cluster_label = by)
  }

  structure(
    V,
    netis_vcov_type = "cluster",
    netis_n_clusters = length(ug),
    netis_cluster_by = by,
    netis_cr_type = resolved,
    # What this estimator DOES and does not address, carried on the object so
    # nothing downstream has to infer it from the level name -- the whole point
    # of the group/node distinction is that it cannot be inferred safely.
    netis_addresses = if (identical(by, "group")) {
      "arbitrary residual dependence within each network, assuming independence between networks"
    } else {
      "each actor contributes one outcome row, so the clusters are singletons: a heteroskedasticity-consistent variance, assuming actors are independent of one another -- this is NOT network-robust"
    },
    netis_small_g = length(ug) < .CLUSTER_SMALL_G
  )
}

#' The fitted design matrix, used only to recover which rows survived
#'
#' @noRd
.cluster_design_matrix <- function(object) {
  tryCatch(stats::model.matrix(object$backend_fit), error = function(e) NULL)
}

#' Small-sample corrections a clustered sandwich may use
#'
#' `CR0` is the uncorrected sandwich. `CR1` applies the familiar finite-cluster
#' multiplier. `CR2` is the cluster analogue of HC2 -- the bias-reduced
#' correction, and the one with usable behavior when the clusters are few,
#' which for network data is the ordinary case rather than the exception.
#'
#' @noRd
.influence_cr_types <- c("auto", "CR0", "CR1", "CR2")

#' Choose a correction when the caller did not
#'
#' `auto` prefers CR2 wherever it is available: with few clusters the
#' uncorrected sandwich is downward-biased, and rnetis designs routinely have
#' few networks. It falls back to the dependency-free CR0 only for an
#' OLS-estimated fit, and refuses rather than silently downgrading a mixed fit
#' -- where CR0 would have to be built from the wrong bread anyway.
#'
#' @noRd
.resolve_cr_type <- function(cr_type, backend, n_clusters) {
  have_club <- requireNamespace("clubSandwich", quietly = TRUE)
  if (!identical(cr_type, "auto")) {
    if (!have_club && !(identical(cr_type, "CR0") && identical(backend, "stats"))) {
      stop(.cr_missing_package_message(cr_type, backend), call. = FALSE)
    }
    return(cr_type)
  }
  if (have_club) return("CR2")
  if (identical(backend, "stats")) return("CR0")
  stop(.cr_missing_package_message("CR2", backend), call. = FALSE)
}

#' The message a missing optional dependency earns
#'
#' Names the install, names the fallback that exists, and -- for a mixed fit --
#' says why there is no dependency-free route rather than leaving the refusal
#' looking arbitrary.
#'
#' @noRd
.cr_missing_package_message <- function(cr_type, backend) {
  sprintf(
    "vcov = \"cluster\" with cr_type = \"%s\"%s needs the 'clubSandwich' package, which is not installed. Install it with install.packages(\"clubSandwich\"), or use cr_type = \"CR0\" on an OLS-estimated fit, which rnetis computes itself.%s",
    cr_type,
    if (identical(backend, "stats")) "" else sprintf(" on a %s-backed fit", backend),
    if (identical(backend, "stats")) "" else " A mixed fit needs it either way: its sandwich must be built from the generalized-least-squares estimating equations, whose bread is (X'V^-1 X)^-1 rather than (X'X)^-1, and rnetis does not compute that itself."
  )
}

#' The dependency-free CR0 sandwich, for an OLS-estimated fit only
#'
#' `(X'X)^-1 (sum_g X_g' e_g e_g' X_g) (X'X)^-1`. Correct exactly when the
#' coefficients came from ordinary least squares -- the bread has to match the
#' estimating equations, and this one matches only OLS's.
#'
#' @noRd
.cluster_sandwich_ols <- function(object, groups) {
  X <- stats::model.matrix(object$backend_fit)
  e <- as.numeric(stats::residuals(object$backend_fit))
  bread <- tryCatch(chol2inv(chol(crossprod(X))), error = function(err) NULL)
  if (is.null(bread)) {
    stop("vcov = \"cluster\" could not invert the cross-product of the design matrix; the design is rank deficient.", call. = FALSE)
  }
  meat <- matrix(0, ncol(X), ncol(X))
  for (g in unique(groups)) {
    idx <- which(groups == g)
    sg <- crossprod(X[idx, , drop = FALSE], e[idx])
    meat <- meat + tcrossprod(sg)
  }
  V <- bread %*% meat %*% bread
  dimnames(V) <- list(colnames(X), colnames(X))
  V
}

#' The clustered sandwich via clubSandwich, with the cluster passed DELIBERATELY
#'
#' `clubSandwich` can infer a clustering from a fitted model's own structure,
#' and for an `lmerMod` that inference is the random-effects grouping factor.
#' That is not necessarily the level the caller asked to cluster on, so the
#' cluster vector is always supplied explicitly -- never omitted and never left
#' to a default.
#'
#' @noRd
.cluster_sandwich_club <- function(object, groups, cr_type, backend,
                                   cluster_label = "group") {
  V <- tryCatch(
    clubSandwich::vcovCR(object$backend_fit, cluster = groups, type = cr_type),
    error = function(e) e
  )
  if (inherits(V, "error")) {
    msg <- conditionMessage(V)
    # THE NESTING CONSTRAINT, TRANSLATED. A clustered sandwich for a mixed fit
    # is only defined when the clusters sit at or above the random-effects
    # grouping: clusters finer than the modeled grouping would split a random
    # effect across clusters, and the correction has no meaning there. That is
    # a real restriction on what can be asked, not a package quirk, so it is
    # stated in rnetis's own terms rather than passed through as backend text.
    if (grepl("[Nn]on-nested", msg)) {
      stop(
        sprintf(
          "Clustering by \"%s\" is not available for this fit: the clusters are finer than the random-effects grouping it was fitted with, so a random effect would be split across clusters and the correction is undefined. Cluster at or above the modeled grouping level -- for a fit carrying (1 | group), cluster by \"group\" -- or refit with a grouping that matches the dependence you want to allow for.",
          cluster_label
        ),
        call. = FALSE
      )
    }
    stop(
      sprintf("The clustered variance could not be computed for this %s-backed fit: %s",
              backend, msg),
      call. = FALSE
    )
  }
  out <- as.matrix(V)
  nm <- names(stats::coef(object))
  if (!is.null(nm) && identical(nrow(out), length(nm))) {
    dimnames(out) <- list(nm, nm)
  }
  out
}

#' Satterthwaite degrees of freedom for a small-cluster test
#'
#' The other half of the few-clusters problem. CR2 repairs the point of the
#' variance estimate; the reference distribution still has to stop being a
#' normal, and with a handful of networks the effective degrees of freedom can
#' be far below the cluster count. Returned as `NULL` when unavailable rather
#' than defaulting to a large-sample reference, since that default is the
#' error the correction exists to prevent.
#'
#' @noRd
.cluster_satterthwaite_df <- function(object, groups, cr_type) {
  if (!requireNamespace("clubSandwich", quietly = TRUE)) {
    return(NULL)
  }
  # THIS RECOMPUTES THE CR MATRIX THE CALLER JUST BUILT. `coef_test()` does accept
  # an already-computed `vcovCR` object in place of the type string, which would
  # halve the work -- but `.cluster_sandwich_club()` returns `as.matrix(V)`, so by
  # the time the caller has it the class carrying the cluster structure is gone and
  # clubSandwich rejects it. Threading the classed object through would change the
  # shape of the value every caller of that function receives, for one extra
  # sandwich per call.
  ct <- tryCatch(
    clubSandwich::coef_test(object$backend_fit, vcov = cr_type,
                            cluster = groups, test = "Satterthwaite"),
    error = function(e) NULL
  )
  if (is.null(ct)) return(NULL)
  nm <- intersect(c("df_Satt", "df"), names(ct))
  if (length(nm) == 0L) NULL else ct[[nm[[1]]]]
}

#' Which cluster does each row of the fitted design matrix belong to?
#'
#' `model.matrix()` drops incomplete rows, so its row names -- not positions --
#' are what map back to the assembled table. A positional join would silently
#' misalign every row after the first dropped one and every cluster sum would
#' be formed from mismatched pairs while looking healthy, the same failure
#' `.align_residuals()` guards against in the dependence family.
#'
#' @noRd
.cluster_ids_for_rows <- function(tbl, X, schema, col = NULL) {
  if (is.null(col)) col <- schema$group_col
  if (is.null(tbl) || is.null(schema) || is.null(col) || !(col %in% names(tbl))) {
    return(NULL)
  }
  nm <- rownames(X)
  idx <- if (is.null(nm)) {
    if (nrow(X) != nrow(tbl)) return(NULL)
    seq_len(nrow(tbl))
  } else {
    suppressWarnings(as.integer(nm))
  }
  if (anyNA(idx) || any(idx < 1L) || any(idx > nrow(tbl))) return(NULL)
  as.character(tbl[[col]][idx])
}

#' The schema a fit was estimated under
#'
#' `spec$data_schema`, recorded at fit time rather than recovered from the
#' data, so a saved fit can answer this without the original `netis_data`
#' object -- which is the same reason the spec carries everything else.
#'
#' @noRd
.fit_data_schema <- function(object) {
  object$spec$data_schema
}

#' Compare a robust variance against the model-based one
#'
#' The "does it matter" layer. Reports the RATIO and the percent change, and
#' deliberately does not call it attenuation: a robust standard error can come
#' out SMALLER than the model-based one, which is a reportable outcome rather
#' than an error, and "attenuation" would name only one direction.
#'
#' @param object a `netis_influence` fit.
#' @param type the robust type to compare against `"model"`.
#' @return a data frame with one row per coefficient.
#' @noRd
.influence_se_ratio <- function(object, type = "cluster", by = "group",
                                cr_type = "auto") {
  vm <- .influence_vcov(object, "model")
  vr <- .influence_vcov(object, type, by = by, cr_type = cr_type)
  keep <- intersect(rownames(vm), rownames(vr))
  if (length(keep) == 0L) {
    return(NULL)
  }
  se_m <- sqrt(diag(vm)[keep])
  se_r <- sqrt(diag(vr)[keep])
  out <- data.frame(
    term = keep,
    se_model = unname(se_m),
    se_robust = unname(se_r),
    ratio = unname(se_r / se_m),
    pct_change = unname(100 * (se_r / se_m - 1)),
    stringsAsFactors = FALSE
  )
  # The provenance rides along: which level was clustered on, how many clusters
  # the outer variance was estimated from, and whether that count is small
  # enough that the sandwich understates uncertainty. A ratio without its
  # cluster count is not interpretable.
  # Satterthwaite df, where the correction can supply them. With few networks
  # the effective degrees of freedom can fall far below the cluster count, and
  # a normal reference would undo what CR2 just bought.
  if (identical(type, "cluster")) {
    s <- .fit_data_schema(object)
    col <- if (identical(by, "group")) s$group_col else s$node_col
    cl <- .cluster_ids_for_rows(object$model_frame,
                                .cluster_design_matrix(object), s, col)
    df <- if (is.null(cl)) NULL else {
      .cluster_satterthwaite_df(object, cl, attr(vr, "netis_cr_type"))
    }
    if (!is.null(df) && length(df) == nrow(out)) out$df_satterthwaite <- df
  }
  structure(
    out,
    cluster_by = attr(vr, "netis_cluster_by"),
    n_clusters = attr(vr, "netis_n_clusters"),
    cr_type = attr(vr, "netis_cr_type"),
    addresses = attr(vr, "netis_addresses"),
    small_g = attr(vr, "netis_small_g")
  )
}

#' Robust standard errors for an Influence fit
#'
#' Recomputes the variance matrix under a different dependence assumption,
#' **without refitting**, and reports how much the standard errors move.
#'
#' # What this does and does not do
#'
#' A robust variance estimator changes the *uncertainty* around the
#' coefficients, never the coefficients themselves. If residual dependence in
#' your data reflects an omitted variable that is also related to exposure,
#' the coefficient is biased and no variance estimator repairs it — you would
#' get a more honest interval around the wrong number. Robust variance is for
#' dependence you have chosen not to model, not for confounding.
#'
#' rnetis never switches to a robust estimator on your behalf after inspecting a
#' diagnostic. Choosing the variance estimator by looking at the data makes the
#' resulting interval's coverage depend on that choice, so the procedure has
#' neither estimator's properties. Read the residual-dependence diagnostics,
#' decide, and ask for what you decided.
#'
#' # Types
#'
#' - `"model"` — the estimator's own variance.
#' - `"cluster"` — a clustered sandwich, whose meaning depends on what it
#'   clusters on. The two levels answer **different questions and are not
#'   substitutes**:
#'
#'   - `by = "group"` clusters on the **network** (in the rnetis data model a
#'     group *is* a network, since ties never cross groups). It permits
#'     arbitrary residual dependence *within* each network and assumes
#'     independence *between* them. This is the correction for residual
#'     network dependence, and it needs several independent networks — with
#'     one, it refuses rather than falling back to something else.
#'   - `by = "node"` clusters on the **actor**. Each actor contributes one
#'     outcome row, so the clusters are singletons and this is the
#'     heteroskedasticity-consistent variance (HC0 at `"CR0"`, HC2 at `"CR2"`):
#'     each row may have its own variance, and no two rows may covary. It assumes
#'     actors are independent, which is exactly what residual network dependence
#'     violates, so **it is not network-robust** and is not a single-network
#'     stand-in for `"group"`.
#'
#'   Either way the outer variance is estimated from variation across
#'   clusters, so with few clusters it is downward-biased — often severely.
#'   The count is returned as an attribute and flagged in `small_g` below 30 clusters,
#'   a package convention rather than a published cutoff.
#'
#'   Neither type models residual dependence *within* a single network. On a
#'   single network rnetis says it cannot compute a clustered variance rather
#'   than returning a number that answers a different question.
#'
#' # Small samples
#'
#' With few clusters the uncorrected sandwich understates uncertainty, so
#' `cr_type = "auto"` selects **CR2** (the cluster analogue of HC2) at any cluster
#' count wherever `clubSandwich` is installed, and reports **Satterthwaite degrees of
#' freedom** alongside — with a handful of networks the effective df can fall
#' well below the cluster count, and a normal reference would give back what
#' CR2 just bought. The small-cluster flag survives the correction on purpose:
#' CR2 improves the estimator, it does not turn six networks into many.
#'
#' # Which fits this is available for
#'
#' OLS-estimated and mixed (`lme4`-backed) fits. The bread has to match the
#' estimating equations: rnetis computes the OLS sandwich itself, while a mixed
#' fit solves a generalized-least-squares system whose bread is
#' `(X'V^-1 X)^-1` rather than `(X'X)^-1`, and that route goes through
#' `clubSandwich` rather than being reimplemented here. Without that package
#' installed, the mixed route refuses with an install pointer instead of
#' failing, as do `"CR1"` and `"CR2"` on an OLS fit; the dependency-free
#' `cr_type = "CR0"` is available for OLS fits. On a mixed fit `by = "node"` is
#' refused, because actors are finer than the random-effects grouping. The
#' simultaneous network-autocorrelation (`lnam`) route refuses both levels.
#'
#' @param object a fitted [fit_influence()] model.
#' @param type `"cluster"` (default) or `"model"`, which compares the model-based
#'   variance with itself and carries none of the cluster attributes below.
#' @param by the clustering level: `"group"` (the network, default — the
#'   network-dependence correction) or `"node"` (the actor — with one row per
#'   actor, a heteroskedasticity-consistent variance, *not* network-robust). See
#'   above; these are different corrections, not alternatives.
#' @param cr_type the small-sample correction: `"auto"` (default — CR2 where
#'   available, else the dependency-free CR0 on an OLS fit), or one of
#'   `"CR0"`, `"CR1"`, `"CR2"` explicitly.
#' @return a data frame with one row per coefficient: `term`, `se_model`,
#'   `se_robust`, `ratio` (robust over model-based) and `pct_change`, plus
#'   `df_satterthwaite` where the correction supplies it. A ratio below 1 means
#'   the robust standard error is *smaller*, which is a real result and not an
#'   error. Attributes carry the provenance the ratio is not interpretable
#'   without: `cluster_by`, `n_clusters`, `cr_type`, `addresses` (what this
#'   estimator does and does not permit), and `small_g` (whether the cluster
#'   count is low enough that the sandwich understates uncertainty).
#' @references
#' Liang, K.-Y. and Zeger, S. L. (1986) Longitudinal data analysis using
#' generalized linear models. *Biometrika* **73**(1), 13-22.
#'
#' Bell, R. M. and McCaffrey, D. F. (2002) Bias reduction in standard errors for
#' linear regression with multi-stage samples. *Survey Methodology* **28**(2),
#' 169-181. For CR2.
#'
#' Pustejovsky, J. E. and Tipton, E. (2018) Small-sample methods for
#' cluster-robust variance estimation and hypothesis testing in fixed effects
#' models. *Journal of Business & Economic Statistics* **36**(4), 672-683. For the
#' Satterthwaite degrees of freedom.
#'
#' Pustejovsky, J. E. (2026) *clubSandwich: Cluster-Robust (Sandwich) Variance
#' Estimators with Small-Sample Corrections*. R package version 0.7.0.
#' @seealso [fit_influence()], [netis_assumptions()] -- assumption A3 is the
#'   one this bears on
#' @examples
#' # Three networks, so there are several clusters to estimate the outer
#' # variance from. Clustering on the network is what permits arbitrary
#' # residual dependence inside each one.
#' set.seed(1)
#' grp <- rep(c("g1", "g2", "g3"), each = 12)
#' ids <- paste0(grp, "_", rep(letters[1:6], times = 6))
#' nodes <- data.frame(
#'   group = grp, node = ids, wave = rep(rep(1:2, each = 6), 3),
#'   y = stats::rnorm(36), x = stats::rnorm(36)
#' )
#' ties <- do.call(rbind, lapply(c("g1", "g2", "g3"), function(g) {
#'   a <- paste0(g, "_", letters[1:6])
#'   data.frame(group = g, source = a[-6], target = a[-1], wave = 1, tie = 1)
#' }))
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- fit_influence(y ~ ar() + exposure(x, via = tie) + (1 | group),
#'                      data = nd)
#' influence_se_comparison(fit, by = "group")
#' @export
influence_se_comparison <- function(object, type = "cluster", by = "group",
                                    cr_type = "auto") {
  if (!inherits(object, "netis_influence")) {
    stop("object must be a fitted netis_influence model.", call. = FALSE)
  }
  .assert_enum(type, .influence_vcov_types, "type")
  .assert_enum(by, .influence_cluster_levels, "by")
  .assert_enum(cr_type, .influence_cr_types, "cr_type")
  out <- .influence_se_ratio(object, type, by = by, cr_type = cr_type)
  if (is.null(out)) {
    stop("No coefficients are shared between the two variance matrices.", call. = FALSE)
  }
  out
}
