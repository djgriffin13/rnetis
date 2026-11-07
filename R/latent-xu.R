# Xu's two-stage latent-space adjustment for latent homophily.
#
# Xu, R. (2018) Alternative estimation methods for identifying contagion
#   effects in dynamic social networks: A latent-space adjusted approach.
#   Social Networks 54, 101-117.
# Xu, R. (2021) Estimating Social Influence Effects in Networks Using a Latent
#   Space Adjusted Approach in R. The R Journal 13(2), 57-69.
#
# THE PROBLEM IT ADDRESSES, in Xu's framing: influence and homophilous
# selection are entangled when an unobserved trait co-determines both, and
# that entanglement is an omitted-variable problem. If the trait drives who
# befriends whom AND what each actor does, then an alter's behavior predicts
# ego's partly through the trait, and an influence coefficient absorbs it.
#
# THE PROCEDURE, in two stages, and rnetis implements exactly these:
#
#   1. Fit a latent-space model of NETWORK FORMATION conditioning on the
#      observed selection mechanisms. Xu (2021) eq. (3):
#
#        logodds(Z_ij = 1 | c_i, c_j, x_ij, alpha, beta)
#            = alpha + beta' x_ij - |c_i - c_j|
#
#      estimated by `latentnet::ergmm()`, with the positions taken as the
#      Minimum Kullback-Leibler (MKL) estimates (Shortreed, Handcock & Hoff
#      2006), which is `fit$mkl$Z`.
#
#   2. Enter those positions into the behavioral model as ordinary
#      actor-level covariates -- proxies for the unobserved trait.
#
# WHAT THIS FILE DELIBERATELY DOES NOT DO. No joint estimation of the two
# stages, no generated-regressor correction, no propagation of stage-1
# posterior uncertainty into stage 2, no averaging over posterior draws, and
# no identification claim beyond Xu's own. The procedure is a plug-in
# adjustment and is implemented as one. Improving it would make rnetis the
# author of a different method wearing this one's citation.

#' Latent-space adjustment term for an Influence model formula
#'
#' Implements the two-stage latent-space adjusted approach of Xu (2018, 2021).
#' A latent-space model of network formation is fitted conditioning on the
#' observed selection mechanisms; its estimated actor positions are then
#' entered into the influence model as actor-level covariates, standing in for
#' an unobserved trait that drives both who befriends whom and what each actor
#' does.
#'
#' @section What it assumes, in Xu's terms:
#' The adjustment addresses the case where **an unobserved trait co-determines
#' both influence and selection**. Xu (2021) states its scope directly: it
#' "does not improve the estimation of social influence effects when the
#' unobserved traits are only present in one process but not the other". If
#' the trait you are worried about affects behavior but not tie formation --
#' or the reverse -- this does not address it.
#'
#' Three further points Xu makes about using it:
#'
#' * **Condition the selection model on the other mechanisms.** For the
#'   positions to approximate the unobserved trait, the latent-space model
#'   should control for what else drives tie formation: homophily on observed
#'   variables, and where relevant transitivity, and ego and alter effects.
#'   `homophily =` and `terms =` are how you do that, and neither has a
#'   default, because what belongs in a selection model is a question about
#'   your design.
#' * **The scale and location of the positions are arbitrary.** Latent-space
#'   positions are identified only up to rotation, reflection and translation,
#'   so their values need not resemble the unobserved trait's. Xu's point is
#'   that this does not matter for the adjustment: what is required is that
#'   actors close in latent position are close in the unobserved trait, and
#'   the influence coefficient can be consistently estimated on that basis.
#'   Do not interpret a position coefficient as the effect of the trait.
#' * **It is a plug-in adjustment.** The positions enter as ordinary
#'   covariates. rnetis does not propagate the stage-one posterior into stage
#'   two, and does not claim to; Xu's own procedure does not either.
#'
#' @section What rnetis adds, and what it does not:
#' For one network rnetis fits the same model Xu fits and extracts the same
#' quantity: the Minimum Kullback-Leibler positions (Shortreed, Handcock & Hoff,
#' 2006), which is `m1$mkl$Z` in his code. It does not jointly estimate the two
#' stages, correct for the positions being generated regressors, average over
#' posterior draws, or make any identification claim of its own.
#'
#' The network handed to `ergmm` is the declared network: a stored tie from
#' `a` to `b` is the edge `a -> b`, so directional `terms` (`nodeocov`,
#' `nodeicov`, `rsender`, `rreceiver`, ...) read the ties the way they were
#' recorded. An undirected network is passed as undirected. A valued network
#' is reduced to presence (any non-zero value is an edge), because the
#' selection model is a model of whether a relation exists, and a self-tie is not
#' part of it. A node variable named in `homophily` or quoted in `terms` is attached
#' to the network; an actor with no node row at the modeled wave then stops the
#' fit, since the term has no value for it. A derived network ([binarize()], [symmetrize()], [multiplex()]) is
#' resolved first, exactly as [exposure()] resolves it.
#'
#' The actors are those observed at the modeled wave, plus any node of the group
#' a tie at that wave reaches. A node with neither was not part of that network,
#' and adding it as an isolate would tell the selection model that it had no ties.
#' An actor with no position (observed at the outcome wave but not the modeled
#' one) has a missing position, and its row is dropped from the fit.
#'
#' @section Xu's worked example:
#' Xu (2021) works through the Teenage Friends and Lifestyle Study, using
#' `RSiena`'s `s50` networks with alcohol, smoking, drug use and sport. The
#' package's tests reproduce the procedure: the positions match
#' `latentnet::ergmm()` called directly on his specification, compared as a
#' geometry, since a latent space has no canonical orientation. His published
#' table cannot be reproduced from `RSiena` alone, because drug use and sport come
#' from separate data files that his script reads. With those files, a manual test
#' asserts his unadjusted exposure estimate of 0.15298 (p. 63). The adjusted
#' estimate depends on a stochastic first stage, so that test compares it with his
#' procedure rerun in the same session.
#'
#' @section Several groups (experimental):
#' **The multi-group form is experimental.** Xu's procedure is defined for one network.
#' This form is this package's own extension of it, has no published validation, and
#' has not been validated for statistical performance. Separate per-network fits are the
#' recommended primary analysis; compare any multi-group fit against them.
#' [fit_influence()] warns whenever it fits this form.
#'
#' With several groups rnetis fits one selection model per group (ties never cross
#' groups) and enters each group's positions as that group's own columns, zero on other
#' groups' rows. A single model then imposes the following across networks:
#'
#' * one shared exposure coefficient, and shared [ar()] and other terms;
#' * one residual variance;
#' * the same `d` and the same stage-one specification;
#' * position coefficients that are per group and not comparable across groups.
#'
#' Adjustment quality can differ between networks, and the output does not show this.
#' Small groups spend `d` of their rows on the adjustment.
#'
#' Before entering the model each group's positions are centered on that group's mean
#' position. A latent space is identified only up to rotation, reflection and
#' translation; one network's intercept absorbs the translation, but with several groups
#' the location of each group's space would otherwise leak into the group-level terms
#' (for example a `(1 | group)` random intercept), so the estimate would depend on where
#' each group's sampler happened to place its space. Centering makes the fit invariant to
#' a per-group translation, and the per-group columns make it invariant to a per-group
#' rotation or reflection. With one group, centering only moves the intercept.
#'
#' @param network the tie variable whose formation is modeled -- a bare
#'   column name, or a [binarize()]/[symmetrize()]/[multiplex()] call, exactly
#'   as [exposure()] takes it.
#' @param d dimensions of the latent space. Xu's example uses `d = 1`, chosen
#'   there to mimic a single unobserved trait; his argument extends to more.
#' @param homophily node variable names to condition the selection model on
#'   through absolute differences (`absdiff` in `ergmm` terms) -- the form
#'   Xu's own example uses. Empty by default: what belongs in the selection
#'   model is yours to decide, and guessing would put rnetis's choice inside
#'   your adjustment.
#' @param terms additional `ergmm` model terms as text, for anything
#'   `homophily` cannot express (`"nodecov('x')"`, `"gwesp(0.5, fixed = TRUE)"`,
#'   ...). Passed through verbatim, so `latentnet` decides what it accepts: it
#'   fits a dyad-dependent term such as `gwesp` by pseudolikelihood and warns, and
#'   needs its decay fixed. For example, `latent(tie, d = 1, homophily = "y",
#'   terms = c("gwesp(0.5, fixed = TRUE)", "nodecov('x')"))` gives
#'   `euclidean(d = 1) + absdiff("y") + gwesp(0.5, fixed = TRUE) + nodecov('x')`.
#' @param lag which wave's network to model, counted back from the outcome
#'   wave: a single whole number, `0` (the outcome wave) or more. `1` (the
#'   default) is the network at the wave the exposure is measured at.
#' @param name column name for the resolved positions. Defaults to
#'   `lat_<network>_<lag>`, with `_d1`, `_d2` ... appended when `d > 1`.
#' @param control a list passed to `latentnet::ergmm.control()`. The sampler's
#'   settings change what the positions are, so they are recorded on the fit, in
#'   `fit$resolved_latent[[name]]$term`.
#' @param seed the random-number seed for the stage-one sampler, or `NULL`
#'   (the default). `latentnet::ergmm()` is a Markov chain Monte Carlo
#'   sampler, so the positions depend on the random-number stream. With a
#'   seed, the stage-one fit runs from that seed and the caller's
#'   random-number stream is left exactly as it was. With `NULL`, one integer
#'   is drawn from the caller's stream and used as the seed, so the result is
#'   reproducible under the caller's own `set.seed()`. Either way the seed
#'   used is recorded on the fit (`fit$resolved_latent[[name]]$seed`) and
#'   printed by `summary()`.
#' @return a `netis_latent_term` specification. Like every other term
#'   constructor here it computes nothing; the engine resolves it.
#' @references
#' Xu, R. (2018) Alternative estimation methods for identifying contagion
#' effects in dynamic social networks: A latent-space adjusted approach.
#' *Social Networks* **54**, 101-117.
#'
#' Xu, R. (2021) Estimating Social Influence Effects in Networks Using a
#' Latent Space Adjusted Approach in R. *The R Journal* **13**(2), 57-69.
#'
#' Hoff, P. D., Raftery, A. E. and Handcock, M. S. (2002) Latent space
#' approaches to social network analysis. *Journal of the American Statistical
#' Association* **97**(460), 1090-1098.
#'
#' Krivitsky, P. N. and Handcock, M. S. (2008) Fitting position latent cluster
#' models for social networks with latentnet. *Journal of Statistical Software*
#' **24**(5).
#'
#' Shalizi, C. R. and Thomas, A. C. (2011) Homophily and contagion are generically
#' confounded in observational social network studies. *Sociological Methods &
#' Research* **40**(2), 211-239.
#'
#' Shortreed, S., Handcock, M. S. and Hoff, P. (2006) Positional estimation
#' within a latent space model for networks. *Methodology* **2**(1), 24-33.
#' @seealso [exposure()], [ar()], [fit_influence()],
#'   `vignette("latent-space")`
#' @examples
#' # A specification; nothing is fitted until fit_influence() resolves it.
#' latent(friendship, d = 1, homophily = c("alcohol", "smoke"))
#' @export
latent <- function(network, d = 1L, homophily = character(0),
                   terms = character(0), lag = 1L, name = NULL,
                   control = list(), seed = NULL) {
  conduit <- .as_conduit(.capture_network_arg(substitute(network), parent.frame()))
  if (!is.numeric(d) || length(d) != 1L || is.na(d) || d != round(d) || d < 1L) {
    stop("latent(d = ) must be a single positive integer: the number of latent dimensions.", call. = FALSE)
  }
  d <- as.integer(d)
  lag <- .assert_wave_lag(lag, "latent(lag = )", min = 0L)
  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed != round(seed))) {
    stop("latent(seed = ) must be NULL or a single integer.", call. = FALSE)
  }
  if (!is.character(homophily) || anyNA(homophily)) {
    stop("latent(homophily = ) must be a character vector of node variable names.", call. = FALSE)
  }
  if (!is.character(terms) || anyNA(terms)) {
    stop("latent(terms = ) must be a character vector of ergmm model terms, e.g. \"nodecov('x')\".", call. = FALSE)
  }
  if (!is.list(control)) {
    stop("latent(control = ) must be a list, passed to latentnet::ergmm.control().", call. = FALSE)
  }

  base <- name %||% sprintf("lat_%s_%d", .conduit_label(conduit), lag)
  structure(
    list(
      conduit = conduit, d = d, homophily = homophily, terms = terms,
      lag = lag, name = base, control = control,
      seed = if (is.null(seed)) NULL else as.integer(seed)
    ),
    class = "netis_latent_term"
  )
}

#' The `ergmm` formula this term implies
#'
#' Assembled here rather than in the resolver so that it can be shown to the
#' user and tested without fitting anything. Xu (2021) writes his as
#'
#'   `g1 ~ euclidean(d = 1) + absdiff("a") + absdiff("s") + absdiff("sp") + absdiff("d")`
#'
#' and this produces the same shape from the term's own fields.
#'
#' @param term a `netis_latent_term`.
#' @return a one-sided formula, with `.` standing for the network object the
#'   resolver substitutes.
#' @noRd
.latent_ergmm_rhs <- function(term) {
  parts <- c(
    sprintf("euclidean(d = %d)", term$d),
    if (length(term$homophily)) sprintf("absdiff(\"%s\")", term$homophily) else NULL,
    if (length(term$terms)) term$terms else NULL
  )
  paste(parts, collapse = " + ")
}

#' Column names the resolved term produces
#'
#' One per latent dimension, and -- when there is more than one group -- one
#' set per group. A single dimension keeps the bare name, so the commonest
#' case reads as one covariate rather than as the first of a series.
#'
#' THE PER-GROUP SPLIT IS NOT COSMETIC. Independently fitted latent spaces have
#' no shared orientation, so a coefficient on a pooled column would not be
#' invariant to the arbitrary rotation each group's sampler landed on. Separate
#' columns give each group its own nuisance coefficients, and the fit is then
#' invariant to rotating any group's solution.
#'
#' @param term a `netis_latent_term`.
#' @param groups every group in the data, or `NULL` for the base names alone.
#' @param group one group, to get just its own columns.
#' @return a character vector of column names.
#' @noRd
.latent_colnames <- function(term, groups = NULL, group = NULL) {
  base <- if (term$d == 1L) term$name else sprintf("%s_d%d", term$name, seq_len(term$d))
  if (is.null(groups) || length(groups) <= 1L) {
    return(base)
  }
  if (!is.null(group)) {
    return(sprintf("%s__%s", base, group))
  }
  as.vector(t(outer(base, groups, function(b, g) sprintf("%s__%s", b, g))))
}

#' Fit the stage-one selection model and return the MKL positions
#'
#' @param term a `netis_latent_term`.
#' @param adjacency the network at the resolved wave, as a square 0/1 matrix
#'   with node ids as dimnames.
#' @param node_attrs a data frame of node attributes in the same row order as
#'   `adjacency`, supplying whatever `homophily` and `terms` name. Every column
#'   becomes a vertex attribute.
#' @param directed whether the relation is directed.
#' @return a list with `positions` (a matrix, one row per actor, `d` columns),
#'   `formula` (what was fitted, as text), and `n_actors`.
#' @noRd
.latent_fit_positions <- function(term, adjacency, node_attrs, directed = TRUE) {
  if (!requireNamespace("latentnet", quietly = TRUE)) {
    stop(
      "latent() implements Xu's two-stage latent-space adjustment, whose first stage is a latent-space network model fitted by latentnet::ergmm(). The latentnet package is not installed. Install it with install.packages(\"latentnet\"), or fit without the latent() term.",
      call. = FALSE
    )
  }
  if (!requireNamespace("network", quietly = TRUE)) {
    stop(
      "latent() needs the network package to build the object latentnet::ergmm() takes. Install it with install.packages(\"network\").",
      call. = FALSE
    ) # nocov
  }

  ids <- rownames(adjacency)
  net <- network::network(adjacency, directed = directed)
  for (v in term$homophily) {
    if (!v %in% names(node_attrs)) {
      stop(
        sprintf(
          "latent(homophily = ) names '%s', which is not a node variable in this data. The selection model conditions on observed homophily, so every name has to resolve to an actor attribute.",
          v
        ),
        call. = FALSE
      )
    }
  }
  for (v in names(node_attrs)) {
    network::set.vertex.attribute(net, v, node_attrs[[v]])
  }

  rhs <- .latent_ergmm_rhs(term)
  form <- stats::as.formula(paste("net ~", rhs))
  environment(form) <- environment()

  ctrl <- if (length(term$control)) {
    do.call(latentnet::ergmm.control, term$control)
  } else {
    latentnet::ergmm.control()
  }
  fit <- latentnet::ergmm(form, control = ctrl, verbose = FALSE)

  # THE MKL POSITIONS, which is what Xu (2021) extracts (`m1$mkl$Z`). Not the
  # posterior mean, and not a per-draw sample: the Minimum Kullback-Leibler
  # positions of Shortreed, Handcock & Hoff (2006) are the point estimate the
  # published procedure uses, and substituting another would be a different
  # method under the same citation.
  Z <- fit$mkl$Z
  if (is.null(Z)) {
    stop(
      "latentnet::ergmm() returned no MKL positions for this network, so there is nothing for the adjustment to use as a proxy. This usually means the sampler did not run long enough to produce them; raise sample.size and burnin through latent(control = ).",
      call. = FALSE
    ) # nocov
  }
  Z <- as.matrix(Z)
  if (ncol(Z) != term$d) {
    stop(
      sprintf(
        "Internal error: latentnet returned %d latent dimension(s) for a term asking for %d.",
        ncol(Z), term$d
      ),
      call. = FALSE
    ) # nocov
  }
  rownames(Z) <- ids
  list(positions = Z, formula = paste("network ~", rhs), n_actors = length(ids))
}

#' Warn that a `latent()` term is being fitted across several networks
#'
#' @noRd
.warn_multigroup_latent <- function() {
  warning(
    "Multi-group latent() is experimental: it is this package's own extension of a single-network procedure and is not validated for statistical performance. The exposure coefficient is shared across networks. Compare against separate per-network fits. See ?latent.",
    call. = FALSE
  )
}

#' Resolve a `latent()` term into actor-level position columns
#'
#' ONE SELECTION MODEL PER GROUP, because in the rnetis data model a group IS a
#' network -- ties never cross groups -- so there is no single network to fit.
#' Pooling the groups into one adjacency would invent ties that do not exist
#' and place actors from unconnected networks in one latent space, where their
#' distances would be an artifact of the fit rather than a proxy for anything.
#'
#' THE CONSEQUENCE IS FOR THE ESTIMATOR, NOT ONLY FOR THE DOCUMENTATION. Each
#' group's latent space is identified only up to rotation, reflection and
#' translation (Hoff, Raftery & Handcock 2002), so group A's coordinate 1 and
#' group B's coordinate 1 are not the same quantity.
#'
#' So the columns are GROUP-SPECIFIC when there is more than one group: each
#' group contributes its own `d` columns, zero outside its own rows, and
#' therefore its own `d` nuisance coefficients. Because a linear model's fitted
#' values depend on the column SPAN and rotation preserves the span of a
#' group's block, the adjustment is invariant to each group's arbitrary
#' orientation. Translation does NOT preserve that span once other groups'
#' rows are zero: shifting group B's positions by `c` adds `c` times group B's
#' indicator, which a random group intercept does not absorb. Each group's
#' positions are therefore centered on the group's mean position before they
#' are entered, which removes the translation exactly.
#'
#' With one group the names are unchanged, which is the common case.
#'
#' THE NETWORK IS PASSED IN DECLARED ORIENTATION (row = source, column =
#' target), the convention `network::network()` reads an adjacency matrix in.
#' The exposure engine's canonical `W` is the transpose of that under the
#' default direction and is not used here.
#'
#' THE SAMPLER'S SEED. Every group's selection model is fitted from one seeded
#' stream (each group continuing where the previous one stopped, so groups do
#' not share draws), and the caller's stream is restored afterwards.
#'
#' @param term a `netis_latent_term`.
#' @param data the `netis_data`.
#' @param schema its schema.
#' @param wave the wave whose network is modeled.
#' @return a list with `columns` (a data frame keyed by group and node),
#'   `formula`, `n_actors` (per group), and `seed` (the seed the stage-one
#'   fits ran from).
#' @noRd
.resolve_latent_positions <- function(term, data, schema, wave) {
  nodes <- netis_nodes(data)
  ties <- netis_ties(data)
  groups <- sort(unique(as.character(nodes[[schema$group_col]])))
  cols <- .latent_colnames(term, groups)

  seed <- term$seed
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }
  .sim_restore_stream_on_exit(seed)

  per_group <- lapply(groups, function(gid) {
    ties_g <- .filter_group_wave(ties, schema, gid, wave)
    .assert_tie_endpoints_on_roster(
      ties_g, schema, gid,
      unique(as.character(nodes[[schema$node_col]][as.character(nodes[[schema$group_col]]) == gid]))
    )
    # The network at the modeled wave: the nodes observed there and the nodes
    # its ties reach. A group absent from the wave has no network to model.
    ids <- sort(unique(c(
      as.character(.filter_group_wave(nodes, schema, gid, wave)[[schema$node_col]]),
      as.character(ties_g[[schema$source_col]]),
      as.character(ties_g[[schema$target_col]])
    )))
    if (length(ids) == 0L) {
      out <- stats::setNames(data.frame(character(0), character(0), stringsAsFactors = FALSE),
                             c(schema$group_col, schema$node_col))
      for (nm in cols) out[[nm]] <- numeric(0)
      return(list(columns = out, n_actors = 0L))
    }

    # The adjacency the selection model is fitted to, built by the same
    # resolver every other network-reading term uses -- a plain tie variable
    # or a derived network alike -- in DECLARED orientation.
    adj <- .conduit_declared_matrix(
      term$conduit, ties_g, ids,
      source_col = schema$source_col, target_col = schema$target_col,
      data = data, group = gid, wave = wave,
      directed = isTRUE(schema$directed)
    )
    # ergmm models tie PRESENCE. A valued conduit is reduced to presence here
    # and the reduction is stated rather than assumed: Xu's selection model is
    # a logistic model of whether a relation exists. A missing tie value has no
    # presence to give, and is refused rather than read as an absence.
    if (anyNA(adj)) {
      stop(
        sprintf(
          "latent(): the network for group '%s' at wave %s has missing tie value(s). The selection model needs to know whether each tie exists, and a missing value is neither a tie nor an absence. Resolve the missing values, or use binarize() with a rule that says what they mean.",
          gid, as.character(wave)
        ),
        call. = FALSE
      )
    }
    adj[] <- as.numeric(adj != 0)
    diag(adj) <- 0

    attrs <- .latent_node_attrs(nodes, schema, gid, wave, ids,
                                union(term$homophily, .latent_terms_node_vars(term, names(nodes))))
    fit <- .latent_fit_positions(term, adj, attrs,
                                 directed = isTRUE(schema$directed))
    pos <- fit$positions[ids, , drop = FALSE]
    if (length(groups) > 1L) {
      pos <- sweep(pos, 2L, colMeans(pos), "-")
    }
    out <- data.frame(
      g = gid, n = ids, stringsAsFactors = FALSE
    )
    names(out) <- c(schema$group_col, schema$node_col)
    # EVERY group's columns exist on EVERY group's rows, zero off-block. That
    # is what makes the per-group blocks estimable as separate coefficients in
    # one design matrix, and it is the ordinary indicator-interaction encoding
    # of a group-specific slope written out explicitly rather than left to a
    # formula operator -- which keeps it visible in `model_frame` and works
    # identically under lm() and lmer().
    for (nm in cols) out[[nm]] <- 0
    mine <- .latent_colnames(term, groups, group = gid)
    for (k in seq_along(mine)) out[[mine[k]]] <- unname(pos[, k])
    list(columns = out, n_actors = fit$n_actors)
  })

  list(
    columns = do.call(rbind, lapply(per_group, function(x) x$columns)),
    formula = paste("network ~", .latent_ergmm_rhs(term)),
    n_actors = vapply(per_group, function(x) x$n_actors, integer(1)),
    seed = seed
  )
}

#' The node variables `terms =` names, which the network must carry
#'
#' An `ergmm` term reads its covariate as a vertex attribute, written as a
#' quoted name (`nodecov('x')`). Only the variables a term names are attached, so
#' an actor reached only by a tie (who has no node row) stops the fit only when
#' a term actually needs that actor's attributes.
#'
#' @param term a `netis_latent_term`.
#' @param available the node table's column names.
#' @return the names of `available` that appear quoted in `term$terms`.
#' @noRd
.latent_terms_node_vars <- function(term, available) {
  quoted <- function(v, q) any(grepl(paste0(q, v, q), term$terms, fixed = TRUE))
  available[vapply(available, function(v) quoted(v, "'") || quoted(v, "\""), logical(1))]
}

#' Node attributes for one group at one wave, in the adjacency's row order
#'
#' @noRd
.latent_node_attrs <- function(nodes, schema, gid, wave, ids, vars) {
  if (length(vars) == 0L) {
    return(data.frame(row.names = seq_along(ids)))
  }
  rows <- .filter_group_wave(nodes, schema, gid, wave)
  missing_vars <- setdiff(vars, names(rows))
  if (length(missing_vars)) {
    stop(
      sprintf(
        "latent(homophily = ) names %s, which %s not node variable(s) in this data.",
        paste(missing_vars, collapse = ", "),
        if (length(missing_vars) == 1L) "is" else "are"
      ),
      call. = FALSE
    )
  }
  # BY NAME, in the adjacency's order. The node table's row order is its own
  # and need not match the sorted id order the matrix uses.
  idx <- match(ids, as.character(rows[[schema$node_col]]))
  if (anyNA(idx)) {
    stop(
      sprintf(
        "latent(): %d actor(s) in group '%s' have no node row at the wave whose network is being modeled (%s), so the selection model has no attributes for them.",
        sum(is.na(idx)), gid, as.character(wave)
      ),
      call. = FALSE
    )
  }
  out <- rows[idx, vars, drop = FALSE]
  row.names(out) <- NULL
  out
}
