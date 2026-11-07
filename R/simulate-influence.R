#' Simulate data under a known linear Influence model
#'
#' Generates one or more random networks and a multi-wave outcome following
#' the canonical Influence Model, `y_t = gamma * y_(t-1) + beta *
#' exposure_(t-1) + alpha_i + c_g + error`, with `exposure` computed by the
#' package's own exposure engine (`compute_exposure()`) from the content
#' variable at `t-1` through the generated network, by default with the outcome
#' itself as the alter characteristic driving exposure.
#'
#' This is useful for parameter-recovery testing (fit a model to the
#' simulated data and check the estimates against the known parameters used
#' to generate it), teaching, and reproducible examples. It is deliberately
#' *not* independent of the exposure engine -- generating the network and
#' computing exposure through it is exactly what the exposure engine is for,
#' and reusing it here is what makes this function validate the engine end to
#' end.
#'
#' # Persistent node heterogeneity and the initial condition
#'
#' `node_sd > 0` gives each node a time-invariant effect `alpha_i` that
#' enters the outcome at every wave. Combined with a lagged outcome on the
#' right-hand side this is the *dynamic panel* structure: `y_(i,t-1)` is a
#' function of `alpha_i` by construction, so ordinary least squares and
#' ordinary mixed-effects estimation are both inconsistent for `gamma`, and
#' the inconsistency propagates into `beta`. Generating data with
#' `node_sd > 0` is how that failure mode is measured rather than assumed;
#' rnetis itself provides no dynamic-panel estimator that removes it.
#'
#' `initial_condition` controls how the wave-1 outcome relates to
#' `alpha_i`, which is not a detail -- it is the assumption dynamic-panel
#' estimators differ over:
#'
#' - `"stationary"` (default) starts the process at its closed-form AR(1)
#'   equilibrium and then **burns in** (see `burnin`) so that it reaches the
#'   actual stationary distribution of the process being generated. The
#'   closed form alone is not enough once exposure feeds the outcome back
#'   through the network: it gets the mean and the marginal variance about
#'   right and the *cross-sectional covariance* wrong, and that difference is
#'   precisely the collinearity between a node's lagged outcome and its
#'   exposure to alters' lagged outcomes. This is the assumption that
#'   system-GMM level moment conditions require (an estimator rnetis does not
#'   provide).
#' - `"independent"` draws `y_i1` independently of `alpha_i`, a deliberately
#'   non-stationary start. Estimators that assume stationarity should be
#'   expected to do worse here, and that contrast is the point.
#'
#' # Process stability
#'
#' When the exposure content is the outcome itself, iterating the model is a
#' vector autoregression with companion matrix `gamma * I + beta * W`, and
#' the generated series diverges unless that matrix's spectral radius is
#' below 1. With `normalize = "none"` the exposure term is a sum over alters,
#' so the radius grows roughly as `gamma + beta * mean_degree` -- at the
#' defaults with any appreciable density that is well above 1, and by the
#' fifth wave the outcome's variance is orders of magnitude larger than the
#' noise it was generated from. A single transition has nothing to iterate, so a
#' two-wave design does not show this.
#'
#' The spectral radius is therefore computed from the realized networks, using
#' the exposure engine's own orientation (`W[i, j]` is the weight with which
#' `j` influences `i`; an undirected tie influences both endpoints), and
#' reported as a warning when it is at or above 1 and `n_waves > 2`, naming the
#' three ways out
#' (lower `beta`, `normalize = "row"`, or lower `density`). It is a warning
#' and not an error because an explosive process is a legitimate thing to
#' generate deliberately -- for studying near-unit-root behavior, say -- and
#' refusing it would make that region unreachable.
#'
#' # Error dependence structure
#'
#' `error_structure` selects what the disturbances do, which is a different
#' question from what the regressors are built from. A regressor can be
#' network-derived while the disturbances remain conditionally independent;
#' what breaks the independence assumptions of panel estimators is dependence
#' in the error itself.
#'
#' - `"independent"`: conditionally independent disturbances.
#' - `"group_shock"`: a shared shock per (group, wave), so rows within a
#'   group-wave are correlated.
#' - `"network"`: a simultaneous-autoregressive disturbance process over the
#'   generated network, `eps = (I - error_rho * W)^-1 u` with `W` the
#'   row-normalized influence matrix in the exposure engine's orientation,
#'   giving residual network autocorrelation.
#'
#' @param n_nodes number of nodes per group.
#' @param density expected proportion of possible ties that are
#'   present (excluding self-ties), used to generate an Erdos-Renyi-style
#'   random network. A value in `(0, 1)`.
#' @param gamma the true autoregressive coefficient.
#' @param beta the true exposure coefficient. May be `0`, which is the
#'   setting under which false-positive behavior is assessed.
#' @param resid_sd standard deviation of the Gaussian outcome noise.
#' @param directed `TRUE` (default) or `FALSE` for the generated network(s).
#' @param seed optional seed passed to [set.seed()] for reproducibility, with the
#'   caller's random stream restored when the function returns. If `NULL` (the
#'   default), the draws come from the caller's stream, which they advance.
#' @section What rnetis can fit, and what it only simulates:
#' The simulator generates more designs than [fit_influence()] estimates. A
#' simulator's capability is not a widening of the estimator's scope.
#'
#' **Designs rnetis can fit.** One transition between two waves:
#' `n_waves = 2`, or any two waves of a longer simulation named with
#' `fit_influence(waves = c(from, to))`, which are then analyzed exactly as
#' two-wave data, with `y ~ ar() + exposure(y, via = tie)` (or
#' `exposure(x, via = tie)` for `exposure_content = "covariate"`). The
#' estimators that match the generating model are the two [fit_influence()]
#' resolves for such a formula: pooled least squares, and a mixed model when
#' the formula adds a group random intercept `(1 | group)` (the generating
#' model's `c_g` when `group_sd > 0`). Under the default settings
#' (`node_sd = 0`, `error_structure = "independent"`, `homophily = 0`) these
#' are fitted to the model that generated the data. The simulator generates
#' lagged exposure only, so the contemporaneous `lnam` route is not its
#' generating model.
#'
#' **Options that generate designs rnetis only simulates.** Each of these is
#' kept because it generates a design worth studying, but no rnetis estimator
#' models what it adds; fitting such data measures how the available
#' estimators behave when their model is wrong.
#' \itemize{
#'   \item `n_waves > 2`: the multi-wave process is not modeled. Each fit
#'     uses one transition; the others are present but unused.
#'   \item `burnin`: a property of the generating process only (how close the
#'     first recorded wave is to equilibrium).
#'   \item `node_sd > 0`: a persistent node effect with a lagged outcome is a
#'     dynamic panel, for which pooled and mixed estimates of `gamma` and
#'     `beta` are inconsistent; rnetis has no dynamic-panel estimator.
#'   \item `initial_condition`: matters only to dynamic-panel estimators,
#'     which rnetis does not provide.
#'   \item `error_structure = "group_shock"` or `"network"`: dependent
#'     disturbances. No rnetis estimator models them; [netis_dependence()]
#'     measures the residual dependence they leave.
#'   \item `homophily > 0`: tie formation that depends on the persistent node
#'     effect, so the network is endogenous. rnetis's estimators condition on
#'     the observed network.
#'   \item `covariate_persistence` and `node_x_cor`: shape the covariate arm
#'     (`exposure_content = "covariate"`) for dynamic-panel evaluation; with
#'     `node_x_cor != 0` and `node_sd > 0` the covariate is correlated with
#'     an omitted node effect, which no rnetis estimator removes.
#' }
#'
#' @param n_waves number of waves to generate. `2` (the default) generates
#'   a two-wave design; higher values give each node repeated
#'   outcome measurements.
#' @param n_groups number of independent groups (networks). `1` by default.
#' @param node_sd standard deviation of the time-invariant node effect
#'   `alpha_i`. `0` (the default) means no persistent node heterogeneity.
#' @param group_sd standard deviation of the time-invariant group effect
#'   `c_g`. `0` by default.
#' @param burnin number of extra waves generated and discarded before wave 1,
#'   so the process reaches its own stationary distribution. `NULL` (default)
#'   uses 50 when `initial_condition = "stationary"` and 0 when it is
#'   `"independent"`. Burn-in is skipped when the process is explosive
#'   (companion spectral radius at or above 1): `truth$burnin` then records the 0
#'   actually used, `truth$burnin_requested` the value asked for and
#'   `truth$companion_radius` the radius, and a warning (more than two waves) or a
#'   message (two waves) says so. This is not cosmetic when exposure content is the
#'   outcome: the closed-form draw gets the mean and marginal variance about
#'   right but starts every node's disturbance independent, while in
#'   equilibrium neighboring outcomes are network-correlated. Early waves are
#'   then systematically less collinear than later ones, which makes anything
#'   estimated from them depend on which transition it came from.
#' @param initial_condition `"stationary"` (default) or `"independent"`. See
#'   Persistent node heterogeneity above.
#' @param error_structure one of `"independent"` (default), `"group_shock"`,
#'   or `"network"`. See Error dependence structure above.
#' @param error_rho dependence strength for the non-independent error
#'   structures: the within-group-wave shock's share of error variance for
#'   `"group_shock"`, and the SAR disturbance parameter for `"network"`.
#'   Ignored when `error_structure = "independent"`.
#' @param exposure_content `"outcome"` (default) generates exposure to
#'   alters' values of the outcome itself (`W Y`); `"covariate"` generates
#'   exposure to alters' values of a separate exogenous covariate `x`
#'   (`W X`). These are different identification problems: with `W Y` a node's lagged
#'   outcome and its exposure are coupled through the network, and with `W X` they
#'   are not.
#' @param normalize `"none"` (default) or `"row"`, passed through to
#'   [compute_exposure()]. This is not cosmetic in a multi-wave design: with
#'   `"none"` the exposure term is a weighted *sum* over alters, so `beta`
#'   is multiplied by something that grows with degree and the generated
#'   process diverges unless `beta` is scaled down as density rises. With
#'   `"row"` it is an average, and the stability condition is close to
#'   `|gamma| + |beta| < 1` regardless of density. See Process stability.
#' @param isolate_rule what an isolate's exposure is when `normalize = "row"`
#'   leaves it undefined: `"zero"` (the default here), `"na"`, or `"error"`.
#'   Note this default differs deliberately from [compute_exposure()]'s
#'   `"na"`. In a generator an `NA` exposure does not stay local -- it enters
#'   the outcome at that wave and then every later wave through the
#'   autoregression, so a single isolate silently destroys that node's entire
#'   series. `"zero"` is also the substantively right generative reading: a
#'   node with no alters receives no influence. The choice is recorded in
#'   `truth` either way. Irrelevant under `normalize = "none"`, where an
#'   isolate's empty sum is unambiguously `0`.
#' @param node_x_cor correlation between the node effect `alpha_i` and the
#'   *persistent component* of the exogenous covariate `x_it`, in `[-1, 1]`.
#'   Only meaningful when `exposure_content = "covariate"` and `node_sd > 0`.
#' @param covariate_persistence AR(1) persistence of `x_it` around its
#'   per-node level, in `(-1, 1)`; `0.5` by default. The first value is drawn from
#'   this process's stationary distribution. The covariate is
#'   time-varying by design: a constant one would make `W X` identical at
#'   every wave, leaving a regressor with no within-unit variation, which no
#'   dynamic-panel estimator can be evaluated on.
#' @param homophily non-negative tie-formation tilt toward similarity in
#'   `alpha_i`. `0` (the default) gives an Erdos-Renyi network independent of
#'   node effects; larger values make ties more likely between nodes with
#'   similar persistent effects, which is network-formation endogeneity and
#'   is *not* addressed by any dynamic-panel estimator.
#' @param network_change `FALSE` (default) generates one static network per
#'   group, reused across waves; `TRUE` generates an independent network per
#'   (group, wave). A draw with no ties is an observed empty network, to the
#'   generating process and to a fit alike.
#'
#' @return a list with two elements: `data`, a [netis_data()] object with
#'   `n_waves` waves of the outcome `y` (plus the covariate `x` when
#'   `exposure_content = "covariate"`) and the generated network `tie`; and
#'   `truth`, a list recording the parameters used to generate it for
#'   comparison against recovered estimates. `truth` additionally carries
#'   `node_effects` and `group_effects`, the realized `alpha_i` and `c_g`
#'   (named `"group:node"` and by group),
#'   so a simulation study can condition on the heterogeneity it actually
#'   drew rather than only on the standard deviation it requested, and
#'   `burnin_requested` and `companion_radius`.
#' @references
#' Erdos, P. and Renyi, A. (1959) On random graphs I. *Publicationes Mathematicae*
#' **6**, 290-297.
#'
#' Blundell, R. and Bond, S. (1998) Initial conditions and moment restrictions in
#' dynamic panel data models. *Journal of Econometrics* **87**(1), 115-143. For
#' system GMM.
#' @seealso [compute_exposure()], [netis_data()]
#' @examples
#' sim <- simulate_influence(n_nodes = 40, gamma = 0.3, beta = 0.5, seed = 1)
#' nodes <- netis_nodes(sim$data)
#' exposure <- compute_exposure(sim$data, "y", "tie", wave = 1)
#'
#' # A recovery check using base R. Join on the node id rather than
#' # assuming a shared row order: compute_exposure() returns rows sorted by
#' # node, netis_nodes() returns them as supplied, and pairing the two
#' # positionally would silently regress each node's outcome on another
#' # node's exposure.
#' w1 <- nodes[nodes$wave == 1, c("node", "y")]
#' w2 <- nodes[nodes$wave == 2, c("node", "y")]
#' d <- merge(w1, w2, by = "node", suffixes = c("1", "2"))
#' d <- merge(d, exposure[, c("node", "ex_y_tie")], by = "node")
#' fit <- lm(y2 ~ y1 + ex_y_tie, data = d)
#' coef(fit)  # near c(Intercept ~ 0, y1 ~ 0.3, exposure ~ 0.5), up to noise
#' sim$truth
#'
#' # A five-wave, ten-group design with persistent node heterogeneity --
#' # the dynamic-panel setting. Row-normalized exposure keeps the iterated
#' # process stable; with the default normalize = "none", beta multiplies a
#' # sum over alters and the series diverges (see Process stability).
#' dyn <- simulate_influence(
#'   n_nodes = 20, n_waves = 5, n_groups = 10, node_sd = 1,
#'   normalize = "row", beta = 0.3, seed = 2
#' )
#' table(netis_nodes(dyn$data)$wave)
#' @export
simulate_influence <- function(n_nodes = 30, density = 0.15, gamma = 0.3, beta = 0.5,
                                resid_sd = 1, directed = TRUE, seed = NULL,
                                n_waves = 2, n_groups = 1, node_sd = 0, group_sd = 0,
                                initial_condition = c("stationary", "independent"),
                                burnin = NULL,
                                error_structure = c("independent", "group_shock", "network"),
                                error_rho = 0.5,
                                exposure_content = c("outcome", "covariate"),
                                normalize = c("none", "row"),
                                isolate_rule = c("zero", "na", "error"),
                                node_x_cor = 0, covariate_persistence = 0.5,
                                homophily = 0, network_change = FALSE) {
  # THE GLOBAL STREAM IS RESTORED ON EXIT. Calling `set.seed(seed)` and leaving it set would
  # silently reseed the caller's session, so everything drawn afterwards
  # would follow from rnetis's seed rather than the user's. Reproducibility is
  # unaffected: the same `seed` gives the same simulation, which is all the
  # argument promises.
  #
  # R/sim-seed.R holds the one definition of whether a seed argument is scoped.
  if (!is.null(seed)) {
    .sim_restore_stream_on_exit(seed, parent = environment())
  }
  .assert_flag(directed, "directed")
  .assert_flag(network_change, "network_change")
  initial_condition <- match.arg(initial_condition)
  # A closed-form stationary draw is only available for a plain AR(1). Once
  # exposure feeds the outcome back through the network the process is a VAR
  # whose stationary covariance solves V = A V A' + sigma^2 I, and no
  # element-wise formula gives it. Iterating the actual recursion does, so
  # "stationary" means "burnt in" rather than "drawn from a formula".
  if (is.null(burnin)) {
    burnin <- if (identical(initial_condition, "stationary")) 50L else 0L
  }
  .assert_count(burnin, "burnin", min = 0L)
  error_structure <- match.arg(error_structure)
  exposure_content <- match.arg(exposure_content)
  normalize <- match.arg(normalize)
  isolate_rule <- match.arg(isolate_rule)

  if (!is.numeric(density) || length(density) != 1L || density <= 0 || density >= 1) {
    stop("density must be a single number strictly between 0 and 1.", call. = FALSE)
  }
  .assert_count(n_nodes, "n_nodes", min = 2L)
  .assert_count(n_waves, "n_waves", min = 2L)
  .assert_count(n_groups, "n_groups", min = 1L)
  .assert_nonneg_scalar(node_sd, "node_sd")
  .assert_nonneg_scalar(group_sd, "group_sd")
  .assert_nonneg_scalar(resid_sd, "resid_sd")
  .assert_nonneg_scalar(homophily, "homophily")
  if (!is.numeric(node_x_cor) || length(node_x_cor) != 1L ||
      is.na(node_x_cor) || node_x_cor < -1 || node_x_cor > 1) {
    stop("node_x_cor must be a single number between -1 and 1.", call. = FALSE)
  }
  if (!is.numeric(covariate_persistence) || length(covariate_persistence) != 1L ||
      is.na(covariate_persistence) || abs(covariate_persistence) >= 1) {
    stop("covariate_persistence must be a single number with |value| < 1.", call. = FALSE)
  }
  if (identical(initial_condition, "stationary") && abs(gamma) >= 1) {
    stop(
      sprintf(
        "initial_condition = \"stationary\" requires |gamma| < 1 (the process has no stationary distribution at gamma = %s). Use initial_condition = \"independent\" to generate a non-stationary start deliberately.",
        format(gamma)
      ),
      call. = FALSE
    )
  }
  if (!identical(error_structure, "independent")) {
    if (!is.numeric(error_rho) || length(error_rho) != 1L || is.na(error_rho)) {
      stop("error_rho must be a single number.", call. = FALSE)
    }
    if (identical(error_structure, "group_shock") && (error_rho < 0 || error_rho > 1)) {
      stop(
        "With error_structure = \"group_shock\", error_rho is the shared shock's share of error variance and must be between 0 and 1.",
        call. = FALSE
      )
    }
    if (identical(error_structure, "network") && abs(error_rho) >= 1) {
      stop(
        "With error_structure = \"network\", error_rho is a SAR disturbance parameter and must satisfy |error_rho| < 1 for the process to be invertible.",
        call. = FALSE
      )
    }
  }

  ids <- as.character(seq_len(n_nodes))
  group_ids <- seq_len(n_groups)

  # Persistent heterogeneity is drawn once per node and per group and then
  # reused at every wave -- that time-invariance is precisely what makes the
  # lagged outcome endogenous, so it must not be redrawn inside the wave
  # loop.
  node_z <- stats::rnorm(n_nodes * n_groups)
  node_effects <- node_sd * node_z
  group_effects <- stats::rnorm(n_groups, sd = group_sd)
  node_key <- .netis_unit_key(rep(group_ids, each = n_nodes), rep(ids, times = n_groups))

  # x is built from the same standard normal draw that generated alpha so
  # that node_x_cor is the realized correlation between them, rather than a
  # nominal one that a second independent draw would not deliver.
  # The covariate is time-VARYING, with a persistent per-node component that
  # carries the requested correlation with the node effect. A time-invariant
  # covariate would make this arm useless for its purpose: `W X` would then be
  # identical at every wave, giving a regressor with no within-unit variation
  # at all.
  covariate_fixed <- NULL
  if (identical(exposure_content, "covariate")) {
    covariate_fixed <- node_x_cor * node_z +
      sqrt(1 - node_x_cor^2) * stats::rnorm(n_nodes * n_groups)
  }
  # The AR(1) around the level has stationary variance 1, so the first value is
  # the level plus a draw from that distribution. Starting at the level itself
  # would make the first waves' variance grow toward the stationary one, a
  # transient no burn-in removes under initial_condition = "independent".
  covariate <- if (is.null(covariate_fixed)) {
    NULL
  } else {
    covariate_fixed + stats::rnorm(length(covariate_fixed))
  }
  .draw_covariate <- function(prev) {
    if (is.null(covariate_fixed)) {
      return(NULL)
    }
    # AR(1) around the node's persistent level, so the persistent part keeps
    # its correlation with alpha while the series still moves over time.
    covariate_fixed + covariate_persistence * (prev - covariate_fixed) +
      sqrt(1 - covariate_persistence^2) * stats::rnorm(length(covariate_fixed))
  }

  ties <- .simulate_networks(
    ids = ids, group_ids = group_ids, density = density, directed = directed,
    node_effects = node_effects, homophily = homophily,
    n_waves = n_waves, network_change = network_change
  )
  content_var <- if (identical(exposure_content, "outcome")) "y" else "x"
  content_static <- identical(exposure_content, "covariate")

  # The companion matrix's spectral radius governs two separate things: whether
  # to warn, and whether burning in is even meaningful. Computed once so the
  # two cannot disagree. With exposure to an exogenous covariate there is no
  # feedback through the network and the recursion is a plain AR(1).
  radius <- if (identical(exposure_content, "outcome")) {
    .companion_radius(ties, ids, group_ids, gamma, beta, normalize, network_change,
                      directed = directed)
  } else {
    abs(gamma)
  }
  if (n_waves > 2L && radius >= 1) {
    .warn_explosive(radius, beta)
  }
  # An explosive process has no stationary distribution to burn in *to*:
  # iterating it just runs the outcome off to infinity, which would corrupt
  # every wave rather than equilibrate them. Skip it, and say so, rather than
  # silently producing a diverged series.
  burnin_requested <- burnin
  if (burnin > 0L && radius >= 1) {
    burnin <- 0L
    # With more than two waves .warn_explosive() has said so. With two, a process
    # that diverges only when iterated is fine to generate, so this is a message:
    # wave 1 is the closed-form draw, which is not a stationary distribution.
    if (n_waves <= 2L) {
      message(sprintf(
        "This process has no stationary distribution (companion spectral radius %.2f), so burn-in was skipped and wave 1 is the closed-form draw. `truth$burnin` records 0.",
        radius
      ))
    }
  }

  # Wave 1. Under "stationary" the initial draw carries the persistent
  # effects through the process's own long-run variance, so y_i1 already
  # reflects alpha_i -- which is what makes the initial-conditions problem
  # real rather than an artifact of an arbitrary starting value.
  fixed_part <- node_effects + rep(group_effects, each = n_nodes)
  if (identical(initial_condition, "stationary")) {
    stationary_sd <- resid_sd / sqrt(1 - gamma^2)
    y_prev <- fixed_part / (1 - gamma) + stats::rnorm(n_nodes * n_groups, sd = stationary_sd)
  } else {
    y_prev <- stats::rnorm(n_nodes * n_groups, sd = resid_sd)
  }

  # Burn-in. The closed-form draw above gets the mean and the marginal
  # variance approximately right, but not the *cross-sectional covariance*:
  # it starts every node's disturbance independent, whereas in equilibrium
  # neighboring nodes' outcomes are correlated through the network. That
  # difference is invisible in the mean and the marginal SD, and it matters --
  # it is exactly the collinearity between a node's own lagged outcome and its
  # exposure to alters' lagged outcomes. Without burn-in the early waves are
  # systematically less collinear than the later ones, so anything estimated
  # from them depends on which transition it came from, and a study that
  # varies the number of waves measures that artifact rather than the
  # estimator.
  for (b in seq_len(burnin)) {
    exposure_prev <- .simulate_exposure_at(
      group_ids = group_ids, ids = ids, wave = 1L, y = y_prev,
      covariate = covariate, content_var = content_var,
      content_static = content_static, ties = ties, normalize = normalize,
      isolate_rule = isolate_rule, network_change = network_change,
      directed = directed, node_key = node_key
    )
    eps <- .simulate_errors(
      n_nodes = n_nodes, group_ids = group_ids, resid_sd = resid_sd,
      error_structure = error_structure, error_rho = error_rho,
      ties = ties, ids = ids, wave = 1L, network_change = network_change,
      directed = directed
    )
    y_prev <- gamma * y_prev + beta * exposure_prev + fixed_part + eps
    covariate <- .draw_covariate(covariate)
  }

  wave_frames <- vector("list", n_waves)
  wave_frames[[1L]] <- .simulate_wave_frame(
    group_ids, ids, wave = 1L, y = y_prev, covariate = covariate
  )

  for (w in seq.int(2L, n_waves)) {
    prev_wave <- w - 1L
    exposure_prev <- .simulate_exposure_at(
      group_ids = group_ids, ids = ids, wave = prev_wave, y = y_prev,
      covariate = covariate, content_var = content_var,
      content_static = content_static, ties = ties, normalize = normalize,
      isolate_rule = isolate_rule,
      network_change = network_change, directed = directed, node_key = node_key
    )
    eps <- .simulate_errors(
      n_nodes = n_nodes, group_ids = group_ids, resid_sd = resid_sd,
      error_structure = error_structure, error_rho = error_rho,
      ties = ties, ids = ids, wave = prev_wave, network_change = network_change,
      directed = directed
    )
    y_prev <- gamma * y_prev + beta * exposure_prev + fixed_part + eps
    covariate <- .draw_covariate(covariate)
    wave_frames[[w]] <- .simulate_wave_frame(
      group_ids, ids, wave = w, y = y_prev, covariate = covariate
    )
  }

  nodes_full <- do.call(rbind, wave_frames)
  rownames(nodes_full) <- NULL
  # Every drawn network was generated, so each is observed, ties or none.
  observed_tie_waves <- if (network_change) {
    expand.grid(group = group_ids, wave = seq_len(n_waves))
  }
  nd <- netis_data(nodes_full, ties, wave_col = "wave", directed = directed,
                   observed_tie_waves = observed_tie_waves)

  list(
    data = nd,
    truth = list(
      gamma = gamma, beta = beta, resid_sd = resid_sd,
      density = density, n_nodes = n_nodes, n_waves = n_waves, n_groups = n_groups,
      node_sd = node_sd, group_sd = group_sd, initial_condition = initial_condition,
      error_structure = error_structure, error_rho = error_rho, burnin = burnin,
      burnin_requested = burnin_requested, companion_radius = radius,
      exposure_content = exposure_content, normalize = normalize,
      isolate_rule = isolate_rule, node_x_cor = node_x_cor,
      covariate_persistence = covariate_persistence,
      homophily = homophily, network_change = network_change,
      node_effects = stats::setNames(
        node_effects,
        paste(rep(group_ids, each = n_nodes), rep(ids, times = n_groups), sep = ":")
      ),
      group_effects = stats::setNames(group_effects, as.character(group_ids))
    )
  )
}

#' Warn when the generated outcome process would diverge across waves
#'
#' Iterating `y_t = gamma*y_(t-1) + beta*W y_(t-1) + ...` is a VAR with
#' companion matrix `gamma*I + beta*W`; the series diverges when that
#' matrix's spectral radius reaches 1. The radius is computed from the
#' realized networks rather than approximated by `gamma + beta*mean_degree`,
#' because the approximation is only good for a homogeneous-degree network
#' and homophily deliberately breaks that.
#'
#' Only reachable when the exposure content is the outcome -- exposure to an
#' exogenous covariate does not feed back, so only `|gamma| < 1` governs
#' there, and that case is already covered by the stationary-start check.
#'
#' `W` is the exposure engine's canonical matrix, `W[i, j]` the weight with
#' which `j` influences `i`, built by the engine's own builder and normalizer
#' (see `.sim_canonical_W()`), so the radius describes the matrix the
#' recursion actually iterates. Row normalization is what makes orientation
#' matter: normalizing the rows of the transposed matrix gives a different
#' matrix with a different spectral radius.
#'
#' @noRd
.companion_radius <- function(ties, ids, group_ids, gamma, beta, normalize, network_change,
                              directed = TRUE) {
  n_nodes <- length(ids)
  radius <- 0
  for (g in group_ids) {
    gt <- ties[ties$group == g, , drop = FALSE]
    waves <- if (network_change) unique(gt$wave) else NA
    for (w in waves) {
      wt <- if (network_change) gt[gt$wave == w, , drop = FALSE] else gt
      w_mat <- .sim_canonical_W(wt, ids, directed, normalize)
      companion <- gamma * diag(n_nodes) + beta * w_mat
      radius <- max(radius, max(Mod(eigen(companion, only.values = TRUE)$values)))
    }
  }
  radius
}

#' The canonical influence matrix of one simulated network
#'
#' `W[i, j]` is the weight with which `j` influences `i` under the default
#' `direction = "source_influences_target"`: a tie from `source` to `target`
#' sits at `W[target, source]`. Built through the exposure engine's own
#' `.build_canonical_W()` and `.apply_normalization()` so the simulator's
#' stability check and its network-correlated errors use the orientation the
#' exposure engine uses, not a second convention. An undirected tie is
#' stored once and influences both of its endpoints; the engine mirrors it.
#' Isolates' rows are zero.
#'
#' @noRd
.sim_canonical_W <- function(wt, ids, directed, normalize) {
  wt <- wt[, c("source", "target", "tie"), drop = FALSE]
  wt$source <- as.character(wt$source)
  wt$target <- as.character(wt$target)
  W <- .build_canonical_W(wt, ids, source_col = "source", target_col = "target",
                          value_col = "tie", direction = "source_influences_target",
                          directed = directed)
  .apply_normalization(W, normalize, "zero")
}

#' Warn that the generated outcome process diverges across waves
#' @noRd
.warn_explosive <- function(radius, beta) {
  warning(
    sprintf(
      "The generated outcome process is explosive: the companion matrix (gamma * I + beta * W) has spectral radius %.2f, so the simulated outcome grows without bound across waves and its later waves carry almost no recoverable signal. Burn-in is skipped, since a process with no stationary distribution has nothing to burn in to. Reduce beta (currently %s), use normalize = \"row\" so exposure is an average over alters rather than a sum, or reduce density (currently the network's degree is what multiplies beta).",
      radius, format(beta)
    ),
    call. = FALSE
  )
  invisible(radius)
}

#' Build one wave's node rows
#'
#' Kept separate so the wave loop reads as the generative recursion it is,
#' rather than as data-frame assembly interleaved with statistics.
#'
#' @noRd
.simulate_wave_frame <- function(group_ids, ids, wave, y, covariate) {
  out <- data.frame(
    group = rep(group_ids, each = length(ids)),
    node = rep(ids, times = length(group_ids)),
    wave = as.integer(wave),
    y = y,
    stringsAsFactors = FALSE
  )
  if (!is.null(covariate)) {
    out$x <- covariate
  }
  out
}

#' Compute exposure at one wave through the package's own exposure engine
#'
#' Builds a single-wave netis_data holding the content variable's values at
#' `wave` and routes it through compute_exposure(), so the simulator
#' exercises the same code path a user's fit would. Returns exposure in the
#' (group, node) order given by `node_key`, which is the order every other
#' vector in the wave loop uses -- an alignment that is checked rather than
#' assumed, because a silent reordering here would corrupt the generated
#' outcome in a way no downstream test could attribute back to this line.
#'
#' @noRd
.simulate_exposure_at <- function(group_ids, ids, wave, y, covariate, content_var,
                                  content_static, ties, normalize, isolate_rule,
                                  network_change, directed, node_key) {
  snapshot <- data.frame(
    group = rep(group_ids, each = length(ids)),
    node = rep(ids, times = length(group_ids)),
    stringsAsFactors = FALSE
  )
  snapshot[[content_var]] <- if (content_static) covariate else y

  wave_ties <- ties
  if (network_change) {
    wave_ties <- ties[ties$wave == wave, setdiff(names(ties), "wave"), drop = FALSE]
    rownames(wave_ties) <- NULL
  }
  nd <- netis_data(snapshot, wave_ties, directed = directed)
  ex <- compute_exposure(nd, content = content_var, via = "tie",
                         normalize = normalize, isolate_rule = isolate_rule)

  ex_key <- .netis_unit_key(ex$group, ex$node)
  idx <- match(node_key, ex_key)
  if (anyNA(idx)) {
    stop(
      "Internal error in simulate_influence(): compute_exposure() did not return a row for every (group, node). Please report this as a bug.",
      call. = FALSE
    )
  }
  ex[[paste0("ex_", content_var, "_tie")]][idx]
}

#' Draw the disturbance vector under the requested dependence structure
#'
#' Returns errors in (group, node) order. The three structures are
#' deliberately parameterized to hold total error variance at `resid_sd^2`
#' where possible, so that changing `error_structure` changes the
#' *dependence* rather than confounding it with a change in noise
#' magnitude -- otherwise an estimator comparison across structures would be
#' reading a signal-to-noise difference as a dependence effect.
#'
#' @noRd
.simulate_errors <- function(n_nodes, group_ids, resid_sd, error_structure, error_rho,
                             ties, ids, wave, network_change, directed = TRUE) {
  n_groups <- length(group_ids)
  total <- n_nodes * n_groups

  if (identical(error_structure, "independent")) {
    return(stats::rnorm(total, sd = resid_sd))
  }

  if (identical(error_structure, "group_shock")) {
    shock_sd <- resid_sd * sqrt(error_rho)
    idio_sd <- resid_sd * sqrt(1 - error_rho)
    shocks <- stats::rnorm(n_groups, sd = shock_sd)
    return(rep(shocks, each = n_nodes) + stats::rnorm(total, sd = idio_sd))
  }

  # "network": eps = (I - rho * W)^-1 u with W the canonical influence matrix
  # (W[i, j] = weight with which j influences i), row-normalized within group.
  # Solved per group because the groups are separate networks with no
  # cross-group ties, which makes the full matrix block diagonal.
  out <- numeric(total)
  for (gi in seq_along(group_ids)) {
    g <- group_ids[gi]
    gt <- ties[ties$group == g, , drop = FALSE]
    if (network_change) {
      gt <- gt[gt$wave == wave, , drop = FALSE]
    }
    w <- .sim_canonical_W(gt, ids, directed, "row")
    u <- stats::rnorm(n_nodes, sd = resid_sd)
    slice <- ((gi - 1L) * n_nodes + 1L):(gi * n_nodes)
    out[slice] <- solve(diag(n_nodes) - error_rho * w, u)
  }
  out
}

#' Generate the network(s) as long tie data
#'
#' One Erdos-Renyi-style network per group, or per (group, wave) when
#' `network_change` is TRUE. With `homophily > 0` the tie probability is
#' tilted toward similarity in the node effects and then rescaled so the
#' expected density still matches `density` -- otherwise homophily and
#' density would be confounded, and a homophily sweep would silently also be
#' a density sweep.
#'
#' @noRd
.simulate_networks <- function(ids, group_ids, density, directed, node_effects,
                               homophily, n_waves, network_change) {
  n_nodes <- length(ids)
  frames <- list()
  for (gi in seq_along(group_ids)) {
    g <- group_ids[gi]
    alpha_g <- node_effects[((gi - 1L) * n_nodes + 1L):(gi * n_nodes)]
    waves <- if (network_change) seq_len(n_waves) else NA_integer_
    for (w in waves) {
      one <- .simulate_one_network(ids, density, directed, alpha_g, homophily)
      # rep() rather than a scalar: a sparse draw can yield zero edges, and
      # assigning a length-1 value into a zero-row data frame is an error
      # ("replacement has 1 row, data has 0"). An all-isolate network is a
      # legitimate structure, not a failure -- rep(g, 0) gives the empty column
      # the empty frame needs.
      one$group <- rep(g, nrow(one))
      if (network_change) one$wave <- rep(as.integer(w), nrow(one))
      frames[[length(frames) + 1L]] <- one
    }
  }
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  cols <- c("group", "source", "target", "tie")
  if (network_change) cols <- c(cols, "wave")
  out[, cols, drop = FALSE]
}

#' Generate a single random directed (or undirected) network as long tie data
#'
#' An Erdos-Renyi-style random network: each possible ordered pair (excluding
#' self-ties) is present independently with probability `density`, optionally
#' tilted toward homophily on the node effects.
#'
#' @noRd
.simulate_one_network <- function(ids, density, directed, alpha, homophily) {
  pairs <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$source != pairs$target, ]
  if (!directed) {
    pairs <- pairs[as.integer(pairs$source) < as.integer(pairs$target), ]
  }
  if (homophily > 0) {
    dissimilarity <- abs(alpha[match(pairs$source, ids)] - alpha[match(pairs$target, ids)])
    tilt <- exp(-homophily * dissimilarity)
    # Rescale so the mean tie probability is still `density`, keeping the
    # homophily and density axes separable. Cap at 1 in case an extreme tilt
    # would otherwise push individual probabilities past a valid range.
    p <- pmin(density * tilt / mean(tilt), 1)
  } else {
    p <- rep(density, nrow(pairs))
  }
  keep <- stats::runif(nrow(pairs)) < p
  pairs <- pairs[keep, , drop = FALSE]
  # rep() rather than a scalar 1: a small, sparse draw can legitimately keep
  # zero ties, and `pairs$tie <- 1` errors on a zero-row data frame instead
  # of producing the empty network that case actually calls for.
  pairs$tie <- rep(1, nrow(pairs))
  rownames(pairs) <- NULL
  pairs
}
