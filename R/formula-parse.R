# Formula parsing for fit_influence().
#
# Uses stats::terms(formula, specials = ...) -- the standard R mechanism other
# packages use for exactly this (mgcv's s(), survival's strata()/cluster()) --
# rather than a hand-rolled parser.
# ar()/lagged()/exposure()/latent() (R/formula-terms.R) are ordinary NSE
# functions; the extracted call for each special term is eval()'d in an
# environment binding them, which correctly captures their column-name
# arguments via substitute() without ever evaluating those names against data.

#' @noRd
.parse_influence_formula <- function(formula) {
  spec_env <- list2env(
    list(
      ar = ar, lagged = lagged, exposure = exposure, latent = latent,
      centrality = centrality, network_stat = network_stat,
      binarize = binarize, symmetrize = symmetrize, multiplex = multiplex,
      presence = presence, nonzero = nonzero, positive = positive,
      threshold = threshold, abs_threshold = abs_threshold,
      quantile_threshold = quantile_threshold, above_mean = above_mean,
      above_median = above_median, custom_binarize_rule = custom_binarize_rule,
      inverse = inverse,
      # Mechanism kernel constructors. Every package function a term argument
      # may legitimately name is bound HERE, in the child, rather than left to
      # be found further up, so exposure(mechanism = bi(...)), which is
      # documented usage, always finds bi().
      #
      # Binding them in the child is also what makes them WIN. A user with an
      # object called `exposure` or `bi` in scope cannot shadow the term
      # constructor, whatever the parent contains.
      bi = bi, bi_components = bi_components
    ),
    # THE PARENT IS THE FORMULA'S OWN ENVIRONMENT, which is ordinary R formula
    # semantics -- `y ~ f(x)` has always been evaluated where the formula was
    # written. A sandboxed parent such as baseenv() would mean a term argument
    # could not reference ANY user value: `latent(control = my_settings)`,
    # `exposure(normalize = my_choice)` and a user-defined mechanism kernel
    # would all fail with "object not found", for a formula that reads exactly
    # like every other formula in R. The bindings above are what keep rnetis's own
    # constructors from being shadowed.
    parent = environment(formula) %||% parent.frame()
  )

  tt <- stats::terms(formula, specials = c("ar", "lagged", "exposure", "latent", "centrality", "network_stat"))

  # Indexing convention verified empirically against stats::terms(), not
  # assumed: as.list(attr(tt, "variables"))[-1] drops the leading `list`
  # symbol, leaving element 1 = response, element k = the (k-1)th formula
  # term -- and attr(tt, "specials")/attr(tt, "response") index into that
  # same de-listed vector, i.e. response = 1, first term = 2, etc.
  var_list <- as.list(attr(tt, "variables"))[-1]

  response_idx <- attr(tt, "response")
  if (response_idx == 0L) {
    stop("fit_influence() requires a response variable in the formula (outcome ~ ...).", call. = FALSE)
  }
  outcome <- deparse(var_list[[response_idx]])

  specials <- attr(tt, "specials")
  ar_idx <- specials$ar
  lagged_idx <- specials$lagged
  exposure_idx <- specials$exposure
  latent_idx <- specials$latent
  centrality_idx <- specials$centrality
  network_stat_idx <- specials$network_stat

  term_idx <- setdiff(seq_along(var_list), response_idx)
  covariate_idx <- setdiff(term_idx, c(ar_idx, lagged_idx, exposure_idx, latent_idx, centrality_idx, network_stat_idx))

  # Two-way moderation. Resolved BEFORE the completeness check below, so a
  # hierarchical, correctly-paired interaction is accounted for rather than
  # reported as an unrecognized construction -- and resolved here rather than
  # after ar_terms/exposure_terms exist, because THIS check has to run before
  # anything downstream treats the partition as complete (the same ordering
  # constraint the completeness check itself is bound by). Hierarchy violations
  # and unsupported moderand pairings stop() immediately, by name; a three-way-
  # or-higher interaction is simply left unresolved for
  # .assert_terms_all_consumed() to report.
  interaction_families <- list(
    ar = ar_idx, lagged = lagged_idx, exposure = exposure_idx, latent = latent_idx,
    centrality = centrality_idx, network_stat = network_stat_idx, covariate = covariate_idx
  )
  interactions <- .resolve_influence_interactions(tt, var_list, interaction_families)

  # Nothing the user wrote may be discarded. `term_idx` is every non-response
  # variable, and by this line each one is destined for a special family or
  # for the covariate bucket -- so it is exactly the consumed set, and any
  # term label it does not account for -- beyond the two-way interactions just
  # resolved -- is a construction this parser never saw. Checked here rather
  # than later because everything downstream reads the partition as though it
  # were complete.
  .assert_terms_all_consumed(tt, term_idx, family = "influence",
                             extra_consumed_labels = interactions$consumed_labels)
  .assert_intercept_retained(tt, "fit_influence()")

  # ar() takes no variable argument. This is checked on the *unevaluated*
  # call, before eval() ever touches it -- evaluating a bare column-name symbol
  # like `ar(y)`/`ar(x)` would otherwise fail with R's generic
  # "object not found" (the argument is looked up starting in spec_env, which
  # has no knowledge of data column names), which is true but useless.
  # Inspecting the raw call lets the error name the outcome and point at
  # lagged() instead.
  for (i in ar_idx) .assert_ar_call_has_no_variable(var_list[[i]], outcome)

  ar_terms <- lapply(ar_idx, function(i) eval(var_list[[i]], envir = spec_env))
  for (i in seq_along(ar_terms)) ar_terms[[i]]$variable <- outcome

  lagged_terms <- lapply(lagged_idx, function(i) eval(var_list[[i]], envir = spec_env))
  exposure_terms <- lapply(exposure_idx, function(i) eval(var_list[[i]], envir = spec_env))
  latent_terms <- lapply(latent_idx, function(i) eval(var_list[[i]], envir = spec_env))
  centrality_terms <- lapply(centrality_idx, function(i) eval(var_list[[i]], envir = spec_env))
  network_stat_terms <- lapply(network_stat_idx, function(i) eval(var_list[[i]], envir = spec_env))
  covariates <- vapply(covariate_idx, function(i) deparse(var_list[[i]]), character(1))

  # Default exposure column names, disambiguated the same way
  # compute_exposure() names its output column, with collisions rejected
  # explicitly rather than silently overwritten.
  exposure_names <- vapply(exposure_terms, function(e) {
    if (!is.null(e$name)) e$name else paste0("ex_", e$content, "_", .conduit_label(e$conduit))
  }, character(1))
  if (anyDuplicated(exposure_names) > 0L) {
    stop(
      sprintf(
        "Multiple exposure() terms would produce the same column name: %s. Give them distinct name = arguments.",
        paste(unique(exposure_names[duplicated(exposure_names)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  for (i in seq_along(exposure_terms)) {
    exposure_terms[[i]]$name_given <- !is.null(exposure_terms[[i]]$name)
    exposure_terms[[i]]$name <- exposure_names[i]
  }

  # Phase two of interaction resolution: attach the actual resolved, named
  # term objects now that ar_terms and exposure_terms exist. Waited until
  # here specifically because exposure_terms only just acquired its `$name`
  # above -- an interaction spec referencing an unnamed exposure term would
  # be referencing a half-built object.
  interaction_terms <- .attach_interaction_terms(
    interactions$resolved, var_list,
    ar_idx = ar_idx, ar_terms = ar_terms,
    exposure_idx = exposure_idx, exposure_terms = exposure_terms
  )

  # The same collision check for latent() terms. latent() always sets its name:
  # the base each dimension's column is built from (<name>_d1, ...), so a
  # collision here would collide every dimension column too, not just one.
  latent_names <- vapply(latent_terms, function(e) e$name, character(1))
  if (anyDuplicated(latent_names) > 0L) {
    stop(
      sprintf(
        "Multiple latent() terms would produce the same column name: %s. Give them distinct name = arguments.",
        paste(unique(latent_names[duplicated(latent_names)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # centrality()/network_stat() default names use the network's variable name
  # only when it is a bare tie-variable reference (netis_network_ref); a
  # derived (binarize()/symmetrize()/multiplex()) network has no single
  # variable name to build one from, so the default omits it and an explicit
  # name = is required on any collision -- the same disambiguation pattern
  # exposure()/latent() already use, generalized to a case they don't have
  # (an unnamed conduit).
  .netfeat_default_name <- function(term, prefix) {
    if (!is.null(term$name)) {
      return(term$name)
    }
    net <- term$network
    if (!is.null(net) && inherits(net, "netis_network_ref")) {
      paste0(prefix, "_", term$measure, "_", net$variable)
    } else {
      paste0(prefix, "_", term$measure)
    }
  }
  .assert_no_name_collision <- function(names_vec, term_label) {
    if (anyDuplicated(names_vec) > 0L) {
      stop(
        sprintf(
          "Multiple %s terms would produce the same column name: %s. Give them distinct name = arguments.",
          term_label, paste(unique(names_vec[duplicated(names_vec)]), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  centrality_names <- vapply(centrality_terms, .netfeat_default_name, character(1), prefix = "cent")
  .assert_no_name_collision(centrality_names, "centrality()")
  for (i in seq_along(centrality_terms)) centrality_terms[[i]]$name <- centrality_names[i]

  network_stat_names <- vapply(network_stat_terms, .netfeat_default_name, character(1), prefix = "net")
  .assert_no_name_collision(network_stat_names, "network_stat()")
  for (i in seq_along(network_stat_terms)) network_stat_terms[[i]]$name <- network_stat_names[i]

  list(
    outcome = outcome,
    ar = ar_terms,
    lagged = lagged_terms,
    exposures = exposure_terms,
    latent = latent_terms,
    centralities = centrality_terms,
    network_stats = network_stat_terms,
    covariates = covariates,
    interactions = interaction_terms
  )
}

#' Reject a variable argument to ar() on the unevaluated call
#'
#' Any argument that is a bare symbol (a column-name reference, e.g. the `y`
#' in `ar(y)`) and not explicitly named `lag` is a variable-argument
#' usage. A numeric literal or a `:`/`c()`/`seq()`-style
#' expression is not a symbol and passes through untouched -- `ar(1:2)` and
#' `ar(lag = 1:2)` are both valid positional/named lag specifications.
#'
#' Two distinct messages, because the two mistakes have different fixes:
#' `ar(y)` where `y` is the outcome needs no argument at all;
#' `ar(x)` where `x` is not the outcome should become `lagged(x)`.
#'
#' @noRd
.assert_ar_call_has_no_variable <- function(call_obj, outcome) {
  args <- as.list(call_obj)[-1]
  if (length(args) == 0L) {
    return(invisible(TRUE))
  }
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))
  for (i in seq_along(args)) {
    val <- args[[i]]
    if (!identical(arg_names[i], "lag") && is.symbol(val)) {
      var_text <- deparse(val)
      if (identical(var_text, outcome)) {
        stop(
          sprintf(
            "ar(%s) is not valid: ar() takes no variable argument. The autoregressive variable is always the model's outcome ('%s'), read automatically from the formula's left-hand side. Write ar() or ar(lag = ...) instead.",
            var_text, outcome
          ),
          call. = FALSE
        )
      }
      stop(
        sprintf(
          "ar(%s) is not valid: ar() takes no variable argument, and the autoregressive variable is always the model's outcome ('%s'), not '%s'. If you meant to lag a different variable, use lagged(%s) instead.",
          var_text, outcome, var_text, var_text
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}
