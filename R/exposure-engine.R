# The exposure resolution engine.
#
# .resolve_exposure() is the single function behind compute_exposure()
# (R/exposure-compute.R), which fit_influence() also calls for its exposure()
# terms, so a fit and a direct computation cannot compute different answers.
#
# Two context shapes are supported:
#   - "focal" only (the linear kernel): one resolved W and X for one group at
#     the requested wave.
#   - "focal" + "w_history" + "x_history" (the bi() kernel): the same, plus the
#     full chain of prior-wave networks and content values back to the
#     earliest available wave.
# These three are the whole requires vocabulary (R/exposure-spec.R); a
# mechanism declaring anything else is refused when its specification is
# built.

#' @noRd
.resolve_exposure <- function(spec, data, wave = NULL, conduit_wave = NULL) {
  .check_netis_data(data)
  if (!inherits(spec, "netis_exposure_spec")) {
    stop("spec must be a netis_exposure_spec.", call. = FALSE)
  }

  needs_history <- any(c("w_history", "x_history") %in% spec$requires)

  s <- netis_schema(data)
  .assert_wave_argument(s, wave)
  # A separately lagged conduit: the CONTENT is read at
  # `wave` and the NETWORK at `conduit_wave`. NULL means the same wave, which
  # is the common case.
  if (is.null(conduit_wave)) {
    conduit_wave <- wave
  } else {
    .assert_wave_argument(s, conduit_wave)
    if (needs_history && !identical(conduit_wave, wave)) {
      # A path-dependent mechanism defines for itself which historical
      # networks and values it reads; "the
      # conduit, but shifted" has no defined meaning inside that accumulation,
      # so the combination is refused rather than given an invented one.
      stop(
        "This mechanism accumulates over network history, so a separately ",
        "lagged conduit (conduit_wave different from wave) has no defined ",
        "meaning for it: the mechanism itself decides which historical ",
        "networks enter the accumulation. Use conduit_wave = NULL, or the ",
        "linear mechanism.", call. = FALSE)
    }
  }

  nodes_all <- netis_nodes(data)
  ties_all <- netis_ties(data)
  .assert_columns_present(nodes_all, spec$content, "node data")

  groups <- sort(unique(as.character(nodes_all[[s$group_col]])))
  pieces <- lapply(groups, function(g) {
    .resolve_exposure_group(spec, nodes_all, ties_all, s, g, wave, needs_history,
                            data, conduit_wave)
  })
  # A silent NA is much safer than a silent zero and still leaves the user to
  # work out why a column went missing, so the affected snapshots are named.
  # Absence is a property of the NETWORK, so the label names the wave the
  # network was read at -- with a separately lagged conduit that is
  # conduit_wave, and naming the content wave instead would point the user at
  # a snapshot that exists.
  absent <- groups[vapply(pieces, function(p) isTRUE(attr(p, "network_absent")),
                          logical(1))]
  absent_labels <- if (length(absent) > 0L) {
    sprintf("group %s wave %s", absent, conduit_wave)
  } else {
    character(0)
  }
  # The historical case names the wave that was missing, not the focal one: a
  # user told "group 2 wave 4" about an accumulation that failed because wave 2
  # was never recorded would look in the wrong place.
  hist_labels <- unlist(lapply(seq_along(groups), function(i) {
    aw <- attr(pieces[[i]], "network_absent_history")
    if (length(aw) == 0L) return(NULL)
    sprintf("group %s history wave %s (affecting the wave %s value)",
            groups[i], aw, wave)
  }), use.names = FALSE)

  .warn_absent_networks(c(absent_labels, hist_labels), .conduit_label(spec$conduit))

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out <- .restore_key_types(out, nodes_all, s)
  # WHICH SNAPSHOTS WENT NA, AND WHY. Without this the downstream balance
  # diagnostic attributes every undefined exposure to a missing alter value,
  # which is the wrong cause and points the user at the wrong fix.
  # Carried as an attribute
  # rather than a column so the returned frame's shape is unchanged.
  attr(out, "absent_network_groups") <- absent
  attr(out, "absent_network_history_groups") <-
    groups[vapply(pieces, function(p) length(attr(p, "network_absent_history")) > 0L,
                  logical(1))]
  out
}

#' Is this group's network absent at this wave, rather than empty?
#'
#' Absent and empty are different states and the package must not collapse
#' them. They are distinguishable only when the tie data is wave-indexed: a
#' *static* network has no per-wave existence to be missing, so the same input
#' is correct and unremarkable there, which is why the distinction is easy to miss.
#'
#' A snapshot is absent when it has no tie rows, is not in
#' `schema$observed_tie_waves`, and the same group has an observed network at
#' another wave. That establishes the network exists and this wave's observation
#' does not. A snapshot in the record with no rows is an observed empty network.
#'
#' A group with no observed network at any wave has not established that its
#' network exists at all, so "absent at this wave" cannot be inferred and it is
#' not covered. The test applies at a *historical* wave too (`.materialize_history()`), where the
#' whole accumulation becomes `NA`: a zero matrix there would cut every path
#' through that wave.
#'
#' @param ties_all every group and wave's ties, long format.
#' @param ties_focal the rows for this group and wave.
#' @param schema the data schema.
#' @param group_id,wave the snapshot being resolved.
#' @return `TRUE` when the snapshot's network is absent rather than empty.
#' @noRd
.network_absent_at_wave <- function(ties_all, ties_focal, schema, group_id, wave) {
  if (is.null(wave) || is.null(schema$wave_col) ||
        !schema$wave_col %in% names(ties_all)) {
    return(FALSE)
  }
  if (nrow(ties_focal) > 0L) {
    return(FALSE)
  }
  observed <- schema$observed_tie_waves
  in_group <- observed$group == as.character(group_id)
  any(in_group) && !(as.character(wave) %in% observed$wave[in_group])
}

#' Name the snapshots of a selection whose network is unobserved
#'
#' @param ties every group and wave's ties, long format.
#' @param schema the data schema.
#' @param groups the groups, as text.
#' @param wave the wave; `NULL` without a wave dimension.
#' @return labels such as `"group 1 wave 3"`.
#' @noRd
.unobserved_network_labels <- function(ties, schema, groups, wave) {
  if (is.null(wave) || length(groups) == 0L) {
    return(character(0))
  }
  absent <- vapply(groups, function(g) {
    .network_absent_at_wave(ties, .filter_group_wave(ties, schema, g, wave), schema, g, wave)
  }, logical(1))
  if (any(absent)) sprintf("group %s wave %s", groups[absent], format(wave)) else character(0)
}

#' Warn that an export returned no ties for a network nobody observed
#'
#' The export is an empty network object, and nothing in that object can say it
#' stands for an unobserved network rather than an observed empty one.
#'
#' @param labels from `.unobserved_network_labels()`.
#' @param what the kind of result, e.g. `"graph"`.
#' @noRd
.warn_unobserved_export <- function(labels, what) {
  if (length(labels) == 0L) {
    return(invisible(FALSE))
  }
  warning(
    sprintf(
      "The network is unobserved at %s: there are no tie rows there and no record that it was observed. The %s for %s no ties, and that does not mean no ties existed. If the network was observed and empty, declare it with netis_data(observed_tie_waves = ).",
      paste(labels, collapse = ", "), what,
      if (length(labels) == 1L) "that snapshot has" else "those snapshots have"
    ),
    call. = FALSE
  )
  invisible(TRUE)
}

#' Warn once for every snapshot whose network was absent
#'
#' An `NA` is far safer than a silent zero, but a silent `NA` is still a result
#' the user has to notice on their own. This names the snapshots so the reason
#' is visible at the point the exposure is computed, not inferred later from a
#' column of missing values.
#'
#' @noRd
.warn_absent_networks <- function(absent, conduit, quantity = "Exposure") {
  if (length(absent) == 0L) {
    return(invisible(FALSE))
  }
  warning(
    sprintf(
      "The '%s' network is absent at %d group-wave snapshot(s): %s.
%s there is NA, not 0: an unobserved network is not an empty one, and treating it as empty would attenuate the exposure toward zero with nothing reported. Where a HISTORY wave is the absent one, the consequence is larger rather than smaller -- a zero matrix severs every path through that wave, so a path-dependent mechanism loses all accumulation before it, not one term's worth. If those waves genuinely had no ties, declare them with netis_data(observed_tie_waves = ) or record them as observed zeros in the tie data; otherwise exclude those waves from the fit.",
      conduit, length(absent), paste(absent, collapse = ", "), quantity
    ),
    call. = FALSE
  )
  invisible(TRUE)
}

#' The node set an exposure's matrices are built over
#'
#' Normally the content-wave roster. A node of this group that is not observed at
#' the content wave but takes part in a tie being used -- a tie at the conduit wave,
#' or anywhere in the history a path-dependent mechanism accumulates over -- is kept
#' as a vertex, with its content value missing at any wave it was not observed.
#' Removing it would silently delete every path through it; keeping it lets the
#' missing value propagate by the package's one masking rule, so an exposure that
#' depends on it is NA rather than a wrong number.
#'
#' `netis_dependence()` reads the same set, so the operator it describes has the row
#' sums the exposure was divided by.
#'
#' @param focal_ids the nodes observed at the content wave.
#' @param ties_used the tie rows the exposure reads.
#' @param schema the data schema.
#' @param roster every node id of the group, across all waves.
#' @noRd
.exposure_matrix_ids <- function(focal_ids, ties_used, schema, roster) {
  sort(unique(c(
    focal_ids,
    intersect(c(as.character(ties_used[[schema$source_col]]),
                as.character(ties_used[[schema$target_col]])), roster)
  )))
}

#' @noRd
.resolve_exposure_group <- function(spec, nodes_all, ties_all, schema, group_id,
                                    wave, needs_history, data,
                                    conduit_wave = wave) {
  # The output has one row per node observed at the CONTENT wave, which is what
  # the estimating table joins on. The network comes from the CONDUIT wave.
  nodes_focal <- .filter_group_wave(nodes_all, schema, group_id, wave)
  focal_ids <- sort(unique(as.character(nodes_focal[[schema$node_col]])))
  roster <- sort(unique(as.character(
    nodes_all[[schema$node_col]][as.character(nodes_all[[schema$group_col]]) == group_id]
  )))

  ties_focal  <- .filter_group_wave(ties_all, schema, group_id, conduit_wave)
  hist_waves <- if (needs_history) .history_waves(schema, wave) else NULL
  ties_used <- if (needs_history) {
    .filter_group_waves(ties_all, schema, group_id, c(conduit_wave, hist_waves))
  } else {
    ties_focal
  }
  # A tie whose endpoint is not a node of this group at all cannot be placed
  # anywhere. Dropping it would change every exposure it touches with nothing
  # reported, so it is refused.
  .assert_tie_endpoints_on_roster(ties_used, schema, group_id, roster)

  ids <- .exposure_matrix_ids(focal_ids, ties_used, schema, roster)

  W <- .build_canonical_W(
    ties_focal, ids,
    source_col = schema$source_col, target_col = schema$target_col,
    value_col = spec$conduit, direction = spec$direction,
    directed = isTRUE(schema$directed),
    data = data, group = group_id, wave = conduit_wave
  )
  W <- .apply_normalization(W, spec$normalize, spec$isolate_rule,
                            check_rows = focal_ids)
  X <- .resolve_content_vector(nodes_focal, schema$node_col, spec$content, ids)

  context <- list(W = W, X = X)
  absent_history <- character(0)
  if (needs_history) {
    hist <- .materialize_history(spec, nodes_all, ties_all, schema, group_id, wave, ids, data)
    context$W_history <- hist$K
    context$X_history <- hist$e
    absent_history <- hist$absent_waves
  }

  value <- spec$mechanism$kernel(context, spec)

  # A dynamic network that is ABSENT at this wave is not an empty network at
  # this wave, and the difference is exactly rnetis's "a measured zero-weight
  # edge and a structurally absent edge are never automatically the same
  # thing". Without this, every node's exposure would come back as a clean 0 --
  # no error, no warning, the wave still listed as valid -- which attenuates
  # the exposure coefficient toward zero for that transition with nothing
  # printed: a silently wrong number, not a crash.
  #
  # Deliberately narrow. It fires only when this snapshot has no ties, is not
  # recorded as observed, AND the same group has an observed network at some
  # other wave, so the network demonstrably exists and this wave's is missing. A
  # group with no observed network at ANY wave is a different question --
  # possibly a genuinely tie-less group, possibly an unmeasured one -- and is left
  # alone rather than resolved by assumption here.
  # Only for a mechanism whose value comes FROM the focal network. A
  # path-dependent mechanism such as bi() accumulates over prior waves, so its
  # exposure at a wave with no ties of its own is not undefined -- it is the
  # accumulated history, which is the whole point of the mechanism. The B-I
  # known-answer tests are built on that fixture: a focal wave with no network
  # and an exposure that is exactly 10 + 10*delta.
  absent <- !needs_history &&
    .network_absent_at_wave(ties_all, ties_focal, schema, group_id,
                            conduit_wave)

  # And the historical counterpart. A path-dependent mechanism is immune to an
  # absent FOCAL network -- that is the point of accumulating over history -- but
  # it is not immune to an absent HISTORICAL one, where the zero-filled matrix
  # cuts every path through that wave. The accumulation over an unobserved wave
  # is undefined, so it is NA, matching the focal-wave decision rather than
  # inventing a second convention. Skipping the wave and continuing would assume
  # no ties existed then, which is exactly what the data does not support.
  if (length(absent_history) > 0L) {
    value[] <- NA_real_
  }
  if (absent) {
    value[] <- NA_real_
  }

  keep_rows <- match(focal_ids, ids)
  value <- if (is.matrix(value)) value[keep_rows, , drop = FALSE] else value[keep_rows]

  out <- data.frame(x = group_id, y = focal_ids, stringsAsFactors = FALSE)
  names(out) <- c(schema$group_col, schema$node_col)

  if (is.matrix(value)) {
    # A multi-column mechanism (e.g. bi_components()): expand into one column
    # per matrix column, named by the matrix's own colnames, prefixed by a name
    # the caller gave. There is no single output column for the name to label.
    if (is.null(colnames(value))) {
      stop(
        "A multi-column mechanism kernel must set column names on its result matrix (see bi_components() for an example).",
        call. = FALSE
      )
    }
    if (isTRUE(spec$name_given)) {
      colnames(value) <- paste0(spec$name, "_", colnames(value))
    }
    value_df <- as.data.frame(value)
    out <- cbind(out, value_df)
  } else {
    out[[spec$name]] <- value
  }
  # Carried on the piece rather than warned here, so several absent snapshots
  # produce one message naming all of them instead of one warning per group.
  attr(out, "network_absent") <- absent
  attr(out, "network_absent_history") <- absent_history
  out
}

#' Materialize network and content history for a path-dependent mechanism
#'
#' Returns the chain of prior-wave conduit matrices and content vectors needed
#' by mechanisms declaring `w_history`/`x_history` (e.g. [bi()]), ordered
#' **most-recent-first**: `K[[1]]`/`e[[1]]` is the wave immediately before
#' `focal_wave`, `K[[2]]`/`e[[2]]` the wave before that, and so on back to the
#' earliest observed wave. A focal wave with no prior waves available (the
#' first observed wave) yields empty lists, which mechanisms must treat as "no
#' history contribution" rather than an error.
#'
#' The node/actor set (`ids`) is the focal wave's roster plus every node of
#' the group that takes part in a tie anywhere in the history, and it is
#' reused for every historical wave. A node absent from a particular wave's
#' node data yields `NA` for its content value at that wave, so paths through
#' it are kept and only what genuinely depends on an unobserved value is
#' masked.
#'
#' @noRd
.materialize_history <- function(spec, nodes_all, ties_all, schema, group_id, focal_wave, ids, data = NULL) {
  # By POSITION in the ordered wave vocabulary: comparing labels with `<`
  # puts wave "10" before wave "9" once labels are text.
  prior_waves <- .history_waves(schema, focal_wave)
  K <- vector("list", length(prior_waves))
  e <- vector("list", length(prior_waves))
  absent <- character(0)
  for (i in seq_along(prior_waves)) {
    w <- prior_waves[i]
    nodes_w <- .filter_group_wave(nodes_all, schema, group_id, w)
    ties_w  <- .filter_group_wave(ties_all, schema, group_id, w)
    # THE SAME CONFLATION AS THE FOCAL WAVE, ONE LEVEL DOWN, AND WORSE.
    # `.build_canonical_W()` fills 0 for every cell with no matching row, which
    # is correct when a wave's network was observed to be empty (it is then in
    # `schema$observed_tie_waves`) and wrong when it was never observed: a zero matrix at a historical wave does not
    # attenuate one term, it severs every path through that wave, so a
    # path-dependent accumulation silently loses all history before it.
    # Recorded here and acted on by the caller, which is the only place that
    # knows whether the mechanism actually reads history.
    if (.network_absent_at_wave(ties_all, ties_w, schema, group_id, w)) {
      absent <- c(absent, as.character(w))
    }
    # The history chain uses the SAME builder as the focal wave, including for a
    # derived conduit. Two paths here would drift, and a path-dependent
    # mechanism reading a differently-constructed network at historical waves
    # than at the focal one would be wrong in a way no single-wave test sees.
    K[[i]] <- .build_canonical_W(
      ties_w, ids,
      source_col = schema$source_col, target_col = schema$target_col,
      value_col = spec$conduit, direction = spec$direction,
      directed = isTRUE(schema$directed),
      data = data, group = group_id, wave = w
    )
    e[[i]] <- .resolve_content_vector(nodes_w, schema$node_col, spec$content, ids)
  }
  list(K = K, e = e, absent_waves = absent)
}

#' Build the canonical exposure matrix for a conduit
#'
#' A composition rather than a construction: build the DECLARED-orientation matrix (`.conduit_declared_matrix()`), then
#' orient it (`.orient_canonical()`). The split is what lets `via =` accept a
#' derived network without the engine gaining a second matrix builder, and it
#' keeps the transposition in exactly one place -- see `R/exposure-conduit.R`.
#'
#' `data`/`group`/`wave` are needed only by the derived-conduit path, which
#' resolves through the shared network-construction layer. A plain-name conduit
#' ignores them.
#'
#' @param directed `TRUE` for directed data. For undirected data the declared
#'   matrix is mirrored once (`.build_declared_matrix()`), so both endpoints of
#'   a stored pair expose each other. Required: an undirected network read one
#'   way gives each tie to only one of its endpoints.
#' @noRd
.build_canonical_W <- function(ties_g, ids, source_col, target_col, value_col, direction,
                                directed, data = NULL, group = NULL, wave = NULL) {
  if (.conduit_is_derived(value_col)) {
    M <- .conduit_declared_matrix(
      value_col, ties_g, ids, source_col, target_col, data, group, wave,
      directed = directed
    )
    return(.orient_canonical(M, direction))
  }
  .build_canonical_W_from_name(ties_g, ids, source_col, target_col, value_col, direction,
                               directed)
}

#' @noRd
.build_canonical_W_from_name <- function(ties_g, ids, source_col, target_col, value_col,
                                         direction, directed) {
  # 0-fill for a cell with no matching row is correct regardless of the tie
  # variable's presence_mode: under row_presence, an absent row means no tie,
  # so 0 contribution to the weighted sum is exactly right; under
  # complete_dyadic, every dyad already has an explicit row (including measured
  # zeros), so this fill is never actually exercised there. This is a weighted-
  # sum computation, not an edge-presence resolution -- .resolve_tie_presence()
  # (R/data-tie-meta.R) is the function that needs presence_mode, and this one
  # deliberately does not call it.
  #
  # The conduit column is checked even for a group/wave with no tie rows
  # (inside .build_declared_matrix()): column names survive a zero-row
  # selection, so a misspelled conduit is caught even for an all-isolated
  # group rather than returning all-zero exposure.
  #
  # Built in DECLARED orientation (row = source, column = target, mirrored for
  # undirected data), then oriented by `.orient_canonical()` -- the one
  # transposition point.
  M <- .build_declared_matrix(ties_g, ids, source_col, target_col, value_col,
                              directed = directed)
  .orient_canonical(M, direction)
}

#' The waves before `focal_wave`, most recent first
#'
#' @noRd
.history_waves <- function(schema, focal_wave) {
  # By position in the ordered vocabulary, most recent first.
  .waves_before(schema$waves, focal_wave)
}

#' Filter a long table to one group and a set of waves
#'
#' @noRd
.filter_group_waves <- function(df, schema, group_id, waves) {
  out <- df[as.character(df[[schema$group_col]]) == group_id, , drop = FALSE]
  if (!is.null(waves) && !is.null(schema$wave_col) && schema$wave_col %in% names(out)) {
    out <- out[out[[schema$wave_col]] %in% waves, , drop = FALSE]
  }
  out
}

#' Give a computed table's identifiers the type the node table holds them in
#'
#' Per-group computation keys rows by text; a table handed back should join to
#' the user's data without a type conversion.
#'
#' @noRd
.restore_key_types <- function(out, nodes_all, schema) {
  for (col in intersect(c(schema$group_col, schema$node_col), names(out))) {
    idx <- match(as.character(out[[col]]), as.character(nodes_all[[col]]))
    if (!anyNA(idx)) {
      out[[col]] <- nodes_all[[col]][idx]
    }
  }
  out
}

#' Refuse ties that cannot be placed in the group's network
#'
#' A tie naming a node that has no row in the group's node data at any wave
#' cannot be placed in the group's network. Every consumer that builds a
#' network from tie rows refuses it here rather than leaving the tie out:
#' leaving it out changes degree, density, exposure and every other quantity
#' the tie touches, with nothing reported. `diagnose_data()` reports the same
#' condition for the whole object.
#'
#' A self-tie under `self_ties = "prohibited"` is refused for the same reason: it
#' would enter exposure and every matrix-based measure as an ordinary tie. A
#' self-tie row whose values are all zero or missing carries no tie (the diagonal
#' of a complete dyadic table) and is accepted.
#'
#' @param ties_g tie rows for one group.
#' @param schema the data schema.
#' @param group_id the group.
#' @param roster every node id of the group, across all waves.
#' @noRd
.assert_tie_endpoints_on_roster <- function(ties_g, schema, group_id, roster) {
  if (nrow(ties_g) == 0L) {
    return(invisible(TRUE))
  }
  src <- as.character(ties_g[[schema$source_col]])
  tgt <- as.character(ties_g[[schema$target_col]])
  if (identical(schema$self_ties, "prohibited") && any(src == tgt)) {
    has_tie <- .self_tie_carries_value(ties_g, schema)
    if (any(has_tie)) {
      stop(
        sprintf(
          "%d self-tie(s) in group '%s' (%s), but self_ties = \"prohibited\". A self-tie would enter the network as an ordinary tie, so a node would be exposed to its own value. Remove those rows, or declare self_ties = \"permitted\" in netis_data().",
          sum(has_tie), group_id, paste(utils::head(unique(src[has_tie]), 5), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  bad <- !(src %in% roster) | !(tgt %in% roster)
  if (!any(bad)) {
    return(invisible(TRUE))
  }
  unknown <- unique(c(src[!(src %in% roster)], tgt[!(tgt %in% roster)]))
  stop(
    sprintf(
      "%d tie(s) in group '%s' reference node(s) that do not appear in node_data for that group: %s. A tie to a node outside the roster cannot be placed in the network, and leaving it out would change every quantity it touches. Add the missing node rows, or remove those ties; diagnose_data() lists every such tie.",
      sum(bad), group_id, paste(utils::head(unknown, 5), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' @noRd
.resolve_content_vector <- function(nodes_g, node_col, content_var, ids) {
  vals <- nodes_g[[content_var]]
  names(vals) <- as.character(nodes_g[[node_col]])
  # Name-based lookup, not positional: aligns exactly to W's column order
  # (`ids`) regardless of the row order nodes_g happened to arrive in. Yields
  # NA for any id absent from nodes_g (e.g. a node not yet present at an
  # earlier historical wave).
  as.numeric(vals[ids])
}

#' @noRd
.apply_normalization <- function(W, normalize, isolate_rule, check_rows = NULL) {
  if (normalize == "none") {
    return(W)
  }
  row_sums <- rowSums(W)
  # AN UNDEFINED DENOMINATOR IS NOT AN ISOLATE. An isolate has no reference-set
  # members and `isolate_rule` governs it. Two other rows have no denominator
  # either and it does not: one whose weights are partly unknown, and one whose
  # signed weights cancel to zero while it does have alters. Their normalized
  # weights are NA. Renormalizing over the observed alters instead would be
  # imputation, and would silently change the estimand.
  undefined_rows <- is.na(row_sums) | (row_sums == 0 & rowSums(W != 0) > 0)
  zero_rows <- !undefined_rows & row_sums == 0

  # Only the rows whose exposure is reported are checked: a node that is in the
  # matrix solely as an alter (see .resolve_exposure_group()) has no exposure
  # of its own to refuse.
  refuse_rows <- zero_rows
  if (!is.null(check_rows)) {
    refuse_rows <- zero_rows & rownames(W) %in% check_rows
  }
  if (any(refuse_rows) && isolate_rule == "error") {
    stop(
      sprintf(
        "Row-normalization requested but %d node(s) have no reference-set members (isolates), so their normalized exposure denominator is undefined: %s. Set isolate_rule to \"na\" or \"zero\", or use normalize = \"none\".",
        sum(refuse_rows), paste(utils::head(rownames(W)[refuse_rows], 5), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Relies on R recycling a length-nrow(W) vector down columns of a matrix,
  # i.e. W[i, j] / row_sums[i] for every j -- ordinary row-wise division.
  W_norm <- W / row_sums

  if (any(zero_rows)) {
    if (isolate_rule %in% c("na", "error")) {
      W_norm[zero_rows, ] <- NA_real_
    } else if (isolate_rule == "zero") {
      W_norm[zero_rows, ] <- 0
    }
  }
  # Stated rather than left to arise from the division: `W / row_sums` already
  # yields NA across an undefined row, and saying so here is what stops a
  # later edit from "fixing" it into a zero.
  if (any(undefined_rows)) {
    W_norm[undefined_rows, ] <- NA_real_
  }
  W_norm
}
