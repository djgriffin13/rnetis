# Internal storage split and schema construction for netis_data.
#
# THE NODES/TIES/GROUPS INTERFACE IS LOGICAL, NOT PHYSICAL. Internal storage
# may separate static from dynamic columns so static values are not physically
# duplicated at every wave. This file implements that split and its
# inverse (reconstruction), and builds the schema object that records which
# variables landed on which side.

#' Coerce a tibble (or other data.frame subclass) to a plain data frame
#'
#' The tibble boundary. rnetis accepts a tibble anywhere it accepts a data
#' frame, and stores a plain `data.frame`.
#'
#' `.maybe_tibble(df, "data.frame")` returns its input untouched, so a tibble left
#' in storage would come back from `netis_nodes(x, as = "data.frame")` still a
#' tibble. Coercing on the way in settles that at the boundary rather than at each
#' accessor: storage has one representation, and `as =` alone decides what comes
#' back out.
#' The conversion is lossless for the shapes rnetis accepts -- it drops the tibble
#' *class*, not any column or value -- so it is not a transformation
#' needing a record.
#'
#' @noRd
.as_plain_data_frame <- function(df) {
  if (is.null(df) || identical(class(df), "data.frame")) {
    return(df)
  }
  if (!is.data.frame(df)) {
    return(df)
  }
  as.data.frame(df, stringsAsFactors = FALSE)
}

#' Split a long entity-by-wave data frame into static and dynamic parts
#'
#' A column is static if, within every entity (defined by `key_cols`), its value is
#' constant across all rows for that entity -- including the trivial case of a
#' single row per entity when `wave_col` is `NULL`. This is a storage decision only:
#' `.reconstruct_long()` (see below) undoes it exactly, so no information is lost
#' and no modeling behavior depends on which side a column landed on.
#'
#' "Exactly" is meant literally, and it is what forces the `!grid_complete`
#' condition on the dynamic table below. The wave dimension may be collapsed only
#' when `schema$waves` can rebuild the rows -- true for a complete entity-by-wave
#' grid, false for a ragged one, where which entity was present at which wave is
#' recorded nowhere else.
#'
#' @param data a data frame in long entity-by-wave form
#' @param key_cols character vector of columns identifying an entity (e.g. group and
#'   node columns)
#' @param wave_col name of the wave column, or `NULL` if the data has no wave
#'   dimension
#' @param waves the dataset's wave vocabulary (`schema$waves`), built from
#'   every table. Grid completeness is judged against it, not against the waves
#'   this one table happens to cover: a table observed at waves 1 and 3 of a
#'   three-wave study is not complete, and collapsing it would assert values at
#'   wave 2 that were never recorded. `NULL` falls back to this table's own
#'   waves.
#' @return a list with `static` (one row per entity) and `dynamic` (one row per
#'   entity-wave, or `NULL` if every column was static)
#' @noRd
.split_static_dynamic <- function(data, key_cols, wave_col, waves = NULL) {
  other_cols <- setdiff(names(data), c(key_cols, wave_col))

  if (is.null(wave_col) || nrow(data) == 0L) {
    return(list(static = data, dynamic = NULL))
  }

  key <- do.call(paste, c(as.list(data[key_cols]), sep = .NETIS_KEY_SEP))

  # An entity absent from some waves carries information in its *row pattern*,
  # not only in its values -- for a sparse tie list that pattern is the
  # network's own time variation, since row presence is what defines edge
  # presence there. Collapsing such a table would keep every value intact while
  # silently replacing a sequence of per-wave networks with their union, and
  # `.reconstruct_long()` could no longer undo the split exactly. When the entity-by-wave
  # grid is incomplete, nothing is treated as static.
  n_waves <- if (is.null(waves)) length(unique(data[[wave_col]])) else length(waves)
  n_entities <- length(unique(key))
  grid_complete <- nrow(unique(data[c(key_cols, wave_col)])) == n_entities * n_waves

  is_static <- vapply(other_cols, function(col_name) {
    if (!grid_complete) {
      return(FALSE)
    }
    values_by_entity <- split(data[[col_name]], key)
    all(vapply(values_by_entity, function(v) length(unique(v)) <= 1L, logical(1)))
  }, logical(1))

  static_cols <- other_cols[is_static]
  dynamic_cols <- other_cols[!is_static]

  # Exactly one row per key by construction: every column kept here is constant
  # within key, so collapsing to unique rows cannot merge or split entities.
  static_df <- unique(data[c(key_cols, static_cols)])
  rownames(static_df) <- NULL

  # THE `!grid_complete` HALF IS NOT REDUNDANT.
  #
  # The guard above stops a ragged table's COLUMNS being classified static, but
  # a table with no columns beyond its keys has none to classify -- so that
  # guard has nothing to act on, and dropping the wave dimension would collapse
  # a roster of seven (node, wave) rows to three nodes: "this node had not
  # joined yet" would become "this node was there at every wave". That is the
  # ragged-roster / missing-value conflation rnetis keeps separate, reached
  # through the one shape the column guard cannot see.
  #
  # The rule this encodes: collapse the wave dimension only when the collapse is
  # REVERSIBLE FROM THE SCHEMA. A complete grid is -- every entity appears at
  # every wave, so `schema$waves` reconstructs the rows exactly, and dropping
  # them loses nothing. A ragged grid is not: which entity was present at which
  # wave exists nowhere else, so the rows must be kept even when they carry no
  # values.
  dynamic_df <- if (length(dynamic_cols) > 0L || !grid_complete) {
    out <- data[c(key_cols, wave_col, dynamic_cols)]
    rownames(out) <- NULL
    out
  } else {
    NULL
  }

  list(static = static_df, dynamic = dynamic_df)
}

#' Reconstruct the logical long-format view from a static/dynamic split
#'
#' Inverse of `.split_static_dynamic()` (above). If there is no dynamic part, the static
#' table already *is* the logical view. Otherwise, static values are joined onto
#' every wave row of the dynamic table -- this is the "not physically duplicated"
#' guarantee made visible only when a user actually asks for the expanded view.
#'
#' @noRd
.reconstruct_long <- function(split, key_cols, wave_col) {
  if (is.null(split$dynamic)) {
    return(split$static)
  }
  # merge() fans static values out across every wave row that shares a key -- this
  # is where the "not physically duplicated internally" storage becomes an
  # ordinary long-format data frame again. Correct even when static has no columns
  # beyond key_cols (nothing to fan out; every dynamic row matches on key alone).
  merge(split$static, split$dynamic, by = key_cols, all = TRUE, sort = FALSE)
}

#' Current schema format version. Bump when a field is added, removed, or
#' repurposed; `.current_schema()` refuses any other version rather than reading
#' a schema whose fields it cannot vouch for.
#' @noRd
.CURRENT_SCHEMA_VERSION <- "1"

#' @noRd
.build_schema <- function(node_col, group_col, wave_col, source_col, target_col,
                           directed, self_ties, waves,
                           node_split, tie_split, group_split,
                           tie_data, node_data, group_data, tie_meta, time_meta,
                           derived_meta = NULL, observed_tie_waves = NULL) {
  var_names <- function(split, exclude) {
    list(
      static  = setdiff(names(split$static), exclude),
      dynamic = if (is.null(split$dynamic)) character(0) else setdiff(names(split$dynamic), exclude)
    )
  }

  list(
    schema_version = .CURRENT_SCHEMA_VERSION,
    node_col = node_col,
    group_col = group_col,
    wave_col = wave_col,
    source_col = source_col,
    target_col = target_col,
    directed = directed,
    self_ties = self_ties,
    # Recorded explicitly at construction rather than inferred from whichever
    # dynamic table happens to survive the static/dynamic split -- a dataset can
    # have a wave_col with every variable turning out static (e.g. genuinely
    # time-invariant node data measured at several waves), in which case no
    # dynamic table exists anywhere to recover this from.
    waves = waves,
    node_vars  = var_names(node_split, c(node_col, group_col, wave_col)),
    tie_vars   = var_names(tie_split, c(source_col, target_col, group_col, wave_col)),
    group_vars = if (is.null(group_split)) {
      list(static = character(0), dynamic = character(0))
    } else {
      var_names(group_split, c(group_col, wave_col))
    },
    # Per-tie-variable semantics: weighted, weight_semantics,
    # direction_semantics, zero_meaning, presence_mode and its resolution.
    # Built from the raw (pre-split) tie_data/node_data because presence-mode
    # inference needs actual row counts and node rosters, which the
    # static/dynamic split does not preserve in one place.
    tie_descriptors = .build_tie_descriptors(
      tie_data = tie_data, node_data = node_data,
      group_col = group_col, node_col = node_col,
      source_col = source_col, target_col = target_col, wave_col = wave_col,
      directed = directed, self_ties = self_ties, tie_meta = tie_meta
    ),
    # Where every declaration came from. A hand-built object
    # crossed no boundary, so `source` is NULL here and an importer fills it in
    # through `.record_boundary()` -- the one seam, so an importer cannot
    # produce an object that has forgotten where it came from.
    boundary = .new_boundary(),
    # The group x wave time coordinate. Read by nothing in
    # estimation; see R/data-time-index.R's header for why wave_position is
    # global while time_value is per group.
    time_index = .build_time_index(
      node_data = node_data, tie_data = tie_data, group_data = group_data,
      group_col = group_col, wave_col = wave_col, waves = waves,
      time_meta = time_meta, observed_tie_waves = observed_tie_waves
    ),
    # The (group, wave) snapshots whose network was observed, with or without
    # ties. See `.build_observed_tie_waves()`.
    observed_tie_waves = observed_tie_waves,
    # Registry of derived-variable provenance, keyed by the derived column's
    # name. Empty at construction -- populated only by
    # add_centering()/add_decomposition(), which reconstruct the netis_data
    # object through this same constructor and pass the accumulated registry
    # back in via `derived_meta` (see below) rather than through a second
    # mutation path.
    derived = if (is.null(derived_meta)) list() else derived_meta
  )
}

#' Return a netis_data object's schema, refusing a version this installation
#' does not understand
#'
#' `netis_schema()` (R/data-netis-data.R) is the one public accessor for schema
#' content. An unrecognized version is a hard error rather than a silent partial
#' read.
#'
#' @noRd
.current_schema <- function(x) {
  s <- x$schema
  if (identical(s$schema_version, .CURRENT_SCHEMA_VERSION)) {
    return(s)
  }
  stop(
    sprintf(
      "This netis_data object has schema_version = \"%s\", which this version of rnetis (schema_version \"%s\") does not understand. Reconstruct it with the current netis_data(), or install a version of rnetis that supports schema_version \"%s\".",
      s$schema_version, .CURRENT_SCHEMA_VERSION, s$schema_version
    ),
    call. = FALSE
  )
}

#' Record which (group, wave) snapshots have an observed network
#'
#' A wave-indexed tie table says which snapshots hold ties, and a sparse table
#' cannot say more: a snapshot with no rows is either a network observed to be
#' empty or a network nobody observed. This record is what tells the two apart.
#' It lists every snapshot with a tie row, plus every snapshot declared observed
#' through `netis_data(observed_tie_waves = )`, which is how an all-zero matrix or
#' an edgeless graph keeps its status through an importer.
#'
#' Read by `.network_absent_at_wave()`. `NULL` when ties are not wave-indexed: a
#' network with no per-wave existence cannot be missing at a wave.
#'
#' @param declared the validated declaration, or `NULL`.
#' @param tie_data the tie table.
#' @return a data frame with character columns `group` and `wave`, or `NULL`.
#' @noRd
.build_observed_tie_waves <- function(declared, tie_data, group_col, wave_col) {
  rows <- data.frame(
    group = as.character(tie_data[[group_col]]),
    wave = as.character(tie_data[[wave_col]]),
    stringsAsFactors = FALSE
  )
  if (!is.null(declared)) {
    rows <- rbind(rows, data.frame(
      group = as.character(declared[[group_col]]),
      wave = as.character(declared[[wave_col]]),
      stringsAsFactors = FALSE
    ))
  }
  out <- unique(rows)
  rownames(out) <- NULL
  out
}

#' Validate a `netis_data(observed_tie_waves = )` declaration
#'
#' @noRd
.assert_observed_tie_waves <- function(declared, node_data, group_col, wave_col, tie_wave) {
  if (is.null(declared)) {
    return(invisible(TRUE))
  }
  if (!is.data.frame(declared)) {
    stop("observed_tie_waves must be a data frame or NULL.", call. = FALSE)
  }
  if (is.null(wave_col)) {
    stop("observed_tie_waves names waves, but no wave_col was supplied.", call. = FALSE)
  }
  if (is.null(tie_wave)) {
    stop(
      sprintf(
        "observed_tie_waves was supplied, but tie_data has no '%s' column. A tie table with no wave column holds at every wave, so no wave of it can be unobserved.",
        wave_col
      ),
      call. = FALSE
    )
  }
  .assert_columns_present(declared, c(group_col, wave_col), "observed_tie_waves")
  if (anyNA(declared[[group_col]])) {
    stop(sprintf("observed_tie_waves has a missing '%s'.", group_col), call. = FALSE)
  }
  unknown <- setdiff(as.character(declared[[group_col]]), as.character(node_data[[group_col]]))
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "observed_tie_waves names group(s) with no node rows: %s.",
        paste(utils::head(unknown, 5), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' The `netis_data(observed_tie_waves = )` declaration that rebuilds a schema's
#' record
#'
#' For code that reconstructs an object through `netis_data()`. Wave labels return
#' to the type the schema holds them in. `NULL` when the rebuilt tie table will have
#' no wave column to read them against.
#'
#' @param s a schema.
#' @param ties the tie table being passed to the rebuild.
#' @noRd
.observed_tie_waves_declaration <- function(s, ties) {
  rec <- s$observed_tie_waves
  if (is.null(rec) || nrow(rec) == 0L || !(s$wave_col %in% names(ties))) {
    return(NULL)
  }
  out <- data.frame(group = rec$group, stringsAsFactors = FALSE)
  names(out) <- s$group_col
  out[[s$wave_col]] <- s$waves[match(rec$wave, as.character(s$waves))]
  out
}

#' A snapshot is one wave
#' @noRd
.assert_single_wave <- function(wave) {
  if (length(wave) != 1L || is.na(wave)) {
    stop(
      sprintf("wave must be a single wave label; got %s.", paste(deparse(wave), collapse = " ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Resolve a requested wave against a schema's observed waves
#'
#' Shared by every "one snapshot at a time" export path. The resolution rule
#' is the one [as_igraph()] applies to `network`/`group`/`wave`: inferred when uniquely
#' determined, an error naming the candidates otherwise. Inference is safe
#' only when it cannot be wrong -- with a single observed wave there is
#' nothing to choose between; with several, guessing would silently pick one
#' snapshot of a longitudinal process and return it as though it were the
#' whole network.
#'
#' @param schema a netis_data schema.
#' @param wave the requested wave, or `NULL` to infer.
#' @param multi_hint text appended to the multi-wave error naming the
#'   caller-appropriate way to get every wave at once. Callers differ here
#'   (`as_igraph_list()` for the igraph path, an explicit loop for adjacency
#'   export), so the shared resolver takes the hint rather than guessing it.
#' @return the resolved wave, or `NULL` if the data has no wave dimension.
#' @noRd
.resolve_wave <- function(schema, wave, multi_hint) {
  has_waves <- !is.null(schema$wave_col) && length(schema$waves) > 0L
  if (!has_waves) {
    if (!is.null(wave)) {
      stop(
        "This data has no wave dimension (no wave_col was ever supplied), so wave = ... is not applicable.",
        call. = FALSE
      )
    }
    return(NULL)
  }
  if (!is.null(wave)) {
    .assert_single_wave(wave)
    if (!(wave %in% schema$waves)) {
      stop(
        sprintf(
          "wave = %s is not one of the observed waves: %s.",
          wave, paste(schema$waves, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(wave)
  }
  if (length(schema$waves) == 1L) {
    return(schema$waves[[1]])
  }
  stop(
    sprintf(
      "This data has %d waves (%s); wave = ... must name one%s",
      length(schema$waves), paste(schema$waves, collapse = ", "), multi_hint
    ),
    call. = FALSE
  )
}

#' Subset a long table to one group, and optionally one wave
#'
#' @param df a node, tie, or group table.
#' @param schema the `netis_data` schema, supplying the group and wave columns.
#' @param group_id the group to keep, compared as character.
#' @param wave the wave to keep, or `NULL` for every wave.
#' @return `df` with rows outside the requested group (and wave) removed.
#' @noRd
.filter_group_wave <- function(df, schema, group_id, wave) {
  out <- df[as.character(df[[schema$group_col]]) == group_id, , drop = FALSE]
  if (!is.null(wave) && !is.null(schema$wave_col) && schema$wave_col %in% names(out)) {
    out <- out[out[[schema$wave_col]] == wave, , drop = FALSE]
  }
  out
}

#' Validate a `wave` argument against the schema's observed waves
#'
#' @param schema the `netis_data` schema.
#' @param wave the requested wave, or `NULL`.
#' @return `TRUE` invisibly; errors when the request and the data disagree
#'   about whether a wave dimension exists, or when the wave is not observed.
#' @noRd
.assert_wave_argument <- function(schema, wave) {
  has_waves <- !is.null(schema$waves) && length(schema$waves) > 0L
  if (has_waves && is.null(wave)) {
    stop(
      sprintf(
        "This data has a wave dimension (waves: %s) but no wave was specified. Pass wave = <one of these values> to select which wave's node characteristics and network to use.",
        paste(schema$waves, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!has_waves && !is.null(wave)) {
    stop("This data has no wave dimension (no wave_col was ever supplied), so wave = ... is not applicable.", call. = FALSE)
  }
  if (!is.null(wave)) .assert_single_wave(wave)
  if (has_waves && !is.null(wave) && !(wave %in% schema$waves)) {
    stop(
      sprintf("wave = %s is not one of the observed waves: %s.", wave, paste(schema$waves, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
