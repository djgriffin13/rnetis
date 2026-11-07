# The time index.
#
# TWO COORDINATES, AND ONLY ONE OF THEM MODELS.
#
#   wave_position  1, 2, 3, ...   REQUIRED. Every estimator, ar(), lagged(),
#                                 prior_tie(), exposure history.
#   time_value     0, 7, 30 days  OPTIONAL. Interoperability, provenance,
#                                 plotting. Read by nothing in estimation.
#
# The whole claim of the time index is that this file is inert with respect to
# modeling: changing `time_value` while holding wave order fixed must move no
# estimate. Nothing here is called from the fitting path, and nothing here is
# consulted by lag resolution.
#
# WHY wave_position IS GLOBAL AND time_value IS PER GROUP.
#
# `.influence_lag_wave()` and `.selection_lag_wave()` both step through
# `schema$waves` -- the global ordered sequence (with a named transition's
# skipped waves left out, see `.lag_sequence()`) -- so a lag of 1 from wave 5
# is whatever wave precedes 5 in THAT list. wave_position is therefore an index
# into the same list, because the index whose job is to describe the modeling
# occasions must describe the ones the model actually uses. Numbering positions
# per group instead would look more natural and be wrong: a group missing wave 2
# would have its wave 3 labeled position 2 while `lagged(x, 1)` from wave 3
# still resolved to wave 2 globally, and the index would quietly disagree with
# the estimator it exists to describe.
#
# Metric time is the opposite case. Two groups may genuinely run on different
# schedules -- a study arm followed up at 30 days and another at 90 -- so
# `time_value` is stored per (group, wave) and asserting a shared schedule would
# be the same class of fabrication as asserting a spacing.

#' Build the group x wave time index for a schema
#'
#' Rows are the (group, wave) combinations actually observed across node, tie,
#' and group data -- not the Cartesian product, since a group absent at a wave
#' has no occasion there and inventing a row for it would assert an observation
#' that was never made.
#'
#' @param waves the schema's global sorted wave vector, or `NULL`.
#' @param time_meta the user's optional metric-time declaration, already
#'   validated by `.validate_time_meta()`.
#' @return a data frame with `group`, `wave_position`, `wave_label`,
#'   `time_value`, `time_unit`, `time_source`. Zero rows when the data has no
#'   wave dimension.
#' @noRd
.build_time_index <- function(node_data, tie_data, group_data,
                              group_col, wave_col, waves, time_meta,
                              observed_tie_waves = NULL) {
  empty <- data.frame(
    group = character(0), wave_position = integer(0), wave_label = character(0),
    time_value = numeric(0), time_unit = character(0), time_source = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(wave_col) || is.null(waves) || length(waves) == 0L) {
    return(empty)
  }

  observed <- do.call(rbind, lapply(
    list(node_data, tie_data, group_data),
    function(df) {
      if (is.null(df) || !all(c(group_col, wave_col) %in% names(df))) {
        return(NULL)
      }
      data.frame(
        group = as.character(df[[group_col]]),
        wave_label = as.character(df[[wave_col]]),
        stringsAsFactors = FALSE
      )
    }
  ))
  # An observed network with no tie rows is still an occasion of its group.
  if (!is.null(observed_tie_waves)) {
    observed <- rbind(observed, data.frame(
      group = observed_tie_waves$group, wave_label = observed_tie_waves$wave,
      stringsAsFactors = FALSE
    ))
  }
  if (is.null(observed) || nrow(observed) == 0L) {
    return(empty)
  }
  observed <- unique(observed)

  idx <- data.frame(
    group = observed$group,
    wave_position = match(observed$wave_label, as.character(waves)),
    wave_label = observed$wave_label,
    time_value = NA_real_,
    time_unit = NA_character_,
    time_source = "absent",
    stringsAsFactors = FALSE
  )
  idx <- idx[order(idx$group, idx$wave_position), , drop = FALSE]
  rownames(idx) <- NULL

  if (is.null(time_meta)) {
    return(idx)
  }

  key <- paste(idx$group, idx$wave_label, sep = .NETIS_KEY_SEP)
  meta_key <- paste(time_meta$group, time_meta$wave_label, sep = .NETIS_KEY_SEP)
  hit <- match(key, meta_key)
  has <- !is.na(hit)
  idx$time_value[has] <- time_meta$time_value[hit[has]]
  idx$time_unit[has] <- time_meta$time_unit[hit[has]]
  idx$time_source[has] <- time_meta$time_source[hit[has]]
  idx
}

#' Validate and normalize a user-supplied metric-time declaration
#'
#' Accepts exactly one shape -- a data frame carrying the group column, the wave
#' column, and `time_value`, with `time_unit` either a column or a single string
#' passed as `time_unit`. Deliberately not a second convenience shape keyed by
#' wave alone: two accepted shapes for one declaration is one shape that has
#' to be told apart by inspection.
#'
#' The group column may be omitted when the data has exactly one group. That is
#' inference-when-unique, the policy `as_igraph()` and `.resolve_wave()` already
#' follow, not a guess: with one group there is nothing to choose between.
#'
#' @noRd
.validate_time_meta <- function(time_meta, group_col, wave_col, waves, groups, time_unit) {
  if (is.null(time_meta)) {
    return(NULL)
  }
  if (is.null(wave_col)) {
    stop(
      "time_meta was supplied but this data has no wave dimension (no wave_col). Metric observation time is recorded per group and wave, so there is nothing for it to attach to.",
      call. = FALSE
    )
  }
  if (!is.data.frame(time_meta)) {
    stop(
      sprintf(
        "time_meta must be a data frame with columns %s, %s, and time_value (see netis_time_index()); got %s.",
        group_col, wave_col, paste(class(time_meta), collapse = "/")
      ),
      call. = FALSE
    )
  }
  .assert_columns_present(time_meta, c(wave_col, "time_value"), "time_meta")

  if (!(group_col %in% names(time_meta))) {
    if (length(groups) != 1L) {
      stop(
        sprintf(
          "time_meta has no '%s' column and this data has %d groups (%s). Metric observation time is recorded per group -- different groups may be on different schedules -- so it must say which group each row describes.",
          group_col, length(groups), paste(utils::head(groups, 5), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    time_meta[[group_col]] <- groups[[1L]]
  }

  out <- data.frame(
    group = as.character(time_meta[[group_col]]),
    wave_label = as.character(time_meta[[wave_col]]),
    time_value = as.numeric(time_meta$time_value),
    stringsAsFactors = FALSE
  )

  unknown_wave <- setdiff(out$wave_label, as.character(waves))
  if (length(unknown_wave) > 0L) {
    stop(
      sprintf(
        "time_meta names wave(s) not observed in the data: %s. Observed waves: %s.",
        paste(unknown_wave, collapse = ", "), paste(waves, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  unknown_group <- setdiff(out$group, as.character(groups))
  if (length(unknown_group) > 0L) {
    stop(
      sprintf(
        "time_meta names group(s) not present in the data: %s.",
        paste(unknown_group, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (anyDuplicated(paste(out$group, out$wave_label, sep = .NETIS_KEY_SEP)) > 0L) {
    stop("time_meta has more than one row for the same group and wave.", call. = FALSE)
  }
  if (any(is.na(out$time_value)) || any(!is.finite(out$time_value))) {
    stop(
      "time_meta$time_value must be finite. A wave whose observation time is unknown is left out of time_meta entirely, which records it as absent rather than as a missing number.",
      call. = FALSE
    )
  }

  unit <- if ("time_unit" %in% names(time_meta)) as.character(time_meta$time_unit) else time_unit
  if (is.null(unit) || anyNA(unit) || !all(nzchar(unit))) {
    stop(
      "time_meta supplies time_value but no time_unit for every row. A number without a unit is not an observation time -- pass time_unit = \"day\" (or \"week\", \"month\", ...), or add a complete time_unit column to time_meta.",
      call. = FALSE
    )
  }
  # Times are compared across a group's waves, which needs one unit to compare in.
  if (!(length(unit) %in% c(1L, nrow(out)))) {
    stop("time_unit must give one time unit, or one per row of time_meta.", call. = FALSE)
  }
  out$time_unit <- rep_len(unit, nrow(out))
  units_per_group <- tapply(out$time_unit, out$group, function(u) length(unique(u)))
  if (any(units_per_group > 1L)) {
    stop(
      sprintf(
        "time_meta gives more than one time unit for group(s) %s. Each group's observation times must be stated in one time unit, or their order cannot be read.",
        paste(names(units_per_group)[units_per_group > 1L], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  out$time_source <- "declared"

  .assert_time_monotonic(out, waves)
  out
}

#' Refuse a metric schedule that contradicts the wave order
#'
#' Wave order is the modeling coordinate and is not negotiable; metric time is a
#' description of it. If a later wave carries an earlier observation time, the
#' two disagree about what happened first, and rnetis cannot know which one the
#' user meant. Proceeding would leave an object whose own two time
#' representations contradict each other -- and since only one of them drives
#' estimation, the contradiction would be invisible in every result.
#'
#' @noRd
.assert_time_monotonic <- function(tm, waves) {
  pos <- match(tm$wave_label, as.character(waves))
  for (g in unique(tm$group)) {
    keep <- tm$group == g
    o <- order(pos[keep])
    tv <- tm$time_value[keep][o]
    if (length(tv) > 1L && any(diff(tv) <= 0)) {
      labs <- tm$wave_label[keep][o]
      stop(
        sprintf(
          "time_meta for group '%s' is not increasing across the observed wave order: waves %s carry times %s. Wave order is the modeling coordinate, so a metric time that runs backwards against it contradicts the data rather than describing it.",
          g, paste(labs, collapse = " -> "), paste(tv, collapse = " -> ")
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' The observation-time index of a netis_data object
#'
#' Returns one row per observed group and wave, carrying both temporal
#' coordinates rnetis holds: the **wave position** that every model actually uses,
#' and the **metric observation time**, if any is known.
#'
#' # The two coordinates are not interchangeable
#'
#' `wave_position` is the modeling coordinate. `ar()`, [lagged()],
#' [prior_tie()] and exposure history all step through
#' wave *positions*: a lag of 1 is the previous observed occasion, whether the
#' gap was a day or a decade. With observed waves 1, 3, 5, a lag of 1 from wave 5
#' is wave 3 -- wave labels are ordered occasions, never numbers to subtract.
#'
#' `time_value` is metadata. It records **when observations happened**, for
#' interoperability, provenance, and plotting. **No estimator reads it.**
#' Changing it while leaving wave order alone changes no estimate. rnetis has no
#' model in which elapsed time has an effect, and supplying `time_value` does not
#' quietly provide one.
#'
#' Positions are global across the dataset, because that is what lag resolution
#' steps through. Observation times are per group, because different groups may
#' genuinely be on different schedules.
#'
#' @param x a [netis_data()] object.
#' @return a data frame with one row per observed group and wave:
#'   \describe{
#'     \item{`group`}{group identifier.}
#'     \item{`wave_position`}{`1, 2, 3, ...`, the index of this wave in the
#'       dataset's observed wave sequence.}
#'     \item{`wave_label`}{the wave value as you supplied it.}
#'     \item{`time_value`}{metric observation time, or `NA` when unknown.}
#'     \item{`time_unit`}{unit of `time_value`, or `NA`.}
#'     \item{`time_source`}{`"declared"` (you supplied it through `time_meta`) or
#'       `"absent"`.}
#'   }
#'   Zero rows if the data has no wave dimension.
#' @seealso [netis_data()]'s `time_meta` argument to declare observation times,
#'   [netis_boundary()] for the parallel provenance record on network semantics,
#'   [netis_schema()]
#' @examples
#' nodes <- data.frame(
#'   group = 1, node = rep(c("a", "b"), each = 3), wave = rep(1:3, times = 2),
#'   y = c(1, 2, 3, 2, 3, 4)
#' )
#' ties <- data.frame(group = 1, source = "a", target = "b", wave = 1:3, w = 1)
#'
#' # Without declared times: positions only.
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' netis_time_index(nd)
#'
#' # With them: the positions are unchanged -- they are what models use.
#' nd2 <- netis_data(
#'   nodes, ties, wave_col = "wave",
#'   time_meta = data.frame(wave = 1:3, time_value = c(0, 7, 30)),
#'   time_unit = "day"
#' )
#' netis_time_index(nd2)
#' @export
netis_time_index <- function(x) {
  .check_netis_data(x)
  netis_schema(x)$time_index
}
