# Structural checks on tie rows that every network built from them depends on.

#' Which self-tie rows carry a tie
#'
#' A row whose endpoints are the same node, with a nonzero value in some tie
#' variable. A row whose values are all zero or missing carries no tie (the
#' diagonal of a complete dyadic table). A table with no tie variable at all has
#' nothing but the row to go on, so its row is the tie. The one rule used both by
#' the network builders' refusal and by `diagnose_data()`.
#'
#' @param ties a tie table.
#' @param schema the data schema.
#' @return a logical vector, one per row.
#' @noRd
.self_tie_carries_value <- function(ties, schema) {
  is_self <- as.character(ties[[schema$source_col]]) == as.character(ties[[schema$target_col]])
  if (!any(is_self)) {
    return(is_self)
  }
  values <- ties[setdiff(names(ties), c(schema$group_col, schema$source_col,
                                        schema$target_col, schema$wave_col))]
  if (ncol(values) == 0L) {
    return(is_self)
  }
  is_self & rowSums(!is.na(values) & values != 0) > 0L
}

#' Refuse an undirected tie table that stores the same pair twice
#'
#' THE UNDIRECTED CONVENTION. An undirected `netis_data` object stores exactly
#' one row per unordered pair (per group, and per wave when ties carry one),
#' with the pair's endpoints in either column order. Every matrix built from it
#' writes the stored value to both cells, once. A table that stores both
#' orientations of a pair would give that pair two values for one undirected
#' tie -- and whichever row was written last would silently win -- so it is
#' refused here, at construction, rather than resolved by a consumer.
#'
#' @param tie_data the tie table.
#' @param group_col,source_col,target_col key columns.
#' @param tie_wave the wave column if ties carry one, else `NULL`.
#' @return `TRUE` invisibly, or an error naming the duplicated pairs.
#' @noRd
.assert_undirected_pairs_unique <- function(tie_data, group_col, source_col,
                                            target_col, tie_wave) {
  if (nrow(tie_data) < 2L) {
    return(invisible(TRUE))
  }
  src <- as.character(tie_data[[source_col]])
  tgt <- as.character(tie_data[[target_col]])
  lo <- ifelse(src <= tgt, src, tgt)
  hi <- ifelse(src <= tgt, tgt, src)
  parts <- list(as.character(tie_data[[group_col]]), lo, hi)
  if (!is.null(tie_wave)) {
    parts <- c(parts, list(as.character(tie_data[[tie_wave]])))
  }
  key <- do.call(paste, c(parts, sep = .NETIS_KEY_SEP))
  dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (!any(dup)) {
    return(invisible(TRUE))
  }
  shown <- unique(sprintf("%s-%s", lo[dup], hi[dup]))
  stop(
    sprintf(
      "directed = FALSE, but tie_data stores %d unordered pair(s) more than once (for example both a -> b and b -> a): %s. An undirected network stores one row per pair, and rnetis mirrors that row to both directions itself. Keep one row per pair (combining the two values with a rule you choose, e.g. their maximum or mean), or use directed = TRUE if the two directions are different relations.",
      length(shown), paste(utils::head(shown, 5), collapse = ", ")
    ),
    call. = FALSE
  )
}
