#' Build a netis_data object from one or more adjacency matrices
#'
#' Converts adjacency matrices to the long tie format `netis_data()` expects and
#' constructs the object. This is a convenience wrapper: internally, everything is
#' still normalized into the ordinary long-form `netis_data` representation, and
#' `netis_data_from_adjacency()` does no more than build that long form from
#' matrices and then call [netis_data()].
#'
#' This takes one matrix per group: a single network snapshot. For a wave series
#' use [netis_data_from_adjacency_series()].
#'
#' @param adjacency a square numeric matrix, or a named list of square numeric
#'   matrices for multiple groups (the list names become group identifiers). A
#'   single unnamed matrix is treated as one group with group identifier `1`.
#'   Rows are senders and columns receivers, each labeled by its own dimnames:
#'   row `i` is the node `rownames(m)[i]` and column `j` is the node
#'   `colnames(m)[j]`. When both are present they must name the same set of
#'   nodes, in any order; when only one is present it labels both dimensions;
#'   when neither is, identifiers `"n1"`, `"n2"`, ... are generated.
#' @param node_data optional node data frame, as in [netis_data()]. If `NULL`
#'   (the default), node data is derived from the matrix dimnames (see
#'   `adjacency`), with no variables beyond the identifiers -- callers who need
#'   node attributes should supply `node_data` themselves.
#' @param value_name name to give the tie-weight column in the constructed tie
#'   data. Default `"weight"`.
#' @param directed `TRUE` (default) or `FALSE`. If `FALSE`, only the upper
#'   triangle of each matrix is used as the stored representation of the
#'   undirected tie -- see Details.
#' @param drop_zero if `TRUE` (default), cells equal to `0` are
#'   treated as absent ties and dropped rather than stored as explicit
#'   zero-weight ties. This is a real transformation, and it is recorded, not
#'   silent: the constructed object declares `value_name`'s `zero_meaning`
#'   as `"structural_absence"` when `drop_zero = TRUE` (the dropped rows are
#'   exactly the analyst's assertion that a zero means no relationship) and
#'   `"measured_zero"` when `drop_zero = FALSE` (a stored zero was deliberately
#'   kept as a real observation). An `NA` cell is a dyad whose tie status is
#'   unknown, which is neither a tie nor an absence: with `drop_zero = TRUE` it
#'   is an **error** naming the cells (replace the `NA`s with `0` if they mean
#'   no tie), and with `drop_zero = FALSE` it is kept as a row with an `NA`
#'   value.
#' @param self_ties one of `"prohibited"` (default) or `"permitted"`, as in
#'   [netis_data()]. If `"prohibited"` and any matrix has a nonzero, non-`NA`
#'   diagonal entry, this errors rather than silently discarding it -- the
#'   caller must decide whether that value is a genuine self-tie (pass
#'   `self_ties = "permitted"`) or an artifact to be removed before conversion.
#' @param group_col,node_col,source_col,target_col column names to use in the
#'   constructed node and tie data, as in [netis_data()]. Supplying these here
#'   (rather than relying on downstream renaming) is what keeps the generated
#'   data internally consistent.
#'
#' @details
#' For `directed = FALSE`, this function stores **one row per unordered pair**
#' (the upper triangle, `i < j`) rather than duplicating each tie in both
#' directions (see Undirected data on [netis_data()]). If a matrix is not symmetric --
#' including a cell that is `NA` in one direction only -- the lower triangle
#' is discarded, so a warning is issued naming the largest asymmetry found;
#' review the input before trusting the result in that case.
#'
#' A matrix with no ties at all is valid and gives a network of isolates.
#'
#' @return a [netis_data()] object.
#' @seealso [netis_data()], [netis_data_from_adjacency_series()] for several
#'   waves, [netis_adjacency()] for the inverse conversion.
#' @examples
#' m <- matrix(c(0, 1, 0,
#'               0, 0, 1,
#'               1, 0, 0), nrow = 3, byrow = TRUE,
#'             dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
#' nd <- netis_data_from_adjacency(m)
#' netis_ties(nd)
#' @export
netis_data_from_adjacency <- function(adjacency,
                                       node_data = NULL,
                                       value_name = "weight",
                                       directed = TRUE,
                                       drop_zero = TRUE,
                                       self_ties = "prohibited",
                                       group_col = "group",
                                       node_col = "node",
                                       source_col = "source",
                                       target_col = "target") {
  .assert_flag(directed, "directed")
  .assert_flag(drop_zero, "drop_zero")
  .assert_enum(self_ties, c("prohibited", "permitted"), "self_ties")

  mats <- if (is.matrix(adjacency)) list(`1` = adjacency) else adjacency
  if (!is.list(mats) || length(mats) == 0L) {
    stop("adjacency must be a square matrix or a named list of square matrices.", call. = FALSE)
  }
  if (is.null(names(mats)) || any(!nzchar(names(mats)))) {
    stop("A list of adjacency matrices must be named; names become group identifiers.",
         call. = FALSE)
  }

  cols <- list(group = group_col, source = source_col, target = target_col)

  pieces <- Map(.adjacency_to_ties, mats, names(mats),
                MoreArgs = list(value_name = value_name, directed = directed,
                                 drop_zero = drop_zero, self_ties = self_ties, cols = cols))
  tie_data <- do.call(rbind, pieces)
  rownames(tie_data) <- NULL

  if (is.null(node_data)) {
    node_pieces <- Map(function(m, g) {
      ids <- .matrix_node_ids(m)
      out <- data.frame(x = g, y = ids, stringsAsFactors = FALSE)
      names(out) <- c(group_col, node_col)
      out
    }, mats, names(mats))
    node_data <- do.call(rbind, node_pieces)
    rownames(node_data) <- NULL
  }

  # Record the drop_zero choice rather than leaving it untraceable in the
  # resulting object's schema (every transformation must be recorded and
  # surfaced) -- see the drop_zero @param docs above.
  netis_data(
    node_data, tie_data,
    group_col = group_col, node_col = node_col,
    source_col = source_col, target_col = target_col,
    directed = directed, self_ties = self_ties,
    tie_meta = stats::setNames(
      list(list(zero_meaning = if (drop_zero) "structural_absence" else "measured_zero")),
      value_name
    )
  )
}

#' Node identifiers of an adjacency matrix, one per row
#'
#' The ROW identifiers, which is also the order `.adjacency_to_ties()` reorders
#' the columns into (`.align_adjacency_columns()`), so after alignment row `i`
#' and column `i` are the same node.
#'
#' @noRd
.matrix_node_ids <- function(m) {
  ids <- rownames(m)
  if (is.null(ids)) ids <- colnames(m)
  if (is.null(ids)) ids <- paste0("n", seq_len(nrow(m)))
  ids
}

#' Put an adjacency matrix's columns in its row order, by name
#'
#' Rows are senders labeled by `rownames()`, columns receivers labeled by
#' `colnames()`. A matrix whose column labels are a permutation of its row
#' labels is reordered so column `j` is the node of row `j`; reading both
#' dimensions from `rownames()` instead would silently attach every tie to the
#' wrong receiver. Labels that are not the same set are an error, because
#' there is then no node a column can be matched to.
#'
#' @noRd
.align_adjacency_columns <- function(m, group_id) {
  rn <- rownames(m)
  cn <- colnames(m)
  if (is.null(rn) || is.null(cn)) {
    if (!is.null(rn) && anyDuplicated(rn)) {
      stop(sprintf("adjacency matrix for group '%s' has duplicated row names.", group_id), call. = FALSE)
    }
    if (!is.null(cn) && anyDuplicated(cn)) {
      stop(sprintf("adjacency matrix for group '%s' has duplicated column names.", group_id), call. = FALSE)
    }
    return(m)
  }
  if (anyDuplicated(rn) || anyDuplicated(cn)) {
    stop(sprintf("adjacency matrix for group '%s' has duplicated row or column names.", group_id), call. = FALSE)
  }
  if (!setequal(rn, cn)) {
    stop(
      sprintf(
        "adjacency matrix for group '%s' has row names and column names that are not the same set of nodes (in rows only: %s; in columns only: %s). Rows are senders and columns receivers of the same node set, so every node must label one row and one column.",
        group_id,
        paste(utils::head(setdiff(rn, cn), 5), collapse = ", "),
        paste(utils::head(setdiff(cn, rn), 5), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  m[, rn, drop = FALSE]
}

#' @noRd
.adjacency_to_ties <- function(m, group_id, value_name, directed, drop_zero, self_ties, cols) {
  if (!is.matrix(m) || nrow(m) != ncol(m)) {
    stop(sprintf("adjacency matrix for group '%s' must be square.", group_id), call. = FALSE)
  }
  m <- .align_adjacency_columns(m, group_id)
  ids <- .matrix_node_ids(m)
  n <- nrow(m)

  diag_vals <- diag(m)
  has_self_tie_values <- any(!is.na(diag_vals) & diag_vals != 0)
  if (has_self_tie_values && self_ties == "prohibited") {
    stop(
      sprintf(
        paste0(
          "adjacency matrix for group '%s' has nonzero diagonal entries but ",
          "self_ties = \"prohibited\". Set self_ties = \"permitted\" if these are ",
          "genuine self-ties, or zero out the diagonal before conversion."
        ),
        group_id
      ),
      call. = FALSE
    )
  }

  if (!directed) {
    asym <- abs(m - t(m))
    one_sided_na <- xor(is.na(m), is.na(t(m)))
    asym[is.na(asym)] <- 0
    if (any(asym > 0) || any(one_sided_na)) {
      warning(
        sprintf(
          "adjacency matrix for group '%s' is not symmetric (max |difference| = %s; %d pair(s) missing in one direction only) but directed = FALSE; only the upper triangle is kept, discarding the asymmetric part.",
          group_id, format(max(asym), digits = 3), sum(one_sided_na) / 2
        ),
        call. = FALSE
      )
    }
  }

  row_idx <- rep(seq_len(n), times = n)
  col_idx <- rep(seq_len(n), each = n)
  keep <- if (directed) {
    (row_idx != col_idx) | (self_ties == "permitted" & row_idx == col_idx)
  } else {
    (row_idx < col_idx) | (self_ties == "permitted" & row_idx == col_idx)
  }

  values <- m[cbind(row_idx[keep], col_idx[keep])]
  src <- ids[row_idx[keep]]
  tgt <- ids[col_idx[keep]]

  if (drop_zero) {
    # A missing cell is a dyad whose tie status is unknown. Dropping it would
    # record it as "no tie" -- the same as a zero under drop_zero -- which is
    # a claim the data does not make.
    if (anyNA(values)) {
      bad <- which(is.na(values))
      stop(
        sprintf(
          "adjacency matrix for group '%s' has %d missing (NA) cell(s), e.g. %s. With drop_zero = TRUE a missing cell would be stored as no tie, which is not what NA means. Replace the NAs with 0 if they mean no tie, or use drop_zero = FALSE to keep them as rows with a missing value.",
          group_id, length(bad),
          paste(utils::head(sprintf("%s -> %s", src[bad], tgt[bad]), 3), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    present <- values != 0
    values <- values[present]
    src <- src[present]
    tgt <- tgt[present]
  }

  out <- data.frame(x = rep(group_id, length(src)), y = src, z = tgt,
                    stringsAsFactors = FALSE)
  names(out) <- c(cols$group, cols$source, cols$target)
  out[[value_name]] <- values
  out
}
