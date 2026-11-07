# netis_diagnostics(): the accessor for a fit's findings, as data.

#' Every finding rnetis raised on a fit
#'
#' Returns the fit's diagnostic record as a data frame: the findings (severity
#' `ERROR`/`WARNING`/`NOTE`) recorded when the model was fitted, the ones
#' `summary()` counts.
#'
#' # What is NOT here
#'
#' Model **results** — the Johnson-Neyman region, conditional-effect tables,
#' exposure effects — are not diagnostics and are not returned here. They are
#' what you asked the model for: see [netis_estimates()] and [influence_effect()].
#'
#' @param object a fitted rnetis model.
#' @param ... unused.
#' @return a `data.frame` with `family`, `code`, `level`, `summary` (one line) and
#'   `detail` (the full explanation). Zero rows where the fit recorded nothing.
#' @seealso [netis_glance()], whose `n_error`/`n_warning`/`n_note` count these
#'   rows; [diagnose_data()] and [diagnose_design()] for the pre-fit checks.
#' @examples
#' set.seed(1)
#' gs <- c("a", "b", "c", "d")
#' nodes <- do.call(rbind, lapply(gs, function(g) {
#'   ids <- sprintf("%s%02d", g, 1:7)
#'   data.frame(group = g, node = rep(ids, 2), wave = rep(1:2, each = 7),
#'              y = stats::rnorm(14), x = stats::rnorm(14))
#' }))
#' ties <- do.call(rbind, lapply(gs, function(g) {
#'   ids <- sprintf("%s%02d", g, 1:7)
#'   data.frame(group = g, source = ids[-7], target = ids[-1], wave = 1, tie = 1)
#' }))
#' nd <- netis_data(nodes, ties, wave_col = "wave")
#' fit <- suppressWarnings(fit_influence(
#'   y ~ ar() + exposure(x, via = tie) + (1 | group), data = nd))
#' netis_diagnostics(fit)
#' @export
netis_diagnostics <- function(object, ...) {
  UseMethod("netis_diagnostics")
}

#' @rdname netis_diagnostics
#' @export
netis_diagnostics.default <- function(object, ...) {
  stop(
    sprintf(
      "netis_diagnostics() has no method for an object of class %s. It is defined for fitted rnetis models.",
      paste(class(object), collapse = "/")
    ),
    call. = FALSE
  )
}

#' @rdname netis_diagnostics
#' @export
netis_diagnostics.netis_fit <- function(object, ...) {
  out <- .diagnostics_as_rows(object$diagnostics)
  row.names(out) <- NULL
  out
}

#' @noRd
.diagnostics_as_rows <- function(d) {
  if (is.null(d) || length(d) == 0L) {
    return(data.frame(family = character(0), code = character(0), level = character(0),
                      summary = character(0), detail = character(0),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(unclass(d), function(f) {
    data.frame(
      family = f$family %||% NA_character_,
      code = f$code %||% NA_character_,
      level = f$level %||% NA_character_,
      # A finding's `message` says what was wrong, what was expected and what to
      # do, so its first sentence is a usable one-line summary and the whole text
      # is the detail. Findings therefore carry no separate summary field.
      summary = .first_sentence(f$message),
      detail = f$message %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}

#' First sentence of a message, for one-line console output
#'
#' A sentence ends at terminal punctuation followed by a space or the end of the
#' text, unless the word before it is a common abbreviation ("e.g.", "i.e.",
#' "cf.", "vs."). Falls back to the whole string.
#'
#' @noRd
.first_sentence <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NA_character_)
  ends <- gregexpr("[.!?](\\s|$)", x)[[1]]
  for (e in ends) {
    if (e < 0L) break
    head <- substr(x, 1L, e)
    if (grepl("(^|[^[:alnum:]])(e\\.g|i\\.e|cf|vs)\\.$", head)) next
    return(trimws(head))
  }
  x
}
