# The stable half of the public plotting API.
#
# plot() returns a ggplot whose internal construction is explicitly NOT stable
# API; plot_data() returns the quantities behind it and IS. A user who needs a
# guarantee builds from here.

#' Extract the quantities behind a rnetis plot
#'
#' Returns the data a [plot()] method computes before it draws anything: the
#' values, their identity, the grouping and facet variables, the missingness
#' state of every row, and — where a panel needs more than one table — a named
#' list of tables rather than one flattened rectangle.
#'
#' Reach for this when you want to build your own figure, check a number the plot
#' shows, or feed a rnetis quantity into another tool. Its columns are stable; the
#' plot's internal ggplot construction is not (see *Stability*).
#'
#' # Missingness is a column, never an absent row
#'
#' A row is not dropped because its value is missing. `missing_state` says why a
#' value is what it is, and the distinction it protects is the one this package
#' cares most about: an **isolate** has zero alters and an exposure genuinely
#' equal to zero (`structural_zero`), while a node whose alter went unobserved has
#' no defined exposure at all (`missing`). Both print as an unremarkable number or
#' a blank; they are not the same finding. A plot that silently dropped `NA` rows
#' would be doing listwise deletion in a costume.
#'
#' `undetermined` is not a hedge — it means the object cannot answer the question.
#' Whether a network was *observed* at a given wave is sometimes not recoverable
#' from a `netis_data` object, and rnetis reports that rather than guessing.
#'
#' # Stability
#'
#' `plot_data()` is the stable numerical interface: column names, types, and
#' meanings change only through the ordinary deprecation route. The ggplot object
#' `plot()` returns is a different matter — its layer sequence, geoms, theme, and
#' internal arrangement may change between versions. Restyle a returned plot
#' freely; build anything you depend on from `plot_data()`.
#'
#' @param x A rnetis object with a plot method.
#' @param what Character scalar naming the panel, using the same vocabulary as
#'   the corresponding `plot()` method — the two are learned once. Each method
#'   documents its full set; an unrecognized value errors naming the valid ones.
#'   `NULL` (default) selects that object's default panel.
#' @param ... Passed to the method.
#'
#' @return Either a data frame, or a **named list** of data frames for a panel
#'   whose meaning does not fit one rectangle. The columns are documented per
#'   object and `what` value on the relevant `plot()` page.
#'
#'   Columns present wherever they apply, with stable names:
#'
#'   \describe{
#'     \item{identifiers}{`node`, `group`, `wave`, `term` — whichever the panel is
#'       keyed by, always as character except `wave`.}
#'     \item{`value`, `lower`, `upper`}{the quantity and its interval, where the
#'       panel shows uncertainty.}
#'     \item{`missing_state`}{a factor with a fixed level set:
#'       `observed`, `structural_zero`, `missing`, `absent_row`, `undetermined`.
#'       Every level is retained whether or not it occurs, so two plots of the
#'       same quantity stay comparable.}
#'   }
#'
#'   Attributes: `netis_caption` carries the limitations the plot displays;
#'   `layout_method` records which layout actually ran, on network panels.
#'
#' @seealso [netis_estimates()] and [netis_glance()] for numeric result
#'   extraction independent of plotting; [compute_exposure()] for the exposure
#'   quantities several panels display.
#'
#' @examples
#' # plot_data() is defined for rnetis objects. Anything else gets an actionable
#' # refusal rather than an empty frame.
#' try(plot_data(1:10))
#'
#' @export
plot_data <- function(x, what = NULL, ...) {
  UseMethod("plot_data")
}

#' @export
plot_data.default <- function(x, what = NULL, ...) {
  stop(
    sprintf(
      paste0(
        "plot_data() has no method for an object of class %s.\n",
        "It is defined for rnetis objects that have a plot() method."
      ),
      paste(sQuote(class(x)), collapse = "/")
    ),
    call. = FALSE
  )
}

# ---- internals shared by every method ---------------------------------------

#' Resolve and validate a `what` selector against a panel vocabulary
#'
#' Centralized so every plot surface refuses the same way and no method invents
#' its own spelling of the error (actionable, names the offending value and the
#' expected set).
#'
#' @param what The user's value, possibly `NULL`.
#' @param choices Character vector of valid panels; the first is the default.
#' @param object_label What to call the object in the message.
#' @noRd
.netis_what <- function(what, choices, object_label) {
  if (is.null(what)) return(choices[1])
  if (!is.character(what) || length(what) != 1L || is.na(what)) {
    stop(
      sprintf("`what` must be a single panel name; expected one of: %s.",
              paste(choices, collapse = ", ")),
      call. = FALSE
    )
  }
  if (!what %in% choices) {
    stop(
      sprintf(
        "\"%s\" is not a panel of %s.\nExpected one of: %s.",
        what, object_label, paste(choices, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  what
}

#' Build a validated plot table
#'
#' Every panel's data goes through here, so the documented columns are enforced at
#' construction rather than asserted in prose. In particular a table
#' claiming to describe values must carry `missing_state`: the moment that becomes
#' optional is the moment a panel silently drops its `NA` rows.
#'
#' @param df The assembled data frame.
#' @param required Columns the panel documents.
#' @param references A `netis_references` set, or `NULL`.
#' @noRd
.netis_plot_table <- function(df, required = character(0), references = NULL) {
  if (!is.data.frame(df)) {
    stop("A plot table must be a data frame.", call. = FALSE)
  }
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf(
        "Plot table is missing required column(s): %s.\nPresent: %s.",
        paste(missing_cols, collapse = ", "),
        paste(names(df), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if ("missing_state" %in% names(df)) {
    if (!is.factor(df$missing_state) ||
          !identical(levels(df$missing_state), .NETIS_MISSING_STATES)) {
      stop(
        paste0(
          "`missing_state` must be the fixed-level factor built by .netis_missing_state().\n",
          "A per-dataset level set makes two plots of the same quantity incomparable."
        ),
        call. = FALSE
      )
    }
  }
  if (!is.null(references)) {
    if (!inherits(references, "netis_references")) {
      stop("`references` must be built by .netis_references().", call. = FALSE)
    }
    attr(df, "netis_references") <- references
  }
  df
}
