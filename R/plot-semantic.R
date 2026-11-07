# The semantic layer: everything rnetis decides before ggplot2 draws anything.
# Direction, comparability, missingness state, reference legitimacy and
# limitations are resolved here and handed to the renderer already settled,
# which is what keeps a hard ggplot2 dependency from turning the package into a
# collection of geom_*() calls.
#
# Nothing in this file constructs, touches, or knows about a ggplot.

# ---- references -------------------------------------------------------------

# The closed allowlist.
#
#   null      a mathematically defined null value (a coefficient of zero)
#   chance    a chance level under the display's own sampling scheme (AUC 0.5)
#   baseline  an explicitly identified comparison baseline (base-rate Brier)
#   identity  a definitional equality (predicted = observed, on a calibration
#             display, which is what perfect calibration MEANS)
#   nominal   the coverage a stated-level interval procedure is DEFINED to
#             target (0.95 for a 95% interval) -- a property of the
#             procedure's own definition, not a hypothesis under test and not
#             an acceptability bar it must clear
#
# The list is closed rather than merely documented because the alternative -- a
# convention that reference lines "should" be one of these -- is not
# enforceable. A renderer writing geom_hline(yintercept = 1.01) passes every
# naming convention and still ships a cutoff, which rnetis does not draw: a line at
# a conventional value is a pass/fail verdict. Making the type a constructor argument validated
# against this vector means the prohibited annotation has no route into a plot
# at all.
#
# THE CRITERION FOR ADDING A TYPE, because a closed list that grows casually is
# an open list with extra steps: a type may be added only if it names a
# position that is mathematically or definitionally determined -- something the
# reader could derive without reference to whether the model is any good. It
# may never name a standard of acceptability, quality, or decision. `identity`
# meets that rule: y = x on a calibration plot is what perfect calibration is,
# not a bar it must clear. Calling it `null` instead would make one of the
# types semantically wrong -- and a type system that lies about what its
# categories mean stops being worth enforcing. `nominal` meets the same rule:
# a coverage panel's 0.95 line is not `null` (a 95% procedure achieving 0.95
# coverage is not the absence of an effect) and is not an acceptability
# threshold either -- it is what the interval procedure's own stated level
# MEANS, exactly the same relationship `identity` has to a calibration
# display. Stretching `null` to cover either would conflate them.
.NETIS_REFERENCE_TYPES <- c("null", "chance", "baseline", "identity", "nominal")

#' Construct an approved reference annotation
#'
#' The only legitimate way to get a reference line into a rnetis plot. Every
#' reference a plot draws must have been constructed here first, and
#' `.netis_reference_layer()` draws nothing else. This constructor is one half
#' of that two-layer enforcement; the layer function is the other.
#'
#' @param type One of `.NETIS_REFERENCE_TYPES`. Anything else is an error, and
#'   the error names the prohibited category explicitly rather than only listing
#'   the valid values -- a caller reaching for `"threshold"` has made a
#'   statistical mistake, not a typo, and deserves to be told which.
#' @param value The position on the relevant scale. Must be a single finite
#'   number: an `NA` reference would draw nothing while reading in the returned
#'   data as though a reference existed.
#' @param label Short text naming what the reference *is* ("no effect", "chance").
#'   Never an evaluation ("acceptable", "good fit").
#' @param source Optional provenance. Required in practice for a `baseline`
#'   derived from data or from preliminary evidence: preliminary evidence is
#'   labeled wherever it is cited, and a reference line drawn on a plot is a
#'   citation.
#'
#' @return A `netis_reference` list with fields `type`, `value`, `label`, `source`.
#' @noRd
.netis_reference <- function(type, value, label, source = NULL) {
  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    stop("A reference `type` must be a single non-NA string.", call. = FALSE)
  }
  if (!type %in% .NETIS_REFERENCE_TYPES) {
    stop(
      sprintf(
        paste0(
          "Reference type \"%s\" is not permitted.\n",
          "Expected one of: %s.\n",
          "rnetis draws a reference only where it marks a mathematically defined null, a ",
          "chance level, or an explicitly identified comparison baseline. An acceptability, ",
          "quality, or decision threshold is never drawn: a cutoff on a ",
          "plot is a pass/fail verdict, whatever it is called.\n",
          "If this value is a conventional cutoff (an R-hat of 1.01),",
          "report it in the caption as a convention and let the reader judge."
        ),
        type,
        paste(.NETIS_REFERENCE_TYPES, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    stop(
      sprintf(
        "A reference `value` must be a single finite number; got %s.",
        paste(utils::capture.output(utils::str(value)), collapse = " ")
      ),
      call. = FALSE
    )
  }
  if (!is.character(label) || length(label) != 1L || is.na(label)) {
    stop("A reference `label` must be a single non-NA string naming what the reference is.",
         call. = FALSE)
  }
  if (!is.null(source) && (!is.character(source) || length(source) != 1L)) {
    stop("A reference `source`, when supplied, must be a single string.", call. = FALSE)
  }

  structure(
    list(type = type, value = value, label = label, source = source),
    class = "netis_reference"
  )
}

#' Collect references into the set a panel declares
#'
#' A panel's references travel as a list so the renderer can draw them and the
#' tests can compare what was drawn against what was declared. Validating
#' membership here means a hand-built list cannot smuggle an unapproved element
#' past `.netis_reference_layer()`.
#'
#' @param ... `netis_reference` objects, or a single list of them.
#' @return A `netis_references` list, possibly empty.
#' @noRd
.netis_references <- function(...) {
  refs <- list(...)
  if (length(refs) == 1L && is.list(refs[[1]]) && !inherits(refs[[1]], "netis_reference")) {
    refs <- refs[[1]]
  }
  ok <- vapply(refs, inherits, logical(1), what = "netis_reference")
  if (!all(ok)) {
    stop(
      sprintf(
        paste0(
          "Every reference must be built by .netis_reference(); element(s) %s were not.\n",
          "This is what stops an unapproved reference reaching a plot."
        ),
        paste(which(!ok), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  structure(refs, class = "netis_references")
}

# ---- missingness states -----------------------------------------------------

# Missingness is carried as an explicit factor column, never as an absent row,
# so that a plot can draw the distinction between the states and a test can
# assert on it. The levels are fixed and ordered for
# stable aesthetic mapping.
#
# "structural_zero" and "missing" are a pair the exposure engine must keep
# apart: an isolate has zero alters and a defined
# exposure of zero; a node whose alter was not observed has no defined exposure at
# all. One glyph for both is the visual form of that conflation.
#
# "undetermined" is not a hedge. netis_data cannot always
# answer whether a network was observed at a wave, and rnetis never fabricates
# the answer -- so the honest state gets a level of its own and renders as one.
.NETIS_MISSING_STATES <- c(
  "observed",       # present, and its value is defined
  "structural_zero",# present, and defined as zero by structure (an isolate)
  "missing",        # present on the roster, value not observed
  "absent_row",     # not on the roster at this wave -- a ragged roster, not an NA
  "undetermined"    # the object cannot say
)

#' Build the missingness-state column for a plot table
#'
#' @param n Number of rows.
#' @param state Either one state recycled to length `n`, or a length-`n` vector.
#' @return An ordered factor with all of `.NETIS_MISSING_STATES` as levels,
#'   whether or not each occurs. Keeping absent levels is deliberate: a legend
#'   that gains and loses entries according to which states a particular dataset
#'   happens to contain makes two plots of the same quantity incomparable.
#' @noRd
.netis_missing_state <- function(n, state = "observed") {
  if (length(state) == 1L) state <- rep(state, n)
  if (length(state) != n) {
    stop(
      sprintf("`state` must be length 1 or %d; got %d.", n, length(state)),
      call. = FALSE
    )
  }
  bad <- setdiff(unique(state[!is.na(state)]), .NETIS_MISSING_STATES)
  if (length(bad) > 0) {
    stop(
      sprintf(
        "Unknown missingness state(s): %s.\nExpected one of: %s.",
        paste(bad, collapse = ", "),
        paste(.NETIS_MISSING_STATES, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  factor(state, levels = .NETIS_MISSING_STATES)
}

#' Classify values into structural / missing / undetermined / observed
#'
#' The caller is the only thing that knows *why* a value is what it is.
#' `compute_exposure()` knows an isolate produced its zero; this function cannot
#' recover that from the number, and guessing is the defect being guarded against.
#'
#' **Precedence, and it is deliberate: a known structural state wins over `NA`.**
#' The opposite precedence — `NA` beating structure — might seem safer, on the
#' reasoning that claiming a structural zero for an unobserved entry would
#' fabricate a quantity. That conflates two different unknowns. Not
#' knowing a *value* is not the same as not knowing the *structure*: when degree
#' is zero the isolate status is an observed fact about the network, and any `NA`
#' that follows is a consequence of a declared rule (`isolate_rule = "na"`, say),
#' not an unobserved measurement. Reporting it as `missing` misattributes the
#' cause, the same error the exposure engine guards against one layer down — a
#' balance diagnostic blaming a missing alter for an exposure that was `NA`
#' because the network was unobserved, sending users to look at the wrong data.
#'
#' The three-way rule:
#'
#' | Caller knows | State |
#' |---|---|
#' | the entry is structurally determined | `structural_zero` |
#' | the entry is eligible but its value is unavailable | `missing` |
#' | eligibility or presence is itself unresolved | `undetermined` |
#'
#' @param x The values.
#' @param structural A logical vector: `TRUE` where the entry is known to be
#'   structurally determined, `FALSE` where it is known not to be, and `NA` where
#'   the caller cannot resolve eligibility at all. `NA` maps to `undetermined`,
#'   which is why this argument is deliberately three-valued rather than a flag.
#' @noRd
.netis_classify_missing <- function(x, structural = rep(FALSE, length(x))) {
  if (length(structural) == 1L) structural <- rep(structural, length(x))
  state <- ifelse(
    is.na(structural), "undetermined",
    ifelse(structural, "structural_zero",
           ifelse(is.na(x), "missing", "observed"))
  )
  .netis_missing_state(length(x), state)
}

# ---- captions ---------------------------------------------------------------

#' Attach the honesty sentences a panel must carry
#'
#' Limitations render *in the plot*, not in the help page, because the plot is what
#' gets pasted into a slide deck. A small chain's diagnostics captioned only in
#' `?plot.netis_selection` are captioned nowhere that matters.
#'
#' @param x A plot data table or list of tables.
#' @param ... Character scalars, each one sentence.
#' @noRd
.netis_caption <- function(x, ...) {
  lines <- unlist(list(...), use.names = FALSE)
  if (length(lines) == 0L) return(x)
  if (!is.character(lines) || anyNA(lines)) {
    stop("Caption lines must be non-NA strings.", call. = FALSE)
  }
  attr(x, "netis_caption") <- c(attr(x, "netis_caption"), lines)
  x
}

#' Read a table's accumulated caption lines
#' @noRd
.netis_caption_of <- function(x) {
  out <- attr(x, "netis_caption")
  if (is.null(out)) character(0) else out
}
