# Lightweight assertions where a silent misalignment would otherwise pass: ID
# alignment, structural well-formedness, enum validity. These are cheap relative to
# anything statistical and exist to catch silent misalignment early rather than
# let it surface as a wrong number three layers up.

#' @noRd
.assert_columns_present <- function(data, cols, data_name) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "%s is missing required column(s): %s.\nExpected: %s\nFound: %s",
        data_name,
        paste(missing, collapse = ", "),
        paste(cols, collapse = ", "),
        paste(names(data), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @noRd
.assert_unique_key <- function(data, key_cols, data_name) {
  key <- do.call(paste, c(as.list(data[key_cols]), sep = .NETIS_KEY_SEP))
  dup <- duplicated(key)
  if (any(dup)) {
    offending <- unique(key[dup])
    stop(
      sprintf(
        "%s has duplicate rows for key (%s): %d duplicate key value(s), e.g. %s.\n%s",
        data_name,
        paste(key_cols, collapse = ", "),
        length(offending),
        paste(utils::head(gsub("\r", ", ", offending, fixed = TRUE), 3), collapse = " | "),
        "netis_data requires exactly one row per entity key (add a wave column if these are repeated measures, or de-duplicate the input)."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Refuse a missing identifier, or one holding the key separator
#'
#' A row whose group, node, source, target or group identifier is missing
#' belongs to no entity: it would vanish from some computations and appear as a
#' vertex called "NA" in others. A label holding the separator could make two
#' entities share one composite key (see R/keys.R).
#'
#' @noRd
.assert_identifier_columns <- function(data, key_cols, data_name) {
  for (col in key_cols) {
    values <- data[[col]]
    if (anyNA(values)) {
      stop(
        sprintf(
          "%s has %d row(s) with a missing %s identifier. Every row must say which entity it describes: drop those rows, or give them their identifier.",
          data_name, sum(is.na(values)), col
        ),
        call. = FALSE
      )
    }
    if (any(grepl(.NETIS_KEY_SEP, as.character(values), fixed = TRUE))) {
      stop(
        sprintf(
          "%s has a %s identifier containing a carriage return, the character this package joins identifiers with. Two different entities could then share one key and be merged silently, so the label is refused. Remove the control character from the identifier.",
          data_name, col
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' @noRd
.assert_enum <- function(value, choices, arg_name) {
  if (!is.character(value) || length(value) != 1 || !(value %in% choices)) {
    stop(
      sprintf(
        "%s must be one of: %s. Got: %s",
        arg_name,
        paste(dQuote(choices, q = FALSE), collapse = ", "),
        if (is.null(value)) "NULL" else paste(deparse(value), collapse = " ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @noRd
.assert_flag <- function(value, arg_name) {
  if (!is.logical(value) || length(value) != 1 || is.na(value)) {
    stop(sprintf("%s must be TRUE or FALSE.", arg_name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Assert a single whole number at or above a floor
#'
#' Accepts a double that holds a whole number (`5` as well as `5L`), since
#' requiring the `L` suffix from users would be a needless trap.
#'
#' @noRd
.assert_count <- function(value, arg_name, min = 1L) {
  ok <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    value == as.integer(value) && value >= min
  if (!ok) {
    stop(
      sprintf(
        "%s must be a single whole number >= %d. Got: %s",
        arg_name, min, paste(deparse(value), collapse = " ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @noRd
.assert_nonneg_scalar <- function(value, arg_name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < 0) {
    stop(
      sprintf(
        "%s must be a single non-negative number. Got: %s",
        arg_name, paste(deparse(value), collapse = " ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Every term the user wrote must be accounted for by the parser
#'
#' Both formula parsers partition `attr(terms(), "variables")` into their
#' special-term families and a residual covariate bucket. That attribute
#' contains only the VARIABLES a formula mentions, never a compound term
#' label: `y ~ x * z` yields variables `x` and `z`, and the interaction exists
#' only in `attr(terms(), "term.labels")`. A parser reading the first and not
#' the second therefore does not misroute an interaction -- it never sees one.
#' Without this check, `y ~ x * z` would fit main effects only and report
#' nothing.
#'
#' The check is stated as completeness rather than as a test for `:` on
#' purpose. Refusing colons would cover interactions and nothing else: a
#' residual-category parser with no completeness check will swallow the next construction nobody anticipated in the same silence. Every
#' label `terms()` produced must be claimed by something the parser
#' understood, and whatever is left over is refused by name.
#'
#' MATCHING IS BY INDEX, NOT BY TEXT. Comparing `deparse(variable)` against
#' the term label looks exact and is not: `deparse()` splits at
#' `width.cutoff` (60 by default) and rejoining the pieces embeds the
#' continuation indent, so a long but ordinary term such as
#' `exposure(X, via = W, x_lag = 0, w_lag = 0, normalize = "none",
#' isolate_rule = "na")` would compare unequal to its own label and be
#' refused as unparseable. The
#' `factors` matrix removes the question: its rownames are the variables in
#' the same order as the de-listed `variables` vector, its colnames are the
#' term labels, and a nonzero entry says which variables compose which term.
#' A term is consumed when it is composed of exactly one variable and the
#' parser claimed that variable. No string is compared anywhere.
#'
#' `offset()` needs no special handling: it appears in `variables` but is
#' excluded from `term.labels` (it is tracked by `attr(tt, "offset")`), so its
#' row composes no column and cannot be reported as unconsumed.
#'
#' @param tt a `terms` object.
#' @param consumed_idx indices into the de-listed
#' `variables` vector that the parser assigned to a special family or to the
#' covariate bucket. The response index must not be included -- it is not a
#' term.
#' @param family `"influence"` or `"selection"`, selecting the advice the
#' message ends with. The two families support different things.
#' @param
#' extra_consumed_labels term labels accounted for by a mechanism this function
#' does not itself evaluate -- exactly the two-way interactions
#' `.resolve_influence_interactions()` already resolved or refused by name
#' before this runs. Selection passes the default, `character(0)`: it resolves
#' none, so every interaction label still reaches this function and gets
#' the generic refusal below.
#' @return invisibly `TRUE`, or an error naming
#' every unconsumed label.
#' @noRd
.assert_terms_all_consumed <- function(tt, consumed_idx, family = c("influence", "selection"),
                                       extra_consumed_labels = character(0)) {
  family <- match.arg(family)
  labels <- attr(tt, "term.labels")
  if (length(labels) == 0L) {
    return(invisible(TRUE))
  }

  factors <- attr(tt, "factors")
  if (length(factors) == 0L) {
    return(invisible(TRUE))
  }
  # Column j of `factors` is term label j; a nonzero row is a variable that
  # composes it. One composing variable means a main effect, and it is
  # consumed when the parser claimed that variable's index. Anything else --
  # an interaction (two or more composing variables) or a main effect on a
  # variable no family claimed -- is unconsumed, UNLESS its label is already
  # in `extra_consumed_labels`.
  is_consumed <- vapply(seq_len(ncol(factors)), function(j) {
    rows <- which(factors[, j] != 0L)
    (length(rows) == 1L && rows %in% consumed_idx) ||
      colnames(factors)[j] %in% extra_consumed_labels
  }, logical(1))
  unconsumed <- colnames(factors)[!is_consumed]
  if (length(unconsumed) == 0L) {
    return(invisible(TRUE))
  }

  # An interaction is the case users will actually hit, so it gets a message
  # naming the mechanism rather than the generic one. Anything else is a
  # construction the parser has no route for at all, and saying so plainly is
  # more useful than guessing at intent.
  interactions <- grep(":", unconsumed, fixed = TRUE, value = TRUE)
  others <- setdiff(unconsumed, interactions)

  parts <- character(0)
  if (length(interactions) > 0L) {
    parts <- c(parts, sprintf(
      "This formula contains an interaction (%s), which %s.",
      paste(sprintf("'%s'", interactions), collapse = ", "),
      if (identical(family, "influence")) {
        # Reaching this line means a two-way interaction was already resolved
        # or refused BY NAME (hierarchy, moderand pairing) upstream, so
        # everything left is beyond two-way -- three-way and up chief among
        # it (rnetis supports exactly two-way moderation).
        "goes beyond the two-way moderation rnetis supports (a plain covariate, exposure(), or ar(), each times one moderator)"
      } else {
        "rnetis cannot estimate for the Selection model"
      }
    ))
  }
  if (length(others) > 0L) {
    parts <- c(parts, sprintf(
      "This formula contains %s, which the parser has no route for.",
      paste(sprintf("'%s'", others), collapse = ", ")
    ))
  }
  parts <- c(parts, if (identical(family, "influence")) {
    paste(
      "Express anything beyond two-way moderation yourself: add the derived",
      "variable to your node data as a column and enter it as an ordinary",
      "covariate. rnetis will treat it as a predictor and will not reinterpret it."
    )
  } else {
    paste(
      "Moderation is not supported for Selection. Express the product as a",
      "derived dyadic or nodal variable and enter it through dyad(), sender()",
      "or receiver()."
    )
  })
  parts <- c(parts, paste(
    "Refusing rather than dropping the term is deliberate: dropping it",
    "silently would return an unmoderated fit."
  ))

  stop(paste(parts, collapse = " "), call. = FALSE)
}

#' An intercept the user removed must not come back silently
#'
#' The same failure as a dropped interaction wearing different clothes:
#' `stats::terms()` records intercept suppression in `attr(tt, "intercept")`,
#' and a parser that does not read that attribute fits `y ~ ar() + x - 1`
#' WITH an intercept -- coefficients identical to the formula without the
#' `- 1`, no error, no warning. The user asks for one model and receives
#' another.
#'
#' It refuses rather than honouring the request because no estimator route has
#' been validated without an intercept: the AR coefficient's meaning and every
#' centering decision would be affected. A specification the package cannot
#' estimate correctly gets a refusal, not a number.
#'
#' @param tt a `terms` object.
#' @param fn the calling function's name, for the message.
#' @return invisibly `TRUE`, or an error.
#' @noRd
.assert_intercept_retained <- function(tt, fn) {
  if (identical(as.integer(attr(tt, "intercept")), 0L)) {
    stop(
      sprintf(
        paste(
          "This formula suppresses the intercept ('- 1' or '0 +'), which %s does not support.",
          "No rnetis estimator has been validated without an intercept, and the term interacts",
          "with the AR coefficient's interpretation and with any centering. Remove the",
          "suppression to fit the model with an intercept. Refusing is deliberate: ignoring",
          "the request would silently fit an intercept anyway."
        ),
        fn
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# --- The null-coalescing operator, defined ONCE ------------------------------
#
# Defined in exactly one place on purpose. DESCRIPTION has no Collate field,
# so R sources R/ alphabetically, and if several files defined `%||%` with
# different semantics the last one sourced would bind for the WHOLE package --
# which definition applied would depend on a filename. A variant that also
# fell through on a length-1 NA would make every `x %||% default` treat a
# scalar NA as absent, including the many places where NA is a meaningful
# value rather than a missing one.
#
# One definition, base R 4.4's semantics (NULL only).

#' Fall back to `b` when `a` is NULL
#'
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

