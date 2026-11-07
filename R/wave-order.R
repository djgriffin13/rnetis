# Wave order and wave lags, shared by the data layer and both model families.
#
# Waves are ordered occasions. Their order is a fact about the data, decided
# once when the wave vocabulary is built, and every later question -- which wave
# is "before" another, which wave a lag of k reaches -- is answered by POSITION
# in that vocabulary, never by comparing or subtracting labels. Comparing labels
# is what makes waves 9 and 10 run backwards once they are stored as text.

#' Build the ordered wave vocabulary of a dataset
#'
#' One vocabulary for every table, built from the wave columns of all of them.
#' The order is taken from the labels' own type:
#'
#' - numeric (or `Date`) labels sort by value;
#' - a factor declares its order through its levels (several factor columns
#'   must agree on the relative order of the levels they share);
#' - text labels that all read as numbers ("9", "10") sort by numeric value,
#'   whether or not other tables store the same waves as numbers;
#' - text labels that do not read as numbers carry no order rnetis can know, so
#'   two or more of them are refused with a pointer to a factor.
#'
#' Missing wave labels are refused: a row with no wave is not on any occasion.
#'
#' @param cols a named list of wave columns (`NULL` entries are ignored); the
#'   names are used in error messages.
#' @return the observed waves in order: numeric when every column is numeric
#'   (or the columns mix numbers with numeric text), `Date` for dates, and
#'   character otherwise. `NULL` when no column is given.
#' @noRd
.wave_vocabulary <- function(cols) {
  cols <- Filter(Negate(is.null), cols)
  if (length(cols) == 0L) {
    return(NULL)
  }
  for (nm in names(cols)) {
    if (anyNA(cols[[nm]])) {
      stop(
        sprintf(
          "%s has %d row(s) with a missing wave label. Every row of a table with a wave column must name its wave; drop the rows, or give them their wave.",
          nm, sum(is.na(cols[[nm]]))
        ),
        call. = FALSE
      )
    }
  }

  is_fac <- vapply(cols, is.factor, logical(1))
  if (any(is_fac)) {
    return(.wave_vocabulary_factor(cols, is_fac))
  }

  is_num <- vapply(cols, function(v) is.numeric(v) && !is.factor(v), logical(1))
  is_date <- vapply(cols, function(v) inherits(v, c("Date", "POSIXt")), logical(1))
  if (all(is_num)) {
    return(sort(unique(unlist(cols, use.names = FALSE))))
  }
  if (all(is_date)) {
    return(sort(unique(do.call(c, unname(cols)))))
  }

  labels <- unique(as.character(unlist(lapply(cols, as.character), use.names = FALSE)))
  as_num <- suppressWarnings(as.numeric(labels))
  if (!anyNA(as_num)) {
    if (anyDuplicated(as_num) > 0L) {
      clash <- labels[as_num %in% as_num[duplicated(as_num)]]
      stop(
        sprintf(
          "Wave labels %s are different text for the same number. Use one spelling per wave.",
          paste(sprintf("'%s'", clash), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    if (any(is_num)) {
      # Numbers in one table and numeric text in another: the same waves,
      # stored two ways. Kept as numbers, which is the reading that orders them;
      # a text label that does not print back as that number would then fail to
      # match its own rows, so it is refused rather than silently lost.
      bad <- labels[labels != as.character(as_num)]
      if (length(bad) > 0L) {
        stop(
          sprintf(
            "The wave column is numeric in some tables and text in others, and text label(s) %s do not match any number's printed form. Store the wave column with one type in every table.",
            paste(sprintf("'%s'", bad), collapse = ", ")
          ),
          call. = FALSE
        )
      }
      return(sort(unique(as_num)))
    }
    return(labels[order(as_num)])
  }

  if (any(is_num)) {
    stop(
      sprintf(
        "The wave column is numeric in %s but holds text that is not a number in %s. Store the wave column with one type in every table.",
        paste(names(cols)[is_num], collapse = ", "),
        paste(names(cols)[!is_num], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (length(labels) > 1L) {
    stop(
      sprintf(
        "Wave labels %s are text, and text has no order rnetis can read: sorted alphabetically, 'w10' would come before 'w9'. Supply the wave column as numbers, or as a factor whose levels list the waves in time order, e.g. factor(wave, levels = c(%s)).",
        paste(sprintf("'%s'", labels), collapse = ", "),
        paste(sprintf("\"%s\"", labels), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  labels
}

#' @noRd
.wave_vocabulary_factor <- function(cols, is_fac) {
  lvl_sets <- lapply(cols[is_fac], levels)
  # The longest level set leads, so a table whose levels are a subset of
  # another's slots into that order rather than reordering it.
  lvl_sets <- lvl_sets[order(-lengths(lvl_sets))]
  order_lv <- unique(unlist(lvl_sets, use.names = FALSE))
  for (lv in lvl_sets) {
    # Each factor's levels must appear in the combined order in the same
    # relative order; otherwise two tables declare two different time orders.
    if (is.unsorted(match(lv, order_lv))) {
      stop(
        "The wave column is a factor in several tables, and their levels put the waves in different orders. Give every table the same levels.",
        call. = FALSE
      )
    }
  }
  present <- unique(unlist(lapply(cols, as.character), use.names = FALSE))
  stray <- setdiff(present, order_lv)
  if (length(stray) > 0L) {
    stop(
      sprintf(
        "The wave column is a factor in %s, and wave label(s) %s in %s are not among its levels, so their place in time is unknown. Add them to the levels, or store the wave column as a factor in every table.",
        paste(names(cols)[is_fac], collapse = ", "),
        paste(sprintf("'%s'", stray), collapse = ", "),
        paste(names(cols)[!is_fac], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  order_lv[order_lv %in% present]
}

#' Typed wave values for an importer
#'
#' Importers read wave labels from list names or graph attributes, where they
#' are text. Returned as the type that keeps their order: the caller's own
#' values when it supplied them (numbers stay numbers), numeric text as text
#' (the wave vocabulary orders it numerically), and other text as a factor
#' whose levels follow the order the labels were given in -- the only order the
#' importer was told.
#'
#' @param values the labels, in the order the input supplied them.
#' @return `values`, typed.
#' @noRd
.import_wave_values <- function(values) {
  if (is.factor(values) || is.numeric(values) || inherits(values, c("Date", "POSIXt"))) {
    return(values)
  }
  values <- as.character(values)
  if (!anyNA(suppressWarnings(as.numeric(values)))) {
    return(values)
  }
  factor(values, levels = unique(values))
}

#' The wave sequence a fit's lags step through
#'
#' A fit analyses one transition `c(from, to)` (or one wave, or none). Its lags
#' are positional steps back from `to` through this sequence: the observed waves
#' up to and including `from`, then `to`. Waves strictly between `from` and
#' `to` are left out, so a lag of 1 always reads `from` -- the transition the
#' fit reports -- and a deeper lag steps back from `from` through the earlier
#' observed waves. For an adjacent pair this is simply the observed sequence up
#' to `to`.
#'
#' @param observed the ordered wave vocabulary (`schema$waves`).
#' @param waves the analyzed waves: `NULL`, one wave, or `c(from, to)`.
#' @return the sequence, ending with the outcome wave; `NULL` for no waves.
#' @noRd
.lag_sequence <- function(observed, waves) {
  if (length(waves) == 0L) {
    return(NULL)
  }
  to <- waves[[length(waves)]]
  if (length(waves) == 1L) {
    pos <- match(as.character(to), as.character(observed))
    return(observed[seq_len(pos)])
  }
  from_pos <- match(as.character(waves[[1L]]), as.character(observed))
  to_pos <- match(as.character(to), as.character(observed))
  observed[c(seq_len(from_pos), to_pos)]
}

#' Is a pair of analyzed waves adjacent in the observed sequence?
#' @noRd
.waves_adjacent <- function(observed, waves) {
  if (length(waves) < 2L) {
    return(TRUE)
  }
  pos <- match(as.character(waves), as.character(observed))
  identical(as.integer(diff(pos)), 1L)
}

#' Waves before a wave, in order (positional)
#'
#' @return the observed waves strictly before `wave`, most recent first.
#' @noRd
.waves_before <- function(observed, wave) {
  if (length(wave) == 0L) {
    return(observed[0])
  }
  pos <- match(as.character(wave), as.character(observed))
  if (is.na(pos) || pos <= 1L) {
    return(observed[0])
  }
  rev(observed[seq_len(pos - 1L)])
}

#' Validate a wave lag argument
#'
#' A lag counts occasions back from the outcome wave, so it is a whole number.
#' A fractional lag has no reading, and a negative one would read a later wave
#' than the outcome. Whether 0 is allowed depends on the term: a lagged outcome
#' at lag 0 is the outcome itself, while an exposure or a network feature at lag
#' 0 is the contemporaneous reading.
#'
#' @param lag the value supplied.
#' @param what the argument as the user wrote it, e.g. `"ar(lag = )"`.
#' @param min the smallest allowed lag, `0` or `1`.
#' @param scalar whether a single value is required.
#' @return `lag` as an integer vector.
#' @noRd
.assert_wave_lag <- function(lag, what, min = 1L, scalar = TRUE) {
  ok <- is.numeric(lag) && !is.factor(lag) && length(lag) >= 1L &&
    !anyNA(lag) && all(is.finite(lag)) && all(lag == round(lag))
  if (ok && scalar && length(lag) != 1L) ok <- FALSE
  if (!ok) {
    stop(
      sprintf(
        "%s must be %s whole number%s of waves; got %s.",
        what,
        if (scalar) "a single" else "one or more",
        if (scalar) "" else "s",
        paste(deparse(lag), collapse = "")
      ),
      call. = FALSE
    )
  }
  if (any(lag < min)) {
    stop(
      sprintf(
        "%s must be at least %d; got %s. %s",
        what, min, paste(lag, collapse = ", "),
        if (min >= 1L) {
          "A lag counts waves back from the outcome: 0 would be the outcome wave itself, and a negative lag a later wave."
        } else {
          "A lag counts waves back from the outcome: 0 is the outcome wave, and a negative lag would read a later wave."
        }
      ),
      call. = FALSE
    )
  }
  if (!scalar && anyDuplicated(lag) > 0L) {
    stop(sprintf("%s names the same lag twice: %s.", what, paste(lag, collapse = ", ")), call. = FALSE)
  }
  as.integer(lag)
}
