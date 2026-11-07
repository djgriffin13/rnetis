# Which waves a Selection fit reads: T = 1 or T = 2, and nothing else implicitly.
#
# This is the Selection counterpart of the Influence rule in influence-fit.R,
# and it is deliberately the same rule with the same vocabulary rather than a
# parallel one. A user who has read what
# `fit_influence(waves = )` means already knows what this means.
#
# Every Selection fit is one network per group through `amen::ame()`. Which
# wave supplies the outcome is a question about the design, answered here.
#
# THE SCOPE, STATED ONCE. Two or more waves supplying an outcome is
# replication, and replication treats repeated observations of the same actors
# as independent given the AME structure. They are not. The package refuses it
# rather than fitting it, which is why `ame_rep()` is not used. So at most
# ONE wave ever supplies the outcome:
#
#   0 waves            a single cross-section per group.
#   1 wave             that wave is the outcome. A lagged relation term has
#                      nothing to reach back to and is refused upstream by
#                      .check_selection_wave_depth().
#   2 waves            the later wave is the outcome; the earlier one is
#                      available to lagged terms. With no lagged term at all,
#                      both waves would supply an outcome -- that is
#                      replication, and it is refused by name.
#   3 or more waves    refused unless `waves = c(from, to)` names exactly two.
#                      `to` is the outcome and a lag of 1 reads `from`, also
#                      when the pair is not adjacent; a deeper lag steps back
#                      from `from` through the earlier observed waves. Waves
#                      strictly between `from` and `to` are never read.
#
# The refusal at three-plus waves is the point of the whole file. rnetis will
# not pick a transition out of a longer panel: picking one silently is how a
# ten-wave dataset gets reported as though all ten were modeled.

#' Resolve which wave supplies the outcome, and which are merely available
#'
#' @param spec a parsed Selection formula.
#' @param schema a `netis_data` schema.
#' @param waves the user's `waves` argument: `NULL`, or `c(from, to)`.
#' @return a list with `outcome_waves` (length 0 or 1), `initial_waves` (those
#'   consumed as an initial condition by a lagged relation), `waves_available`
#'   (every wave the data holds), `waves_analyzed` (those the fit reads, by any
#'   term), `lag_sequence` (the waves lags step back through, ending with the
#'   outcome wave; see `.lag_sequence()`), and `reason` (a sentence for
#'   `summary()`).
#' @noRd
.resolve_selection_waves <- function(spec, schema, waves = NULL) {
  # Already in time order (netis_data() builds it); sorting again would put
  # text labels back in alphabetical order.
  observed <- schema$waves
  n_waves <- length(observed)

  # The deepest LAGGED RELATION term sets how many leading waves become
  # initial conditions. A lagged covariate does not: it reaches back for a
  # predictor value without stopping its own wave supplying an outcome.
  lags <- vapply(
    Filter(Negate(is.null), list(spec$prior_tie, spec$prior_reciprocity)),
    function(t) t$lag, numeric(1)
  )
  depth <- if (length(lags) == 0L) 0L else as.integer(max(lags))

  if (n_waves == 0L) {
    if (!is.null(waves)) {
      stop(
        "waves = was given, but this data has no wave dimension: there are no waves to name. Remove waves =, or supply data with a wave column.",
        call. = FALSE
      )
    }
    return(.selection_wave_result(
      outcome = NULL, initial = NULL, observed = observed,
      reason = "the data has no wave dimension, so there is one network per group"
    ))
  }

  if (!is.null(waves)) {
    # Same validation as Influence -- exactly two, both observed, from < to.
    # Shared on purpose: two functions that accept `waves = c(from, to)` must
    # not drift into accepting different things.
    pair <- .resolve_influence_waves(schema, waves)
    lag_seq <- .lag_sequence(observed, pair)
    return(.selection_wave_result(
      outcome = pair[2],
      initial = .selection_waves_read(spec, lag_seq, relation_only = TRUE),
      read = .selection_waves_read(spec, lag_seq),
      observed = observed, named = TRUE, lag_sequence = lag_seq,
      reason = sprintf(
        "waves = c(%s, %s) named the transition; %s is the outcome and %s supplies its prior values",
        pair[1], pair[2], pair[2], pair[1]
      )
    ))
  }

  if (n_waves == 1L) {
    return(.selection_wave_result(
      outcome = observed, initial = NULL, observed = observed,
      lag_sequence = observed,
      reason = "one observed wave, fitted as a single cross-section"
    ))
  }

  if (n_waves == 2L) {
    if (depth == 0L) {
      # Both waves would supply an outcome. That is replication, and this
      # package does not fit it. Refuse with the two ways forward rather than
      # quietly modeling the later wave and discarding the earlier.
      stop(
        sprintf(
          "This data has two waves (%s) and the formula has no lagged relation term, so both waves would supply an outcome. Fitting them together would treat two observations of the same actors as independent given the AME structure, which they are not, and this package does not do that. Either add prior_tie() -- which makes the earlier wave the initial condition and models the transition -- or name what to fit with waves = c(%s, %s), which fits the later wave and leaves the earlier one available to lagged predictors.",
          paste(observed, collapse = ", "), observed[1], observed[2]
        ),
        call. = FALSE
      )
    }
    return(.selection_wave_result(
      outcome = observed[2], initial = observed[1], observed = observed,
      read = .selection_waves_read(spec, observed),
      lag_sequence = observed,
      reason = sprintf(
        "two observed waves; %s supplies the initial condition for the lagged relation and %s is the modeled outcome",
        observed[1], observed[2]
      )
    ))
  }

  # Three or more. rnetis does not choose.
  stop(
    sprintf(
      "This data has %d waves (%s), and this version fits one transition. rnetis will not choose which one: a fit reported without naming its transition reads as though the whole panel were modeled. Name it -- waves = c(%s, %s) for the last transition, or any other observed pair. In the named pair, `to` is the outcome and a lag of 1 reads `from`.",
      n_waves, paste(observed, collapse = ", "),
      observed[n_waves - 1L], observed[n_waves]
    ),
    call. = FALSE
  )
}

#' The waves a specification's lagged terms read
#'
#' Each lag steps back from the outcome wave (the last element of `lag_seq`)
#' by position. A lag reaching before the first wave of the sequence is
#' refused here, naming the term, rather than deep inside assembly.
#'
#' @param relation_only only the lagged relation terms ([prior_tie()],
#'   [prior_reciprocity()]), which supply the initial condition.
#' @return the waves read, in time order.
#' @noRd
.selection_waves_read <- function(spec, lag_seq, relation_only = FALSE) {
  terms <- Filter(Negate(is.null), list(spec$prior_tie, spec$prior_reciprocity))
  if (!relation_only) {
    terms <- c(terms, spec$senders, spec$receivers, spec$dyads, spec$groups)
  }
  n <- length(lag_seq)
  read <- character(0)
  for (term in terms) {
    if (term$lag == 0L) next
    if (n - term$lag < 1L) {
      stop(
        sprintf(
          "%s(lag = %d) reaches %d wave(s) before the outcome wave %s, but only %d earlier wave(s) are available to this fit (%s). A named transition c(from, to) reads `from` at lag 1 and never the waves between the two. Reduce the lag, or name a later `from`.",
          .selection_term_label(class(term)), term$lag, term$lag, lag_seq[[n]], n - 1L,
          paste(lag_seq, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    read <- c(read, as.character(lag_seq[[n - term$lag]]))
  }
  out <- lag_seq[as.character(lag_seq) %in% read]
  if (length(out) == 0L) NULL else out
}

#' Assemble the resolution, and work out what went unused
#'
#' `waves_unused` is computed here rather than at each return so that every
#' path reports it the same way. A wave counts as used if the fit reads it --
#' as the outcome, as the initial condition a lagged relation reaches back to,
#' or as the wave a lagged covariate is read at.
#'
#' @noRd
.selection_wave_result <- function(outcome, initial, observed, reason,
                                   named = FALSE, read = NULL,
                                   lag_sequence = NULL) {
  used <- as.character(c(initial, read, outcome))
  analyzed <- observed[as.character(observed) %in% used]
  list(
    outcome_waves = outcome,
    initial_waves = initial,
    waves_available = observed,
    waves_analyzed = analyzed,
    waves_unused = observed[!as.character(observed) %in% used],
    lag_sequence = lag_sequence,
    named = named,
    reason = reason
  )
}

#' The wave sentence `summary()` prints for a Selection fit
#'
#' Says which waves were read AND how many were not. A dataset with ten waves
#' fitted over one transition must never print as though ten were modeled;
#' stating the unused count is what makes the omission visible without the
#' reader having to compare two numbers themselves.
#'
#' @noRd
.format_selection_waves <- function(resolved) {
  if (is.null(resolved) || length(resolved$waves_available) == 0L) {
    return("Waves: none; a single cross-section per group")
  }
  n_avail <- length(resolved$waves_available)
  if (n_avail == 1L) {
    return("Waves: one wave; a single cross-section per group")
  }
  n_unused <- length(resolved$waves_unused)
  sprintf(
    "Waves: analyzed %s of %d present; %d wave%s not used",
    paste(as.character(resolved$waves_analyzed), collapse = " -> "),
    n_avail, n_unused, if (n_unused == 1L) "" else "s"
  )
}
