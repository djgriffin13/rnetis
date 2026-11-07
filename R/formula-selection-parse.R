# Formula parsing for fit_selection().
#
# Same mechanism as .parse_influence_formula(): stats::terms(specials = ),
# the R-native approach, with the role terms as ordinary NSE constructors
# eval()'d from the extracted calls. The indexing convention was re-verified
# empirically for this grammar rather than assumed to carry over, because it
# has two properties the Influence grammar does not: a zero-argument special
# (prior_tie()) and a special named `group`, which is also an ordinary English
# word likely to appear as a column name. Both behave: specials index into the
# same de-listed variables vector, response = 1, first term = 2.

#' @noRd
.SELECTION_SPECIALS <- c("sender", "receiver", "dyad", "group",
                         "prior_tie", "prior_reciprocity")

#' Parse a Selection model formula into a specification
#'
#' @param formula a two-sided formula whose response is a tie variable.
#' @return a list with `outcome` (the tie variable), one element per role
#'   (`senders`, `receivers`, `dyads`, `groups`), `prior_tie`, and
#'   `prior_reciprocity`. Every term carries a resolved, collision-checked
#'   `name`.
#' @noRd
.parse_selection_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("fit_selection() requires a formula, e.g. friendship ~ sender(tenure) + prior_tie().", call. = FALSE)
  }

  spec_env <- list2env(
    list(
      sender = sender, receiver = receiver, dyad = dyad, group = group,
      prior_tie = prior_tie, prior_reciprocity = prior_reciprocity
    ),
    parent = baseenv()
  )

  tt <- stats::terms(formula, specials = .SELECTION_SPECIALS)
  var_list <- as.list(attr(tt, "variables"))[-1]

  response_idx <- attr(tt, "response")
  if (response_idx == 0L) {
    stop(
      "fit_selection() requires a tie variable on the left-hand side of the formula (tie_variable ~ ...). That variable is the relation being modeled.",
      call. = FALSE
    )
  }
  outcome <- deparse(var_list[[response_idx]])

  specials <- attr(tt, "specials")

  # Nothing the user wrote may be discarded. This parser partitions the
  # `variables` attribute, which never contains a compound term label, so without
  # this an interaction would be invisible rather than misrouted. Checked BEFORE the bare-covariate refusal below,
  # because `sender(x) * z` should be reported as an unsupported interaction
  # rather than as a roleless bare covariate `z`: both are true, and the first
  # is what the user actually wrote.
  .assert_terms_all_consumed(tt, setdiff(seq_along(var_list), response_idx),
                             family = "selection")
  .assert_intercept_retained(tt, "fit_selection()")

  # A bare covariate carries no role, and in a dyadic model there is no
  # defensible default: `tenure` could be a property of the sender, of the
  # receiver, or of both, and those are three different models
  # -- roles come from terms, never from the data. Refuse rather than pick
  # one.
  special_idx <- unlist(specials, use.names = FALSE)
  bare_idx <- setdiff(setdiff(seq_along(var_list), response_idx), special_idx)
  if (length(bare_idx) > 0L) {
    bare <- vapply(bare_idx, function(i) deparse(var_list[[i]]), character(1))
    stop(
      sprintf(
        "Selection formulas give every predictor an explicit role, so a bare term is not interpretable: %s. Wrap each one in sender(), receiver(), or dyad() -- for example sender(%s) for a property of the tie's sender, or sender(%s) + receiver(%s) to allow both.",
        paste(bare, collapse = ", "), bare[[1]], bare[[1]], bare[[1]]
      ),
      call. = FALSE
    )
  }

  # prior_tie()/prior_reciprocity() take no variable argument, for the same
  # reason ar() does not: the relation is the response. Checked on the
  # *unevaluated* call so the message can name the outcome, rather than
  # letting eval() fail with R's generic "object not found" for a bare symbol
  # that spec_env has no knowledge of.
  for (i in c(specials$prior_tie, specials$prior_reciprocity)) {
    .assert_prior_tie_call_has_no_variable(var_list[[i]], outcome)
  }

  ev <- function(idx) lapply(idx, function(i) eval(var_list[[i]], envir = spec_env))
  senders <- ev(specials$sender)
  receivers <- ev(specials$receiver)
  dyads <- ev(specials$dyad)
  groups <- ev(specials$group)
  prior_tie_terms <- ev(specials$prior_tie)
  prior_recip_terms <- ev(specials$prior_reciprocity)

  if (length(prior_tie_terms) > 1L) {
    stop("Only one prior_tie() term is allowed; a second would enter the same lagged relation twice. Use prior_tie(lag = ...) to choose the lag.", call. = FALSE)
  }
  if (length(prior_recip_terms) > 1L) {
    stop("Only one prior_reciprocity() term is allowed. Use prior_reciprocity(lag = ...) to choose the lag.", call. = FALSE)
  }

  # The relation these two lag is the response, filled in here -- the terms
  # themselves cannot know it, and the parser is the first point that does.
  for (i in seq_along(prior_tie_terms)) prior_tie_terms[[i]]$variable <- outcome
  for (i in seq_along(prior_recip_terms)) prior_recip_terms[[i]]$variable <- outcome

  senders <- .name_selection_terms(senders, "snd", "sender()")
  receivers <- .name_selection_terms(receivers, "rcv", "receiver()")
  dyads <- .name_selection_terms(dyads, "dyd", "dyad()")
  groups <- .name_selection_terms(groups, "grp", "group()")
  prior_tie_terms <- .name_lagged_tie_terms(prior_tie_terms, "prior_tie")
  prior_recip_terms <- .name_lagged_tie_terms(prior_recip_terms, "prior_recip")

  # Names are unique within a role by the checks above, but the assembled
  # design array is one flat namespace, so a collision across roles would be
  # just as damaging and is not caught by any per-role check.
  all_names <- c(
    vapply(senders, function(t) t$name, character(1)),
    vapply(receivers, function(t) t$name, character(1)),
    vapply(dyads, function(t) t$name, character(1)),
    vapply(groups, function(t) t$name, character(1)),
    vapply(prior_tie_terms, function(t) t$name, character(1)),
    vapply(prior_recip_terms, function(t) t$name, character(1))
  )
  if (anyDuplicated(all_names) > 0L) {
    stop(
      sprintf(
        "Two Selection terms would produce the same design column name: %s. Give one of them a distinct name = argument.",
        paste(unique(all_names[duplicated(all_names)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(
    outcome = outcome,
    senders = senders,
    receivers = receivers,
    dyads = dyads,
    groups = groups,
    prior_tie = if (length(prior_tie_terms)) prior_tie_terms[[1]] else NULL,
    prior_reciprocity = if (length(prior_recip_terms)) prior_recip_terms[[1]] else NULL
  )
}

#' Resolve default names for one role's terms and reject collisions
#'
#' Default is `<prefix>_<variable>`, with `_lag<k>` appended for a non-focal
#' lag -- without that suffix `sender(x) + sender(x, lag = 1)`, a perfectly
#' reasonable specification, would silently collide.
#'
#' @noRd
.name_selection_terms <- function(terms_list, prefix, label) {
  if (length(terms_list) == 0L) {
    return(terms_list)
  }
  nms <- vapply(terms_list, function(t) {
    if (!is.null(t$name)) {
      return(t$name)
    }
    base <- paste0(prefix, "_", t$variable)
    if (t$lag != 0L) paste0(base, "_lag", t$lag) else base
  }, character(1))
  if (anyDuplicated(nms) > 0L) {
    stop(
      sprintf(
        "Multiple %s terms would produce the same column name: %s. Give them distinct name = arguments.",
        label, paste(unique(nms[duplicated(nms)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  for (i in seq_along(terms_list)) terms_list[[i]]$name <- nms[i]
  terms_list
}

#' Resolve the default name for a lagged-outcome-relation term
#'
#' The default omits the lag suffix at the conventional lag of 1, since
#' `prior_tie` reads better than `prior_tie_lag1` and one term of each kind is
#' allowed per formula.
#'
#' @noRd
.name_lagged_tie_terms <- function(terms_list, base) {
  if (length(terms_list) == 0L) {
    return(terms_list)
  }
  for (i in seq_along(terms_list)) {
    if (is.null(terms_list[[i]]$name)) {
      terms_list[[i]]$name <- if (terms_list[[i]]$lag == 1L) {
        base
      } else {
        paste0(base, "_lag", terms_list[[i]]$lag)
      }
    }
  }
  terms_list
}

#' Reject a variable argument to prior_tie()/prior_reciprocity()
#'
#' Mirrors .assert_ar_call_has_no_variable(). A bare symbol argument that
#' is not the named `lag` or `name` is the mistake this catches. A numeric
#' literal or an expression is not a symbol and passes through, so
#' `prior_tie(2)` and `prior_tie(lag = 2)` are both valid.
#'
#' Two messages, because the two mistakes have different fixes: naming the
#' outcome relation is redundant, while naming a *different* relation means
#' the user wanted dyad(), which is a genuinely different model term.
#'
#' @noRd
.assert_prior_tie_call_has_no_variable <- function(call_obj, outcome) {
  fname <- deparse(call_obj[[1]])
  args <- as.list(call_obj)[-1]
  if (length(args) == 0L) {
    return(invisible(TRUE))
  }
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))
  for (i in seq_along(args)) {
    val <- args[[i]]
    if (!arg_names[i] %in% c("lag", "name") && is.symbol(val)) {
      var_text <- deparse(val)
      if (identical(var_text, outcome)) {
        stop(
          sprintf(
            "%s(%s) is not valid: %s() takes no variable argument. The relation it lags is always the model's outcome ('%s'), read from the formula's left-hand side. Write %s() or %s(lag = ...) instead.",
            fname, var_text, fname, outcome, fname, fname
          ),
          call. = FALSE
        )
      }
      stop(
        sprintf(
          "%s(%s) is not valid: %s() lags the model's outcome relation ('%s'), not '%s'. To use a different relation as a predictor, write dyad(%s, lag = 1).",
          fname, var_text, fname, outcome, var_text, var_text
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}
