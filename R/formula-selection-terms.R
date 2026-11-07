# Selection formula terms.
#
# Role assignment is the whole point of this grammar. Roles are assigned by
# model terms and never inferred from the data, and a dyadic model is where
# that matters most: a node variable such as
# `tenure` can enter as a property of the sender, of the receiver, or of both,
# and those are three different models. Nothing about the column says which was
# meant, so rnetis refuses to guess -- a bare covariate in a Selection formula
# is an error naming the role wrappers (see .parse_selection_formula()).
#
# Lag convention follows Influence's exactly: a term with lag = 0 is
# measured at the focal wave (the wave whose ties are the outcome), lag = 1 is
# the previous wave. Keeping one convention across both model families matters
# more than optimizing either in isolation.

#' Sender (row) effect in a Selection model
#'
#' Declares a node variable as a property of the **sender** of a tie -- the
#' actor the tie originates from, under the edge direction declared in
#' [netis_data()]. In the fitted model this is a row effect: it shifts an
#' actor's general tendency to send ties.
#'
#' `sender(x)` and `receiver(x)` on the same variable are different effects and
#' both may appear in one formula. Neither implies the other.
#'
#' @param var the node variable, given as a bare column name (captured without
#'   evaluation).
#' @param lag waves back from the focal wave. `0` (default) is the focal wave
#'   itself, which is **contemporaneous** with the tie outcome and raises a
#'   design warning that temporal ordering is not established; `1` is the
#'   previous wave.
#' @param name optional column name for the assembled design array. Defaults to
#'   `snd_<var>`, with `_lag<k>` appended for a nonzero lag; supply this to resolve a collision between two terms that
#'   would otherwise share a name.
#' @return an object of class `netis_sender_term`.
#' @seealso [receiver()], [dyad()], [group()], [prior_tie()], [fit_selection()]
#' @examples
#' sender(tenure)
#' sender(tenure, lag = 1)
#' @export
sender <- function(var, lag = 0, name = NULL) {
  .new_selection_node_term(deparse(substitute(var)), lag, name, "netis_sender_term")
}

#' Receiver (column) effect in a Selection model
#'
#' Declares a node variable as a property of the **receiver** of a tie -- the
#' actor the tie points to, under the edge direction declared in
#' [netis_data()]. In the fitted model this is a column effect: it shifts an
#' actor's general tendency to attract ties.
#'
#' @inheritParams sender
#' @param name optional column name for the assembled design array. Defaults to
#'   `rcv_<var>`, with `_lag<k>` appended for a nonzero lag.
#' @return an object of class `netis_receiver_term`.
#' @seealso [sender()], [dyad()], [group()], [prior_tie()], [fit_selection()]
#' @examples
#' receiver(tenure)
#' @export
receiver <- function(var, lag = 0, name = NULL) {
  .new_selection_node_term(deparse(substitute(var)), lag, name, "netis_receiver_term")
}

#' Dyadic effect in a Selection model
#'
#' Declares a **tie variable** as a dyad-level predictor of the outcome tie --
#' a property of the pair rather than of either actor. Typical uses are a
#' second relation measured on the same actors (advice predicting friendship)
#' or a stored dyadic attribute (shared unit, geographic distance).
#'
#' `var` must be a tie variable in the [netis_data()] object. Dyadic quantities
#' *derived from node variables* -- absolute difference, category match, and
#' similar homophily constructions -- are not built by this term;
#' compute them as tie variables and pass them here, and they are used as
#' supplied without reinterpretation.
#'
#' @param var the tie variable, given as a bare column name.
#' @param lag waves back from the focal wave. `0` (default) is the focal wave,
#'   which is contemporaneous with the outcome and raises a design warning;
#'   `1` is the previous wave. To model persistence of the *outcome* relation
#'   itself, use [prior_tie()] rather than `dyad()`.
#' @param name optional column name. Defaults to `dyd_<var>`, with `_lag<k>`
#'   appended for a nonzero lag.
#' @return an object of class `netis_dyad_term`.
#' @seealso [sender()], [receiver()], [prior_tie()], [fit_selection()]
#' @examples
#' dyad(same_unit)
#' dyad(advice, lag = 1)
#' @export
dyad <- function(var, lag = 0, name = NULL) {
  .new_selection_node_term(deparse(substitute(var)), lag, name, "netis_dyad_term")
}

#' Group-level effect in a Selection model
#'
#' Declares a group variable as a predictor of tie formation. The value is
#' constant within a group at the wave a fit reads, so within any one group's
#' fitted network it is perfectly collinear with the intercept.
#' **[fit_selection()] therefore refuses a `group()` term on every route.** A
#' group-level predictor is estimated across groups as a
#' moderator of a random intercept:
#' `pooling = selection_pooling(random = "intercept", moderators = ...)`.
#'
#' @param var the group variable, given as a bare column name.
#' @param lag waves back from the focal wave. `0` (default) is the focal wave.
#' @param name optional column name. Defaults to `grp_<var>`.
#' @return an object of class `netis_group_term`.
#' @seealso [sender()], [receiver()], [dyad()], [fit_selection()]
#' @examples
#' group(group_size)
#' @export
group <- function(var, lag = 0, name = NULL) {
  .new_selection_node_term(deparse(substitute(var)), lag, name, "netis_group_term")
}

#' Tie persistence in a Selection model
#'
#' The previous wave's value of the **same** relation that is the model's
#' outcome: `y[i, j]` at the prior wave predicting `y[i, j]` now. This is the
#' dyadic analogue of [ar()] in the Influence grammar, and it takes no variable
#' argument for the same reason: the relation is definitionally the formula's
#' left-hand side, so naming it again adds nothing and admits nonsense like
#' `friendship ~ prior_tie(advice)`.
#'
#' Requires at least two waves; a single-wave specification is an error.
#'
#' When [fit_selection()] binarizes a valued outcome (`binarize =`), the prior
#' wave is binarized by the same rule, so the predictor is the earlier value of
#' the binary tie being modeled. [prior_reciprocity()] is treated the same way.
#'
#' @section Interpretation:
#' A `prior_tie()` fit is a **conditional dynamic model**: it answers what
#' predicts change given the network as it stood at the initial wave, and does
#' not model how that initial network arose. Because the same persistent actor
#' heterogeneity that the model estimates also helped generate the prior
#' network, persistence and heterogeneity are not cleanly separated -- the
#' initial-conditions problem for dynamic models. The coefficient is reported
#' as a **one-transition conditional persistence** effect; rnetis does not
#' report a long-run or cumulative multiplier, because propagating a tie effect
#' forward would require a fully specified transition process this model does
#' not define.
#'
#' @param lag waves back for the prior relation. Default `1`.
#' @param name optional column name. Defaults to `prior_tie`, or
#'   `prior_tie_lag<k>` for a lag other than 1.
#' @return an object of class `netis_prior_tie_term`.
#' @seealso [prior_reciprocity()] for the reverse direction, [ar()] for the
#'   Influence analogue, [fit_selection()]
#' @examples
#' prior_tie()
#' prior_tie(lag = 2)
#' @export
prior_tie <- function(lag = 1, name = NULL) {
  .new_selection_lagged_tie_term(lag, name, "netis_prior_tie_term")
}

#' Delayed reciprocity in a Selection model
#'
#' The previous wave's value of the outcome relation in the **reverse**
#' direction: `y[j, i]` at the prior wave predicting `y[i, j]` now. Like
#' [prior_tie()], it takes no variable argument -- the relation is the
#' formula's left-hand side.
#'
#' This is a distinct mechanism from persistence and from contemporaneous
#' reciprocity. Persistence asks whether a tie tends to survive; delayed
#' reciprocity asks whether being chosen leads to choosing back at the next
#' occasion; the model's dyadic correlation captures within-dyad dependence at
#' a single occasion. Omitting this term can distort both the persistence
#' coefficient and the estimated contemporaneous dependence.
#'
#' @param lag waves back for the reverse relation. Default `1`.
#' @param name optional column name. Defaults to `prior_recip`, or
#'   `prior_recip_lag<k>` for a lag other than 1.
#' @return an object of class `netis_prior_reciprocity_term`.
#' @seealso [prior_tie()], [fit_selection()]
#' @examples
#' prior_reciprocity()
#' @export
prior_reciprocity <- function(lag = 1, name = NULL) {
  .new_selection_lagged_tie_term(lag, name, "netis_prior_reciprocity_term")
}

#' Build a Selection term that names a variable
#'
#' Shared constructor for [sender()]/[receiver()]/[dyad()]/[group()], which
#' differ only in the role they assign and the class they carry. The variable
#' name arrives already deparsed, because `substitute()` must run in the
#' user-facing function to see the caller's unevaluated argument.
#'
#' @param variable the deparsed column name.
#' @param lag non-negative integer wave offset.
#' @param name optional explicit column name.
#' @param cls the S3 class to attach.
#' @return a classed list with `variable`, `lag`, and `name`.
#' @noRd
.new_selection_node_term <- function(variable, lag, name, cls) {
  .assert_selection_lag(lag, cls)
  structure(
    list(variable = variable, lag = as.integer(lag), name = name),
    class = c(cls, "netis_selection_term")
  )
}

#' Build a Selection term that lags the outcome relation
#'
#' Shared constructor for [prior_tie()]/[prior_reciprocity()]. Neither carries
#' a variable: the relation is the formula's response, filled in by the parser
#' once it knows what that is. A lag of 0 is refused rather than silently
#' meaning "the outcome itself".
#'
#' @param lag positive integer wave offset.
#' @param name optional explicit column name.
#' @param cls the S3 class to attach.
#' @return a classed list with `variable` (`NULL` until the parser fills it),
#'   `lag`, and `name`.
#' @noRd
.new_selection_lagged_tie_term <- function(lag, name, cls) {
  .assert_selection_lag(lag, cls)
  if (as.integer(lag) < 1L) {
    stop(
      sprintf(
        "%s(lag = %s) is not valid: the lag must be at least 1. A lag of 0 would be the outcome relation itself, which cannot predict itself.",
        .selection_term_label(cls), format(lag)
      ),
      call. = FALSE
    )
  }
  structure(
    list(variable = NULL, lag = as.integer(lag), name = name),
    class = c(cls, "netis_selection_term")
  )
}

#' @noRd
.assert_selection_lag <- function(lag, cls) {
  if (length(lag) != 1L || is.na(lag) || !is.numeric(lag) ||
      lag != as.integer(lag) || lag < 0) {
    stop(
      sprintf(
        "%s()'s lag must be a single non-negative whole number; got %s.",
        .selection_term_label(cls), paste(deparse(lag), collapse = "")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Map a Selection term class to the function name users typed
#'
#' Error messages name the call the user wrote, not the internal class.
#'
#' @noRd
.selection_term_label <- function(cls) {
  switch(
    cls[[1]],
    netis_sender_term = "sender",
    netis_receiver_term = "receiver",
    netis_dyad_term = "dyad",
    netis_group_term = "group",
    netis_prior_tie_term = "prior_tie",
    netis_prior_reciprocity_term = "prior_reciprocity",
    cls[[1]]
  )
}
