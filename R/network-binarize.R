# Binarization rules for binarize() (R/network-spec.R).
#
# "Unweighted" is never a synonym for "weight > 0". Every rule here is an
# explicit, named choice, constructed the same way exposure()'s
# mechanism = bi(delta = 0.8, alpha = ..., gamma = ...) is: a constructor, not a
# string. presence() is the one rule that is NOT a value predicate -- it resolves
# structural presence via .resolve_tie_presence(), and is undefined (errors) for a network
# whose presence_mode is complete_dyadic with no declared indicator/rule.
#
# quantile_threshold() is named as such, not quantile(), specifically to avoid
# a second stats::-masking collision beyond the package's one accepted case
# (ar()).

#' @noRd
.binarize_scopes <- c("snapshot", "group", "wave", "global")

#' Binarization rule: structural edge presence
#'
#' Reads presence from the network's declared `presence_mode` rather than from the tie variable's
#' value: a `row_presence` network reads every stored row as present
#' regardless of its recorded weight (a measured zero-weight tie is still
#' present); a `complete_dyadic` network resolves presence from a declared
#' `presence_col`/`presence_rule` and errors if neither was declared, rather
#' than guessing `value != 0`. An inferred `row_presence` network that stores
#' zeros is refused too, unless `zero_meaning = "measured_zero"` is declared. Only
#' applies directly to a tie-variable reference: a derived (already
#' binarized/symmetrized/multiplexed) network has no presence semantics of its own
#' to read, and is refused.
#'
#' @return an object of class `netis_binarize_rule`.
#' @seealso [binarize()], [nonzero()], [positive()]
#' @examples
#' presence()
#' binarize(communication, rule = presence())
#' @export
presence <- function() {
  structure(list(type = "presence"), class = "netis_binarize_rule")
}

#' Binarization rule: nonzero value
#'
#' `value != 0`. Both positive and negative relationships count present --
#' the appropriate reading for a signed network, where a negative value is a
#' real (antagonistic) relationship rather than an absence.
#'
#' @return an object of class `netis_binarize_rule`.
#' @seealso [presence()], [positive()]
#' @examples
#' nonzero()
#' # A signed (e.g. centered advice-seeking) network: both antagonism (< 0)
#' # and support (> 0) count as a present tie.
#' binarize(sentiment, rule = nonzero())
#' @export
nonzero <- function() {
  structure(list(type = "nonzero"), class = "netis_binarize_rule")
}

#' Binarization rule: positive value
#'
#' `value > 0`. An explicit choice -- not the meaning of "unweighted" by
#' default -- for when negative and zero values should both read as absent.
#'
#' @return an object of class `netis_binarize_rule`.
#' @seealso [presence()], [nonzero()]
#' @examples
#' positive()
#' # Only strictly positive sentiment counts as a tie; negative and zero do not.
#' binarize(sentiment, rule = positive())
#' @export
positive <- function() {
  structure(list(type = "positive"), class = "netis_binarize_rule")
}

#' Binarization rule: value above a fixed threshold
#'
#' Counts a tie as present when its value is above a fixed threshold:
#' `value > c`.
#'
#' @param c the threshold.
#' @return an object of class `netis_binarize_rule`.
#' @seealso [abs_threshold()], [quantile_threshold()]
#' @examples
#' threshold(2)
#' # Only communication counts of more than 2 events form a tie.
#' binarize(communication, rule = threshold(2))
#' @export
threshold <- function(c) {
  if (!is.numeric(c) || length(c) != 1L) {
    stop("threshold()'s c must be a single number.", call. = FALSE)
  }
  structure(list(type = "threshold", c = c), class = "netis_binarize_rule")
}

#' Binarization rule: absolute value above a fixed threshold
#'
#' `abs(value) > c`. Useful for a signed/centered network where both strongly
#' positive and strongly negative values should count as present.
#'
#' @param c the threshold.
#' @return an object of class `netis_binarize_rule`.
#' @seealso [threshold()]
#' @examples
#' abs_threshold(1.5)
#' # Either strong support (> 1.5) or strong antagonism (< -1.5) forms a tie.
#' binarize(sentiment, rule = abs_threshold(1.5))
#' @export
abs_threshold <- function(c) {
  if (!is.numeric(c) || length(c) != 1L) {
    stop("abs_threshold()'s c must be a single number.", call. = FALSE)
  }
  structure(list(type = "abs_threshold", c = c), class = "netis_binarize_rule")
}

#' Binarization rule: value above a data-derived quantile
#'
#' `value > quantile(values, p)`. Named `quantile_threshold()`, not
#' `quantile()`, to avoid masking `stats::quantile()` -- the package already
#' accepts one such collision ([ar()]) and does not add a second.
#'
#' @param p the quantile probability, in `(0, 1)`.
#' @param scope which values the quantile is computed over: `"snapshot"`
#'   (default; the resolved group/wave network only -- deliberately, to avoid
#'   a later wave's data determining an earlier wave's network structure),
#'   `"group"` (all waves within the resolved group), `"wave"` (all groups at
#'   the resolved wave), or `"global"` (every group and wave). The pool is the
#'   stored rows, so it holds only dyads with a row. A data-derived cut needs a
#'   direct tie-variable reference; on a derived network it is refused at every
#'   scope. The cut is computed when the network is resolved and is not
#'   reported by [compute_exposure()] or [compute_centrality()]; compute it from
#'   [netis_ties()] if you need the number.
#' @return an object of class `netis_binarize_rule`.
#' @seealso [above_mean()], [above_median()]
#' @examples
#' quantile_threshold(0.9)
#' # Only the top decile of communication ties, computed within each
#' # resolved (group, wave) snapshot, count as present.
#' binarize(communication, rule = quantile_threshold(0.9))
#' # Compute the cutoff over the whole group's history instead of one snapshot.
#' binarize(communication, rule = quantile_threshold(0.9, scope = "group"))
#' @export
quantile_threshold <- function(p, scope = c("snapshot", "group", "wave", "global")) {
  scope <- match.arg(scope, .binarize_scopes)
  if (!is.numeric(p) || length(p) != 1L || p <= 0 || p >= 1) {
    stop("quantile_threshold()'s p must be a single number in (0, 1).", call. = FALSE)
  }
  structure(list(type = "quantile", p = p, scope = scope), class = "netis_binarize_rule")
}

#' Binarization rule: value above the data-derived mean
#'
#' Reads a tie as present when its value exceeds the mean of the values in
#' scope. The threshold is **derived from the data**, so the scope matters:
#' it defaults to the resolved snapshot (group x wave), because computing
#' one cut across waves would let a later wave decide an earlier wave's
#' network -- a temporal leak into a model whose purpose is temporal
#' ordering. Reach for
#' [above_median()] instead when the value distribution is skewed.
#'
#' @inheritParams quantile_threshold
#' @return an object of class `netis_binarize_rule`.
#' @seealso [quantile_threshold()], [above_median()]
#' @examples
#' above_mean()
#' binarize(communication, rule = above_mean())
#' @export
above_mean <- function(scope = c("snapshot", "group", "wave", "global")) {
  scope <- match.arg(scope, .binarize_scopes)
  structure(list(type = "above_mean", scope = scope), class = "netis_binarize_rule")
}

#' Binarization rule: value above the data-derived median
#'
#' As [above_mean()], but cutting at the median, which is what you want
#' when a few very large tie values would drag a mean threshold above most
#' of the distribution. The same scope rule applies.
#'
#' @inheritParams quantile_threshold
#' @return an object of class `netis_binarize_rule`.
#' @seealso [quantile_threshold()], [above_mean()]
#' @examples
#' above_median()
#' # Robust to outliers relative to above_mean() when the tie-value
#' # distribution is skewed (e.g. communication counts).
#' binarize(communication, rule = above_median())
#' @export
above_median <- function(scope = c("snapshot", "group", "wave", "global")) {
  scope <- match.arg(scope, .binarize_scopes)
  structure(list(type = "above_median", scope = scope), class = "netis_binarize_rule")
}

#' A user-supplied binarization rule
#'
#' Wraps an arbitrary predicate as a recorded binarization rule, for the
#' cutoffs the built-in rules cannot express -- an asymmetric rule, a band,
#' a multi-condition test. It is a rule like any other, not an escape
#' hatch: the rule and its name stay on the specification. Prefer a built-in
#' rule where one fits, since a named rule reconstructs from a saved
#' specification and an anonymous function does not.
#'
#' @param fn `function(values)` returning a logical vector the same length as
#'   `values`, `TRUE` where the tie should read as present.
#' @param name a name for provenance, since an anonymous function cannot be
#'   fully reconstructed from a saved specification.
#' @return an object of class `netis_binarize_rule`.
#' @seealso [threshold()], [abs_threshold()] for the common cases this
#'   generalizes; use a custom rule only when none of the built-in rules
#'   express the intended cutoff (e.g. an asymmetric or multi-condition rule).
#' @examples
#' # Present only for values in a specific band, e.g. moderate but not
#' # extreme communication frequency.
#' custom_binarize_rule(function(v) v > 1 & v < 10, name = "moderate_band")
#' binarize(communication, rule = custom_binarize_rule(
#'   function(v) v > 1 & v < 10,
#'   name = "moderate_band"
#' ))
#' @export
custom_binarize_rule <- function(fn, name = NULL) {
  if (!is.function(fn)) {
    stop("fn must be a function(values) returning a logical vector.", call. = FALSE)
  }
  structure(list(type = "custom", fn = fn, name = name), class = "netis_binarize_rule")
}

#' @noRd
.resolve_network_binarize <- function(spec, data, group, wave) {
  inner <- .resolve_network(spec$network, data, group, wave)
  out <- if (identical(spec$rule$type, "presence")) {
    .apply_presence_rule(inner, data)
  } else {
    .apply_value_binarize_rule(inner, spec$rule, data, group, wave)
  }
  out$resolved <- list(type = "binarize", rule = spec$rule, threshold_used = out$threshold_used, inner = inner$resolved)
  out
}

#' Resolve structural presence for a direct tie-variable reference
#'
#' `inner$descriptor` is only populated by `.resolve_network_ref()`
#' (R/network-spec.R) -- a derived network (already binarized, symmetrized, or
#' multiplexed) has `descriptor = NULL`, which is exactly the signal used here
#' to refuse: `presence()` reads schema-declared semantics, and a derived
#' network has none of its own.
#'
#' @noRd
.apply_presence_rule <- function(inner, data) {
  if (is.null(inner$descriptor)) {
    stop(
      "binarize(rule = presence()) requires a direct tie-variable reference (a bare column name) -- a derived network (already binarized, symmetrized, or multiplexed) has no declared presence semantics of its own to read.",
      call. = FALSE
    )
  }
  s <- netis_schema(data)
  ties_g <- inner$ties_snapshot
  present <- .resolve_tie_presence(inner$descriptor, ties_g, inner$variable) # errors if undetermined
  M <- inner$M
  B <- matrix(0, nrow = nrow(M), ncol = ncol(M), dimnames = dimnames(M))
  if (nrow(ties_g) > 0L) {
    src <- as.character(ties_g[[s$source_col]])
    tgt <- as.character(ties_g[[s$target_col]])
    keep <- src %in% inner$ids & tgt %in% inner$ids
    if (any(keep)) {
      B <- .place_declared(B, src[keep], tgt[keep], as.numeric(present[keep]),
                           isTRUE(inner$directed))
    }
  }
  list(
    M = B, ids = inner$ids, directed = inner$directed, weighted = FALSE,
    weight_semantics = "unitless", variable = inner$variable, descriptor = NULL,
    ties_snapshot = ties_g, threshold_used = NULL
  )
}

#' Apply a value-based binarize rule elementwise to the resolved matrix
#'
#' Value-based rules (everything except `presence()`) apply their predicate
#' to the cell *value*. This is only safe as-is when the predicate is
#' guaranteed false at `v = 0` for every cell the predicate ever sees --
#' true for `nonzero()`/`positive()`/`abs_threshold(c >= 0)`, but **not**
#' guaranteed for `threshold(c)` with `c < 0`, `abs_threshold(c < 0)`, or an
#' arbitrary `custom_binarize_rule()`. Any of those, applied naively to `M`
#' (whose fill value for a cell with no underlying row is also `0`), would
#' silently turn every non-edge into a phantom edge. Guarded here by
#' `.has_row_mask()` (R/network-spec.R): for a direct tie-variable
#' reference (`inner$ties_snapshot` available), the predicate's result is
#' restricted to cells that genuinely have a stored row, regardless of what
#' the predicate says about an unobserved `0`. A derived network
#' (`ties_snapshot` is `NULL` -- already binarized/symmetrized/multiplexed)
#' has no further "missing row" ambiguity: its `M` is already the fully
#' resolved intended representation, so the predicate applies to it
#' directly, matching `.resolve_component_presence()`'s same reasoning for
#' derived networks elsewhere in this layer.
#'
#' @noRd
.apply_value_binarize_rule <- function(inner, rule, data, group, wave) {
  M <- inner$M
  threshold_used <- NULL

  pred <- if (rule$type == "nonzero") {
    function(v) v != 0
  } else if (rule$type == "positive") {
    function(v) v > 0
  } else if (rule$type == "threshold") {
    function(v) v > rule$c
  } else if (rule$type == "abs_threshold") {
    function(v) abs(v) > rule$c
  } else if (rule$type %in% c("quantile", "above_mean", "above_median")) {
    pool <- .scoped_value_pool(inner, data, group, wave, rule$scope)
    threshold_used <- if (rule$type == "quantile") {
      stats::quantile(pool, probs = rule$p, na.rm = TRUE, names = FALSE)
    } else if (rule$type == "above_mean") {
      mean(pool, na.rm = TRUE)
    } else {
      stats::median(pool, na.rm = TRUE)
    }
    function(v) v > threshold_used
  } else if (rule$type == "custom") {
    rule$fn
  } else {
    stop(sprintf("Unknown binarize rule type '%s'.", rule$type), call. = FALSE) # nocov
  }

  raw <- pred(M)
  if (!is.null(inner$ties_snapshot)) {
    s <- netis_schema(data)
    has_row <- .has_row_mask(inner$ties_snapshot, inner$ids, s$source_col, s$target_col,
                             directed = isTRUE(inner$directed))
    raw <- raw & has_row
  }

  B <- matrix(as.numeric(raw), nrow = nrow(M), ncol = ncol(M), dimnames = dimnames(M))
  list(
    M = B, ids = inner$ids, directed = inner$directed, weighted = FALSE,
    weight_semantics = "unitless", variable = inner$variable, descriptor = NULL,
    ties_snapshot = inner$ties_snapshot, threshold_used = threshold_used
  )
}

#' The value pool a data-derived threshold rule (quantile/above_mean/
#' above_median) is computed over, per its declared scope
#'
#' Requires a direct tie-variable reference for **every** scope, including
#' `"snapshot"` -- `inner$ties_snapshot`/`inner$variable` are only populated
#' by `.resolve_network_ref()` (R/network-spec.R). A symmetrized network's
#' resolved matrix can hold a value at a cell that duplicates its mirror
#' (e.g. under `rule = "max"`, both `M[i,j]` and `M[j,i]` carry the larger of
#' the two raw values), so "the pool of values at this snapshot" does not
#' faithfully represent what was actually observed; a
#' multiplex-derived network combines several variables and has no single
#' pool to speak of at all. Both cases refuse here rather than silently
#' computing a statistic over a distorted or undefined pool -- the same
#' derived-network refusal `presence()` (`.apply_presence_rule()`, above)
#' applies for the same underlying reason.
#'
#' `"snapshot"` (default, for a direct reference) uses only the
#' already-resolved group/wave network's own recorded values -- deliberately,
#' so a threshold never depends on data from a different wave -- which would
#' leak information across the very boundary the analysis is about.
#'
#' @noRd
.scoped_value_pool <- function(inner, data, group, wave, scope) {
  if (is.null(inner$ties_snapshot) || is.null(inner$variable)) {
    stop(
      sprintf(
        "A %s-scoped binarize threshold requires a direct tie-variable reference; it cannot be computed for a derived (symmetrized or multiplexed) network. Apply the rule before deriving, or supply a fixed threshold() instead.",
        scope
      ),
      call. = FALSE
    )
  }
  if (identical(scope, "snapshot")) {
    return(inner$ties_snapshot[[inner$variable]])
  }
  s <- netis_schema(data)
  ties_all <- netis_ties(data)
  var <- inner$variable
  if (identical(scope, "group")) {
    return(ties_all[[var]][as.character(ties_all[[s$group_col]]) == group])
  }
  if (identical(scope, "wave")) {
    if (is.null(wave) || !(s$wave_col %in% names(ties_all))) {
      return(ties_all[[var]])
    }
    return(ties_all[[var]][ties_all[[s$wave_col]] == wave])
  }
  ties_all[[var]] # "global"
}
