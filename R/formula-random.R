# Random-effects formula syntax for fit_influence().
#
# Reuses lme4's own `(term | group)` syntax, split out via reformulas --
# the maintained home of the formula-splitting code lme4 itself uses
# (findbars()/nobars()) -- rather than hand-rolling a parser, the same
# "R-native behavior" argument already applied to stats::terms(specials = )
# in R/formula-parse.R. nobars() strips the random-effects term(s) out,
# leaving a formula .parse_influence_formula() can consume completely
# unchanged: random-effects parsing does not need ar()/exposure()/latent()
# parsing to know it exists.
#
# The bar's left-hand side (e.g. "1 + ex_y_w", "0 + ex_y_w", "ex_y_w - 1")
# is itself a small formula, so it is parsed with stats::terms() rather
# than by hand -- reusing R's own intercept/term-label logic instead of
# re-deriving it from the deparsed text.

#' Work around a reformulas::findbars() crash on zero-argument calls
#'
#' Confirmed empirically, independent of anything rnetis-specific: the
#' minimal formula `y ~ foo()` already errors inside
#' `reformulas::findbars()` (a "subscript out of bounds" error), because
#' its internal recursive walker (`fbx()`, reformulas 0.4.1) assumes every
#' call node it descends into has at least one argument -- a call with
#' none, like `foo()`, fails the very next attempt to index into it.
#' `ar()` is written with empty
#' parentheses in ordinary use (`y ~ ar() + ...`), which triggers exactly
#' this bug the moment the formula reaches `findbars()`, before our own
#' specials parsing ever runs. `reformulas::nobars()` does **not** have the
#' same problem (verified separately: it short-circuits on any subtree with
#' no bars, so it never descends into `ar()`'s empty argument list), so
#' only the `findbars()` call needs this.
#'
#' The fix operates on a throwaway copy of the language tree, used only to
#' locate bar terms: recursively insert a dummy numeric argument into any
#' zero-argument call before handing it to `findbars()`, which only cares
#' about locating `|` expressions and never inspects a non-bar call's
#' arguments. The original, unmodified `formula` is used everywhere else --
#' `nobars()`'s `fixed_formula` below, and the untouched formula
#' `.parse_influence_formula()` receives -- so this rewrite is invisible
#' outside this one function.
#'
#' @noRd
.pad_empty_calls <- function(term) {
  if (!is.call(term)) {
    return(term)
  }
  if (length(term) == 1L) {
    return(as.call(list(term[[1]], 1)))
  }
  as.call(lapply(as.list(term), .pad_empty_calls))
}

#' @noRd
.parse_random_effects <- function(formula) {
  bars <- reformulas::findbars(.pad_empty_calls(formula))
  fixed_formula <- reformulas::nobars(formula)
  random_terms <- lapply(bars, function(b) {
    lhs_terms <- stats::terms(stats::as.formula(paste("~", deparse(b[[2]]))))
    list(
      group = deparse(b[[3]]),
      slopes = attr(lhs_terms, "term.labels"),
      intercept = attr(lhs_terms, "intercept") == 1
    )
  })
  list(fixed_formula = fixed_formula, random_terms = random_terms)
}
