# The shared fitted object.
#
# netis_fit is the base every fitted object carries; netis_influence and
# netis_selection add their own family-specific fields on top. Defining the
# required fields in one constructor is what keeps two families from ending up with
# two subtly different notions of the same object.
#
# BOTH FAMILIES CONSTRUCT THROUGH HERE. With two construction paths, a field
# added to one family's object and not the other's would not be caught; with
# one, `.NETIS_FIT_REQUIRED` is enforced on every fit at construction rather
# than asserted about one in a test.

#' Fields every rnetis fit carries
#'
#' Split by where they come from, because that is what makes the object
#' auditable: a reader can tell which values were requested, which were
#' resolved, and which the backend returned.
#'
#' @noRd
.NETIS_FIT_REQUIRED <- c(
  # From the specification
  "spec", "call", "netis_version",
  # Backend identity
  "backend", "backend_version",
  # The estimation regime the fit ran under, so summary() can state it. NULL on
  # the deterministic optimizers (stats, lme4, lnam), which have no regime to
  # record; the field is not optional, because absent and "none apply" are
  # different answers.
  "estimation_controls",
  # From backend_extract()
  "coefficients", "vcov", "convergence", "loglik",
  # Computed downstream
  "diagnostics"
)

#' Construct a fitted rnetis object with the shared required fields
#'
#' @param subclass `"netis_influence"` or `"netis_selection"`.
#' @param fields a named list of required fields; every name in
#'   `.NETIS_FIT_REQUIRED` must be present, though many may be `NULL` (an
#'   absent value and an absent *field* are different things, and only the
#'   second is an error).
#' @param extra a named list of family-specific fields.
#' @return an object of class `c(subclass, "netis_fit")`.
#' @noRd
.new_netis_fit <- function(subclass, fields, extra = list()) {
  missing <- setdiff(.NETIS_FIT_REQUIRED, names(fields))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "Internal error: a %s object is missing required field(s): %s. Every required field must be present even when its value is NULL, so that a saved fit can be read without the original script.",
        subclass, paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  clash <- intersect(names(extra), names(fields))
  if (length(clash) > 0L) {
    stop(
      sprintf(
        "Internal error: family-specific field(s) %s would overwrite a shared required field.",
        paste(clash, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  structure(c(fields, extra), class = c(subclass, "netis_fit"))
}

# --- Posterior draws ---------------------------------------------------------
#
# A sampler does not necessarily retain
# per-draw values for everything it estimates, and a quantity available only
# as a posterior mean cannot support the same claims as one available per
# draw. amen retains draws for coefficients and variance components but
# returns additive actor effects as posterior means only.
#
# Encoding that distinction in the object -- rather than in the head of
# whoever writes the effect-size code -- is what stops a plug-in interval
# being reported as a full posterior interval.

#' Represent what a sampler kept, and what it only summarized
#'
#' The distinction is the point of the class. A quantity the sampler stored at
#' every kept scan is a draw and supports an interval; a quantity it reduced to
#' a posterior mean is a summary and does not. Keeping them in separate slots
#' is what stops a summary being reported as though its uncertainty had been
#' propagated.
#'
#' @param parameters a draws-by-parameter matrix of genuinely per-draw values.
#' @param summarized a named list of quantities available only as posterior
#' summaries, each of which must not be treated as a draw.
#' @param n_chains number of chains the draws came from.
#' @param note text
#' stating what these draws can and cannot establish.
#' @param chain optional chain label per row of `parameters`. Several chains
#'   are stacked in `parameters`, and this is what lets a diagnostic compare
#'   them rather than treat the stack as one chain.
#' @return an object of
#' class `netis_draws`.
#' @noRd
.new_netis_draws <- function(parameters, summarized = list(),
                             n_chains = 1L, note = NULL, chain = NULL) {
  if (!is.matrix(parameters)) {
    stop("Internal error: posterior draws must be a draws-by-parameter matrix.", call. = FALSE)
  }
  if (!is.null(chain) && length(chain) != nrow(parameters)) {
    stop("Internal error: one chain label is needed per draw.", call. = FALSE)
  }
  structure(
    list(
      parameters = parameters,
      summarized = summarized,
      n_draws = nrow(parameters),
      n_chains = n_chains,
      chain = chain,
      note = note
    ),
    class = "netis_draws"
  )
}

#' Describe the uncertainty a contrast over these draws can carry
#'
#' Produces the sentence that must accompany any interval computed from a
#' partially-retained posterior, naming the quantities held at their posterior
#' means rather than letting the interval imply they were integrated over.
#'
#' @noRd
.describe_draw_coverage <- function(draws) {
  if (is.null(draws)) {
    return("No posterior draws were retained, so no interval can reflect posterior uncertainty.")
  }
  summarized <- names(draws$summarized)
  if (length(summarized) == 0L) {
    return(sprintf(
      "Intervals reflect posterior uncertainty in all %d retained parameters.",
      ncol(draws$parameters)
    ))
  }
  # PLUG-IN, AND NAMED AS SUCH IN THE FIRST CLAUSE. A sentence that led with
  # what the interval DOES reflect and reached the limitation at the end would
  # let someone skimming a printed effect see "posterior uncertainty" and an
  # interval and reasonably conclude the interval was fully integrated. The
  # label goes first so it cannot be skimmed past, and a future maintainer
  # reading a stored coverage string sees the word "plug-in" in it.
  sprintf(
    "PLUG-IN interval: evaluated at posterior-mean %s, which %s held fixed rather than integrated over. It reflects uncertainty in the retained parameters only (%s), and is therefore narrower than a fully propagated one.",
    paste(summarized, collapse = ", "),
    if (length(summarized) == 1L) "is" else "are",
    paste(colnames(draws$parameters), collapse = ", ")
  )
}
