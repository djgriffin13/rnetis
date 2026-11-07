# Internal exposure specification object.
#
# Not exported: compute_exposure() (R/exposure-compute.R) is the only
# producer, and fit_influence() computes its exposure() terms through
# compute_exposure(), so the two share exactly one resolution engine.

#' @noRd
.new_exposure_spec <- function(content, conduit, name = NULL,
                                normalize = "none", mechanism = "linear",
                                isolate_rule = "na",
                                direction = "source_influences_target") {
  if (!is.character(content) || length(content) != 1L || is.na(content)) {
    stop("content must be a single node variable name.", call. = FALSE)
  }
  .assert_enum(isolate_rule, c("na", "zero", "error"), "isolate_rule")
  .assert_enum(direction, c("source_influences_target", "target_influences_source"), "direction")

  resolved <- .resolve_mechanism(mechanism)
  .assert_enum(normalize, c("none", resolved$supports_normalize), "normalize")

  conduit <- .as_conduit(conduit)
  name_given <- !is.null(name)
  if (is.null(name)) {
    # A derived conduit has no single variable name, so the label is DEFINED
    # (`.conduit_label()`) rather than defaulted -- a fallback that happened to
    # produce a string would produce a different one per construction and none
    # of them stable enough to reference in a formula.
    name <- paste0("ex_", content, "_", .conduit_label(conduit))
  }

  structure(
    list(
      name = name,
      name_given = name_given,
      content = content,
      conduit = conduit,
      normalize = normalize,
      mechanism = resolved,
      requires = resolved$requires,
      isolate_rule = isolate_rule,
      direction = direction,
      portable = resolved$portable
    ),
    class = "netis_exposure_spec"
  )
}

#' @noRd
.resolve_mechanism <- function(mechanism) {
  if (identical(mechanism, "linear")) {
    return(list(
      label = "linear",
      kernel = .kernel_linear,
      requires = "focal",
      supports_normalize = "row",
      # Package-supplied: fully reconstructible from name + package version alone.
      provenance = list(type = "package", package = "rnetis", name = "linear"),
      portable = TRUE
    ))
  }
  if (is.function(mechanism)) {
    requires <- attr(mechanism, "requires")
    if (is.null(requires)) requires <- "focal"
    .assert_requires(requires)
    supports_normalize <- attr(mechanism, "supports_normalize")
    if (is.null(supports_normalize)) supports_normalize <- character(0)

    # Kernels built by a package constructor (e.g. bi()) carry their own
    # structured provenance -- the constructor name plus parameters fully
    # reconstructs them, unlike an arbitrary closure. Check this before
    # falling back to the name-lookup heuristic below.
    built_in_provenance <- attr(mechanism, "mechanism_provenance")
    if (!is.null(built_in_provenance)) {
      return(list(
        label = built_in_provenance$name,
        kernel = mechanism,
        requires = requires,
        supports_normalize = supports_normalize,
        provenance = built_in_provenance,
        portable = TRUE
      ))
    }

    fn_name <- .function_name_if_any(mechanism)
    provenance <- if (is.null(fn_name)) {
      # An anonymous function cannot be reconstructed from a saved
      # specification alone, so the provenance records only that it was one.
      list(type = "anonymous")
    } else {
      list(type = "named_function", name = fn_name)
    }

    return(list(
      label = if (is.null(fn_name)) "custom" else fn_name,
      kernel = mechanism,
      requires = requires,
      supports_normalize = supports_normalize,
      provenance = provenance,
      portable = !is.null(fn_name)
    ))
  }
  stop(
    "mechanism must be \"linear\" or a kernel function; see Custom kernels in ?compute_exposure.",
    call. = FALSE
  )
}

#' @noRd
.exposure_context_vocabulary <- c("focal", "w_history", "x_history")

#' @noRd
.assert_requires <- function(requires) {
  bad <- setdiff(requires, .exposure_context_vocabulary)
  if (length(bad) > 0L) {
    stop(
      sprintf(
        "Unknown requires component(s): %s. The requires vocabulary is closed: %s.",
        paste(bad, collapse = ", "), paste(.exposure_context_vocabulary, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @noRd
.function_name_if_any <- function(fn) {
  env <- environment(fn)
  if (is.null(env)) return(NULL)
  bound <- Filter(function(nm) identical(get(nm, envir = env), fn), ls(env))
  # Bound to several names, none of them is THE name: the first in sort order
  # would record an alias the user may never have called it by.
  if (length(bound) == 1L) bound else NULL
}
