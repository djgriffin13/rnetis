# rnetis layers, scales, theme, and the reference-integrity enforcement.
#
# These construct ggplot objects but never render them: no device is opened and
# no image is compared. Construction is where the invariants are decided.

fixture <- function() {
  d <- data.frame(term = c("a", "b"), value = c(0.4, -0.2),
                  lower = c(0.1, -0.5), upper = c(0.7, 0.1),
                  stringsAsFactors = FALSE)
  d$missing_state <- .netis_missing_state(2, "observed")
  d
}

test_that("the theme builds and is a ggplot2 theme", {
  expect_s3_class(.netis_theme(), "theme")
})

test_that("the palette is categorical and colourblind-safe", {
  cols <- .netis_palette(4)
  expect_length(cols, 4L)
  expect_true(all(grepl("^#", cols)))
  # Okabe-Ito is a qualitative palette. If this ever becomes a ramp, a discrete
  # scale starts reading as an ordering -- and on ERROR/WARNING/NOTE that reads
  # as severity, which the diagnostics deliberately do not encode.
  expect_false(anyDuplicated(cols) > 0)
})

test_that("missingness scales keep every level regardless of the data", {
  # drop = FALSE is the mechanism; assert it directly, because losing it would
  # silently make two plots of one quantity incomparable.
  expect_false(.netis_scale_missing()$drop)
  expect_false(.netis_scale_missing_shape()$drop)
})

test_that("missingness is distinguishable without colour", {
  # Must survive greyscale printing and a colourblind reader: the
  # isolate-versus-unobserved distinction is the last one that can afford to be
  # lost to a bad projector.
  shp <- .netis_scale_missing_shape()$palette(5)
  expect_length(unique(shp), 5L)
})

test_that("the interval layer builds in both orientations", {
  h <- .netis_interval_layer(horizontal = TRUE)
  v <- .netis_interval_layer(horizontal = FALSE)
  expect_length(h, 2L)   # linerange + point
  expect_length(v, 2L)
  expect_s3_class(h[[1]], "Layer")
})

# ---- the reference invariant ------------------------------------------------

test_that("the reference layer draws approved references", {
  refs <- .netis_references(
    .netis_reference("null", 0, "no effect"),
    .netis_reference("chance", 0.5, "chance")
  )
  layers <- .netis_reference_layer(refs, "v")
  expect_length(layers, 2L)
  expect_s3_class(layers[[1]], "Layer")
})

test_that("the reference layer refuses anything not an approved set", {
  # Belt and braces: even if a hand-built list reached this far, it stops here.
  expect_error(
    .netis_reference_layer(list(list(type = "threshold", value = 0.1)), "h"),
    "only a set built by"
  )
})

test_that("an empty reference set draws nothing", {
  expect_null(.netis_reference_layer(.netis_references(), "h"))
  expect_null(.netis_reference_layer(NULL, "h"))
})

test_that("references are distinguished by linetype, not colour", {
  # A coloured reference invites reading the colour as a judgement. Grey and
  # dashed is the most a reference line should assert.
  refs <- .netis_references(.netis_reference("null", 0, "no effect"))
  lyr <- .netis_reference_layer(refs, "h")[[1]]
  expect_identical(lyr$aes_params$colour, "grey40")
  expect_identical(lyr$aes_params$linetype, "dashed")
})

test_that("drawn references can be read back off a built plot", {
  refs <- .netis_references(.netis_reference("null", 0, "no effect"))
  p <- ggplot2::ggplot(fixture(), ggplot2::aes(x = value, y = term)) +
    ggplot2::geom_point() +
    .netis_reference_layer(refs, "v")
  expect_identical(.netis_drawn_references(p), 0)
})

test_that("THE semantic check: drawn references match declared references", {
  # The load-bearing half of the enforcement. The lexical grep catches
  # the obvious bypass; this catches everything else, by comparing what the built
  # plot contains against what the semantic layer authorized.
  refs <- .netis_references(
    .netis_reference("null", 0, "no effect"),
    .netis_reference("chance", 0.5, "chance")
  )
  p <- ggplot2::ggplot(fixture(), ggplot2::aes(x = value, y = term)) +
    ggplot2::geom_point() +
    .netis_reference_layer(refs, "v")
  p <- .netis_declare_references(p, refs)

  declared <- vapply(attr(p, "netis_references"), function(r) r$value, numeric(1))
  expect_setequal(.netis_drawn_references(p), declared)
})

test_that("the semantic check FAILS on a smuggled reference", {
  # The test that proves the check works. A plot that declares one reference and
  # draws two has had a cutoff added by a route the contract does not cover, and
  # the comparison must notice.
  refs <- .netis_references(.netis_reference("null", 0, "no effect"))
  p <- ggplot2::ggplot(fixture(), ggplot2::aes(x = value, y = term)) +
    ggplot2::geom_point() +
    .netis_reference_layer(refs, "v") +
    ggplot2::geom_vline(xintercept = 0.10)      # the smuggled fit cutoff
  p <- .netis_declare_references(p, refs)

  declared <- vapply(attr(p, "netis_references"), function(r) r$value, numeric(1))
  drawn <- .netis_drawn_references(p)
  expect_length(drawn, 2L)
  expect_false(setequal(drawn, declared))
})

test_that("the reference reader understands every reference geom it permits", {
  # Each geom names its position differently -- hline/vline carry y/xintercept,
  # abline carries `intercept`. A reader that knows only two of the three returns
  # NA for a reference that IS drawn, which reports a bypass where none exists
  # and stays silent about a real one on the geom it cannot read -- e.g. a
  # calibration panel's identity line.
  for (o in c("h", "v", "identity")) {
    refs <- .netis_references(.netis_reference("null", 0.25, "x"))
    p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      .netis_reference_layer(refs, orientation = o)
    expect_equal(.netis_drawn_references(p), 0.25,
                 info = paste("orientation:", o))
  }
})

test_that("general-purpose geoms remain available", {
  # The invariant is "no reference bypasses .netis_reference_layer()", NOT "these
  # geoms are banned". geom_ribbon is how an uncertainty band or a predictive
  # interval gets drawn, and outlawing it would block the package's own section-11
  # displays -- a safeguard that breaks intended plots is a bad safeguard.
  d <- data.frame(x = 1:3, lower = c(0, 1, 2), upper = c(2, 3, 4))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2)
  expect_s3_class(p, "ggplot")
  # And a ribbon is not a reference, so the reference reader must not see it.
  expect_length(.netis_drawn_references(p), 0L)
})

test_that("the lexical check: no reference geom outside the primitive", {
  # Enforcement layer 1, the lexical half. Narrow on purpose: only the three
  # geoms whose sole real use here is a reference line. Reads the function
  # bodies in the namespace, so it holds on an installed package too.
  ns <- asNamespace("rnetis")
  reference_geoms <- c("geom_hline", "geom_vline", "geom_abline")
  fns <- Filter(function(n) is.function(get(n, envir = ns)),
                ls(ns, all.names = TRUE))
  expect_gt(length(fns), 100L)
  uses <- vapply(fns, function(n) {
    any(reference_geoms %in% all.names(body(get(n, envir = ns))))
  }, logical(1))
  expect_identical(names(uses)[uses], ".netis_reference_layer")
})

# ---- captions ---------------------------------------------------------------

test_that("captions render, and reference labels ride along", {
  d <- .netis_caption(fixture(), "Diagnostics from a short chain are not representative.")
  refs <- .netis_references(.netis_reference("null", 0, "no effect"))
  lab <- .netis_caption_lab(d, refs)
  # ggplot2 4.x labels are S7 ("ggplot2::labels"/"gg"/"S7_object"), so assert on
  # the content rather than the class string -- pinning an undocumented class name
  # is exactly the ggplot-internals coupling the plotting contract declines to take on.
  expect_false(is.null(lab))
  txt <- lab$caption
  expect_match(txt, "short chain")
  # An unlabeled line is an assertion the reader has to guess at, and the guess
  # is usually "that must be the cutoff".
  expect_match(txt, "no effect = 0", fixed = TRUE)
})

test_that("a reference source is shown, so preliminary evidence stays labeled", {
  # Preliminary evidence stays labeled wherever it is cited, and a reference
  # line drawn on a plot is a citation.
  refs <- .netis_references(
    .netis_reference("baseline", 0.21, "base-rate Brier", source = "preliminary")
  )
  txt <- .netis_caption_lab(data.frame(x = 1), refs)$caption
  expect_match(txt, "(preliminary)", fixed = TRUE)
})

test_that("no caption and no references produces no caption element", {
  expect_null(.netis_caption_lab(data.frame(x = 1)))
})
