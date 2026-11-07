# Node-coordinate resolution.
#
# The load-bearing property is installation-invariance: a default plot must not
# change because an optional package is or is not present.

edges_connected <- data.frame(
  from = c("a", "b", "c"),
  to   = c("b", "c", "d"),
  stringsAsFactors = FALSE
)
ids4 <- c("a", "b", "c", "d")

test_that("circle and grid are defined for any node set", {
  for (m in c("circle", "grid")) {
    out <- .netis_layout(ids4, edges_connected, method = m)
    expect_identical(out$node, ids4)
    expect_true(all(is.finite(out$x)) && all(is.finite(out$y)))
  }
  # Including the degenerate sizes that special-case in most implementations.
  expect_equal(nrow(.netis_layout("a", NULL, "circle")), 1L)
  expect_equal(nrow(.netis_layout(character(0), NULL, "circle")), 0L)
})

test_that("circle and grid need no edges at all", {
  # An all-isolate network is a legitimate thing to draw and must not error.
  out <- .netis_layout(ids4, NULL, method = "circle")
  expect_equal(nrow(out), 4L)
  expect_true(all(is.finite(out$x)))
})

test_that("native layouts are deterministic", {
  # Same input, same coordinates -- otherwise the "same network looks the same
  # everywhere" guarantee is empty.
  expect_identical(
    .netis_layout(ids4, edges_connected, "circle"),
    .netis_layout(ids4, edges_connected, "circle")
  )
  expect_identical(
    .netis_layout(ids4, edges_connected, "mds"),
    .netis_layout(ids4, edges_connected, "mds")
  )
})

test_that("the resolved layout method is recorded", {
  # Makes a drawing auditable after the fact.
  expect_identical(attr(.netis_layout(ids4, edges_connected, "grid"), "layout_method"), "grid")
})

test_that("mds records that its distances are unweighted", {
  # Provenance for a choice a reader cannot see: on a WEIGHTED network the
  # positions ignore the weights, and nothing about the layout name says so.
  out <- .netis_layout(ids4, edges_connected, "mds")
  expect_identical(attr(out, "layout_distance"), "unweighted_geodesic")
})

test_that("layouts that use no distance matrix record none", {
  # NA rather than a fabricated label: circle and grid do not compute distances
  # at all, so claiming a basis would be inventing provenance.
  for (m in c("circle", "grid")) {
    expect_true(is.na(attr(.netis_layout(ids4, edges_connected, m), "layout_distance")))
  }
})

test_that("layout does not reorder nodes", {
  # A layout that quietly re-sorts breaks the caller's row alignment.
  odd <- c("10", "2", "1")
  expect_identical(.netis_layout(odd, NULL, "circle")$node, odd)
})

# ---- the finiteness rule ----------------------------------------------------

test_that("mds works on a connected network", {
  out <- .netis_layout(ids4, edges_connected, method = "mds")
  expect_true(all(is.finite(out$x)) && all(is.finite(out$y)))
})

test_that("mds refuses on a network with an isolate", {
  # THE case this rule exists for. The layouts preserve isolates, and
  # an isolate is a disconnected component, so infinite geodesics are ordinary.
  ids5 <- c(ids4, "e")   # e has no ties
  expect_error(
    .netis_layout(ids5, edges_connected, method = "mds"),
    "disconnected"
  )
})

test_that("mds refuses on two disconnected components with no isolates", {
  ids <- c("a", "b", "c", "d")
  e <- data.frame(from = c("a", "c"), to = c("b", "d"), stringsAsFactors = FALSE)
  expect_error(.netis_layout(ids, e, method = "mds"), "2 components")
})

test_that("the mds refusal names the alternatives and the reason", {
  # say what was wrong, what was expected, what to do.
  err <- tryCatch(
    .netis_layout(c(ids4, "e"), edges_connected, method = "mds"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "isolate", fixed = TRUE)
  expect_match(err, "circle", fixed = TRUE)
  expect_match(err, "grid", fixed = TRUE)
  # And states the substitution it is refusing to make, so nobody "helpfully"
  # adds it back later.
  expect_match(err, "visual claim", fixed = TRUE)
})

test_that("mds never silently fills an infinite distance", {
  # The negative form of the rule: no filler, ever. If this test starts passing
  # by returning coordinates, someone has added a 2*max substitution.
  expect_error(.netis_layout(c(ids4, "e"), edges_connected, "mds"))
})

# ---- geodesics --------------------------------------------------------------

test_that("geodesic distances are hop counts, with Inf across components", {
  d <- .netis_geodesic(c(ids4, "e"), edges_connected)
  expect_identical(d["a", "a"], 0)
  expect_identical(d["a", "b"], 1)
  expect_identical(d["a", "c"], 2)   # a-b-c
  expect_identical(d["a", "d"], 3)   # a-b-c-d
  expect_true(is.infinite(d["a", "e"]))
})

test_that("geodesics ignore edge direction", {
  # A layout places nodes in a plane and a plane has no direction. Arrowheads come
  # from the semantic layer's resolved endpoints, never from this symmetrization.
  fwd <- .netis_geodesic(c("a", "b"), data.frame(from = "a", to = "b"))
  rev <- .netis_geodesic(c("a", "b"), data.frame(from = "b", to = "a"))
  expect_identical(fwd, rev)
})

test_that("component detection counts isolates", {
  comp <- .netis_components(c(ids4, "e", "f"), edges_connected)
  expect_equal(length(unique(comp)), 3L)   # {a,b,c,d}, {e}, {f}
})

# ---- igraph is explicit, never a silent default -----------------------------

test_that("an unknown layout name errors rather than falling back", {
  expect_error(.netis_layout(ids4, edges_connected, "definitely_not_a_layout"), "Unknown layout")
})

test_that("an igraph layout produces coordinates when igraph is available", {
  skip_if_not_installed("igraph")
  out <- .netis_layout(ids4, edges_connected, method = "fr")
  expect_equal(nrow(out), 4L)
  expect_true(all(is.finite(out$x)))
  expect_identical(attr(out, "layout_method"), "fr")
})

test_that("defaults do not consult igraph at all", {
  # Installation-invariance, tested directly: the native paths must reach no
  # igraph code, so their output cannot depend on whether it is installed.
  # .netis_layout_circle/grid take only n, which is the structural guarantee.
  expect_identical(names(formals(.netis_layout_circle)), "n")
  expect_identical(names(formals(.netis_layout_grid)), "n")
  expect_identical(formals(.netis_layout)$method, "circle")
})

test_that("the igraph-absent path is exercised, not merely argued", {
  # The structural test above says the native layouts CANNOT consult igraph
  # because they take only `n`. That is an argument, not a run, and the criterion
  # asks for the guarded path tested both ways -- so this makes igraph
  # unavailable and checks what actually happens.
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "igraph")) FALSE else base::requireNamespace(package, ...)
    },
    .package = "base"
  )

  # Native layouts are unaffected: identical coordinates to the real run.
  expect_identical(
    .netis_layout(ids4, edges_connected, "circle")$x,
    .netis_layout_circle(4)$x
  )
  expect_s3_class(.netis_layout(ids4, edges_connected, "grid"), "data.frame")
  expect_s3_class(.netis_layout(ids4, edges_connected, "mds"), "data.frame")

  # And the explicit igraph request refuses ACTIONABLY rather than degrading --
  # the whole point of installation-invariance is that rnetis does not quietly
  # substitute a different algorithm for the one that was asked for.
  err <- tryCatch(.netis_layout(ids4, edges_connected, "fr"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "igraph package is not installed", fixed = TRUE)
  expect_match(err, "circle, grid, mds", fixed = TRUE)
  expect_match(err, "does not silently substitute", fixed = TRUE)
})

# ---- seeded igraph layouts ---------------------------------------------------

.ring8 <- function() {
  netis_data(data.frame(group = 1, node = letters[1:8], v = 1:8),
             data.frame(group = 1, source = letters[1:8],
                        target = letters[c(2:8, 1)], w = 1))
}

test_that("an igraph layout is reproducible and leaves the caller's stream alone", {
  skip_if_not_installed("igraph")
  ring <- .ring8()
  set.seed(1)
  s0 <- .Random.seed
  a <- plot_data(ring, layout = "fr")
  expect_identical(.Random.seed, s0)
  b <- plot_data(ring, layout = "fr")
  expect_identical(a$nodes$x, b$nodes$x)
  expect_identical(a$nodes$y, b$nodes$y)
  expect_identical(attr(a, "layout_seed"), 1L)
  # plot() goes through the same path.
  invisible(plot(ring, layout = "fr"))
  expect_identical(.Random.seed, s0)
})

test_that("seed = NULL draws from the session stream and advances it", {
  skip_if_not_installed("igraph")
  ring <- .ring8()
  set.seed(1)
  s0 <- .Random.seed
  a <- plot_data(ring, layout = "fr", seed = NULL)
  expect_false(identical(.Random.seed, s0))
  set.seed(1)
  b <- plot_data(ring, layout = "fr", seed = NULL)
  expect_identical(a$nodes$x, b$nodes$x)
  expect_null(attr(a, "layout_seed"))
})

test_that("a different seed gives the layout that seed produces", {
  skip_if_not_installed("igraph")
  ring <- .ring8()
  a <- plot_data(ring, layout = "fr", seed = 2)
  b <- plot_data(ring, layout = "fr", seed = 2)
  c1 <- plot_data(ring, layout = "fr", seed = 1)
  expect_identical(a$nodes$x, b$nodes$x)
  expect_false(identical(a$nodes$x, c1$nodes$x))
  expect_error(plot_data(ring, layout = "fr", seed = "x"), "whole number")
})

# ---- the layouts through plot() and plot_data() ------------------------------

test_that("plot() refuses mds on a disconnected network and offers what works", {
  nd <- netis_data(data.frame(group = 1, node = letters[1:5], v = 1:5),
                   data.frame(group = 1, source = c("a", "b", "c"), target = c("b", "c", "d"), w = 1))
  # e is an isolate, kept by the drawing, so the network is disconnected.
  err <- tryCatch(plot(nd, layout = "mds"), error = function(e) conditionMessage(e))
  expect_match(err, "2 components including 1 isolate(s)", fixed = TRUE)
  expect_match(err, "will not substitute a finite value", fixed = TRUE)
  expect_match(err, "layout = \"circle\"", fixed = TRUE)
  expect_error(plot_data(nd, layout = "mds"), "disconnected")
  expect_s3_class(plot(nd, layout = "circle"), "ggplot")
  expect_s3_class(plot(nd, layout = "grid"), "ggplot")

  # Connected: mds runs, records itself, and reproduces the geodesics of a path.
  path <- netis_data(data.frame(group = 1, node = letters[1:4], v = 1:4),
                     data.frame(group = 1, source = c("a", "b", "c"), target = c("b", "c", "d"), w = 1))
  d <- plot_data(path, layout = "mds")
  expect_identical(attr(d, "layout_method"), "mds")
  expect_identical(attr(d, "layout_distance"), "unweighted_geodesic")
  drawn <- as.matrix(stats::dist(d$nodes[match(letters[1:4], d$nodes$node), c("x", "y")]))
  expect_lt(abs(drawn[1, 4] - 3), 1e-8)
  expect_lt(abs(drawn[1, 2] - 1), 1e-8)
})

test_that("an unknown layout is refused by name, not replaced", {
  skip_if_not_installed("igraph")
  nd <- netis_data(data.frame(group = 1, node = letters[1:4], v = 1:4),
                   data.frame(group = 1, source = c("a", "b", "c"), target = c("b", "c", "d"), w = 1))
  expect_error(plot(nd, layout = "no_such_layout"), "Unknown layout \"no_such_layout\"", fixed = TRUE)
  # A second igraph layout, seeded the same way.
  a <- plot_data(nd, layout = "kk", seed = 3)
  b <- plot_data(nd, layout = "kk", seed = 3)
  expect_identical(a$nodes$x, b$nodes$x)
  expect_identical(attr(a, "layout_method"), "kk")
  expect_identical(attr(a, "layout_seed"), 3)
})
