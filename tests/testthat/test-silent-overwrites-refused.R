# Refusals that stand in place of a silent overwrite, the presence declarations a
# refusal asks for, and the rule symmetrize() requires.

# --- igraph list: reserved attribute names ------------------------------------

.gl_graph <- function() {
  igraph::graph_from_data_frame(
    data.frame(from = "a", to = "b", weight = 2), directed = TRUE,
    vertices = data.frame(name = c("a", "b"))
  )
}

test_that("netis_data_from_igraph_list() refuses reserved vertex attributes", {
  skip_if_not_installed("igraph")
  for (attr_name in c("node", "group")) {
    g <- igraph::set_vertex_attr(.gl_graph(), attr_name, value = c("x", "y"))
    expect_error(netis_data_from_igraph_list(list(g), groups = "g1"),
                 sprintf("'%s'", attr_name))
  }
  g <- igraph::set_vertex_attr(.gl_graph(), "wave", value = c(1, 2))
  expect_error(netis_data_from_igraph_list(list(g), groups = "g1", waves = 1), "'wave'")
  # With no wave assigned, a wave attribute is an ordinary node variable.
  back <- netis_data_from_igraph_list(list(g), groups = "g1")
  expect_identical(netis_nodes(back)$wave, c(1, 2))
})

test_that("netis_data_from_igraph_list() refuses reserved edge attributes", {
  skip_if_not_installed("igraph")
  for (attr_name in c("source", "target", "group")) {
    g <- igraph::set_edge_attr(.gl_graph(), attr_name, value = "x")
    expect_error(netis_data_from_igraph_list(list(g), groups = "g1"),
                 sprintf("'%s'", attr_name))
  }
  g <- igraph::set_edge_attr(.gl_graph(), "wave", value = 9)
  expect_error(netis_data_from_igraph_list(list(g), groups = "g1", waves = 1), "'wave'")
})

test_that("as_igraph_list() output is not caught by the reserved names", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2), y = 1:4)
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = c(2, 3))
  nd <- netis_data(nodes, ties, wave_col = "wave")
  back <- netis_data_from_igraph_list(as_igraph_list(nd))
  expect_identical(netis_ties(back)$w, c(2, 3))
})

# --- add_* helpers -------------------------------------------------------------

test_that("add_baseline_provenance() refuses a name the node data already holds", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2),
                      y = c(1, 3, 2, 5), keep = c(9, 8, 7, 6))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(add_baseline_provenance(nd, "y", name = "keep"), "'keep', which already exists")
  # The default name collides on a second call rather than rewriting the first.
  once <- add_baseline_provenance(nd, "y")
  expect_error(add_baseline_provenance(once, "y"), "already exists")
  # On a node table with no wave column as well.
  static <- netis_data(data.frame(group = 1, node = c("a", "b"), y = c(1, 3), keep = 0:1),
                       ties, wave_col = "wave")
  expect_error(add_baseline_provenance(static, "y", name = "keep"), "already exists")
  # The refused call left the values where they were.
  expect_identical(sort(netis_nodes(add_baseline_provenance(nd, "y", name = "new"))$keep), c(6, 7, 8, 9))
})

test_that("add_centering() and add_decomposition() refuse a collision too", {
  nodes <- data.frame(group = 1, node = rep(c("a", "b"), 2), wave = c(1, 1, 2, 2),
                      y = c(1, 3, 2, 5), keep = c(9, 8, 7, 6))
  ties <- data.frame(group = 1, source = "a", target = "b", wave = 1:2, w = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")
  expect_error(add_centering(nd, "y", name = "keep"), "already exists")
  expect_error(add_decomposition(nd, "y", unit = "actor", name_within = "keep"), "already exists")
  expect_error(add_decomposition(nd, "y", unit = "actor", name_between = "keep"), "already exists")
})

# --- presence declarations -----------------------------------------------------

.pd_nodes <- function() data.frame(group = 1, node = c("a", "b", "c"))
.pd_full <- function(values) {
  p <- expand.grid(source = c("a", "b", "c"), target = c("a", "b", "c"), stringsAsFactors = FALSE)
  p <- p[p$source != p$target, ]
  data.frame(group = 1, p, w = values)
}

# The declaration lines of an error, as R values.
.pd_suggested <- function(msg) {
  lines <- trimws(strsplit(msg, "\n", fixed = TRUE)[[1]])
  lines <- lines[startsWith(lines, "tie_meta = list(")]
  lapply(lines, function(l) eval(parse(text = paste0("list(", l, ")")))$tie_meta)
}

test_that("a dense table with nothing ambiguous is accepted, not refused for being dense", {
  # Every dyad present and all 1: row presence and nonzero value agree everywhere.
  ones <- netis_data(.pd_nodes(), .pd_full(1))
  expect_identical(netis_schema(ones)$tie_descriptors$w$presence_mode, "row_presence")
  m <- netis_adjacency(ones, "w", presence = TRUE)
  expect_identical(sum(m), 6)
  expect_identical(unname(diag(m)), c(0, 0, 0))

  # A valued full table whose zeros are declared structural, with none stored.
  valued <- netis_data(.pd_nodes(), .pd_full(c(1, 2, 3, 4, 5, 6)),
                       tie_meta = list(w = list(zero_meaning = "structural_absence")))
  expect_identical(netis_schema(valued)$tie_descriptors$w$presence_mode, "row_presence")
  expect_identical(sum(netis_adjacency(valued, "w", presence = TRUE)), 6)
})

test_that("an ambiguous dense table still needs a declaration", {
  # Valued.
  v <- netis_data(.pd_nodes(), .pd_full(c(1, 0, 3, 0, 5, 6)))
  expect_identical(netis_schema(v)$tie_descriptors$w$presence_mode, "complete_dyadic")
  expect_error(netis_adjacency(v, "w", presence = TRUE), "undetermined")
  # Near-complete and all ones: a missing dyad may be a non-tie left out or an
  # unobserved dyad, so 19 of 20 rows does not settle it.
  ids <- letters[1:5]
  p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
  p <- p[p$source != p$target, ][-1, ]
  near <- netis_data(data.frame(group = 1, node = ids), data.frame(group = 1, p, w = 1))
  expect_identical(netis_schema(near)$tie_descriptors$w$presence_mode, "complete_dyadic")
  expect_error(netis_adjacency(near, "w", presence = TRUE), "undetermined")
})

test_that("the declarations an undetermined error suggests work as written", {
  ties <- .pd_full(c(4, 0, 3, 0, 5, 6))
  ties$flag <- as.numeric(ties$w > 3)
  nd <- netis_data(.pd_nodes(), ties)
  msg <- tryCatch(netis_adjacency(nd, "w", presence = TRUE), error = conditionMessage)
  suggested <- .pd_suggested(msg)
  expect_length(suggested, 3L)

  # Nonzero value means a tie: 4 of the 6 rows, on known cells.
  by_rule <- netis_data(.pd_nodes(), ties, tie_meta = suggested[[1]])
  m <- netis_adjacency(by_rule, "w", presence = TRUE)
  expect_identical(sum(m), 4)
  expect_identical(m[["b", "a"]], 1)
  expect_identical(m[["c", "a"]], 0)

  # A column says so: the placeholder is the one thing to fill in.
  col <- suggested[[2]]
  expect_identical(col$w$presence_col, "<column>")
  col$w$presence_col <- "flag"
  by_col <- netis_data(.pd_nodes(), ties, tie_meta = col)
  expect_identical(sum(netis_adjacency(by_col, "w", presence = TRUE)), 3)

  # Every row is a tie.
  by_row <- netis_data(.pd_nodes(), ties, tie_meta = suggested[[3]])
  expect_identical(sum(netis_adjacency(by_row, "w", presence = TRUE)), 6)
})

test_that("the declarations a stored-zero refusal suggests work as written", {
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(0, 2))
  nd <- netis_data(.pd_nodes(), ties)
  msg <- tryCatch(netis_adjacency(nd, "w", presence = TRUE), error = conditionMessage)
  suggested <- .pd_suggested(msg)
  expect_length(suggested, 4L)
  measured <- netis_data(.pd_nodes(), ties, tie_meta = suggested[[1]])
  expect_identical(sum(netis_adjacency(measured, "w", presence = TRUE)), 2)
  ruled <- netis_data(.pd_nodes(), ties, tie_meta = suggested[[2]])
  expect_identical(sum(netis_adjacency(ruled, "w", presence = TRUE)), 1)
  every <- netis_data(.pd_nodes(), ties, tie_meta = suggested[[4]])
  expect_identical(sum(netis_adjacency(every, "w", presence = TRUE)), 2)
})

test_that("a suggested declaration is valid R for a name that needs quoting", {
  ties <- .pd_full(c(4, 0, 3, 0, 5, 6))
  names(ties)[names(ties) == "w"] <- "my tie"
  nd <- netis_data(.pd_nodes(), ties)
  msg <- tryCatch(netis_adjacency(nd, "my tie", presence = TRUE), error = conditionMessage)
  fixed <- netis_data(.pd_nodes(), ties, tie_meta = .pd_suggested(msg)[[1]])
  expect_identical(sum(netis_adjacency(fixed, "my tie", presence = TRUE)), 4)
})

test_that("the zero_meaning refusal on a Selection outcome suggests working declarations", {
  nd <- netis_data(.pd_nodes(), .pd_full(c(1, 0, 1, 0, 0, 1)))
  msg <- tryCatch(.build_selection_y(nd, "w", "1", NULL), error = conditionMessage)
  suggested <- .pd_suggested(msg)
  expect_length(suggested, 2L)
  y <- .build_selection_y(netis_data(.pd_nodes(), .pd_full(c(1, 0, 1, 0, 0, 1)),
                                     tie_meta = suggested[[1]]), "w", "1", NULL)
  expect_identical(unname(y$counts[["observed"]]), 6L)
  z <- .build_selection_y(netis_data(.pd_nodes(), .pd_full(c(1, 0, 1, 0, 0, 1)),
                                     tie_meta = suggested[[2]]), "w", "1", NULL)
  expect_identical(unname(z$counts[["observed"]]), 3L)
})

# --- symmetrize ------------------------------------------------------------------

test_that("symmetrize() requires a rule and lists the choices", {
  expect_error(symmetrize(w), "requires rule =")
  for (choice in c("max", "min", "sum", "mean", "any", "both")) {
    expect_error(symmetrize(w), sprintf("\"%s\"", choice))
  }
  # No partial matching of a rule, and nothing that is not one of them.
  expect_error(symmetrize(w, rule = "bogus"), "must be one of")
  expect_error(symmetrize(w, rule = "ma"), "must be one of")
  expect_error(symmetrize(w, rule = c("max", "min")), "must be one of")
  expect_s3_class(symmetrize(w, rule = "both"), "netis_symmetrize_spec")
})

test_that("a formula or via without a rule fails with the same message", {
  nodes <- data.frame(group = 1, node = c("a", "b"), y = 1:2)
  nd <- netis_data(nodes, data.frame(group = 1, source = "a", target = "b", w = 1))
  expect_error(compute_exposure(nd, "y", symmetrize(w)), "requires rule =")
})

# --- self-ties ---------------------------------------------------------------------

test_that("the documented self-tie rule is the one exposure applies", {
  expect_error(
    compute_exposure(
      netis_data(data.frame(group = 1, node = c("a", "b"), y = 1:2),
                 data.frame(group = 1, source = "a", target = "a", w = 1)),
      "y", "w"
    ),
    "self_ties = \"prohibited\""
  )
  # Declaring the policy lets the self-tie through as an ordinary tie: a receives
  # its own y (1) at weight 2.
  ex <- compute_exposure(
    netis_data(data.frame(group = 1, node = c("a", "b"), y = c(1, 7)),
               data.frame(group = 1, source = "a", target = "a", w = 2),
               self_ties = "permitted"),
    "y", "w"
  )
  expect_identical(ex$ex_y_w, c(2, 0))
})
