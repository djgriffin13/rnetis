# The round-trip battery (DF.9).
#
# ONE HOSTILE FIXTURE, EVERY SUPPORTED TYPE.
#
# The fixture is built so that each failure mode is individually visible:
#
#   directed + ASYMMETRIC (a->b = 2, b->a = 5)  a transpose changes the answer
#   an ISOLATE (`z`, no ties at all)            silently dropped by any
#                                               endpoint-derived roster
#   an NA tie weight                            distinguishable from an absent
#                                               tie, or it is not
#   a node variable                             carried, or announced as lost
#   two groups                                  a single-group converter must
#                                               say so rather than quietly
#                                               returning one
#
# A convenient fixture cannot tell "handles this" from "was never asked", which
# is why nothing here is convenient.
#
# EACH CONVERTER DECLARES WHAT IT CARRIES. `netis_adjacency()` is a tie-variable
# exporter and never claimed to carry node attributes; calling that a "loss"
# would be inventing a promise it did not make. So the table below records the
# claims per type and the harness holds each type to its own, which is what
# makes "lossless where lossless is possible" a checkable statement rather than
# a slogan.

.rt_fixture <- function() {
  nodes <- data.frame(
    group = c(rep("A", 3), rep("B", 2)),
    node = c("a", "b", "z", "c", "d"),
    trait = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = c("A", "A", "B"),
    source = c("a", "b", "c"),
    target = c("b", "a", "d"),
    w = c(2, 5, NA),
    stringsAsFactors = FALSE
  )
  netis_data(nodes, ties, tie_meta = list(w = list(
    weight_semantics = "strength",
    direction_semantics = "source advises target",
    zero_meaning = "structural_absence"
  )))
}

# Round-trip adapters. Each returns the reconstructed object for ONE group of
# the fixture, so single-group and multi-group converters are comparable.
.rt_adapters <- function() {
  list(
    adjacency = list(
      carries = c("roster", "direction", "values", "directedness"),
      run = function(nd, g) {
        m <- netis_adjacency(nd, "w", drop = FALSE)[[g]]
        netis_data_from_adjacency(m, drop_zero = FALSE, value_name = "w")
      }
    ),
    edgelist = list(
      carries = c("roster", "direction", "values", "directedness", "node_vars"),
      run = function(nd, g) {
        n <- netis_nodes(nd)
        n <- n[n$group == g, setdiff(names(n), "group"), drop = FALSE]
        t <- netis_ties(nd)
        t <- t[t$group == g, , drop = FALSE]
        netis_data_from_edgelist(t[setdiff(names(t), "group")], nodes = n)
      }
    ),
    table_pair = list(
      carries = c("roster", "direction", "values", "directedness", "node_vars", "declarations"),
      run = function(nd, g) {
        n <- netis_nodes(nd)
        t <- netis_ties(nd)
        as_netis_data(netis_tables(n[n$group == g, ], t[t$group == g, ]),
          tie_meta = list(w = list(
            weight_semantics = "strength",
            direction_semantics = "source advises target",
            zero_meaning = "structural_absence"
          ))
        )
      }
    ),
    igraph = list(
      carries = c("roster", "direction", "values", "directedness", "node_vars", "declarations"),
      needs = "igraph",
      run = function(nd, g) netis_data_from_igraph(as_igraph(nd, network = "w", group = g))
    ),
    igraph_list = list(
      carries = c("roster", "direction", "values", "directedness", "node_vars", "declarations"),
      needs = "igraph",
      run = function(nd, g) {
        back <- netis_data_from_igraph_list(as_igraph_list(nd, network = "w"))
        .rt_subset(back, g)
      }
    ),
    network = list(
      carries = c("roster", "direction", "values", "directedness", "node_vars", "declarations"),
      needs = "network",
      run = function(nd, g) netis_data_from_network(as_network(nd, network = "w", group = g))
    )
  )
}

#' Keep one group of a reconstructed object, so multi-group converters compare
#' against the same expectations as single-group ones.
#' @noRd
.rt_subset <- function(x, g) {
  n <- netis_nodes(x)
  t <- netis_ties(x)
  s <- netis_schema(x)
  netis_data(
    n[as.character(n$group) == g, , drop = FALSE],
    t[as.character(t$group) == g, , drop = FALSE],
    directed = s$directed, self_ties = s$self_ties,
    tie_meta = .tie_meta_from_descriptors(s)
  )
}

test_that("every converter round-trips the properties it claims to carry", {
  nd <- .rt_fixture()
  before_nodes <- netis_nodes(nd)
  before_ties <- netis_ties(nd)

  for (name in names(.rt_adapters())) {
    spec <- .rt_adapters()[[name]]
    if (!is.null(spec$needs)) skip_if_not_installed(spec$needs)

    back <- spec$run(nd, "A")
    label <- sprintf("[%s]", name)

    if ("roster" %in% spec$carries) {
      # The isolate is the point. An endpoint-derived roster loses `z` and
      # every density and centrality moves with it.
      expect_setequal(sort(netis_nodes(back)$node), c("a", "b", "z"))
      expect_identical(nrow(netis_nodes(back)), 3L, info = label)
    }
    if ("direction" %in% spec$carries) {
      # Asymmetric by construction: a transpose swaps 2 and 5.
      m <- netis_adjacency(back, "w", drop = TRUE, fill = NA)
      expect_equal(m["a", "b"], 2, info = label)
      expect_equal(m["b", "a"], 5, info = label)
    }
    if ("directedness" %in% spec$carries) {
      expect_true(netis_schema(back)$directed, info = label)
      expect_identical(netis_schema(back)$self_ties, "prohibited", info = label)
    }
    if ("node_vars" %in% spec$carries) {
      n <- netis_nodes(back)
      expect_true("trait" %in% names(n), info = label)
      expect_equal(n$trait[match(c("a", "b", "z"), n$node)], c(1, 2, 3), info = label)
    }
    if ("declarations" %in% spec$carries) {
      d <- netis_schema(back)$tie_descriptors$w
      expect_identical(d$weight_semantics, "strength", info = label)
      expect_identical(d$direction_semantics, "source advises target", info = label)
      expect_identical(d$zero_meaning, "structural_absence", info = label)
      b <- netis_boundary(back)
      expect_identical(
        b$semantics$state[b$semantics$field == "weight_semantics"], "declared",
        info = label
      )
    }
  }
  expect_identical(nrow(before_nodes), 5L) # fixture unchanged by the battery
  expect_identical(nrow(before_ties), 3L)
})

test_that("an NA tie weight stays distinguishable from an absent tie", {
  # rnetis's hardest pair at a boundary. Group B's only tie has an unknown
  # weight; it must come back as a tie whose weight is unknown, not as no tie
  # and not as a zero.
  nd <- .rt_fixture()
  for (name in c("table_pair", "igraph", "network")) {
    spec <- .rt_adapters()[[name]]
    if (!is.null(spec$needs)) skip_if_not_installed(spec$needs)
    back <- spec$run(nd, "B")
    t <- netis_ties(back)
    expect_identical(nrow(t), 1L, info = name)
    expect_true(is.na(t$w), info = name)
  }
})

test_that("a group with no ties still appears, rather than vanishing", {
  nodes <- data.frame(
    group = c("A", "A", "B"), node = c("a", "b", "solo"),
    trait = c(1, 2, 3), stringsAsFactors = FALSE
  )
  ties <- data.frame(group = "A", source = "a", target = "b", w = 1, stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties)
  # Group B has a node and no ties at all -- legal, and a converter that built
  # groups from tie data would drop it.
  expect_identical(nrow(netis_adjacency(nd, "w", drop = FALSE)$B), 1L)

  skip_if_not_installed("igraph")
  gl <- as_igraph_list(nd, network = "w")
  expect_true("B" %in% names(gl))
  expect_equal(igraph::vcount(gl$B), 1)
})

test_that("a wave series round-trips its wave structure", {
  nodes <- data.frame(
    group = "A", node = rep(c("a", "b", "z"), each = 3),
    wave = rep(1:3, times = 3), y = seq_len(9), stringsAsFactors = FALSE
  )
  ties <- data.frame(
    group = "A", source = c("a", "b", "a"), target = c("b", "a", "b"),
    wave = c(1, 1, 3), w = c(2, 5, 9), stringsAsFactors = FALSE
  )
  nd <- netis_data(nodes, ties, wave_col = "wave")

  skip_if_not_installed("igraph")
  expect_warning(graphs <- as_igraph_list(nd, network = "w"), "group A wave 2")
  back <- netis_data_from_igraph_list(graphs)
  expect_identical(as.character(netis_schema(back)$waves), c("1", "2", "3"))
  expect_identical(nrow(netis_ties(back)), 3L)
  # Wave 2 was unobserved going out, and is unobserved coming back.
  expect_identical(netis_schema(back)$observed_tie_waves$wave, c("1", "3"))
})


# ---- loss is announced, and the announcement is the contract ----------------

test_that("every registry entry offers a way forward", {
  # A refusal without a resolution is a dead end rather than a guardrail.
  l <- netis_losses()
  expect_true(all(nzchar(l$resolution)))
  expect_true(nrow(l) > 0L)
})

test_that("each known-lossy path announces with its own loss_id", {
  cases <- list(
    list(
      id = "isolate_roster",
      run = function() netis_data_from_edgelist(data.frame(source = "a", target = "b"))
    ),
    list(
      id = "tie_value_collapse",
      needs = "network",
      run = function() {
        mp <- network::network.initialize(2, directed = TRUE, multiple = TRUE)
        mp <- network::add.edges(mp, tail = c(1, 1), head = c(2, 2))
        netis_data_from_network(mp)
      }
    )
  )
  for (case in cases) {
    if (!is.null(case$needs)) skip_if_not_installed(case$needs)
    e <- tryCatch(case$run(), netis_semantic_loss = function(e) e)
    expect_s3_class(e, "netis_semantic_loss")
    expect_identical(e$loss_id, case$id)
    expect_true(nzchar(e$resolution))
  }
})

test_that("a representational loss warns and can be authorized by name only", {
  skip_if_not_installed("igraph")
  nodes <- data.frame(group = 1, node = c("a", "b"), stringsAsFactors = FALSE)
  ties <- data.frame(group = 1, source = "a", target = "b", w = 2, stringsAsFactors = FALSE)
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(
    presence_mode = "value_rule", presence_rule = function(v) v > 1
  )))

  w <- tryCatch(as_igraph(nd, network = "w"), netis_representational_loss = function(e) e)
  expect_identical(w$loss_id, "presence_metadata")
  expect_silent(as_igraph(nd, network = "w", allow_loss = "presence_metadata"))
  # Authorizing a DIFFERENT loss does not silence this one.
  expect_warning(
    as_igraph(nd, network = "w", allow_loss = "foreign_attributes"),
    class = "netis_representational_loss"
  )
})

# ---- Suggests guards ---------------------------------------------------------

test_that("each optional-package guard names the package and the operation", {
  # The suite must behave when igraph/network/networkDynamic are absent. The
  # guards are the single place that decides what "absent" looks like, so they
  # are tested directly rather than by uninstalling anything: every entry point
  # routes through one of these three.
  expect_error(.require_igraph("as_igraph()"), NA)
  expect_error(.require_network("as_network()"), NA)

  # With the package unavailable, the message must name both the package and
  # what the caller was trying to do.
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(.require_igraph("as_igraph()"), "'igraph' package")
  expect_error(.require_igraph("as_igraph()"), "as_igraph\\(\\)")
  expect_error(.require_network("as_network()"), "'network' package")
  # No networkDynamic guard: this package has no networkDynamic converter, so
  # there is nothing for a guard to protect.
})
