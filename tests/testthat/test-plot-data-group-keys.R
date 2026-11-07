# Data panels and diagnose_data() key nodes by (group, node).

# Two groups that SHARE node labels a, b, c, with different states, so a panel
# keyed on the label alone would show group G1's values for G2:
#   - G2's trait is missing at wave 1 (G1's is observed);
#   - G2/c is off the roster at wave 2 (G1/c is present);
#   - G2 has no ties at all, so every G2 node is an isolate with in-degree 0.
.shared_label_data <- function() {
  nm <- data.frame(
    group = rep(c("G1", "G2"), each = 6),
    node = rep(c("a", "b", "c"), 4),
    wave = rep(rep(1:2, each = 3), 2),
    trait = c(1, 2, 3, 4, 5, 6, NA, NA, NA, 10, 11, 12)
  )
  nm <- nm[!(nm$group == "G2" & nm$node == "c" & nm$wave == 2), ]
  tm <- data.frame(group = "G1", source = c("a", "b", "a", "b"),
                   target = c("b", "c", "b", "c"), wave = c(1, 1, 2, 2), w = 1)
  netis_data(nm, tm, wave_col = "wave")
}

test_that("coverage keeps both groups' units and finds the one absent node-wave", {
  cv <- plot_data(.shared_label_data(), what = "coverage")
  expect_equal(nrow(cv), 12L)                       # 6 units x 2 waves
  row <- cv[cv$group == "G2" & cv$node == "c" & cv$wave == 2, ]
  expect_equal(nrow(row), 1L)
  expect_identical(as.character(row$missing_state), "absent_row")
  same_label_g1 <- cv[cv$group == "G1" & cv$node == "c" & cv$wave == 2, ]
  expect_identical(as.character(same_label_g1$missing_state), "observed")
  expect_identical(as.character(row$unit), "G2: c")
  cap <- attr(cv, "netis_caption")
  expect_false(any(grepl("Every node", cap)))
  expect_true(any(grepl("^1 node-wave", cap)))
})

test_that("missing panel reads each group's own values", {
  ms <- plot_data(.shared_label_data(), what = "missing", node_variable = "trait")
  g2w1 <- ms[ms$group == "G2" & ms$wave == 1, ]
  expect_equal(g2w1$node, c("a", "b", "c"))
  expect_true(all(is.na(g2w1$value)))
  expect_true(all(as.character(g2w1$missing_state) == "missing"))
  expect_equal(ms$value[ms$group == "G2" & ms$node == "b" & ms$wave == 2], 11)
  expect_equal(ms$value[ms$group == "G1" & ms$node == "b" & ms$wave == 1], 2)
})

test_that("exposure in-degree is counted within the node's own group", {
  ex <- plot_data(.shared_label_data(), what = "exposure", content = "trait",
                  variable = "w", wave = 1)
  g2 <- ex[ex$group == "G2", ]
  expect_equal(g2$degree, c(0L, 0L, 0L))
  expect_true(all(as.character(g2$missing_state) == "structural_zero"))
  g1 <- ex[ex$group == "G1", ]
  expect_equal(g1$degree[match(c("a", "b", "c"), g1$node)], c(0L, 1L, 1L))
})

test_that("exposure in-degree counts only present ties of the via network", {
  n4 <- data.frame(group = 1, node = c("a", "b", "c"), trait = c(1, 2, 3))
  # w: only a -> b is a tie (zeros are absences); other: b -> c and a -> c.
  t4 <- data.frame(group = 1, source = c("a", "b", "a"), target = c("b", "c", "c"),
                   w = c(1, 0, 0), other = c(0, 1, 1))
  rule <- list(presence_mode = "value_rule", presence_rule = function(v) v != 0)
  nd4 <- netis_data(n4, t4, tie_meta = list(w = rule, other = rule))
  ex <- plot_data(nd4, what = "exposure", content = "trait", variable = "w")
  expect_equal(ex$degree[match(c("a", "b", "c"), ex$node)], c(0L, 1L, 0L))
  ex2 <- plot_data(nd4, what = "exposure", content = "trait", variable = "other")
  expect_equal(ex2$degree[match(c("a", "b", "c"), ex2$node)], c(0L, 0L, 2L))
})

test_that("centrality marks only isolates as structural zeros", {
  # Path a -> b -> c plus isolate d: betweenness is 0 for a and c (connected)
  # and for d (isolated); only d is a structural zero.
  n <- data.frame(group = 1, node = c("a", "b", "c", "d"), v = 1:4)
  t <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = 1)
  cen <- plot_data(netis_data(n, t), what = "centrality", measure = "betweenness")
  st <- as.character(cen$missing_state)[match(c("a", "b", "c", "d"), cen$node)]
  expect_identical(st, c("observed", "observed", "observed", "structural_zero"))
})

test_that("diagnose_data counts units, not node-wave rows", {
  nodes <- data.frame(group = c("A", "A", "A", "A", "B", "B"),
                      node = c("a", "b", "a", "b", "solo", "solo"),
                      wave = c(1, 1, 2, 2, 1, 2), y = c(1, 2, 3, 4, 5, 6))
  ties <- data.frame(group = "A", source = "a", target = "b", wave = 1, w = 1)
  d <- diagnose_data(netis_data(nodes, ties, wave_col = "wave"))
  codes <- vapply(d, `[[`, "", "code")
  iso <- d[[which(codes == "isolated_nodes")]]
  expect_identical(iso$details$nodes, "solo")
  expect_identical(iso$details$groups, "B")
  # B has one distinct node, observed at two waves: no tie is possible.
  expect_true("group_too_small_for_ties" %in% codes)
  small <- d[[which(codes == "group_too_small_for_ties")]]
  expect_identical(small$details$groups, "B")
})

test_that("diagnose_data names isolates by group when labels repeat", {
  nodes <- data.frame(group = c("A", "A", "B", "B"), node = c("a", "b", "a", "b"))
  ties <- data.frame(group = "A", source = "a", target = "b")
  d <- diagnose_data(netis_data(nodes, ties))
  codes <- vapply(d, `[[`, "", "code")
  iso <- d[[which(codes == "isolated_nodes")]]
  expect_identical(iso$details$groups, c("B", "B"))
  expect_identical(iso$details$nodes, c("a", "b"))
  expect_match(iso$message, "B: a, B: b", fixed = TRUE)
})
