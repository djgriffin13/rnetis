# A dynamic network absent at a wave: exposure there is NA.
#
# The failure these tests guard against is a silent zero: exposure coming back
# as a clean 0 for every node, with no warning, no finding, and the wave still
# listed as valid. A measured zero-weight edge and a structurally absent edge
# are never automatically the same thing, and treating them as one attenuates
# the exposure coefficient toward zero for that transition with nothing printed
# -- the silent-wrong-number case, not a crash.

miss_nodes <- function(n_waves = 3) {
  set.seed(11)
  data.frame(
    group = 1, node = letters[1:8], wave = rep(seq_len(n_waves), each = 8),
    y = rnorm(8 * n_waves), stringsAsFactors = FALSE
  )
}

dyn_ties <- function(waves_present, group = 1) {
  do.call(rbind, lapply(waves_present, function(w) {
    data.frame(
      group = group, wave = w,
      source = c("a", "b", "c", "e", "f", "g"),
      target = c("b", "c", "d", "f", "g", "h"),
      tie = c(1, 2, 3, 1, 2, 3) * w, stringsAsFactors = FALSE
    )
  }))
}

test_that("a dynamic network absent at a wave gives NA, not zero", {
  nd <- netis_data(miss_nodes(), dyn_ties(c(1, 3)), wave_col = "wave")

  ex <- suppressWarnings(compute_exposure(nd, "y", "tie", wave = 2))
  expect_true(all(is.na(ex$ex_y_tie)))
  # The specific regression: it used to be exactly 0 for every node.
  expect_false(any(ex$ex_y_tie == 0, na.rm = TRUE))
})

test_that("the absent wave is named rather than left as a silent NA", {
  nd <- netis_data(miss_nodes(), dyn_ties(c(1, 3)), wave_col = "wave")

  expect_warning(compute_exposure(nd, "y", "tie", wave = 2), "absent at 1 group-wave")
  expect_warning(compute_exposure(nd, "y", "tie", wave = 2), "group 1 wave 2")
  # The message has to say why NA rather than 0, and what to do about it.
  expect_warning(compute_exposure(nd, "y", "tie", wave = 2), "not an empty one")
  expect_warning(compute_exposure(nd, "y", "tie", wave = 2), "observed zeros")
})

test_that("waves whose network is present are unaffected and silent", {
  nd <- netis_data(miss_nodes(), dyn_ties(c(1, 3)), wave_col = "wave")

  expect_silent(ex1 <- compute_exposure(nd, "y", "tie", wave = 1))
  expect_false(any(is.na(ex1$ex_y_tie)))
  expect_silent(ex3 <- compute_exposure(nd, "y", "tie", wave = 3))
  expect_false(any(is.na(ex3$ex_y_tie)))
})

test_that("an isolate in a present wave still gets zero, not NA", {
  # The distinction the fix must preserve: a node with no ties in an OBSERVED
  # network is genuinely unexposed. Only the whole snapshot being missing is
  # undetermined. Collapsing these would trade one wrong answer for another.
  nd <- netis_data(miss_nodes(), dyn_ties(c(1, 3)), wave_col = "wave")

  ex <- compute_exposure(nd, "y", "tie", wave = 1)
  # "a" is a sender only, so under the default direction it receives nothing.
  expect_false(is.na(ex$ex_y_tie[ex$node == "a"]))
})

test_that("a static network is untouched, which is why the defect hid", {
  # A static network has no per-wave existence to be absent, so the same input
  # is correct and unremarkable there. Only the dynamic path was affected.
  static_ties <- data.frame(
    group = 1, source = c("a", "b", "c"), target = c("b", "c", "d"),
    tie = c(1, 2, 3), stringsAsFactors = FALSE
  )
  nd <- netis_data(miss_nodes(), static_ties, wave_col = "wave")

  expect_silent(ex <- compute_exposure(nd, "y", "tie", wave = 2))
  expect_false(any(is.na(ex$ex_y_tie)))
})

test_that("only the absent group is affected when others have that wave", {
  nodes <- rbind(miss_nodes(), transform(miss_nodes(), group = 2))
  ties <- rbind(dyn_ties(c(1, 2, 3), group = 1), dyn_ties(c(1, 3), group = 2))
  nd <- netis_data(nodes, ties, wave_col = "wave")

  ex <- suppressWarnings(compute_exposure(nd, "y", "tie", wave = 2))
  expect_false(any(is.na(ex$ex_y_tie[ex$group == 1])))
  expect_true(all(is.na(ex$ex_y_tie[ex$group == 2])))

  expect_warning(compute_exposure(nd, "y", "tie", wave = 2), "group 2 wave 2")
})

test_that("a group with no ties at any wave is left alone, and says so", {
  # The engine's behavior is deliberate: with no ties anywhere there is no
  # evidence the network exists, so "absent at wave 2" is not established, and
  # inferring it would be guessing rather than computing.
  #
  # The situation is REPORTED, though. Treating every node as an isolate
  # without saying so leaves a user unable to tell "this group genuinely has no
  # ties" from "nobody measured this group's network", which is the
  # zero-versus-missing distinction the package does not collapse.
  nodes <- rbind(miss_nodes(), transform(miss_nodes(), group = 2))
  ties <- dyn_ties(c(1, 2, 3), group = 1)
  nd <- netis_data(nodes, ties, wave_col = "wave")

  ex <- compute_exposure(nd, "y", "tie", wave = 2)
  expect_false(any(is.na(ex$ex_y_tie[ex$group == 2])))

  d <- diagnose_data(nd)
  finding <- Filter(function(f) f$code == "group_without_any_ties", d)
  expect_length(finding, 1L)
  expect_identical(finding[[1]]$level, "NOTE")
  expect_identical(finding[[1]]$details$groups, "2")
  expect_match(finding[[1]]$message, "cannot tell a genuinely tie-less group")
})

test_that("no such note when every group has ties, or when none does", {
  # Two negatives. The first is the ordinary case. The second matters because a
  # dataset with no network at all is a different conversation entirely, not a
  # per-group anomaly to list every group under.
  nodes <- rbind(miss_nodes(), transform(miss_nodes(), group = 2))
  both <- rbind(dyn_ties(1:3, group = 1), dyn_ties(1:3, group = 2))
  nd_both <- netis_data(nodes, both, wave_col = "wave")
  codes <- vapply(diagnose_data(nd_both), function(f) f$code, character(1))
  expect_false("group_without_any_ties" %in% codes)

  nd_none <- netis_data(
    nodes, dyn_ties(1:3, group = 1)[0, , drop = FALSE], wave_col = "wave"
  )
  codes_none <- vapply(diagnose_data(nd_none), function(f) f$code, character(1))
  expect_false("group_without_any_ties" %in% codes_none)
})

test_that("an absent HISTORICAL network is NA too, and the warning names that wave", {
  # The case with the worse consequence. A path-dependent mechanism is immune to an absent FOCAL network -- that is the
  # point of accumulating over history -- but a zero-filled matrix at a
  # historical wave severs every path through it, so the accumulation silently
  # loses all history before that wave rather than attenuating one term.
  nodes <- miss_nodes(n_waves = 3)
  nodes <- add_baseline_provenance(
    netis_data(nodes, dyn_ties(c(1, 3)), wave_col = "wave"), "y", name = "prov"
  )

  expect_warning(
    ex <- compute_exposure(nodes, content = "prov", via = "tie", wave = 3,
                           mechanism = bi(delta = 0.5, alpha = 1, gamma = 0)),
    "history wave 2"
  )
  # Wave 2's network was never recorded, so every path through it is unknown and
  # the accumulated exposure at wave 3 is undefined -- NA, matching the
  # focal-wave decision rather than inventing a second convention.
  expect_true(all(is.na(ex$ex_prov_tie)))
})

test_that("a complete history leaves the path-dependent exposure defined", {
  # The negative half. Without it the test above would pass on a build that
  # NA'd every bi() exposure unconditionally -- which would destroy the
  # mechanism while looking like a careful fix.
  nd <- add_baseline_provenance(
    netis_data(miss_nodes(n_waves = 3), dyn_ties(1:3), wave_col = "wave"),
    "y", name = "prov"
  )
  ex <- compute_exposure(nd, content = "prov", via = "tie", wave = 3,
                         mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))
  expect_false(anyNA(ex$ex_prov_tie))
})
