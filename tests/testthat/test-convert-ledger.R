# The semantic ledger.
#
# Tier 1, feature group `interop`. These are contract tests, not value tests:
# what matters is that provenance is recorded correctly and cannot be
# manufactured, since every refusal in the conversion ledger reads these states.

test_that("an undeclared field is recorded as undeclared, not as its default value", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 1))
  b <- netis_boundary(netis_data(nodes, ties))

  ws <- b$semantics[b$semantics$field == "weight_semantics", ]
  # The stored VALUE is "unitless" and the STATE is undeclared. Conflating the
  # two is the defect the ledger exists to prevent: a consumer reading only the
  # value cannot tell this apart from a user who declared unitless.
  expect_identical(ws$value, "unitless")
  expect_identical(ws$state, "undeclared")

  expect_identical(b$semantics$state[b$semantics$field == "zero_meaning"], "undeclared")
  expect_identical(b$semantics$state[b$semantics$field == "direction_semantics"], "undeclared")
})

test_that("declaring a field moves exactly that field to declared", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 1))
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(weight_semantics = "strength")))
  b <- netis_boundary(nd)

  expect_identical(b$semantics$state[b$semantics$field == "weight_semantics"], "declared")
  # and nothing else moved with it
  expect_identical(b$semantics$state[b$semantics$field == "zero_meaning"], "undeclared")
  expect_identical(b$semantics$state[b$semantics$field == "direction_semantics"], "undeclared")
})

test_that("a structural inference is recorded as inferred and names its rule", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 1))
  b <- netis_boundary(netis_data(nodes, ties))

  wt <- b$semantics[b$semantics$field == "weighted", ]
  expect_identical(wt$state, "inferred")
  expect_identical(wt$rule, "values_in_0_1")
  # The rule identifier is documented, not a private string.
  expect_true(wt$rule %in% netis_boundary_rules()$rule)

  pm <- b$semantics[b$semantics$field == "presence_mode", ]
  expect_identical(pm$state, "inferred")
  expect_identical(pm$rule, "row_completeness_vs_roster")
})

test_that("a declared value is never overwritten by an inference", {
  # `w` takes only 0/1, which .infer_weighted() would call binary. Declaring it
  # weighted must win, and must be recorded as the user's call.
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(1, 0))
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(weighted = TRUE)))

  expect_true(netis_schema(nd)$tie_descriptors$w$weighted)
  b <- netis_boundary(nd)
  expect_identical(b$semantics$state[b$semantics$field == "weighted"], "declared")
  expect_true(is.na(b$semantics$rule[b$semantics$field == "weighted"]))
})

test_that("the legacy *_inferred flags are derived from the ledger, so they cannot drift", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 1))
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(presence_mode = "row_presence")))
  d <- netis_schema(nd)$tie_descriptors$w

  expect_identical(d$weighted_inferred, identical(d$ledger$weighted$state, "inferred"))
  expect_identical(d$presence_mode_inferred, identical(d$ledger$presence_mode$state, "inferred"))
  expect_false(d$presence_mode_inferred)
})

test_that("directedness is declared, never inferred, and records who said so", {
  nodes <- data.frame(group = 1, node = c("a", "b", "c"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "c"), w = c(2, 1))
  b <- netis_boundary(netis_data(nodes, ties))

  expect_identical(b$directedness$state, "declared")
  expect_false(b$directedness$source_encoded)

  # The invariant, asserted structurally rather than by inspection: the conversion ledger
  # forbids deriving directedness from matrix symmetry or edgelist reciprocity,
  # so there must be NO way to produce an "inferred" directedness entry.
  expect_identical(.ledger_directedness(TRUE)$state, "declared")
  expect_identical(.ledger_directedness(FALSE)$state, "declared")
  expect_true(isTRUE(.ledger_directedness(TRUE)$source_encoded))
})

test_that("a symmetric network does not come back undirected", {
  # A symmetric adjacency matrix may perfectly well be a directed network whose
  # ties happen to be reciprocal. The declaration wins.
  m <- matrix(c(0, 1, 1, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  nd <- netis_data_from_adjacency(m, directed = TRUE)
  expect_true(netis_schema(nd)$directed)
  expect_identical(netis_boundary(nd)$directedness$state, "declared")
})

test_that("an inferred ledger entry cannot be built without a known rule", {
  # The audit trail is only worth having if it cannot be half-filled.
  expect_error(.ledger_entry("inferred"), "must name a rule")
  expect_error(.ledger_entry("inferred", "not_a_real_rule"), "must name a rule")
  expect_silent(.ledger_entry("inferred", "values_in_0_1"))
})

test_that("a hand-built object reports no conversion boundary", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  b <- netis_boundary(netis_data(nodes, ties))
  # NULL rather than a placeholder row: this object crossed no boundary, and an
  # absence says so more honestly than "source: none".
  expect_null(b$source)
  expect_output(print(b), "no conversion boundary was crossed")
})

test_that("the boundary seam records the converter and the source class", {
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = "a", target = "b", w = 1)
  nd <- .record_boundary(netis_data(nodes, ties), "some_importer", "someClass",
    source_encoded_directedness = TRUE
  )
  b <- netis_boundary(nd)
  expect_identical(b$source$converter, "some_importer")
  expect_identical(b$source$source_class, "someClass")
  expect_true(b$directedness$source_encoded)
  expect_output(print(b), "imported by some_importer\\(\\) from a someClass object")
})

test_that("presence_mode's ledger value shows whether presence is determined", {
  # "complete_dyadic" alone does not say whether presence can be resolved, and a
  # ledger that reported only the mode would hide the undetermined case that
  # every presence-needing operation refuses on.
  nodes <- data.frame(group = 1, node = c("a", "b"))
  ties <- data.frame(group = 1, source = c("a", "b"), target = c("b", "a"), w = c(1, 0))
  nd <- netis_data(nodes, ties, tie_meta = list(w = list(presence_mode = "complete_dyadic")))
  b <- netis_boundary(nd)
  expect_match(b$semantics$value[b$semantics$field == "presence_mode"], "undetermined")
})

test_that("every registry entry states a resolution", {
  # A refusal without a way forward is a dead end rather than a guardrail.
  l <- netis_losses()
  expect_true(all(nzchar(l$resolution)))
  expect_true(all(nzchar(l$what_is_lost)))
  expect_true(all(nzchar(l$consequence)))
  expect_true(all(l$severity %in% c("semantic", "representational")))
})

test_that("a semantic loss refuses and a representational loss warns", {
  sem <- tryCatch(.loss_raise("isolate_roster"), netis_semantic_loss = function(e) e)
  expect_s3_class(sem, "netis_semantic_loss")
  expect_identical(sem$loss_id, "isolate_roster")
  expect_identical(sem$severity, "semantic")

  rep <- tryCatch(.loss_raise("foreign_attributes"), netis_representational_loss = function(e) e)
  expect_s3_class(rep, "netis_representational_loss")
  expect_identical(rep$severity, "representational")
})

test_that("authorizing one loss does not authorize another", {
  # The whole reason a global flag was rejected: one boolean cannot say which
  # sacrifice the user actually reviewed.
  expect_silent(.loss_raise("foreign_attributes", allow_loss = "foreign_attributes"))
  expect_warning(
    .loss_raise("presence_metadata", allow_loss = "foreign_attributes"),
    class = "netis_representational_loss"
  )
})

test_that("a non-overridable loss refuses the authorization itself", {
  # Not silently ignored: a user who wrote allow_loss = "isolate_roster"
  # believes they permitted something, and letting the call fail elsewhere -- or
  # proceed -- would leave them guessing which.
  e <- tryCatch(
    .loss_raise("isolate_roster", allow_loss = "isolate_roster"),
    netis_semantic_loss = function(e) e
  )
  expect_s3_class(e, "netis_semantic_loss")
  expect_match(conditionMessage(e), "cannot be authorized")
})

test_that("an unknown allow_loss identifier is refused, not ignored", {
  # A misspelled identifier authorizes nothing; discovering that through an
  # unrelated refusal would look like the argument was broken.
  expect_error(.check_allow_loss("isolate_rooster"), "unknown loss identifier")
  expect_silent(.check_allow_loss(c("foreign_attributes", "presence_metadata")))
  expect_error(.check_allow_loss(TRUE), "must be a character vector")
})

test_that("the loss carries its fields on the condition, not only in its text", {
  e <- tryCatch(.loss_raise("isolate_roster"), netis_semantic_loss = function(e) e)
  expect_true(all(c("loss_id", "what_is_lost", "consequence", "resolution") %in% names(e)))
  expect_false(e$overridable)
})

test_that("derived_provenance is representational, not semantic", {
  # Values a derived column carries do not change on export -- only the
  # provenance record marking it derived rather than measured is lost, the
  # same shape of loss presence_metadata already catalogs (resolved values
  # survive; the rule that produced them does not). An earlier draft of
  # classed this semantic by analogy; checked against the two
  # existing semantic entries (both of which DO silently change a computed
  # value if allowed) rather than taken on analogy alone.
  w <- tryCatch(.loss_raise("derived_provenance"), netis_representational_loss = function(e) e)
  expect_s3_class(w, "netis_representational_loss")
  expect_true(w$overridable)
})
