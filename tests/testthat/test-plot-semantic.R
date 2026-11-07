# The semantic layer: references, missingness states, captions.
#
# These assert on values, not on pixels. Nothing here opens a graphics device
# or constructs a ggplot -- that separation is the reason the invariants are
# testable at all.

test_that("approved reference types construct", {
  r <- .netis_reference("null", 0, "no effect")
  expect_s3_class(r, "netis_reference")
  expect_identical(r$type, "null")
  expect_identical(r$value, 0)

  expect_identical(.netis_reference("chance", 0.5, "chance")$type, "chance")
  expect_identical(
    .netis_reference("baseline", 0.21, "base-rate Brier", source = "observed base rate")$source,
    "observed base rate"
  )
})

test_that("identity is its own type, not a stretched null", {
  # y = x on a calibration plot is a definitional equality, not a statistical
  # null. Typing it `null` would have kept the allowlist at three by making one
  # of the three mean something it does not.
  r <- .netis_reference("identity", 0, "perfect calibration")
  expect_identical(r$type, "identity")
  expect_true("identity" %in% .NETIS_REFERENCE_TYPES)
})

test_that("nominal is its own type, not a stretched null", {
  # A stated interval level (0.95 for a 95% interval) is what the procedure is
  # DEFINED to target -- a property of the procedure's own definition, the same
  # relationship identity has to a calibration display, and not a statistical
  # null (there is no "no effect" reading of a coverage rate) or an
  # acceptability bar a coverage panel must clear.
  r <- .netis_reference("nominal", 0.95, "nominal coverage")
  expect_identical(r$type, "nominal")
  expect_true("nominal" %in% .NETIS_REFERENCE_TYPES)
})

test_that("every approved type names a determined position, never a standard", {
  # The criterion for adding a type, asserted so a future addition has to face
  # it: each admitted type must be derivable without reference to whether the
  # model is any good. None may name acceptability or quality.
  expect_setequal(.NETIS_REFERENCE_TYPES, c("null", "chance", "baseline", "identity", "nominal"))
  for (bad in c("acceptable", "good", "poor", "adequate", "target", "minimum")) {
    expect_false(bad %in% .NETIS_REFERENCE_TYPES)
  }
})

test_that("prohibited reference types are unconstructable", {
  # The whole point of the contract. Each of these is a pass/fail verdict
  # wearing a different word, and the package reports profiles, not verdicts.
  for (bad in c("threshold", "cutoff", "acceptable", "failure", "good", "poor")) {
    expect_error(.netis_reference(bad, 0.1, "x"), "not permitted", fixed = TRUE)
  }
})

test_that("the refusal explains the statistical mistake, not just the valid values", {
  # A caller reaching for "threshold" has not made a typo, so the message says
  # what was wrong and what to do instead.
  err <- tryCatch(.netis_reference("threshold", 0.10, "cutoff"), error = function(e) conditionMessage(e))
  expect_match(err, "null, chance, baseline", fixed = TRUE)
  expect_match(err, "never drawn", fixed = TRUE)
  expect_match(err, "let the reader judge", fixed = TRUE)
})

test_that("the two conventional cutoffs this package must never draw are refused by name", {
  # Named explicitly because both are the kind a well-meaning implementer reaches
  # for: an example cutoff of .10, which is not an authoritative value, and
  # R-hat's 1.01, which is a convention rather than a null.
  expect_error(.netis_reference("threshold", 0.10, "example cutoff"), "not permitted")
  expect_error(.netis_reference("acceptable", 1.01, "R-hat"), "not permitted")
})

test_that("reference values must be a single finite number", {
  # An NA reference draws nothing while reading in the data as though a reference
  # existed -- the returned table would claim a line the plot does not have.
  expect_error(.netis_reference("null", NA_real_, "x"), "single finite number")
  expect_error(.netis_reference("null", Inf, "x"), "single finite number")
  expect_error(.netis_reference("null", c(0, 1), "x"), "single finite number")
})

test_that("a reference set refuses anything not built by the constructor", {
  ok <- .netis_reference("null", 0, "no effect")
  expect_length(.netis_references(ok), 1L)
  expect_length(.netis_references(list(ok, ok)), 2L)
  expect_length(.netis_references(), 0L)

  # A hand-rolled list with the right field names must not pass: that is exactly
  # the bypass the typed contract exists to close.
  smuggled <- list(type = "threshold", value = 0.10, label = "cutoff")
  expect_error(.netis_references(list(ok, smuggled)), "must be built by")
})

test_that("missingness states keep every level whether or not it occurs", {
  s <- .netis_missing_state(3, "observed")
  # A legend that gains and loses entries by dataset makes two plots of the same
  # quantity incomparable, so absent levels are retained deliberately.
  expect_identical(levels(s), .NETIS_MISSING_STATES)
  expect_true(all(s == "observed"))
})

test_that("structural zero and missing are distinct states, not one", {
  # an isolate has zero alters and a defined exposure of zero; a node whose
  # alter went unobserved has no defined exposure. Same printed value in one
  # case, entirely different meaning.
  x <- c(0, 0, NA, 1.5)
  structural <- c(TRUE, FALSE, FALSE, FALSE)
  s <- .netis_classify_missing(x, structural)

  expect_identical(as.character(s), c("structural_zero", "observed", "missing", "observed"))
  # The two zeros must not collapse: element 1 is an isolate, element 2 a measured zero.
  expect_false(identical(as.character(s[1]), as.character(s[2])))
})

test_that("a KNOWN structural state wins over NA", {
  # Precedence, and it is the reverse of this function's first version. Not
  # knowing a value is not the same as not knowing the structure: an isolate's
  # NA under isolate_rule = "na" follows from a declared rule, not from an
  # unobserved measurement, and reporting it as "missing" would send a user to
  # look at the wrong data.
  s <- .netis_classify_missing(c(NA, 0), structural = c(TRUE, TRUE))
  expect_identical(as.character(s), c("structural_zero", "structural_zero"))
})

test_that("unresolved eligibility is undetermined, not missing", {
  # The third arm. A caller that cannot say whether an entry is even eligible
  # passes NA, and gets a state that says so rather than one that blames the
  # data for being absent.
  s <- .netis_classify_missing(c(1, NA, 0), structural = c(NA, NA, FALSE))
  expect_identical(as.character(s), c("undetermined", "undetermined", "observed"))
})

test_that("an eligible entry with no value is missing", {
  s <- .netis_classify_missing(c(NA, 2), structural = FALSE)
  expect_identical(as.character(s), c("missing", "observed"))
})

test_that("unknown missingness states are refused", {
  expect_error(.netis_missing_state(2, "probably_fine"), "Unknown missingness state")
  expect_error(.netis_missing_state(3, c("observed", "missing")), "must be length 1 or 3")
})

test_that("undetermined is a first-class state", {
  # netis_data cannot always say whether a network was observed at a wave. The
  # package does not invent the answer, so "cannot say" needs somewhere to live.
  expect_true("undetermined" %in% .NETIS_MISSING_STATES)
  expect_identical(as.character(.netis_missing_state(1, "undetermined")), "undetermined")
})

test_that("captions accumulate on a table and read back", {
  d <- data.frame(x = 1:2)
  d <- .netis_caption(d, "Diagnostics from a short chain are not representative.")
  d <- .netis_caption(d, "Values are illustrative.")
  expect_length(.netis_caption_of(d), 2L)
  expect_match(.netis_caption_of(d)[1], "short chain")
  expect_identical(.netis_caption_of(data.frame(x = 1)), character(0))
})

test_that("captions refuse non-strings", {
  expect_error(.netis_caption(data.frame(x = 1), NA_character_), "non-NA strings")
})

# ---------------------------------------------------------------------------
# plot_data(): same feature group, kept in one file.
# ---------------------------------------------------------------------------

# The plot_data() generic and the table contract.

test_that("plot_data() refuses an object it has no method for", {
  # No result from a question the package cannot answer.
  expect_error(plot_data(1:10), "no method for an object of class")
  expect_error(plot_data(1:10), "rnetis objects that have a plot")
})

test_that("what resolves to the default when NULL", {
  expect_identical(.netis_what(NULL, c("a", "b"), "an object"), "a")
  expect_identical(.netis_what("b", c("a", "b"), "an object"), "b")
})

test_that("an unrecognized what names the valid set", {
  # DOCUMENTATION.md the data model: never "see below" without a below, and the
  # error is the runtime form of that promise.
  err <- tryCatch(.netis_what("nope", c("coefficients", "effects"), "a netis_influence fit"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "not a panel of a netis_influence fit", fixed = TRUE)
  expect_match(err, "coefficients, effects", fixed = TRUE)

  expect_error(.netis_what(c("a", "b"), c("a"), "x"), "single panel name")
  expect_error(.netis_what(NA_character_, c("a"), "x"), "single panel name")
})

test_that("a plot table must carry its required columns", {
  d <- data.frame(term = "x", value = 1)
  expect_s3_class(.netis_plot_table(d, required = c("term", "value")), "data.frame")
  expect_error(
    .netis_plot_table(d, required = c("term", "value", "lower")),
    "missing required column"
  )
  expect_error(.netis_plot_table(list(a = 1)), "must be a data frame")
})

test_that("missing_state must be the fixed-level factor", {
  # A per-dataset level set makes two plots of the same quantity incomparable,
  # so the contract is enforced at construction rather than hoped for.
  good <- data.frame(value = c(1, NA))
  good$missing_state <- .netis_missing_state(2, c("observed", "missing"))
  expect_s3_class(.netis_plot_table(good), "data.frame")

  bad <- data.frame(value = c(1, NA))
  bad$missing_state <- factor(c("observed", "missing"))   # levels from the data
  expect_error(.netis_plot_table(bad), "fixed-level factor")

  worse <- data.frame(value = 1, missing_state = "observed")  # not a factor
  expect_error(.netis_plot_table(worse), "fixed-level factor")
})

test_that("declared references round-trip on a table", {
  refs <- .netis_references(.netis_reference("null", 0, "no effect"))
  d <- .netis_plot_table(data.frame(value = 1), references = refs)
  expect_length(.netis_references_of(d), 1L)
  # Absent is an empty set, not NULL, so callers need no special case.
  expect_length(.netis_references_of(data.frame(value = 1)), 0L)
})

test_that("a table refuses a reference set not built by the constructor", {
  expect_error(
    .netis_plot_table(data.frame(value = 1), references = list(list(type = "threshold"))),
    "must be built by"
  )
})
