# REPRODUCTION -- Xu's latent-space adjustment, on the s50 data.
#
# Xu, R. (2018) Social Networks 54, 101-117.
# Xu, R. (2021) The R Journal 13(2), 57-69.
#
# Reference numbers come from Frank's knitted "latent space influence"
# tutorial, which runs Xu's analysis end to end and prints every coefficient.
# Its unadjusted five-covariate model is Xu's own published table.
#
# THE FILE SPLITS IN TWO, AND THE SPLIT IS THE WHOLE POINT.
#
#   THE UNADJUSTED MODELS ARE DETERMINISTIC -- a row-normalised exposure and
#   OLS. They are asserted EXACTLY, at double-precision tolerance, including
#   Xu's headline estimate. Any delta is a rnetis defect.
#
#   THE ADJUSTED MODEL IS NOT. Stage one is latentnet::ergmm(), whose internal
#   seeding rnetis does not control, so the positions are a different draw every
#   run -- and the published figure is itself one draw. It is therefore
#   compared against the REFERENCE IMPLEMENTATION RERUN IN THIS SESSION, and
#   asserted only to agree within that reference's own run-to-run spread.

# ---- deterministic ---------------------------------------------------------

test_that("the three unadjusted models reproduce exactly", {
  r <- .s50()
  infl <- .s50_reference_frame(r)
  nd <- .s50_netis_data(r, "pooled")

  .expect_matches_reference(
    stats::lm(alcohol ~ expo, data = infl),
    fit_influence(alcohol ~ exposure(alcohol, via = friend, normalize = "row",
                                     isolate_rule = "zero"), data = nd),
    tolerance = 1e-10)

  .expect_matches_reference(
    stats::lm(alcohol ~ lag_alc + expo, data = infl),
    fit_influence(alcohol ~ ar() + exposure(alcohol, via = friend, normalize = "row",
                                            isolate_rule = "zero"), data = nd),
    tolerance = 1e-10)

  .expect_matches_reference(
    stats::lm(alcohol ~ lag_alc + expo + smoke + sport + drug, data = infl),
    fit_influence(alcohol ~ ar() + exposure(alcohol, via = friend, normalize = "row",
                                            isolate_rule = "zero") +
                    smoke + sport + drug, data = nd),
    tolerance = 1e-10)
})

test_that("Xu's published exposure estimate comes back to his printed precision", {
  # Xu (2021) p.63, unadjusted: 0.15298 (SE 0.07516, p = 0.0446).
  #
  # THIS MODEL HAS NO LATENT POSITIONS IN IT, which is why it can be asserted
  # against the paper at all. The tolerance is his own five-decimal rounding
  # and nothing looser: rnetis returns 0.15297923 (SE 0.07516131).
  r <- .s50()
  fit <- fit_influence(
    alcohol ~ ar() + exposure(alcohol, via = friend, normalize = "row",
                              isolate_rule = "zero") + smoke + sport + drug,
    data = .s50_netis_data(r, "pooled"))

  expect_equal(unname(coef(fit)[["ex_alcohol_friend"]]), 0.15298, tolerance = 5e-5)
  expect_equal(unname(sqrt(diag(vcov(fit)))[["ex_alcohol_friend"]]), 0.07516, tolerance = 5e-5)
})

test_that("rnetis refuses the pooled fit the published analysis performs", {
  # NOT AN INCIDENTAL CHECK. The published outcome model pools two unrelated
  # networks into one intercept, which rnetis declines. That refusal is why the
  # deterministic tests above use the 100-actor single-group encoding, and
  # pinning it here means the encoding choice cannot quietly stop being
  # necessary without this file noticing.
  r <- .s50()
  expect_error(
    fit_influence(alcohol ~ ar() + exposure(alcohol, via = friend, normalize = "row",
                                            isolate_rule = "zero"),
                  data = .s50_netis_data(r, "pergroup")),
    "random-effects term")
})

# ---- stochastic ------------------------------------------------------------

test_that("the latent adjustment agrees with the reference within its own noise", {
  skip_if_not_installed("latentnet")
  skip_if_not_installed("network")
  if (!identical(Sys.getenv("NETIS_MANUAL_MCMC"), "1")) {
    skip("Set NETIS_MANUAL_MCMC=1 to run the MCMC reproduction (several minutes).")
  }

  # THE SAMPLER WARNS, AND SO DOES THE PUBLISHED RUN. latentnet emits
  # "Backing off: too few acceptances" at these control settings, and the
  # knitted reference tutorial prints that same warning above its own results.
  # It is a property of this specification at this chain length, not a rnetis
  # symptom, so it is suppressed HERE, narrowly and by message, rather than
  # left to bury the test output -- and never suppressed in package code.
  quiet_ergmm <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      if (grepl("Backing off", conditionMessage(w))) invokeRestart("muffleWarning")
    })
  }

  r <- .s50()
  infl <- .s50_reference_frame(r)
  nd <- .s50_netis_data(r, "pergroup")
  HOM <- c("alcohol", "smoke", "sport", "drug")
  CTRL <- list(sample.size = 5000, burnin = 20000, interval = 10, Z.delta = 5)
  REPS <- as.integer(Sys.getenv("NETIS_MANUAL_MCMC_REPS", "5"))

  # The reference implementation, run here rather than quoted, so the
  # comparison is against a distribution instead of against one printed draw.
  ref_positions <- function(k, seed) {
    g <- network::network(r$net[[k]], directed = TRUE)
    network::set.vertex.attribute(g, "a", r$a[, k])
    network::set.vertex.attribute(g, "s", r$s[, k])
    network::set.vertex.attribute(g, "sp", r$sp[, k])
    network::set.vertex.attribute(g, "d", r$d[, k])
    set.seed(seed)
    quiet_ergmm(as.vector(latentnet::ergmm(
      g ~ euclidean(d = 1) + absdiff("a") + absdiff("s") + absdiff("sp") + absdiff("d"),
      control = do.call(latentnet::ergmm.control, CTRL), verbose = FALSE)$mkl$Z))
  }

  netis_positions <- function(seed) {
    set.seed(seed)
    fit <- quiet_ergmm(fit_influence(
      alcohol ~ ar() + exposure(alcohol, via = friend, normalize = "row",
                                isolate_rule = "zero") + smoke + sport + drug +
        latent(friend, d = 1, homophily = HOM, control = CTRL) + (1 | group),
      data = nd))
    mf <- fit$model_frame
    cols <- grep("^lat_friend_1", names(mf), value = TRUE)
    expect_length(cols, 2L)
    list(
      Z1 = mf[[grep("__A$", cols, value = TRUE)]][mf$group == "A"][
        order(as.character(mf$node[mf$group == "A"]))],
      Z2 = mf[[grep("__B$", cols, value = TRUE)]][mf$group == "B"][
        order(as.character(mf$node[mf$group == "B"]))])
  }

  # The positions are fed into the PUBLISHED outcome design, not rnetis's. That
  # isolates stage one, which is the part under test: rnetis keeps each
  # network's positions in its own block (for rotation invariance) where the
  # published analysis repeats both networks' positions across all rows, so a
  # direct coefficient comparison would confound two different questions.
  adjusted_expo <- function(Z1, Z2) {
    d <- infl; d$latent_pos1 <- rep(Z1, 2); d$latent_pos2 <- rep(Z2, 2)
    stats::coef(stats::lm(alcohol ~ lag_alc + expo + smoke + sport + drug +
                            latent_pos1 + latent_pos2, data = d))[["expo"]]
  }

  got <- ref <- numeric(REPS)
  for (i in seq_len(REPS)) {
    np <- netis_positions(100 + i)
    got[i] <- adjusted_expo(np$Z1, np$Z2)
    ref[i] <- adjusted_expo(ref_positions(1, 200 + i), ref_positions(2, 300 + i))
  }

  # THE ASSERTION IS AGAINST THE SAMPLER'S OWN SPREAD, not against a fixed
  # number. rnetis's mean must sit within one reference sd of the reference's
  # mean; on a healthy run it lands well inside (observed: a gap of 0.0018
  # against a rnetis sd of 0.0092). A tighter threshold would fail on chain
  # noise alone, and a looser one would pass a genuinely broken stage one.
  expect_lt(abs(mean(got) - mean(ref)), stats::sd(ref))

  # And the published value is inside rnetis's own spread. Expressed as two
  # standard deviations rather than as min/max containment: with a handful of
  # draws an observed range is itself noisy, so a range check would fail
  # sometimes for no reason but the draw count, and a test that fails at random
  # teaches a reader to ignore it.
  #
  # A POINT COMPARISON WOULD BE WRONG HERE WHATEVER THE TOLERANCE, because the
  # published figure is one draw too -- the reference implementation rerun in
  # this test misses its own printed value by about 3%. Asserting rnetis hits it
  # exactly would assert something the method cannot deliver.
  expect_lt(abs(mean(got) - 0.146127), 2 * stats::sd(got))
})
