# Xu's two-stage latent-space adjustment.
#
# Xu, R. (2018) Social Networks 54, 101-117.
# Xu, R. (2021) The R Journal 13(2), 57-69.
#
# THE HIGHEST-VALUE TEST HERE IS THE REPRODUCTION, and it comes in two parts
# because only one of them can be exact offline:
#
#   1. THE PROCEDURE reproduces here. rnetis's latent() must produce the same
#      positions as calling latentnet::ergmm() directly with Xu's
#      specification and taking $mkl$Z. NOT to machine precision: rnetis does
#      not control latentnet's internal seeding, so the two are different
#      draws. The invariant checked is the GEOMETRY -- pairwise distances,
#      which the latent space is identified up to -- and the tolerance below
#      is what that supports, not what would look tighter.
#
#   2. THE PUBLISHED TABLE IS REPRODUCED, BUT NOT FROM IN HERE. Xu's example
#      uses five covariates -- alcohol, smoking, drug use, sport, and
#      friendship -- from the Teenage Friends and Lifestyle Study. RSiena
#      ships the networks (s501, s502, s503), alcohol (s50a) and smoking
#      (s50s). It does NOT ship drug use or sport; those come from
#      s50-drugs.dat and s50-sport.dat, which are downloaded, and this suite
#      may not reach the network (and rnetis redistributes no one's data).
#
#      So the reproduction of the published table, which needs the two
#      missing covariates, is outside the package test suite. That model is
#      deterministic -- row-normalised exposure and OLS -- so exactness to
#      Xu's reported rounding is the right expectation there.
#
#      What is checked IN HERE is that the adjustment is wired correctly on
#      the covariates that ARE available. Xu's qualitative finding -- that the
#      adjustment attenuates the influence estimate -- is deliberately NOT
#      asserted: on this reduced covariate set the adjustment moves the
#      estimate only slightly and not consistently in either direction, and
#      asserting the direction anyway would be asserting what the method is
#      supposed to do rather than what this fixture shows.

.xu_s50 <- function() {
  skip_if_not_installed("RSiena")
  ids <- sprintf("p%02d", seq_len(50))
  edge <- function(m, wave) {
    ij <- which(m == 1, arr.ind = TRUE)
    data.frame(group = 1, source = ids[ij[, 1]], target = ids[ij[, 2]],
               wave = wave, friend = 1, stringsAsFactors = FALSE)
  }
  nodes <- data.frame(
    group = 1, node = rep(ids, 3), wave = rep(1:3, each = 50),
    alcohol = as.vector(RSiena::s50a), smoke = as.vector(RSiena::s50s),
    stringsAsFactors = FALSE
  )
  ties <- rbind(edge(RSiena::s501, 1), edge(RSiena::s502, 2),
                edge(RSiena::s503, 3))
  list(
    data = netis_data(nodes, ties, wave_col = "wave",
                      tie_meta = list(friend = list(zero_meaning = "measured_zero"))),
    ids = ids
  )
}

# Short chains. These check that rnetis calls the same estimator with the same
# specification and reads the same field -- not that the chain converged.
.xu_control <- list(sample.size = 400, burnin = 800, interval = 5, Z.delta = 5)

# ---- the specification the term implies -------------------------------------

test_that("the ergmm formula matches the shape Xu writes", {
  # Xu (2021) p. 62:
  #   g1 ~ euclidean(d = 1) + absdiff("a") + absdiff("s") + absdiff("sp") + absdiff("d")
  term <- latent(friend, d = 1, homophily = c("a", "s", "sp", "d"))
  expect_identical(
    .latent_ergmm_rhs(term),
    'euclidean(d = 1) + absdiff("a") + absdiff("s") + absdiff("sp") + absdiff("d")'
  )
})

test_that("extra ergmm terms pass through verbatim", {
  # Xu's first note: the selection model should control for what else drives
  # tie formation -- transitivity, ego and alter effects -- and `homophily`
  # cannot express those.
  term <- latent(friend, d = 2, homophily = "alcohol", terms = c("gwesp", "nodecov('x')"))
  rhs <- .latent_ergmm_rhs(term)
  expect_match(rhs, "euclidean(d = 2)", fixed = TRUE)
  expect_match(rhs, "gwesp", fixed = TRUE)
  expect_match(rhs, "nodecov('x')", fixed = TRUE)
})

test_that("neither homophily nor terms has a default", {
  # What belongs in a selection model is a question about the design. A
  # default would put rnetis's choice inside the user's adjustment.
  term <- latent(friend)
  expect_length(term$homophily, 0L)
  expect_length(term$terms, 0L)
  expect_identical(.latent_ergmm_rhs(term), "euclidean(d = 1)")
})

test_that("column names carry the dimension only when there is more than one", {
  expect_identical(.latent_colnames(latent(friend, d = 1)), "lat_friend_1")
  expect_identical(.latent_colnames(latent(friend, d = 2)),
                   c("lat_friend_1_d1", "lat_friend_1_d2"))

  # With several groups, one set of columns PER GROUP -- each group's latent
  # space has its own arbitrary orientation, so each needs its own nuisance
  # coefficients. One group leaves the names alone.
  expect_identical(.latent_colnames(latent(friend, d = 1), groups = "g1"),
                   "lat_friend_1")
  expect_identical(
    .latent_colnames(latent(friend, d = 1), groups = c("g1", "g2")),
    c("lat_friend_1__g1", "lat_friend_1__g2")
  )
  expect_identical(
    .latent_colnames(latent(friend, d = 2), groups = c("g1", "g2")),
    c("lat_friend_1_d1__g1", "lat_friend_1_d1__g2",
      "lat_friend_1_d2__g1", "lat_friend_1_d2__g2")
  )
})

# ---- reproduction, part 1: the procedure ------------------------------------

test_that("rnetis's positions are latentnet's own MKL positions", {
  skip_if_not_installed("latentnet")
  skip_if_not_installed("RSiena")

  d <- .xu_s50()
  ids <- d$ids

  # rnetis's route. With latent(seed = 11) the stage-one sampler runs from the
  # same seed as the direct call below, so the two are the same draw.
  fit <- fit_influence(
    alcohol ~ ar() + exposure(alcohol, via = friend, normalize = "row") +
      latent(friend, d = 1, homophily = c("alcohol", "smoke"),
             control = .xu_control, seed = 11),
    data = d$data, waves = c(2, 3)
  )
  got <- fit$model_frame$lat_friend_1
  names(got) <- as.character(fit$model_frame$node)

  # latentnet's route, called directly with the same specification on the same
  # network -- the wave the term's lag = 1 resolves to, which is wave 2.
  adj <- matrix(0, 50, 50, dimnames = list(ids, ids))
  ij <- which(RSiena::s502 == 1, arr.ind = TRUE)
  adj[cbind(ids[ij[, 1]], ids[ij[, 2]])] <- 1
  net <- network::network(adj, directed = TRUE)
  network::set.vertex.attribute(net, "alcohol", RSiena::s50a[, 2])
  network::set.vertex.attribute(net, "smoke", RSiena::s50s[, 2])
  set.seed(11)
  direct <- latentnet::ergmm(
    net ~ euclidean(d = 1) + absdiff("alcohol") + absdiff("smoke"),
    control = do.call(latentnet::ergmm.control, .xu_control),
    verbose = FALSE
  )
  want <- as.vector(direct$mkl$Z)
  names(want) <- ids

  # COMPARED AS A GEOMETRY, NOT COORDINATE BY COORDINATE. A latent space is
  # identified only up to translation, reflection, ROTATION and -- with d > 1
  # -- axis permutation, so `Z` and `Z R` for an orthogonal `R` are the same
  # answer. Coordinate-wise correlation would fail on two identical geometries
  # whenever the sampler landed on a different orientation; at d = 1 it would
  # happen to work only because the rotation group is then just {+1, -1}, and
  # relying on that would make the test wrong the moment anyone used d > 1.
  #
  # PAIRWISE DISTANCES ARE THE INVARIANT. They are what the model is a model
  # of -- the log-odds fall with |c_i - c_j| -- and they are unchanged by
  # every transformation the space is unidentified up to.
  common <- intersect(names(got), names(want))
  expect_gt(length(common), 40L)
  d_got <- as.vector(stats::dist(matrix(got[common], ncol = 1L)))
  d_want <- as.vector(stats::dist(matrix(want[common], ncol = 1L)))

  # The sampler is stochastic and rnetis does not control latentnet's internal
  # seeding, so these are not the same draw. What must hold is that rnetis
  # extracts the same QUANTITY from the same model, which shows up as the two
  # distance structures agreeing closely.
  expect_gt(stats::cor(d_got, d_want), 0.9)
  # Same seed, same declared network: the same positions, not just a similar
  # geometry.
  expect_lt(max(abs(got[common] - want[common])), 1e-8)
  # On a common scale, not merely a monotone one.
  expect_lt(abs(stats::sd(d_got) - stats::sd(d_want)) / stats::sd(d_want), 0.5)
})

test_that("the fit records which selection model produced the positions", {
  skip_if_not_installed("latentnet")
  skip_if_not_installed("RSiena")

  d <- .xu_s50()
  fit <- fit_influence(
    alcohol ~ ar() + exposure(alcohol, via = friend, normalize = "row") +
      latent(friend, d = 1, homophily = c("alcohol", "smoke"),
             control = .xu_control),
    data = d$data, waves = c(2, 3)
  )
  rec <- fit$resolved_latent[["lat_friend_1"]]
  expect_match(rec$selection_formula, "euclidean(d = 1)", fixed = TRUE)
  expect_match(rec$selection_formula, 'absdiff("alcohol")', fixed = TRUE)
  expect_identical(unname(rec$feature_wave), 2)
  expect_identical(unname(rec$n_actors), 50L)

  # A position column is meaningless without the model behind it, so summary()
  # states it -- along with the two things that change how the coefficient
  # reads.
  out <- paste(utils::capture.output(print(summary(fit))), collapse = " ")
  expect_match(out, "Latent-space adjustment", fixed = TRUE)
  expect_match(out, "Xu 2018, 2021", fixed = TRUE)
  expect_match(out, "scale and location are arbitrary", fixed = TRUE)
  expect_match(out, "plug-in", fixed = TRUE)
})

# ---- reproduction, part 2: Xu's qualitative finding -------------------------

test_that("the adjustment enters the model as one covariate and moves the estimate", {
  skip_if_not_installed("latentnet")
  skip_if_not_installed("RSiena")

  # WHAT XU FOUND, AND WHAT THIS CAN AND CANNOT CHECK.
  #
  # On his full covariate set (2021, p. 63) the exposure coefficient falls from
  # 0.15298 (SE 0.07516, p = 0.0446) to 0.128865 (SE 0.083382, p = 0.1257) when
  # the two latent-position columns are added -- an influence effect that would
  # otherwise be reported as significant does not survive the adjustment.
  #
  # THAT DIRECTION IS NOT ASSERTED HERE, and the reason is recorded rather than
  # worked around. Xu's model has five covariates (alcohol, smoking, drug use,
  # sport, friendship), a STACKED pair of transitions, and TWO position columns
  # from the 1995 and 1996 networks. RSiena ships the networks, alcohol and
  # smoking; drug use and sport come from .dat files Xu downloads, and tests
  # may not reach the network. On the subset that IS available, the adjustment
  # moves the exposure coefficient only slightly and not consistently in either
  # direction across seeds.
  #
  # Asserting attenuation anyway would be asserting what the method is supposed
  # to do rather than what this fixture shows. What IS checked is that the adjustment is
  # wired correctly: it adds exactly the column it claims to, and the estimate
  # responds to it.
  #
  # Xu (2021)'s published table is not reproduced OFFLINE, here: RSiena ships
  # the networks, alcohol and smoking, but drug use and sport come from files
  # his script downloads. Two of five covariates are unavailable to this
  # suite, so these coefficients cannot match his. That is a data-availability
  # limitation of the test environment, not a limitation of the code; with all
  # five covariates present the reproduction runs outside the package test
  # suite, as this file's header records.
  d <- .xu_s50()
  base_f <- alcohol ~ ar() + exposure(alcohol, via = friend, normalize = "row") + smoke
  adj_f <- alcohol ~ ar() + exposure(alcohol, via = friend, normalize = "row") +
    smoke + latent(friend, d = 1, homophily = c("alcohol", "smoke"),
                   control = .xu_control)

  set.seed(3)
  unadjusted <- fit_influence(base_f, data = d$data, waves = c(2, 3))
  adjusted <- fit_influence(adj_f, data = d$data, waves = c(2, 3))

  e_un <- unname(coef(unadjusted)[["ex_alcohol_friend"]])
  e_ad <- unname(coef(adjusted)[["ex_alcohol_friend"]])
  expect_true(is.finite(e_un) && is.finite(e_ad))

  # Exactly one column added, and it is the one named.
  expect_true("lat_friend_1" %in% names(coef(adjusted)))
  expect_false("lat_friend_1" %in% names(coef(unadjusted)))
  expect_equal(length(coef(adjusted)) - length(coef(unadjusted)), 1L)

  # The estimate responds to it: a covariate that changed nothing at all would
  # mean the column was constant, or never reached the design matrix.
  expect_false(isTRUE(all.equal(e_un, e_ad)))
  expect_gt(stats::var(adjusted$model_frame$lat_friend_1), 0)
})

# ---- refusals ---------------------------------------------------------------

test_that("a homophily variable that is not a node variable is refused by name", {
  skip_if_not_installed("latentnet")
  skip_if_not_installed("RSiena")
  d <- .xu_s50()
  expect_error(
    fit_influence(
      alcohol ~ ar() + latent(friend, d = 1, homophily = "not_a_variable",
                              control = .xu_control),
      data = d$data, waves = c(2, 3)
    ),
    "not_a_variable"
  )
})

test_that("latent() validates its own arguments before anything is fitted", {
  expect_error(latent(friend, d = 0), "positive integer")
  expect_error(latent(friend, lag = -1), "at least 0")
  expect_error(latent(friend, homophily = 1), "character vector")
  expect_error(latent(friend, terms = 1), "ergmm model terms")
  expect_error(latent(friend, control = "fast"), "must be a list")
})

test_that("a nodal ergmm term named only in terms = fits", {
  skip_if_not_installed("latentnet")
  r <- .xu_s50()
  term <- latent(friend, d = 1, terms = "nodecov('alcohol')", control = .xu_control, seed = 1)
  res <- suppressWarnings(.resolve_latent_positions(term, r$data, netis_schema(r$data), 1))
  expect_equal(nrow(res$columns), length(r$ids))
  expect_true(all(is.finite(res$columns$lat_friend_1)))
})
