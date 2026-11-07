# Group-level moderators on a random coefficient.
#
#   theta_kg = z_g' gamma_k + zeta_kg,   z_g = (1, z_g1, ..., z_gM)'
#
# gamma_k is shared, so it joins the kernel's expanded map exactly as mu_k did;
# the only structural change is that row k of group g's T carries z_g'.

test_that("with no moderators the map is the no-moderator map exactly, not approximately", {
  # THE REDUCTION IS THE TEST. At M = 0, z_g = 1 and gamma_k0 IS mu_k, so the
  # moderator code path must reproduce the no-moderator map identically --
  # same names, same positions, same T. "Close enough" here would mean the
  # no-moderator results silently stop being reproducible by the moderator code.
  reg <- .mgc_regime(c("intercept", "x", "w"), c("group", "random", "shared"))
  a <- .mgc_expanded_map(reg)
  b <- .mgc_expanded_map(reg, Z = matrix(1, 3, 1, dimnames = list(NULL, "(Intercept)")))

  expect_identical(a$regime$coef_names, b$regime$coef_names)
  expect_identical(a$mu_pos, b$mu_pos)
  expect_identical(a$zeta_pos, b$zeta_pos)
  expect_true("mu:x" %in% a$regime$coef_names)
  for (g in 1:3) expect_equal(.mgc_group_T(b, g), a$T)
})

test_that("exactly one of T and T_list is populated", {
  # A CALLER MUST NOT BE ABLE TO APPLY ONE GROUP'S MAP TO ANOTHER'S
  # COEFFICIENTS. With moderators every group has its own T, and a common `T`
  # left lying beside the list is an invitation to use it -- which would
  # attribute one group's moderator values to all of them, with nothing in the
  # output to show for it.
  reg <- .mgc_regime(c("intercept", "x"), c("group", "random"))
  Z <- cbind("(Intercept)" = 1, size = c(-1, 0, 1))
  plain <- .mgc_expanded_map(reg)
  with_mod <- .mgc_expanded_map(reg, Z = Z)

  expect_false(is.null(plain$T));    expect_null(plain$T_list)
  expect_null(with_mod$T);           expect_false(is.null(with_mod$T_list))
  expect_length(with_mod$T_list, 3L)
})

test_that("each group's map carries its own moderator row", {
  # KNOWN ANSWER, by hand. Random coefficient "x" is design column 2, and its
  # gamma occupies two shared columns (intercept, size). Group 3 has
  # size = 1, so row "x" of its T reads (1, 1) across those columns; group 1
  # has size = -1 and reads (1, -1). The deviation column stays 1 throughout.
  reg <- .mgc_regime(c("intercept", "x"), c("group", "random"))
  Z <- cbind("(Intercept)" = 1, size = c(-1, 0, 1))
  m <- .mgc_expanded_map(reg, Z = Z)

  expect_identical(m$regime$coef_names,
                   c("gamma:x:(Intercept)", "gamma:x:size", "intercept", "zeta:x"))
  expect_equal(unname(m$gamma_pos$x), 1:2)
  expect_equal(.mgc_group_T(m, 1)["x", 1:2], c("gamma:x:(Intercept)" = 1, "gamma:x:size" = -1))
  expect_equal(.mgc_group_T(m, 3)["x", 1:2], c("gamma:x:(Intercept)" = 1, "gamma:x:size" = 1))
  expect_equal(unname(.mgc_group_T(m, 2)["x", m$zeta_pos]), 1)

  # And the map does what it says: theta_x for group g is gamma0 + z_g gamma1
  # plus the deviation. Checked as arithmetic, not as structure.
  phi <- c(0.4, 0.25, -0.1, 0.05)   # gamma0, gamma1, intercept, zeta
  for (g in 1:3) {
    theta <- c(.mgc_group_T(m, g) %*% phi)
    expect_equal(unname(theta[2]), unname(0.4 + Z[g, "size"] * 0.25 + 0.05))
  }
})

test_that("the moderator matrix is refused when it cannot mean what it claims", {
  reg <- .mgc_regime(c("intercept", "x"), c("group", "random"))
  ok <- cbind("(Intercept)" = 1, size = c(-1, 0, 1))

  # No intercept column: every group's random coefficient would be forced
  # through the origin, which is a different model, silently.
  expect_error(.mgc_expanded_map(reg, Z = cbind(size = c(-1, 0, 1))),
               "intercept as its first column")
  # A missing moderator leaves a group with no defined population mean.
  bad <- ok; bad[2, 2] <- NA
  expect_error(.mgc_expanded_map(reg, Z = bad), "missing values")
  # An unnamed column cannot be reported per moderator.
  un <- ok; colnames(un) <- c("(Intercept)", "")
  expect_error(.mgc_expanded_map(reg, Z = un), "must be named")
  # Nothing to moderate.
  flat <- .mgc_regime(c("intercept", "x"), c("group", "shared"))
  expect_error(.mgc_expanded_map(flat, Z = ok), "at least one random coefficient")
})

test_that("the expanded shared prior is sized from the map, not from a count", {
  # THE SILENT-NARROWING FAILURE, in the place it would occur. gamma_k is
  # (1 + M) wide; sizing the prior block from the NUMBER of random
  # coefficients would build it too small and land the shared prior on the
  # wrong columns -- a real prior, applied to the wrong parameters.
  reg <- .mgc_regime(c("intercept", "x"), c("shared", "random"))
  Z <- cbind("(Intercept)" = 1, size = c(-1, 0, 1))
  m <- .mgc_expanded_map(reg, Z = Z)
  # gamma_sd is a RANDOM-COEFFICIENT x MODERATOR matrix, not a vector.
  # predictor_sd is indexed by coefficient and delta_Z by moderator; they are
  # different axes, so a vector here would be recycled against one of them and
  # hand a coefficient the scale built from the wrong moderator. Here q = 1 and
  # M = 1, so it is 1 x 1 -- and it is still a matrix, because the shape is the
  # contract rather than an artifact of this fixture's size.
  pri <- list(shared = list(P = matrix(1, 1, 1), m = 0),
              random = list(mu_sd = 2, mu_mean = 0.3,
                            gamma_sd = matrix(0.5, 1, 1,
                                              dimnames = list("x", "size"))))

  # NOTE the ORIGINAL regime, not the expanded one: the function derives
  # the expanded layout from the map. Passing the expanded regime makes `s`
  # count the gamma columns too -- my first version of this test did, and it
  # filled the whole matrix with the shared prior.
  sp <- .mgc_expanded_shared_prior(pri, reg, m)
  expect_equal(dim(sp$P), c(length(m$regime$shared), length(m$regime$shared)))

  # The intercept of gamma keeps mu's prior exactly; the moderator slope takes
  # gamma_sd and a zero mean. They are different quantities in different
  # units, and sharing a scale between them would be a coincidence, not a
  # decision.
  gi <- m$gamma_pos$x
  expect_equal(sp$P[gi[1], gi[1]], 1 / 2^2)
  expect_equal(sp$m[gi[1]], 0.3)
  expect_equal(sp$P[gi[2], gi[2]], 1 / 0.5^2)
  expect_equal(sp$m[gi[2]], 0)

  # And with no moderators it is the no-moderator prior unchanged.
  m0 <- .mgc_expanded_map(reg)
  s0 <- .mgc_expanded_shared_prior(pri, reg, m0)
  expect_equal(s0$P, .mgc_expanded_shared_prior(pri, reg)$P)
  expect_equal(s0$m, .mgc_expanded_shared_prior(pri, reg)$m)
})

test_that("moderator rows are matched to groups by name when they are labeled", {
  # POSITION IS NOT AN IDENTITY. Labeled rows supplied in a different order
  # than the fit's groups must be reordered, not trusted -- attaching one
  # group's moderators to another produces a fit that looks entirely normal.
  skip_if_not_installed("amen")
  gs <- .mgc_moderator_groups(G = 3, n = 8, seed = 4)
  Z <- cbind("(Intercept)" = 1, size = c(a = -1, b = 0, c = 1))
  shuffled <- Z[c("c", "a", "b"), , drop = FALSE]

  f1 <- .mgc_sample(gs, regime = c(intercept = "group", x.row = "random"),
                    family = "bin", R = 0L, nscan = 20, burn = 5, odens = 5,
                    seed = 7, moderators = Z)
  f2 <- .mgc_sample(gs, regime = c(intercept = "group", x.row = "random"),
                    family = "bin", R = 0L, nscan = 20, burn = 5, odens = 5,
                    seed = 7, moderators = shuffled)
  expect_equal(f1$theta, f2$theta)

  # A row naming a group the fit does not have is refused outright.
  wrong <- Z; rownames(wrong) <- c("a", "b", "zzz")
  expect_error(
    .mgc_sample(gs, regime = c(intercept = "group", x.row = "random"), family = "bin",
                R = 0L, nscan = 20, burn = 5, odens = 5, seed = 7, moderators = wrong),
    "Missing: c"
  )
  expect_error(
    .mgc_sample(gs, regime = c(intercept = "group", x.row = "random"), family = "bin",
                R = 0L, nscan = 20, burn = 5, odens = 5, seed = 7,
                moderators = Z[1:2, , drop = FALSE]),
    "2 row\\(s\\) for 3 group\\(s\\)"
  )
})

test_that("a constant-intercept moderator fit reduces to the no-moderator fit", {
  # THE END-TO-END REDUCTION, through the sampler rather than the map alone.
  # Supplying only an intercept column must reproduce the no-moderator fit draw for
  # draw under the same seed: same model, same parameterization, same RNG
  # consumption. A difference here means the moderator path perturbs the
  # sampler even when it has nothing to say.
  skip_if_not_installed("amen")
  gs <- .mgc_moderator_groups(G = 3, n = 8, seed = 5)
  reg <- c(intercept = "group", x.row = "random")
  args <- list(groups = gs, regime = reg, family = "bin", R = 0L,
               nscan = 40, burn = 10, odens = 5, seed = 11)

  plain <- do.call(.mgc_sample, args)
  unit <- do.call(.mgc_sample, c(args, list(
    moderators = matrix(1, 3, 1, dimnames = list(names(gs), "(Intercept)")))))

  expect_equal(plain$theta, unit$theta)
  expect_equal(plain$tau, unit$tau)
})

test_that("a moderator that explains the between-group spread is recovered", {
  # A PLAUSIBILITY CHECK, not a calibration study. Groups are built with theta_xg increasing in
  # a known moderator, and the question is only whether gamma picks up the
  # right SIGN and rough magnitude, and whether the deviations shrink relative
  # to a fit that is denied the moderator. No coverage claim is made here.
  skip_if_not_installed("amen")
  gs <- .mgc_moderator_groups(G = 6, n = 12, seed = 21,
                              slope = function(z) 0.2 + 0.8 * z)
  z <- attr(gs, "z")
  Z <- cbind("(Intercept)" = 1, size = z)
  rownames(Z) <- names(gs)

  fit <- .mgc_sample(gs, regime = c(intercept = "group", x.row = "random"),
                     family = "bin", R = 0L, nscan = 400, burn = 100, odens = 5,
                     seed = 3, moderators = Z)
  expect_identical(fit$moderators, "size")

  gam <- fit$gamma[, "gamma:x.row:size"]
  expect_true(mean(gam) > 0)
  # The truth is 0.8; at this size the interval is wide, so the assertion is
  # on the order of magnitude and the sign, which is what a plausibility
  # check can carry.
  expect_true(stats::quantile(gam, 0.9) > 0.2)
})
