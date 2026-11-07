# The AME variance shares in netis_variance_components() for a Selection fit.
#
# For a directed dyad (i != j) under
#     z_ij = x'beta + a_i + b_j + u_i'v_j + e_ij,
# (a_i, b_i) ~ N(0, [[va, cab], [cab, vb]]), the latent variance of one dyad is
#     Var(z_ij | x) = va + vb + V_uv + s2.
# There is NO 2*cab term: a_i and b_j belong to different actors and are
# independent. cab enters the reciprocal-pair covariance (2*cab + ... ) and an
# actor's combined sending+receiving variance (va + vb + 2*cab), never a single
# dyad's variance. For a symmetric relation z_ij = x'beta + a_i + a_j + ... the
# two actors are again independent, so the actor contribution is 2*va.
#
# TWO DEFECTS THIS GUARDS AGAINST:
#   (1) a share denominator of va + vb + ve, leaving out the latent-factor
#       (multiplicative) variance and overstating every share whenever R > 0.
#   (2) a symmetric fit counting va once while pseudo_r2() -- correctly --
#       uses 2*va, so the two functions disagree about the same fit.
#
# Deterministic: the per-draw variance components are held constant so every
# expected value is written in by hand.

.vc_draws <- function(values, n = 20L) {
  m <- matrix(rep(values, each = n), nrow = n)
  colnames(m) <- names(values)
  m
}

.vc_fixture <- function(vc, R = 0L, uv = NULL, symmetric = FALSE, n = 6L) {
  ids <- sprintf("a%d", seq_len(n))
  Y <- matrix(0, n, n, dimnames = list(ids, ids))
  diag(Y) <- NA
  extract <- list(
    variance_components = vc,
    multiplicative = uv %||% matrix(0, n, n)
  )
  structure(
    list(per_group = list(g1 = extract), groups = "g1", n_groups = 1L,
         symmetric = symmetric, R = R, designs = list(g1 = list(Y = Y))),
    class = c("netis_selection", "netis_fit")
  )
}

.share_of <- function(tbl, label) {
  tbl$residual_variance_share[tbl$term == label]
}

test_that("the dyad variance has no 2*cab term (known answer)", {
  parts <- .ame_dyad_variance_parts(va = 1, vb = 0.5, cab = 0.8, v_uv = 0.25, s2 = 1.25)
  expect_equal(parts[["total"]], 1 + 0.5 + 0.25 + 1.25)
  # Changing cab alone must not move the total.
  parts2 <- .ame_dyad_variance_parts(va = 1, vb = 0.5, cab = -0.4, v_uv = 0.25, s2 = 1.25)
  expect_identical(parts[["total"]], parts2[["total"]])
})

test_that("a symmetric dyad's actor contribution is 2 * va", {
  parts <- .ame_dyad_variance_parts(va = 0.7, v_uv = 0, s2 = 1, symmetric = TRUE)
  expect_equal(parts[["actor"]], 1.4)
  expect_equal(parts[["total"]], 2.4)
})

test_that("directed shares divide by va + vb + ve when R = 0, cab excluded", {
  vc <- .vc_draws(c(va = 1, cab = 0.8, vb = 0.5, rho = 0.3, ve = 1))
  tbl <- netis_variance_components(.vc_fixture(vc))
  total <- 1 + 0.5 + 1
  expect_equal(.share_of(tbl, "sender variance"), 1 / total)
  expect_equal(.share_of(tbl, "receiver variance"), 0.5 / total)
  expect_equal(.share_of(tbl, "dyadic residual variance"), 1 / total)
  expect_true(is.na(.share_of(tbl, "sender-receiver covariance")))
  expect_equal(sum(tbl$residual_variance_share, na.rm = TRUE), 1)
  expect_false("multiplicative (latent-factor) variance" %in% tbl$term)
})

test_that("with R > 0 the latent-factor variance enters the denominator", {
  n <- 6L
  set.seed(3)
  uv <- matrix(stats::rnorm(n * n), n, n)
  mask <- matrix(TRUE, n, n)
  diag(mask) <- FALSE
  v_uv <- stats::var(uv[mask])

  vc <- .vc_draws(c(va = 1, cab = 0.2, vb = 0.5, rho = 0.3, ve = 1))
  tbl <- netis_variance_components(.vc_fixture(vc, R = 1L, uv = uv, n = n))
  total <- 1 + 0.5 + v_uv + 1

  expect_equal(.share_of(tbl, "sender variance"), 1 / total)
  expect_equal(.share_of(tbl, "multiplicative (latent-factor) variance"), v_uv / total)
  expect_equal(sum(tbl$residual_variance_share, na.rm = TRUE), 1)
  # Posterior-mean U V' understates the per-draw variance; the table says so.
  expect_match(attr(tbl, "multiplicative_note"), "posterior-mean", fixed = TRUE)
})

test_that("a symmetric fit counts the actor variance twice", {
  vc <- .vc_draws(c(va = 1, ve = 1))
  tbl <- netis_variance_components(.vc_fixture(vc, symmetric = TRUE))
  actor_label <- "actor variance (symmetric: one additive effect)"
  # z_ij = a_i + a_j + e_ij: Var = 2*va + ve = 3.
  expect_equal(.share_of(tbl, actor_label), 2 / 3)
  expect_equal(.share_of(tbl, "dyadic residual variance"), 1 / 3)
  expect_equal(sum(tbl$residual_variance_share, na.rm = TRUE), 1)
  # The estimate column still carries the parameter itself, not the contribution.
  expect_equal(tbl$estimate[tbl$term == actor_label], 1)
})

test_that("the population offset has variance va + vb, not va + vb + 2*cab", {
  # .population_offset_variance() is the variance selection_effect(type =
  # "population") integrates over. A large cab must not change it.
  expect_identical(
    .population_offset_variance(c(va = 0.8, cab = 0.6, vb = 0.5, rho = 0, ve = 1)), 1.3)
})
