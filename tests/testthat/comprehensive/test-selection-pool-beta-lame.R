# Gate 4 of the pooling contract: agreement with lame::lame_multi().
#
# MOVED OUT OF THE SHIPPED SUITE so that `lame` can leave Suggests. The main
# tier's gates 1-3 establish what the pooling must be and remain there; this
# gate is corroboration from an independent implementation, and corroboration
# is exactly the kind of check that can live in the tier a maintainer runs
# deliberately rather than the one R CMD check runs for every user.
#
# `tests/testthat/comprehensive/` is .Rbuildignore'd, so nothing here reaches
# the built package and `lame::` is not an undeclared dependency.
#
#   Rscript -e 'testthat::test_dir("tests/testthat/comprehensive")'

# ---- gate 4: agreement with lame::lame_multi() ------------------------------
#
# CORROBORATION, NOT AUTHORITY. Gates 1-3 above establish what the pooling must
# be; this gate says a second, independent implementation of the same idea
# agrees. `lame` (Dorff, Minhas & Salau) does not depend on `amen` and shares no
# code with it or with rnetis, so agreement here is a real cross-check rather
# than two copies of one routine.
#
# If rnetis and lame_multi() ever disagree, gates 1-3 decide which is right. The
# discrepancy gets investigated; rnetis is NOT adjusted until it matches. If the
# disagreement turns out to expose a defect or an unsupported assumption in
# lame, that is documented and rnetis keeps the mathematically validated
# behavior.
#
# WHAT IS AND IS NOT COMPARED. rnetis does not claim to reproduce lame's sampler
# -- different sampler, different data structures, different priors. The claim
# is only about the COMBINATION RULE. So the comparison feeds rnetis's pooler
# lame's OWN per-panel posterior draws and checks that rnetis recovers the
# pooled values lame reports. That isolates the pooling from everything
# upstream of it, which is the only part rnetis reimplements.

test_that("rnetis's pooler reproduces lame_multi()'s pooled beta from lame's own draws", {
  skip_if_not_installed("lame")

  set.seed(4)
  n <- 8
  mk_y <- function() {
    y <- matrix(stats::rbinom(n * n, 1, 0.3), n, n)
    diag(y) <- NA                      # amen/lame convention: no self-ties
    dimnames(y) <- list(paste0("a", 1:n), paste0("a", 1:n))
    y
  }
  mk_x <- function() {
    array(stats::rnorm(n * n), dim = c(n, n, 1L),
          dimnames = list(paste0("a", 1:n), paste0("a", 1:n), "x"))
  }

  # Two panels. Short chains: the gate is about the arithmetic applied to the
  # draws, not about whether the chains converged, so a long run would cost
  # time without testing anything more.
  fitted <- lame::lame_multi(
    Y_list = list(list(mk_y()), list(mk_y())),
    Xdyad_list = list(list(mk_x()), list(mk_x())),
    family = "bin", nscan = 60, burn = 10, odens = 5,
    print = FALSE, plot = FALSE
  )

  # Hand lame's per-panel BETA matrices to rnetis's pooler in the shape the
  # pooler reads, and compare against what lame itself reported.
  per_group <- lapply(fitted$fits, function(f) list(extracted = list(draws = f$BETA)))
  ours <- .pool_beta_precision(per_group)

  # THE EQUIVALENCE MUST NOT REST ON A SHARED NUMERICAL CONVENTION. rnetis and
  # lame both guard the 1/v division, and both happen to guard it at 1e-8. If
  # any panel variance came near that, agreement here would demonstrate two
  # implementations rounding the same way rather than computing the same
  # pooling formula. Asserted, not assumed.
  panel_vars <- unlist(lapply(fitted$fits, function(f) apply(f$BETA, 2L, stats::var)))
  expect_true(all(panel_vars > 1e4 * .POOL_VARIANCE_FLOOR))
  expect_length(ours$floored, 0L)

  expect_equal(unname(ours$mean), unname(as.numeric(fitted$beta_shared)),
               tolerance = 1e-12)
  expect_equal(unname(ours$var), unname(as.numeric(fitted$beta_shared_var)),
               tolerance = 1e-12)

  # And the same coefficients, in the same order -- an agreement on values
  # under a scrambled naming would not be an agreement.
  expect_identical(names(ours$mean), colnames(fitted$fits[[1]]$BETA))
})
