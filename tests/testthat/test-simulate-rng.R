# RNG semantics for simulate_influence().
#
# `seed =` is SCOPED, and an unseeded call is an ORDINARY consumer of the
# caller's stream. Those are two different promises and both need pinning,
# because the defect this replaced got the first one wrong and a naive fix
# gets the second one wrong.
#
# WHAT WENT WRONG BEFORE. The function called `set.seed(seed)` and left it set,
# so a simulation silently reseeded the caller's session: everything drawn
# afterwards followed from rnetis's seed rather than the user's. That is fixed.
#
# WHAT A CARELESS FIX WOULD BREAK. Saving and restoring the stream around
# EVERY call -- seeded or not -- would make an unseeded simulation leave
# `.Random.seed` untouched, so two successive unseeded calls would return
# identical data while looking random. An unseeded call must consume the
# stream like any other R function that draws.

test_that("a seeded call reproduces", {
  a <- simulate_influence(n_nodes = 8, n_waves = 2, beta = 0.3, seed = 4)
  b <- simulate_influence(n_nodes = 8, n_waves = 2, beta = 0.3, seed = 4)
  expect_identical(a$data, b$data)
})

test_that("a seeded call restores the caller's stream instead of replacing it", {
  # THE ORIGINAL DEFECT. `seed =` scopes reproducibility to this call; it is
  # not a request to reseed the session.
  set.seed(99)
  before <- .Random.seed
  invisible(simulate_influence(n_nodes = 8, n_waves = 2, beta = 0.3, seed = 4))
  expect_identical(before, .Random.seed)

  # And the observable consequence: what the caller draws next is what they
  # would have drawn without the simulation.
  set.seed(99)
  expected <- stats::runif(3)
  set.seed(99)
  invisible(simulate_influence(n_nodes = 8, n_waves = 2, beta = 0.3, seed = 4))
  expect_identical(expected, stats::runif(3))
})

test_that("an UNSEEDED call advances the caller's stream, like any other draw", {
  # THE OTHER HALF, and the one a careless fix breaks. Without a seed the
  # simulation is an ordinary consumer of the caller's stream: it must move
  # it, or successive unseeded calls would return identical data while
  # appearing random.
  set.seed(123)
  before <- .Random.seed
  first <- simulate_influence(n_nodes = 8, n_waves = 2, beta = 0.3)
  expect_false(identical(before, .Random.seed))

  second <- simulate_influence(n_nodes = 8, n_waves = 2, beta = 0.3)
  expect_false(identical(first$data, second$data))
})

test_that("an unseeded run is governed by an outer seed", {
  # What "consumes the caller's stream" buys: a whole script, simulation
  # included, is reproducible from one seed at the top. That is the ordinary
  # R contract, and it only holds because the unseeded path does NOT isolate
  # itself.
  set.seed(7)
  a <- simulate_influence(n_nodes = 8, n_waves = 2, beta = 0.3)
  set.seed(7)
  b <- simulate_influence(n_nodes = 8, n_waves = 2, beta = 0.3)
  expect_identical(a$data, b$data)
})

test_that("a seeded call works with no pre-existing stream, and leaves none", {
  # The edge the restore logic has to handle: a fresh session has no
  # `.Random.seed` at all, so "restore what was there" means removing what
  # set.seed() created rather than assigning an old value.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  invisible(simulate_influence(n_nodes = 6, n_waves = 2, beta = 0.3, seed = 5))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})
