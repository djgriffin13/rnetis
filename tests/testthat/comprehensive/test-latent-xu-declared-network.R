# latent()'s stage one is latentnet::ergmm() fitted to the DECLARED network.
#
# With a recorded seed the stage-one fit is reproducible, so rnetis's positions
# can be compared with a direct ergmm() call EXACTLY rather than as a geometry.
# The Euclidean distance term alone is symmetric in i and j, so for it a
# transposed network gives the same likelihood; the orientation itself is
# pinned on the network handed to ergmm in test-latent-resolve.R.

.decl_fixture <- function() {
  ids <- sprintf("n%02d", 1:12)
  src <- c(rep("n01", 6), rep("n02", 5), "n03", "n04", "n05")
  tgt <- c("n03", "n04", "n05", "n06", "n07", "n08",
           "n07", "n08", "n09", "n10", "n11", "n12", "n12", "n06")
  nodes <- data.frame(group = 1, node = rep(ids, 2), wave = rep(1:2, each = 12),
                      y = as.numeric(rep(1:12, 2)) + rep(c(0, 0.5), each = 12))
  ties <- data.frame(group = 1, source = src, target = tgt, wave = 1, f = 1)
  ties <- rbind(ties, transform(ties, wave = 2))
  list(data = netis_data(nodes, ties, wave_col = "wave"), ids = ids, src = src, tgt = tgt)
}

.decl_control <- list(sample.size = 200, burnin = 200, interval = 2)

test_that("rnetis's stage-one positions equal a direct ergmm() fit of the declared network", {
  skip_if_not_installed("latentnet")
  fx <- .decl_fixture()
  s <- netis_schema(fx$data)

  got <- .resolve_latent_positions(
    latent(f, d = 1, control = .decl_control, seed = 2024), fx$data, s, 1
  )

  adj <- matrix(0, 12, 12, dimnames = list(fx$ids, fx$ids))
  adj[cbind(fx$src, fx$tgt)] <- 1
  net <- network::network(adj, directed = TRUE)
  set.seed(2024)
  direct <- latentnet::ergmm(net ~ euclidean(d = 1),
                             control = do.call(latentnet::ergmm.control, .decl_control),
                             verbose = FALSE)
  want <- as.vector(direct$mkl$Z)
  # One group: positions enter uncentered.
  got_z <- got$columns$lat_f_1[match(fx$ids, got$columns$node)]
  expect_equal(got_z, want, tolerance = 1e-10)
  expect_identical(got$seed, 2024L)
})

test_that("the same seed gives the same fit, and summary() reports it", {
  skip_if_not_installed("latentnet")
  fx <- .decl_fixture()
  fit_once <- function() {
    suppressWarnings(fit_influence(
      y ~ ar() + latent(f, d = 1, control = .decl_control, seed = 77),
      data = fx$data
    ))
  }
  a <- fit_once()
  b <- fit_once()
  expect_identical(a$model_frame$lat_f_1, b$model_frame$lat_f_1)
  expect_identical(a$resolved_latent[["lat_f_1"]]$seed, 77L)
  expect_match(paste(capture.output(summary(a)), collapse = "\n"), "stage-one seed 77")
})
