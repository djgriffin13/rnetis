# The simultaneous network-autocorrelation (SAR) route: Y = rho * W1 Y + Xb + e.
#
# Reached when the formula puts the outcome's OWN contemporaneous exposure on
# the right-hand side -- a term least squares cannot fit, because the regressor
# is determined jointly with the outcome.
#
# Three failure modes of this adapter are guarded below. Each is MEASURED
# against what sna::lnam actually returns rather than assumed, because a test
# that assumes the adapter handles it proves nothing about whether it does.

.sar_chain <- function(n = 20, rho = 0.3, seed = 7, na_at = integer(0)) {
  set.seed(seed)
  ids <- sprintf("a%02d", seq_len(n))
  # A chain, i -- i+1, stored in both directions because the tie table is a
  # directed edge list and the conduit is read as given.
  edges <- rbind(
    data.frame(group = 1, source = ids[-n], target = ids[-1], friend = 1,
               stringsAsFactors = FALSE),
    data.frame(group = 1, source = ids[-1], target = ids[-n], friend = 1,
               stringsAsFactors = FALSE)
  )
  W <- matrix(0, n, n, dimnames = list(ids, ids))
  for (i in seq_len(n - 1)) { W[i, i + 1] <- 1; W[i + 1, i] <- 1 }

  x <- stats::rnorm(n)
  # Generate from the REDUCED FORM, which is what the SAR model implies:
  # y = (I - rho W)^-1 (Xb + e). Generating y = rho W y + ... directly would
  # need a y that does not exist yet.
  y <- as.vector(solve(diag(n) - rho * W) %*% (1 + 0.8 * x + stats::rnorm(n, sd = 0.5)))

  nodes <- data.frame(group = 1, node = ids, y = y, x = x, stringsAsFactors = FALSE)
  if (length(na_at)) nodes$x[na_at] <- NA
  list(
    data = netis_data(nodes, edges,
                      tie_meta = list(friend = list(zero_meaning = "measured_zero"))),
    W = W, ids = ids, y = y, x = x
  )
}

.sar_formula <- y ~ x + exposure(y, via = friend, x_lag = 0)

# ---- the route is chosen by the formula, never named ------------------------

test_that("a same-outcome contemporaneous exposure selects the SAR estimator", {
  est <- .resolve_estimator(has_random_effects = FALSE, has_endogenous = TRUE)
  expect_identical(as.character(est), "lnam")
  expect_match(attr(est, "reason"), "contemporaneous network exposure")
})

test_that("the endogenous term outranks the random-effects term", {
  # Precedence, not preference: the specification is not estimable by least
  # squares at all, so nothing about a random-effects term could override it.
  est <- .resolve_estimator(has_random_effects = TRUE, has_endogenous = TRUE)
  expect_identical(as.character(est), "lnam")
})

test_that("a lagged exposure does NOT select the SAR route", {
  expect_identical(
    as.character(.resolve_estimator(has_random_effects = FALSE, has_endogenous = FALSE)),
    "pooled"
  )
})

# ---- 1: convergence must not be claimed --------------------------------------

test_that("sna::lnam reports no optimizer status, so rnetis must not claim one", {
  skip_if_not_installed("sna")
  # THE MEASUREMENT, not the assumption. A hardcoded `converged = TRUE` would
  # be a claim about something the optimizer never reports. If a future sna ever
  # does return a status, this test fails and the adapter should start using
  # it -- which is the right outcome, not a nuisance.
  set.seed(3)
  n <- 8
  W <- matrix(0, n, n)
  for (i in seq_len(n - 1)) { W[i, i + 1] <- 1; W[i + 1, i] <- 1 }
  xx <- cbind(1, stats::rnorm(n))
  yy <- as.vector(solve(diag(n) - 0.2 * W) %*% (xx %*% c(1, 0.5) + stats::rnorm(n, sd = 0.3)))
  raw <- sna::lnam(y = yy, x = xx, W1 = W)
  expect_false("convergence" %in% names(raw))
})

test_that("convergence is derived from the estimates, not asserted", {
  skip_if_not_installed("sna")
  fit <- fit_influence(.sar_formula, data = .sar_chain()$data)
  # All finite estimates is what CAN be known; the flag must track that rather
  # than being TRUE unconditionally.
  expect_identical(fit$convergence$converged, all(is.finite(coef(fit))))
  expect_true(fit$convergence$converged)
})

# ---- 2: residuals must not be named positionally ----------------------------

test_that("sna::lnam returns unnamed residuals, so the adapter must supply names", {
  skip_if_not_installed("sna")
  set.seed(3)
  n <- 8
  W <- matrix(0, n, n)
  for (i in seq_len(n - 1)) { W[i, i + 1] <- 1; W[i + 1, i] <- 1 }
  xx <- cbind(1, stats::rnorm(n))
  yy <- as.vector(solve(diag(n) - 0.2 * W) %*% (xx %*% c(1, 0.5) + stats::rnorm(n, sd = 0.3)))
  raw <- sna::lnam(y = yy, x = xx, W1 = W)
  expect_null(names(raw$residuals))
})

test_that("rnetis names residuals by the rows the model frame kept", {
  skip_if_not_installed("sna")
  fit <- fit_influence(.sar_formula, data = .sar_chain()$data)
  r <- residuals(fit)
  expect_false(is.null(names(r)))
  expect_identical(names(r), rownames(fit$model_frame))
})

# ---- 3: the NA dimension crash ----------------------------------------------

test_that("an incomplete row no longer crashes the SAR fit", {
  skip_if_not_installed("sna")
  # Guards against W1 being built for every row while model.frame() drops the
  # incomplete one, which makes sna::lnam refuse with
  #   "Order of W1 must match length of y."
  d <- .sar_chain(na_at = 5L)
  fit <- fit_influence(.sar_formula, data = d$data)
  expect_s3_class(fit, "netis_influence")

  # The ASSEMBLED table still holds every row -- the drop happens inside the
  # adapter, where model.frame() removes the incomplete one. What shrank is the
  # fitted system, which is what the residual count and the dropped-node record
  # report.
  expect_equal(nrow(fit$model_frame), 20L)
  expect_length(residuals(fit), 19L)
  expect_length(fit$dropped_nodes, 1L)
})

test_that("the removed node is NAMED, not absorbed", {
  skip_if_not_installed("sna")
  d <- .sar_chain(na_at = 5L)
  fit <- fit_influence(.sar_formula, data = d$data)
  expect_identical(fit$dropped_nodes, "a05")

  # And said out loud wherever the fit is printed, because in a simultaneous
  # model this is not listwise deletion: it removes a node from the NETWORK.
  txt <- paste(utils::capture.output(print(summary(fit))), collapse = " ")
  expect_match(txt, "Removed from the network: 1 node", fixed = TRUE)
  expect_match(txt, "not just an observation from the sample", fixed = TRUE)
})

test_that("subsetting W1 equals rebuilding it over the retained nodes", {
  # The claim .lnam_align_w1()'s comment makes, tested rather than asserted.
  # It holds only because .build_canonical_W() returns the RAW adjacency: had
  # W1 been row-normalized first, the subset would leave rows no longer
  # summing to their intended total and the two would differ.
  n <- 6
  ids <- sprintf("a%02d", seq_len(n))
  W <- matrix(0, n, n, dimnames = list(ids, ids))
  for (i in seq_len(n - 1)) { W[i, i + 1] <- 1; W[i + 1, i] <- 1 }
  table_rows <- as.character(seq_len(n))

  keep <- table_rows[-3]
  aligned <- .lnam_align_w1(W, keep, table_rows)

  expect_identical(aligned$dropped, "a03")
  expect_equal(dim(aligned$W1), c(5L, 5L))
  # Rebuilding over the retained node set gives the same matrix: ties to the
  # removed node simply vanish.
  expect_equal(aligned$W1, W[ids[-3], ids[-3]])
})

test_that("a complete fit drops nothing and leaves W1 untouched", {
  n <- 4
  ids <- sprintf("a%02d", seq_len(n))
  W <- matrix(seq_len(n * n), n, n, dimnames = list(ids, ids))
  aligned <- .lnam_align_w1(W, as.character(seq_len(n)), as.character(seq_len(n)))
  expect_identical(aligned$W1, W)
  expect_length(aligned$dropped, 0L)
})

# ---- reference equivalence: the package's central correctness claim ---------

test_that("the rnetis SAR fit equals calling sna::lnam() directly", {
  skip_if_not_installed("sna")
  d <- .sar_chain()
  fit <- fit_influence(.sar_formula, data = d$data)

  # The same three inputs, assembled by hand from the fit's own estimating
  # sample so that any disagreement is rnetis's translation and not a different
  # dataset.
  # The assembled table names the outcome `.outcome`, and every row of it was
  # complete here, so the fitted system is the whole network.
  mf <- fit$model_frame
  ids <- as.character(mf$node)
  direct <- sna::lnam(
    y = mf$.outcome,
    x = cbind(`(Intercept)` = 1, x = mf$x),
    W1 = d$W[ids, ids]
  )

  expect_equal(unname(as.vector(direct$beta)), unname(coef(fit)[c("(Intercept)", "x")]),
               tolerance = 1e-8)
  expect_equal(unname(direct$rho1), unname(coef(fit)[["ex_y_friend"]]),
               tolerance = 1e-8)
  expect_equal(unname(direct$sigma), unname(fit$sigma), tolerance = 1e-8)
})

test_that("equivalence holds when a row was dropped, on the reduced network", {
  skip_if_not_installed("sna")
  # The case the fix creates. Calling lnam directly on the RETAINED nodes'
  # subnetwork must reproduce rnetis exactly -- which is the precise claim the
  # subsetting makes.
  d <- .sar_chain(na_at = 5L)
  fit <- fit_influence(.sar_formula, data = d$data)
  # Only the RETAINED rows, which is the subnetwork rnetis fitted.
  mf <- fit$model_frame[!is.na(fit$model_frame$x), , drop = FALSE]
  ids <- as.character(mf$node)
  direct <- sna::lnam(y = mf$.outcome, x = cbind(`(Intercept)` = 1, x = mf$x),
                      W1 = d$W[ids, ids])
  expect_equal(unname(direct$rho1), unname(coef(fit)[["ex_y_friend"]]),
               tolerance = 1e-8)
})

# ---- recovery ---------------------------------------------------------------

test_that("the SAR route recovers a known rho from data generated with it", {
  skip_if_not_installed("sna")
  # Not a tolerance chosen to pass: 200 actors and a modest rho, generated
  # from the reduced form the model implies. A route that recovered nothing
  # would miss this by far more than the interval allows.
  d <- .sar_chain(n = 200, rho = 0.4, seed = 11)
  fit <- fit_influence(.sar_formula, data = d$data)
  expect_lt(abs(unname(coef(fit)[["ex_y_friend"]]) - 0.4), 0.048)
  expect_equal(unname(coef(fit)[["x"]]), 0.8, tolerance = 0.15)
})

# ---- the simultaneous system is reported as a fact, not a verdict -----------

test_that("the spectral radius and invertibility are reported", {
  skip_if_not_installed("sna")
  fit <- fit_influence(.sar_formula, data = .sar_chain()$data)
  expect_true(is.finite(fit$w1_spectral_radius))
  expect_true(is.finite(fit$rho))
  expect_true(isTRUE(fit$sar_operator_invertible))

  txt <- paste(utils::capture.output(print(fit)), collapse = " ")
  expect_match(txt, "Simultaneous system: rho =", fixed = TRUE)
  expect_match(txt, "spectral radius of W1", fixed = TRUE)
  # A structural fact, stated as what was checked. No pass/fail grade.
  expect_match(txt, "min |1 - rho * lambda|", fixed = TRUE)
  expect_match(txt, "is invertible and the reduced form exists", fixed = TRUE)
  expect_false(grepl("PASS|FAIL|good|poor|acceptable", txt))
})

test_that("invertibility is the eigenvalue condition, not |rho| * radius < 1", {
  # Eigenvalues 2 and -0.5 (upper triangular), so (I - rho W) is singular at
  # rho = 0.5 and rho = -2 and nowhere else.
  W <- matrix(c(2, 0, 1, -0.5), 2, 2)
  ev <- eigen(W, only.values = TRUE)$values
  expect_false(.sar_operator_check(0.5, ev)$invertible)
  expect_false(.sar_operator_check(-2, ev)$invertible)
  # rho = 1: |rho| * radius = 2, yet 1 - lambda is -1 and 1.5.
  ok <- .sar_operator_check(1, ev)
  expect_true(ok$invertible)
  expect_lt(abs(ok$min_modulus - 1), 1e-12)
  expect_lt(abs(det(diag(2) - 1 * W) - (-1 * 1.5)), 1e-12)
})

test_that("an admissible negative rho above 1 / radius is not called singular", {
  skip_if_not_installed("sna")
  # A symmetric network whose extreme eigenvalues are far apart: the
  # admissible interval (1 / lambda_min, 1 / lambda_max) contains rho values
  # with |rho| * radius > 1, and the operator is invertible there.
  set.seed(5)
  n <- 30
  ids <- sprintf("n%02d", 1:n)
  A <- matrix(stats::rbinom(n * n, 1, .25), n, n); diag(A) <- 0
  A <- pmax(A, t(A)); dimnames(A) <- list(ids, ids)
  ev <- eigen(A, only.values = TRUE)$values
  rho <- 0.8 / min(ev)
  x <- stats::rnorm(n)
  y <- as.numeric(solve(diag(n) - rho * A, 1 + x + stats::rnorm(n, sd = .3)))
  i <- which(A == 1, arr.ind = TRUE)
  nd <- netis_data(data.frame(group = "g", node = ids, y = y, x = x),
                   data.frame(group = "g", source = ids[i[, 1]], target = ids[i[, 2]], tie = 1))
  fit <- fit_influence(y ~ exposure(y, via = tie, x_lag = 0) + x, data = nd)
  expect_gt(abs(fit$rho) * fit$w1_spectral_radius, 1)
  expect_true(isTRUE(fit$sar_operator_invertible))
  expect_lt(abs(fit$sar_operator_min_modulus - min(Mod(1 - fit$rho * ev))), 1e-8)
  expect_false("sar_operator_not_invertible" %in%
                 as.data.frame(netis_diagnostics(fit))$code)
})

test_that("a singular system says so rather than being graded", {
  fake <- structure(
    list(backend = "lnam", rho = 0.5, w1_spectral_radius = 2,
         sar_operator_min_modulus = 0, sar_operator_invertible = FALSE,
         information_matrix_available = TRUE, dropped_nodes = character(0)),
    class = "netis_influence"
  )
  txt <- paste(.format_sar_system(fake), collapse = " ")
  expect_match(txt, "numerically zero", fixed = TRUE)
  expect_match(txt, "is singular at this estimate", fixed = TRUE)
})

test_that("nothing is printed off the SAR route", {
  expect_length(.format_sar_system(structure(list(backend = "stats"),
                                             class = "netis_influence")), 0L)
})

test_that("missing standard errors are reported as missing", {
  fake <- structure(
    list(backend = "lnam", rho = 0.2, w1_spectral_radius = 1.5,
         information_matrix_available = FALSE, dropped_nodes = character(0)),
    class = "netis_influence"
  )
  expect_match(paste(.format_sar_system(fake), collapse = " "),
               "Standard errors are unavailable", fixed = TRUE)
})

# ---- refusals ---------------------------------------------------------------

test_that("the SAR route refuses a random-effects term", {
  skip_if_not_installed("sna")
  d <- .sar_chain()
  expect_error(
    fit_influence(y ~ x + exposure(y, via = friend, x_lag = 0) + (1 | group),
                  data = d$data),
    "random"
  )
})

test_that("predict() on new data refuses rather than returning the wrong thing", {
  skip_if_not_installed("sna")
  fit <- fit_influence(.sar_formula, data = .sar_chain()$data)
  # With no newdata this returns the fit's own fitted values, as on every other
  # route. Predicting for NEW data is what has no implementation: it needs
  # (I - rho * W1)^-1 solved for that data's own network, so it refuses by name
  # instead of returning X %*% beta, which is not a SAR prediction.
  expect_error(stats::predict(fit, newdata = fit$model_frame),
               "simultaneous autoregressive")
})

test_that("the estimator needs both sna and numDeriv, and says which is missing", {
  # numDeriv is sna's own Suggests, so a fresh sna install can lack it and the
  # fit then silently returns no standard errors. Required here rather than
  # left to chance.
  reg <- .estimator_registry()$lnam
  expect_identical(reg$requires, c("sna", "numDeriv"))
  expect_identical(reg$backend, "lnam")
})

test_that("the SAR fit is the maximum-likelihood estimate on a directed network, by hand", {
  skip_if_not_installed("sna")
  skip_if_not_installed("numDeriv")
  # Directed and asymmetric, so reading W1 the wrong way round gives another fit.
  n <- 12
  ids <- sprintf("a%02d", seq_len(n))
  src <- c(1, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 10, 11, 12, 12, 2, 5)
  tgt <- c(2, 3, 3, 4, 5, 1, 6, 7, 8, 8, 9, 10, 11, 12, 1, 5, 7, 11)
  # The default direction: the source influences the target, so row = target.
  W <- matrix(0, n, n, dimnames = list(ids, ids))
  W[cbind(tgt, src)] <- 1
  set.seed(42)
  x <- stats::rnorm(n)
  z <- stats::rnorm(n)
  y <- as.vector(solve(diag(n) - 0.25 * W,
                       1 + 0.8 * x + 0.5 * as.vector(W %*% z) + stats::rnorm(n, sd = 0.4)))
  nd <- netis_data(
    data.frame(group = 1, node = ids, y = y, x = x, z = z),
    data.frame(group = 1, source = ids[src], target = ids[tgt], friend = 1),
    tie_meta = list(friend = list(zero_meaning = "measured_zero"))
  )
  fit <- fit_influence(
    y ~ x + exposure(z, via = friend, x_lag = 0, name = "ex_z") +
      exposure(y, via = friend, x_lag = 0),
    data = nd
  )
  mf <- fit$model_frame
  o <- as.character(mf$node)
  W1 <- W[o, o]
  yy <- mf$.outcome
  X <- cbind(1, mf$x, as.vector(W1 %*% z[match(o, ids)]))
  expect_lt(max(abs(X[, 3] - mf$ex_z)), 1e-12)

  # The concentrated log-likelihood, maximised directly.
  concentrated <- function(rho) {
    A <- diag(n) - rho * W1
    e <- A %*% yy - X %*% solve(crossprod(X), crossprod(X, A %*% yy))
    as.numeric(determinant(A)$modulus) - n / 2 * log(sum(e^2) / n) - n / 2 * (1 + log(2 * pi))
  }
  radius <- max(Mod(eigen(W1, only.values = TRUE)$values))
  opt <- stats::optimize(concentrated, c(-0.99, 0.99) / radius, maximum = TRUE, tol = 1e-10)
  A <- diag(n) - opt$maximum * W1
  b <- solve(crossprod(X), crossprod(X, A %*% yy))
  s2 <- sum((A %*% yy - X %*% b)^2) / n

  terms <- c("(Intercept)", "x", "ex_z")
  expect_lt(abs(fit$rho - opt$maximum), 1e-4)
  expect_lt(max(abs(coef(fit)[terms] - as.vector(b))), 1e-4)
  expect_lt(abs(fit$sigma - sqrt(s2)), 1e-4)
  expect_lt(abs(as.numeric(logLik(fit)) - opt$objective), 1e-6)

  # The covariance is the inverse information of the same likelihood.
  full <- function(th) {
    A <- diag(n) - th[4] * W1
    e <- A %*% yy - X %*% th[1:3]
    as.numeric(determinant(A)$modulus) - n / 2 * log(2 * pi * th[5]) - sum(e^2) / (2 * th[5])
  }
  V <- solve(-numDeriv::hessian(full, c(as.vector(b), opt$maximum, s2)))
  se <- sqrt(diag(stats::vcov(fit)))[c(terms, "ex_y_friend")]
  expect_lt(max(abs(se - sqrt(diag(V))[1:4])), 1e-4)

  # The transpose is a different model on this network.
  flipped <- sna::lnam(y = yy, x = X, W1 = t(W1))
  expect_gt(abs(fit$rho - unname(flipped$rho1)), 0.1)

  # What influence_effect() reports is beta times the range, and what it excludes
  # is real: the reduced form gives another number for a change of that size.
  e <- influence_effect(fit)$exposures$ex_z
  q <- stats::quantile(mf$ex_z, c(0.1, 0.9), names = FALSE)
  product <- unname(coef(fit)["ex_z"]) * (q[2] - q[1])
  expect_lt(abs(e$coefficient_times_contrast - product), 1e-12)
  S <- solve(diag(n) - fit$rho * W1)
  everyone_shifted <- product * mean(rowSums(S))
  expect_gt(abs(everyone_shifted - product), 0.25 * abs(product))
})
