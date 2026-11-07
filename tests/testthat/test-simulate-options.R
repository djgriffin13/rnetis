# What each simulate_influence() option does to the generated data, measured on
# the data against the value the help page states.

.so_nodes <- function(s, w) {
  n <- netis_nodes(s$data)
  n <- n[n$wave == w, ]
  n[order(n$group, n$node), ]
}

.so_alpha <- function(s, nodes) {
  unname(s$truth$node_effects[paste(nodes$group, nodes$node, sep = ":")])
}

# The row-normalized influence matrix of a one-group simulation, row = receiver.
.so_W <- function(s, ids) {
  t <- netis_ties(s$data)
  W <- matrix(0, length(ids), length(ids), dimnames = list(ids, ids))
  W[cbind(as.character(t$target), as.character(t$source))] <- 1
  W / ifelse(rowSums(W) > 0, rowSums(W), 1)
}

test_that("n_waves > 2 generates every wave, and a fit reads the one transition named", {
  s <- simulate_influence(n_nodes = 60, n_waves = 4, beta = 0.2, gamma = 0.4,
                          normalize = "row", seed = 3)
  expect_identical(as.integer(table(netis_nodes(s$data)$wave)), rep(60L, 4))
  expect_identical(s$truth$n_waves, 4)

  fit <- fit_influence(y ~ ar() + exposure(y, via = tie, normalize = "row", isolate_rule = "zero"),
                       s$data, waves = c(2, 3))
  expect_identical(netis_estimand(fit)$description, "A single transition, 2 -> 3.")
  y2 <- .so_nodes(s, 2)
  y3 <- .so_nodes(s, 3)
  W <- .so_W(s, y2$node)
  ref <- stats::lm(y3$y ~ y2$y + as.vector(W %*% y2$y))
  expect_lt(max(abs(unname(coef(fit)[c("(Intercept)", "y_lag1", "ex_y_tie")]) -
                      unname(stats::coef(ref)))), 1e-10)
})

test_that("burnin is recorded, applied, and skipped when the process is explosive", {
  # Undirected, so a node's alters are influenced by it in turn.
  args <- list(n_nodes = 400, density = 0.02, gamma = 0.3, beta = 0.6, normalize = "row",
               directed = FALSE, seed = 4)
  default <- do.call(simulate_influence, args)
  none <- do.call(simulate_influence, c(args, list(burnin = 0)))
  expect_identical(default$truth$burnin, 50L)
  expect_identical(none$truth$burnin, 0)
  expect_identical(
    do.call(simulate_influence, c(args, list(initial_condition = "independent")))$truth$burnin,
    0L
  )
  # The documented reason for it: without burn-in wave 1 lacks the equilibrium
  # correlation between a node's outcome and its exposure to alters' outcomes.
  collinearity <- function(s) {
    y <- .so_nodes(s, 1)
    W <- .so_W(s, y$node)
    W <- (W + t(W)) > 0
    W <- W / ifelse(rowSums(W) > 0, rowSums(W), 1)
    stats::cor(y$y, as.vector(W %*% y$y))
  }
  expect_lt(abs(collinearity(none)), 0.12)
  expect_gt(collinearity(default), collinearity(none) + 0.15)

  # Explosive: a message at two waves, a warning at more, and the 0 used is recorded.
  expect_message(
    two <- simulate_influence(n_nodes = 40, density = 0.5, beta = 1, seed = 4),
    "burn-in was skipped"
  )
  expect_identical(two$truth$burnin, 0L)
  expect_identical(two$truth$burnin_requested, 50L)
  expect_gt(two$truth$companion_radius, 1)
  expect_warning(simulate_influence(n_nodes = 40, density = 0.5, beta = 1, n_waves = 3, seed = 4),
                 "explosive")
})

test_that("node_sd enters every wave, and initial_condition decides wave 1's link to it", {
  args <- list(n_nodes = 1500, density = 0.002, node_sd = 2, beta = 0, gamma = 0.5,
               normalize = "row", seed = 5)
  stationary <- do.call(simulate_influence, args)
  independent <- do.call(simulate_influence, c(args, list(initial_condition = "independent")))
  slope <- function(s, w) {
    n <- .so_nodes(s, w)
    unname(stats::coef(stats::lm(n$y ~ .so_alpha(s, n)))[2])
  }
  expect_lt(abs(stats::sd(stationary$truth$node_effects) - 2), 0.15)
  # At equilibrium y loads on alpha by 1 / (1 - gamma) = 2.
  expect_lt(abs(slope(stationary, 1) - 2), 0.08)
  expect_lt(abs(slope(stationary, 2) - 2), 0.08)
  # Independent start: no link at wave 1, and one recursion step gives slope 1.
  expect_lt(abs(slope(independent, 1)), 0.08)
  expect_lt(abs(slope(independent, 2) - 1), 0.08)
  # With no burn-in wave 1 has the stated variance of the independent draw.
  expect_lt(abs(stats::var(.so_nodes(independent, 1)$y) - 1), 0.12)
})

test_that("node_effects are named by group and node", {
  s <- simulate_influence(n_nodes = 4, n_groups = 2, node_sd = 1, density = 0.5, beta = 0.1,
                          normalize = "row", seed = 2)
  expect_identical(names(s$truth$node_effects),
                   paste(rep(1:2, each = 4), rep(1:4, 2), sep = ":"))
  expect_identical(names(s$truth$group_effects), c("1", "2"))
})

test_that("group_sd shifts each group by its recorded effect", {
  s <- simulate_influence(n_nodes = 25, n_groups = 150, group_sd = 3, beta = 0, gamma = 0.5,
                          density = 0.1, normalize = "row", seed = 6)
  expect_lt(abs(stats::sd(s$truth$group_effects) - 3), 0.5)
  n <- .so_nodes(s, 1)
  means <- tapply(n$y, n$group, mean)
  fit <- stats::lm(as.vector(means) ~ s$truth$group_effects[names(means)])
  expect_lt(abs(unname(stats::coef(fit)[2]) - 2), 0.05)  # 1 / (1 - gamma)
})

test_that("group_shock gives the group-wave shock the stated share of error variance", {
  s <- simulate_influence(n_nodes = 30, n_groups = 300, beta = 0, gamma = 0, density = 0.1,
                          error_structure = "group_shock", error_rho = 0.6, seed = 7)
  n <- .so_nodes(s, 2)
  within <- mean(tapply(n$y, n$group, stats::var))
  between <- stats::var(tapply(n$y, n$group, mean)) - within / 30
  expect_lt(abs(between / (between + within) - 0.6), 0.08)
  expect_lt(abs(stats::var(n$y) - 1), 0.15)
  expect_error(simulate_influence(error_structure = "group_shock", error_rho = 1.5), "between 0 and 1")
})

test_that("error_structure = 'network' reaches the data as a SAR disturbance", {
  # Maximum likelihood over several independent networks, at gamma = beta = 0, where
  # the outcome is the disturbance. The generator's matrix is checked cell by cell in
  # test-simulate-orientation.R; this checks that it reaches the generated data.
  rho_hat <- function(s) {
    n <- netis_nodes(s$data)
    n <- n[n$wave == 2, ]
    t <- netis_ties(s$data)
    parts <- lapply(unique(n$group), function(g) {
      m <- n[n$group == g, ]
      w <- t[t$group == g, ]
      W <- matrix(0, nrow(m), nrow(m), dimnames = list(m$node, m$node))
      W[cbind(as.character(w$target), as.character(w$source))] <- 1
      list(y = m$y, W = W / ifelse(rowSums(W) > 0, rowSums(W), 1))
    })
    likelihood <- function(rho) {
      sum(vapply(parts, function(p) {
        A <- diag(length(p$y)) - rho * p$W
        as.numeric(determinant(A)$modulus) - sum((A %*% p$y)^2) / 2
      }, numeric(1)))
    }
    stats::optimize(likelihood, c(-0.95, 0.95), maximum = TRUE)$maximum
  }
  args <- list(n_nodes = 60, n_groups = 8, density = 0.08, beta = 0, gamma = 0,
               normalize = "row", seed = 8)
  dependent <- do.call(simulate_influence, c(args, list(error_structure = "network", error_rho = 0.6)))
  expect_lt(abs(rho_hat(dependent) - 0.6), 0.12)
  expect_lt(abs(rho_hat(do.call(simulate_influence, args))), 0.12)
  expect_error(simulate_influence(error_structure = "network", error_rho = 1), "invertible")
})

test_that("homophily tilts ties toward similar node effects and keeps the density", {
  gap <- function(h) {
    s <- simulate_influence(n_nodes = 200, density = 0.1, node_sd = 1, homophily = h,
                            beta = 0, seed = 9)
    t <- netis_ties(s$data)
    a <- stats::setNames(s$truth$node_effects, sub("^1:", "", names(s$truth$node_effects)))
    c(gap = mean(abs(a[as.character(t$source)] - a[as.character(t$target)])),
      density = nrow(t) / (200 * 199))
  }
  plain <- gap(0)
  tilted <- gap(3)
  # E|a_i - a_j| = 2 / sqrt(pi) for independent standard normals.
  expect_lt(abs(plain[["gap"]] - 2 / sqrt(pi)), 0.06)
  expect_lt(tilted[["gap"]], 0.5 * plain[["gap"]])
  expect_lt(abs(plain[["density"]] - 0.1), 0.006)
  expect_lt(abs(tilted[["density"]] - 0.1), 0.006)
})

test_that("node_x_cor and covariate_persistence deliver the stated covariate process", {
  s <- simulate_influence(n_nodes = 3000, n_waves = 3, density = 0.002, node_sd = 1, node_x_cor = 0.7,
                          covariate_persistence = 0.8, exposure_content = "covariate",
                          beta = 0, gamma = 0, normalize = "row", seed = 10)
  one <- .so_nodes(s, 1)
  two <- .so_nodes(s, 2)
  # x = level + AR(1), each of variance 1, so var(x) = 2, cor(x_t, x_t+1) = (1 + phi) / 2,
  # and the level's correlation with alpha is diluted by sqrt(2) in x itself.
  expect_lt(abs(stats::var(one$x) - 2), 0.15)
  expect_lt(abs(stats::cor(one$x, two$x) - 0.9), 0.02)
  expect_lt(abs(stats::cor(one$x, .so_alpha(s, one)) - 0.7 / sqrt(2)), 0.05)
  # Exposure content is the covariate: a node's y is not driven by alters at beta = 0,
  # and the outcome-only arm generates no x at all.
  outcome_arm <- simulate_influence(n_nodes = 10, density = 0.3, beta = 0.1, normalize = "row", seed = 1)
  expect_false("x" %in% names(netis_nodes(outcome_arm$data)))
  expect_error(simulate_influence(node_x_cor = 2), "between -1 and 1")
  expect_error(simulate_influence(covariate_persistence = 1), "|value| < 1", fixed = TRUE)
})
