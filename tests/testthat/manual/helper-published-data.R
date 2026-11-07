# Locating the published data, and the rnetis-side constructions the
# reproductions share. See README.md in this directory for where the files
# come from -- rnetis ships none of them.

.pub_root <- function() Sys.getenv("NETIS_PUBLISHED_DATA", "")

# A skip that TELLS THE READER WHAT TO DO. A bare skip("no data") in a
# directory whose entire purpose is external data would be read as a hold that
# someone forgot to lift.
.skip_without <- function(...) {
  root <- .pub_root()
  if (!nzchar(root)) {
    testthat::skip(paste0(
      "NETIS_PUBLISHED_DATA is not set. These reproduce published results on ",
      "their authors' own data, which rnetis does not ship. See ",
      "tests/testthat/manual/README.md for the two download locations."))
  }
  # c(...) first: file.path() would otherwise JOIN several names into one path.
  want <- file.path(root, c(...))
  missing <- want[!file.exists(want)]
  if (length(missing)) {
    testthat::skip(paste0(
      "Missing from NETIS_PUBLISHED_DATA: ", paste(basename(missing), collapse = ", "),
      ". See tests/testthat/manual/README.md."))
  }
  invisible(want)
}

# ---- Frank's toy data ------------------------------------------------------

.frank_toy <- function() {
  f <- .skip_without("Toyatt.csv", "Toynet.csv")
  list(att = utils::read.csv(f[1], stringsAsFactors = FALSE),
       net = utils::read.csv(f[2], stringsAsFactors = FALSE))
}

# Frank's own pipeline: exposure is the mean of yvar1 over the actors ego
# nominates, then lm(yvar2 ~ yvar1 + exposure_mean).
.frank_toy_reference <- function(d, yvar2 = d$att$attr2) {
  edges <- data.frame(nominator = d$net$sender, nominee = d$net$receiver,
                      relate = d$net$weight)
  alter <- data.frame(nominee = d$att$node, yvar1 = d$att$attr1)
  m <- merge(edges, alter, by = "nominee")
  m$exposure <- m$relate * m$yvar1
  ex <- stats::aggregate(exposure ~ nominator, data = m, FUN = mean)
  names(ex) <- c("node", "exposure_mean")
  final <- merge(ex, data.frame(node = d$att$node, yvar1 = d$att$attr1,
                                yvar2 = yvar2), by = "node")
  final <- final[order(final$node), ]
  list(final = final,
       model = stats::lm(yvar2 ~ yvar1 + exposure_mean, data = final))
}

# ORIENTATION, WHICH IS THE ONLY SUBTLE PART OF THE TRANSLATION. Frank's ego is
# the nominator and his alters are the actors ego nominates, so influence runs
# nominee -> nominator. Toynet.csv is (sender, receiver) = (nominator,
# nominee). rnetis's default direction makes ego the TARGET, so the tie table is
# declared source = nominee, target = nominator. That is not a fudge to make
# numbers line up: it states Frank's claim in rnetis's vocabulary.
.frank_toy_netis <- function(d, yvar2 = d$att$attr2) {
  ids <- as.character(d$att$node)
  nodes <- data.frame(
    group = 1L, node = rep(ids, 2L), wave = rep(1:2, each = length(ids)),
    y = c(d$att$attr1, yvar2), stringsAsFactors = FALSE)
  ties <- data.frame(
    group = 1L,
    source = as.character(d$net$receiver),
    target = as.character(d$net$sender),
    wave = 1L, tie = d$net$weight, stringsAsFactors = FALSE)
  rnetis::fit_influence(
    y ~ ar() + exposure(y, via = tie, normalize = "row", isolate_rule = "zero"),
    data = rnetis::netis_data(nodes, ties, wave_col = "wave"))
}

# ---- the SIENA s50 excerpt -------------------------------------------------

.S50_FILES <- c("s50-network1.dat", "s50-network2.dat", "s50-network3.dat",
                "s50-alcohol.dat", "s50-smoke.dat", "s50-drugs.dat",
                "s50-sport.dat")

.s50 <- function() {
  .skip_without(file.path("s50", .S50_FILES))
  rd <- function(f) as.matrix(utils::read.table(
    file.path(.pub_root(), "s50", f), header = FALSE))
  list(net = lapply(.S50_FILES[1:3], rd),
       a = rd("s50-alcohol.dat"), s = rd("s50-smoke.dat"),
       d = rd("s50-drugs.dat"), sp = rd("s50-sport.dat"))
}

# The stacked frame the published analysis regresses on: two transitions,
# 2->3 above 1->2, 100 rows.
.s50_reference_frame <- function(r) {
  E <- matrix(0, 50, 3)
  for (i in 1:50) for (k in 1:3) {
    if (sum(r$net[[k]][i, ]) != 0) {
      E[i, k] <- (r$net[[k]][i, ] %*% r$a[, k]) / sum(r$net[[k]][i, ])
    }
  }
  data.frame(alcohol = c(r$a[, 3], r$a[, 2]), lag_alc = c(r$a[, 2], r$a[, 1]),
             expo = c(E[, 2], E[, 1]), drug = c(r$d[, 3], r$d[, 2]),
             smoke = c(r$s[, 3], r$s[, 2]), sport = c(r$sp[, 3], r$sp[, 2]),
             actor = rep(1:50, 2))
}

# TWO ENCODINGS, BECAUSE rnetis REFUSES THE ONE THE PAPER USES -- and the
# refusal is correct. The published outcome model is a single pooled OLS across
# both transitions with no group term. Declared to rnetis as two groups, that
# fit is refused ("the fit would pool rows from unrelated networks into one
# intercept"), so:
#
#   "pooled"   one group of 100 actors (a01..a50, b01..b50) with ties only
#              inside each 50-block. Row for row this IS the published design
#              matrix, and it is what the deterministic tests use. No latent()
#              term goes near it.
#   "pergroup" two groups of 50, which is what latent() requires: a group is a
#              network, and stage one is fitted per network. Pooling the two
#              into one latent space would put actors from unconnected
#              networks in one geometry.
.s50_netis_data <- function(r, mode = c("pooled", "pergroup")) {
  mode <- match.arg(mode)
  ids <- sprintf("p%02d", 1:50)
  mk_nodes <- function(gid, prefix, k_lag, k_out) {
    nid <- paste0(prefix, ids)
    rbind(
      data.frame(group = gid, node = nid, wave = 1L, alcohol = r$a[, k_lag],
                 smoke = r$s[, k_lag], sport = r$sp[, k_lag], drug = r$d[, k_lag],
                 stringsAsFactors = FALSE),
      data.frame(group = gid, node = nid, wave = 2L, alcohol = r$a[, k_out],
                 smoke = r$s[, k_out], sport = r$sp[, k_out], drug = r$d[, k_out],
                 stringsAsFactors = FALSE))
  }
  # Same orientation argument as the toy data: row i of an s50 matrix is i's
  # outgoing nominations, and ego's exposure is over that row.
  mk_ties <- function(gid, prefix, m) {
    ij <- which(m == 1, arr.ind = TRUE)
    nid <- paste0(prefix, ids)
    data.frame(group = gid, source = nid[ij[, 2]], target = nid[ij[, 1]],
               wave = 1L, friend = 1, stringsAsFactors = FALSE)
  }
  if (mode == "pooled") {
    nodes <- rbind(mk_nodes("all", "a", 1L, 2L), mk_nodes("all", "b", 2L, 3L))
    ties  <- rbind(mk_ties("all", "a", r$net[[1]]), mk_ties("all", "b", r$net[[2]]))
  } else {
    nodes <- rbind(mk_nodes("A", "", 1L, 2L), mk_nodes("B", "", 2L, 3L))
    ties  <- rbind(mk_ties("A", "", r$net[[1]]), mk_ties("B", "", r$net[[2]]))
  }
  rnetis::netis_data(nodes, ties, wave_col = "wave",
                    tie_meta = list(friend = list(zero_meaning = "measured_zero")))
}

# rnetis coefficient name -> the name the published tutorial prints.
.S50_NAMES <- c("(Intercept)" = "(Intercept)", "alcohol_lag1" = "lag_alc",
                "ex_alcohol_friend" = "expo", "smoke" = "smoke",
                "sport" = "sport", "drug" = "drug")

.expect_matches_reference <- function(ref_model, fit, tolerance) {
  rc <- stats::coef(ref_model); nc <- stats::coef(fit)
  rse <- summary(ref_model)$coefficients[, 2]
  nse <- sqrt(diag(stats::vcov(fit)))
  keep <- names(nc)[names(nc) %in% names(.S50_NAMES)]
  ref_names <- unname(.S50_NAMES[keep])
  testthat::expect_equal(unname(nc[keep]), unname(rc[ref_names]), tolerance = tolerance)
  testthat::expect_equal(unname(nse[keep]), unname(rse[ref_names]), tolerance = tolerance)
}
