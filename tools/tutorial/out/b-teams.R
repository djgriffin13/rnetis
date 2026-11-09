set.seed(42)
teams <- sprintf("t%02d", 1:12)

team_nodes <- do.call(rbind, lapply(teams, function(g) {
  data.frame(group = g, node = sprintf("%s_%d", g, 1:8),
             wave = rep(1:2, each = 8),
             extraversion = rep(stats::rnorm(8), 2))
}))

team_ties <- do.call(rbind, lapply(teams, function(g) {
  ids <- sprintf("%s_%d", g, 1:8)
  x <- team_nodes$extraversion[team_nodes$group == g & team_nodes$wave == 1]
  p <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)
  p <- p[p$source != p$target, ]
  eta <- stats::rnorm(1, -0.6, 0.4) +
         stats::rnorm(1, 0.4, 0.25) * x[match(p$source, ids)]
  do.call(rbind, lapply(1:2, function(w) {
    data.frame(group = g, p, wave = w,
               tie = stats::rbinom(nrow(p), 1, stats::pnorm(eta)))
  }))
}))

teams_data <- netis_data(team_nodes, team_ties, wave_col = "wave",
                         tie_meta = list(tie = list(zero_meaning = "measured_zero")))
teams_data
