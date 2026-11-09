rough_nodes <- data.frame(group = 1, node = letters[1:8],
                          wave = rep(1:2, each = 8),
                          y = c(0.4, -1.2, 0.9, 0.1, -0.6, 1.4, 0.2, -0.3,
                                0.8, -0.9, 1.1, 0.5, -0.2, 1.8, NA, 0.1),
                          x = 1)
rough_ties <- data.frame(group = 1, source = c("a", "b", "c"),
                         target = c("b", "c", "d"), tie = 1)

rough <- netis_data(rough_nodes, rough_ties, wave_col = "wave")
diagnose_data(rough)
