nodes2 <- data.frame(group = 1, node = rep(c("a", "b", "c"), 2),
                     wave  = rep(1:2, each = 3),
                     x     = c(10, 20, 30, 12, 19, 34))
ties2  <- data.frame(group = 1, source = c("b", "c", "b"), target = c("a", "a", "c"),
                     wave = c(1, 1, 2), w = c(1, 3, 2))

nd2 <- netis_data(nodes2, ties2, wave_col = "wave")
nd2
