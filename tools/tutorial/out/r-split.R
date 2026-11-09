who   <- data.frame(group = 1, node = letters[1:4], tenure = c(2, 5, 1, 7))
what  <- data.frame(group = 1, node = rep(letters[1:4], 2),
                    wave = rep(1:2, each = 4), y = c(1, 2, 3, 4, 2, 2, 4, 3))
edges <- data.frame(group = 1, source = c("a", "b", "c"),
                    target = c("b", "c", "d"), tie = 1)

netis_data_from_split(node_static = who, node_dynamic = what,
                      tie_static = edges, wave_col = "wave")
