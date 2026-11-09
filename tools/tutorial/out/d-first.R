nodes <- data.frame(group = 1, node = c("a", "b", "c"), x = c(10, 20, 30))
ties  <- data.frame(group = 1, source = c("b", "c"), target = c("a", "a"),
                    w = c(1, 3))

nd <- netis_data(nodes, ties)
nd
