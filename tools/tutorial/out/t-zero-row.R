# Three people, and every ordered pair asked about. Two pairs have a
# relation; the other four were asked and answered no. The zeros are
# data, and they are in the table.
who  <- data.frame(group = 1, node = c("a", "b", "c"))
full <- data.frame(group = 1,
                   source = c("a", "a", "b", "b", "c", "c"),
                   target = c("b", "c", "a", "c", "a", "b"),
                   r      = c(  1,   0,   1,   0,   0,   0))
full
