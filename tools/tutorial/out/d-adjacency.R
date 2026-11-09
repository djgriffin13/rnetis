m <- matrix(c(0, 1, 0,
              1, 0, 1,
              0, 0, 0), nrow = 3, byrow = TRUE,
            dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
netis_data_from_adjacency(m)
