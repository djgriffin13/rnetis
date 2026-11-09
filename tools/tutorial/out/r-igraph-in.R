g <- igraph::make_graph(~ a-+b, b-+c, c-+a, d-+a)
netis_data_from_igraph(g)
