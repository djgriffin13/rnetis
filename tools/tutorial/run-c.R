# Blocks for the reference pages: derived networks, binarization rules,
# import and export.
#   Rscript run-c.R <outdir>
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")

# A small weighted network to transform. Contact counts, two waves.
cap("n-setup", R(
  'co_nodes <- data.frame(group = 1, node = rep(letters[1:6], 2),',
  '                       wave = rep(1:2, each = 6),',
  '                       y = c(2.1, 3.4, 1.8, 2.9, 3.1, 2.2,',
  '                             2.6, 3.1, 2.4, 3.3, 3.0, 2.7))',
  'co_ties <- data.frame(',
  '  group  = 1,',
  '  source = c("a", "b", "c", "d", "e", "a", "c", "f"),',
  '  target = c("b", "a", "a", "c", "d", "c", "b", "e"),',
  '  wave   = 1,',
  '  contact = c(1, 7, 2, 9, 4, 3, 6, 1),',
  '  advice  = c(1, 1, 0, 1, 0, 0, 1, 1)',
  ')',
  '',
  'co <- netis_data(co_nodes, co_ties, wave_col = "wave", tie_meta = list(',
  '  contact = list(weighted = TRUE, zero_meaning = "measured_zero",',
  '                 weight_semantics = "count"),',
  '  advice  = list(weighted = FALSE, zero_meaning = "measured_zero")',
  '))',
  'co'
))

cap("n-weighted-exposure", R(
  'compute_exposure(co, content = "y", via = "contact", wave = 1)'
))

cap("n-binarize", R(
  'compute_exposure(co, content = "y", via = binarize(contact, above_mean()),',
  '                 wave = 1)'
))

cap("n-binarize-threshold", R(
  'compute_exposure(co, content = "y", via = binarize(contact, threshold(5)),',
  '                 wave = 1)'
))

cap("n-binarize-rule-print", R(
  'above_mean()',
  'threshold(5)',
  'quantile_threshold(0.75)'
))

cap("n-binarize-custom", R(
  'strong <- custom_binarize_rule(function(x) x >= 6, name = "contact >= 6")',
  'compute_exposure(co, content = "y", via = binarize(contact, strong), wave = 1)'
))

cap("n-symmetrize", R(
  'compute_exposure(co, content = "y", via = symmetrize(contact, "max"), wave = 1)'
))

cap("n-symmetrize-refuse", R(
  'compute_exposure(co, content = "y", via = symmetrize(contact), wave = 1)'
), expect_error = TRUE)

cap("n-multiplex", R(
  'compute_exposure(co, content = "y",',
  '                 via = multiplex(binarize(contact, threshold(3)), advice,',
  '                                 combine = "intersection"),',
  '                 wave = 1)'
))

cap("n-transform-refuse", R(
  'strength_ties <- transform(co_ties, closeness = contact)',
  'st <- netis_data(co_nodes, strength_ties, wave_col = "wave", tie_meta = list(',
  '  closeness = list(weighted = TRUE, weight_semantics = "strength",',
  '                   zero_meaning = "measured_zero")',
  '))',
  'compute_centrality(st, network = "closeness", measure = "betweenness",',
  '                   weights = "weighted", wave = 1)'
), expect_error = TRUE)

cap("n-transform", R(
  'compute_centrality(st, network = "closeness", measure = "betweenness",',
  '                   weights = "weighted", weight_transform = inverse(),',
  '                   wave = 1)'
))

cap("n-centrality", R(
  'compute_centrality(co, network = "contact", measure = "strength",',
  '                   mode = "in", wave = 1)'
))

cap("n-network-stat", R(
  'compute_network_stat(co, network = "contact", measure = "density", wave = 1)',
  'compute_network_stat(co, network = "contact", measure = "reciprocity", wave = 1)'
))

# ---------------------------------------------------------------------------
# Import and export
# ---------------------------------------------------------------------------

cap("r-igraph-in", R(
  'g <- igraph::make_graph(~ a-+b, b-+c, c-+a, d-+a)',
  'netis_data_from_igraph(g)'
))

cap("r-igraph-out", R(
  'as_igraph(demo_influence, network = "tie", wave = 1)'
))

cap("r-adjacency-out", R(
  'netis_adjacency(co, "contact", wave = 1)'
))

cap("r-network-out", R(
  'as_network(demo_influence, network = "tie", wave = 1)'
))

cap("r-split", R(
  'who   <- data.frame(group = 1, node = letters[1:4], tenure = c(2, 5, 1, 7))',
  'what  <- data.frame(group = 1, node = rep(letters[1:4], 2),',
  '                    wave = rep(1:2, each = 4), y = c(1, 2, 3, 4, 2, 2, 4, 3))',
  'edges <- data.frame(group = 1, source = c("a", "b", "c"),',
  '                    target = c("b", "c", "d"), tie = 1)',
  '',
  'netis_data_from_split(node_static = who, node_dynamic = what,',
  '                      tie_static = edges, wave_col = "wave")'
))

.log("run-c done")
