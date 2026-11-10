# Declaring a network binary or symmetric in the DATA, then fitting it.
#
#   Rscript run-l.R <outdir>
#
# A6.5 shows binarize() and symmetrize() used inside a term, which is the
# right tool when you want the original network kept and a derived view used
# for one model. It is the wrong tool when the binary or undirected version IS
# your measurement -- then the conversion belongs in the tie table, where it
# happens once, is visible in the data, and every later call inherits it
# without a wrapper.
#
# This script builds the same contact network both ways and fits it, so the
# page can show the two routes landing in the same place.
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")

# The n-setup network, rebuilt so this script stands alone.
setup(R(
  'co_nodes <- data.frame(group = 1, node = rep(letters[1:6], 2),',
  '                       wave = rep(1:2, each = 6),',
  '                       y = c(2.1, 3.4, 1.8, 2.9, 3.1, 2.2,',
  '                             2.6, 3.1, 2.4, 3.3, 3.0, 2.7))',
  'co_ties <- data.frame(',
  '  group  = 1,',
  '  source = c("a", "b", "c", "d", "e", "a", "c", "f"),',
  '  target = c("b", "a", "a", "c", "d", "c", "b", "e"),',
  '  wave   = 1,',
  '  contact = c(1, 7, 2, 9, 4, 3, 6, 1)',
  ')'
))

cap("l-binary-data", R(
  '# The conversion done once, in the tie table. `met` is a binary variable',
  '# in its own right, declared as one, and every later call reads it',
  '# without a wrapper.',
  'co_ties$met <- as.numeric(co_ties$contact >= 3)',
  '',
  'binary <- netis_data(co_nodes, co_ties, wave_col = "wave",',
  '  tie_meta = list(met = list(weighted = FALSE,',
  '                             zero_meaning = "measured_zero")))',
  'binary'
))

cap("l-binary-fit", R(
  '# No binarize() anywhere: the network is already what the model needs.',
  'fit_influence(y ~ ar() + exposure(y, via = met), data = binary)'
))

cap("l-symmetric-data", R(
  '# An undirected measurement: each pair recorded once, and the object told',
  '# so. "6 nodes, 8 ties, undirected" is the object agreeing.',
  'pair  <- paste(pmin(co_ties$source, co_ties$target),',
  '               pmax(co_ties$source, co_ties$target))',
  'un_ties <- co_ties[!duplicated(pair), ]',
  '',
  'undirected <- netis_data(co_nodes, un_ties, wave_col = "wave",',
  '  directed = FALSE,',
  '  tie_meta = list(contact = list(weighted = TRUE,',
  '                                 weight_semantics = "count",',
  '                                 zero_meaning = "measured_zero")))',
  'undirected'
))

cap("l-symmetric-fit", R(
  '# No symmetrize() and no rule = : the relation is undirected, so there',
  '# are no two directions left to choose between.',
  'fit_influence(y ~ ar() + exposure(y, via = contact), data = undirected)'
))

cap("l-both", R(
  '# Both declarations at once, and the model reads as if the network had',
  '# always been this shape.',
  'un_ties$met <- as.numeric(un_ties$contact >= 3)',
  'plain <- netis_data(co_nodes, un_ties, wave_col = "wave",',
  '  directed = FALSE,',
  '  tie_meta = list(met = list(weighted = FALSE,',
  '                             zero_meaning = "measured_zero")))',
  '',
  'summary(fit_influence(y ~ ar() + exposure(y, via = met), data = plain))'
))
