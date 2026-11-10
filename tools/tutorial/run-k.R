# Binary against weighted, and directed against symmetric, on Path B.
#
#   Rscript run-k.R <outdir>
#
# Path A demonstrates the same two distinctions with compute_exposure(), where
# the contrast is a column of numbers you can read off. On Path B the contrast
# is in the MODEL: whether the outcome is a tie or a value changes the family,
# and whether the relation is directed or symmetric changes how many additive
# effects each actor gets. Neither is visible in a data frame, so both need a
# fit to show.
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")

setup(R(
  'nodes <- netis_nodes(demo_selection)',
  'ties  <- netis_ties(demo_selection)'
))

# --- directed against symmetric ------------------------------------------
#
# An undirected relation stores each pair ONCE. Keeping both orderings and
# declaring the relation undirected would be a contradiction, so the fixture
# drops one ordering, which is also what real undirected data looks like.
setup(R(
  'un_ties <- ties[ties$source < ties$target, ]',
  'undirected <- netis_data(nodes, un_ties, wave_col = "wave",',
  '                         directed = FALSE,',
  '                         tie_meta = list(tie = list(',
  '                           zero_meaning = "measured_zero")))'
))

cap("k-undirected-refuse", R(
  '# The relation is declared undirected. Fitting it as a directed model',
  '# would read one tie as two observations.',
  'fit_selection(tie ~ sender(tenure) + prior_tie(), data = undirected,',
  '              nscan = 500, burn = 100, odens = 10, seed = 1)'
), expect_error = TRUE)

.log("fitting the symmetric model")
cap("k-symmetric", R(
  '# Same data, fitted as what it is. One additive effect per actor, not a',
  '# sender effect and a receiver effect.',
  'sym <- fit_selection(tie ~ sender(tenure) + prior_tie(),',
  '                     data = undirected, symmetric = TRUE,',
  '                     nscan = 3000, burn = 1000, odens = 10, seed = 1)',
  'netis_variance_components(sym)[, c("term", "estimate", "residual_variance_share")]'
))

# --- binary against weighted ----------------------------------------------
#
# A valued relation: how many times the pair interacted, rather than whether
# they did at all. Built from the binary tie so the two fits are about the
# same relation and differ only in what is being modelled.
setup(R(
  'set.seed(4)',
  'ties$contacts <- ifelse(ties$tie == 1,',
  '                        stats::rpois(nrow(ties), 6) + 1, 0)',
  'valued <- netis_data(nodes, ties, wave_col = "wave",',
  '                     tie_meta = list(',
  '                       tie      = list(zero_meaning = "measured_zero"),',
  '                       contacts = list(weighted = TRUE,',
  '                                       weight_semantics = "count",',
  '                                       zero_meaning = "measured_zero")))'
))

cap("k-binarize-refuse", R(
  '# binarize = is for turning a valued outcome INTO a tie. It has no',
  '# meaning when the values themselves are being modelled.',
  'fit_selection(contacts ~ sender(tenure), data = valued, family = "nrm",',
  '              binarize = nonzero(), nscan = 500, burn = 100, seed = 1)'
), expect_error = TRUE)

.log("fitting the valued model")
cap("k-valued", R(
  '# family = "nrm" models the counts themselves. The coefficients are on',
  '# the outcome\'s own scale -- contacts -- not on a latent probit scale.',
  'val <- fit_selection(contacts ~ sender(tenure) + receiver(tenure),',
  '                     waves = c(1, 2),',
  '                     data = valued, family = "nrm",',
  '                     nscan = 3000, burn = 1000, odens = 10, seed = 1)',
  'val'
))
