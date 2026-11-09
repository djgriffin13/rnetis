# Captures added in the second pass, from the maintainer's review notes.
#
# The theme of the notes was that the tutorial leaned on the packaged demo
# datasets before the reader knew what a netis_data was made of, and that the
# Influence path reported no fit statistics. So this script builds one small
# dataset by hand from two plain data frames and carries it through the
# teaching pages, then captures the fit-statistic surfaces (which do exist:
# r.squared, sigma, rmse, logLik, AIC, BIC) and the refusals that bound the
# scope to one transition.
#
#   Rscript run-g.R <outdir>
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")

# ===========================================================================
# One hand-built dataset: six people, an advice network, wellbeing twice.
#
# The shape is chosen to make four separate lessons visible in ONE object:
#   * Ada has four in-neighbours, so she is a usable ego for an exposure figure
#   * Ada<->Ben and Ben<->Cyd are mutual, so reciprocity has something to show
#   * Fay has no ties at all, so row-normalized exposure has no average to take
#   * tenure never varies by wave and wellbeing does, so rnetis has to sort
#     static from dynamic rather than being told
# ===========================================================================
cap("t-nodes", R(
  '# One row per person per occasion. "node" names the person; everything',
  '# else is a variable measured on them.',
  'people <- data.frame(',
  '  group     = 1,',
  '  node      = c("Ada", "Ben", "Cyd", "Dev", "Eli", "Fay"),',
  '  wave      = rep(1:2, each = 6),',
  '  wellbeing = c(50, 10, 20, 30, 40, 60,    # wave 1',
  '                44, 14, 22, 31, 38, 58),   # wave 2',
  '  tenure    = c(8, 1, 3, 6, 2, 4,          # years; does not change',
  '                8, 1, 3, 6, 2, 4)',
  ')',
  'people'
))

cap("t-ties", R(
  '# One row per tie. "source" is the person the relation comes FROM and',
  '# "target" the person it goes TO: this row says Ben gives advice to Ada.',
  'advice <- data.frame(',
  '  group    = 1,',
  '  source   = c("Ben", "Cyd", "Dev", "Eli", "Ada", "Cyd", "Ben", "Dev", "Ada"),',
  '  target   = c("Ada", "Ada", "Ada", "Ada", "Ben", "Ben", "Cyd", "Eli", "Dev"),',
  '  strength = c(    1,     3,     2,     1,     2,     1,     1,     1,     1)',
  ')',
  'advice'
))

cap("t-build", R(
  '# Two data frames in, one object out. `wave_col` names the occasion',
  '# column; `tie_meta` is where you say what a tie value means.',
  'team <- netis_data(',
  '  node_data = people,',
  '  tie_data  = advice,',
  '  wave_col  = "wave",',
  '  tie_meta  = list(strength = list(',
  '    weighted         = TRUE,',
  '    weight_semantics = "strength",',
  '    zero_meaning     = "measured_zero"))',
  ')',
  'team'
))

cap("t-back", R(
  '# and straight back out again, unchanged',
  'netis_nodes(team)[1:3, ]',
  'netis_ties(team)[1:3, ]'
))

cap("t-static-dynamic", R(
  '# rnetis worked out which variables move with the wave and which do not.',
  '# `advice` has no wave column at all, so the network is one static',
  '# structure observed across both occasions.',
  'list(node_variables = netis_schema(team)$node_vars,',
  '     tie_variables  = netis_schema(team)$tie_vars)'
))

cap("t-adjacency", R(
  '# The same network as a matrix. Rows are senders, columns receivers --',
  '# the same orientation the source and target columns went in as.',
  'netis_adjacency(team, "strength", wave = 1)'
))

cap("t-boundary", R(
  'netis_boundary(team)'
))

# ---------------------------------------------------------------------------
# Direction, reciprocity, and the zero/NA distinction
# ---------------------------------------------------------------------------
cap("t-reciprocity", R(
  '# Of the nine ties, how many are matched by one going the other way?',
  'compute_network_stat(team, "strength", "reciprocity", wave = 1)',
  '',
  '# Ada -> Ben and Ben -> Ada are both present, so that dyad is mutual.',
  '# Cyd -> Ada is not matched by Ada -> Cyd, so that one is asymmetric.',
  'A <- netis_adjacency(team, "strength", wave = 1)',
  'A["Ada", "Ben"]; A["Ben", "Ada"]',
  'A["Cyd", "Ada"]; A["Ada", "Cyd"]'
))

cap("t-symmetrize-refuse", R(
  '# Collapsing the two directions into one is several different operations',
  '# on a weighted network, so rnetis will not pick one.',
  'compute_exposure(team, content = "wellbeing",',
  '                 via = symmetrize("strength"), wave = 1)'
), expect_error = TRUE)

cap("t-exposure-sum", R(
  'compute_exposure(team, content = "wellbeing", via = "strength", wave = 1)'
))

cap("t-exposure-row", R(
  'compute_exposure(team, content = "wellbeing", via = "strength",',
  '                 normalize = "row", wave = 1)'
))

cap("t-exposure-zero", R(
  '# "zero" says an isolate belongs at zero on the scale. Here wellbeing',
  '# runs from 10 to 60, so zero is not a neutral value -- it is off the',
  '# bottom of the observed range, and Fay would be the most extreme case',
  '# in the dataset purely because nobody advises her.',
  'compute_exposure(team, content = "wellbeing", via = "strength",',
  '                 normalize = "row", isolate_rule = "zero", wave = 1)'
))

cap("t-exposure-error", R(
  'compute_exposure(team, content = "wellbeing", via = "strength",',
  '                 normalize = "row", isolate_rule = "error", wave = 1)'
), expect_error = TRUE)

cap("t-exposure-direction", R(
  '# The default reads a stored tie as "source influences target". Reversing',
  '# it asks a different question of the same data: not what reaches Ada',
  '# through the people who advise her, but what reaches her through the',
  '# people she advises.',
  'compute_exposure(team, content = "wellbeing", via = "strength",',
  '                 direction = "target_influences_source", wave = 1)'
))

# ---------------------------------------------------------------------------
# Why a missing row is not a zero
# ---------------------------------------------------------------------------
cap("t-zero-row", R(
  '# Three people, and every ordered pair asked about. Two pairs have a',
  '# relation; the other four were asked and answered no. The zeros are',
  '# data, and they are in the table.',
  'who  <- data.frame(group = 1, node = c("a", "b", "c"))',
  'full <- data.frame(group = 1,',
  '                   source = c("a", "a", "b", "b", "c", "c"),',
  '                   target = c("b", "c", "a", "c", "a", "b"),',
  '                   r      = c(  1,   0,   1,   0,   0,   0))',
  'full'
))

cap("t-presence-refuse", R(
  '# Saying every pair was observed is not enough on its own: rnetis still',
  '# needs to know which of those rows counts as a tie. The object builds,',
  '# and the question is put to you the moment something needs the answer.',
  'cd <- netis_data(who, full,',
  '                 tie_meta = list(r = list(presence_mode = "complete_dyadic")))',
  'compute_network_stat(cd, "r", "density")'
), expect_error = TRUE)

cap("t-presence-contrast", R(
  '# The same six rows under the two readings. "row_presence" means a row',
  '# IS a tie, so the four zeros are counted as ties and the network looks',
  '# complete. "value_rule" says a zero row is an observed non-tie.',
  'rows <- netis_data(who, full,',
  '  tie_meta = list(r = list(presence_mode = "row_presence")))',
  'vals <- netis_data(who, full,',
  '  tie_meta = list(r = list(presence_mode = "value_rule",',
  '                           presence_rule = function(value) value != 0)))',
  '',
  'compute_network_stat(rows, "r", "density")',
  'compute_network_stat(vals, "r", "density")'
))

cap("t-missing-row", R(
  '# And now the third case: the pair is not in the table at all. Under',
  '# "row_presence" that is simply not a tie. Under "complete_dyadic" the',
  '# claim is that it WAS observed, so an absent pair is a measured zero.',
  '# Neither of those is "we do not know", which is what a genuinely',
  '# unobserved pair is -- and that is why the declaration is yours.',
  'thin <- full[full$r == 1, ]',
  'thin',
  '',
  'compute_centrality(',
  '  netis_data(who, thin,',
  '    tie_meta = list(r = list(presence_mode = "row_presence"))),',
  '  "r", "degree", mode = "in")'
))

# ===========================================================================
# Path A: the lm/lmer parallel
# ===========================================================================
cap("a2-lm-parallel", R(
  '# rnetis builds a design matrix and hands it to stats::lm. Doing the same',
  '# thing by hand gives the same numbers -- which is the point: `ar()` and',
  '# `exposure()` are shorthand for columns, not a different estimator.',
  'ex <- compute_exposure(demo_influence, content = "y", via = "tie",',
  '                       normalize = "row", wave = 1)',
  'nd <- netis_nodes(demo_influence)',
  '',
  'panel <- data.frame(',
  '  y_now  = nd$y[nd$wave == 2],',
  '  y_prev = nd$y[nd$wave == 1],',
  '  ex     = ex$ex_y_tie[match(nd$node[nd$wave == 2], ex$node)]',
  ')',
  '',
  'round(coef(lm(y_now ~ y_prev + ex, data = panel)), 6)'
))

cap("a2-lm-parallel-rnetis", R(
  'fit <- fit_influence(y ~ ar() + exposure(y, via = tie, normalize = "row"),',
  '                     data = demo_influence)',
  'round(coef(fit), 6)'
))

# ---------------------------------------------------------------------------
# Path A: fit statistics
# ---------------------------------------------------------------------------
cap("a2-glance", R(
  '# One row per fit, with the fit statistics in it',
  'as.data.frame(netis_glance(fit))'
))

cap("a2-ic", R(
  'logLik(fit)',
  'AIC(fit)',
  'BIC(fit)'
))

cap("a2-compare-ic", R(
  '# Information criteria compare models fitted to THE SAME rows. These two',
  '# are, so the comparison is admissible.',
  'plain <- fit_influence(y ~ ar(), data = demo_influence)',
  'withx <- fit_influence(y ~ ar() + exposure(y, via = tie, normalize = "row"),',
  '                       data = demo_influence)',
  '',
  'data.frame(',
  '  model = c("ar() only", "ar() + exposure()"),',
  '  nobs  = c(netis_glance(plain)$nobs, netis_glance(withx)$nobs),',
  '  r2    = round(c(netis_glance(plain)$r.squared, netis_glance(withx)$r.squared), 4),',
  '  rmse  = round(c(netis_glance(plain)$rmse, netis_glance(withx)$rmse), 4),',
  '  AIC   = round(c(AIC(plain), AIC(withx)), 2),',
  '  BIC   = round(c(BIC(plain), BIC(withx)), 2)',
  ')'
))

cap("a2-pseudo", R(
  'pseudo_r2(fit)'
))

# ===========================================================================
# Path B: build the data by hand, including the pair attribute
# ===========================================================================
cap("b2-raw", R(
  '# demo_selection is two plain data frames, like any other netis_data.',
  '# The node table is static -- tenure does not change -- so it has no',
  '# wave column at all.',
  'head(netis_nodes(demo_selection), 3)',
  'head(netis_ties(demo_selection), 4)'
))

cap("b2-shape", R(
  '# 20 nodes make 380 ordered pairs, and both waves were observed, so the',
  '# tie table has a row for every pair at every wave -- zeros included.',
  'nrow(netis_nodes(demo_selection))',
  'nrow(netis_ties(demo_selection))',
  'table(netis_ties(demo_selection)$wave)',
  'table(netis_ties(demo_selection)$tie)'
))

cap("b2-pair", R(
  '# A pair attribute is a TIE variable: one value per ordered pair, not a',
  '# property of either person on their own. Here, whether the two are in',
  '# the same unit of the organisation.',
  'nodes <- netis_nodes(demo_selection)',
  'ties  <- netis_ties(demo_selection)',
  '',
  'unit <- rep(c("u1", "u2"), length.out = nrow(nodes))',
  'names(unit) <- nodes$node',
  'ties$same_unit <- as.numeric(unit[ties$source] == unit[ties$target])',
  '',
  'head(ties[, c("source", "target", "wave", "tie", "same_unit")], 4)'
))

cap("b2-build", R(
  'staff <- netis_data(',
  '  node_data = nodes,',
  '  tie_data  = ties,',
  '  wave_col  = "wave",',
  '  tie_meta  = list(',
  '    tie       = list(zero_meaning = "measured_zero"),',
  '    same_unit = list(zero_meaning = "measured_zero"))',
  ')',
  'staff'
))

cap("b2-igraph-refuse", R(
  '# An igraph graph has edges, so exporting one means deciding which rows',
  '# of a complete-dyadic table are edges. The declaration we made says a',
  '# zero is a measured absence; it does not yet say a zero is not an edge.',
  'as_igraph(staff, "tie", wave = 2)'
), expect_error = TRUE)

cap("b2-igraph", R(
  '# Say it, and the round trip works. This is the same declaration the',
  '# fits on the following pages rely on.',
  'staff <- netis_data(',
  '  node_data = nodes,',
  '  tie_data  = ties,',
  '  wave_col  = "wave",',
  '  tie_meta  = list(',
  '    tie = list(zero_meaning  = "measured_zero",',
  '               presence_mode = "value_rule",',
  '               presence_rule = function(value) value != 0),',
  '    same_unit = list(zero_meaning = "measured_zero"))',
  ')',
  '',
  'g <- as_igraph(staff, "tie", wave = 2)',
  'g',
  '',
  'netis_data_from_igraph(g)'
))

# ---------------------------------------------------------------------------
# Path B: why one transition
# ---------------------------------------------------------------------------
cap("b2-waves3-refuse", R(
  '# Three occasions of the same relation. A fit models one transition, and',
  '# will not pick which.',
  'three <- netis_data(',
  '  nodes,',
  '  rbind(ties, transform(ties[ties$wave == 2, ], wave = 3)),',
  '  wave_col = "wave",',
  '  tie_meta = list(tie = list(zero_meaning = "measured_zero"),',
  '                  same_unit = list(zero_meaning = "measured_zero")))',
  '',
  'fit_selection(tie ~ sender(tenure) + prior_tie(), data = three,',
  '              nscan = 500, burn = 100, odens = 10, seed = 1)'
), expect_error = TRUE)

cap("b2-waves3-named-refuse", R(
  '# Naming the transition is how you proceed -- but here waves 2 and 3 were',
  '# built as copies of each other, and a lagged relation identical to the',
  '# response is not a predictor.',
  'fit_selection(tie ~ sender(tenure) + prior_tie(), data = three,',
  '              waves = c(2, 3), nscan = 500, burn = 100, odens = 10, seed = 1)'
), expect_error = TRUE)

cap("b2-nologlik-refuse", R(
  'sel <- fit_selection(tie ~ sender(tenure) + receiver(tenure) +',
  '                       dyad(same_unit) + prior_tie(),',
  '                     data = staff, nscan = 5000, burn = 1000,',
  '                     odens = 10, seed = 1)',
  'logLik(sel)'
), expect_error = TRUE)

cap("b2-glance", R(
  '# What a Selection fit reports in place of an information criterion',
  'as.data.frame(netis_glance(sel))'
))

cap("b2-tuning", R(
  '# The chain settings, all in one place. These are the ones used on this',
  '# page; none of them is a default to accept without looking.',
  'sel <- fit_selection(',
  '  tie ~ sender(tenure) + receiver(tenure) + dyad(same_unit) + prior_tie(),',
  '  data   = staff,',
  '  nscan  = 5000,   # draws kept after burn-in',
  '  burn   = 1000,   # draws discarded first',
  '  odens  = 10,     # thinning: keep one draw in ten',
  '  R      = 0,      # dimension of the multiplicative term',
  '  seed   = 1',
  ')',
  'sel'
))

.log("run-g done")
