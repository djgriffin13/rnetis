# Posterior predictive checks for Path B's fit sections.
#
#   Rscript run-i.R <outdir>
#
# THE ARGUMENT THESE CAPTURES CARRY. Page B3 fits an additive model -- sender
# and receiver effects, plus covariates -- and page B4 adds multiplicative
# effects. The reason to move from one to the other is not that someone says
# so; it is that the additive model generates networks with the right degree
# spread and the wrong amount of closure, and you can see it. netis_gof()
# reports the five network statistics amen simulates at every retained scan,
# and running it on both fits turns B4's motivation into evidence from B3.
#
# SAME FITS AS run-b.R AND run-e.R, deliberately, down to the seed: these
# captures sit beside the coefficient tables on the same pages, and a reader
# who sees a different chain behind two numbers on one page has been misled
# about which model produced which.
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")
W <- 11.5          # inches; matches run-e.R

# --- the staff dataset, as run-b.R builds it ------------------------------
setup(R(
  'nodes <- netis_nodes(demo_selection)',
  'ties  <- netis_ties(demo_selection)',
  'unit <- rep(c("u1", "u2"), length.out = nrow(nodes))',
  'names(unit) <- nodes$node',
  'ties$same_unit <- as.numeric(unit[ties$source] == unit[ties$target])',
  'staff <- netis_data(nodes, ties, wave_col = "wave", tie_meta = list(',
  '  tie       = list(zero_meaning = "measured_zero"),',
  '  same_unit = list(zero_meaning = "measured_zero")',
  '))'
))

.log("fitting the additive (R = 0) model")
setup(R(
  'sel <- fit_selection(',
  '  tie ~ sender(tenure) + receiver(tenure) + dyad(same_unit) + prior_tie(),',
  '  data = staff,',
  '  nscan = 5000, burn = 1000, odens = 10, seed = 1',
  ')'
))

.log("fitting the AME (R = 2) model")
setup(R(
  'ame2 <- fit_selection(',
  '  tie ~ sender(tenure) + receiver(tenure) + prior_tie(),',
  '  data = staff, R = 2,',
  '  nscan = 5000, burn = 1000, odens = 10, seed = 1',
  ')'
))

# --- B3.7: the additive model's posterior predictive check ----------------
cap("b3-gof", R(
  '# Five statistics of the observed network, each beside the range the',
  '# fitted model actually generates. Sender and receiver effects are what',
  '# explain unequal sending and unequal popularity; nothing additive',
  '# explains closure.',
  'netis_gof(sel)[, c("statistic", "observed", "predicted_lower",',
  '                   "predicted_upper", "ppp", "z")]'
))

# --- B4.7: the same five, after multiplicative effects --------------------
cap("b4-gof", R(
  'netis_gof(ame2)[, c("statistic", "observed", "predicted_lower",',
  '                    "predicted_upper", "ppp", "z")]'
))

cap("b4-gof-compare", R(
  '# The question B4 exists to answer, as one table: did the latent',
  '# dimensions move the triadic statistics toward the middle?',
  'a <- netis_gof(sel); m <- netis_gof(ame2)',
  'data.frame(',
  '  statistic = a$statistic,',
  '  z_additive = round(a$z, 2),',
  '  z_ame      = round(m$z, 2),',
  '  ppp_additive = a$ppp,',
  '  ppp_ame      = m$ppp',
  ')'
))

# --- the panels -----------------------------------------------------------
capfig("b3-plot-gof", R('plot(sel, what = "gof")'), width = W, height = 3.1)
capfig("b4-plot-gof", R('plot(ame2, what = "gof")'), width = W, height = 3.1)

# --- B3.6: the convergence table, as numbers rather than a panel ----------
#
# plot(what = "mcmc") draws these; the page needs to walk a reader through
# three unfamiliar quantities, and that is easier against a printed table
# than against a figure whose axis is the quantity being explained.
cap("b3-mcmc-table", R(
  'plot_data(sel, what = "mcmc")'
))
