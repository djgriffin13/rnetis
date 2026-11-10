# What a failing posterior predictive check looks like.
#
#   Rscript run-j.R <outdir>
#
# WHY A SECOND DATASET. run-i.R ran netis_gof() on the tutorial's own
# Selection fit and the additive model passed: all five statistics inside
# their predictive ranges, every |z| under 0.9, and adding R = 2 moved
# nothing. That is the honest result for demo_selection and the page says so.
#
# But a reader who only ever sees a check pass has not learned to read it.
# They have learned that it prints five numbers. So this script builds a
# network with closure deliberately in it -- three dense blocks -- and fits
# the SAME additive model, so the contrast is the model's adequacy and not
# the model's specification.
#
# p = 0.85 within blocks rather than 1: a perfectly blocked network makes
# amen::ame() singular, because the additive effects have nothing to vary
# over, and the fixture would then fail for a reason unrelated to what it is
# showing.
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")
W <- 11.5

setup(R(
  'set.seed(11)',
  'n_block <- 5; n_blocks <- 3; n <- n_block * n_blocks',
  'ids <- sprintf("p%02d", seq_len(n))',
  'block <- rep(seq_len(n_blocks), each = n_block)',
  'cl_nodes <- data.frame(group = "1", node = rep(ids, 2),',
  '                       wave = rep(1:2, each = n),',
  '                       tenure = stats::rnorm(2 * n))',
  'pairs <- expand.grid(source = ids, target = ids, stringsAsFactors = FALSE)',
  'pairs <- pairs[pairs$source != pairs$target, ]',
  'same <- block[match(pairs$source, ids)] == block[match(pairs$target, ids)]',
  'cl_ties <- do.call(rbind, lapply(1:2, function(w)',
  '  data.frame(group = "1", pairs, wave = w,',
  '             tie = stats::rbinom(nrow(pairs), 1, ifelse(same, 0.85, 0.05)))))',
  'cliques <- netis_data(cl_nodes, cl_ties, wave_col = "wave",',
  '  tie_meta = list(tie = list(zero_meaning = "measured_zero")))'
))

.log("fitting the additive model to a clustered network")
setup(R(
  'cl_fit <- fit_selection(',
  '  tie ~ prior_tie() + sender(tenure) + receiver(tenure),',
  '  data = cliques, family = "bin", R = 0,',
  '  nscan = 4000, burn = 1000, odens = 10, seed = 1',
  ')'
))

cap("b3-gof-misfit", R(
  '# Three dense blocks, so the network is full of closed triads. The same',
  '# additive model as before -- sender and receiver effects, no latent',
  '# dimensions -- fitted to it.',
  'netis_gof(cl_fit)[, c("statistic", "observed", "predicted_lower",',
  '                      "predicted_upper", "ppp", "z")]'
))

capfig("b3-plot-gof-misfit", R('plot(cl_fit, what = "gof")'),
       width = W, height = 3.1)
