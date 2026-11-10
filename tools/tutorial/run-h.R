# Figures of the six-person team the data page actually builds.
#
#   Rscript run-h.R <outdir>
#
# WHY THIS EXISTS. The data page used to show plot(demo_influence), a
# thirty-node circle of synthetic data, on a page whose whole argument is a
# six-person team typed out in front of the reader. Nothing in that picture
# could be pointed at: not Ada's four in-neighbours, not the two mutual dyads,
# not Fay, who has no ties at all and is the reason row-normalised exposure
# returns NA rather than 0. A figure you cannot point at is decoration.
#
# Six nodes is also the only size at which a reader can check the drawing
# against the tie table above it, which is the difference between a figure
# that illustrates and a figure that is merely present.
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")
W <- 11.5          # inches; matches run-e.R, so the renders are interchangeable

# The same two data frames as t-nodes / t-ties, rebuilt here because this
# script must stand alone: run-g.R captures them for display, and a figure
# that depended on another script's leftovers would silently draw stale data.
setup(R(
  'people <- data.frame(',
  '  group     = 1,',
  '  node      = c("Ada", "Ben", "Cyd", "Dev", "Eli", "Fay"),',
  '  wave      = rep(1:2, each = 6),',
  '  wellbeing = c(50, 10, 20, 30, 40, 60,',
  '                44, 14, 22, 31, 38, 58),',
  '  tenure    = c(8, 1, 3, 6, 2, 4,',
  '                8, 1, 3, 6, 2, 4)',
  ')',
  'advice <- data.frame(',
  '  group    = 1,',
  '  source   = c("Ben", "Cyd", "Dev", "Eli", "Ada", "Cyd", "Ben", "Dev", "Ada"),',
  '  target   = c("Ada", "Ada", "Ada", "Ada", "Ben", "Ben", "Cyd", "Eli", "Dev"),',
  '  strength = c(    1,     3,     2,     1,     2,     1,     1,     1,     1)',
  ')',
  'team <- netis_data(',
  '  node_data = people, tie_data = advice, wave_col = "wave",',
  '  tie_meta  = list(strength = list(',
  '    weighted = TRUE, weight_semantics = "strength",',
  '    zero_meaning = "measured_zero"))',
  ')'
))

# Height is generous relative to run-e.R's network panel: six nodes need less
# plot area but the same legend and caption, and ggsave clips rather than
# shrinks, so the fixed furniture sets the floor.
capfig("t-plot-network", R('plot(team, wave = 1)'), width = W, height = 7.0)
