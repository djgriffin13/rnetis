# Captures for the Installation page.
#   Rscript run-start.R <outdir>                                  (~1 s)
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")

cap("g-check", R(
  'library(rnetis)',
  'packageVersion("rnetis")'
))

cap("g-optional", R(
  'for (p in c("amen", "latentnet", "sna", "clubSandwich", "igraph", "network")) {',
  '  cat(sprintf("%-14s %s\\n", p,',
  '      if (requireNamespace(p, quietly = TRUE)) "installed" else "not installed"))',
  '}'
))
