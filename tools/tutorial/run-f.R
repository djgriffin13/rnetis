# Re-capture the convergence block from the chain run-d.R already fitted.
#
# The full convergence table carries a row per group coefficient and a row per
# nuisance parameter -- 77 rows for twelve groups. Printing all of it on the
# page would bury the five rows a reader actually checks first, so the block
# shows those and then summarises the rest by its worst R-hat.
#   Rscript run-f.R <outdir>
source("cap.R")

R <- function(...) paste(c(...), collapse = "\n")

rds <- file.path(OUT, "hier.rds")
if (!file.exists(rds)) stop("run run-d.R first: ", rds, " is missing")
assign("hier", readRDS(rds), envir = ENV)

cap("b-hier-conv", R(
  'conv <- netis_hierarchy(hier)$convergence',
  '',
  '# the rows you read first: the shared coefficient, the population means,',
  '# and the between-group standard deviations',
  'subset(conv, !family %in% c("group_coefficient", "nuisance"),',
  '       select = c(family, parameter, rhat, ess_bulk, ess_tail))'
))

cap("b-hier-conv-rest", R(
  '# and the rest, summarised rather than printed: 12 groups make hundreds of rows',
  'table(conv$family)',
  'max(conv$rhat)'
))

.log("run-f done")
