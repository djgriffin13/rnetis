# Package attach hook (rnetis's message-quality standard; CRAN "Writing R
# Extensions" on namespace hooks: loading should be silent where possible, and
# startup text belongs in .onAttach() via packageStartupMessage() -- never
# cat()/message()/print() -- so suppressPackageStartupMessages() can silence
# it. No library()/require() here.

#' @noRd
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "rnetis ", as.character(utils::packageVersion("rnetis")),
    " -- Network Influence & Selection models\n",
    "Griffin Dynamics LLC. Research conducted in partnership with the\n",
    "T.A.C.T.I.C.S Lab, University of South Florida\n"
  )
}
