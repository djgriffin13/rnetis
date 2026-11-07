# Build the pkgdown site locally, then clean it.
#
#   Rscript tools/build-site.R
#
# USE THIS RATHER THAN pkgdown::build_site() DIRECTLY. pkgdown renders every
# .md file at the package root, including untracked local development notes,
# which are not public documents. A bare build_site() publishes them. The cleanup lives in tools/clean-site.R so that a
# local build and a deployed build run the same one and cannot diverge.
#
# ON WINDOWS THIS FAILS FROM THE WORKING CHECKOUT, and that is expected.
# pkgdown breaches PATH_MAX (260) when the package sits at a deep path. The error is:
#
#   Total path length must be less than PATH_MAX: 260
#
# raised from fs::path_tidy() before any page is written. The package is fine;
# the path is not. Build from a copy at a short path instead:
#
#   cp -r <pkg> /c/tmp/ns && cd /c/tmp/ns && Rscript tools/build-site.R
#
# Verified 2026-08 on pkgdown 2.2.x: fails from the checkout, succeeds from a
# short path, and tools/clean-site.R removes the local notes from the result.

# install = TRUE, deliberately. pkgdown renders vignettes against the INSTALLED
# package, so with install = FALSE a stale library silently fails the build with
# errors that look like package defects and are not -- observed here as
# "unused arguments (alpha = 1, gamma = 1)" from an installed copy six days
# behind HEAD, while R CMD check (which installs first) rebuilt the same
# vignette cleanly.
pkgdown::build_site(preview = FALSE, install = TRUE, new_process = FALSE)

source(file.path("tools", "clean-site.R"))
