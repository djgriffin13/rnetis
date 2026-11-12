# Re-render the two latent-coordinate figures only.
#   Rscript run-latent-figs.R <outdir>                            (~4 min)
#
# For when the PANEL changes and nothing else does. The two figures need their
# fits, which run-inf.R and run-sel.R make along the way; refitting just those
# two here avoids repeating half an hour of unrelated chains. The code is the
# code of `i7-fit` and `s7-fit`, read back from the capture directory, so the
# figures are drawn from the models the pages show.
source("cap.R")

setup(paste(readLines(file.path(OUT, "i3-people.R")), collapse = "\n"))
setup(paste(readLines(file.path(OUT, "i3-pairs.R")), collapse = "\n"))
setup(paste(readLines(file.path(OUT, "i3-wave2.R")), collapse = "\n"))
setup(paste(readLines(file.path(OUT, "i3-object.R")), collapse = "\n"))

.log("the Influence latent-space fit")
setup(paste(readLines(file.path(OUT, "i7-fit.R")), collapse = "\n"))
capfig("i7-latent", 'plot(fit_latent, what = "latent")', width = 8.5, height = 7.0)

.log("the Selection latent-factor fit")
setup(paste(readLines(file.path(OUT, "s7-fit.R")), collapse = "\n"))
capfig("s7-latent", 'plot(fit_latent, what = "latent")', width = 9.5, height = 8.0)
.log("done")
