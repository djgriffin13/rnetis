# cran-comments

**Not yet submitted.** Version 0.1.0 is a first release, distributed by the
Company at its discretion rather than through CRAN. This file records the check
environment and results so the state of the package is auditable now, and so
that a submission is a matter of running the remaining platforms rather than of
discovering what was never checked.

**The licence is now `AGPL-3 + file LICENSE`**: AGPL-3 with additional terms
under its section 7. The declaration is valid — the check confirms AGPL-3 as a
"base license permitting such" — but **CRAN scrutinises additional restrictions**
and prints the whole `LICENSE` file for human review, as the note below shows.
Clearing those terms with CRAN would be a licence decision, not a packaging one.

## Test environments and results

**Pending re-measurement at this commit.** The previous figures were measured at
a commit that no longer exists: this branch's local history was rebuilt, so the
SHA they named does not resolve and they are not carried forward. A figure
without a resolvable commit is not evidence, which is why they are removed
rather than left in place.

The run is repeated against this commit and recorded in the commit that follows
it, since a commit cannot carry its own SHA.

Superseded and not carried forward: an earlier run on R 4.5.1 recorded
`0 errors | 0 warnings | 1 note` with no commit SHA at all. That R version is no
longer installed here and the figure was never reproducible.

Not yet run on any commit: R-devel, R-oldrel, Linux, macOS, win-builder, or
rhub. The CI workflow exists but is `workflow_dispatch`-only. **Multi-platform
checks are the first thing a real submission needs**, and their absence is the
reason this file does not claim readiness. **R 4.1, the declared floor in
`DESCRIPTION`, has never been checked.**

## Reproducing the check

Vignette building needs pandoc, and this machine has **no standalone pandoc and
none on `PATH`** — only the copy bundled with RStudio. A bare `Rscript` or shell
run of `R CMD build` therefore fails on all four vignettes. Set:

```r
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
```

or install pandoc properly. This is a real reproducibility gap and the reason the
first attempt at this run failed.

## Dependencies

No reverse dependencies: this is a first release.

`Imports` is four packages: `generics`, `ggplot2`, `lme4`, `reformulas`.

Everything optional is in `Suggests` and guarded by
`requireNamespace(..., quietly = TRUE)`, including a dependency's own
transitive `Suggests` where a documented capability needs one. The Selection
model (`amen`), the latent-space adjustment (`latentnet`), the
network-autocorrelation estimator (`sna`, with `numDeriv` for its information
matrix), and cluster-robust standard errors (`clubSandwich`) are all in this
category: the package installs and its core Influence workflow runs without any
of them, and asks for the one it needs when you reach for that capability.

## Examples and tests

No internet access in any example, test, or vignette. No writes outside
`tempdir()`. Tests are seeded and deterministic.

The two slowest capabilities — `amen::ame()` Selection fits and
`latentnet::ergmm()` latent-space fits — are Markov chains, and a chain long
enough to be worth trusting is longer than a vignette build should take. The
`selection` and `latent-space` vignettes therefore show their model fits with
`eval = FALSE` and say so in the first paragraph, rather than shipping a chain
too short to mean anything. Their data-building code and refusals run at build
time. Correctness for both models is covered in the test suite, not in the
vignettes.

## On what this package claims

`rnetis` implements, integrates, and operationalizes published methods. Every
published method is cited on the help page of the function that computes or fits
it (a formula term or option that only feeds such a function is covered on that
function's page), and where an implementation departs from its source the
departure is named at the function.

The package's own constructions are listed in `?rnetis-package` and declared on
their help pages. The main one is the hierarchical multi-group Selection model.
Multi-group `latent()` is experimental and warns when used. Two quantities reported by `pseudo_r2()` for Selection fits are also its own,
because no published work establishes how to partition an AME latent index
containing a bilinear term; they are labeled as members of the standard
pseudo-R-squared family rather than under any author's name.
