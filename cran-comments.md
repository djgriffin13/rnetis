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

## Test environments

* Windows 11 Home (26200), **R 4.6.2 (2026-10-29 ucrt)**, x86_64-w64-mingw32,
  pandoc 3.8.3, **TinyTeX** — `R CMD check --as-cran --run-donttest`, local, run
  2026-11-07 23:09–23:24. The full optional `Suggests` set was installed: `amen`
  1.4.5, `clubSandwich` 0.7.0, `igraph` 2.3.3, `lame` 1.4.1, `latentnet`
  2.12.0, `network` 1.20.0, `numDeriv` 2016.8.1.1, `RSiena` 1.6.6, `sna` 2.8.

Measured on this commit's package tree. This commit also edits
`cran-comments.md`, which `.Rbuildignore` excludes, so the package built from it
is byte-for-byte the content that was checked.

Not run on any commit: R-devel, R-oldrel, Linux, macOS, win-builder, rhub. The
CI workflow exists but is `workflow_dispatch`-only. **R 4.1, the declared floor
in `DESCRIPTION`, has never been checked**, and until it is the supported range
is a claim rather than a result. Note that `RSiena` 1.6.6 declares
`R (>= 4.5.0)`, above that floor; it is needed only for the `s50` example data.

## R CMD check results

```
0 errors | 0 warnings | 2 notes | 0 test failures
```

Both notes are expected or environmental:

1. **CRAN incoming feasibility** — new submission; `License components with
   restrictions and base license permitting such: AGPL-3 + file LICENSE`, which
   prints `LICENSE` in full for human review; and `URL` / `BugReports`
   unreachable while the repository is private.
2. `V8` unavailable, so math rendering in the HTML manual was not checked.

### What this supersedes, and the defect it uncovered

Earlier runs recorded `1 error | 1 warning | 4 notes`, with the error and
warning attributed to this machine having no LaTeX. That attribution was right
about the error and **wrong to stop there**, which is why they were logged as
unresolved rather than written off. Installing TinyTeX cleared the error and
left the warning standing, now naming a cause:

```
! LaTeX Error: Unicode character ₁ (U+2081) not set up for use with LaTeX.
```

A real Rd defect: `R/pseudo-r2.R` wrote "Snijders–Bosker R²₁" with a literal
subscript-one character. The superscript two is Latin-1 and survives; the
subscript is not, and it stopped the PDF manual building. It is now
`\eqn{R^2_1}`, which renders in both HTML and PDF. A sweep of `R/`, `man/` and
`vignettes/` for the same class of character — subscripts, superscripts,
letterlike symbols, arrows, math operators, mathematical alphanumerics — found
no other instance.

Two further notes resolved on the way: `Non-standard file/directory found at top
level: 'LICENSE.md'` (the AGPL-3 text now ships from `inst/`), and the leftover
`rnetis-manual.tex` (a by-product of the failed PDF build, gone now that the
build succeeds).

### Reproducing the PDF stage

**TinyTeX alone is not sufficient.** Its minimal install lacks what R's
`Rd2pdf` needs, and the failures arrive one at a time with unhelpful messages —
at one point `R CMD check` reported `LaTeX errors found:` followed by nothing at
all. Beyond a TinyTeX base, install: `makeindex`, `nameref`, `oberdiek`,
`grffile`, `grfext`, `courier`. `R CMD Rd2pdf --no-clean` run directly is the
way to see the real error; the check's own summary hides it. The manual builds
at 634 KB once those are present.

## Test suite, all three tiers

`R CMD check` reports only that tests passed, which hides skips, so each tier was
run directly against the installed package.

| tier | tests | passed | failed | errors | skipped |
|---|---:|---:|---:|---:|---:|
| default (`tests/testthat`) | 986 | 3613 | 0 | 0 | **0** |
| `comprehensive/` | 300 | 860 | 0 | 0 | **0** |
| `manual/` (published reproductions) | 7 | 20 | **1** at REPS=5, **0** at REPS=20 | 0 | **0** |
| total | 1293 | 4493 | 1 | 0 | **0** |

**Zero skips across all three tiers**, which is what the full `Suggests` set
buys: without it roughly seventy blocks skip and still report green.

The `manual/` tier ran for the first time, against data fetched per
`tests/testthat/manual/README.md` (Frank's `Toyatt.csv` / `Toynet.csv`; the
SIENA `s50_data.zip`), with `NETIS_MANUAL_MCMC=1` so the latent reproduction ran
rather than being skipped.

### Every deterministic reproduction passes

* Frank's toy data: the exposure vector matches `mean_exposure` value for value;
  the coefficients and standard errors are his; both readings of his disagreeing
  materials reproduce. Asserted at floating-point tolerance, not "close enough".
* Xu's s50: the three unadjusted models reproduce exactly; his published
  exposure estimate comes back to his printed precision; rnetis refuses the
  pooled fit the published analysis performs, as designed.

### The one failure, and what it is

`test-reproduce-xu-s50.R:161` — "the latent adjustment agrees with the reference
within its own noise":

```
Expected abs(mean(got) - mean(ref)) < stats::sd(ref)
Actual:  0.0070 >= 0.0056
```

This is the **stochastic** comparison, on the **experimental multi-group
`latent()`** path, which warns on every use and is documented as not validated
for statistical performance. The test emitted that warning five times during the
run. Three things about the number:

1. **The stronger assertion in the same test passed.** rnetis's mean exposure
   estimate is within two of its own standard deviations of Xu's published
   0.146127. Agreement with the *published* figure holds; what tripped is
   agreement with the reference implementation rerun in-session.
2. **The threshold is mis-scaled, and that matters.** The statistic is a
   difference of means over `REPS = 5` draws, whose standard error is about
   `sd * sqrt(2/REPS)` = 0.63 `sd`. Comparing it against `sd(ref)` itself tests
   at roughly 1.6 SE, so **this assertion fails about one run in nine even if
   rnetis is perfectly unbiased.** A principled two-SE threshold would be
   0.0071, and the observed 0.0070 would have passed — by a hair.
3. **So one run cannot settle it.** The observed gap is about 2.0 SE of the
   difference: either a 1-in-20 draw, or a small real difference in stage-one
   positions. Distinguishing them needs more replications
   (`NETIS_MANUAL_MCMC_REPS`), which is a longer run than this one.

### Resolved: it was noise

The question was settled by re-running the tier with more replications, as point
3 said it needed. At **`NETIS_MANUAL_MCMC_REPS = 20`**, against the same
installed package and the same data, 2026-11-07 20:13–21:49 (96 minutes):

```
tests=7  passed=36  failed=0  errors=0  skipped=0
```

**No failures.** Quadrupling the replications halves the standard error of the
difference and turns the same `sd(ref)` threshold into roughly a 3.2 SE test, so
a real bias of the observed size would have shown more clearly, not less. It
did not. The REPS = 5 failure was the one-run-in-nine the mis-scaled threshold
admits, and there is no detectable disagreement between rnetis's stage-one
positions and the reference implementation's.

Two things this does **not** establish, since they were never in question here:
multi-group `latent()` remains this package's own construction and remains
unvalidated for statistical performance — agreeing with a reference
implementation is not the same as having known operating characteristics — and
it still warns on every use. The warning stands on its own grounds.

The threshold itself is still mis-scaled and will still fail about one run in
nine at the default REPS = 5. That is a property of the test, not of the
package, and it is recorded here rather than silently loosened.

Also observed, from the test's own `(1 | group)` design with two groups:
`lme4` "boundary (singular) fit" five times. Expected at two groups; noted so it
is not mistaken later for a new symptom.

## Reproducing the check

Vignette building needs pandoc, and this machine has **no standalone pandoc and
none on `PATH`** — only the copy bundled with RStudio. A bare `Rscript` or shell
run of `R CMD build` therefore fails on all four vignettes. Set:

```r
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
```

or install pandoc properly. This is a real reproducibility gap and the reason the
first attempt at this run failed.

TinyTeX installs into the roaming profile and is **not** on `PATH` either, so a
check that reaches the PDF stage also needs its `bin/windows` prepended. Both
settings, and the extra LaTeX packages listed above, are applied by
`../rnetis-dist-src/recheck-with-latex.R`, which is the script these figures
came from.

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
