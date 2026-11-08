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
  pandoc 3.8.3, no LaTeX — `R CMD check --as-cran --run-donttest`, local, at
  commit **`0c07c6b768e65eeaead692e84b71a918848b30f0`**, run 2026-11-07
  17:24–17:56. The full optional `Suggests` set was installed: `amen` 1.4.5,
  `clubSandwich` 0.7.0, `igraph` 2.3.3, `lame` 1.4.1, `latentnet` 2.12.0,
  `network` 1.20.0, `numDeriv` 2016.8.1.1, `RSiena` 1.6.6, `sna` 2.8.

Not run on any commit: R-devel, R-oldrel, Linux, macOS, win-builder, rhub. The
CI workflow exists but is `workflow_dispatch`-only. **R 4.1, the declared floor
in `DESCRIPTION`, has never been checked**, and until it is the supported range
is a claim rather than a result.

Earlier figures from an R 4.5.1 run carried no commit SHA and are not carried
forward; that R version is no longer installed here.

Changed after the measured run: prose in `README.md` (a note that `RSiena` 1.6.6
declares `R (>= 4.5.0)`, above this package's floor; a reporting section with the
no-real-data rule; a citation section) and a new **`inst/CITATION`**.

`inst/CITATION` is the one of those that `R CMD check` would exercise, since it
is R code evaluated by `citation()` and a malformed one is a check WARNING. It
was validated directly with `utils::readCitationFile()` against `DESCRIPTION`'s
metadata — it parses, yields one entry, and renders its header and footer — but
**it is not covered by the check figures below**, which were measured before it
existed. Nothing else changed in code or metadata.

What a dependency scan found, since R 4.1 itself remains untested: of the four
`Imports`, `ggplot2` 4.0.3 requires R >= 4.1 and sets the floor exactly, while
`generics`, `lme4` and `reformulas` go lower. Among `Suggests`, **`RSiena` 1.6.6
requires R >= 4.5.0** — needed only for the `s50` example data, not by
`latent()` — and every other optional package is satisfied at 4.1 or below. The
package's own code uses no base feature newer than 4.1: no `sort_by()`, no
`grepv()`, no `Sys.setLanguage()`, no `_` pipe placeholder, and its 95 uses of
`%||%` resolve to a local definition at `R/utils-assert.R:318`, not to the base
operator added in R 4.4. A scan is weaker evidence than a run on 4.1, and does
not replace it.

## R CMD check results

```
1 error | 1 warning | 3 notes | 0 test failures
```

**The error and the warning are one environment gap, not a package defect.**
There is no `pdflatex` here, so "checking PDF version of manual without index"
fails with `pdflatex is not available`, and the warning is the same failure
restated as "LaTeX errors when creating PDF version. This typically indicates Rd
problems." That stock wording is misleading in this case — but it also means the
**Rd sources are not cleared for PDF rendering**, and that stays unresolved
until a LaTeX install is available rather than being written off.

The three notes:

1. **CRAN incoming feasibility** — new submission; `License components with
   restrictions and base license permitting such: AGPL-3 + file LICENSE`, which
   prints `LICENSE` in full for human review; and `URL` / `BugReports`
   unreachable while the repository is private.
2. `V8` unavailable, so math rendering in the HTML manual was not checked.
3. A leftover `rnetis-manual.tex`, a by-product of the failed PDF build.

The fourth note of the previous run — `Non-standard file/directory found at top
level: 'LICENSE.md'` — is **resolved**: the AGPL-3 text now ships from
`inst/LICENSE-AGPL-3.md`.

## Test suite, all three tiers

`R CMD check` reports only that tests passed, which hides skips, so each tier was
run directly against the installed package.

| tier | tests | passed | failed | errors | skipped |
|---|---:|---:|---:|---:|---:|
| default (`tests/testthat`) | 986 | 3613 | 0 | 0 | **0** |
| `comprehensive/` | 300 | 860 | 0 | 0 | **0** |
| `manual/` (published reproductions) | 7 | 20 | **1** | 0 | **0** |
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

Recorded as unresolved, not as a pass and not as a defect. It does not touch any
deterministic claim, and the path it concerns already carries the warning this
result is consistent with.

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
