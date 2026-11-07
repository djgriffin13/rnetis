# Manual reproduction tests

Reproductions of published results **on the publishing authors' own data**,
against **their own printed numbers**. They are the package's third test
layer, and they live here rather than in `testthat/` or `comprehensive/` for
one reason: they need data rnetis does not ship.

**Nothing in this directory runs under `devtools::test()`, under
`comprehensive/`, or under `R CMD check`.** `test_check()` does not recurse,
so these are only ever run when someone runs them.

## Getting the data

rnetis redistributes nobody's data. Fetch it yourself, once, into any directory
you like, then point `NETIS_PUBLISHED_DATA` at that directory. Every test here
**skips with an explanatory message** when the files are absent, so a checkout
without them is not a failing checkout.

### Frank's toy data — `Toyatt.csv`, `Toynet.csv`

From Ken Frank's social network resources page:
<https://sites.google.com/msu.edu/kenfrank/social-network-resources>

Both are linked there as Google Drive files (a few hundred bytes each), under
the Influence Models / Selection Models sections. The same page carries the
`influence.R` tutorial these numbers come from, and a knitted "latent space
influence" tutorial that prints the s50 coefficients used below.

### SIENA s50 — `s50-network1.dat` … `s50-sport.dat`

<https://www.stats.ox.ac.uk/~snijders/siena/s50_data.zip> (about 7 kB).

Unpack it into an `s50/` subdirectory of your data directory. **You need this
zip file rather than RSiena's shipped `s50a`/`s50s`**: Xu's analysis uses five
covariates, and drug use and sport are not in the RSiena package. They are in
this zip.

### Layout expected

```
$NETIS_PUBLISHED_DATA/
  Toyatt.csv
  Toynet.csv
  s50/
    s50-network1.dat  s50-network2.dat  s50-network3.dat
    s50-alcohol.dat   s50-smoke.dat     s50-drugs.dat     s50-sport.dat
```

## Running

```r
devtools::load_all(".")
Sys.setenv(NETIS_PUBLISHED_DATA = "/path/to/data")
testthat::test_dir("tests/testthat/manual")
```

The MCMC reproduction is slow (minutes) and off by default. To include it:

```r
Sys.setenv(NETIS_MANUAL_MCMC = "1")
```

## What is claimed, and how strongly

**Deterministic reproductions are exact, and are asserted as such.** Frank's
tutorial estimator and Xu's unadjusted models are a row-normalised exposure
followed by OLS. Nothing samples. Given the same data rnetis returns the
published numbers or rnetis is wrong, and the tolerances here are set at
floating-point noise, not at "close enough".

**The adjusted reproduction is not exact and is not asserted as exact.**
Xu's stage one is `latentnet::ergmm()`, whose internal seeding rnetis does not
control, so its positions are a different draw every run. That test compares
rnetis against **the reference implementation rerun in the same session**, not
against the printed figure, because the printed figure is itself one draw. It
asserts that rnetis agrees with the reference within the reference's own
run-to-run spread — which is the strongest true claim available, and stopping
there is deliberate.
