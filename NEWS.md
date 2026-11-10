# rnetis (development version)

## Requirements

* **The minimum R version is now 4.2**, raised from 4.1. rnetis's own code and
  all four required packages still run on 4.1; the floor is set by `latentnet`,
  which `latent()` needs and which depends on `ergm`, and current `ergm`
  requires R >= 4.2. Declaring 4.2 means an installation that succeeds can run
  every capability the package offers, rather than all but one.

* `RSiena` and `lame` are no longer suggested packages. `RSiena` supplied
  example data for one test in the non-shipping `comprehensive/` tier and was
  never used by the package itself. `lame` backed a single corroboration test
  of the pooling construction; that test moved to `comprehensive/` alongside
  the gates that establish the pooling, which remain in the main suite.
  Neither removal changes what any function does.

## New

* `netis_gof()` reports the posterior predictive check on a Selection fit: for
  each of five network statistics, the value in the observed network against
  its distribution across networks simulated from the posterior. `amen` was
  already simulating them at every retained scan; they were discarded on the
  way out of the backend adapter and are now carried through.

  The check answers the question a coefficient table cannot. A sender/receiver
  model can place every coefficient correctly and still generate networks with
  none of the observed closure, and `trans.dep` and `cycle.dep` are where that
  shows. Nothing additive accounts for either, so an `R = 0` fit reproducing
  the two degree statistics and missing the two triadic ones is the expected
  result rather than a defect — and it is the reason to consider `R > 0`.

  `ppp` is one-sided, so both 0 and 1 are misfit; `z` is the distance from the
  predictive mean in predictive standard deviations and assumes a roughly
  symmetric predictive distribution; the reported quantiles assume nothing. No
  threshold is applied to any of them.

* `plot()` on a Selection fit takes `what = "gof"`, one facet per statistic
  with the observed value against its predictive range.

* A hierarchical fit (`pooling = selection_pooling()`) is refused by name: its
  sampler does not simulate a network per scan, because the simulation does
  not affect the posterior. So is a fit run with `gof = FALSE`, which recorded
  the observed statistics and no distribution to read them against.

# rnetis 0.1.0

First release. `rnetis` gives applied researchers one interface to two established
literatures, network **influence** and network **selection**, that in practice live
in different toolchains, expect different data shapes, and leave exposure
construction to hand-rolled code.

The package implements, integrates, and operationalizes published methods. A small
number of constructions are its own, and each is declared as such on its help page:
see `?rnetis-package` for the list.

## Data

* `netis_data()` builds the canonical node/tie/group structure, with importers
  from adjacency matrices (`netis_data_from_adjacency()`,
  `netis_data_from_adjacency_series()`), edge lists
  (`netis_data_from_edgelist()`), `igraph` and `network` objects, and split
  frames.
* Every transformation an import performs is recorded rather than applied
  silently: `netis_losses()` reports what a conversion dropped,
  `netis_boundary()` the boundary rule in force, `netis_time_index()` the wave
  structure found.
* Wave labels keep their order: numbers by value, factors by level. Text labels
  that are not numbers are refused, with a pointer to using a factor.
* An undirected tie is stored once and reaches both of its endpoints, for every
  consumer of the network.
* `diagnose_data()` reports what is wrong with a dataset before any model is
  specified.

## Exposure

* `compute_exposure()` and the `exposure()` formula term construct network
  exposure from a conduit network and a content variable, with row
  normalization or none, and with the content and the conduit each reachable at
  a lag (`x_lag`, `w_lag`).
* `bi()` implements the Frank & Fahrbach (1999) balance-information model,
  including the erratum (1999, 10(4), 514-515). `bi_components()` and
  `bi_sweep()` expose the parts and the parameter sweep.
* Binarization rules (`above_mean()`, `above_median()`, `abs_threshold()`,
  `quantile_threshold()`, `custom_binarize_rule()`) are objects that travel with
  the fit, so what counted as a tie is always recoverable.

## Models

* `fit_influence()` fits the Influence model, a node characteristic as a function
  of its prior state and its network exposure, with `stats::lm()`, `lme4::lmer()`
  for `(1 | group)` random intercepts, and `sna::lnam()` for the
  network-autocorrelation form.
* `fit_selection()` fits the Selection model through `amen::ame()`. Multi-group
  data is fitted one group at a time and the coefficients combined by
  inverse-variance weighting, reproducing the `lame::lame_multi()` pooling
  algorithm; `pool_beta = FALSE` keeps the groups separate.
* `fit_selection(pooling = selection_pooling(...))` fits a hierarchical
  multi-group Selection model, the package's own construction from published
  components (`?hierarchical_selection`). Each design column is estimated
  separately in every group, held identical across groups, or given a
  between-group distribution; random coefficients may be correlated, and
  group-level moderators may predict them.
* `latent()` implements the Xu (2018, 2021) two-stage latent-space adjustment for
  latent homophily, through `latentnet::ergmm()`. Its use on several groups is
  experimental and warns: each network gets its own positions and position
  coefficients with one shared exposure coefficient, a construction of this package
  that `?latent` describes and that is not validated for statistical performance.
* `simulate_influence()` generates data from the Influence model.

## Scope

* `T = 1` and `T = 2`. Three or more waves is refused unless `waves =` names a
  transition `c(from, to)`, which fits `to` on `from` exactly as reported.
* Two Selection waves without a lagged relation term is refused as replication
  rather than fitted: it would treat two observations of the same actors as
  independent.
* Multi-group Influence data without a random intercept is refused rather than
  pooled into one intercept.
* Options a model cannot apply are refused rather than accepted and ignored.

## Diagnostics and reporting

* `netis_assumptions()` returns a ledger in which each assumption carries three
  fields, `source`, `handling` and `diagnostic_evidence`. Nothing is ever reported
  as satisfied.
* `netis_dependence()` reports residual dependence across the network as
  quantities without a reference distribution.
* `influence_se_comparison()` compares model-based with cluster-robust standard
  errors (`clubSandwich`), naming what each addresses.
* `influence_effect()` and `selection_effect()` convert coefficients to
  interpretable contrasts, naming the range convention on the result. On a
  network-autocorrelation fit, `influence_effect()` reports a coefficient-scale
  quantity that excludes network feedback.
* `pseudo_r2()` reports a latent-scale variance share, a null comparison for fits
  without latent factors, and Tjur's D, each labeled with the scale it lives on.
* `icc1()`, `icc2()`, `rwg()` and `rwg_j()` cover aggregation reliability and
  within-group agreement. `rwg()` has no default null, because the null decides
  the answer.
* `netis_estimates()`, `netis_glance()`, `tidy()` and `glance()` return results as
  data; `plot_data()` and the `plot()` methods return the plotted frame as well as
  the plot.

## Deliberately absent

Dynamic-panel estimators, network-HAC standard errors, distance-shell network
autocorrelation, and `amen::ame_rep()` replication fits. The first three fall
outside the two-wave scope; the fourth assumes independent repeated observations,
which is not a defensible assumption here.
