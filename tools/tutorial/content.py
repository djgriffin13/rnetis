"""The tutorial's pages, in order.

Imported for its side effects: every `page()` call runs at import time and
appends to `build.PAGES`, so the import order IS the page order.

NO CAPTURED OUTPUT IS TYPED HERE. `block()`, `result()` and `refusal()` read
what R actually printed into out/, and the build refuses to emit a page whose
capture is missing. The only code written by hand is `codeonly()`, which is
illustrative by construction and never shows a result.

Welcome and Getting started, the Influence and Selection sequences, and the
two advanced walkthroughs live in their own modules and are imported below, in
page order. This file holds Feedback, Reference and Help.
"""

import build
from build import (block, card, cards, codeonly, eq, fig, h3, h4, note, p,
                   page, quote, rare, refusal, table, ul)

# ---------------------------------------------------------------------------
# Everything before Feedback
# ---------------------------------------------------------------------------
# In their own modules, imported HERE because the import order is the page
# order: these pages come before everything below.
import content_start  # noqa: E402,F401
import content_influence  # noqa: E402,F401
import content_selection  # noqa: E402,F401
import content_advanced  # noqa: E402,F401

GS = content_start.GS

# ---------------------------------------------------------------------------
# Feedback
# ---------------------------------------------------------------------------
GF = "Feedback"

page(
    GF, "Feedback", "Under construction, and feedback is wanted",
    '<p class="lede">This tutorial, and the package it documents, are under '
    "construction. We greatly appreciate any feedback we receive.</p>",

    h3("Why yours is worth sending"),
    p("rnetis has been exercised by very few people so far, and first outside "
      "use is what finds the parts of an interface that were only ever "
      "obvious to the person who wrote them. Everything is useful here: a "
      "refusal that did not tell you what to do next, a page that assumed "
      "something it never said, a number you could not interpret, a function "
      "you went looking for and did not find, or simply the step where you "
      "got stuck."),
    p("Refusals are worth singling out. They are this package's main "
      "interface — it would rather decline than guess — so a refusal whose "
      "message failed to name the fix is a defect in its own right, not a "
      "usage error on your part."),

    h3("How to send it"),
    fig("feedback-qr", "Scan this code to send feedback.", cls="qr"),
    note("**Please do not send real or participant data.** The shipped demo "
         "datasets (`demo_influence`, `demo_influence_groups`, "
         "`demo_selection`) and `simulate_influence()` will reproduce almost "
         "anything. If something genuinely cannot be shown without your own "
         "data, say that and nothing else rather than attaching it. rnetis "
         "writes nothing outside `tempdir()`, and a bug report is not the "
         "place to make the exception."),
    p("If you are reporting something specific, the R version and the "
      "versions of `amen`, `latentnet`, `sna` and `lme4` usually decide the "
      "answer, so `sessionInfo()` is worth pasting in. A few lines of code "
      "that show the problem on demo data are worth more than a description "
      "of it."),

    h3("What is still moving"),
    p("Two things on these pages are this package's own constructions rather "
      "than established methods, and both say so where they are documented: "
      "the hierarchical multi-group Selection model "
      "([page B8](#p-the-multilevel-selection-model)) and the "
      "latent-scale variance share. `?rnetis-package` carries the full list. "
      "Results from them are worth reporting as provisional, and feedback on "
      "them is especially welcome."),
)

# ---------------------------------------------------------------------------
# Reference
# ---------------------------------------------------------------------------
GR = "Reference"

page(
    GR, "Formula terms", "Formula terms at a glance",
    '<p class="lede">'
    "This page lists every formula term with the question it answers. It is the page to come back to.</p>",

    h3("Influence terms"),
    table(["Term", "What it does", "Key arguments"], [
        ["`ar()`", "the unit's **own** earlier outcome", "`lag`"],
        ["`exposure()`", "what the alters scored, along a network",
         "`via`, `normalize`, `direction`, `isolate_rule`, `x_lag`"],
        ["`lagged()`", "a covariate at an earlier wave", "`lag`"],
        ["`latent()`", "latent-space positions, as controls",
         "`d`, `homophily`, `terms`, `lag`, `control`, `seed`"],
        ["`centrality()`", "a structural position measure as a predictor",
         "the measure, `mode`"],
        ["`network_stat()`", "a whole-network statistic as a predictor", "the statistic"],
        ["`(1 | group)`", "a random intercept; switches the backend to lme4", "—"],
        ["`a:b`", "a two-way interaction (Influence only)", "—"],
    ]),
    h3("Selection terms"),
    table(["Term", "What it does"], [
        ["`sender(x)`", "does a high-`x` actor **name** more?"],
        ["`receiver(x)`", "is a high-`x` actor **named** more?"],
        ["`dyad(z)`", "does this property of the **pair** predict a tie?"],
        ["`prior_tie()`", "was the tie already there at the earlier wave?"],
        ["`prior_reciprocity()`", "did the other one name me first?"],
        ["`group(z)`", "a group-level predictor — refused as written; it "
         "belongs in the [hierarchical model](#p-the-multilevel-selection-model)"],
    ]),
    note("`R` is not a formula term: it is an argument to `fit_selection()`, "
         "because it sets the rank of the multiplicative effects rather than "
         "adding a named predictor. See "
         "[page B7](#p-the-latent-factor-selection-model)."),
    h3("Network transformations, usable inside a term"),
    table(["Call", "What it does"], [
        ["`binarize(net, rule)`", "a weighted network to a binary one"],
        ["`symmetrize(net, rule = )`", "collapse the two directions; `rule` is required"],
        ["`multiplex(a, b, combine = )`", "combine networks (`union`, `intersection`)"],
        ["`inverse(net)`", "invert the weights"],
    ]),
    p("Binarization rules: `above_mean()`, `above_median()`, `threshold()`, "
      "`abs_threshold()`, `quantile_threshold()`, `nonzero()`, `positive()`, "
      "`presence()`, and `custom_binarize_rule()` for anything else. Each is "
      "a named object rather than a string, so a typo is an error rather than "
      "a silent fallback."),
)

page(
    GR, "Importing and exporting", "Importing, exporting and deriving",
    '<p class="lede">'
    "This page covers getting your data into the shape rnetis wants, and getting it back out again.</p>",

    h3("Importers"),
    table(["You have", "Use"], [
        ["An edge list", "`netis_data_from_edgelist()`"],
        ["An adjacency matrix", "`netis_data_from_adjacency()`"],
        ["One matrix per wave", "`netis_data_from_adjacency_series()`"],
        ["An `igraph` object", "`netis_data_from_igraph()`"],
        ["A list of `igraph` objects", "`netis_data_from_igraph_list()`"],
        ["A `network` / statnet object", "`netis_data_from_network()`"],
        ["Separate frames per group", "`netis_data_from_split()`"],
        ["Something else tabular", "`as_netis_data()`"],
    ]),
    h3("Exporters"),
    table(["You want", "Use"], [
        ["An `igraph` object", "`as_igraph()`"],
        ["One per wave or group", "`as_igraph_list()`, `as_network_list()`"],
        ["A `network` object", "`as_network()`"],
        ["A plain matrix", "`netis_adjacency()`"],
        ["The tables back", "`netis_nodes()`, `netis_ties()`, `netis_tables()`"],
    ]),
    h3("What a conversion records"),
    p("Every import crosses a boundary where meaning can be lost: an `igraph` "
      "object does not carry a notion of what a zero means or who was on the "
      "roster. Two functions make that visible rather than letting it pass."),
    p("**`netis_boundary()`** reports, for each declaration, whether it was "
      "*declared* by you or *inferred* — and when inferred, by which rule. "
      "[page A3](#p-the-data-people-ties-and-waves) reads one in full."),
    p("**`netis_losses()`** reports what a conversion actually dropped: "
      "isolates an edge list could not represent, attributes with no home in "
      "the target format, dyads that existed in one direction only. It is "
      "worth calling after any import you did not do by hand."),
    h3("Derived networks"),
    p("`binarize()`, `symmetrize()`, `multiplex()` and `inverse()` build a "
      "new network from an existing one, and can be used inline inside an "
      "`exposure()` term so the derivation travels with the fit. None of them "
      "has a default rule where the choice is substantive — "
      "[`symmetrize()` refuses without one](#p-the-data-people-ties-and-waves)."),
    p("`bi()`, `bi_components()` and `bi_sweep()` handle the Burt-style "
      "indirect-exposure constructions, including sweeping a threshold to "
      "check that a conclusion does not depend on it."),
    h3("Derived variables"),
    table(["Function", "What it adds"], [
        ["`add_centering()`",
         "group-mean, grand-mean or context centring, with the unit named"],
        ["`add_decomposition()`",
         "splits a variable into between- and within-group parts"],
        ["`add_baseline_provenance()`",
         "records which rows a baseline value came from"],
    ]),
    p("Each writes a new column and records what it did, rather than "
      "modifying the original in place. Centring in particular is a choice "
      "with consequences for what a coefficient means, and `unit` is required "
      "rather than defaulted."),
)

page(
    GR, "The rest of the package", "The rest of the package",
    '<p class="lede">'
    "This page gives one line per function, saying <em>why you would reach for it</em>, which a function index does not tell you.</p>",

    h3("Reading a fit"),
    table(["Function", "Reach for it when"], [
        ["`netis_estimates()`",
         "you want the coefficient table as a data frame to compute on"],
        ["`tidy()`, `glance()`", "you already use broom and want the same shapes"],
        ["`netis_glance()`", "you want one row describing the whole fit"],
        ["`netis_estimand()`", "you want to know precisely what was estimated"],
        ["`netis_diagnostics()`", "the fit mentioned findings and you want them"],
        ["`filter_diagnostics()`", "there are too many findings to read"],
        ["`plot_data()`", "you want the table behind any plot, to draw it yourself"],
    ]),
    h3("Effect sizes and explained variation"),
    table(["Function", "Reach for it when"], [
        ["`influence_effect()`",
         "you need an Influence effect on the outcome's own scale"],
        ["`selection_effect()`",
         "you need a Selection effect as a probability contrast"],
        ["`pseudo_r2()`", "someone has asked for an R-squared"],
        ["`netis_variance_components()`", "you want to know where the variance sits"],
        ["`netis_gof()`", "you want to know whether the model generates networks like yours"],
    ]),
    h3("Assumptions, dependence and sensitivity"),
    table(["Function", "Reach for it when"], [
        ["`netis_assumptions()`", "you are writing the limitations section"],
        ["`netis_dependence()`", "you want to know what structure is left in the residuals"],
        ["`influence_se_comparison()`", "you suspect clustering is affecting the standard errors"],
        ["`diagnose_data()`", "before fitting anything"],
        ["`diagnose_design()`", "before paying for a long chain"],
    ]),
    h3("Aggregation, reliability and agreement"),
    table(["Function", "Answers"], [
        ["`icc1()`", "how much variance is between groups?"],
        ["`icc2()`", "are the group means reliable enough to use?"],
        ["`rwg()`, `rwg_j()`", "do members of a group agree with each other?"],
        ["`netis_reliability()`", "all of the above, bundled"],
    ]),
    h3("Network features outside a model"),
    table(["Function", "What it computes"], [
        ["`compute_exposure()`", "exposure as a column, for inspection"],
        ["`compute_centrality()`", "degree and other centralities"],
        ["`compute_network_stat()`", "density, reciprocity and other whole-network statistics"],
        ["`simulate_influence()`", "a synthetic dataset with a known effect"],
    ]),
    note("Everything here has a help page with worked examples. "
         "`?rnetis-package` groups the full set the same way, and is the "
         "authoritative list if this one falls behind."),
)

page(
    GR, "What rnetis refuses", "What rnetis refuses, and why",
    '<p class="lede">A refusal is a design statement, not an error. This page '
    "collects them, and says what is out of scope and why.</p>",

    h3("The principle"),
    p("rnetis refuses in one circumstance: when returning a number would "
      "require a choice that is yours to make, or would produce a quantity "
      "whose name misdescribes it. It does not refuse to protect you from "
      "small samples, odd data or bad ideas — those get a finding, and you "
      "decide."),
    p("The practical test is whether the alternative is a **silently wrong "
      "number**. A wrong number looks exactly like a right one, travels into "
      "a paper, and is never caught. An error is caught immediately, by "
      "definition."),
    h3("The refusals you will meet"),
    table(["Situation", "Why", "The fix it names"], [
        ["`symmetrize()` without a rule",
         "max, min, sum and mean are different operations on a weighted network",
         "pass `rule =`"],
        ["Row-normalised exposure with isolates",
         "the denominator is undefined, and 0 is not a neutral value",
         "`isolate_rule = \"na\"` or `\"zero\"`"],
        ["`complete_dyadic` with no presence rule",
         "rows exist regardless of tie status, so which rows are ties is undeclared",
         "`presence_rule`, `presence_col`, or `row_presence`"],
        ["Several groups, no random-effects term",
         "the fit would pool unrelated networks into one intercept",
         "add `(1 | group)`, or subset to one group"],
        ["Three or more waves on a Selection fit",
         "rnetis fits one transition and will not choose which",
         "name it with `waves = c(from, to)`"],
        ["`logLik()` on a Selection fit",
         "amen returns none, and rebuilding one from draws renames a different quantity",
         "`pseudo_r2()`"],
        ["`group(z)` in a Selection formula",
         "collinear with the group intercept inside any one fit",
         "a moderator on a random intercept"],
        ["Cluster-robust SEs on one network",
         "the outer variance is estimated across clusters, and there is one",
         "nothing — and *not* clustering on actors"],
        ["`netis_gof()` on a hierarchical fit",
         "that sampler simulates no networks",
         "fit the groups separately"],
        ["Interactions in a Selection formula",
         "the latent scale shifts with modelled heterogeneity",
         "subgroup fits, or a moderator"],
        ["Multi-step exposure on a network with unresolved missing dyads",
         "an unknown tie two steps out is not an absent one",
         "declare the missingness"],
    ]),
    h3("What is out of scope, and why"),
    ul("**Two waves at most.** With three or more, rnetis requires you to "
       "name the wave or the transition rather than choosing on your behalf.",
       "**One outcome occasion per unit per fit.** Wave *depth* is "
       "unrestricted — lags and exposure history may reach further back — but "
       "repeated outcome rows per unit are not modelled.",
       "**Gaussian continuous Influence outcomes.** Binary and count "
       "outcomes in an Influence model are not supported."),
    p("Two families of method are **absent because of that scope, not "
      "because of any doubt about them**: dynamic-panel estimators, which "
      "would address the bias "
      "[page B2](#p-selection-the-theory-and-the-hypothesis) mentions, and distance-shell "
      "network autocorrelation models. Both are published, both are sound, "
      "and neither fits inside what this package currently promises."),
)

# ---------------------------------------------------------------------------
# Help
# ---------------------------------------------------------------------------
GH = "Help"

page(
    GH, "Common errors", "Common errors",
    '<p class="lede">'
    "The errors are listed roughly in the order you will meet them.</p>",

    p("rnetis's messages are written to be read, and most of them name the "
      "fix. If you have hit one not listed here, the message itself is "
      "usually the best documentation available — it was written for the "
      "situation you are in, which a general page cannot be."),

    h3("“Edge presence for ‘x’ is undetermined”"),
    p("You declared the network `complete_dyadic` — every pair has a row — "
      "but not which rows count as ties. Pass a `presence_rule`, a "
      "`presence_col`, or `row_presence` if every row is a tie. "
      "[page A3](#p-the-data-people-ties-and-waves) works through the difference, and the "
      "three readings give genuinely different densities."),

    h3("“Row-normalization requested but n node(s) have no reference-set members”"),
    p("Someone has no alters, so the denominator is zero. Decide what an "
      "isolate means in your design: `isolate_rule = \"na\"` (undefined, and "
      "usually right), or `\"zero\"` (they sit at zero on the outcome's "
      "scale, which is rarely what you want). "
      "[page A4](#p-running-the-influence-model)."),

    h3("“This netis_data object has N groups, but the formula has no random-effects term”"),
    p("Add `(1 | group)`, or subset to one group's rows and rebuild. "
      "[page A8](#p-the-multilevel-influence-model)."),

    h3("“This data has N waves … and this version fits one transition”"),
    p("Name the transition: `waves = c(1, 2)`. The refusal exists because a "
      "result reported without naming its transition reads as though the "
      "whole panel were modelled. "
      "See [page B2](#p-selection-the-theory-and-the-hypothesis)."),

    h3("“symmetrize() requires rule =”"),
    p("On a weighted network, collapsing the two directions is several "
      "different operations. Pick one: `\"max\"`, `\"min\"`, `\"sum\"`, "
      "`\"mean\"`, `\"any\"`, `\"both\"`."),

    h3("“A Selection fit has no log-likelihood”"),
    p("`logLik()`, `AIC()` and `BIC()` are not defined for these models. Use "
      "`pseudo_r2()` for explained variation and `netis_gof()` for whether "
      "the model reproduces the network. "
      "See [page B5](#p-interpreting-the-selection-model)."),

    h3("“Internal error: the fitted model has N coefficient(s) but rnetis assembled M name(s)”"),
    p("A design column had no variation across the whole network, so "
      "`amen::design_array()` dropped the intercept. Usually a small or "
      "heavily masked group, or a sender/receiver covariate that is constant "
      "within it. `diagnose_design()` finds this before the chain runs."),

    h3("The fit completed but reported WARNING findings"),
    p("Not an error, and the result is usable. `netis_diagnostics(fit)` lists "
      "them with a level each. A **NOTE** is worth a look, a **WARNING** will "
      "affect a result, an **ERROR** would have stopped the fit."),
)

page(
    GH, "Slow fits", "Slow fits and MCMC settings",
    '<p class="lede">'
    "This page explains what the chain arguments trade against each other, and what to do when a fit is taking too long.</p>",

    h3("The four arguments"),
    table(["Argument", "What it controls", "Raising it"], [
        ["`nscan`", "total iterations", "more draws, more time, linearly"],
        ["`burn`", "iterations discarded as warm-up",
         "safer start, fewer retained draws"],
        ["`odens`", "thinning — keep one draw in every `odens`",
         "fewer, less correlated draws; **does not** make the chain shorter"],
        ["`seed`", "reproducibility", "—"],
    ]),
    p("Retained draws are `(nscan - burn) / odens`. The defaults used in this "
      "tutorial — `nscan = 5000, burn = 1000, odens = 10` — keep 400, which "
      "is enough for stable coefficient summaries and thin for a posterior "
      "predictive check: it sets `ppp`'s granularity at 1/400."),

    h3("What actually costs time"),
    ul("**Actors, quadratically.** The sampler works over ordered pairs, so "
       "doubling the actors roughly quadruples the work per scan.",
       "**The rank R.** Each dimension adds 2n latent parameters and they "
       "mix more slowly than the coefficients do.",
       "**Groups.** Per-group fits are one chain each; the hierarchical "
       "sampler puts every group in one chain and is not cheaper."),

    h3("If a fit is too slow"),
    p("**Shorten the chain to check the pipeline, not the result.** Run at "
      "`nscan = 200, burn = 50` to confirm the formula parses, the terms "
      "identify and the output looks right. Then run the real chain once. "
      "Never report coefficients from the short run."),
    p("**Check the design first.** `diagnose_design()` catches a collinear or "
      "constant term in a second rather than after twenty minutes."),
    p("**Lower R before lowering nscan.** A short chain on a complex model is "
      "worse than an adequate chain on a simpler one, and "
      "[page B7](#p-the-latent-factor-selection-model) gives a criterion for "
      "whether the complexity is earning anything."),
    note("**Resist raising `odens` to make a fit faster.** Thinning discards "
         "draws that have already been computed; it reduces storage and "
         "autocorrelation, not runtime. If you need the chain to finish "
         "sooner, that is `nscan`."),
)

page(
    GH, "Read more", "Where to read more",
    '<p class="lede">'
    "This page lists what rnetis is built on and where the methods come from.</p>",

    h3("The packages rnetis builds on"),
    table(["Package", "What it does here"], [
        ["`amen`", "fits every Selection model — the AME sampler, and "
                   "the five posterior-predictive network statistics"],
        ["`latentnet`", "stage one of `latent()`: estimates the latent positions"],
        ["`lme4`", "fits every Influence model with a random-effects term"],
        ["`stats`", "fits every single-group Influence model, by OLS"],
        ["`clubSandwich`", "the cluster-robust standard errors in "
                           "`influence_se_comparison()`"],
        ["`sna`, `numDeriv`", "the network-autocorrelation (SAR) estimator"],
        ["`igraph`, `network`", "import from and export to the two main "
                                "network representations in R"],
        ["`ggplot2`", "every figure"],
    ]),
    note("rnetis does not reimplement any of these. Where a result comes "
         "from another package, the fit says so — `summary()` prints the "
         "backend and its version, and `netis_boundary()` records what a "
         "conversion inferred."),

    h3("The methods"),
    h4("Influence"),
    ul("Friedkin, N. E. and Johnsen, E. C. (1990) Social influence and "
       "opinions. *Journal of Mathematical Sociology* **15**, 193–206. "
       "— network exposure.",
       "Leenders, R. T. A. J. (2002) Modeling social influence through "
       "network autocorrelation. *Social Networks* **24**, 21–47. "
       "— row normalisation, and what the choice of weights means.",
       "Shalizi, C. R. and Thomas, A. C. (2011) Homophily and contagion are "
       "generically confounded in observational social network studies. "
       "*Sociological Methods & Research* **40**, 211–239.",
       "Xu, R. (2018, 2021) — the two-stage latent-space adjustment "
       "behind `latent()`. Published as a procedure with code, not as a "
       "package."),
    h4("Selection"),
    ul("Hoff, P. D. (2021) Additive and multiplicative effects network "
       "models. *Statistical Science* **36**(1), 34–50.",
       "Hoff, P., Fosdick, B. and Volfovsky, A. (2024) *amen: Additive and "
       "Multiplicative Effects Models for Networks and Relational Data*.",
       "Nickell, S. (1981) Biases in dynamic models with fixed effects. "
       "*Econometrica* **49**(6), 1417–1426.",
       "Heckman, J. J. (1981) The incidental parameters problem and the "
       "problem of initial conditions. — with Nickell, the reason "
       "rnetis fits one transition.",
       "Tjur, T. (2009) Coefficients of determination in logistic regression "
       "models. *The American Statistician* **63**(4), 366–372."),
    h4("Checking and combining"),
    ul("Gelman, A., Meng, X.-L. and Stern, H. (1996) Posterior predictive "
       "assessment of model fitness via realized discrepancies. *Statistica "
       "Sinica* **6**(4), 733–760. — the logic of `netis_gof()`.",
       "Vehtari, A. et al. (2021) Rank-normalization, folding, and "
       "localization: an improved R-hat. *Bayesian Analysis* **16**(2), "
       "667–718.",
       "Cochran, W. G. (1954); Hedges, L. V. and Olkin, I. (1985) — "
       "inverse-variance pooling, behind `pool_beta`."),
    h3("And the package itself"),
    p("`?rnetis-package` lists every function grouped by task, and — the part "
      "worth reading — a section naming **what is this package's own**: the "
      "constructions that are not published methods, each declared on its own "
      "help page. The hierarchical Selection model and the latent-scale "
      "variance share are both on that list."),
)
