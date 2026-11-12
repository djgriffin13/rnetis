"""The tutorial's pages, in order.

Imported for its side effects: every `page()` call runs at import time and
appends to `build.PAGES`, so the import order IS the page order.

TWO BUILDS, ONE SOURCE. `make.py` publishes this into pkgdown/assets;
`make_preview.py` writes a review copy with the outstanding work marked. The
only difference is the DRAFT flag below -- with it set, `figtodo()` renders a
visible "New figure" marker, and without it the marker renders as nothing.
Everything else is identical, so what gets reviewed is what ships.

NO CAPTURED OUTPUT IS TYPED HERE. `block()`, `result()` and `refusal()` read
what R actually printed into out/, and the build refuses to emit a page whose
capture is missing. The only code written by hand is `codeonly()`, which is
illustrative by construction and never shows a result.

The Welcome page, Getting started and the Influence sequence live in
content_start.py and content_influence.py and are imported below, in page
order. This file holds the Selection pages (not yet rewritten), Feedback,
Reference and Help.
"""

import os

import build
from build import (block, card, cards, codeonly, eq, fig, h3, h4, note, p,
                   page, quote, rare, refusal, table, ul)

import diagrams

# ---------------------------------------------------------------------------
# draft markers
# ---------------------------------------------------------------------------
# READ FROM THE ENVIRONMENT, not set as an attribute afterwards. Every page()
# call runs at import time, so figtodo() has already decided what to render
# before an importer gets a chance to assign anything. make_preview.py sets
# the variable before it imports this module; make.py leaves it unset.
#
# Keeping the marker out of the published page while leaving it in the copy
# being reviewed is the whole point: a reviewer who cannot see what is missing
# will not notice that it is.
DRAFT = os.environ.get("TUTORIAL_DRAFT") == "1"

# Appended to the shared stylesheet by make_preview.py rather than committed
# into build.py's CSS: the published page emits no marker, and a rule for a
# class that never appears is dead weight in every copy of it.
EXTRA_CSS = """
.todo{border-left:3px solid var(--accent);padding:.35rem 0 .35rem .7rem;
margin:.6rem 0;color:var(--muted);font-size:15px}
.todo b{color:var(--ink);font-weight:600}
"""


def figtodo(what, where):
    """A figure that is specified but not yet drawn.

    Renders as nothing unless DRAFT is set, so the same source publishes and
    reviews. The arguments stay in the source either way, which is the point:
    they are the specification for the figure, and deleting the marker would
    delete the brief along with it.
    """
    if not DRAFT:
        return ""
    return (
        '<p class="todo"><b>New figure:</b> %s <span class="note">(%s)</span></p>'
        % (build.inline(what), build.inline(where))
    )


# ---------------------------------------------------------------------------
# Welcome, Getting started and the Influence sequence
# ---------------------------------------------------------------------------
# In their own modules, imported HERE because the import order is the page
# order: these pages come before everything below.
import content_start  # noqa: E402,F401
import content_influence  # noqa: E402,F401

GS = content_start.GS

# ---------------------------------------------------------------------------
# Selection
#
# NOT YET REWRITTEN. These pages keep the earlier layout, the earlier example
# data and output captured before print() and summary() changed. They are next:
# the plan is the Influence sequence's, page for page, on the same team data.
# ---------------------------------------------------------------------------
GB = "Selection (being rewritten)"

page(
    GB, "B1 · Background", "Selection: the question",
    '<p class="lede">The mirror of Path A’s first page, and the place '
    "where homophily stops being a nuisance and becomes the thing being "
    "measured.</p>",

    h3("B1.1 What selection means"),
    p("The question is who forms a tie with whom. Not whether a network "
      "exists, and not what flows through it once it does — what explains the "
      "presence of this particular tie, between this sender and this "
      "receiver, rather than some other."),
    p("Three kinds of answer are usually on the table, and a model worth "
      "fitting separates them:"),
    ul("**Something about the sender.** Some people name a lot of people. "
       "Expansiveness is a property of the person doing the naming, and it "
       "inflates every tie they appear in.",
       "**Something about the receiver.** Some people get named a lot. "
       "Popularity is a property of the person being named, and it does the "
       "same thing from the other side.",
       "**Something about the pair.** They are in the same unit, they have "
       "similar tenure, one of them already named the other last year. This "
       "is the category that usually carries the hypothesis."),
    p("Keeping those apart is the whole reason for the model on page 3. If "
      "you do not, an apparent effect of *similarity* can be nothing more "
      "than two people who both happen to name a lot of people."),
    note("**Homophily has changed sides.** On "
         "[Path A](#p-the-theory-and-the-hypothesis) it was the confound — the thing "
         "that made an influence estimate untrustworthy. Here it is the "
         "estimand. The same process, and whether it is a nuisance or a "
         "finding depends entirely on which question you asked."),

    h3("B1.2 The hypothesis, stated so it could fail"),
    quote("**H1.** A pair who share some attribute are more likely to form a "
          "tie than a pair who do not — **net of** how much the sender names "
          "in general, how much the receiver is named in general, and whether "
          "a tie was already there."),
    p("Every clause after “net of” is a rival explanation being "
      "closed off, and each has its own term on page 3. Under the null, the "
      "pair coefficient is zero: knowing the pair are similar tells you "
      "nothing once you know who is doing the naming and who is being named."),
    p("A **negative** coefficient is heterophily — ties forming across the "
      "attribute rather than within it — which is a real pattern in advice "
      "networks in particular, where people often go to someone unlike "
      "themselves precisely because they know something different. Decide "
      "which you expect before you fit."),

    h3("B1.3 Why one transition, and not more"),
    p("Path A argued that one occasion cannot identify influence. Path B has "
      "the opposite-looking constraint: rnetis fits **exactly one transition** "
      "and refuses to pool several. That is a deliberate limit, and the reason "
      "is worth understanding before it arrives as an error message."),
    build.dia(diagrams.one_transition(),
              "Two waves make one transition: the earlier network is the "
              "initial condition, the later one the outcome."),
    p("The natural model for tie formation includes **whether the tie was "
      "already there** — `prior_tie()` on page 3. That makes it a dynamic "
      "panel model: a lagged dependent variable on the right-hand side. And "
      "dynamic panels have two well-known problems that do not go away by "
      "collecting more waves."),
    p("**Nickell bias** (1981). When a lagged dependent variable sits "
      "alongside actor-level effects, the lagged term is correlated with the "
      "error, and the resulting bias is of order 1/T. With a short panel it is "
      "not small, and it does not vanish as the number of *actors* grows — "
      "only as the number of *waves* does."),
    p("**The initial conditions problem** (Heckman 1981). The first observed "
      "network is not a starting point the process chose; it is the state the "
      "process happened to be in when you arrived. Treating it as exogenous "
      "assumes away everything that produced it."),
    p("**rnetis corrects for neither.** There are estimators that address "
      "them, and they are outside this package's scope. So rather than fit "
      "across several transitions and return a number carrying a bias nobody "
      "mentions, it fits one and makes you name it:"),
    refusal("b2-waves3-refuse"),
    p("Read the reason given: *a fit reported without naming its transition "
      "reads as though the whole panel were modeled*. The refusal is about "
      "what the result would **look like to a reader**, not only about what it "
      "would be. With two waves there is one transition and the question does "
      "not arise, which is why this tutorial uses two throughout."),
)

page(
    GB, "B2 · Data", "The Selection dataset",
    '<p class="lede">The same object Path A builds, with the emphasis moved: '
    "the unit of analysis is the ordered pair, and what has two waves is the "
    "<em>network</em> rather than a score.</p>",
    h3("B2.1 The same two frames, read differently"),
    p("If you have come from Path A, the data structure is not new and this "
      "section exists so you do not re-learn it. Same two frames — a node "
      "table and a tie table — same `netis_data()`, same declarations about "
      "what a zero means and who was on the roster."),
    p("What changes is **which part the model is trying to predict**. On Path "
      "A the outcome was a column of the node table and the network carried "
      "the exposure. Here the outcome is the **tie table itself**: the "
      "earlier network is the initial condition and the later one is what we "
      "are explaining."),
    build.dia(diagrams.data_structure("selection"),
              "The same object as Path A builds. What changes is which table "
              "the model is trying to predict."),
    h3("B2.2 The ordered pair is the row"),
    p("On Path A the unit of analysis was the person. Here it is the "
      "**ordered pair**: *i naming j* is a different observation from *j "
      "naming i*, and a model that cannot tell them apart cannot separate "
      "expansiveness from popularity."),
    build.dia(diagrams.selection_model(),
              "One ordered pair, and everything the model says about it: a "
              "baseline shared by every pair, one effect for the sender, one "
              "for the receiver, and whatever the pair have in common. Those "
              "four are exactly what the coefficient table on page 3 "
              "separates, which is why a raw tie count cannot stand in for "
              "any of them."),
    p("That changes how much data you have, in a way worth seeing before you "
      "fit. Twenty actors is not twenty rows — it is 20 × 19 = **380 ordered "
      "pairs**, since self-ties are excluded. The sample size in a Selection "
      "model grows with the square of the number of actors, which is why "
      "networks that feel small still support a model with several terms."),
    p("It also means the rows are **not independent**, and nothing you do to "
      "the data frame will make them so. All 19 rows where i is the sender "
      "share i. All 19 where i is the receiver share i too. And the pair "
      "(i,j) and (j,i) are the same two people. Page 3's model is built "
      "around exactly that."),

    h3("B2.3 Building a pair attribute"),
    p("Sender and receiver attributes come from the node table, as on Path A. "
      "A **pair** attribute does not: it is a property of the ordered pair and "
      "belongs in the tie table, with one value per pair:"),
    block("b2-pair"),
    p("`same_unit` is 1 when the two people are in the same organisational "
      "unit and 0 otherwise. Note it sits beside `tie` in the tie table, not "
      "in the node table — because there is no value of `same_unit` for a "
      "person on their own. It only exists relative to somebody else."),
    p("That is the test for where a variable belongs. Tenure is a fact about "
      "a person: node table. Whether two people share a unit is a fact about "
      "a pair: tie table. Difference in tenure would also be a pair fact, and "
      "would go in the tie table even though it is derived from a node "
      "variable."),

    h3("B2.4 Building the object, and which wave is the outcome"),
    p("Same function as Path A, same arguments:"),
    block("b2-build"),
    p("Twenty nodes, **380 ties**, directed, two waves. The word “ties” "
      "there means *rows in the tie table*, and the table has a row for every "
      "ordered pair at every wave, most of them recording a zero. That is what "
      "`complete_dyadic` means in the semantics block below it, and it is the "
      "right shape for a Selection model: the model needs the non-ties as much "
      "as the ties, because it is modelling which of the 380 possible ties "
      "exist."),
    p("Read the variable lines. `tenure` is a **static node** variable. "
      "`same_unit` is a **static tie** variable. `tie` itself is a "
      "**dynamic tie** variable — it has a value at each wave, and that is "
      "what makes a transition possible to model."),
    p("Which wave does what follows from "
      "[§B1.3](#p-selection-the-question): the **earlier** network is the "
      "initial condition, reached by `prior_tie()`, and the **later** one is "
      "the outcome. With two waves there is nothing to choose."),

    h3("B2.5 Interop, both ways"),
    p("Everything on Path A's data page applies here — the same importers, "
      "the same exporters. One thing bites more often on this path, because "
      "Selection data is usually `complete_dyadic`:"),
    refusal("b2-igraph-refuse"),
    p("An `igraph` graph has edges or it does not; there is no “row "
      "recording a zero” in that representation. So converting a "
      "complete-dyadic network to `igraph` requires knowing which rows are "
      "ties, and if you have not said, rnetis asks rather than assuming every "
      "row is an edge — which would hand you a complete graph. Declare the "
      "rule and the conversion goes through:"),
    block("b2-igraph"),

    h3("B2.6 Check it before modelling it"),
    p("`diagnose_data()` works as on Path A. `diagnose_design()` is the one "
      "worth adding here: it looks at the model you are about to fit against "
      "the data you have, rather than at the data alone — whether a term has "
      "any variation to identify it, whether a group is too small, whether a "
      "covariate is constant within every dyad. On a Selection model those "
      "are easy mistakes to make and expensive to discover after a "
      "twenty-minute chain."),
)

page(
    GB, "B3 · SRM", "The basic selection model",
    '<p class="lede">The social relations model. Its premise is that my ties '
    "are not independent of each other, because they are all mine — and its "
    "method is to give every actor two numbers that say so.</p>",

    h3("B3.1 The equation"),
    p("The probability that i names j, as a function of three things about "
      "that pair:"),
    eq(
        "Pr(y<sub>ij</sub> = 1) = "
        "Φ(μ + a<sub>i</sub> + b<sub>j</sub> + "
        "β'x<sub>ij</sub>)",
        "i is the sender and j the receiver; Φ is the normal CDF."
    ),
    p("Φ is the normal CDF — the **probit** link, which maps a number on "
      "an unbounded latent scale to a probability between 0 and 1. μ is "
      "the baseline: how dense the network is before anything else is said. "
      "Then the three substantive parts:"),
    table(["Term", "What it is", "Under the null"], [
        ["aᵢ", "sender effect: how much **i names** people in general",
         "all equal; everybody names at the same rate"],
        ["bⱼ", "receiver effect: how much **j is named** in general",
         "all equal; nobody is more popular than anybody"],
        ["β'xᵢⱼ", "the **pair** terms — the hypothesis",
         "**zero**"],
        ["μ", "the baseline log-odds of a tie", "—"],
    ]),
    p("The two latent scales are where the care is needed. Because Φ "
      "maps an unbounded scale onto (0, 1), a coefficient is a shift on the "
      "**latent** scale and not a change in probability — the same "
      "coefficient means a large probability change near the middle of the "
      "curve and almost none out in the tail. That is why "
      "[§B3.5](#p-the-basic-selection-model) converts before reporting, and "
      "why the fit itself prints a warning about it."),
    figtodo(
        "equation anatomy for the SRM, keyed to the term subsections",
        "matched pair with A3.1's"
    ),
    h3("B3.2 Sender and receiver effects"),
    p("Before any syntax, be clear about what **aᵢ** and **bⱼ** buy, "
      "because they are the whole reason this is not just a logistic "
      "regression on 380 rows."),
    p("**aᵢ is actor i's expansiveness**: a single number saying how much "
      "i names people in general, over and above anything the covariates "
      "explain. **bⱼ is actor j's popularity**: how much j gets named. "
      "Every row where i is the sender carries the same aᵢ, and every row "
      "where j is the receiver carries the same bⱼ."),
    p("That is what makes the rows dependent, and it is also what rescues the "
      "estimate. Without those two terms, a finding that “people in the "
      "same unit name each other” could be produced entirely by one "
      "large unit containing several people who name everybody. With them, "
      "that explanation has already been given its own parameters, and the "
      "pair term has to earn whatever is left."),
    fig("b-plot-actor", "Each actor's sender effect against their receiver "
                        "effect. The two are not the same thing, and an actor "
                        "can be high on one and low on the other."),
    p("The spread of those effects is itself a finding. A network where "
      "sender variance is large is one where naming behaviour is dominated by "
      "who is doing the naming — and in such a network, raw tie counts are "
      "close to useless as a measure of anything about the pairs."),

    h3("B3.3 The terms, one at a time"),
    p("Five terms, and the thing to learn is which question each one answers "
      "rather than its arguments:"),
    table(
        ["Term", "The question it answers"],
        [
            ["`sender(x)`", "Do people high on x send more ties?"],
            ["`receiver(x)`", "Do people high on x get chosen more?"],
            ["`dyad(z)`", "Does this property of the pair matter?"],
            ["`prior_tie()`", "Was there already a tie here?"],
            ["`prior_reciprocity()`", "Did they send to me first?"],
        ],
    ),
    note("**“Fit” is a verb on this page.** On Path A, fitting a "
         "model and getting fit statistics were two steps of one activity. "
         "Here they come apart: `fit_selection()` fits, and the quantities "
         "you might call *fit statistics* mostly do not exist — "
         "[§B3.6](#p-the-basic-selection-model) says what replaces them. When "
         "this page says fit, it means run."),

    h3("B3.4 Running the model"),
    p("Read the formula before the code:"),
    codeonly(
        'tie ~ sender(tenure) + receiver(tenure) + dyad(same_unit) + prior_tie()'
    ),
    p("`tie` at the later wave is the outcome. `sender(tenure)` asks whether "
      "longer-tenured people name more; `receiver(tenure)` whether they get "
      "named more; `dyad(same_unit)` is the hypothesis, whether sharing a "
      "unit predicts a tie; `prior_tie()` reaches back to the earlier network "
      "and asks whether the tie was already there."),
    p("`family = \"bin\"` makes it a **probit** on tie presence. The fit is "
      "Bayesian — a Markov chain through `amen::ame()` — so `nscan`, `burn` "
      "and `odens` control chain length, warm-up and thinning, and `seed` "
      "makes the run reproducible. This is where the fifteen seconds go."),
    block("b-summary"),
    p("**The header** names the family, the backend, the groups and the "
      "waves — and then spends a paragraph on scale, which is worth reading "
      "once properly."),
    quote("*Coefficients are on the latent probit scale, not a probability or "
          "odds scale: `exp(coefficient)` is not an odds ratio for this "
          "model.*"),
    p("That warning earns its paragraph. Reflexively exponentiating a "
      "coefficient is the single most common error made with these models, "
      "and it yields a number that reads as an odds ratio while being "
      "something else. The warning also "
      "notes that these coefficients **shift with the modelled heterogeneity**, "
      "so comparing them across specifications is not like comparing linear "
      "regression coefficients."),
    p("**The coefficients.** `prior_tie` is 1.74 [1.27, 2.24] — by far the "
      "strongest term, and unsurprising: the best predictor of a tie is a tie "
      "that was already there. `snd_tenure` 0.52 [0.26, 0.77] and "
      "`rcv_tenure` 0.41 [0.19, 0.65] are both clear of zero: longer-tenured "
      "people both name more and are named more."),
    p("And the hypothesis: `dyd_same_unit` is **0.127 [-0.214, 0.539]**. The "
      "interval contains zero and is wide. On this data, once you account for "
      "who names a lot, who gets named a lot, and what was already there, "
      "sharing a unit adds nothing detectable."),
    p("That is the second null in this tutorial, and it is worth noticing why "
      "it is a *stronger* result than it looks. The naive analysis — count "
      "ties within units against ties across them — would almost certainly "
      "have found something, because tenure effects alone would produce it. "
      "The model's job was to take that explanation away first."),
    fig("b-plot-coef",
        "One row per term. The **dot** is the posterior mean and the **line** "
        "the 95% credible interval — a range the parameter plausibly "
        "lies in, not a repeated-sampling statement. The x axis is the "
        "**latent probit scale**, so distances here are not probabilities and "
        "not odds; §B3.5's effect size is the version to report. Read "
        "each row for whether it clears zero: `prior_tie` and both tenure "
        "terms do, `dyd_same_unit` does not."),
    p("**Fit-time findings: 3 WARNING.** Those are not errors and the fit "
      "completed, but something was recorded; `netis_diagnostics(fit)` lists "
      "them and the [common errors](#p-common-errors) page covers the usual "
      "causes."),

    h4("An effect size on a scale people understand"),
    p("Since the coefficients are not interpretable as probabilities, "
      "`selection_effect()` does the conversion:"),
    block("b-effect-ref"),
    p("**+10.3 percentage points [4.8, 15.8]** for a one-unit increase in the "
      "sender's tenure. That is a sentence you can put in a paper. Read the "
      "footnote with it: *actor effects are set to zero, so this is a "
      "hypothetical pair with exactly average sender and receiver tendencies, "
      "not an average over actual dyads.* The `type` argument offers the "
      "other estimand — an average over the dyads you actually observed — and "
      "they answer different questions."),
    h3("B3.6 Fit, and what it is not"),
    p("Path A's fit section had r², AIC and BIC. This page has none of "
      "them, and the first thing to do is understand why rather than go "
      "looking for substitutes."),
    refusal("b2-nologlik-refuse"),
    p("The reason is in the message: `amen::ame()` returns no log-likelihood, "
      "and **reconstructing one from posterior draws would be a different "
      "quantity under the same name**. AIC and BIC are built on a "
      "log-likelihood, so they go too. rnetis could have returned something "
      "plausible. It returns an error that names the alternative instead."),

    h4("Convergence comes first"),
    p("Before any question about how well the model fits, there is a question "
      "about whether the posterior was sampled properly at all. A chain that "
      "has not converged has no fit to discuss — whatever number you compute "
      "from it is a number about the chain, not about the model."),
    fig("b-plot-trace", "The chain in sequence. You are looking for a stable "
                        "band with no drift and no stretches where the chain "
                        "sticks in one place."),
    p("The trace is the eyeball check. The numeric version is three "
      "quantities per parameter, and they are worth understanding properly "
      "rather than glancing at — they are the standard modern set (Vehtari et "
      "al. 2021) and they appear in every Bayesian output you will meet:"),
    block("b3-mcmc-table"),

    h4("What the three numbers mean"),
    p("**R-hat** asks whether the chain has settled. It compares how much the "
      "draws vary *within* stretches of the chain against how much they vary "
      "*between* stretches. If the sampler has converged, those two are "
      "measuring the same distribution and the ratio is 1. If it is still "
      "wandering — drifting, or stuck somewhere it has not left — the between "
      "part is larger and R-hat rises above 1."),
    p("Here every R-hat is 1.000 or within a thousandth of it. A couple sit "
      "fractionally **below** 1 (0.99808): that is sampling noise in a ratio "
      "whose expected value is 1, and it reads the same as 1.000."),
    p("**Bulk ESS** and **Tail ESS** answer a different question: not *has it "
      "settled* but *how much is this chain actually worth*. MCMC draws are "
      "correlated — each one starts from where the last ended — so 400 "
      "retained draws are not 400 independent ones. Effective sample size is "
      "how many independent draws they are equivalent to."),
    p("There are two of them because a chain can be worth different amounts "
      "for different questions:"),
    ul("**Bulk ESS** is for the middle of the distribution — the posterior "
       "mean and median, which is where your point estimate comes from.",
       "**Tail ESS** is for the extremes — the 2.5% and 97.5% quantiles, "
       "which is where your **interval endpoints** come from."),
    p("That distinction matters more than it first appears. A chain can "
      "explore the middle of the posterior perfectly well and visit the tails "
      "rarely, which gives you a well-estimated centre and interval endpoints "
      "that wobble from run to run. Since what you report is usually the "
      "interval, tail ESS is often the binding constraint, and a single "
      "“ESS” number would hide the problem."),
    p("In this fit both sit around 400–520 against 400 retained draws. ESS "
      "close to the number of draws means the chain is barely autocorrelated "
      "at all — it is behaving almost like independent sampling, which is the "
      "good case. (ESS can slightly exceed the draw count when consecutive "
      "draws are mildly *anti*-correlated; it is not a bug.)"),
    fig("b-plot-mcmc", "The same three quantities as a panel, one row per "
                       "parameter. R-hat is read against 1; the two ESS "
                       "columns are read against the number of draws you "
                       "retained."),
    note("**rnetis reports these and does not grade them.** The published "
         "reference points are widely used — R-hat below about 1.01, and "
         "both effective sample sizes above about 400 — and you will find "
         "them in Vehtari et al. (2021). The package prints no verdict "
         "against them, for the reason it prints none anywhere else: a "
         "threshold turns a continuous piece of evidence into a yes, and the "
         "yes then travels further than the evidence deserves."),
    p("What to do when the numbers are poor: a high R-hat means run longer "
      "or start from different values, and a low ESS with an acceptable "
      "R-hat means run longer. Both are `nscan`. Thinning with `odens` does "
      "**not** help — see "
      "[slow fits](#p-slow-fits-and-mcmc-settings)."),

    h4("How much do the predictors account for?"),
    p("`pseudo_r2()` gives three answers, and is explicit that they are three:"),
    block("b-pseudo"),
    p("**Tjur's D (0.462)** is the most interpretable: the average predicted "
      "probability for pairs that are tied, minus the average for pairs that "
      "are not. It is on the probability scale and it is itself a "
      "**posterior-predictive** quantity, which makes it the bridge to the "
      "next section."),
    p("The other two come with caveats the function prints rather than "
      "hides. The latent-scale variance share is **this package's own "
      "construction, not a published statistic** — said plainly, in the "
      "output, where it cannot be missed. And the null-comparison figure "
      "understates explained variation on a binary probit, because the "
      "residual variance is fixed at 1 for identification and so predictors "
      "cannot reduce it. Three measures, three questions, and the closing "
      "line: *none is ‘the’ R-squared*."),
    h3("B3.7 The posterior predictive check"),
    p("This is the part of the page with no counterpart on Path A, and it is "
      "the one that earns its length. `netis_gof()` reports five statistics "
      "of the **network as a whole**, each beside the range the fitted model "
      "actually generates:"),
    table(
        ["Statistic", "Measures", "Which part of the model should explain it"],
        [
            ["`sd.rowmean`", "spread of out-degree", "sender effects"],
            ["`sd.colmean`", "spread of in-degree", "receiver effects"],
            ["`dyad.dep`", "reciprocation", "`prior_reciprocity()`"],
            ["`cycle.dep`", "cyclic triads", "**nothing additive**"],
            ["`trans.dep`", "transitive triads, closure", "**nothing additive**"],
        ],
    ),
    p("The logic is simple. The model is generative: it can produce networks. "
      "So produce a few thousand of them, measure each one, and see whether "
      "the network you actually observed looks like something this model "
      "would have produced. A statistic that falls outside the range the "
      "model generates is a feature of your data the model **cannot account "
      "for**, however good its coefficients look."),
    block("b3-gof"),
    p("Read a row at a time. `observed` is the value in the real network; "
      "`predicted_lower` and `predicted_upper` bracket the middle 95% of "
      "simulated networks. `ppp` is the share of simulated networks at least "
      "as extreme — it is **one-sided**, so 0 and 1 are both misfit and 0.5 "
      "is agreement. `z` is the distance from the predictive mean in "
      "predictive standard deviations."),
    p("**Every statistic sits inside its range.** `trans.dep` observed at "
      "0.0304 against [-0.023, 0.068]; `dyad.dep` at 0.252 against [0.081, "
      "0.414]. No `|z|` exceeds 0.9 and every `ppp` is between 0.37 and 0.82. "
      "This additive model generates networks that look like the one it was "
      "fitted to, on all five measures."),
    fig("b3-plot-gof",
        "The same five rows as a picture. Each panel is one statistic, on its "
        "own x scale. The **dot** is the value in the network you observed; "
        "the **grey bar** is the central 95% of the values across networks "
        "simulated from the fitted model. A dot inside its bar means the "
        "model reproduces that feature; a dot outside means it cannot."),
    p("That is the whole reading, and it is worth doing with your eye before "
      "going back to the numbers. Every dot here sits well inside its bar, "
      "and in four of the five panels it is close to the middle. The bars "
      "are not confidence intervals around the dots — the dot is the data "
      "and the bar is the model, and the question is only whether one "
      "contains the other."),
    p("That is the honest result for this dataset, and it is worth saying "
      "what it does and does not license. It does **not** mean the model is "
      "correct. These are five statistics; a model can reproduce all of them "
      "and still be wrong about the mechanism. What it does mean is that "
      "there is no evidence *here* of structure the additive model is "
      "missing — which is a real and useful thing to be able to say, and it "
      "is a thing a coefficient table cannot say at all."),

    h4("What a failing check looks like"),
    p("A reader who only ever sees a check pass has learned that it prints "
      "five numbers. So here is the same model — sender effects, receiver "
      "effects, `prior_tie()`, no latent dimensions — fitted to a network "
      "built out of three dense blocks, where closure is everywhere by "
      "construction:"),
    block("b3-gof-misfit"),
    p("**`trans.dep` observed at 0.419 against a predictive range of [0.065, "
      "0.258]** — far outside, `ppp` exactly 0.000, `z` of 5.12. "
      "`cycle.dep` is as bad at 4.90. The model simply cannot generate "
      "networks with this much closure, and the check says so unambiguously."),
    fig("b3-plot-gof-misfit",
        "The same figure, same model, on a clustered network. **All five dots "
        "sit outside their bars** — and the picture says so before any of the "
        "numbers do."),
    p("Two details reward attention. First, `ppp = 0.000` means *not one* "
      "simulated network reached the observed value — and with 400 draws the "
      "granularity is 1/400, so 0.000 means “below 0.0025” rather "
      "than literally impossible. Second, the two **degree** statistics are "
      "also outside, in the other direction: `z` of -2.2 and -2.8. Dense "
      "blocks make everybody's degree unusually similar, and the model — free "
      "to spread sender and receiver effects — generates more variation than "
      "the data has."),
    p("That second point is the one to take away. A failing check tells you "
      "*which* feature the model cannot reproduce, and the pattern across the "
      "five statistics is diagnostic. Closure out and degrees too even "
      "together says “there are groups here”, which is a different "
      "problem from closure out on its own."),
    p("Neither of those readings is available from a coefficient table, and "
      "neither is a verdict. The check tells you what the model misses. "
      "Whether that matters depends on what you are claiming, and "
      "[the next page](#p-adding-multiplicative-effects) is about what to do "
      "when it does."),
)

page(
    GB, "B4 · AME", "Adding multiplicative effects",
    '<p class="lede">The additive model can say who names and who is named. '
    "It has no way at all to say <em>who with whom</em>. This page adds the "
    "part that can.</p>",

    h3("B4.1 What the additive model cannot absorb"),
    p("aᵢ and bⱼ are **actor-level** quantities. They say that i "
      "names a lot and that j is named a lot, and from those two facts the "
      "model predicts the i→j tie. That construction has a consequence worth "
      "stating baldly: **in an additive model, two actors with the same "
      "sender effect have the same predicted tie to every receiver.** There "
      "is no room for i and j to have a particular affinity that neither has "
      "with anyone else."),
    p("So the additive model cannot produce **third-order structure**: "
      "transitivity (my contacts' contacts are my contacts), clustering, "
      "groups, or any pattern that depends on *combinations* of actors rather "
      "than on actors singly. Whatever closure exists in your network is "
      "sitting in the residual, and it is not a small thing to leave there — "
      "most social networks have a great deal of it."),
    note("**And on this dataset it does not matter.** "
         "[§B3.7](#p-the-basic-selection-model) ran the check and the "
         "additive model reproduced all five statistics, closure included. "
         "That is the honest starting point for this page, and it is the one "
         "most readers will have: you add multiplicative effects **to find "
         "out** whether they are needed, not because a diagnostic has already "
         "told you they are. The clustered contrast in §B3.7 is what the "
         "other case looks like."),

    h3("B4.2 The equation, with the multiplicative term"),
    p("One term added, and it is the only one in either path that multiplies "
      "two unknowns together:"),
    eq(
        "Pr(y<sub>ij</sub> = 1) = "
        "Φ(μ + a<sub>i</sub> + b<sub>j</sub> + "
        "β'x<sub>ij</sub> + u<sub>i</sub>'v<sub>j</sub>)",
        "Only the product U V' is identified; u and v separately are not, "
        "which is why §B4.3 reports the product and nothing else."
    ),
    p("Each actor gets **two vectors of length R**: uᵢ, used when they "
      "send, and vⱼ, used when they receive. The term in the model is "
      "their inner product, uᵢ'vⱼ, which is large when i's sending "
      "vector points the same way as j's receiving vector. That is what "
      "“affinity between this particular pair” looks like when you "
      "write it as arithmetic."),
    p("The product is also why third-order structure becomes possible. If i "
      "and j are aligned, and j and k are aligned, then i and k are likely to "
      "be aligned too — the geometry produces transitivity for free, without "
      "anyone writing a triangle term."),
    p("**R is the rank**: how many dimensions each vector has. R = 0 is the "
      "additive model of [page 3](#p-the-basic-selection-model); the "
      "multiplicative term vanishes."),

    h3("B4.3 The same geometry, a different job"),
    p("If you have read [§A4.3](#p-the-latent-space-influence-model), this is "
      "the picture you already have: actors placed in a space, with proximity "
      "standing for the propensity to tie. Nothing about the geometry "
      "changes."),
    p("**What changes is the job it is doing**, and a reader coming from Path "
      "A will carry the wrong intuition across if nobody says so. There, the "
      "latent positions were a **nuisance control** — something we wanted out "
      "of the exposure estimate, so that β₂ was not contaminated by "
      "homophily. Here the positions are **part of the model of tie "
      "formation**. They are not being removed from a claim; they are a claim: "
      "that there is latent structure organising who ties to whom."),
    p("One thing carries over unchanged. **Only the product is identified.** "
      "`U` and `V` can be rotated, reflected and rescaled without changing a "
      "single predicted tie, so an individual coordinate means nothing. rnetis "
      "reports `UV'`, the posterior mean of the **product**, and deliberately "
      "does not hand you `U` and `V` separately. Averaging them separately "
      "would average across each draw's arbitrary rotation and shrink the "
      "structure toward nothing — a computation that looks reasonable and "
      "destroys the quantity it claims to summarise."),

    h3("B4.4 Choosing R"),
    p("Path A had no criterion for `d`. Here there is one, and it is the "
      "reason [§B3.7](#p-the-basic-selection-model) came before this page: "
      "**raise R until the statistics the model was missing stop moving, and "
      "no further.**"),
    p("The temptation to resist is treating a better fit to the observed "
      "network as evidence. A higher R always fits the observed network "
      "better — more parameters always do. What the posterior predictive "
      "check asks is different: whether the model now *generates* networks "
      "like yours. That can stop improving while in-sample fit keeps rising, "
      "and the point where it stops is the answer."),
    p("The costs are real. Each dimension adds 2n parameters — a vector per "
      "actor on each side — estimated from the same n(n-1) observations, so "
      "the latent structure gets noisier as R rises. Chains take longer and "
      "mix worse. And the practical ceiling arrives quickly: on a network of "
      "twenty actors, R = 2 is already asking for eighty latent parameters."),
    p("The honest default is to start at R = 1 or 2, run the check, and go up "
      "only if the triadic statistics are still outside their ranges."),

    h3("B4.5 Running it"),
    p("The same formula, with `R = 2`. Note that `dyad(same_unit)` is dropped "
      "here: §B3 found nothing for it, and leaving it in would make the "
      "comparison below about two changes at once:"),
    block("b-fit-multiplicative"),
    p("Every coefficient has moved, and all in the same direction — away from "
      "zero. `snd_tenure` from 0.522 to 0.636, `rcv_tenure` from 0.407 to "
      "0.476, `prior_tie` from 1.741 to 2.058, and the intercept from -1.570 "
      "to -1.759."),

    h3("B4.6 What changed, and why coefficients moved"),
    p("That across-the-board shift is the result readers are least prepared "
      "for, and it is not a sign that something went wrong. It is the scale "
      "moving."),
    p("A probit identifies the latent scale by fixing the residual variance "
      "at 1. Adding multiplicative effects moves variance out of the residual "
      "and into the structured part of the model — but the residual stays "
      "pinned at 1 by definition, so everything else must grow to compensate. "
      "**The coefficients are measured against a different yardstick in the "
      "two fits.**"),
    p("This is exactly what [§B3.4](#p-the-basic-selection-model)'s scale "
      "warning meant by *they shift with the modelled heterogeneity, so "
      "comparing them across specifications is not like comparing linear "
      "regression coefficients*. The right comparison is on the probability "
      "scale, through `selection_effect()`, where the rescaling has been "
      "undone."),
    p("The variance components make the mechanism visible. Here they are for "
      "the additive fit:"),
    block("b-vc"),
    p("Sender variance 0.239, receiver variance 0.192, and the dyadic "
      "residual pinned at **exactly 1.000** — that is the identification "
      "constraint, not an estimate. It accounts for 69.9% of dyadic variance "
      "in this fit. Multiplicative effects claim some of that back, and "
      "everything measured against it rescales accordingly."),
    p("Also worth reading: the **within-dyad residual correlation of -0.109**. "
      "That is reciprocity beyond what the covariates explain, and it is "
      "slightly negative here — i naming j makes j naming i marginally *less* "
      "likely than the model otherwise expects, once tenure and prior ties "
      "are accounted for."),
    fig("b-plot-mult", "The rotation-invariant product U V' for each ordered "
                       "pair. Only the product is identified, so this is the "
                       "only view of the latent structure that means "
                       "anything."),
    fig("b-plot-posterior", "The posterior for each term. These are draws, "
                            "not a normal curve fitted to an estimate and a "
                            "standard error."),
    h3("B4.7 The same five statistics, again"),
    p("Now the question the page exists to answer. Run "
      "[§B3.7](#p-the-basic-selection-model)'s check on this fit:"),
    block("b4-gof"),
    p("And side by side with the additive model, which is the comparison that "
      "actually settles it:"),
    block("b4-gof-compare"),
    fig("b4-plot-gof",
        "The AME fit's five statistics, drawn the same way: dot for the "
        "observed network, grey bar for the central 95% of what this model "
        "generates. Put it beside the additive panel on "
        "[the previous page](#p-the-basic-selection-model) — the bars shift "
        "slightly, the dots are in exactly the same places, and every dot was "
        "already inside."),
    p("**Nothing moved.** `trans.dep` went from z = 0.33 to 0.40, `cycle.dep` "
      "from -0.89 to -0.68, and both were comfortably inside their ranges to "
      "begin with. The multiplicative effects had nothing left to explain, "
      "because the additive model was not missing anything to explain."),
    p("So on this dataset the answer is: **R = 2 was not needed.** That is a "
      "result, it was reached by running a check rather than by assuming, and "
      "it is the one a reader should be most prepared to get. Eighty extra "
      "latent parameters bought no improvement in the thing they exist to "
      "improve."),
    p("Had the triadic statistics moved from outside their ranges to inside, "
      "the multiplicative effects would have earned their keep and you would "
      "have watched a modelling decision made on evidence. Had they stayed "
      "outside, R = 2 would have been too small and R = 3 the next thing to "
      "try. All three outcomes are informative, and the check is what "
      "distinguishes them."),
    note("**What to report.** If the check says the additive model suffices, "
         "report the additive model — it is simpler, its coefficients are on "
         "a more stable scale, and its chains mix better. Say that you fitted "
         "the multiplicative version and that it did not improve the "
         "posterior predictive fit. That sentence is worth more than the "
         "larger model would have been, because it tells a reader you looked."),
)

page(
    GB, "B5 · Multilevel", "Several groups, and one hierarchical model",
    '<p class="lede">Twelve teams is twelve networks, not one big one. There '
    "are three ways to handle that and they are different models, not three "
    "spellings of one.</p>",

    h3("B5.1 Why several networks are not one network"),
    p("A Selection model is fitted to a network: a set of actors, all of whom "
      "could in principle have named each other. Twelve teams do not form "
      "one such set. Nobody in team 3 could have named anybody in team 7, so "
      "those pairs are not non-ties — they are **not pairs at all**, and "
      "stacking the teams into one adjacency matrix would invent 90% of its "
      "cells as structural absences."),
    block("b-teams"),
    p("96 nodes **in 12 groups**. The object keeps the groups separate, and "
      "every route below respects that. What differs is what happens to the "
      "twelve sets of coefficients afterwards."),

    h3("B5.2 No pooling: twelve separate fits"),
    p("The honest first move is to fit each network on its own and look at "
      "the spread. `pool_beta = FALSE` does that and keeps the results "
      "apart:"),
    block("b-separate"),
    p("Two groups here rather than twelve, for brevity. Their coefficients "
      "are strikingly similar — `snd_tenure` 0.462 and 0.456, `prior_tie` "
      "1.787 and 1.790 — which is the ideal case for anything that follows. "
      "Had they differed sharply, that would be the finding, and no amount of "
      "pooling would be an improvement on reporting it."),
    fig("b-plot-groups",
        "One row per group per term, dot for the estimate and line for the "
        "interval, on the latent probit scale. The reading is the **spread "
        "down each term**: groups whose intervals overlap heavily are "
        "consistent with one common coefficient, and pooling them will "
        "sharpen it. Groups whose intervals barely overlap are telling you "
        "the coefficient differs, and pooling would average away the finding "
        "rather than estimate it."),
    note("**Look at this plot before pooling anything.** Every method below "
         "combines per-group estimates into one, and all of them are a bad "
         "idea if the groups genuinely disagree. Pooling is a way of "
         "sharpening an estimate you believe is common, not a way of "
         "resolving a disagreement."),

    h3("B5.3 Complete pooling: one coefficient, precision-weighted"),
    p("If you are willing to assume the coefficient is the same in every "
      "group, combine the per-group estimates by inverse-variance weighting — "
      "the classical meta-analytic approach (Cochran; Hedges & Olkin). Each "
      "group contributes in proportion to how precisely it was estimated:"),
    block("b-pooled"),
    p("One set of coefficients, with intervals narrower than any single "
      "group's. And three sentences of caveat printed with them, each worth "
      "taking seriously:"),
    ul("*This assumes beta is COMMON across groups* — **there is no "
       "between-group variance component**, so heterogeneity is assumed away "
       "rather than estimated. If the groups differ, this does not tell you.",
       "*Pooling is coefficient-wise, so no covariance between coefficients "
       "survives it* — per-coefficient intervals are valid; **joint tests are "
       "not**, because the covariance they would need was never computed.",
       "It is two-stage: each group is fitted, then the results are combined. "
       "Nothing flows back from the pooling to the fits."),

    h3("B5.4 Partial pooling: one hierarchical model"),
    p("The third option fits **all groups in one model**, with coefficients "
      "drawn from a common distribution. Groups with little information get "
      "pulled toward the overall mean; groups with a lot stay where they are. "
      "This is not a different way of combining results — it is a different "
      "model, and it is fitted in one pass:"),
    block("b-hier"),
    p("Read the regime column, which is the part with no counterpart in the "
      "other two routes. `prior_tie` is **shared**: one coefficient, common "
      "to all twelve teams. `intercept` and `snd_extraversion` are "
      "**random**: each team has its own, drawn from a distribution whose "
      "mean is what is reported here. You choose which is which, through "
      "`selection_pooling(random = ...)`, and that choice is a substantive "
      "claim about what varies between teams."),
    p("Note the backend line: **rnetis hierarchical sampler**, not "
      "`amen::ame()`. Every group is in one chain. That has a consequence "
      "§B5.6 comes back to."),
    fig("b-hier-panel-groups", "Partial pooling in one picture: each team's "
                               "estimate, shrunk toward the common mean by an "
                               "amount that depends on how much that team "
                               "had to say."),
    p("`pool_beta` and `pooling` select different models, so passing both is "
      "refused rather than silently resolved — one combines coefficients "
      "after fitting, the other fits one model over everything, and there is "
      "no sensible way to do both."),
    note("**The hierarchical route is experimental**, and this is the place "
         "you need to know rather than a scope page at the end. It is this "
         "package's own construction — see `?hierarchical_selection` — not a "
         "published method with an established track record. It is "
         "documented, tested and usable. It has not been validated for "
         "statistical performance against alternatives, and a conclusion that "
         "rests on it alone should say that it does."),

    h3("B5.5 A refusal about group-level predictors"),
    p("A natural next question is whether a property **of the team** — its "
      "size, its age, its function — predicts tie formation. Written the "
      "obvious way, it is refused, and the refusal repays reading:"),
    refusal("b-group-refuse"),
    p("The diagnosis is exact. Within any one group, a group-level variable "
      "takes one value, so it is **perfectly collinear with that group's "
      "intercept**. The variation is *between* fits, not inside any of them. "
      "And it closes off the obvious workaround before you try it: "
      "`pool_beta` does not rescue it either, because pooling combines "
      "coefficients after each group has already been fitted."),
    p("Then it names the route that does work: make the group-level variable "
      "a **moderator of a random intercept** in the hierarchical model, where "
      "between-group variation is the thing being modelled and so is "
      "available to be explained."),

    h3("B5.6 Where the posterior predictive check stops"),
    p("[§B3.7](#p-the-basic-selection-model) taught you a check. On a "
      "hierarchical fit it refuses, and you should know why rather than "
      "assume it broke."),
    p("The hierarchical sampler does not simulate a network at every scan. "
      "The simulation does not affect the posterior — it is purely a "
      "reporting device — so computing it would consume random numbers and "
      "chain time for nothing. That is a deliberate design decision, and the "
      "consequence is that `netis_gof()` has no posterior predictive draws to "
      "read on such a fit and says so by name rather than returning "
      "something empty."),
    p("The practical answer: **fit the groups separately and check them "
      "there.** A per-group fit goes through `amen::ame()`, which does "
      "simulate, so you get the five statistics for each group. If the "
      "additive model reproduces each network on its own, the hierarchical "
      "model built from the same structure is not likely to be the thing "
      "going wrong."),
)

page(
    GB, "B6 · Other points", "Beyond Path B: Selection",
    '<p class="lede">The mirror of Path A’s last page, including the one '
    "place the mirror breaks.</p>",

    h3("B6.1 Missing dyads"),
    p("A Selection model is unusually exposed to the "
      "[zero-versus-missing](#p-the-data-people-ties-and-waves) distinction, because "
      "the non-ties are not background — they are **the comparison group**. "
      "The model is explaining which of the possible ties exist, so every "
      "pair you did not ask about is a row it would otherwise have used."),
    p("Treat an unobserved pair as a measured zero and you tell the model "
      "that someone was asked and declined. Do that systematically — a "
      "bounded name generator capped at five, say — and you have manufactured "
      "non-ties for exactly the pairs most likely to have been ties. The "
      "resulting coefficients are not approximately right."),
    p("rnetis does not impute, here or anywhere. What it does is make the "
      "declaration explicit at `netis_data()` and keep it consistent "
      "downstream. If your design has a cap, say so, and prefer a presence "
      "rule that marks the unasked pairs as undetermined over one that calls "
      "them zero."),

    h3("B6.2 Checking a design before collecting it"),
    p("Path A has `simulate_influence()`. Path B's equivalent is to build a "
      "small synthetic network with a known structure and fit the model you "
      "intend to use — which is exactly what "
      "[§B3.7](#p-the-basic-selection-model)'s clustered contrast does."),
    p("That is worth doing for your own design, and it answers a question "
      "nothing else will: **with n actors and the effect you expect, will the "
      "interval be narrow enough to be worth reporting?** Remember that the "
      "sample size grows with n², so twenty actors is 380 pairs and "
      "forty is 1,560; the gain from a modest increase in recruitment is "
      "larger here than most people's intuition from ordinary regression "
      "suggests."),

    h3("B6.3 Sensitivity analysis"),
    p("Three things worth varying, each answering a different “would "
      "this change my mind”."),
    ul("**The chain.** Refit with a different `seed` and confirm the "
       "coefficients land in the same place. If they do not, the chain is "
       "too short and no amount of interpretation will fix it.",
       "**The rank.** [§B4.4](#p-adding-multiplicative-effects)'s procedure: "
       "run the posterior predictive check across R and see where it stops "
       "moving.",
       "**The presence rule.** If your network's zeros are at all ambiguous, "
       "refit under the alternative declaration. "
       "[§A2.7](#p-the-data-people-ties-and-waves) showed a density going from 1 to "
       "0.33 on that choice alone; a coefficient can move just as far."),

    h3("B6.4 Binary or valued, directed or symmetric"),
    p("[Path A's §A6.5](#p-running-the-influence-model) shows what these two "
      "declarations do to **exposure**, where the consequence is a column of "
      "numbers. Here the consequence is in the model, and it is larger."),
    h4("Directed or symmetric: how many effects each actor gets"),
    p("An undirected relation — co-authorship, co-presence, mutual "
      "friendship — stores each pair once. Fitting it as though it were "
      "directed is refused, not warned about:"),
    refusal("k-undirected-refuse"),
    p("The reason is the one that matters: *fitting an undirected relation as "
      "directed would treat the two directions of one tie as two separate "
      "observations*. You would double your apparent sample size with copies "
      "of data you already had."),
    p("Fitted as what it is, the model changes shape:"),
    block("k-symmetric"),
    p("**One additive effect per actor**, not a sender effect and a receiver "
      "effect — and the output names it that way rather than reporting "
      "`va` and leaving you to wonder where `vb` went. That is the whole "
      "difference, and it is a substantive one: in a symmetric model there is "
      "no such thing as being expansive but unpopular, because there is no "
      "direction for those to differ along. [§B3.2](#p-the-basic-selection-model)'s "
      "sender-against-receiver plot has nothing to draw."),
    h4("Binary or valued: what is being modelled"),
    p("Everything on Path B so far has modelled **whether** a tie exists. If "
      "you measured **how much** — how many times they interacted, how long, "
      "how strongly — then `family = \"nrm\"` models the values themselves:"),
    block("k-valued"),
    p("Three things changed. The family line reads **valued tie (normal)** "
      "rather than binary probit. The coefficients are on the **outcome's own "
      "scale** — `snd_tenure` of 1.20 means 1.20 more contacts, which needs "
      "no conversion and no `selection_effect()` call to report. And the "
      "latent-scale warning from [§B3.4](#p-the-basic-selection-model) is "
      "absent, because there is no latent scale: nothing was squashed through "
      "Φ."),
    p("The temptation to resist is binarising a valued relation to get the "
      "familiar probit. You can — `binarize =` does it — but you are "
      "discarding the measurement you paid to collect, and "
      "[§A6.5](#p-running-the-influence-model) shows how much a threshold can "
      "move things. Ask for the binary version only if tie *presence* is "
      "genuinely your question."),
    p("And the two options do not combine, which rnetis says rather than "
      "silently ignoring one of them:"),
    refusal("k-binarize-refuse"),

    h3("B6.5 Several relations"),
    p("Advice, friendship and reporting lines over the same actors are three "
      "networks. The Selection equivalent of Path A's `multiplex()` is "
      "simpler and usually better: put the other relation in as a **dyadic "
      "covariate**."),
    codeonly(
        "tie ~ sender(tenure) + receiver(tenure) + dyad(friendship) + prior_tie()"
    ),
    p("That asks directly whether a friendship tie predicts an advice tie, "
      "and gives you a coefficient for it rather than a combination rule you "
      "chose. It is a genuinely different question from Path A's, where the "
      "networks are conduits rather than predictors, and the asymmetry "
      "between the paths here is the right one."),

    h3("B6.6 Moderation is refused"),
    p("Path A's [§A6.1](#p-running-the-influence-model) covers interactions. "
      "There is no counterpart here: **`fit_selection()` refuses "
      "interactions unconditionally.**"),
    p("This section exists to say so rather than to leave a gap, because a "
      "reader arriving from Path A will go looking. The restriction is "
      "deliberate. An interaction in a probit with sender, receiver and "
      "dyadic heterogeneity is not the quantity it looks like — the latent "
      "scale shifts with the modelled heterogeneity "
      "([§B4.6](#p-adding-multiplicative-effects)), so a product term is not "
      "interpretable as “the effect of x when z is high” in the way "
      "the same syntax means in a linear model."),
    p("What to do instead: fit the model separately in subgroups and compare, "
      "which makes the scale question visible rather than hiding it inside a "
      "coefficient; or, with enough groups, use the hierarchical model with a "
      "**moderator on a random coefficient**, which is the construction "
      "[§B5.5](#p-several-groups-and-one-hierarchical-model)'s refusal points "
      "at."),
)

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
      "([§B5](#p-several-groups-and-one-hierarchical-model)) and the "
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
        ["`group(z)`", "a group-level predictor — "
         "[refused as written](#p-several-groups-and-one-hierarchical-model)"],
    ]),
    note("`R` is not a formula term: it is an argument to `fit_selection()`, "
         "because it sets the rank of the multiplicative effects rather than "
         "adding a named predictor. See "
         "[§B4.4](#p-adding-multiplicative-effects)."),
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
      "[§A2.9](#p-the-data-people-ties-and-waves) reads one in full."),
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
       "outcomes on Path A are not supported."),
    p("Two families of method are **absent because of that scope, not "
      "because of any doubt about them**: dynamic-panel estimators, which "
      "would address the bias "
      "[§B1.3](#p-selection-the-question) describes, and distance-shell "
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
      "[§A2.7](#p-the-data-people-ties-and-waves) works through the difference, and the "
      "three readings give genuinely different densities."),

    h3("“Row-normalization requested but n node(s) have no reference-set members”"),
    p("Someone has no alters, so the denominator is zero. Decide what an "
      "isolate means in your design: `isolate_rule = \"na\"` (undefined, and "
      "usually right), or `\"zero\"` (they sit at zero on the outcome's "
      "scale, which is rarely what you want). "
      "[§A3.2](#p-running-the-influence-model)."),

    h3("“This netis_data object has N groups, but the formula has no random-effects term”"),
    p("Add `(1 | group)`, or subset to one group's rows and rebuild. "
      "[§A5.1](#p-the-multilevel-influence-model)."),

    h3("“This data has N waves … and this version fits one transition”"),
    p("Name the transition: `waves = c(1, 2)`. The refusal exists because a "
      "result reported without naming its transition reads as though the "
      "whole panel were modelled. "
      "[§B1.3](#p-selection-the-question)."),

    h3("“symmetrize() requires rule =”"),
    p("On a weighted network, collapsing the two directions is several "
      "different operations. Pick one: `\"max\"`, `\"min\"`, `\"sum\"`, "
      "`\"mean\"`, `\"any\"`, `\"both\"`."),

    h3("“A Selection fit has no log-likelihood”"),
    p("`logLik()`, `AIC()` and `BIC()` are not defined for these models. Use "
      "`pseudo_r2()` for explained variation and `netis_gof()` for whether "
      "the model reproduces the network. "
      "[§B3.6](#p-the-basic-selection-model)."),

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
      "[§B4.4](#p-adding-multiplicative-effects) gives a criterion for "
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
