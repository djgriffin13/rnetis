# -*- coding: utf-8 -*-
"""The Selection path, the reference pages, and the help pages."""

from build import (
    page, p, h3, h4, ul, ol, quote, note, table, code, result, block,
    refusal, codeonly, fig, card, cards, rare, filterbox, dia, eq,
)
import diagrams as D

# ===========================================================================
# Path B
# ===========================================================================
page(
    "Path B · Selection", "The question", "Selection: the question",

    p("A Selection model turns the Influence question around. There, the "
      "network was given and the outcome was a person's characteristic. Here "
      "the **tie itself is the outcome**, and the predictors are properties of "
      "the sender, the receiver, the pair, and the relation's own history."),

    quote("Path A asked: *does what my alters do change what I do?* "
          "Path B asks: *does who we are explain who is tied to whom?* "
          "They are the two halves of the same confound — which is why "
          "Path A's [latent-space page](#p-homophily-and-the-latent-space) had "
          "to fit a selection model before it could trust an influence "
          "coefficient."),

    p("The outcome is binary: for every ordered pair, a tie or no tie. So "
      "everything on these pages is about predicting a 0/1 relation, and the "
      "quantities that matter are probabilities."),

    h3("The path from here"),
    ol("**[The data](#p-the-selection-dataset)** — what a dyadic dataset "
       "looks like, how to build a pair attribute, and why a fit models one "
       "transition.",
       "**[Specify a model](#p-specify-a-model)** — the model's equation, "
       "the role grammar, and why a bare covariate is an error.",
       "**[Run the model](#p-run-the-model)** — the Markov chain, the "
       "settings worth changing, and how to tell whether it worked.",
       "**[Effect sizes](#p-effect-sizes)** — turning a probit coefficient "
       "into a probability contrast anyone can read, and choosing which one.",
       "**[Several groups](#p-several-groups)** — fitting each network "
       "separately and combining, and what pooling assumes away.",
       "**[One hierarchical model](#p-one-hierarchical-model)** — random "
       "intercepts and slopes across groups, in one chain.",
       "**[Beyond this path](#p-beyond-path-b)** — what else the Selection "
       "side can do, and which parts are experimental."),
)

page(
    "Path B · Selection", "1 · The data", "The Selection dataset",

    p("A Selection model reads the same `netis_data` as an Influence model — "
      "nodes, ties, waves — but it uses it differently, so it is worth "
      "looking at the shape before fitting anything. The vocabulary from "
      "[Nodes, ties and waves](#p-nodes-ties-and-waves) is the vocabulary "
      "here: `source` and `target`, sender and receiver, mutual and "
      "asymmetric, and the difference between a zero and an absent row."),

    h3("Two data frames, as always"),
    p("`demo_selection` is nothing special. Two tables: twenty colleagues "
      "with a tenure score, and a `tie` relation observed twice."),
    block("b2-raw"),
    note("The node table has no `wave` column, because `tenure` does not "
         "change. The tie table does, because the relation does."),

    h3("The shape that matters"),
    p("An Influence dataset has one row per person per wave. A dyadic dataset "
      "has one row per **ordered pair** per wave, and that is a much bigger "
      "table than people expect:"),
    block("b2-shape"),
    quote("Twenty people make 20 × 19 = 380 ordered pairs, and both "
          "waves were observed, so the tie table has 760 rows. Of the 380 "
          "pairs at a wave, most are zeros. That is normal: networks are "
          "sparse, and the zeros are the majority of the data a selection "
          "model learns from."),
    p("Which is exactly why the declaration from the earlier page matters "
      "here more than anywhere. These zeros are **measured**: every pair was "
      "asked about and most answered no. A dataset where the absent pairs "
      "were simply never collected is a different dataset, and would give "
      "different coefficients from the same fit call."),

    h3("Building a pair attribute"),
    p("`sender()` and `receiver()` read node variables, which you already "
      "have. `dyad()` reads a **tie** variable: one value per ordered pair, a "
      "property of the pair rather than of either person. Those you usually "
      "have to construct. Here, whether the two colleagues are in the same "
      "unit of the organisation:"),
    block("b2-pair"),
    p("Then rebuild the object with the new column declared alongside the "
      "relation:"),
    block("b2-build"),
    note("`zero_meaning = \"measured_zero\"` on both. For `tie` it says a "
         "zero is an observed non-tie; for `same_unit` it says that two "
         "people in different units really are coded 0, rather than that the "
         "pair was never classified."),

    h3("Moving data in and out"),
    p("Nothing about this shape locks you in. The object came from two data "
      "frames and gives them back; it converts to and from `igraph` and "
      "`statnet` a wave at a time. What it will not do is convert silently "
      "when the conversion needs a decision from you:"),
    refusal("b2-igraph-refuse"),
    p("An `igraph` graph has edges, so exporting one means saying which rows "
      "of a complete-dyadic table *are* edges. Say it, and the round trip "
      "works:"),
    block("b2-igraph"),
    note("Read the warnings rather than skipping them: they name exactly what "
         "the graph cannot carry — the other tie variable's declarations, "
         "and the presence rule itself, which has no serialized form. "
         "A round trip through `igraph` re-infers what it cannot restore. "
         "[Importing and exporting](#p-importing-and-exporting) has the full "
         "list of converters."),

    h3("Why a fit models one transition"),
    p("Two waves of a relation give **one** transition, and that is what a "
      "Selection model fits. With three or more waves present, rnetis will "
      "not choose which:"),
    refusal("b2-waves3-refuse"),
    dia(D.one_transition(),
        "Two waves, one modelled transition. `prior_tie()` uses the earlier "
        "network as an initial condition and the later one as the outcome. A "
        "third wave offers a second transition, and stacking it on the first "
        "is a different estimation problem rather than more of the same data."),

    p("This is a scope decision with a reason, and the reason is worth "
      "knowing before you wish it away."),
    cards(
        card("Dynamic-panel bias",
             p("A lagged outcome on the right-hand side leaves you two "
               "choices, and neither is free. Leave persistent actor "
               "heterogeneity in the error, and the lagged term is correlated "
               "with it. Sweep the heterogeneity out with a within "
               "transformation, and the lagged term picks up the bias "
               "Nickell (1981) describes — of order 1/T, so worst in "
               "exactly the short panels network studies have."),
             p("The estimators built to resolve this — Arellano–Bond, "
               "Blundell–Bond and their successors — need more "
               "transitions than a two-wave design supplies. **rnetis does not "
               "implement them**, which is why the assumption ledger reads "
               "`unaddressed` for persistent actor heterogeneity."), num=1,
             open=True),
        card("The initial-conditions problem",
             p("In a dynamic model of a binary outcome, the first observed "
               "state is not a neutral starting point: it was generated by "
               "the same process, and treating it as fixed biases the "
               "persistence parameter (Heckman 1981). `prior_tie()` conditions "
               "on wave 1 and so takes exactly that assumption — which "
               "is defensible for one transition and gets steadily less so as "
               "you stack more."), num=2),
    ),
    quote("So rnetis fits one transition and makes you name it, rather than "
          "stacking several and returning a number that looks like a panel "
          "estimate. A wrong number is worse than a narrow one."),
    p("Naming the transition is how you proceed. It is still a model, though, "
      "and the data still has to support it — here waves 2 and 3 were "
      "built as copies of each other, and a lagged relation identical to the "
      "response is not a predictor:"),
    refusal("b2-waves3-named-refuse"),
    note("Wave *depth* is not restricted the way wave *count* is: lagged "
         "attributes and exposure histories may reach as far back as the data "
         "goes. It is the **outcome** that is one occasion per fit. "
         "[Scope](#p-scope-what-is-not-here) is the full statement."),
)

page(
    "Path B · Selection", "2 · Specify a model", "Specify a model",

    p("`fit_selection()` fits an **additive and multiplicative effects** model "
      "(Hoff 2005, 2021) through `amen::ame()`. Before the grammar, the "
      "model — because the grammar is a direct transcription of it, and "
      "every argument on the next pages is an argument about one of these "
      "terms."),

    h3("The model"),
    p("For each ordered pair, a latent propensity is built, and a tie exists "
      "if the propensity crosses zero:"),
    eq('<i>y*<sub>ij</sub></i> <span class="op">=</span> <i>μ</i> '
       '<span class="op">+</span> <i>β<sub>s</sub>x<sub>i</sub></i> '
       '<span class="op">+</span> <i>β<sub>r</sub>x<sub>j</sub></i> '
       '<span class="op">+</span> <i>β<sub>d</sub>w<sub>ij</sub></i> '
       '<span class="op">+</span> <i>a<sub>i</sub></i> '
       '<span class="op">+</span> <i>b<sub>j</sub></i> '
       '<span class="op">+</span> <i>u<sub>i</sub></i>′<i>v<sub>j</sub></i> '
       '<span class="op">+</span> <i>ε<sub>ij</sub></i>',
       "x_i is the sender's attribute, x_j the receiver's, w_ij a property of "
       "the pair; a_i and b_j are the actor effects; u_i′v_j is the "
       "multiplicative term, present only if you ask for it."),
    eq('tie<sub><i>ij</i></sub> <span class="op">=</span> 1 '
       '<span class="op">if</span> <i>y*<sub>ij</sub></i> '
       '<span class="op">&gt;</span> 0,  '
       '<span class="op">so</span>  P(tie<sub><i>ij</i></sub>) '
       '<span class="op">=</span> <i>Φ</i>(<i>y*<sub>ij</sub></i> '
       '<span class="op">−</span> <i>ε<sub>ij</sub></i>)',
       "Φ is the standard normal CDF. This is a probit, which is why the "
       "coefficients are not odds ratios and why effect sizes need their own "
       "page."),

    dia(D.selection_model(),
        "The model for one ordered pair. Each predictor enters through a "
        "named role, two actor effects absorb how much each person sends and "
        "is sought out, and the sum passes through a probit link to a "
        "probability."),

    p("The two parts of the name are doing different jobs, and it is worth "
      "being clear which you are relying on:"),
    cards(
        card("The additive part — aᵢ and bⱼ",
             p("Every actor gets a **sender effect** and a **receiver "
               "effect**. Without them, a person who nominates everybody makes "
               "every covariate they score highly on look important, and a "
               "person nobody nominates drags every covariate they score "
               "highly on down. These are not nuisance parameters to be "
               "tolerated — they are the reason a dyadic model is not "
               "just a logistic regression on pairs."),
             p("They also carry the within-dyad residual correlation, which is "
               "what reciprocity in the *residuals* means here, as distinct "
               "from `prior_reciprocity()` as a predictor."), num=1, open=True),
        card("The multiplicative part — uᵢ′vⱼ",
             p("Optional, and off by default (`R = 0`). It adds latent factors "
               "that capture third-order structure additive terms cannot: "
               "clustering, and the “my friends' friends are my "
               "friends” pattern. Each dimension costs chain time, so turn "
               "it on when you have a reason."), num=2),
    ),

    quote("**This is a Bayesian model fitted by MCMC, not a regression.** "
          "Everything on the following pages is a posterior summary, the chain "
          "has to be looked at, and a fit takes seconds to minutes rather than "
          "milliseconds."),

    h3("Every predictor names its role"),
    p("A variable attached to a person can enter as a property of the sender, "
      "of the receiver, or both — and these are different hypotheses about "
      "the same variable. So rnetis will not choose:"),
    refusal("b-bare-refuse"),

    table(["Term", "Asks"], [
        ["`sender(tenure)`", "Does a longer-tenured actor **send** more ties?"],
        ["`receiver(tenure)`", "Does a longer-tenured actor **receive** more?"],
        ["`dyad(same_unit)`", "Is a property of the *pair* associated with a tie? "
         "Held as a tie variable — "
         "[built on the previous page](#p-the-selection-dataset)."],
        ["`prior_tie()`", "Did the relation exist at the previous wave?"],
        ["`prior_reciprocity()`", "Did the *reverse* relation exist at the "
         "previous wave?"],
        ["`group(size)`", "A group-level predictor. Only meaningful in a "
         "[hierarchical model](#p-one-hierarchical-model)."],
    ]),
    note("Putting both `sender(x)` and `receiver(x)` in is normal and often "
         "right: it asks whether the attribute matters for sending, for "
         "receiving, or both, and lets the data answer separately."),

    rare("Lagging an attribute as well",
         p("`sender()`, `receiver()` and `dyad()` all take `lag`. The default "
           "is `lag = 0`, the outcome wave, which is fitted and recorded with "
           "a design warning you will see on the next page. `lag = 1` reads "
           "the previous wave instead, and is the more defensible predictor "
           "whenever the attribute could plausibly be a consequence of the "
           "tie.")),
)

page(
    "Path B · Selection", "3 · Run the model", "Run the model",

    note("A note on the word. **“Fit” here is a verb** — "
         "running the chain. It is not the “fit” of fit statistics: a "
         "Selection model has no R-squared or AIC, and the last section of "
         "this page says why. Checking that the run *worked* is a question "
         "about the Markov chain, not about model fit."),

    h3("One call, and the settings worth knowing"),
    p("Everything the model needs is in one call. The chain settings are "
      "arguments rather than defaults to accept blindly, so they are written "
      "out here with what each one does:"),
    block("b2-tuning"),

    table(["Argument", "What it does", "Where to start"], [
        ["`nscan`", "Draws kept after burn-in. The main lever on precision "
         "and on how long you wait.",
         "5,000 while exploring; more once the specification is settled."],
        ["`burn`", "Draws discarded first, before the chain has settled.",
         "1,000."],
        ["`odens`", "Thinning: keep one draw in `odens`. Reduces "
         "autocorrelation between stored draws and the memory they take.",
         "10."],
        ["`R`", "Dimension of the multiplicative term. `0` is additive "
         "effects only.", "0 until you have a reason."],
        ["`chains`", "Independent chains. Two or more are what makes R-hat "
         "mean anything, since it compares chains that started apart.",
         "1 while exploring; 2 or more to report."],
        ["`seed`", "Reproducibility. The same seed and settings give the same "
         "draws.", "Always set it."],
        ["`waves`", "Which transition, when more than two waves are present.",
         "Named explicitly, or rnetis refuses."],
    ]),
    note("These settings took about fifteen seconds on the machine this page "
         "was built on. [Slow fits](#p-slow-fits-and-mcmc-settings) is the "
         "page for when that stops being true."),

    h3("What the summary tells you"),
    block("b-summary"),
    quote("The summary says it itself, and it is worth taking literally: those "
          "numbers are **not** probabilities, not odds ratios, and not "
          "comparable across specifications. There is no mental arithmetic "
          "that turns a probit coefficient into a magnitude — the "
          "[next page](#p-effect-sizes) computes the quantity you actually "
          "want from it."),

    p("The transition the fit analyzed is in the header, and it is also a "
      "value rather than only printed text — worth asserting in a script "
      "rather than reading off a summary:"),
    block("b-waves"),

    p("And the coefficient table as an ordinary data frame, which is what you "
      "want for a manuscript table — the same surface, with the same "
      "`role` and `scale` columns, as "
      "[Path A, page 4](#p-read-the-results):"),
    block("b-estimates"),

    p("A credible interval here is not a confidence interval. It is a range "
      "the posterior puts 95% of its mass in, given the model and the priors, "
      "and it is read directly: the parameter is probably in there. An "
      "interval that excludes zero is the Bayesian analogue of the finding you "
      "would report, not a hypothesis test."),

    h3("Design findings, recorded at fit time"),
    p("The summary ended with warnings. They are structured data rather than "
      "text, so they can be inspected and reported rather than just read:"),
    block("b-diagnostics"),
    p("Each one carries a full statement of what it found and why it matters:"),
    block("b-diagnostics-detail"),
    note("`tenure` is static in this dataset, so `lag = 1` would read the same "
         "values; the warning still fires, because the design claim is about "
         "measurement order, not about whether the numbers happen to differ."),

    h3("Did the chain work?"),
    p("A posterior summary means nothing if the chain did not explore the "
      "posterior. Two panels answer that, and they are the two to look at on "
      "every fit."),
    block("b-plot-trace"),
    fig("b-plot-trace",
        "Traces. You want a stationary band — a fuzzy horizontal "
        "caterpillar. A trend, a drift, or long excursions mean the chain has "
        "not settled and the summary above is premature."),
    block("b-plot-mcmc"),
    fig("b-plot-mcmc",
        "Effective sample size and autocorrelation. A low effective sample "
        "size means the draws you kept carry less information than their "
        "number suggests, and the interval is less reliable than it looks."),

    rare("The other panels, and the tables behind them",
         block("b-plot-posterior"),
         fig("b-plot-posterior", "Posterior densities for each coefficient."),
         block("b-plot-coef"),
         fig("b-plot-coef", "Coefficients with 95% credible intervals."),
         p("Every panel has its table behind it:"),
         block("b-plot-data-trace"),
         p("And the actor effects the additive part estimated:"),
         block("b-plot-actor"),
         fig("b-plot-actor", "Each actor's sender and receiver effect.")),

    h3("Where the variance sits"),
    p("The actor effects are part of the model, so their variances are "
      "reported alongside the coefficients. This is the answer to “how "
      "much of this network is just that some people send more?”:"),
    block("b-vc"),
    note("On a binary probit the dyadic residual variance is fixed at 1 for "
         "identification. The shares are relative to that, which is also why "
         "a latent-scale share here is not comparable with a linear model's "
         "R-squared."),

    h3("No R-squared, no AIC, and why"),
    p("Asking a Selection fit for a log-likelihood does not give you a "
      "worse answer. It gives you a refusal:"),
    refusal("b2-nologlik-refuse"),
    p("What you get instead is a one-row summary of what was fitted, with the "
      "number of draws rather than an information criterion:"),
    block("b2-glance"),
    quote("This is the package's governing rule in one place: never return a "
          "result it cannot compute correctly. An AIC reconstructed from "
          "posterior draws would be a different quantity under a familiar "
          "name, and somebody would compare it with an `lm()` fit's. "
          "For how much variation the predictors account for, "
          "`pseudo_r2()` is on the [next page](#p-effect-sizes), with its own "
          "caveats attached."),

    rare("Reciprocity as a predictor",
         p("`prior_reciprocity()` asks whether the reverse relation existing "
           "at the previous wave predicts this one. That is a different "
           "question from the within-dyad residual correlation the additive "
           "part already estimates — the first is a hypothesis about "
           "history, the second is a property of the errors."),
         block("b-reciprocity")),
)

page(
    "Path B · Selection", "4 · Effect sizes", "Effect sizes",

    p("A probit coefficient is not a magnitude anyone can read. On the latent "
      "scale, 0.52 means “half a standard deviation of the latent "
      "propensity”, which answers no question a reader has. What they want "
      "to know is how much more likely a tie is — and that depends on "
      "where on the curve you are standing."),

    h3("Why a probit coefficient will not do"),
    p("Because the link is nonlinear, the same coefficient implies a large "
      "change in probability for a pair near the middle of the curve and "
      "almost none for a pair already near 0 or 1. There is no single "
      "“change in probability per unit of x”. So the quantity has to be "
      "stated as a **contrast**: the probability under one value of the "
      "predictor against the probability under another, with everything else "
      "held somewhere specific."),
    p("`selection_effect()` does exactly that. For a binary term, presence "
      "against absence:"),
    block("b-effect-prior"),
    p("Read it as: a pair that was tied at the previous wave has a predicted "
      "probability this much higher than an otherwise comparable pair that "
      "was not. That is a sentence that can go in a paper."),
    quote("Read the closing line of the output too. The interval reflects "
          "uncertainty in the **coefficients only**, because the fit holds the "
          "actor effects at their posterior means. It is therefore narrower "
          "than a fully propagated interval — and it says so, rather than "
          "leaving you to discover it."),

    h3("“Everything else held somewhere specific” — but where?"),
    p("That phrase is where the real choice hides, and `type` is how you make "
      "it. The three options are genuinely different quantities, about "
      "different dyads, and they will not agree:"),
    table(["`type =`", "The dyad it is about", "Use it when", "What the interval covers"], [
        ['`"conditional"` (default)',
         "Averaged over the dyads you observed, each actor's effect at its "
         "fitted value.",
         "You are describing this network: these people, these ties.",
         "Coefficients only."],
        ['`"population"`',
         "A randomly drawn dyad, integrated over the distribution of actor "
         "effects.",
         "You are generalising to actors like these but not these ones.",
         "Coefficients **and** the actor-effect variances — so it is wider."],
        ['`"reference"`',
         "A hypothetical pair with both actor effects at zero.",
         "You want a clean, comparable benchmark rather than a description.",
         "Coefficients only."],
    ]),

    p("A continuous term has no presence and absence, so the contrast moves "
      "each dyad from its observed value by `delta` — one unit by default, "
      "and worth setting to something substantively meaningful:"),
    block("b-effect-pop"),
    block("b-effect-ref"),
    quote("The three numbers differ, and **none of them is “the” "
          "effect**. Which one belongs in your paper depends on whether you "
          "are describing these actors or a population they stand for. Pick "
          "on that basis, before you see the numbers, and say which you "
          "reported."),

    h3("Explained variation, with its caveats"),
    p("There is no AIC here, and no R-squared in the ordinary sense "
      "— [the previous page](#p-run-the-model) says why. What "
      "`pseudo_r2()` gives you is three different quantities that each answer "
      "a narrower question:"),
    block("b-pseudo"),
    quote("Two of those are this package's own constructions, and say so in "
          "their own output. The third is Tjur's D, which is published and "
          "has a plain reading: the mean fitted probability for pairs that "
          "were tied, minus the mean for pairs that were not. The note about "
          "the probit's fixed residual variance is the reason a latent-scale "
          "share is not comparable with a linear model's R-squared. "
          "`?pseudo_r2` is worth reading before any of it goes in a "
          "manuscript."),
    note("The honest order of reporting for this model family: the "
         "coefficient with its credible interval, a probability contrast with "
         "its named estimand, the variance components, and the convergence "
         "diagnostics. An explained-variation number is a supplement to that, "
         "not a substitute for any of it."),
)

page(
    "Path B · Selection", "5 · Several groups", "Several groups",

    p("Most designs have more than one network, and on this path that raises "
      "a question Path A did not have to face: an AME model is fitted to one "
      "network at a time, because the actor effects and the latent factors "
      "are defined within a network. So “several groups” means deciding "
      "what to do with several separate fits."),

    p("rnetis offers three answers, and they are a ladder:"),
    table(["Approach", "What it does", "What it assumes", "Call it with"], [
        ["Pooled", "Fits each group, then combines each coefficient by "
         "inverse-variance weighting (Cochran 1954; Hedges & Olkin 1985).",
         "That the coefficient means the same thing in every group.",
         "the default"],
        ["Separate", "Fits each group and leaves the estimates apart.",
         "Nothing across groups — and gives you nothing across groups "
         "either.", "`pool_beta = FALSE`"],
        ["Hierarchical", "One model over every group in a single chain, with "
         "between-group variation **estimated**.",
         "A distribution the group coefficients are drawn from.",
         "[`pooling =`](#p-one-hierarchical-model)"],
    ]),

    h3("Pooling, and what it assumes away"),
    block("b-two-groups"),
    block("b-pooled"),
    p("The pooled coefficient is a weighted average, with each group "
      "contributing in proportion to its precision. An effect size follows "
      "from it in the ordinary way:"),
    block("b-pooled-effect"),

    quote("Pooling assumes a coefficient means the same thing in every group. "
          "Between-group differences are **assumed away, not estimated**: "
          "there is no parameter in a pooled fit that could tell you whether "
          "the groups differ, so a pooled result cannot reassure you that they "
          "do not. If whether groups differ is anywhere near your question, "
          "this is the wrong tool."),
    p("The structural consequence is that pooling happens one coefficient at "
      "a time. There is no joint covariance across coefficients to take a "
      "contrast from, so asking for one is an error rather than a number:"),
    refusal("b-pooled-vcov-refuse"),

    h3("Leaving the groups apart"),
    p("`pool_beta = FALSE` does not combine them at all. `coef()` then "
      "returns one row per group, which is what you want when the groups are "
      "the finding:"),
    block("b-separate"),
    block("b-plot-groups"),
    fig("b-plot-groups",
        "Per-group estimates, unpooled. Reading the spread here by eye is the "
        "informal version of what the hierarchical model estimates "
        "formally — informal because nothing in this picture separates real "
        "between-group variation from the sampling noise of two small fits."),

    h3("A group-level predictor is refused here"),
    p("Within any single group, a group-level variable is constant — so it "
      "is collinear with that group's intercept, and separate per-group fits "
      "have nothing to estimate it from:"),
    refusal("b-group-refuse"),
    p("Group-level predictors belong in the hierarchical model, where they "
      "enter as moderators of the random coefficients. That is the "
      "[next page](#p-one-hierarchical-model), and it is also the answer to "
      "most of the limitations on this one."),

    quote("**Which rung to stand on.** If the groups are a nuisance and you "
          "believe the coefficient is common: pool. If the groups are the "
          "finding: leave them apart and report them. If you want to know "
          "*whether* they differ, and by how much: the hierarchical model is "
          "the only one of the three that estimates it — and it is this "
          "package's own construction, so read the warning on that page "
          "before you rely on it."),
)

page(
    "Path B · Selection", "6 · One hierarchical model", "One hierarchical model",

    quote("**This model is this package's own construction** from published "
          "components: the AME model, hierarchical partial pooling, and half-t "
          "and LKJ priors. It is not a published method under a published "
          "name. `?hierarchical_selection` specifies the model, the priors and "
          "the algorithm. Its tests establish that it computes what that page "
          "specifies; **they do not establish its statistical performance**. "
          "Treat results from it as you would results from a method you "
          "implemented yourself."),

    p("`pooling = selection_pooling()` fits one model over every group in a "
      "single Markov chain. A coefficient you name in `random` gets a value "
      "for each group, drawn from a between-group distribution whose mean and "
      "spread are estimated; a group-level predictor enters as a moderator of "
      "those coefficients."),
    eq('<i>β<sub>g</sub></i> <span class="op">~</span> '
       'Normal(<i>μ</i>, <i>τ</i><sup>2</sup>)',
       "One coefficient per group, drawn from a distribution. μ is the "
       "population mean and τ the between-group standard deviation — the "
       "parameter that answers “do groups differ on this?”"),

    h3("Twelve teams that genuinely differ"),
    p("These twelve simulated teams differ in baseline and in sender slope, so "
      "there is something for the model to find:"),
    block("b-teams"),
    block("b-pooling-spec"),

    h3("The fit"),
    p("This is the longest-running block in the tutorial — two chains "
      "over twelve groups. It took about eight minutes on the machine this "
      "page was built on. Fit one group first if you are still deciding on a "
      "specification; see [Slow fits](#p-slow-fits-and-mcmc-settings)."),
    block("b-hier"),

    h3("Reading it"),
    p("`netis_hierarchy()` separates the three questions a hierarchical fit "
      "answers. **`population`** gives the mean `mu` and the between-group "
      "standard deviation `tau` of each random coefficient:"),
    block("b-hier-pop"),
    p("`tau` is the answer to “do groups differ on this?”, and it is "
      "estimated rather than assumed. That is the whole reason to come up "
      "this rung from pooling."),

    p("**`groups`** gives each team's own values:"),
    block("b-hier-groups"),

    p("**`convergence`** gives R-hat and effective sample sizes for every "
      "parameter. These are the rows to read first — the shared "
      "coefficient, the population means, and the between-group standard "
      "deviations:"),
    block("b-hier-conv"),
    p("The rest is one row per group coefficient and one per nuisance "
      "parameter — for twelve groups, seventy-odd more. Summarise rather "
      "than read them one by one:"),
    block("b-hier-conv-rest"),
    note("The `tau` rows have far smaller effective sample sizes than the "
         "`mu` rows. That is usual: a between-group standard deviation is "
         "estimated from twelve groups, and a variance parameter near zero "
         "mixes slowly. It is the parameter to watch when deciding whether to "
         "run the chain longer — and the one whose convergence a reader "
         "should be told about."),

    p("And `what = \"groups\"` draws the spread the model estimated:"),
    block("b-hier-panel-groups"),
    fig("b-hier-panel-groups",
        "Each team's own intercept and sender slope, with 95% intervals. "
        "The caption restates the rule that matters here: a between-group "
        "comparison carries information proportional to the number of "
        "**groups**, not the number of observations."),
    note("`netis_variance_components()` is **not** defined for a hierarchical "
         "fit — its shares are for per-group `amen::ame()` fits. Use "
         "`netis_hierarchy()` for the between-group distribution; each "
         "group's within-network quantities and their convergence diagnostics "
         "are in `fit$convergence_table`."),
)

page(
    "Path B · Selection", "7 · Beyond this path", "Beyond Path B",

    p("The path you have just finished is the supported route: build the "
      "dyadic data, specify roles explicitly, run the chain and check it, "
      "report a probability contrast with a named estimand, and handle several "
      "networks at whichever rung of the ladder your question needs. This page "
      "is the rest of the Selection side."),

    h3("Marked experimental, or the package's own"),
    cards(
        card("The hierarchical multi-group model",
             p("Everything on [page 6](#p-one-hierarchical-model). It is "
               "assembled from published components but is not itself a "
               "published method: the tests establish that it computes what "
               "`?hierarchical_selection` specifies, and nothing about its "
               "statistical performance."),
             p("**If between-group variation is central to your claim**, "
               "report the per-group fits from "
               "[page 5](#p-several-groups) alongside it, and say which "
               "you relied on."),
             step="package's own", num=1, open=True),
        card("The latent-scale variance share",
             p("Two of the three quantities `pseudo_r2()` reports for a "
               "Selection fit are this package's own constructions, and say so "
               "in their output. Tjur's D is the published one."),
             step="package's own", num=2),
    ),

    h3("Also available, and ordinary"),
    table(["You want", "Use", "Where"], [
        ["Third-order structure the actor effects miss",
         "`R = 1`, `R = 2`, … and `plot(fit, what = \"multiplicative\")`",
         "below"],
        ["Reciprocity of the previous wave as a predictor",
         "`prior_reciprocity()`",
         "[Specify a model](#p-specify-a-model)"],
        ["Correlated random effects, moderators, the prior on `tau`",
         "`selection_pooling(covariance = \"unstructured\", ...)`",
         "`?selection_pooling`"],
        ["A pair attribute built from a derived network",
         "`binarize()`, `symmetrize()`, `multiplex()`",
         "[Derived networks](#p-derived-networks)"],
        ["Everything else", "—",
         "[The rest of the package](#p-the-rest-of-the-package)"],
    ]),

    h3("The multiplicative part, in practice"),
    p("`R` sets the dimension. `R = 0`, the default, is additive effects only; "
      "a positive `R` adds latent factors that capture structure the additive "
      "terms cannot:"),
    block("b-fit-multiplicative"),
    block("b-plot-mult"),
    fig("b-plot-mult", "The rotation-invariant multiplicative structure at `R = 2`."),
    note("A latent position here is identified only up to rotation, exactly as "
         "in [Path A, page 5](#p-homophily-and-the-latent-space). The panel "
         "shows the rotation-invariant structure for that reason, and a "
         "coordinate from it is not an interpretable quantity."),

    h3("Where Path B ends"),
    p("You can now specify a tie model in which every predictor's role is "
      "explicit, run it with additive and multiplicative actor effects, check "
      "that the chain worked, report magnitudes as probability contrasts with "
      "a named estimand, handle several networks at three different levels of "
      "assumption, and fit one hierarchical model that estimates between-group "
      "variation instead of assuming it away — while knowing which of "
      "those surfaces are published methods and which are this package's "
      "own."),
)

# ===========================================================================
# Reference
# ===========================================================================
page(
    "Reference", "Formula terms", "Formula terms at a glance",

    p("Both model families use a formula, and in both of them every term names "
      "its role. The two grammars do not overlap: an Influence term is not "
      "valid in a Selection formula and the reverse."),

    h3("Influence terms"),
    table(["Term", "Arguments", "What it adds"], [
        ["`ar()`", "`lag = 1`", "The outcome's own prior value."],
        ["`exposure()`", "`content`, `via`, `name`, `x_lag = 1`, `w_lag`, "
         "`normalize`, `mechanism`, `direction`, `isolate_rule`",
         "Alters' characteristic carried through a network."],
        ["`lagged()`", "`var`, `lag = 1`", "An earlier wave's value of a node variable."],
        ["`centrality()`", "`network`, `measure`, `mode`, `weights`, "
         "`weight_transform`, `lag`, `name`", "A node's structural standing."],
        ["`network_stat()`", "`network`, `measure`, `summary`, `weights`, "
         "`weight_transform`, `lag`, `name`", "A whole-network statistic."],
        ["`latent()`", "`network`, `d = 1`, `homophily`, `terms`, `lag`, "
         "`name`, `control`, `seed`", "Latent-space positions as covariates."],
        ["`bi()`", "`delta`, `alpha`, `gamma`",
         "The Frank & Fahrbach (1999) balance-information mechanism."],
        ["`(1 | group)`", "—", "A random intercept per network."],
        ["a bare variable", "—", "A node variable at the outcome wave."],
    ]),

    h3("Selection terms"),
    table(["Term", "Arguments", "What it adds"], [
        ["`sender()`", "`var`, `lag = 0`, `name`", "A property of the tie's sender."],
        ["`receiver()`", "`var`, `lag = 0`, `name`", "A property of the tie's receiver."],
        ["`dyad()`", "`var`, `lag = 0`, `name`", "A property of the pair, held as a tie variable."],
        ["`group()`", "`var`, `lag = 0`, `name`",
         "A group-level predictor; hierarchical fits only."],
        ["`prior_tie()`", "`lag = 1`, `name`", "Did the relation exist at the previous wave?"],
        ["`prior_reciprocity()`", "`lag = 1`, `name`",
         "Did the reverse relation exist at the previous wave?"],
    ]),

    h3("Centrality and network-statistic measures"),
    p("Available to `centrality()`, `network_stat()`, `compute_centrality()` "
      "and `compute_network_stat()`:"),
    codeonly("degree, strength, density, reciprocity, betweenness, closeness,\n"
             "eigenvector, pagerank, constraint", lang="text"),
    p("Computed outside a model, each needs a wave where the data has one:"),
    block("n-centrality"),
    block("n-network-stat"),

    h3("A weight-semantics guardrail"),
    p("A measure that reads weights as *distances* — betweenness, "
      "closeness — will not run on a network whose weights you declared "
      "as *strengths*, because a larger value would silently be read as a "
      "shorter distance:"),
    refusal("n-transform-refuse"),
    p("`inverse()` is the one weight transform rnetis provides, and it is the "
      "standard strength-to-distance conversion (Opsahl, Agneessens & "
      "Skvoretz 2010):"),
    block("n-transform"),
)

page(
    "Reference", "Derived networks", "Derived networks",

    p("The conduit an exposure term reads need not be a raw tie variable. "
      "A derived network is a transformation of one, written in the formula "
      "so that what counted as a tie travels with the fit instead of living "
      "in a script that made the data."),

    p("These pages use a small weighted contact network:"),
    block("n-setup"),
    block("n-weighted-exposure"),

    h3("binarize()"),
    p("`binarize(network, rule)` turns a weighted network into a yes/no one. "
      "The rule is an **object**, not a number buried in an argument, so the "
      "definition of a tie is part of the fitted model:"),
    block("n-binarize"),
    block("n-binarize-threshold"),

    table(["Rule", "A tie is a cell that is"], [
        ["`presence()`", "present in the table at all (the default)."],
        ["`nonzero()`", "not zero."],
        ["`positive()`", "greater than zero."],
        ["`threshold(c)`", "at least `c`."],
        ["`abs_threshold(c)`", "at least `c` in absolute value."],
        ["`above_mean(scope)`", "above the mean of its scope."],
        ["`above_median(scope)`", "above the median of its scope."],
        ["`quantile_threshold(p, scope)`", "above the `p`th quantile of its scope."],
        ["`custom_binarize_rule(fn, name)`", "whatever your function says."],
    ]),
    note("`scope` is `\"snapshot\"`, `\"group\"`, `\"wave\"` or `\"global\"`: "
         "above the mean *of what?* is a real question whenever there is more "
         "than one network or more than one wave."),
    block("n-binarize-custom"),

    h3("symmetrize()"),
    p("Making a directed network undirected is several different operations on "
      "a weighted one, so none of them is assumed:"),
    refusal("n-symmetrize-refuse"),
    block("n-symmetrize"),
    note("The rules are `\"max\"`, `\"min\"`, `\"sum\"`, `\"mean\"`, "
         "`\"any\"` and `\"both\"`."),

    h3("multiplex()"),
    p("Combine two or more networks into one conduit — here, contacts of "
      "at least 3 **and** an advice tie:"),
    block("n-multiplex"),
    note("`combine` takes `\"union\"`/`\"any\"`, `\"intersection\"`/`\"all\"`, "
         "`\"k_of_n\"` with `k`, `\"count\"`, `\"sum\"` or `\"mean\"`."),

    rare("Rules print as objects",
         block("n-binarize-rule-print"),
         p("That is the point: the rule is a value you can store, pass around "
           "and attach to a fit, rather than a threshold someone typed once "
           "into a data-cleaning script.")),
)

page(
    "Reference", "Derived variables", "Derived variables",

    p("Three functions add a column to a `netis_data` and record how it was "
      "made, so the derivation travels with the data rather than living in "
      "whatever script produced it."),

    h3("Centering"),
    block("v-centering"),
    note("`method = \"grand\"` centres on the overall mean; "
         "`method = \"constant\"` with `at =` centres on a value you name, "
         "which is what you want when zero should mean something specific."),

    h3("Within and between"),
    p("Splitting a variable into the part that varies within a context and the "
      "part that distinguishes contexts is a different model, not a "
      "presentational choice:"),
    block("v-decomposition"),
    note("`unit` takes `\"actor\"`, `\"group_wave\"`, `\"context\"` or "
         "`\"group_pooled\"`. A node-level variable's group decomposition is "
         "`\"context\"` or `\"group_pooled\"`; `\"group_wave\"` is for "
         "group-level variables and is refused for a node-level one, by name."),

    h3("Baseline provenance"),
    p("`add_baseline_provenance()` records a variable's first-wave value as a "
      "column, with the record of where it came from:"),
    block("v-baseline"),
)

page(
    "Reference", "The rest of the package", "The rest of the package",

    p("The two paths use maybe half of what rnetis exports. This page is the "
      "remainder, grouped by what you would be trying to do. Nothing here is "
      "needed to follow the tutorial; all of it is documented, and every "
      "published method is cited on its own help page."),

    filterbox(
        "Filter by name or by what it does…",
        card("Building and importing data",
             table(["Function", "What it does"], [
                 ["`netis_data()`", "The constructor: nodes, ties, optionally groups."],
                 ["`netis_data_from_adjacency()`", "From one matrix."],
                 ["`netis_data_from_adjacency_series()`", "From one matrix per wave."],
                 ["`netis_data_from_edgelist()`", "From an edge list plus a roster."],
                 ["`netis_data_from_igraph()`, `..._list()`", "From `igraph`."],
                 ["`netis_data_from_network()`", "From `statnet`."],
                 ["`netis_data_from_split()`", "From separate static and dynamic tables."],
                 ["`as_netis_data()`", "Generic dispatch over the above."],
             ]), step="data"),
        card("Inspecting and exporting data",
             table(["Function", "What it does"], [
                 ["`netis_nodes()`, `netis_ties()`, `netis_groups()`",
                  "The tables back out, unchanged."],
                 ["`netis_tables()`", "All of them at once."],
                 ["`netis_schema()`", "The full schema: columns, waves, static/dynamic."],
                 ["`netis_boundary()`, `netis_boundary_rules()`",
                  "What was declared, what was inferred, and by which rule."],
                 ["`netis_losses()`", "What a conversion could not carry."],
                 ["`netis_time_index()`", "Wave positions and any metric time."],
                 ["`netis_adjacency()`", "One snapshot as a matrix."],
                 ["`as_igraph()`, `as_igraph_list()`", "Out to `igraph`."],
                 ["`as_network()`, `as_network_list()`", "Out to `statnet`."],
             ]), step="data"),
        card("Checking data before modelling",
             table(["Function", "What it does"], [
                 ["`diagnose_data()`", "Structured findings about the object."],
                 ["`diagnose_design()`", "Findings about a formula against that object, "
                  "before you fit it."],
                 ["`filter_diagnostics()`, `has_level()`", "Select findings by level "
                  "or family."],
                 ["`plot()`, `plot_data()`", "Five panels, and the table behind each."],
             ]), step="data"),
        card("Network features computed outside a model",
             table(["Function", "What it does"], [
                 ["`compute_exposure()`", "Exposure as a column, with every argument "
                  "`exposure()` takes."],
                 ["`compute_centrality()`", "One centrality per node."],
                 ["`compute_network_stat()`", "One statistic per network snapshot."],
                 ["`binarize()`, `symmetrize()`, `multiplex()`", "Derived networks."],
                 ["`inverse()`", "Strength-to-distance weight transform."],
                 ["`presence()`, `nonzero()`, `positive()`, `threshold()`, "
                  "`abs_threshold()`, `above_mean()`, `above_median()`, "
                  "`quantile_threshold()`, `custom_binarize_rule()`",
                  "The binarization rules, as objects."],
             ]), step="networks"),
        card("Derived variables",
             table(["Function", "What it does"], [
                 ["`add_centering()`", "A centred copy of a variable, with its rule "
                  "recorded."],
                 ["`add_decomposition()`", "Within and between parts as separate "
                  "columns."],
                 ["`add_baseline_provenance()`", "A first-wave value as a column."],
             ]), step="variables"),
        card("Influence models",
             table(["Function", "What it does"], [
                 ["`fit_influence()`", "The fit. `estimator =` also offers the "
                  "network-autocorrelation (SAR) backend, which needs `sna` and "
                  "`numDeriv`."],
                 ["`ar()`, `exposure()`, `lagged()`, `centrality()`, "
                  "`network_stat()`, `latent()`", "The formula terms."],
                 ["`bi()`, `bi_components()`, `bi_sweep()`",
                  "Influence along longer, discounted paths (Frank & Fahrbach 1999)."],
                 ["`influence_effect()`", "A magnitude on the outcome's scale."],
                 ["`influence_se_comparison()`", "Model against cluster-robust "
                  "standard errors."],
                 ["`netis_assumptions()`", "The assumption ledger."],
                 ["`netis_dependence()`", "Residual dependence with respect to the "
                  "network."],
                 ["`AIC()`, `BIC()`, `logLik()`, `pseudo_r2()`", "Fit statistics."],
                 ["`simulate_influence()`", "Data from a known influence process."],
             ]), step="influence"),
        card("Selection models",
             table(["Function", "What it does"], [
                 ["`fit_selection()`", "The fit, through `amen::ame()`."],
                 ["`sender()`, `receiver()`, `dyad()`, `group()`, `prior_tie()`, "
                  "`prior_reciprocity()`", "The formula terms."],
                 ["`selection_effect()`", "A probability-scale contrast."],
                 ["`selection_pooling()`", "The hierarchical specification: random "
                  "coefficients, moderators, covariance, priors."],
                 ["`netis_hierarchy()`", "Population, per-group and convergence "
                  "tables from a hierarchical fit."],
             ]), step="selection"),
        card("Reading any fit",
             table(["Function", "What it does"], [
                 ["`coef()`, `confint()`, `fitted()`, `summary()`",
                  "The usual R surfaces."],
                 ["`netis_estimates()`, `tidy()`", "The coefficient table as a data "
                  "frame, with `role` and `scale`."],
                 ["`netis_glance()`, `glance()`", "One row per fit, with the fit "
                  "statistics."],
                 ["`netis_estimand()`", "In words, what the fit is an estimate of."],
                 ["`netis_variance_components()`", "Where the variance sits."],
                 ["`netis_diagnostics()`", "Findings recorded at fit time."],
                 ["`pseudo_r2()`", "Explained variation, by family, with caveats."],
             ]), step="results"),
        card("Multilevel reliability",
             table(["Function", "What it does"], [
                 ["`icc1()`, `icc2()`", "ICC(1) and group-mean reliability "
                  "(Bliese 2000)."],
                 ["`rwg()`, `rwg_j()`", "Within-group agreement (James, Demaree & "
                  "Wolf 1984)."],
                 ["`netis_reliability()`", "The collected reliability surface."],
             ]), step="groups"),
    ),
)

page(
    "Reference", "What rnetis refuses", "What rnetis refuses",

    p("rnetis refuses rather than guessing wherever a guess could change a "
      "number without anybody noticing. These are the refusals this tutorial "
      "met, in full, in one place — each with the decision it is asking "
      "you to make. Type into the box to narrow the list."),

    filterbox(
        "Filter the refusals…",
        card("A complete-dyadic network with no presence rule",
             refusal("t-presence-refuse"),
             p("**Why.** Saying every pair was observed does not say which "
               "rows are ties. The two readings gave densities of 1 and 0.33 "
               "on the same six rows."),
             p("**Fix.** `presence_mode = \"value_rule\"` with a "
               "`presence_rule`, `\"indicator\"` with a `presence_col`, or "
               "`\"row_presence\"` if every row is a tie."),
             step="data"),
        card("symmetrize() with no rule", refusal("t-symmetrize-refuse"),
             p("**Why.** On a weighted network, max, min, sum and mean are "
               "different operations."),
             p("**Fix.** Name one."),
             step="data"),
        card("Row-normalized exposure for a node with no alters",
             refusal("t-exposure-error"),
             p("**Why.** There is no average of an empty set. A zero would "
               "place the isolate at zero on the outcome's scale, which is a "
               "substantive claim."),
             p("**Fix.** `isolate_rule = \"na\"` (the default) to drop the "
               "row, or `\"zero\"` if zero is genuinely right."),
             step="influence"),
        card("An edge list with no roster", refusal("d-edgelist-refuse"),
             p("**Why.** An edge list names only nodes that have ties. Every "
               "density, degree and centrality would be computed over a "
               "smaller node set than the study has."),
             p("**Fix.** Pass the roster as `nodes =`."),
             step="data"),
        card("An adjacency matrix with a nonzero diagonal",
             refusal("d-adjacency-refuse"),
             p("**Why.** Self-ties are either real or an artefact, and the "
               "matrix cannot say which."),
             p("**Fix.** `self_ties = \"permitted\"`, or zero the diagonal."),
             step="data"),
        card("Three or more waves, no transition named",
             refusal("a-waves-refuse"),
             p("**Why.** Analysing a three-wave study through one transition "
               "is a decision about your design, and stacking transitions is "
               "a different estimation problem — see "
               "[the Selection data page](#p-the-selection-dataset)."),
             p("**Fix.** `waves = c(from, to)`."),
             step="influence"),
        card("A lagged relation identical to the response",
             refusal("b2-waves3-named-refuse"),
             p("**Why.** If the relation did not change across the named "
               "transition, `prior_tie()` *is* the outcome."),
             p("**Fix.** Choose a transition over which the relation changes, "
               "or drop the lagged term."),
             step="selection"),
        card("Several networks, no random intercept", refusal("a-mg-refuse"),
             p("**Why.** Without one, separate networks are pooled into a "
               "single intercept."),
             p("**Fix.** Add `(1 | group)`."),
             step="influence"),
        card("Cluster-robust standard errors on one network",
             refusal("a-se-refuse"),
             p("**Why.** `by = \"group\"` assumes independence *between* "
               "networks, so it needs several."),
             p("**Fix.** Use several networks, or `by = \"node\"`, which is a "
               "different correction and is never network-robust."),
             step="influence"),
        card("A bare covariate in a Selection formula", refusal("b-bare-refuse"),
             p("**Why.** Sender and receiver effects are different hypotheses "
               "about the same variable."),
             p("**Fix.** `sender()`, `receiver()` or `dyad()` — or both "
               "of the first two."),
             step="selection"),
        card("Two waves, no lagged relation term", refusal("b-waves-refuse"),
             p("**Why.** Both waves would supply an outcome, treating two "
               "observations of the same actors as independent."),
             p("**Fix.** `prior_tie()` to model the transition, or "
               "`waves = c(1, 2)` to fit the later wave only. They are "
               "different models."),
             step="selection"),
        card("A log-likelihood from a Selection fit",
             refusal("b2-nologlik-refuse"),
             p("**Why.** `amen::ame()` returns none, and one reconstructed "
               "from posterior draws would be a different quantity under a "
               "name people compare across model families."),
             p("**Fix.** `pseudo_r2()`, read with `?pseudo_r2`."),
             step="selection"),
        card("Exporting a complete-dyadic network to igraph",
             refusal("b2-igraph-refuse"),
             p("**Why.** A graph has edges; a complete-dyadic table has rows "
               "that may or may not be edges."),
             p("**Fix.** Declare a `presence_rule` or a `presence_col`."),
             step="data"),
        card("A joint covariance from a pooled fit",
             refusal("b-pooled-vcov-refuse"),
             p("**Why.** Inverse-variance pooling works one coefficient at a "
               "time, so there is no joint covariance."),
             p("**Fix.** `pool_beta = FALSE`, or the hierarchical model."),
             step="selection"),
        card("A group-level predictor in a per-group fit",
             refusal("b-group-refuse"),
             p("**Why.** Within a group it is constant, so it is collinear "
                "with that group's intercept."),
             p("**Fix.** Fit the hierarchical model, where it is a moderator."),
             step="selection"),
        card("A distance measure on strength-declared weights",
             refusal("n-transform-refuse"),
             p("**Why.** A larger value would be read as a shorter distance."),
             p("**Fix.** `weight_transform = inverse()`, or "
               "`weights = \"unweighted\"`."),
             step="networks"),
    ),

    h3("The pattern"),
    p("Every one of these refusals has the same shape: rnetis has found a "
      "point where two readings of your data would give different numbers, it "
      "cannot tell which you meant, and the choice is recorded as yours rather "
      "than taken as a default. The message names the argument that makes it. "
      "A refusal is not an obstacle to route around — it is the one place "
      "in the analysis where the decision is visible."),
)

page(
    "Reference", "Scope", "Scope: what is not here",

    p("rnetis has a deliberately narrow scope, and several well-established "
      "methods are absent **because of the scope, not because of any doubt "
      "about them**."),

    h3("The scope"),
    ul("**Two waves at most.** `T = 1` and `T = 2`. With three or more waves "
       "present, rnetis makes you name the wave or the transition rather than "
       "choosing one for you.",
       "**One outcome occasion per unit per fit.** Wave *depth* is "
       "unrestricted — lags and exposure history may reach further back "
       "— but repeated outcome rows per unit are not modelled.",
       "**Gaussian-continuous Influence outcomes.**"),

    h3("Deliberately absent"),
    table(["Not here", "Why"], [
        ["Dynamic-panel estimators (Arellano–Bond, Blundell–Bond, "
         "Bollen–Brand/AMW, the within transformation)",
         "They need more than the one transition a fit analyzes. This is why "
         "the assumption ledger reads `unaddressed` for sequential exogeneity "
         "and persistent actor heterogeneity, and why both paths fit one "
         "transition — "
         "[the reasoning is on Path B, page 1](#p-the-selection-dataset)."],
        ["A correction for the initial-conditions problem",
         "`prior_tie()` conditions on the first observed state and so assumes "
         "it fixed (Heckman 1981). Defensible for one transition; not "
         "corrected for."],
        ["Network-HAC standard errors", "Outside the scope above."],
        ["Distance-shell network autocorrelation", "Outside the scope above."],
    ]),

    h3("What the package's tests establish"),
    quote("The tests establish that the package's own constructions — the "
          "hierarchical multi-group Selection model, the latent-scale variance "
          "share and null comparison, multi-group `latent()`, "
          "`add_baseline_provenance()`, the assumption ledger and the numeric "
          "thresholds in the diagnostics — are computed as specified. "
          "**They do not establish their statistical performance.** "
          "Multi-group `latent()` is additionally marked experimental and "
          "warns whenever it is used."),

    h3("Licence and provenance"),
    p("rnetis is developed and owned by Griffin Dynamics LLC, licensed under "
      "AGPL-3 with additional terms. Part of the hierarchical sampler derives "
      "from `amen` (Hoff, Fosdick, Volfovsky and He), and the package "
      "redistributes no one else's data: every dataset in it is synthetic and "
      "generated by `data-raw/demo-datasets.R`."),
)

# ===========================================================================
# Help
# ===========================================================================
page(
    "Help", "Precision", "How precise is an exposure coefficient?",

    p("The worked Influence example produced an exposure interval wide enough "
      "to contain both nothing and a good deal, and both information criteria "
      "preferred the model without exposure in it. That is not a quirk of the "
      "demo data. An exposure coefficient is estimated from the *differences* "
      "between nodes in what reaches them, and in a small network there is "
      "not much of that to go round."),

    p("`simulate_influence()` generates a network and an outcome from a "
      "process you specify, so you can see the relationship between size and "
      "precision directly. Here, the same process at three sizes, with mean "
      "degree held constant:"),
    block("s-precision"),

    quote("Thirty nodes is a small network for this question, and the demo "
          "data has thirty nodes. An inconclusive exposure interval is not "
          "evidence against influence, and reading it as though it were is an "
          "easy mistake to make in a two-wave network study of this size."),

    note("This is a development illustration of one process at one setting, "
         "not a study of the estimator's operating characteristics, and "
         "nothing here should be cited as one."),

    rare("What simulate_influence() controls",
         p("`n_nodes`, `density`, `gamma` (the autoregressive coefficient), "
           "`beta` (the exposure coefficient), `resid_sd`, `n_waves`, "
           "`n_groups`, `node_sd`, `group_sd`, `error_structure`, "
           "`exposure_content`, `normalize`, `isolate_rule`, `homophily` and "
           "`network_change`, among others. It returns the `netis_data` in "
           "`$data` and the parameters in `$truth`."),
         p("If you ask for a process that cannot be stationary — a large "
           "`beta` on an unnormalized network, say — it tells you the "
           "spectral radius and what to change, rather than handing back an "
           "outcome that grew without bound.")),
)

page(
    "Help", "Slow fits", "Slow fits and MCMC settings",

    p("Everything in Path A is fast, apart from the first stage of `latent()`. "
      "Path B fits Markov chains, and the settings are yours."),

    table(["Argument", "What it does", "Used here"], [
        ["`nscan`", "Iterations after burn-in.", "5,000 single-group; 10,000 hierarchical"],
        ["`burn`", "Iterations discarded first.", "1,000"],
        ["`odens`", "Thinning: keep one draw in `odens`.", "10"],
        ["`chains`", "Independent chains. Two or more make R-hat meaningful.",
         "2, hierarchical only"],
        ["`seed`", "Reproducibility.", "1"],
        ["`R`", "Dimension of the multiplicative term. `0` is additive only.", "0, and 2 once"],
    ]),

    h3("If a fit is too slow"),
    ol("Cut `nscan` while you are still deciding on a specification. Raise it "
       "once the specification is settled — a thinned 500-draw posterior "
       "is enough to see whether a term belongs, and not enough to report.",
       "Keep `R = 0` until you have a reason to want multiplicative effects. "
       "Each dimension costs.",
       "Fit one group first. A hierarchical fit over twelve groups is roughly "
       "twelve times the work of one.",
       "Check the chain before you lengthen it. If `netis_hierarchy(fit)$convergence` "
       "shows a poorly mixing parameter, more iterations of the same chain may "
       "not be the answer."),

    h3("If a fit will not converge"),
    p("Look at the traces first (`plot(fit, what = \"trace\")`), then the "
      "effective sample sizes (`plot(fit, what = \"mcmc\")`). A parameter "
      "that wanders rather than hovering is usually telling you the model is "
      "not identified by the data you gave it — a dyadic covariate that "
      "barely varies, or a random slope on a coefficient that is near zero in "
      "every group."),
)

page(
    "Help", "Common errors", "Common errors",

    filterbox(
        "Filter the errors…",
        card("“Edge presence for … is undetermined”",
             p("You declared a network `complete_dyadic` — or rnetis "
               "inferred it — and then asked for something that needs to "
               "know which rows are ties: a density, a centrality, an "
               "`igraph` export."),
             p("**Fix.** Declare it: `presence_mode = \"value_rule\"` with a "
               "`presence_rule`, `\"indicator\"` with a `presence_col`, or "
               "`\"row_presence\"`. See "
               "[Nodes, ties and waves](#p-nodes-ties-and-waves).")),
        card("“This data has a wave dimension … but no wave was specified”",
             p("You called `compute_exposure()`, `compute_centrality()`, "
               "`compute_network_stat()`, `netis_adjacency()`, `as_igraph()` "
               "or `plot()` on data with more than one wave. These compute a "
               "single snapshot."),
             p("**Fix.** Pass `wave =`. Inside a *formula* you do not need to: "
               "`fit_influence()` resolves the wave from the transition it is "
               "fitting.")),
        card("“node_data has duplicate rows for key (group, node)”",
             p("`netis_data()` wants exactly one row per entity. Repeated rows "
               "for the same node almost always mean repeated measures."),
             p("**Fix.** Add a wave column and name it with `wave_col`, or "
               "de-duplicate.")),
        card("“Unknown measure …”",
             p("The measure names are `degree`, `strength`, `density`, "
               "`reciprocity`, `betweenness`, `closeness`, `eigenvector`, "
               "`pagerank` and `constraint`. Direction is a separate argument."),
             p("**Fix.** `centrality(tie, \"degree\", mode = \"in\")` rather "
               "than an `\"indegree\"` measure.")),
        card("“netis_assumptions() takes a fitted Influence model”",
             p("The assumption ledger is an Influence surface. A Selection fit "
               "records its design findings at fit time instead."),
             p("**Fix.** `netis_diagnostics(fit)` for a Selection fit.")),
        card("“A Selection fit has no log-likelihood”",
             p("`logLik()`, `AIC()` and `BIC()` are not defined for a "
               "Selection fit, on purpose — "
               "[Run the model](#p-run-the-model) says why."),
             p("**Fix.** `pseudo_r2()` for explained variation; "
               "`netis_glance()` for what was fitted.")),
        card("“unit = \"group_wave\" is defined for group-level variables only”",
             p("`add_decomposition()` supports different units for different "
               "kinds of variable."),
             p("**Fix.** For a node-level variable's group decomposition, "
               "`unit = \"context\"` or `\"group_pooled\"`.")),
        card("A coefficient comes back `NA`",
             p("The term was collinear with something already in the model. "
               "Binarizing an already-binary network is a common way to "
               "produce this: the derived conduit is identical to the "
               "original, or constant."),
             p("**Fix.** Check that the derived network you built differs from "
               "the one you started with — "
               "`compute_exposure()` on each, side by side.")),
        card("A row disappeared from the fit",
             p("An `NA` in any term drops the row. The two usual sources are a "
               "row-normalized exposure for a node with no alters, and a "
               "`latent()` position for an actor not observed at the modelled "
               "wave."),
             p("**Fix.** Compare `netis_glance(fit)$nobs` with the rows you "
               "expected, and look at the term that introduced the `NA`.")),
        card("The package tells me to install something",
             p("rnetis names the package you need at the point you need it. "
               "That is by design: nothing optional is a hard dependency."),
             p("**Fix.** Install it; see "
               "[Install rnetis](#p-install-rnetis) for what each one unlocks.")),
    ),
)

page(
    "Help", "Read more", "Where to read more",

    h3("In the package"),
    table(["Read", "For"], [
        ["`vignette(\"influence\")`", "The Influence model, concisely."],
        ["`vignette(\"selection\")`", "The Selection model, concisely."],
        ["`vignette(\"latent-space\")`", "The latent-space adjustment and its limits."],
        ["`vignette(\"model-fit\")`", "Checking a fit."],
        ["`?netis_data`", "Declaring tie semantics yourself."],
        ["`?exposure`", "Every exposure argument, with citations."],
        ["`?hierarchical_selection`", "The hierarchical model, priors and algorithm, "
         "specified as the package's own construction."],
        ["`?pseudo_r2`", "What each explained-variation quantity means."],
        ["`?netis_assumptions`", "The assumption ledger."],
        ["`?latent`", "The adjustment, and the experimental multi-group form."],
    ]),
    note("Every published method is cited on the help page of the function "
         "that computes or fits it, and where an implementation departs from "
         "its source, the departure is named at the function."),

    h3("The packages rnetis builds on"),
    p("If you use these capabilities, cite the packages that provide them as "
      "well as rnetis:"),
    table(["Package", "What rnetis uses it for"], [
        ["`amen` (Hoff, Fosdick, Volfovsky, He)",
         "Every Selection fit. `fit_selection()` calls `amen::ame()`."],
        ["`latentnet` (Krivitsky & Handcock)",
         "The first stage of `latent()` — the latent-space model whose "
         "positions the adjustment carries forward. This is the package Xu's "
         "own code uses."],
        ["`lme4` (Bates, Mächler, Bolker, Walker)",
         "Every mixed Influence fit — anything with `(1 | group)`."],
        ["`clubSandwich` (Pustejovsky)", "Cluster-robust standard errors."],
        ["`igraph`, `network` / `statnet`", "Import and export."],
        ["`sna`, `numDeriv`", "The network-autocorrelation (SAR) estimator."],
    ]),

    h3("The methods"),
    ul("Bell, R. M. and McCaffrey, D. F. (2002) Bias reduction in standard "
       "errors for linear regression with multi-stage samples. *Survey "
       "Methodology* **28**(2), 169–181.",
       "Bliese, P. D. (2000) Within-group agreement, non-independence, and "
       "reliability. In Klein and Kozlowski (eds), *Multilevel Theory, "
       "Research, and Methods in Organizations*, 349–381. Jossey-Bass.",
       "Bliese, P. D. and Halverson, R. R. (1998) Group size and measures of "
       "group-level properties. *Journal of Management* **24**(2), 157–172.",
       "Cochran, W. G. (1954) The combination of estimates from different "
       "experiments. *Biometrics* **10**(1), 101–129.",
       "Frank, K. A. and Fahrbach, K. (1999) Organization culture as a complex "
       "system: balance and information in models of influence and selection. "
       "*Organization Science* **10**(3), 253–277.",
       "Friedkin, N. E. and Johnsen, E. C. (1990) Social influence and "
       "opinions. *Journal of Mathematical Sociology* **15**(3–4), 193–206.",
       "Heckman, J. J. (1981) The incidental parameters problem and the "
       "problem of initial conditions in estimating a discrete time-discrete "
       "data stochastic process. In Manski and McFadden (eds), *Structural "
       "Analysis of Discrete Data with Econometric Applications*, 179–195. "
       "MIT Press.",
       "Hedges, L. V. and Olkin, I. (1985) *Statistical Methods for "
       "Meta-Analysis*. Academic Press.",
       "Hoff, P. D. (2005) Bilinear mixed-effects models for dyadic data. "
       "*JASA* **100**(469), 286–295.",
       "Hoff, P. D. (2021) Additive and multiplicative effects network models. "
       "*Statistical Science* **36**(1), 34–50.",
       "Hoff, P. D., Raftery, A. E. and Handcock, M. S. (2002) Latent space "
       "approaches to social network analysis. *JASA* **97**(460), 1090–1098.",
       "James, L. R., Demaree, R. G. and Wolf, G. (1984) Estimating "
       "within-group interrater reliability with and without response bias. "
       "*Journal of Applied Psychology* **69**(1), 85–98.",
       "Krivitsky, P. N. and Handcock, M. S. (2008) Fitting position latent "
       "cluster models for social networks with latentnet. *Journal of "
       "Statistical Software* **24**(5).",
       "Leenders, R. Th. A. J. (2002) Modeling social influence through "
       "network autocorrelation: constructing the weight matrix. *Social "
       "Networks* **24**(1), 21–47.",
       "Liang, K.-Y. and Zeger, S. L. (1986) Longitudinal data analysis using "
       "generalized linear models. *Biometrika* **73**(1), 13–22.",
       "Marsden, P. V. and Friedkin, N. E. (1993) Network studies of social "
       "influence. *Sociological Methods & Research* **22**(1), 127–151.",
       "Nickell, S. (1981) Biases in dynamic models with fixed effects. "
       "*Econometrica* **49**(6), 1417–1426.",
       "Opsahl, T., Agneessens, F. and Skvoretz, J. (2010) Node centrality in "
       "weighted networks. *Social Networks* **32**(3), 245–251.",
       "Shalizi, C. R. and Thomas, A. C. (2011) Homophily and contagion are "
       "generically confounded in observational social network studies. "
       "*Sociological Methods & Research* **40**(2), 211–239.",
       "Shortreed, S., Handcock, M. S. and Hoff, P. (2006) Positional "
       "estimation within a latent space model for networks. *Methodology* "
       "**2**(1), 24–33.",
       "Tjur, T. (2009) Coefficients of determination in logistic regression "
       "models — a new proposal: the coefficient of discrimination. "
       "*The American Statistician* **63**(4), 366–372.",
       "Xu, R. (2018) Alternative estimation methods for identifying contagion "
       "effects in dynamic social networks. *Social Networks* **54**, 101–117.",
       "Xu, R. (2021) Estimating social influence effects in networks using a "
       "latent space adjusted approach in R. *The R Journal* **13**(2), 57–69."),

    h3("This page"),
    p("Every code block and every result above was produced by running the "
      "code in a live R session against this version of the package. "
      "The build date and version are in the header."),
)
