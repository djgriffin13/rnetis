# -*- coding: utf-8 -*-
"""The Selection path, the reference pages, and the help pages."""

from build import (
    page, p, h3, h4, ul, ol, quote, note, table, code, result, block,
    refusal, codeonly, fig, card, cards, rare, filterbox,
)

# ===========================================================================
# Path B
# ===========================================================================
page(
    "Path B · Selection", "The question", "Selection: the question",

    p("A Selection model asks what predicts a tie. The outcome is the relation "
      "itself; the predictors are properties of the sender, the receiver, the "
      "pair, and the relation's own history."),

    p("`fit_selection()` fits an **additive and multiplicative effects** model "
      "(Hoff 2005, 2021) through `amen::ame()`. The additive part gives every "
      "actor a sender effect and a receiver effect, so a person who nominates "
      "everybody does not make every covariate look important. The "
      "multiplicative part, if you ask for it, captures the third-order "
      "structure that additive terms cannot."),

    quote("This is a Bayesian model fitted by MCMC, not a regression. "
          "Everything on these pages is a posterior summary, the chain has to "
          "be looked at, and a fit takes seconds to minutes rather than "
          "milliseconds."),

    h3("The data"),
    block("b-demo"),
    p("Twenty nodes, observed twice, with one node variable (`tenure`) and one "
      "relation (`tie`). Note `complete_dyadic`: every ordered pair was "
      "observed, so the count is the 380 ordered pairs that 20 nodes make "
      "(20 × 19), one row per pair per wave. A zero here is a measured "
      "absence, not a gap, which is why the fits below declare "
      "`zero_meaning = \"measured_zero\"`."),

    h3("The path from here"),
    ol("**[Specify a model](#p-specify-a-model)** — the role grammar, and "
       "why a bare covariate is an error.",
       "**[Fit and read](#p-fit-and-read)** — the chain, the coefficients, "
       "the design warnings, and the diagnostic plots.",
       "**[Effect sizes](#p-effect-sizes)** — turning a probit coefficient "
       "into a probability contrast anyone can read.",
       "**[Several groups](#p-several-groups)** — fitting each network "
       "separately and combining, and what pooling assumes away.",
       "**[One hierarchical model](#p-one-hierarchical-model)** — random "
       "intercepts and slopes across groups, in one chain."),
)

page(
    "Path B · Selection", "1 · Specify a model", "Specify a model",

    p("Every predictor in a Selection formula names its role. A variable "
      "attached to a person can enter as a property of the sender, of the "
      "receiver, or both, and these are different hypotheses — so rnetis "
      "will not choose:"),
    refusal("b-bare-refuse"),

    table(["Term", "Asks"], [
        ["`sender(tenure)`", "Does a longer-tenured actor **send** more ties?"],
        ["`receiver(tenure)`", "Does a longer-tenured actor **receive** more?"],
        ["`dyad(same_unit)`", "Is a property of the *pair* associated with a tie? "
         "Held as a tie variable, not a node variable."],
        ["`group(size)`", "A group-level predictor. Only meaningful in a "
         "[hierarchical model](#p-one-hierarchical-model)."],
        ["`prior_tie()`", "Did the relation exist at the previous wave?"],
        ["`prior_reciprocity()`", "Did the *reverse* relation exist at the "
         "previous wave?"],
    ]),

    h3("Building a pair attribute"),
    p("`demo_selection` has one node variable and one relation, so `dyad()` "
      "has nothing to read yet. A pair attribute is a tie variable: one value "
      "per ordered pair. Here, whether the two actors are in the same unit."),
    block("b-prepare"),
    note("The `tie_meta` declarations matter. `zero_meaning = \"measured_zero\"` "
         "says a zero is an observed absence rather than an unasked question "
         "— which, on complete dyadic data, is exactly what it is.")
    ,

    h3("Two waves make one outcome"),
    p("`prior_tie()` uses the earlier wave as an initial condition, so two "
      "waves give **one** modelled outcome and the model is about the "
      "transition. Without a lagged relation term, both waves would supply an "
      "outcome, and that would treat two observations of the same actors as "
      "independent. rnetis refuses rather than doing it:"),
    refusal("b-waves-refuse"),
    p("The message names both ways out, and they are different models: "
      "`prior_tie()` models the transition; `waves = c(1, 2)` fits the later "
      "wave and leaves the earlier one available to lagged predictors."),

    rare("Lagging an attribute as well",
         p("`sender()`, `receiver()` and `dyad()` all take `lag`. The default "
           "is `lag = 0`, the outcome wave, which is fitted and recorded with "
           "a design warning you will see on the next page. `lag = 1` reads "
           "the previous wave instead, and is the more defensible predictor "
           "whenever the attribute could plausibly be a consequence of the "
           "tie.")),
)

page(
    "Path B · Selection", "2 · Fit and read", "Fit and read",

    p("The chain settings are arguments, not defaults to accept blindly. "
      "`nscan` is the number of iterations after `burn`, and `odens` thins "
      "them. These settings take about fifteen seconds on an ordinary laptop."),
    block("b-fit"),

    h3("What the summary adds"),
    block("b-summary"),
    quote("**Coefficients are on the latent probit scale.** `exp(coefficient)` "
          "is not an odds ratio for this model, and because the coefficients "
          "shift with the modelled sender, receiver and dyadic heterogeneity, "
          "comparing them across specifications is not like comparing linear "
          "regression coefficients. For magnitude, use the probability-scale "
          "contrasts on the [next page](#p-effect-sizes)."),

    h3("Findings recorded at fit time"),
    p("The summary ended with three warnings. They are data, not text:"),
    block("b-diagnostics"),
    p("Each carries a full statement:"),
    block("b-diagnostics-detail"),
    note("`tenure` is static in this dataset, so `lag = 1` would read the same "
         "values; the warning still fires, because the design claim is about "
         "measurement order, not about whether the numbers happen to differ."),

    h3("What was actually fitted"),
    block("b-waves"),
    block("b-estimates"),
    block("b-glance"),

    h3("Where the variance sits"),
    p("The additive effects are part of the model, and their variances are "
      "reported alongside the coefficients:"),
    block("b-vc"),
    note("On a binary probit the dyadic residual variance is fixed at 1 for "
         "identification. The shares are relative to that."),

    h3("Look at the chain"),
    p("A posterior summary means nothing if the chain did not mix. Four panels "
      "are worth a look on every fit:"),
    block("b-plot-trace"),
    fig("b-plot-trace", "Traces. You are looking for a stationary band, not a trend."),
    block("b-plot-mcmc"),
    fig("b-plot-mcmc", "MCMC diagnostics: effective sample size and autocorrelation."),
    block("b-plot-posterior"),
    fig("b-plot-posterior", "Posterior densities for each coefficient."),
    block("b-plot-coef"),
    fig("b-plot-coef", "Coefficients with 95% credible intervals."),

    p("Every panel has its table behind it:"),
    block("b-plot-data-trace"),

    h3("Actor effects and the multiplicative part"),
    block("b-plot-actor"),
    fig("b-plot-actor", "Each actor's sender and receiver effect."),
    p("`R` sets the dimension of the multiplicative term. `R = 0`, the "
      "default, is additive effects only; a positive `R` adds latent factors "
      "that capture structure the additive terms cannot:"),
    block("b-fit-multiplicative"),
    block("b-plot-mult"),
    fig("b-plot-mult", "The rotation-invariant multiplicative structure at `R = 2`."),
    note("A latent position here is identified only up to rotation, as in "
         "[Path A, page 6](#p-homophily-and-latent). The panel shows the "
         "rotation-invariant structure for that reason."),

    rare("Reciprocity as a predictor",
         p("`prior_reciprocity()` asks whether the reverse relation existing "
           "at the previous wave predicts this one — a different question "
           "from the within-dyad residual correlation the AME model already "
           "estimates."),
         block("b-reciprocity")),
)

page(
    "Path B · Selection", "3 · Effect sizes", "Effect sizes",

    p("A probit coefficient is not a magnitude anyone can read. "
      "`selection_effect()` converts one into a contrast in predicted tie "
      "probability."),
    block("b-effect-prior"),
    p("A binary term is compared as presence against absence. Read the "
      "closing sentence: this interval reflects uncertainty in the "
      "coefficients **only**, because the fit keeps the actor effects as "
      "posterior means. It is narrower than a fully propagated one, and it "
      "says so rather than leaving you to find out."),

    h3("Three estimands, not one"),
    p("`type` chooses *which dyad* the contrast is about, and they are "
      "genuinely different quantities:"),
    table(["`type =`", "The dyad it is about", "What the interval covers"], [
        ['`"conditional"` (default)',
         "Averaged over the observed dyads, each actor's effect at its fitted value.",
         "Coefficients only."],
        ['`"population"`',
         "A randomly drawn dyad, integrated over the actor-effect distribution.",
         "Coefficients and the additive actor-effect variances."],
        ['`"reference"`',
         "A hypothetical pair with actor effects at zero.",
         "Coefficients only."],
    ]),

    p("A continuous term moves each dyad from its observed value by `delta`:"),
    block("b-effect-pop"),
    block("b-effect-ref"),
    note("The three numbers differ, and none of them is “the” effect. "
         "Which one belongs in your paper depends on whether you are "
         "describing these actors or a population they stand for — so "
         "state which you reported."),

    h3("Explained variation"),
    block("b-pseudo"),
    quote("Two of those quantities are this package's own constructions and "
          "say so in the output. The third is Tjur's D. They answer different "
          "questions, and the note about the probit's fixed residual variance "
          "is the reason a latent-scale share is not comparable with a linear "
          "model's R-squared. `?pseudo_r2` is worth reading before any of it "
          "goes in a manuscript."),
)

page(
    "Path B · Selection", "4 · Several groups", "Several groups",

    p("With several groups, `fit_selection()` fits each one on its own and "
      "then combines the coefficients by inverse-variance weighting (Cochran "
      "1954; Hedges & Olkin 1985)."),
    block("b-two-groups"),
    block("b-pooled"),
    block("b-pooled-effect"),

    h3("What pooling assumes away"),
    quote("Pooling assumes a coefficient means the same thing in every group. "
          "Between-group differences are **assumed away, not estimated**. If "
          "the question you care about is whether groups differ, this is the "
          "wrong tool and the [hierarchical model](#p-one-hierarchical-model) "
          "is the right one."),
    p("Pooling also works one coefficient at a time, so there is no joint "
      "covariance to take a contrast from, and asking for one is an error "
      "rather than a number:"),
    refusal("b-pooled-vcov-refuse"),

    h3("Leaving the groups separate"),
    p("`pool_beta = FALSE` does not combine them at all, and `coef()` returns "
      "one row per group:"),
    block("b-separate"),
    block("b-plot-groups"),
    fig("b-plot-groups", "Per-group estimates, unpooled."),

    h3("A group-level predictor is refused here"),
    p("Within any single group, a group-level variable is constant — so "
      "it is collinear with that group's intercept, and separate per-group "
      "fits have nothing to estimate it from:"),
    refusal("b-group-refuse"),
    p("Group-level predictors belong in the hierarchical model, where they "
      "enter as moderators of the random coefficients. That is the "
      "[next page](#p-one-hierarchical-model)."),
)

page(
    "Path B · Selection", "5 · One hierarchical model", "One hierarchical model",

    quote("**This model is this package's own construction** from published "
          "components: the AME model, hierarchical partial pooling, and half-t "
          "and LKJ priors. It is not a published method under a published "
          "name. `?hierarchical_selection` specifies the model, the priors and "
          "the algorithm. Its tests establish that it computes what that page "
          "specifies; they do not establish its statistical performance."),

    p("`pooling = selection_pooling()` fits one model over every group in a "
      "single Markov chain. A coefficient you name in `random` gets a value "
      "for each group, drawn from a between-group distribution, and a "
      "group-level predictor enters as a moderator of those coefficients."),

    h3("Twelve teams that genuinely differ"),
    p("These twelve simulated teams differ in baseline and in sender slope, so "
      "there is something for the model to find:"),
    block("b-teams"),

    h3("The pooling specification"),
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
      "estimated rather than assumed."),

    p("**`groups`** gives each team's own values:"),
    block("b-hier-groups"),

    p("**`convergence`** gives R-hat and effective sample sizes, for every "
      "parameter in the model. These are the rows to read first — the "
      "shared coefficient, the population means, and the between-group "
      "standard deviations:"),
    block("b-hier-conv"),
    p("The rest of the table is one row per group coefficient and one per "
      "nuisance parameter — for twelve groups, seventy-odd more rows. "
      "Summarise rather than read them one by one:"),
    block("b-hier-conv-rest"),
    note("The `tau` rows have far smaller effective sample sizes than the "
         "`mu` rows. That is usual: a between-group standard deviation is "
         "estimated from twelve groups, and a variance parameter near zero "
         "mixes slowly. It is the parameter to watch when deciding whether to "
         "run the chain longer."),
    note("Run two or more chains. Later chains start from dispersed values, so "
         "R-hat compares chains that began apart — which is the only way "
         "it means anything."),

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

    h3("Where Path B ends"),
    p("You can now specify a tie model in which every predictor's role is "
      "explicit, fit it with additive and multiplicative actor effects, check "
      "the chain, report magnitudes as probability contrasts with a named "
      "estimand, handle several networks either by pooling or by leaving them "
      "apart, and fit one hierarchical model that estimates between-group "
      "variation instead of assuming it away."),
    note("`?selection_pooling` covers moderators, correlated random effects "
         "(`covariance = \"unstructured\"`) and the prior on `tau`."),
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
         "`weight_transform`, `lag`, `name`", "A node's position in a network."],
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

    h3("A rule you can see"),
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
    "Reference", "What rnetis refuses", "What rnetis refuses",

    p("rnetis refuses rather than guessing wherever a guess could change a "
      "number without anybody noticing. These are the refusals this tutorial "
      "met, in full, in one place — each with the decision it is asking "
      "you to make. Type into the box to narrow the list."),

    filterbox(
        "Filter the refusals…",
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
               "is a decision about your design."),
             p("**Fix.** `waves = c(from, to)`."),
             step="influence"),
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
        card("symmetrize() with no rule", refusal("n-symmetrize-refuse"),
             p("**Why.** On a weighted network, max, min, sum and mean are "
               "different operations."),
             p("**Fix.** Name one."),
             step="networks"),
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
         "and persistent actor heterogeneity."],
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
      "to contain both nothing and a good deal. That is not a quirk of the "
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

    p("Everything in Path A is fast. Path B fits Markov chains, and the "
      "settings are yours."),

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
       "Opsahl, T., Agneessens, F. and Skvoretz, J. (2010) Node centrality in "
       "weighted networks. *Social Networks* **32**(3), 245–251.",
       "Shalizi, C. R. and Thomas, A. C. (2011) Homophily and contagion are "
       "generically confounded in observational social network studies. "
       "*Sociological Methods & Research* **40**(2), 211–239.",
       "Shortreed, S., Handcock, M. S. and Hoff, P. (2006) Positional "
       "estimation within a latent space model for networks. *Methodology* "
       "**2**(1), 24–33.",
       "Xu, R. (2018) Alternative estimation methods for identifying contagion "
       "effects in dynamic social networks. *Social Networks* **54**, 101–117.",
       "Xu, R. (2021) Estimating social influence effects in networks using a "
       "latent space adjusted approach in R. *The R Journal* **13**(2), 57–69."),

    h3("This page"),
    p("Every code block and every result above was produced by running the "
      "code in a live R session against this version of the package. "
      "The build date and version are in the header."),
)
