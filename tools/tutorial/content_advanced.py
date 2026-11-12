"""The two advanced walkthroughs.

WHO THESE ARE FOR. A reader who has been through pages A1 to A8 or B1 to B8 and
now has to make the choices those pages made for them. Each walkthrough goes
back over the same steps in the same order and, at each one, shows the
alternatives: what else the function can do, what the other settings mean,
and what changes in the result.

ONE THING VARIES AT A TIME. Every block starts from the model the main pages
fitted, on the same data, and changes one decision. A reader can therefore
compare any block with the result they already know.

EACH ENDS IN A TABLE OF FUNCTIONS, grouped by the step they belong to.
"""

from build import (block, codeonly, fig, h3, h4, note, p, page, quote, rare,
                   refusal, table, ul)

GX = "Advanced"

# ---------------------------------------------------------------------------
# Influence in depth
# ---------------------------------------------------------------------------
page(
    GX, "Influence in depth", "Advanced walkthrough: Influence",
    '<p class="lede">'
    "This page goes back over the Influence sequence and shows the choices "
    "the main pages made for you: what the alternatives are at each step, "
    "and what each one changes.</p>",

    h3("Before you start"),
    p("This page assumes you have read the Influence sequence, "
      "[A1](#p-influence-run-a-model-first) to "
      "[A8](#p-the-multilevel-influence-model). It uses the same team and "
      "the same model, and every block changes one decision about that "
      "model, so each result can be compared with the one you already "
      "know: a contagion effect of about 0.48."),
    block("xi-setup"),
    p("The sections follow the order of an analysis: the data, the exposure "
      "term, the rest of the formula, the fitted model, the latent space, "
      "several teams, and planning a study. The page ends with a "
      "[table of functions](#adv-influence-functions)."),

    h3("Check the data before modelling it"),
    p("`diagnose_data()` reads a data object and reports anything that "
      "would cause trouble later: variables that never vary, people with "
      "no ties, waves with missing rows."),
    block("xi-diagnose"),
    p("This team has nothing to report. On your own data, run it before "
      "the first model. A problem found here costs a line of output, and "
      "the same problem found later costs a model that fits and misleads."),
    rare("The declarations that change every later number",
         p("Three things are declared when the data object is built, and "
           "each changes what an exposure term computes."),
         table(["Declaration", "The alternatives", "What it changes"], [
             ["What a zero means",
              '`zero_meaning = "measured_zero"`, or leave pairs out',
              "Whether a pair with no tie was observed as a non-tie or was never asked about"],
             ["Direction", "`directed = TRUE` (default) or `FALSE`",
              "Whether *i* to *j* and *j* to *i* are two ties or one"],
             ["Weights", "`weighted = TRUE`, or a 0/1 column",
              "Whether stronger ties carry more of the content"],
         ]),
         p("rnetis never guesses these. A pair missing from the tie table "
           "is not treated as a zero, and a zero is not treated as missing, "
           "unless you say so.")),

    h3("Exposure is a column you can look at"),
    p("In a model you write `exposure()` inside the formula. To see the "
      "numbers it produces, `compute_exposure()` returns the same quantity "
      "as an ordinary column:"),
    block("xi-compute"),
    p("Each value is the average first-survey affect of the people who "
      "confide in that person. Looking at this column once on your own "
      "data is a good check that the exposure means what you intend."),
    p("With that column in hand, the whole model can be fitted by hand "
      "with `lm()`:"),
    block("xi-lm"),
    p("The coefficients are the same as the ones at the top of the page. "
      "`ar()` and `exposure()` are shorthand for columns, and the estimator "
      "is ordinary least squares. What `fit_influence()` adds is the "
      "bookkeeping: matching people across waves, building the exposure "
      "from the right wave, handling people with no ties, and recording "
      "every decision."),
    note("Use `compute_exposure()` for looking and `exposure()` for "
         "fitting. A model fitted to a column you built by hand does not "
         "know how that column was made."),

    h3("Average or total exposure"),
    p("`normalize` decides how a person's ties are combined. The two "
      "settings answer different questions."),
    table(["Setting", "What it computes", "The question it answers"], [
        ['`normalize = "row"`', "The average over a person's ties",
         "What are the people around me like?"],
        ['`normalize = "none"` (the default)', "The total over a person's ties",
         "How much reaches me altogether?"],
    ]),
    block("xi-sum"),
    p("The two columns rank people differently. Someone with many "
      "confiders has a large total even if each confider's affect is "
      "ordinary. Choose by theory: a norm or a climate is an average, and "
      "a dose or a load is a total. The data here were built with an "
      "average, which is why page A4 found nothing with the total."),

    h3("People with nobody to average over"),
    p("An average needs at least one tie. To see what happens without "
      "one, count two people as tied only if they talk four or more hours "
      "a week:"),
    block("xi-isolate-na"),
    p("Those people have no close contacts, so their average exposure is "
      "undefined and is recorded as missing. The model then leaves them "
      "out, and says how many rows it used:"),
    block("xi-isolate-fit"),
    p("`isolate_rule` controls this, and there are three choices."),
    table(["Setting", "What happens to a person with no ties"], [
        ['`isolate_rule = "na"` (the default)',
         "Exposure is missing, and the person is left out of the model"],
        ['`isolate_rule = "zero"`',
         "Exposure is set to 0, and the person stays in"],
        ['`isolate_rule = "error"`',
         "The fit stops, so you find out that isolates exist"],
    ]),
    block("xi-isolate-zero"),
    p("Setting the exposure to zero keeps everyone and changes the "
      "estimate. Affect here runs from about 1 to 5, so a zero is far "
      "below any value a real average could take, and the people given it "
      "pull the line toward themselves. Use it only when zero is a "
      "meaningful exposure, which it is for a total and rarely is for an "
      "average. When you expect no isolates at all, ask rnetis to stop:"),
    refusal("xi-isolate-error"),

    h3("Weighted or binary ties"),
    p("`talk` is a count of hours. Used as it is, each contact is weighted "
      "by how much the pair talks. `binarize()` turns it into a yes/no tie "
      "by a rule you name. The table shows the exposure coefficient under "
      "four readings of the same variable:"),
    block("xi-weighted"),
    p("The estimate depends on the rule. A threshold changes who counts as "
      "a contact at all, so it changes the people each average is taken "
      "over. That is why the rule is something you write down and not a "
      "default."),
    table(["Rule", "A tie exists where"], [
        ["`nonzero()`", "the value is not zero"],
        ["`positive()`", "the value is above zero"],
        ["`threshold(c)`", "the value is at least `c`"],
        ["`above_mean()`, `above_median()`", "the value is above the network's mean or median"],
        ["`quantile_threshold(q)`", "the value is in the top share `q`"],
        ["`custom_binarize_rule(...)`", "your own function says so"],
    ]),
    p("If the binary version is what you measured, store it as its own "
      "column of the tie table. `binarize()` in a formula is for trying a "
      "different reading of a network you also use as it is."),

    h3("Direction, and more than one network"),
    p("The default reads a tie as running from source to target: affect "
      "reaches a person from those who confide in them. Two other readings "
      "are one function away."),
    block("xi-direction"),
    ul("**`symmetrize(confides, \"max\")`** treats a tie in either "
       "direction as a connection. The rule says how to combine the two "
       "directions: `\"max\"`, `\"min\"`, `\"sum\"` or `\"mean\"`.",
       "**`multiplex(confides, advice, combine = \"union\")`** makes one "
       "network out of two. `\"union\"` keeps a tie present in either, "
       "and `\"intersection\"` keeps only ties present in both.",
       "**`direction = \"target_influences_source\"`** inside `exposure()` "
       "reverses the flow, as page A4 showed."),
    p("Combining networks assumes a rule. The alternative is to give each "
      "network its own exposure term and let the model say which one "
      "carries the effect:"),
    block("xi-two-exposures"),
    p("Confiding carries the effect and advice does not, which is how the "
      "data were built."),

    h3("Timing"),
    p("`x_lag` and `w_lag` say when the content and the network were "
      "measured, counted back from the outcome's wave. The main pages use "
      "1 for both. Setting `w_lag = 0` keeps earlier affect as the content "
      "and takes the network from the later survey:"),
    block("xi-wlag"),
    p("That asks a different question: whether a person's affect follows "
      "the earlier affect of the people who confide in them *now*. It "
      "also lets the network be a result of the change you are "
      "explaining, so use it only when the theory calls for it."),
    p("Setting both lags to 0 makes the exposure contemporaneous: affect "
      "now explained by confiders' affect now. Everyone's outcome is then "
      "on both sides of the equation, and ordinary least squares is no "
      "longer valid. rnetis switches estimator and says so:"),
    block("xi-sar"),
    p("The header names the estimator, a simultaneous network "
      "autocorrelation model fitted through the `sna` package, and reports "
      "the quantities that say whether the system is well behaved. Its "
      "table reports z tests and has no R-squared. This is a different "
      "model from the lagged one, with different assumptions, and its "
      "coefficient is not comparable with the one from pages A1 to A8."),

    h3("Covariates and moderation"),
    p("A bare variable name adds a covariate at its value in the outcome's "
      "wave. `lagged()` adds it at its earlier value, which is the safer "
      "choice for anything that could itself respond to the outcome:"),
    block("xi-lagged"),
    p("To ask whether contagion is stronger for some people than others, "
      "multiply the exposure term by a covariate. Here the question is "
      "whether it differs for people who work remotely:"),
    block("xi-moderation"),
    p("The row `ex_affect_confides:remote` is the difference in the "
      "contagion effect between remote and on-site people. "
      "`influence_effect()` turns the interaction into the effect at each "
      "value of the moderator:"),
    block("xi-moderation-effect"),
    p("With forty people the difference is poorly estimated. One rule is "
      "enforced: one side of the product must be an ordinary covariate. "
      "Two network terms multiplied together are refused, because the "
      "result has no clear meaning."),
    rare("A person's position in the network as a predictor",
         p("`centrality()` adds a person's position as a covariate, here "
           "the number of people who confide in them:"),
         block("xi-centrality"),
         p("`network_stat()` does the same for a characteristic of the "
           "whole network, which is useful with several teams.")),

    h3("Comparing models"),
    p("An influence model fitted by least squares has a likelihood, so "
      "`AIC()` and `BIC()` work. They compare models fitted to the **same "
      "rows**, so check the row counts first:"),
    block("xi-compare"),
    p("Both models use all forty people, so the comparison is fair. AIC "
      "and BIC are lower with the exposure term, and R-squared is higher. "
      "Three readings now agree: the interval excludes zero, the "
      "information criteria prefer the model, and the effect is large "
      "enough to matter. When they disagree, that tells you something "
      "specific about the model."),
    note("A model that drops people, for instance because they have no "
         "ties, is fitted to fewer rows. Its AIC cannot be compared with "
         "the AIC of a model that kept them."),

    h3("Effect sizes and getting numbers out"),
    p("`influence_effect()` reports the predicted change in the outcome "
      "across a range of exposure. The default range is the 10th to the "
      "90th percentile. `probs` changes it, here to the middle half of "
      "the team:"),
    block("xi-effect-probs"),
    p("For tables and further analysis, three functions return data "
      "frames in place of printed output."),
    table(["Function", "Returns"], [
        ["`netis_estimates(fit)`",
         "One row per coefficient: estimate, standard error, interval, role"],
        ["`netis_glance(fit)`",
         "One row for the model: estimator, rows, R-squared, residual spread"],
        ["`netis_estimand(fit)`",
         "What the model estimates: the transition and the estimator, with the reason"],
    ]),
    block("xi-estimates"),
    p("`tidy()` and `glance()` are the same two functions under the names "
      "the `broom` package uses, and `coef()`, `confint()`, `vcov()`, "
      "`AIC()` and `BIC()` work as they do for `lm()`."),

    h3("Choosing the latent space"),
    p("[Page A7](#p-the-latent-space-influence-model) added `latent()` with "
      "two dimensions. Two questions come with it: how many dimensions, "
      "and how much the answer depends on the random numbers behind the "
      "positions."),
    block("xi-latent-d"),
    p("There is no test that picks `d`. Each dimension uses up a "
      "coefficient and widens the interval, and the estimate can move a "
      "good deal between settings. Report the result at more than one "
      "value of `d`, and say so if the conclusion depends on it."),
    p("The positions come from a random sampler, so a different `seed` "
      "gives different positions and a slightly different adjusted "
      "estimate:"),
    block("xi-latent-seed"),
    p("The spread across seeds is how much of the adjusted estimate is "
      "due to the sampler alone. If it is large beside the interval, the "
      "first step needs a longer run, which `control =` sets. Always fix "
      "the seed in work you report."),
    table(["Argument of `latent()`", "What it does"], [
        ["`d`", "The number of dimensions"],
        ["`homophily`",
         "Person characteristics the network model accounts for directly: "
         "see the box on page A7"],
        ["`terms`", "Further network-model terms, passed to `latentnet`"],
        ["`lag`", "Which wave's network the positions come from"],
        ["`seed`", "The random-number seed for the positions"],
        ["`control`", "Sampler settings, passed to `latentnet::ergmm.control()`"],
    ]),

    h3("Several teams"),
    p("[Page A8](#p-the-multilevel-influence-model) added `(1 | group)`. "
      "Three more quantities describe how much the teams matter, and they "
      "are often confused."),
    block("xi-mg"),
    p("These are the **conditional** variances: what is left between "
      "teams and within them after the model's predictors."),
    block("xi-icc"),
    p("**ICC(1)** is the share of the outcome's variance that lies between "
      "teams **before** any predictor. It describes the data and not the "
      "model, so it is a different number from the conditional share "
      "above. **ICC(2)** is the reliability of a team's average:"),
    block("xi-icc2"),
    p("`rwg()` and `rwg_j()` measure agreement within a team, which is a "
      "third thing again, and `netis_reliability()` reports the set."),
    h4("Are the standard errors safe?"),
    p("`influence_se_comparison()` puts the model's standard errors beside "
      "cluster-robust ones, which allow people in the same team to share "
      "unexplained variation:"),
    block("xi-se"),
    p("Ratios near 1 mean the two agree. The last column is the degrees "
      "of freedom of the robust version: with ten teams it is small, so "
      "the robust numbers are themselves rough. On a single team there is "
      "nothing to compare, and rnetis says why:"),
    refusal("xi-se-refuse"),

    h3("Planning a study"),
    p("`simulate_influence()` generates a network, an outcome and a "
      "contagion effect of a size you choose. Fitting the model to "
      "simulated data answers a question worth asking before collecting "
      "anything: *would a study of this size detect the effect I "
      "expect?*"),
    block("xi-simulate"),
    p("The true effect is 0.5 in all three. With 40 people the interval "
      "is too wide to say much, and it narrows as the team grows. Set "
      "`n_groups`, `n_waves`, `density` and the effect sizes to match "
      "your own design."),

    h3("Functions used in an Influence analysis", anchor="adv-influence-functions"),
    table(["Step", "Function", "What it is for"], [
        ["Data", "`netis_data()`", "Build the data object from a node table and a tie table"],
        ["", "`netis_data_from_edgelist()`, `_from_adjacency()`, `_from_igraph()`, `_from_network()`",
         "Build it from other formats"],
        ["", "`netis_nodes()`, `netis_ties()`, `netis_schema()`",
         "Get the tables and the declarations back out"],
        ["", "`diagnose_data()`", "Report problems in the data before modelling"],
        ["Exposure", "`exposure()`", "The exposure term, inside a formula"],
        ["", "`compute_exposure()`", "The same quantity as a column, for looking"],
        ["", "`binarize()` with `threshold()`, `nonzero()`, `above_mean()`, `quantile_threshold()`",
         "Read a valued tie as yes/no"],
        ["", "`symmetrize()`, `multiplex()`", "Combine directions, or combine networks"],
        ["Formula", "`ar()`", "The outcome at the earlier wave"],
        ["", "`lagged()`", "A covariate at its earlier value"],
        ["", "`centrality()`, `network_stat()`",
         "A person's position, or a network's characteristic, as a covariate"],
        ["", "`latent()`", "The latent-space adjustment"],
        ["", "`(1 | group)`", "A random intercept across groups"],
        ["Fitting", "`fit_influence()`", "Fit the model"],
        ["Reading", "`summary()`, `print()`", "The fitted model, in full or in brief"],
        ["", "`netis_estimates()`, `netis_glance()`, `netis_estimand()`",
         "The coefficients, the model summary and the estimand, as data"],
        ["", "`coef()`, `confint()`, `vcov()`, `AIC()`, `BIC()`", "As for `lm()`"],
        ["", "`influence_effect()`", "The predicted change across a range of exposure"],
        ["", "`netis_assumptions()`", "What the result rests on"],
        ["", "`netis_diagnostics()`", "What was noticed while fitting"],
        ["Checking", "`netis_dependence()`", "Pattern left in the residuals"],
        ["", "`influence_se_comparison()`", "Model standard errors beside cluster-robust ones"],
        ["Groups", "`netis_variance_components()`", "Variance between and within groups"],
        ["", "`icc1()`, `icc2()`, `rwg()`, `rwg_j()`, `netis_reliability()`",
         "Variance share, reliability and agreement"],
        ["Plotting", "`plot(data)`", "The network"],
        ["", "`plot(fit)`, with `what = \"effects\"`, `\"latent\"`, `\"heterogeneity\"`",
         "Coefficients, effect sizes, latent positions, group differences"],
        ["", "`plot_data()`", "The table behind any plot"],
        ["Planning", "`simulate_influence()`", "Simulated data with a known effect"],
    ]),
    p("Each function has a help page: `?exposure`, `?latent`, "
      "`?influence_effect` and so on."),
)

# ---------------------------------------------------------------------------
# Selection in depth
# ---------------------------------------------------------------------------
page(
    GX, "Selection in depth", "Advanced walkthrough: Selection",
    '<p class="lede">'
    "This page goes back over the Selection sequence and shows the choices "
    "the main pages made for you: what the alternatives are at each step, "
    "and what each one changes.</p>",

    h3("Before you start"),
    p("This page assumes you have read the Selection sequence, "
      "[B1](#p-selection-run-a-model-first) to "
      "[B8](#p-the-multilevel-selection-model). It uses the same team and "
      "the same model, and each block changes one decision about that "
      "model. The result to compare against is a selection effect of about "
      "−0.47 on the affect gap."),
    block("xs-setup"),
    p("This reference fit runs the default chain. The other fits on this "
      "page run shorter chains (3,000 draws after 500 of warm-up) so that "
      "the page's code runs in about twenty minutes. For work you report, "
      "use the longer chain. The page ends with a "
      "[table of functions](#adv-selection-functions)."),

    h3("Check the design, and name the waves"),
    p("`diagnose_design()` reads the data object and reports anything "
      "about its design that a model would trip over, such as a single "
      "wave or a network that never changes:"),
    block("xs-design"),
    p("A selection model needs to know which survey is the outcome and "
      "which is the starting point. `prior_tie()` says so. Without it, and "
      "with two surveys, rnetis refuses to guess:"),
    refusal("xs-noprior-refuse"),
    p("The message gives both ways forward. `waves = c(1, 2)` names the "
      "surveys and fits the later network without the earlier tie as a "
      "predictor:"),
    block("xs-waves"),
    p("The two models answer different questions. With `prior_tie()` the "
      "coefficients describe which ties **formed or ended**. Without it "
      "they describe which ties **exist**, including all the ones that "
      "were already there, so both effects look larger. Selection is a "
      "claim about change, so the model with `prior_tie()` is the one that "
      "tests it."),

    h3("Another tie as a predictor"),
    p("A column of the tie table can be a predictor whether it is a "
      "characteristic of the pair or another relationship between them. "
      "Does asking someone for advice go with coming to confide in them?"),
    block("xs-tie-predictor"),
    p("`dyd_advice_lag1` is the answer: the effect of an advice tie at the "
      "first survey on a confiding tie at the second. Nothing links the "
      "two in these data. This is usually a better way to study several "
      "relationships than combining them into one network, because it "
      "gives a coefficient for the link between them."),

    h3("A yes/no tie or an amount"),
    p("Everything so far modelled whether a tie exists. `talk` is an "
      "amount, hours a week, and `family = \"nrm\"` models the amount "
      "itself. `talk` was recorded once, so there is no earlier value to "
      "condition on, and `waves` names the surveys:"),
    block("xs-valued"),
    p("Three things changed. The family line reads *valued tie (normal)*. "
      "The coefficients are in the outcome's own units, hours, so they can "
      "be read directly with no probit scale and no `selection_effect()`. "
      "And the scale warning has gone from the header."),
    p("The fit check matters more here than the coefficients. A count of "
      "hours is not bell-shaped, and the check reports which features of "
      "the network a normal model fails to reproduce. `binarize =` fits "
      "the same variable as a yes/no tie, by a rule you name:"),
    block("xs-binarize"),
    p("These are probit coefficients again, for whether two people talk "
      "two or more hours a week. Turning an amount into yes/no throws away "
      "the amounts, so do it only when presence is the question. The two "
      "settings do not combine:"),
    refusal("xs-binarize-refuse"),

    h3("A tie with no direction"),
    p("Some relationships have no direction: working on the same task, "
      "being related, or here a **mutual** confiding tie, where each "
      "confides in the other. An undirected tie table lists each pair "
      "once, and the data object is declared `directed = FALSE`:"),
    block("xs-undirected-data"),
    p("780 ties is each of the 40 × 39 / 2 pairs, once. Fitting this as a "
      "directed model is refused:"),
    refusal("xs-undirected-refuse"),
    p("The reason is in the message: a directed model would count each "
      "pair twice. With `symmetric = TRUE` the model fits it as what it "
      "is:"),
    block("xs-symmetric"),
    block("xs-symmetric-vc"),
    p("Each person now has **one** tendency to have ties, where a directed "
      "model has a sending and a receiving tendency. The coefficients read "
      "as before. The fit check has fewer features to count, because "
      "out-degree and in-degree are the same thing when ties have no "
      "direction."),

    h3("Three ways to state an effect size"),
    p("`selection_effect()` converts a coefficient into a change in "
      "probability. There are three versions, and they answer slightly "
      "different questions."),
    table(["`type =`", "The change in probability..."], [
        ['`"conditional"` (the default)',
         "...averaged over the pairs in this network, with each person's own tendencies as estimated"],
        ['`"population"`',
         "...averaged over people like these, allowing for how much people's tendencies vary"],
        ['`"reference"`',
         "...for one hypothetical pair in which both people have exactly average tendencies"],
    ]),
    block("xs-effect-types"),
    p("Here they agree, because people in this team do not differ much in "
      "how widely they confide. They separate when sender and receiver "
      "variances are large. Say which one you report."),
    p("`delta` sets the size of the change being asked about. For a 0/1 "
      "predictor it is the difference between 0 and 1. For a number it "
      "defaults to one unit, and half a point of affect gap may be a more "
      "realistic difference:"),
    block("xs-effect-delta"),

    h3("How long a chain"),
    p("A chain that is too short gives numbers that look like results and "
      "are not. This one stops after 300 draws:"),
    block("xs-short"),
    p("Compare the reference fit:"),
    block("xs-long"),
    p("On the short chain the effective sample sizes are a few dozen, so "
      "each estimate rests on very little, and some R-hat values are well "
      "away from 1. On the reference fit R-hat is within a hundredth of 1 "
      "and the effective sample sizes are in the hundreds."),
    table(["If you see", "Do this"], [
        ["R-hat well above 1", "Raise `nscan`: the chain had not settled"],
        ["R-hat near 1 but a small ESS", "Raise `nscan`: the chain settled but is short"],
        ["A slow fit", "Lower `R` before lowering `nscan`"],
    ]),
    p("Raising `odens` does not help. It keeps fewer draws from the same "
      "chain. A second check is to fit again with another seed:"),
    block("xs-seed"),
    p("Two chains from different random numbers agree to within about a "
      "hundredth. If they did not, neither could be trusted."),

    h3("A fit check that fails"),
    p("The team data passes its fit check, so pages B5 and B7 never showed "
      "what failure looks like. This small firm is built to fail. Its "
      "twenty-four people sit in three offices and confide mostly within "
      "their own office, and the office is not in the data:"),
    block("xs-offices"),
    p("Fit the model with no latent factors and run the check:"),
    block("xs-misfit"),
    codeonly('plot(fit_r0, what = "gof")'),
    fig("xs-misfit-plot", "The same five panels as on page B6. Where the "
        "vertical line falls outside the shaded band, the model cannot "
        "produce what was observed."),
    p("The two triad features are far outside their ranges, with `ppp` at "
      "zero: no simulated network had as much clustering as the real one. "
      "Ties are also returned more often than the model expects. The "
      "pattern says what is wrong. Clustering the model cannot reach "
      "means there are groups it does not know about."),

    h3("Choosing R with the fit check"),
    p("Latent factors are how the model can learn about groups nobody "
      "recorded. Fit the same model at increasing `R` and watch the "
      "clustering feature. Each row is one fit:"),
    block("xs-choose-r"),
    p("Read down the simulated range. With no latent factors the model "
      "produces almost no clustering. One latent dimension moves the "
      "range a long way toward the observed value, and further dimensions "
      "do not move it again. That is the rule for choosing `R`: raise it "
      "while the features that were outside keep moving, and stop when "
      "they stop. Each added dimension costs two numbers per person, so "
      "the smallest `R` that does the work is the one to use."),
    p("Notice also that the observed value is still above the range. The "
      "latent factors account for much of the clustering in this firm and "
      "not all of it. That is worth saying when you report the model, and "
      "the check is what lets you say it."),
    h4("What the latent factors found"),
    block("xs-offices-r2"),
    codeonly('plot(fit_r2, what = "latent")'),
    fig("xs-offices-latent", "The firm at `R = 2`. Blue points are "
        "positions as a sender, orange points positions as a receiver, and "
        "grey lines are observed ties."),
    p("The people fall into three clusters, and they are the three "
      "offices: `q01` to `q08`, `q09` to `q16`, and `q17` to `q24`. Within "
      "a cluster the blue and orange points lie in the same direction "
      "from the centre, which is what makes ties inside it likely, and "
      "most grey lines stay inside a cluster. The model was never told "
      "about offices. Compare this with the single cloud on "
      "[page B7](#p-the-latent-factor-selection-model), where there was "
      "nothing to find."),
    h4("Measuring it is better than inferring it"),
    p("A picture like that is a prompt to find out what the clusters are. "
      "If the office can be recorded, it belongs in the model as a "
      "characteristic of the pair:"),
    block("xs-office-known"),
    p("With the office in the model and no latent factors at all, both "
      "triad features are well inside their ranges. A measured predictor "
      "that explains the clustering does better than latent factors that "
      "approximate it, and its coefficient can be interpreted. Use latent "
      "factors to find structure and to adjust for what you cannot "
      "measure, and prefer the measurement when you can get it."),

    h3("People's own tendencies"),
    p("Every person has a sender effect and a receiver effect. "
      "`plot_data()` returns them as a table, here sorted to show who is "
      "confided in most beyond what the predictors explain:"),
    block("xs-actor"),
    block("xs-vc"),
    p("The variances say how much people differ in each tendency. The "
      "covariance says whether people who confide widely are also confided "
      "in widely. The within-dyad correlation is the tendency for a tie "
      "to be returned, beyond what the predictors explain."),

    h3("Several teams, in more depth"),
    p("[Page B8](#p-the-multilevel-selection-model) showed three ways to "
      "handle several teams. They are three different models."),
    table(["Route", "The call", "What it assumes"], [
        ["Pooled", "the default", "The effect is the same in every team"],
        ["Separate", "`pool_beta = FALSE`", "Nothing: each team has its own estimate"],
        ["Hierarchical", "`pooling = selection_pooling(random = ...)`",
         "Teams' effects differ around a common average"],
    ]),
    p("For a hierarchical fit, `netis_hierarchy()` reports what varies "
      "between teams. `mu` is the average across teams and `tau` is the "
      "standard deviation between them:"),
    block("xs-hier"),
    block("xs-hier-groups"),
    p("`beta_g` is each team's own effect. `shrinkage` is how far that "
      "team's estimate was pulled toward the average: near 1 means the "
      "team had little information of its own, and near 0 means it kept "
      "its own estimate. Here `tau` is small, so the teams are estimated "
      "to be much alike and are pulled strongly together."),
    p("The pooled and hierarchical routes cannot be asked for at once:"),
    refusal("xs-pooling-refuse"),
    note("The hierarchical model is experimental: it is this package's "
         "own construction and has not been validated for statistical "
         "performance. See `?hierarchical_selection`."),

    h3("Functions used in a Selection analysis", anchor="adv-selection-functions"),
    table(["Step", "Function", "What it is for"], [
        ["Data", "`netis_data()`", "Build the data object; `directed = FALSE` for undirected ties"],
        ["", "`netis_nodes()`, `netis_ties()`", "Get the tables back out"],
        ["", "`diagnose_data()`, `diagnose_design()`",
         "Report problems in the data and the design before modelling"],
        ["Formula", "`sender()`, `receiver()`", "A characteristic of one end of the tie"],
        ["", "`dyad()`", "A characteristic of the pair, or another tie"],
        ["", "`prior_tie()`, `prior_reciprocity()`",
         "The tie, or the reverse tie, at the earlier wave"],
        ["Fitting", "`fit_selection()`", "Fit the model"],
        ["", "`family = \"bin\"` or `\"nrm\"`", "A yes/no tie, or an amount"],
        ["", "`binarize =` with `threshold()`, `nonzero()`, `above_mean()`",
         "Read a valued outcome as yes/no"],
        ["", "`symmetric = TRUE`", "An undirected tie"],
        ["", "`R =`", "The number of latent dimensions"],
        ["", "`waves =`", "Which surveys to use"],
        ["", "`nscan`, `burn`, `odens`, `seed`", "The chain"],
        ["", "`pool_beta = FALSE`", "Several teams, kept separate"],
        ["", "`pooling = selection_pooling()`", "Several teams, one hierarchical model"],
        ["Reading", "`summary()`, `print()`", "The fitted model, in full or in brief"],
        ["", "`netis_estimates()`, `coef()`", "The coefficients, as data"],
        ["", "`selection_effect()`", "A coefficient as a change in probability"],
        ["", "`netis_variance_components()`", "How much people differ in sending and receiving"],
        ["", "`netis_hierarchy()`", "What varies between teams, on a hierarchical fit"],
        ["", "`netis_diagnostics()`", "What was noticed while fitting"],
        ["Checking", "`netis_gof()`", "The fit check: observed network against simulated ones"],
        ["", "`pseudo_r2()`", "How much the predictors account for"],
        ["", "`plot_data(fit, what = \"mcmc\")`", "R-hat and effective sample size"],
        ["Plotting", "`plot(data)`", "The network"],
        ["", "`plot(fit)`", "The coefficients"],
        ["", "`what = \"gof\"`, `\"trace\"`, `\"posterior\"`, `\"mcmc\"`",
         "The fit check and the chain"],
        ["", "`what = \"actor_effects\"`, `\"heterogeneity\"`",
         "People's tendencies and their variances"],
        ["", "`what = \"latent\"`, `\"multiplicative\"`", "The latent coordinates and their product"],
        ["", "`what = \"groups\"`", "Each team's estimates"],
        ["", "`plot_data()`", "The table behind any plot"],
    ]),
    p("Each function has a help page: `?fit_selection`, `?selection_effect`, "
      "`?netis_gof` and so on."),
)
