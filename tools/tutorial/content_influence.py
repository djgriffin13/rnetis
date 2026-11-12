"""The Influence sequence, pages A1 to A8.

THE PLAN OF EVERY PAGE. Code first, and its real output; then what was just
run, a piece at a time; then, behind expandable boxes, everything a reader
might go looking for -- the functions used, other settings and what they
change, the equation. A novice reads the open text and has a result within
minutes. Someone who wants more opens the boxes.

ONLY THE FUNCTIONS A FIRST ANALYSIS NEEDS. The open text uses netis_data(),
fit_influence(), the terms inside its formula, summary() and plot(). Anything
else the package offers is named inside a box or left to the reference pages.

NUMBERS IN THE PROSE COME FROM THE CAPTURE. `F()` reads a value run-inf.R
recorded from the fitted object, so a sentence cannot quote a coefficient the
table above it does not show.
"""

import build
from build import (block, codeonly, eq, fig, h3, h4, note, p, page, quote,
                   rare, refusal, table, ul)

GA = "Influence"


def F(fid):
    """A value recorded by the capture script, for the prose to quote."""
    build.USED.append("fact-%s" % fid)
    v = build.read("fact-%s.txt" % fid)
    return v.strip() if v is not None else "[missing fact: %s]" % fid


# ---------------------------------------------------------------------------
# A1
# ---------------------------------------------------------------------------
page(
    GA, "A1 · Start here", "Influence: run a model first",
    '<p class="lede">Two lines of code, a fitted model, and what it says. '
    "The rest of the sequence goes back over how it worked.</p>",

    p("A team of forty people was surveyed twice. Each time, everyone "
      "reported their **affect** (how positive their mood has been, on a "
      "1-to-5 scale) and who they **confide** in. The question: *does a "
      "person's affect move toward the affect of the people who confide in "
      "them?* That is affect contagion, and this fits it:"),
    block("i1-run"),

    h3("What it says"),
    p("Look at the **Coefficients** table. It has one row per term in the "
      "model:"),
    ul("**`affect_lag1`** (%s). People's affect carries over. Someone one "
       "point higher at the first survey is about %s of a point higher at "
       "the second." % (F("i-ar"), F("i-ar")),
       "**`ex_affect_confides`** (%s). **This is the contagion effect.** "
       "For each point higher the average affect of the people who confide "
       "in you, your own affect at the second survey is about %s of a point "
       "higher, over and above where you started." % (F("i-ex"), F("i-ex")),
       "**The 95%% interval** for that effect runs from %s to %s. It does "
       "not include zero, the p-value is %s, and the star marks it."
       % (F("i-ex-lo"), F("i-ex-hi"), F("i-ex-p")),
       "**R-squared** is %s: the two terms together account for about "
       "two-thirds of the variation in affect at the second survey."
       % F("i-r2")),
    p("In one sentence: *in this team, people's affect moved toward the "
      "affect of those who confided in them.*"),
    note("The data are simulated, so the right answer is known: the "
         "contagion effect was built in at 0.5 and the carry-over at 0.4. "
         "The model recovered both."),

    h3("How that worked, and where this goes next"),
    p("Those two lines did a lot. They found each person's confiders, "
      "averaged the confiders' earlier affect, lined that up with each "
      "person's own earlier and later affect, and fitted a regression. The "
      "next pages take that apart:"),
    table(["Page", "What it covers"], [
        ["[A2 · Theory](#p-the-theory-and-the-hypothesis)",
         "What an influence claim says, and how to write the hypothesis"],
        ["[A3 · Data](#p-the-data-people-ties-and-waves)",
         "Building this dataset yourself: people, ties, and the object that holds them"],
        ["[A4 · Running the model](#p-running-the-influence-model)",
         "The formula, one term at a time"],
        ["[A5 · Interpreting](#p-interpreting-the-influence-model)",
         "Everything `summary()` printed, and how to judge the fit"],
        ["[A6 · Visualizing](#p-visualizing-the-influence-model)",
         "The network, the coefficients and the size of the effect, as pictures"],
        ["[A7 · Latent space](#p-the-latent-space-influence-model)",
         "Guarding against the rival explanation: people choosing similar confidants"],
        ["[A8 · Multilevel](#p-the-multilevel-influence-model)",
         "The same model across ten teams"],
    ]),
    p("If you only have a few minutes, read A3 and A4. You will be able to "
      "fit this model to your own data."),
)

# ---------------------------------------------------------------------------
# A2
# ---------------------------------------------------------------------------
page(
    GA, "A2 · Theory", "The theory and the hypothesis",
    '<p class="lede">What a claim of social influence says, and the five '
    "things a hypothesis has to pin down before a model can test it.</p>",

    h3("What influence means"),
    p("**Influence** is a claim about change in a person: *something about "
      "you changes because of the people you are connected to.* Affect "
      "contagion is one example. People who share their feelings with you "
      "pass some of those feelings on, so over time your mood drifts toward "
      "theirs."),
    p("Three ingredients are always present:"),
    ul("an **outcome** that can change: here, a person's affect;",
       "a **network** that carries the influence: here, who confides in whom;",
       "something that travels along it, the **content**: here, the "
       "confiders' own affect."),

    h3("Why it takes two surveys"),
    p("At a single point in time, people who confide in each other having "
      "similar moods tells you nothing about direction. Their moods may have "
      "converged, or they may have sought each other out *because* their "
      "moods were alike. With two surveys you can ask a sharper question: "
      "among people who started in the same mood, did those with "
      "happier confiders end up happier? That is why the model includes each "
      "person's own earlier affect."),

    h3("Writing the hypothesis"),
    p("A testable influence hypothesis names five things. Each one becomes "
      "a piece of the model formula on [page A4](#p-running-the-influence-model)."),
    table(["The hypothesis has to say", "In this example", "In the formula"], [
        ["What changes", "A person's affect at the second survey", "`affect ~`"],
        ["Compared with what", "Their own affect at the first survey", "`ar()`"],
        ["What they are exposed to", "Other people's affect at the first survey",
         "`exposure(affect, ...)`"],
        ["Through which relationship, in which direction",
         "From the people who confide in them", "`via = confides`"],
        ["How exposures combine", "The average across their confiders",
         '`normalize = "row"`'],
    ]),
    quote("**H1.** Controlling for a person's own affect at time 1, the "
          "higher the average time-1 affect of the people who confide in "
          "them, the higher that person's affect at time 2."),
    p("Written that way it can fail. If the exposure coefficient is zero or "
      "negative, H1 is not supported."),

    h3("The rival explanation"),
    p("The main threat to an influence claim is **selection**: people choose "
      "their ties. If people tend to confide in others who are like them in "
      "ways that also shape mood, then confiders' affect predicts your later "
      "affect without anything being passed along. Two pages deal with "
      "this: [A7](#p-the-latent-space-influence-model) adjusts the influence "
      "model for it, and the Selection sequence models the choosing "
      "directly."),

    rare("The equation",
         eq("<i>y</i><sub><i>i</i>,2</sub> = β<sub>0</sub> + "
            "γ <i>y</i><sub><i>i</i>,1</sub> + "
            "β ⋅ (1/<i>n<sub>i</sub></i>) "
            "∑<sub><i>j</i>→<i>i</i></sub> <i>y</i><sub><i>j</i>,1</sub> "
            "+ ε<sub><i>i</i></sub>",
            "γ is the carry-over, β is the contagion effect"),
         p("*y* is affect, the subscripts 1 and 2 are the two surveys, and "
           "the sum runs over the people *j* who confide in person *i*; "
           "*n* with the subscript *i* is how many of them there are. γ (gamma) is how much of a person's earlier affect "
           "carries over; β (beta) is the contagion effect H1 is about.")),
    rare("What this model does not establish",
         p("A positive β is what contagion predicts. It is also what you "
           "would see if something unmeasured raised the mood of a whole "
           "cluster of confidants between the two surveys, or if people "
           "chose confidants on a trait that drives later mood. The model "
           "rests on assumptions about those possibilities that no statistic "
           "can check. [Page A5](#p-interpreting-the-influence-model) shows "
           "where rnetis lists them.")),
)

# ---------------------------------------------------------------------------
# A3
# ---------------------------------------------------------------------------
page(
    GA, "A3 · Data", "The data: people, ties and waves",
    '<p class="lede">Two tables and one function. One table describes '
    "people, the other describes pairs of people, and `netis_data()` joins "
    "them into the object every model on these pages uses.</p>",

    p("The model on page A1 used `demo_affect`, which ships with rnetis. "
      "This page builds the same data from scratch, so you can see what "
      "shape your own has to be in. The same object is used for the "
      "Selection sequence too: **you build it once.**"),

    h3("1. People: the node table"),
    p("One row per person. Every column is a **characteristic of an "
      "individual**: something you could know about a person without knowing "
      "anyone else on the team."),
    block("i3-people"),
    table(["Column", "What it is", "Kind"], [
        ["`node`", "The person's ID", "label"],
        ["`affect`", "Positive affect at the first survey, roughly 1 to 5", "number"],
        ["`extraversion`", "A personality score, centred on zero", "number"],
        ["`tenure`", "Years on the team", "number"],
        ["`remote`", "Works remotely (1) or on site (0)", "yes/no"],
        ["`project`", "Which of three projects they are on", "category"],
    ]),

    h3("2. Pairs: the tie table"),
    p("One row per **ordered pair** of people. Every column is a "
      "**characteristic of a relationship**: it belongs to the pair, and "
      "would make no sense attached to one person alone."),
    block("i3-pairs"),
    table(["Column", "What it is", "Kind"], [
        ["`source`, `target`", "The two people. The tie runs *from* source *to* target", "labels"],
        ["`confides`", "Source confides in target", "yes/no"],
        ["`advice`", "Source asks target for advice", "yes/no"],
        ["`talk`", "Hours a week source spends talking with target", "count"],
        ["`same_project`", "The two are on the same project", "yes/no"],
        ["`affect_gap`", "How far apart their affect was at the first survey", "number"],
    ]),
    quote("**People versus pairs.** `remote` is about a person. "
          "`same_project` is about two people. The first goes in the node "
          "table, the second in the tie table. If a variable needs two "
          "names to state (*Ada confides in Ben*), it is a tie variable."),
    p("**Direction matters.** The row `p02 → p01` and the row `p01 → p02` "
      "are different rows, and one can be 1 while the other is 0. Here a "
      "tie from source to target means the source's feelings reach the "
      "target, which is the direction influence will flow."),

    h3("3. The second survey"),
    p("People were surveyed twice, so the node table needs each person's "
      "affect at both times, and the tie table needs the confiding ties at "
      "both times. Both tables are stacked with a `wave` column saying which "
      "survey a row is from."),
    block("i3-wave2"),
    p("Eighty node rows is forty people at two waves. 3,120 tie rows is "
      "every ordered pair (40 × 39) at two waves."),

    h3("4. One object: `netis_data()`"),
    p("`netis_data()` takes the two tables and checks that they fit "
      "together. The result is what you hand to a model."),
    block("i3-object"),
    p("Read the printout as a receipt: forty people, one team, two waves; "
      "which variables describe people and which describe ties; and which of "
      "them change between waves (`affect` and `confides`)."),
    p("And it is the dataset page A1 used:"),
    block("i3-identical"),

    rare("What the `tie_meta` lines are for",
         p("A tie table that lists *every* pair, with a 0 where there is no "
           "tie, looks the same to software as one where 0 means *not "
           "asked*. rnetis does not guess. `tie_meta` is where you say, for "
           "each tie variable:"),
         table(["Setting", "What it declares"], [
             ['`zero_meaning = "measured_zero"`',
              "A 0 is an observed non-tie, not missing data"],
             ['`presence_mode = "value_rule"`, `presence_rule = is_tie`',
              "A tie exists where the value is not 0 (`is_tie` is the one-line "
              "function defined just above)"],
             ["`weighted = TRUE`", "The values are amounts, not just yes/no"],
             ['`weight_semantics = "count"`', "The amounts are counts"],
         ]),
         p("If your tie table lists only the ties that exist (an *edge "
           "list*), you need less of this: see "
           "[importing and exporting](#p-importing-exporting-and-deriving).")),
    rare("Every function used on this page",
         table(["Function", "What it did here"], [
             ["`set.seed(5)`", "Fixes the random numbers, so you get exactly these data"],
             ['`sprintf("p%02d", 1:n)`', "Makes the IDs `p01` to `p40`"],
             ["`data.frame(...)`", "Builds a table, one argument per column"],
             ["`rnorm(n)`", "Random numbers from a bell curve; `sd =` sets the spread"],
             ["`runif(n, 0, 10)`", "Random numbers spread evenly between 0 and 10"],
             ["`rbinom(n, 1, 0.3)`", "Random 0/1 values that are 1 about 30% of the time"],
             ["`rpois(n, lambda)`", "Random counts averaging `lambda`"],
             ["`sample(x, n, replace = TRUE)`", "Draws `n` values from `x` at random"],
             ["`round(x, 2)`", "Rounds to two decimals"],
             ["`expand.grid(source = id, target = id)`", "Every combination of two IDs"],
             ["`match(x, id)`", "The row of `people` each ID is on"],
             ["`plogis(x)`", "Turns any number into a probability between 0 and 1"],
             ["`tapply(x, g, mean)`", "The mean of `x` within each group `g`"],
             ["`transform(d, col = new)`", "A copy of table `d` with one column replaced"],
             ["`rbind(a, b)`", "Stacks two tables"],
             ["`head(d)`", "The first six rows"],
             ["`netis_data(nodes, ties, wave_col, tie_meta)`",
              "Joins the node and tie tables into one checked object"],
             ["`netis_nodes()`, `netis_ties()`",
              "Give the node or tie table back out of the object"],
         ]),
         p("Only `netis_data()` is rnetis. The rest is ordinary R for "
           "making example data, and with real data you would read your "
           "tables from files instead.")),
    rare("How the second survey was simulated, and what is planted in it",
         p("Two processes are built into wave 2, so that the models have "
           "something real to find:"),
         ul("**Contagion.** `affect2` is 0.4 of a person's own earlier "
            "affect plus 0.5 of the average earlier affect of the people who "
            "confide in them (`confiders_affect`), plus noise. That 0.5 is "
            "the effect the Influence model estimates.",
            "**Selection.** `confides2` makes a tie likelier where one "
            "already existed, where the two share a project, and where "
            "their affect was close (`affect_gap` small). That is what the "
            "Selection sequence estimates."),
         p("`attr(demo_affect, \"truth\")` holds those numbers.")),
    rare("Using your own data instead",
         p("You need the same two tables: a node table with one row per "
           "person per wave, and a tie table with a `source` and a `target` "
           "column. The columns must be named `group`, `node`, `source` and "
           "`target`, or you tell `netis_data()` what they are called. If "
           "your network is already an `igraph` or `network` object, or an "
           "adjacency matrix, there are importers: see "
           "[importing and exporting](#p-importing-exporting-and-deriving).")),
)

# ---------------------------------------------------------------------------
# A4
# ---------------------------------------------------------------------------
page(
    GA, "A4 · Running the model", "Running the influence model",
    '<p class="lede">One function, one formula. This page runs it, then '
    "reads the formula a term at a time.</p>",

    block("i4-fit"),
    p("This is the model from page A1, now on the `team` object you built. "
      "The next page reads the output; this one is about what you typed."),

    h3("The formula as a whole"),
    codeonly('affect ~ ar() + exposure(affect, via = confides, normalize = "row")'),
    p("Read `~` as *is explained by*. The outcome is on the left and each "
      "term on the right, separated by `+`, adds one explanation:"),
    table(["Piece", "In words"], [
        ["`affect ~`", "A person's affect at the later survey is explained by..."],
        ["`ar()`", "...their own affect at the earlier survey..."],
        ['`exposure(affect, via = confides, normalize = "row")`',
         "...and the average earlier affect of the people who confide in them."],
    ]),

    h3("The outcome"),
    p("`affect` is a column of the node table. rnetis takes the outcome from "
      "the **later** wave and everything on the right from the **earlier** "
      "one, so you never write the wave numbers yourself."),

    h3("`ar()`: where the person started"),
    p("`ar()` adds the person's own earlier value of the outcome. (*AR* is "
      "short for autoregressive: the outcome regressed on its own past.) It "
      "is what turns the question from *who is happy* into *who became "
      "happier than their starting point predicts*. In the output it appears "
      "as `affect_lag1`."),

    h3("`exposure()`: what reached them through the network"),
    p("`exposure()` is the heart of an influence model. It builds one number "
      "per person: a summary of the people they are tied to. It has three "
      "parts."),
    table(["Argument", "Here", "What it means"], [
        ["first argument (the content)", "`affect`",
         "*What* travels: the other people's affect"],
        ["`via`", "`confides`", "*Which tie* carries it"],
        ["`normalize`", '`"row"`',
         "*How it combines*: `\"row\"` averages over a person's confiders"],
    ]),
    p("So for each person, rnetis finds everyone who confides in them, takes "
      "those people's affect at the earlier survey, and averages it. In the "
      "output that column is named `ex_affect_confides`."),

    h3("Timing: the lag"),
    p("By default both `ar()` and `exposure()` look **one wave back**. "
      "Affect at survey 2 is explained by affect and confiders at survey 1. "
      "The line `Waves: analyzed 1 -> 2` in the output confirms which pair "
      "of surveys was used. Cause before effect is built in."),

    h3("`data`"),
    p("`data = team` is the object from the last page. The formula can use "
      "any node variable as an outcome or a covariate, and any tie variable "
      "after `via =`."),

    h3("Adding covariates"),
    p("Other characteristics of the person go in as ordinary terms. Does the "
      "contagion effect survive adjusting for extraversion and remote work?"),
    block("i4-covariates"),
    p("It does. Neither covariate changes the picture, and the exposure "
      "coefficient stays close to where it was."),

    rare("Other settings, and what they change",
         h4("Average or total exposure"),
         p("Without `normalize = \"row\"`, exposure is the **sum** over "
           "confiders, not the average. That is a different hypothesis: "
           "every additional confider adds exposure, so people with many "
           "confiders are exposed to *more*, not just to a different "
           "average. In these data, where the average was what mattered, "
           "the sum finds nothing:"),
         block("i4-sum"),
         p("Choose by theory. *The mood around me* is an average; *how "
           "much emotional traffic reaches me* is a sum."),
         h4("Which way the tie points"),
         p("`exposure()` assumes influence runs from `source` to `target`. "
           "If your tie is recorded the other way round, say so with "
           "`direction`. Here that asks whether people take on the affect "
           "of those *they* confide in, which is not how these data were "
           "built:"),
         block("i4-direction"),
         h4("A different tie"),
         p("Any tie variable can carry exposure. `talk` is hours per week, "
           "so with `normalize = \"row\"` this is an average weighted by "
           "how much each pair talks:"),
         block("i4-other-tie"),
         h4("A different content"),
         p("The content does not have to be the outcome. "
           "`exposure(extraversion, via = confides, normalize = \"row\")` "
           "asks whether having extraverted confiders changes your affect.")),
    rare("Terms you can add to the formula",
         table(["Term", "What it adds"], [
             ["`ar()`", "The outcome at the earlier wave"],
             ["`exposure(content, via = tie)`", "A network exposure"],
             ["a node variable, such as `tenure`", "A covariate"],
             ["`x * z`", "An interaction (moderation)"],
             ["`latent(tie, d = 2)`", "A latent-space adjustment: page A7"],
             ["`(1 | group)`", "A random intercept across groups: page A8"],
         ]),
         p("The full list is on "
           "[formula terms at a glance](#p-formula-terms-at-a-glance).")),
    rare("What rnetis did behind the one line",
         ul("Worked out which two waves to use, and checked there was exactly "
            "one transition to analyze.",
            "Built the exposure column from the earlier wave's network and "
            "the earlier wave's affect.",
            "Lined up each person's later affect, earlier affect and "
            "exposure in one table.",
            "Saw that the formula has no group term and fitted it by "
            "ordinary least squares (`lm()`).",
            "Recorded what the result assumes, and anything it noticed "
            "about the data.")),
)

# ---------------------------------------------------------------------------
# A5
# ---------------------------------------------------------------------------
page(
    GA, "A5 · Interpreting", "Interpreting the influence model",
    '<p class="lede">`summary()` in four parts: what was fitted, the '
    "coefficients, how well the model fits, and what the result rests on.</p>",

    block("i5-summary"),

    h3("1. The header: what was fitted"),
    ul("**Formula** is what you typed.",
       "**Estimator** says how it was fitted. With one team and no group "
       "term, that is ordinary least squares, the same as `lm()`.",
       "**Waves** says which surveys were used: `1 -> 2`.",
       "Check these first. If the header is not the model you meant, nothing "
       "below it matters."),

    h3("2. The coefficients"),
    p("The table reads like the one from `lm()`, with the 95% interval added."),
    table(["Column", "What it tells you"], [
        ["`Estimate`", "The size of the effect, in units of the outcome"],
        ["`SE`", "The standard error: how uncertain the estimate is"],
        ["`95% interval`", "The range of values consistent with the data"],
        ["`df`", "Degrees of freedom the test uses: rows minus coefficients"],
        ["`t value`", "The estimate divided by its standard error"],
        ["`Pr(>|t|)`", "The p-value for *this coefficient is zero*"],
        ["stars", "`*` below .05, `**` below .01, `***` below .001"],
    ]),
    p("Row by row:"),
    ul("**`(Intercept)`** is the predicted later affect for someone whose "
       "earlier affect and exposure were both zero. Affect runs from about 1 "
       "to 5, so nobody is there, and the row is rarely of interest.",
       "**`affect_lag1`** = %s. Stability. Two people one point apart at "
       "the first survey are about %s apart at the second." % (F("i-ar"), F("i-ar")),
       "**`ex_affect_confides`** = %s. The contagion effect. Two people "
       "who started in the same mood, one of whom has confiders averaging a "
       "full point happier, end up about %s of a point apart."
       % (F("i-ex"), F("i-ex"))),
    p("The interval for the contagion effect is [%s, %s]. Report that, and "
      "not just the star: the data are consistent with a small effect and "
      "with a large one, but not with none." % (F("i-ex-lo"), F("i-ex-hi"))),

    h3("3. How well the model fits"),
    table(["Line", "What it tells you"], [
        ["Residual standard error",
         "The typical distance between a person's actual and predicted "
         "affect, in affect points, with its degrees of freedom (%s)" % F("i-df")],
        ["R-squared",
         "The share of the variation in later affect the model accounts for (%s)" % F("i-r2")],
        ["adjusted R-squared", "The same, penalized for the number of terms"],
        ["F-statistic", "A test of whether the terms together explain anything"],
        ["logLik, AIC, BIC",
         "For comparing models fitted to the same data: lower AIC or BIC is better"],
    ]),
    p("An R-squared of %s is high, and most of it is stability: people's "
      "mood at one survey predicts their mood at the next. To see what the "
      "exposure term adds, compare this model's fit with one that leaves it "
      "out." % F("i-r2")),

    h3("4. What the result rests on"),
    p("The last lines are short on purpose. They say that the fit rests on "
      "a set of assumptions and that nothing unusual was noticed while "
      "fitting, and they name the function that shows the detail."),
    rare("See the assumptions: `netis_assumptions(fit)`",
         p("Every regression assumes things. rnetis writes this model's "
           "assumptions down, and says which ones no statistic can test. "
           "The two that matter most for a contagion claim are **A4** and "
           "**B2**: nothing unmeasured is driving both who confides in whom "
           "and how mood changes."),
         block("i5-assumptions"),
         p("These are not warnings that something went wrong. They are the "
           "conditions under which the coefficient means what H1 says it "
           "means.")),

    h3("Is the effect big?"),
    p("A coefficient of %s is per one-point difference in confiders' "
      "average affect. Whether that is a lot depends on how much confiders' "
      "affect actually varies across the team." % F("i-ex")),
    rare("The effect in real units: `influence_effect(fit)`",
         block("i5-effect"),
         p("The *contrast* is the range of exposure actually seen in the "
           "team, from the 10th to the 90th percentile. Moving across that "
           "whole range is predicted to change a person's affect by the "
           "amount shown. That is the number to quote when someone asks "
           "whether the effect matters in practice.")),
    rare("Is anything left in the residuals? `netis_dependence(fit, team)`",
         p("A model can fit well and still leave a pattern behind. This "
           "asks whether the people the model over-predicts are confiders "
           "of other people it over-predicts."),
         block("i5-dependence"),
         p("A value near zero is what a model that has captured the "
           "network process should leave behind. rnetis reports the number "
           "without a p-value, because no agreed reference distribution "
           "exists for it, and repeats the two standing cautions about "
           "treating the network as given.")),
    rare("Getting the numbers out as a table",
         p("`netis_estimates(fit)` returns the coefficient table as a data "
           "frame, `confint(fit)` the intervals, and `netis_glance(fit)` a "
           "one-row summary of the fit. `coef()`, `AIC()` and `BIC()` work "
           "as they do for `lm()`.")),
)

# ---------------------------------------------------------------------------
# A6
# ---------------------------------------------------------------------------
page(
    GA, "A6 · Visualizing", "Visualizing the influence model",
    '<p class="lede">Two pictures worth making for every model: the '
    "network the influence travels through, and the coefficients.</p>",

    h3("The network"),
    p("`plot()` on the data object draws the network. Say which tie and "
      "which wave. `layout = \"mds\"` places people who are closely "
      "connected near each other:"),
    codeonly('plot(team, variable = "confides", wave = 1, layout = "mds")'),
    fig("i6-network", "Who confides in whom at the first survey. Each arrow "
        "runs from the person confiding to the person confided in."),
    p("Look for three things: whether anyone is cut off (a person nobody "
      "confides in has no exposure), whether the team splits into clusters, "
      "and whether there is a dense core with people at the edges. All "
      "three shape what an exposure term can estimate. Here there is a core "
      "and a periphery, and nobody is cut off."),

    h3("The coefficients"),
    p("`plot()` on the fitted model draws each estimate with its 95% "
      "interval:"),
    codeonly("plot(fit)"),
    fig("i6-coef", "Each point is a coefficient and each bar its 95% "
        "interval. The dashed line is zero."),
    p("This is the coefficient table as a picture. An interval that stays "
      "clear of the dashed line is one whose p-value is below .05. The "
      "picture adds what the table hides: how wide each interval is "
      "compared with the others."),

    p("Later pages add one plot each: the latent space on "
      "[page A7](#p-the-latent-space-influence-model), and the differences "
      "between teams on [page A8](#p-the-multilevel-influence-model)."),

    rare("Other plots, and options",
         table(["Call", "What it draws"], [
             ['`plot(team, variable = "advice", wave = 1)`', "A different tie"],
             ['`plot(team, variable = "confides", wave = 2)`', "The network at the second survey"],
             ['`plot(team, variable = "confides", wave = 1)`',
              "The default layout: everyone on a circle"],
             ['`plot(fit, what = "effects")`',
              "The predicted change in affect across the range of exposure "
              "the team actually saw: one point per exposure term"],
             ["`plot(fit2)`", "The coefficients of the model with covariates"],
             ['`plot(fit_latent, what = "latent")`', "The latent space: page A7"],
             ['`plot(fit_teams, what = "heterogeneity")`', "Differences between teams: page A8"],
         ]),
         p("Every plot is a `ggplot` object, so you can add a title or a "
           "theme with `+`. `plot_data()` with the same arguments returns "
           "the table behind a plot.")),
)

# ---------------------------------------------------------------------------
# A7
# ---------------------------------------------------------------------------
page(
    GA, "A7 · Latent space", "The latent-space influence model",
    '<p class="lede">The same model, with one added term that guards '
    "against people having chosen confidants who were already like them.</p>",

    h3("Where we are"),
    p("So far: `affect ~ ar() + exposure(...)` estimated a contagion effect "
      "of %s. The open question from [page A2](#p-the-theory-and-the-hypothesis) "
      "is whether that is contagion or **selection**: people confiding in "
      "others who resemble them on something unmeasured that also shapes "
      "mood." % F("i-ex")),

    h3("The idea"),
    p("If people choose confidants by some hidden similarity, that "
      "similarity leaves a trace in the network itself: similar people end "
      "up close together in the web of ties. A **latent-space model** reads "
      "that trace. It places every person at a position in an imagined "
      "space, so that people who are near each other are likely to be tied. "
      "A person's position then stands in for the hidden traits they were "
      "chosen on, and adding the positions to the influence model adjusts "
      "for them."),

    h3("Run it"),
    p("One more term, `latent()`. It names the tie to read positions from "
      "and how many dimensions the space has. This needs the `latentnet` "
      "package and takes a minute or two."),
    block("i7-fit"),

    h3("Reading it"),
    p("Two things are new in the output."),
    ul("**The header** says a latent-space adjustment was made, on which "
       "network, and that the positions are *estimated*.",
       "**Two new rows**, `lat_confides_1_d1` and `lat_confides_1_d2`, "
       "are the two dimensions of each person's position. **Do not "
       "interpret these coefficients.** The space has no fixed orientation, "
       "so the numbers would change if it were rotated. They are there to "
       "adjust the row above them."),
    p("What to read is how the contagion effect changed:"),
    block("i7-compare"),
    p("The estimate fell from %s to %s, and its interval now includes zero. "
      "Part of what the basic model called contagion is accounted for by "
      "where people sit in the network." % (F("i-ex"), F("i7-ex"))),
    quote("**How to report this.** Both numbers. The basic model is the "
          "estimate under the assumption that confidants were not chosen on "
          "anything that drives mood. The adjusted model relaxes that "
          "assumption and costs precision. In these simulated data the "
          "truth is 0.5 and selection on affect was also built in, so both "
          "models are behaving as they should: with forty people, the "
          "adjustment cannot tell the two processes fully apart."),

    h3("See the space"),
    codeonly('plot(fit_latent, what = "latent")'),
    fig("i7-latent", "Each point is a person at their estimated position. "
        "People close together are those the model found likely to confide "
        "in one another. Colour is affect at the second survey."),
    p("Read **distances**, not axes. Whether a point is on the left or the "
      "right means nothing; who it is near does. If the colours cluster, "
      "with similar moods sitting together, position and mood are related, "
      "which is exactly the situation the adjustment is for."),

    rare("How many dimensions?",
         p("`d` is the number of dimensions. More dimensions can represent "
           "a more complicated pattern of who-is-near-whom, and each one "
           "costs a coefficient in a model with only forty rows. One or two "
           "is usual. With `d = 1`:"),
         block("i7-d1"),
         p("Here one dimension barely moves the contagion estimate, while "
           "two moves it a good deal. When the answer depends on `d`, say "
           "so and show both. There is no rule that picks the right one.")),
    rare("Other settings",
         table(["Argument", "What it does"], [
             ["`d`", "Number of latent dimensions"],
             ['`homophily = c("tenure")`',
              "Also condition the positions on observed similarity in these variables"],
             ["`seed`", "Makes the positions reproducible: they come from a random sampler"],
             ["`lag`", "Which wave's network to read positions from (default: the earlier one)"],
         ])),
    rare("What this adjustment is, and is not",
         p("The method is Xu's latent-space adjusted approach (2018, 2021). "
           "It is a two-step procedure: positions are estimated first, then "
           "used as if they were known. The uncertainty in the positions "
           "does not reach the standard errors of the influence model, and "
           "the output says so. It reduces bias from selection on hidden "
           "traits under its assumptions; it does not prove the remaining "
           "effect is causal.")),
)

# ---------------------------------------------------------------------------
# A8
# ---------------------------------------------------------------------------
page(
    GA, "A8 · Multilevel", "The multilevel influence model",
    '<p class="lede">Ten teams, not one. The formula gains one term, and '
    "the summary gains a section.</p>",

    h3("Where we are"),
    p("Everything so far used one team of forty. Most studies have several "
      "teams, classrooms or sites. People in the same team share a manager, "
      "a workload and a mood, so their outcomes are not independent, and "
      "ties only exist within a team."),

    h3("The data"),
    p("`demo_affect_teams` ships with rnetis: ten teams of twenty people, "
      "the same variables as before, built the same way. The only "
      "difference in the tables is that the `group` column now has ten "
      "values."),
    block("i8-data"),

    h3("What happens if you ignore the teams"),
    p("rnetis will not fit the single-team model to ten teams:"),
    refusal("i8-refuse"),
    p("The message says what to do. A model with no team term would treat "
      "two hundred people as one team with one average mood."),

    h3("Run it"),
    p("Add `(1 | group)`: *each team gets its own baseline.* This is a "
      "random intercept, the same notation `lmer()` uses."),
    block("i8-fit"),

    h3("Reading it"),
    p("The coefficients read exactly as before. The contagion effect is "
      "%s, and with two hundred people its interval is much narrower than "
      "it was with forty. Three things are new:" % F("i8-ex")),
    ul("**`df` now has decimals.** In a multilevel model the degrees of "
       "freedom are estimated (Satterthwaite's method, the one `lmerTest` "
       "uses), and they differ by coefficient. The note under the table "
       "says so.",
       "**Random effects.** `group (Intercept)` is how much teams differ "
       "in their baseline affect, as a variance and a standard deviation. "
       "`Residual` is what is left within teams. Compare the two standard "
       "deviations: teams differ by about %s of a point, people within a "
       "team by about %s." % (F("i8-sd-team"), F("i8-sd-res")),
       "**Two R-squared values.** *Marginal* is the share explained by the "
       "terms in the formula alone. *Conditional* adds the team baselines. "
       "The gap between them is how much knowing someone's team helps."),

    h3("See the difference between teams"),
    codeonly('plot(fit_teams, what = "heterogeneity")'),
    fig("i8-heterogeneity", "The standard deviations from the random-effects "
        "table: how much teams differ, beside how much people differ within "
        "a team."),

    rare("Can the contagion effect differ between teams?",
         p("Yes. A random slope lets each team have its own exposure "
           "effect. Name the exposure column inside the brackets:"),
         block("i8-slope"),
         p("With ten teams there is little information about how much a "
           "slope varies, so treat this as exploratory. A random slope "
           "also changes the degrees of freedom for that coefficient, as "
           "the `df` column shows.")),
    rare("Other settings",
         table(["Formula", "What it fits"], [
             ["`+ (1 | group)`", "Each team has its own baseline"],
             ["`+ (1 + ex_affect_confides | group)`",
              "Each team has its own baseline and its own contagion effect"],
             ["`+ tenure + (1 | group)`", "A covariate, as before"],
             ["`+ latent(confides, d = 1) + (1 | group)`",
              "The latent-space adjustment, with one space per team"],
         ]),
         p("The plots and the functions from pages A5 and A6 work on a "
           "multilevel fit unchanged. The latent-space plot needs a team: "
           "`plot(fit, what = \"latent\", group = \"team01\")`.")),
    rare("How many teams is enough?",
         p("The team-level variance is estimated from the number of teams, "
           "not the number of people. Ten is few. The fixed effects (the "
           "coefficient table) are well estimated here; the random-effects "
           "numbers are rough. If the summary says the fit is *singular*, "
           "the model could not tell the team variance from zero.")),

    h3("Where to go from here"),
    p("That is the Influence sequence: a model you can fit, read, plot, "
      "adjust for selection, and extend to several groups. The Selection "
      "sequence asks the other question of the same data: who comes to "
      "confide in whom. For anything not covered here, start with "
      "[formula terms at a glance](#p-formula-terms-at-a-glance) and "
      "[the rest of the package](#p-the-rest-of-the-package)."),
)
