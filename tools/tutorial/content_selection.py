"""The Selection sequence, pages B1 to B8.

THE SAME PLAN AS THE INFLUENCE SEQUENCE, page for page: run a model, the
theory, the data, the formula, reading the result, plotting it, latent
factors, several teams. Code first and its real output; then what was run, a
piece at a time; then expandable boxes for everything a reader might go
looking for.

THE SAME DATA. `demo_affect` and `demo_affect_teams`, the objects the Influence
pages use. Page A3 builds the first by hand, so the data page here reads the
object and does not rebuild it.

NUMBERS IN THE PROSE COME FROM THE CAPTURE, through `F()`, as on the Influence
pages.
"""

import build
from build import (block, codeonly, eq, fig, h3, h4, note, p, page, quote,
                   rare, refusal, table, ul)

GB = "Selection"


def F(fid):
    """A value recorded by the capture script, for the prose to quote."""
    build.USED.append("fact-%s" % fid)
    v = build.read("fact-%s.txt" % fid)
    return v.strip() if v is not None else "[missing fact: %s]" % fid


FORMULA = ('confides ~ dyad(affect_gap, lag = 1) + dyad(same_project, lag = 1) +\n'
           '  prior_tie(lag = 1)')

# ---------------------------------------------------------------------------
# B1
# ---------------------------------------------------------------------------
page(
    GB, "B1 · Start here", "Selection: run a model first",
    '<p class="lede">'
    "This page fits a selection model with one call and reads the result. "
    "The pages after it go back over how each part worked.</p>",

    h3("The model"),
    p("This is the team from the Influence pages: forty people, surveyed "
      "twice, who each reported their **affect** and who they **confide** "
      "in. The Influence pages asked whether affect spreads along confiding "
      "ties. The question here runs the other way: *who comes to confide in "
      "whom?* In particular, do people come to confide in others whose "
      "affect is close to their own? This call fits a model of it, and "
      "takes about two minutes because it runs a Markov chain:"),
    block("s1-run"),

    h3("What it says"),
    p("Look at the **Coefficients** table. Each row is one thing that could "
      "make a confiding tie more or less likely at the second survey:"),
    ul("**`dyd_affect_gap_lag1`** (%s). **This is the selection effect.** "
       "`affect_gap` is how far apart two people's affect was at the first "
       "survey. The coefficient is negative, so the further apart two people "
       "were, the less likely one came to confide in the other. People "
       "chose confidants whose mood was like theirs." % F("s-gap"),
       "**`dyd_same_project_lag1`** (%s). Two people on the same project "
       "were more likely to confide in each other." % F("s-project"),
       "**`prior_tie`** (%s). A tie that existed at the first survey was "
       "much more likely to exist at the second. Confiding relationships "
       "persist." % F("s-prior"),
       "**The 95%% interval** for the selection effect runs from %s to %s "
       "and does not include zero." % (F("s-gap-lo"), F("s-gap-hi"))),
    p("Put in one sentence, the result is that *people in this team came to "
      "confide in others whose affect was close to their own, over and "
      "above sharing a project and already confiding in them.*"),
    note("The data are simulated, so the right answer is known. All three "
         "effects were built in, with these signs. "
         "[Page B5](#p-interpreting-the-selection-model) compares the sizes."),
    p("Two things in this output differ from an Influence model, and later "
      "pages explain both. The coefficients are on a **probit scale**, so "
      "they are not changes in probability. And the model is **Bayesian**, "
      "so the table reports posterior summaries where a regression would "
      "report tests."),

    h3("How that worked, and where this goes next"),
    p("That one call did a lot. It took the confiding ties at the second "
      "survey as the outcome, one row for every ordered pair of people. It "
      "lined each pair up with its affect gap, its shared project and "
      "whether the tie already existed at the first survey. Then it fitted "
      "a model that also gives every person their own tendency to confide "
      "and to be confided in. The next pages take that apart:"),
    table(["Page", "What it covers"], [
        ["[B2 · Theory](#p-selection-the-theory-and-the-hypothesis)",
         "What a selection claim says, and how to write the hypothesis"],
        ["[B3 · Data](#p-the-data-read-for-selection)",
         "The same dataset, read with the tie as the outcome"],
        ["[B4 · Running the model](#p-running-the-selection-model)",
         "The formula, one term at a time, and the chain settings"],
        ["[B5 · Interpreting](#p-interpreting-the-selection-model)",
         "Everything `summary()` printed, and how to judge the fit"],
        ["[B6 · Visualizing](#p-visualizing-the-selection-model)",
         "The network, the coefficients and the fit check, as pictures"],
        ["[B7 · Latent factors](#p-the-latent-factor-selection-model)",
         "Letting the model find structure the covariates do not explain"],
        ["[B8 · Multilevel](#p-the-multilevel-selection-model)",
         "The same model across ten teams"],
    ]),
    p("If you only have a few minutes, read B4 and B5. You will be able to "
      "fit this model to your own data and report it."),
)

# ---------------------------------------------------------------------------
# B2
# ---------------------------------------------------------------------------
page(
    GB, "B2 · Theory", "Selection: the theory and the hypothesis",
    '<p class="lede">'
    "This page explains what a claim of selection says, and the five things "
    "a hypothesis has to pin down before a model can test it.</p>",

    h3("What selection means"),
    p("**Selection** is a claim about change in a relationship: *a tie "
      "forms, or lasts, because of who the two people are.* Choosing "
      "confidants whose mood matches yours is one example. The outcome is "
      "the tie itself, so the thing being explained belongs to a pair of "
      "people and not to one person."),
    p("When the claim is that people choose others who are *like* them, it "
      "is called **homophily**. On the Influence pages homophily was the "
      "rival explanation that had to be guarded against. Here it is the "
      "thing being measured."),

    h3("Three kinds of explanation"),
    p("A tie from one person to another can be explained by something about "
      "either person or something about the pair. A selection model keeps "
      "the three apart."),
    table(["Explanation", "Example", "In the formula"], [
        ["Something about the **sender**",
         "Extraverted people confide in more people", "`sender(extraversion)`"],
        ["Something about the **receiver**",
         "Extraverted people are confided in more", "`receiver(extraversion)`"],
        ["Something about the **pair**",
         "People with similar affect confide in each other", "`dyad(affect_gap)`"],
    ]),
    p("Keeping them apart matters. If a few people confide in everyone, "
      "pairs that include them have many ties whatever the pair has in "
      "common. The model therefore gives every person their own tendency "
      "to send ties and their own tendency to receive them, and a pair "
      "characteristic has to explain what is left."),

    h3("Why it takes two surveys"),
    p("At a single point in time, two people who confide in each other and "
      "have similar moods could have chosen each other for that reason, or "
      "could have grown alike afterwards. With two surveys you can ask "
      "about the order: *were they alike at the first survey, before the "
      "tie at the second?* The model also includes whether the tie already "
      "existed at the first survey, so it explains the ties that formed or "
      "ended and not the ones that were always there."),

    h3("Writing the hypothesis"),
    p("A testable selection hypothesis names five things. Each one becomes "
      "a piece of the model formula on "
      "[page B4](#p-running-the-selection-model)."),
    table(["The hypothesis has to say", "In this example", "In the formula"], [
        ["Which tie is explained", "Who confides in whom at the second survey",
         "`confides ~`"],
        ["Compared with what", "Whether that tie existed at the first survey",
         "`prior_tie(lag = 1)`"],
        ["What about the pair matters", "How far apart their affect was",
         "`dyad(affect_gap, ...)`"],
        ["Measured when", "At the first survey, before the tie being explained",
         "`lag = 1`"],
        ["Net of what else", "Sharing a project",
         "`dyad(same_project, lag = 1)`"],
    ]),
    quote("**H1.** The further apart two people's affect at time 1, the "
          "less likely one confides in the other at time 2, controlling "
          "for whether that tie existed at time 1 and whether the two "
          "share a project."),
    p("Written that way it can fail. If the coefficient on the affect gap "
      "is zero or positive, H1 is not supported."),

    rare("The equation",
         eq("Pr(<i>y</i><sub><i>ij</i>,2</sub> = 1) = "
            "Φ( μ + <i>a<sub>i</sub></i> + <i>b<sub>j</sub></i> + "
            "β <i>x<sub>ij</sub></i> + γ <i>y</i><sub><i>ij</i>,1</sub> )",
            "β is the selection effect, γ is the persistence of a tie"),
         p("*y* is whether *i* confides in *j*, and the subscripts 1 and 2 "
           "are the two surveys. *x* is a characteristic of the pair, such "
           "as their affect gap. *a* is person *i*'s own tendency to "
           "confide in others and *b* is person *j*'s tendency to be "
           "confided in. Φ (phi) is the normal curve's cumulative "
           "probability, which turns the sum into a probability between 0 "
           "and 1. That is what makes this a *probit* model.")),
    rare("What this model does not establish",
         p("A negative β is what choosing similar confidants predicts. It "
           "is also what you would see if something unmeasured made two "
           "people both similar in mood and likely to confide, such as "
           "sitting in the same office. The model compares pairs; it "
           "cannot rule out every reason two people might be alike.")),
    rare("Why the model uses one pair of surveys",
         p("rnetis fits one transition: an earlier survey and a later one. "
           "With three or more surveys it asks you to name which pair to "
           "use. Pooling several transitions with a term for the earlier "
           "tie has known statistical problems that this package does not "
           "correct for, so it does not do it silently.")),
)

# ---------------------------------------------------------------------------
# B3
# ---------------------------------------------------------------------------
page(
    GB, "B3 · Data", "The data, read for Selection",
    '<p class="lede">'
    "Selection uses the same data object as Influence. This page reads it "
    "again with the tie as the outcome, and does not build anything new.</p>",

    h3("The same object"),
    p("`demo_affect` ships with rnetis, and "
      "[page A3](#p-the-data-people-ties-and-waves) builds it from two "
      "ordinary tables. If you have not read that page, it is the place to "
      "see what shape your own data has to be in. Nothing about the object "
      "changes for a selection model."),
    block("s3-object"),

    h3("The outcome is the tie table"),
    p("In an Influence model the outcome was a column of the node table. "
      "Here it is a column of the **tie table**, which has one row for "
      "every ordered pair of people at each survey:"),
    block("s3-ties"),
    p("Forty people make 40 × 39 = 1,560 ordered pairs. *Ordered* means "
      "that `p02 → p01` and `p01 → p02` are two rows: one person can "
      "confide in another without it being returned. The model is fitted to "
      "all 1,560 rows of the second survey, the ties and the non-ties "
      "alike, because it explains which of the possible ties exist."),

    h3("How much the network changed"),
    p("A selection model learns from ties that formed and ties that ended, "
      "so it is worth counting them before fitting:"),
    block("s3-density"),
    p("There were %s confiding ties at the first survey and %s at the "
      "second. The table shows what became of each pair: some ties lasted, "
      "some ended, and some new ones formed. If almost nothing had changed, "
      "there would be little for the model to explain."
      % (F("s-ties-1"), F("s-ties-2"))),

    h3("Three kinds of predictor"),
    p("Every predictor in a selection model has a role, and the role "
      "decides which table it comes from."),
    table(["Role", "Comes from", "In these data"], [
        ["Sender: the person the tie comes from", "the node table",
         "`extraversion`, `tenure`, `remote`"],
        ["Receiver: the person the tie goes to", "the node table",
         "`extraversion`, `tenure`, `remote`"],
        ["Pair: the two people together", "the tie table",
         "`affect_gap`, `same_project`, `advice`, `talk`"],
    ]),
    p("One node variable can be used in both roles at once: whether "
      "extraverted people confide in more people, and whether they are "
      "confided in more, are separate questions with separate coefficients."),

    rare("Every function used on this page",
         table(["Function", "What it did here"], [
             ["`netis_ties(team)`", "Gives the tie table back out of the object"],
             ["`head(d)`", "The first six rows"],
             ["`tapply(x, g, sum)`", "The sum of `x` within each group `g`"],
             ["`table(a, b)`", "Counts every combination of `a` and `b`"],
         ])),
    rare("Making a pair variable from a node variable",
         p("`affect_gap` is the absolute difference between two people's "
           "affect. It was computed when the tie table was built on page "
           "A3, with one line: `abs(people$affect[s] - people$affect[t])`. "
           "Any similarity or difference between two people is made the "
           "same way and stored as a column of the tie table. A selection "
           "model needs it there because the model works on pairs.")),
    rare("Using your own data instead",
         p("You need what an Influence model needs: a node table and a tie "
           "table, joined by `netis_data()`. For selection the tie table "
           "should list **every** pair, with a 0 where there is no tie, and "
           "you should declare that a 0 is an observed non-tie. Page A3's "
           "box on `tie_meta` shows how. A pair nobody was asked about is "
           "not a 0, and treating it as one tells the model that someone "
           "was asked and said no.")),
)

# ---------------------------------------------------------------------------
# B4
# ---------------------------------------------------------------------------
page(
    GB, "B4 · Running the model", "Running the selection model",
    '<p class="lede">'
    "<code>fit_selection()</code> takes a formula, a data object and the "
    "settings of a Markov chain. This page runs it and then reads the call "
    "one piece at a time.</p>",

    h3("The model"),
    block("s4-fit"),
    p("This is the model from page B1 on the `team` object, with the chain "
      "settings written out. The next page reads the output; this one is "
      "about what you typed."),

    h3("The formula as a whole"),
    codeonly(FORMULA),
    p("Read `~` as *is explained by*. The outcome is on the left, and each "
      "term on the right adds one explanation:"),
    table(["Piece", "In words"], [
        ["`confides ~`",
         "Whether one person confides in another at the later survey is explained by..."],
        ["`dyad(affect_gap, lag = 1)`", "...how far apart their affect was earlier..."],
        ["`dyad(same_project, lag = 1)`", "...whether they share a project..."],
        ["`prior_tie(lag = 1)`", "...and whether that tie already existed earlier."],
    ]),

    h3("The outcome"),
    p("`confides` is a column of the tie table. rnetis takes the outcome "
      "from the **later** wave. Because `confides` is 0 or 1, the model is "
      "a probit model of whether the tie exists, which is the default "
      "(`family = \"bin\"`)."),

    h3("`dyad()`: something about the pair"),
    p("`dyad()` names a column of the tie table: a characteristic of the "
      "two people together. `dyad(affect_gap, lag = 1)` is the term H1 is "
      "about. In the output it appears as `dyd_affect_gap_lag1`."),

    h3("`sender()` and `receiver()`: something about one person"),
    p("These name a column of the node table and say which end of the tie "
      "it describes. Do extraverted people confide in more people, and are "
      "they confided in more? Adding both asks both questions:"),
    block("s4-covariates"),
    p("Neither is clearly different from zero here, and the selection "
      "effect is unchanged. Every predictor needs one of the three roles. "
      "A bare variable name is refused, because rnetis cannot tell which "
      "end of the tie it describes:"),
    refusal("s4-refuse-plain"),

    h3("`prior_tie()`: where the pair started"),
    p("`prior_tie()` adds whether the same tie existed at the earlier "
      "survey. It does for a selection model what `ar()` does for an "
      "influence model: it turns the question from *who confides in whom* "
      "into *which ties formed or ended*. It is also what tells rnetis "
      "that the earlier survey is the starting point and the later one is "
      "the outcome."),

    h3("Timing: `lag`"),
    p("`lag = 1` says that a predictor is taken from one wave before the "
      "outcome. Ties at survey 2 are therefore explained by the affect "
      "gap, the projects and the ties of survey 1, so each cause is "
      "measured before the effect. The line `Waves: analyzed 1 -> 2` in "
      "the output confirms which pair of surveys was used."),
    p("`prior_tie()` uses `lag = 1` by default. `dyad()`, `sender()` and "
      "`receiver()` use `lag = 0` by default, which takes the value "
      "measured alongside the outcome. That is rarely what a hypothesis "
      "about selection means, so these pages write `lag = 1` on every "
      "term. In these data the covariates were recorded once and do not "
      "change between surveys, so the lag selects the same values either "
      "way, and rnetis says so in a note."),

    h3("The chain settings"),
    p("A selection model is fitted by a Markov chain: the computer draws "
      "thousands of plausible sets of coefficients, and the results are "
      "summaries of those draws. Four settings control it."),
    table(["Setting", "Here", "What it does"], [
        ["`nscan`", "`10000`", "How many draws the chain makes after warming up"],
        ["`burn`", "`500`", "How many early draws are thrown away as warm-up"],
        ["`odens`", "`25`", "Keep every 25th draw, which leaves 400"],
        ["`seed`", "`1`", "Fixes the random numbers, so the fit can be repeated exactly"],
    ]),
    p("These are the defaults, written out so they can be seen. A longer "
      "chain gives steadier numbers and takes proportionally longer. Page "
      "B5 shows how to check whether a chain was long enough."),

    rare("Other settings, and what they change",
         h4("Reciprocity"),
         p("`prior_reciprocity()` asks whether a tie is likelier where the "
           "other person confided first: whether *j* confiding in *i* at "
           "the first survey makes *i* confide in *j* at the second."),
         block("s4-reciprocity"),
         p("No reciprocity was built into these data, and the estimate is "
           "small beside the persistence of a tie. Look at its interval "
           "with `summary(fit_recip)` before reading anything into it."),
         h4("A valued tie"),
         p("If the outcome is an amount, such as hours spent talking, "
           "`family = \"nrm\"` models the amount itself. The coefficients "
           "are then on the outcome's own scale, with no probit."),
         h4("An undirected tie"),
         p("If a tie has no direction, such as working on the same task, "
           "declare it undirected in `netis_data()` and fit with "
           "`symmetric = TRUE`. Each person then has one tendency to have "
           "ties, with no separate sending and receiving.")),
    rare("Terms you can add to the formula",
         table(["Term", "What it adds"], [
             ["`sender(x, lag = 1)`", "A characteristic of the person the tie comes from"],
             ["`receiver(x, lag = 1)`", "A characteristic of the person the tie goes to"],
             ["`dyad(z, lag = 1)`", "A characteristic of the pair, from the tie table"],
             ["`prior_tie(lag = 1)`", "Whether the tie existed at the earlier wave"],
             ["`prior_reciprocity(lag = 1)`",
              "Whether the reverse tie existed at the earlier wave"],
         ]),
         p("Another tie can be a predictor: `dyad(advice, lag = 1)` asks "
           "whether asking someone for advice goes with coming to confide "
           "in them. Interactions between terms are not available in a "
           "selection model. The full list is on "
           "[formula terms at a glance](#p-formula-terms-at-a-glance).")),
    rare("What rnetis did behind the one call",
         ul("Worked out which two waves to use, and checked there was "
            "exactly one transition to analyze.",
            "Built a 40 × 40 table of the outcome and one of each "
            "predictor, with every pair in the same place in each.",
            "Gave every person a sender effect and a receiver effect, so "
            "that people who confide widely or are confided in widely do "
            "not distort the pair terms.",
            "Ran the Markov chain through the `amen` package and kept "
            "every 25th draw.",
            "Simulated a network from the model at each kept draw, for "
            "the fit check on the next page.",
            "Recorded anything it noticed about the specification.")),
)

# ---------------------------------------------------------------------------
# B5
# ---------------------------------------------------------------------------
page(
    GB, "B5 · Interpreting", "Interpreting the selection model",
    '<p class="lede">'
    "The output of <code>summary()</code> has four parts: what was fitted, "
    "the coefficients, how well the model fits, and what was noticed while "
    "fitting. This page goes through them in order.</p>",

    h3("The output"),
    block("s5-summary"),

    h3("1. The header: what was fitted"),
    ul("**Formula** is what you typed.",
       "**Family** says the outcome is a yes/no tie, modelled with a "
       "probit.",
       "**Backend** names the software that ran the chain, `amen`.",
       "**Waves** says which surveys were used: `1 -> 2`.",
       "**The paragraph about scale** is a warning worth reading once. The "
       "next section explains it."),

    h3("2. The coefficients"),
    p("The table has the shape of a regression table, but the columns are "
      "summaries of the chain's draws and not tests."),
    table(["Column", "What it tells you"], [
        ["`Estimate`", "The average of the draws (the posterior mean)"],
        ["`Post.SD`", "How much the draws vary: the counterpart of a standard error"],
        ["`95% interval`", "The range that holds the middle 95% of the draws"],
        ["`Est/SD`", "The estimate divided by `Post.SD`: the counterpart of a t value"],
        ["`Tail prob.`",
         "Twice the share of draws on the other side of zero: the counterpart of a p-value"],
        ["stars", "Marks on `Tail prob.`: `*` below .05, `**` below .01, `***` below .001"],
    ]),
    p("`Tail prob.` is counted from the 400 kept draws, so it cannot be "
      "smaller than 2/400. That is why a clear effect shows `<0.005` and "
      "two stars, and why three stars need a longer chain."),
    p("Row by row:"),
    ul("**`intercept`** sets how common ties are overall. It is negative "
       "because most pairs do not have a tie.",
       "**`dyd_affect_gap_lag1`** = %s. This is the selection effect: "
       "pairs whose affect was further apart were less likely to have a "
       "tie. Its interval is [%s, %s]."
       % (F("s-gap"), F("s-gap-lo"), F("s-gap-hi")),
       "**`dyd_same_project_lag1`** = %s. Pairs on the same project were "
       "more likely to have a tie." % F("s-project"),
       "**`prior_tie`** = %s. This is persistence, and it is the largest "
       "effect in the table." % F("s-prior")),

    h3("How big is the effect?"),
    p("The coefficients are on the **probit scale**. A coefficient is a "
      "shift along the curve that turns the model's sum into a "
      "probability, so the same coefficient changes the probability a lot "
      "in the middle of the curve and very little near 0 or 1. Do not read "
      "a coefficient as a change in probability, and do not exponentiate "
      "it into an odds ratio. `selection_effect()` does the conversion:"),
    block("s5-effect"),
    p("Sharing a project raises the probability of a confiding tie by "
      "about %s percentage points, averaged over the pairs in this team. "
      "For the selection effect, ask what one more point of affect gap "
      "does:" % F("s-eff-project")),
    block("s5-effect-gap"),
    p("A pair one point further apart in affect is about %s percentage "
      "points less likely to have a tie. That is the number to quote when "
      "someone asks how much similarity matters." % F("s-eff-gap")),

    h3("3. How well the model fits"),
    p("A selection model has no R-squared, AIC or BIC. `summary()` reports "
      "two other things."),
    ul("**Pseudo R-squared.** Two measures of how much the predictors "
       "account for. *Tjur's D* is the easier one to read: the average "
       "predicted probability for pairs that have a tie, minus the average "
       "for pairs that do not.",
       "**The posterior predictive check.** The fitted model can generate "
       "networks. rnetis generated one at each kept draw, measured five "
       "features of each, and compared them with the same five features of "
       "the network you observed. The line says on how many of the five "
       "the observed network falls inside the range the model produces."),
    p("The small table under that line lists the five features. Out-degree "
      "and in-degree spread are about how unequally people send and "
      "receive ties. Dyadic dependence is about ties being returned. The "
      "two triad features are about ties clustering in threes. Here the "
      "observed network is inside the range on all five, which means the "
      "model can produce networks that look like this one. It does not "
      "mean the model is correct."),

    h3("4. What was noticed while fitting"),
    p("The last line counts **findings**: things rnetis noticed about the "
      "specification. A NOTE is for information and a WARNING deserves a "
      "look. `netis_diagnostics(fit)` lists them:"),
    block("s5-diagnostics"),
    p("These two notes are about the lags. The covariates do not change "
      "between surveys in these data, so `lag = 1` selects the same values "
      "as leaving it out. The notes are expected here."),

    rare("The fit check in full: `netis_gof(fit)`",
         block("s5-gof"),
         p("`observed` is the value in your network. `predicted_lower` and "
           "`predicted_upper` hold the middle 95% of the simulated "
           "networks. `ppp` is the share of simulated networks with a "
           "value at least as large as the observed one. It is one-sided, "
           "so values near 0 and near 1 both mean the model misses that "
           "feature, and values near 0.5 mean it reproduces it."),
         p("A feature outside its range tells you *what* the model cannot "
           "reproduce. The two triad features are the ones a model with "
           "only sender, receiver and pair terms has no way to produce, "
           "and [page B7](#p-the-latent-factor-selection-model) is about "
           "what to add when they fall outside.")),
    rare("The pseudo R-squared in full: `pseudo_r2(fit)`",
         block("s5-pseudo"),
         p("The function prints what each measure is and what it is not. "
           "The latent-scale variance share is this package's own "
           "construction and is labelled as such. None of the three is "
           "*the* R-squared.")),
    rare("Was the chain long enough?",
         p("Before trusting any number from a Markov chain, check that the "
           "chain settled. Two numbers per coefficient do that:"),
         block("s5-mcmc"),
         ul("**R-hat** compares one part of the chain with another. Values "
            "close to 1 mean the chain was drawing from the same place "
            "throughout. Values well above 1 mean it was still moving.",
            "**Bulk ESS** and **Tail ESS** say how many independent draws "
            "the 400 kept draws are worth, for the estimate and for the "
            "ends of the interval. Draws from a chain are correlated, so "
            "this can be far below 400."),
         p("Common reference points are an R-hat below about 1.01 and both "
           "effective sample sizes above about 400 (Vehtari et al. 2021). "
           "rnetis reports the numbers and does not grade them. If they "
           "are poor, raise `nscan`.")),
    rare("How much people differ: `netis_variance_components(fit)`",
         block("s5-vc"),
         p("*Sender variance* is how much people differ in their tendency "
           "to confide in others, beyond what the predictors explain. "
           "*Receiver variance* is the same for being confided in. The "
           "dyadic residual variance is fixed at 1: that is how a probit "
           "model sets its scale, and it is not an estimate. The "
           "within-dyad correlation is how far a tie in one direction goes "
           "with a tie in the other.")),
    rare("Why there is no AIC",
         refusal("s5-loglik"),
         p("The message gives the reason and names what to use instead.")),
    rare("Comparing with the truth",
         p("The three effects were built into the data on a logit scale: "
           "−1.1 for the affect gap, 0.8 for a shared project and 2.4 "
           "for an earlier tie. A probit coefficient is roughly a logit "
           "coefficient divided by 1.7, which gives about −0.65, 0.47 and "
           "1.41. The estimates are %s, %s and %s. The simulation also had "
           "no sender or receiver effects, so this is a rough comparison "
           "and not an exact one."
           % (F("s-gap"), F("s-project"), F("s-prior")))),
    rare("Getting the numbers out as a table",
         p("`netis_estimates(fit)` returns the coefficient table as a data "
           "frame and `coef(fit)` the estimates. `selection_effect()` "
           "returns its number and interval in a list.")),
)

# ---------------------------------------------------------------------------
# B6
# ---------------------------------------------------------------------------
page(
    GB, "B6 · Visualizing", "Visualizing the selection model",
    '<p class="lede">'
    "Three pictures are worth making for every selection model: the "
    "network being explained, the coefficients, and the fit check.</p>",

    h3("The network"),
    p("`plot()` on the data object draws the network. The outcome of this "
      "model is the confiding network at the second survey:"),
    codeonly('plot(team, variable = "confides", wave = 2, layout = "mds")'),
    fig("s6-network", "Who confides in whom at the second survey. Each "
        "arrow runs from the person confiding to the person confided in."),
    p("Compare it with the first survey's network on "
      "[page A6](#p-visualizing-the-influence-model). The model explains "
      "the difference between the two."),

    h3("The coefficients"),
    codeonly("plot(fit)"),
    fig("s6-coef", "Each point is a posterior mean and each bar its 95% "
        "interval, on the probit scale. The dashed line is zero."),
    p("This is the coefficient table as a picture. An interval that stays "
      "clear of the dashed line belongs to a term the data support. The "
      "horizontal axis is the probit scale, so the distances are not "
      "probabilities: use `selection_effect()` for those."),

    h3("The fit check"),
    codeonly('plot(fit, what = "gof")'),
    fig("s6-gof", "One panel per network feature. The histogram is that "
        "feature across the networks simulated from the fitted model, the "
        "shaded band is the middle 95% of them, and the vertical line is "
        "the network that was observed."),
    p("Look at where each line falls in its histogram. A line inside the "
      "shaded band means the model generates networks like yours on that "
      "feature. A line out in a tail, or beyond the histogram altogether, "
      "means the model cannot produce what you observed. Here every line "
      "is inside its band."),

    rare("Who confides widely, and who is confided in",
         codeonly('plot(fit, what = "actor_effects")'),
         fig("s6-actor", "Each point is a person: their sender effect "
             "against their receiver effect."),
         p("A person far to the right confides in more people than the "
           "predictors explain. A person high up is confided in by more. "
           "The two need not go together.")),
    rare("The chain, drawn",
         codeonly('plot(fit, what = "trace")'),
         fig("s6-trace", "The kept draws for each coefficient, in order."),
         p("A chain that has settled looks like a level band of noise. A "
           "drift up or down, or long flat stretches, means the chain "
           "needs to be longer.")),
    rare("Other plots, and options",
         table(["Call", "What it draws"], [
             ['`plot(team, variable = "confides", wave = 1, layout = "mds")`',
              "The network at the first survey"],
             ["`plot(fit2)`", "The coefficients of the model with extraversion"],
             ['`plot(fit, what = "posterior")`', "The draws for each coefficient, as a density"],
             ['`plot(fit, what = "mcmc")`', "R-hat and effective sample size, per coefficient"],
             ['`plot(fit, what = "heterogeneity")`', "The sender and receiver variances"],
             ['`plot(fit_latent, what = "latent")`', "The latent coordinates: page B7"],
             ['`plot(fit_separate, what = "groups")`', "Each team's estimates: page B8"],
         ]),
         p("Every plot is a `ggplot` object, so you can add a title or a "
           "theme with `+`. `plot_data()` with the same arguments returns "
           "the table behind a plot.")),
)

# ---------------------------------------------------------------------------
# B7
# ---------------------------------------------------------------------------
page(
    GB, "B7 · Latent factors", "The latent-factor selection model",
    '<p class="lede">'
    "This page fits the same model with latent factors added, which let "
    "the model pick up patterns of who confides in whom that the measured "
    "predictors do not explain.</p>",

    h3("Where we are"),
    p("So far the model explains a tie by the sender, the receiver and "
      "measured characteristics of the pair. It has no way to say that two "
      "particular people have an affinity that nobody measured, or that "
      "the team falls into clusters. The fit check on "
      "[page B5](#p-interpreting-the-selection-model) is how you find out "
      "whether that matters: clustering shows up in the two triad "
      "features."),

    h3("The idea"),
    p("Give every person a position in a small hidden space, twice: once "
      "as a sender and once as a receiver. A tie from one person to "
      "another is likelier when the sender's position and the receiver's "
      "position point the same way. These are **latent factors**, also "
      "called multiplicative effects, and `R` is how many dimensions the "
      "space has."),
    p("If you have read the Influence pages, this is close to the latent "
      "space on [page A7](#p-the-latent-space-influence-model), with a "
      "different job. There the positions were an adjustment, used to "
      "protect another coefficient. Here they are part of the model of "
      "who ties to whom."),

    h3("Run it"),
    p("The formula is unchanged. `R = 2` adds two latent dimensions."),
    block("s7-fit"),

    h3("Reading it"),
    p("The coefficient table has the same rows. Compare the estimates "
      "with the model that has no latent factors:"),
    block("s7-compare"),
    p("They have barely moved. Then compare the fit check, feature by "
      "feature:"),
    block("s7-gof"),
    p("The model without latent factors already reproduced all five "
      "features, and the latent factors do not change that. In these data "
      "they were not needed. That is a result worth reporting, because it "
      "says you looked: *adding two latent dimensions did not change the "
      "estimates or the fit check.*"),
    quote("**When they are needed.** If the two triad features fall "
          "outside their ranges without latent factors and inside with "
          "them, the latent factors have found structure the predictors "
          "missed, and the model with them is the one to report."),

    h3("See the space"),
    codeonly('plot(fit_latent, what = "latent")'),
    fig("s7-latent", "Each person appears twice: in blue at their "
        "position as a sender and in orange at their position as a "
        "receiver. Each grey line is an observed confiding tie, drawn from "
        "the sender's blue point to the receiver's orange point."),
    p("Read a blue point against an orange point. A person is likelier to "
      "confide in someone whose orange point lies in the **same direction "
      "from the centre** as their own blue point, and less likely where "
      "the two lie in opposite directions. People near the centre are "
      "barely affected. The axes themselves mean nothing, because the "
      "space can be rotated without changing the model."),
    p("The grey lines show the ties the model was fitted to. If the latent "
      "factors were capturing structure, the lines would mostly join blue "
      "and orange points on the same side of the centre, and few would "
      "cross it. Here the points form one cloud with no separate groups "
      "and the lines run in every direction, which is what *not needed* "
      "looks like."),

    rare("How many dimensions?",
         p("Start with `R = 1` or `R = 2` and look at the fit check. Raise "
           "`R` only while the features that were outside their ranges "
           "keep moving inside. Each dimension adds two numbers per "
           "person, estimated from the same ties, so a larger `R` makes "
           "everything noisier and the chain slower. With `R = 1`:"),
         block("s7-r1")),
    rare("Where the coordinates come from",
         p("The model does not identify each person's position, only the "
           "**product** of a sender's position and a receiver's. Within "
           "the chain the positions rotate freely from draw to draw, so "
           "averaging them would blur everything toward the centre. rnetis "
           "therefore keeps the average of the product, and "
           "`what = \"latent\"` computes coordinates from that one table, "
           "choosing the two directions that carry most of it. The "
           "caption on the plot says how much of the product those two "
           "directions carry."),
         p("This way of drawing the coordinates is rnetis's own display "
           "choice. `what = \"multiplicative\"` shows the product itself, "
           "one cell per ordered pair:"),
         codeonly('plot(fit_latent, what = "multiplicative")'),
         fig("s7-mult", "The product for every ordered pair. Orange cells "
             "are pairs the latent factors make likelier to have a tie, "
             "blue cells less likely.")),
    rare("Why the coefficients can move when latent factors are added",
         p("A probit model fixes its leftover variance at 1 to set its "
           "scale. When latent factors explain part of what was leftover, "
           "everything else is measured against a smaller remainder, and "
           "the coefficients can all grow together. That is a change of "
           "scale and not a change in the effects. Compare models with "
           "`selection_effect()`, which is on the probability scale.")),
)

# ---------------------------------------------------------------------------
# B8
# ---------------------------------------------------------------------------
page(
    GB, "B8 · Multilevel", "The multilevel selection model",
    '<p class="lede">'
    "This page moves from one team to ten. The call is the same, and "
    "rnetis fits each team's network and combines the results.</p>",

    h3("Where we are"),
    p("Everything so far used one team of forty. With several teams, a "
      "tie can only exist inside a team: nobody in one team could have "
      "confided in someone in another. Ten teams are therefore ten "
      "networks, and stacking them into one would treat every pair across "
      "teams as a tie that was possible and did not happen."),

    h3("The data"),
    p("`demo_affect_teams` is the dataset from "
      "[page A8](#p-the-multilevel-influence-model): ten teams of twenty "
      "people, with the same variables and the same planted effects."),
    block("i8-data"),

    h3("Run it"),
    p("The call does not change. rnetis sees ten groups, fits the model "
      "to each team's network separately, and then pools the ten sets of "
      "coefficients into one. The chains here are shorter than on the "
      "earlier pages, to keep ten fits to a few minutes."),
    block("s8-fit"),

    h3("Reading it"),
    p("The selection effect is %s, and with ten teams its interval is "
      "narrower than it was with one. Three things are new:"
      % F("s8-gap")),
    ul("**The pooling paragraph.** The coefficients were combined by "
       "weighting each team's estimate by its precision. That assumes the "
       "effect is the **same in every team**. If the teams truly differ, "
       "this model does not tell you.",
       "**`SE`, `z value` and `Pr(>|z|)`.** The pooled table reports "
       "ordinary tests. Pooling combines ten estimates and their standard "
       "errors, so there are no pooled draws to summarise.",
       "**The fit check counts teams.** Each team has its own network and "
       "its own five features, so the check reports how many of the fifty "
       "team-by-feature comparisons fall inside their range. Here four "
       "fall outside, all of them about how unequally people in a team "
       "send or receive ties."),

    h3("Do the teams agree?"),
    p("Pooling is only sensible if the teams tell the same story. Fit "
      "them separately, with `pool_beta = FALSE`, and look:"),
    block("s8-separate"),
    codeonly('plot(fit_separate, what = "groups")'),
    fig("s8-groups", "Each team's estimate and 95% interval, term by term."),
    p("The selection effect ranges from %s to %s across the ten teams, and "
      "the intervals overlap heavily. That is what agreement looks like, "
      "and it is what makes the pooled estimate a fair summary. If the "
      "intervals barely overlapped, the difference between teams would be "
      "the finding, and pooling would hide it."
      % (F("s8-gap-min"), F("s8-gap-max"))),

    rare("One model for all teams: the hierarchical fit",
         p("A third option fits all ten teams in one model and lets chosen "
           "coefficients differ from team to team around a common "
           "average. Teams with little information are pulled toward the "
           "average. `selection_pooling(random = ...)` names the "
           "coefficients that may differ:"),
         block("s8-hier"),
         p("The **Regime** column says which coefficients are shared by "
           "all teams and which vary. For a varying coefficient the table "
           "reports the average across teams. `netis_hierarchy(fit_hier)` "
           "gives the spread between teams and each team's own value."),
         note("The hierarchical fit is experimental. It is this package's "
              "own construction and has not been validated for statistical "
              "performance against alternatives: see "
              "`?hierarchical_selection`. A conclusion that rests on it "
              "alone should say so. The pseudo R-squared and the fit check "
              "are not available for it, so check the teams separately.")),
    rare("Other settings",
         table(["Call", "What it fits"], [
             ["`fit_selection(..., data = teams)`",
              "Each team separately, then pooled into one set of coefficients"],
             ["`pool_beta = FALSE`", "Each team separately, results kept apart"],
             ["`pooling = selection_pooling(random = ...)`",
              "One hierarchical model for all teams (experimental)"],
             ["`R = 2`", "Latent factors, with one space per team"],
         ]),
         p("The functions and plots from pages B5 and B6 work on a "
           "multi-team fit. Those that describe one network need a team: "
           "`plot(fit_separate, what = \"actor_effects\", group = \"team01\")`.")),
    rare("How many teams is enough?",
         p("Pooling works with any number of teams, and each added team "
           "narrows the pooled interval. Judging whether teams *differ* "
           "needs more: ten teams give only a rough idea of how much an "
           "effect varies between them.")),

    h3("Where to go from here"),
    p("That is the Selection sequence: a model you can fit, read, check, "
      "plot, extend with latent factors, and run across several groups. "
      "Together with the Influence sequence it answers both questions "
      "about the same team: people's affect moved toward that of their "
      "confiders, and people came to confide in others whose affect was "
      "like theirs. For anything not covered here, start with "
      "[formula terms at a glance](#p-formula-terms-at-a-glance) and "
      "[the rest of the package](#p-the-rest-of-the-package)."),
)
