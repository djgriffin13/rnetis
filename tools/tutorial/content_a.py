# -*- coding: utf-8 -*-
"""Welcome, getting started, and the Influence path."""

from build import (
    page, p, h3, h4, ul, ol, quote, note, table, code, result, block,
    refusal, codeonly, fig, card, cards, rare, filterbox, dia, eq,
)
import diagrams as D

# ===========================================================================
# Welcome
# ===========================================================================
page(
    None, "Welcome", "Welcome",
    '<img class="hero" src="logo.png" alt="rnetis">',
    '<p class="lede">rnetis puts two established families of social network '
    "model behind one data structure and one formula grammar: "
    "<strong>Influence</strong>, where a node characteristic changes with what "
    "the nodes around it were doing, and <strong>Selection</strong>, where a "
    "tie forms or persists as a function of who the sender and receiver are.</p>",

    p("This tutorial works each one end to end. You will build a data object "
      "out of two ordinary data frames, construct network exposure, fit a "
      "model, read the result on a scale someone can interpret, check it, and "
      "then ask what it took for granted. The two paths run in parallel and "
      "are self-contained, so you can start with whichever matches your "
      "question."),

    h3("Please read this first"),
    quote("**This page is shared for a workshop, and the software is not yet "
          "public.** rnetis is proprietary software owned by Griffin "
          "Dynamics LLC. The full version is under review and will be "
          "released publicly in due course."),
    p("You are welcome to run the code in any distributable you have been "
      "given, on your own data, for your own work. Please do **not** "
      "redistribute the software, this page, or the materials that came with "
      "them, and please do not pass them on to anyone who was not given "
      "access directly."),
    p("If you would like to use rnetis in a project, would like to be told "
      "when it is released, or want to talk about a collaboration, write to "
      "**Daniel Griffin** at "
      "[danieljgriffin618@gmail.com](mailto:danieljgriffin618@gmail.com) "
      "and we will send release information when it is available."),


    '<div class="pathbox">'
    '<div><h4>Path A · Influence</h4>'
    "<p>Does a person's outcome move toward the outcomes of the people they "
    "are tied to? Eight pages: the data, exposure, the fit, reading it, the "
    "homophily problem, several networks, checking the fit, and what else is "
    "there.</p>"
    '<p><a href="#p-influence-the-question">Start Path A →</a></p></div>'
    '<div><h4>Path B · Selection</h4>'
    "<p>What predicts a tie? Seven pages: the data, the model and its "
    "equation, running it, probability-scale effect sizes, several groups, "
    "one hierarchical model, and what else is there.</p>"
    '<p><a href="#p-selection-the-question">Start Path B →</a></p></div>'
    "</div>",
    h3("What you need"),
    ul("**R 4.1 or later**, and the rnetis package. [Install rnetis](#p-install-rnetis) "
       "covers both, and the optional packages each capability needs.",
       "No compiler: rnetis is pure R.",
       "No data of your own. Everything here either is typed out in front of "
       "you or uses `demo_influence`, `demo_selection` or "
       "`demo_influence_groups`, which are synthetic and ship with the package."),

    h3("How long it takes"),
    p("Path A runs in seconds end to end; it is ordinary least squares and a "
      "mixed model, apart from the latent-space adjustment. Path B fits "
      "Markov chains through `amen::ame()`: at the settings used here a "
      "single-group fit took about fifteen seconds on the machine this page "
      "was built on, and the hierarchical model on the last page took about "
      "eight minutes. Reading carefully, rather than waiting for R, is what "
      "will take you an afternoon."),

    h3("Every result on this page is real"),
    p("The grey blocks are code. The block beneath each one is what R actually "
      "printed when that exact code was run, captured from a live session, "
      "including the warnings and the refusals. Nothing on this page was "
      "typed by hand to look like output. Where a number here differs from "
      "yours, the cause is a version or a seed, not an invention."),

    quote("**rnetis reports quantities, not verdicts.** No surface in the "
          "package prints “good fit” or “assumption satisfied”, "
          "and this tutorial does not either. Several pages end by naming what "
          "the result you have just produced does *not* establish. That is not "
          "hedging; it is the part of the analysis that usually goes unwritten."),

    p("Reference material sits at the end: the [formula terms](#p-formula-terms-at-a-glance), "
      "[derived networks](#p-derived-networks), "
      "[the rest of the package](#p-the-rest-of-the-package), "
      "[what rnetis refuses](#p-what-rnetis-refuses) and "
      "[what is deliberately not here](#p-scope-what-is-not-here). "
      "The search box at the top searches every page at once."),
)

# ===========================================================================
# Getting started
# ===========================================================================
page(
    "Getting started", "Install rnetis", "Install rnetis",

    p("rnetis is not on CRAN. It is distributed by Griffin Dynamics LLC to "
      "people it gives a copy to, so installation depends on what you were "
      "given."),

    cards(
        card("You were given a source tarball", codeonly(
            'install.packages("rnetis_0.1.0.tar.gz", repos = NULL, type = "source")'),
            num=1, open=True),
        card("You were given access to the repository", codeonly(
            '# install.packages("remotes")\n'
            'remotes::install_github("djgriffin13/rnetis", build_vignettes = TRUE)'),
            num=2),
        card("You have a local checkout", codeonly(
            'remotes::install_local(".", build_vignettes = TRUE)\n'
            '# or, for development:\n'
            'devtools::load_all(".")'),
            num=3),
    ),

    h3("Check that it worked"),
    block("g-check"),

    p("Four required packages come with rnetis — `generics`, `ggplot2`, "
      "`lme4` and `reformulas` — and the install step pulls them from CRAN "
      "for you. `ggplot2` is what sets the R 4.1 floor."),

    h3("Optional packages, by what they unlock"),
    p("Nothing below is installed by default, and none of it is needed until "
      "you reach for that capability. rnetis names the package you need, at "
      "the point you need it."),

    table(["Install", "What it unlocks", "Needed for"], [
        ['`install.packages("amen")`', "`fit_selection()` — the Selection model",
         "All of Path B"],
        ['`install.packages("latentnet")`', "`latent()` — the latent-space adjustment",
         "[Path A, page 5](#p-homophily-and-the-latent-space)"],
        ['`install.packages("clubSandwich")`', "cluster-robust standard errors",
         "[Path A, page 6](#p-several-networks)"],
        ['`install.packages(c("igraph", "network"))`',
         "importing from and exporting to `igraph` and `statnet`",
         "[Importing and exporting](#p-importing-and-exporting)"],
        ['`install.packages(c("sna", "numDeriv"))`',
         "the network-autocorrelation (SAR) estimator",
         "[The rest of the package](#p-the-rest-of-the-package)"],
        ['`install.packages("RSiena")`', "the `s50` data of Xu's worked example",
         "Nothing in this tutorial"],
    ]),

    p("To follow every page of this tutorial, you want `amen`, `latentnet`, "
      "`clubSandwich` and `igraph`. Check what you have:"),
    block("g-optional"),

    rare("A note on RSiena and the R version",
         p("`RSiena` 1.6.6 requires R ≥ 4.5, which is above rnetis's own "
           "floor of 4.1. On an older R, install an archived `RSiena` or skip "
           "it entirely: `latent()` does not need it, only Xu's example data "
           "does, and this tutorial uses neither.")),
)

page(
    "Getting started", "How to read this", "How to read this tutorial",

    p("Each page is shown on its own. Move with **Next** and **Previous** at "
      "the foot of the page, the sidebar, or the Contents menu on a narrow "
      "screen. **Show all** in the header puts every page on one scroll, which "
      "is what you want for your browser's own find. The page remembers where "
      "you were."),

    h3("The two kinds of block"),
    p("A code block is meant to be run. Copy it with the button, or type it; "
      "the examples build on each other within a page, so run them in order."),
    code("t-nodes"),
    p("Directly underneath, in a quieter box labelled **Result**, is what R "
      "printed when that code was run:"),
    result("t-nodes"),

    h3("Refusals are shown on purpose"),
    p("rnetis refuses more often than most R packages, and a refusal is "
      "usually the most informative thing it will tell you. Where a block is "
      "meant to fail, its result box is labelled **rnetis refuses** and the "
      "message is reproduced in full. Read those messages: each one names the "
      "decision it is asking you to make, and most of them name the argument "
      "that makes it."),
    refusal("t-symmetrize-refuse"),
    note("That is the real message, not a paraphrase. "
         "[What rnetis refuses](#p-what-rnetis-refuses) collects them in one place."),

    h3("Conventions"),
    table(["You see", "It means"], [
        ["A **Result** box", "Captured from a live R session running the code above it."],
        ["A **rnetis refuses** box", "The code above it is *supposed* to fail; the "
         "message is the lesson."],
        ["A drawn figure", "A diagram explaining an idea. Every figure carrying a "
         "number from a fit is a plot R produced."],
        ["A collapsed card", "Detail you can skip on a first pass."],
        ["`?function_name`", "The package's own help page, which carries the citation "
         "for whatever that function computes."],
    ]),

    h3("Where the method lives"),
    p("rnetis's contribution is unification and usability more than new "
      "methodology: it implements and integrates published methods, and every "
      "one is cited on the help page of the function that computes it. A small "
      "number of things *are* the package's own — the hierarchical "
      "multi-group Selection model, the latent-scale variance share, "
      "multi-group `latent()`, the assumption ledger and the numeric "
      "thresholds in the diagnostics — and each says so where it is "
      "documented. This tutorial flags them in the same places."),
)

# ===========================================================================
# Nodes, ties and waves  (the data page)
# ===========================================================================
page(
    "Getting started", "Nodes, ties and waves", "Nodes, ties and waves",

    p("Everything in rnetis reads one structure, `netis_data`. You do not have "
      "to learn a new way of holding data to build one: it is assembled from "
      "ordinary data frames, and it hands them back unchanged. This page "
      "builds one from scratch, in front of you, and uses it to settle the "
      "three questions that decide what every later number means — which "
      "way a tie points, what a weight is, and what an absent tie is."),

    h3("Step 1 · a table of people"),
    p("One row per person per occasion. The column named `node` identifies the "
      "person; `wave` says which occasion; everything else is a variable "
      "measured on them. Six colleagues, measured twice."),
    block("t-nodes"),

    h3("Step 2 · a table of ties"),
    p("One row per tie. `source` is the end the relation comes **from** and "
      "`target` the end it goes **to**. So the first row says that Ben gives "
      "advice to Ada, with strength 1."),
    block("t-ties"),
    note("This tie table has no `wave` column. That is a claim: the advice "
         "network is one structure observed across both occasions. Add a "
         "`wave` column and the network becomes dynamic instead."),

    h3("Step 3 · make it a netis_data"),
    p("`netis_data()` takes the two data frames and the few declarations that "
      "cannot be read off them."),
    block("t-build"),
    p("The printed summary is the part worth reading rather than skipping. It "
      "says what rnetis **inferred** and marks it as inferred — here, that "
      "a row's presence is what encodes a tie — and it reports that it "
      "sorted the node variables into static and dynamic by itself:"),
    block("t-static-dynamic"),

    p("And the tables come straight back out:"),
    block("t-back"),

    h3("Which way does a tie point?"),
    p("A directed tie has a sender and a receiver, and almost every quantity "
      "in a network model depends on which is which. rnetis never transposes "
      "a network behind your back: `source` and `target` mean what they say, "
      "from import to result."),
    dia(D.directed_vs_mutual(),
        "A tie row draws one arrow, from `source` to `target`. Two rows "
        "pointing opposite ways are what makes a dyad mutual — there is no "
        "other way to get one."),

    p("As a matrix, that is rows for senders and columns for receivers:"),
    block("t-adjacency"),
    p("**Reciprocity** is the share of ties whose reverse also exists. It is a "
      "property of pairs of rows, never inferred from the fact that two people "
      "are in the same network:"),
    block("t-reciprocity"),

    p("Collapsing the two directions into one undirected network is a real "
      "operation with several possible meanings, so it is never done for you:"),
    refusal("t-symmetrize-refuse"),
    note("`symmetrize()` with a named rule is in "
         "[Derived networks](#p-derived-networks). The vocabulary is worth "
         "fixing now: **source** and **target** are the two ends of a stored "
         "row; **sender** and **receiver** are the same two ends named by the "
         "part they play; a **mutual** or **reciprocated** dyad is one where "
         "both rows exist; and **undirected** means the distinction has been "
         "given up deliberately."),

    h3("Zero, and the absence of a row, are not the same thing"),
    p("A dyad can be in one of three states, and only two of them are "
      "visible in the table."),
    dia(D.zero_versus_missing(),
        "A row with a value, a row with a zero, and no row at all. The third "
        "is the one that needs a decision from you."),

    p("Here is a complete table for three people — every ordered pair "
      "asked about, two of them ties and four observed non-ties:"),
    block("t-zero-row"),
    p("Saying that every pair was observed is not, on its own, enough. rnetis "
      "still needs to know which of those rows counts as a tie, and it asks "
      "the moment something needs the answer:"),
    refusal("t-presence-refuse"),

    p("The reason it refuses is that the two readings of the same six rows "
      "give different networks. Under `row_presence`, a row **is** a tie, so "
      "the four zeros are counted as ties and the network looks complete. "
      "Under a value rule, a zero row is an observed non-tie:"),
    block("t-presence-contrast"),
    quote("One declaration, and the density went from 1 to 0.33. Nothing else "
          "changed. Every density, degree, centrality and exposure in an "
          "analysis is built on this, which is why rnetis will not pick for "
          "you and why it prints which reading it used."),

    p("The third state is a pair with no row at all. Under `row_presence` that "
      "is simply not a tie; under `complete_dyadic` the claim is that it "
      "*was* observed, so an absent pair is a measured zero. Neither of those "
      "says “we do not know”, which is what a genuinely unobserved pair "
      "is:"),
    block("t-missing-row"),
    note("This is why **missing ≠ zero ≠ structurally absent** is a rule "
         "rather than a slogan. A zero says the relation was looked for and "
         "was not there. A missing cell says nobody looked. Treating the "
         "second as the first inflates every denominator in the study."),

    h3("The four declarations"),
    p("Declaring these replaces rnetis's inference with your statement, and is "
      "the single most useful thing you can do to a dataset before modelling "
      "it. The ledger records which is which:"),
    block("t-boundary"),

    table(["Declaration", "The question it answers"], [
        ["`weighted`", "Is a larger value a stronger tie, or is this yes/no?"],
        ["`weight_semantics`", "Is the weight a count, a strength, or a distance? "
         "A measure that reads weights as distances refuses a network declared "
         "as strengths unless you transform it."],
        ["`zero_meaning`", "Is a zero a measured absence, or a cell nobody asked about? "
         "`measured_zero` says the absence is data."],
        ["`presence_mode`", "Which rows count as ties — all of them, the nonzero "
         "ones, or the ones an indicator column names?"],
    ]),

    h3("Waves"),
    p("Wave **positions** are what the models use: wave 1 then wave 2, "
      "whatever the calendar says. Metric time, if you have it, is carried "
      "alongside as metadata rather than driving anything:"),
    block("d-time-index"),
    note("A node variable that varies by wave is dynamic and a lag of it is "
         "meaningful; one that does not is static, and `lagged()` on it reads "
         "the same numbers. rnetis works out which is which, and says so."),
)

page(
    "Getting started", "Importing and exporting", "Importing and exporting",

    p("You will usually already have the data in some other shape. An "
      "importer builds the same object from it and records anything the "
      "conversion could not carry; an exporter hands it to the package you "
      "want to draw or compute with. Nothing is one-way."),

    table(["You have", "Use"], [
        ["Two data frames", "`netis_data()` — "
         "[the previous page](#p-nodes-ties-and-waves)"],
        ["An adjacency matrix", "`netis_data_from_adjacency()`"],
        ["One adjacency matrix per wave", "`netis_data_from_adjacency_series()`"],
        ["An edge list", "`netis_data_from_edgelist()`"],
        ["An `igraph` graph, or a list of them",
         "`netis_data_from_igraph()`, `netis_data_from_igraph_list()`"],
        ["A `network` object", "`netis_data_from_network()`"],
        ["Separate static and dynamic tables", "`netis_data_from_split()`"],
    ]),
    note("And in the other direction: `netis_nodes()` and `netis_ties()` for "
         "the data frames, `netis_adjacency()` for a matrix, `as_igraph()` and "
         "`as_igraph_list()` for `igraph`, `as_network()` and "
         "`as_network_list()` for `statnet`."),

    h3("From a matrix"),
    block("d-adjacency"),
    p("A matrix whose diagonal is not zero is a decision, not a detail, so it "
      "is put back to you:"),
    refusal("d-adjacency-refuse"),

    h3("From an edge list, and the isolate problem"),
    p("An edge list names only nodes that have ties. If your study has "
      "isolates, they are not in it — and every density, degree and "
      "centrality computed afterwards would quietly be computed over the wrong "
      "node set. rnetis will not guess:"),
    refusal("d-edgelist-refuse"),
    p("Supply the roster and the ambiguity is gone:"),
    block("d-edgelist-roster"),
    note("Node `d` has no ties, and is in the object. That is the whole point."),

    h3("From separate static and dynamic tables"),
    block("r-split"),

    h3("What a conversion can lose"),
    p("Every loss a conversion can incur has a stable identifier. "
      "**Semantic** losses can change a downstream number, and they refuse. "
      "**Representational** losses cannot, and they warn — or pass "
      "silently once you have authorized that particular one by name through "
      "`allow_loss`."),
    block("d-losses"),
    note("There is deliberately no blanket “allow everything” switch: one "
         "flag cannot express that you reviewed a particular sacrifice. "
         "The two severities and this table are the package's own bookkeeping, "
         "not a published classification."),

    h3("Converting back out"),
    block("r-igraph-out"),
    block("r-adjacency-out"),
    note("Rows are senders and columns receivers, matching the `source` and "
         "`target` columns the ties went in as."),
    rare("From igraph, and to statnet",
         block("r-igraph-in"),
         block("r-network-out")),
)

page(
    "Getting started", "Check the data", "Check the data",

    p("`diagnose_data()` looks at the object you built and reports what it "
      "found. It returns a structured collection, not prose, and it has no "
      "opinion about whether you should proceed."),

    block("d-diagnose-clean"),
    p("The packaged demo data is clean. Most real data is not. Here is a "
      "small object with two ordinary problems in it — isolates, and a "
      "variable that never varies:"),
    block("d-diagnose-findings"),

    p("Findings are data. Filter them by level or family:"),
    block("d-filter"),

    note("A NOTE is not a failure and an ERROR is not a verdict about your "
         "study. They are statements about the object: four nodes have no "
         "ties, and `x` cannot contribute to a model that needs it to vary."),

    h3("Look at it"),
    p("`plot()` on a `netis_data` draws one panel at a time. A network drawing "
      "shows a single snapshot, so with more than one wave you name which:"),
    block("d-plot-network"),
    fig("d-plot-network", "`demo_influence` at wave 1: 30 nodes, 118 directed ties."),

    p("`what = \"coverage\"` asks a different question — what is actually "
      "observed, by variable and wave:"),
    block("d-plot-coverage"),
    fig("d-plot-coverage", "Coverage by variable and wave."),

    p("Every panel has a table behind it, and `plot_data()` returns that table "
      "so you can draw it yourself or put the numbers in a paper:"),
    block("d-plot-data"),

    rare("The panels available",
         table(["`what =`", "Shows"], [
             ['`"network"` (default)', "The network itself, one snapshot."],
             ['`"coverage"`', "What is observed, by variable and wave."],
             ['`"missing"`', "Where the missing cells are."],
             ['`"exposure"`', "The distribution of a constructed exposure."],
             ['`"centrality"`', "The distribution of a centrality measure."],
         ])),
)

# ===========================================================================
# Path A
# ===========================================================================
page(
    "Path A · Influence", "The question", "Influence: the question",

    p("An Influence model asks whether a node's characteristic changes with "
      "its own prior state and with what the nodes around it were doing. The "
      "outcome is a node variable measured at two occasions; the predictor of "
      "interest is **exposure**, a summary of the alters' characteristic "
      "carried through the network."),

    quote("The question is *not* “are connected people similar?” "
          "They usually are, and on its own it tells you very little. The "
          "question is whether a node's outcome **shifts toward** its alters' "
          "from one occasion to the next, once you have accounted for the "
          "node's own earlier value."),

    p("That last clause is the whole design. Two people who were already "
      "alike will look alike again at wave 2 whether or not anything passed "
      "between them. Putting the earlier outcome in the model is what turns "
      "the question from “are they similar?” into “did this one "
      "move?”"),

    eq('<i>y<sub>i,t</sub></i> <span class="op">=</span> <i>α</i> '
       '<span class="op">+</span> <i>γ y<sub>i,t−1</sub></i> '
       '<span class="op">+</span> <i>β E<sub>i,t−1</sub></i> '
       '<span class="op">+</span> <i>x<sub>i</sub></i>′<i>δ</i> '
       '<span class="op">+</span> <i>ε<sub>i,t</sub></i>',
       "y is the outcome, E the exposure the next page builds, γ the "
       "carry-over from the node's own earlier value, and β the coefficient "
       "the whole path is about."),

    h3("The path from here"),
    ol("**[The data](#p-the-influence-dataset)** — the dataset the worked "
       "example uses, and what to look at before fitting anything.",
       "**[Exposure](#p-exposure)** — what reaches a node through its ties, "
       "and the two ways of summarising it that answer different questions.",
       "**[Fit the model](#p-fit-the-model)** — the formula grammar, and "
       "how closely it parallels `lm()` and `lmer()`.",
       "**[Read the results](#p-read-the-results)** — the coefficient, and a "
       "magnitude on the outcome's own scale.",
       "**[Homophily and the latent space](#p-homophily-and-the-latent-space)** "
       "— the confound that makes all of the above an association rather "
       "than an effect, and one published adjustment for it.",
       "**[Several networks](#p-several-networks)** — random intercepts, "
       "cluster-robust standard errors, and whether a group mean is reliable.",
       "**[Check the fit](#p-check-the-fit)** — the fit statistics, the "
       "residuals with respect to the network, and the assumption ledger.",
       "**[Beyond this path](#p-beyond-path-a)** — what else the Influence "
       "side can do, and which parts are experimental."),
)

page(
    "Path A · Influence", "1 · The data", "The Influence dataset",

    p("The worked example uses `demo_influence`, which ships with the package. "
      "It is two data frames, exactly like the ones you built on "
      "[Nodes, ties and waves](#p-nodes-ties-and-waves) — just bigger, and "
      "generated from a known process so that the code on these pages has "
      "something to run on."),
    block("d-demo"),
    p("Thirty nodes in one network, observed twice, with an outcome `y`, a "
      "covariate `x`, and a directed relation `tie` that does not change "
      "between waves. The substantive story to keep in mind: `y` is something "
      "that could plausibly travel — an attitude, a practice, a level of "
      "engagement — and `tie` is the relation it could travel along."),

    h3("Look before you fit"),
    p("Two panels are worth a minute before any model. The network itself, "
      "which tells you whether the structure is anything like what you "
      "assumed:"),
    block("d-plot-network"),
    fig("d-plot-network", "`demo_influence` at wave 1: 30 nodes, 118 directed ties."),
    p("And coverage, which tells you what is actually observed before a fit "
      "silently drops the rest:"),
    block("d-plot-coverage"),
    fig("d-plot-coverage", "Coverage by variable and wave."),
    note("`diagnose_data()` on [Check the data](#p-check-the-data) is the "
         "structured version of the same look, and is the one to run on your "
         "own data, where the answer will be less tidy than this."),

    h3("One thing to notice now"),
    p("Thirty nodes is a small network for this question, and it will matter "
      "when the interval on the exposure coefficient turns out wide. That is "
      "not a flaw in the example; it is the ordinary situation in a two-wave "
      "study of one team, and "
      "[How precise is an exposure coefficient?](#p-how-precise-is-an-exposure-coefficient) "
      "comes back to it with simulations."),
)

page(
    "Path A · Influence", "2 · Exposure", "Exposure",

    p("Exposure is how much of a characteristic reaches a node through its "
      "ties. It is the predictor the whole path is about, and it is "
      "constructed rather than measured — so what it means is whatever you "
      "built. The default is the weighted sum of Friedkin & Johnsen (1990) "
      "and Marsden & Friedkin (1993):"),

    eq('<i>E<sub>i</sub></i> <span class="op">=</span> '
       '<span style="font-size:1.25em">Σ</span><sub><i>j</i></sub> '
       '<i>W<sub>ij</sub> x<sub>j</sub></i>',
       "W[i, j] is the strength with which j influences i, and x is the "
       "characteristic being carried. The sum runs over everyone who reaches "
       "i."),

    p("The team from [Nodes, ties and waves](#p-nodes-ties-and-waves) is "
      "small enough to check by hand. Four people advise Ada, with different "
      "strengths, and they differ in the characteristic being carried:"),
    dia(D.ego_exposure(),
        "Ada's exposure, drawn. Colour is each person's wellbeing — the "
        "characteristic travelling through the network. Line width is the "
        "strength of the advice tie. Both go into the sum; neither is "
        "sufficient on its own."),

    p("The sum weights each adviser's wellbeing by how strong that advice tie "
      "is:"),
    eq('<i>E</i><sub>Ada</sub> <span class="op">=</span> '
       '1<span class="op">×</span>10 <span class="op">+</span> '
       '3<span class="op">×</span>20 <span class="op">+</span> '
       '2<span class="op">×</span>30 <span class="op">+</span> '
       '1<span class="op">×</span>40 <span class="op">=</span> 170'),
    block("t-exposure-sum"),
    p("Cyd, with strength 3, contributes three times what Ben does at the same "
      "wellbeing — that is what a weighted conduit means. Fay gets 0: "
      "nothing reaches her, so nothing sums to anything."),

    h3("Sum or average — a choice, not a default"),
    p("Row normalization divides by the node's total tie weight (Leenders "
      "2002), which turns the total inflow into the **average** of a node's "
      "advisers:"),
    eq('<i>E<sub>i</sub></i> <span class="op">=</span> '
       '<span style="font-size:1.25em">Σ</span><sub><i>j</i></sub> '
       '<i>W<sub>ij</sub> x<sub>j</sub></i> '
       '<span class="op">⁄</span> '
       '<span style="font-size:1.25em">Σ</span><sub><i>j</i></sub> '
       '<i>W<sub>ij</sub></i>'),
    block("t-exposure-row"),
    note("Read the top row first: Ada's 170 has become 170 ⁄ 7 = 24.29, "
         "the average wellbeing of her advisers, weighted by advice strength. "
         "The other rows are the same arithmetic — and Fay's `NA` is the "
         "subject of the next section, not a mistake."),

    p("The two quantities answer different questions, and the difference is "
      "not cosmetic:"),
    table(["", "`normalize = \"none\"` (default)", "`normalize = \"row\"`"], [
        ["Quantity", "Total inflow", "Average alter"],
        ["Rises with the number of ties", "Yes", "No"],
        ["A node with twice as many alters",
         "Has twice the exposure, other things equal",
         "Has the same exposure, other things equal"],
        ["Suits", "A dose that accumulates", "A norm the node is being pulled toward"],
    ]),
    p("Pick the one your theory implies, and say which you used. A finding "
      "that holds for the total but not the average is a finding about "
      "accumulation, and it should be reported that way."),

    h3("A node nobody advises"),
    p("Fay has no incoming ties. There is no average of her advisers to take, "
      "because she has none — so rnetis makes it `NA` rather than quietly "
      "calling it zero:"),
    block("t-exposure-row"),
    quote("A zero would be a claim: that Fay's advisers average zero on the "
          "scale. On this scale wellbeing runs from 10 to 60, so zero would "
          "put her off the bottom of the observed range and make her the most "
          "extreme case in the dataset — purely because nobody advises "
          "her. An `NA` makes no claim at all, and the row drops out of the "
          "fit, which is the honest default."),
    block("t-exposure-zero"),
    p("There are situations where zero is genuinely right: when the "
      "characteristic is a dose and zero means none of it arrived, and the "
      "scale has a real zero. `isolate_rule` lets you say so — or stop "
      "instead:"),
    refusal("t-exposure-error"),
    note("What matters is **incoming** ties, not ties at all. A node with "
         "plenty of outgoing ties and none coming in has no exposure under "
         "the default reading, because nothing reaches it."),

    h3("Reversing the reading"),
    p("The default reads a stored tie as “source influences target”. "
      "Reversing it asks a different question of the same data: not what "
      "reaches Ada through the people who advise her, but what reaches her "
      "through the people she advises."),
    block("t-exposure-direction"),
    note("This is the one place in the package that reverses a network, it is "
         "an argument you have to type, and the fit records that you typed it."),

    h3("On the worked dataset"),
    block("a-exposure-demo"),

    rare("Influence along longer paths",
         p("Exposure through a single step of the network is the common case "
           "and the one this tutorial uses. Influence that travels along "
           "longer paths, with each step discounted, is the "
           "balance-information mechanism of Frank & Fahrbach (1999): "
           "`bi(delta, alpha, gamma)`, with `bi_components()` and `bi_sweep()` "
           "alongside it. See "
           "[The rest of the package](#p-the-rest-of-the-package).")),
)

page(
    "Path A · Influence", "3 · Fit the model", "Fit the model",

    p("`fit_influence()` takes a formula and a data object, and if you have "
      "ever fitted a regression in R you already know most of the grammar."),
    block("a-fit"),

    h3("This is lm() and lmer(), with the network parts written down"),
    p("The formula is deliberately the same shape as the ones you already "
      "write. What changes is that the two columns a network model needs — "
      "the lagged outcome and the exposure — are *named in the formula* "
      "instead of being built by hand in a script beforehand."),

    table(["Written with", "The formula", "Who builds the columns"], [
        ["`stats::lm()`", "`y2 ~ y1 + x`", "You do, before fitting."],
        ["`lme4::lmer()`", "`y2 ~ y1 + x + (1 | group)`", "You do, before fitting."],
        ["`rnetis::fit_influence()`", "`y ~ ar() + x + (1 | group)`",
         "The formula does, from the data object."],
    ]),

    p("That is not an analogy — it is the same arithmetic. Build the "
      "lagged column and the exposure column yourself and hand them to "
      "`lm()`:"),
    block("a2-lm-parallel"),
    p("…and then let the formula do it:"),
    block("a2-lm-parallel-rnetis"),
    quote("The same three numbers, to six decimal places. `ar()` and "
          "`exposure()` are instructions for building columns, not a different "
          "estimator: with no random-effects term, `fit_influence()` calls "
          "`stats::lm()`. The gain is that the instructions travel with the "
          "fit, so what counted as exposure is recoverable from the result "
          "rather than remembered from a script."),
    note("Add `(1 | group)` and the estimator becomes `lme4::lmer()`, in the "
         "same way and for the same reason — see "
         "[Several networks](#p-several-networks). The estimator line in the "
         "printed output always says which was used."),

    h3("The terms you will actually use"),
    p("Three things in that first fit are worth naming."),
    cards(
        card("`ar()` — the outcome's own prior value",
             p("With it in the model, the exposure coefficient is about "
               "*change* rather than *level*: it asks whether a node moved "
               "toward its alters, given where it started. Without it you are "
               "asking a much weaker question, and one that similarity alone "
               "will answer for you."), num=1, open=True),
        card("`exposure(y, via = tie)` — what reached the node",
             p("The alters' prior outcome carried through the `tie` network, "
               "built exactly as on the "
               "[previous page](#p-exposure). Every argument that changed the "
               "quantity there can be given here, and is recorded on the fit.")
             , num=2),
        card("The estimator line — what was used, and why",
             p("No random-effects term in the formula means ordinary least "
               "squares, and the output says so rather than leaving you to "
               "infer it. It also reports the transition it analyzed, which is "
               "the other thing a reader needs.")
             , num=3),
    ),

    h3("Ordinary covariates"),
    p("A bare covariate is read at the outcome wave, which is the usual "
      "default and also the usual mistake: a predictor measured at the same "
      "time as the outcome could be a consequence of it. `lagged()` reads an "
      "earlier wave instead, and is the more defensible choice whenever that "
      "is a live worry."),
    block("a-fit-covariates"),

    p("`centrality()` adds a node's **structural standing** in the network "
      "as a predictor — how central it is, computed from the network at "
      "the lag you name. Useful when you suspect that well-connected people "
      "differ in the outcome for reasons that have nothing to do with "
      "exposure:"),
    block("a-fit-centrality"),
    note("Keep “structural standing” and “latent position” "
         "apart in your head. A centrality is an observed, interpretable "
         "property of the recorded network. A latent position, on "
         "[page 5](#p-homophily-and-the-latent-space), is an estimated "
         "coordinate with no substantive scale at all. They look similar in a "
         "formula and mean entirely different things."),

    p("And the conduit itself can be a transformation of a tie variable "
      "rather than the raw one — see "
      "[Derived networks](#p-derived-networks):"),
    block("a-fit-derived"),

    h3("One transition at a time"),
    p("A fit analyzes one transition between two waves. With three or more "
      "waves present, rnetis will not pick one for you:"),
    refusal("a-waves-refuse"),
    p("Name the transition and it proceeds:"),
    block("a-waves-named"),
    note("Why one transition rather than the whole panel is a question about "
         "estimators, and it has the same answer on both paths. "
         "[Path B, page 1](#p-the-selection-dataset) sets it out, and "
         "[Scope](#p-scope-what-is-not-here) lists what is deliberately "
         "absent."),

    h3("The terms, in one table"),
    table(["Term", "What it adds"], [
        ["`ar(lag = 1)`", "The outcome's own prior value."],
        ["`exposure(content, via, ...)`", "Alters' characteristic through a network."],
        ["`lagged(var, lag = 1)`", "An earlier wave's value of a node variable."],
        ["`var`", "A node variable at the outcome wave."],
        ["`centrality(network, measure, ...)`", "A node's structural standing."],
        ["`network_stat(network, measure, ...)`", "A whole-network statistic."],
        ["`latent(network, d, ...)`",
         "Latent-space positions as covariates — "
         "[page 5](#p-homophily-and-the-latent-space)."],
        ["`(1 | group)`", "A random intercept per network."],
    ]),
    note("[Formula terms at a glance](#p-formula-terms-at-a-glance) has the "
         "full list with arguments, for both model families."),
)

page(
    "Path A · Influence", "4 · Read the results", "Read the results",

    p("`summary()` prints the coefficients together with what the fit is "
      "assuming to get them:"),
    block("a-summary"),

    h3("Reading this particular result"),
    quote("The exposure coefficient is 0.07 with a 95% interval from "
          "−0.38 to 0.53. The interval includes zero and a good deal "
          "either side of it. What this fit supports is: **on these 30 nodes, "
          "across this one transition, the data do not distinguish an exposure "
          "effect from no effect, and would not distinguish a fairly large one "
          "either.** It is not evidence that influence is absent."),
    p("The lagged outcome, by contrast, is estimated away from zero: these "
      "nodes' outcomes persist. That is the ordinary pattern in a two-wave "
      "study of this size, and "
      "[How precise is an exposure coefficient?](#p-how-precise-is-an-exposure-coefficient) "
      "shows what it takes to narrow the exposure interval."),

    h3("A magnitude on the outcome's scale"),
    p("A coefficient on exposure is not a magnitude anyone can read, because "
      "exposure is a constructed quantity on a scale nobody has intuitions "
      "about. `influence_effect()` converts it into a predicted change in the "
      "outcome across a stated range of exposure — by default the "
      "observed 10th to 90th percentile."),
    block("a-effect"),
    p("The interval covers uncertainty in the coefficient; the range itself is "
      "treated as fixed. Choose a different range and the magnitude moves with "
      "it, which is why the range is printed next to the number:"),
    block("a-effect-probs"),
    note("This is the number to put in a paper, with its range named. "
         "A reader can judge whether a change of that size over that range of "
         "exposure matters; they cannot judge a probit-scale or "
         "constructed-predictor coefficient at all."),

    h3("The coefficient table as data"),
    p("For a manuscript table you want the estimates as an ordinary data "
      "frame rather than printed text:"),
    block("a-estimates"),
    note("Note the `role` and `scale` columns. Every estimate says what kind "
         "of term it is and what units it is in, so a table assembled from "
         "this cannot silently mix a raw coefficient with a standardized one."),

    h3("Pictures"),
    block("a-plot-coef"),
    fig("a-plot-coef", "Coefficients with 95% intervals."),
    block("a-plot-effects"),
    fig("a-plot-effects", "The predicted change behind `influence_effect()`."),

    rare("The effect object's own fields",
         p("`influence_effect()` returns a structured object, not a sentence, "
           "so the coefficient, the contrast range it used and the predicted "
           "change are each available by name:"),
         block("a-effect-fields")),

    rare("The other one-row summaries",
         p("`netis_glance()` gives one row per fit, with the fit statistics "
           "in it, and is what [Check the fit](#p-check-the-fit) uses. "
           "`netis_estimand()` states in words what the fit is an estimate "
           "*of* — which transition, which estimator, and under what "
           "propagation."),
         block("a-glance"),
         block("a-estimand")),

    p("Two pages follow, and the order matters. The next one is about the "
      "confound that decides whether any of this is an effect at all; after "
      "that come several networks; and only then "
      "[Check the fit](#p-check-the-fit), so that it can report on every fit "
      "built by then rather than just this one."),
)

# ---------------------------------------------------------------------------
# latent() comes BEFORE several networks and before checking the fit, so that
# the fit-statistics page has the adjusted fit available to compare.
# ---------------------------------------------------------------------------
page(
    "Path A · Influence", "5 · Homophily and latent()",
    "Homophily and the latent space",

    p("Everything up to here produced an association. This page is about why "
      "it is not yet an effect, and about the one adjustment rnetis "
      "implements for it."),

    quote("If an unobserved trait makes people both choose similar friends "
          "**and** behave a certain way, an influence coefficient absorbs part "
          "of that trait. Shalizi and Thomas (2011) show that influence and "
          "homophily are generically confounded this way in observational "
          "network studies. This is not a small correction to worry about "
          "later; it is the central identification problem of the design."),

    dia(D.homophily_vs_influence(),
        "The two stories. On the left the alters' outcome reaches the ego "
        "along the tie. On the right nothing passes between them at all: an "
        "unobserved trait decides both who is tied to whom and what each "
        "person does. Both produce tied actors with similar outcomes, which "
        "is all a two-wave study sees."),

    p("Nothing in the previous pages distinguishes those two diagrams. The "
      "residual diagnostics do not, the fit statistics do not, and a narrower "
      "confidence interval would not either — a precisely estimated "
      "coefficient on the right-hand diagram is a precisely estimated "
      "confound."),

    h3("The idea of the adjustment"),
    p("Xu's (2018, 2021) observation is that if the trait drives who "
      "befriends whom, then **the network itself carries information about "
      "the trait**. Fit a model of tie formation in which each actor has a "
      "position, and actors with similar positions are likelier to be tied; "
      "those estimated positions are then a proxy for the trait, and can go "
      "into the behavioural model as ordinary covariates."),

    p("The first stage is the latent-space model of Hoff, Raftery & Handcock "
      "(2002). In Xu's (2021) formulation, the log-odds that a tie exists "
      "falls with the distance between the two actors' positions:"),
    eq('logodds(<i>Z<sub>ij</sub></i> <span class="op">=</span> 1) '
       '<span class="op">=</span> <i>α</i> <span class="op">+</span> '
       '<i>β</i>′<i>x<sub>ij</sub></i> <span class="op">−</span> '
       '|<i>c<sub>i</sub></i> <span class="op">−</span> <i>c<sub>j</sub></i>|',
       "x_ij is whatever observed selection mechanism you condition on, and "
       "c_i is actor i's position in the latent space. Xu (2021), eq. (3)."),

    dia(D.latent_space(),
        "What the first stage estimates: a coordinate per actor, arranged so "
        "that the ties you observed are the likely ones. The positions have "
        "no units and no fixed orientation — only the distances between "
        "them carry information."),

    p("The positions taken forward are the Minimum Kullback-Leibler positions "
      "of Shortreed, Handcock & Hoff (2006) — `fit$mkl$Z` in Xu's own "
      "code. They enter the influence model as actor-level covariates, so the "
      "second stage is the same model as before with extra columns:"),
    eq('<i>y<sub>i,t</sub></i> <span class="op">=</span> <i>α</i> '
       '<span class="op">+</span> <i>γ y<sub>i,t−1</sub></i> '
       '<span class="op">+</span> <i>β E<sub>i,t−1</sub></i> '
       '<span class="op">+</span> <i>c<sub>i</sub></i>′<i>λ</i> '
       '<span class="op">+</span> <i>ε<sub>i,t</sub></i>',
       "The estimated positions enter as c_i. β is still the coefficient of "
       "interest; λ is a nuisance, and is not the effect of the trait.")
    ,

    h3("Credit where it is due"),
    quote("The procedure is **Xu's**, from Xu (2018) and the worked account in "
          "Xu (2021). Xu did not publish a package for it: the article sets "
          "out the two stages and the code to run them. The first stage is "
          "fitted by **`latentnet`** (Krivitsky & Handcock 2008), which is the "
          "package Xu's own code uses and the one rnetis calls — install "
          "it, and cite it, if you use `latent()`. What rnetis adds is that "
          "the two stages are one formula term instead of a script, and that "
          "the resulting fit records what the first stage conditioned on."),

    h3("Doing it"),
    p("The first stage is a Markov chain, so this takes appreciably longer "
      "than anything else in Path A:"),
    block("c-latent"),
    p("`homophily` conditions the first stage on observed similarity through "
      "`absdiff()` terms, so this gives `latentnet::ergmm()` the right-hand "
      "side `euclidean(d = 1) + absdiff(\"y\") + absdiff(\"x\")`. `terms` "
      "adds anything else — transitivity, ego and alter effects. Neither "
      "has a default, because what belongs in the selection model is a "
      "question about your design, not a setting."),
    quote("This is the argument to think hardest about. For the positions to "
          "stand in for the *unobserved* trait, the first stage has to account "
          "for the selection mechanisms you *can* observe. Leave an observed "
          "homophily out and the positions will absorb it, and then you have "
          "controlled for something you could have measured."),

    p("Side by side with the unadjusted fit:"),
    block("c-latent-compare"),
    note("On this dataset the two are close, and both intervals comfortably "
         "include zero — so the adjustment changes no conclusion here. "
         "That is a fact about `demo_influence`, not a general result: the "
         "adjustment moves an estimate when the network carries information "
         "about the trait, and how much it moves is not something you can "
         "predict before fitting it."),

    h3("Four limits, each of which matters"),
    cards(
        card("It addresses a trait present in **both** processes",
             p("Xu (2021, p. 60) states that the adjustment does not improve "
               "the estimate when the trait is present in only one process. If "
               "the trait affects behaviour but not tie formation, or the "
               "reverse, the network carries no information about it. A fit "
               "will still produce numbers."), num=1, open=True),
        card("Positions are treated as known",
             p("They enter as ordinary covariates. rnetis does not propagate "
               "first-stage uncertainty into the second stage, and neither "
               "does Xu's procedure. It does not jointly estimate the two "
               "stages, and it does not correct for generated regressors."), num=2),
        card("A position has no substantive scale",
             p("A latent space is identified only up to rotation, reflection "
               "and translation. Actors close in position should be close in "
               "the trait, but a position coefficient is **not** the effect of "
               "the trait and should not be reported as one."), num=3),
        card("With several networks, the form is experimental",
             p("Xu's procedure is defined for one network. The multi-group "
               "form is this package's own extension of it, has no published "
               "validation, and has not been validated for statistical "
               "performance — so `fit_influence()` warns whenever it fits "
               "one. See [Beyond this path](#p-beyond-path-a)."), num=4),
    ),
)

page(
    "Path A · Influence", "6 · Several networks", "Several networks",

    p("Most designs have more than one network: several teams, classrooms, "
      "clinics. `demo_influence_groups` has 42 nodes across four separate "
      "ones. Fitting them together without saying so would pool four networks "
      "into a single intercept, so it is refused:"),
    refusal("a-mg-refuse"),

    p("A random intercept per network says what you meant, and switches the "
      "estimator from `lm()` to `lme4::lmer()` — the same substitution you "
      "would make by hand:"),
    block("a-mg"),
    p("The variance components say how much of the outcome's variation sits "
      "between networks rather than within them, which is worth knowing "
      "before you interpret anything else:"),
    block("a-mg-vc"),
    note("A predictor measured on a higher-order unit is informed by the "
         "number of *those* units. Four networks is four, however many nodes "
         "they contain."),
    block("a-plot-heterogeneity"),
    fig("a-plot-heterogeneity", "Where the variance sits, across the four networks."),

    h3("Cluster-robust standard errors"),
    p("A random intercept is one assumption about the dependence within a "
      "network. Clustered standard errors are a different one: they allow "
      "arbitrary dependence inside each network and assume independence "
      "between them. `influence_se_comparison()` sets the two side by side "
      "(Liang & Zeger 1986; Bell & McCaffrey 2002). It is a comparison, not a "
      "replacement:"),
    block("a-se"),
    p("Where the two disagree, that is information: it says the model's "
      "variance assumption is doing work. And the function states what it "
      "does and does not address rather than leaving you to assume:"),
    block("a-se-addresses"),

    p("`by = \"group\"` needs several networks, because independence between "
      "them is the thing it assumes, so it is refused on one:"),
    refusal("a-se-refuse"),
    note("`by = \"node\"` clusters on the actor instead. It is a different "
         "correction, is never network-robust, and is refused on a mixed fit. "
         "The `addresses` attribute says so in each case."),

    h3("Is a group mean worth modelling?"),
    p("Where a construct is aggregated to the group — a team's average "
      "climate, say — ask whether the group mean is reliable before you "
      "model it, or you are modelling noise. `icc1()` is the one-way ANOVA "
      "ICC(1) of Bliese (2000) on the rows the fit used; `icc2()` is the "
      "reliability of the group mean."),
    block("a-icc"),

    rare("Rater agreement, and an eta-squared that is positive under no effect",
         p("For agreement among raters on a scale, `rwg()` and `rwg_j()` "
           "implement James, Demaree & Wolf (1984). `rwg()` takes one item, "
           "`rwg_j()` a matrix of them. The null distribution decides the "
           "answer, so it has no default:"),
         block("a-rwg"),
         block("a-rwg-j"),
         p("With at least five networks, `netis_dependence()` also reports "
           "`residual_eta_squared`, the between-group sum of squares of the "
           "residuals over the total. It is above zero in expectation with no "
           "group effect whatsoever — `(J - 1) / (N - 1)` for `J` groups "
           "and `N` rows, and larger when groups are small (Bliese & Halverson "
           "1998). Compare it against that, never against zero.")),
)

page(
    "Path A · Influence", "7 · Check the fit", "Check the fit",

    p("This page comes last in the path on purpose: by now there are several "
      "fits to compare — the plain one, the latent-adjusted one, the "
      "multi-group one — and most of what follows is only useful "
      "comparatively."),

    h3("The fit statistics"),
    p("`netis_glance()` returns one row per fit, and the fit statistics are "
      "in it:"),
    block("a2-glance"),
    table(["Field", "What it is"], [
        ["`r.squared`", "Proportion of the outcome's variance the model accounts "
         "for. A genuine R-squared here, because the estimator is least squares."],
        ["`sigma`", "The residual standard deviation, on the outcome's own scale."],
        ["`rmse`", "Root mean squared error on the fitted rows — in-sample, "
         "so it is a description of this fit, not a prediction error."],
        ["`nobs`", "Rows the fit actually used. Compare it with the rows you "
         "expected: the difference is what was dropped."],
        ["`estimator`, `backend`", "What was used. `pooled` with backend `stats` "
         "is `lm()`."],
    ]),

    p("A Gaussian Influence fit is a likelihood model, so the information "
      "criteria are defined and reported:"),
    block("a2-ic"),

    h3("Comparing models"),
    p("An information criterion is only meaningful between models fitted to "
      "**the same rows**. These two are, so the comparison is admissible:"),
    block("a2-compare-ic"),
    quote("Adding exposure raised AIC by about 1.9 and BIC by about 3.3, and "
          "moved R-squared by 0.003. Both criteria prefer the model without "
          "it. That agrees with the coefficient interval from "
          "[page 4](#p-read-the-results) and it is the honest summary of this "
          "dataset: nothing here supports an exposure effect. It is **not** "
          "evidence that there is none — see "
          "[How precise is an exposure coefficient?](#p-how-precise-is-an-exposure-coefficient)."),
    note("Two traps. A latent-adjusted fit drops any row whose actor has no "
         "estimated position, so its `nobs` can differ and an AIC comparison "
         "against the unadjusted fit is then invalid — check `nobs` first. "
         "And a mixed fit's criteria come from `lme4`, are on a different "
         "likelihood, and are never comparable with an `lm()` fit's."),

    p("`pseudo_r2()` is the one to read the help page for before you quote "
      "it. On an OLS fit it reports the ordinary R-squared under its own "
      "name; on a mixed or probit fit the same function reports several "
      "quantities that answer different questions, and none of them is "
      "“the” R-squared:"),
    block("a2-pseudo"),

    h3("Residual dependence with respect to the network"),
    p("The question a fit statistic cannot answer: are the residuals still "
      "patterned along the network? `netis_dependence()` needs the data as "
      "well as the fit, because it rebuilds the network from it:"),
    block("a-dependence"),
    p("Read the second sentence of the Moran-type line carefully. The "
      "statistic is reported **without a reference distribution**, because the "
      "usual analytical moments are not derived for a directed `W`. There is "
      "no p-value to read, and the quantity is not confined to "
      "`[−1, 1]`. It is a described quantity, to be compared across "
      "specifications rather than against a threshold:"),
    block("a-plot-dependence"),
    fig("a-plot-dependence", "Residual dependence, reported as described quantities."),

    h3("The assumption ledger"),
    p("`netis_assumptions()` lists what a fit takes for granted, with its "
      "source and whether anything in the fit addresses it. It is the "
      "package's own construction rather than a published checklist, and it "
      "is long — the point of it is that nothing on the list is "
      "invisible."),
    block("a-assumptions"),
    p("Most rows read `unaddressed`. That is a statement about the design, not "
      "about your study: the dynamic-panel estimators that would address "
      "sequential exogeneity and persistent actor heterogeneity need more "
      "than the one transition a fit analyzes, so none of the package's "
      "estimators has a device for them. See "
      "[Scope](#p-scope-what-is-not-here)."),
    p("Two rows are yours to declare. A declaration changes what the fit "
      "**reports**; it does not change a single estimate:"),
    block("a-declared"),
    note("`all.equal(coef(fit), coef(declared))` is `TRUE`. Declaring the "
         "network exogenous does not make it so and moves no number; it "
         "records that the claim is yours, so a reader knows who made it."),

    h3("What none of this establishes"),
    p("No surface here tells you the model is right, or that an association "
      "is influence. Homophilous selection on an unobserved trait produces "
      "the same pattern — that is [page 5](#p-homophily-and-the-latent-space) "
      "— and no statistic on this page distinguishes them. Together these "
      "tools tell you how much variation the model accounts for, whether "
      "adding a term improved the fit on the same rows, whether the residuals "
      "are still patterned along the network, what a different variance "
      "assumption does to the standard errors, and what the fit assumed. "
      "The step from “no adverse evidence” to “the assumption "
      "holds” is yours."),
)

page(
    "Path A · Influence", "8 · Beyond this path", "Beyond Path A",

    p("The path you have just finished is the supported route: build the data, "
      "construct exposure deliberately, fit one transition, report a magnitude "
      "on the outcome's scale, adjust for latent homophily, and check what the "
      "fit assumed. This page is the rest of the Influence side — what it "
      "can also do, and where the ground is less firm."),

    h3("Marked experimental"),
    cards(
        card("Multi-group latent() — experimental, and it warns",
             p("Xu's procedure is defined for one network. rnetis will fit it "
               "across several, and warns every time, because this form is the "
               "package's own extension with no published validation."),
             block("c-latent-mg"),
             p("Each group is fitted separately in the first stage, its "
               "positions centred on the group's mean, entering as that "
               "group's own columns and zero elsewhere. One model across "
               "networks still imposes one shared exposure coefficient, one "
               "residual variance, the same `d` and the same stage-one "
               "specification, and position coefficients that are per group "
               "and not comparable across groups."),
             quote("**Separate per-network fits are the recommended primary "
                   "analysis**, with a multi-group fit as something to compare "
                   "against them — not the other way round."),
             step="experimental", num=1, open=True),
        card("The latent-scale variance share",
             p("`pseudo_r2()` on a probit or mixed fit reports quantities that "
               "are the package's own constructions, and they say so in their "
               "own output. Read `?pseudo_r2` before any of them goes in a "
               "manuscript."),
             step="package's own", num=2),
    ),

    h3("Also available, and ordinary"),
    table(["You want", "Use", "Where"], [
        ["Influence along longer, discounted paths",
         "`bi()`, `bi_components()`, `bi_sweep()`",
         "Frank & Fahrbach (1999); `?bi`"],
        ["A whole-network statistic as a predictor", "`network_stat()`",
         "[Formula terms](#p-formula-terms-at-a-glance)"],
        ["A transformed conduit — binarized, symmetrized, multiplex",
         "`binarize()`, `symmetrize()`, `multiplex()`",
         "[Derived networks](#p-derived-networks)"],
        ["Centred, or within/between split, predictors",
         "`add_centering()`, `add_decomposition()`",
         "[Derived variables](#p-derived-variables)"],
        ["A network-autocorrelation (SAR) estimator",
         "`fit_influence(estimator = \"sar\")`",
         "needs `sna` and `numDeriv`"],
        ["Data generated from a known influence process", "`simulate_influence()`",
         "[Precision](#p-how-precise-is-an-exposure-coefficient)"],
    ]),
    note("[The rest of the package](#p-the-rest-of-the-package) is the full "
         "appendix, for both paths."),

    h3("Where Path A ends"),
    p("You can now build a data object from your own data frames, construct "
      "exposure as the quantity you actually mean, fit a two-wave influence "
      "model, report a magnitude on the outcome's scale, apply one published "
      "adjustment for the confound that threatens it, handle several networks, "
      "and read the fit statistics against a comparison that is admissible. "
      "What you cannot do, from this design, is call the result an effect of "
      "influence without an argument that comes from outside the fit."),
    note("[Path B](#p-selection-the-question) models the other side of the "
         "same coin: not how behaviour follows ties, but how ties follow "
         "attributes."),
)
