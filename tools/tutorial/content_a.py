# -*- coding: utf-8 -*-
"""Welcome, getting started, the data object, and the Influence path."""

from build import (
    page, p, h3, h4, ul, ol, quote, note, table, code, result, block,
    refusal, codeonly, fig, card, cards, rare, filterbox,
)

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

    p("This tutorial works each one end to end on data that ships with the "
      "package. You will build a data object, construct network exposure, fit "
      "a model, read the coefficients on a scale a reader can interpret, and "
      "then ask what the fit took for granted. Both paths are self-contained, "
      "so you can start with whichever matches your question."),

    '<div class="pathbox">'
    '<div><h4>Path A · Influence</h4>'
    "<p>Does a person's outcome move toward the outcomes of the people they "
    "are tied to? Six pages: exposure, the fit, reading the result, checking "
    "it, several networks at once, and the homophily problem.</p>"
    '<p><a href="#p-influence-the-question">Start Path A →</a></p></div>'
    '<div><h4>Path B · Selection</h4>'
    "<p>What predicts a tie? Five pages: the role grammar, the fit and its "
    "Markov chain, probability-scale effect sizes, several groups, and one "
    "hierarchical model across all of them.</p>"
    '<p><a href="#p-selection-the-question">Start Path B →</a></p></div>'
    "</div>",

    h3("What you need"),
    ul("**R 4.1 or later**, and the rnetis package. [Install rnetis](#p-install-rnetis) "
       "covers both, and the optional packages each capability needs.",
       "No compiler: rnetis is pure R.",
       "No data of your own. Every example uses `demo_influence`, "
       "`demo_selection` or `demo_influence_groups`, which are synthetic and "
       "ship with the package."),

    h3("How long it takes"),
    p("Path A runs in seconds end to end; it is ordinary least squares and a "
      "mixed model, apart from the latent-space adjustment on its last page. "
      "Path B fits Markov chains through `amen::ame()`: at the settings used "
      "here a single-group fit took about fifteen seconds on the machine this "
      "page was built on, and the hierarchical model on the last page took "
      "about eight minutes. Reading carefully, rather than waiting for R, is "
      "what will take you an afternoon."),

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
      "[derived networks](#p-derived-networks), [what rnetis refuses](#p-what-rnetis-refuses) "
      "and [what is deliberately not here](#p-scope-what-is-not-here). "
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
         "[Path A, page 6](#p-homophily-and-latent)"],
        ['`install.packages("RSiena")`', "the `s50` data of Xu's worked example",
         "Nothing in this tutorial"],
        ['`install.packages(c("sna", "numDeriv"))`',
         "the network-autocorrelation (SAR) estimator", "Nothing in this tutorial"],
        ['`install.packages("clubSandwich")`', "cluster-robust standard errors",
         "[Path A, page 5](#p-several-networks)"],
        ['`install.packages(c("igraph", "network"))`',
         "importing from and exporting to `igraph` and `statnet`",
         "[Importing and exporting](#p-importing-and-exporting)"],
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
    code("d-first"),
    p("Directly underneath, in a quieter box labelled **Result**, is what R "
      "printed when that code was run:"),
    result("d-first"),

    h3("Refusals are shown on purpose"),
    p("rnetis refuses more often than most R packages, and a refusal is "
      "usually the most informative thing it will tell you. Where a block is "
      "meant to fail, its result box is labelled **rnetis refuses** and the "
      "message is reproduced in full. Read those messages: each one names the "
      "decision it is asking you to make, and most of them name the argument "
      "that makes it."),
    refusal("b-bare-refuse"),
    note("That is the real message, not a paraphrase. "
         "[What rnetis refuses](#p-what-rnetis-refuses) collects them in one place."),

    h3("Conventions"),
    table(["You see", "It means"], [
        ["`demo_influence`", "A synthetic dataset that ships with the package. "
         "Nothing here uses real data."],
        ["A **Result** box", "Captured from a live R session running the code above it."],
        ["A **rnetis refuses** box", "The code above it is *supposed* to fail; the "
         "message is the lesson."],
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
# The data object
# ===========================================================================
page(
    "Getting started", "The data object", "The data object",

    p("Every model in rnetis reads one structure, `netis_data`: a table of "
      "nodes, a table of ties, and optionally a table of groups. Build it "
      "once and both paths of this tutorial will read it."),

    h3("Nodes and ties"),
    p("Each row of the node table is a node; each row of the tie table is a "
      "tie. `group` names the network, `node` names the node, and `source` and "
      "`target` name the ends of a tie. Everything else is a variable."),
    block("d-first"),
    p("The printed summary is worth reading rather than skipping. It reports "
      "what rnetis *inferred* about each tie variable and marks it as "
      "inferred — here, that `w` is weighted, and that a row's presence is "
      "what encodes a tie."),

    p("The two tables come back out as they went in:"),
    block("d-accessors"),

    h3("Repeated observation"),
    p("Add a wave column to both tables and name it with `wave_col`. A node "
      "variable that varies by wave becomes dynamic; one that does not stays "
      "static, and rnetis works out which is which."),
    block("d-waves"),
    p("Wave *positions* are what the models use. Metric time, if you have it, "
      "is metadata carried alongside:"),
    block("d-time-index"),

    h3("Four questions about every tie variable"),
    p("A network of counts and a network of yes/no answers are different "
      "objects, and a zero in one does not mean what a zero in the other "
      "means. rnetis will infer answers where it can, and it records that it "
      "inferred them:"),
    block("d-boundary-inferred"),

    p("Declaring them yourself replaces the inference with your statement. "
      "This is the single most useful thing you can do to a dataset before "
      "modelling it:"),
    block("d-declare"),

    table(["Declaration", "The question it answers"], [
        ["`weighted`", "Is a larger value a stronger tie, or is this yes/no?"],
        ["`weight_semantics`", "Is the weight a count, a strength, or a distance? "
         "A measure that reads weights as distances refuses a network declared "
         "as strengths unless you transform it."],
        ["`zero_meaning`", "Is a zero a measured absence, or a cell nobody asked about? "
         "`measured_zero` says the absence is data."],
        ["`direction_semantics`", "What does the arrow mean for this relation?"],
    ]),

    rare("Why presence mode matters",
         p("`presence_mode` is how rnetis knows whether a dyad that has no row "
           "is a measured zero or an unobserved cell. `row_presence` means "
           "only ties present in the table exist; `complete_dyadic` means "
           "every ordered pair was observed and the absent ones are genuine "
           "zeros. `demo_selection` is complete-dyadic, which is why it has "
           "380 tie rows for 20 nodes — every ordered pair, zeros "
           "included. Getting this wrong changes every density, every "
           "degree and every exposure in the analysis, which is why rnetis "
           "says which one it assumed.")),
)

page(
    "Getting started", "Importing and exporting", "Importing and exporting",

    p("If your data is already in another shape, an importer builds the same "
      "object from it, and records anything the conversion could not carry."),

    table(["You have", "Use"], [
        ["An adjacency matrix", "`netis_data_from_adjacency()`"],
        ["One adjacency matrix per wave", "`netis_data_from_adjacency_series()`"],
        ["An edge list", "`netis_data_from_edgelist()`"],
        ["An `igraph` graph, or a list of them",
         "`netis_data_from_igraph()`, `netis_data_from_igraph_list()`"],
        ["A `network` object", "`netis_data_from_network()`"],
        ["Separate static and dynamic tables", "`netis_data_from_split()`"],
    ]),

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
          "They usually are, and it tells you very little. The question is "
          "whether a node moves **toward** its alters between one occasion and "
          "the next, holding where it started."),

    h3("The data"),
    block("d-demo"),
    p("Thirty nodes in one network, observed twice, with an outcome `y`, a "
      "covariate `x`, and a directed relation `tie` that does not change "
      "between waves."),

    h3("The path from here"),
    ol("**[Exposure](#p-exposure)** — what reaches a node through its ties, "
       "and the two ways of summarising it that answer different questions.",
       "**[Fit the model](#p-fit-the-model)** — the formula grammar: `ar()`, "
       "`exposure()`, lags, centralities and derived networks.",
       "**[Read the results](#p-read-the-results)** — the coefficient, and a "
       "magnitude on the outcome's own scale.",
       "**[Check the fit](#p-check-the-fit)** — what it assumed, and what the "
       "residuals look like with respect to the network.",
       "**[Several networks](#p-several-networks)** — random intercepts, "
       "cluster-robust standard errors, and whether a group mean is reliable.",
       "**[Homophily and latent()](#p-homophily-and-latent)** — the confound "
       "that makes all of the above an association rather than an effect, and "
       "one published adjustment for it."),
)

page(
    "Path A · Influence", "1 · Exposure", "Exposure",

    p("Exposure is how much of a characteristic reaches a node through its "
      "ties. The default is the weighted sum of Friedkin & Johnsen (1990) and "
      "Marsden & Friedkin (1993):"),
    '<p style="text-align:center;font-size:17px"><em>E<sub>i</sub></em> = '
    'Σ<sub><em>j</em></sub> <em>W<sub>ij</sub> X<sub>j</sub></em></p>',
    p("`W[i, j]` is the strength with which `j` influences `i`. By default a "
      "row with `source = \"b\"` and `target = \"a\"` means **b influences a**; "
      "the `direction` argument reverses that reading, and nothing else in the "
      "package transposes a network behind your back."),

    p("Take the three-node object from "
      "[The data object](#p-the-data-object): `b` sends to `a` with weight 1, "
      "`c` sends to `a` with weight 3, and `x` is 10, 20, 30."),
    block("a-exposure-sum"),
    p("`a` receives from `b` with weight 1 and from `c` with weight 3, so its "
      "exposure is `1 × 20 + 3 × 30 = 110`. `b` and `c` receive from "
      "nobody, and get 0."),

    h3("Sum or average — a choice, not a default"),
    p("Row normalization divides by the node's total tie weight (Leenders "
      "2002), giving the **average** of a node's alters rather than the total:"),
    block("a-exposure-row"),

    p("`a` is now `110 / 4 = 27.5`. The two answer different questions, and "
      "the difference is not cosmetic:"),
    table(["", "`normalize = \"none\"` (default)", "`normalize = \"row\"`"], [
        ["Quantity", "Total inflow", "Average alter"],
        ["Rises with the number of ties", "Yes", "No"],
        ["A node with twice as many alters",
         "Has twice the exposure, other things equal",
         "Has the same exposure, other things equal"],
        ["Suits", "A dose that accumulates", "A norm the node is being pulled toward"],
    ]),

    h3("A node with no incoming ties"),
    p("Under row normalization, a node with no alters has no average to take. "
      "rnetis makes it `NA` by default rather than quietly calling it zero, "
      "which would place the isolate at the origin of the scale:"),
    block("a-exposure-iso"),
    note("`isolate_rule` takes `\"na\"` (the default), `\"zero\"` if a zero is "
         "genuinely what an isolate should get, or `\"error\"` to stop."),

    h3("Reversing the reading"),
    block("a-exposure-direction"),
    p("Now the arrow is read the other way: `a` influences nobody's "
      "neighbours, and `b` and `c` each receive from `a`."),

    h3("On the demo data"),
    block("a-exposure-demo"),

    rare("Influence along paths, across waves",
         p("Exposure through a single step of the network is the common case "
           "and the one this tutorial uses. Influence that travels along "
           "longer paths, with each step discounted, is the "
           "balance-information mechanism of Frank & Fahrbach (1999): "
           "`bi(delta, alpha, gamma)`, with `bi_components()` and `bi_sweep()` "
           "alongside it. See `?bi`.")),
)

page(
    "Path A · Influence", "2 · Fit the model", "Fit the model",

    p("`fit_influence()` takes a formula and a data object. The outcome on the "
      "left is a node variable; the terms on the right each name their role."),
    block("a-fit"),

    p("Three things in that output are worth naming:"),
    ul("**`ar()`** is the outcome's own prior value. With it in the model the "
       "exposure coefficient is about *change* rather than *level*: it asks "
       "whether a node moved toward its alters, given where it started. "
       "Without it, you are asking a much weaker question.",
       "**`exposure(y, via = tie)`** is the alters' prior outcome carried "
       "through the `tie` network.",
       "**The estimator line** says what was used and why. No random-effects "
       "term in the formula means ordinary least squares, and it says so."),

    h3("The other terms"),
    p("A bare covariate is read at the outcome wave; `lagged()` reads an "
      "earlier one. Both are ordinary node-level predictors."),
    block("a-fit-covariates"),

    p("`centrality()` puts a node's position in the model as a predictor, "
      "computed from the network at the lag you name:"),
    block("a-fit-centrality"),

    p("And the conduit itself can be a transformation of a tie variable rather "
      "than the raw one — see [Derived networks](#p-derived-networks):"),
    block("a-fit-derived"),

    h3("Two waves at a time"),
    p("A fit analyzes one transition between two waves. With three or more "
      "waves present, rnetis will not pick one for you:"),
    refusal("a-waves-refuse"),
    p("Name the transition and it proceeds:"),
    block("a-waves-named"),

    h3("The terms, in one table"),
    table(["Term", "What it adds"], [
        ["`ar(lag = 1)`", "The outcome's own prior value."],
        ["`exposure(content, via, ...)`", "Alters' characteristic through a network."],
        ["`lagged(var, lag = 1)`", "An earlier wave's value of a node variable."],
        ["`var`", "A node variable at the outcome wave."],
        ["`centrality(network, measure, ...)`", "A node's position in a network."],
        ["`network_stat(network, measure, ...)`", "A whole-network statistic."],
        ["`latent(network, d, ...)`",
         "Latent-space positions as covariates — "
         "[page 6](#p-homophily-and-latent)."],
        ["`(1 | group)`", "A random intercept per network."],
    ]),
    note("[Formula terms at a glance](#p-formula-terms-at-a-glance) has the "
         "full list with arguments, for both model families."),
)

page(
    "Path A · Influence", "3 · Read the results", "Read the results",

    p("`summary()` prints the coefficients together with what the fit is "
      "assuming to get them:"),
    block("a-summary"),

    p("The coefficient table is also available as an ordinary data frame, "
      "which is what you want for a manuscript table:"),
    block("a-estimates"),
    p("Note the `role` and `scale` columns. Every estimate says what kind of "
      "term it is and what units it is in, so a table assembled from this "
      "cannot silently mix a raw coefficient with a standardized one."),

    h3("One row per fit"),
    block("a-glance"),
    block("a-estimand"),

    h3("A magnitude on the outcome's scale"),
    p("A coefficient on exposure is not a magnitude anyone can read. "
      "`influence_effect()` converts it into a predicted change in the "
      "outcome across a stated range of exposure — by default the "
      "observed 10th to 90th percentile."),
    block("a-effect"),
    block("a-effect-fields"),
    p("The interval covers uncertainty in the coefficient; the range itself is "
      "treated as fixed. Choose a different range and the magnitude moves with "
      "it, which is why the range is printed next to the number:"),
    block("a-effect-probs"),

    h3("Explained variation"),
    block("a-pseudo"),
    note("Read `?pseudo_r2` before quoting any of it. For a mixed or probit "
         "fit the same function reports several different quantities, and they "
         "answer different questions."),

    h3("Pictures"),
    block("a-plot-coef"),
    fig("a-plot-coef", "Coefficients with 95% intervals."),
    block("a-plot-effects"),
    fig("a-plot-effects", "The predicted change behind `influence_effect()`."),

    h3("Reading this particular result"),
    quote("The exposure coefficient is 0.07 with a 95% interval from "
          "−0.38 to 0.53. The interval includes zero and a good deal "
          "either side of it. What this fit supports is: **on these 30 nodes, "
          "across this one transition, the data do not distinguish an exposure "
          "effect from no effect, and would not distinguish a fairly large one "
          "either.** It is not evidence that influence is absent."),
    p("The lagged outcome, by contrast, is estimated away from zero: these "
      "nodes' outcomes persist. That is the ordinary pattern in a two-wave "
      "study of this size, and [How precise is an exposure coefficient?]"
      "(#p-how-precise-is-an-exposure-coefficient) shows what it takes to "
      "narrow the exposure interval."),
    p("The next page asks the other question: what did this fit have to assume "
      "to produce those numbers at all?"),
)

page(
    "Path A · Influence", "4 · Check the fit", "Check the fit",

    p("rnetis keeps a ledger of what a fit takes for granted. It is the "
      "package's own construction, not a published checklist, and it is "
      "deliberately uncomfortable reading."),
    block("a-assumptions"),

    p("Each row answers three questions: whether the assumption is maintained "
      "by the model, by an rnetis default, or by your own declaration; whether "
      "anything in the fit addresses it; and whether any statistic from a fit "
      "could bear on it at all."),
    table(["Field", "Question it answers"], [
        ["`source`", "Is this the model's, rnetis's default, or your declaration?"],
        ["`handling`", "Does anything in the fit address it?"],
        ["`diagnostic_evidence`", "`\"not_testable\"`, with the reason, where no "
         "statistic from a fit can bear on the row; otherwise `\"none\"`."],
    ]),
    p("Most rows read `unaddressed`, and that is honest rather than "
      "apologetic. The dynamic-panel estimators that would address sequential "
      "exogeneity and persistent actor heterogeneity need more than the one "
      "transition a fit analyzes, so none of the package's estimators has a "
      "device for them. See [what is deliberately not here](#p-scope-what-is-not-here)."),

    h3("Two things are yours to declare"),
    p("A declaration changes what the fit *reports*, and no estimate:"),
    block("a-declared"),
    note("`all.equal(coef(fit), coef(declared))` is `TRUE`. Declaring the "
         "network exogenous does not make it so and does not move a single "
         "number; it records that the claim is yours, so a reader knows who "
         "made it."),

    h3("Residual dependence"),
    p("`netis_dependence()` needs the data as well as the fit, because it "
      "rebuilds the network from the data:"),
    block("a-dependence"),
    p("Read the second sentence of the Moran-type line carefully. The "
      "statistic is reported **without a reference distribution**, because the "
      "usual analytical moments are not derived for a directed `W`. There is "
      "no p-value to read, and the quantity is not confined to "
      "`[−1, 1]`."),
    block("a-plot-dependence"),
    fig("a-plot-dependence", "Residual dependence, reported as described quantities."),

    h3("What none of this establishes"),
    p("No surface here tells you the model is right, or that an association is "
      "influence. Homophilous selection on an unobserved trait produces the "
      "same pattern, and no diagnostic on this page distinguishes them. "
      "Together these tools tell you:"),
    ul("what the fit takes for granted, and which of those nothing addresses;",
       "what the residuals look like with respect to the network and the "
       "groups, as numbers without a reference distribution;",
       "what a different variance assumption does to the standard errors "
       "— the [next page](#p-several-networks);",
       "whether a group-level aggregate is reliable enough to model."),
    p("The step from “no adverse evidence” to “the assumption "
      "holds” is yours, and [page 6](#p-homophily-and-latent) is about the "
      "assumption that usually matters most."),
)

page(
    "Path A · Influence", "5 · Several networks", "Several networks",

    p("`demo_influence_groups` has 42 nodes across four separate networks. "
      "Fitting them together without saying so would pool four networks into "
      "one intercept, so it is refused:"),
    refusal("a-mg-refuse"),

    p("A random intercept per network says what you meant, and switches the "
      "estimator to a mixed model:"),
    block("a-mg"),
    block("a-mg-vc"),
    note("A predictor measured on a higher-order unit is informed by the "
         "number of *those* units. Four networks is four, however many nodes "
         "they contain."),

    h3("Cluster-robust standard errors"),
    p("`influence_se_comparison()` sets the model's standard errors beside "
      "clustered ones (Liang & Zeger 1986; Bell & McCaffrey 2002). It is a "
      "comparison, not a replacement:"),
    block("a-se"),
    p("And it states what it does and does not address:"),
    block("a-se-addresses"),

    p("`by = \"group\"` permits arbitrary residual dependence within each "
      "network and assumes independence between them, so it needs several "
      "networks and is refused on one:"),
    refusal("a-se-refuse"),
    note("`by = \"node\"` clusters on the actor instead. It is a different "
         "correction, is never network-robust, and is refused on a mixed fit. "
         "The `addresses` attribute says so in each case."),

    h3("Is a group mean worth modelling?"),
    p("Where a construct is aggregated to the group, ask whether the group "
      "mean is reliable before you model it. `icc1()` is the one-way ANOVA "
      "ICC(1) of Bliese (2000) on the rows the fit used; `icc2()` is the "
      "reliability of the group mean."),
    block("a-icc"),

    p("For rater agreement on a scale, `rwg()` and `rwg_j()` implement James, "
      "Demaree & Wolf (1984). `rwg()` takes one item, `rwg_j()` a matrix of "
      "them. The null distribution decides the answer, so it has no default:"),
    block("a-rwg"),
    block("a-rwg-j"),

    h3("Between-group variation, drawn"),
    block("a-plot-heterogeneity"),
    fig("a-plot-heterogeneity", "Where the variance sits, across the four networks."),

    rare("An eta-squared that is positive under no group effect at all",
         p("With at least five networks, `netis_dependence()` also reports "
           "`residual_eta_squared`, the between-group sum of squares of the "
           "residuals over the total. It is above zero in expectation with no "
           "group effect whatsoever — `(J - 1) / (N - 1)` for `J` groups "
           "and `N` rows, and larger when groups are small (Bliese & Halverson "
           "1998). Compare it against that, never against zero.")),
)

page(
    "Path A · Influence", "6 · Homophily and latent()", "Homophily and latent()",

    p("Everything up to here produced an association. This page is about why "
      "it is not yet an effect."),

    quote("If an unobserved trait makes people both choose similar friends "
          "**and** behave a certain way, an influence coefficient absorbs part "
          "of that trait. Shalizi and Thomas (2011) show that influence and "
          "homophily are generically confounded this way in observational "
          "network studies. This is not a small correction to worry about "
          "later; it is the central identification problem of the design."),

    h3("The idea of the adjustment"),
    p("If the trait drives who befriends whom, then the network itself carries "
      "information about the trait. `latent()` implements the two-stage "
      "adjustment Xu (2018, 2021) proposes:"),
    ol("Fit a latent-space model of tie formation (Hoff, Raftery & Handcock "
       "2002), conditioning on the selection mechanisms you *can* observe, so "
       "that each actor gets a position and closer actors are likelier to be "
       "tied.",
       "Put the estimated positions — the Minimum Kullback-Leibler "
       "positions of Shortreed, Handcock & Hoff (2006) — into the "
       "influence model as ordinary actor-level covariates."),

    h3("Doing it"),
    p("The first stage is a Markov chain, so this takes appreciably longer "
      "than anything else in Path A:"),
    block("c-latent"),
    p("`homophily` conditions the first stage on observed similarity through "
      "`absdiff()` terms, so this gives `latentnet::ergmm()` (Krivitsky & "
      "Handcock 2008) the right-hand side "
      "`euclidean(d = 1) + absdiff(\"y\") + absdiff(\"x\")`. `terms` adds "
      "anything else — transitivity, ego and alter effects. Neither has a "
      "default, because what belongs in the selection model is a question "
      "about your design, not a setting."),

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
               "one."),
             block("c-latent-mg"),
             p("Each group is fitted separately in the first stage, its "
               "positions centred on the group's mean, entering as that "
               "group's own columns and zero elsewhere. One model across "
               "networks still imposes one shared exposure coefficient, one "
               "residual variance, the same `d` and the same stage-one "
               "specification, and position coefficients that are per group "
               "and not comparable across groups. "
               "**Separate per-network fits are the recommended primary "
               "analysis**, with a multi-group fit as something to compare "
               "against them."), num=4),
    ),

    h3("Where Path A ends"),
    p("You can now build a data object, construct exposure as the quantity you "
      "actually mean, fit a two-wave influence model, report a magnitude on "
      "the outcome's scale, say what the fit assumed, and apply one published "
      "adjustment for the confound that threatens it. What you cannot do, from "
      "this design, is call the result an effect of influence without an "
      "argument that comes from outside the fit."),
    note("[Path B](#p-selection-the-question) models the other side of the same "
         "coin: not how behaviour follows ties, but how ties follow attributes."),
)
