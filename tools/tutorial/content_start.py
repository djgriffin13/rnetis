"""Welcome and Getting started.

SHORT ON PURPOSE. These pages exist to get a reader to the first model, which
is the first page of the Influence sequence. Anything that is not needed to get
there is either behind an expandable box or on a reference page at the end.
"""

from build import block, codeonly, h3, note, p, page, quote, rare, table, ul

GS = "Getting started"

page(
    None, "Welcome", "Welcome",
    '<img class="hero" src="logo.png" alt="rnetis">',
    '<p class="lede">rnetis fits two families of social network model from one '
    "data object and one formula: <strong>Influence</strong>, where a person "
    "changes with the people around them, and <strong>Selection</strong>, "
    "where a tie forms or persists depending on who the two people are.</p>",

    p("This tutorial gets you to a fitted, interpreted model in a few minutes, "
      "and then goes back over how it worked. Both halves use one running "
      "example: **how people's mood spreads through a team, and who comes to "
      "confide in whom.**"),

    '<div class="pathbox">'
    '<div><h4>Influence</h4>'
    "<p>Does a person's affect move toward the affect of the people who "
    "confide in them? Eight short pages: run a model, the theory, the data, "
    "the formula, reading the result, plotting it, then the latent-space and "
    "multilevel versions.</p>"
    '<p><a href="#p-influence-run-a-model-first">Start Influence →</a></p></div>'
    '<div><h4>Selection</h4>'
    "<p>Who comes to confide in whom? The same team and the same data, with "
    "the tie as the outcome.</p>"
    '<p><a href="#p-selection-the-question">Start Selection →</a></p></div>'
    "</div>",

    p("New here? [Installation](#p-installation) takes a couple of minutes, "
      "and the [overview](#p-overview) says how the pages are laid out."),

    h3("Please read this first"),
    quote("**This page is shared for a workshop, and the software is not yet "
          "public.** rnetis is proprietary software owned by Griffin "
          "Dynamics LLC. The full version is under review and will be "
          "released publicly in due course."),
    p("You are welcome to run the code on your own data, for your own work. "
      "Please do **not** redistribute the software, this page, or the "
      "materials that came with them, and please do not pass them on to "
      "anyone who was not given access directly."),
    p("If you would like to use rnetis in a project, would like to be told "
      "when it is released, or want to talk about a collaboration, write to "
      "**Daniel Griffin** at "
      "[danieljgriffin618@gmail.com](mailto:danieljgriffin618@gmail.com)."),
)

page(
    GS, "Overview", "Overview",
    '<p class="lede">'
    "This page introduces the two questions the tutorial asks about one team, and explains how the pages are laid out.</p>",

    h3("Two questions about the same team"),
    p("Suppose the people on a team who confide in each other end up in "
      "similar moods. There are two ways that can happen, and they are "
      "different claims:"),
    ul("**Influence.** Mood spreads. Your affect moves toward the affect of "
       "the people who confide in you. The thing that changes is a "
       "characteristic of a *person*.",
       "**Selection.** People choose. You come to confide in people whose "
       "mood already resembles yours. The thing that changes is a "
       "*relationship*."),
    p("rnetis fits both from the same data object, with the same kind of "
      "formula. You only need one of them to get started, and each sequence "
      "stands on its own."),

    h3("How every page works"),
    ul("**The code comes first.** Each page opens by running something, and "
       "shows what R printed. Then it explains what you just ran, one piece "
       "at a time.",
       "**The detail is one click away.** Grey lines that start with a "
       "triangle open a box: every function used on the page, other settings "
       "and what they change, the equation. Skip them on a first read. They "
       "are there when you go looking.",
       "**Every result is real.** The block under each piece of code is what "
       "R printed when that exact code ran. Nothing was typed to look like "
       "output."),

    h3("The two sequences"),
    table(["", "Influence", "Selection"], [
        ["1", "Run a model, and read it", "Run a model, and read it"],
        ["2", "The theory and the hypothesis", "The theory and the hypothesis"],
        ["3", "The data", "The data (the same data)"],
        ["4", "Running the model", "Running the model"],
        ["5", "Interpreting the model", "Interpreting the model"],
        ["6", "Visualizing the model", "Visualizing the model"],
        ["7", "The latent space", "Multiplicative effects"],
        ["8", "Several teams", "Several teams"],
    ]),
    note("The Influence sequence is in its new form. The Selection pages are "
         "being rewritten to the same plan and still show the earlier layout."),

    h3("What you need"),
    ul("**R 4.2 or later** and the rnetis package: see "
       "[installation](#p-installation).",
       "No data of your own. The team data ships with the package, and the "
       "data page shows how to build it from two ordinary data frames."),

    h3("Where everything else is"),
    p("Reference material sits at the end: the "
      "[formula terms](#p-formula-terms-at-a-glance), "
      "[importing and exporting](#p-importing-exporting-and-deriving), "
      "[the rest of the package](#p-the-rest-of-the-package), "
      "[what rnetis refuses](#p-what-rnetis-refuses-and-why) and "
      "[common errors](#p-common-errors). The search box at the top searches "
      "every page at once."),
)

page(
    GS, "Installation", "Installation",
    p("rnetis is not on CRAN. Install it from GitHub, which takes a couple of "
      "minutes."),

    h3("Install it"),
    p("Installing from GitHub needs the **remotes** package. The first line "
      "gets it if you do not have it and does nothing if you do:"),
    codeonly(
        'if (!requireNamespace("remotes", quietly = TRUE)) {\n'
        '  install.packages("remotes")\n'
        '}\n'
        '\n'
        'remotes::install_github("djgriffin13/rnetis")'),
    p("That also installs the packages rnetis requires: `generics`, "
      "`ggplot2`, `lme4`, `lmerTest` and `reformulas`."),
    rare("If you get `401 Bad credentials`",
         p("The repository is public, so this install needs no credentials. "
           "A 401 means an old GitHub token saved on your machine is being "
           "sent anyway and rejected. It often shows up as *“unknown "
           "package”*."),
         p("**Quickest fix:** download the archive directly, which does not "
           "authenticate:"),
         codeonly(
             'remotes::install_url(\n'
             '  "https://github.com/djgriffin13/rnetis/archive/'
             'refs/heads/main.tar.gz"\n'
             ')'),
         p("**Or clear the stale token**, which is worth doing anyway:"),
         codeonly(
             '# shows which token is being found, and from where\n'
             'gitcreds::gitcreds_get()\n'
             '\n'
             '# remove it\n'
             'gitcreds::gitcreds_delete()'),
         p("On Windows the same entry is under **Credential Manager → "
           "Windows Credentials**, listed as `git:https://github.com`.")),

    h3("Check that it worked"),
    p("If this prints a version, you are ready:"),
    block("g-check"),

    h3("Optional packages"),
    p("Three more packages unlock particular models. rnetis tells you which "
      "one you need at the moment you need it."),
    table(["Install", "What it unlocks", "First needed"], [
        ['`install.packages("latentnet")`', "`latent()`, the latent-space adjustment",
         "[Influence, page 7](#p-the-latent-space-influence-model)"],
        ['`install.packages("amen")`', "`fit_selection()`, the Selection model",
         "All of Selection"],
        ['`install.packages("igraph")`', "network layouts, and importing from `igraph`",
         "[Importing and exporting](#p-importing-exporting-and-deriving)"],
    ]),
    p("To follow every page, install all three now:"),
    codeonly('install.packages(c("latentnet", "amen", "igraph"))'),
    rare("More optional packages, and what each is for",
         table(["Install", "What it unlocks"], [
             ['`install.packages("clubSandwich")`', "cluster-robust standard errors"],
             ['`install.packages("network")`', "importing from and exporting to `statnet`"],
             ['`install.packages(c("sna", "numDeriv"))`',
              "the network-autocorrelation (SAR) estimator"],
         ]),
         p("To see which you already have:"),
         block("g-optional")),

    h3("If your own machine will not cooperate"),
    p("[Posit Cloud](https://posit.cloud) runs R and RStudio in a browser "
      "tab. Sign up for a free account, create a new project, and run the "
      "same commands as above. Nothing else on this page changes."),
    rare("What to know before relying on it",
         p("The free plan caps compute per month and gives each project a "
           "modest amount of memory; the current limits are on "
           "[posit.cloud](https://posit.cloud/plans). The Influence "
           "sequence fits comfortably. Selection runs Markov chains, and its "
           "multi-team model is the one piece substantial enough to feel a "
           "ceiling, so run that one early.")),
)
