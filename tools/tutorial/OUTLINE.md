# Tutorial outline — third pass

Status: **proposed, for review.** Nothing below is built yet. This supersedes the
page structure now in `content_a.py` and `content_b.py`.

The brief: the tutorial should read like someone walking you through an analysis,
not like a reference card. The present page is a line of prose, a code block, a
line of prose, a code block. That is a cheat sheet. A tutorial has to say what we
are about to do, why anyone would do it, what the output means, and what it would
have looked like if the answer had been different.

---

## The writing standard

This is the part that matters most, and it is a rule rather than an aspiration.

1. **No code block without a reason in front of it.** Before every block, the
   reader knows what question the next command answers and what they expect to
   see. A block introduced by "Now run:" has failed.
2. **No code block without a reading after it.** Every block is followed by prose
   that names the numbers in its output and says what they mean. Where a number
   is surprising, the prose says why.
3. **Two code blocks never touch.** If two blocks sit adjacent with nothing
   between them, one of them is redundant or the explanation is missing.
4. **Every function that appears gets its purpose stated once, in words,** before
   its arguments are shown. Not "`exposure()` takes a network and a variable" but
   "`exposure()` computes, for each person, what their contacts' scores add up to".
5. **Helpers that exist for inspection stay out of the main line.** They are
   notes, not steps — see `compute_exposure()` in §A3.2.
6. **Length target per model page: 1200–2000 words, 4–7 code blocks.** The present
   model pages run about 400 words. That ratio is the defect.
7. **Every number shown is captured from a real run.** Unchanged from passes 1–2:
   `build.py` refuses to emit a block whose capture is missing, so no figure in
   the prose can drift away from the code that produced it.

---

## Shape

```
(top level)                Welcome
Getting started            Overview · Installation
Path A · Influence         A1 Background → A2 Data → A3 Basic → A4 Latent → A5 Multilevel → A6 Other
Path B · Selection         B1 Background → B2 Data → B3 SRM → B4 AME → B5 Multilevel → B6 Other
Reference                  Formula terms · Importing and exporting · The rest of the package · What rnetis refuses
Help                       Common errors · Slow fits · Read more
```

Twenty-two pages, down from thirty-three, and the shrinkage is all in Reference
and Help — the two paths get longer, not shorter. They are deliberately the same
six-page shape, so a reader who has done one knows where they are in the other.

What is gone, and where it went: "Exposure" folds into A3 as its longest
subsection; "Read the results" folds into A3 and A4 as the section that reads each
model's own output; "Check the fit" stops being a page and becomes the last
section of every model page, so each model's fit is discussed where that model
is; "Several groups" and "One hierarchical model" merge into B5; "Derived
networks", "Derived variables", "Scope" and "Precision" fold into the Reference
pages. "Nodes, ties and waves" becomes A2/B2.

## Front matter — **written**

Revised 2026-11-09 on the maintainer's instruction: Welcome stands alone as the
front door, and **Getting started holds two pages**, Overview and Installation.
"How to read this" is gone as a page and subsumed into Overview.

- **Welcome** (top level). Short, and the front door. The lede, the access
  notice exactly as written in pass 2 — public for the workshop, full version
  under review, code in the distributables usable but not redistributable,
  contact address — the two path boxes, now describing six pages each, and a
  pointer into Getting started. It is the one block on the site with legal
  weight and it survived the restructure untouched.
- **Overview.** What rnetis is for, and how to read what follows. The two
  questions and why they are each other's rival explanation; the six-step shape
  as a two-column table so the parallel is visible rather than asserted; that
  one path is enough; that the equations are skippable; what you need; how long
  it takes; that every result shown is real and every drawn figure carries no
  numbers; quantities not verdicts; where the reference material is.
- **Installation.** Two routes, cloud first, per the maintainer's instruction.
  **Route 1** is a prepared Posit Cloud project: a browser tab with rnetis and
  every optional package installed, nothing landing on the reader's machine. It
  is argued for rather than just offered — it separates "can I get this
  working" from "do I understand the model", which otherwise arrive together.
  The free plan's limits are described in shape rather than in numbers, because
  the published numbers disagree with each other; the page points at
  posit.cloud and tells a session leader to fit the hierarchical model on the
  free plan *before* the day. Binder and a `rocker` container are a collapsed
  aside. **Route 2** is the three local-install cards from pass 2, unchanged.

  **Open action:** the Posit Cloud project does not exist yet and the page
  carries a marked placeholder for its link.

---

# Path A · Influence

## A1 · Background: the influence question

No code on this page. It exists to make the reader want the model.

1. **What influence means substantively.** A person's score moves toward the
   scores of the people they are connected to. Conformity, social learning,
   contagion — name the mechanisms, because they imply different exposure modes
   later.
2. **Why one occasion cannot answer it.** In a cross-section, similar connected
   people are the prediction of influence *and* of homophily *and* of shared
   context. Two waves is the minimum that distinguishes them, and this tutorial
   is built on two throughout.
3. **The hypothesis, stated so it could fail.** H1: conditional on a person's own
   earlier score, exposure to alters' earlier scores predicts their later score.
   State the null in words. State what a negative coefficient would mean.
4. **The rival explanation, named now rather than later.** Homophily (Shalizi &
   Thomas 2011): if similar people befriend each other, §A3's estimate is
   confounded. Promise §A4, and say plainly that §A3 alone is not a causal claim.
   — *figure: `homophily_vs_influence()` (built)*
5. **What this path answers and what it does not.**

## A2 · Data: building it, and bringing it in

The current page reaches for the packaged demo data before the reader knows what
a `netis_data` holds. This page builds one instead.

1. **What a `netis_data` is made of.** Nodes, ties, waves — three tables and the
   declarations that bind them. — *figure: new, two data frames on the left,
   the object on the right, arrows for which column becomes what*
2. **The node table, typed out.** Six people, a group label, and a score at each
   of two waves. Shown as a literal `data.frame()` call, so every value is on the
   page.
3. **The tie table, typed out.** Sender, receiver, weight, wave. The six rows are
   chosen so that one person has four in-neighbours (a usable ego for §A3.2), two
   dyads are mutual (§A2.5), and one person has no in-ties at all (§A3.2's NA).
4. **`netis_data()`.** The call, the printed object, `netis_schema()`. What the
   object knows that the two data frames did not.
5. **Direction: source and target.** Which column sends and which receives, the
   receiver-row convention, and what `symmetrize()` does and refuses. This is
   the section the rest of the tutorial points back to.
   — *figure: `directed_vs_mutual()` (built)*
6. **Zero, missing, and structurally absent are three things.** The density
   contrast is the demonstration: identical rows give 1 under `row_presence` and
   0.33 under a value rule. A network declared `complete_dyadic` without saying
   which rows are ties refuses the moment anything needs the answer.
   — *figure: `zero_versus_missing()` (built)*
7. **Waves and time.** Waves are positional; metric dates are metadata. **This
   tutorial uses two waves everywhere,** and §B1 says why two rather than more.
8. **Or bring your own.** Short, and a note rather than a tour:
   `netis_data_from_edgelist()`, `_from_adjacency()`, `_from_adjacency_series()`,
   `_from_igraph()`, `_from_network()`, `as_netis_data()`, and back out again with
   `as_igraph()` / `as_network()`. One block, one table, pointer to the Reference
   page.
9. **Check it before modelling it.** `diagnose_data()`, `netis_boundary()`.

## A3 · The basic influence model

The long page. Built up a term at a time, with the whole formula assembled only
at §A3.5.

### A3.1 The equation

The display equation first, with each part labelled and the labels colour-keyed
to the subsections that follow:

> y_i(t2) = β₀ + β₁·y_i(t1) + β₂·E_i(t1) + β₃·x_i(t1) + ε_i

Walk every symbol. For each coefficient, say what it means in words and what it
is under the null. Say which part is the one we came for (β₂) and which parts
are there to make β₂ mean anything.

### A3.2 Exposure — E_i(t1)

The longest subsection in the tutorial.

- What it is: a summary of what this person's contacts scored. Friedkin & Johnsen.
  — *figure: `ego_exposure()` (built) — colour is the characteristic, width is the
  tie weight*
- **The three modes are three different estimands**, and the choice is
  substantive, not technical. `sum` (how much total influence reaches me),
  `mean` (what my contacts are like on average), `row_norm` (Leenders'
  weighted average). Say which mechanism each matches.
- The term: `exposure(network, variable, mode =, lag =)`.
- One block showing `compute_exposure()` output as a column beside the node
  table, to make E concrete — and immediately a **note**, not a block: in a model
  you write `exposure()` inside the formula, so the lag, the missingness mask and
  the provenance record travel with the fit. `compute_exposure()` is for looking,
  not for fitting.
- **The NAs.** A person with no in-ties has no contacts to average, so `row_norm`
  gives NA, not 0. This is the zero-versus-missing lesson of §A2.6 arriving where
  it bites. Read the top row of the comparison and say explicitly which column
  the other rows belong to.

### A3.3 The prior level — `ar()`

Why conditioning on y(t1) is not optional: without it β₂ absorbs every stable
difference between people, and the estimand stops being change. Regression toward
the mean. What `ar()` does and what its `lag` means.

### A3.4 Lagged covariates — `lagged()`

Why a covariate measured at t2 is contaminated by the outcome. What belongs here
and what does not.

> **A note on the brief's "corresponding helper function".** Exposure has a
> standalone helper — `compute_exposure()` — because exposure is constructed and
> a reader needs to see the construction once. `ar()` and `lagged()` have no
> standalone equivalent: they select an existing column at an earlier wave, and
> there is nothing to materialise. Rather than invent a parallel that does not
> exist, §A3.3 and §A3.4 show the column each term selects by printing the node
> table's two waves side by side. Same pedagogical job, no fictional API.

### A3.5 Running it

The full formula written out and discussed **before** any code: each term, why it
is in there, what `data =` and `waves =` do, and which backend this resolves to
(`stats::lm`, one group). Then — and only then — the block.

Here is where the `lm()` / `lmer()` parallel goes, explicitly: the same formula
interface, the same `~`, the same coefficient table. The existing capture that
builds the lagged and exposure columns by hand and hands them to `stats::lm()`,
reproducing all three coefficients to six decimals, is the proof that this is a
familiar regression with the network bookkeeping done for you.

### A3.6 Reading the results

`summary()` walked through region by region — the estimand header, the
coefficient table, the diagnostics, the losses — saying for each region what
question it answers. Then, briefly, the programmatic route: `tidy()`,
`netis_estimates()`. Then `influence_effect()` for an effect size a reader can
report in a sentence.

### A3.7 Variance explained, and fit

`netis_glance()` (r.squared, sigma, rmse), `logLik` / `AIC` / `BIC`, the model
comparison on identical rows — which comes out *against* exposure on this
dataset, and the prose says so — and `pseudo_r2()`. The assumption ledger
(`netis_assumptions()`), cut back to what a reader will act on. Close by saying
what these numbers do not license.

## A4 · The latent-space influence model

1. **The problem, restated with teeth.** §A3's β₂ is biased if unobserved
   similarity drives both tie formation and the outcome. Spell out the mechanism.
2. **The equation again, with the latent terms added,** and what they absorb.
3. **What a latent space is.** Positions in d dimensions; distance stands for
   propensity to tie; proximity is the unobserved similarity we could not measure.
   One or two equations. — *figure: `latent_space()` (built), plus a new homophily
   figure showing similar nodes drawn together*
4. **`latent(network, d =, homophily =, lag =)`.** The two stages: `latentnet::ergmm`
   estimates positions, then the positions enter the influence model as controls.
   Credit `latentnet` by name; say where Xu's procedure ends and the package's
   assembly begins. (Xu published no package — the R Journal article gives the
   procedure, the first stage is `latentnet`'s work.)
5. **Choosing d.** What rising d buys and what it costs; why d = 1 is the usual
   starting point (Xu's example uses it); that this is a sensitivity question
   rather than a selection rule. Here the criterion is weaker than §B4.4's: the
   latent positions are controls, not the object of inference, so the question is
   whether the exposure coefficient is stable across d, not whether the space
   fits. Show it across two or three values of d and say what instability would
   mean. State plainly that rnetis provides no selection rule for d.
6. **Run it.**
7. **What changed.** Side by side with §A3. Code block, then an interpretation of
   the direction and the size of the shift — and what it would have meant if
   nothing had moved.
8. **Fit, revisited.** What the fit statistics mean now that estimated positions
   sit in the design matrix, and why a comparison against §A3 is not a clean
   nested test.

## A5 · The multilevel influence model

Only what is new, and why it is needed.

1. **Start from the single-group fit.** Per the brief: the page opens by taking
   §A3's model, unchanged, to data that has groups in it — teams, classrooms,
   cohorts — and showing what it gets wrong. The standard errors are the honest
   casualty, and the point is visible rather than argued. Only then does the page
   say what grouping adds.
2. **The equation with random effects.**
3. **`group()`, `has_level()`, and the `lme4` backend.** What gets a random
   intercept, what can get a random slope.
4. **Variance decomposition** — the section the brief asks for special care on.
   `netis_variance_components()`, `icc1()`, `icc2()`, `rwg()` / `rwg_j()`,
   `netis_reliability()`. What ICC(1) and ICC(2) each answer, and why they are
   not interchangeable.
5. **Run and read.**
6. **Fit for a mixed model.** Why R² is contested here, what `netis_glance()`
   reports on an lme4-backed fit, and the standing warning that a predictor on a
   higher-order set is informed by the number of groups, not the number of people.

## A6 · Other points (Influence)

Short subsections, each: why you would do this, the function, one small block or
a pointer.

- **Moderation.** Ordinary `a:b` interactions, with the moderand/moderator
  distinction the parser enforces.
- **Missing data.** What rnetis refuses to impute, and what it masks instead.
- **Simulating data.** `simulate_influence()` — and it is more than a toy:
  `n_groups`, `n_waves`, `error_structure`, `initial_condition` let a reader test
  whether their design can recover an effect they believe in before collecting it.
- **Sensitivity analysis.** `influence_se_comparison()`, `netis_dependence()`,
  `bi_sweep()`.
- **Several networks.** `multiplex()` and more than one exposure term — with the
  experimental warning where the reader meets it, not in a footnote.

---

# Path B · Selection

Same six-page shape.

## B1 · Background: the selection question

1. **What selection means.** Who forms a tie with whom. Here homophily is the
   object of interest rather than the nuisance it was in Path A — say so, and
   point back to §A1.
2. **The hypothesis, stated so it could fail.**
3. **Why one transition.** Expanded per the brief: more waves are possible in
   principle, but a dynamic panel with a lagged dependent variable carries
   Nickell (1981) bias, and the initial-conditions problem (Heckman 1981) does
   not go away by adding waves. **rnetis corrects for neither**, so it permits a
   single transition and refuses more rather than returning a biased number
   quietly. — *figure: `one_transition()` (built)*

## B2 · Data

Parallel to §A2, same dataset, different emphasis: the unit of analysis is the
**ordered pair**, and there are two waves of *ties* rather than two waves of a
score.

1. The ordered pair as the row. Building a pair. — *figure: `selection_model()`
   (built)*
2. Which wave is the predictor and which is the outcome.
3. Interop both ways — `as_igraph()`, `as_network()`, and the presence-rule
   declaration that an `as_igraph()` call needs before it will answer.
4. Check it: `diagnose_data()`, `diagnose_design()`.

## B3 · The basic selection model — SRM

1. **The equation.** Probit link; μ + sender effect + receiver effect +
   covariates. What the social relations model is and what it buys: the fact that
   my ties are not independent of each other because they are all mine.
2. **Sender and receiver variance,** and what each says substantively
   (expansiveness, popularity) before any term is introduced.
3. **The terms, one subsection each:** `sender()`, `receiver()`, `dyad()`,
   `prior_tie()`, `prior_reciprocity()`. For each: the question it answers first,
   the syntax second.
4. **Running it.** The whole formula discussed before the block; `fit_selection()`
   with `R = 0`; what `family = "bin"` means and what the sampler is doing.
5. **Reading the results.** `summary()` region by region. `selection_effect()`
   for a reportable effect size.
6. **Fit, and the posterior predictive check.** Convergence first (trace,
   effective size — a posterior you have not sampled properly has no fit to
   discuss), then `pseudo_r2()`'s Tjur's D, which is itself a posterior-predictive
   quantity, then `netis_glance()`. Say plainly that a Selection fit refuses
   `logLik()` by name, and why: there is no single likelihood to report.
7. **Then the five network statistics** — the heart of the section, and the
   reason §B4 exists. For each of `sd.rowmean`, `sd.colmean`, `dyad.dep`,
   `cycle.dep` and `trans.dep`: what network feature it measures, and what it
   means when the observed value sits in the tail of the posterior predictive
   distribution rather than in the middle of it. The expected result here is that
   **the SRM reproduces the two degree statistics and misses the two triadic
   ones** — sender and receiver effects are exactly what explains unequal sending
   and unequal popularity, and they have nothing to say about closure. Do not
   assert that; run it and read what comes out.
   — *figure: observed value against the posterior predictive distribution, one
   small panel per statistic*

## B4 · The AME model

1. **What SRM cannot absorb.** This page opens on §B3.7's failing statistics, so
   the reader arrives already knowing what is wrong: additive sender and receiver
   effects explain expansiveness and popularity but not third-order structure —
   transitivity, clustering, who-with-whom. The motivation for AME is evidence
   from the previous page, not an assertion on this one.
2. **The equation with the multiplicative term,** UV′, and what it stands for.
3. **The latent space here.** Same geometry as §A4, different role: there it was
   a control, here it is part of the model of tie formation. Cross-reference §A4.3
   rather than re-explaining.
4. **Choosing R.** Per the brief: what R = 1, 2, 3 buy, and the cost in draws and
   in identification. The rank question has an answer here that §A4.5 does not
   have: the five network statistics give an actual criterion, so the honest
   advice is to raise R until the triadic statistics stop moving and no further,
   rather than to pick a number. Say that a higher R always fits the observed
   network better and that this is not evidence. Matched pair with §A4.5, which
   has to make do with coefficient stability.
5. **Run it** with `R = 2`.
6. **What changed.** Against §B3's estimates. Code block, then interpretation.
7. **Fit again, and the same five statistics.** The payoff: re-run §B3.7's
   posterior predictive check on the AME fit and compare. If `trans.dep` and
   `cycle.dep` have moved from the tail toward the middle, the multiplicative
   effects earned their keep, and the reader has watched a model-building
   decision being made on evidence. If they have not, the page says that
   instead — and that is the more useful lesson about rank.

## B5 · Several groups, and one hierarchical model

1. **Why more than one group,** and why fitting them separately is the honest
   first move.
2. **Per-group fits,** then combining: `pool_beta` / `selection_pooling()`,
   inverse-variance pooling (Cochran; Hedges & Olkin). What pooling assumes.
3. **Then the hierarchical model:** `pooling =`, one model over all groups, and
   the error rnetis raises if you pass both — because they are different models,
   not two spellings of one.
4. **Experimental,** said where the reader meets it.
5. Variance across groups, and what it means when it is large.

## B6 · Other points (Selection)

- **Missing dyads.**
- **Simulating selection data.**
- **Sensitivity analysis.**
- **Several relations.**
- **Moderation is refused.** Selection refuses interactions unconditionally. This
  is a real asymmetry with §A6 and the page says so rather than leaving a gap.

---

---

## Parallelism audit

The brief asks for the paths to match. Here is where they do, and the three
places they cannot — each of which the prose names rather than hides.

| | Path A · Influence | Path B · Selection |
|---|---|---|
| 1 Background | influence vs homophily; two waves needed | tie formation; one transition only, and why |
| 2 Data | same object, node-and-attribute emphasis | same object, ordered-pair emphasis |
| 3 Basic | OLS: `ar()` + `exposure()` + `lagged()` | SRM: sender + receiver + dyad covariates |
| 4 Latent | `latent(d =)` as a **control** for confounding | `R =` as **part of the model** of tie formation |
| 5 Multilevel | `group()`, lme4, variance components | per-group → pooled → hierarchical |
| 6 Other | moderation, missing, simulation, sensitivity, multiplex | missing, simulation, sensitivity, several relations |
| Fit | r², AIC/BIC, nested comparison | convergence, Tjur's D, five network statistics |

**Where they genuinely diverge, and the prose says so:**

1. **The latent space changes job between the paths.** On A it is a nuisance
   control — we want it out of the exposure estimate. On B it is substantive —
   the latent positions *are* a claim about how ties form. §B4.3 cross-references
   §A4.3 for the geometry and then states the difference in role outright, since
   a reader who has done Path A will otherwise carry the wrong intuition across.
2. **Fit means different things.** A has a likelihood, an r², and a nested
   comparison. B has none of those and refuses `logLik()` by name; what it has
   instead is a posterior and a posterior predictive check. The two fit sections
   should not be made to look alike, and §B3.6 opens by saying why they are not.
3. **Moderation exists on A and is refused on B.** `fit_selection()` refuses
   interactions unconditionally. §B6 states the refusal rather than omitting the
   topic, because a reader coming from §A6 will go looking for it.

---

## Figures

Seven diagrams exist from pass 2 and all seven survive, several moving pages.
Four are new. Nothing here reports a number from a fit: every figure that carries
an estimate is a captured ggplot, so no drawing can disagree with a result.

| figure | status | where it goes |
|---|---|---|
| `homophily_vs_influence()` | built | §A1.4 — the rival explanation, on the background page |
| `directed_vs_mutual()` | built | §A2.5 — the section everything else points back to |
| `zero_versus_missing()` | built | §A2.6 |
| `ego_exposure()` | built | §A3.2 — colour is the characteristic, width is the weight |
| `latent_space()` | built | §A4.3 |
| `selection_model()` | built | §B2.1 — the ordered pair as the unit |
| `one_transition()` | built | §B1.3 — why two waves and not more |
| **data structure** | new | §A2.1 / §B2.1 — two data frames on the left, the object on the right, arrows for which column becomes what. The brief asks for this one specifically. |
| **homophily drawing nodes together** | new | §A4.3, beside `latent_space()` — the brief asks for a homophily figure |
| **equation anatomy ×3** | new | §A3.1, §A4.2, §B3.1 — see below |
| **GOF panels** | new, captured | §B3.7, §B4.7 — observed value against the posterior predictive distribution, one panel per statistic. A real ggplot from real draws, not a diagram. |

---

## Equations

The brief wants the equation foregrounded on every model page, with its parts
labelled and tied to the subsections that explain them. The nine HTML equations
from pass 2 are fine for an inline formula and wrong for this.

**Decision: the three anatomy equations are hand-built inline SVG, not HTML and
not a maths renderer.** Reasons, in order of weight:

1. The labels have to be *colour-keyed to the subsections below them*, with
   leader lines or braces from each term to its label. That is a drawing. No
   maths renderer gives you a stable hook on "the third term" to colour and
   annotate; SVG does, and the existing diagram palette already follows
   light/dark through the page's own colour tokens.
2. KaTeX or MathJax would cost a CDN or about 300 KB of script and fonts
   embedded, and the page's whole premise is that it is one self-contained file
   that works offline. A formula that needs a CDN is a formula that is blank on
   a train.
3. The remaining inline equations stay as HTML. They are small and they do not
   need annotating.

Cost: three hand-drawn SVGs, in the same style as the seven existing diagrams,
using the same helpers in `diagrams.py`. Each is reused as a thumbnail at the top
of its page's subsections, so the reader always knows which term is under
discussion.

---

## Reference and Help: what is cut

The brief says there is too much unneeded information. These are the cuts, so
they can be argued with rather than discovered later.

| page | fate |
|---|---|
| Formula terms | **kept** — the one-table summary of every term, the page a returning reader actually wants |
| Importing and exporting | **kept**, absorbing "Derived networks" and "Derived variables" |
| The rest of the package | **kept** — the appendix from pass 2, one line per exported function with why you would reach for it |
| What rnetis refuses | **kept** — the refusals with their codes |
| Derived networks | folded into Importing and exporting |
| Derived variables | folded into Importing and exporting |
| Scope: what is not here | folded into What rnetis refuses, which is the same subject from the other side |
| Precision | folded into §A3.7, where the question is actually asked |
| Slow fits | **kept** in Help — a reader with a slow MCMC fit needs it findable |
| Common errors | **kept** in Help |
| Read more | **kept** in Help, including the packages-rnetis-builds-on table |

---

## Build order

The restructure touches the captures, not just the prose, so the order matters.

1. **The GOF work first** (scoped below). §B3 and §B4 cannot be written against
   numbers the package does not return.
2. **One dataset, built once.** §A2 and §B2 are two pages over one object. Build
   it in a single capture script so neither page can drift from the other.
3. **New captures before new prose**, as in pass 2: write the block, run it, read
   what actually came back, then write the paragraph around it. Three of pass 2's
   best sections came from a capture refusing when I expected it to succeed, and
   that only works if the code runs first.
4. **The four new figures** once their sections exist, so the drawing matches the
   prose rather than the other way round.
5. **`run-e.R` last, always.** `run-a.R` and `run-b.R` re-render figures at their
   own narrower widths as a by-product and `ggsave` clips rather than shrinks, so
   anything that re-runs them silently truncates the publication renders. This
   bit once already; the README carries the warning.

---

## Decisions (2026-11-09)

1. **Two data pages**, §A2 and §B2, both building the same object from the same
   captures. §B2 is the shorter and spends its prose on the ordered pair.
2. **Several networks and several relations go in §A6 / §B6**, with the
   experimental warning attached where the reader meets it.
3. **The network-level posterior predictive statistics get surfaced in the
   package first**, and §B3.7 and §B4.7 are written against them. Scoped below.

## Package work this outline depends on — **done, 2026-11-09**

`netis_gof()` is implemented, exported, documented and tested; `plot(fit,
what = "gof")` draws it. What follows is the task as scoped, kept because it
records why; the decisions it left open are settled at the end.

Writing §B3.7 and §B4.7 needs five numbers the package computes and then throws
away. The task is small and well bounded.

**What is already true.** `fit_selection()` fits every group through
`amen::ame()` (`R/selection-fit.R:299`), and it already passes `gof = TRUE`
(`:698`), which is also amen's own default. So `amen` is already computing, each
scan, a posterior predictive draw of five network statistics, confirmed against
`amen::gofstats()`:

| statistic | what it measures | which part of the model should explain it |
|---|---|---|
| `sd.rowmean` | spread of out-degree — how unequally people send | sender effects |
| `sd.colmean` | spread of in-degree — how unequally people are chosen | receiver effects |
| `dyad.dep` | dyadic dependence — reciprocation beyond the covariates | `prior_reciprocity()`, dyadic correlation |
| `cycle.dep` | cyclic triads — generalised exchange | nothing in the SRM |
| `trans.dep` | transitive triads — closure and clustering | nothing in the SRM |

`backend-amen.R:168` already reaches into `fit$fit` to keep
`posterior_predictive = fit$fit$YPM`. `fit$fit$GOF` sits beside it, unkept.

**The work.**

1. Keep it: `gof = fit$fit$GOF` in `backend_extract()` beside
   `posterior_predictive`. Row 1 of that matrix is the observed network; the rest
   are posterior predictive draws.
2. An accessor — `netis_gof()` — returning, per statistic, the observed value,
   the posterior predictive mean and spread, and a tail probability. A tail
   probability is the thing a reader can act on, and it is the thing the tutorial
   will quote.
3. A plot in `plot-selection.R`: one small panel per statistic, observed value
   against the predictive distribution.
4. **A named refusal on the hierarchical route.** `R/mgc-sampler.R` deliberately
   does not compute the posterior predictive simulation or these statistics,
   because they do not affect the posterior and would consume RNG only to keep
   amen's stream aligned. That decision stands — but `netis_gof()` on a
   `pooling =` fit must then refuse by name and point here, rather than return
   nothing. §B5 says so in prose, which is also the honest answer to "why does
   the check I just learned stop working on this page".
5. Tests, and a fixture asymmetric under the failure mode: a network with real
   transitivity, where a sender/receiver-only model has to miss `trans.dep`.

**Why this is worth doing before the prose rather than after.** It is what makes
the SRM → AME progression the brief asks for into an argument instead of an
assertion. §B3 fits the additive model and finds that it reproduces the degree
spread and misses closure; §B4 adds multiplicative effects and checks whether
closure comes back. That is a reader watching a modelling decision get made on
evidence, and it is the same shape as §A3 → §A4, where the latent space is
introduced because homophily confounds the exposure estimate. Without the five
statistics, §B4 opens by asserting that SRM is not enough and asking the reader
to take it on trust.

### What the work settled

**Tail probability, standardised distance, or both?** Both, with an order of
precedence stated in the help page and the plot caption. `ppp` is primary and is
one-sided, so 0 and 1 are both misfit and 0.5 is agreement; its granularity is
`1 / n_draws`, which a short chain makes coarse. `z` is the tie-breaker when
`ppp` saturates, and it assumes a roughly symmetric predictive distribution,
which `dyad.dep` need not have. `predicted_lower` / `predicted_upper` are
predictive quantiles and assume nothing. No threshold is applied to any of them.

**Two refusals, not one.** A `pooling =` fit has no simulated networks, and a
fit run with `gof = FALSE` kept the observed statistics and no distribution to
read them against. They need different instructions — "fit the groups
separately" against "refit with gof = TRUE" — so they are separate refusals,
and a fit saved before this change is a third.

**The caption depends on the model, not the statistic list.** "Nothing additive
accounts for the triadic statistics" is true of an `R = 0` fit and actively
misleading on an AME fit, where those statistics are what `R > 0` was added to
account for. A reader who saw the additive caption on an AME fit would wave away
the one result they should be weighing, so the sentence switches on `R`.

### Two things this changes for the prose

1. **The mechanism is confirmed, the tutorial's own numbers are not.** On a test
   fixture of three dense blocks, an additive fit misses closure by about five
   predictive standard deviations while the same model on an unclustered network
   sits inside its predictive range. So the §B3 → §B4 argument has a working
   mechanism behind it. It is **not** a result for the tutorial's dataset, which
   has not been run. §B3.7 gets written from a capture, as the build order says.
2. **§B2's dataset may need more actors than §A2's.** The triadic statistics are
   computed over triples, and a six-person network has too few to say anything —
   the predictive range would swamp any observed value. Either §B2 builds a
   larger network than the six-person team of §A2, or §B3.7 runs on a second
   dataset introduced for it. That is a decision to take when §B2 is written,
   and it is the one place the two paths' shared dataset may have to give.
