# -*- coding: utf-8 -*-
"""Hand-drawn SVG diagrams for the tutorial.

These are *conceptual* figures: they explain what a term means, never what a
fit found. Every figure that carries a number from an estimate is a captured
ggplot rendered by R (`fig()` in build.py), so no drawing on the page can
disagree with a result.

Colour. The one figure that encodes a continuous quantity -- the egocentric
exposure picture -- uses a single-hue sequential ramp, with the low end
receding toward the surface in both themes (so the light theme runs
light-to-dark and the dark theme dark-to-light; the legend labels both ends,
and every node also carries its value in its label, so nothing is read from
colour alone). The two-mechanism figure uses two categorical hues validated as
a pair. Everything else is drawn in the page's own ink and line tokens.
"""

# --- shared defs -----------------------------------------------------------
# Arrowheads have to be declared per colour: a marker's fill does not inherit
# from the path that uses it in every browser we care about.
DEFS = """<defs>
<marker id="aA" viewBox="0 0 10 8" refX="9" refY="4" markerWidth="7" markerHeight="6"
 orient="auto-start-reverse"><path d="M0,0 L10,4 L0,8 z" fill="var(--dA)"/></marker>
<marker id="aB" viewBox="0 0 10 8" refX="9" refY="4" markerWidth="7" markerHeight="6"
 orient="auto-start-reverse"><path d="M0,0 L10,4 L0,8 z" fill="var(--dB)"/></marker>
<marker id="aI" viewBox="0 0 10 8" refX="9" refY="4" markerWidth="7" markerHeight="6"
 orient="auto-start-reverse"><path d="M0,0 L10,4 L0,8 z" fill="var(--dink)"/></marker>
<marker id="aG" viewBox="0 0 10 8" refX="9" refY="4" markerWidth="7" markerHeight="6"
 orient="auto-start-reverse"><path d="M0,0 L10,4 L0,8 z" fill="var(--dline)"/></marker>
</defs>"""


def _svg(w, h, body, label):
    return (
        '<svg viewBox="0 0 %d %d" role="img" aria-label="%s" '
        'xmlns="http://www.w3.org/2000/svg">%s%s</svg>' % (w, h, label, DEFS, body)
    )


def _t(x, y, s, cls="lbl", anchor="middle", pre=False):
    # SVG collapses runs of whitespace like HTML does, so a monospaced block
    # whose columns are lined up with spaces loses its alignment unless it
    # says otherwise.
    sp = ' xml:space="preserve"' if pre else ""
    return '<text x="%s" y="%s" class="%s" text-anchor="%s"%s>%s</text>' % (
        x, y, cls, anchor, sp, s)


def _node(x, y, r, label, fill="var(--dfaint)"):
    return ('<circle cx="%s" cy="%s" r="%s" class="node" fill="%s"/>' % (x, y, r, fill)
            + (_t(x, y + 5, label) if label else ""))


def _box(x, y, w, h, lines, stroke="var(--dline)"):
    out = ('<rect x="%s" y="%s" width="%s" height="%s" rx="7" fill="var(--dsurf)" '
           'stroke="%s" stroke-width="1.3"/>' % (x, y, w, h, stroke))
    cx = x + w / 2.0
    n = len(lines)
    first = y + h / 2.0 - (n - 1) * 8 + 5
    for i, ln in enumerate(lines):
        cls = "lbl" if i == 0 else "sub"
        out += _t(cx, first + i * 16, ln, cls)
    return out


def _panel(x, y, w, h):
    return ('<rect x="%s" y="%s" width="%s" height="%s" rx="9" class="panel" '
            'stroke-width="1.2"/>' % (x, y, w, h))


def _arrow(x1, y1, x2, y2, colour="var(--dA)", sw=2.2, marker="aA", dash=None):
    d = ' stroke-dasharray="%s"' % dash if dash else ""
    return ('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="%s" '
            'marker-end="url(#%s)"%s/>' % (x1, y1, x2, y2, colour, sw, marker, d))


# ===========================================================================
# 1. source, target, and what makes a dyad mutual
# ===========================================================================
def directed_vs_mutual():
    b = _t(176, 18, "ONE ROW, ONE DIRECTION", "ttl") + _panel(8, 28, 336, 152)
    b += _node(95, 100, 26, "Cyd") + _node(255, 100, 26, "Ada")
    b += ('<line x1="121" y1="100" x2="225" y2="100" stroke="var(--dA)" '
          'stroke-width="5" marker-end="url(#aI)"/>')
    b += ('<circle cx="173" cy="100" r="12" class="panel" stroke-width="1.2"/>'
          + _t(173, 105, "3", "mono"))
    b += _t(176, 150, "Cyd advises Ada. Ada does not advise Cyd.", "sub")
    b += _t(8, 205, "source   target   strength", "mono", "start", pre=True)
    b += _t(8, 224, "Cyd      Ada             3", "mono", "start", pre=True)

    b += _t(524, 18, "TWO ROWS, A MUTUAL DYAD", "ttl") + _panel(356, 28, 336, 152)
    b += _node(445, 100, 26, "Ada") + _node(605, 100, 26, "Ben")
    b += ('<path d="M466,88 Q525,52 584,88" class="edge" stroke-width="3.5" '
          'marker-end="url(#aI)"/>')
    b += ('<path d="M584,116 Q525,150 466,116" class="edge" stroke-width="2" '
          'marker-end="url(#aI)"/>')
    b += _t(525, 44, "2", "mono") + _t(525, 168, "1", "mono")
    b += _t(356, 205, "source   target   strength", "mono", "start", pre=True)
    b += _t(356, 224, "Ada      Ben             2", "mono", "start", pre=True)
    b += _t(356, 243, "Ben      Ada             1", "mono", "start", pre=True)
    return _svg(700, 258, b,
                "Left: one tie row draws one arrow from source to target. "
                "Right: two rows in opposite directions make a mutual dyad.")


# ===========================================================================
# 2. a one, a zero, and no row at all
# ===========================================================================
def zero_versus_missing():
    b = ""
    cols = [
        (8, "A ROW WITH 1", "solid", "A tie.",
         "Present, and weighted 1."),
        (246, "A ROW WITH 0", "zero", "Asked, and the answer was no.",
         "A measured zero: the absence is data."),
        (484, "NO ROW AT ALL", "none", "Not a tie — or never asked.",
         "Only you can say which."),
    ]
    for x0, title, kind, l1, l2 in cols:
        cx = x0 + 104
        b += _t(cx, 18, title, "ttl") + _panel(x0, 28, 208, 118)
        b += _node(x0 + 48, 88, 22, "i") + _node(x0 + 160, 88, 22, "j")
        if kind == "solid":
            b += _arrow(x0 + 72, 88, x0 + 134, 88, sw=2.6, marker="aI")
            b += _t(cx, 68, "1", "mono")
        elif kind == "zero":
            b += ('<line x1="%d" y1="88" x2="%d" y2="88" class="ghost" '
                  'stroke-width="1.6"/>' % (x0 + 72, x0 + 138))
            b += _t(cx, 68, "0", "mono")
        else:
            b += _t(cx, 95, "?", "lbl")
        b += _t(cx, 172, l1, "lbl") + _t(cx, 192, l2, "sub")
    return _svg(700, 206, b,
                "Three states for one ordered pair: a tie, an observed "
                "non-tie, and a pair with no row in the table.")


# ===========================================================================
# 3. egocentric exposure: colour is the characteristic, width is the weight
# ===========================================================================
def ego_exposure():
    ada = (330, 220, 34, "var(--d5)")
    alters = [
        # x,    y,   r, fill,        weight, stroke, label,        lx,  ly
        (130, 100, 26, "var(--d1)", 1, 2.0, "Ben · 10", 130, 60),
        (130, 340, 26, "var(--d2)", 3, 5.0, "Cyd · 20", 130, 392),
        (530, 100, 26, "var(--d3)", 2, 3.5, "Dev · 30", 530, 60),
        (530, 340, 26, "var(--d4)", 1, 2.0, "Eli · 40", 530, 392),
    ]
    ends = [
        (152.3, 113.4, 294.0, 198.4, 223.2, 155.9),
        (152.3, 326.6, 294.0, 241.6, 223.2, 284.1),
        (507.7, 113.4, 366.0, 198.4, 436.9, 155.9),
        (507.7, 326.6, 366.0, 241.6, 436.9, 284.1),
    ]
    b = ""
    # Ada's own outgoing tie, drawn as a reminder that it does not enter her
    # own exposure: the default reading is that a tie carries influence from
    # source to target, so only arrows pointing AT Ada count.
    b += ('<path d="M362,186 C432,132 478,104 508,78" class="ghost" '
          'stroke-width="2" marker-end="url(#aG)"/>')
    b += _t(400, 100, "Ada → Dev is an outgoing tie,", "sub")
    b += _t(400, 116, "so it is not part of Ada's exposure", "sub")

    for (ax, ay, ar, fill, w, sw, lab, lx, ly), (x1, y1, x2, y2, mx, my) in zip(
            alters, ends):
        b += ('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="var(--dA)" '
              'stroke-width="%s" marker-end="url(#aA)"/>' % (x1, y1, x2, y2, sw))
        b += ('<circle cx="%s" cy="%s" r="13" class="panel" stroke-width="1.2"/>'
              % (mx, my)) + _t(mx, my + 5, str(w), "mono")
        b += _node(ax, ay, ar, "", fill) + _t(lx, ly, lab)

    b += _node(ada[0], ada[1], ada[2], "", ada[3]) + _t(330, 178, "Ada · 50")
    b += _node(690, 220, 26, "", "var(--d6)") + _t(690, 178, "Fay · 60")
    b += _t(690, 268, "nobody advises Fay", "sub")

    # legend
    b += _t(40, 418, "WELLBEING, THE CHARACTERISTIC CARRIED", "ttl", "start")
    for i in range(6):
        b += ('<rect x="%d" y="426" width="22" height="13" fill="var(--d%d)" '
              'stroke="var(--dink)" stroke-width="0.6"/>' % (40 + i * 24, i + 1))
    b += _t(40, 456, "10", "sub", "start") + _t(184, 456, "60", "sub", "end")
    b += _t(330, 418, "ADVICE STRENGTH, THE TIE WEIGHT", "ttl", "start")
    for i, (sw, lab) in enumerate([(2.0, "1"), (3.5, "2"), (5.0, "3")]):
        x = 330 + i * 70
        b += ('<line x1="%d" y1="433" x2="%d" y2="433" stroke="var(--dA)" '
              'stroke-width="%s"/>' % (x, x + 40, sw))
        b += _t(x + 50, 438, lab, "mono")
    return _svg(760, 470, b,
                "Ada's four advisers, with colour showing each one's "
                "wellbeing and line width the strength of the advice tie.")


# ===========================================================================
# 4. the two stories that produce the same data
# ===========================================================================
def homophily_vs_influence():
    b = _t(176, 18, "WHAT INFLUENCE SAYS", "ttl") + _panel(8, 28, 336, 212)
    b += _box(98, 54, 156, 46, ["alter's y", "at wave 1"])
    b += _box(98, 168, 156, 46, ["ego's y", "at wave 2"])
    b += _arrow(176, 100, 176, 164, sw=2.6)
    b += _t(264, 128, "carried along", "sub", "start")
    b += _t(264, 144, "the tie", "sub", "start")

    b += _t(524, 18, "WHAT LATENT HOMOPHILY SAYS", "ttl") + _panel(356, 28, 336, 212)
    b += _box(448, 46, 152, 44, ["an unobserved trait"], "var(--dB)")
    b += _box(372, 168, 142, 46, ["who is tied", "to whom"])
    b += _box(534, 168, 142, 46, ["both actors'", "y"])
    b += _arrow(500, 92, 462, 164, "var(--dB)", 2.6, "aB")
    b += _arrow(548, 92, 586, 164, "var(--dB)", 2.6, "aB")
    b += _t(524, 232, "and no arrow between the two y's at all", "sub")
    return _svg(700, 252, b,
                "Two causal stories. Both imply that tied actors have "
                "similar outcomes, and a two-wave influence model cannot "
                "tell them apart.")


# ===========================================================================
# 5. the latent space the first stage estimates
# ===========================================================================
def latent_space():
    """Actors placed in a plane, so distance is distance on the page.

    DRAWN AT d = 2 RATHER THAN d = 1. On a line, "close" and "far" have to be
    read off one axis while the picture itself is two-dimensional, and every
    reader spends a moment working out that the vertical is meaningless. In a
    plane the geometry on the page IS the geometry of the model: two actors
    near each other are near each other, and the eye needs no translation.
    """
    # Two neighbourhoods and one actor between them, so the picture shows both
    # what proximity buys and what it costs -- Eli is close to nobody in
    # particular, which is a position the model can hold and a line cannot
    # show without ambiguity.
    pos = [
        (150, 118, "Dev"), (218, 168, "Ada"), (352, 140, "Eli"),
        (486, 238, "Ben"), (546, 170, "Cyd"), (604, 252, "Fay"),
    ]
    xy = {lab: (x, y) for x, y, lab in pos}

    # The plane, as two axes rather than a box: a box reads as a bounded
    # region, and the space is not bounded.
    b = ('<line x1="74" y1="316" x2="668" y2="316" stroke="var(--dline)" '
         'stroke-width="1.3"/>'
         '<line x1="74" y1="316" x2="74" y2="58" stroke="var(--dline)" '
         'stroke-width="1.3"/>')
    b += _t(676, 321, "z₁", "lbl", anchor="start")
    b += _t(74, 48, "z₂", "lbl")

    def seg(a, c, cls, sw):
        (x1, y1), (x2, y2) = xy[a], xy[c]
        return ('<line x1="%s" y1="%s" x2="%s" y2="%s" class="%s" '
                'stroke-width="%s"/>' % (x1, y1, x2, y2, cls, sw))

    for a, c in (("Dev", "Ada"), ("Ben", "Cyd"), ("Cyd", "Fay"), ("Ben", "Fay")):
        b += seg(a, c, "edge", 2.4)
    b += seg("Ada", "Ben", "ghost", 1.8)

    b += _t(150, 206, "close together:", "sub")
    b += _t(150, 222, "a tie is likely", "sub")
    # BELOW the dashed line, not on it. Centred on the Ada-Ben midpoint the
    # label sits exactly where the line runs, and the strikethrough reads as
    # though the text itself were crossed out.
    b += _t(318, 246, "no tie here:", "sub")
    b += _t(318, 262, "they are far apart", "sub")

    for x, y, lab in pos:
        b += _node(x, y, 15, "") + _t(x, y - 24, lab, "sub")

    b += _t(371, 352, "estimated latent position  z = (z₁, z₂)", "lbl")
    return _svg(700, 368, b,
                "The first stage places every actor in a latent space so that "
                "actors close together are likelier to be tied. Drawn at "
                "d = 2, so distance on the page is distance in the model.")


# ===========================================================================
# 5b. two data frames in, one netis_data out
# ===========================================================================
def data_structure(kind="influence"):
    """What the reader types, and what the object makes of it.

    ONE FUNCTION, TWO PATHS. The node and tie tables are the same object on
    both paths; what differs is which part the model reads as the outcome, and
    that is the only thing a reader needs the picture to tell them apart. Two
    near-identical drawings would drift.
    """
    selection = kind == "selection"

    # TWO COLUMNS WITH THEIR OWN HEADERS, rather than one title over the whole
    # drawing. The figure's argument is a transformation -- this becomes that
    # -- and a single centred title describes neither side of it.
    b = _t(148, 30, "what you type", "ttl")
    b += _t(550, 30, "what you get", "ttl")

    # The two input tables. Column names and a line on the row granularity;
    # no values, because the point is the shape. The granularity line lives
    # INSIDE each box: as a caption underneath it sat four pixels below the
    # border and read as though it belonged to whatever came next.
    b += _box(28, 52, 240, 92, [
        "nodes",
        "node · wave · y · x",
        "one row per person, per occasion",
    ])
    b += _box(28, 184, 240, 92, [
        "ties",
        "source · target · wave · weight",
        "one row per observed tie",
    ])

    # THE CALL IS A STEP, NOT A CAPTION. Floating `netis_data()` between two
    # converging arrows put the word on top of one of them. Drawn as a node in
    # the flow it has somewhere to sit, and it reads as the thing that does
    # the work rather than as an annotation about the arrows.
    b += ('<rect x="300" y="146" width="100" height="36" rx="18" '
          'fill="var(--dsurf)" stroke="var(--dA)" stroke-width="1.6"/>')
    b += _t(350, 169, "netis_data()", "mono")

    b += _arrow(268, 98, 296, 156)
    b += _arrow(268, 230, 296, 172)
    b += _arrow(400, 164, 424, 164)

    b += _panel(428, 52, 244, 224)
    b += _t(550, 76, "netis_data", "ttl")
    b += _box(444, 90, 212, 48, ["node table", "checked against the roster"])
    b += _box(444, 146, 212, 48, ["tie table", "oriented, receiver-row"])
    b += _box(444, 202, 212, 58, [
        "declarations",
        "waves · zero meaning · boundary",
    ])

    # A rule before the footer, so the closing statement reads as being about
    # the whole picture rather than about the box nearest to it.
    b += ('<line x1="28" y1="300" x2="672" y2="300" stroke="var(--dline)" '
          'stroke-width="1" stroke-dasharray="3 4"/>')

    if selection:
        tail = ["The outcome is the TIE table:",
                "wave 1 is the initial condition, wave 2 is what the model predicts."]
    else:
        tail = ["The outcome is a column of the NODE table:",
                "wave 1 predicts wave 2, and the ties carry the exposure."]
    b += _t(350, 324, tail[0], "lbl")
    b += _t(350, 344, tail[1], "sub")

    return _svg(700, 364, b,
                "Two data frames go in; one object comes out, carrying the "
                "declarations that make the tables mean something.")


# ===========================================================================
# 6. what a Selection model says about one ordered pair
# ===========================================================================
def selection_model():
    b = _t(10, 16, "FOR EACH ORDERED PAIR  i → j", "ttl", "start")
    b += _node(48, 70, 22, "i") + _node(48, 250, 22, "j")
    b += _arrow(48, 94, 48, 224, "var(--dline)", 1.6, "aG")
    b += _t(40, 155, "the tie", "sub", "end")
    b += _t(40, 171, "to model", "sub", "end")

    b += _box(120, 36, 190, 42, ["sender(tenure)", "i's own attribute"])
    b += _box(120, 126, 190, 42, ["dyad(same_unit)", "a property of the pair"])
    b += _box(120, 216, 190, 42, ["receiver(tenure)", "j's attribute"])
    b += _arrow(72, 62, 116, 54, "var(--dline)", 1.5, "aG")
    b += _arrow(72, 84, 116, 140, "var(--dline)", 1.5, "aG")
    b += _arrow(72, 236, 116, 154, "var(--dline)", 1.5, "aG")
    b += _arrow(72, 258, 116, 240, "var(--dline)", 1.5, "aG")

    b += _box(372, 44, 140, 38, ["aᵢ", "i sends a lot"])
    b += _box(372, 212, 140, 38, ["bⱼ", "j is sought out"])
    b += _box(372, 126, 140, 42, ["y*ᵢⱼ", "latent propensity"], "var(--dA)")
    b += _arrow(442, 82, 442, 122, "var(--dA)", 2.0)
    b += _arrow(442, 212, 442, 172, "var(--dA)", 2.0)
    b += _arrow(310, 57, 368, 136, "var(--dA)", 2.0)
    b += _arrow(310, 147, 368, 147, "var(--dA)", 2.0)
    b += _arrow(310, 237, 368, 158, "var(--dA)", 2.0)

    b += _box(570, 126, 120, 42, ["P(tie)", "on 0 to 1"])
    b += _arrow(512, 147, 566, 147, "var(--dA)", 2.0)
    b += _t(540, 134, "Φ", "mono")
    return _svg(700, 282, b,
                "Every predictor enters through a named role, the two actor "
                "effects soak up how much each person sends and receives, and "
                "the sum passes through a probit link.")


# ===========================================================================
# 7. two waves make one transition
# ===========================================================================
def one_transition():
    def mini(x0, edges, faded=False):
        o = 0.35 if faded else 1.0
        s = '<g opacity="%s">' % o + _panel(x0, 40, 160, 120)
        pts = {"a": (x0 + 34, 132), "b": (x0 + 126, 132), "c": (x0 + 80, 70)}
        for u, v in edges:
            (x1, y1), (x2, y2) = pts[u], pts[v]
            dx, dy = x2 - x1, y2 - y1
            ln = (dx * dx + dy * dy) ** 0.5
            s += _arrow(x1 + 13 * dx / ln, y1 + 13 * dy / ln,
                        x2 - 20 * dx / ln, y2 - 20 * dy / ln,
                        "var(--dA)", 2.0)
        for k, (x, y) in pts.items():
            s += _node(x, y, 13, "")
        return s + "</g>"

    b = _t(100, 28, "WAVE 1", "ttl") + mini(20, [("a", "b"), ("c", "a")])
    b += _t(350, 28, "WAVE 2", "ttl") + mini(270, [("a", "b"), ("b", "c"), ("c", "a")])
    b += _t(600, 28, "WAVE 3", "ttl") + mini(520, [("b", "c"), ("a", "c")], faded=True)

    b += _arrow(186, 100, 262, 100, "var(--dA)", 2.6)
    b += _t(224, 88, "prior_tie()", "mono")
    b += _t(224, 182, "one transition:", "lbl")
    b += _t(224, 200, "this is the model", "sub")

    b += ('<line x1="438" y1="100" x2="512" y2="100" class="ghost" '
          'stroke-width="2"/>')
    b += ('<line x1="468" y1="90" x2="482" y2="110" stroke="var(--dB)" '
          'stroke-width="2.4"/><line x1="482" y1="90" x2="468" y2="110" '
          'stroke="var(--dB)" stroke-width="2.4"/>')
    b += _t(475, 182, "a second transition", "lbl")
    b += _t(475, 200, "is not stacked on the first", "sub")
    return _svg(700, 214, b,
                "A fit models one transition. Stacking a second one on top of "
                "it is a different estimation problem, not more of the same "
                "data.")
