"""Assemble TUTORIAL.html for rnetis.

Every code block and every printed result on the page comes out of the
capture directory written by run-a.R / run-b.R: the same file that was
evaluated supplies the code, and its sibling .txt supplies the output. The
builder refuses to emit a block whose capture is missing, so a page can
never show code that was not run or output that was not produced by it.
"""

import base64
import html
import io
import os
import re
import sys
import datetime

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "out")


def _find_repo():
    """The package root: two levels up when this lives in tools/tutorial/."""
    here = HERE
    for _ in range(4):
        if os.path.exists(os.path.join(here, "DESCRIPTION")):
            return here
        here = os.path.dirname(here)
    raise SystemExit(
        "cannot find the package root (no DESCRIPTION above %s); "
        "run this from tools/tutorial/ inside the rnetis checkout" % HERE
    )


REPO = _find_repo()

MISSING = []
USED = []


# ---------------------------------------------------------------------------
# inline markup: `code`, **bold**, *em*, [text](href), $math$ left alone
# ---------------------------------------------------------------------------
def inline(s):
    s = html.escape(s, quote=False)
    s = re.sub(r"\[([^\]]+)\]\(([^)]+)\)", r'<a href="\2">\1</a>', s)
    s = re.sub(r"`([^`]+)`", r"<code>\1</code>", s)
    s = re.sub(r"\*\*([^*]+)\*\*", r"<strong>\1</strong>", s)
    s = re.sub(r"(?<![\w*])\*([^*\n]+)\*(?![\w*])", r"<em>\1</em>", s)
    return s


def read(name):
    path = os.path.join(OUT, name)
    if not os.path.exists(path):
        MISSING.append(name)
        return None
    with io.open(path, encoding="utf-8", errors="replace") as fh:
        return fh.read().rstrip("\n")


# ---------------------------------------------------------------------------
# block constructors
# ---------------------------------------------------------------------------
def p(text):
    return "<p>%s</p>" % inline(text)


def h3(text, anchor=None):
    a = ' id="%s"' % anchor if anchor else ""
    return "<h3%s>%s</h3>" % (a, inline(text))


def h4(text):
    return "<h4>%s</h4>" % inline(text)


def ul(*items):
    return "<ul>%s</ul>" % "".join("<li>%s</li>" % inline(i) for i in items)


def ol(*items):
    return "<ol>%s</ol>" % "".join("<li>%s</li>" % inline(i) for i in items)


def quote(text):
    return "<blockquote>%s</blockquote>" % inline(text)


def note(text):
    return '<p class="note">%s</p>' % inline(text)


def table(headers, rows):
    th = "".join("<th>%s</th>" % inline(h) for h in headers)
    tr = "".join(
        "<tr>%s</tr>" % "".join("<td>%s</td>" % inline(c) for c in row) for row in rows
    )
    return (
        '<div class="tablewrap"><table><thead><tr>%s</tr></thead>'
        "<tbody>%s</tbody></table></div>" % (th, tr)
    )


def copy_block(text, lang="r"):
    return (
        '<div class="copy"><pre class="lang-%s"><code>%s</code></pre>'
        '<button type="button" class="copybtn" aria-label="Copy">Copy</button></div>'
        % (lang, html.escape(text, quote=False))
    )


def code(cid):
    """The exact code that was evaluated, as a copyable block."""
    USED.append(cid)
    src = read(cid + ".R")
    if src is None:
        return '<p class="missing">[missing code: %s]</p>' % cid
    return copy_block(src)


def result(cid, label="Result"):
    """The output that code actually produced."""
    USED.append(cid)
    txt = read(cid + ".txt")
    if txt is None:
        return '<p class="missing">[missing output: %s]</p>' % cid
    if not txt.strip():
        return ""
    return (
        '<div class="res"><span class="reslbl">%s</span><pre>%s</pre></div>'
        % (html.escape(label), html.escape(txt, quote=False))
    )


def block(cid, label="Result"):
    return code(cid) + result(cid, label)


def refusal(cid):
    return code(cid) + result(cid, "rnetis refuses")


def codeonly(text, lang="r"):
    """A block that is illustrative only; never used for a shown result."""
    return copy_block(text, lang)


def dia(markup, caption):
    """A hand-drawn SVG diagram.

    Inline rather than a PNG: a diagram built from the page's own colour
    tokens follows the reader's light/dark setting, stays sharp at any zoom,
    and costs no extra request. Nothing here is data from a fit -- every
    figure that reports a number is a captured ggplot, so that no drawn
    picture can disagree with a result.
    """
    DIAGRAMS.append(caption)
    return (
        '<figure class="dia">%s<figcaption>%s</figcaption></figure>'
        % (markup, inline(caption))
    )


def eq(markup, note=None):
    """A display equation, written as HTML rather than rendered maths.

    No KaTeX or MathJax: the page is one self-contained file served from the
    pkgdown site root, and a formula that needs a CDN is a formula that is
    blank on a train. Everything here is reachable with <i>, <sub> and the
    Unicode operators.
    """
    tail = '<span class="eqn">%s</span>' % inline(note) if note else ""
    return '<div class="eq">%s%s</div>' % (markup, tail)


DIAGRAMS = []


def figure_source(cid):
    """Where this figure's PNG is: freshly rendered, or already published.

    out/png/ is the R scripts' output and is not committed; the published
    copies under pkgdown/assets/ are. Looking in both means the page can be
    rebuilt from a clean checkout without re-running the Markov chains, and
    a fresh render still wins when there is one.
    """
    fresh = os.path.join(OUT, "png", cid + ".png")
    if os.path.exists(fresh):
        return fresh
    published = os.path.join(REPO, "pkgdown", "assets", FIGDIR, cid + ".png")
    if os.path.exists(published):
        return published
    return None


def fig(cid, caption, cls=None):
    """A captured PNG. `cls` adds a figure class; see `qr` in the CSS.

    The default styling rounds corners and draws a border tight against the
    image, which is right for a plot and wrong for anything a machine has to
    read back. Pass cls="qr" for those.
    """
    if figure_source(cid) is None:
        MISSING.append(cid + ".png")
        return '<p class="missing">[missing figure: %s]</p>' % cid
    FIGURES.append(cid)
    klass = ' class="%s"' % cls if cls else ""
    return (
        '<figure%s><a href="%s/%s.png" target="_blank" rel="noopener">'
        '<img src="%s/%s.png" alt="%s" loading="lazy"></a>'
        "<figcaption>%s</figcaption></figure>"
        % (klass, FIGDIR, cid, FIGDIR, cid, html.escape(caption), inline(caption))
    )


# Published next to TUTORIAL.html at the site root. Deliberately NOT
# "tutorials": pkgdown emits a directory of that name itself, and two
# near-identical names in the site-root allowlist is how one of them gets
# removed by mistake.
FIGDIR = "tutorial-figures"


FIGURES = []


def card(title, *body, **kw):
    """A collapsible card. `step` adds a pill, `open` expands it."""
    step = kw.get("step")
    num = kw.get("num")
    opened = " open" if kw.get("open") else ""
    pill = '<span class="step">%s</span>' % html.escape(step) if step else ""
    bullet = '<span class="num">%s</span>' % html.escape(str(num)) if num else ""
    return (
        '<details class="card"%s><summary>%s<span class="ttl">%s</span>%s</summary>'
        '<div class="body">%s</div></details>'
        % (opened, bullet, inline(title), pill, "".join(body))
    )


def cards(*items):
    return '<div class="cards">%s</div>' % "".join(items)


def rare(title, *body):
    return (
        '<details class="rare"><summary>%s</summary>%s</details>'
        % (inline(title), "".join(body))
    )


def filterbox(placeholder, *items):
    return (
        '<input class="filter" type="search" placeholder="%s" aria-label="%s">'
        '<div class="cards">%s</div>' % (html.escape(placeholder), html.escape(placeholder), "".join(items))
    )


# ---------------------------------------------------------------------------
# page assembly
# ---------------------------------------------------------------------------
PAGES = []


def page(group, nav, title, *body):
    PAGES.append(
        {
            "group": group,
            "nav": nav,
            "title": title,
            "id": "p-" + re.sub(r"[^a-z0-9]+", "-", title.lower()).strip("-"),
            "body": "".join(body),
        }
    )


def data_uri(path):
    with open(path, "rb") as fh:
        return "data:image/png;base64," + base64.b64encode(fh.read()).decode("ascii")


CSS = """
:root{--bg:#f6f7f8;--panel:#ffffff;--ink:#16202b;--muted:#5a6672;--line:#dfe4e8;
--accent:#024d6f;--accent-soft:#e2eef4;--brand:#01284b;--mint:#0f7f4e;--mint-soft:#e3f3ea;
--code:#eef1f3;--codeink:#16202b;--out:#f3f5f6;--outink:#39454f;--mark:#ffe9a8;
--warn:#8a5a12;--warn-soft:#fbf1dd;--stop:#95331f;--stop-soft:#fbe8e2}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){--bg:#121619;--panel:#1a1f24;
--ink:#e7ecf0;--muted:#9aa6b1;--line:#2c343b;--accent:#74b8d8;--accent-soft:#14313f;--brand:#9ec8e4;
--mint:#5cd39a;--mint-soft:#15302a;--code:#222930;--codeink:#e7ecf0;--out:#1e252b;--outink:#b9c4ce;
--mark:#5c4f16;--warn:#e0b063;--warn-soft:#332a18;--stop:#f0a08a;--stop-soft:#36211c}}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--ink);
font:16px/1.58 system-ui,-apple-system,"Segoe UI",Roboto,sans-serif}
header{position:sticky;top:0;z-index:5;display:flex;gap:12px;align-items:center;flex-wrap:wrap;
padding:10px 16px;background:var(--panel);border-bottom:1px solid var(--line)}
header .brand{display:flex;align-items:center;gap:10px;margin-right:auto}
header .logo{width:36px;height:36px;flex:none}
header h1{font-size:17px;margin:0}
#stamp{font-size:12px;color:var(--muted)}
#search{flex:1 1 220px;max-width:400px;padding:8px 12px;border:1px solid var(--line);
border-radius:8px;background:var(--bg);color:var(--ink);font:inherit}
header button,.pager button{font:inherit;padding:7px 12px;border-radius:8px;
border:1px solid var(--line);background:var(--bg);color:var(--ink);cursor:pointer}
header button:hover,.pager button:hover:not(:disabled){border-color:var(--accent);color:var(--accent)}
.pager button:disabled{opacity:.45;cursor:default}
#count{font-size:13px;color:var(--muted)}
#toc{display:none;font:inherit;padding:7px 10px;border-radius:8px;border:1px solid var(--line);
background:var(--bg);color:var(--ink);max-width:100%}
.wrap{display:flex;max-width:1200px;margin:0 auto}
aside{width:252px;flex:none;padding:18px 12px;position:sticky;top:58px;align-self:flex-start;
max-height:calc(100vh - 60px);overflow:auto}
aside a{display:block;padding:6px 10px;border-radius:7px;color:var(--ink);text-decoration:none;font-size:15px}
aside a.sub{padding-left:22px;font-size:14.5px}
aside a:hover{color:var(--accent)}
.grphead{display:flex;align-items:center;margin-top:10px}
aside .grp{flex:1;display:block;padding:6px 10px;font-size:12px;font-weight:700;letter-spacing:.07em;
text-transform:uppercase;color:var(--muted);cursor:pointer;border-radius:7px}
aside a.grp:hover,aside span.grp:hover{color:var(--accent)}
.tog{border:0;background:none;color:var(--muted);cursor:pointer;font-size:14px;padding:4px 8px;
transition:transform .15s}
.navgrp.open .tog{transform:rotate(90deg)}
.navgrp:not(.open) .grpbody{display:none}
aside a.top{margin-top:12px;font-weight:600}
aside a.on{background:var(--accent-soft);color:var(--accent);font-weight:600}
main{flex:1;min-width:0;padding:18px 16px 60px}
.page{background:var(--panel);border:1px solid var(--line);border-radius:12px;
padding:6px 24px 18px;margin-bottom:18px;scroll-margin-top:72px}
h3,h4{scroll-margin-top:72px}
.page h2{font-size:25px;margin:18px 0 10px;letter-spacing:-.01em}
h3{font-size:19px;margin:24px 0 6px}
h4{font-size:16px;margin:18px 0 4px}
a{color:var(--accent);text-underline-offset:2px}
a:hover{color:var(--ink)}
code{background:var(--code);color:var(--codeink);padding:1px 5px;border-radius:5px;
font-size:.9em;font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace}
blockquote{margin:14px 0;padding:10px 14px;border-left:4px solid var(--accent);
background:var(--accent-soft);border-radius:0 8px 8px 0}
blockquote p:first-child{margin-top:0}blockquote p:last-child{margin-bottom:0}
.tablewrap{overflow-x:auto}
table{border-collapse:collapse;margin:12px 0;width:100%;font-size:15px}
th,td{border:1px solid var(--line);padding:6px 9px;text-align:left;vertical-align:top}
th{background:var(--accent-soft)}
.copy{position:relative;margin:8px 0 0}
.copy pre{margin:0;background:var(--code);color:var(--codeink);border:1px solid var(--line);
border-radius:8px;padding:12px 70px 12px 13px;overflow-x:auto;font-size:13.5px;line-height:1.5;
font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace}
/* Join a code block to the result directly beneath it. Stated this way round
   so that a browser without :has() leaves every block fully rounded, rather
   than leaving standalone blocks squared off at the bottom. */
.copy:has(+.res) pre{border-radius:8px 8px 0 0}
.copy+.res pre{border-top:0;border-radius:0 0 8px 8px}
.copybtn{position:absolute;top:7px;right:7px;font:13px system-ui,sans-serif;padding:4px 10px;
border-radius:6px;border:1px solid var(--line);background:var(--panel);color:var(--ink);cursor:pointer}
.copybtn.done{background:var(--accent);color:var(--panel);border-color:var(--accent)}
.res{position:relative;margin:0 0 14px}
.res pre{margin:0;background:var(--out);color:var(--outink);border:1px solid var(--line);
border-radius:8px;padding:11px 13px 11px 13px;overflow-x:auto;font-size:13px;line-height:1.45;
white-space:pre-wrap;word-break:break-word;
font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace}
.reslbl{position:absolute;top:0;right:9px;transform:translateY(-50%);background:var(--panel);
padding:0 6px;font-size:11px;letter-spacing:.06em;text-transform:uppercase;color:var(--muted)}
.cards{margin:8px 0 14px}
details.card{border:1px solid var(--line);border-left:5px solid var(--accent);border-radius:9px;
margin:8px 0;background:var(--panel)}
details.card>summary{list-style:none;cursor:pointer;display:flex;gap:10px;align-items:center;padding:10px 12px}
details.card>summary::-webkit-details-marker{display:none}
details.card>summary::after{content:"\\25b8";margin-left:6px;color:var(--muted)}
details.card[open]>summary::after{content:"\\25be"}
details.card .body{padding:0 14px 8px}
.num{flex:none;width:26px;height:26px;border-radius:50%;background:var(--accent-soft);color:var(--accent);
font-weight:700;font-size:14px;display:flex;align-items:center;justify-content:center}
.ttl{flex:1;font-weight:600}
.step{margin-left:8px;font-size:12px;font-weight:600;padding:1px 8px;border-radius:12px;
background:var(--mint-soft);color:var(--mint);white-space:nowrap}
details.rare{margin:12px 0}
details.rare>summary{cursor:pointer;color:var(--muted);font-weight:600;padding:6px 0}
.filter{width:100%;max-width:420px;padding:7px 11px;border:1px solid var(--line);border-radius:8px;
background:var(--bg);color:var(--ink);font:inherit;margin:4px 0 6px}
.note{color:var(--muted);font-size:15px}
.missing{color:var(--stop);font-weight:700}
.pager{display:flex;justify-content:space-between;margin-top:22px}
.hidden{display:none!important}
mark{background:var(--mark);color:inherit}
figure{margin:14px 0}
/* Diagram palette. Sequential steps are the documented blue ramp; the dark
   column is stepped for the dark surface rather than flipped, so in both
   modes the low end is the one that recedes toward the background. */
:root{--d1:#b7d3f6;--d2:#86b6ef;--d3:#5598e7;--d4:#2a78d6;--d5:#1c5cab;--d6:#0d366b;
--dA:#2a78d6;--dB:#eb6834;--dink:#16202b;--dline:#b9c2cb;--dsurf:#ffffff;--dfaint:#eef1f3}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){
--d1:#104281;--d2:#1c5cab;--d3:#2a78d6;--d4:#5598e7;--d5:#86b6ef;--d6:#b7d3f6;
--dA:#3987e5;--dB:#d95926;--dink:#e7ecf0;--dline:#495562;--dsurf:#1a1f24;--dfaint:#222930}}
:root[data-theme="dark"]{--d1:#104281;--d2:#1c5cab;--d3:#2a78d6;--d4:#5598e7;--d5:#86b6ef;
--d6:#b7d3f6;--dA:#3987e5;--dB:#d95926;--dink:#e7ecf0;--dline:#495562;--dsurf:#1a1f24;
--dfaint:#222930}
figure.dia{background:var(--bg);border:1px solid var(--line);border-radius:10px;
padding:10px 12px 6px}
figure.dia svg{display:block;width:100%;height:auto;overflow:visible}
figure.dia figcaption{margin-top:8px}
.dia text{font-family:system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;fill:var(--dink)}
.dia .lbl{font-size:13px}
.dia .sub{font-size:11.5px;fill:var(--muted)}
.dia .ttl{font-size:12px;font-weight:700;letter-spacing:.05em;text-transform:uppercase;
fill:var(--muted)}
.dia .mono{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:12px}
.dia .node{stroke:var(--dink);stroke-width:1.4}
.dia .edge{fill:none;stroke:var(--dA)}
.dia .ghost{fill:none;stroke:var(--dline);stroke-dasharray:4 3}
.dia .panel{fill:var(--dsurf);stroke:var(--line)}
.eq{margin:14px 0;padding:10px 14px;background:var(--bg);border:1px solid var(--line);
border-left:4px solid var(--accent);border-radius:0 8px 8px 0;text-align:center;
font-size:17.5px;line-height:1.9;overflow-x:auto}
.eq i{font-family:Georgia,"Times New Roman",serif;font-style:italic;padding:0 .5px}
.eq .op{padding:0 .28em;color:var(--muted);font-style:normal}
.eq .eqn{display:block;margin-top:6px;font-size:13.5px;line-height:1.5;color:var(--muted);
text-align:left}
.eq .tag{float:right;font-size:13px;color:var(--muted)}
figure img{max-width:100%;height:auto;border:1px solid var(--line);border-radius:8px;display:block;background:#fff}
/* A QR code is read by a machine, so the defaults above are all wrong for it.
   border-radius clips an arc into the three finder patterns a scanner uses to
   locate the code -- and this one ships with a zero-module quiet zone, so
   there is nothing to spare. The padding supplies the 4-module quiet zone the
   QR spec asks for (~22px at this module size) in white, the border moves
   outside that pad, and pixelated rendering stops a browser softening the
   modules if it scales.

   The edge is an OUTLINE, not a border, because box-sizing:border-box counts
   a border inside the declared width and this border resolves to 0.8px a side
   rather than 1px -- at width 248 the image rendered at 198.4px and at 250 it
   rendered at 200.4px, so no whole number was ever going to land it on its
   native 200. An outline draws outside the box and changes no geometry, which
   leaves 248 - 48 padding = exactly 200. */
figure.qr{text-align:center}
figure.qr img{border:0;outline:1px solid var(--line);border-radius:0;
padding:24px;width:248px;max-width:100%;background:#fff;
image-rendering:pixelated;margin:0 auto}
figcaption{font-size:13px;color:var(--muted);margin-top:5px}
.hero{float:right;width:108px;margin:2px 0 10px 18px}
.lede{font-size:17px;color:var(--ink)}
.pathbox{display:flex;gap:14px;flex-wrap:wrap;margin:14px 0}
.pathbox>div{flex:1 1 260px;border:1px solid var(--line);border-left:4px solid var(--accent);
border-radius:9px;padding:12px 14px;background:var(--bg)}
.pathbox h4{margin:0 0 6px}
.kbd{border:1px solid var(--line);border-bottom-width:2px;border-radius:5px;padding:0 5px;
font-size:.88em;background:var(--bg)}
/* The mobile header is three rows tall (title, search, contents select), so
   an anchor landing needs more clearance than the one-row desktop header. */
@media (max-width:860px){aside{display:none}#toc{display:block;width:100%}
.page{padding:4px 14px 14px;scroll-margin-top:150px}h3,h4{scroll-margin-top:150px}
header h1{font-size:15px}.hero{width:84px;margin-left:12px}}
"""

JS = r"""
(function(){
  var pages=[].slice.call(document.querySelectorAll('.page')),
      links=[].slice.call(document.querySelectorAll('#nav a'));
  var cur=0, showAll=false;
  function store(k,v){try{localStorage.setItem(k,v)}catch(e){}}
  function load(k){try{return localStorage.getItem(k)}catch(e){return null}}
  function show(n){
    cur=Math.max(0,Math.min(pages.length-1,n));
    pages.forEach(function(p,i){p.classList.toggle('hidden',!showAll&&i!==cur)});
    links.forEach(function(a){a.classList.toggle('on',+a.getAttribute('data-page')===cur)});
    [].slice.call(document.querySelectorAll('.navgrp')).forEach(function(g){
      g.classList.toggle('open',!!g.querySelector('[data-page="'+cur+'"]'))});
    var t=document.getElementById('toc'); if(t) t.value='';
    store('rnetis-tut-page',cur);
  }
  links.forEach(function(a){a.addEventListener('click',function(e){
    if(!showAll){e.preventDefault();show(+a.getAttribute('data-page'));window.scrollTo(0,0)}})});
  var groups=[].slice.call(document.querySelectorAll('.navgrp'));
  function toggleGroup(g){
    var opening=!g.classList.contains('open');
    groups.forEach(function(o){o.classList.remove('open')});
    if(opening)g.classList.add('open');
  }
  groups.forEach(function(g){
    g.querySelector('.tog').addEventListener('click',function(){toggleGroup(g)});
    var sp=g.querySelector('span.grp');
    if(sp)sp.addEventListener('click',function(){
      var first=g.querySelector('.grpbody a[data-page]');
      if(first){showAll=false;document.getElementById('all').textContent='Show all';
        show(+first.getAttribute('data-page'));window.scrollTo(0,0)}});
  });
  document.getElementById('toc').addEventListener('change',function(){
    if(this.value==='')return;
    showAll=false;document.getElementById('all').textContent='Show all';
    show(+this.value);window.scrollTo(0,0)});
  pages.forEach(function(p,i){
    var pv=p.querySelector('.prev'), nx=p.querySelector('.next');
    pv.disabled=i===0; nx.disabled=i===pages.length-1;
    pv.addEventListener('click',function(){show(i-1);window.scrollTo(0,0)});
    nx.addEventListener('click',function(){show(i+1);window.scrollTo(0,0)});
  });
  document.getElementById('all').addEventListener('click',function(){
    showAll=!showAll;this.textContent=showAll?'One page at a time':'Show all';show(cur)});
  document.addEventListener('click',function(e){
    var b=e.target.closest('.copybtn'); if(!b)return;
    var node=b.parentNode.querySelector('code'), t=node.textContent;
    function ok(){b.textContent='Copied';b.classList.add('done');
      setTimeout(function(){b.textContent='Copy';b.classList.remove('done')},1400)}
    function fallback(){var r=document.createRange();r.selectNodeContents(node);
      var s=window.getSelection();s.removeAllRanges();s.addRange(r);
      try{document.execCommand('copy');ok()}catch(err){b.textContent='Press Ctrl+C'}}
    if(navigator.clipboard&&window.isSecureContext){navigator.clipboard.writeText(t).then(ok,fallback)}
    else{fallback()}
  });
  var blocks=[].slice.call(document.querySelectorAll(
    '.page > *:not(h2):not(.pager), details.card, details.rare'));
  var q=document.getElementById('search'), count=document.getElementById('count');
  function clearMarks(){
    [].slice.call(document.querySelectorAll('mark')).forEach(function(m){
      m.replaceWith(document.createTextNode(m.textContent))});
    document.normalize()}
  function markIn(el,term){
    var w=document.createTreeWalker(el,NodeFilter.SHOW_TEXT,null),nodes=[],n;
    while(n=w.nextNode())nodes.push(n);
    nodes.forEach(function(t){
      var i=t.data.toLowerCase().indexOf(term);
      if(i<0||t.parentNode.closest('button'))return;
      var m=document.createElement('mark'),after=t.splitText(i);
      after.splitText(term.length);m.textContent=after.data;after.replaceWith(m)})}
  q.addEventListener('input',function(){
    clearMarks();
    var term=q.value.trim().toLowerCase();
    if(!term){blocks.forEach(function(b){b.classList.remove('hidden')});
      count.textContent='';show(cur);return}
    var hits=0;
    pages.forEach(function(p){p.classList.remove('hidden')});
    blocks.forEach(function(b){
      var m=b.textContent.toLowerCase().indexOf(term)>=0;
      if(b.matches('details.rare')){b.classList.toggle('hidden',!m);if(m)b.open=true;return}
      if(b.closest('details.rare')&&!b.matches('details.card'))return;
      b.classList.toggle('hidden',!m);
      if(m){hits++;if(b.matches('details'))b.open=true;markIn(b,term)}});
    pages.forEach(function(p){
      var any=[].slice.call(p.querySelectorAll(
        '.page > *:not(h2):not(.pager), details.card')).some(function(b){
          return !b.classList.contains('hidden')});
      p.classList.toggle('hidden',!any)});
    count.textContent=hits+' match'+(hits===1?'':'es')});
  document.addEventListener('click',function(e){
    var a=e.target.closest('a[href^="#p-"]'); if(!a||a.closest('#nav'))return;
    var i=pages.findIndex(function(p){return '#'+p.id===a.getAttribute('href')});
    if(i>=0&&!showAll){e.preventDefault();show(i);window.scrollTo(0,0)}});
  document.addEventListener('input',function(e){
    if(!e.target.classList.contains('filter'))return;
    var t=e.target.value.trim().toLowerCase();
    [].slice.call(e.target.nextElementSibling.querySelectorAll('details.card'))
      .forEach(function(c){c.classList.toggle('hidden',!!t&&c.textContent.toLowerCase().indexOf(t)<0)})});
  function fromHash(){
    if(!location.hash)return false;
    var i=pages.findIndex(function(p){return '#'+p.id===location.hash});
    if(i<0)return false;
    show(i);return true}
  // A hash arriving after load -- a deep link followed in place, or the back
  // button -- changes no document, so the initial run above would miss it.
  window.addEventListener('hashchange',function(){if(fromHash())window.scrollTo(0,0)});
  var saved=parseInt(load('rnetis-tut-page'),10);
  show(isNaN(saved)?0:saved);
  fromHash();
})();
"""


def render(stamp):
    # sidebar + select
    nav = []
    toc = ['<option value="">Contents\u2026</option>']
    seen = []
    for i, pg in enumerate(PAGES):
        if pg["group"] is None:
            nav.append(
                '<a class="top" href="#%s" data-page="%d">%s</a>'
                % (pg["id"], i, html.escape(pg["nav"]))
            )
            continue
        if not seen or seen[-1] != pg["group"]:
            seen.append(pg["group"])
            nav.append("__GROUP__" + pg["group"])
        nav.append(
            '<a class="sub" href="#%s" data-page="%d">%s</a>'
            % (pg["id"], i, html.escape(pg["nav"]))
        )

    # fold the flat list into groups
    out, i = [], 0
    while i < len(nav):
        if nav[i].startswith("__GROUP__"):
            gname = nav[i][len("__GROUP__"):]
            i += 1
            body = []
            while i < len(nav) and not nav[i].startswith("__GROUP__") and 'class="top"' not in nav[i]:
                body.append(nav[i])
                i += 1
            out.append(
                '<div class="navgrp"><div class="grphead"><span class="grp">%s</span>'
                '<button type="button" class="tog" aria-label="Show or hide %s">\u25b8</button>'
                '</div><div class="grpbody">%s</div></div>'
                % (html.escape(gname), html.escape(gname), "".join(body))
            )
        else:
            out.append(nav[i])
            i += 1

    # grouped <optgroup> for the mobile select.
    #
    # The placeholder has to be first and has to carry value="": show() resets
    # the select to "" on every page change, and a select whose value matches
    # no option renders blank rather than falling back to its first one.
    toc_groups = ['<option value="">Contents…</option>']
    last = None
    for i, pg in enumerate(PAGES):
        if pg["group"] is None:
            if last is not None:
                toc_groups.append("</optgroup>")
                last = None
            toc_groups.append('<option value="%d">%s</option>' % (i, html.escape(pg["nav"])))
            continue
        if pg["group"] != last:
            if last is not None:
                toc_groups.append("</optgroup>")
            toc_groups.append('<optgroup label="%s">' % html.escape(pg["group"]))
            last = pg["group"]
        toc_groups.append('<option value="%d">%s</option>' % (i, html.escape(pg["nav"])))
    if last is not None:
        toc_groups.append("</optgroup>")

    sections = []
    for i, pg in enumerate(PAGES):
        sections.append(
            '<section class="page" id="%s" data-page="%d"><h2>%s</h2>%s'
            '<nav class="pager"><button type="button" class="prev">\u2190 Previous</button>'
            '<button type="button" class="next">Next \u2192</button></nav></section>'
            % (pg["id"], i, inline(pg["title"]), pg["body"])
        )

    logo = data_uri(os.path.join(REPO, "man", "figures", "logo.png"))

    return """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>rnetis tutorial: influence and selection, end to end</title>
<meta name="description" content="A worked tutorial for the rnetis R package: build a netis_data object, fit an Influence model and a Selection model, read the results, and check what the fit assumes. Every result on the page was produced by the code shown above it.">
<link rel="icon" type="image/png" href="%s">
<style>%s</style>
</head>
<body>
<header>
  <div class="brand"><img class="logo" src="%s" alt="">
    <div><h1>rnetis tutorial</h1><div id="stamp">%s</div></div></div>
  <input id="search" type="search" placeholder="Search the tutorial\u2026" aria-label="Search">
  <span id="count"></span>
  <button type="button" id="all">Show all</button>
  <select id="toc" aria-label="Contents">%s</select>
</header>
<div class="wrap">
  <aside><nav id="nav">%s</nav></aside>
  <main>%s</main>
</div>
<script>%s</script>
</body>
</html>
""" % (
        logo,
        CSS,
        logo,
        html.escape(stamp),
        "".join(toc_groups),
        "".join(out),
        "".join(sections),
        JS,
    )


def write(path):
    stamp = "rnetis %s \u00b7 built %s" % (
        VERSION,
        datetime.date.today().isoformat(),
    )
    doc = render(stamp)
    # newline="\n" because .gitattributes declares `* text=auto eol=lf`, so the
    # checked-out file has LF. Writing Windows line endings here would leave the
    # working tree dirty after every rebuild with no change in content, which
    # trains a reader to ignore `git status` on this file.
    with io.open(path, "w", encoding="utf-8", newline="\n") as fh:
        fh.write(doc)
    return doc


def _version():
    with io.open(os.path.join(REPO, "DESCRIPTION"), encoding="utf-8") as fh:
        for line in fh:
            if line.startswith("Version:"):
                return line.split(":", 1)[1].strip()
    return "unknown"


VERSION = _version()
