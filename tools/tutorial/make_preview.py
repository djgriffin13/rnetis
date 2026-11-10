"""Build the third-pass structure as a clickable page, for review.

  python make_preview.py

Writes preview/TUTORIAL-SKELETON.html with its figures beside it. The preview
directory is git-ignored: this is a review artifact, not something to publish.

DELIBERATELY NOT make.py WITH A FLAG. make.py writes TUTORIAL.html into
pkgdown/assets/, which is the published page, and the published page is still
the second pass -- it is what the workshop will see if Pages is ever switched
on. A flag on the same script would be one typo away from overwriting a built
page with a skeleton, and the mistake would be invisible until someone opened
the site. Two scripts, two outputs, no shared flag.
"""

import os
import shutil
import sys

# BEFORE the import: content.py registers its pages as it is imported, so the
# flag has to be in the environment by then.
os.environ["TUTORIAL_DRAFT"] = "1"

import build          # noqa: E402
import content        # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
TARGET = os.path.join(HERE, "preview")

assert content.DRAFT, "the draft flag did not reach content.py"
build.CSS = build.CSS + content.EXTRA_CSS

os.makedirs(TARGET, exist_ok=True)
out_html = os.path.join(TARGET, "TUTORIAL-SKELETON.html")
doc = build.write(out_html)

figdir = os.path.join(TARGET, build.FIGDIR)
os.makedirs(figdir, exist_ok=True)
copied = 0
for cid in build.FIGURES:
    src = build.figure_source(cid)
    dst = os.path.join(figdir, cid + ".png")
    if src and os.path.abspath(src) != os.path.abspath(dst):
        shutil.copy2(src, dst)
        copied += 1

# Which of the fourteen rendered figures this structure has not found a home
# for. Not an error: the structure is allowed to drop a panel. But a figure
# that exists and is shown nowhere is usually a section someone forgot.
all_figs = sorted(
    os.path.splitext(f)[0]
    for f in os.listdir(os.path.join(build.REPO, "pkgdown", "assets", build.FIGDIR))
    if f.endswith(".png")
)
unplaced = [f for f in all_figs if f not in build.FIGURES]

# The hero image on Welcome is served from the site root by pkgdown, which
# this preview is not. Copied in so the page looks like the published one
# rather than showing a broken image the reviewer has to mentally discount.
logo_src = os.path.join(build.REPO, "man", "figures", "logo.png")
if os.path.exists(logo_src):
    shutil.copy2(logo_src, os.path.join(TARGET, "logo.png"))

groups = []
for pg in build.PAGES:
    # None is a top-level entry, NOT a group. Folding it into the first group's
    # count would report four pages under "Getting started" when the sidebar
    # shows two, and the count is the thing being checked.
    g = pg["group"] or "(top level)"
    if not groups or groups[-1][0] != g:
        groups.append([g, 0])
    groups[-1][1] += 1

print("pages:    %d" % len(build.PAGES))
for name, n in groups:
    print("            %-24s %d" % (name, n))
print("diagrams: %d placed" % len(build.DIAGRAMS))
print("figures:  %d placed, %d copied" % (len(set(build.FIGURES)), copied))
print("size:     %.0f KB" % (len(doc.encode("utf-8")) / 1024.0))
print("written:  %s" % out_html)
if unplaced:
    print("UNPLACED figures (%d), rendered but shown nowhere:" % len(unplaced))
    for u in unplaced:
        print("  - %s" % u)
if build.MISSING:
    print("MISSING (%d):" % len(build.MISSING))
    for m in sorted(set(build.MISSING)):
        print("  - %s" % m)
    sys.exit(1)
print("no missing assets")
