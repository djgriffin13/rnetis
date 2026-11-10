"""Build TUTORIAL.html and copy it, with its figures, into the rnetis repo.

  python make.py            # build into the repo's pkgdown/assets
  python make.py --dry      # build beside this script instead

THE PUBLISHED BUILD. Pages come from content.py, with the draft markers off:
`TUTORIAL_DRAFT` is left unset, so figtodo() renders nothing and a figure that
is specified but not yet drawn leaves no trace on the page. Use
make_preview.py to see what is outstanding.
"""

import os
import shutil
import sys

import build
import content  # noqa: F401  (importing registers the pages, in order)

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = build.REPO
ASSETS = os.path.join(REPO, "pkgdown", "assets")

dry = "--dry" in sys.argv
target_dir = HERE if dry else ASSETS
os.makedirs(target_dir, exist_ok=True)

out_html = os.path.join(target_dir, "TUTORIAL.html")
doc = build.write(out_html)

# figures referenced by the page travel with it
figdir = os.path.join(target_dir, build.FIGDIR)
os.makedirs(figdir, exist_ok=True)
copied = 0
for cid in build.FIGURES:
    src = build.figure_source(cid)
    dst = os.path.join(figdir, cid + ".png")
    if src and os.path.abspath(src) != os.path.abspath(dst):
        shutil.copy2(src, dst)
        copied += 1

# a stray PNG in the asset folder would be published too
stale = [
    f for f in os.listdir(figdir)
    if f.endswith(".png") and f[:-4] not in build.FIGURES
]

# A capture nothing shows is either a block someone meant to put on the page
# and forgot, or a leftover from a chunk that was renamed. Neither is an error,
# but both should be noticed rather than committed silently.
captured = {
    os.path.splitext(f)[0]
    for f in os.listdir(build.OUT)
    if f.endswith(".txt")
}
unused = sorted(captured - set(build.USED))

print("pages:    %d" % len(build.PAGES))
print("captures: %d used" % len(set(build.USED) & captured))
print("figures:  %d copied" % copied)
print("size:     %.0f KB" % (len(doc.encode("utf-8")) / 1024.0))
print("written:  %s" % out_html)
if stale:
    print("STALE figures in %s: %s" % (figdir, ", ".join(stale)))
if unused:
    print("UNUSED captures (%d), shown nowhere on the page:" % len(unused))
    for u in unused:
        print("  - %s" % u)
if build.MISSING:
    print("MISSING captures (%d):" % len(build.MISSING))
    for m in sorted(set(build.MISSING)):
        print("  - %s" % m)
    sys.exit(1)
print("no missing captures")
