# Remove the local development notes pkgdown publishes whether or not you
# want it to. Operates on an already-built docs/ directory.
#
#   Rscript tools/clean-site.R
#
# Sourced by tools/build-site.R. It is a separate file so that a deploy
# workflow, if one is ever enabled, runs THE SAME cleanup as a local build.
#
# WHY THIS IS NEEDED. pkgdown renders EVERY .md file in the package root.
# pkgdown:::package_mds() globs `*.md` there and drops only a hardcoded list
# (README, LICENSE, LICENCE, NEWS, 404, the issue/PR templates, cran-comments).
# `.Rbuildignore` excludes these notes from the package and pkgdown never
# consults it. There is no configuration option, which is why this is a
# post-build step.
#
# The notes are git-ignored rather than tracked, but they can still sit on disk
# at the package root, and that is enough for pkgdown to find and publish them.
# Being untracked is not being absent.
INTERNAL_DOCS <- c("CLAUDE")

docs <- "docs"

# BOTH extensions, and the .md is the one that matters: pkgdown writes the
# rendered .html AND copies the .md source next to it, so deleting
# only the HTML leaves the complete text readable at /CLAUDE.md.
targets <- file.path(docs, c(paste0(INTERNAL_DOCS, ".html"),
                             paste0(INTERNAL_DOCS, ".md")))
present <- targets[file.exists(targets)]

if (length(present)) {
  file.remove(present)
  cat("
Removed local development notes from the site:
")
  cat(paste0("  - ", basename(present)), sep = "
")
} else {
  cat("
No local development notes found in the built site.
")
}

# Fail loudly rather than reporting success on a site that still carries one.
# The check is on the built output, not on the removal having been attempted.
remaining <- targets[file.exists(targets)]
if (length(remaining)) {
  stop("Failed to remove: ", paste(basename(remaining), collapse = ", "),
       call. = FALSE)
}

# The generated indexes can outlive the pages they describe -- as an entry
# pointing at a 404, or, worse, as an excerpt of the removed page's content.
# search.json indexes by path, so dropping the matching entries is sufficient.
search_index <- file.path(docs, "search.json")
if (file.exists(search_index)) {
  lines <- readLines(search_index, warn = FALSE)
  pattern <- paste0("(", paste(INTERNAL_DOCS, collapse = "|"), ")[.](html|md)")
  hits <- grep(pattern, lines)
  if (length(hits)) {
    writeLines(lines[-hits], search_index)
    cat("Stripped", length(hits), "reference(s) from search.json
")
  }
}

# llms.txt (pkgdown >= 2.2) is deleted rather than edited. It embeds the
# CONTENT of the root .md files, not merely links to them, so a path-based
# strip removes the links and leaves the prose. Section-stripping a file whose
# structure is not pinned would risk a partial removal that LOOKS clean, which
# is worse than none, because nobody checks a second time. It is an optional
# convenience index, regenerated on every build and referenced by nothing.
llms <- file.path(docs, "llms.txt")
if (file.exists(llms)) {
  file.remove(llms)
  cat("Removed llms.txt (it embeds root .md content, not just links)
")
}

# Catch a NEW local document added later that nobody added to the list
# above. Anything at the site root that is not an expected pkgdown page is
# reported -- not removed, since it may be legitimate, but named so it is a
# decision rather than an oversight.
expected <- c("index.html", "404.html", "authors.html", "LICENSE.html",
              "LICENSE-text.html", "news", "reference", "articles", "deps",
              "search.json", "sitemap.xml",
              # pkgdown >= 2.2 copies the .md source next to each rendered root
              # page, and ships two further favicon forms. These are generated
              # output, not local documents. They were absent when this list was
              # written, so the report below fired on every build -- which
              # trains a reader to skim past it, and is how OWNERSHIP.html went
              # unnoticed in the 2026-11-07 build even though it was named.
              "index.md", "404.md", "authors.md", "LICENSE-text.md",
              "favicon.svg", "favicon-96x96.png",
              "pkgdown.yml", "link.svg", "logo.png", "favicon.ico",
              "apple-touch-icon.png", "apple-touch-icon-120x120.png",
              "apple-touch-icon-152x152.png", "apple-touch-icon-180x180.png",
              "apple-touch-icon-60x60.png", "apple-touch-icon-76x76.png",
              "favicon-16x16.png", "favicon-32x32.png", "CNAME",
              "lightswitch.js", "katex-auto.js", "bootstrap-toc.css",
              "bootstrap-toc.js", "docsearch.css", "docsearch.js", "extra.css",
              "extra.js", "pkgdown.css", "pkgdown.js")
unexpected <- setdiff(list.files(docs), expected)
if (length(unexpected)) {
  cat("
Unexpected files at the site root -- check none is internal:
")
  cat(paste0("  - ", unexpected), sep = "
")
}

cat("
Site built and cleaned.
")
