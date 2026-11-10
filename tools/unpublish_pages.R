#!/usr/bin/env Rscript
# ---- Remove maintainer pages from the built site ----
# Usage: Rscript tools/unpublish_pages.R [package-dir]
# Build the site with
#   Rscript -e 'pkgdown::clean_site("."); pkgdown::build_site(".", preview = FALSE)' &&
#     Rscript tools/unpublish_pages.R .
# CONTRACT.md (the suite's shared table contract), and DEPLOY.md where a
# package has one, are written for the maintainer. pkgdown 2.0.9 renders
# every top-level .md file as a page and does not read .Rbuildignore, so this
# runs after pkgdown::build_site(). It deletes the pages. It then points the
# rdrr.io links downlit writes for the suite's packages at their own
# reference pages, a step that runs only in a checkout of the whole suite.
# Last, it rebuilds the search index and the sitemap from the pages left.

# ---- Settings ----
pages <- c("CONTRACT.html", "DEPLOY.html")

args <- commandArgs(trailingOnly = TRUE)
pkg_dir <- normalizePath(if (length(args)) args[1] else ".")
docs <- file.path(pkg_dir, "docs")

# ---- Delete the pages ----
for (p in pages) cat(p, "present before:", file.exists(file.path(docs, p)), "\n")
unlink(file.path(docs, pages))

# ---- Point suite reference links at the suite's sites ----
# downlit falls back to rdrr.io for detqc, detfield, arraydesign and
# arraydesignR when it cannot resolve their sites; a script of the suite
# checkout rewrites those links. Run with -I so Python reads no module from
# docs/.
relink <- file.path(dirname(pkg_dir), "tools", "relink_reference.py")
if (!file.exists(relink)) stop("Not found: ", relink)
status <- system2("python3", c("-I", shQuote(relink), shQuote(docs)))
if (status != 0) stop("relink_reference.py exited with status ", status)

# ---- Rebuild search index and sitemap ----
# build_search() rewrites search.json only; the sitemap is rebuilt by the
# internal build_sitemap() of pkgdown 2.0.9, from the HTML files on disk.
pkgdown::build_search(pkg_dir)
utils::getFromNamespace("build_sitemap", "pkgdown")(pkgdown::as_pkgdown(pkg_dir))

# ---- Report what still names the pages ----
mentions <- function(file, page) sum(grepl(page, readLines(file, warn = FALSE), fixed = TRUE))
html <- list.files(docs, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
for (p in pages) {
  cat(p, "present after:", file.exists(file.path(docs, p)), "\n")
  cat("  sitemap.xml lines naming it:", mentions(file.path(docs, "sitemap.xml"), p), "\n")
  cat("  search.json lines naming it:", mentions(file.path(docs, "search.json"), p), "\n")
  cat("  pages linking it:", sum(vapply(html, mentions, numeric(1), page = p) > 0), "of",
      length(html), "\n")
}
