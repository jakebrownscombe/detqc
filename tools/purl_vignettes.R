#!/usr/bin/env Rscript
# ---- Vignette scripts ----
# Usage: Rscript tools/purl_vignettes.R [package-dir]
# Writes vignettes/<name>.R from each vignettes/<name>.Rmd with knitr::purl(),
# so that every vignette is also a plain R script. Chunks marked purl = FALSE
# (the knitr setup) are left out. Each chunk label becomes a section header
# in the `# ---- Name ----` form. The same script is copied to
# inst/scripts/, which installs, so that a reader finds it with
# system.file("scripts", package = ...) whether or not vignettes were built.
# Run it after editing a vignette and commit all three files;
# tests/testthat/test-vignette-scripts.R fails when a committed script
# differs from its vignette.

purl_one <- function(rmd, out) {
  raw <- tempfile(fileext = ".R")
  on.exit(unlink(raw))
  knitr::purl(rmd, output = raw, documentation = 1, quiet = TRUE)
  lines <- readLines(raw, warn = FALSE, encoding = "UTF-8")
  is_head <- grepl("^## ----", lines)
  label <- sub("^## -+([^,-][^,]*?)?(,.*)?-*$", "\\1", lines[is_head], perl = TRUE)
  label <- trimws(gsub("-+$", "", label))
  label <- gsub("-", " ", label)
  label <- paste0(toupper(substr(label, 1, 1)), substring(label, 2))
  lines[is_head] <- ifelse(nzchar(label), sprintf("# ---- %s ----", label), NA_character_)
  lines <- lines[!is.na(lines)]
  # Runs of blank lines left by knitr collapse to one.
  blank <- !nzchar(trimws(lines))
  lines <- lines[!(blank & c(FALSE, blank[-length(blank)]))]
  while (length(lines) && !nzchar(trimws(lines[1]))) lines <- lines[-1]
  while (length(lines) && !nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
  title <- rmarkdown::yaml_front_matter(rmd)$title
  head <- c(
    sprintf("# Code of the vignette \"%s\".", title),
    sprintf("# Extracted from vignettes/%s by tools/purl_vignettes.R, a repository file", basename(rmd)),
    "# the installed package does not carry. The vignette is the source; this file follows it.",
    ""
  )
  writeLines(c(head, lines), out, useBytes = TRUE)
  invisible(out)
}

purl_vignettes <- function(pkg_dir = ".", out_dir = file.path(pkg_dir, "vignettes"),
                           install_dir = file.path(pkg_dir, "inst", "scripts")) {
  rmds <- list.files(file.path(pkg_dir, "vignettes"), pattern = "\\.Rmd$", full.names = TRUE)
  outs <- file.path(out_dir, sub("\\.Rmd$", ".R", basename(rmds)))
  for (i in seq_along(rmds)) purl_one(rmds[i], outs[i])
  if (!is.null(install_dir)) {
    dir.create(install_dir, showWarnings = FALSE, recursive = TRUE)
    file.copy(outs, install_dir, overwrite = TRUE)
    outs <- c(outs, file.path(install_dir, basename(outs)))
  }
  invisible(outs)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  outs <- purl_vignettes(if (length(args)) args[1] else ".")
  cat(sprintf("wrote %s\n", outs), sep = "")
}
