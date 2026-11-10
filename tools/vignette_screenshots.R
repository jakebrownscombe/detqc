#!/usr/bin/env Rscript
# ---- Vignette screenshots ----
# Usage: Rscript tools/vignette_screenshots.R [package-dir]
# Re-shoots the five browser screenshots in vignettes/figures from the
# reports and the review cards that the vignette scripts and the site build
# write, and records in vignettes/figures/screenshots.json the report
# numbers the report screenshot shows. tests/testthat/test-vignette-scripts.R
# compares those numbers with the report the script writes, so a screenshot
# that differs from its page fails the tests. Run it after any change to the
# report, the cards or the vignette code before them. Needs Google Chrome
# (or the browser named in the CHROME variable), node and magick.

# The numbers of the report's header and summary, the part the screenshot
# shows, as "key=value" from each data-src span. The time it was written
# is left out.
report_values <- function(file) {
  h <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  top <- sub('<section id="inputs".*$', "", h)
  m <- regmatches(top, gregexpr('data-src="[^"]+">[^<]*<', top))[[1]]
  key <- sub('^data-src="([^"]+)">.*$', "\\1", m)
  val <- sub('^.*">([^<]*)<$', "\\1", m)
  out <- unique(paste(key, val, sep = "="))
  out[!startsWith(out, "meta:created=") & !startsWith(out, "sys:generated=")]
}

# Runs a vignette script with its files written to `out_dir`.
run_script <- function(script, out_dir) {
  lines <- readLines(script, warn = FALSE)
  lines <- sub("^out_dir <- tempdir\\(\\)", sprintf("out_dir <- %s", deparse(out_dir)), lines)
  env <- new.env(parent = globalenv())
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  utils::capture.output(suppressMessages(eval(parse(text = lines), envir = env)))
  env
}

# Device pixel ratio of the shots. 1.25 keeps text sharp at the width of the
# vignette column. The three shots ship twice (in vignettes/figures and inside
# the built HTML); at 1.5 they take the tarball past its 2 MB limit.
shot_scale <- 1.25

# Each PNG is reduced to 128 colours with no dithering, which keeps text
# sharp and the package tarball under its 2 MB limit; the smaller file is
# kept. At 256 colours the card panels took 196 kB, at 128 colours 72 kB.
shoot <- function(page, out, width, scale, prepare, clip, helper) {
  res <- system2("node", c(helper, shQuote(page), shQuote(out), width, scale, shQuote(prepare),
                           shQuote(clip)), stdout = TRUE)
  img <- magick::image_quantize(magick::image_read(out), max = 128, dither = FALSE)
  small <- tempfile(fileext = ".png")
  magick::image_write(img, small, format = "png", depth = 8)
  if (file.size(small) < file.size(out)) file.copy(small, out, overwrite = TRUE)
  cat(res, sprintf("%s: %s bytes kept", basename(out), format(file.size(out), big.mark = ",")),
      sep = "\n")
  invisible(res)
}

# Moves a card's cursor to `day` and its window to `win` days, and hides the
# trailing-window table, which runs past the right edge of a window narrower
# than about 1,400 px.
card_prepare <- function(day, win) {
  sprintf(paste0(
    "(() => { const s = document.getElementById('slider');",
    " s.value = 0; s.dispatchEvent(new Event('input'));",
    " const d0 = new Date(document.getElementById('curdate').textContent + 'T00:00:00Z');",
    " s.value = Math.round((new Date('%sT00:00:00Z') - d0) / 864e5);",
    " s.dispatchEvent(new Event('input'));",
    " [...document.querySelectorAll('button.win')].find(b => b.textContent.trim() === '%d d')",
    ".click();",
    " document.querySelector('div.readout').style.visibility = 'hidden';",
    " return document.getElementById('curdate').textContent; })()"), day, win)
}

box_js <- function(top, bottom, pad = 8) {
  sprintf(paste0(
    "(() => { const a = document.querySelector('%s').getBoundingClientRect();",
    " const b = document.querySelector('%s').getBoundingClientRect();",
    " return {x: Math.max(0, a.x - %d), y: Math.max(0, a.y - %d),",
    " width: Math.max(a.width, b.width) + 2 * %d, height: b.bottom - a.y + 2 * %d}; })()"),
    top, bottom, pad, pad, pad, pad)
}

# The card is the example's animal ST-02, the walleye that dies on 4 June,
# with its cursor the day after and a 7-day window.
vignette_screenshots <- function(pkg_dir = ".", card = "ST-02", day = "2024-06-05", win = 7) {
  pkg_dir <- normalizePath(pkg_dir)
  pkgload::load_all(pkg_dir, quiet = TRUE)
  helper <- file.path(pkg_dir, "tools", "screenshot_clip.mjs")
  fig_dir <- file.path(pkg_dir, "vignettes", "figures")
  out_dir <- tempfile("shots_")
  dir.create(out_dir)
  run_script(file.path(pkg_dir, "vignettes", "exporting.R"), out_dir)
  report <- file.path(out_dir, "processing_report.html")
  shoot(report, file.path(fig_dir, "processing_report_first_screen.png"), 820, shot_scale, "",
        "(() => { const b = document.getElementById('summary').getBoundingClientRect();
          return {x: 0, y: 0, width: 820, height: b.bottom + 16}; })()", helper)
  # The Get started page links the report its own run writes to the site,
  # pkgdown/assets/example, which logs one step more (the review cards);
  # its screenshot is shot from that file so that image and link agree.
  # That file is stale unless pkgdown::build_site() ran before this script.
  site_report <- file.path(pkg_dir, "pkgdown", "assets", "example", "processing_report.html")
  shoot(site_report, file.path(fig_dir, "processing_report_get_started.png"), 820, shot_scale, "",
        "(() => { const b = document.getElementById('summary').getBoundingClientRect();
          return {x: 0, y: 0, width: 820, height: b.bottom + 16}; })()", helper)
  run_script(file.path(pkg_dir, "vignettes", "mortality-review.R"), out_dir)
  page <- file.path(out_dir, "mortality_review", "cards", paste0(card, ".html"))
  # The clip ends at the map card: div.upper also holds the hidden window table,
  # which on a short record would leave a blank band below the map.
  shoot(page, file.path(fig_dir, "review_card_top.png"), 1000, shot_scale, card_prepare(day, win),
        box_js("h1", "div.upper > div"), helper)
  # The panels are shot in a window as wide as the vignette column, 700 px,
  # so their labels show at the size the card gives them. Shot at 1,000 px
  # and shrunk to the column, the station lanes read at 6 to 7 px. The clip
  # ends below the key, which names the marks the caption leaves out.
  shoot(page, file.path(fig_dir, "review_card_panels.png"), 700, shot_scale, card_prepare(day, win),
        box_js("#stickyzone", "#stackkey"), helper)
  # The Get started page links the cards its own run writes to the site; the
  # mortality article adds a rule stream of its own, so its card differs.
  # The Get started screenshot is shot from the site's card, so that image
  # and link are the same card. Build the site first so that it is current.
  site_card <- file.path(pkg_dir, "pkgdown", "assets", "example", "mortality_review", "cards",
                         paste0(card, ".html"))
  shoot(site_card, file.path(fig_dir, "review_card_get_started.png"), 1000, shot_scale,
        card_prepare(day, win), box_js("h1", "div.upper > div"), helper)
  rec <- list(
    processing_report_first_screen.png = list(script = "exporting.R",
                                              report = "processing_report.html",
                                              values = report_values(report)),
    processing_report_get_started.png = list(script = "detqc.R",
                                             report = "pkgdown/assets/example/processing_report.html",
                                             values = report_values(site_report)),
    review_card_top.png = list(script = "mortality-review.R", card = card, cursor = day,
                               window_days = win),
    review_card_panels.png = list(script = "mortality-review.R", card = card, cursor = day,
                                  window_days = win),
    review_card_get_started.png = list(script = "detqc.R",
                                       page = "pkgdown/assets/example/mortality_review/cards",
                                       card = card, cursor = day, window_days = win)
  )
  jsonlite::write_json(rec, file.path(fig_dir, "screenshots.json"), pretty = TRUE,
                       auto_unbox = TRUE)
  invisible(rec)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  rec <- vignette_screenshots(if (length(args)) args[1] else ".")
  cat(rec$processing_report_first_screen.png$values, sep = "\n")
}
