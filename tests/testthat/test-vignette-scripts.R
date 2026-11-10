# ---- Vignette scripts ----
# Each vignette's code is also a plain R script, vignettes/<name>.R, written
# by tools/purl_vignettes.R and committed, with a copy in inst/scripts/ that
# installs. A script must match its vignette and run on the files the
# package installs, with no error and no warning but those listed in
# expected_warnings below. In a source checkout the
# scripts are read from vignettes/; in R CMD check, from the scripts folder
# of the installed package.

# Scripts that can take 3 s or more run only off CRAN. detqc.R takes up to
# 4 s when it is the first to load detqc's imports; the other three take
# under 2 s.
slow_scripts <- "detqc.R"

# Packages a script needs beyond detqc's imports; none of the four needs
# detfield or arraydesign.
script_needs <- list()
# Warnings a script shows on purpose, as text each must contain. Any other
# warning fails, and so does an expected one that no longer occurs.
# reading-files.R shows the warning for an id with no code space in an OTN
# TRANSMITTER column, and the audit's warning that the example detections
# hold none of the ids.
expected_warnings <- list(
  "reading-files.R" = c("1 receiver_transmitter value is not a transmitter id",
                        "19 reference tags given; none occurs in the detections")
)
# exporting.R runs actel::preload() when actel is installed. actel warns on
# two true properties of the example, which the page explains: the species
# "walleye" is longer than the six characters actel advises for a group
# (actel:::loadBio), and receivers 15 to 20 heard no tag (actel:::
# createStandards). preload() also ends with sink(), which removes the sink
# of the capture.output() below; capture.output() then warns that it has
# none to remove. knitr restores its sink, so the page shows no such warning.
actel_warnings <- c("Long group names detected",
                    "No detections were found for receiver(s) 15, 16, 17, 18, 19, 20.",
                    "no sink to remove")
if (requireNamespace("actel", quietly = TRUE)) {
  expected_warnings[["exporting.R"]] <- actel_warnings
}

# The scripts the four pages ship. A page added or cut changes this list,
# so a missing script fails here instead of dropping out of the loop below.
expected_scripts <- c("detqc.R", "exporting.R", "mortality-review.R",
                      "reading-files.R")

source_vignettes <- function() {
  testthat::test_path("..", "..", "vignettes")
}

vignette_scripts <- function() {
  src <- source_vignettes()
  dir <- if (dir.exists(src)) src else system.file("scripts", package = "detqc")
  normalizePath(list.files(dir, pattern = "\\.R$", full.names = TRUE))
}

test_that("every vignette has a script", {
  scripts <- vignette_scripts()
  expect_setequal(basename(scripts), expected_scripts)
  src <- source_vignettes()
  skip_if_not(dir.exists(src), "vignette sources are not beside the tests")
  rmds <- list.files(src, pattern = "\\.Rmd$")
  expect_setequal(sub("\\.Rmd$", ".R", rmds), basename(scripts))
})

test_that("each committed script matches its vignette", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  src <- source_vignettes()
  tool <- test_path("..", "..", "tools", "purl_vignettes.R")
  skip_if_not(dir.exists(src) && file.exists(tool), "vignette sources are not beside the tests")
  env <- new.env()
  sys.source(tool, envir = env)
  out_dir <- withr::local_tempdir()
  fresh <- env$purl_vignettes(dirname(src), out_dir = out_dir, install_dir = NULL)
  installed <- file.path(dirname(src), "inst", "scripts")
  for (f in fresh) {
    for (committed in file.path(c(src, installed), basename(f))) {
      expect_true(file.exists(committed), info = committed)
      expect_identical(readLines(committed), readLines(f), info = committed)
    }
  }
})

for (script in vignette_scripts()) {
  test_that(paste("vignette script", basename(script), "runs without error or warning"), {
    if (basename(script) %in% slow_scripts) skip_on_cran()
    for (pkg in script_needs[[basename(script)]]) skip_if_not_installed(pkg)
    withr::local_dir(withr::local_tempdir())
    grDevices::pdf(NULL)
    withr::defer(grDevices::dev.off())
    # The scripts show the errors a reader will meet inside try(); their text
    # goes to a null connection, which keeps the test log clean.
    sink_to <- file(nullfile(), open = "w")
    withr::defer(close(sink_to))
    withr::local_options(try.outFile = sink_to)
    warned <- character()
    expect_error(
      withCallingHandlers(
        utils::capture.output(source(script, local = new.env(parent = globalenv()))),
        warning = function(w) {
          warned <<- c(warned, conditionMessage(w))
          invokeRestart("muffleWarning")
        },
        message = function(m) invokeRestart("muffleMessage")
      ),
      NA
    )
    expected <- expected_warnings[[basename(script)]]
    known <- vapply(warned, function(w) any(vapply(expected, grepl, TRUE, x = w, fixed = TRUE)),
                    TRUE)
    expect_identical(unname(warned[!known]), character())
    for (e in expected) expect_true(any(grepl(e, warned, fixed = TRUE)), info = e)
  })
}

# tools/vignette_screenshots.R records the numbers the report screenshot
# shows; the report the export article's script writes must give the same,
# so the screenshot cannot show fewer logged steps than the page.
test_that("the report screenshot matches the report the export script writes", {
  src <- source_vignettes()
  tool <- test_path("..", "..", "tools", "vignette_screenshots.R")
  shots <- file.path(src, "figures", "screenshots.json")
  skip_if_not(file.exists(tool) && file.exists(shots), "screenshot record is not beside the tests")
  script <- normalizePath(file.path(src, "exporting.R"))
  shots <- normalizePath(shots)
  tools_env <- new.env()
  sys.source(tool, envir = tools_env)
  withr::local_dir(withr::local_tempdir())
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  sink_to <- file(nullfile(), open = "w")
  withr::defer(close(sink_to))
  withr::local_options(try.outFile = sink_to)
  env <- new.env(parent = globalenv())
  # The warnings the test above allows for this script are muffled here;
  # any other still reaches the test log.
  allowed <- expected_warnings[["exporting.R"]]
  withCallingHandlers(
    suppressMessages(utils::capture.output(source(script, local = env))),
    warning = function(w) {
      if (any(vapply(allowed, grepl, TRUE, x = conditionMessage(w), fixed = TRUE))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  shown <- jsonlite::read_json(shots, simplifyVector = TRUE)$processing_report_first_screen.png
  now <- tools_env$report_values(env$report)
  expect_identical(sort(now[!startsWith(now, "sys:detqc=")]),
                   sort(shown$values[!startsWith(shown$values, "sys:detqc=")]))
})

# An inline expression broken after its opening "`r" is not run, and the
# page prints its code as text. purl drops inline code, so the script test
# cannot see it.
test_that("no inline R expression is split across lines", {
  src <- source_vignettes()
  skip_if_not(dir.exists(src), "vignette sources are not beside the tests")
  for (rmd in list.files(src, pattern = "\\.Rmd$", full.names = TRUE)) {
    expect_false(any(grepl("`r\\s*$", readLines(rmd))), info = basename(rmd))
  }
})

# Without build_vignettes the pages are stubs or missing. detqc installs
# alone (its pages need no other positionR package), so the README's
# install block installs detqc only. The whole suite is one sentence linking
# the positionR landing page, with no install line of its own.
test_that("the README installs detqc alone, with its vignettes", {
  src <- source_vignettes()
  readme <- file.path(dirname(src), "README.md")
  skip_if_not(file.exists(readme), "package sources are not beside the tests")
  lines <- readLines(readme)
  expect_true(any(grepl('install.packages("remotes")', lines, fixed = TRUE)))
  gh <- grep("install_github\\(", lines, value = TRUE)
  pkgs <- sub("^.*install_github\\(\"jakebrownscombe/([^\"]+)\".*$", "\\1", gh)
  expect_identical(pkgs, "detqc")
  expect_true(any(grepl("](https://jakebrownscombe.github.io/positionR/)", lines, fixed = TRUE)))
  expect_false(any(grepl("go public", lines, fixed = TRUE)))
  expect_match(gh[1], "build_vignettes = TRUE", fixed = TRUE)
  # The vignettes need terra, sf and knitr, which are Suggests, so a clean R
  # builds them only with dependencies = TRUE.
  at <- grep("install_github(\"jakebrownscombe/detqc\"", lines, fixed = TRUE)
  expect_match(paste(lines[at + 0:1], collapse = " "), "dependencies = TRUE", fixed = TRUE)
  others <- c("detfield", "arraydesign", "arraydesignR")
  expect_false(any(grepl(paste0("(positionR|jakebrownscombe)/(", paste(others, collapse = "|"),
                                ")\\b"), lines)))
  expect_false(any(grepl("positionR/detqc", lines, fixed = TRUE)))
})
