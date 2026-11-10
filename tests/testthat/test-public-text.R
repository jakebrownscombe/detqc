test_that("the help of mortality_rules() gives each setting and its default", {
  rd <- help_text("mortality_rules")
  settings <- unique(unlist(lapply(mortality_rules(), function(r) names(r$settings))))
  for (nm in settings) expect_match(rd, nm, fixed = TRUE, info = nm)
})

# ---- The README and DESCRIPTION of the source tree ----
pkg_file <- function(...) {
  f <- testthat::test_path("..", "..", ...)
  testthat::skip_if_not(file.exists(f), "the source tree is not available")
  f
}

test_that("the README states the AI disclosure", {
  txt <- paste(readLines(pkg_file("README.md"), warn = FALSE), collapse = " ")
  # The section is the same in every package of the suite.
  expect_match(gsub("\\s+", " ", txt), paste(
    "The code and this documentation were written with an AI coding assistant,",
    "Claude Code v2.1.145."
  ), fixed = TRUE)
})

test_that("DESCRIPTION names the repository that serves glatos", {
  d <- read.dcf(pkg_file("DESCRIPTION"))
  expect_true("Additional_repositories" %in% colnames(d))
  expect_match(d[1, "Additional_repositories"], "ocean-tracking-network.r-universe.dev",
               fixed = TRUE)
  expect_match(d[1, "Suggests"], "glatos", fixed = TRUE)
})

test_that("every function the README names is an export", {
  txt <- readLines(pkg_file("README.md"), warn = FALSE)
  inline <- unlist(regmatches(txt, gregexpr("`[a-z_]+\\(\\)`", txt)))
  # The example names functions by calling them. Calls through `::`,
  # functions of base R and utils, and helpers the block defines are not
  # detqc's to export.
  code <- txt[cumsum(grepl("^```", txt)) %% 2 == 1]
  calls <- unlist(regmatches(code, gregexpr("(?<![:.\\w])[a-z_]+(?=\\()", code, perl = TRUE)))
  defined <- sub("^\\s*([a-z_]+) <- function.*$", "\\1", grep("<- function", code, value = TRUE))
  named <- unique(c(gsub("[`()]", "", inline), calls))
  named <- setdiff(named, c(ls(baseenv()), ls("package:utils"), defined))
  expect_gt(length(named), 5)
  expect_setequal(setdiff(named, getNamespaceExports("detqc")), character())
})
