# Shared helpers for the card, review and store tests.

# Runs browser/card_check.mjs on a card (and an index, or "") in headless
# Chrome and returns its JSON result as a list. Skips where Chrome or node is
# missing.
run_card_check <- function(card, index = "") {
  chrome <- find_chrome()
  node <- Sys.which("node")
  testthat::skip_if(!nzchar(chrome) || !nzchar(node), "headless Chrome and node are needed")
  port <- as.character(9400L + sample.int(500L, 1L))
  res <- system2(node, c(shQuote(testthat::test_path("browser", "card_check.mjs")), shQuote(card),
                         shQuote(index), shQuote(chrome), port), stdout = TRUE, stderr = FALSE)
  jsonlite::fromJSON(utils::tail(grep("^\\{", res, value = TRUE), 1), simplifyVector = TRUE)
}

write_calls <- function(df, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  utils::write.csv(df, f, row.names = FALSE)
  f
}

# The example detset and its bands, built once per test file.
example_review <- function(dir, ...) {
  x <- example_detset()
  mb <- quiet_raw(detect_mortality(x))
  list(x = x, bands = mb, y = build_mortality_cards(x, mb, dir, ...))
}
