test_that("review_mortality() builds when there is no index, then opens it", {
  x <- example_detset()
  dir <- withr::local_tempdir()
  opened <- NULL
  withr::local_options(browser = function(url) opened <<- url)
  expect_message(y <- review_mortality(x, dir, open = TRUE), "default rules of\ndetect_mortality")
  expect_match(opened, "index.html$")
  expect_true(file.exists(file.path(dir, "index.html")))
  expect_equal(detset_log(y)$step, c("detset", "detect_mortality", "build_mortality_cards"))
  b <- detect_mortality(x)$bands
  expect_equal(nrow(y$cards), sum(b$band %in% c("dead", "review")))
  # An index built from these detections is opened as it stands, and the
  # opening is logged.
  z <- review_mortality(x, dir, open = FALSE)
  expect_equal(detset_log(z)$step, c("detset", "review_mortality"))
  expect_match(detset_log(z)$reason[2], "opened the review in .* no card was rewritten")
  expect_equal(detset_log(z)$removed[2], 0L)
  expect_null(z$cards)
})

test_that("review_mortality() stops on a stale review instead of rebuilding it at the defaults", {
  ex <- hard_cases()
  x <- example_detset()
  dir <- withr::local_tempdir()
  expect_message(y <- review_mortality(x, dir, open = FALSE), "at the default rules")
  two <- c("EX-01", "EX-02")
  x2 <- detset(ex$detections[ex$detections$animal_id %in% two, ], ex$deployments,
               ex$animals[ex$animals$animal_id %in% two, ])
  expect_error(review_mortality(x2, dir, open = FALSE),
               "the index was built from other detections.*bands = detect_mortality\\(x")
  # With bands, the review is rebuilt from them.
  y2 <- review_mortality(x2, dir, bands = detect_mortality(x2), open = FALSE)
  b2 <- detect_mortality(x2)$bands
  expect_equal(nrow(y2$cards), sum(b2$band %in% c("dead", "review")))
  expect_equal(length(list.files(file.path(dir, "cards"))), nrow(y2$cards))
  expect_equal(length(read_payload(file.path(dir, "index.html"))$rows$id), 2L)
  # all = TRUE on an index without alive-band cards stops too, and says how.
  dir5 <- withr::local_tempdir()
  suppressMessages(review_mortality(x, dir5, open = FALSE))
  expect_null(review_mortality(x, dir5, open = FALSE)$cards)
  expect_error(review_mortality(x, dir5, all = TRUE, open = FALSE),
               "all = TRUE asks for alive-band cards.*all = TRUE\\)")
  # An index with no key is not rebuilt either.
  idx <- file.path(dir5, "index.html")
  h <- readLines(idx, warn = FALSE)
  writeLines(gsub("\"detset_key\":\"[0-9a-f]+\",", "", h), idx)
  expect_null(read_payload(idx)$detset_key)
  expect_error(review_mortality(x, dir5, open = FALSE), "carries no key of its detections")
})

test_that("cards built with the user's rules survive new detections", {
  x <- example_detset()
  dir <- withr::local_tempdir()
  rules <- mortality_rules(drop = c("record_end", "persistence"))
  mine <- detect_mortality(x, rules)
  y <- review_mortality(x, dir, bands = mine, all = TRUE, open = FALSE)
  alive <- mine$bands$animal_id[mine$bands$band == "alive"]
  expect_gt(length(alive), 0)
  x2 <- x
  x2$detections <- x2$detections[-(1:10), ]
  calls <- 0L
  trace_env <- environment()
  testthat::local_mocked_bindings(detect_mortality = function(...) {
    trace_env$calls <- trace_env$calls + 1L
    stop("detect_mortality() must not run")
  })
  expect_error(review_mortality(x2, dir, all = TRUE, open = FALSE), "other detections")
  expect_equal(calls, 0L)
  # The user's alive-band cards are still in the index.
  ids <- read_payload(file.path(dir, "index.html"))$rows$id
  expect_true(all(alive %in% ids))
})

test_that("review_mortality() rebuilds from given bands, with every band when all = TRUE", {
  x <- example_detset()
  mb <- detect_mortality(x)
  dir <- withr::local_tempdir()
  y <- review_mortality(x, dir, bands = mb, all = TRUE, open = FALSE,
                        control = list(title = "All bands"))
  expect_equal(nrow(y$cards), 20L)
  expect_equal(read_payload(attr(y$cards, "index"))$title, "All bands")
  expect_error(review_mortality(x, c("a", "b")), "one folder path")
  expect_error(review_mortality(list(), dir), "must be a detset")
})
