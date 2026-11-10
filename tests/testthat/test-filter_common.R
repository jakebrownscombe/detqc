# Edge cases of the checks, the filters, the listening audit, the summary
# and the processing report. Fixtures are the bundled example or written
# here.

utc_t <- function(s) as.POSIXct(s, tz = "UTC")

test_that("the after_tag_life note names only the column that is missing", {
  x <- example_detset()
  x$animals$tag_life_days <- NULL
  s <- check_detset(x, checks = "after_tag_life")$checks$summary
  expect_equal(s$status, "not run")
  expect_equal(s$note, "the animals table has no tag_life_days")
})

test_that("repeats a fraction of a second apart are one detection with a tolerance", {
  ex <- hard_cases()
  late <- ex$detections[1:5, ]
  late$datetime <- late$datetime + 0.496
  x <- detset(rbind(ex$detections, late), ex$deployments, ex$animals)
  # Exact matching finds none of them, as on real GLATOS and VDAT pairs.
  expect_equal(utils::tail(filter_duplicates(x)$log$removed, 1), 0L)
  y <- filter_duplicates(x, tolerance_s = 1)
  expect_equal(utils::tail(y$log$removed, 1), 5L)
  # The earlier stamp of each pair is kept.
  expect_true(all(ex$detections$datetime[1:5] %in% y$detections$datetime))
  expect_error(filter_duplicates(x, by = c("receiver_serial", "transmitter"), tolerance_s = 1),
               "`tolerance_s` needs `datetime` in `by`")
  s <- check_detset(x, checks = "duplicate_rows",
                    settings = check_settings(duplicate_tol_s = 1))$checks$summary
  expect_equal(s$flagged, 5L)
  # Detections from VDAT and a whole-second format: a message names the fix.
  x$meta$sources <- data.frame(table = "detections", file = c("a.csv", "b.csv"),
                               format = c("vdat", "glatos"), rows = 1L)
  expect_message(filter_duplicates(x), "tolerance_s = 1 matches them")
})

test_that("the release and tag-life filters skip, with a message, what they cannot do", {
  x <- example_detset()
  x$animals$tag_life_days <- NA_real_
  n <- 0L
  count_warning <- function(w) {
    n <<- n + 1L
    invokeRestart("muffleWarning")
  }
  withCallingHandlers(expect_message(y <- filter_tag_life(x), paste(
    "filter_tag_life\\(\\) did not run: no transmitter has a tag life"
  )), warning = count_warning)
  expect_equal(n, 0L)
  expect_equal(nrow(y$detections), nrow(x$detections))
  expect_match(utils::tail(y$log$reason, 1), "^not run: no transmitter has a tag life")
  z <- example_detset()
  z$animals$release_datetime <- NULL
  expect_message(filter_post_release(z, hours = 24), "the animals table has no release_datetime")
})

test_that("filter_animals cuts to a species after detset(), as a logged step", {
  tt <- toy_tables()
  det <- tt$det
  det$animal_id <- NA_character_
  ani <- data.frame(animal_id = c("W1", "M1", "W2"), transmitter = c("T-1", "T-1", "T-2"),
                    species = c("Walleye", "Muskellunge", "walleye"),
                    release_datetime = utc_t(c("2024-01-01", "2024-01-12", "2024-01-01")))
  x <- suppressWarnings(detset(det, tt$dep, ani))
  expect_equal(x$detections$animal_id, c("W1", "W1", "M1", "W2", NA))
  y <- filter_animals(x, species = "walleye")
  expect_equal(y$detections$animal_id, c("W1", "W1", "W2"))
  expect_equal(sort(y$animals$animal_id), c("W1", "W2"))
  expect_equal(utils::tail(y$log$step, 1), "filter_animals")
  expect_equal(unlist(utils::tail(y$log$removed_by, 1)), c(no_animal = 1L, other_animal = 1L))
  # Cut before detset(), the reused tag gives the muskellunge's detection to
  # the walleye: the trap the logged step avoids.
  wrong <- detset(det, tt$dep, ani[ani$species != "Muskellunge", ])
  expect_equal(wrong$detections$animal_id[3], "W1")
  z <- filter_animals(x, animals = "M1", transmitters = "T-3")
  expect_equal(z$detections$transmitter, c("T-1", "T-3"))
  expect_error(filter_animals(x), "Give `species`, `animals` or both")
  expect_error(filter_animals(x, species = "pike"),
               "No animal in the animals table is of species pike")
})

test_that("reference tags absent from the detections warn in both steps", {
  x <- example_detset()
  ref <- data.frame(transmitter = c("A69-9999-1", "A69-9999-2"))
  expect_warning(filter_speed(x, max_speed_ms = 3, min_dist_m = 2000, reference_tags = ref),
                 "filter_speed\\(\\): 2 reference tags given; none occurs in the detections")
  expect_warning(audit_listening(x, reference_tags = ref),
                 "audit_listening\\(\\): 2 reference tags given; none occurs")
  # A reference tag that is present gives no such warning.
  ok <- data.frame(transmitter = x$detections$transmitter[1])
  expect_silent(audit_listening(x, reference_tags = ok))
})

test_that("a network export's silent runs warn, and known receivers keep listening", {
  x <- example_detset()
  y <- audit_listening(x, max_silent_days = 1)
  runs <- sum(y$listening$status == "silent_run")
  expect_gt(runs, 0L)
  x$meta$sources <- data.frame(table = "detections", file = "LOALE.csv", format = "glatos",
                               rows = nrow(x$detections))
  expect_warning(z <- audit_listening(x, max_silent_days = 1),
                 sprintf("%s of %s receiver-days are silent_run.*come from GLATOS exports",
                         format(runs, big.mark = ","), format(nrow(y$listening), big.mark = ",")))
  known <- unique(y$listening$receiver_serial[y$listening$status == "silent_run"])
  k <- audit_listening(x, max_silent_days = 1, known_listening = known)
  expect_equal(sum(k$listening$status == "silent_run"), 0L)
  expect_equal(sum(k$listening$status == "silent"),
               sum(y$listening$status %in% c("silent", "silent_run")))
  expect_match(utils::tail(k$log$reason, 1),
               sprintf("%s silent_run days of receivers known to work kept as silent",
                       format(runs, big.mark = ",")))
  expect_error(audit_listening(x, known_listening = data.frame(a = 1)), "`known_listening` must")
})

test_that("processing_report() logs the bands it is given, once", {
  x <- example_detset()
  mb <- quiet_raw(detect_mortality(x))
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, bands = mb, figures = NULL)
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "data-src=\"log:2:step\">detect_mortality</span>", fixed = TRUE)
  rc <- report_check(f, x, mb)
  expect_true(all(rc$traced$match))
  expect_length(rc$untraced, 0L)
  # Bands already logged by the card builder are not logged twice.
  y <- build_mortality_cards(x, mb, withr::local_tempdir())
  g <- withr::local_tempfile(fileext = ".html")
  processing_report(y, g, bands = mb, figures = NULL)
  hg <- paste(readLines(g, warn = FALSE), collapse = "\n")
  expect_match(hg, sprintf("data-src=\"log:n\">%d</span>", nrow(y$log)), fixed = TRUE)
  expect_equal(sum(y$log$step == "detect_mortality"), 1L)
})

test_that("the summary counts listening days by status", {
  x <- audit_listening(example_detset())
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = "timeline")
  txt <- gsub("\\s+", " ", gsub("<[^>]+>", "", paste(readLines(f, warn = FALSE), collapse = " ")))
  st <- table(x$listening$status)
  listening <- sum(x$listening$listening)
  expect_match(txt, sprintf("%s receiver-days at %s stations, by the status audit_listening() ",
                            format(nrow(x$listening), big.mark = ","),
                            length(unique(x$listening$station_id))), fixed = TRUE)
  expect_match(txt, sprintf("%s silent (nothing heard", format(st[["silent"]], big.mark = ",")),
               fixed = TRUE)
  # One word, one count.
  expect_match(txt, sprintf("The audit counts %s of these receiver-days as listening",
                            format(listening, big.mark = ",")), fixed = TRUE)
  expect_false(grepl("receiver-days of listening", txt, fixed = TRUE))
  # The example audit counts no receiver-day as not listening.
  expect_equal(sum(!x$listening$listening), 0L)
  expect_match(txt, sprintf(paste("counts all %s as listening, silent days included, so the",
                                  "figure has no ticks."),
                            format(nrow(x$listening), big.mark = ",")), fixed = TRUE)
  rc <- report_check(f, x)
  expect_true(all(rc$traced$match))
  expect_length(rc$untraced, 0L)
})

test_that("the summary counts the first step's animals after it", {
  ex <- hard_cases()
  d <- ex$detections
  d$animal_id <- NA_character_
  x <- detset(d, ex$deployments, ex$animals)
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL)
  txt <- gsub("\\s+", " ", gsub("<[^>]+>", "", paste(readLines(f, warn = FALSE), collapse = " ")))
  expect_match(txt, sprintf(paste("40,506 detections came in at the first logged step, detset,",
                                  "which left %d animals"), count_animals(x$detections)),
               fixed = TRUE)
  expect_false(grepl("of 0 animals came in", txt, fixed = TRUE))
})

test_that("a wide table's caption sits above its scroll box", {
  x <- filter_duplicates(example_detset())
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL)
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "<p class=\"tcap\">One row per logged step, in order.", fixed = TRUE)
  expect_false(grepl("<div class=\"wide\"><table><caption>", h, fixed = TRUE))
  # The kind of step sits under its name, so the table has nine columns.
  steps <- regmatches(h, regexpr("<p class=\"tcap\">One row per logged step.*?</thead>", h,
                                 perl = TRUE))
  expect_equal(lengths(regmatches(steps, gregexpr("<th[ >]", steps))), 9L)
})

test_that("an open deployment after the last detection draws no zero-length arrow", {
  ex <- hard_cases()
  dep <- ex$deployments
  late <- dep[1, ]
  late$receiver_serial <- "999999"
  late$deploy_datetime <- max(ex$detections$datetime) + 86400
  late$recover_datetime <- as.POSIXct(NA, tz = "UTC")
  x <- detset(ex$detections, rbind(dep, late), ex$animals)
  expect_silent(out <- plot_data(plot_timeline(x)))
  expect_true(any(out$active & out$drawn_to <= out$deploy_datetime))
})
