test_that("apply_mortality_calls() stores calls, drops dates on alive calls and logs", {
  x <- example_detset()
  calls <- data.frame(
    animal_id = c("EX-01", "EX-04", "EX-05", "EX-06", "EX-07"),
    band = "dead", call = c("dead_or_shed", "alive", "uncertain", "dead_or_shed", ""),
    death_date = c("2023-07-15", "2023-06-01", "", "", ""),
    date_source = c("cursor", "typed", "", "", ""),
    note = c("flat, \"quiet\"", "", "small range", "", "x"), annotated_at = "2026-10-29T10:00:00Z"
  )
  f <- write_calls(calls)
  expect_warning(y <- apply_mortality_calls(x, f),
                 "1 dead_or_shed call has no death date and will remove nothing: EX-06")
  m <- y$mortality
  expect_named(m, c("animal_id", "call", "death_date", "death_time", "date_source", "note",
                    "band", "annotated_at", "file", "md5"))
  expect_equal(nrow(m), 4L)
  expect_equal(m$death_date[m$animal_id == "EX-01"], as.Date("2023-07-15"))
  expect_equal(m$date_source[m$animal_id == "EX-01"], "cursor")
  expect_equal(m$note[m$animal_id == "EX-01"], "flat, \"quiet\"")
  expect_true(is.na(m$death_date[m$animal_id == "EX-04"]))
  expect_true(is.na(m$date_source[m$animal_id == "EX-04"]))
  expect_equal(unique(m$md5), unname(tools::md5sum(f)))
  lg <- detset_log(y)
  expect_equal(lg$step[2], "apply_mortality_calls")
  expect_match(lg$reason[2], paste(
    "4 calls read: 1 alive, 2 dead_or_shed, 1 uncertain; 1 death date dropped from alive",
    "calls; 1 dead_or_shed call with no date; 0 death dates before the first detection;",
    "no candidate_date column; 1 row with no call"
  ))
  expect_equal(lg$removed[2], 0L)
  expect_output(print(y), "mortality calls: 4 animals")
  # A second file replaces the calls it names and keeps the rest.
  f2 <- write_calls(data.frame(animal_id = "EX-05", call = "alive", annotated_at = "2026-10-30"))
  z <- apply_mortality_calls(y, f2)
  expect_equal(nrow(z$mortality), 4L)
  expect_equal(z$mortality$call[z$mortality$animal_id == "EX-05"], "alive")
})

test_that("a store that lacks date_source and band takes new calls", {
  x <- example_detset()
  x$mortality <- data.frame(animal_id = "EX-02", call = "alive", death_date = as.Date(NA),
                            note = NA_character_, annotated_at = NA_character_, file = "old.csv",
                            md5 = "0")
  y <- apply_mortality_calls(x, write_calls(data.frame(animal_id = "EX-01", call = "alive")))
  expect_equal(y$mortality$animal_id, c("EX-01", "EX-02"))
  expect_true(all(is.na(y$mortality$date_source)))
})

test_that("the candidate date column counts calls far from the candidate", {
  x <- example_detset()
  f <- write_calls(data.frame(animal_id = c("EX-01", "EX-02"), call = "dead_or_shed",
                              death_date = c("2023-07-15", "2023-05-02"),
                              candidate_date = c("2023-07-15", "2023-08-10")))
  far <- "1 death date falls more than 30 days before the candidate date.*: EX-02\\.$"
  expect_warning(expect_warning(y <- apply_mortality_calls(x, f),
                                "before the animal's first detection: EX-02"), far)
  expect_match(detset_log(y)$reason[2],
               "1 dated call more than 30 days from the candidate date \\(of 2 with both\\)")
  # A date stamped near the start of a long record, as a slip of the cursor
  # stamps one, is named; a date 5 days after the candidate is not.
  slip <- write_calls(data.frame(animal_id = c("EX-01", "EX-03"), call = "dead_or_shed",
                                 death_date = c("2023-05-06", "2023-08-23"),
                                 candidate_date = c("2023-07-15", "2023-08-18")))
  want <- paste("1 death date falls more than 30 days before the candidate date, and",
                "filter_mortality\\(\\) will remove the detections between them: EX-01\\.$")
  expect_warning(z <- apply_mortality_calls(x, slip), want)
  expect_match(detset_log(z)$reason[2],
               "1 dated call more than 30 days from the candidate date \\(of 2 with both\\)")
  late <- write_calls(data.frame(animal_id = "EX-01", call = "dead_or_shed",
                                 death_date = "2023-09-30", candidate_date = "2023-07-15"))
  expect_no_warning(apply_mortality_calls(x, late))
  early <- write_calls(data.frame(animal_id = "EX-01", call = "dead_or_shed",
                                  death_date = "2020-01-01"))
  expect_warning(apply_mortality_calls(x, early),
                 "1 death date falls before the animal's first detection: EX-01")
})

test_that("apply_mortality_calls() keeps the latest of repeated rows", {
  x <- example_detset()
  f <- write_calls(data.frame(animal_id = c("EX-01", "EX-01"), call = c("alive", "dead_or_shed"),
                              death_date = c("", "2023-07-16"),
                              annotated_at = c("2026-10-29T10:00:00Z", "2026-10-29T11:00:00Z")))
  m <- apply_mortality_calls(x, f)$mortality
  expect_equal(nrow(m), 1L)
  expect_equal(m$call, "dead_or_shed")
})

test_that("apply_mortality_calls() refuses a bad file", {
  x <- example_detset()
  expect_error(apply_mortality_calls(x, tempfile()), "File not found")
  expect_error(apply_mortality_calls(x, write_calls(data.frame(id = "EX-01", call = "alive"))),
               "needs the columns animal_id and call")
  expect_error(apply_mortality_calls(x, write_calls(data.frame(animal_id = "EX-01",
                                                               call = "dead"))),
               "call \"dead\" is not one of alive, dead_or_shed, uncertain")
  expect_error(apply_mortality_calls(x, write_calls(data.frame(animal_id = c("EX-01", "NOPE"),
                                                               call = "alive"))),
               "1 animal is not in the detset, such as \"NOPE\"")
  bad_date <- data.frame(animal_id = "EX-01", call = "dead_or_shed", death_date = "15/07/2023")
  expect_error(apply_mortality_calls(x, write_calls(bad_date)),
               "death_date \"15/07/2023\" is not a date")
})

test_that("filter_mortality() removes the detections after each death day only", {
  x <- example_detset()
  expect_error(filter_mortality(x), "apply_mortality_calls\\(\\) first")
  f <- write_calls(data.frame(animal_id = c("EX-01", "EX-03", "EX-05", "EX-09"),
                              call = c("dead_or_shed", "dead_or_shed", "uncertain", "dead_or_shed"),
                              death_date = c("2023-07-15", "", "", "2023-12-01")))
  y <- filter_mortality(suppressWarnings(apply_mortality_calls(x, f)), margin_days = 0)
  d <- x$detections
  expected <- sum(d$animal_id == "EX-01" & as.Date(d$datetime) > as.Date("2023-07-15"))
  expect_equal(nrow(y$detections), nrow(d) - expected)
  kept <- y$detections[y$detections$animal_id == "EX-01", ]
  expect_equal(max(as.Date(kept$datetime)), as.Date("2023-07-15"))
  expect_equal(sum(y$detections$animal_id == "EX-03"), sum(d$animal_id == "EX-03"))
  lg <- detset_log(y)
  expect_equal(lg$step, c("detset", "apply_mortality_calls", "filter_mortality"))
  expect_equal(lg$removed[3], expected)
  expect_equal(lg$removed_by[[3]], c(after_death_date = expected))
  expect_equal(lg$rows_after[3], lg$rows_before[3] - expected)
  expect_match(lg$reason[3], "of 1 animal called dead_or_shed removed")
  expect_match(lg$reason[3], "1 animal called dead_or_shed with a date had none after it")
  expect_match(lg$reason[3], paste("1 animal called dead_or_shed without a date kept whole;",
                                   "1 animal called uncertain"))
  expect_identical(validate_detset(y), y)
})

test_that("the bundled example's planted dead reach the dead band with and without min-lag", {
  # The positionRtools example fixture is too sparse for this (median 18.8 h between
  # detections on one receiver, so min-lag at 3,600 s removes 94.69% of it). The
  # hard-case bench, hard_cases(), is built for it: 11 planted dead of 20 animals.
  ex <- hard_cases()
  dead <- ex$cases$animal_id[!is.na(ex$cases$true_death_date)]
  expect_length(dead, 11L)
  x <- example_detset()
  for (y in list(x, filter_min_lag(x, max_lag_s = 3600))) {
    expect_warning_fresh(b <- detect_mortality(y)$bands, "cast no sensor vote")
    expect_setequal(b$animal_id[b$band == "dead"], dead)
    when <- ex$cases$true_death_date[match(dead, ex$cases$animal_id)]
    calls <- data.frame(animal_id = dead, call = "dead_or_shed", death_date = format(when))
    z <- filter_mortality(apply_mortality_calls(y, write_calls(calls)), margin_days = 0)
    d <- z$detections
    dd <- z$mortality$death_date[match(d$animal_id, z$mortality$animal_id)]
    expect_equal(sum(!is.na(dd) & as.Date(d$datetime) > dd), 0L)
    expect_gt(detset_log(z)$removed[nrow(detset_log(z))], 0L)
  }
})

test_that("a call with a death time cuts at the time; a date keeps the death day", {
  ex <- example_mortality
  x <- detset(ex$detections, ex$deployments, ex$animals)
  death <- ex$fates$death_datetime[ex$fates$animal_id == "ST-02"]
  expect_equal(format(death, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2024-06-04 02:00:00")
  f <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(animal_id = "ST-02", call = "dead_or_shed",
                              death_time = "2024-06-04T02:00:00Z"), f, row.names = FALSE)
  timed <- filter_mortality(apply_mortality_calls(x, f), margin_days = 0)
  expect_equal(timed$mortality$death_date, as.Date("2024-06-04"))
  st02 <- timed$detections[timed$detections$animal_id == "ST-02", ]
  expect_lte(max(st02$datetime), death)
  utils::write.csv(data.frame(animal_id = "ST-02", call = "dead_or_shed",
                              death_date = "2024-06-04"), f, row.names = FALSE)
  dated <- filter_mortality(apply_mortality_calls(x, f), margin_days = 0)
  st02 <- dated$detections[dated$detections$animal_id == "ST-02", ]
  expect_equal(as.Date(max(st02$datetime)), as.Date("2024-06-04"))
  expect_gt(max(st02$datetime), death)
  removed <- function(y) utils::tail(detset_log(y)$removed, 1)
  expect_equal(c(removed(timed), removed(dated)), c(1158, 1021))
})

test_that("margin_days removes the days before each death and the log says so", {
  ex <- example_mortality
  x <- detset(ex$detections, ex$deployments, ex$animals)
  f <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(animal_id = "ST-02", call = "dead_or_shed",
                              death_time = "2024-06-04 02:00:00"), f, row.names = FALSE)
  y <- apply_mortality_calls(x, f)
  death <- as.POSIXct("2024-06-04 02:00:00", tz = "UTC")
  st02 <- x$detections$animal_id == "ST-02"
  for (m in c(0, 1, 7)) {
    z <- filter_mortality(y, margin_days = m)
    expected <- sum(st02 & x$detections$datetime > death - m * 86400)
    expect_equal(utils::tail(detset_log(z)$removed, 1), expected)
    expect_match(utils::tail(detset_log(z)$reason, 1),
                 sprintf("margin of %s,", n_of(m, "day")), fixed = TRUE)
  }
  # The default is 7 days, which on this nine-day study removes all of ST-02.
  z <- filter_mortality(y)
  expect_match(utils::tail(detset_log(z)$parameters, 1), "margin_days=7", fixed = TRUE)
  expect_equal(sum(z$detections$animal_id == "ST-02"), 0L)
  expect_equal(sum(z$detections$animal_id != "ST-02"), sum(!st02))
  expect_error(filter_mortality(y, margin_days = -1), "margin_days")
})
