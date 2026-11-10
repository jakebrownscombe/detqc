# ---- as_detfield_input() ----

test_that("as_detfield_input() reads a silent_run day as not listening when the flag is absent", {
  # silent_run is one of the statuses that are not listening (CONTRACT.md).
  # When the listening flag is absent, as_detfield_input() reads the status
  # and must not count a silent_run day as listening.
  x <- audit_listening(example_detset())
  x$listening$listening <- NULL
  x$listening$status <- rep_len(c("listening", "silent", "silent_run", "reference_silent",
                                  "not_downloaded", "reference_missed", "partial"),
                                nrow(x$listening))
  inp <- suppressMessages(as_detfield_input(x, "days"))
  off <- c("not_downloaded", "reference_silent", "silent_run")
  expect_equal(inp$listening$listening, !x$listening$status %in% off)
})

test_that("the message counts the detections after a download in the tables returned", {
  fx <- test_path("fixtures")
  dep <- suppressWarnings(read_deployments(file.path(fx, "otn_serialless_deployments.csv")))
  det <- read_detections(file.path(fx, "otn_serialless_detections.csv"))
  x <- suppressWarnings(suppressMessages(audit_listening(detset(det, dep))))
  msgs <- function(...) {
    m <- character()
    withCallingHandlers(as_detfield_input(x, ...), message = function(cnd) {
      m <<- c(m, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    })
    m
  }
  expect_match(msgs(), "1 of 14 detections falls after the deployment's recorded download",
               all = FALSE)
  expect_match(msgs(stations = "SYN002"), "1 of 5 detections falls after", all = FALSE)
  # The late detection is not among those kept, so no message.
  expect_false(any(grepl("recorded download", msgs(stations = "SYN004"))))
  # A station whose deployments carry no serial keeps its detections.
  expect_match(msgs(stations = "SYN002"), "kept 1 of 5 deployments, 5 of 14 detections",
               all = FALSE)
})

test_that("both forms end each deployment at its recorded download", {
  fx <- test_path("fixtures")
  dep <- suppressWarnings(read_deployments(file.path(fx, "otn_serialless_deployments.csv")))
  det <- read_detections(file.path(fx, "otn_serialless_detections.csv"))
  x <- suppressWarnings(suppressMessages(audit_listening(detset(det, dep))))
  before <- suppressMessages(as_detfield_input(x))
  ends <- function(inp) format(inp$listening$deployments$recover_datetime, "%Y-%m-%d %H:%M:%S")
  expect_equal(ends(before)[3], "2024-01-11 00:00:00")
  # Removing the detection after the download changes no end and no day.
  y <- x
  y$detections <- y$detections[!y$detections$after_download, ]
  after <- suppressMessages(as_detfield_input(y))
  expect_identical(ends(after), ends(before))
  expect_identical(after$listening$days, before$listening$days)
  one <- suppressMessages(as_detfield_input(y, "deployments"))$listening
  expect_identical(format(one$recover_datetime, "%Y-%m-%d %H:%M:%S"), ends(before))
  # Every listening day of the days table overlaps a deployment of the
  # deployments form.
  d <- after$listening$deployments
  days <- after$listening$days[after$listening$days$listening, ]
  inside <- vapply(seq_len(nrow(days)), function(i) {
    day0 <- as.POSIXct(days$date[i], tz = "UTC")
    any(d$station_id == days$station_id[i] & d$deploy_datetime < day0 + 86400 &
          (is.na(d$recover_datetime) | d$recover_datetime > day0))
  }, logical(1))
  expect_true(all(inside))
  # The record survives a save to RDS, and without it the ends are the same.
  f <- withr::local_tempfile(fileext = ".rds")
  write_detset(x, f)
  back <- read_detset(f)
  expect_identical(ends(suppressMessages(as_detfield_input(back))), ends(before))
  expect_identical(back$detections$after_download, x$detections$after_download)
  z <- x
  attr(z$listening, "data_end") <- NULL
  expect_identical(ends(suppressMessages(as_detfield_input(z))), ends(before))
})

test_that("the audit's ends match deployments by row, then by a key no row shares", {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  dep <- data.frame(receiver_serial = NA_character_, station_id = c("A", "A", "B"),
                    deploy_datetime = utc("2024-01-01"))
  lst <- data.frame(date = as.Date("2024-01-01"))
  attr(lst, "data_end") <- cbind(dep, data_end = utc(c("2024-01-05", "2024-01-09", "2024-01-07")))
  expect_equal(audited_data_end(lst, dep), as.numeric(utc(c("2024-01-05", "2024-01-09",
                                                            "2024-01-07"))))
  # Out of order: the two A rows share a key and are left to the detections.
  expect_equal(audited_data_end(lst, dep[3:1, ]),
               c(as.numeric(utc("2024-01-07")), NA, NA))
  expect_true(all(is.na(audited_data_end(data.frame(date = 1), dep))))
})

test_that("the tag_delay message names what is missing: the animals table or its delays", {
  ex <- detqc_example
  x <- detset(ex$detections, ex$deployments)
  expect_message(inp <- as_detfield_input(x, listening = "days"),
                 "The detset has no animals table, so it holds no delays")
  expect_null(inp$tag_delay)
  animals <- ex$animals
  animals$min_delay <- NA_real_
  y <- detset(ex$detections, ex$deployments, animals)
  expect_message(as_detfield_input(y, listening = "days"),
                 "The animals table has no min_delay and max_delay")
})
