# audit_listening(): receiver-days by serial with a status for each.

listen_case <- function() {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  dep <- data.frame(
    receiver_serial = c("R1", "R2", "R3", "R4"), station_id = c("A", "B", "B", "C"),
    deploy_datetime = utc(c("2024-01-01 12:00:00", "2024-01-01 00:00:00",
                            "2024-01-05 12:00:00", "2024-01-01 00:00:00")),
    recover_datetime = utc(c("2024-01-10 00:00:00", "2024-01-05 12:00:00", NA, NA)),
    deploy_lat = c(45, 45.01, 45.01, 45.02), deploy_lon = -78
  )
  dep$download_datetime <- utc(c(NA, NA, NA, "2024-01-07 00:00:00"))
  days <- seq(utc("2024-01-02 06:00:00"), utc("2024-01-09 06:00:00"), by = "day")
  ref <- data.frame(datetime = days, receiver_serial = "R1", transmitter = "REF")
  ref <- ref[-4, ]
  fish <- data.frame(datetime = utc(c("2024-01-01 13:00:00", "2024-01-03 10:00:00",
                                      "2024-01-06 10:00:00", "2024-01-06 11:00:00")),
                     receiver_serial = c("R2", "R2", "R3", "R4"), transmitter = "T1")
  det <- rbind(ref, fish)
  det$animal_id <- ifelse(det$transmitter == "T1", "a1", NA)
  detset(det, dep)
}

test_that("each receiver-day gets the right status", {
  x <- audit_listening(listen_case(), reference_tags = data.frame(transmitter = "REF"))
  l <- x$listening
  expect_named(l, c("receiver_serial", "station_id", "date", "deployed_share", "detections",
                    "reference_detections", "status", "listening"))
  r1 <- l[l$receiver_serial == "R1", ]
  expect_equal(r1$status[1], "partial")
  expect_equal(r1$deployed_share[1], 0.5)
  expect_equal(as.character(r1$date[r1$status == "reference_silent"]), "2024-01-05")
  expect_equal(sum(r1$status == "listening"), 7L)
  expect_false(any(as.character(r1$date) == "2024-01-10"))
  r4 <- l[l$receiver_serial == "R4", ]
  expect_true(all(r4$status[r4$date >= as.Date("2024-01-07")] == "not_downloaded"))
  expect_false(any(r4$listening[r4$date >= as.Date("2024-01-07")]))
  expect_equal(r4$status[as.character(r4$date) == "2024-01-06"], "listening")
  # A swap at station B on 2024-01-05 leaves two receivers with a row each.
  b5 <- l[l$station_id == "B" & as.character(l$date) == "2024-01-05", ]
  expect_setequal(b5$receiver_serial, c("R2", "R3"))
  expect_equal(sum(b5$deployed_share), 1)
  expect_true(all(is.na(l$reference_detections[l$receiver_serial != "R1"])))
  # The audit covers the dates of the first to the last detection only.
  expect_equal(range(l$date), as.Date(c("2024-01-01", "2024-01-09")))
  expect_true("silent" %in% l$status)
  lg <- detset_log(x)
  expect_equal(lg$step[nrow(lg)], "audit_listening")
  expect_match(lg$reason[nrow(lg)], "1 reference tag, 1 expected receiver pair")
})

test_that("named receiver pairs replace the share rule", {
  x <- listen_case()
  y <- audit_listening(x, reference_tags = data.frame(transmitter = "REF",
                                                      receiver_serial = c("R1", "R4")))
  l <- y$listening
  r4 <- l[l$receiver_serial == "R4" & l$status != "not_downloaded" &
            l$date >= as.Date("2024-01-02"), ]
  # R4 logged a fish on 2024-01-06: it missed the reference tag but was
  # working, so that day stays listening.
  expect_equal(r4$status[r4$date == as.Date("2024-01-06")], "reference_missed")
  expect_true(r4$listening[r4$date == as.Date("2024-01-06")])
  expect_true(all(r4$status[r4$date != as.Date("2024-01-06")] == "reference_silent"))
  expect_equal(l$status[l$receiver_serial == "R4" & l$date == as.Date("2024-01-01")], "silent")
  z <- audit_listening(x)
  expect_true(all(is.na(z$listening$reference_detections)))
  expect_false("reference_silent" %in% z$listening$status)
  expect_error(audit_listening(x, reference_tags = data.frame(code = 1)), "transmitter")
  expect_warning(none <- audit_listening(x, reference_tags = data.frame(transmitter = "NOPE")),
                 paste("1 reference tag given; it does not occur in the detections, so it",
                       "tests no receiver"))
  expect_true(all(is.na(none$listening$reference_detections)))
})

test_that("the audit is the receiver-day table detfield reads", {
  x <- audit_listening(example_detset())
  l <- x$listening
  expect_s3_class(l$date, "Date")
  expect_type(l$receiver_serial, "character")
  expect_type(l$listening, "logical")
  expect_false(anyDuplicated(l[, c("receiver_serial", "date")]) > 0)
  validate_detset(x)
})

test_that("a day with 100 detections and no reference tag stays listening", {
  x <- listen_case()
  det <- x$detections
  extra <- data.frame(datetime = as.POSIXct("2024-01-05 08:00:00", tz = "UTC") + 60 * (1:100),
                      receiver_serial = "R1", transmitter = "T2", animal_id = "a2")
  y <- detset(rbind(det[, names(extra)], extra), x$deployments)
  l <- audit_listening(y, reference_tags = data.frame(transmitter = "REF"))$listening
  d5 <- l[l$receiver_serial == "R1" & l$date == as.Date("2024-01-05"), ]
  expect_equal(d5$detections, 100L)
  expect_equal(d5$reference_detections, 0L)
  expect_equal(d5$status, "reference_missed")
  expect_true(d5$listening)
  lg <- detset_log(audit_listening(y, reference_tags = data.frame(transmitter = "REF")))
  expect_match(lg$reason[nrow(lg)], "1 reference_missed")
})

test_that("a receiver silent on every day, or for a long run, is not listening", {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  dep <- data.frame(receiver_serial = c("R1", "R2"), station_id = c("A", "B"),
                    deploy_datetime = utc("2024-01-01"), recover_datetime = utc(NA),
                    deploy_lat = c(45, 45.01), deploy_lon = -78)
  # R1 hears T1 every day except 2024-01-05 to 2024-01-12; R2 hears nothing.
  days <- seq(utc("2024-01-01 06:00:00"), utc("2024-01-20 06:00:00"), by = "day")
  days <- days[!as.Date(days) %in% seq(as.Date("2024-01-05"), as.Date("2024-01-12"), by = 1)]
  det <- data.frame(datetime = days, receiver_serial = "R1", transmitter = "T1", animal_id = "a1")
  x <- detset(det, dep)
  l <- audit_listening(x)$listening
  r2 <- l[l$receiver_serial == "R2", ]
  expect_equal(nrow(r2), 20L)
  expect_true(all(r2$status == "silent_run"))
  expect_false(any(r2$listening))
  r1 <- l[l$receiver_serial == "R1", ]
  expect_equal(sum(r1$status == "silent"), 8L)
  expect_true(all(r1$listening))
  # With a run length of 7 days the eight silent days of R1 go too; with 9
  # they stay.
  l7 <- audit_listening(x, max_silent_days = 7)$listening
  expect_equal(sum(l7$status == "silent_run" & l7$receiver_serial == "R1"), 8L)
  expect_equal(sum(!l7$listening), 28L)
  l9 <- audit_listening(x, max_silent_days = 9)$listening
  expect_equal(sum(l9$status == "silent_run" & l9$receiver_serial == "R1"), 0L)
  expect_error(audit_listening(x, max_silent_days = 0), "max_silent_days")
  expect_match(detset_log(audit_listening(x))$reason[2], "20 silent_run")
})

test_that("an audit of a set cut to some animals warns, and the log keeps why", {
  ex <- hard_cases()
  ani <- ex$animals
  ani$species[ani$animal_id %in% c("EX-02", "EX-04")] <- "Smallmouth Bass"
  keep <- ex$detections$animal_id %in% c("EX-02", "EX-04")
  # Cut to one species before detset().
  x <- detset(ex$detections[keep, ], ex$deployments, ani)
  w <- character()
  y <- withCallingHandlers(audit_listening(x, max_silent_days = 7), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  runs <- sum(y$listening$status == "silent_run")
  expect_gt(runs, 0L)
  expect_match(w, sprintf("%s of %s receiver-days are silent_run.*hold 1 of the 2 species",
                          format(runs, big.mark = ","), format(nrow(y$listening), big.mark = ",")),
               all = FALSE)
  expect_match(utils::tail(detset_log(y)$reason, 1), "audited on a cut set: the detections hold 1")
  # Cut after detset() with filter_animals(): named as well.
  z <- filter_animals(detset(ex$detections, ex$deployments, ani), animals = c("EX-02", "EX-04"))
  expect_warning(audit_listening(z, max_silent_days = 7), "filter_animals\\(\\) ran before it")
  # The whole set, every species, gives no such warning.
  full <- detset(ex$detections, ex$deployments, ani)
  w2 <- character()
  withCallingHandlers(audit_listening(full, max_silent_days = 7), warning = function(cnd) {
    w2 <<- c(w2, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_false(any(grepl("may be cut", w2)))
})

# ---- Deployment sheets with no receiver serial ----
# Three stations with no serial, as OTN's deployment CSV gives them, and a
# fourth deployment with one. Detections carry receiver serials the sheet
# does not name, as OTN extracts do.
serialless_case <- function() {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  dep <- data.frame(
    receiver_serial = c(NA, NA, NA, "R9"), station_id = c("S1", "S2", "S3", "S4"),
    deploy_datetime = utc("2024-01-01 00:00:00"), recover_datetime = utc("2024-01-11 00:00:00"),
    deploy_lat = c(45, 45.01, 45.02, 45.03), deploy_lon = -78, stringsAsFactors = FALSE
  )
  det <- data.frame(
    datetime = utc(c("2024-01-02 10:00:00", "2024-01-02 11:00:00", "2024-01-05 10:00:00",
                     "2024-01-06 10:00:00", "2024-01-09 10:00:00", "2024-01-08 10:00:00")),
    receiver_serial = c("111", "111", "222", "222", "111", "R9"),
    station_id = c("S1", "S1", "S2", "S2", "S1", "S4"),
    transmitter = "T1", animal_id = "a1", stringsAsFactors = FALSE
  )
  detset(det, dep)
}

test_that("a sheet with no serials keys its days by station and its rows hold the detections", {
  x <- audit_listening(serialless_case())
  l <- x$listening
  # 4 deployments of 10 days, cut to the detections' dates, 2 to 9 January.
  expect_equal(nrow(l), 4L * 8L)
  expect_equal(sum(is.na(l$receiver_serial)), 3L * 8L)
  expect_equal(as.vector(table(l$station_id)), rep(8L, 4))
  heard <- l[l$detections > 0, ]
  heard <- heard[order(heard$station_id, heard$date), ]
  expect_equal(paste(heard$station_id, heard$date),
               c("S1 2024-01-02", "S1 2024-01-09", "S2 2024-01-05", "S2 2024-01-06",
                 "S4 2024-01-08"))
  expect_equal(heard$detections, c(2L, 1L, 1L, 1L, 1L))
  expect_true(all(heard$listening))
  # Every station-day with a detection is a listening row of that station.
  d <- unique(paste(x$detections$station_id, as.Date(x$detections$datetime)))
  on <- paste(l$station_id, l$date)[l$listening]
  expect_true(all(d %in% on))
  # S3 heard nothing in 8 full days: the silent run is its own, and S1 and S2 have none.
  expect_true(all(l$status[l$station_id == "S3"] == "silent_run"))
  expect_false(any(l$status[l$station_id %in% c("S1", "S2")] == "silent_run"))
  expect_match(tail(x$log$reason, 1),
               "32 receiver-days of 1 receiver and 3 stations with no receiver serial")
})

test_that("detfield takes the serial-less rows of the audit by station", {
  skip_if_not_installed("detfield")
  x <- audit_listening(serialless_case())
  inp <- suppressMessages(as_detfield_input(x))
  b <- suppressMessages(detfield::bin_detections(inp$detections, inp$listening,
                                                 data.frame(transmitter = "T1", animal_id = "a1",
                                                            min_delay = 60, max_delay = 120),
                                                 bin = "hour", codes = "shared"))
  heard <- b[b$y > 0, ]
  expect_equal(nrow(heard), 6L)
  expect_true(all(heard$listening))
})

# An OTN station deployment sheet with no serials, in the columns of the
# public hfx files of glatos (fixtures/otn_serialless_*.csv, built by
# data-raw/fixture_serialless_sheet.R).
serialless_sheet <- function() {
  f <- function(n) test_path("fixtures", n)
  dep <- suppressWarnings(read_deployments(f("otn_serialless_deployments.csv")))
  det <- read_detections(f("otn_serialless_detections.csv"))
  suppressMessages(suppressWarnings(audit_listening(detset(det, dep))))
}

test_that("an OTN station sheet with no serials gives one listening row per station-day", {
  x <- serialless_sheet()
  l <- x$listening
  expect_equal(nrow(x$deployments), 5L)
  expect_true(all(is.na(l$receiver_serial)))
  expect_equal(anyDuplicated(paste(l$station_id, l$date)), 0L)
  # 29 days, 2 to 30 January, at SYN001 over its two deployments in a row.
  expect_equal(sum(l$station_id == "SYN001"), 29L)
  d <- unique(paste(x$detections$station_id, as.Date(x$detections$datetime)))
  key <- paste(l$station_id, l$date)
  # Every station-day with a detection is a row of that station, and it is
  # listening unless its detections fall after the recorded download.
  expect_true(all(d %in% key))
  day_key <- paste(x$detections$station_id, as.Date(x$detections$datetime))
  late <- unique(day_key[x$detections$after_download])
  expect_length(late, 1L)
  expect_true(all(l$listening[key %in% setdiff(d, late)]))
  expect_equal(l$status[key %in% late], "not_downloaded")
  # SYN004 heard nothing on any day; the recovery "failed" leaves SYN003 in
  # the water to the last detection.
  expect_true(all(l$status[l$station_id == "SYN004"] == "silent_run"))
  expect_equal(max(l$date[l$station_id == "SYN003"]), as.Date("2024-01-30"))
})

test_that("a detection after the download on the sheet keeps the data end, flagged", {
  f <- function(n) test_path("fixtures", n)
  dep <- suppressWarnings(read_deployments(f("otn_serialless_deployments.csv")))
  x <- detset(read_detections(f("otn_serialless_detections.csv")), dep)
  # SYN002: download on 11 January, one detection at 14:00 on 15 January.
  late <- x$detections$station_id == "SYN002" &
    x$detections$datetime == as.POSIXct("2024-01-15 14:00:00", tz = "UTC")
  expect_type(x$detections$after_download, "logical")
  expect_identical(x$detections$after_download, late)
  expect_match(x$log$reason[1], "1 detection after the recorded download, marked in after_download")
  msg <- character()
  y <- withCallingHandlers(suppressWarnings(audit_listening(x)), message = function(cnd) {
    msg <<- c(msg, conditionMessage(cnd))
    invokeRestart("muffleMessage")
  })
  hit <- grep("after the recorded download", msg, value = TRUE)
  expect_length(hit, 1L)
  expect_match(hit, "1 detection on 1 deployment falls after the recorded download")
  expect_match(hit, "1 receiver-day with such detections is not_downloaded and not listening")
  expect_match(hit, "a download time recorded too early")
  expect_match(tail(y$log$reason, 1),
               "1 detection after the recorded download on 1 deployment, on 1 receiver-day")
  st <- y$listening[y$listening$station_id == "SYN002", ]
  day15 <- st[as.character(st$date) == "2024-01-15", ]
  expect_equal(day15$status, "not_downloaded")
  expect_false(day15$listening)
  expect_equal(day15$detections, 1L)
  expect_equal(sum(st$status == "not_downloaded"), 10L)
  # The detection stays in the set.
  expect_equal(nrow(y$detections), nrow(x$detections))
  expect_message(inp <- as_detfield_input(y),
                 "as_detfield_input\\(\\): 1 of 14 detections falls after")
  expect_equal(format(inp$listening$deployments$recover_datetime[3], "%Y-%m-%d %H:%M:%S"),
               "2024-01-11 00:00:00")
  expect_equal(nrow(inp$detections), 14L)
  # A sheet whose downloads are not passed by any detection gives no message.
  z <- x
  z$detections <- z$detections[z$detections$datetime < as.POSIXct("2024-01-11", tz = "UTC"), ]
  expect_no_message(suppressWarnings(audit_listening(z)),
                    message = "after the recorded download")
})

test_that("check_detset() counts the detections after the download per deployment", {
  f <- function(n) test_path("fixtures", n)
  dep <- suppressWarnings(read_deployments(f("otn_serialless_deployments.csv")))
  x <- detset(read_detections(f("otn_serialless_detections.csv")), dep)
  res <- check_detset(x, checks = "detections_after_download")$checks
  s <- res$summary
  expect_equal(s$status, "flag")
  expect_equal(s$flagged, 1L)
  # SYN002 is the one deployment whose download comes before its recovery.
  expect_equal(s$of, 1L)
  expect_equal(s$unit, "deployments")
  expect_match(s$note, "1 detection after the recorded download")
  expect_match(s$note, "a receiver that kept logging, or a clock problem")
  r <- res$rows$detections_after_download
  expect_equal(r$station_id, "SYN002")
  expect_equal(r$deployment, 3L)
  expect_equal(r$detections_after, 1L)
  expect_equal(r$days_after, 1L)
  expect_equal(r$first_after, as.POSIXct("2024-01-15 14:00:00", tz = "UTC"))
  # The check reads the deployments as they are, so it passes once the
  # detection is gone, and does not run without download times.
  y <- x
  y$detections <- y$detections[!y$detections$after_download, ]
  expect_equal(check_detset(y, checks = "detections_after_download")$checks$summary$status,
               "pass")
  ex <- detqc_example
  no_dl <- check_detset(detset(ex$detections, ex$deployments),
                        checks = "detections_after_download")$checks$summary
  expect_equal(no_dl$status, "not run")
  expect_match(no_dl$note, "no `download_datetime`")
  expect_false("after_download" %in% names(detset(ex$detections, ex$deployments)$detections))
})

# ---- A download before the recovery ----
download_case <- function(extra = NULL) {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  dep <- data.frame(
    receiver_serial = c("R1", "R2"), station_id = c("A", "B"),
    deploy_datetime = utc("2024-01-01 00:00:00"), recover_datetime = utc("2024-01-10 00:00:00"),
    deploy_lat = c(45, 45.01), deploy_lon = -78
  )
  dep$download_datetime <- utc(c("2024-01-06 06:00:00", NA))
  hours <- seq(utc("2024-01-02 03:00:00"), utc("2024-01-09 03:00:00"), by = "day")
  det <- data.frame(datetime = c(hours[hours < utc("2024-01-06 06:00:00")], hours),
                    receiver_serial = c(rep("R1", 5), rep("R2", 8)), transmitter = "T1",
                    animal_id = "a1")
  if (!is.null(extra)) det <- rbind(det, extra)
  detset(det, dep)
}

test_that("a 06:00 download day is partial and the deployment ends at the download", {
  x <- audit_listening(download_case())
  r1 <- x$listening[x$listening$receiver_serial == "R1", ]
  day6 <- r1[as.character(r1$date) == "2024-01-06", ]
  expect_equal(day6$status, "partial")
  expect_equal(day6$deployed_share, 1)
  expect_true(day6$listening)
  expect_true(all(r1$status[r1$date > as.Date("2024-01-06")] == "not_downloaded"))
  inp <- suppressMessages(as_detfield_input(x))
  dep <- inp$listening$deployments
  expect_equal(format(dep$recover_datetime, "%Y-%m-%d %H:%M"),
               c("2024-01-06 06:00", "2024-01-10 00:00"))
  skip_if_not_installed("detfield")
  b <- suppressMessages(detfield::bin_detections(inp$detections, inp$listening,
                                                 data.frame(transmitter = "T1", animal_id = "a1",
                                                            min_delay = 60, max_delay = 120),
                                                 bin = "hour", codes = "shared"))
  late <- b$receiver_serial == "R1" & b$bin_start >= as.POSIXct("2024-01-06 06:00", tz = "UTC")
  expect_equal(sum(late), 0L)
})

test_that("a detection after the recorded download leaves the data end at the download", {
  late <- data.frame(datetime = as.POSIXct("2024-01-07 12:00:00", tz = "UTC"),
                     receiver_serial = "R1", transmitter = "T1", animal_id = "a1")
  base <- download_case(late)
  expect_equal(sum(base$detections$after_download), 1L)
  expect_true(base$detections$after_download[base$detections$datetime ==
                                               as.POSIXct("2024-01-07 12:00:00", tz = "UTC")])
  expect_message(x <- audit_listening(base),
                 "1 receiver-day with such detections is not_downloaded and not listening")
  r1 <- x$listening[x$listening$receiver_serial == "R1", ]
  st <- stats::setNames(r1$status, as.character(r1$date))
  expect_equal(unname(st[c("2024-01-06", "2024-01-07", "2024-01-08")]),
               c("partial", "not_downloaded", "not_downloaded"))
  expect_equal(r1$detections[as.character(r1$date) == "2024-01-07"], 1L)
  inp <- suppressMessages(as_detfield_input(x))
  expect_equal(format(inp$listening$deployments$recover_datetime[1], "%Y-%m-%d %H:%M:%S"),
               "2024-01-06 06:00:00")
  expect_equal(attr(x$listening, "data_end")$data_end[1],
               as.POSIXct("2024-01-06 06:00:00", tz = "UTC"))
  # A detection at the download second is after it; one a second before is not.
  edge <- data.frame(datetime = as.POSIXct(c("2024-01-06 05:59:59", "2024-01-06 06:00:00"),
                                           tz = "UTC"),
                     receiver_serial = "R1", transmitter = "T1", animal_id = "a1")
  e <- download_case(edge)
  at <- e$detections$datetime %in% edge$datetime
  expect_equal(e$detections$after_download[at], c(FALSE, TRUE))
  skip_if_not_installed("detfield")
  delays <- data.frame(transmitter = "T1", animal_id = "a1", min_delay = 60, max_delay = 120)
  b <- suppressWarnings(suppressMessages(
    detfield::bin_detections(inp$detections, inp$listening, delays, bin = "hour",
                             codes = "shared")
  ))
  off <- b[b$receiver_serial == "R1" & b$bin_start >= as.POSIXct("2024-01-06 06:00", tz = "UTC"), ]
  expect_equal(nrow(off), 1L)
  expect_false(off$listening)
})

test_that("a redeployed receiver's detections at its next station are not after a download", {
  # Receiver 900001: at S1 with no recovery time and a download on day 5,
  # then at S2 from day 10. Its detections at S2 belong to S2.
  t0 <- as.POSIXct("2025-07-01", tz = "UTC")
  day <- 86400
  dep <- data.frame(receiver_serial = c("900001", "900001", "900002"),
                    station_id = c("S1", "S2", "S3"),
                    deploy_datetime = t0 + c(0, 10, 0) * day,
                    recover_datetime = t0 + c(NA, 20, 20) * day,
                    download_datetime = t0 + c(5, NA, NA) * day,
                    deploy_lat = c(45, 45.02, 45.04), deploy_lon = -78)
  hrs <- c(seq(1, 4 * 24, by = 12), seq(10 * 24 + 1, 19.5 * 24, by = 12))
  det <- data.frame(datetime = t0 + hrs * 3600, receiver_serial = "900001",
                    transmitter = "A69-1601-9", animal_id = NA_character_,
                    station_id = ifelse(hrs < 10 * 24, "S1", "S2"))
  det <- rbind(det, data.frame(datetime = t0 + (1:19) * day + 7200, receiver_serial = "900002",
                               transmitter = "A69-1601-9", animal_id = NA_character_,
                               station_id = "S3"))
  ani <- data.frame(animal_id = "F1", transmitter = "A69-1601-9", release_datetime = t0)
  x <- suppressWarnings(suppressMessages(detset(det, dep, ani)))
  m <- character()
  a <- withCallingHandlers(suppressWarnings(audit_listening(x)), message = function(cnd) {
    m <<- c(m, conditionMessage(cnd))
    invokeRestart("muffleMessage")
  })
  expect_false(any(grepl("after the recorded download", m)))
  s1 <- a$listening[a$listening$station_id == "S1", ]
  after <- s1$date >= as.Date(t0 + 5 * day)
  expect_equal(sum(s1$listening & after), 0L)
  expect_true(all(s1$status[after] == "not_downloaded"))
  inp <- suppressMessages(as_detfield_input(a))
  d <- inp$listening$deployments
  expect_equal(d$recover_datetime[d$station_id == "S1"], t0 + 5 * day)
  expect_lte(as.numeric(d$recover_datetime[d$station_id == "S1"]),
             as.numeric(d$deploy_datetime[d$station_id == "S2"]))
})

test_that("a deployment with no recovery time ends where its receiver is deployed again", {
  t0 <- as.POSIXct("2025-07-01", tz = "UTC")
  dep <- data.frame(receiver_serial = c("900001", "900001"), station_id = c("S1", "S2"),
                    deploy_datetime = t0 + c(0, 10) * 86400,
                    recover_datetime = t0 + c(NA, 20) * 86400)
  expect_equal(next_deployment_start(dep), c(t0 + 10 * 86400, NA))
  end <- deployment_data_end(dep)
  expect_equal(as.numeric(end), as.numeric(t0 + c(10, 20) * 86400))
})

test_that("audit_listening() refuses a min_share outside 0 to 1", {
  x <- example_detset()
  for (v in list(2, -0.1, NA_real_, c(0.2, 0.3), "half")) {
    expect_error(audit_listening(x, min_share = v), "from 0 to 1", class = "detqc_error")
  }
  expect_no_error(suppressWarnings(suppressMessages(audit_listening(x, min_share = 1))))
})
