# read_receiver_logs() and the statuses audit_listening() reads from logs.
# Every log file here is synthetic, written by
# helper-receiver-logs.R in the layout of Fathom VDAT CSV exports.

log_dir <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  d
}

utc <- function(s) as.POSIXct(s, tz = "UTC")

# ---- The reader, one variant at a time ----

test_that("a VR2Tx log in format 2.0.0 gives one row per status record and event", {
  d <- log_dir()
  # Hourly from 14:00 on 1 June to 12:00 on 3 June: 47 hours, of which
  # 03:00 to 06:00 on 2 June (4 hours) are off, so 43 hours of DIAG, TEMP,
  # ATTITUDE and BATTERY; HEALTH_VR2TX at 00:00 on 2 June only (00:00 on
  # 3 June is logged too: 2 rows). Events: EVENT_INIT, one EVENT_FAULT, the
  # REBOOT_COMPLETED after the outage, EVENT_OFFLOAD.
  f <- write_vdat_log(file.path(d, "a.csv"), "900001", "2024-06-01 14:00", "2024-06-03 12:00",
                      off = list(c("2024-06-02 03:00", "2024-06-02 06:00")), fault = TRUE)
  logs <- read_receiver_logs(f)
  expect_named(logs, c("receiver_serial", "receiver_model", "datetime", "record_type",
                       "temperature_c", "noise", "noise_unit", "tilt_deg", "depth_m", "pings",
                       "log_detections", "battery_v", "battery_pct", "memory_pct", "event",
                       "event_detail", "source_file"))
  tab <- table(logs$record_type)
  expect_equal(as.integer(tab[c("DIAG", "TEMP", "ATTITUDE", "BATTERY")]), rep(43L, 4))
  expect_equal(as.integer(tab[c("HEALTH_VR2TX", "EVENT", "EVENT_FAULT", "EVENT_INIT",
                                "EVENT_OFFLOAD")]), c(2L, 1L, 1L, 1L, 1L))
  expect_true(is_utc(logs$datetime))
  expect_identical(unique(logs$receiver_serial), "900001")
  diag <- logs[logs$record_type == "DIAG", ]
  expect_true(all(diag$noise_unit == "mV"))
  expect_true(all(diag$pings == 400))
  expect_true(all(is.na(diag$depth_m)))
  expect_equal(logs$event[logs$record_type == "EVENT"], "REBOOT_COMPLETED")
  expect_equal(logs$event_detail[logs$record_type == "EVENT"], "Power-on reset")
  expect_equal(logs$event[logs$record_type == "EVENT_FAULT"], "UNKNOWN")
  expect_equal(logs$datetime[logs$record_type == "EVENT"], utc("2024-06-02 06:00:00"))
  expect_true(all(logs$battery_v[logs$record_type == "BATTERY"] == 3.6))
  src <- attr(logs, "source")[[1]]
  expect_equal(src$format_version, "2.0.0")
  expect_equal(src$rows, 43L * 4L + 2L)
})

test_that("format 1.0.0 names its event fields Event Type and Event Details", {
  d <- log_dir()
  f <- write_vdat_log(file.path(d, "b.csv"), "900002", "2024-06-01 14:00", "2024-06-02 12:00",
                      variant = "vr2tx_1", off = list(c("2024-06-02 01:00", "2024-06-02 02:00")))
  logs <- read_receiver_logs(f)
  expect_equal(attr(logs, "source")[[1]]$format_version, "1.0.0")
  expect_equal(logs$event[logs$record_type == "EVENT"], "REBOOT_COMPLETED")
  expect_equal(logs$event_detail[logs$record_type == "EVENT"], "Power-on reset")
  # 14:00 on 1 June to 12:00 on 2 June is 23 hours; 01:00 and 02:00 are off.
  expect_equal(sum(logs$record_type == "DIAG"), 21L)
})

test_that("a NexTrak log gives DIAG_NEXTRAK every 10 minutes with noise in dB", {
  d <- log_dir()
  f <- write_vdat_log(file.path(d, "c.csv"), "900003", "2024-06-01 14:00", "2024-06-01 18:00",
                      variant = "nextrak", pings = 55)
  logs <- read_receiver_logs(f)
  diag <- logs[logs$record_type == "DIAG_NEXTRAK", ]
  # 14:00 to 18:00 every 600 s: 25 records.
  expect_equal(nrow(diag), 25L)
  expect_true(all(diag$noise_unit == "dB"))
  expect_true(all(diag$pings == 55))
  expect_equal(unique(diag$receiver_model), "NexTrak-R1")
  expect_equal(sum(logs$record_type == "HEALTH_NEXTRAK"), 25L)
})

test_that("a NexTrak event keeps its text, and DATA_ERROR records are counted in a message", {
  d <- log_dir()
  f <- write_vdat_log(file.path(d, "e.csv"), "900009", "2024-06-01 14:00", "2024-06-01 18:00",
                      variant = "nextrak", off = list(c("2024-06-01 15:05", "2024-06-01 15:55")),
                      data_error = TRUE)
  expect_message(logs <- read_receiver_logs(f),
                 "which name an error, a memory page and an offset: 1 in 1 file of 1")
  ev <- logs[logs$record_type == "EVENT", ]
  expect_equal(ev$event, "REBOOT_COMPLETED")
  expect_equal(ev$event_detail, "Power-on reset")
  expect_false("DATA_ERROR" %in% logs$record_type)
  expect_equal(attr(logs, "source")[[1]]$data_errors, 1L)
  # A file with no DATA_ERROR record gives no such message
  g <- write_vdat_log(file.path(d, "g.csv"), "900010", "2024-06-01 14:00", "2024-06-01 15:00",
                      variant = "nextrak")
  expect_no_message(read_receiver_logs(g))
})

test_that("a NexTrak export with no DIAG_NEXTRAK record says so and still reads", {
  d <- log_dir()
  f <- write_vdat_log(file.path(d, "f.csv"), "900011", "2024-06-01 14:00", "2024-06-01 18:00",
                      variant = "nextrak", records = c("TEMP", "ATTITUDE", "BATTERY",
                                                       "HEALTH_NEXTRAK"))
  expect_message(logs <- read_receiver_logs(f),
                 "status records but no DIAG or DIAG_NEXTRAK record: 1 of 1, such as f.csv")
  expect_equal(sum(logs$record_type == "TEMP"), 25L)
  expect_true(all(is.na(logs$noise)))
  expect_true(all(is.na(logs$log_detections)))
})

test_that("an RLD export gives no status record, with a message that counts it", {
  d <- log_dir()
  f <- write_vdat_log(file.path(d, "d.csv"), "900004", "2024-06-01 14:00", "2024-06-01 18:00",
                      variant = "rld")
  expect_message(logs <- read_receiver_logs(f), "1 of 1, such as d.csv")
  expect_equal(sum(logs$record_type %in% c("EVENT_INIT", "EVENT_OFFLOAD")), 2L)
  expect_equal(attr(logs, "source")[[1]]$rows, 0L)
})

test_that("a file cut part-way loses its last line with a warning", {
  d <- log_dir()
  # The last line, EVENT_OFFLOAD, is cut in half and has no line end.
  f <- write_vdat_log(file.path(d, "e.csv"), "900005", "2024-06-01 14:00", "2024-06-01 18:00",
                      cut = TRUE)
  w <- character()
  logs <- withCallingHandlers(read_receiver_logs(f), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_length(w, 2L)
  expect_match(w[1], "ends part-way through")
  expect_match(w[2], "no EVENT_OFFLOAD record")
  expect_equal(sum(logs$record_type == "EVENT_OFFLOAD"), 0L)
  expect_equal(sum(logs$record_type == "DIAG"), 5L)
})

test_that("Time is read at its offset, and Device Time stands in where Time is blank", {
  d <- log_dir()
  f <- file.path(d, "t.csv")
  desc <- vdat_log_desc()
  writeLines(c(
    "VEMCO DATA LOG,2.0.0,detqc synthetic example; layout follows Fathom VDAT CSV",
    paste(c("RECORD TYPE", rep("FIELD", 25)), collapse = ","), desc[["TEMP"]],
    # Corrected time 2024-06-01 10:00 at offset -4 h is 14:00 UTC.
    "TEMP,2024-06-01 13:59:59,2024-06-01 10:00:00,-4,0.5,VR2Tx-69,900006,18.0,",
    # No corrected time: the device time, in UTC, is used.
    "TEMP,2024-06-01 15:00:00,,,,VR2Tx-69,900006,18.5,"
  ), f)
  expect_message(logs <- read_receiver_logs(f), "no DIAG or DIAG_NEXTRAK record")
  expect_equal(logs$datetime, utc(c("2024-06-01 14:00:00", "2024-06-01 15:00:00")))
  expect_equal(logs$temperature_c, c(18, 18.5))
})

test_that("several files stack with their names; files that are not logs stop", {
  d <- log_dir()
  f1 <- write_vdat_log(file.path(d, "one.csv"), "900007", "2024-06-01 14:00", "2024-06-01 16:00")
  f2 <- write_vdat_log(file.path(d, "two.csv"), "900008", "2024-06-01 14:00", "2024-06-01 16:00")
  logs <- read_receiver_logs(c(f1, f2))
  expect_setequal(unique(logs$source_file), c("one.csv", "two.csv"))
  expect_setequal(unique(logs$receiver_serial), c("900007", "900008"))
  expect_length(attr(logs, "source"), 2L)
  vue <- system.file("extdata", "stoney_vue.csv", package = "detqc")
  expect_error(read_receiver_logs(vue), "stoney_vue.csv is not a Fathom VDAT CSV")
  expect_error(read_receiver_logs(c(f1, vue)), "Reading stoney_vue.csv")
  empty <- file.path(d, "empty.csv")
  file.create(empty)
  expect_error(read_receiver_logs(empty), "empty \\(0 bytes\\)")
  expect_error(read_receiver_logs(file.path(d, "none.csv")), "File not found")
  expect_error(read_receiver_logs(f1, format = "vue"), "`format` must be one of \"vdat\"")
  expect_error(read_receiver_logs(character()), "one or more paths")
})

test_that("the shipped example logs read whole", {
  files <- list.files(system.file("extdata", "receiver_logs", package = "detqc"),
                      full.names = TRUE)
  expect_length(files, 19L)
  logs <- read_receiver_logs(files)
  expect_setequal(unique(logs$receiver_serial), sprintf("R%03d", 1:19))
  # R007 has no status record on 6 and 7 June (data-raw/receiver_logs_example.R).
  r7 <- logs[logs$receiver_serial == "R007" & logs$record_type == "DIAG", ]
  expect_false(any(as.Date(r7$datetime) %in% as.Date(c("2024-06-06", "2024-06-07"))))
})

# ---- The statuses, on hand-built receiver-days ----

# Five receivers deployed from 00:00 UTC on 1 June to 00:00 UTC on 6 June
# 2024, so 1 to 5 June are full days. A reference tag REF is expected by
# R1 and R2 (paired by receiver_serial). Detections:
# * R1 hears REF on 1, 2 and 5 June; nothing on 3 and 4 June.
# * R2 hears animal A on every day, and REF on every day but 3 June.
# * R3 hears animal A on 1 and 5 June only.
# * R4 hears animal A on 1 and 5 June only.
# * R5 hears animal A on 1 June only.
status_case <- function() {
  t <- function(day, hh = "12:00:00") utc(sprintf("2024-06-%02d %s", day, hh))
  det <- data.frame(
    datetime = c(t(c(1, 2, 5)), t(1:5), t(c(1, 2, 4, 5), "13:00:00"), t(c(1, 5)), t(c(1, 5)),
                 t(1)),
    receiver_serial = c(rep("R1", 3), rep("R2", 5), rep("R2", 4), rep("R3", 2), rep("R4", 2),
                        "R5"),
    transmitter = c(rep("REF", 3), rep("A", 5), rep("REF", 4), rep("A", 2), rep("A", 2), "A"),
    animal_id = c(rep(NA, 3), rep("a1", 5), rep(NA, 4), rep("a1", 2), rep("a1", 2), "a1"),
    stringsAsFactors = FALSE
  )
  dep <- data.frame(receiver_serial = paste0("R", 1:5), station_id = paste0("S", 1:5),
                    deploy_datetime = utc("2024-06-01"), recover_datetime = utc("2024-06-06"),
                    deploy_lat = 45, deploy_lon = -78)
  ani <- data.frame(animal_id = "a1", transmitter = "A")
  list(x = detset(det, dep, ani),
       ref = data.frame(transmitter = "REF", receiver_serial = c("R1", "R2")))
}

# Logs, written as the reader returns them:
# * R1 logs hourly from 00:00 on 1 June to 23:00 on 5 June, except all of
#   4 June. 3 June: logged, REF expected, nothing heard: deaf. 4 June: in the
#   span, no record: off.
# * R2 logs every day except 3 June; it heard A that day, so the detections
#   win and the day stays reference_missed, counted as a day with
#   detections and no status record.
# * R3 logs every day: 2 to 4 June, silent with no reference tag expected,
#   become logging_silent.
# * R4 logs 1 and 2 June only, and the file ends there (no offload): 2 June
#   is logging_silent; 3 and 4 June lie outside its log and stay silent.
# * R5 has no log: 2 to 5 June stay silent, and as R5 heard A only on 1 June
#   nothing changes.
status_logs <- function() {
  hours <- function(days) {
    unlist(lapply(days, function(d) {
      format(utc(sprintf("2024-06-%02d", d)) + 3600 * 0:23, "%Y-%m-%d %H:%M:%S")
    }))
  }
  rec <- function(rx, days, type = "DIAG") {
    h <- hours(days)
    data.frame(receiver_serial = rx, datetime = utc(h), record_type = type, pings = 100,
               source_file = paste0(rx, ".csv"), stringsAsFactors = FALSE)
  }
  off <- data.frame(receiver_serial = "R1", datetime = utc("2024-06-05 23:30:00"),
                    record_type = "EVENT_OFFLOAD", pings = NA, source_file = "R1.csv")
  rbind(rec("R1", c(1, 2, 3, 5)), off, rec("R2", c(1, 2, 4, 5)), rec("R3", 1:5),
        rec("R4", 1:2))
}

test_that("each receiver-day gets the status the rules give", {
  case <- status_case()
  logs <- status_logs()
  y <- audit_listening(case$x, reference_tags = case$ref, logs = logs)
  lst <- y$listening
  st <- function(rx) lst$status[lst$receiver_serial == rx]
  expect_equal(st("R1"), c("listening", "listening", "deaf", "off", "listening"))
  expect_equal(st("R2"), c("listening", "listening", "reference_missed", "listening",
                           "listening"))
  expect_equal(st("R3"), c("listening", rep("logging_silent", 3), "listening"))
  expect_equal(st("R4"), c("listening", "logging_silent", "silent", "silent", "listening"))
  expect_equal(st("R5"), c("listening", rep("silent", 4)))
  expect_equal(lst$listening[lst$receiver_serial == "R1"], c(TRUE, TRUE, FALSE, FALSE, TRUE))
  # log_records: NA outside a log; 0 on R1's off day; 24 on a logged day.
  expect_equal(lst$log_records[lst$receiver_serial == "R4"], c(24L, 24L, NA, NA, NA))
  expect_equal(lst$log_records[lst$receiver_serial == "R1"], c(24L, 24L, 24L, 0L, 24L))
  expect_true(all(is.na(lst$log_records[lst$receiver_serial == "R5"])))
  expect_equal(lst$pings[lst$receiver_serial == "R1"][3], 2400)
  expect_match(tail(y$log$reason, 1),
               paste("receiver logs of 4 receivers cover 17 receiver-days of 4 of the 5",
                     "receivers audited"))
  expect_match(tail(y$log$reason, 1), "with detections and no status record: 1")
  expect_match(tail(y$log$reason, 1), "4 logging_silent, 1 deaf, 1 off")
  # Without logs, the statuses are reference_silent and silent.
  z <- audit_listening(case$x, reference_tags = case$ref)
  expect_equal(z$listening$status[z$listening$receiver_serial == "R1"],
               c("listening", "listening", "reference_silent", "reference_silent", "listening"))
  expect_false(any(c("log_records", "pings") %in% names(z$listening)))
})

test_that("off beats silent_run, and silent_run beats logging_silent unless known", {
  # R6 hears nothing on any day: the dead-receiver rule makes its quiet days
  # silent_run. Its log covers 1 to 5 June with no record on 3 June: that
  # day is off; the others stay silent_run, as a log shows the receiver ran,
  # not that it heard. Named in known_listening, they become logging_silent.
  case <- status_case()
  dep <- rbind(case$x$deployments[, c("receiver_serial", "station_id", "deploy_datetime",
                                      "recover_datetime", "deploy_lat", "deploy_lon")],
               data.frame(receiver_serial = "R6", station_id = "S6",
                          deploy_datetime = utc("2024-06-01"), recover_datetime = utc("2024-06-06"),
                          deploy_lat = 45, deploy_lon = -78))
  x <- detset(case$x$detections, dep, case$x$animals)
  logs <- status_logs()
  r6 <- logs[logs$receiver_serial == "R3" & as.Date(logs$datetime) != as.Date("2024-06-03"), ]
  r6$receiver_serial <- "R6"
  r6$source_file <- "R6.csv"
  logs <- rbind(logs, r6)
  y <- audit_listening(x, logs = logs)
  st6 <- y$listening$status[y$listening$receiver_serial == "R6"]
  expect_equal(st6, c("silent_run", "silent_run", "off", "silent_run", "silent_run"))
  k <- audit_listening(x, logs = logs, known_listening = "R6")
  st6k <- k$listening$status[k$listening$receiver_serial == "R6"]
  expect_equal(st6k, c("logging_silent", "logging_silent", "off", "logging_silent",
                       "logging_silent"))
  # A receiver in known_listening that has no log is judged by its detections,
  # as any receiver without a log is.
  k5 <- audit_listening(x, max_silent_days = 2, known_listening = "R5", logs = logs)
  expect_equal(k5$listening$status[k5$listening$receiver_serial == "R5"],
               c("listening", rep("silent", 4)))
})

# ---- The receiver's own count of detections ----

# Hourly DIAG records of one receiver over the given days, each with a count
# of detections; `count` gives it per record (the record's datetime).
counted_logs <- function(rx, days, count = function(t) 0) {
  h <- do.call(c, lapply(days, function(d) utc(sprintf("2024-06-%02d", d)) + 3600 * 0:23))
  data.frame(receiver_serial = rx, datetime = h, record_type = "DIAG", pings = 100,
             log_detections = count(h), source_file = paste0(rx, ".csv"),
             stringsAsFactors = FALSE)
}

# The status case with R6 added: deployed 1 to 5 June like the others, and
# not one detection of it in the set, as when it heard only other projects'
# tags.
with_r6 <- function() {
  case <- status_case()
  dep <- rbind(case$x$deployments[, c("receiver_serial", "station_id", "deploy_datetime",
                                      "recover_datetime", "deploy_lat", "deploy_lon")],
               data.frame(receiver_serial = "R6", station_id = "S6",
                          deploy_datetime = utc("2024-06-01"), recover_datetime = utc("2024-06-06"),
                          deploy_lat = 45, deploy_lon = -78))
  list(x = detset(case$x$detections, dep, case$x$animals), ref = case$ref)
}

test_that("a receiver that heard only other projects' tags listens on the days its log counted", {
  case <- with_r6()
  # R6's log counts 2 detections at 10:00 on 2 and 4 June and none otherwise.
  heard <- function(t) as.numeric(format(t, "%H") == "10" & format(t, "%d") %in% c("02", "04")) * 2
  logs <- counted_logs("R6", 1:5, heard)
  y <- audit_listening(case$x, logs = logs)
  lst <- y$listening[y$listening$receiver_serial == "R6", ]
  # Days with a count listen. The receiver heard, so it is not dead, and its
  # other days, logged and quiet, are logging_silent rather than silent_run.
  expect_equal(lst$status, c("logging_silent", "listening", "logging_silent", "listening",
                             "logging_silent"))
  expect_true(all(lst$listening))
  expect_equal(lst$detections, rep(0L, 5))
  expect_match(tail(y$log$reason, 1),
               "quiet receiver-days on which the log counted detections, kept as listening: 2")
  # The same log with no counts: the dead-receiver rule makes every day silent_run.
  z <- audit_listening(case$x, logs = counted_logs("R6", 1:5))
  expect_equal(unique(z$listening$status[z$listening$receiver_serial == "R6"]), "silent_run")
  # Listening days belong to the receivers, so a cut to one species leaves them.
  expect_identical(filter_animals(y, animals = "a1")$listening$status, y$listening$status)
})

test_that("a logged count lifts silent_run and logging_silent and leaves off; detections beat it", {
  case <- with_r6()
  # Counts on every record of 2 and 3 June; no record at all on 4 June; an
  # event that carries a count on 4 June (a table of the user's own) does
  # not make that day heard, because counts are read from status records.
  logs <- counted_logs("R6", c(1, 2, 3, 5),
                       function(t) as.numeric(format(t, "%d") %in% c("02", "03")))
  ev <- data.frame(receiver_serial = "R6", datetime = utc("2024-06-04 12:00:00"),
                   record_type = "EVENT", pings = NA, log_detections = 5,
                   source_file = "R6.csv")
  y <- audit_listening(case$x, logs = rbind(logs, ev))
  st6 <- y$listening$status[y$listening$receiver_serial == "R6"]
  expect_equal(st6, c("logging_silent", "listening", "listening", "off", "logging_silent"))
  expect_equal(y$listening$listening[y$listening$receiver_serial == "R6"],
               c(TRUE, TRUE, TRUE, FALSE, TRUE))
  # R1 is expected to hear REF; on 3 June it heard nothing in the set. With
  # a log that counts detections that day the receiver was not deaf: the day
  # reads as reference_missed, as a day with other detections does.
  r1 <- counted_logs("R1", 1:5, function(t) as.numeric(format(t, "%d") == "03"))
  y1 <- audit_listening(case$x, reference_tags = case$ref, logs = r1)
  expect_equal(y1$listening$status[y1$listening$receiver_serial == "R1"],
               c("listening", "listening", "reference_missed", "deaf", "listening"))
  # R2 heard animal A on every day; its log's counts change nothing.
  r2 <- counted_logs("R2", 1:5, function(t) rep(3, length(t)))
  y2 <- audit_listening(case$x, reference_tags = case$ref, logs = r2)
  z2 <- audit_listening(case$x, reference_tags = case$ref)
  expect_equal(y2$listening$status[y2$listening$receiver_serial == "R2"],
               z2$listening$status[z2$listening$receiver_serial == "R2"])
})

test_that("a count is given to a day only when its interval lies inside the day", {
  case <- with_r6()
  # The only count is on the 00:00 record of 3 June, which covers 23:00 on
  # 2 June to 00:00 on 3 June: it may belong to 2 June, so neither day is
  # lifted. The first record of the file has no interval and is left out.
  cnt <- function(t) {
    as.numeric(t == utc("2024-06-03 00:00:00") | t == utc("2024-06-01 00:00:00")) * 4
  }
  y <- audit_listening(case$x, logs = counted_logs("R6", 1:5, cnt))
  expect_equal(unique(y$listening$status[y$listening$receiver_serial == "R6"]), "silent_run")
})

# ---- The audit on NexTrak cadence, from the shipped example ----

# Receiver 800001, whose log is inst/extdata/nextrak_receiver_log.csv
# (data-raw/nextrak_log_example.R), and 800002, with no log, deployed from
# 31 May to 4 June 2024. 800001 heard animal A 5 times on 1 June, the
# detections its log counts; 800002 heard A once on 3 June, so the audit
# covers 1 to 3 June.
nextrak_case <- function() {
  t <- utc(c("2024-06-01 08:03:12", "2024-06-01 08:41:50", "2024-06-01 09:15:07",
             "2024-06-01 13:22:40", "2024-06-01 13:58:31", "2024-06-03 12:00:00"))
  det <- data.frame(datetime = t, receiver_serial = c(rep("800001", 5), "800002"),
                    transmitter = "A", animal_id = "a1", stringsAsFactors = FALSE)
  dep <- data.frame(receiver_serial = c("800001", "800002"), station_id = c("N1", "N2"),
                    deploy_datetime = utc("2024-05-31 12:00:00"),
                    recover_datetime = utc("2024-06-04 12:00:00"),
                    deploy_lat = 45, deploy_lon = -78)
  detset(det, dep, data.frame(animal_id = "a1", transmitter = "A"))
}

nextrak_file <- function() system.file("extdata", "nextrak_receiver_log.csv", package = "detqc")

test_that("the example NexTrak log gives listening, off and logging_silent days", {
  x <- nextrak_case()
  logs <- read_receiver_logs(nextrak_file())
  diag <- logs[logs$record_type == "DIAG_NEXTRAK", ]
  expect_equal(nrow(diag), 288L)
  expect_equal(as.numeric(stats::median(diff(as.numeric(diag$datetime)))), 600)
  expect_true(all(diag$noise_unit == "dB"))
  expect_equal(sum(diag$log_detections), 5)
  expect_equal(logs$event_detail[logs$record_type == "EVENT"], "Power-on reset")
  y <- audit_listening(x, logs = logs)
  n1 <- y$listening[y$listening$receiver_serial == "800001", ]
  expect_equal(n1$date, as.Date(c("2024-06-01", "2024-06-02", "2024-06-03")))
  expect_equal(n1$status, c("listening", "off", "logging_silent"))
  expect_equal(n1$listening, c(TRUE, FALSE, TRUE))
  # 144 DIAG_NEXTRAK and 144 HEALTH_NEXTRAK records a day; none on 2 June
  expect_equal(n1$log_records, c(288L, 0L, 288L))
  expect_equal(n1$pings, c(144 * 40, NA, 144 * 40))
  # Without the log, the two quiet days are silent
  z <- audit_listening(x)
  expect_equal(z$listening$status[z$listening$receiver_serial == "800001"],
               c("listening", "silent", "silent"))
  # 800002 has no log: its days are as without logs
  expect_equal(y$listening$status[y$listening$receiver_serial == "800002"],
               z$listening$status[z$listening$receiver_serial == "800002"])
})

test_that("a NexTrak export with no DIAG record still audits from its other status records", {
  x <- nextrak_case()
  d <- log_dir()
  f <- file.path(d, "no_diag.csv")
  lines <- readLines(nextrak_file())
  writeLines(lines[!startsWith(lines, "DIAG_NEXTRAK")], f)
  expect_message(logs <- read_receiver_logs(f), "no DIAG or DIAG_NEXTRAK record: 1 of 1")
  y <- audit_listening(x, logs = logs)
  n1 <- y$listening[y$listening$receiver_serial == "800001", ]
  expect_equal(n1$status, c("listening", "off", "logging_silent"))
  expect_equal(n1$log_records, c(144L, 0L, 144L))
  expect_true(all(is.na(n1$pings)))
})

test_that("on NexTrak cadence a quiet day the log counted detections on is listening", {
  # The same receiver, which on 3 June also heard a tag of another project
  # at 10:05: not in the set, counted by the 10:10 record.
  x <- nextrak_case()
  d <- log_dir()
  f <- file.path(d, "other_tag.csv")
  heard <- c(x$detections$datetime[x$detections$receiver_serial == "800001"],
             utc("2024-06-03 10:05:00"))
  write_vdat_log(f, "800001", start = "2024-06-01 00:00:00", end = "2024-06-03 23:50:00",
                 variant = "nextrak", off = list(c("2024-06-02 00:00:00", "2024-06-02 23:59:59")),
                 pings = 40, records = c("DIAG_NEXTRAK", "HEALTH_NEXTRAK"), detections = heard)
  y <- audit_listening(x, logs = read_receiver_logs(f))
  expect_equal(y$listening$status[y$listening$receiver_serial == "800001"],
               c("listening", "off", "listening"))
})

test_that("bad logs stop with what is needed", {
  case <- status_case()
  expect_error(audit_listening(case$x, logs = data.frame(a = 1)), "`receiver_serial`, `datetime`")
  bad <- status_logs()
  bad$datetime <- format(bad$datetime)
  expect_error(audit_listening(case$x, logs = bad), "must be a date-time in UTC")
})

test_that("without logs the audit reads detections and reference tags only", {
  ex <- detqc_example
  dets <- detset(ex$detections, ex$deployments, ex$animals)
  a <- audit_listening(dets)
  b <- audit_listening(dets, logs = NULL)
  expect_identical(a$listening, b$listening)
  # Without logs the audit gives these counts and this log text.
  expect_equal(as.vector(table(a$listening$status)[c("listening", "silent", "silent_run")]),
               c(77L, 49L, 54L))
  expect_identical(tail(a$log$reason, 1), paste(
    "180 receiver-days of 20 receivers: 77 listening, 0 partial, 49 silent, 0 reference_missed,",
    "0 reference_silent, 54 silent_run, 0 not_downloaded; 0 reference tags, 0 expected receiver",
    "pairs; 0 silent_run days of receivers known to work kept as silent"
  ))
  expect_identical(tail(a$log$parameters, 1), paste(
    "reference_tags = NULL; min_share = 0.5; max_silent_days = NULL; known_listening = NULL"
  ))
  expect_equal(nrow(stoney_clean$listening), 180L)
  # stoney_clean is audited before the filters, so the 8 receiver-days whose
  # only detections the filters remove stay listening.
  expect_equal(as.vector(table(stoney_clean$listening$status)), c(77L, 103L))
})

test_that("the example logs mark R007 off for two days and leave R020 as it was", {
  ex <- detqc_example
  dets <- detset(ex$detections, ex$deployments, ex$animals)
  files <- list.files(system.file("extdata", "receiver_logs", package = "detqc"),
                      full.names = TRUE)
  logs <- read_receiver_logs(files)
  y <- audit_listening(dets, known_listening = unique(dets$deployments$receiver_serial),
                       logs = logs)
  lst <- y$listening
  expect_equal(lst$date[lst$status == "off"], as.Date(c("2024-06-06", "2024-06-07")))
  expect_equal(unique(lst$receiver_serial[lst$status == "off"]), "R007")
  expect_equal(unique(lst$status[lst$receiver_serial == "R020"]), "silent")
  expect_equal(as.vector(table(lst$status)[c("listening", "logging_silent", "off", "silent")]),
               c(77L, 92L, 2L, 9L))
  # Each receiver's logged count equals the detections it has in the
  # example, every transmitter, so no quiet day has a count to lift it.
  diag <- logs[logs$record_type == "DIAG", ]
  logged <- tapply(diag$log_detections, factor(diag$receiver_serial, sprintf("R%03d", 1:19)), sum)
  heard <- table(factor(ex$detections$receiver_serial, sprintf("R%03d", 1:19)))
  expect_equal(as.vector(logged), as.vector(heard))
  expect_match(tail(y$log$reason, 1), "kept as listening: 0")
})

# ---- What reads the statuses ----

test_that("a deaf or off receiver-day is not listening for detfield, the report and the plots", {
  case <- status_case()
  y <- audit_listening(case$x, reference_tags = case$ref, logs = status_logs())
  inp <- suppressMessages(as_detfield_input(y, "days"))
  lst <- inp$listening
  expect_false(any(lst$listening[lst$status %in% c("deaf", "off")]))
  expect_true(all(lst$listening[lst$status == "logging_silent"]))
  # The fallback, with the logical column dropped, gives the same answer.
  y2 <- y
  y2$listening$listening <- NULL
  inp2 <- suppressMessages(as_detfield_input(y2, "days"))
  expect_equal(inp2$listening$listening, lst$listening)
  # The report names deaf and off among the not-listening statuses.
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(y, f, figures = NULL)
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(html, "not_downloaded, reference_silent, silent_run, deaf, off")
  rc <- report_check(f, y)
  expect_equal(rc$traced$key[!rc$traced$match], character())
  # The timeline marks the two not-listening days of R1 with red ticks.
  p <- plot_timeline(y)
  built <- ggplot2::ggplot_build(p)
  ticks <- Filter(function(d) "label" %in% names(d), built$data)
  expect_gte(sum(vapply(ticks, nrow, 1L)), 2L)
  skip_if_not_installed("detfield")
  # detfield's bin_detections() takes the days and leaves out R1 on 3 and
  # 4 June: no bin of those days lists R1 as listening.
  inp$tag_delay <- data.frame(transmitter = "A", animal_id = "a1", min_delay = 60,
                              max_delay = 120)
  bins <- suppressWarnings(suppressMessages(do.call(detfield::bin_detections,
                                                    c(inp, list(codes = "shared")))))
  r1 <- bins[bins$receiver_serial == "R1", ]
  expect_gt(nrow(r1), 0L)
  expect_false(any(as.Date(r1$bin_start) %in% as.Date(c("2024-06-03", "2024-06-04"))))
  r3 <- bins[bins$receiver_serial == "R3", ]
  expect_true(any(as.Date(r3$bin_start) == as.Date("2024-06-03")))
})
