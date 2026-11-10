test_that("detset() builds the object and logs one step", {
  x <- example_detset()
  expect_s3_class(x, "detset")
  expect_named(x, c("detections", "deployments", "animals", "listening", "log", "meta"))
  expect_equal(nrow(x$detections), nrow(hard_cases()$detections))
  expect_true(is.data.frame(x$detections) && !inherits(x$detections, "data.table"))
  expect_equal(nrow(x$log), 1L)
  expect_equal(x$log$rows_before, x$log$rows_after)
  expect_equal(x$log$animals_after, 20L)
  expect_match(x$log$reason, "station from a deployment for 40,506 of 40,506 detections")
  expect_identical(x$meta$epsg, 4326L)
  expect_identical(validate_detset(x), x)
  expect_identical(detset_log(x), x$log)
})

test_that("a detection at the swap instant belongs to the later deployment", {
  tt <- toy_tables()
  x <- detset(tt$det, tt$dep)
  # R1 moves from A to B at 2024-01-10 12:00:00; the detection at that instant is B.
  expect_equal(x$detections$station_id, c("A", "B", "B", "C", NA))
  # R2 has no recovery time and keeps its detection; R9 has no deployment and
  # stays in the set with no station.
  expect_equal(nrow(x$detections), 5L)
  expect_match(x$log$reason, "1 matched none")
  # The synthetic swap at G10: the detection at 2023-08-01 12:00:00 is on the new receiver.
  ex <- example_detset()$detections
  at_swap <- ex[ex$datetime == as.POSIXct("2023-08-01 12:00:00", tz = "UTC"), ]
  expect_equal(at_swap$receiver_serial, "VR2Tx-489999")
  expect_equal(at_swap$station_id, "G10")
})

test_that("a deployment's station replaces the station a detection carried", {
  tt <- toy_tables()
  tt$det$station_id <- c("A", "X", "B", "C", "Z")
  x <- detset(tt$det, tt$dep)
  expect_equal(x$detections$station_id, c("A", "B", "B", "C", "Z"))
  expect_match(x$log$reason, "1 source station_id value replaced")
})

test_that("listening days follow the half-open intervals", {
  tt <- toy_tables()
  x <- detset(tt$det, tt$dep)
  l <- x$listening
  # R1 at A: 1 to 10 January, the swap day counts for A (deployed until noon)
  # and B; R1 at B: 10 to 19 January; R2 open: 1 January to the last detection, 1 March.
  expect_equal(sum(l$station_id == "A"), 10L)
  expect_equal(sum(l$station_id == "B"), 10L)
  expect_equal(sum(l$station_id == "C"), 61L)
  expect_equal(nrow(x$listening), 81L)
})

test_that("times given as text are read in tz and stored in UTC", {
  tt <- toy_tables()
  tt$det$datetime <- format(tt$det$datetime, "%Y-%m-%d %H:%M:%S")
  tt$dep$deploy_datetime <- format(tt$dep$deploy_datetime, "%Y-%m-%d %H:%M:%S")
  tt$dep$recover_datetime <- format(tt$dep$recover_datetime, "%Y-%m-%d %H:%M:%S")
  x <- detset(tt$det, tt$dep, tz = "America/Toronto")
  expect_identical(attr(x$detections$datetime, "tzone"), "UTC")
  expect_equal(format(x$detections$datetime[1], "%H:%M"), "05:00")
  expect_true(is.na(x$deployments$recover_datetime[3]))
})

test_that("detset() refuses times outside UTC and bad arguments", {
  tt <- toy_tables()
  det <- tt$det
  attr(det$datetime, "tzone") <- "America/Toronto"
  expect_error(detset(det, tt$dep), "detections\\$datetime` is in time zone \"America/Toronto\"")
  expect_error(detset(tt$det, tt$dep, tz = "Mars/Base"), "OlsonNames")
  expect_error(detset(tt$det, tt$dep, crs = "none"), "one EPSG code")
  expect_error(detset(list(), tt$dep), "must be a data frame")
  bad <- tt$det
  bad$transmitter <- NULL
  expect_error(detset(bad, tt$dep), "has no column `transmitter`")
  bad <- tt$det
  bad$animal_id <- 1:5
  expect_error(detset(bad, tt$dep), "detections\\$animal_id` must be character")
})

test_that("detset() checks the deployments", {
  tt <- toy_tables()
  dep <- tt$dep
  dep$recover_datetime[1] <- dep$deploy_datetime[1] - 1
  expect_error(detset(tt$det, dep), paste("1 deployment row ends before it starts, on a receiver",
                                          "the detections are on, such as row 1"))
  # A zero-length deployment holds no detection; it is dropped and counted
  # (hfx_deployments.csv row 244, "caught: 2014-06-22" on its deploy day).
  dep <- tt$dep
  dep$recover_datetime[1] <- dep$deploy_datetime[1]
  expect_warning(z <- detset(tt$det, dep), "1 deployment ends at the moment it starts")
  expect_equal(nrow(z$deployments), 2L)
  expect_match(z$log$reason, "1 zero-length deployment dropped")
  dep <- tt$dep
  dep$deploy_datetime[2] <- NA
  expect_error(detset(tt$det, dep), "1 deployment has no `deploy_datetime`")
  dep <- tt$dep
  dep$deploy_lat[3] <- 95
  expect_error(detset(tt$det, dep), "row 3 has no valid position")
  dep <- tt$dep
  dep$recover_datetime <- NA
  expect_s3_class(detset(tt$det, dep), "detset")
})

test_that("animals fill animal_id by transmitter and never add rows", {
  tt <- toy_tables()
  ani <- data.frame(animal_id = c("a1", "a1", "a3", "a4", "a5"),
                    transmitter = c("T-1", "T-1", "T-3", "T-2", "T-9"), stringsAsFactors = FALSE)
  # The disagreement is warned as well as logged.
  expect_warning(x <- detset(tt$det, tt$dep, ani),
                 "1 of 5 detections carries an animal_id that disagrees with the animals table")
  expect_equal(nrow(x$detections), 5L)
  expect_equal(x$detections$animal_id, c("a1", "a1", "a1", "a2", "a3"))
  expect_match(x$log$reason, "animal_id filled for 2 detections from animals")
  expect_match(x$log$reason, "disagreeing with animals: 1")
  expect_match(x$log$reason, "1 duplicate animal row dropped")
  # A reused transmitter on two animals fills nothing.
  ani2 <- data.frame(animal_id = c("a1", "b1"), transmitter = c("T-1", "T-1"))
  y <- detset(tt$det, tt$dep, ani2)
  expect_equal(nrow(y$detections), 5L)
  expect_true(is.na(y$detections$animal_id[3]))
  expect_match(y$log$reason, "transmitters on more than one animal, filling nothing: 1")
})

test_that("validate_detset() names what is wrong", {
  x <- example_detset()
  expect_error(validate_detset(list()), "must be a detset")
  y <- x
  y$log <- NULL
  expect_error(validate_detset(y), "no `log` part")
  y <- x
  y$detections$sensor_value[1] <- 3
  expect_error(validate_detset(y), "1 detection carries a `sensor_value` with no `sensor_unit`")
  y <- x
  y$detections$sensor_unit <- NULL
  y$detections$sensor_value[1] <- 3
  expect_error(validate_detset(y), "a `sensor_value` with no `sensor_unit`")
  y <- x
  y$meta$epsg <- 4326
  expect_error(validate_detset(y), "meta\\$epsg")
})

test_that("print() and summary() report the counts with their units", {
  x <- example_detset()
  # A line wider than the console scrolls on a 76-column page.
  narrow <- withr::with_options(list(width = 76), utils::capture.output(print(x)))
  expect_true(all(nchar(narrow) <= 76))
  # expect_output() prints at 80 columns; these read the unwrapped lines.
  wide <- function(obj) {
    out <- withr::with_options(list(width = 200), utils::capture.output(print(obj)))
    paste(out, collapse = "\n")
  }
  expect_match(wide(x), paste("40,506 detections of 20 animals \\(22 transmitters\\),",
                              "heard on 25 of 25 receivers at 24 of 24 stations"))
  expect_match(wide(x), "4,676 receiver-days deployed \\(listening not audited\\)")
  y <- audit_listening(x, known_listening = unique(x$deployments$receiver_serial))
  expect_match(wide(y), sprintf("%s of %s receiver-days listening \\(audited\\)",
                                format(sum(y$listening$listening), big.mark = ","),
                                format(nrow(y$listening), big.mark = ",")))
  s <- summary(x)
  expect_s3_class(s, "summary.detset")
  expect_equal(nrow(s$animals), 20L)
  expect_equal(sum(s$animals$detections), 40506L)
  expect_match(wide(s), "Per animal \\(20 animals")
  big <- s
  big$animals <- rbind(s$animals, s$animals)
  expect_match(wide(big), "20 more animals")
})

test_that("detset() warns when the animals table lists none of the detected transmitters", {
  tt <- toy_tables()
  ani <- data.frame(animal_id = c("b1", "b2"), transmitter = c("A69-9001-1", "A69-9001-2"))
  expect_warning(detset(tt$det, tt$dep, ani),
                 "0 of 3 detected transmitters are in the animals table, which lists 2")
  ok <- data.frame(animal_id = c("a1", "a2"), transmitter = c("T-1", "T-2"))
  expect_no_warning(detset(tt$det, tt$dep, ok))
  ex <- hard_cases()
  expect_no_warning(detset(ex$detections, ex$deployments, ex$animals))
})

test_that("print() counts heard receivers among the deployed ones", {
  tt <- toy_tables()
  x <- detset(tt$det, tt$dep)
  # R9 heard a detection and is in no deployment.
  expect_output(print(x), "heard on 2 of 2\\s+receivers\\s+at 3 of 3 stations")
  expect_output(print(x), "1 receiver serial in the detections is in no deployment")
  dep <- tt$dep
  dep$receiver_serial <- NA_character_
  y <- suppressWarnings(detset(tt$det, dep))
  # Every detection then matches a deployment by station, so the print has
  # no receiver count.
  out <- paste(capture.output(print(y)), collapse = " ")
  expect_match(out, "heard at [0-9]+ of 3 stations")
  expect_false(grepl("0 of 0", out, fixed = TRUE))
  # Every detection is matched by station, so no serial line is printed.
  expect_false(grepl("in no deployment", out, fixed = TRUE))
})

# ---- Deployments with no receiver serial ----
# Deployments with no receiver serial: listening rows keyed by station, and
# the glatos hfx station sheet, which names stations and no receivers.

test_that("deployments with no serial keep listening rows keyed by station", {
  tt <- toy_tables()
  dep <- tt$dep
  dep$receiver_serial[3] <- NA
  det <- tt$det
  det$station_id <- c("A", "B", "B", "C", "C")
  x <- suppressWarnings(detset(det, dep))
  bare <- x$listening[is.na(x$listening$receiver_serial), ]
  expect_gt(nrow(bare), 0)
  expect_setequal(unique(bare$station_id), "C")
  # The detection of R2 at C matches no deployment and keeps its own station,
  # which is what detfield matches the bare listening rows by.
  expect_identical(x$detections$station_id[4], "C")
  expect_output(print(x), "1 of 3 deployments has no receiver_serial")
  y <- x
  y$listening$station_id[which(is.na(y$listening$receiver_serial))[1]] <- NA
  expect_error(validate_detset(y), "1 listening rows have neither a receiver_serial")
  y <- x
  y$deployments$station_id[3] <- NA
  expect_error(validate_detset(y), "1 deployment has neither a receiver_serial")
})

test_that("the hfx station sheet gives listening rows that detfield matches", {
  dep <- suppressWarnings(read_deployments(glatos_file("hfx_deployments.csv"), "otn"))
  h <- suppressMessages(read_detections(glatos_file("hfx_detections.csv"), "otn"))
  x <- suppressWarnings(detset(h, dep))
  expect_gt(nrow(x$listening), 0)
  expect_true(all(is.na(x$listening$receiver_serial)))
  expect_false(anyNA(x$listening$station_id))
  expect_false(anyNA(x$detections$station_id))
  skip_if_not_installed("detfield")
  inp <- suppressMessages(as_detfield_input(x, listening = "days"))
  # hfx carries no animal ids; three transmitters stand in for animals.
  inp$detections$animal_id <- inp$detections$transmitter
  tx <- unique(inp$detections$transmitter)[1:3]
  one <- inp$detections$transmitter %in% tx
  delays <- data.frame(transmitter = tx, min_delay = 60, max_delay = 180)
  b <- suppressMessages(detfield::bin_detections(inp$detections[one, ], inp$listening,
                                                 tag_delay = delays, bin = "day"))
  expect_gt(sum(b$y[b$listening]), 0)
  expect_identical(sum(b$y[b$listening]), sum(b$y))
})

test_that("print() and summary() write a count of one in the singular", {
  ex <- detqc_example
  keep <- ex$detections$animal_id %in% "ST-02"
  x <- detset(ex$detections[keep, ], ex$deployments, ex$animals[ex$animals$animal_id == "ST-02", ])
  out <- paste(capture.output(print(x)), collapse = " ")
  expect_match(out, "detections of 1 animal \\(1 transmitter\\)")
  expect_match(out, "1 animal row;")
  expect_match(paste(capture.output(print(summary(x))), collapse = " "), "Per animal \\(1 animal;")
})
