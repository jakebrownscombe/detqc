contract_det <- c("datetime", "receiver_serial", "transmitter", "animal_id", "station_id",
                  "sensor_raw", "sensor_value", "sensor_unit")

test_that("the OTN reader loads 3 of 3 OTN extracts that glatos 0.9.8 ships", {
  files <- c(blue_shark_detections.csv = 3000L, blue_shark_detections_old.csv = 3000L,
             hfx_detections.csv = 65499L)
  loaded <- 0L
  for (f in names(files)) {
    d <- read_detections(glatos_file(f), "otn")
    expect_equal(nrow(d), files[[f]], label = f)
    expect_identical(names(d)[1:8], contract_det)
    expect_identical(attr(d$datetime, "tzone"), "UTC")
    expect_false(anyNA(d$datetime))
    expect_false(anyNA(d$receiver_serial))
    expect_false(anyNA(d$transmitter))
    loaded <- loaded + 1L
  }
  expect_equal(loaded, 3L)
  d <- read_detections(glatos_file("blue_shark_detections.csv"), "otn")
  expect_equal(length(unique(d$animal_id)), 15L)
  expect_equal(format(min(d$datetime)), "2014-08-29 06:11:09")
  h <- read_detections(glatos_file("hfx_detections.csv"), "otn")
  # Model and serial are split, so the serial joins OTN and GLATOS
  # sheets; collector numbers that are not model-serial stay whole.
  expect_equal(h$receiver_serial[1], "315")
  expect_equal(h$receiver_model[1], "VR3UWM")
  expect_equal(attr(h, "source")[[1]]$serials_split, sum(!is.na(h$receiver_model)))
  # collectornumber holds the serial; `receiver` holds OTN deployment ids
  # such as HFX068-00348-1, which are not serials.
  expect_equal(sum(grepl("^HFX", h$receiver_serial)), 0L)
  expect_equal(length(unique(h$receiver_serial)), 372L)
  expect_equal(h$transmitter[1], "A69-1303-49382")
})

test_that("the OTN reader finds columns by name, whatever their order", {
  src <- glatos_file("blue_shark_detections.csv")
  raw <- utils::read.csv(src, check.names = FALSE, colClasses = "character")
  shuffled <- raw[, rev(seq_along(raw))]
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(shuffled, f, row.names = FALSE)
  a <- read_detections(src, "otn")
  b <- read_detections(f, "otn")
  expect_identical(a[, contract_det], b[, contract_det])
})

test_that("the OTN reader loads the two OTN metadata sheets glatos ships", {
  skip_if_not_installed("readxl")
  dep <- read_deployments(glatos_file("hfx_deploy_simplified.xlsx"), "otn")
  expect_equal(nrow(dep), 40L)
  expect_equal(dep$station_id[1], "HFX047")
  expect_equal(format(dep$deploy_datetime[1]), "2014-06-22")
  expect_true(all(is.na(dep$receiver_serial)))
  ani <- read_animals(glatos_file("otn_nsbs_tag_metadata.xls"), "otn")
  expect_equal(nrow(ani), 20L)
  expect_equal(ani$transmitter[1], "A69-9001-24403")
  expect_equal(ani$tag_life_days[1], 3650)
  expect_equal(format(ani$release_datetime[1]), "2014-08-23")
})

test_that("the OTN reader loads OTN deployment CSV and AAT files", {
  w <- character()
  dep <- withCallingHandlers(read_deployments(glatos_file("hfx_deployments.csv"), "otn"),
                             warning = function(cnd) {
                               w <<- c(w, conditionMessage(cnd))
                               invokeRestart("muffleWarning")
                             })
  expect_match(w, "24 values are not dates, such as \"failed\"", all = FALSE)
  # A recovery written with a status keeps its date and the text.
  expect_match(w, "52 recovery values carry a status", all = FALSE)
  expect_equal(nrow(dep), 937L)
  expect_equal(sum(!is.na(dep$recovery_status)), 52L)
  r <- dep[!is.na(dep$recovery_status) & dep$recovery_status == "failed: 2010-03-07", ]
  expect_equal(format(r$recover_datetime), "2010-03-07")
  aat <- read_deployments(glatos_file("otn_aat_receivers.csv"), "otn")
  expect_equal(nrow(aat), 823L)
  expect_equal(aat$receiver_serial[1], "350")
  rel <- read_animals(glatos_file("otn_aat_tag_releases.csv"), "otn")
  expect_equal(nrow(rel), 83L)
})

test_that("the GLATOS reader loads 2 of 2 GLATOS detection exports", {
  w <- read_detections(glatos_file("walleye_detections.csv"), "glatos")
  l <- read_detections(glatos_file("lamprey_detections.csv"), "glatos")
  expect_equal(c(nrow(w), nrow(l)), c(7180L, 5923L))
  # ADC readings are raw: sensor_raw holds them, sensor_value waits for a calibration.
  expect_equal(c(sum(!is.na(w$sensor_raw)), sum(!is.na(l$sensor_raw))), c(4134L, 1499L))
  expect_true(all(is.na(c(w$sensor_value, l$sensor_unit))))
  expect_equal(w$transmitter[1], "A69-9001-32054")
  expect_warning(rcv <- read_deployments(glatos_file("sample_receivers.csv"), "glatos"),
                 "3 of 559 station_id values hold deployments more than 1 km apart")
  expect_equal(nrow(rcv), 898L)
  ani <- read_animals(glatos_file("walleye_detections.csv"), "glatos")
  expect_equal(nrow(ani), 3L)
})

test_that("the VUE reader puts ADC readings in sensor_raw and leaves the unit NA", {
  v <- read_detections(glatos_file("VR2W_109924_20110718_1.csv"), "vue")
  expect_equal(nrow(v), 69708L)
  expect_equal(sum(!is.na(v$sensor_raw)), 1227L)
  expect_true(all(is.na(v$sensor_unit)))
  expect_true(all(is.na(v$deploy_lat)))
  expect_equal(attr(v, "source")[[1]]$tz_source, "UTC")
})

test_that("the readers read the packaged examples with no other package", {
  ext <- function(f) ext_file(f)
  v <- read_detections(ext("vue_example.csv"), "vue")
  g <- read_detections(ext("glatos_detections_example.csv"), "glatos")
  o <- read_detections(ext("otn_detections_example.csv"), "otn")
  expect_equal(nrow(v), nrow(g))
  expect_equal(nrow(v), nrow(o))
  expect_equal(v$datetime, g$datetime)
  expect_equal(sum(!is.na(v$sensor_raw)), sum(!is.na(o$sensor_raw)))
  r <- read_deployments(ext("glatos_receivers_example.csv"), "glatos")
  expect_equal(nrow(r), 25L)
  expect_equal(sum(is.na(r$recover_datetime)), 1L)
  expect_true(all(c("species", "tag_model") %in% names(g)))
})

test_that("a VUE file with no sensor columns still reads", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Date and Time (UTC),Receiver,Transmitter",
               "2024-05-01 10:00:00,VR2W-1,A69-1601-1",
               "2024-05-01 11:00:00,VR2W-1,A69-1601-1"), f)
  d <- read_detections(f, "vue")
  expect_equal(nrow(d), 2L)
  expect_identical(names(d)[1:8], contract_det)
  expect_true(all(is.na(d$sensor_raw)))
})

test_that("a custom column map replaces the packaged one", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("when,rx,tag,fish,depth_raw",
               "2024-05-01 10:00,R1,A69-1601-1,F1,12",
               "2024-05-02 11:30,R1,A69-1601-1,F1,13"), f)
  map <- data.frame(target = c("datetime", "receiver_serial", "transmitter", "animal_id",
                               "sensor_raw"),
                    source = c("when", "rx", "tag", "fish", "depth_raw"),
                    type = c("datetime", "character", "character", "character", "numeric"))
  d <- read_detections(f, "otn", col_map = map)
  expect_equal(d$sensor_raw, c(12, 13))
  expect_equal(format(d$datetime[2]), "2024-05-02 11:30:00")
  mf <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(map, mf, row.names = FALSE)
  expect_equal(read_detections(f, "otn", col_map = mf)$animal_id, c("F1", "F1"))
  expect_error(read_detections(f, "otn", col_map = data.frame(a = 1)),
               "needs columns `target` and `source`")
})

test_that("the readers stop with a message that names the problem", {
  expect_error(read_detections(tempfile(), "otn"), "File not found")
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("station,receiver", "A,R1"), f)
  expect_error(read_detections(f, "otn"),
               "No column for `datetime`.*Looked for: datecollectedutc, datecollected")
  writeLines(c("datecollected,timezone,receiver,tagname,station",
               "2014-01-01 00:00:00,EST,R,T,S"), f)
  expect_error(read_detections(f, "otn"), "timezone column holds EST")
  writeLines(c("datecollected,receiver,tagname", "yesterday,R,T"), f)
  expect_error(read_detections(f, "otn"), "1 value is not a date, such as \"yesterday\"")
})

test_that("text dates, Excel serials and placeholders parse to UTC", {
  p <- parse_utc(c("2011-06-08T14:28:38Z", "41812.5", NA, "-"), "UTC", "x")
  expect_equal(format(p[1:2], "%Y-%m-%d %H:%M:%S"), c("2011-06-08 14:28:38", "2014-06-22 12:00:00"))
  expect_true(all(is.na(p[3:4])))
  expect_equal(format(parse_utc("41812", "UTC", "x")), "2014-06-22")
  expect_warning(q <- parse_utc(c("2014-01-01", "failed"), "UTC", "y", strict = FALSE),
                 "1 value is not a date")
  expect_true(is.na(q[2]))
  expect_equal(format(parse_utc("2024-01-01 00:00", "America/Toronto", "x"), "%H"), "05")
  expect_equal(parse_number(c("3650 days", "+0", "NaN", "-", "x")), c(3650, 0, NA, NA, NA))
  expect_equal(clean_names(c("Date and Time (UTC)", "LENGTH (m)", "﻿animal id")),
               c("date_and_time_utc", "length", "animal_id"))
})

test_that("the sheet whose header names the most mapped columns is read", {
  skip_if_not_installed("readxl")
  # The OTN tagging workbook has a data dictionary sheet before the data sheet.
  ani <- read_animals(glatos_file("otn_nsbs_tag_metadata.xls"), "otn")
  expect_equal(names(ani)[1:2], c("animal_id", "transmitter"))
  expect_error(read_deployments(glatos_file("otn_nsbs_tag_metadata.xls"), "glatos"),
               "No sheet in otn_nsbs_tag_metadata.xls has a header row")
})
