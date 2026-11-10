# Readers at full scope: format detection, OTN extract kinds, VDAT,
# time offsets, all-NA columns, several files, parquet and the tag sheets.

contract_det <- c("datetime", "receiver_serial", "transmitter", "animal_id", "station_id",
                  "sensor_raw", "sensor_value", "sensor_unit")

test_that("auto finds the format of each packaged example, rows in equal rows out", {
  files <- c(vue_example.csv = "vue", glatos_detections_example.csv = "glatos",
             otn_detections_example.csv = "otn", vdat_example.csv = "vdat")
  for (f in names(files)) {
    d <- suppressMessages(read_detections(ext_file(f)))
    expect_equal(attr(d, "source")[[1]]$format, files[[f]], label = f)
    expect_identical(names(d)[1:8], contract_det)
    rows_in <- if (files[[f]] == "vdat") 150L else length(readLines(ext_file(f))) - 1L
    expect_equal(nrow(d), rows_in, label = f)
  }
  expect_error(read_detections(ext_file("vue_example.csv"), "csv"), "must be one of")
  expect_error(read_detections(character()), "one or more paths")
  expect_error(read_detections(ext_file("vue_example.csv"), tz = "Mars/Base"), "OlsonNames")
})

test_that("a file of an unknown layout or a workbook stops with its columns named", {
  f <- write_lines_file(c("when,rx,tag", "2024-05-01 10:00:00,R1,T1"))
  expect_error(read_detections(f),
               "not one read_detections\\(\\) knows: .* in its header. The file has: when, rx, tag")
  expect_error(read_detections(withr::local_tempfile(fileext = ".xlsx")), "File not found")
  wb <- withr::local_tempfile(fileext = ".xlsx")
  file.create(wb)
  expect_error(read_detections(wb), "is a workbook")
})

test_that("an OTN matched extract sets its release records aside and counts them", {
  hdr <- paste("collectionCode,catalogNumber,commonName,detectedBy,station,receiver,tagName,",
               "codeSpace,sensorName,sensorRaw,sensorType,sensorValue,sensorUnit,",
               "dateCollectedUTC,decimalLongitude,decimalLatitude", sep = "")
  rows <- c(
    "P,F-1,walleye,P,,release,A69-1601-1,,,,,,,2024-05-01 12:00:00,-78.1,44.5",
    "P,F-1,walleye,P,S1,VR2W-123456,A69-1601-1,A69-1601,,,,,,2024-05-02 10:00:00,-78.2,44.6",
    paste0("P,F-1,walleye,P,S1,VR2W-123456,A69-1601-1,A69-1601,A69-1601-2,51,depth,7.1,m,",
           "2024-05-02 10:01:00,-78.2,44.6"),
    "P,F-2,walleye,P,,RELEASE,A69-1601-3,,,,,,,2024-05-03 12:00:00,-78.1,44.5"
  )
  f <- write_lines_file(c(hdr, rows))
  expect_message(d <- read_detections(f), "2 OTN release records are not detections")
  expect_equal(nrow(d), 2L)
  expect_equal(attr(d, "source")[[1]]$otn_kind, "matched")
  expect_equal(attr(d, "source")[[1]]$release_rows, 2L)
  rel <- attr(d, "releases")
  expect_equal(rel$animal_id, c("F-1", "F-2"))
  expect_equal(format(rel$release_datetime[1]), "2024-05-01 12:00:00")
  # A sensor detection's code is sensorName; tagName keeps the pinger.
  expect_equal(d$transmitter, c("A69-1601-1", "A69-1601-2"))
  expect_equal(d$tag_name, c("A69-1601-1", "A69-1601-1"))
  expect_equal(d$sensor_value[2], 7.1)
  expect_equal(d$receiver_serial, c("123456", "123456"))
  # The release count reaches the detset's log.
  dep <- data.frame(receiver_serial = "123456", station_id = "S1",
                    deploy_datetime = as.POSIXct("2024-04-01", tz = "UTC"),
                    recover_datetime = as.POSIXct(NA, tz = "UTC"), deploy_lat = 44.6,
                    deploy_lon = -78.2)
  x <- detset(d, dep)
  expect_match(x$log$reason, "2 OTN release records and 0 repeated VDAT records set aside")
  expect_equal(x$meta$sources$rows, 2L)
})

test_that("OTN qualified, unqualified and sentinel extracts give no animal_id", {
  hdr <- paste0("basisOfRecord,institutionCode,collectionCode,dateLastModified,",
                "dateCollectedUTC,uncorrectedDateCollectedUTC,trackerCode,tagName,",
                "catalogNumber,station,receiverSerial,decimalLongitude,decimalLatitude")
  row <- paste0("MachineObservation,X,P,2025-01-01,2023-07-17 14:02:39,2023-07-17 14:01:47,",
                "T,A69-1601-33868,A69-1601-33868-10632667,S9,483512,-73.1,40.6")
  for (k in c("qualified", "unqualified", "sentinel_tag")) {
    f <- file.path(withr::local_tempdir(), sprintf("proj_%s_detections_2023.csv", k))
    writeLines(c(hdr, row), f)
    d <- read_detections(f)
    expect_equal(attr(d, "source")[[1]]$otn_kind, k)
    expect_true(is.na(d$animal_id))
    expect_equal(d$catalog_number, "A69-1601-33868-10632667")
    expect_equal(d$receiver_serial, "483512")
    expect_equal(d$transmitter, "A69-1601-33868")
  }
  f <- write_lines_file(c(hdr, row))
  expect_equal(attr(read_detections(f), "source")[[1]]$otn_kind, "qualified")
})

test_that("an OTN extract matched to external partners stops: it holds counts", {
  lines <- c("collectionCode,catalogNumber,detectedBy,count,yearCollected,detectionRegion,tagName",
             "P,F-1,EXT,12,2023,Gulf,A69-1601-1")
  f <- write_lines_file(lines)
  expect_error(read_detections(f, "otn"), "matched to external partners")
})

test_that("the VDAT reader reads DET and DET_SENS records and drops repeats", {
  expect_message(v <- read_detections(ext_file("vdat_example.csv"), "vdat"),
                 "5 DET records repeat a DET_SENS record")
  expect_equal(nrow(v), 150L)
  expect_equal(attr(v, "source")[[1]]$repeated_records, 5L)
  expect_equal(sum(!is.na(v$sensor_value)), 5L)
  expect_equal(unique(v$sensor_unit[!is.na(v$sensor_value)]), "m/s2")
  expect_equal(unique(v$receiver_serial), "480013")
  expect_equal(unique(v$receiver_model), "VR2Tx-69")
  # Time is the corrected clock: 0.0123 s after the device time.
  expect_equal(format(v$datetime[1], "%Y-%m-%d %H:%M:%OS4"), "2023-05-03 01:06:06.0000")
  cfg <- attr(v, "receiver_transmitters")
  expect_equal(cfg$transmitter, "A69-1601-60013")
  expect_equal(c(cfg$min_delay, cfg$max_delay), c(540, 660))
  # The file was written from the first 150 detections at receiver 480013.
  ex <- hard_cases()$detections
  at_g13 <- sort(ex$datetime[ex$receiver_serial == "VR2Tx-480013"])[1:150]
  expect_equal(sort(v$datetime), at_g13)
})

test_that("VDAT times are read at the row's offset, or from the device clock", {
  det <- c(
    paste0("DET,2024-05-01 10:00:00,2024-05-01 06:00:00.5,-4,0.5,VR2W-69,1001,1,PPM,",
           "A69-1601-1,1,,,,,,,,,,"),
    "DET,2024-05-01 11:00:00,2024-05-01 16:30:00,5.5,0,VR2W-69,1001,1,PPM,A69-1601-1,1,,,,,,,,,,",
    "DET,2024-05-01 12:00:00,,,,VR2W-69,1001,1,PPM,A69-1601-1,1,,,,,,,,,,"
  )
  f <- write_lines_file(vdat_lines(det))
  v <- read_detections(f)
  expect_equal(format(v$datetime, "%H:%M:%OS1"), c("10:00:00.5", "11:00:00.0", "12:00:00.0"))
  expect_identical(attr(v$datetime, "tzone"), "UTC")
  sens <- paste0("DET_SENS,2024-05-01 12:00:00,,,,VR2W-69,1001,1,PPM,A69-1601-2,2,10,,,,,,,,,,",
                 "PPM,Depth,2.5,m,0.1,Tag")
  only <- read_detections(write_lines_file(vdat_lines(character(), sens)))
  expect_equal(only$sensor_value, 2.5)
  expect_null(attr(only, "receiver_transmitters"))
  expect_error(read_detections(ext_file("vue_example.csv"), "vdat"), "not a Fathom VDAT CSV")
  bad <- write_lines_file(c("VEMCO DATA LOG,2.0.0,x", "RECORD TYPE,FIELD", "EVENT_DESC,a,b"))
  expect_error(read_detections(bad, "vdat"), "no DET or DET_SENS record header")
})

test_that("a header or a stamp that states an offset is read at that offset", {
  f <- write_lines_file(c("Date and Time (UTC-04:00),Receiver,Transmitter",
                          "2024-05-01 06:00:00,VR2W-123456,A69-1601-1"))
  d <- read_detections(f)
  expect_equal(attr(d, "source")[[1]]$format, "vue")
  expect_equal(format(d$datetime), "2024-05-01 10:00:00")
  g <- write_lines_file(c("Date and Time (UTC),Receiver,Transmitter",
                          "2024-05-01T06:00:00-04:00,VR2W-1,A69-1601-1",
                          "2024-05-01T10:00:00Z,VR2W-1,A69-1601-1",
                          "2024-05-01 15:30:00+05:30,VR2W-1,A69-1601-1"))
  expect_equal(format(read_detections(g)$datetime), rep("2024-05-01 10:00:00", 3))
  # A file exported in local time, with no zone in its stamps.
  h <- write_lines_file(c("Date and Time (UTC),Receiver,Transmitter",
                          "2024-01-15 05:00:00,VR2W-1,A69-1601-1",
                          "2024-07-15 06:00:00,VR2W-1,A69-1601-1"))
  local <- read_detections(h, tz = "America/Toronto")
  expect_equal(format(local$datetime, "%H"), c("10", "10"))
  expect_equal(header_offsets(c("Date (UTC+5)", "x")), c(date_utc = 5))
})

test_that("an all-NA column reads as NA of the contract's type", {
  hdr <- paste("Date and Time (UTC),Receiver,Transmitter,Transmitter Name,Sensor Value",
               "Sensor Unit,Station Name", sep = ",")
  f <- write_lines_file(c(hdr, "2024-05-01 10:00:00,VR2W-1,A69-1601-1,,,,",
                          "2024-05-01 11:00:00,VR2W-1,A69-1601-1,,,,"))
  d <- read_detections(f)
  expect_equal(nrow(d), 2L)
  expect_true(all(is.na(d$animal_id)) && is.character(d$animal_id))
  expect_true(all(is.na(d$station_id)) && is.character(d$station_id))
  expect_true(all(is.na(d$sensor_raw)) && is.numeric(d$sensor_raw))
  expect_true(all(is.na(d$sensor_value)) && is.numeric(d$sensor_value))
  # An all-NA required time column stops the reader, and says it is empty.
  g <- write_lines_file(c("Date and Time (UTC),Receiver,Transmitter", ",VR2W-1,A69-1601-1"))
  expect_error(read_detections(g, "vue"), "No values for `datetime`.*empty in the file's only row")
})

test_that("several files are read and stacked with their names", {
  files <- c(ext_file("vue_example.csv"), ext_file("vdat_example.csv"))
  d <- suppressMessages(read_detections(files))
  expect_equal(nrow(d), 833L + 150L)
  expect_equal(as.vector(table(d$source_file)[c("vdat_example.csv", "vue_example.csv")]),
               c(150L, 833L))
  expect_length(attr(d, "source"), 2L)
  expect_identical(names(d)[1:8], contract_det)
  hdr <- "collectionCode,catalogNumber,station,receiver,tagName,dateCollectedUTC"
  a <- write_lines_file(c(hdr, "P,F-1,,release,A69-1601-1,2024-05-01 12:00:00",
                          "P,F-1,S1,VR2W-1,A69-1601-1,2024-05-02 12:00:00"))
  m <- suppressMessages(read_detections(c(a, ext_file("vue_example.csv"))))
  expect_equal(nrow(attr(m, "releases")), 1L)
  x <- detset(m, hard_cases()$deployments)
  expect_match(x$log$reason, "1 OTN release record and")
})

test_that("an OTN parquet extract reads as its CSV does", {
  skip_if_not_installed("arrow")
  src <- ext_file("otn_detections_example.csv")
  raw <- utils::read.csv(src, colClasses = "character", check.names = FALSE)
  raw$dateCollectedUTC <- as.POSIXct(raw$dateCollectedUTC, tz = "UTC")
  pq <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(raw, pq)
  a <- read_detections(src)
  b <- read_detections(pq)
  expect_equal(attr(b, "source")[[1]]$format, "otn")
  expect_identical(a[, contract_det], b[, contract_det])
})

test_that("column_map() returns the packaged maps and names a missing format", {
  m <- column_map("detections")
  expect_true(all(c("otn", "otn_qualified", "glatos", "vue", "vdat") %in% m$format))
  expect_named(m, c("format", "target", "source", "sep", "type", "tz", "note"))
  expect_equal(unique(column_map("animals", "innovasea")$format), "innovasea")
  expect_error(column_map("detections", "thelma"), "has no format \"thelma\"")
  bad <- data.frame(target = "datetime", source = "when", type = "datetime", tz = "Mars/Base")
  f <- write_lines_file(c("when,rx", "2024-05-01,R"))
  expect_error(read_detections(f, "vue", col_map = bad), "not in OlsonNames")
})

test_that("deployments and animals take tz, and read_animals counts what it drops", {
  lines <- c("station,ins_serial_no,deploy_date_time,recover_date_time,deploy_lat,deploy_long",
             "S1,1001,2024-05-01 08:00:00,2024-06-01 08:00:00,44.5,-78.1")
  f <- write_lines_file(lines)
  utc <- read_deployments(f, "glatos")
  local <- read_deployments(f, "glatos", tz = "America/Toronto")
  expect_equal(as.numeric(local$deploy_datetime - utc$deploy_datetime, units = "hours"), 4)
  expect_error(read_deployments(f, "glatos", tz = "Nowhere"), "OlsonNames")
  lines <- c("animal_id,transmitter_codespace,transmitter_id,utc_release_date_time",
             "F1,A69-1601,1,2024-05-01 12:00:00", ",A69-1601,2,2024-05-01 12:00:00",
             "F3,A69-1601,1,2024-05-01 12:00:00")
  g <- write_lines_file(lines)
  # F1 and F3 share a transmitter: both animals are kept, with a
  # warning.
  w <- character()
  ani <- withCallingHandlers(read_animals(g, "glatos", tz = "UTC"), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_match(w, "A69-1601-1 \\(F1, F3\\)", all = FALSE)
  # The row with no animal_id is named in the warning.
  expect_match(w, "1 of 3 distinct rows were dropped: 1 have no animal_id", all = FALSE)
  expect_equal(nrow(ani), 2L)
  expect_equal(attr(ani, "source")$dropped_no_id, 1L)
  expect_equal(attr(ani, "source")$dropped_repeat_rows, 0L)
  expect_equal(attr(ani, "source")$reused_transmitters, 1L)
})

test_that("read_tag_specs() reads the InnovaSea sheet as glatos does", {
  path <- glatos_file("lamprey_tag_specs.xls")
  skip_if_not_installed("readxl")
  s <- read_tag_specs(path)
  expect_equal(nrow(s), 4L)
  expect_equal(s$transmitter[3], "A69-9002-7189")
  expect_equal(c(s$sensor_slope[3], s$sensor_intercept[3]), c(0.2198, -0.8794))
  expect_equal(s$sensor_type[3], "depth")
  expect_equal(s$sensor_unit[3], "m")
  expect_equal(c(s$min_delay[1], s$max_delay[1]), c(40, 80))
  skip_if_not_installed("glatos", "0.9.8")
  read_specs <- glatos::read_vemco_tag_specs
  g <- read_specs(path, file_format = "vemco_xls")$specs
  k <- match(s$transmitter, paste(g$code_space, g$id_code, sep = "-"))
  expect_false(anyNA(k))
  expect_equal(s$sensor_slope, g$sensor_slope[k])
  expect_equal(s$sensor_intercept, g$sensor_intercept[k])
  expect_equal(s$tag_serial, g$serial_number[k])
})

test_that("read_tag_specs() reads the lab CSV layout", {
  s <- read_tag_specs(ext_file("tag_specs_example.csv"))
  expect_equal(nrow(s), 7L)
  expect_equal(table(s$sensor_type)[["acceleration"]], 5L)
  expect_equal(unique(s$sensor_unit[s$sensor_type == "depth"]), "m")
  expect_equal(unique(s$max_delay), 160)
  expect_error(read_tag_specs(write_lines_file(c("a,b", "1,2"))), "No column for `transmitter`")
})

# ---- OTN parquet with a geometry column ----
test_that("an OTN parquet extract with a binary geometry column reads, naming the column", {
  skip_if_not_installed("arrow")
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  n <- 3
  # The column names and types of an OTN matched extract in parquet, with
  # invented values; geometry holds well-known binary, as OTN writes it.
  tab <- arrow::arrow_table(
    collectionCode = rep("EXA", n), catalogNumber = paste0("EXA-", 1:n),
    organismID = rep(NA_character_, n), scientificName = "Esox masquinongy",
    commonName = "muskellunge", dateLastModified = utc(rep("2024-06-01", n)),
    detectedBy = "EXA", station = c("S1", "S2", "S1"),
    receiver = c("VR2W-100001", "VR2W-100002", "VR2W-100001"),
    bottomDepth = c(5, 6, 5), receiverDepth = c(4, 5, 4),
    tagName = c("A69-1601-1", "A69-1601-2", "A69-1601-1"), codeSpace = "A69-1601",
    sensorName = rep(NA_character_, n), sensorRaw = rep(NA_real_, n),
    sensorType = rep(NA_character_, n), sensorValue = rep(NA_real_, n),
    sensorUnit = arrow::Array$create(rep(NA, n), type = arrow::null()),
    dateCollectedUTC = utc(c("2024-05-01 10:00:00", "2024-05-01 11:00:00", "2024-05-02 10:00:00")),
    uncorrectedDateCollectedUTC = utc(rep("2024-05-01", n)),
    decimalLongitude = c(-78, -78.01, -78), decimalLatitude = c(45, 45.01, 45),
    geodeticDatum = "WGS84",
    geometry = arrow::Array$create(lapply(1:n, function(i) as.raw(c(1, 1, 0, 0, i))),
                                   type = arrow::binary()),
    localArea = NA_character_, citation = "synthetic", unqDetecID = paste0("u", 1:n),
    contactPI = NA_character_, contactPOC = NA_character_
  )
  pq <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(tab, pq)
  expect_message(d <- read_detections(pq), "1 column holds binary or list values .*: geometry")
  expect_equal(nrow(d), 3L)
  expect_equal(attr(d, "source")[[1]]$format, "otn")
  expect_equal(d$receiver_serial, c("100001", "100002", "100001"))
  expect_false("geometry" %in% names(d))
  expect_equal(format(d$datetime[3]), "2024-05-02 10:00:00")
})

# ---- Raw receiver files and archives ----
test_that("a VRL, a VDAT and a zip file stop with the step that makes them readable", {
  write_bytes <- function(bytes, ext) {
    f <- withr::local_tempfile(fileext = ext, .local_envir = parent.frame())
    writeBin(c(bytes, as.raw(c(3, 1, 0, 0, 0, 255, 17))), f)
    f
  }
  vrl <- write_bytes(charToRaw("VRLF"), ".vrl")
  vdat <- write_bytes(charToRaw("VDAT"), ".vdat")
  zip <- write_bytes(as.raw(c(0x50, 0x4b, 0x03, 0x04)), ".zip")
  for (fmt in c("auto", "vdat", "vue")) {
    expect_error(read_detections(vrl, fmt), "receiver's own VRL file.*Fathom Connect")
    expect_error(read_detections(vdat, fmt), "receiver's own VDAT file.*Fathom Connect")
    expect_error(read_detections(zip, fmt), "is a zip archive. Unzip it")
  }
  expect_error(read_receiver_logs(vrl), "receiver's own VRL file.*read_receiver_logs")
  expect_error(read_receiver_logs(zip), "is a zip archive")
  expect_error(read_detections(c(ext_file("vue_example.csv"), vrl)), "Reading .*Fathom Connect")
})

test_that("the raw receiver files glatos ships are named as raw files", {
  skip_if_not_installed("glatos")
  raw <- list.files(system.file("extdata", "detection_files_raw", package = "glatos"),
                    full.names = TRUE)
  skip_if(!length(raw), "glatos ships no raw receiver files")
  for (f in raw) expect_error(read_detections(f), "Fathom Connect")
})

# ---- OTN receiver files ----
test_that("the OTN moorings and stations-receivers files give their receiver serials", {
  moorings <- write_lines_file(c(
    paste0("\"station_status\",\"collectioncode\",\"station_name\",\"rcvr_location\",",
           "\"deploy_long\",\"deploy_lat\",\"rcvrcatnumber\",\"rcvrserial\",\"rcvrstatus\",",
           "\"instrument\",\"astype\",\"deploy_date\",\"download_date\",\"recovery_date\",",
           "\"downloads\",\"off_set\""),
    "active,EXA,S1,loc,-78,45,EXA-1,100001,returned,VR2W,rcvr,2024-01-01,,2024-06-01,1,0",
    "active,EXA,S2,loc,-78.01,45.01,EXA-2,VR2W-100002,returned,VR2W,rcvr,2024-01-01,,,0,0",
    "active,EXA,S3,loc,-78.02,45.02,EXA-3,,returned,VR2W,rcvr,2024-01-01,,2024-06-01,1,0"
  ))
  m <- read_deployments(moorings)
  expect_equal(attr(m, "source")$format, "otn")
  expect_equal(m$receiver_serial, c("100001", "100002", NA))
  expect_equal(m$station_id, c("S1", "S2", "S3"))
  stations <- write_lines_file(c(
    paste0("FID,collectioncode,seriescode,ocean,station_name,station_type,stn_lat,stn_long,",
           "stationstatus,locality,model,instrumenttype,deploy_date,last_download,",
           "recovery_date,downloads,off_set,depth,rcvrstatus,rcvr_serial,map_code"),
    paste0("f.1,EXA,EXA,ATL,S1,rcvr,45,-78,active,bay,VR2W,rcvr,2024-01-01,2024-03-01,",
           "2024-06-01,1,0,5,returned,100001,EXA"),
    "f.2,EXA,EXA,ATL,S2,rcvr,45.01,-78.01,active,bay,VR2W,rcvr,2024-01-01,,,0,0,6,lost,,EXA"
  ))
  s <- read_deployments(stations)
  expect_equal(attr(s, "source")$format, "otn")
  expect_equal(s$receiver_serial, c("100001", NA))
  expect_equal(format(s$download_datetime), c("2024-03-01", NA))
  expect_true(all(c("rcvrserial", "rcvr_serial") %in%
                    column_map("deployments", "otn")$source))
})

# ---- Deployment and animal sheets: the format ----
test_that("read_deployments() finds a GLATOS receiver file and stops when told it is OTN", {
  f <- ext_file("glatos_receivers_example.csv")
  d <- read_deployments(f)
  expect_equal(attr(d, "source")$format, "glatos")
  expect_false(anyNA(d$station_id))
  expect_error(read_deployments(f, "otn"),
               "format = \"otn\" fills no `station_id`.*format = \"glatos\"",
               class = "detqc_error")
  expect_identical(read_deployments(f, "glatos"), d)
})

test_that("read_animals() finds a GLATOS tagging sheet whose release times are local", {
  sheet <- write_lines_file(c(
    paste0("animal_id,tag_code_space,tag_id_code,tag_serial_number,common_name_e,",
           "glatos_release_date_time,glatos_timezone,utc_release_date_time,release_latitude,",
           "release_longitude"),
    "F1,A69-1601,1,S1,muskellunge,2024-05-01 08:00:00,Eastern,,45,-78",
    "F2,A69-1601,2,S2,muskellunge,2024-05-02 08:00:00,Eastern,,45,-78"
  ))
  a <- read_animals(sheet)
  expect_equal(attr(a, "source")$format, "glatos")
  expect_equal(format(a$release_datetime), c("2024-05-01 12:00:00", "2024-05-02 12:00:00"))
  expect_warning(o <- read_animals(sheet, "otn"), "fills no `release_datetime`")
  expect_true(all(is.na(o$release_datetime)))
  # An OTN tagging sheet with UTC release times reads as OTN.
  otn <- write_lines_file(c(
    "ANIMAL_ID,TAG_CODE_SPACE,TAG_ID_CODE,UTC_RELEASE_DATE_TIME,RELEASE_LATITUDE,RELEASE_LONGITUDE",
    "F1,A69-1601,1,2024-05-01T12:00:00,45,-78"
  ))
  b <- read_animals(otn)
  expect_equal(attr(b, "source")$format, "otn")
  expect_equal(format(b$release_datetime), "2024-05-01 12:00:00")
})

test_that("an OTN tagging sheet with blank ANIMAL_ID stops with a pointer to missing_id", {
  otn <- write_lines_file(c(
    "ANIMAL_ID,TAG_CODE_SPACE,TAG_ID_CODE,UTC_RELEASE_DATE_TIME,RELEASE_LATITUDE,RELEASE_LONGITUDE",
    ",A69-1601,1,2024-05-01T12:00:00,45,-78",
    ",A69-1601,2,2024-05-02T12:00:00,45,-78"
  ))
  err <- tryCatch(read_animals(otn), error = function(cnd) cnd)
  expect_s3_class(err, "detqc_error")
  expect_match(conditionMessage(err), "2 with no animal_id, 0 with no transmitter")
  expect_match(conditionMessage(err), "OTN tagging sheets often leave ANIMAL_ID blank")
  expect_match(conditionMessage(err), "missing_id = \"transmitter\"")
  expect_match(conditionMessage(err), "in ?read_animals", fixed = TRUE)
  a <- read_animals(otn, missing_id = "transmitter")
  expect_equal(a$animal_id, c("A69-1601-1", "A69-1601-2"))
})
