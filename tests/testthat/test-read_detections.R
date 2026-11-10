# The readers and detset() on damaged files and on the layouts of network
# exports. Each fixture is synthetic: cut from the bundled example files or
# written here.

lines_of <- function(name) readLines(ext_file(name), warn = FALSE)

# A copy of a file cut part-way through a line, as an interrupted copy or
# sync leaves it: no line end after the cut.
cut_copy <- function(lines, keep, cut_chars, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  body <- paste0(paste(lines[seq_len(keep)], collapse = "\n"), "\n",
                 substr(lines[keep + 1], 1, cut_chars))
  writeBin(charToRaw(body), f)
  f
}

test_that("a VDAT file cut mid-line drops the cut line with a warning", {
  v <- lines_of("vdat_example.csv")
  k <- max(grep("^DET,", v))
  f <- cut_copy(v, k - 1, 40)
  expect_warning(d <- read_detections(f, "vdat"),
                 "ends part-way through line [0-9]+, a DET record with 3 of 21 fields")
  whole <- suppressMessages(read_detections(ext_file("vdat_example.csv"), "vdat"))
  expect_equal(nrow(d), sum(whole$datetime < max(whole$datetime)) +
                 sum(whole$datetime == max(whole$datetime)) - 1L)
  expect_false(anyNA(d$receiver_serial))
  expect_false(anyNA(d$transmitter))
  expect_equal(attr(d, "source")[[1]]$cut_rows, 1L)
  # A time cut to "2" is not read as Excel day 2 (1900-01-01).
  expect_true(min(d$datetime) > as.POSIXct("2000-01-01", tz = "UTC"))
  expect_warning(p <- parse_utc("2", "UTC", "x", strict = FALSE), "1 value is not a date")
  expect_true(is.na(p))
})

test_that("a VDAT file that declares EVENT_OFFLOAD and holds none is named", {
  v <- lines_of("vdat_example.csv")
  f <- write_lines_file(c(v, "EVENT_OFFLOAD_DESC,Device Time (UTC),Time,Detection Count"))
  expect_warning(read_detections(f, "vdat"), "has no EVENT_OFFLOAD record")
  g <- write_lines_file(c(v, "EVENT_OFFLOAD_DESC,Device Time (UTC),Time,Detection Count",
                          "EVENT_OFFLOAD,2023-09-01 00:00:00,2023-09-01 00:00:00,150"))
  expect_silent(suppressMessages(read_detections(g, "vdat")))
})

test_that("a GLATOS file cut inside a field drops that row; a whole file is untouched", {
  g <- lines_of("glatos_detections_example.csv")
  # Cut row 501 three characters into receiver_sn (column 11).
  pos <- gregexpr(",", g[502])[[1]][10]
  f <- cut_copy(g, 501, pos + 3)
  expect_warning(d <- read_detections(f, "glatos"),
                 "ends part-way through data row 501 \\(11 of 17 fields and no line end\\)")
  expect_equal(nrow(d), 500L)
  expect_false("119" %in% d$receiver_serial)
  expect_false(any(nchar(d$receiver_serial) < 6))
  expect_silent(read_detections(ext_file("glatos_detections_example.csv"), "glatos"))
  # A file whose last line is whole but has no line end keeps it.
  h <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw(paste(g[1:11], collapse = "\n")), h)
  expect_equal(nrow(read_detections(h, "glatos")), 10L)
})

test_that("a last row cut short but ended with a line end is dropped with one warning", {
  g <- lines_of("glatos_detections_example.csv")
  # Cut rows 500 and 501 into their time, each with its line end kept.
  short <- c(g[1:500], substr(g[501], 1, 20), substr(g[502], 1, 20))
  f <- write_lines_file(short)
  expect_warning(d <- read_detections(f, "glatos"),
                 "ends in 2 rows with no `transmitter`, `receiver_serial` or `station_id`")
  expect_equal(nrow(d), 499L)
  expect_false(anyNA(d$transmitter))
  expect_equal(attr(d, "source")[[1]]$cut_rows, 2L)
  expect_equal(attr(d, "source")[[1]]$rows_read, 501L)
  one <- write_lines_file(c(g[1:11], substr(g[12], 1, 20)))
  expect_warning(e <- read_detections(one, "glatos"),
                 "ends in 1 row with no .* The row was dropped")
  expect_equal(nrow(e), 10L)
  # A last row that holds a transmitter is kept, with no receiver serial.
  no_rx <- strsplit(g[12], ",", fixed = TRUE)[[1]]
  no_rx[11] <- ""
  kept <- read_detections(write_lines_file(c(g[1:11], paste(no_rx, collapse = ","))), "glatos")
  expect_equal(nrow(kept), 11L)
  expect_true(is.na(kept$receiver_serial[11]))
})

test_that("an empty file is named, alone and among others", {
  z <- withr::local_tempfile(fileext = ".csv")
  file.create(z)
  expect_error(read_detections(z), "is empty \\(0 bytes\\). A cloud-synced file that is online")
  expect_error(read_detections(z, "vdat"), "is empty \\(0 bytes\\)")
  expect_error(read_detections(c(ext_file("vdat_example.csv"), z)),
               paste0("Reading ", basename(z), ": .*is empty"))
  expect_error(read_deployments(z, "glatos"), "is empty \\(0 bytes\\)")
})

test_that("the source attribute is a list of records for one file as for several", {
  one <- suppressMessages(read_detections(ext_file("vdat_example.csv")))
  two <- suppressMessages(read_detections(c(ext_file("vdat_example.csv"),
                                            ext_file("vue_example.csv"))))
  expect_length(attr(one, "source"), 1L)
  expect_length(attr(two, "source"), 2L)
  expect_equal(vapply(attr(one, "source"), `[[`, "", "format"), "vdat")
  expect_equal(vapply(attr(two, "source"), `[[`, "", "format"), c("vdat", "vue"))
  # detset() reads both shapes into one row per file.
  x <- detset(two, hard_cases()$deployments)
  expect_equal(nrow(x$meta$sources), 2L)
})

test_that("cut keeps part of each file as it is read, and counts the rest", {
  files <- c(ext_file("vdat_example.csv"), ext_file("vue_example.csv"))
  all <- suppressMessages(read_detections(files))
  cut <- list(from = "2023-05-10", to = as.POSIXct("2023-06-01", tz = "UTC"),
              transmitters = c("A69-9004-7001", "A69-1604-5001"))
  expect_message(d <- read_detections(files, cut = cut), "the cut kept [0-9,]+ of [0-9,]+")
  one <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(detqc_example$detections[1, ], one, row.names = FALSE)
  expect_message(read_detections(one, cut = list(from = "2000-01-01")),
                 "the cut kept 1 of 1 detection in 1 file.", fixed = TRUE)
  want <- all$datetime >= as.POSIXct("2023-05-10", tz = "UTC") &
    all$datetime < cut$to & all$transmitter %in% cut$transmitters
  expect_equal(nrow(d), sum(want))
  expect_gt(nrow(d), 0L)
  src <- attr(d, "source")
  expect_equal(sum(vapply(src, `[[`, 0L, "rows_outside_cut")), nrow(all) - nrow(d))
  expect_error(read_detections(files, cut = list(when = 1)), "no element `when`")
  expect_error(read_detections(files, cut = list(from = "soon")), "1 value is not a date")
})

test_that("an OTN qualified extract takes the serial from collectornumber", {
  hdr <- paste("basisofrecord,catalognumber,datecollected,timezone,receiver,station,",
               "collectornumber,fieldnumber,tagname,codespace", sep = "")
  rows <- c("MachineObservation,A69-1303-1,2014-06-01 10:00:00,UTC,HFX068-00348-1,HFX068,348,",
            "MachineObservation,A69-1303-1,2014-06-01 11:00:00,UTC,,HFX008,VR3UWM-315,")
  rows <- paste0(rows, c("A69-1303-1,A69-1303-1,A69-1303", "A69-1303-1,A69-1303-1,A69-1303"))
  f <- write_lines_file(c(hdr, rows), ext = "_qualified.csv")
  d <- suppressMessages(read_detections(f, "otn"))
  expect_equal(d$receiver_serial, c("348", "315"))
})

test_that("a deployment row that names an anchor and no serial is set aside", {
  r <- lines_of("glatos_receivers_example.csv")
  anchor <- sub(",VR2Tx,69,480001,", ",anchor,,,", r[2], fixed = TRUE)
  f <- write_lines_file(c(r, anchor))
  expect_message(dep <- read_deployments(f, "glatos"),
                 "1 row names an instrument that is not a receiver, such as \"anchor\"")
  expect_equal(nrow(dep), length(r) - 1L)
  expect_equal(attr(dep, "source")$dropped_not_receiver, 1L)
  # A receiver model with "release" in a word of its own is set aside; a
  # VR2AR, a receiver with a built-in release, is not.
  expect_equal(not_a_receiver(data.frame(receiver_serial = NA_character_,
                                         receiver_model = c("acoustic release", "VR2AR"))),
               c(TRUE, FALSE))
})

test_that("read_animals counts both causes and fills codes from the specs", {
  hdr <- paste("ANIMAL_ID,TAG_CODE_SPACE,TAG_ID_CODE,TAG_SERIAL_NUMBER,COMMON_NAME_E,",
               "UTC_RELEASE_DATE_TIME", sep = "")
  rows <- c("F1,A69-9004,7001,1500001,walleye,2023-05-01 15:00:00",
            "F2,,7002,1500002,walleye,2023-05-01 15:00:00",
            ",A69-9004,7003,1500003,walleye,2023-05-01 15:00:00")
  f <- write_lines_file(c(hdr, rows))
  w <- character()
  ani <- withCallingHandlers(read_animals(f, "otn"), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_match(w, paste("2 of 3 distinct rows were dropped: 1 have no transmitter .* such as",
                        "animal F2; .*1 have no animal_id, such as transmitter A69-9004-7003"))
  src <- attr(ani, "source")
  expect_equal(c(src$dropped_no_transmitter, src$dropped_no_animal_id), c(1L, 1L))
  expect_equal(ani$animal_id, "F1")
  # With the specs, F2's blank code space is filled by its serial, and a
  # second code of F1's tag joins F1.
  specs <- read_tag_specs(ext_file("tag_specs_example.csv"))
  specs <- rbind(specs, transform(specs[1, ], transmitter = "A69-1105-7001"))
  ani2 <- suppressWarnings(read_animals(f, "otn", tag_specs = specs))
  expect_setequal(ani2$transmitter[ani2$animal_id == "F1"], c("A69-9004-7001", "A69-1105-7001"))
  expect_equal(ani2$transmitter[ani2$animal_id == "F2"], "A69-9004-7002")
  expect_equal(attr(ani2, "source")$second_codes, 1L)
  expect_equal(attr(ani2, "source")$transmitters_from_specs, 1L)
  expect_equal(ani2$tag_model[ani2$transmitter == "A69-1105-7001"], specs$tag_model[1])
  expect_equal(ani2$species[ani2$transmitter == "A69-1105-7001"], "walleye")
})

test_that("a GLATOS export with blank animal_id is named at every step", {
  g <- lines_of("glatos_detections_example.csv")
  blank <- sub("^[^,]*,", ",", g[-1])
  f <- write_lines_file(c(g[1], blank))
  expect_warning(d <- read_detections(f, "glatos"), "animal_id is blank on all 833 detections")
  # The export reduces to one row per animal and transmitter: 4 here.
  expect_error(read_animals(f, "glatos"),
               "none of its 4 distinct rows has both an animal_id and a transmitter")
  ani <- read_animals(f, "glatos", missing_id = "transmitter")
  expect_equal(ani$animal_id, ani$transmitter)
  x <- detset(d, hard_cases()$deployments)
  expect_warning(mb <- detect_mortality(x), "No detection has an animal_id")
  expect_equal(nrow(mb$bands), 0L)
  # Named by transmitter, the animals reach the bands.
  y <- detset(d, hard_cases()$deployments, ani)
  expect_gt(length(unique(stats::na.omit(y$detections$animal_id))), 0L)
})

test_that("read_tag_specs keeps the sheet's sensor type and classes predation", {
  s <- lines_of("tag_specs_example.csv")
  add <- c('"1500099","A69-9004-7099","V7D-2x",120,30,90,"predation (digestion)","",,',
           '"1500098","A69-9004-7098","V7-2x",120,30,90,"none","",,',
           '"1500097","A69-9004-7097","V7-2x",120,30,90,"salinity","",,')
  f <- write_lines_file(c(s, add))
  expect_message(sp <- read_tag_specs(f),
                 "1 of 10 rows has a sensor type no class fits \\(salinity\\)")
  expect_equal(sp$sensor_type[sp$transmitter == "A69-9004-7099"], "predation")
  expect_equal(sp$sensor_type_raw[sp$transmitter == "A69-9004-7099"], "predation (digestion)")
  expect_true(is.na(sp$sensor_type[sp$transmitter == "A69-9004-7098"]))
  expect_equal(attr(sp, "source")$sensor_type_unclassed, 1L)
  expect_equal(attr(sp, "source")$sensor_type_none, 1L)
})

test_that("blank text from fread is a missing animal_id, and conflicts warn", {
  ex <- hard_cases()
  f <- withr::local_tempfile(fileext = ".csv")
  d <- ex$detections
  d$animal_id <- NA
  data.table::fwrite(d, f)
  # As a reader that takes every column as text: an empty column is "".
  back <- as.data.frame(data.table::fread(f, colClasses = "character"))
  back$datetime <- as.POSIXct(back$datetime, tz = "UTC")
  for (cl in c("sensor_raw", "sensor_value")) back[[cl]] <- as.numeric(back[[cl]])
  back$sensor_unit[back$sensor_unit == ""] <- NA
  expect_true(all(back$animal_id == ""))
  x <- detset(back, ex$deployments, ex$animals)
  expect_equal(count_animals(x$detections), count_animals(example_detset()$detections))
  expect_match(x$log$reason,
               paste0(format(nrow(d), big.mark = ","), " blank text values read as NA"))
  # A detection that names another animal than the animals table keeps its
  # own, with a warning that counts them.
  e <- ex$detections
  e$animal_id[e$animal_id %in% "EX-01"][1:3] <- "SOMEONE"
  expect_warning(y <- detset(e, ex$deployments, ex$animals),
                 "3 of 40,506 detections carry an animal_id that disagrees with the animals table")
  expect_equal(sum(y$detections$animal_id %in% "SOMEONE"), 3L)
})

test_that("deployments with no serial join detections on station and time", {
  tt <- toy_tables()
  dep <- tt$dep
  dep$receiver_serial <- NA_character_
  det <- tt$det
  det$station_id <- c("A", "A", "B", "C", "Z")
  x <- detset(det, dep)
  # Detection 2 sits at the end of A's deployment: the half-open rule leaves
  # it outside A, and a station key matches only its own station.
  expect_equal(x$detections$station_id, c("A", "A", "B", "C", "Z"))
  expect_match(x$log$reason, "station from a deployment for 3 of 5 detections")
  expect_match(x$log$reason, "3 on station_id to a deployment with no receiver serial")
  y <- filter_deployment_period(detset(det, dep))
  expect_equal(unlist(utils::tail(y$log$removed_by, 1)), c(outside_deployment = 1L,
                                                           unknown_receiver = 1L))
})

test_that("a missing serial is not one receiver in the checks", {
  tt <- toy_tables()
  dep <- rbind(tt$dep, data.frame(receiver_serial = NA_character_, station_id = c("P", "Q"),
                                  deploy_datetime = tt$dep$deploy_datetime[1],
                                  recover_datetime = as.POSIXct(NA, tz = "UTC"),
                                  deploy_lat = 45.2, deploy_lon = c(-78.2, -78.3)))
  det <- tt$det
  det$receiver_serial[4] <- NA
  res <- check_detset(list(detections = det, deployments = dep),
                      checks = c("overlapping_deployments", "serial_not_deployed"))
  s <- res$summary
  expect_equal(s$flagged[s$check == "overlapping_deployments"], 0L)
  expect_match(s$note[s$check == "overlapping_deployments"],
               "2 deployments with no receiver serial are not tested")
  # R9 and the detection with no serial are counted, the second on its own.
  expect_equal(s$flagged[s$check == "serial_not_deployed"], 2L)
  expect_match(s$note[s$check == "serial_not_deployed"],
               "1 receiver serial appears in no deployment; 1 of the detections has no")
  expect_equal(serial_in_deployments(c(NA, "R1"), c(NA, "R1")), c(FALSE, TRUE))
})

test_that("a backward deployment on a receiver no detection is on is set aside", {
  tt <- toy_tables()
  bad <- data.frame(receiver_serial = "R77", station_id = "LKM-007",
                    deploy_datetime = as.POSIXct("2024-11-18", tz = "UTC"),
                    recover_datetime = as.POSIXct("2024-10-28", tz = "UTC"),
                    deploy_lat = 45, deploy_lon = -78, glatos_project = "GLTKZ")
  dep <- rbind(cbind(tt$dep, glatos_project = "MINE"), bad)
  msg <- paste("1 deployment row ends before it starts, such as row 4: receiver R77 at station",
               "LKM-007 \\(glatos_project GLTKZ\\), deployed 2024-11-18, recovered 2024-10-28")
  expect_warning(x <- detset(tt$det, dep), msg)
  expect_equal(nrow(x$deployments), 3L)
  expect_match(x$log$reason, paste("deployments that end before they start, on receivers no",
                                   "detection is on, set aside: 1"))
  bad$receiver_serial <- "R2"
  expect_error(detset(tt$det, rbind(cbind(tt$dep, glatos_project = "MINE"), bad)),
               "ends before it starts, on a receiver the detections are on.*check_detset")
})

test_that("to_glatos keeps a GLATOS export's own glatos_array and station_no", {
  d <- read_detections(ext_file("glatos_detections_example.csv"), "glatos")
  x <- detset(d, read_deployments(ext_file("glatos_receivers_example.csv"), "glatos"))
  g <- to_glatos(x)
  expect_equal(g$glatos_array, x$detections$glatos_array)
  expect_equal(g$station_no, as.character(x$detections$station_no))
  expect_equal(g$station, x$detections$station_id)
  # A lake array read with no array codes takes station_id.
  v <- to_glatos(example_detset())
  expect_equal(v$glatos_array, example_detset()$detections$station_id)
})

test_that("every listed code takes spec columns", {
  # The sheet lists the pinger code of a two-code tag with its own tag life
  # and no delays; the specs list both codes. A second animal's V13AP tag is
  # absent from the specs.
  hdr <- paste("ANIMAL_ID,TAG_CODE_SPACE,TAG_ID_CODE,TAG_SERIAL_NUMBER,TAG_MODEL,EST_TAG_LIFE,",
               "COMMON_NAME_E,UTC_RELEASE_DATE_TIME", sep = "")
  rows <- c("F1,A69-9004,7001,1500001,V13AP,3650,walleye,2023-05-01 15:00:00",
            "F9,A69-9004,9901,1599999,V13AP,900,walleye,2023-05-01 15:00:00")
  f <- write_lines_file(c(hdr, rows))
  specs <- read_tag_specs(ext_file("tag_specs_example.csv"))
  second <- transform(specs[1, ], transmitter = "A69-1105-7001", sensor_type = "depth",
                      sensor_unit = "m")
  specs <- rbind(specs, second)
  w <- character()
  ani <- withCallingHandlers(read_animals(f, "otn", tag_specs = specs), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  f1 <- ani[ani$animal_id == "F1", ]
  expect_setequal(f1$transmitter, c("A69-9004-7001", "A69-1105-7001"))
  # Both codes carry the delays and sensor columns of the specs.
  expect_false(anyNA(f1[, c("min_delay", "max_delay", "sensor_type", "sensor_slope")]))
  expect_equal(f1$min_delay, rep(specs$min_delay[1], 2))
  expect_equal(f1$sensor_type[f1$transmitter == "A69-9004-7001"], "acceleration")
  # The sheet's own tag life is kept, and the difference is counted and named.
  expect_equal(f1$tag_life_days[f1$transmitter == "A69-9004-7001"], 3650)
  expect_match(w, "1 cell of the sheet differ .*\\(tag_life_days 1\\), such as tag_life_days 3650",
               all = FALSE)
  src <- attr(ani, "source")
  expect_equal(src$spec_conflicts[["tag_life_days"]], 1L)
  expect_gt(src$spec_cells_filled, 0L)
  # The serial absent from the specs is named, sensor tags first.
  expect_match(w, "1 of 2 tag serials in the sheet are not in `tag_specs`, 1 of them .*1599999",
               all = FALSE)
  expect_equal(src$serials_not_in_specs, 1L)
  # The detfield hand-off has a delay for both codes of F1.
  x <- suppressWarnings(detset(hard_cases()$detections[0, ], hard_cases()$deployments, ani))
  td <- as_detfield_input(x)$tag_delay
  expect_true(all(c("A69-9004-7001", "A69-1105-7001") %in% td$transmitter))
})

test_that("a station code reused far away is counted and named", {
  g <- lines_of("glatos_receivers_example.csv")
  # Station G01 again, in another project 1,100 km away.
  far <- sub("^G01,EXA,01,1,,,44.5296516360085,-78.1378162764628",
             "G01,LSA,01,1,,,46.708,-90.825", g[2])
  far <- sub(",480001,", ",131773,", far)
  far <- sub(",\"\",\"\"$", ",\"LSSLS\",\"\"", far)
  f <- write_lines_file(c(g, far))
  expect_warning(dep <- read_deployments(f, "glatos"),
                 paste("1 of 24 station_id values hold deployments more than 1 km apart \\(1 more",
                       "than 50 km; 1 across more than one glatos_project\\), such as G01, 1,0"))
  expect_equal(attr(dep, "source")$stations_over_1km, 1L)
  # The example file itself has no such station.
  expect_equal(attr(read_deployments(ext_file("glatos_receivers_example.csv"), "glatos"),
                    "source")$stations_over_1km, 0L)
})

test_that("download_datetime is read, typed and audited", {
  ex <- hard_cases()
  dep <- ex$deployments
  # As text it is read in `tz` and stored in UTC; a POSIXct in another zone stops.
  dep$download_datetime <- "2023-09-01 12:00:00"
  x <- detset(ex$detections, dep, ex$animals)
  expect_true(is_utc(x$deployments$download_datetime))
  dep_local <- ex$deployments
  dep_local$download_datetime <- as.POSIXct("2023-09-01 12:00:00", tz = "America/Toronto")
  expect_error(detset(ex$detections, dep_local), "download_datetime` is in time zone")
  y <- suppressMessages(suppressWarnings(audit_listening(x)))
  expect_gt(sum(y$listening$status == "not_downloaded"), 0L)
  # A detset changed by hand to text is caught by name before data.table reads it.
  bad <- x
  bad$deployments$download_datetime <- format(bad$deployments$download_datetime)
  expect_error(audit_listening(bad), "deployments\\$download_datetime` must be POSIXct in UTC")
  # OTN's short form and deployment CSV name it; both map to it.
  m <- column_map("deployments", "otn")
  expect_setequal(m$source[m$target == "download_datetime"], c("download_date_time",
                                                               "last_download"))
  hdr <- "station_name,deploy_date,recovery_date,last_download,stn_lat,stn_long,ins_serial_no"
  rows <- c("HFX001,2014-06-01,2014-09-01,2014-08-15,44.1,-63.1,111",
            "HFX002,2014-06-01,,2014-08-20,44.2,-63.2,112")
  d <- suppressWarnings(read_deployments(write_lines_file(c(hdr, rows)), "otn"))
  expect_equal(format(d$download_datetime, "%Y-%m-%d"), c("2014-08-15", "2014-08-20"))
  expect_true(is_utc(d$download_datetime))
})

# ---- Compressed files ----
# Each construction is built here from the example export: whole and cut
# gzip, bzip2 and xz, a gzip of two members, and a gzip that lacks the end of
# its 8-byte end record.
compressed_cases <- function(env = parent.frame()) {
  lines <- lines_of("stoney_detections.csv")
  txt <- charToRaw(paste0(paste(lines, collapse = "\n"), "\n"))
  file_of <- function(ext) withr::local_tempfile(fileext = ext, .local_envir = env)
  pack <- function(raw, open_con, ext) {
    f <- file_of(ext)
    con <- open_con(f, "wb")
    writeBin(raw, con)
    close(con)
    f
  }
  keep <- function(f, n_keep) {
    bytes <- readBin(f, "raw", file.size(f))
    g <- file_of(sub("^[^.]*", "", basename(f)))
    writeBin(bytes[seq_len(n_keep(length(bytes)))], g)
    g
  }
  at90 <- function(n) floor(n * 0.9)
  xz <- function(f, m) xzfile(f, m, compression = 6)
  half <- which(txt == as.raw(10))[length(lines) %/% 2]
  two <- function() {
    a <- pack(txt[seq_len(half)], gzfile, ".csv.gz")
    b <- pack(txt[(half + 1):length(txt)], gzfile, ".csv.gz")
    f <- file_of(".csv.gz")
    writeBin(c(readBin(a, "raw", file.size(a)), readBin(b, "raw", file.size(b))), f)
    f
  }
  gz <- pack(txt, gzfile, ".csv.gz")
  bz <- pack(txt, bzfile, ".csv.bz2")
  xzw <- pack(txt, xz, ".csv.xz")
  big <- charToRaw(paste0(paste(c(lines[1], rep(lines[-1], 40)), collapse = "\n"), "\n"))
  bz_big <- pack(big, bzfile, ".csv.bz2")
  gz_two <- two()
  list(gz = gz, gz_cut = keep(gz, at90), gz_short = keep(gz, function(n) n - 4),
       gz_two = gz_two, gz_two_cut = keep(gz_two, at90),
       bz = bz, bz_cut = keep(bz, at90), bz_big = bz_big, bz_big_cut = keep(bz_big, at90),
       xz = xzw, xz_cut = keep(xzw, at90))
}

test_that("a whole gzip, bzip2 or xz file reads every row with no warning", {
  cc <- compressed_cases()
  plain <- read_detections(ext_file("stoney_detections.csv"))
  for (nm in c("gz", "gz_two", "bz", "xz")) {
    expect_no_warning(d <- read_detections(cc[[nm]]), message = NULL)
    expect_equal(nrow(d), nrow(plain), info = nm)
    expect_identical(d$datetime, plain$datetime, info = nm)
    expect_equal(attr(d, "source")[[1]]$cut_rows, 0L, info = nm)
  }
  # The deployments reader reads a compressed file on the same path.
  rx <- withr::local_tempfile(fileext = ".csv.xz")
  con <- xzfile(rx, "wb")
  writeLines(lines_of("stoney_receivers.csv"), con)
  close(con)
  expect_equal(nrow(read_deployments(rx, format = "glatos")),
               nrow(read_deployments(ext_file("stoney_receivers.csv"), format = "glatos")))
})

test_that("a gzip, bzip2 or xz file cut part-way warns, names the file and drops the part-row", {
  cc <- compressed_cases()
  plain <- read_detections(ext_file("stoney_detections.csv"))
  for (nm in c("gz_cut", "gz_two_cut", "xz_cut")) {
    expect_warning(d <- read_detections(cc[[nm]]),
                   paste0(basename(cc[[nm]]), " is a compressed file whose data stop before"),
                   fixed = TRUE, class = "detqc_warning")
    expect_lt(nrow(d), nrow(plain))
    expect_gt(nrow(d), nrow(plain) / 2)
    expect_identical(d$datetime, plain$datetime[seq_len(nrow(d))], info = nm)
    # The last row read is whole: no padded NA in the columns every row fills.
    expect_false(anyNA(d$transmitter[nrow(d)]), info = nm)
  }
  big_whole <- read_detections(cc$bz_big)
  expect_warning(d <- read_detections(cc$bz_big_cut), "data stop before the end of the stream",
                 class = "detqc_warning")
  expect_lt(nrow(d), nrow(big_whole))
  expect_identical(d$datetime, big_whole$datetime[seq_len(nrow(d))])
  # R's bzip2 decoder drops the text of the read call that meets the cut.
  # The reader keeps every whole line of the complete blocks, so it returns
  # more lines than one pass of 1 MB reads.
  con <- bzfile(cc$bz_big_cut, "rb")
  lines_1mb <- 0
  repeat {
    block <- suppressWarnings(readBin(con, "raw", 1048576))
    if (!length(block)) break
    lines_1mb <- lines_1mb + sum(block == as.raw(10))
  }
  close(con)
  expect_gt(nrow(d), lines_1mb - 1)
})

# Padding, as tape blocks and erased flash memory leave it, sits after the
# last byte of a compressed file.
padded_copy <- function(file, pad) {
  out <- withr::local_tempfile(fileext = ".csv.gz", .local_envir = parent.frame())
  writeBin(c(readBin(file, "raw", file.size(file)), pad), out)
  out
}

test_that("a whole gzip padded with zero or 0xFF bytes reads with no warning", {
  cc <- compressed_cases()
  plain <- read_detections(ext_file("stoney_detections.csv"))
  for (nm in c("gz", "gz_two")) {
    for (pad in list(raw(512), as.raw(rep(0xff, 512)))) {
      expect_no_warning(d <- read_detections(padded_copy(cc[[nm]], pad)), message = NULL)
      expect_equal(nrow(d), nrow(plain), info = nm)
    }
  }
  # Cut inside its end record and then padded, the compressed text is whole.
  expect_no_warning(d <- read_detections(padded_copy(cc$gz_short, raw(512))), message = NULL)
  expect_equal(nrow(d), nrow(plain))
})

test_that("a whole gzip with 1 to 3 bytes after it reads with no warning", {
  cc <- compressed_cases()
  plain <- read_detections(ext_file("stoney_detections.csv"))
  tails <- list(as.raw(0xff), as.raw(rep(0xff, 2)), as.raw(rep(0xff, 3)), charToRaw("   "),
                as.raw(c(0x1f, 0x8b)), as.raw(c(0x1f, 0x8b, 0x08)), raw(3))
  for (pad in tails) {
    expect_no_warning(d <- read_detections(padded_copy(cc$gz, pad)), message = NULL)
    expect_equal(nrow(d), nrow(plain))
  }
  # A gzip of two members followed by the start of a third reads whole too.
  expect_no_warning(d <- read_detections(padded_copy(cc$gz_two, as.raw(c(0x1f, 0x8b)))),
                    message = NULL)
  expect_equal(nrow(d), nrow(plain))
  # 4 or more bytes that begin as a gzip member stop the reader.
  expect_error(read_detections(padded_copy(cc$gz, as.raw(c(0x1f, 0x8b, 0x08, 0x00)))),
               "whose stream stops before its end", class = "detqc_error")
})

test_that("a gzip cut inside its end record reads only when the length alone is cut and refilled", {
  cc <- compressed_cases()
  plain <- read_detections(ext_file("stoney_detections.csv"))
  b <- readBin(cc$gz, "raw", file.size(cc$gz))
  short <- function(k, tail = raw(0)) {
    out <- withr::local_tempfile(fileext = ".csv.gz", .local_envir = parent.frame())
    writeBin(c(b[seq_len(length(b) - k)], tail), out)
    out
  }
  ff2 <- as.raw(c(0xff, 0xff))
  # Nothing after the cut, or fewer bytes than it lacks, or a cut check value
  for (f in list(short(1), short(2), short(4), short(4, ff2), short(5, raw(512)),
                 short(8, as.raw(rep(0xff, 512))))) {
    expect_error(read_detections(f), "whose stream stops before its end", class = "detqc_error")
  }
  # The length cut and refilled: every row, with the warning unless padded
  for (f in list(short(1, ff2), short(2, ff2), short(4, charToRaw("    ")))) {
    expect_warning(d <- read_detections(f), "data stop before the end of the stream",
                   class = "detqc_warning")
    expect_equal(nrow(d), nrow(plain))
  }
  for (f in list(short(2, raw(4)), short(4, as.raw(rep(0xff, 4))))) {
    expect_no_warning(d <- read_detections(f), message = NULL)
    expect_equal(nrow(d), nrow(plain))
  }
})

test_that("a cut gzip with 1 to 3 0xFF bytes after it is read to the cut", {
  cc <- compressed_cases()
  cut <- suppressWarnings(read_detections(cc$gz_cut))
  for (k in 1:3) {
    f <- padded_copy(cc$gz_cut, as.raw(rep(0xff, k)))
    expect_warning(d <- read_detections(f), "data stop before the end of the stream",
                   class = "detqc_warning")
    expect_identical(d$datetime, cut$datetime)
  }
  # The run is taken off only after the decoder fails on the file.
  expect_identical(trailing_pad(as.raw(c(1, 255, 255, 255))), 0L)
  expect_identical(trailing_pad(as.raw(c(1, 255, 255, 255)), ff_min = 1L), 3L)
})

test_that("a cut gzip padded with zero or 0xFF bytes is read to the cut, with the warning", {
  cc <- compressed_cases()
  cut <- suppressWarnings(read_detections(cc$gz_cut))
  for (pad in list(raw(512), as.raw(rep(0xff, 512)))) {
    f <- padded_copy(cc$gz_cut, pad)
    expect_warning(d <- read_detections(f),
                   paste0(basename(f), " is a compressed file whose data stop before"),
                   fixed = TRUE, class = "detqc_warning")
    expect_identical(d$datetime, cut$datetime)
  }
})

test_that("a failed decode leaves no text file in tempdir()", {
  cc <- compressed_cases()
  before <- list.files(tempdir())
  expect_error(read_detections(cc$gz_short), class = "detqc_error")
  expect_error(read_detections(cc$bz_cut), class = "detqc_error")
  expect_length(setdiff(list.files(tempdir()), before), 0L)
})

test_that("a gzip without its end record, or a small cut bzip2, stops and names the file", {
  cc <- compressed_cases()
  expect_error(read_detections(cc$gz_short),
               paste(basename(cc$gz_short), "is a gzip file whose stream stops before its end"),
               fixed = TRUE, class = "detqc_error")
  expect_error(read_deployments(cc$gz_short, format = "glatos"), class = "detqc_error")
  # bzip2 decodes whole 900 kB blocks: a small file cut part-way holds no text.
  expect_error(read_detections(cc$bz_cut),
               paste(basename(cc$bz_cut), "holds no line of text"), fixed = TRUE,
               class = "detqc_error")
  expect_error(read_deployments(cc$bz_cut, format = "glatos"), "900 kB", class = "detqc_error")
})

# ---- The readers, the animals table, calibration and the converters ----
# The readers, the animals and tags table, calibration and the converters.
# Each test names the behaviour it pins.

planted_reuse <- function(env = parent.frame()) {
  ex <- hard_cases()
  ani <- ex$animals[!duplicated(ex$animals$transmitter), ]
  tx <- "A69-1604-5001"
  d <- ex$detections
  sel <- d$transmitter == tx
  cut <- sort(d$datetime[sel])[floor(sum(sel) / 2)]
  sp <- strsplit(ani$transmitter, "-")
  codespace <- vapply(sp, function(z) paste(z[1:2], collapse = "-"), "")
  csv <- data.frame(animal_id = ani$animal_id, transmitter_codespace = codespace,
                    transmitter_id = vapply(sp, `[`, "", 3),
                    utc_release_date_time = format(ani$release_datetime, "%Y-%m-%d %H:%M:%S"))
  extra <- csv[ani$transmitter == tx, ][1, ]
  first_id <- extra$animal_id
  extra$animal_id <- "REUSED-B"
  extra$utc_release_date_time <- format(cut, "%Y-%m-%d %H:%M:%S")
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  utils::write.csv(rbind(csv, extra), f, row.names = FALSE)
  d$animal_id[sel] <- NA
  list(file = f, det = d, dep = ex$deployments, tx = tx, cut = cut, first_id = first_id,
       n_csv = nrow(csv) + 1L)
}

test_that("a reused tag keeps both animals and detset() splits it at the release", {
  p <- planted_reuse()
  expect_warning(ani <- read_animals(p$file, "glatos"), "1 transmitter is on more than one")
  expect_equal(nrow(ani), p$n_csv)
  expect_setequal(ani$animal_id[ani$transmitter == p$tx], c(p$first_id, "REUSED-B"))
  x <- suppressWarnings(detset(p$det, p$dep, ani))
  d <- x$detections[x$detections$transmitter == p$tx, ]
  rel1 <- ani$release_datetime[ani$animal_id == p$first_id]
  expect_equal(sum(d$animal_id %in% "REUSED-B"), sum(d$datetime >= p$cut))
  expect_equal(sum(d$animal_id %in% p$first_id), sum(d$datetime >= rel1 & d$datetime < p$cut))
  expect_equal(sum(is.na(d$animal_id)), sum(d$datetime < rel1))
  expect_match(x$log$reason[1],
               "transmitters on more than one animal with distinct release times: 1, filling")
})

test_that("detections before the first release of a reused tag stay NA and are counted", {
  p <- planted_reuse()
  ani <- suppressWarnings(read_animals(p$file, "glatos"))
  rel1 <- ani$release_datetime[ani$animal_id == p$first_id]
  early <- p$det[p$det$transmitter == p$tx, ][1:2, ]
  early$datetime <- rel1 - c(7200, 3600)
  x <- suppressWarnings(detset(rbind(p$det, early), p$dep, ani))
  d <- x$detections[x$detections$transmitter == p$tx, ]
  n_before <- sum(d$datetime < rel1)
  expect_gte(n_before, 2L)
  expect_equal(sum(is.na(d$animal_id)), n_before)
  expect_match(x$log$reason[1], sprintf("leaving %d before the first release NA", n_before))
  # A tied release time names neither animal, so the transmitter fills nothing.
  tied <- ani
  tied$release_datetime[tied$animal_id == "REUSED-B"] <- rel1
  y <- suppressWarnings(detset(p$det, p$dep, tied))
  expect_true(all(is.na(y$detections$animal_id[y$detections$transmitter == p$tx])))
  expect_match(y$log$reason[1], "transmitters on more than one animal, filling nothing: 1")
})

test_that("a GLATOS workbook reads as glatos reads it", {
  skip_if_not_installed("readxl")
  gl <- glatos_file("walleye_workbook.xlsx")
  skip_if_not_installed("glatos", "0.9.8")
  read_wb <- glatos::read_glatos_workbook
  # The comparison needs glatos's own reading. glatos 0.9.8 built from source
  # on the Linux CI runner stops inside its reader on this workbook (a tibble
  # size error), while detqc reads the file there.
  gw <- tryCatch(suppressWarnings(read_wb(gl)), error = function(e) {
    skip(paste("glatos cannot read its own workbook here:", conditionMessage(e)))
  })
  # glatos's example workbook moves 2 stations by about 2 km.
  expect_warning(dep <- read_deployments(gl, "glatos"), "2 of 210 station_id values")
  expect_equal(nrow(dep), 1039L)
  expect_equal(attr(dep, "source")$recoveries_joined, 1039L - 69L)
  pair <- function(s, t) sort(paste(s, format(t, "%Y-%m-%d %H:%M:%S")))
  expect_identical(pair(dep$receiver_serial, dep$deploy_datetime),
                   pair(gw$receivers$ins_serial_no, gw$receivers$deploy_date_time))
  expect_identical(pair(dep$receiver_serial, dep$recover_datetime),
                   pair(gw$receivers$ins_serial_no, gw$receivers$recover_date_time))
  expect_warning(ani <- read_animals(gl, "glatos"), "90 transmitters are on more than one")
  expect_equal(nrow(ani), 543L)
  ga <- gw$animals
  expect_setequal(paste(ani$animal_id, ani$transmitter, format(ani$release_datetime)),
                  paste(ga$animal_id, paste(ga$tag_code_space, ga$tag_id_code, sep = "-"),
                        format(ga$utc_release_date_time)))
})

test_that("GLATOS local times are read at the row's zone", {
  head <- paste0("station,ins_serial_no,deploy_date_time,glatos_deploy_date_time,",
                 "glatos_timezone,recover_date_time,deploy_lat,deploy_long")
  f <- write_lines_file(c(head, "S1,1001,,2024-05-01 08:00:00,Eastern,,44.5,-78.1",
                          "S2,1002,,2024-01-15 08:00:00,eastern,,44.5,-78.1",
                          "S3,1003,2024-05-01 09:00:00,2024-05-01 08:00:00,Central,,44.5,-78.1",
                          "S4,1004,,2024-05-01 08:00:00,Atlantis,,44.5,-78.1"))
  w <- character()
  dep <- withCallingHandlers(read_deployments(f, "glatos"), warning = function(e) {
    w <<- c(w, conditionMessage(e))
    invokeRestart("muffleWarning")
  })
  expect_match(w[1], "1 row names a time zone that is not in OlsonNames.*\"Atlantis\"")
  expect_match(w[2], "1 deployment row has no deployment time .*\\(station S4\\)")
  expect_equal(format(dep$deploy_datetime, "%H:%M"), c("12:00", "13:00", "09:00"))
  expect_equal(dep$station_id, c("S1", "S2", "S3"))
  # A tz argument does not move a time the reader already converted.
  dep2 <- suppressWarnings(read_deployments(f, "glatos", tz = "Asia/Tokyo"))
  expect_equal(format(dep2$deploy_datetime[1:2], "%H:%M"), c("12:00", "13:00"))
})

test_that("unitless values that are not counts stay out of sensor_raw", {
  specs <- read_tag_specs(ext_file("tag_specs_example.csv"))
  v <- utils::read.csv(ext_file("vue_example.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  adc <- v[["Sensor Unit"]] %in% "ADC"
  k <- match(v$Transmitter, specs$transmitter)
  v[["Sensor Value"]][adc] <- specs$sensor_intercept[k[adc]] +
    specs$sensor_slope[k[adc]] * v[["Sensor Value"]][adc]
  v[["Sensor Unit"]] <- NULL
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(v, f, row.names = FALSE, na = "")
  expect_warning(d <- read_detections(f), sprintf("%d sensor readings", sum(adc)))
  expect_equal(sum(!is.na(d$sensor_raw)), 0L)
  expect_equal(sum(!is.na(d$sensor_value)), sum(adc))
  ex <- hard_cases()
  expect_error(suppressWarnings(detset(d, ex$deployments, ex$animals)),
               "carry a `sensor_value` with no `sensor_unit`")
})

test_that("calibrate_sensors() will not calibrate a value that is not a count", {
  x <- example_detset()
  raw <- which(!is.na(x$detections$sensor_raw))[1:2]
  x$detections$sensor_raw[raw] <- c(-1, 0.448)
  specs <- read_tag_specs(ext_file("tag_specs_example.csv"))
  expect_warning(y <- calibrate_sensors(x, specs), "2 sensor_raw readings, such as -1")
  expect_true(all(is.na(y$detections$sensor_value[raw])))
  expect_match(detset_log(y)$reason[2], "not whole counts of zero or more, so not\\s+calibrated: 2")
})

test_that("to_ato() tables carry ATO's template columns and release_location", {
  x <- example_detset()
  x$animals$release_lat <- 44.5
  x$animals$release_lon <- -78.1
  a <- to_ato(x)
  # ATO 0.0.0.9006, R/ATO_class.R.
  det <- c("datetime", "frac_second", "receiver_serial", "transmitter", "sensor_value", "valid")
  dep <- c("deploy_datetime", "recover_datetime", "deploy_location", "deploy_lat", "deploy_lon",
           "deploy_z", "recover_lat", "recover_lon", "receiver_manufacturer", "receiver_model",
           "receiver_serial", "receiver_codeset", "transmitter", "transmitter_manufacturer",
           "transmitter_ping_rate", "transmitter_model", "transmitter_serial", "valid")
  tag <- c("manufacturer", "model", "power_level", "ping_rate", "ping_variation", "serial",
           "transmitter", "activation_datetime", "battery_life", "sensor_type", "sensor_unit",
           "animal", "valid")
  ani <- c("animal", "release_location", "release_datetime", "release_lat", "release_lon",
           "capture_location", "capture_datetime", "capture_lat", "capture_lon", "valid")
  tmpl <- list(det = det, dep = dep, tag = tag, ani = ani)
  for (t in names(tmpl)) {
    expect_identical(names(a[[t]])[seq_along(tmpl[[t]])], tmpl[[t]])
    expect_identical(names(a[[t]])[length(tmpl[[t]]) + 1], "tz")
    expect_true(all(a[[t]]$valid))
  }
  expect_false("animal" %in% names(a$det))
  expect_equal(unique(a$ani$release_location), "R1")
  expect_true(is_utc(a$ani$capture_datetime) && is_utc(a$tag$activation_datetime))
  expect_error(to_ato(x, as = "ATO"), "is not supported")
})

test_that("a header-only file reads as zero rows, alone or with others", {
  vue_head <- readLines(ext_file("vue_example.csv"), n = 1)
  f <- write_lines_file(vue_head)
  d <- read_detections(f)
  expect_equal(nrow(d), 0L)
  expect_true(is_utc(d$datetime) && is.character(d$transmitter) && is.numeric(d$sensor_raw))
  expect_equal(attr(d, "source")[[1]]$rows_read, 0L)
  both <- read_detections(c(ext_file("vue_example.csv"), f))
  expect_equal(nrow(both), 833L)
  v <- write_lines_file(vdat_lines(character()))
  expect_equal(nrow(read_detections(v)), 0L)
  g <- write_lines_file(readLines(ext_file("glatos_detections_example.csv"), n = 1))
  expect_equal(nrow(read_detections(g)), 0L)
  expect_equal(nrow(read_animals(g, "glatos")), 0L)
  r <- write_lines_file(readLines(ext_file("glatos_receivers_example.csv"), n = 1))
  expect_equal(nrow(read_deployments(r, "glatos")), 0L)
})

test_that("one actel receiver per receiver_serial", {
  x <- example_detset()
  rs <- sort(unique(x$deployments$receiver_serial))
  otn <- stats::setNames(c(sprintf("HFX%03d-25%04d-1", seq_along(rs[-1]), seq_along(rs[-1])),
                           "144(lost/found)"), rs)
  x$deployments$receiver_serial <- unname(otn[x$deployments$receiver_serial])
  x$detections$receiver_serial <- unname(otn[x$detections$receiver_serial])
  a <- suppressWarnings(to_actel(x, "UTC"))
  rx <- attr(a, "receivers")
  expect_equal(nrow(rx), length(rs))
  expect_equal(length(unique(rx$Receiver)), length(rs))
  expect_equal(nrow(a$detections), nrow(x$detections))
  expect_equal(unname(attr(a, "dropped")), c(0L, 0L))
  # Serials that are numbers keep them; a model prefix is dropped.
  b <- attr(suppressWarnings(to_actel(example_detset(), "UTC")), "receivers")
  expect_identical(as.character(b$Receiver), split_receiver(b$receiver_serial)$serial)
})

test_that("the 372 hfx receivers stay 372 actel receivers", {
  # collectornumber holds the serial; the OTN deployment id in `receiver`
  # would give 487 receivers.
  h <- suppressMessages(read_detections(glatos_file("hfx_detections.csv")))
  rx <- actel_receivers(data.frame(receiver_serial = character()), h)
  expect_equal(length(unique(h$receiver_serial)), 372L)
  expect_equal(length(unique(rx$Receiver)), 372L)
})

test_that("raw sensor readings go to actel as ADC", {
  x <- example_detset()
  a <- suppressWarnings(to_actel(x, "UTC"))
  expect_equal(sum(a$detections$Sensor.Unit %in% "ADC"), sum(!is.na(x$detections$sensor_raw)))
})

test_that("the actel round trip is counted and whole", {
  skip_if_not_installed("actel")
  preload <- actel::preload
  tz <- "America/Toronto"
  x <- example_detset()
  d <- x$detections
  r1 <- d[d$transmitter == "A69-1604-5001", ][1, ]
  early <- r1
  early$datetime <- x$animals$release_datetime[x$animals$transmitter == "A69-1604-5001"][1] - 3600
  unk <- r1
  unk$transmitter <- "A69-9001-99999"
  unk$animal_id <- NA
  x2 <- detset(rbind(d, early, unk), hard_cases()$deployments, hard_cases()$animals)
  w <- character()
  a2 <- withCallingHandlers(to_actel(x2, tz), warning = function(e) {
    w <<- c(w, conditionMessage(e))
    invokeRestart("muffleWarning")
  })
  expect_true(any(grepl("actel::preload\\(\\) will drop 2 of 40,508 detections", w)))
  expect_equal(unname(attr(a2, "actel_drops")), c(1L, 1L))
  withr::local_dir(withr::local_tempdir())
  p2 <- suppressWarnings(suppressMessages(do.call(preload, c(a2, list(tz = tz)))))
  kept <- sum(vapply(p2$detections.list, nrow, integer(1)))
  expect_equal(kept, nrow(a2$detections) - sum(attr(a2, "actel_drops")))
  # The clean example comes back with its receivers and its raw readings.
  a <- suppressWarnings(to_actel(x, tz))
  p <- suppressWarnings(suppressMessages(do.call(preload, c(a, list(tz = tz)))))
  y <- suppressWarnings(read_detset_from(p, receivers = attr(a, "receivers")))
  key <- function(d) {
    sort(paste(format(d$datetime), d$transmitter, d$station_id, d$animal_id, d$receiver_serial))
  }
  expect_identical(key(y$detections), key(x$detections))
  expect_equal(sum(!is.na(y$detections$sensor_raw)), sum(!is.na(x$detections$sensor_raw)))
  expect_false(anyNA(y$detections$receiver_model))
  expect_match(detset_log(y)$reason[2], "receiver names restored")
  expect_error(read_detset_from(p, receivers = data.frame(a = 1)), "`receivers` attribute")
})

test_that("to_actel() stops on a reused transmitter, which actel cannot read", {
  x <- example_detset()
  extra <- x$animals[1, ]
  extra$animal_id <- "REUSED-B"
  extra$release_datetime <- extra$release_datetime + 86400 * 30
  # With no detection of its own, the second animal is left out.
  y <- suppressWarnings(detset(x$detections, x$deployments, rbind(x$animals, extra)))
  expect_message(suppressWarnings(to_actel(y, "UTC")), "1 animal shares a transmitter")
  # With detections of both animals on one code, actel cannot read it.
  d <- x$detections
  after <- d$transmitter == extra$transmitter & d$datetime > extra$release_datetime
  d$animal_id[after] <- "REUSED-B"
  y2 <- suppressWarnings(detset(d, x$deployments, rbind(x$animals, extra)))
  expect_error(to_actel(y2, "UTC"), "one animal per transmitter")
})

test_that("daylight-saving folds warn and gaps name the remedy", {
  head <- "Date and Time (UTC),Receiver,Transmitter"
  f <- write_lines_file(c(head, "2024-11-03 01:10:00,VR2W-1,A69-1601-1",
                          "2024-11-03 01:30:00,VR2W-1,A69-1601-1",
                          "2024-07-01 12:00:00,VR2W-1,A69-1601-1"))
  expect_warning(d <- read_detections(f, "vue", tz = "America/Toronto"),
                 "2 clock times fall in the repeated autumn hour")
  expect_equal(format(d$datetime), c("2024-11-03 06:10:00", "2024-11-03 06:30:00",
                                     "2024-07-01 16:00:00"))
  g <- write_lines_file(c(head, "2024-03-10 02:30:00,VR2W-1,A69-1601-1"))
  expect_error(read_detections(g, "vue", tz = "America/Toronto"), "spring daylight-saving gap")
  # A fixed-offset zone reads the same stamp.
  fixed <- read_detections(g, "vue", tz = "Etc/GMT+5")
  expect_equal(format(fixed$datetime), "2024-03-10 07:30:00")
  # A recovery in the gap is not strict: NA with a warning.
  r <- write_lines_file(c(
    "station,ins_serial_no,deploy_date_time,recover_date_time,deploy_lat,deploy_long",
    "S1,1001,2024-01-01 08:00:00,2024-03-10 02:30:00,44.5,-78.1"
  ))
  expect_warning(dep <- read_deployments(r, "glatos", tz = "America/Toronto"),
                 "daylight-saving gap")
  expect_true(is.na(dep$recover_datetime))
})

test_that("a repeated header row stops the reader", {
  lines <- readLines(ext_file("glatos_detections_example.csv"))
  f <- write_lines_file(c(lines[1:100], lines[1], lines[101:120]))
  expect_error(read_animals(f, "glatos"), "Data row 100 of .* repeats the header")
  expect_error(read_detections(f), "repeats the header")
})

test_that("the format is the one whose columns the header fills best", {
  g <- utils::read.csv(ext_file("glatos_detections_example.csv"), check.names = FALSE,
                       stringsAsFactors = FALSE)
  g[["Date and Time (UTC)"]] <- g$detection_timestamp_utc
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(g, f, row.names = FALSE, na = "")
  d <- read_detections(f)
  expect_equal(attr(d, "source")[[1]]$format, "glatos")
  expect_equal(nrow(d), 833L)
  both <- write_lines_file(paste0("Date and Time (UTC),Receiver,Transmitter,",
                                  "detection_timestamp_utc,receiver_sn,transmitter_codespace,",
                                  "transmitter_id"))
  expect_error(read_detections(both), "\"vue\" and \"glatos\" formats equally well")
})

test_that("rows_read counts the file's rows before any set aside", {
  v <- suppressMessages(read_detections(ext_file("vdat_example.csv")))
  src <- attr(v, "source")[[1]]
  expect_equal(c(src$rows_read, src$rows, src$repeated_records), c(155L, 150L, 5L))
  lines <- readLines(ext_file("otn_detections_example.csv"))
  rel <- sub("^([^,]*,[^,]*,)[^,]*", "\\1release", lines[2:3])
  f <- write_lines_file(c(lines, rel))
  o <- suppressMessages(read_detections(f))
  src <- attr(o, "source")[[1]]
  expect_equal(src$rows_read, length(lines) + 1L)
  expect_equal(src$rows_read - src$rows - src$release_rows, 0L)
})

test_that("glatos reads the packaged GLATOS receiver file", {
  skip_if_not_installed("glatos", "0.9.8")
  rd <- glatos::read_glatos_receivers
  r <- rd(ext_file("glatos_receivers_example.csv"))
  expect_equal(nrow(r), 25L)
  expect_equal(nrow(read_deployments(ext_file("glatos_receivers_example.csv"), "glatos")), 25L)
})

test_that("messages quote a bad value as the file writes it", {
  f <- write_lines_file(c("Date and Time (UTC),Receiver,Transmitter", "Tuesday,VR2W-1,A69-1601-1"))
  expect_error(read_detections(f, "vue"), "such as \"Tuesday\" \\(row 1\\)")
  g <- write_lines_file(c("Date and Time (UTC),Receiver,Transmitter",
                          "2024-05-01T10:00:00Z,VR2W-1,A69-1601-1"))
  expect_equal(format(read_detections(g, "vue")$datetime), "2024-05-01 10:00:00")
})

test_that("a VUE export whose rows hold fewer fields than its header reads", {
  f <- write_lines_file(c(paste0("﻿Date and Time (UTC),Receiver,Transmitter,Transmitter Name,",
                                 "Sensor Value,Sensor Unit,Station Name"),
                          "2023-06-19 17:34:12,VR2Tx-123456,A69-1601-62990",
                          "2023-06-19 17:36:12,VR2Tx-123456,A69-1601-62990"))
  d <- read_detections(f)
  expect_equal(attr(d, "source")[[1]]$format, "vue")
  expect_equal(nrow(d), 2L)
})

test_that("format auto still sends an OTN partners extract to its own stop", {
  lines <- c("collectionCode,catalogNumber,detectedBy,count,yearCollected,detectionRegion,tagName",
             "P,F-1,EXT,12,2023,Gulf,A69-1601-1")
  f <- write_lines_file(lines)
  expect_error(read_detections(f), "matched to external partners")
})

test_that("to_actel() warns when it leaves out a detection with no station", {
  x <- example_detset()
  x$detections$station_id[1] <- NA
  w <- character()
  a <- withCallingHandlers(to_actel(x, "UTC"), warning = function(e) {
    w <<- c(w, conditionMessage(e))
    invokeRestart("muffleWarning")
  })
  expect_true(any(grepl("leaves out 1 of 40,506 detections: 1 with no station", w)))
  expect_equal(unname(attr(a, "dropped")), c(1L, 0L))
})

test_that("a workbook given as detections stops with a message that says so", {
  f <- tempfile(fileext = ".xlsx")
  writeLines("not a workbook", f)
  expect_error(read_detections(f), "is a workbook; detections are read from CSV or parquet")
})

# A per-tag export laid out as Thelma Biotel software writes one: the code
# space and frequency share `Protocol`, and the ID is in `Id`.
thelma_like <- function(id = c(7, 7), protocol = c("DS256-69kHz", "DS256-69kHz")) {
  f <- tempfile(fileext = ".csv")
  rows <- sprintf("2024-06-0%dT10:00:00.250Z,%d,4.5,[m],%s,1234", seq_along(id), id, protocol)
  writeLines(c("Date and Time (UTC),Id,Depth (Data 1),Data 1 unit,Protocol,Receiver", rows), f)
  f
}

test_that("a file read in the format auto guessed says so before the missing column", {
  f <- thelma_like()
  e <- expect_error(read_detections(f), class = "detqc_error")
  expect_match(conditionMessage(e), paste("was read as format = \"vue\", which \"auto\" guessed",
                                          "from its header: it has the \"vue\" columns for",
                                          "`datetime` and `receiver_serial` (date_and_time_utc,",
                                          "receiver) and none for `transmitter`. If it is not a",
                                          "VUE file, give `format` and a `col_map`. No column for",
                                          "`transmitter`"), fixed = TRUE)
  e <- expect_error(read_detections(f, "vue"), class = "detqc_error")
  expect_match(conditionMessage(e), "^No column for `transmitter`")
})

test_that("a full map reads a file no format fits, with or without format", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("date_time,receiver_id,tag_id,fish_id",
               "2024-06-03 09:06:06,1234,OPi-101,F1", "2024-06-03 09:16:06,1234,OPs-102,F2"), f)
  e <- expect_error(read_detections(f), class = "detqc_error")
  expect_match(conditionMessage(e), "no format's column map finds two of", fixed = TRUE)
  map <- data.frame(target = c("datetime", "receiver_serial", "transmitter", "animal_id"),
                    source = c("date_time", "receiver_id", "tag_id", "fish_id"),
                    type = c("datetime", "character", "character", "character"))
  d <- expect_silent(read_detections(f, col_map = map))
  expect_identical(d$transmitter, c("OPi-101", "OPs-102"))
  expect_identical(attr(d, "source")[[1]]$format, "col_map")
  cols <- c("datetime", "receiver_serial", "transmitter", "animal_id")
  expect_identical(d[cols], read_detections(f, "vue", col_map = map)[cols])
  # Pairs still need a format the file fits.
  expect_error(read_detections(f, col_map = c(transmitter = "tag_id")),
               "is not one read_detections() knows", fixed = TRUE, class = "detqc_error")
})

test_that("the route ?column_map gives for a shared code-space column reads, and keeps Protocol", {
  map <- column_map("detections", "vue")
  map <- map[map$target %in% c("datetime", "receiver_serial", "transmitter", "sensor_value",
                               "sensor_unit"), ]
  map$source[map$target == "transmitter"] <- "CodeSpace;Id"
  map$sep[map$target == "transmitter"] <- "-"
  map$source[map$target == "sensor_value"] <- "Depth (Data 1)"
  map$source[map$target == "sensor_unit"] <- "Data 1 unit"
  derived <- function(f) {
    y <- utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
    y$CodeSpace <- sub("-.*$", "", y$Protocol)
    out <- tempfile(fileext = ".csv")
    utils::write.csv(y, out, row.names = FALSE)
    out
  }
  d <- read_detections(derived(thelma_like()), col_map = map)
  expect_identical(d$transmitter, c("DS256-7", "DS256-7"))
  expect_identical(d$sensor_unit, c("m", "m"))
  # One ID on two frequencies reads as one transmitter; Protocol shows it.
  two <- read_detections(derived(thelma_like(protocol = c("DS256-69kHz", "DS256-71kHz"))),
                         col_map = map)
  expect_identical(unique(two$transmitter), "DS256-7")
  expect_identical(length(unique(two$protocol)), 2L)
})
