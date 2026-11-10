# Regression tests for detqc: each test pins one behaviour and says what it
# is.

ext <- function(f) ext_file(f)
utc <- function(s) as.POSIXct(s, tz = "UTC")

# ---- Receiver serial forms ----
test_that("VUE and OTN detections join GLATOS deployments on the serial", {
  rcv <- read_deployments(ext("glatos_receivers_example.csv"), "glatos")
  ani <- read_animals(ext("glatos_detections_example.csv"), "glatos")
  for (f in c(vue = "vue_example.csv", otn = "otn_detections_example.csv")) {
    fmt <- if (grepl("^vue", f)) "vue" else "otn"
    d <- read_detections(ext(f), fmt)
    expect_true(all(d$receiver_model == "VR2Tx"))
    x <- detset(d, rcv, ani)
    expect_match(x$log$reason, "station from a deployment for 833 of 833 detections")
    # Detections and listening days name receivers the same way.
    expect_true(all(x$detections$receiver_serial %in% x$listening$receiver_serial))
    mb <- detect_mortality(x)
    expect_equal(nrow(mb$bands), length(unique(stats::na.omit(x$detections$animal_id))))
    expect_equal(mb$settings$unlocated, 0L)
  }
})

test_that("a model-prefixed serial in a user table matches a bare deployment serial", {
  tt <- toy_tables()
  dep <- tt$dep
  dep$receiver_serial <- c("480001", "480001", "480002")
  det <- tt$det
  det$receiver_serial <- c(rep("VR2W-480001", 3), "VR2W-480002", "VR2W-480009")
  x <- detset(det, dep)
  expect_equal(x$detections$station_id, c("A", "B", "B", "C", NA))
  expect_equal(x$detections$receiver_serial, c(rep("480001", 3), "480002", "VR2W-480009"))
  expect_match(x$log$reason,
               "for 4 of 5 detections .*; 4 of them matched on the bare receiver serial")
  # Two receiver forms that share a serial name neither.
  dep2 <- dep
  dep2$receiver_serial <- c("VR2W-480001", "VR2W-480001", "HR2-480001")
  det2 <- tt$det
  det2$receiver_serial <- c(rep("480001", 4), "R9")
  expect_warning(y <- detset(det2, dep2), "Only 0 of 5 detections match a deployment")
  expect_true(all(is.na(y$detections$station_id)))
  # A collector number that is not model-serial is never cut down.
  expect_equal(split_receiver(c("HFX068-00348-1", "VR3-UWM-301", "VR2W-1"))$serial,
               c("HFX068-00348-1", "301", "VR2W-1"))
})

test_that("OTN hfx detections match the OTN AAT receiver file", {
  h <- read_detections(glatos_file("hfx_detections.csv"), "otn")
  aat <- read_deployments(glatos_file("otn_aat_receivers.csv"), "otn")
  x <- suppressWarnings(detset(h, aat))
  n <- as.integer(gsub(",", "", sub(".*for ([0-9,]+) of.*", "\\1", x$log$reason)))
  # The AAT file names receivers by serial, so most detections match one.
  expect_gt(n, 5000L)
  expect_true(all(x$detections$receiver_serial[!is.na(x$detections$station_id)] != ""))
})

# ---- NA keys ----
test_that("a detection with no receiver serial keeps its own station", {
  tt <- toy_tables()
  extra <- data.frame(receiver_serial = NA_character_, station_id = c("N1", "N2"),
                      deploy_datetime = utc(c("2024-01-01", "2024-02-01")),
                      recover_datetime = utc(NA), deploy_lat = c(46, 46.1), deploy_lon = -78)
  dep <- rbind(tt$dep, extra)
  det <- tt$det
  det$receiver_serial[5] <- NA
  det$station_id <- c(NA, NA, NA, NA, "N1")
  x <- detset(det, dep)
  # An NA key must not join any NA-serial deployment, such as N2. The
  # detection joins N1, the serial-less deployment of its own station, by
  # station and time.
  expect_equal(x$detections$station_id[5], "N1")
  expect_match(x$log$reason, "station from a deployment for 5 of 5")
  expect_match(x$log$reason, "1 on station_id to a deployment with no receiver serial")
})

# ---- Stationary across a site boundary ----
line_array <- function() {
  lat0 <- 45
  dlat <- function(m) m / 111195
  dlon <- function(m) m / (111195 * cos(lat0 * pi / 180))
  st <- data.frame(station_id = c(sprintf("D%02d", 0:10), "W1", "W2", "W3"),
                   lat = c(lat0 + dlat(300 * 0:10), lat0 - dlat(c(5000, 10000, 15000))),
                   lon = c(rep(-78, 11), rep(-78, 3) + dlon(c(3000, 6000, 9000))))
  data.frame(receiver_serial = paste0("R-", st$station_id), station_id = st$station_id,
             deploy_datetime = utc("2024-01-01"), recover_datetime = utc("2025-01-01"),
             deploy_lat = st$lat, deploy_lon = st$lon)
}

plant <- function(id, stations, start, days) {
  d <- utc(start) + rep((seq_len(days) - 1) * 86400, each = 4) + c(3600, 7200, 36000, 72000)
  rx <- paste0("R-", rep_len(rep(stations, each = 4), length(d)))
  data.frame(datetime = d, receiver_serial = rx, transmitter = paste0("T-", id), animal_id = id)
}

test_that("a dead tag between two close stations in different sites goes to review", {
  dep <- line_array()
  mb0 <- detect_mortality(detset(plant("x", "W1", "2024-01-10", 1), dep))
  site <- stats::setNames(mb0$sites$site, mb0$sites$station_id)
  d <- sprintf("D%02d", 0:10)
  split_at <- which(site[d][-1] != site[d][-11])[1]
  same_at <- which(site[d][-1] == site[d][-11])[1]
  pair_split <- d[c(split_at, split_at + 1)]
  pair_same <- d[c(same_at, same_at + 1)]
  det <- rbind(
    plant("SPLIT", c("W1", "W2", "W3"), "2024-01-10", 30),
    plant("SPLIT", pair_split, "2024-02-09", 30),
    plant("SAME", c("W1", "W2", "W3"), "2024-01-10", 30),
    plant("SAME", pair_same, "2024-02-09", 30)
  )
  mb <- detect_mortality(detset(det, dep))
  b <- mb$bands
  # A terminal run split across a site boundary is review. The alive band
  # gets no card by default.
  expect_equal(b$band[b$animal_id == "SPLIT"], "review")
  expect_equal(b$deciding_rule[b$animal_id == "SPLIT"], "terminal_run_split_site")
  expect_equal(format(as.Date(b$death_date[b$animal_id == "SPLIT"])), "2024-02-09")
  expect_equal(round(b$window_diag_km[b$animal_id == "SPLIT"], 2), 0.30)
  # SAME's run starts 29 days before its last detection, which is the data
  # horizon, so record_end sends it to review rather than dead.
  expect_equal(b$band[b$animal_id == "SAME"], "review")
  expect_equal(b$deciding_rule[b$animal_id == "SAME"], "record_end")
  expect_equal(b$terminal_vote[b$animal_id == "SAME"], "dead")
  v <- mb$votes[mb$votes$animal_id == "SPLIT" & mb$votes$rule == "terminal_run", ]
  expect_equal(v$vote, "review")
  expect_match(v$detail, "stationary across a site boundary")
  # The card is written by default, and its lane names the vote.
  out <- build_mortality_cards(detset(det, dep), mb, withr::local_tempdir())$cards
  expect_true("SPLIT" %in% out$animal_id)
})

test_that("in-water distances decide whether a split window is stationary", {
  x <- example_detset()
  apart <- function(lat, lon) {
    m <- matrix(5, length(lat), length(lat))
    diag(m) <- 0
    m
  }
  b <- detect_mortality(x, rules = mortality_rules(site_distance = apart))$bands
  # Apart, EX-02 has no death evidence; its record ends near the horizon.
  expect_equal(b$band[b$animal_id == "EX-02"], "review")
  expect_equal(b$deciding_rule[b$animal_id == "EX-02"], "record_end")
  expect_equal(b$terminal_vote[b$animal_id == "EX-02"], "none")
  near <- function(lat, lon) dist_km_matrix(lat, lon)
  b2 <- detect_mortality(x, rules = mortality_rules(site_distance = near))$bands
  expect_equal(b2$band[b2$animal_id == "EX-02"], "dead")
  expect_equal(geometric_run(c(45, 45.001, 45.002, 45.003), rep(-78, 4), 0.25, near), 2L)
  expect_equal(geometric_run(c(45, 45.001, 45.002, 45.003), rep(-78, 4), 0.25), 2L)
  expect_equal(geometric_run(rep(45, 3), rep(-78, 3), 0.25, near), 1L)
})

# ---- Animals with no located detection ----
test_that("an animal with no located detection keeps its band, row and card", {
  ex <- hard_cases()
  moved <- ex$detections$animal_id %in% c("EX-03", "EX-01")
  ex$detections$receiver_serial[moved] <- "VR2W-999999"
  x <- detset(ex$detections, ex$deployments, ex$animals)
  mb <- detect_mortality(x)
  b <- mb$bands
  expect_equal(nrow(b), 20L)
  # EX-03's accelerometer votes dead, and the animal stays in the bands.
  expect_equal(b$band[b$animal_id == "EX-03"], "dead")
  expect_equal(b$deciding_rule[b$animal_id == "EX-03"], "accel_flatline")
  expect_equal(b$band[b$animal_id == "EX-01"], "review")
  expect_equal(b$deciding_rule[b$animal_id == "EX-01"], "no_position")
  expect_equal(b$located[b$animal_id == "EX-01"], 0L)
  expect_equal(mb$settings$unlocated_animals, 2L)
  expect_output(print(mb), "2 animals have no located detection")
  # EX-01, dead at one station, has lost its positions, so it is review.
  expect_output(print(mb), "dead +10 of 20 animals")
  expect_output(print(mb), "review +10 of 20 animals .*no_position 1")
  out <- build_mortality_cards(x, mb, withr::local_tempdir(), all = TRUE)$cards
  expect_equal(nrow(out), 20L)
  expect_true(all(out$checked))
  idx <- read_payload(attr(out, "index"))
  expect_equal(length(idx$rows$id), 20L)
  expect_match(idx$lede, "2 of them have no located detection")
})

test_that("a set with no located detection at all still gets bands", {
  tt <- toy_tables()
  det <- tt$det
  det$receiver_serial <- "R-NONE"
  x <- suppressWarnings(detset(det, tt$dep))
  mb <- detect_mortality(x)
  expect_equal(mb$bands$animal_id, c("a1", "a2"))
  expect_equal(unique(mb$bands$deciding_rule), "no_position")
  expect_equal(unique(mb$bands$band), "review")
  expect_true(all(is.na(mb$bands$run_start)))
})

# ---- Accelerometer onset and spikes ----
test_that("the flatline onset is the day after the last active day", {
  dates <- as.Date("2024-01-01") + c(0:9, 40:99)
  flat <- c(rep(FALSE, 10), rep(TRUE, 60))
  first <- as.POSIXct(format(dates), tz = "UTC") + 3600
  r <- flatline_one(dates, flat, first, 14)
  # The onset is the day after the last active day; the first flat day
  # observed comes 30 days later.
  expect_equal(format(r$flat_onset), "2024-01-11")
  expect_equal(format(r$first_flat_observed), "2024-02-10 01:00:00")
  expect_equal(r$flat_days, 60L)
  spike <- flat
  spike[40] <- FALSE
  r1 <- flatline_one(dates, spike, first, 14, 1L)
  expect_equal(r1$last_active, dates[40])
  r2 <- flatline_one(dates, spike, first, 14, 2L)
  expect_equal(r2$last_active, dates[10])
  expect_equal(r2$spikes_ignored, 1L)
  expect_equal(format(r2$flat_onset), "2024-01-11")
  # A short active spell at the end of the record is the sensor's last word.
  tail_active <- flat
  tail_active[70] <- FALSE
  r3 <- flatline_one(dates, tail_active, first, 14, 2L)
  expect_equal(r3$last_active, dates[70])
  expect_false(r3$flatline)
  never <- flatline_one(dates, rep(TRUE, 70), first, 14)
  expect_equal(never$flat_onset, first[1])
  expect_output(print(mortality_rules(min_active_days = 2)), "min_active_days = 2")
  expect_error(mortality_rules(min_active_days = 0), "min_active_days")
  x <- example_detset()
  s <- detect_mortality(x, rules = mortality_rules(min_active_days = 2))$sensor
  expect_true(all(c("spikes_ignored", "first_flat_observed") %in% names(s)))
})

# ---- Deployment sheets ----
test_that("the real OTN deployment CSV forms a detset with its detections", {
  dep <- suppressWarnings(read_deployments(glatos_file("hfx_deployments.csv"), "otn"))
  h <- read_detections(glatos_file("hfx_detections.csv"), "otn")
  # Row 244 reads "caught: 2014-06-22" on its deploy day: a zero-length
  # deployment. detset() drops it with a warning and goes on.
  w <- character()
  x <- withCallingHandlers(detset(h, dep), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_match(w, "1 deployment ends at the moment it starts", all = FALSE)
  # The OTN deployment CSV records no receiver serial, so detset() joins
  # each detection to the serial-less deployment of its own station: 64,049
  # of 65,499.
  expect_match(x$log$reason, paste("station from a deployment for 64,049 of 65,499 detections",
                                   "\\(half-open intervals\\); 0 of them matched on the bare",
                                   "receiver serial and 64,049 on station_id"))
  expect_false(any(grepl("Only 0 of 65,499", w)))
  expect_s3_class(x, "detset")
  # hfx_detections.csv is a qualified extract: catalognumber names the tag,
  # not an animal, so no detection has an animal_id and no animal is banded.
  expect_true(all(is.na(h$animal_id)))
  expect_warning(mb <- detect_mortality(x), "No detection has an animal_id")
  expect_equal(nrow(mb$bands), 0L)
  expect_equal(nrow(x$detections), 65499L)
  expect_match(x$log$reason, "1 zero-length deployment dropped")
})

test_that("deployment rows with no deployment time are counted and logged", {
  raw <- utils::read.csv(ext("glatos_receivers_example.csv"), colClasses = "character")
  raw$deploy_date_time[c(1, 5, 9)] <- ""
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(raw, f, row.names = FALSE)
  expect_warning(dep <- read_deployments(f, "glatos"), paste(
    "3 deployment rows have no deployment time and were dropped",
    "\\(stations G01, G05, G09\\)"
  ))
  expect_equal(nrow(dep), 22L)
  expect_equal(attr(dep, "source")$dropped_no_deploy_time, 3L)
  det <- read_detections(ext("glatos_detections_example.csv"), "glatos")
  x <- suppressWarnings(detset(det, dep))
  expect_match(x$log$reason,
               "deployment rows with no deployment time dropped by the read function: 3")
})

# ---- The browser store key ----
test_that("two reviews in folders of the same name do not share a store", {
  root <- withr::local_tempdir()
  a <- card_control(list(), file.path(root, "lakeA", "review"))$store
  b <- card_control(list(), file.path(root, "lakeB", "review"))$store
  expect_false(identical(a, b))
  expect_match(a, "^detqc_calls_review_[0-9a-f]{10}$")
  expect_identical(a, card_control(list(), file.path(root, "lakeA", "review"))$store)
  x <- example_detset()
  mb <- detect_mortality(x)
  dir <- file.path(root, "lakeA", "review")
  review_mortality(x, dir, bands = mb, open = FALSE, control = list(store = "lake_a"))
  expect_equal(read_payload(file.path(dir, "index.html"))$store, "detqc_calls_lake_a")
})

# ---- Death dates that cannot be right ----
test_that("apply_mortality_calls() names early and undated death dates", {
  x <- example_detset()
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(animal_id = c("EX-01", "EX-02", "EX-09"), call = "dead_or_shed",
                              death_date = c("2023-05-01", "", "2023-09-30"),
                              candidate_date = c("2023-07-15", "2023-08-10", "2023-05-20")),
                   f, row.names = FALSE)
  w <- character()
  y <- withCallingHandlers(apply_mortality_calls(x, f), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_match(w, "1 death date falls before the animal's first detection: EX-01", all = FALSE)
  expect_match(w, "1 dead_or_shed call has no death date and will remove nothing: EX-02",
               all = FALSE)
  expect_match(detset_log(y)$reason[2],
               "2 dated calls more than 30 days from the candidate date \\(of 2 with both\\)")
  expect_false("candidate_date" %in% names(y$mortality))
})

# ---- A card checked against planted truth ----
test_that("a card's numbers equal a planted record that never passed through detset", {
  dep <- line_array()
  truth <- data.frame(day = as.Date(c("2024-03-01", "2024-03-01", "2024-03-03", "2024-03-07")),
                      station = c("W1", "W2", "W2", "D04"))
  det <- data.frame(datetime = as.POSIXct(format(truth$day), tz = "UTC") + 3600 * seq_len(4),
                    receiver_serial = paste0("R-", truth$station), transmitter = "T-P",
                    animal_id = "PLANT")
  x <- detset(det, dep)
  mb <- detect_mortality(x, mortality_rules(min_days = 2, min_flat_days = 2))
  out <- build_mortality_cards(x, mb, withr::local_tempdir(), all = TRUE)$cards
  p <- read_payload(out$file[out$animal_id == "PLANT"])
  days <- as.Date(p$t0) + which(p$series$dets > 0) - 1
  expect_equal(days, unique(truth$day))
  expect_equal(p$series$dets[p$series$dets > 0], as.vector(table(truth$day)))
  ds <- matrix(p$ds, ncol = 3, byrow = TRUE)
  expect_setequal(p$stations$id[ds[, 2] + 1], unique(truth$station))
  expect_equal(sum(ds[, 3]), nrow(truth))
})

test_that("checks, exports, the report and the readers keep their guards on empty inputs", {
  ex <- hard_cases()
  x <- detset(ex$detections, ex$deployments, ex$animals)
  # long_after_release needs release times; reappeared_elsewhere needs detections.
  ani <- ex$animals
  ani$release_datetime <- NULL
  tabs <- list(detections = ex$detections, deployments = ex$deployments, animals = ani)
  s <- check_detset(tabs, checks = "long_after_release",
                    settings = check_settings(max_liberty_years = 1))$summary
  expect_equal(s$note, "the animals table has no release times")
  tabs$detections <- tabs$detections[0, ]
  # The context of an empty set warns on its time ranges, before any check runs.
  s <- suppressWarnings(check_detset(tabs, checks = "reappeared_elsewhere"))$summary
  expect_equal(s$status, "not run")
  # An export of no deployment counts none; the bands of no named animal cap none.
  b <- bounded_deployments(ex$deployments[0, ], ex$detections, "to_actel")
  expect_equal(sum(attr(b, "counts")), 0)
  expect_equal(uncalibrated_animals(data.table::data.table(animal_id = NA_character_,
                                                           transmitter = "T", scale = "raw")),
               character())
  # The report's control, and its parts with nothing to say.
  f <- withr::local_tempfile(fileext = ".html")
  expect_error(processing_report(x, f, control = 1), "`control` must be a list")
  expect_equal(cut_audit_note(NULL), "")
  x$meta$sources <- NULL
  expect_equal(inputs_cut(x), "")
  expect_error(as_detfield_input(x, stations = 1), "`stations` must be station_id values")
  # Specifications that list none of the sheet's transmitters fill nothing.
  specs <- data.frame(transmitter = "A69-0000-1", tag_serial = "1", min_delay = 60,
                      stringsAsFactors = FALSE)
  out <- fill_spec_columns(data.table::as.data.table(ex$animals), specs)
  expect_equal(out$filled, 0L)
})

# ---- Empty inputs ----
test_that("year_shifts() gives no shift and no base-R warning when no window holds an orphan", {
  d <- data.table::data.table(.dep = 1L, .key = "R1",
                              deploy_datetime = utc("2024-01-01"),
                              recover_datetime = utc("2024-02-01"))
  far <- data.table::data.table(.key = "R1", datetime = utc("2030-01-01"))
  expect_no_warning(r <- year_shifts(d, far))
  expect_equal(nrow(r$rows), 0L)
  expect_null(r$windows)
})
