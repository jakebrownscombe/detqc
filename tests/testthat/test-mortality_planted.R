# detect_mortality() on planted hard cases, each built in a fixed synthetic
# array.

# ---- A stray burst in a dead tag's terminal window ----
# Dead at G1 from 1 June; G1 is recovered on 31 October, 61 days before the
# horizon, so record_end cannot fire.
stray_set <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    dep <- plant_deployments(list(G1 = "2024-10-31 12:00"))
    base <- function(tx) {
      rbind(plant_walk("2024-05-02", "2024-05-31", tx, sprintf("G%d", 2:9)),
            plant_stay("2024-06-01", "2024-10-31", "G1", tx))
    }
    stray <- function(tx, st) {
      data.frame(date = as.Date("2024-10-25"), station_id = st, transmitter = tx)
    }
    dets <- list(
      plant_detections(base("S0"), dep, "DEAD_NOSTRAY"),
      plant_detections(rbind(base("S1"), stray("S1", "G9")), dep, "DEAD_STRAY_5.7KM"),
      plant_detections(rbind(base("S2"), stray("S2", "G5")), dep, "DEAD_STRAY_2.8KM"),
      rbind(plant_detections(base("S3"), dep, "DEAD_ONE_DETECTION_5.7KM"),
            plant_detections(stray("S3", "G9"), dep, "DEAD_ONE_DETECTION_5.7KM", n = 1L)),
      # 40 days, under persistence's 45-day minimum, so only space and
      # listening judge it.
      plant_detections(plant_walk("2024-05-02", "2024-06-10", "W1", sprintf("G%d", 2:9)), dep,
                       "WALKER_LEFT"),
      plant_other(dep)
    )
    ids <- c("DEAD_NOSTRAY", "DEAD_STRAY_5.7KM", "DEAD_STRAY_2.8KM", "DEAD_ONE_DETECTION_5.7KM",
             "WALKER_LEFT", "OTHER_ALIVE")
    ani <- plant_animals(ids, c("S0", "S1", "S2", "S3", "W1", "OT"))
    cache <<- plant_detset(dets, dep, ani)
    cache
  }
})

test_that("a dead tag with no stray is dead, and one with a stray burst is not alive", {
  mb <- detect_mortality(stray_set())
  expect_equal(plant_band(mb, "DEAD_NOSTRAY")$band, "dead")
  for (id in c("DEAD_STRAY_5.7KM", "DEAD_STRAY_2.8KM", "DEAD_ONE_DETECTION_5.7KM")) {
    r <- plant_band(mb, id)
    expect_equal(r$band, "review", label = id)
    expect_equal(r$rule, "terminal_run_stray", label = id)
    expect_match(r$detail[["terminal_run"]],
                 "apart from 1 day heard at other sites \\(2024-10-25\\)")
    # The run starts on its first day at G1, before the stray.
    expect_equal(as.Date(r$b$death_date), as.Date("2024-06-01"), label = id)
  }
  # An animal that left while the array listened stays alive.
  expect_equal(plant_band(mb, "WALKER_LEFT")$band, "alive")
  expect_equal(plant_band(mb, "WALKER_LEFT")$votes[["site_listening"]], "none")
})

test_that("each guard alone keeps the stray tags out of the alive band; neither leaves the hole", {
  x <- stray_set()
  ids <- c("DEAD_STRAY_5.7KM", "DEAD_STRAY_2.8KM", "DEAD_ONE_DETECTION_5.7KM")
  band_of <- function(mb) vapply(ids, function(id) plant_band(mb, id)$band, "")
  # At the default listening_share = 0 the listening stream is evidence only.
  expect_equal(unname(band_of(detect_mortality(x, mortality_rules(stray_days = 0)))),
               rep("alive", 3))
  only_listening <- detect_mortality(x, mortality_rules(stray_days = 0, listening_share = 0.5))
  expect_equal(unname(band_of(only_listening)), rep("review", 3))
  expect_equal(plant_band(only_listening, "DEAD_STRAY_5.7KM")$rule, "site_listening")
  expect_match(plant_band(only_listening, "DEAD_STRAY_5.7KM")$detail[["site_listening"]],
               "listened on 0 of the 30 days")
  only_stray <- detect_mortality(x, mortality_rules(drop = "site_listening"))
  expect_equal(unname(band_of(only_stray)), rep("review", 3))
  neither <- detect_mortality(x, mortality_rules(stray_days = 0, drop = "site_listening"))
  expect_equal(unname(band_of(neither)), rep("alive", 3))
})

test_that("the stray tags stay out of the alive band after filter_min_lag()", {
  y <- suppressWarnings(filter_min_lag(stray_set(), max_lag_s = 3600))
  mb <- detect_mortality(y)
  for (id in c("DEAD_STRAY_5.7KM", "DEAD_STRAY_2.8KM", "DEAD_ONE_DETECTION_5.7KM")) {
    expect_false(plant_band(mb, id)$band == "alive", label = id)
  }
})

# ---- A dead tag heard by two stations 2.5 km apart ----
test_that("a dead tag between two far stations that are then recovered needs the listening vote", {
  dep <- plant_deployments(list(F1 = "2024-10-31 12:00", F2 = "2024-10-31 12:00"))
  g <- sprintf("G%d", 1:9)
  late <- plant_detections(rbind(plant_walk("2024-05-02", "2024-08-31", "E2", g),
                                 plant_stay("2024-09-01", "2024-10-31", c("F1", "F2"), "E2")),
                           dep, "FARPAIR_DEAD_LATE")
  x <- plant_detset(list(late, plant_other(dep)), dep,
                    plant_animals(c("FARPAIR_DEAD_LATE", "OTHER_ALIVE"), c("E2", "OT")))
  # At the defaults this dead tag is alive: its window spans 2.5 km every
  # day and its record ends when F1 and F2 come out. The card shows that
  # the site stopped listening; the vote is opt-in.
  r0 <- plant_band(detect_mortality(x), "FARPAIR_DEAD_LATE")
  expect_equal(r0$band, "alive")
  expect_equal(r0$votes[["site_listening"]], "none")
  expect_match(r0$detail[["site_listening"]], "listened on 0 of the 30 days")
  r <- plant_band(detect_mortality(x, mortality_rules(listening_share = 0.5)),
                  "FARPAIR_DEAD_LATE")
  expect_equal(r$band, "review")
  expect_equal(r$rule, "site_listening")
  expect_equal(r$votes[["terminal_run"]], "none")
})

test_that("the listening stream reads the audit's not-listening days and needs a listening table", {
  x <- stray_set()
  mb <- detect_mortality(x)
  expect_equal(plant_band(mb, "WALKER_LEFT")$votes[["site_listening"]], "none")
  # Days flagged not listening count as silence.
  y <- x
  y$listening$listening <- !(y$listening$date > as.Date("2024-06-10") &
                               y$listening$date <= as.Date("2024-07-25"))
  on <- mortality_rules(listening_share = 0.5)
  expect_equal(plant_band(detect_mortality(y), "WALKER_LEFT")$votes[["site_listening"]], "none")
  expect_equal(plant_band(detect_mortality(y, on), "WALKER_LEFT")$votes[["site_listening"]],
               "review")
  # With no listening table the stream cannot vote.
  z <- x
  z$listening <- z$listening[0, ]
  v <- plant_band(detect_mortality(z, on), "WALKER_LEFT")
  expect_equal(v$votes[["site_listening"]], "unavailable")
  expect_equal(v$band, "alive")
  expect_error(mortality_rules(listening_share = 2), "`listening_share` must be at most 1")
})

# ---- Two transmitters, one shed and one still moving ----
test_that("an animal whose second tag keeps moving is not in the dead band", {
  dep <- plant_deployments()
  act <- function(d, flat_from) {
    d$sensor_raw <- ifelse(as.Date(d$datetime) < as.Date(flat_from), 150, 1)
    d
  }
  a <- act(plant_detections(rbind(plant_walk("2024-05-02", "2024-06-30", "T7A",
                                             sprintf("G%d", 2:9)),
                                  plant_stay("2024-07-01", "2024-12-31", "G1", "T7A")),
                            dep, "TWO_ID"), "2024-07-01")
  b <- plant_detections(plant_walk("2024-05-02", "2024-12-31", "T7B", sprintf("G%d", 2:9)),
                        dep, "TWO_ID")
  ani <- rbind(plant_animals("TWO_ID", "T7A", accel = TRUE), plant_animals("TWO_ID", "T7B"))
  r <- plant_band(detect_mortality(plant_detset(list(a, b), dep, ani)), "TWO_ID")
  expect_equal(r$votes[["accel_flatline"]], "dead")
  expect_equal(r$votes[["movement_resumption"]], "alive")
  expect_match(r$detail[["movement_resumption"]], "^T7B: ")
  expect_equal(r$band, "review")
  expect_equal(r$rule, "movement_resumption")
})

# ---- A death date after a listening gap ----
test_that("a run that starts after a listening gap gives the earliest bound", {
  extra <- data.frame(receiver_serial = "VR2W-777777", station_id = "G4",
                      deploy_datetime = as.POSIXct("2024-09-10", tz = "UTC"),
                      recover_datetime = as.POSIXct("2024-11-01", tz = "UTC"))
  dep <- plant_deployments(list(G4 = "2024-08-01"), extra)
  g <- plant_detections(rbind(plant_walk("2024-05-02", "2024-07-31", "GP",
                                         setdiff(sprintf("G%d", 1:9), "G4")),
                              plant_stay("2024-09-10", "2024-10-31", "G4", "GP")),
                        dep, "GAP_THEN_DEAD")
  x <- plant_detset(list(g, plant_other(dep)), dep,
                    plant_animals(c("GAP_THEN_DEAD", "OTHER_ALIVE"), c("GP", "OT")))
  mb <- detect_mortality(x)
  r <- plant_band(mb, "GAP_THEN_DEAD")
  expect_equal(r$band, "dead")
  # The card's vote cell joins basis and detail: the gap clause appears once.
  v <- mb$votes[mb$votes$animal_id == "GAP_THEN_DEAD" & mb$votes$rule == "terminal_run", ]
  cell <- paste(unique(c(v$basis, v$detail)), collapse = "; ")
  expect_equal(lengths(regmatches(cell, gregexpr("when its site was not listening", cell))), 1L)
  expect_equal(as.Date(r$b$death_date), as.Date("2024-09-10"))
  basis <- paste("after 40 days when its site was not listening; the death may be as early as",
                 "the detection before the run, 2024-07-31 \\(terminal_run\\)")
  expect_match(r$b$death_date_basis, basis)
  # The gap note is in the basis only: the card joins basis and detail in one
  # cell, so it appears once.
  expect_false(grepl("as early as", r$detail[["terminal_run"]]))
  # A run with no gap keeps the plain basis.
  r2 <- plant_band(detect_mortality(stray_set()), "DEAD_NOSTRAY")
  expect_equal(r2$b$death_date_basis, "the first detection of the final run (terminal_run)")
})

# ---- An uncalibrated accelerometer ----
test_that("raw-only accelerometers raise a warning that counts them, and the log says so", {
  dep <- plant_deployments()
  d <- plant_detections(plant_stay("2024-05-02", "2024-10-31", "G7", "C2"), dep,
                        "SLOPE_NA_RESIDENT")
  d$sensor_raw <- 150
  ani <- plant_animals(c("SLOPE_NA_RESIDENT", "OTHER_ALIVE"), c("C2", "OT"),
                       accel = c(TRUE, FALSE))
  ani$sensor_slope <- NA_real_
  ani$sensor_intercept <- NA_real_
  x <- plant_detset(list(d, plant_other(dep)), dep, ani)
  # The warning is given once per session for one set of transmitters.
  expect_warning_fresh(mb <- detect_mortality(x),
                       "1 of 1 accelerometer transmitters have raw readings only")
  expect_equal(unname(mb$settings$raw_accelerometers[["raw"]]), 1L)
  expect_match(mb$settings$reason, "1 of 1 accelerometer raw, with no sensor vote")
  expect_equal(plant_band(mb, "SLOPE_NA_RESIDENT")$votes[["accel_flatline"]], "unavailable")
  # Calibrated, the same animal votes alive and leaves the dead band.
  ani$sensor_slope[1] <- plant_slope
  ani$sensor_intercept[1] <- 0
  mb2 <- expect_silent(detect_mortality(plant_detset(list(d, plant_other(dep)), dep, ani)))
  expect_equal(plant_band(mb2, "SLOPE_NA_RESIDENT")$votes[["accel_flatline"]], "alive")
  expect_false(plant_band(mb2, "SLOPE_NA_RESIDENT")$band == "dead")
})
