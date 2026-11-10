# detect_mortality() on the synthetic hard cases of hard_mortality(): one
# test per case, each naming the band the rule streams must give it.

# ---- The hard cases, one test each ----
test_that("a dead animal with an accelerometer flatline is in the dead band", {
  r <- expect_case("accel_flatline_dead", "dead", "accel_flatline")
  expect_equal(death_gap(r), 0L)
  expect_equal(r$b$death_date_basis, "the day after the last active day (accel_flatline)")
  expect_equal(r$votes[["terminal_run"]], "dead")
})

test_that("a resident alive animal within two neighbouring stations is in review", {
  r <- expect_case("resident_alive", "review", "terminal_run_near_stationary")
  expect_equal(r$votes[["accel_flatline"]], "alive")
  expect_match(r$detail[["terminal_run"]], "nearly stationary")
})

test_that("a dormant winter animal whose accelerometer recovers is alive", {
  r <- expect_case("dormant_winter_alive", "alive", "none")
  expect_equal(r$votes[["accel_flatline"]], "alive")
  expect_true(is.na(r$b$death_date))
})

test_that("a wide-ranging animal heard 3 days at one station and then never again is alive", {
  r <- expect_case("wide_ranger_alive", "alive", "none")
  expect_equal(r$votes[["terminal_run"]], "none")
  expect_match(r$detail[["record_end"]], "79 days before the data horizon")
})

test_that("a record that ends at tag life goes to review", {
  r <- expect_case("record_ends_at_tag_life", "review", "record_end")
  expect_match(r$detail[["record_end"]], "3 days before the tag life \\(2024-09-28\\)")
  expect_equal(r$b$end_basis, "tag life")
})

test_that("a record that ends at the data horizon goes to review", {
  r <- expect_case("record_ends_at_horizon", "review", "record_end")
  expect_equal(r$b$end_basis, "data horizon")
})

test_that("a tag shed at release is dead from its first detection", {
  r <- expect_case("shed_at_release", "dead", "terminal_run")
  expect_equal(death_gap(r), 0L)
  expect_equal(r$b$run_start, r$b$first_detection)
})

test_that("a tag shed mid-record between stations 400 m apart is dead on the right day", {
  r <- expect_case("shed_mid_record", "dead", "terminal_run")
  expect_equal(death_gap(r), 0L)
  expect_equal(round(r$b$window_diag_km, 2), 0.40)
})

test_that("a dead tag at a mooring dragged 1.5 km goes to review, dated from its first day there", {
  r <- expect_case("dragged_mooring_dead", "review", "terminal_run_moved_station")
  expect_equal(death_gap(r), 0L)
  expect_match(r$detail[["terminal_run"]], "one station whose recorded position moved 1.508 km")
})

test_that("an animal heard by one receiver only is in the dead band", {
  r <- expect_case("one_receiver_only", "dead", "terminal_run")
  expect_equal(death_gap(r), 0L)
  expect_equal(r$b$window_diag_km, 0)
})

test_that("a two-code tag is one animal, judged on both codes, and alive", {
  r <- expect_case("dual_code_alive", "alive", "none")
  x <- mortality_example()
  tx <- unique(x$detections$transmitter[x$detections$animal_id %in% r$id])
  expect_length(tx, 2L)
  expect_equal(sum(example_bands()$bands$animal_id == r$id), 1L)
  expect_equal(r$votes[["accel_flatline"]], "alive")
})

test_that("an animal with a depth sensor only is dead by its run, with depth as evidence", {
  r <- expect_case("depth_sensor_only_dead", "dead", "terminal_run")
  expect_equal(r$votes[["depth_flatline"]], "dead")
  expect_equal(r$votes[["accel_flatline"]], "unavailable")
  expect_equal(death_gap(r), 0L)
  # The dead tag reads 6.1 m with one count of noise either side, a range of
  # 0.3008 m; judged at the sensor's resolution, depth is flat from the day
  # of death, not 97 days later.
  mb <- example_bands()
  depth_date <- mb$votes$date[mb$votes$animal_id == r$id & mb$votes$rule == "depth_flatline"]
  expect_equal(as.Date(depth_date), r$truth$true_death_date)
})

test_that("an accelerometer flatline followed by movement goes to review", {
  r <- expect_case("accel_flat_then_moved", "review", "movement_resumption")
  expect_equal(r$votes[["accel_flatline"]], "dead")
  expect_equal(r$votes[["movement_resumption"]], "alive")
  expect_match(r$detail[["movement_resumption"]], "^57 days heard only more than 1 km")
})

test_that("a dead tag heard by stations 1.6 km apart goes to review", {
  r <- expect_case("near_stationary_dead", "review", "terminal_run_near_stationary")
  expect_equal(death_gap(r), 0L)
})

test_that("a sparse record goes to review", {
  expect_case("sparse_record", "review", "sparse_record")
})

test_that("a dead tag heard every day by stations 2.5 km apart goes to review by persistence", {
  r <- expect_case("persistent_far_pair_dead", "review", "persistence")
  expect_equal(r$votes[["terminal_run"]], "none")
  expect_gte(r$b$persistence, 0.95)
  # Ranking only, the same dead tag reaches the alive band.
  b <- quiet_raw(detect_mortality(mortality_example(), mortality_rules(review_at = Inf)))$bands
  expect_equal(b$band[b$animal_id == r$id], "alive")
  dead_ids <- hard_mortality()$cases$animal_id[!is.na(hard_mortality()$cases$true_death_date)]
  expect_equal(sum(b$band[b$animal_id %in% dead_ids] == "alive"), 1L)
})

test_that("a moored reference tag reaches the dead band", {
  r <- expect_case("moored_reference_tag", "dead", "terminal_run")
  expect_equal(death_gap(r), 0L)
})

test_that("an uncalibrated accelerometer cannot vote, and the moving animal is alive", {
  r <- expect_case("accel_uncalibrated_alive", "alive", "none")
  s <- example_bands()$sensor
  expect_equal(s$reason[s$animal_id == r$id],
               "accelerometer readings are raw: give sensor_slope and sensor_intercept")
  # detect_mortality() says so with a warning that counts them. The
  # warning is given once per session for one set of transmitters.
  expect_warning_fresh(detect_mortality(mortality_example()),
                       "1 of 6 accelerometer transmitters have raw readings only")
})

test_that("death evidence that begins near the end of tag life goes to review", {
  r <- expect_case("death_near_tag_life", "review", "record_end")
  expect_equal(r$votes[["terminal_run"]], "dead")
  expect_equal(death_gap(r), 0L)
})

test_that("a dead tag across a site boundary goes to review", {
  r <- expect_case("dead_across_site_boundary", "review", "terminal_run_split_site")
  expect_equal(death_gap(r), 0L)
})

test_that("every case of the fixture is in its expected band and rule", {
  mb <- example_bands()
  cmp <- merge(hard_mortality()$cases, mb$bands, by = "animal_id")
  expect_equal(nrow(cmp), 20L)
  expect_equal(cmp$band, cmp$expected_band)
  expect_equal(cmp$deciding_rule, cmp$expected_rule)
  # The null subjects: the resident, the dormant animal and the wide ranger
  # never reach the dead band; the moored tag does.
  null <- c("resident_alive", "dormant_winter_alive", "wide_ranger_alive")
  expect_false(any(cmp$band[cmp$case %in% null] == "dead"))
  expect_equal(cmp$band[cmp$case == "moored_reference_tag"], "dead")
})

# ---- The bands survive min-lag ----
test_that("the dead cases keep their band after filter_min_lag() at 3,600 s", {
  x <- mortality_example()
  y <- filter_min_lag(x, max_lag_s = 3600)
  expect_lt(nrow(y$detections), nrow(x$detections))
  expect_gt(nrow(y$detections) / nrow(x$detections), 0.99)
  b <- merge(hard_mortality()$cases, detect_mortality(y)$bands, by = "animal_id")
  expect_equal(b$band, b$expected_band)
  expect_equal(sum(b$band == "dead"), 6L)
})

test_that("the positionRtools example loses its dead band under min-lag", {
  f <- readRDS(fixture("positionrtools_example.rds"))
  x <- detset(f$detections, f$deployments, f$animals)
  mb <- detect_mortality(x)
  expect_setequal(mb$bands$animal_id[mb$bands$band == "dead"], f$planted_dead)
  planted <- c("2023-08-01", "2023-08-20", "2023-09-01", "2023-09-20", "2023-10-05")
  expect_setequal(format(as.Date(mb$bands$death_date[mb$bands$band == "dead"])), planted)
  # Its median gap between detections on one receiver is 18.8 h, so min-lag
  # removes most rows and no planted animal keeps a terminal run.
  y <- filter_min_lag(x, max_lag_s = 3600)
  b <- detect_mortality(y)$bands
  expect_equal(sum(b$band == "dead"), 0L)
  expect_true(all(b$band[b$animal_id %in% f$planted_dead] == "review"))
})

# ---- Options and arguments ----
test_that("detect_mortality() checks its arguments and options", {
  x <- mortality_example()
  expect_error(detect_mortality(x, rules = list()), "list of streams")
  expect_error(detect_mortality(list()), "must be a detset")
  expect_error(detect_mortality(x, control = 1), "`control` must be a list")
  expect_error(detect_mortality(x, control = list(sites = 1)), "no option `sites`")
  expect_error(detect_mortality(x, control = list(sites_km = 0)), "sites_km")
  expect_error(detect_mortality(x, control = list(site_distance = 3)), "site_distance")
  expect_error(detect_mortality(x, control = list(horizon = "2024-12-31")), "horizon")
  expect_error(detect_mortality(x, control = list(sparse_to_review = NA)), "sparse_to_review")
  expect_error(detect_mortality(x, foo = 1), "unused argument")
  expect_error(detect_mortality(x, mortality_rules(), list(), 2), "unused argument")
})

test_that("a horizon past the detset does not end a record heard to its end", {
  x <- mortality_example()
  later <- as.POSIXct("2025-06-30", tz = "UTC")
  mb <- detect_mortality(x, control = list(horizon = later))
  b <- mb$bands
  # The record that stops at the end of the detections is cut there, not
  # ended: a horizon half a year later cannot clear it alive, because the
  # detset holds nothing after its last detection.
  id <- case_row("record_ends_at_horizon")$id
  expect_equal(b$record_end_vote[b$animal_id == id], "review")
  expect_equal(b$band[b$animal_id == id], "review")
  expect_equal(b$deciding_rule[b$animal_id == id], "record_end")
  expect_equal(b$end_basis[b$animal_id == id], "end of the detection record")
  expect_equal(format(mb$settings$horizon), "2025-06-30")
  v <- mb$votes[mb$votes$animal_id == id & mb$votes$rule == "record_end", ]
  expect_match(v$detail, "end of the detection record")
  expect_match(v$detail, "the data run to 2025-06-30, so the record is cut there and the rules")
  # Every band is as at the default horizon: the later horizon moves no
  # animal, since the detset ends at the same detection.
  b0 <- example_bands()$bands
  expect_equal(b$band[match(b0$animal_id, b$animal_id)], b0$band)
  # At the default, the card says the horizon is the detset's last detection.
  v0 <- example_bands()$votes
  v0 <- v0[v0$animal_id == id & v0$rule == "record_end", ]
  expect_match(v0$detail, "the data horizon is the last detection in the detset")
  # A horizon before the end of the detections still sets the end.
  early <- max(x$detections$datetime) - 40 * 86400
  e <- quiet_raw(detect_mortality(x, control = list(horizon = early)))$bands
  expect_true(all(e$end_basis %in% c("data horizon", "tag life")))
})

test_that("seen_to() stops at the end of the detection record on a later date", {
  t0 <- as.POSIXct("2025-07-31 23:54:03", tz = "UTC")
  expect_equal(seen_to(as.POSIXct("2026-06-01", tz = "UTC"), t0), t0)
  # A horizon on the same UTC day is the horizon given.
  same <- as.POSIXct("2025-07-31 23:59:53", tz = "UTC")
  expect_equal(seen_to(same, t0), same)
  expect_equal(seen_to(t0 - 86400, t0), t0 - 86400)
})

test_that("in-water distances from control decide sites and windows", {
  x <- mortality_example()
  apart <- function(lat, lon) {
    m <- matrix(5, length(lat), length(lat))
    diag(m) <- 0
    m
  }
  b <- detect_mortality(x, control = list(site_distance = apart))$bands
  # Every station is its own site, and every pair is 5 km apart, so the tag
  # shed between two stations 400 m apart has no terminal run; its record
  # ends at the horizon, which leaves it in review.
  id <- case_row("shed_mid_record")$id
  expect_equal(b$band[b$animal_id == id], "review")
  expect_equal(b$deciding_rule[b$animal_id == id], "record_end")
  expect_equal(b$terminal_vote[b$animal_id == id], "none")
  near <- function(lat, lon) dist_km_matrix(lat, lon)
  b2 <- detect_mortality(x, control = list(site_distance = near))$bands
  expect_equal(b2$band[b2$animal_id == id], "dead")
  expect_error(detect_mortality(x, control = list(site_distance = function(a, b) 1)),
               "must return a 41 by 41 matrix; it returned a 1 by 1")
})

test_that("options given to mortality_rules() travel with the rules", {
  x <- mortality_example()
  r <- mortality_rules(sparse_to_review = FALSE)
  b <- detect_mortality(x, r)$bands
  id <- case_row("sparse_record")$id
  expect_equal(b$band[b$animal_id == id], "alive")
  b2 <- detect_mortality(x, r, control = list(sparse_to_review = TRUE))$bands
  expect_equal(b2$band[b2$animal_id == id], "review")
  expect_error(mortality_rules(sites_km = -1), "sites_km")
})

# ---- Print, empty and unlocated sets ----
test_that("print() states the bands with their denominator", {
  mb <- example_bands()
  expect_output(print(mb), "dead +6 of 20 animals")
  expect_output(print(mb), "review +10 of 20 animals")
  expect_output(print(mb), "5 accelerometers voted \\(2 dead, 3 alive\\)")
  expect_output(print(mb), "triage")
})

test_that("detections with no position are counted, and their animals kept", {
  tt <- toy_tables()
  x <- detset(tt$det, tt$dep)
  mb <- detect_mortality(x, mortality_rules(min_days = 1))
  expect_equal(mb$settings$unlocated, 0L)
  y <- x
  y$detections$receiver_serial[4] <- "R9"
  mb2 <- detect_mortality(y, mortality_rules(min_days = 1))
  expect_equal(mb2$settings$unlocated, 1L)
  expect_output(print(mb2), "1 detection had no station position and was not scored")
  expect_output(print(mb2), "1 animal has no located detection; it is in the bands")
  expect_equal(mb2$bands$deciding_rule[mb2$bands$animal_id == "a2"], "no_position")
})

test_that("a detset with no animal detections gives empty bands", {
  tt <- toy_tables()
  det <- tt$det
  det$animal_id <- NA_character_
  # A set with no animal at all is named in a warning.
  expect_warning(mb <- detect_mortality(detset(det, tt$dep)),
                 "No detection has an animal_id, so there is no animal to band: 5 detections")
  expect_equal(nrow(mb$bands), 0L)
  expect_equal(nrow(mb$votes), 0L)
  expect_output(print(mb), "0 animals")
})

test_that("a one-station set with no sensors still gets a band", {
  tt <- toy_tables()
  mb <- detect_mortality(detset(tt$det[1:2, ], tt$dep[1, ]))
  expect_equal(nrow(mb$sensor), 0L)
  expect_equal(mb$bands$band, "review")
  expect_equal(mb$bands$deciding_rule, "sparse_record")
})

# ---- The step is logged ----
test_that("the bands' log record joins the detset once, with its counts", {
  x <- mortality_example()
  mb <- example_bands()
  y <- log_mortality_bands(x, mb)
  lg <- detset_log(y)
  expect_equal(nrow(lg), nrow(x$log) + 1L)
  rec <- lg[nrow(lg), ]
  expect_equal(rec$step, "detect_mortality")
  expect_equal(rec$removed, 0L)
  expect_equal(rec$rows_before, nrow(x$detections))
  expect_match(rec$reason, "20 animals sorted by 7 rule streams: dead 6, review 10, alive 4")
  # Each stream's votes and each setting are in the record.
  expect_match(rec$reason, "terminal_run: dead 7, review 4, none 8, unavailable 1")
  expect_match(rec$reason, "accel_flatline: dead 2, alive 3, unavailable 15")
  expect_match(rec$reason, "1 of 6 accelerometers raw")
  expect_match(rec$parameters, "sites_km = 1")
  expect_match(rec$parameters, "persistence = \"\\(window_days = 30; .*review_at = 0.95\\)\"")
  expect_match(rec$parameters, "terminal_run = \"\\(min_days = 14; .*stray_days = 2\\)\"")
  expect_match(rec$parameters, "digest = \"[0-9a-f]{12}\"")
  expect_equal(nrow(log_breaches(lg)), 0L)
  # A second call with the same bands adds nothing.
  expect_equal(nrow(log_mortality_bands(y, mb)$log), nrow(lg))
  # A run with other settings and the same band counts is a second record.
  mb16 <- suppressWarnings(detect_mortality(x, mortality_rules(min_days = 16)))
  expect_equal(table(mb16$bands$band), table(mb$bands$band))
  y16 <- log_mortality_bands(y, mb16)
  expect_equal(nrow(y16$log), nrow(lg) + 1L)
  expect_match(y16$log$parameters[nrow(y16$log)], "min_days = 16")
  # Bands computed on other detections say so in the record.
  z <- filter_min_lag(x, max_lag_s = 3600)
  rz <- log_mortality_bands(z, mb)$log
  expect_match(rz$reason[nrow(rz)],
               "the bands were computed on 19,672 detections and the set holds 19,")
  expect_error(log_mortality_bands(x, list()), "must come from detect_mortality")
})

test_that("the raw-accelerometer warning is given once per session for one set of transmitters", {
  # The bundled example holds one uncalibrated accelerometer on purpose; the
  # warning comes once. The count stays on the bands.
  x <- mortality_example()
  expect_warning_fresh({
    a <- detect_mortality(x)
    b <- detect_mortality(x)
  }, "1 of 6 accelerometer transmitters have raw readings only")
  n <- 0L
  withCallingHandlers(detect_mortality(x), warning = function(w) {
    if (grepl("raw readings only", conditionMessage(w))) n <<- n + 1L
    invokeRestart("muffleWarning")
  })
  expect_equal(n, 0L)
  expect_equal(unname(b$settings$raw_accelerometers[["raw"]]), 1L)
  expect_output(print(b), "1 of 6 accelerometers has raw readings only and cast no vote")
  # Calibrated data never warn.
  cal <- x
  cal$animals$sensor_slope[is.na(cal$animals$sensor_slope) &
                             cal$animals$sensor_type %in% "acceleration"] <- 0.01
  cal$animals$sensor_intercept[is.na(cal$animals$sensor_intercept) &
                                 cal$animals$sensor_type %in% "acceleration"] <- 0
  expect_equal(unname(quiet_raw(detect_mortality(cal))$settings$raw_accelerometers[["raw"]]), 0L)
})
