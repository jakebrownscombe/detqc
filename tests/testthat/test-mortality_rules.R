# The rule streams, one at a time, and the rule objects.

# ---- mortality_rules() and mortality_rule() ----
test_that("mortality_rules() returns seven streams in run order, and prints them", {
  r <- mortality_rules()
  expect_s3_class(r, "mortality_rules")
  expect_equal(names(r), c("accel_flatline", "terminal_run", "depth_flatline", "record_end",
                           "site_listening", "movement_resumption", "persistence"))
  expect_equal(unname(vapply(r, function(z) z$kind, "")),
               c("vote", "vote", "evidence", "censor", "vote", "vote", "rank"))
  narrow <- withr::with_options(list(width = 76), utils::capture.output(print(r)))
  expect_true(all(nchar(narrow) <= 76))
  # expect_output() prints at 80 columns; these read the unwrapped lines.
  wide <- function(obj) {
    out <- withr::with_options(list(width = 200), utils::capture.output(print(obj)))
    paste(out, collapse = "\n")
  }
  expect_match(wide(r), "7 streams, run in this order")
  expect_match(wide(r), "accel_flat = 0.1; min_flat_days = 14")
  expect_match(wide(r$terminal_run),
               "min_days = 14; min_calendar_days = 14; near_factor = 2; stray_days = 2")
  expect_match(wide(r$site_listening), "within_days = 30; listening_share = 0$")
  # One name sets the look-ahead of both streams that end a record.
  r2 <- mortality_rules(within_days = 10)
  expect_equal(c(r2$record_end$settings$within_days, r2$site_listening$settings$within_days),
               c(10, 10))
  expect_error(mortality_rules(stray_days = -1), "stray_days")
})

test_that("settings change by name in every stream that has them", {
  r <- mortality_rules(min_flat_days = 21, accel_flat = 0.05)
  expect_equal(r$accel_flatline$settings$min_flat_days, 21)
  expect_equal(r$depth_flatline$settings$min_flat_days, 21)
  expect_equal(r$accel_flatline$settings$accel_flat, 0.05)
  expect_error(mortality_rules(nope = 1), "No stream has a setting `nope`")
  expect_error(mortality_rules(1), "must be named")
  expect_error(mortality_rules(min_active_days = 0), "min_active_days")
  expect_error(mortality_rules(accel_flat = -1), "accel_flat")
  expect_error(mortality_rules(active_within_days = NA), "active_within_days")
  expect_error(mortality_rules(coverage = 2), "at most 1")
  expect_equal(names(mortality_rules(drop = c("persistence", "depth_flatline"))),
               c("accel_flatline", "terminal_run", "record_end", "site_listening",
                 "movement_resumption"))
  expect_error(mortality_rules(drop = "nope"), "no default stream `nope`")
  expect_error(mortality_rules(drop = 1), "must name streams")
})

test_that("mortality_rule() checks what it wraps", {
  f <- function(data, settings) data.frame(animal_id = character(), vote = character())
  r <- mortality_rule("mine", f, "censor", list(a = 1))
  expect_s3_class(r, "mortality_rule")
  expect_equal(r$kind, "censor")
  expect_error(mortality_rule("My Rule", f), "lower-case name")
  expect_error(mortality_rule("mine", 1), "must be a function")
  expect_error(mortality_rule("mine", f, "other"), "`kind` must be one of", class = "detqc_error")
  expect_error(mortality_rule("mine", f, settings = 1), "must be a list")
})

test_that("a user's stream runs beside the defaults and can decide a band", {
  x <- mortality_example()
  few <- mortality_rule("few_days", function(data, settings) {
    r <- data$records
    data.frame(animal_id = r$animal_id, vote = ifelse(r$n_days < settings$days, "review", "none"),
               score = r$n_days)
  }, settings = list(days = 10))
  rules <- mortality_rules(sparse_to_review = FALSE)
  rules$few_days <- few
  mb <- detect_mortality(x, rules)
  b <- mb$bands
  id <- case_row("sparse_record")$id
  expect_equal(b$deciding_rule[b$animal_id == id], "few_days")
  expect_equal(b$rule_label[b$animal_id == id], "few days")
  expect_true("few_days_vote" %in% names(b))
  expect_equal(sum(mb$votes$rule == "few_days"), 20L)
})

test_that("a user's stream that returns the wrong shape is named", {
  x <- mortality_example()
  add <- function(fun) {
    r <- mortality_rules(drop = c("persistence", "movement_resumption", "depth_flatline"))
    r$bad <- mortality_rule("bad", fun)
    r
  }
  ids <- function(data) data$records$animal_id
  expect_error(detect_mortality(x, add(function(data, settings) 1)), "`bad` must return")
  expect_error(detect_mortality(x, add(function(data, settings) data.frame(vote = "dead"))),
               "`bad` have no column `animal_id`")
  expect_error(detect_mortality(x, add(function(data, settings) {
    data.frame(animal_id = ids(data), vote = "maybe")
  })), "voted \"maybe\"")
  expect_error(detect_mortality(x, add(function(data, settings) {
    data.frame(animal_id = rep(ids(data)[1], 2), vote = "none")
  })), "voted twice")
  expect_error(detect_mortality(x, add(function(data, settings) {
    data.frame(animal_id = "ghost", vote = "none")
  })), "animal ghost, which has no detection")
  expect_error(detect_mortality(x, add(function(data, settings) {
    data.frame(animal_id = ids(data)[1], vote = "dead", date = as.Date("2024-06-01"))
  })), "must be POSIXct in UTC")
  expect_error(detect_mortality(x, add(function(data, settings) stop("boom"))),
               "Rule stream `bad` failed: boom")
  r <- mortality_rules()
  names(r)[1] <- "other"
  expect_error(detect_mortality(x, r), "under its own name")
  r2 <- mortality_rules()
  r2$terminal_run$name <- "accel_flatline"
  names(r2)[2] <- "accel_flatline"
  expect_error(detect_mortality(x, r2), "share the name `accel_flatline`")
})

# ---- Terminal stationary run ----
test_that("the terminal window spans 14 detection days and at least 14 calendar days", {
  t0 <- as.POSIXct("2024-01-01", tz = "UTC")
  d <- data.table::data.table(datetime = t0 + (c(0:9, 30:43) * 86400),
                              site = c(rep(2L, 10), rep(1L, 14)),
                              lat = c(rep(45.1, 10), rep(45, 14)), lon = -78,
                              station_id = c(rep("B", 10), rep("A", 14)))
  d[, date := as.Date(datetime)]
  s <- list(min_days = 14, min_calendar_days = 14, near_factor = 2)
  r <- terminal_one(d, s, 1)
  expect_equal(r$vote, "dead")
  expect_equal(r$window_start, as.Date("2024-01-31"))
  expect_equal(r$run_detection_days, 14L)
  expect_identical(r$reason, paste("the final run, 2024-01-31 to 2024-02-13, is at one site;",
                                   "its last 14 detection days, 2024-01-31 to 2024-02-13, span",
                                   "0.000 km"))
  # At 40 calendar days the window reaches back to the other site.
  r30 <- terminal_one(d, utils::modifyList(s, list(min_calendar_days = 40)), 1)
  expect_equal(r30$vote, "none")
  expect_equal(r30$window_sites, 2L)
  # The text counts the detection days in the window, 6 + 14.
  expect_match(r30$reason, "^the record's last 20 detection days, 2024-01-05 to 2024-02-13, span 2")
  sparse <- terminal_one(d[1:5], s, 1)
  expect_equal(sparse$vote, "unavailable")
  expect_match(sparse$reason, "sparse: 5 detection days of the 14")
})

test_that("window geometry tells dead, split, moved, near and none apart", {
  w <- function(lat, site, station) {
    data.table::data.table(lat = lat, lon = -78, site = site, station_id = station)
  }
  km <- 1 / 111.195
  expect_equal(window_geometry(w(c(45, 45 + 0.5 * km), 1L, c("A", "B")), 1, NULL)$vote, "dead")
  expect_equal(window_geometry(w(c(45, 45 + 0.5 * km), 1:2, c("A", "B")), 1, NULL)$vote, "split")
  expect_equal(window_geometry(w(c(45, 45 + 1.5 * km), 1:2, "A"), 1, NULL)$vote, "moved")
  expect_equal(window_geometry(w(c(45, 45 + 1.5 * km), 1:2, c("A", "B")), 1, NULL)$vote, "near")
  expect_equal(window_geometry(w(c(45, 45 + 1.5 * km), 1:2, c("A", "B")), 1, NULL, 0)$vote,
               "none")
  expect_equal(window_geometry(w(c(45, 45 + 3 * km), 1:2, c("A", "B")), 1, NULL)$vote, "none")
  # One site whose stations span more than sites_km cannot happen under
  # complete linkage, but a user's distances can make it: no dead vote.
  expect_equal(window_geometry(w(c(45, 45 + 1.5 * km), 1L, c("A", "B")), 1, NULL)$vote, "near")
})

test_that("runs are followed back by geometry and by station", {
  near <- function(lat, lon) dist_km_matrix(lat, lon)
  expect_equal(geometric_run(c(45, 45.001, 45.002, 45.003), rep(-78, 4), 0.25, near), 2L)
  expect_equal(geometric_run(c(45, 45.001, 45.002, 45.003), rep(-78, 4), 0.25), 2L)
  expect_equal(geometric_run(rep(45, 3), rep(-78, 3), 0.25, near), 1L)
  expect_equal(station_run(c("A", "B", "B", "A", "A")), 4L)
  expect_equal(station_run(c("A", "A")), 1L)
  expect_equal(window_spread(rep(45, 2), rep(-78, 2), near), 0)
})

# ---- Accelerometer flatline ----
test_that("the flatline onset is the day after the last active day, and spikes can be skipped", {
  dates <- as.Date("2024-01-01") + c(0:9, 40:99)
  flat <- c(rep(FALSE, 10), rep(TRUE, 60))
  first <- as.POSIXct(format(dates), tz = "UTC") + 3600
  r <- flatline_one(dates, flat, first, 14)
  expect_equal(format(r$flat_onset), "2024-01-11")
  expect_equal(format(r$first_flat_observed), "2024-02-10 01:00:00")
  expect_equal(r$flat_days, 60L)
  spike <- flat
  spike[40] <- FALSE
  expect_equal(flatline_one(dates, spike, first, 14, 1L)$last_active, dates[40])
  r2 <- flatline_one(dates, spike, first, 14, 2L)
  expect_equal(r2$last_active, dates[10])
  expect_equal(r2$spikes_ignored, 1L)
  tail_active <- flat
  tail_active[70] <- FALSE
  expect_false(flatline_one(dates, tail_active, first, 14, 2L)$flatline)
  never <- flatline_one(dates, rep(TRUE, 70), first, 14)
  expect_equal(never$flat_onset, first[1])
  x <- mortality_example()
  s <- detect_mortality(x, mortality_rules(min_active_days = 2))$sensor
  expect_true(all(c("spikes_ignored", "first_flat_observed") %in% names(s)))
})

test_that("the sensor stream reads sensor_raw through the animals' calibration", {
  x <- mortality_example()
  r <- sensor_readings(x)
  acc <- r[r$transmitter == "A69-9004-62001", ]
  expect_true(all(acc$scale == "calibrated"))
  expect_equal(acc$value, 0.013588 * acc$raw)
  expect_true(all(acc$unit == "m/s2"))
  s <- example_bands()$sensor
  expect_equal(nrow(s), 7L)
  expect_equal(s$vote[s$class == "depth"], "unavailable")
  expect_equal(s$reason[s$class == "depth"], "depth sensor: evidence only, no vote")
  expect_equal(nrow(sensor_readings(detset(toy_tables()$det, toy_tables()$dep))), 0L)
})

test_that("a sensor class or unit that cannot be read is said so", {
  expect_equal(stream_reason(c("unknown", "acceleration"), c(0L, 0L), c(5L, 5L),
                             c(NA, NA), c(NA, NA), 14),
               c("sensor class unknown: give sensor_type or sensor_unit",
                 "too few readings per day to judge"))
  withr::local_locale(c(LC_CTYPE = "C"))
  expect_equal(sensor_class(NA, c("m/s²", "°C", "Meters", "ADC")),
               c("acceleration", "temperature", "depth", NA))
})

test_that("an animal with two accelerometers is judged on the one with more readings", {
  s <- data.table::data.table(animal_id = c("a", "a"), transmitter = c("t1", "t2"),
                              class = "acceleration", readings = c(10L, 50L),
                              raw_only = 0L, vote = c("dead", "alive"))
  expect_equal(pick_accel_vote(s)$transmitter, "t2")
})

# ---- Depth flatline, evidence only ----
test_that("the depth threshold shrinks in shallow water", {
  x <- mortality_example()
  ev <- example_bands()$evidence$depth_flatline
  # The example's sensor has 0.1504 m per count. The limit shrinks with the
  # median depth but never below two counts, 0.3008 m, which a dead tag's
  # noise makes; so at this resolution every day's limit is two counts.
  share_limit <- pmin(0.3, 0.1 * pmax(ev$med, 0))
  expect_true(any(share_limit[ev$med < 3] < 0.3))
  expect_equal(ev$limit, pmax(share_limit, 2 * 0.1504), tolerance = 1e-12)
  # A shallow live animal: range 0.25 m at a median depth of 1.5 m is not
  # flat (limit 0.15 m), though a fixed 0.3 m threshold would call it flat.
  r <- data.table::data.table(animal_id = "s", transmitter = "t", date = as.Date("2024-01-01"),
                              datetime = as.POSIXct("2024-01-01", tz = "UTC") + 1:3,
                              value = c(1.4, 1.5, 1.65), class = "depth", scale = "calibrated")
  d <- rule_depth_flatline(list(readings = r), mortality_rules()$depth_flatline$settings)
  expect_false(d$evidence$flat)
  expect_equal(d$evidence$limit, 0.15)
  expect_equal(nrow(rule_depth_flatline(list(readings = r[0]),
                                        mortality_rules()$depth_flatline$settings)$votes), 0L)
})

# ---- Record end ----
test_that("the record-end stream measures the gap to the earlier of tag life and horizon", {
  r <- case_row("record_ends_at_tag_life")
  expect_equal(r$votes[["record_end"]], "review")
  expect_equal(format(as.Date(r$b$expected_end)), "2024-09-28")
  mb <- detect_mortality(mortality_example(), mortality_rules(within_days = 0))
  b <- mb$bands
  expect_equal(b$band[b$animal_id == r$id], "alive")
  # An animals table with no tag life leaves the horizon as the end.
  x <- mortality_example()
  x$animals$tag_life_days <- NA_real_
  b2 <- detect_mortality(x)$bands
  expect_equal(b2$end_basis[b2$animal_id == r$id], "data horizon")
  expect_equal(b2$band[b2$animal_id == r$id], "alive")
})

test_that("a missing release time takes the first detection as the start of tag life", {
  x <- mortality_example()
  x$animals$release_datetime <- NULL
  rec <- expected_end(animal_records(x$detections, data.table::data.table(animal_id = character())),
                      x$animals, as.POSIXct("2024-12-31", tz = "UTC"))
  id <- case_row("record_ends_at_tag_life")$id
  expect_equal(rec$tag_end[rec$animal_id == id], rec$first[rec$animal_id == id] + 150 * 86400)
  rec0 <- expected_end(rec[0], NULL, as.POSIXct("2024-12-31", tz = "UTC"))
  expect_equal(nrow(rec0), 0L)
})

# ---- Movement after the candidate date ----
test_that("movement counts days heard only away from the candidate site", {
  t0 <- as.POSIXct("2024-01-01", tz = "UTC")
  d <- data.table::data.table(datetime = t0 + c(0, 1, 2, 3, 3.1, 4, 5) * 86400,
                              station_id = c("A", "A", "A", "A", "Z", "Z", "Z"),
                              lat = c(45, 45, 45, 45, 45.05, 45.05, 45.05), lon = -78)
  d[, date := as.Date(datetime)]
  s <- list(ref_days = 3, min_days_away = 2)
  m <- moved_one(d, s, 1)
  # Day 4 heard A and Z, so only days 5 and 6 are away.
  expect_equal(m$score, 2)
  expect_equal(m$vote, "alive")
  expect_equal(m$first_away, d$datetime[6])
  expect_match(m$detail, "1 station heard")
  expect_equal(moved_one(d, list(ref_days = 3, min_days_away = 3), 1)$vote, "none")
  # Another receiver at the same station is not movement, wherever it sits.
  d2 <- data.table::copy(d)[, station_id := "A"]
  expect_equal(moved_one(d2, s, 1)$score, 0)
})

# ---- Persistence ----
test_that("persistence is the Jaccard of 95% cores 7 days apart", {
  days <- as.Date("2024-01-01") + 0:59
  steady <- data.table::data.table(date = rep(days, each = 2), station_id = c("A", "B"), n = 5L)
  s <- mortality_rules()$persistence$settings
  p <- persistence_pairs(steady, s)
  expect_true(all(p$jaccard == 1))
  # A window from day 10 (10 days in it) to day 52 (7 days before the last).
  expect_equal(nrow(p), 60L - 10L - 7L)
  shift <- data.table::data.table(date = days, station_id = ifelse(days < days[30], "A", "B"),
                                  n = 5L)
  ps <- persistence_pairs(shift, s)
  expect_lt(min(ps$jaccard), 1)
  expect_equal(core_of(c(10, 0, 0), 0.95), c(TRUE, FALSE, FALSE))
  expect_equal(core_of(c(5, 5, 0), 0.95), c(TRUE, TRUE, FALSE))
  expect_equal(core_of(c(0, 0), 0.95), c(FALSE, FALSE))
  one <- data.table::data.table(date = days[1:5], station_id = "A", n = 1L)
  expect_equal(nrow(persistence_pairs(one, s)), 0L)
})

test_that("persistence only ranks when review_at is infinite", {
  x <- mortality_example()
  b <- detect_mortality(x, mortality_rules(review_at = Inf))$bands
  id <- case_row("persistent_far_pair_dead")$id
  expect_equal(b$band[b$animal_id == id], "alive")
  expect_false(is.na(b$persistence[b$animal_id == id]))
  r <- mortality_rules()
  r$persistence$settings$review_at <- NULL
  b2 <- detect_mortality(x, r)$bands
  expect_equal(b2$band[b2$animal_id == id], "alive")
})

test_that("streams with no detections or readings return no votes", {
  data <- list(detections = data.table::data.table(), readings = empty_readings(),
               records = data.table::data.table(animal_id = character(), last = empty_utc()),
               votes = empty_votes_dt(), sites_km = 1)
  expect_equal(nrow(rule_terminal_run(data, list())$votes), 0L)
  expect_equal(nrow(rule_persistence(data, list())), 0L)
  expect_equal(nrow(rule_movement(data, list())), 0L)
  expect_equal(nrow(rule_accel_flatline(data, mortality_rules()$accel_flatline$settings)$votes),
               0L)
})
