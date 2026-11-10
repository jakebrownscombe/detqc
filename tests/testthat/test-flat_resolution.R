# A flat limit is judged at the sensor's resolution: a range that one count
# of noise either side of a dead tag's reading can make (two counts) reads
# flat. The example's depth sensor has 0.1504 m per count, so two counts are
# 0.3008 m, just above the default depth_flat of 0.3 m.

depth_step <- 0.1504
depth_icpt <- -0.6018

# Hourly readings of one depth tag from 1 to 20 June 2024, from counts.
depth_readings <- function(counts, step = depth_step, with_step = TRUE) {
  t <- seq(as.POSIXct("2024-06-01", tz = "UTC"), by = 3600, length.out = length(counts))
  data.table::data.table(
    animal_id = "A1", transmitter = "T1", datetime = t, date = as.Date(t), raw = counts,
    value = depth_icpt + step * counts, unit = "m", class = "depth", scale = "calibrated",
    step = if (with_step) step else NA_real_
  )
}

depth_rule <- function(r, ...) {
  st <- mortality_rules(min_flat_days = 3, ...)$depth_flatline$settings
  rule_depth_flatline(list(readings = r), st)
}

# Alive and diving over 2 to 9 m until 10 June, then dead at 6.1 m with one
# count of noise either side.
dead_on_10_june <- function() {
  set.seed(5)
  n_alive <- 9 * 24
  alive <- round((runif(n_alive, 2, 9) - depth_icpt) / depth_step)
  rest <- round((6.1 - depth_icpt) / depth_step)
  dead <- rest + rep(c(-1L, 0L, 1L, 0L), length.out = 10 * 24)
  c(alive, dead)
}

test_that("a dead tag with one count of noise either side reads flat from the day it died", {
  out <- depth_rule(depth_readings(dead_on_10_june()))
  v <- out$votes
  expect_equal(v$vote, "dead")
  expect_equal(as.Date(v$date), as.Date("2024-06-10"))
  expect_equal(v$flat_days, 10L)
  days <- out$evidence
  # Every dead day spans two counts, 0.3008 m, above the configured 0.3 m
  expect_true(all(abs(days$range_m[days$date >= as.Date("2024-06-10")] - 2 * depth_step) < 1e-9))
  expect_true(all(days$flat[days$date >= as.Date("2024-06-10")]))
  expect_true(all(abs(days$limit - 2 * depth_step) < 1e-12))
})

test_that("a live tag varying by three counts or more does not read flat", {
  base <- round((6.1 - depth_icpt) / depth_step)
  for (k in 3:4) {
    counts <- base + rep(c(0L, k), length.out = 20 * 24)
    out <- depth_rule(depth_readings(counts))
    expect_false(any(out$evidence$flat), label = sprintf("%d counts", k))
    expect_equal(out$votes$vote, "none")
  }
})

test_that("the limit never falls below the configured one", {
  # A fine sensor of 0.01 m per count: two counts are 0.02 m, so the limit
  # stays at depth_flat, 0.3 m, and a range of 0.25 m reads flat.
  counts <- 610L + rep(c(0L, 25L), length.out = 20 * 24)
  out <- depth_rule(depth_readings(counts, step = 0.01))
  expect_true(all(abs(out$evidence$limit - 0.3) < 1e-12))
  expect_true(all(out$evidence$flat))
  # A larger configured limit is kept as given
  out2 <- depth_rule(depth_readings(dead_on_10_june()), depth_flat = 0.5,
                     depth_flat_share = 1)
  expect_true(all(abs(out2$evidence$limit - 0.5) < 1e-12))
})

test_that("depth that arrives calibrated, with no slope, takes its step from the readings", {
  t <- seq(as.POSIXct("2024-06-01", tz = "UTC"), by = 3600, length.out = 19 * 24)
  counts <- dead_on_10_june()[seq_along(t)]
  x <- list(
    detections = data.frame(animal_id = "A1", transmitter = "T1", datetime = t,
                            sensor_value = depth_icpt + depth_step * counts,
                            sensor_unit = "m", sensor_type = "depth"),
    animals = data.frame(animal_id = "A1", transmitter = "T1", sensor_type = "depth",
                         sensor_unit = "m", sensor_slope = NA_real_, sensor_intercept = NA_real_)
  )
  r <- sensor_readings(x)
  expect_equal(unique(r$class), "depth")
  expect_equal(unique(r$step), depth_step, tolerance = 1e-9)
  v <- depth_rule(r)$votes
  expect_equal(as.Date(v$date), as.Date("2024-06-10"))
  # With a slope in the set, the step is the slope
  x$animals$sensor_slope <- depth_step
  expect_equal(unique(sensor_readings(x)$step), depth_step)
})

test_that("the step ignores floating-point gaps and readings with one value", {
  expect_equal(smallest_gap(c(1.3536, 1.3536 + 1e-15, 1.6544, 1.9552)), 0.3008,
               tolerance = 1e-9)
  expect_true(is.na(smallest_gap(c(2, 2, 2))))
  expect_true(at_most(0.1504 * 4 - 0.1504 * 2, 2 * 0.1504))
  expect_false(at_most(0.3009, 0.3008))
})

test_that("the accelerometer limit respects a coarse sensor's step", {
  # A dead accelerometer of 0.06 m/s2 per count reads 0 or 1 count, and two
  # counts are 0.12 m/s2, above accel_flat (0.10).
  t <- seq(as.POSIXct("2024-06-01", tz = "UTC"), by = 3600, length.out = 20 * 24)
  cnt <- c(round(stats::rgamma(9 * 24, 4, 1)), rep(c(0L, 1L, 2L), length.out = 11 * 24))
  r <- data.table::data.table(animal_id = "A1", transmitter = "T1", datetime = t,
                              date = as.Date(t), raw = cnt, value = 0.06 * cnt,
                              unit = "m/s2", class = "acceleration", scale = "calibrated",
                              step = 0.06)
  rec <- data.table::data.table(animal_id = "A1", last = max(t))
  st <- mortality_rules(min_flat_days = 3)$accel_flatline$settings
  s <- sensor_stream(r, rec, st)
  expect_equal(s$votes$vote, "dead")
  expect_equal(as.Date(s$votes$flat_onset), as.Date("2024-06-10"))
})
