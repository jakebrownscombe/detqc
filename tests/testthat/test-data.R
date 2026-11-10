# ---- The two datasets: the Stoney Lake study ----
# Pinned on the output of data-raw/detqc_example.R: 3297 detections, 20
# deployments and 4 transmitters as read, and 3148 detections after
# filter_post_release(hours = 6) (106 removed) and
# filter_min_lag(max_lag_s = 4500) (43 removed).

test_that("detqc_example is the Stoney study as read from its files", {
  ex <- detqc_example
  expect_identical(names(ex), c("detections", "deployments", "animals"))
  expect_equal(vapply(ex, nrow, 1L), c(detections = 3297L, deployments = 20L, animals = 4L))
  expect_setequal(ex$animals$animal_id, sprintf("ST-%02d", 1:4))
  f <- system.file("extdata", "stoney_detections.csv", package = "detqc")
  d <- read_detections(f)
  expect_equal(ex$detections$datetime, d$datetime)
  expect_equal(ex$detections$transmitter, d$transmitter)
  expect_true(all(is.na(ex$detections$sensor_value)))
})

test_that("example_mortality is the prepared study with the true fates", {
  ex <- example_mortality
  expect_identical(names(ex), c("detections", "deployments", "animals", "fates"))
  expect_equal(nrow(ex$detections), 3148L)
  expect_equal(ex$fates$fate, c("alive", "dead", "alive", "alive"))
  expect_equal(ex$fates$death_datetime[2], as.POSIXct("2024-06-04 02:00:00", tz = "UTC"))
  # After its death the dead tag is heard at one station only.
  after <- ex$detections$animal_id == "ST-02" &
    ex$detections$datetime > ex$fates$death_datetime[2]
  expect_identical(unique(ex$detections$station_id[after]), "S011")
  expect_equal(sum(!is.na(ex$detections$sensor_value)), 1237L + 418L)
})

test_that("rules scaled to eight days band ST-02 dead on example_mortality", {
  # Scaled rules: ST-02 dead (terminal_run), ST-01, ST-03 and ST-04 in
  # review; the defaults send all four to review.
  ex <- example_mortality
  x <- detset(ex$detections, ex$deployments, ex$animals)
  expect_equal(unname(table(detect_mortality(x)$bands$band)[["review"]]), 4L)
  short <- mortality_rules(min_days = 3, min_calendar_days = 3, min_flat_days = 2,
                           within_days = 2)
  b <- detect_mortality(x, rules = short)$bands
  expect_equal(b$band[b$animal_id == "ST-02"], "dead")
  expect_equal(b$deciding_rule[b$animal_id == "ST-02"], "terminal_run")
  expect_equal(sort(b$animal_id[b$band == "review"]), c("ST-01", "ST-03", "ST-04"))
})

test_that("the Stoney format files read to the same day of detections", {
  f <- function(n) system.file("extdata", n, package = "detqc")
  vue <- read_detections(f("stoney_vue.csv"))
  otn <- read_detections(f("stoney_otn.csv"))
  expect_equal(nrow(vue), 309L)
  expect_identical(vue$datetime, otn$datetime)
  expect_identical(vue$transmitter, otn$transmitter)
  vdat <- suppressMessages(read_detections(f("stoney_vdat.csv")))
  expect_equal(nrow(vdat), 150L)
  expect_identical(unique(vdat$receiver_serial), "R011")
})

# ---- stoney_clean: the Stoney study read, checked and filtered ----
# Pinned on the output of data-raw/stoney_clean.R
# (data-raw/stoney_clean.Rout): 3,297 detections read, 106 removed by
# filter_post_release(), 43 by filter_min_lag() and 1,210 by
# filter_mortality(), 1,938 left.
test_that("stoney_clean holds the 1,938 detections of the Get started steps", {
  dets <- stoney_clean
  expect_s3_class(dets, "detset")
  expect_silent(validate_detset(dets))
  expect_equal(nrow(dets$detections), 1938L)
  log_table <- detset_log(dets)
  expect_identical(log_table$step,
                   c("detset", "calibrate_sensors", "check_detset", "audit_listening",
                     "filter_duplicates", "filter_post_release", "filter_min_lag",
                     "apply_mortality_calls", "filter_mortality"))
  expect_equal(log_table$removed, c(0, 0, 0, 0, 0, 106, 43, 0, 1210))
  expect_equal(log_table$rows_before[1], 3297L)
  expect_identical(dets$mortality$call, c("alive", "dead_or_shed", "alive", "alive"))
  expect_false(any(grepl("/", dets$meta$sources$file, fixed = TRUE)))
})
