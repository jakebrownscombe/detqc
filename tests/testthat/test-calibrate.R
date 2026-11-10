# calibrate_sensors(): raw counts to physical values, with counts logged.

test_that("calibration applies intercept + slope * raw and logs every outcome", {
  ex <- hard_cases()
  x <- detset(ex$detections, ex$deployments, ex$animals)
  raw <- !is.na(x$detections$sensor_raw)
  y <- calibrate_sensors(x)
  # EX-17's transmitter A69-9004-7005 has a unit but no slope in the animals
  # table, so its readings stay raw.
  n7005 <- sum(raw & x$detections$transmitter == "A69-9004-7005")
  expect_equal(sum(!is.na(y$detections$sensor_value)), sum(raw) - n7005)
  pattern <- "%s of %s raw sensor readings calibrated \\(0 with a calibration from specs\\)"
  expect_match(detset_log(y)$reason[2], sprintf(pattern, fmt_n(sum(raw) - n7005), fmt_n(sum(raw))))
  expect_match(detset_log(y)$reason[2], sprintf("with no calibration: %s \\(1 transmitter\\)",
                                                fmt_n(n7005)))
  i <- which(y$detections$transmitter == "A69-9004-8001" & raw)[1]
  expect_equal(y$detections$sensor_value[i], -0.6018 + 0.1504 * x$detections$sensor_raw[i])
  expect_equal(y$detections$sensor_unit[i], "m")
  expect_equal(nrow(y$detections), nrow(x$detections))
  expect_equal(detset_log(y)$removed[2], 0L)
  expect_equal(detset_log(y)$step[2], "calibrate_sensors")
})

test_that("specs fill the transmitters the animals table leaves uncalibrated", {
  ex <- hard_cases()
  x <- detset(ex$detections, ex$deployments, ex$animals)
  specs <- read_tag_specs(ext_file("tag_specs_example.csv"))
  y <- calibrate_sensors(x, specs)
  raw <- !is.na(x$detections$sensor_raw)
  n7005 <- sum(raw & x$detections$transmitter == "A69-9004-7005")
  expect_equal(sum(!is.na(y$detections$sensor_value)), sum(raw))
  expect_match(detset_log(y)$reason[2], sprintf("\\(%s with a calibration from specs\\)",
                                                fmt_n(n7005)))
  a <- y$animals[y$animals$transmitter == "A69-9004-7005", ]
  expect_equal(c(a$sensor_slope, a$sensor_intercept), c(0.013588, 0))
  expect_match(detset_log(y)$parameters[2], "specs = <data frame, 7 rows>")
  expect_error(calibrate_sensors(x, specs = list(1)), "must be a data frame")
  expect_error(calibrate_sensors(x, overwrite = NA), "TRUE or FALSE")
})

test_that("existing values are kept unless overwrite, and a missing unit blocks", {
  tt <- toy_tables()
  tt$det$sensor_raw <- c(10, 20, NA, 30, 40)
  tt$det$sensor_value <- c(NA, 99, NA, NA, NA)
  tt$det$sensor_unit <- c(NA, "m", NA, NA, NA)
  ani <- data.frame(animal_id = c("a1", "a2", "a3"), transmitter = c("T-1", "T-2", "T-3"),
                    sensor_slope = c(0.5, 2, 1), sensor_intercept = c(1, 0, 0),
                    sensor_unit = c("m", NA, "C"), stringsAsFactors = FALSE)
  x <- detset(tt$det, tt$dep, ani)
  y <- calibrate_sensors(x)
  expect_equal(y$detections$sensor_value, c(6, 99, NA, NA, 40))
  expect_match(detset_log(y)$reason[2], "kept, with a sensor_value already: 1")
  expect_match(detset_log(y)$reason[2], "with a calibration but no unit: 1")
  z <- calibrate_sensors(x, overwrite = TRUE)
  expect_equal(z$detections$sensor_value[2], 11)
  # A detset with no sensor columns is logged with zero readings.
  bare <- detset(toy_tables()$det, toy_tables()$dep)
  b <- calibrate_sensors(bare)
  expect_match(detset_log(b)$reason[2], "^0 of 0 raw sensor readings")
  expect_true(all(is.na(b$detections$sensor_value)))
})

test_that("calibration matches glatos::real_sensor_values() on the lamprey example", {
  skip_if_not_installed("glatos", "0.9.8")
  real <- glatos::real_sensor_values
  read_specs <- glatos::read_vemco_tag_specs
  skip_if_not_installed("readxl")
  det_file <- glatos_file("lamprey_detections.csv")
  spec_file <- glatos_file("lamprey_tag_specs.xls")
  g_det <- glatos::read_glatos_detections(det_file)
  g <- real(g_det, read_specs(spec_file, file_format = "vemco_xls")$specs)
  d <- read_detections(det_file, "glatos")
  ani <- read_animals(det_file, "glatos")
  expect_warning(dep <- read_deployments(glatos_file("sample_receivers.csv"), "glatos"),
                 "3 of 559 station_id values hold deployments more than 1 km apart")
  x <- suppressWarnings(detset(d, dep, ani))
  y <- calibrate_sensors(x, read_tag_specs(spec_file))
  key_g <- paste(format(g$detection_timestamp_utc), g$transmitter_codespace, g$transmitter_id,
                 g$receiver_sn)
  key_y <- paste(format(y$detections$datetime), y$detections$transmitter,
                 y$detections$receiver_serial)
  k <- match(key_y, sub("^(\\S+ \\S+) (\\S+) (\\S+) ", "\\1 \\2-\\3 ", key_g))
  both <- !is.na(y$detections$sensor_value)
  expect_gt(sum(both), 0)
  expect_equal(y$detections$sensor_value[both], g$sensor_value_real[k[both]])
})

test_that("a predation tag's class reaches the triage though it has no calibration", {
  ex <- hard_cases()
  det <- ex$detections
  k <- which(det$transmitter == "A69-1604-5001")[1:20]
  det$sensor_raw[k] <- 1
  x <- detset(det, ex$deployments, ex$animals)
  specs <- data.frame(transmitter = "A69-1604-5001", sensor_type = "predation",
                      sensor_slope = NA_real_, sensor_intercept = NA_real_,
                      sensor_unit = NA_character_, stringsAsFactors = FALSE)
  y <- calibrate_sensors(x, specs)
  expect_equal(y$animals$sensor_type[y$animals$transmitter == "A69-1604-5001"], "predation")
  r <- mortality_data(y, mortality_control(list()))$readings
  expect_equal(unique(r$class[r$transmitter == "A69-1604-5001"]), "predation")
  # Without the specifications the class stays unknown.
  r0 <- mortality_data(x, mortality_control(list()))$readings
  expect_equal(unique(r0$class[r0$transmitter == "A69-1604-5001"]), "unknown")
})
