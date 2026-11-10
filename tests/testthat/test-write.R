# write_detset() and read_detset(): RDS exactly, Parquet losslessly.

calibrated_example <- function() {
  ex <- hard_cases()
  x <- detset(ex$detections, ex$deployments, ex$animals)
  calibrate_sensors(x, read_tag_specs(ext_file("tag_specs_example.csv")))
}

test_that("the RDS round trip returns the identical object", {
  x <- calibrated_example()
  f <- withr::local_tempfile(fileext = ".rds")
  expect_identical(write_detset(x, f), f)
  expect_identical(read_detset(f), x)
  expect_error(write_detset(x, f), "exists; give overwrite = TRUE")
  expect_silent(write_detset(x, f, overwrite = TRUE))
  expect_error(write_detset(x, c("a", "b")), "one file or folder path")
  expect_error(write_detset(list(), f), "must be a detset")
})

test_that("the Parquet round trip is lossless: every table, the log and the metadata", {
  skip_if_not_installed("arrow")
  x <- calibrated_example()
  x$checks <- list(summary = data.frame(check = "a", flagged = 1L))
  x$mortality <- data.frame(animal_id = "EX-01", call = "alive", stringsAsFactors = FALSE)
  d <- file.path(withr::local_tempdir(), "set")
  write_detset(x, d, format = "parquet")
  expect_setequal(list.files(d), c("detections.parquet", "deployments.parquet",
                                   "animals.parquet", "listening.parquet", "log.parquet",
                                   "mortality.parquet", "meta.json", "other.rds"))
  y <- read_detset(d)
  for (nm in c("detections", "deployments", "animals", "listening", "log", "mortality")) {
    expect_identical(y[[nm]], x[[nm]], label = nm)
  }
  expect_identical(y$meta, x$meta)
  expect_identical(y$checks, x$checks)
  expect_setequal(names(y), names(x))
  expect_error(write_detset(x, d, format = "parquet"), "already holds a detset")
})

test_that("Parquet keeps VDAT microseconds and a detset with no animals", {
  skip_if_not_installed("arrow")
  v <- suppressMessages(read_detections(ext_file("vdat_example.csv")))
  dep <- data.frame(receiver_serial = "480013", station_id = "G13",
                    deploy_datetime = as.POSIXct("2023-04-25 12:00:00", tz = "UTC"),
                    recover_datetime = as.POSIXct(NA, tz = "UTC"), deploy_lat = 44.55,
                    deploy_lon = -78.1)
  v$datetime <- v$datetime + 0.123456
  x <- detset(v, dep)
  d <- file.path(withr::local_tempdir(), "vd")
  write_detset(x, d, format = "parquet")
  y <- read_detset(d)
  expect_null(y$animals)
  expect_true("animals" %in% names(y))
  expect_equal(y$detections$datetime, x$detections$datetime, tolerance = 0)
  expect_lt(max(abs(as.numeric(y$detections$datetime) - as.numeric(x$detections$datetime))),
            1e-6)
  expect_identical(y$detections[, -1], x$detections[, -1])
})

test_that("read_detset() names what it cannot read", {
  expect_error(read_detset(tempfile()), "No file or folder at")
  d <- withr::local_tempdir()
  expect_error(read_detset(d), "no meta.json; it is not a saved detset")
})
