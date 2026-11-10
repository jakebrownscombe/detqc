# The animals and tags table (CONTRACT.md version 2) and what detset()
# records about the readers.

test_that("validate_detset() checks the animals and tags table", {
  tt <- toy_tables()
  ani <- data.frame(animal_id = c("a1", "a2"), transmitter = c("T-1", "T-2"),
                    min_delay = c(80, 200), max_delay = c(160, 100),
                    stringsAsFactors = FALSE)
  expect_error(detset(tt$det, tt$dep, ani), "T-2 has min_delay 200 above max_delay 100")
  ani$max_delay[2] <- 300
  ani$sensor_slope <- c(0.1, NA)
  ani$sensor_intercept <- c(NA_real_, NA_real_)
  expect_error(detset(tt$det, tt$dep, ani), "T-1 has a sensor slope or intercept but not both")
  ani$sensor_intercept[1] <- 0
  x <- detset(tt$det, tt$dep, ani)
  expect_s3_class(x, "detset")
  y <- x
  y$animals <- rbind(y$animals, y$animals[1, ])
  y$animals$tag_model <- c("A", "B", "C")
  expect_error(validate_detset(y), "Animal a1 has 2 rows for transmitter T-1")
  bad <- ani
  bad$release_lat <- "north"
  expect_error(detset(tt$det, tt$dep, bad), "animals\\$release_lat` must be numeric")
})

test_that("detset() lists the files the readers read in meta$sources", {
  det <- read_detections(ext_file("vue_example.csv"))
  dep <- read_deployments(ext_file("glatos_receivers_example.csv"), "glatos")
  ani <- read_animals(ext_file("glatos_detections_example.csv"), "glatos")
  x <- detset(det, dep, ani)
  s <- x$meta$sources
  expect_equal(s$table, c("detections", "deployments", "animals"))
  expect_equal(s$format, c("vue", "glatos", "glatos"))
  expect_equal(s$rows, c(833L, 25L, nrow(ani)))
  expect_equal(basename(s$file), c("vue_example.csv", "glatos_receivers_example.csv",
                                   "glatos_detections_example.csv"))
  expect_equal(nrow(detset(toy_tables()$det, toy_tables()$dep)$meta$sources), 0L)
})

test_that("a log that lacks a column of the log stops validate_detset() with its name", {
  x <- detset(toy_tables()$det, toy_tables()$dep)
  x$log$fn <- NULL
  expect_error(validate_detset(x), "`log` has no column `fn`")
})
