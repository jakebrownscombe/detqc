# Summaries and residency: one species per animal, never-detected animals
# kept, and residency read against listening days.

test_that("summarise_detections counts by each unit", {
  x <- example_detset()
  a <- summarise_detections(x, "animal")
  n_ani <- length(unique(stats::na.omit(x$detections$animal_id)))
  expect_equal(nrow(a), n_ani)
  expect_equal(sum(a$detections) + attr(a, "no_animal"), nrow(x$detections))
  s <- summarise_detections(x, "station")
  expect_equal(sum(s$detections), nrow(x$detections))
  r <- summarise_detections(x, "receiver")
  expect_equal(sum(r$detections), nrow(x$detections))
  d <- summarise_detections(x, "day")
  expect_equal(sum(d$detections), nrow(x$detections))
  expect_s3_class(d$date, "Date")
  expect_error(summarise_detections(x, "week"))
})

test_that("an animal under two species keeps one row", {
  ex <- hard_cases()
  ani <- rbind(ex$animals, transform(ex$animals[1, ], transmitter = "A69-1604-9998",
                                     species = "Muskellunge"))
  x <- detset(ex$detections, ex$deployments, ani)
  a <- summarise_detections(x, "animal")
  expect_equal(anyDuplicated(a$animal_id), 0L)
  expect_equal(attr(a, "species_conflicts"), ex$animals$animal_id[1])
  b <- summarise_animals(x)
  expect_equal(anyDuplicated(b$animal_id), 0L)
  expect_equal(b$transmitters[b$animal_id == ex$animals$animal_id[1]], 2L)
})

test_that("summarise_animals keeps animals never detected", {
  ex <- hard_cases()
  ani <- rbind(ex$animals, transform(ex$animals[1, ], animal_id = "NEVER", transmitter = "X-1"))
  x <- detset(ex$detections, ex$deployments, ani)
  b <- summarise_animals(x)
  expect_equal(nrow(b), length(unique(ani$animal_id)))
  nv <- b[b$animal_id == "NEVER", ]
  expect_equal(nv$detections, 0L)
  expect_true(is.na(nv$days_at_liberty))
  y <- detset(ex$detections, ex$deployments)
  expect_equal(nrow(summarise_animals(y)), length(unique(stats::na.omit(y$detections$animal_id))))
})

test_that("residency_index() is days detected over listening days, at most 1", {
  x <- example_detset()
  r <- residency_index(x, "station")
  expect_true(all(r$residency > 0 & r$residency <= 1))
  expect_true(all(r$days_detected <= r$days_listening))
  expect_true(all(r$days_listening <= r$days_period))
  ra <- residency_index(x, "animal")
  expect_equal(nrow(ra), length(unique(stats::na.omit(x$detections$animal_id))))
  # Days a receiver failed leave the denominator.
  y <- audit_listening(x, reference_tags = data.frame(transmitter = "A69-1604-5012"))
  y$listening$listening[y$listening$station_id == "G05"] <- FALSE
  r2 <- residency_index(y, "station")
  g5 <- merge(r[r$station_id == "G05", ], r2[r2$station_id == "G05", ], by = "animal_id")
  expect_true(all(g5$days_listening.y <= g5$days_listening.x))
  expect_true(all(g5$residency.y >= g5$residency.x))
})

test_that("residency_index() on a set with no animal ids says so and has no rows", {
  ex <- detqc_example
  d <- ex$detections
  d$animal_id <- NA_character_
  x <- detset(d, ex$deployments)
  expect_message(r <- residency_index(x, "station"), "no detection has an animal_id")
  expect_equal(nrow(r), 0L)
  expect_named(r, c("animal_id", "station_id", "days_detected", "days_listening",
                    "days_period", "residency"))
  expect_no_warning(suppressMessages(residency_index(x, "animal")))
})

test_that("the summaries of a set with no animal_id give no row, a message and no warning", {
  tt <- toy_tables()
  tt$det$animal_id <- NA_character_
  x <- suppressWarnings(detset(tt$det, tt$dep))
  expect_message(expect_no_warning(a <- summarise_detections(x, "animal")),
                 "no detection has an animal_id, so there is no animal to summarise; 5 detections")
  expect_equal(nrow(a), 0L)
  expect_named(a, c("animal_id", "species", "transmitters", "detections", "stations", "days",
                    "first", "last"))
  expect_equal(attr(a, "no_animal"), 5L)
  expect_message(expect_no_warning(b <- summarise_animals(x)),
                 "5 detections count for no animal and the table has no row")
  expect_equal(nrow(b), 0L)
  # An animals table that names none of the detected transmitters: the
  # message points to the transmitter codes.
  ani <- data.frame(animal_id = "Z1", transmitter = "A69-0000-1",
                    release_datetime = min(tt$det$datetime))
  y <- suppressWarnings(detset(tt$det, tt$dep, ani))
  m <- character()
  withCallingHandlers(z <- summarise_animals(y), message = function(cnd) {
    m <<- c(m, conditionMessage(cnd))
    invokeRestart("muffleMessage")
  })
  expect_match(m, "names none of the detected transmitters", all = FALSE)
  expect_false(any(grepl("Give detset() an animals table", m, fixed = TRUE)))
  expect_equal(z$detections, 0L)
})

test_that("each summary of a set whose animals table matches no detection points to the codes", {
  tt <- toy_tables()
  tt$det$animal_id <- NA_character_
  ani <- data.frame(animal_id = "Z1", transmitter = "A69-0000-1",
                    release_datetime = min(tt$det$datetime))
  y <- suppressWarnings(detset(tt$det, tt$dep, ani))
  x <- suppressWarnings(detset(tt$det, tt$dep))
  said <- function(expr) {
    m <- character()
    withCallingHandlers(expr, message = function(cnd) {
      m <<- c(m, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    })
    paste(m, collapse = " ")
  }
  codes <- "names none of the detected transmitters: check its transmitter codes"
  give <- "Give detset() an animals table that names the transmitters."
  calls <- list(
    summarise_detections = function(s) summarise_detections(s, "animal"),
    residency_station = function(s) residency_index(s, "station"),
    residency_animal = function(s) residency_index(s, "animal"),
    filter_speed = function(s) filter_speed(s, max_speed_ms = 3, min_dist_m = 500)
  )
  for (nm in names(calls)) {
    with_table <- said(calls[[nm]](y))
    expect_match(with_table, codes, fixed = TRUE, info = nm)
    expect_no_match(with_table, give, fixed = TRUE, info = nm)
    without <- said(calls[[nm]](x))
    expect_match(without, give, fixed = TRUE, info = nm)
    expect_no_match(without, codes, fixed = TRUE, info = nm)
  }
  expect_warning(detect_mortality(y), codes, fixed = TRUE)
  expect_warning(detect_mortality(x), give, fixed = TRUE)
})
