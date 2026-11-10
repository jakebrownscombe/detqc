# The opt-in filters: rows out equal rows in minus rows logged, no filter
# adds rows, a missing recovery keeps its detections, and the order of the
# filters that commute does not change the rows kept.

prt_set <- function(animals = NULL, deployments = NULL) {
  fx <- readRDS(fixture("positionrtools_example.rds"))
  detset(fx$detections, if (is.null(deployments)) fx$deployments else deployments,
         if (is.null(animals)) fx$animals else animals)
}

test_that("on the 12,545-row fixture rows out equal rows in minus rows logged", {
  x <- prt_set()
  expect_equal(nrow(x$detections), 12545L)
  y <- filter_deployment_period(x)
  y <- filter_duplicates(y)
  y <- filter_release(y)
  y <- filter_post_release(y, hours = 12)
  y <- filter_tag_life(y)
  y <- filter_min_lag(y, max_lag_s = 3600)
  y <- filter_speed(y, max_speed_ms = 3, min_dist_m = 2000)
  y <- filter_min_detections(y, min_detections = 5, min_days = 2)
  lg <- detset_log(y)
  expect_equal(nrow(y$detections), 12545L - sum(lg$removed))
  expect_equal(lg$rows_before[-1], lg$rows_after[-nrow(lg)])
  expect_equal(lg$rows_after, lg$rows_before - lg$removed)
  sums <- vapply(lg$removed_by, function(v) as.numeric(sum(v)), numeric(1))
  expect_equal(sums, as.numeric(lg$removed))
})

test_that("a missing recovery date keeps 368 of 368 detections", {
  fx <- readRDS(fixture("positionrtools_example.rds"))
  s <- fx$deployments$station_id[1]
  base <- filter_deployment_period(prt_set())
  dep <- fx$deployments
  dep$recover_datetime[dep$station_id == s] <- NA
  open <- filter_deployment_period(prt_set(deployments = dep))
  expect_equal(sum(base$detections$station_id == s), 368L)
  expect_equal(sum(open$detections$station_id == s), 368L)
})

test_that("an animal on two release records adds no rows to any filter", {
  fx <- readRDS(fixture("positionrtools_example.rds"))
  extra <- fx$animals[1, ]
  extra$transmitter <- "A69-9001-99999"
  extra$release_datetime <- extra$release_datetime + 30 * 86400
  x <- prt_set(animals = rbind(fx$animals, extra))
  n <- nrow(x$detections)
  outs <- list(filter_release(x), filter_post_release(x, 24), filter_tag_life(x),
               filter_speed(x, max_speed_ms = 3, min_dist_m = 2000), filter_min_detections(x, 5),
               filter_deployment_period(x), filter_duplicates(x),
               filter_min_lag(x, max_lag_s = 3600))
  for (o in outs) {
    expect_lte(nrow(o$detections), n)
    lg <- o$log[nrow(o$log), ]
    expect_equal(lg$rows_after, n - lg$removed)
  }
  # The earliest release counts, so the later record removes nothing.
  expect_equal(nrow(outs[[1]]$detections), n)
})

test_that("filters that commute keep the same rows in either order", {
  x <- prt_set()
  run <- function(x, fs) Reduce(function(acc, f) f(acc), fs, x)
  fs <- list(filter_deployment_period, filter_duplicates, filter_release,
             function(z) filter_post_release(z, hours = 48), filter_tag_life)
  a <- run(x, fs)$detections
  b <- run(x, rev(fs))$detections
  key <- function(d) sort(paste(d$transmitter, d$receiver_serial, format(d$datetime)))
  expect_equal(key(a), key(b))
  expect_lt(nrow(a), nrow(x$detections))
})

test_that("each filter removes what it says", {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  tt <- toy_tables()
  x <- detset(tt$det, tt$dep)
  y <- filter_deployment_period(x)
  expect_equal(nrow(y$detections), 4L)
  expect_equal(y$log$removed_by[[2]], c(unknown_receiver = 1L))
  z <- filter_deployment_period(x, buffer_s = 3600)
  expect_equal(nrow(z$detections), 4L)
  det2 <- tt$det
  det2$datetime[4] <- utc("2023-12-31 23:30:00")
  w <- filter_deployment_period(detset(det2, tt$dep), buffer_s = 3600)
  expect_equal(nrow(w$detections), 4L)
  d <- filter_duplicates(detset(rbind(tt$det, tt$det[1:2, ]), tt$dep))
  expect_equal(d$log$removed[2], 2L)
  expect_equal(d$log$removed_by[[2]], c(duplicate = 2L))
  expect_error(filter_duplicates(x, by = "nope"), "no column `nope`")
})

test_that("filters on release and tag life read the animals table", {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  tt <- toy_tables()
  ani <- data.frame(animal_id = c("a1", "a2"), transmitter = c("T-1", "T-2"),
                    release_datetime = utc(c("2024-01-06", "2024-01-01")),
                    tag_life_days = c(NA, 30))
  x <- detset(tt$det, tt$dep, ani)
  expect_equal(nrow(filter_release(x)$detections), 4L)
  expect_equal(nrow(filter_post_release(x, hours = 24 * 5)$detections), 4L)
  expect_equal(nrow(filter_tag_life(x)$detections), 4L)
  expect_equal(nrow(filter_tag_life(x, default_days = 5)$detections), 3L)
  # With no animals table the step does not run; it says so and logs it.
  expect_message(y <- filter_release(detset(tt$det, tt$dep)),
                 "filter_release\\(\\) did not run: the detset has no animals table")
  expect_equal(nrow(y$detections), nrow(tt$det))
  expect_match(utils::tail(y$log$reason, 1), "^not run: the detset has no animals table")
  expect_message(filter_tag_life(detset(tt$det, tt$dep)), "did not run")
  expect_error(filter_post_release(x, hours = -1), "at least 0")
  expect_error(filter_release(tt$det), "must be a detset")
})

test_that("filter_min_detections removes whole animals and keeps reference tags", {
  x <- example_detset()
  per <- table(x$detections$animal_id)
  y <- filter_min_detections(x, min_detections = 100)
  expect_setequal(unique(stats::na.omit(y$detections$animal_id)), names(per)[per >= 100])
  expect_equal(sum(is.na(y$detections$animal_id)), sum(is.na(x$detections$animal_id)))
  z <- filter_min_detections(x, min_detections = 1, min_days = 30)
  expect_true(all(names(z$log$removed_by[[2]]) %in% "few_days"))
  expect_error(filter_min_detections(x), "min_detections")
})

# ---- Animal ids that do not agree with the animals table ----
# An OTN matched extract names each animal by catalogNumber; an OTN tagging
# sheet read with missing_id = "transmitter" names it by its transmitter.
otn_id_case <- function() {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  det <- data.frame(
    datetime = utc(c("2024-05-01 06:00:00", "2024-05-01 18:00:00", "2024-05-03 12:00:00",
                     "2024-05-01 20:00:00", "2024-05-04 12:00:00", "2025-06-01 12:00:00")),
    receiver_serial = "R1", transmitter = c(rep("A69-1601-1", 3), rep("A69-1601-2", 3)),
    animal_id = c(rep("EXA-1", 3), rep("EXA-2", 3)), stringsAsFactors = FALSE
  )
  dep <- data.frame(receiver_serial = "R1", station_id = "A",
                    deploy_datetime = utc("2024-04-01"), recover_datetime = utc(NA),
                    deploy_lat = 45, deploy_lon = -78)
  ani <- data.frame(animal_id = c("A69-1601-1", "A69-1601-2"),
                    transmitter = c("A69-1601-1", "A69-1601-2"),
                    release_datetime = utc(c("2024-05-01 12:00:00", "2024-05-01 12:00:00")),
                    tag_life_days = c(NA, 365))
  suppressWarnings(detset(det, dep, ani))
}

test_that("the release filters take a release by transmitter when the ids disagree", {
  x <- otn_id_case()
  expect_message(r <- filter_release(x), "6 of 6 detections carry an animal_id .*2 animal_ids")
  expect_equal(nrow(x$detections) - nrow(r$detections), 1L)
  expect_message(p <- filter_post_release(x, hours = 24), "took the release of their transmitter")
  expect_equal(format(p$detections$datetime),
               c("2024-05-01 06:00:00", "2024-05-03 12:00:00", "2024-05-04 12:00:00",
                 "2025-06-01 12:00:00"))
  expect_message(t <- filter_tag_life(x), "took the release of their transmitter")
  expect_equal(nrow(x$detections) - nrow(t$detections), 1L)
})

test_that("a release filter that matches no detection warns with counts", {
  x <- otn_id_case()
  x$animals$transmitter <- c("A69-9001-1", "A69-9001-2")
  expect_warning(r <- filter_release(x),
                 "no detection matched an animal with a release time.*0 of 2 detected")
  expect_equal(nrow(r$detections), nrow(x$detections))
  det <- x$detections
  det$animal_id <- NA_character_
  y <- suppressWarnings(detset(det, x$deployments, x$animals))
  expect_warning(filter_post_release(y, hours = 24), "0 of 6 detections have an animal_id")
})
