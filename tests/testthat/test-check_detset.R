# check_detset(): every check is caught on its planted error and nothing
# else fires (fixtures/planted, written by make_planted.R).

test_that("the clean base passes every check that can pass", {
  base <- planted_base()
  res <- planted_checks(base)
  expect_s3_class(res, "detset_checks")
  expect_equal(res$summary$check, as.vector(all_checks()))
  expect_equal(flagged_checks(res), character())
  # The base is sorted by receiver and time, which hides a clock jump, so
  # dst_jump cannot pass. Its downloads come after the recoveries, so no
  # deployment is tested for detections after its download.
  ran <- !res$summary$check %in% c("dst_jump", "detections_after_download")
  expect_true(all(res$summary$status[ran] == "pass"))
  expect_true(all(res$summary$of[ran] > 0))
  expect_equal(res$summary$status[!ran], c("not run", "not run"))
})

for (ck in all_checks()) {
  local({
    check <- ck
    test_that(paste("the planted error of", check, "is caught by its check alone"), {
      base <- apply_patch(planted_base(), planted_patch(check))
      res <- planted_checks(base)
      # A download a fortnight before the recovery leaves the receiver's
      # later detections after it, which the second check reports.
      expected <- if (check == "recover_after_download") {
        c(check, "detections_after_download")
      } else {
        check
      }
      expect_identical(flagged_checks(res), expected)
      row <- res$summary[res$summary$check == check, ]
      expect_gt(row$flagged, 0)
      expect_gte(row$of, row$flagged)
      expect_gt(nrow(res$rows[[check]]), 0)
    })
  })
}

test_that("the planted counts are the planted rows", {
  n_of <- function(check) {
    res <- planted_checks(apply_patch(planted_base(), planted_patch(check)))
    res$summary$flagged[res$summary$check == check]
  }
  expect_equal(n_of("serial_not_deployed"), 30L)
  expect_equal(n_of("before_release"), 3L)
  expect_equal(n_of("outside_deployment"), 10L)
  expect_equal(n_of("duplicate_rows"), 25L)
  expect_equal(n_of("codetection_out_of_range"), 10L)
  expect_equal(n_of("station_coordinates"), 2L)
  # The download at 11:00 on the last day leaves 2 detections of receiver
  # 480003 before its recovery at 12:00.
  expect_equal(n_of("detections_after_download"), 1L)
  res <- planted_checks(apply_patch(planted_base(), planted_patch("detections_after_download")))
  expect_equal(res$rows$detections_after_download$detections_after, 2L)
})

test_that("a detset gets the results and one log record", {
  base <- planted_base()
  x <- detset(base$detections, base$deployments, base$animals)
  y <- check_detset(x, water = planted_water(base), field = base$field_m,
                    reference_tags = base$reference_tags)
  expect_s3_class(y, "detset")
  expect_equal(nrow(y$detections), nrow(x$detections))
  expect_named(y$checks, c("summary", "rows"))
  lg <- detset_log(y)
  expect_equal(lg$step[nrow(lg)], "check_detset")
  expect_equal(lg$removed[nrow(lg)], 0L)
  expect_match(lg$reason[nrow(lg)], "30 checks: 0 flagged, 27 passed, 3 not run")
  expect_output(print(y$checks), "30 checks: 0 flagged")
})

test_that("checks without their data report not run, with a message for the field", {
  base <- planted_base()
  tabs <- base[c("detections", "deployments")]
  expect_message(res <- check_detset(tabs), "need `field`")
  s <- res$summary
  not_run <- s$check[s$status == "not run"]
  expect_setequal(not_run, c("on_land", "depth_below_bottom", "transmitter_no_animal",
                             "animal_never_heard", "before_release", "after_tag_life",
                             "long_after_release",
                             "animal_conflict",
                             "duplicate_animal_id", "codetection_out_of_range",
                             "reference_position", "dst_jump",
                             "detections_after_download"))
  expect_true(all(is.na(s$flagged[s$status == "not run"])))
  expect_output(print(res), "not run")
  tabs$deployments$download_datetime <- NULL
  res2 <- check_detset(tabs, checks = c("recover_after_download", "detections_after_download"))
  expect_equal(res2$summary$status, c("not run", "not run"))
  res3 <- check_detset(tabs, checks = "reference_position", field = 800,
                       reference_tags = data.frame(transmitter = "A69-1604-9001"))
  expect_match(res3$summary$note, "deploy_lat")
})

test_that("two station names at one place are station_coordinates", {
  base <- planted_base()
  dep <- base$deployments
  i <- which(dep$station_id == "S3")
  dep$deploy_lat[i] <- dep$deploy_lat[dep$station_id == "S4"][1]
  dep$deploy_lon[i] <- dep$deploy_lon[dep$station_id == "S4"][1] + 2e-5
  res <- check_detset(list(detections = base$detections, deployments = dep),
                      checks = "station_coordinates")
  expect_equal(res$summary$flagged, 2L)
  expect_setequal(res$rows$station_coordinates$problem, "two stations at one place")
})

test_that("an extent in the settings replaces the outlier rule", {
  base <- planted_base()
  tabs <- base[c("detections", "deployments")]
  ext <- c(-78.21, -78.17, 44.49, 44.51)
  res <- check_detset(tabs, checks = "outside_extent", settings = check_settings(extent = ext))
  expect_equal(res$summary$flagged, 0L)
  tight <- check_settings(extent = c(-78.21, -78.19, 44.49, 44.51))
  res2 <- check_detset(tabs, checks = "outside_extent", settings = tight)
  expect_gt(res2$summary$flagged, 0L)
  expect_error(check_settings(extent = 1:3), "extent")
  expect_error(check_settings(no_such = 1), "Unknown setting")
  expect_error(check_settings(1), "needs a name")
})

test_that("an sf polygon is the water body", {
  testthat::skip_if_not_installed("sf")
  base <- apply_patch(planted_base(), planted_patch("on_land"))
  lk <- base$lake
  ring <- matrix(c(lk$lon[1], lk$lat[1], lk$lon[2], lk$lat[1], lk$lon[2], lk$lat[2],
                   lk$lon[1], lk$lat[2], lk$lon[1], lk$lat[1]), ncol = 2, byrow = TRUE)
  water <- sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  res <- check_detset(base[c("detections", "deployments")], checks = "on_land", water = water)
  expect_equal(res$summary$flagged, 1L)
  expect_equal(res$rows$on_land$station_id, "S4")
  expect_error(check_detset(base[c("detections", "deployments")], checks = "on_land",
                            water = "lake"), "sf polygon or a terra raster")
})

# A small de_field: two receivers that each hear only their own half of a
# strip of cells, so a co-detection between them is impossible.
toy_field <- function(base, stations) {
  testthat::skip_if_not_installed("terra")
  dep <- base$deployments
  g <- terra::rast(xmin = -78.21, xmax = -78.17, ymin = 44.49, ymax = 44.52, resolution = 0.001,
                   crs = "EPSG:4326", vals = 1)
  xy <- terra::xyFromCell(g, seq_len(terra::ncell(g)))
  pos <- dep[match(stations, dep$station_id), c("deploy_lon", "deploy_lat")]
  d2 <- sapply(seq_len(nrow(pos)), function(k) {
    (xy[, 1] - pos$deploy_lon[k])^2 + (xy[, 2] - pos$deploy_lat[k])^2
  })
  eta <- ifelse(d2 < 0.004^2, 2, -Inf)
  colnames(eta) <- stations
  structure(list(eta = eta, cells = seq_len(terra::ncell(g)), receivers = stations,
                 key = "station_id", grid = g), class = "de_field")
}

test_that("a de_field and a distance table serve as the field", {
  base <- apply_patch(planted_base(), planted_patch("codetection_out_of_range"))
  tabs <- base[c("detections", "deployments")]
  st <- sort(unique(base$deployments$station_id))
  f <- toy_field(base, st)
  res <- check_detset(tabs, checks = c("codetection_out_of_range", "reference_position"),
                      field = f, reference_tags = base$reference_tags)
  expect_true("codetection_out_of_range" %in% flagged_checks(res))
  bad <- res$rows$codetection_out_of_range
  expect_true(all(c("S1", "S8") %in% c(bad$key_a, bad$key_b)))
  cells <- which(is.finite(f$eta), arr.ind = TRUE)
  dist <- data.frame(cell = f$cells[cells[, 1]], station_id = st[cells[, 2]], distance_m = 100)
  attr(dist, "key") <- "station_id"
  attr(dist, "grid") <- terra::wrap(f$grid)
  res2 <- check_detset(tabs, checks = "codetection_out_of_range",
                       field = list(distances = dist, range_m = 500))
  expect_equal(res2$summary$flagged, res$summary$flagged[1])
  expect_error(check_detset(tabs, checks = "codetection_out_of_range", field = "far"),
               "range in metres")
})

test_that("bad arguments stop with a named reason", {
  base <- planted_base()
  expect_error(check_detset(base[c("detections", "deployments")], checks = "nope"),
               "Unknown check `nope`")
  expect_error(check_detset(base$detections), "detset or a list")
  tabs <- base[c("detections", "deployments")]
  tabs$deployments$deploy_lat <- NULL
  expect_error(check_detset(tabs), "no column `deploy_lat`")
  expect_error(all_checks("Nope"), "Unknown group")
  expect_equal(length(all_checks("Time")), 4L)
  expect_named(attr(all_checks(), "table"), c("check", "group", "unit", "description"))
  expect_error(check_detset(base[c("detections", "deployments")], checks = "transmitter_no_animal",
                            reference_tags = data.frame(code = 1)), "transmitter")
})

test_that("non-UTC time zones are converted and named", {
  base <- planted_base()
  tabs <- base[c("detections", "deployments")]
  tabs$detections$datetime <- lubridate::with_tz(tabs$detections$datetime, "America/Toronto")
  res <- check_detset(tabs, checks = c("non_utc", "outside_deployment"))
  expect_equal(res$summary$flagged, c(0L, 0L))
  expect_match(res$summary$note[1], "America/Toronto")
})

test_that("the checks run on the package examples", {
  ex <- hard_cases()
  res <- check_detset(ex[c("detections", "deployments", "animals")])
  expect_equal(nrow(res$summary), length(all_checks()))
  fx <- readRDS(fixture("positionrtools_example.rds"))
  res2 <- check_detset(fx[c("detections", "deployments", "animals")])
  expect_equal(nrow(res2$summary), length(all_checks()))
})

test_that("a deployment typed a year late is deploy_year_off", {
  base <- planted_base()
  dep <- base$deployments
  i <- which(dep$station_id == "S6")
  dep$deploy_datetime[i] <- dep$deploy_datetime[i] + 366 * 86400
  dep$recover_datetime[i] <- dep$recover_datetime[i] + 366 * 86400
  dep$download_datetime[i] <- dep$download_datetime[i] + 366 * 86400
  tabs <- list(detections = base$detections, deployments = dep)
  res <- check_detset(tabs, checks = c("deploy_year_off", "recover_before_deploy",
                                       "outside_deployment", "receiver_silent"))
  expect_equal(flagged_checks(res), "deploy_year_off")
  expect_match(res$rows$deploy_year_off$problem, "both a year late")
  expect_match(res$summary$note[res$summary$check == "outside_deployment"], "^[1-9]")
})

test_that("a recovery a year after the receiver's last detection is deploy_year_off", {
  base <- planted_base()
  dep <- base$deployments
  i <- which(dep$station_id == "S7")
  dep$recover_datetime[i] <- dep$recover_datetime[i] + 366 * 86400
  dep$download_datetime[i] <- dep$recover_datetime[i] + 7200
  later <- data.frame(datetime = as.POSIXct("2024-09-20 10:00:00", tz = "UTC") + 120 * 0:9,
                      receiver_serial = "480008", transmitter = "A69-1604-1001",
                      animal_id = "A1")
  tabs <- list(detections = rbind(base$detections, later), deployments = dep)
  res <- check_detset(tabs, checks = c("deploy_year_off", "receiver_silent"))
  expect_equal(flagged_checks(res), "deploy_year_off")
  expect_match(res$rows$deploy_year_off$problem, "ends a year after its last detection")
  expect_gt(res$rows$deploy_year_off$days, 365)
})

test_that("a code heard years after release elsewhere is flagged", {
  ex <- hard_cases()
  late <- ex$detections[ex$detections$animal_id %in% "EX-01", ][1:5, ]
  late$datetime <- late$datetime + 2 * 365 * 86400
  late$receiver_serial <- "VR2W-900001"
  late$station_id <- "H01"
  far <- ex$deployments[1, ]
  far$receiver_serial <- "VR2W-900001"
  far$station_id <- "H01"
  far$deploy_datetime <- as.POSIXct("2025-01-01", tz = "UTC")
  far$recover_datetime <- as.POSIXct("2025-12-31", tz = "UTC")
  far$deploy_lat <- 44.0
  far$deploy_lon <- -82.0
  ani <- ex$animals
  ani$tag_life_days <- NULL
  tabs <- list(detections = rbind(ex$detections, late), deployments = rbind(ex$deployments, far),
               animals = ani)
  checks <- c("after_tag_life", "long_after_release", "reappeared_elsewhere")
  res <- check_detset(tabs, checks = checks)
  s <- res$summary
  expect_equal(s$status[s$check == "after_tag_life"], "not run")
  expect_equal(s$status[s$check == "long_after_release"], "not run")
  expect_match(s$note[s$check == "long_after_release"], "max_liberty_years")
  expect_equal(s$flagged[s$check == "reappeared_elsewhere"], 1L)
  r <- res$rows$reappeared_elsewhere
  expect_equal(r$transmitter, "A69-1604-5001")
  expect_equal(r$stations_after, "H01")
  expect_equal(r$detections_after, 5L)
  expect_gt(r$gap_days, 365)
  res2 <- check_detset(tabs, checks = checks, settings = check_settings(max_liberty_years = 1))
  s2 <- res2$summary
  expect_equal(s2$flagged[s2$check == "long_after_release"], 5L)
  expect_equal(s2$of[s2$check == "long_after_release"],
               sum(!is.na(tabs$detections$animal_id) | tabs$detections$transmitter %in%
                     ani$transmitter[!duplicated(ani$transmitter)]))
  expect_error(check_settings(max_liberty_years = -1), "one positive number")
  # The bundled example has no such transmitter.
  clean <- check_detset(list(detections = ex$detections, deployments = ex$deployments,
                             animals = ex$animals), checks = checks[-1],
                        settings = check_settings(max_liberty_years = 3))$summary
  expect_equal(clean$status, c("pass", "pass"))
  # filter_tag_life() names its remedy when it cannot run.
  x <- detset(ex$detections, ex$deployments, ani)
  expect_message(filter_tag_life(x), "give `default_days`")
})

# ---- Edge cases of real exports ----
test_that("a field over some stations judges only them", {
  base <- apply_patch(planted_base(), planted_patch("codetection_out_of_range"))
  tabs <- base[c("detections", "deployments")]
  st <- sort(unique(base$deployments$station_id))
  f <- toy_field(base, setdiff(st, "S8"))
  res <- check_detset(tabs, checks = "codetection_out_of_range", field = f)
  bad <- res$rows$codetection_out_of_range
  expect_false(any(c(bad$key_a, bad$key_b) %in% "S8"))
  expect_match(res$summary$note, "[1-9][0-9]* not tested, at a receiver the field does not hold")
})

test_that("the depth check reads not run when no deployment has a depth", {
  skip_if_not_installed("terra")
  ex <- hard_cases()
  dep <- ex$deployments
  dep$deploy_z <- NA_real_
  x <- detset(ex$detections, dep, ex$animals)
  r <- terra::rast(xmin = -80, xmax = -77, ymin = 43, ymax = 46, resolution = 0.01,
                   crs = "EPSG:4326", vals = 5)
  s <- check_detset(x, checks = "depth_below_bottom", water = r)$checks$summary
  expect_identical(s$status, "not run")
})

test_that("animal_never_heard flags an animal removed from the detections", {
  base <- planted_base()
  gone <- base$animals$transmitter[2]
  base$detections <- base$detections[base$detections$transmitter != gone, ]
  res <- check_detset(base[c("detections", "deployments", "animals")],
                      checks = "animal_never_heard")
  expect_equal(res$summary$status, "flag")
  expect_equal(res$summary$flagged, 1L)
  expect_equal(res$summary$of, length(unique(base$animals$animal_id)))
  expect_identical(res$rows$animal_never_heard$animal_id, base$animals$animal_id[2])
})

test_that("transmitter_no_animal lists unknown codes most heard first", {
  base <- planted_base()
  extra <- base$detections[rep(1, 7), ]
  extra$transmitter <- rep(c("A69-1604-7001", "A69-1604-7002"), c(2, 5))
  extra$animal_id <- NA_character_
  extra$datetime <- extra$datetime + seq_len(7) * 200
  base$detections <- rbind(base$detections, extra)
  res <- check_detset(base[c("detections", "deployments", "animals")],
                      checks = "transmitter_no_animal",
                      reference_tags = base$reference_tags)
  rows <- res$rows$transmitter_no_animal
  expect_identical(rows$transmitter, c("A69-1604-7002", "A69-1604-7001"))
  expect_identical(rows$detections, c(5L, 2L))
})
