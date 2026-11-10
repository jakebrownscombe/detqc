# Shared fixtures for the mortality rule-stream tests.

mortality_example <- function() {
  ex <- hard_mortality()
  detset(ex$detections, ex$deployments, ex$animals)
}

# hard_mortality() holds one uncalibrated accelerometer on purpose, so
# detect_mortality() warns on it; the warning is tested once, and muffled
# where the cached bands are built.
quiet_raw <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("raw readings only", conditionMessage(w))) invokeRestart("muffleWarning")
  })
}

# The raw-accelerometer warning is given once per session for one set of
# transmitters. This expects it afresh, and then restores what was already
# warned, so later tests do not see it again.
expect_warning_fresh <- function(expr, regexp) {
  seen <- ls(warned)
  forget_warnings()
  on.exit(for (k in seen) assign(k, TRUE, envir = warned))
  testthat::expect_warning(expr, regexp)
}

# The bands of the synthetic hard cases at the defaults, computed once per
# test run; each case test reads its row.
example_bands <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- quiet_raw(detect_mortality(mortality_example()))
    cache
  }
})

case_row <- function(case) {
  ex <- hard_mortality()
  id <- ex$cases$animal_id[ex$cases$case == case]
  mb <- example_bands()
  b <- mb$bands[mb$bands$animal_id == id, ]
  v <- mb$votes[mb$votes$animal_id == id, ]
  list(id = id, b = b, votes = stats::setNames(v$vote, v$rule),
       detail = stats::setNames(v$detail, v$rule),
       truth = ex$cases[ex$cases$animal_id == id, ])
}

expect_case <- function(case, band, rule) {
  r <- case_row(case)
  testthat::expect_equal(r$b$band, band, label = paste(case, "band"))
  testthat::expect_equal(r$b$deciding_rule, rule, label = paste(case, "deciding rule"))
  testthat::expect_equal(r$truth$expected_band, band, label = paste(case, "fixture's band"))
  invisible(r)
}

death_gap <- function(r) as.integer(as.Date(r$b$death_date) - r$truth$true_death_date)

# ---- Planted arrays for the hard mortality cases ----
# A 3 x 3 grid of stations 2 km apart (G1 at the origin, G9 at 4 km, 4 km)
# and a pair F1, F2 2.5 km apart, 10 km away, with a fixed walk for each
# planted animal.
plant_t0 <- as.POSIXct("2024-05-01 12:00:00", tz = "UTC")
plant_hz <- as.POSIXct("2024-12-31 23:00:00", tz = "UTC")
plant_slope <- 0.013588

plant_stations <- function() {
  data.frame(station_id = c(sprintf("G%d", 1:9), "F1", "F2"),
             x = c(rep(c(0, 2, 4), 3), 10, 12.5), y = c(rep(c(0, 2, 4), each = 3), 10, 10),
             stringsAsFactors = FALSE)
}

# `ends` sets the recovery time of named stations; `extra` adds deployments.
plant_deployments <- function(ends = list(), extra = NULL) {
  st <- plant_stations()
  km_lat <- 1 / 110.574
  km_lon <- 1 / (111.320 * cos(44.5 * pi / 180))
  dep <- data.frame(receiver_serial = sprintf("VR2W-%06d", 100000 + seq_len(nrow(st))),
                    station_id = st$station_id, deploy_lat = 44.5 + st$y * km_lat,
                    deploy_lon = -78.2 + st$x * km_lon, deploy_datetime = plant_t0 - 3 * 86400,
                    recover_datetime = plant_hz + 5 * 86400, stringsAsFactors = FALSE)
  for (nm in names(ends)) {
    dep$recover_datetime[dep$station_id == nm] <- as.POSIXct(ends[[nm]], tz = "UTC")
  }
  if (!is.null(extra)) {
    extra$deploy_lat <- dep$deploy_lat[match(extra$station_id, dep$station_id)]
    extra$deploy_lon <- dep$deploy_lon[match(extra$station_id, dep$station_id)]
    dep <- rbind(dep, extra[, names(dep)])
  }
  dep
}

plant_days <- function(a, b) seq(as.Date(a), as.Date(b), by = "day")

# One day per row, visiting `stations` in turn, never one station twice running.
plant_walk <- function(a, b, tx, stations) {
  d <- plant_days(a, b)
  data.frame(date = d, station_id = stations[(seq_along(d) - 1L) %% length(stations) + 1L],
             transmitter = tx, stringsAsFactors = FALSE)
}

plant_stay <- function(a, b, stations, tx) {
  d <- plant_days(a, b)
  data.frame(date = rep(d, each = length(stations)), station_id = rep(stations, length(d)),
             transmitter = tx, stringsAsFactors = FALSE)
}

# A burst of `n` detections at 10:00 UTC per plan row, heard by the
# deployment in the water at the time; rows with none are dropped.
plant_detections <- function(plan, dep, animal_id, n = 4L) {
  plan <- plan[rep(seq_len(nrow(plan)), each = n), ]
  off <- rep(c(0, 90, 200, 330)[seq_len(n)], nrow(plan) / n)
  dt <- as.POSIXct(as.character(plan$date), tz = "UTC") + 36000 + off
  k <- vapply(seq_along(dt), function(i) {
    hit <- which(dep$station_id == plan$station_id[i] & dep$deploy_datetime <= dt[i] &
                   dep$recover_datetime > dt[i])
    if (length(hit)) hit[1] else NA_integer_
  }, 1L)
  out <- data.frame(datetime = dt, receiver_serial = dep$receiver_serial[k],
                    transmitter = plan$transmitter, animal_id = animal_id,
                    station_id = plan$station_id, sensor_raw = NA_real_, stringsAsFactors = FALSE)
  out[!is.na(k), ]
}

plant_animals <- function(ids, tx, accel = rep(FALSE, length(ids)), slope = plant_slope) {
  data.frame(animal_id = ids, transmitter = tx, release_datetime = plant_t0, tag_life_days = 900,
             sensor_type = ifelse(accel, "acceleration", NA_character_),
             sensor_unit = ifelse(accel, "m/s2", NA_character_),
             sensor_slope = ifelse(accel, slope, NA_real_),
             sensor_intercept = ifelse(accel, 0, NA_real_), stringsAsFactors = FALSE)
}

plant_detset <- function(dets, dep, ani) {
  d <- do.call(rbind, dets)
  suppressMessages(suppressWarnings(detset(d, dep, ani)))
}

plant_band <- function(mb, id) {
  b <- mb$bands[mb$bands$animal_id == id, ]
  v <- mb$votes[mb$votes$animal_id == id, ]
  list(band = b$band, rule = b$deciding_rule, b = b, votes = stats::setNames(v$vote, v$rule),
       detail = stats::setNames(v$detail, v$rule))
}

# A walker heard to the horizon, so every planted set ends on the same day.
plant_other <- function(dep) {
  plant_detections(plant_walk("2024-05-02", "2024-12-31", "OT", sprintf("G%d", 2:9)), dep,
                   "OTHER_ALIVE")
}
