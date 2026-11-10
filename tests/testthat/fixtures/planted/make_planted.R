# ---- Planted-error fixtures for check_detset() ----
# Run from the package root: Rscript tests/testthat/fixtures/planted/make_planted.R
# Writes base.rds, a clean synthetic array that passes every check, and one
# patch per check (patch_<check>.rds) that plants exactly that error. The
# tests apply each patch to the base and expect check_detset() to flag that
# check and no other. Seed 20261030; no real data.

out_dir <- file.path("tests", "testthat", "fixtures", "planted")
set.seed(20261030)
utc <- function(s) {
  if (is.numeric(s)) return(as.POSIXct(s, origin = "1970-01-01", tz = "UTC"))
  as.POSIXct(s, tz = "UTC")
}

# ---- Array: eight stations on a 2 x 4 grid, 600 m apart ----
lat0 <- 44.50
lon0 <- -78.20
m_lat <- 1 / 111195
m_lon <- 1 / (111195 * cos(lat0 * pi / 180))
grid <- expand.grid(col = 0:3, row = 0:1)
stations <- data.frame(station_id = paste0("S", 1:8),
                       lat = lat0 + grid$row * 600 * m_lat,
                       lon = lon0 + grid$col * 600 * m_lon, stringsAsFactors = FALSE)
start <- utc("2023-06-01 12:00:00")
end <- utc("2023-09-01 12:00:00")
swap <- utc("2023-07-20 12:00:00")
dep <- data.frame(
  receiver_serial = c("480001", "480011", "480002", "480012", sprintf("4800%02d", 3:8)),
  station_id = c("S1", "S1", "S2", "S2", paste0("S", 3:8)),
  deploy_datetime = c(start, swap, start, swap, rep(start, 6)),
  recover_datetime = c(swap, end, swap, end, rep(end, 5), NA),
  stringsAsFactors = FALSE
)
# c() of date-times with an NA drops the time zone in R 4.2; set it again.
attr(dep$recover_datetime, "tzone") <- "UTC"
dep$deploy_lat <- stations$lat[match(dep$station_id, stations$station_id)]
dep$deploy_lon <- stations$lon[match(dep$station_id, stations$station_id)]
dep$deploy_z <- 4
dep$download_datetime <- dep$recover_datetime + 7200

# The receiver listening at a station at a time.
rx_at <- function(st, t) {
  d <- dep[dep$station_id == st & dep$deploy_datetime <= t &
             (is.na(dep$recover_datetime) | t < dep$recover_datetime), ]
  if (nrow(d)) d$receiver_serial[nrow(d)] else NA_character_
}
dist_m <- function(a, b) {
  i <- match(a, stations$station_id)
  j <- match(b, stations$station_id)
  sqrt(((stations$lat[i] - stations$lat[j]) / m_lat)^2 +
         ((stations$lon[i] - stations$lon[j]) / m_lon)^2)
}

# ---- Transmissions and detections ----
# A transmission at a station is heard there with p_here and at each
# station within 650 m (the grid neighbours) with p_near, 0 to 1.5 s later.
hear <- function(tx, st, times, p_here = 0.6, p_near = 0.2) {
  near <- stations$station_id[dist_m(st, stations$station_id) < 650 &
                                stations$station_id != st]
  rows <- list()
  for (t in times) {
    t <- utc(t)
    if (stats::runif(1) < p_here) rows[[length(rows) + 1]] <- c(tx, rx_at(st, t), t)
    for (nb in near) {
      if (stats::runif(1) < p_near) {
        rows[[length(rows) + 1]] <- c(tx, rx_at(nb, t), t + stats::runif(1, 0, 1.5))
      }
    }
  }
  if (!length(rows)) return(NULL)
  m <- do.call(rbind, rows)
  data.frame(transmitter = m[, 1], receiver_serial = m[, 2],
             datetime = utc(as.numeric(m[, 3])), stringsAsFactors = FALSE)
}

animals <- data.frame(
  animal_id = paste0("A", 1:6), transmitter = sprintf("A69-1604-%d", 1001:1006),
  species = rep(c("Walleye", "Smallmouth Bass"), each = 3),
  release_datetime = utc("2023-06-05 12:00:00"), tag_life_days = 400,
  length = c(452, 471, 498, 380, 395, 362), stringsAsFactors = FALSE
)
bout <- function(tx, st, t0, minutes) {
  hear(tx, st, t0 + seq(0, minutes * 60, by = 120) + stats::runif(minutes / 2 + 1, -10, 10))
}
parts <- list()
days <- seq(utc("2023-06-06 00:00:00"), utc("2023-08-31 00:00:00"), by = "day")
for (k in seq_along(days)) {
  for (a in which(seq_len(6) %% 2 == k %% 2)) {
    st <- stations$station_id[(k + a) %% 8 + 1]
    parts[[length(parts) + 1]] <- bout(animals$transmitter[a], st,
                                       days[k] + 3600 * sample(0:22, 1), 60)
  }
}
# A last tour so that every station detects in the hours before recovery;
# no animal is at two stations at once.
for (s in seq_len(8)) {
  t0 <- utc("2023-09-01 06:00:00") + if (s > 6) 3 * 3600 else 0
  parts[[length(parts) + 1]] <- bout(animals$transmitter[(s - 1) %% 6 + 1],
                                     stations$station_id[s], t0, 150)
}
ref_pos <- data.frame(transmitter = "A69-1604-9001", deploy_lat = stations$lat[1] + 50 * m_lat,
                      deploy_lon = stations$lon[1], stringsAsFactors = FALSE)
ref_times <- seq(utc("2023-06-01 13:00:00"), utc("2023-09-01 11:00:00"), by = 3600)
ref <- hear(ref_pos$transmitter, "S1", ref_times, p_here = 0.8, p_near = 0)
ref2 <- hear(ref_pos$transmitter, "S2", ref_times, p_here = 0.3, p_near = 0)
det <- do.call(rbind, c(parts, list(ref, ref2)))
det$animal_id <- animals$animal_id[match(det$transmitter, animals$transmitter)]
det <- det[order(det$receiver_serial, det$datetime), c("datetime", "receiver_serial",
                                                       "transmitter", "animal_id")]
rownames(det) <- NULL

# The water body: a raster of depth (10 m) over the array plus 400 m, land
# around it to 2 km. Stored as numbers; the tests build the terra raster.
lake <- list(lon = lon0 + c(-400, 2200) * m_lon, lat = lat0 + c(-400, 1000) * m_lat,
             extent = c(lon0 - 2000 * m_lon, lon0 + 3800 * m_lon,
                        lat0 - 2000 * m_lat, lat0 + 2600 * m_lat),
             res = 50 * m_lat, depth = 10)
base <- list(detections = det, deployments = dep, animals = animals, reference_tags = ref_pos,
             lake = lake, field_m = 800)
saveRDS(base, file.path(out_dir, "base.rds"), compress = "xz")
cat("base:", nrow(det), "detections,", nrow(dep), "deployments,", nrow(animals), "animals\n")

# ---- Patches ----
# A patch is a list of edits. set: one column of some rows. add: rows bound
# at the end. drop: rows removed. dup: rows copied in place after their
# originals.
edit <- function(op, table, rows = NULL, col = NULL, value = NULL, data = NULL) {
  list(op = op, table = table, rows = rows, col = col, value = value, data = data)
}
dep_row <- function(st, serial = NULL) {
  i <- which(dep$station_id == st)
  if (!is.null(serial)) i <- i[dep$receiver_serial[i] == serial]
  i
}
det_add <- function(tx, serial, times) {
  data.frame(datetime = times, receiver_serial = serial, transmitter = tx,
             animal_id = animals$animal_id[match(tx, animals$transmitter)],
             stringsAsFactors = FALSE)
}
s6 <- which(det$receiver_serial == "480006")
s6_jump <- s6[which(diff(as.numeric(det$datetime[s6])) < 1200 &
                      det$datetime[s6][-1] > utc("2023-07-15"))[1] + 1]
s6_after <- s6[s6 >= s6_jump]
moved <- det[det$receiver_serial == "480008" & det$datetime >= utc("2023-08-10") &
               det$datetime < end, ]
moved$receiver_serial <- "480011"
moved$datetime <- moved$datetime + 0.3
codet_t <- utc("2023-06-10 10:00:00") + 86400 * 3 * (0:9)
patches <- list(
  coordinate_units = list(edit("set", "deployments", dep_row("S6"), "deploy_lat", 4928000),
                          edit("set", "deployments", dep_row("S6"), "deploy_lon", 703000)),
  swapped_lat_lon = list(
    edit("set", "deployments", dep_row("S7"), "deploy_lat", dep$deploy_lon[dep_row("S7")]),
    edit("set", "deployments", dep_row("S7"), "deploy_lon", dep$deploy_lat[dep_row("S7")])
  ),
  wrong_sign = list(edit("set", "deployments", dep_row("S3"), "deploy_lon",
                         -dep$deploy_lon[dep_row("S3")])),
  outside_extent = list(edit("set", "deployments", dep_row("S5"), "deploy_lat",
                             dep$deploy_lat[dep_row("S5")] + 0.5)),
  on_land = list(edit("set", "deployments", dep_row("S4"), "deploy_lat", lat0 - 1000 * m_lat)),
  station_coordinates = list(edit("set", "deployments", dep_row("S2", "480012"), "deploy_lon",
                                  dep$deploy_lon[dep_row("S2", "480012")] + 300 * m_lon)),
  station_reused = list(edit("set", "deployments", dep_row("S2", "480012"), "deploy_lon",
                             dep$deploy_lon[dep_row("S2", "480012")] + 1500 * m_lon)),
  overlapping_deployments = list(edit("add", "deployments", data = transform(
    dep[dep_row("S3"), ], receiver_serial = "480004", deploy_datetime = utc("2023-08-15 12:00:00")
  ))),
  recover_before_deploy = list(edit("set", "deployments", dep_row("S6"), "recover_datetime",
                                    utc("2023-05-30 12:00:00"))),
  deploy_year_off = list(edit("set", "deployments", dep_row("S7"), "deploy_datetime",
                              utc("2024-06-01 12:00:00"))),
  recover_after_download = list(edit("set", "deployments", dep_row("S3"), "download_datetime",
                                     utc("2023-08-15 12:00:00"))),
  # A download an hour before the recovery, within download_tol_s, with the
  # receiver's last detections after it.
  detections_after_download = list(edit("set", "deployments", dep_row("S3"),
                                        "download_datetime", utc("2023-09-01 11:00:00"))),
  depth_below_bottom = list(edit("set", "deployments", dep_row("S5"), "deploy_z", 40)),
  serial_not_deployed = list(edit("add", "detections", data = det_add(
    "A69-1604-1001", "999999", utc("2023-07-10 00:00:00") + 3600 * (0:29)
  ))),
  receiver_silent = list(edit("drop", "detections", which(det$receiver_serial == "480005"))),
  moved_mooring = list(
    edit("drop", "detections", which(det$receiver_serial == "480011" &
                                       det$datetime >= utc("2023-08-10"))),
    edit("add", "detections", data = moved)
  ),
  transmitter_no_animal = list(edit("add", "detections", data = det_add(
    "A69-1604-7777", "480003", utc("2023-07-05 10:00:00") + 120 * (0:4)
  ))),
  # An animal on record whose code was never heard.
  animal_never_heard = list(edit("add", "animals", data = transform(
    animals[1, ], animal_id = "A7", transmitter = "A69-1604-1007", length = 440
  ))),
  before_release = list(edit("add", "detections", data = det_add(
    "A69-1604-1002", "480002", utc("2023-06-03 10:00:00") + 120 * (0:2)
  ))),
  after_tag_life = list(edit("set", "animals", 3, "tag_life_days", 30)),
  # Released two years before the array, with no tag life, as a GLATOS
  # export gives it; the tests run with max_liberty_years = 1.
  long_after_release = list(edit("set", "animals", 4, "release_datetime",
                                 utc("2021-06-05 12:00:00")),
                            edit("set", "animals", 4, "tag_life_days", NA_real_)),
  # A second code of animal A6, heard at S1 in June and then only at S8 in
  # August; the tests run with reappear_gap_days = 20.
  reappeared_elsewhere = list(
    edit("add", "animals", data = transform(animals[6, ], transmitter = "A69-1604-2006")),
    edit("add", "detections", data = rbind(
      det_add("A69-1604-2006", "480001", utc("2023-06-10 10:00:00") + 120 * (0:2)),
      det_add("A69-1604-2006", "480008", utc("2023-08-20 10:00:00") + 120 * (0:2))
    ))
  ),
  animal_conflict = list(edit("add", "animals", data = transform(
    animals[4, ], transmitter = "A69-1604-1014", species = "Northern Pike"
  ))),
  duplicate_animal_id = list(edit("add", "animals", data = transform(
    animals[5, ], transmitter = "A69-1604-1015", release_datetime = utc("2023-07-10 12:00:00")
  ))),
  non_utc = lapply(c("deploy_datetime", "recover_datetime", "download_datetime"), function(cl) {
    edit("set", "deployments", seq_len(nrow(dep)), cl, dep[[cl]] - 4 * 3600)
  }),
  dst_jump = list(edit("set", "detections", s6_after, "datetime", det$datetime[s6_after] - 3600)),
  outside_deployment = list(edit("add", "detections", data = det_add(
    "A69-1604-1001", "480006", utc("2023-09-20 10:00:00") + 60 * (0:9)
  ))),
  duplicate_rows = list(edit("dup", "detections", sort(sample(nrow(det), 25)))),
  codetection_out_of_range = list(edit("add", "detections", data = rbind(
    det_add("A69-1604-1004", "480001", codet_t), det_add("A69-1604-1004", "480008", codet_t + 1)
  ))),
  reference_position = list(edit("set", "reference_tags", 1, "deploy_lat",
                                 ref_pos$deploy_lat + 5000 * m_lat))
)
for (nm in names(patches)) {
  saveRDS(patches[[nm]], file.path(out_dir, paste0("patch_", nm, ".rds")), compress = "xz")
}
cat(length(patches), "patches written\n")
