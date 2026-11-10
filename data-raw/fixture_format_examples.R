# ---- Build the format fixtures of the reader tests ----
# Run from the package root: Rscript data-raw/fixture_format_examples.R
# The twenty hard cases below are built at test time by helper-hardcases.R
# in the tests; the dataset detqc_example holds the Stoney Lake study
# (data-raw/detqc_example.R). This script writes the VUE, GLATOS and OTN
# files of the hard cases, which the reader tests read from the fixtures
# folder of the tests.
# Twenty synthetic animals, one per hard case of the mortality triage.
# Expected bands follow the rule streams: the six live animals heard
# to the data horizon, or near tag life or within 2 km, go to review. No
# real data is included with the package.

library(data.table)
set.seed(20261029)

# ---- Study frame ----
t_start <- as.POSIXct("2023-05-01 00:00:00", tz = "UTC")
t_end <- as.POSIXct("2023-10-31 23:59:59", tz = "UTC")
days <- seq(as.Date(t_start), as.Date(t_end), by = "day")
lat0 <- 44.55
lon0 <- -78.10
km_lat <- 1 / 110.574
km_lon <- 1 / (111.320 * cos(lat0 * pi / 180))

# ---- Stations: a 4 by 5 grid at 1.5 km, and two close pairs ----
grid <- CJ(row = 1:4, col = 1:5)
grid[, station_id := sprintf("G%02d", .I)]
grid[, `:=`(deploy_lat = lat0 + (row - 2.5) * 1.5 * km_lat,
            deploy_lon = lon0 + (col - 3) * 1.5 * km_lon)]
pairs <- data.table(
  station_id = c("P1A", "P1B", "P2A", "P2B"),
  deploy_lat = lat0 + c(-4.5, -4.5, 4.5, 4.5) * km_lat,
  deploy_lon = lon0 + c(0, 0.4, 0, 0.9) * km_lon
)
stations <- rbind(grid[, .(station_id, deploy_lat, deploy_lon)], pairs)
cat("stations:", nrow(stations), "\n")

# ---- Deployments: one receiver per station; G10 swaps receivers; G20 still deployed ----
swap_time <- as.POSIXct("2023-08-01 12:00:00", tz = "UTC")
dep <- stations[, .(
  receiver_serial = sprintf("VR2Tx-48%04d", .I), station_id, deploy_lat, deploy_lon,
  deploy_datetime = as.POSIXct("2023-04-25 12:00:00", tz = "UTC"),
  recover_datetime = as.POSIXct("2023-11-05 12:00:00", tz = "UTC"), deploy_z = 4
)]
dep[station_id == "G10", recover_datetime := swap_time]
dep <- rbind(dep, dep[station_id == "G10"][, `:=`(
  receiver_serial = "VR2Tx-489999", deploy_datetime = swap_time,
  recover_datetime = as.POSIXct("2023-11-05 12:00:00", tz = "UTC")
)])
dep[station_id == "G20", recover_datetime := as.POSIXct(NA, tz = "UTC")]
setorder(dep, station_id, deploy_datetime)
cat("deployments:", nrow(dep), "\n")

# ---- Movement helpers ----
neighbours <- function(s) {
  g <- grid[station_id == s]
  if (!nrow(g)) return(s)
  grid[abs(row - g$row) <= 1 & abs(col - g$col) <= 1, station_id]
}
walk <- function(d0, d1, start = "G08", p_detect = 0.85) {
  dd <- days[days >= as.Date(d0) & days <= as.Date(d1)]
  s <- start
  out <- list()
  for (k in seq_along(dd)) {
    s <- sample(neighbours(s), 1)
    if (runif(1) < p_detect) out[[length(out) + 1]] <- data.table(date = dd[k], station_id = s)
  }
  rbindlist(out)
}
stay <- function(d0, d1, st, p_detect = 1) {
  dd <- days[days >= as.Date(d0) & days <= as.Date(d1)]
  dd <- dd[runif(length(dd)) < p_detect]
  data.table(date = dd, station_id = rep_len(st, length(dd)))
}
alternate <- function(d0, d1, st) {
  dd <- days[days >= as.Date(d0) & days <= as.Date(d1)]
  data.table(date = dd, station_id = rep_len(st, length(dd)))
}
to_detections <- function(plan, lambda) {
  plan[, n := pmax(1L, rpois(.N, lambda))]
  # as.POSIXct() drops the time zone of a Date in R 4.2, so it is set here.
  out <- plan[rep(seq_len(.N), n)]
  out[, datetime := as.POSIXct(as.numeric(date) * 86400 + round(runif(.N, 0, 86399)),
                               origin = "1970-01-01", tz = "UTC")]
  out[, n := NULL]
  out[]
}

# ---- The twenty cases ----
cases <- list(
  list("dead_one_station", "dead", "2023-07-15", function() {
    rbind(walk("2023-05-03", "2023-07-14"), stay("2023-07-15", "2023-10-31", "G07"))
  }),
  list("dead_between_pair_400m", "dead", "2023-08-10", function() {
    rbind(walk("2023-05-03", "2023-08-09", "G02"),
          alternate("2023-08-10", "2023-10-31", c("P1A", "P1B")))
  }),
  list("dead_accel_flatline", "dead", "2023-08-20", function() {
    rbind(walk("2023-05-03", "2023-08-19", "G12"), stay("2023-08-20", "2023-10-31", "G13"))
  }),
  list("dormant_then_active", "review", NA, function() {
    rbind(walk("2023-05-03", "2023-05-31", "G05"), stay("2023-06-01", "2023-07-31", "G04"),
          walk("2023-08-01", "2023-10-31", "G04"))
  }),
  list("resident_two_sites", "review", NA, function() {
    alternate("2023-05-03", "2023-10-31", c("G06", "G06", "G07"))
  }),
  list("resident_one_site_active", "review", NA, function() {
    stay("2023-05-03", "2023-10-31", "G16")
  }),
  list("wide_ranger_brief_end", "review", NA, function() {
    rbind(walk("2023-05-03", "2023-10-20", "G01", 0.9), stay("2023-10-25", "2023-10-27", "G19"))
  }),
  list("record_ends_at_tag_life", "review", NA, function() {
    walk("2023-05-18", "2023-09-15", "G09")
  }),
  list("dead_from_first_detection", "dead", "2023-05-20", function() {
    stay("2023-05-20", "2023-10-31", "G03")
  }),
  list("dead_across_receiver_swap", "dead", "2023-07-10", function() {
    rbind(walk("2023-05-03", "2023-07-09", "G14"), stay("2023-07-10", "2023-10-31", "G10"))
  }),
  list("stray_detection_in_run", "dead", "2023-07-01", function() {
    rbind(walk("2023-05-03", "2023-06-30", "G11"), stay("2023-07-01", "2023-07-31", "G17"),
          data.table(date = as.Date("2023-08-01"), station_id = "G19"),
          stay("2023-08-02", "2023-10-31", "G17"))
  }),
  list("moored_reference_tag", "dead", "2023-05-02", function() {
    stay("2023-05-02", "2023-10-31", "G18")
  }),
  list("accel_flat_from_release", "dead", "2023-06-05", function() {
    stay("2023-06-05", "2023-10-31", "G15")
  }),
  list("single_detection", "review", NA, function() {
    data.table(date = as.Date("2023-06-10"), station_id = "G08")
  }),
  list("dead_between_pair_900m", "dead", "2023-08-25", function() {
    rbind(walk("2023-05-03", "2023-08-24", "G20"),
          alternate("2023-08-25", "2023-10-31", c("P2A", "P2B")))
  }),
  list("two_transmitters_alive", "review", NA, function() {
    walk("2023-05-03", "2023-10-31", "G13")
  }),
  list("accel_raw_uncalibrated", "review", NA, function() {
    walk("2023-05-03", "2023-10-31", "G02")
  }),
  list("dead_depth_tag", "dead", "2023-09-01", function() {
    rbind(walk("2023-05-03", "2023-08-31", "G18"), stay("2023-09-01", "2023-10-31", "G19"))
  }),
  list("sparse_alive", "review", NA, function() {
    walk("2023-05-03", "2023-10-31", "G05", 0.045)
  }),
  list("silent_gap_then_stationary", "dead", "2023-09-15", function() {
    rbind(walk("2023-05-03", "2023-07-15", "G01"), stay("2023-09-15", "2023-10-31", "G02"))
  })
)
case_tab <- data.table(
  animal_id = sprintf("EX-%02d", seq_along(cases)),
  case = vapply(cases, `[[`, "", 1), expected_band = vapply(cases, `[[`, "", 2),
  true_death_date = as.Date(vapply(cases, function(z) as.character(z[[3]]), ""))
)
case_tab[case == "moored_reference_tag", animal_id := "REF-01"]

# ---- Transmitters and sensor calibration ----
accel_cases <- c("dead_accel_flatline", "resident_one_site_active", "accel_flat_from_release",
                 "two_transmitters_alive", "accel_raw_uncalibrated")
ani <- case_tab[, .(animal_id, transmitter = sprintf("A69-1604-%d", 5000 + .I),
                    case)]
ani[case %in% accel_cases, transmitter := sprintf("A69-9004-%d", 7000 + .I)]
extra <- ani[case %in% c("two_transmitters_alive", "dead_depth_tag")]
extra[, transmitter := sprintf("A69-9004-%d", 8000 + .I)]
ani <- rbind(ani, extra)
ani[, sensor_type := NA_character_]
ani[transmitter %like% "A69-9004-7", sensor_type := "acceleration"]
ani[transmitter %like% "A69-9004-8", sensor_type := "depth"]
ani[case == "dead_depth_tag" & !transmitter %like% "A69-9004-8", sensor_type := NA_character_]
ani[, `:=`(sensor_unit = fifelse(sensor_type == "acceleration", "m/s2", "m"),
           sensor_slope = fifelse(sensor_type == "acceleration", 0.013588, 0.1504),
           sensor_intercept = fifelse(sensor_type == "acceleration", 0, -0.6018))]
ani[is.na(sensor_type), `:=`(sensor_unit = NA, sensor_slope = NA, sensor_intercept = NA)]
# The uncalibrated accelerometer: its type is known, its slope is not.
ani[case == "accel_raw_uncalibrated", `:=`(sensor_slope = NA, sensor_intercept = NA)]

# ---- Detections ----
det <- rbindlist(lapply(seq_along(cases), function(k) {
  plan <- cases[[k]][[4]]()
  dead_from <- case_tab$true_death_date[k]
  plan[, dead := !is.na(dead_from) & date >= dead_from]
  if (case_tab$case[k] == "resident_one_site_active") plan[, dead := FALSE]
  d <- to_detections(plan, 14)
  d[, animal_id := case_tab$animal_id[k]]
  d
}))
# A two-transmitter tag alternates its transmitters: each detection takes one.
tx <- ani[, .(transmitter = list(transmitter)), by = animal_id]
det <- tx[det, on = "animal_id"]
det[, transmitter := vapply(transmitter, function(v) v[sample.int(length(v), 1)], "")]
det <- ani[, .(transmitter, sensor_type)][det, on = "transmitter"]
det <- unique(det, by = c("transmitter", "datetime"))
cat("detections before sensors:", nrow(det), "\n")

adc_accel <- function(value) pmin(255, pmax(0, round(value / 0.013588)))
det[, sensor_raw := NA_real_]
det[sensor_type == "acceleration" & !dead, sensor_raw := adc_accel(rgamma(.N, 2, 4))]
det[sensor_type == "acceleration" & dead, sensor_raw := adc_accel(abs(rnorm(.N, 0.02, 0.012)))]
det[sensor_type == "depth" & !dead, sensor_raw := round((runif(.N, 2, 9) + 0.6018) / 0.1504)]
det[sensor_type == "depth" & dead,
    sensor_raw := round((6.1 + rnorm(.N, 0, 0.05) + 0.6018) / 0.1504)]
# Shed at release: flat from its first day.
flat_id <- case_tab[case == "accel_flat_from_release", animal_id]
det[animal_id == flat_id & sensor_type == "acceleration",
    sensor_raw := adc_accel(abs(rnorm(.N, 0.02, 0.012)))]

# One detection lands on the swap instant and belongs to the new receiver.
swap_id <- case_tab[case == "dead_across_receiver_swap", animal_id]
det[animal_id == swap_id & station_id == "G10" & date == as.Date(swap_time),
    datetime := c(swap_time, datetime[-1])]

# ---- Receivers from the station and time ----
dep_dt <- copy(dep)
dep_dt[, end := fifelse(is.na(recover_datetime), as.POSIXct("2100-01-01", tz = "UTC"),
                        recover_datetime)]
det[, row := .I]
hit <- dep_dt[det, on = .(station_id, deploy_datetime <= datetime, end > datetime),
              .(row = i.row, receiver_serial = x.receiver_serial)]
det[hit, on = "row", receiver_serial := i.receiver_serial]
stopifnot(!anyNA(det$receiver_serial))
det[, sensor_value := NA_real_][, sensor_unit := NA_character_]
setorder(det, animal_id, datetime)
detections <- as.data.frame(det[, .(datetime, receiver_serial, transmitter, animal_id,
                                    station_id, sensor_raw, sensor_value, sensor_unit)])
deployments <- as.data.frame(dep[, .(receiver_serial, station_id, deploy_datetime,
                                     recover_datetime, deploy_lat, deploy_lon, deploy_z)])
animals <- as.data.frame(ani[, .(
  animal_id, transmitter, species = "Walleye",
  release_datetime = as.POSIXct("2023-05-01 15:00:00", tz = "UTC"),
  tag_model = fifelse(is.na(sensor_type), "V13-1x", "V13AP-1x"),
  tag_life_days = fifelse(case == "record_ends_at_tag_life", 137, 900),
  sensor_type, sensor_unit, sensor_slope, sensor_intercept
)])
cases_df <- as.data.frame(case_tab)
detqc_example <- list(detections = detections, deployments = deployments, animals = animals,
                      cases = cases_df)
cat("detections:", nrow(detections), " animals:", length(unique(detections$animal_id)),
    " transmitters:", length(unique(detections$transmitter)), "\n")
print(table(cases_df$expected_band))

# ---- Format examples for the reader examples ----
pick <- c("EX-01", "EX-03", "EX-16")
sub <- det[animal_id %in% pick]
sub <- sub[order(datetime)][seq(1, .N, by = 8)]
dep_pos <- dep[, .(receiver_serial, deploy_lat, deploy_lon)]
sub <- dep_pos[sub, on = "receiver_serial"]
cal <- ani[, .(transmitter, sensor_slope, sensor_intercept)]
sub <- cal[sub, on = "transmitter"]
vue <- sub[, .(`Date and Time (UTC)` = format(datetime, "%Y-%m-%d %H:%M:%S"),
               Receiver = receiver_serial, Transmitter = transmitter,
               `Transmitter Name` = "", `Transmitter Serial` = "",
               `Sensor Value` = sensor_raw,
               `Sensor Unit` = fifelse(is.na(sensor_raw), "", "ADC"),
               `Station Name` = station_id, Latitude = "+0", Longitude = "+0")]
fwrite(vue, "tests/testthat/fixtures/extdata/vue_example.csv", na = "")
gl <- sub[, .(animal_id, detection_timestamp_utc = format(datetime, "%Y-%m-%d %H:%M:%S"),
              glatos_array = "EXA", station_no = sub("^[A-Z]+", "", station_id),
              transmitter_codespace = sub("-[0-9]+$", "", transmitter),
              transmitter_id = sub("^.*-", "", transmitter), sensor_value = sensor_raw,
              sensor_unit = fifelse(is.na(sensor_raw), "", "ADC"), deploy_lat,
              deploy_long = deploy_lon, receiver_sn = sub("^.*-", "", receiver_serial),
              tag_model = "V13AP-1x", common_name_e = "walleye",
              release_latitude = lat0, release_longitude = lon0,
              utc_release_date_time = "2023-05-01 15:00:00", station = station_id)]
fwrite(gl, "tests/testthat/fixtures/extdata/glatos_detections_example.csv", na = "")
# glatos::read_glatos_receivers() knows a file by its full column set
# (version 1.1, 25 columns), so every column is written; those the example
# has no value for are blank. A69 codes are sent at 69 kHz.
rcv <- dep[, .(station = station_id, glatos_array = "EXA",
               station_no = sub("^[A-Z]+", "", station_id), consecutive_deploy_no = 1L,
               intend_lat = NA_real_, intend_long = NA_real_,
               deploy_lat, deploy_long = deploy_lon, recover_lat = NA_real_,
               recover_long = NA_real_,
               deploy_date_time = format(deploy_datetime, "%Y-%m-%d %H:%M:%S"),
               recover_date_time = format(recover_datetime, "%Y-%m-%d %H:%M:%S"),
               bottom_depth = NA_real_, riser_length = NA_real_,
               instrument_depth = deploy_z, ins_model_no = "VR2Tx", glatos_ins_frequency = 69L,
               ins_serial_no = sub("^.*-", "", receiver_serial), code_map = "",
               code_map_comment = "", deployed_by = "", comments = "", glatos_seasonal = "",
               glatos_project = "", glatos_vps = "")]
fwrite(rcv, "tests/testthat/fixtures/extdata/glatos_receivers_example.csv", na = "")
otn <- sub[, .(catalogNumber = animal_id, commonName = "walleye", receiver = receiver_serial,
               station = station_id, tagName = transmitter,
               codeSpace = sub("-[0-9]+$", "", transmitter),
               sensorRaw = sensor_raw, sensorType = "", sensorValue = "", sensorUnit = "",
               dateCollectedUTC = format(datetime, "%Y-%m-%d %H:%M:%S"),
               decimalLongitude = deploy_lon, decimalLatitude = deploy_lat)]
fwrite(otn, "tests/testthat/fixtures/extdata/otn_detections_example.csv", na = "")
for (f in list.files("tests/testthat/fixtures/extdata", full.names = TRUE)) {
  cat(f, file.size(f), "bytes\n")
}
