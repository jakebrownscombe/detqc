# ---- Build the Stoney Lake example files ----
# Run from the package root: Rscript data-raw/stoney_example.R
# Writes the files the vignettes read, all in inst/extdata/stoney_*: a GLATOS
# detection export, a GLATOS receiver locations file, a tag specification
# sheet, a range test and a depth raster. The study is synthetic and uses
# the shared example of the positionR suite: Stoney Lake at 100 m, the 20
# planned receivers of arraydesign, the shared range model with no spread,
# the shared correlated random walk and the shared tag delay.
# Four walleye are released at one point and tracked from 2 to 10 June 2024.
# ST-02 dies part-way, and its tag stays where it fell, in range of one
# receiver. ST-02 and ST-03 carry depth sensors, sent as raw counts, so that
# the sensor calibration and the depth rule have data. No real data ship with
# the package. Needs arraydesign and detfield.
#
# The files in inst/extdata are this script's output with detfield's exact
# through-water path.

# ---- Libraries ----
library(data.table)

# ---- Settings ----
# The shared range model: d50 and d05 at 1 m and at 35 m receiver depth.
range_depth <- c(1, 35)
range_d50 <- c(250, 750)
range_d05 <- c(500, 1500)
# The shared movement model and tag delay.
step_mean <- 30
step_sd <- 15
turn_sd <- 40
time_step <- 60
tag_delay <- c(120, 180)
tag_life_days <- 650L
n_animals <- 4L
dead_animal <- 2L
# The study frame, all UTC. Receivers go in a day before the release and come
# out a day after the last position.
deploy_time <- as.POSIXct("2024-06-01 14:00:00", tz = "UTC")
release_time <- as.POSIXct("2024-06-02 15:30:00", tz = "UTC")
end_time <- as.POSIXct("2024-06-10 12:00:00", tz = "UTC")
recover_time <- as.POSIXct("2024-06-11 15:00:00", tz = "UTC")
# ST-02 dies at its first position inside this window where the array hears a
# transmission with probability 0.3 to 0.7, at least 90% of it from one
# receiver. Its dead tag is then heard most hours at one station.
die_after <- as.POSIXct("2024-06-04 00:00:00", tz = "UTC")
die_before <- as.POSIXct("2024-06-07 00:00:00", tz = "UTC")
p_dead_range <- c(0.3, 0.7)
p_one_min <- 0.9
seeds <- c(tracks = 3L, transmissions = 2L, detections = 1L, sensors = 4L)
# Depth tags: the InnovaSea calibration of the detqc examples, metres =
# intercept + slope * raw count. A live walleye swims at 40% to 100% of the
# water depth below it, never shallower than 0.3 m; a dead one lies on the
# bottom.
depth_tags <- c(2L, 3L)
depth_slope <- 0.1504
depth_intercept <- -0.6018

# ---- Depth raster ----
# A byte-identical copy of the suite's Stoney raster. Depth is stored as NEGATIVE
# metres; abs() gives depth below the surface.
source_tif <- "../arraydesignR/inst/extdata/stoney_depth.tif"
tif_md5 <- "21ba8e62d4f1fe7050b155a8fa6c59b3"
file.copy(source_tif, "inst/extdata/stoney_depth.tif", overwrite = TRUE)
stopifnot(unname(tools::md5sum("inst/extdata/stoney_depth.tif")) == tif_md5)
lake <- terra::rast("inst/extdata/stoney_depth.tif")
cat("water cells:", sum(!is.na(terra::values(lake))), "\n")
range(abs(terra::values(lake)), na.rm = TRUE)  # depth, m

# ---- Receivers ----
# The 20 planned stations of arraydesign. place_receivers() reads each
# station's depth from the raster as deploy_z.
stations_file <- system.file("extdata", "planned_stations.csv", package = "arraydesign")
planned <- read.csv(stations_file)
rx <- arraydesign::place_receivers(lake, method = "manual", coords = planned)
nrow(rx)  # receivers
range(rx$deploy_z)  # receiver depth, m

# ---- Detection field of the shared range model ----
model <- detfield::de_logistic(d50 = range_d50, d05 = range_d05, depth = range_depth)
dist <- detfield::water_distances(lake, rx)
field <- detfield::de_field(dist, model)

# ---- Tracks ----
# All four walleye are released at the water cell nearest a point between
# stations S009 and S012, in the middle of the array.
xy0 <- cbind(726592, 4937450)
water_xy <- terra::xyFromCell(lake, which(!is.na(terra::values(lake))))
release_xy <- water_xy[which.min((water_xy[, 1] - xy0[1])^2 + (water_xy[, 2] - xy0[2])^2), ]
release <- data.frame(x = release_xy[[1]], y = release_xy[[2]])
release
movement <- arraydesign::crw_model(step_mean = step_mean, step_sd = step_sd, turn_sd = turn_sd,
                                   time_step_s = time_step)
n_steps <- as.integer(floor(as.numeric(difftime(end_time, release_time, units = "secs")) /
                              time_step))
n_steps  # steps of 60 s per animal
tracks <- arraydesign::simulate_tracks(
  lake, movement, n_animals = n_animals, n_steps = n_steps,
  control = arraydesign::track_control(start = release, start_time = release_time),
  seed = seeds[["tracks"]]
)
tr <- as.data.table(tracks)

## The death of ST-02
p_any <- 1 - apply(1 - field$p, 1, prod)
p_top <- apply(field$p, 1, max) / rowSums(field$p)
tr[, `:=`(p_heard = p_any[match(cell, field$cells)], p_one = p_top[match(cell, field$cells)])]
at_death <- tr[path_id == dead_animal & time >= die_after & p_heard >= p_dead_range[1] &
                 p_heard <= p_dead_range[2] & p_one >= p_one_min][1]
stopifnot(!is.na(at_death$time), at_death$time <= die_before)
death_time <- at_death$time
tr[path_id == dead_animal & time > death_time,
   `:=`(x = at_death$x, y = at_death$y, cell = at_death$cell)]
cat("ST-02 dies at", format(death_time, tz = "UTC"), "UTC; p heard", round(at_death$p_heard, 2),
    "; x", round(at_death$x), "y", round(at_death$y), "\n")
tracks_dead <- tracks
tracks_dead[, c("x", "y", "cell")] <- as.data.frame(tr[, list(x, y, cell)])

# ---- Transmissions and detections ----
tx <- arraydesign::simulate_transmissions(tracks_dead, tag_delay = tag_delay,
                                          seed = seeds[["transmissions"]])
det <- arraydesign::simulate_detections(tx, field, overdispersion = "none",
                                        seed = seeds[["detections"]])
d <- as.data.table(det$detections)
nrow(tx)  # transmissions sent
sum(det$transmissions$n_detections > 0)  # transmissions heard by at least one receiver
nrow(d)  # detections
print(d[, .N, by = animal_id])

# ---- Animals and their tags ----
ids <- seq_len(n_animals)
animals <- data.table(
  path_id = ids,
  animal_id = sprintf("ST-%02d", ids),
  transmitter_codespace = ifelse(ids %in% depth_tags, "A69-9002", "A69-1601"),
  transmitter_id = as.character(61000 + ids),
  tag_serial_number = as.character(1600100 + ids),
  tag_model = ifelse(ids %in% depth_tags, "V13P-1x", "V13-1x"),
  # GLATOS exports give length in metres.
  length = c(0.512, 0.468, 0.547, 0.495)
)
animals[, transmitter := paste(transmitter_codespace, transmitter_id, sep = "-")]
print(animals)

# ---- Depth readings ----
# Each detection of a depth tag carries the raw count of the depth at the
# transmission it decoded. Water depth is abs() of the raster cell.
set.seed(seeds[["sensors"]])
tx_depth <- as.data.table(det$transmissions)[, list(animal_id, datetime, cell)]
tx_depth[, water_m := abs(terra::values(lake)[cell, 1])]
tx_depth[, path_id := as.integer(animal_id)][, animal_id := NULL]
tx_depth[, dead := path_id == dead_animal & datetime > death_time]
# A pressure sensor on the bottom varies by a few centimetres, less than one
# raw count (0.15 m). A live animal is kept 0.3 m or more below the surface.
tx_depth[, depth_m := fifelse(dead, water_m + rnorm(.N, 0, 0.02),
                              pmax(0.3, water_m * runif(.N, 0.4, 1)))]
tx_depth[, raw := pmin(255, pmax(0, round((depth_m - depth_intercept) / depth_slope)))]
tx_depth[!path_id %in% depth_tags, raw := NA]
cat("ST-02 lies on the bottom at", round(tx_depth[dead == TRUE, mean(water_m)], 1), "m\n")

# ---- GLATOS detection export ----
st <- as.data.table(rx)[, list(station_id, receiver_serial, deploy_lat, deploy_lon,
                               deploy_z, station_no = seq_len(.N))]
d[, receiver_serial := as.character(receiver_serial)]
d <- st[d, on = "receiver_serial"]
# The simulated animal_id is the path number.
d[, path_id := as.integer(animal_id)][, animal_id := NULL]
d <- tx_depth[, list(path_id, datetime, raw)][d, on = c("path_id", "datetime")]
d <- animals[d, on = "path_id"]
d[, .(depth_detections = sum(!is.na(raw)), detections = .N), by = animal_id]
release_ll <- terra::crds(terra::project(terra::vect(release, geom = c("x", "y"),
                                                     crs = terra::crs(lake)), "EPSG:4326"))
glatos <- d[, list(
  animal_id,
  detection_timestamp_utc = format(datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  glatos_array = "STN", station_no, transmitter_codespace, transmitter_id,
  sensor_value = raw, sensor_unit = fifelse(is.na(raw), "", "ADC"),
  deploy_lat = round(deploy_lat, 6), deploy_long = round(deploy_lon, 6),
  receiver_sn = receiver_serial, tag_type = fifelse(is.na(raw), "Pinger", "Sensor"), tag_model,
  tag_serial_number,
  common_name_e = "walleye", length,
  release_latitude = round(release_ll[1, 2], 6), release_longitude = round(release_ll[1, 1], 6),
  utc_release_date_time = format(release_time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  glatos_project_transmitter = "STN", glatos_project_receiver = "STN", station = station_id
)]
setorder(glatos, detection_timestamp_utc, receiver_sn)
fwrite(glatos, "inst/extdata/stoney_detections.csv", na = "")
cat("stoney_detections.csv:", nrow(glatos), "rows\n")

# ---- GLATOS receiver locations ----
# Receivers sit on the bottom, so instrument_depth, which the reader takes as
# deploy_z, is the depth of the station's cell. R001 to R019 are VR2Tx
# transceivers, as their logs in inst/extdata/receiver_logs and the VDAT file
# of R011 write them; R020, which has no log, is a VR2W. GLATOS writes the
# model without its frequency, which glatos_ins_frequency holds.
receivers <- st[, list(
  station = station_id, glatos_array = "STN", station_no = sprintf("%02d", station_no),
  consecutive_deploy_no = 1L, intend_lat = NA, intend_long = NA,
  deploy_lat = round(deploy_lat, 7), deploy_long = round(deploy_lon, 7),
  recover_lat = NA, recover_long = NA,
  deploy_date_time = format(deploy_time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  recover_date_time = format(recover_time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  bottom_depth = round(deploy_z, 2), riser_length = NA, instrument_depth = round(deploy_z, 2),
  ins_model_no = ifelse(receiver_serial == "R020", "VR2W", "VR2Tx"),
  glatos_ins_frequency = 69L, ins_serial_no = receiver_serial,
  code_map = "", code_map_comment = "", deployed_by = "", comments = "",
  glatos_seasonal = "", glatos_project = "STN", glatos_vps = ""
)]
fwrite(receivers, "inst/extdata/stoney_receivers.csv", na = "")
cat("stoney_receivers.csv:", nrow(receivers), "rows\n")

# ---- Tag specification sheet ----
depth_tag <- animals$path_id %in% depth_tags
specs <- animals[, list(TAG_SERIAL_NUMBER = tag_serial_number, Transmitter = transmitter,
                        Tag.Type = tag_model, Tag.Life = tag_life_days,
                        Min.Delay.sec = tag_delay[1], Max.Delay.sec = tag_delay[2],
                        Sensor.Type = fifelse(depth_tag, "P", ""),
                        Units = fifelse(depth_tag, "Meters", ""),
                        Slope = fifelse(depth_tag, depth_slope, NA_real_),
                        Intercept = fifelse(depth_tag, depth_intercept, NA_real_))]
fwrite(specs, "inst/extdata/stoney_tag_specs.csv", na = "", quote = TRUE)

# ---- Range test ----
# The example range test of arraydesign, at R006 (station S006) in 1.0 m of
# water, copied unchanged. Fitted alone it gives d50 247.9 m and d05 492.3 m
# (arraydesign/data-raw/extdata.R).
range_file <- system.file("extdata", "range_test.csv", package = "arraydesign")
file.copy(range_file, "inst/extdata/stoney_range_test.csv", overwrite = TRUE)
range_test <- read.csv("inst/extdata/stoney_range_test.csv")
fit <- glm(cbind(detections, transmissions - detections) ~ distance_m,
           family = binomial, data = range_test)
b <- coef(fit)
round(c(d50 = -b[[1]] / b[[2]], d05 = (qlogis(0.05) - b[[1]]) / b[[2]]), 1)

# ---- What the example holds ----
span <- range(d$datetime)
cat("detections:", nrow(d), "of", n_animals, "animals from", format(span[1], tz = "UTC"),
    "to", format(span[2], tz = "UTC"), "UTC\n")
cat("days with a detection:", length(unique(as.Date(d$datetime))), "\n")
cat("ST-02 died at", format(death_time, tz = "UTC"), "UTC\n")
print(file.size(list.files("inst/extdata", "^stoney_", full.names = TRUE)))
