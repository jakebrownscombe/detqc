# Code of the vignette "Base processing: read, check, and filter telemetry data".
# Extracted from vignettes/reading-files.Rmd by tools/purl_vignettes.R, a repository file
# the installed package does not carry. The vignette is the source; this file follows it.

# ---- Result figure ----
library(detqc)
# Locate the example files included with the package
extdata_dir <- system.file("extdata", package = "detqc")
in_file <- function(name) file.path(extdata_dir, name)

# Read 3 June 2024 from each of the three exports
one_day <- list(
  OTN = read_detections(in_file("stoney_otn.csv")),
  GLATOS = read_detections(in_file("stoney_detections.csv"),
                           cut = list(from = "2024-06-03", to = "2024-06-04")),
  VUE = read_detections(in_file("stoney_vue.csv"))
)
vapply(one_day, nrow, 1L)
# Take the UTC hour of each detection, by export
per_hour <- do.call(rbind, lapply(names(one_day), function(export) {
  data.frame(export = export,
             hour = as.integer(format(one_day[[export]]$datetime, "%H",
                                      tz = "UTC")))
}))
# Draw the detections per hour, one panel per export
ggplot2::ggplot(per_hour, ggplot2::aes(hour)) +
  ggplot2::geom_bar(fill = "#31688E", width = 0.85) +
  ggplot2::facet_wrap(~export, ncol = 1) +
  ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 23)) +
  ggplot2::labs(x = "Hour of 3 June 2024 (UTC)", y = "Detections") +
  theme_positionr() +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

# ---- Maps ----
# List the detection file formats, and show the GLATOS map
unique(column_map("detections")$format)
column_map("detections", "glatos")[, c("target", "source", "type", "tz")]

# ---- Three formats ----
# Show the format recognised in each file
vapply(one_day, function(det) attr(det, "source")[[1]]$format, "")
# Test that the OTN and VUE reads give the same times and transmitters
otn <- one_day$OTN
vue <- one_day$VUE
identical(otn$datetime, vue$datetime) && identical(otn$transmitter,
                                                   vue$transmitter)
head(otn[, c("datetime", "receiver_serial", "transmitter", "animal_id",
             "station_id")], 3)
# Count the rows of the VUE export that name no animal
sum(is.na(vue$animal_id))

# ---- Own file ----
# Write a detection file in a lab's own layout, in local time
own_file <- file.path(tempdir(), "my_detections.csv")
writeLines(c(
  "Date_Local,Rx,TagCode,Fish",
  "2024-06-03 09:06:06,R011,A69-9002-61002,ST-02",
  "2024-06-03 18:45:59,R011,A69-9002-61002,ST-02",
  "2024-06-04 06:12:40,R013,A69-1601-61001,ST-01"
), own_file)
# Read it: read_detections() stops and names the columns it found
try(read_detections(own_file))

# ---- Own map ----
# Map each column of the file to the column it becomes, and its type
my_map <- data.frame(
  target = c("datetime", "receiver_serial", "transmitter", "animal_id"),
  source = c("Date_Local", "Rx", "TagCode", "Fish"),
  type = c("datetime", "character", "character", "character")
)
# Read the file through the map, its times in Eastern time
read_detections(own_file, format = "vue", col_map = my_map,
                tz = "America/Toronto")

# ---- Spring gap ----
# Write and read a local time inside the spring daylight-saving gap
gap_file <- file.path(tempdir(), "local_time.csv")
writeLines(c("Date_Local,Rx,TagCode,Fish",
             "2024-03-10 02:30:00,R011,A69-9002-61002,ST-02"), gap_file)
try(read_detections(gap_file, format = "vue", col_map = my_map,
                    tz = "America/Toronto"))

# ---- Stack ----
# Read two files in one call; source_file names each row's file
stacked <- read_detections(c(in_file("stoney_otn.csv"),
                             in_file("stoney_vue.csv")))
table(stacked$source_file)

# ---- Cut ----
# Read 5 and 6 June of one transmitter, cut as the file is read
two_days <- read_detections(in_file("stoney_detections.csv"),
                            cut = list(from = "2024-06-05", to = "2024-06-07",
                                       transmitters = "A69-9002-61003"))
range(two_days$datetime)
# Count the rows read, kept, and outside the cut
cut_record <- attr(two_days, "source")[[1]]
cut_record[c("rows_read", "rows", "rows_outside_cut")]

# ---- Vdat ----
# Read the VDAT file of receiver R011
vdat <- read_detections(in_file("stoney_vdat.csv"))
table(vdat$transmitter)
# Show the receiver's own transmitter settings
attr(vdat, "receiver_transmitters")

# ---- Tolerance set ----
# Read the receivers and tagging records
deployments <- read_deployments(in_file("stoney_receivers.csv"),
                                format = "glatos")
animals <- read_animals(in_file("stoney_detections.csv"), format = "glatos")
# Read the export, and make a copy of it half a second later
whole <- read_detections(in_file("stoney_detections.csv"))
fraction <- whole
fraction$datetime <- fraction$datetime + 0.5
# Build one detection set holding both
dets_doubled <- detset(rbind(whole, fraction), deployments, animals)

# ---- Tolerance ----
# Count the detections removed by the last step of the log
removed <- function(dets) tail(detset_log(dets)$removed, 1)
# Count the copies removed with exact matching, and within 1 s
c(exact = removed(filter_duplicates(dets_doubled)),
  tolerance_1s = removed(filter_duplicates(dets_doubled, tolerance_s = 1)))

# ---- Receiver logs ----
# Read the logs of the example receivers, one file per receiver
log_files <- list.files(in_file("receiver_logs"), full.names = TRUE)
logs <- read_receiver_logs(log_files)
# Count the records by type
table(logs$record_type)

# ---- Audit logs ----
# Build the set, and audit each receiver-day with the logs
dets_logged <- detset(read_detections(in_file("stoney_detections.csv")),
                      deployments, animals)
dets_logged <- audit_listening(
  dets_logged, known_listening = unique(deployments$receiver_serial),
  logs = logs
)
# Count receiver-days by status
table(dets_logged$listening$status)

# ---- Logs figure ----
# Draw one tile per receiver and day, coloured by status
ggplot2::ggplot(dets_logged$listening,
                ggplot2::aes(date, receiver_serial, fill = status)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
  ggplot2::scale_fill_manual(values = c(listening = "#2C5AA0",
                                        logging_silent = "#B8C7DE",
                                        silent = "grey70", off = "#C0392B"),
                             name = NULL) +
  ggplot2::scale_x_date(date_labels = "%d %b", date_breaks = "1 day",
                        expand = c(0, 0)) +
  ggplot2::labs(x = "Date (UTC)", y = "Receiver") +
  theme_positionr() +
  ggplot2::theme(panel.grid = ggplot2::element_blank())

# ---- Logs after filters ----
# Remove the post-release and isolated detections before the audit
dets_filtered <- filter_post_release(dets_logged, hours = 6)
dets_filtered <- filter_min_lag(dets_filtered, max_lag_s = 4500)
# Audit the filtered set without the logs, then with them
working_rx <- unique(deployments$receiver_serial)
no_logs <- audit_listening(dets_filtered, known_listening = working_rx)
with_logs <- audit_listening(dets_filtered, known_listening = working_rx,
                             logs = logs)
# List the quiet days that the logs' detection counts keep listening
quiet_heard <- with_logs$listening$status == "listening" &
  with_logs$listening$detections == 0
data.frame(with_logs$listening[quiet_heard, c("receiver_serial", "date")],
           without_logs = no_logs$listening$status[quiet_heard],
           with_logs = with_logs$listening$status[quiet_heard])

# ---- Nextrak log ----
# Read the included NexTrak log
nextrak <- read_receiver_logs(in_file("nextrak_receiver_log.csv"))
# Count the records by type
table(nextrak$record_type)
# Show three DIAG_NEXTRAK records: 10 minutes apart, noise in dB
nextrak_diag <- nextrak[nextrak$record_type == "DIAG_NEXTRAK", ]
head(nextrak_diag[, c("receiver_model", "datetime", "noise", "noise_unit",
                      "log_detections")], 3)

# ---- Sensor columns ----
# Count the VDAT rows with a raw count, a converted value, or both
has_raw <- !is.na(vdat$sensor_raw)
has_value <- !is.na(vdat$sensor_value)
table(raw = has_raw, value = has_value)
# Show one row of each kind
sensor_cols <- c("transmitter", "sensor_raw", "sensor_value", "sensor_unit")
vdat[c(which(has_raw & !has_value)[1], which(has_raw & has_value)[1]),
     sensor_cols]

# ---- Specs ----
# Read the tag sheet: delays and sensor calibration of each tag
stoney_specs <- read_tag_specs(in_file("stoney_tag_specs.csv"))
stoney_specs[, c("transmitter", "tag_serial", "min_delay", "max_delay",
                 "sensor_type",
                 "sensor_unit", "sensor_slope", "sensor_intercept")]

# ---- Calibrate ----
# Build a detection set and calibrate it with the tag sheet
dets <- calibrate_sensors(detset(whole, deployments, animals), stoney_specs)
# Count readings by unit, and give the range of each animal's values
table(dets$detections$sensor_unit, useNA = "ifany")
tapply(dets$detections$sensor_value, dets$detections$animal_id, range)
# Print the log record of the calibration
detset_log(dets)$reason[2]

# ---- Two codes specs ----
# Read the tag sheet as text, and add a second code for tag 1600102
sheet <- read.csv(in_file("stoney_tag_specs.csv"), colClasses = "character")
second <- sheet[sheet$TAG_SERIAL_NUMBER == "1600102", ]
second[, c("Transmitter", "Tag.Type", "Sensor.Type", "Units", "Slope",
           "Intercept")] <-
  list("A69-1601-61102", "V13P-1x", "", "", "", "")
# Write the new sheet and read it back
two_code_sheet <- file.path(tempdir(), "two_code_specs.csv")
write.csv(rbind(sheet, second), two_code_sheet, row.names = FALSE)
specs <- read_tag_specs(two_code_sheet)
specs[specs$tag_serial == "1600102", c("transmitter", "tag_serial",
                                       "sensor_type")]

# ---- Two codes ----
# Write a lab's tagging CSV with one code per animal
tagging <- file.path(tempdir(), "my_tagging.csv")
writeLines(c(
  "Fish,Code,Serial,Species,Released",
  "ST-01,A69-1601-61001,1600101,walleye,2024-06-02 11:30",
  "ST-02,A69-9002-61002,1600102,walleye,2024-06-02 11:30"
), tagging)
# Map the columns of that file
tag_map <- data.frame(
  target = c("animal_id", "transmitter", "tag_serial", "species",
             "release_datetime"),
  source = c("Fish", "Code", "Serial", "Species", "Released"),
  type = c("character", "character", "character", "character", "datetime")
)
# Read the file as "otn", the default, which adds no step to a plain CSV;
# "glatos" would convert GLATOS local release times.
read_animals(tagging, format = "otn", col_map = tag_map, tz = "America/Toronto",
             tag_specs = specs)[, c("animal_id", "transmitter", "tag_serial",
                                    "min_delay", "sensor_type")]

# ---- Receiver transmitter ----
# Read the OTN deployment sheet with its TRANSMITTER column
otn_deployments <- read_deployments(in_file("stoney_otn_deployments.csv"),
                                    format = "otn")
# Show the last three receivers and their own transmitter ids
tail(otn_deployments[, c("receiver_serial", "receiver_model",
                         "receiver_transmitter")], 3)
# Audit the receiver-days with the ids as the reference tags
dets_own <- detset(read_detections(in_file("stoney_detections.csv")),
                   otn_deployments, animals)
own_rx <- unique(otn_deployments$receiver_serial)
dets_own <- audit_listening(dets_own, known_listening = own_rx)
table(dets_own$listening$status)

# ---- Checks table ----
# List every check, its group, what it flags and what it counts
checks <- attr(all_checks(), "table")
knitr::kable(checks[, c("check", "group", "description", "unit")],
             col.names = c("Check", "Group", "What it flags", "Counted in"))

# ---- Settings ----
# Show the thresholds and their defaults
str(check_settings())
# Run the checks with two thresholds changed
dets <- check_detset(dets, settings = check_settings(station_tol_m = 50,
                                                     year_off_days = 200))

# ---- Study ----
# Read the depth raster: 100 m cells, UTM zone 17N, depth stored as
# zero or negative metres, land NA
lake <- terra::rast(in_file("stoney_depth.tif"))
# Read the tagging records joined to the Stoney tag sheet
animals <- read_animals(in_file("stoney_detections.csv"), format = "glatos",
                        tag_specs = stoney_specs)
# Use the GLATOS export read above
detections <- whole

# ---- Plant swap ----
# Swap the latitude and longitude of S010 in a copy of the deployments
deployments_swapped <- deployments
s010 <- deployments_swapped$station_id == "S010"
deployments_swapped[s010, c("deploy_lat", "deploy_lon")] <-
  deployments[s010, c("deploy_lon", "deploy_lat")]

# ---- Plant false ----
# Project the receivers to the raster's UTM grid to measure distances
lonlat <- as.matrix(deployments[, c("deploy_lon", "deploy_lat")])
utm <- terra::project(lonlat, from = "EPSG:4326", to = terra::crs(lake))
stations <- data.frame(station_id = deployments$station_id, x = utm[, 1],
                       y = utm[, 2])
# Pick a true detection, and the station farthest from its own
true_row <- detections[detections$animal_id == "ST-01", ][100, ]
at_true <- stations$station_id == true_row$station_id
far <- stations[which.max((stations$x - stations$x[at_true])^2 +
                            (stations$y - stations$y[at_true])^2), ]
# Make the false detection: same transmitter, far station, 2 min later
false_row <- true_row
false_row$datetime <- true_row$datetime + 120
false_row$station_id <- far$station_id
far_dep <- deployments$station_id == far$station_id
false_row$receiver_serial <- deployments$receiver_serial[far_dep]
false_row[, c("datetime", "receiver_serial", "station_id")]
# Add the false detection to the detections
detections <- rbind(detections, false_row)

# ---- Plant codes ----
# Leave ST-03's detections out of a copy of the detections
detections_planted <- detections[detections$animal_id != "ST-03", ]
# Write ST-04's first 25 detections with a wrong code space
st04_first <- which(detections_planted$animal_id == "ST-04")[1:25]
detections_planted$transmitter[st04_first] <- "A69-1303-61004"
detections_planted$animal_id[st04_first] <- NA
# Count detections by transmitter in the copy
table(detections_planted$transmitter)

# ---- Check planted ----
# Build a detection set of the planted copies and check it
dets <- check_detset(detset(detections_planted, deployments_swapped, animals),
                     water = abs(lake), field = 1500)
# Print the checks that flagged
check_summary <- dets$checks$summary
check_summary[check_summary$status == "flag",
              c("check", "flagged", "of", "unit")]

# ---- Swapped rows ----
# Show the swapped station, with the position the check suggests
dets$checks$rows$swapped_lat_lon[, c("station_id", "deploy_lat", "deploy_lon",
                                     "suggested_lat", "suggested_lon")]

# ---- Code rows ----
# List the transmitters heard that no animal carries, most heard first
dets$checks$rows$transmitter_no_animal[, c("transmitter", "detections",
                                           "receivers")]
# List the animals of the tagging records with no detection
dets$checks$rows$animal_never_heard

# ---- Fix ----
# Rebuild the set with the correct deployments and the full detections
dets <- check_detset(detset(detections, deployments, animals),
                     water = abs(lake), field = 1500)
# Print the checks that still flag
check_summary <- dets$checks$summary
check_summary[check_summary$status == "flag",
              c("check", "flagged", "of", "unit")]

# ---- Jump map ----
# Map the receivers over the lake depth, sized by their detections
receiver_map <- plot_receivers(dets, water = lake)
# Take the positions of the station that heard ST-01 and of the false
# detection's station
jump_ends <- receiver_map$data[match(c(true_row$station_id, far$station_id),
                                     receiver_map$data$station_id), ]
# The rings cannot show the jump the filter acts on, so a keyed line draws it
jump <- data.frame(x = jump_ends$deploy_lon[1], y = jump_ends$deploy_lat[1],
                   xend = jump_ends$deploy_lon[2], yend = jump_ends$deploy_lat[2])
receiver_map +
  ggplot2::geom_segment(data = jump,
                        ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                                     linetype = "Jump to the false detection"),
                        inherit.aes = FALSE, colour = "red3", linewidth = 0.8) +
  ggplot2::scale_linetype_manual(name = NULL,
                                 values = c("Jump to the false detection" = 2)) +
  # The receiver rings are keyed in every legend; this key shows the line alone
  ggplot2::guides(linetype = ggplot2::guide_legend(override.aes = list(shape = NA)))

# ---- Abacus ----
# Draw the detections by animal, station and hour
plot_abacus(dets, rows = "station")

# ---- Filter release ----
# Remove detections before each release
dets <- filter_release(dets)
tail(detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")], 1)

# ---- Filter deployment period ----
# Remove detections outside every deployment of their receiver
dets <- filter_deployment_period(dets)
tail(detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")], 1)

# ---- Filter tag life ----
# Remove detections after the end of the tag's life
dets <- filter_tag_life(dets)
tail(detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")], 1)

# ---- Speed range ----
# Fit the range test's d05: a logistic curve of the share heard on distance
range_test <- read.csv(in_file("stoney_range_test.csv"))
test_depth <- range_test$deploy_z[1]
range_fit <- glm(cbind(detections, transmissions - detections) ~ distance_m,
                 family = binomial, data = range_test)
d05_test <- (qlogis(0.05) - coef(range_fit)[[1]]) / coef(range_fit)[[2]]
# Read d05 linearly from the test depth to the deepest receiver, towards
# the deep end of the positionR range model of Stoney Lake (1,500 m at
# 35 m), as detfield::de_logistic() reads it.
deep_d05 <- 1500
deep_depth <- 35
d05_deepest_exact <- approx(c(test_depth, deep_depth), c(d05_test, deep_d05),
                            xout = max(deployments$deploy_z))$y
# Round it to the nearest 100 m
d05_deepest <- round(d05_deepest_exact, -2)
# Print the test depth, its d05, the deepest receiver and its d05
round(c(test_depth_m = test_depth, d05_test_m = d05_test,
        deepest_receiver_m = max(deployments$deploy_z),
        d05_deepest_m = d05_deepest_exact, d05_deepest_rounded_m = d05_deepest), 2)

# ---- Speed settings ----
# Set the highest speed accepted, 2 m/s, and the largest distance at which
# two receivers hear one transmission, 2,500 m
max_speed <- 2
min_dist <- 2500

# ---- Speed steps ----
# List each step between consecutive detections of one animal, and its speed
steps <- speed_steps(dets, max_speed_ms = max_speed, min_dist_m = min_dist)
nrow(steps)

# ---- Speed moves ----
# Keep the steps between two different stations
moves <- steps[steps$dist_m > 0, ]
nrow(moves)

# ---- Speed violations ----
# Show the steps farther than min_dist and faster than max_speed
moves[moves$violation, c("animal_id", "datetime", "dist_m", "speed_ms")]

# ---- Speed figure ----
# Mark the two steps into and out of the planted detection
planted <- moves$animal_id == "ST-01" &
  (moves$datetime == false_row$datetime | moves$from == false_row$datetime)
# The axis labels the powers of ten from 0.001 to 1,000 m/s that fall inside
# the plotted range
ticks <- 10^(-3:3)
steps_lab <- "Step into or out of the planted detection"
mark_col <- stats::setNames(c("#31688E", "#D55E00"), c("Step", steps_lab))
mark_shape <- stats::setNames(c(1, 17), c("Step", steps_lab))
corner <- data.frame(xmin = min_dist, xmax = Inf, ymin = max_speed, ymax = Inf)
ggplot2::ggplot(moves, ggplot2::aes(dist_m, speed_ms)) +
  ggplot2::geom_rect(data = corner, ggplot2::aes(xmin = xmin, xmax = xmax,
                                                 ymin = ymin, ymax = ymax,
                                                 fill = "Where the filter acts"),
                     inherit.aes = FALSE, alpha = 0.2) +
  ggplot2::geom_hline(yintercept = max_speed, linetype = 2) +
  ggplot2::geom_vline(xintercept = min_dist, linetype = 2) +
  ggplot2::geom_point(ggplot2::aes(colour = "Step", shape = "Step")) +
  ggplot2::geom_point(data = moves[planted, ],
                      ggplot2::aes(colour = steps_lab, shape = steps_lab),
                      size = 2.5) +
  ggplot2::scale_fill_manual(name = NULL, values = c("Where the filter acts" = "#E69F00")) +
  ggplot2::scale_colour_manual(name = NULL, values = mark_col) +
  ggplot2::scale_shape_manual(name = NULL, values = mark_shape) +
  ggplot2::scale_y_log10(breaks = ticks,
                         labels = format(ticks, scientific = FALSE,
                                         drop0trailing = TRUE)) +
  ggplot2::expand_limits(x = 0) +
  ggplot2::labs(x = "Distance between stations (m)",
                y = "Speed of the step (m/s)") +
  theme_positionr() +
  ggplot2::theme(legend.position = "bottom")

# ---- Filter speed ----
# Remove one end of each violating step
dets <- filter_speed(dets, max_speed_ms = max_speed, min_dist_m = min_dist)
# Print the log record, and the removals by reason
speed_log <- detset_log(dets)
tail(speed_log[, c("step", "rows_before", "rows_after", "removed")], 1)
speed_log$removed_by[[nrow(speed_log)]]
# Test whether the planted detection is still in the set
any(dets$detections$datetime == false_row$datetime &
      dets$detections$station_id == false_row$station_id)

# ---- Lag explore ----
# Set the threshold: 30 times the average delay, the mean of each tag's
# minimum and maximum delay
max_lag <- 30 * mean((animals$min_delay + animals$max_delay) / 2)
max_lag
# Compute the minimum lag of every detection, in seconds; NA for a
# detection that is the only one of its transmitter on its receiver
lags <- minimum_lags(dets)
# Count the detections the threshold would remove: a lag above it, or none
c(above_threshold = sum(lags > max_lag, na.rm = TRUE),
  no_lag = sum(is.na(lags)))
# Give the share of lags under 1,000 s, in per cent
round(100 * mean(lags < 1000, na.rm = TRUE))
# Draw a histogram of the lags with the threshold on it
ggplot2::ggplot(data.frame(lag_s = lags[!is.na(lags)]),
                ggplot2::aes(lag_s)) +
  ggplot2::geom_histogram(bins = 40, fill = "#31688E", colour = "white") +
  ggplot2::geom_vline(xintercept = max_lag, linetype = 2) +
  ggplot2::scale_x_log10(labels = scales::label_comma()) +
  ggplot2::scale_y_sqrt() +
  ggplot2::labs(x = "Minimum lag (s, log scale)",
                y = "Detections (square-root scale)") +
  theme_positionr()

# ---- Filter min lag ----
# Keep the set before the filter, to compare the two forms of the test
before_lag <- dets
# Remove detections isolated in time beyond the threshold, or alone
dets <- filter_min_lag(dets, max_lag_s = max_lag)
tail(detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")], 1)

# ---- Filter min lag long ----
# Apply both conditions to the set before the filter: long intervals are
# over 24 times max_lag, 720 times the average delay
dets_both_conditions <- filter_min_lag(before_lag, max_lag_s = max_lag,
                                       long_lag_s = 24 * max_lag)
# Print the log record of the step
writeLines(strwrap(tail(detset_log(dets_both_conditions)$reason, 1),
                   width = 72))

# ---- Filter min detections ----
# Remove animals heard under 10 times or on under 2 days
dets <- filter_min_detections(dets, min_detections = 10, min_days = 2)
tail(detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")], 1)

# ---- Filter animals ----
# Keep the walleye only
dets <- filter_animals(dets, species = "walleye")
tail(detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")], 1)

# ---- Summarise animals ----
# Summarise each animal in one row
summarise_animals(dets)[, c("animal_id", "detections", "days", "stations",
                            "days_at_liberty")]

# ---- Summarise detections ----
# Count detections by station, and show the first four
head(summarise_detections(dets, by = "station"), 4)

# ---- Residency ----
# Show the residency of ST-02 at each station
station_residency <- residency_index(dets, by = "station")
station_residency[station_residency$animal_id == "ST-02", ]

# ---- Validate ----
# Turn the times into text and validate
broken <- dets
broken$detections$datetime <- format(broken$detections$datetime)
try(validate_detset(broken))
