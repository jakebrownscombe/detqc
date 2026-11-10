# Code of the vignette "Get started with detqc".
# Extracted from vignettes/detqc.Rmd by tools/purl_vignettes.R, a repository file
# the installed package does not carry. The vignette is the source; this file follows it.

# ---- Read ----
library(detqc)
# Locate the example files included with the package
extdata_dir <- system.file("extdata", package = "detqc")
in_file <- function(name) file.path(extdata_dir, name)

# Read the tag sheet: each transmitter's delays and sensor calibration
specs <- read_tag_specs(in_file("stoney_tag_specs.csv"))
# Read the detection export
detections <- read_detections(in_file("stoney_detections.csv"))
# Read the receiver deployments, a GLATOS receiver file
deployments <- read_deployments(in_file("stoney_receivers.csv"),
                                format = "glatos")
# Read the tagging records, joined to the tag sheet
animals <- read_animals(in_file("stoney_detections.csv"), format = "glatos",
                        tag_specs = specs)
# Show each animal's transmitter, release, delays and sensor
animals[, c("animal_id", "transmitter", "release_datetime", "min_delay",
            "max_delay", "sensor_type")]

# ---- Build ----
# Join the three tables into one detection set and start its log
dets <- detset(detections, deployments, animals)
# Convert raw sensor values to units with the tag sheet's calibration
dets <- calibrate_sensors(dets)
# Print the detection set
dets

# ---- Map ----
# Read the depth raster of the lake: 100 m cells, UTM zone 17N,
# depth stored as zero or negative metres, land NA
lake <- terra::rast(in_file("stoney_depth.tif"))
# Map the stations over the lake depth
plot_receivers(dets, water = lake) + theme_positionr(base_size = 13)

# ---- Ring colours ----
# The ring colours of a receiver that heard, one that heard nothing, and the edge
positionr_ring_colours()

# ---- Checks ----
# Run every check; the raster as positive metres serves the land and
# depth checks
dets <- check_detset(dets, water = abs(lake))
# Print the checks that did not pass
checks <- dets$checks$summary
checks[checks$status != "pass", c("check", "status", "flagged", "of", "unit")]

# ---- Check counts ----
silent <- checks$flagged[checks$check == "receiver_silent"]
# Test whether every deployment spans the whole detection record
det_span <- range(dets$detections$datetime)
all_whole <- all(dets$deployments$deploy_datetime <= det_span[1] &
                   dets$deployments$recover_datetime >= det_span[2])

# ---- Timeline ----
# Draw each deployment as a bar from deployment to recovery
plot_timeline(dets) + theme_positionr(base_size = 13)

# ---- Audit ----
# Give each receiver-day a listening status; every receiver here worked
dets <- audit_listening(dets,
                        known_listening = unique(dets$deployments$receiver_serial))
# Count receiver-days by status
table(dets$listening$status)

# ---- Filter duplicates ----
# Remove repeated detections of one transmitter on one receiver
dets <- filter_duplicates(dets)

# ---- Filter post release ----
# Remove the first 6 h of each animal's record, its recovery from tagging
dets <- filter_post_release(dets, hours = 6)

# ---- Lag threshold ----
# Set the threshold: 30 times the average delay, the mean of each tag's
# minimum and maximum delay from the tag sheet
max_lag <- 30 * mean((animals$min_delay + animals$max_delay) / 2)
max_lag

# ---- Filter lag ----
# Remove detections isolated in time beyond the threshold, or alone
dets <- filter_min_lag(dets, max_lag_s = max_lag)
# Print the log so far: detections before and after each step
detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")]

# ---- Filter counts ----
log_now <- detset_log(dets)
removed_by <- function(step) log_now$removed[log_now$step == step]

# ---- Detect ----
# Scale the temporal settings to nine days: a final run of 3 detection
# days and 3 calendar days, 2 flat sensor days, a record end within 2 days
mort_rules <- mortality_rules(min_days = 3, min_calendar_days = 3,
                              min_flat_days = 2, within_days = 2)
# Sort each animal into the dead, review or alive band
bands <- detect_mortality(dets, rules = mort_rules)
bands$bands[, c("animal_id", "band", "deciding_rule", "death_date")]

# ---- Out dir ----
# Change to a folder beside your data, such as "C:/telemetry/2024/processed".
out_dir <- tempdir()

# ---- Cards ----
# Make the outline of the lake for the maps on the cards
outline <- sf::st_as_sf(terra::as.polygons(terra::ifel(is.na(lake), NA, 1)))
# Write a card for every animal, dead, review and alive, and an index
dets <- review_mortality(dets, file.path(out_dir, "mortality_review"),
                         bands = bands, all = TRUE, open = FALSE,
                         control = list(outline = outline))
# List the cards written
dets$cards[, c("animal_id", "band", "checked")]

# ---- Calls ----
# Write the calls: ST-02 dead at 02:00 UTC on 4 June, the other three alive
calls <- data.frame(animal_id = c("ST-01", "ST-02", "ST-03", "ST-04"),
                    call = c("alive", "dead_or_shed", "alive", "alive"),
                    death_date = c("", "2024-06-04", "", ""),
                    death_time = c("", "2024-06-04 02:00:00", "", ""))
calls_file <- file.path(tempdir(), "mortality_calls.csv")
write.csv(calls, calls_file, row.names = FALSE)
# Store the reviewer's calls in the detection set
dets <- apply_mortality_calls(dets, calls_file)

# ---- Cut ----
# Cut ST-02's record 1 day before its death time
dets <- filter_mortality(dets, margin_days = 1)
# Print the last two log records
tail(detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")], 2)

# ---- Export ----
# Convert the detection set to the three tables detfield reads
input <- as_detfield_input(dets)
sapply(input[c("detections", "tag_delay")], nrow)
# Save the detection set with its log
path <- write_detset(dets, file.path(tempdir(), "stoney_detset.rds"),
                     overwrite = TRUE)
basename(path)

# ---- Report ----
# Write the processing report into out_dir
report <- processing_report(dets, file.path(out_dir, "processing_report.html"),
                            bands = bands, control = list(outline = outline))
basename(report)

# ---- Attrition log ----
log_steps <- detset_log(dets)
breach_n <- sum(plot_attrition(dets)$data$breach)
cut_n <- log_steps$removed[log_steps$step == "filter_mortality"]

# ---- Attrition ----
# Draw the detections left after each logged step
plot_attrition(dets) + theme_positionr(base_size = 13)
