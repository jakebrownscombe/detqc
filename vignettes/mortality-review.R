# Code of the vignette "Mortality filtering: base algorithm and review cards".
# Extracted from vignettes/mortality-review.Rmd by tools/purl_vignettes.R, a repository file
# the installed package does not carry. The vignette is the source; this file follows it.

# ---- Out dir ----
# Change to a folder beside your data, such as "C:/telemetry/2024/processed".
out_dir <- tempdir()

# ---- Detset ----
library(detqc)
# Build the detection set from the included study, depth already in metres
detections <- example_mortality$detections
deployments <- example_mortality$deployments
animals <- example_mortality$animals
dets <- detset(detections, deployments, animals)
# Record which receivers listened; every receiver here worked
dets <- audit_listening(dets,
                        known_listening = unique(dets$deployments$receiver_serial))
# Print the prepared set
dets

# ---- Bands default ----
# Band the animals with the default streams
detect_mortality(dets)

# ---- Rules ----
# Print the default streams and their settings
mortality_rules()

# ---- Scaled rules ----
# Scale the streams to nine days: a final run of 3 detection days and
# 3 calendar days, 2 flat sensor days, a record end within 2 days
mort_rules <- mortality_rules(min_days = 3, min_calendar_days = 3,
                              min_flat_days = 2, within_days = 2)

# ---- Bands ----
# Band the animals with the scaled streams
bands <- detect_mortality(dets, rules = mort_rules)
bands
# Show each animal's band, deciding rule, candidate date and depth vote
bands$bands[, c("animal_id", "band", "deciding_rule", "death_date",
                "depth_flatline_vote", "record_end_vote")]

# ---- Depth figure ----
# Take ST-02's band row, with its candidate death date
st02 <- bands$bands[bands$bands$animal_id == "ST-02", ]
# Keep the detections that carry a depth reading
depth <- dets$detections[!is.na(dets$detections$sensor_value), ]
# Plot depth against time, one panel per animal, the candidate date on ST-02
ggplot2::ggplot(depth, ggplot2::aes(datetime, sensor_value)) +
  ggplot2::geom_point(shape = 16, size = 0.6, colour = "#31688E") +
  ggplot2::geom_vline(data = data.frame(animal_id = "ST-02",
                                        candidate = st02$death_date),
                      ggplot2::aes(xintercept = candidate,
                                   colour = "Candidate death date"),
                      linetype = 2) +
  ggplot2::scale_colour_manual(name = NULL, values = c("Candidate death date" = "red3")) +
  ggplot2::facet_wrap(~animal_id, ncol = 2) +
  ggplot2::scale_y_reverse() +
  ggplot2::labs(x = "Date (UTC)", y = "Depth (m)") +
  theme_positionr() +
  ggplot2::theme(legend.position = "bottom")

# ---- Own rule ----
# Define a stream: dead when the last 48 h of depth vary by under 0.1 m
depth_still <- mortality_rule("depth_still", function(data, settings) {
  # Keep the depth readings; last_hours() keeps an animal's last hours
  depth_readings <- data$readings[data$readings$class == "depth", ]
  last_hours <- function(readings) {
    readings[readings$datetime >= max(readings$datetime) -
               settings$hours * 3600, ]
  }
  # Vote once per animal from the spread of its last hours of depth
  vote_one <- function(animal_readings) {
    sd_m <- stats::sd(last_hours(animal_readings)$value)
    data.frame(animal_id = animal_readings$animal_id[1],
               vote = if (sd_m < settings$sd_m) "dead" else "none",
               detail = sprintf("depth SD %.2f m over the last %d h", sd_m,
                                settings$hours))
  }
  votes <- lapply(split(depth_readings, depth_readings$animal_id), vote_one)
  do.call(rbind, votes)
}, kind = "evidence", settings = list(hours = 48, sd_m = 0.1), label = "depth still")

# ---- Own rule run ----
# Add the stream to the rule set and band the animals again
mort_rules$depth_still <- depth_still
bands <- detect_mortality(dets, rules = mort_rules)
bands$bands[, c("animal_id", "band", "depth_still_vote")]

# ---- Cards ----
# Make the lake outline for the card maps from the depth raster
lake <- terra::rast(system.file("extdata", "stoney_depth.tif",
                                package = "detqc"))
outline <- sf::st_as_sf(terra::as.polygons(terra::ifel(is.na(lake), NA, 1)))
# Write the cards and the index, and open the index in a session
review_dir <- file.path(out_dir, "mortality_review")
dets <- review_mortality(dets, review_dir, bands = bands, open = interactive(),
                         control = list(outline = outline))
dets$cards[, c("animal_id", "band", "bytes", "checked")]

# ---- Dead cards ----
# Write cards for the dead band only, without opening a browser
dead_only <- build_mortality_cards(dets, bands,
                                   file.path(out_dir, "dead_cards"),
                                   which = "dead",
                                   control = list(outline = outline))
dead_only$cards[, c("animal_id", "band", "checked")]

# ---- Stand in calls ----
# Write one row per animal: ST-02 dead or shed at 02:00 UTC on 4 June, the
# rest alive, with the candidate date the card showed
calls <- bands$bands[, c("animal_id", "band")]
calls$call <- ifelse(calls$animal_id == "ST-02", "dead_or_shed", "alive")
calls$candidate_date <- format(as.Date(bands$bands$death_date))
calls$candidate_date[is.na(calls$candidate_date)] <- ""
calls$death_date <- ifelse(calls$call == "dead_or_shed", "2024-06-04", "")
calls$death_time <- ifelse(calls$call == "dead_or_shed", "2024-06-04 02:00:00",
                           "")
calls$date_source <- ifelse(calls$call == "dead_or_shed", "typed", "")
calls$note <- ifelse(calls$call == "dead_or_shed",
                     "depth flat from 4 June; one station", "")
# Write the calls file, as the index would export it
calls_file <- file.path(out_dir, "mortality_calls.csv")
write.csv(calls, calls_file, row.names = FALSE)
calls[, c("animal_id", "call", "death_date", "death_time", "date_source")]

# ---- Apply ----
# Store the calls in the detection set
dets <- apply_mortality_calls(dets, calls_file)
dets$mortality[, c("animal_id", "call", "death_date", "death_time", "date_source")]

# ---- Fates ----
# Print the simulated fate of each animal
example_mortality$fates

# ---- Margin ----
# Count detections removed with margins of 7 days (the default) and 1 day
sapply(c(margin_7_days = 7, margin_1_day = 1), function(days) {
  cut <- filter_mortality(dets, margin_days = days)
  nrow(dets$detections) - nrow(cut$detections)
})

# ---- Filter ----
# Remove ST-02's detections from 1 day before its death time
dets <- filter_mortality(dets, margin_days = 1)
tail(detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")], 2)

# ---- New data ----
# Make three detections of ST-02 at S014 on 9 June
new_dets <- detections[rep(which(detections$animal_id == "ST-02")[1], 3), ]
new_dets$datetime <- as.POSIXct("2024-06-09 10:00:00", tz = "UTC") +
  c(0, 150, 290)
new_dets$receiver_serial <- "R014"
new_dets$station_id <- "S014"
# Rebuild the detection set with the new detections
dets_new <- detset(rbind(detections, new_dets), deployments, animals)

# ---- Reconcile ----
# Apply the same calls, band again, and list the calls the new data question
dets_new <- apply_mortality_calls(dets_new, calls_file)
dets_new <- reconcile_mortality(dets_new,
                                detect_mortality(dets_new, rules = mort_rules))
dets_new$reconcile[, c("animal_id", "call", "band", "needs_review",
                       "reason")]
