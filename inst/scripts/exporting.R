# Code of the vignette "Export the dataset and generate a summary report".
# Extracted from vignettes/exporting.Rmd by tools/purl_vignettes.R, a repository file
# the installed package does not carry. The vignette is the source; this file follows it.

# ---- Load ----
library(detqc)
# Load the Stoney Lake study, read, checked and filtered
dets <- stoney_clean
# Print the set: its tables, its log and its checks
dets

# ---- Out dir ----
# Change to a folder beside your data, such as "C:/telemetry/2024/processed".
out_dir <- tempdir()

# ---- Audit status ----
# Count receiver-days by status, and give the dates the audit covers
table(dets$listening$status)
range(dets$listening$date)

# ---- Detfield ----
# Make the three inputs of detfield::bin_detections()
detfield_input <- as_detfield_input(dets)
names(detfield_input)
# Show the delays of each transmitter, from the tag sheet
detfield_input$tag_delay
# List detected transmitters with no delays: none here
setdiff(unique(dets$detections$transmitter),
        detfield_input$tag_delay$transmitter)

# ---- Listening table ----
# Take the receiver-day table detfield reads, with each receiver's station
listening_days <- detfield_input$listening$days
rx_row <- match(listening_days$receiver_serial, dets$deployments$receiver_serial)
listening_days$station <- dets$deployments$station_id[rx_row]
table(listening_days$status)

# ---- Listening figure ----
# Draw one tile per station and day, coloured by status
ggplot2::ggplot(listening_days, ggplot2::aes(as.Date(date), station,
                                             fill = status)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
  ggplot2::scale_fill_manual(values = c(listening = "#2C5AA0",
                                        silent = "#B8C7DE",
                                        partial = "#E69F00"), name = NULL) +
  ggplot2::scale_x_date(date_labels = "%d %b", date_breaks = "1 day",
                        expand = c(0, 0)) +
  ggplot2::labs(x = "Date (UTC)", y = "Station") +
  theme_positionr() +
  ggplot2::theme(panel.grid = ggplot2::element_blank())

# ---- Glatos ----
# Convert the detections to a glatos_detections data frame
glatos_dets <- to_glatos(dets)
class(glatos_dets)
head(glatos_dets[, c("animal_id", "detection_timestamp_utc", "transmitter_id",
                     "receiver_sn", "glatos_array", "station")], 3)

# ---- Glatos events ----
if (requireNamespace("glatos", quietly = TRUE)) {
  # Compute detection events by station with glatos
  events <- glatos::detection_events(glatos_dets, location_col = "station")
  head(events[, c("animal_id", "location", "first_detection", "last_detection",
                  "num_detections")], 3)
}

# ---- Actel ----
# Make the four actel tables, in the time zone of the study area
tz <- "America/Toronto"
actel_input <- to_actel(dets, tz = tz)
# Count rows per table
vapply(actel_input, nrow, 1L)
head(actel_input$biometrics)
# Show the rows to_actel() left out, and those actel will drop
attr(actel_input, "dropped")
attr(actel_input, "actel_drops")

# ---- Actel preload ----
# Run only when the actel package is installed
if (requireNamespace("actel", quietly = TRUE)) {
  # Load the four tables with actel
  preloaded <- do.call(actel::preload, c(actel_input, list(tz = tz)))
  # Bring the actel result back as a detection set
  from_actel <- read_detset_from(preloaded,
                                 receivers = attr(actel_input, "receivers"))
  from_actel
}

# ---- Ato ----
# Make the four ATO tables
ato <- to_ato(dets)
# Count rows per table, and list the columns of the tag table
vapply(ato, nrow, 1L)
names(ato$tag)

# ---- Files ----
# Write a folder of Parquet files, when arrow is installed
set_path <- file.path(out_dir, "my_detset")
if (requireNamespace("arrow", quietly = TRUE)) {
  write_detset(dets, set_path, format = "parquet", overwrite = TRUE)
  list.files(set_path)
}
# Write one RDS file, and read it back to confirm the detections
write_detset(dets, paste0(set_path, ".rds"), overwrite = TRUE)
nrow(read_detset(paste0(set_path, ".rds"))$detections)

# ---- Csv ----
# Write the times as text with the UTC zone marked
det_table <- dets$detections
det_table$datetime <- format(det_table$datetime, "%Y-%m-%dT%H:%M:%SZ")
write.csv(det_table, file.path(out_dir, "detections.csv"), row.names = FALSE)
head(det_table$datetime, 3)

# ---- Bands ----
# Build the set before the mortality cut from example_mortality
before_cut <- detset(example_mortality$detections,
                     example_mortality$deployments, example_mortality$animals)
# Scale the mortality rules to nine days and band the animals
mort_rules <- mortality_rules(min_days = 3, min_calendar_days = 3,
                              min_flat_days = 2, within_days = 2)
bands <- detect_mortality(before_cut, rules = mort_rules)
bands$bands[, c("animal_id", "band", "deciding_rule")]

# ---- Outline ----
# Locate the example files included with the package
extdata_dir <- system.file("extdata", package = "detqc")
in_file <- function(name) file.path(extdata_dir, name)
# Read the depth raster and make the lake outline from its water cells
lake <- terra::rast(in_file("stoney_depth.tif"))
outline <- sf::st_as_sf(terra::as.polygons(terra::ifel(is.na(lake), NA, 1)))

# ---- Report ----
# Write the report file, and open it in an interactive session
report <- processing_report(dets, file.path(out_dir, "processing_report.html"),
                            bands = bands,
                            control = list(outline = outline))
basename(report)
if (interactive()) utils::browseURL(report)

# ---- Back ----
# Rebuild a detection set from the glatos detections alone
from_glatos <- read_detset_from(glatos_dets)
from_glatos
# Show the log record that says the deployments were inferred
detset_log(from_glatos)$reason[2]
# Rebuild with the glatos receiver table, which keeps every deployment
if (requireNamespace("glatos", quietly = TRUE)) {
  receivers <- glatos::read_glatos_receivers(in_file("stoney_receivers.csv"))
  from_glatos <- read_detset_from(glatos_dets, receivers = receivers)
  from_glatos
}
