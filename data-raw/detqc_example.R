# ---- Build detqc_example and example_mortality from the Stoney Lake files ----
# Run from the package root: Rscript data-raw/detqc_example.R
# Both datasets are the Stoney Lake study that data-raw/stoney_example.R
# writes to inst/extdata/stoney_*: four walleye tracked from 2 to 10 June 2024 by 20
# receivers. They are read here with the package's own readers, so a dataset
# and the files always agree. detqc_example is the study as read, with raw
# sensor counts. example_mortality is the study prepared as in
# vignette("mortality-review"): sensors calibrated, the first six hours after
# release and isolated detections removed, with the true fate of each animal.
# The synthetic hard-case benches of the tests are built in
# tests/testthat/helper-hardcases.R. Needs pkgload.

# ---- Libraries ----
pkgload::load_all(".", quiet = TRUE)

# ---- Read the Stoney files ----
in_file <- function(name) file.path("inst", "extdata", name)
specs <- read_tag_specs(in_file("stoney_tag_specs.csv"))
det <- read_detections(in_file("stoney_detections.csv"))
dep <- read_deployments(in_file("stoney_receivers.csv"), format = "glatos")
ani <- read_animals(in_file("stoney_detections.csv"), format = "glatos", tag_specs = specs)
nrow(det)  # detections
nrow(dep)  # deployments
nrow(ani)  # transmitters

# ---- Keep the contract columns ----
# The readers keep every other column of a GLATOS export; the datasets keep
# the columns the contract names, and drop the reader's attributes.
plain <- function(d, cols) {
  d <- as.data.frame(d)[, cols]
  attr(d, "source") <- NULL
  rownames(d) <- NULL
  d
}
det_cols <- c("datetime", "receiver_serial", "transmitter", "animal_id", "station_id",
              "sensor_raw", "sensor_value", "sensor_unit")
dep_cols <- c("receiver_serial", "station_id", "deploy_datetime", "recover_datetime",
              "deploy_lat", "deploy_lon", "deploy_z")
ani <- ani[order(ani$animal_id), ]
detqc_example <- list(detections = plain(det, det_cols), deployments = plain(dep, dep_cols),
                      animals = plain(ani, names(ani)))
sapply(detqc_example, nrow)

# ---- The true fates ----
# ST-02 dies at 02:00 UTC on 4 June 2024, at x 725079, y 4937943 (UTM zone
# 17N), the first position after 4 June inside the window stoney_example.R
# sets (its run prints "ST-02 dies at 2024-06-04 02:00:00 UTC"). The other
# three live to the end of the record.
death <- as.POSIXct("2024-06-04 02:00:00", tz = "UTC")
fates <- data.frame(animal_id = sprintf("ST-%02d", 1:4),
                    fate = c("alive", "dead", "alive", "alive"),
                    death_datetime = death[c(NA, 1, NA, NA)])
# The dead tag is heard at one station only after its death.
after <- detqc_example$detections[detqc_example$detections$animal_id == "ST-02" &
                                    detqc_example$detections$datetime > death, ]
table(after$station_id)
stopifnot(length(unique(after$station_id)) == 1)

# ---- Prepared for the mortality rules ----
x <- detset(detqc_example$detections, detqc_example$deployments, detqc_example$animals)
x <- calibrate_sensors(x)
x <- filter_post_release(x, hours = 6)
x <- filter_min_lag(x, max_lag_s = 4500)
detset_log(x)[, c("step", "rows_before", "rows_after", "removed")]
example_mortality <- list(detections = plain(x$detections, det_cols),
                          deployments = detqc_example$deployments,
                          animals = detqc_example$animals, fates = fates)
sapply(example_mortality, nrow)

# ---- Save ----
save(detqc_example, file = "data/detqc_example.rda", compress = "xz")
save(example_mortality, file = "data/example_mortality.rda", compress = "xz")
file.size(c("data/detqc_example.rda", "data/example_mortality.rda"))  # bytes
