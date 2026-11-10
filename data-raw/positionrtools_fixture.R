# ---- Convert positionRtools example data to the table contract ----
# Run from the package root with the path of a positionRtools clone at the
# commit that `source` records below:
#   Rscript data-raw/positionrtools_fixture.R /path/to/positionRtools
# Writes tests/testthat/fixtures/positionrtools_example.rds (30 synthetic
# animals, 5 planted dead). The data are positionRtools' synthetic example
# data, GPL (>= 3); their generator is largely the work of Paul Bzonek, who
# is credited as a contributor in DESCRIPTION.
args <- commandArgs(trailingOnly = TRUE)
src <- args[1]
stopifnot(dir.exists(src))
e <- new.env()
for (f in list.files(file.path(src, "data"), full.names = TRUE)) load(f, envir = e)
det <- e$example_detections
dep <- e$example_deployments
ani <- e$example_animals
unit <- ifelse(det$sensor_unit %in% c("deg C"), "C", det$sensor_unit)
detections <- data.frame(
  datetime = det$detection_datetime_utc, receiver_serial = det$receiver_sn,
  transmitter = det$transmitter_id, animal_id = det$animal_id, station_id = det$station_id,
  sensor_raw = NA_real_, sensor_value = det$sensor_value,
  sensor_unit = ifelse(is.na(det$sensor_value), NA_character_, unit),
  sensor_type = det$sensor_type, stringsAsFactors = FALSE
)
deployments <- data.frame(
  receiver_serial = dep$receiver_sn, station_id = dep$station_id,
  deploy_datetime = dep$deploy_datetime_utc, recover_datetime = dep$recover_datetime_utc,
  deploy_lat = dep$deploy_lat, deploy_lon = dep$deploy_lon, stringsAsFactors = FALSE
)
animals <- data.frame(
  animal_id = ani$animal_id, transmitter = ani$transmitter_id,
  release_datetime = ani$release_datetime_utc, tag_model = ani$tag_model,
  tag_life_days = ani$tag_life_days, species = ani$species_common,
  sensor_type = ani$sensor_type, stringsAsFactors = FALSE
)
planted_dead <- ani$animal_id[c(4L, 9L, 14L, 19L, 24L)]
out <- list(detections = detections, deployments = deployments, animals = animals,
            planted_dead = planted_dead, source = "positionRtools c532813 data/*.rda")
dir.create("tests/testthat/fixtures", showWarnings = FALSE, recursive = TRUE)
saveRDS(out, "tests/testthat/fixtures/positionrtools_example.rds", compress = "xz")
cat("detections", nrow(detections), "animals", nrow(animals), "planted dead",
    length(planted_dead), "\n")
print(file.size(list.files("tests/testthat/fixtures", full.names = TRUE)))
