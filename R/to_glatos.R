# ---- Export to glatos ----

#' Convert a detset to glatos detections
#'
#' Returns the detections in the form glatos 0.9.8 reads: a data frame of
#' class `glatos_detections` with the four columns its validator requires
#' (`animal_id`, `detection_timestamp_utc`, `deploy_lat`, `deploy_long`) and
#' the columns its filters and summaries use (`transmitter_codespace`,
#' `transmitter_id`, `receiver_sn`, `glatos_array`, `station_no`,
#' `station`, `sensor_value`, `sensor_unit`). glatos need not be installed.
#'
#' glatos groups detections by `glatos_array` by default, as in
#' `glatos::detection_events()`. Detections read from a GLATOS export keep
#' their own `glatos_array` and `station_no`, so glatos groups them as it
#' groups the export. Where a detection has none, as a
#' lake array read from VUE or VDAT files does, `glatos_array` and
#' `station_no` hold `station_id`; give `location_col = "station"` to glatos
#' functions, or fill `glatos_array` with your own array codes. `station`
#' holds `station_id`.
#'
#' Positions come from the deployment that holds each detection under the
#' half-open rule, or from the detection's own `deploy_lat` and `deploy_lon`
#' where no deployment holds it. Raw sensor readings go to `sensor_value` with
#' the unit `"ADC"`, as GLATOS exports write them, where no calibrated value
#' exists. Animal columns (`common_name_e`, `tag_model`,
#' `utc_release_date_time`, `release_latitude`, `release_longitude`) come from
#' the animals table. No row is removed.
#'
#' Pass the result to glatos functions in memory. Written to a CSV, it is
#' not a GLATOS export: `glatos::read_glatos_detections()` identifies a file
#' by the full header of an export and stops on this one with "Detection file
#' version could not be identified". Read such a CSV back with
#' [read_detset_from()], or with `data.table::fread()` and
#' `glatos::as_glatos_detections()`.
#'
#' @param x A `detset`.
#' @return A `glatos_detections` data frame, one row per detection, ordered as
#'   `x$detections`.
#' @seealso [read_detset_from()] for the way back.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' glatos_dets <- to_glatos(dets)
#' class(glatos_dets)
#' head(glatos_dets[, c("animal_id", "detection_timestamp_utc", "transmitter_id",
#'            "receiver_sn")])
to_glatos <- function(x) {
  validate_detset(x)
  d <- x$detections
  tx <- split_transmitter(d$transmitter)
  pos <- detection_positions(x)
  value <- if ("sensor_value" %in% names(d)) d$sensor_value else rep(NA_real_, nrow(d))
  unit <- if ("sensor_unit" %in% names(d)) d$sensor_unit else rep(NA_character_, nrow(d))
  raw <- if ("sensor_raw" %in% names(d)) d$sensor_raw else rep(NA_real_, nrow(d))
  use_raw <- is.na(value) & !is.na(raw)
  value[use_raw] <- raw[use_raw]
  unit[use_raw] <- "ADC"
  own <- function(nm) {
    v <- if (nm %in% names(d)) as.character(d[[nm]]) else rep(NA_character_, nrow(d))
    data.table::fifelse(is.na(v) | !nzchar(v), d$station_id, v)
  }
  out <- data.frame(
    animal_id = d$animal_id, detection_timestamp_utc = d$datetime,
    glatos_array = own("glatos_array"), station_no = own("station_no"), station = d$station_id,
    transmitter_codespace = tx$codespace, transmitter_id = tx$id,
    receiver_sn = d$receiver_serial, deploy_lat = pos$lat, deploy_long = pos$lon,
    sensor_value = as.numeric(value), sensor_unit = unit, stringsAsFactors = FALSE
  )
  out <- cbind(out, glatos_animal_columns(x$animals, d))
  rownames(out) <- NULL
  class(out) <- c("glatos_detections", "data.frame")
  out
}

# "A69-9001-1234" is code space "A69-9001" and ID "1234": the ID is the part
# after the last dash.
split_transmitter <- function(tx) {
  tx <- as.character(tx)
  has <- !is.na(tx) & grepl("-", tx, fixed = TRUE)
  cs <- rep(NA_character_, length(tx))
  id <- tx
  cs[has] <- sub("-[^-]*$", "", tx[has])
  id[has] <- sub("^.*-", "", tx[has])
  list(codespace = cs, id = id)
}

# Latitude and longitude of the deployment holding each detection.
detection_positions <- function(x) {
  d <- x$detections
  dep <- x$deployments
  row <- match_deployment(d, dep)
  lat <- dep$deploy_lat[row]
  lon <- dep$deploy_lon[row]
  if (all(c("deploy_lat", "deploy_lon") %in% names(d))) {
    lat[is.na(row)] <- d$deploy_lat[is.na(row)]
    lon[is.na(row)] <- d$deploy_lon[is.na(row)]
  }
  list(lat = as.numeric(lat), lon = as.numeric(lon), row = row)
}

glatos_animal_columns <- function(ani, d) {
  n <- nrow(d)
  out <- data.frame(common_name_e = rep(NA_character_, n), tag_model = rep(NA_character_, n),
                    utc_release_date_time = empty_utc(n), release_latitude = rep(NA_real_, n),
                    release_longitude = rep(NA_real_, n), stringsAsFactors = FALSE)
  if (is.null(ani) || !n) return(out)
  k <- match(paste(d$animal_id, d$transmitter), paste(ani$animal_id, ani$transmitter))
  take <- c(common_name_e = "species", tag_model = "tag_model",
            utc_release_date_time = "release_datetime", release_latitude = "release_lat",
            release_longitude = "release_lon")
  for (nm in names(take)) {
    if (take[[nm]] %in% names(ani)) out[[nm]] <- ani[[take[[nm]]]][k]
  }
  out
}
