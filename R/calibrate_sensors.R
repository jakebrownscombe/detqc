# ---- Sensor calibration: raw counts to physical values ----

#' Convert raw sensor readings to physical values
#'
#' Converts each detection's `sensor_raw` to `sensor_value` and
#' `sensor_unit` with the calibration of its transmitter:
#' `sensor_value = sensor_intercept + sensor_slope * sensor_raw`. The
#' calibration is read from the animals and tags table of `x` (columns
#' `sensor_slope`, `sensor_intercept` and `sensor_unit`) and, for
#' transmitters it does not calibrate, from `specs`, such as the result of
#' [read_tag_specs()].
#'
#' A reading is left as it is, and counted, when its transmitter has no
#' calibration, when the calibration has no unit, when the reading is not a
#' whole count of zero or more (it looks calibrated already, and a warning
#' says so), or when the reading already has a `sensor_value`, as OTN extracts carry for tags whose
#' specification OTN holds. `overwrite = TRUE` recomputes those values from
#' `sensor_raw`. No detection is removed. The log records the readings by
#' outcome and the transmitters with readings but no calibration.
#'
#' Calibrations taken from `specs` are also written into the animals table,
#' for transmitters it lists, so that the table records what was used. So is
#' the sensor type of a transmitter with no calibration, such as a predation
#' tag, so that [detect_mortality()] knows what the sensor is.
#'
#' @param x A `detset`.
#' @param specs Optional data frame with `transmitter`, `sensor_slope`,
#'   `sensor_intercept` and `sensor_unit`, and optionally `sensor_type`, one
#'   row per transmitter.
#' @param overwrite If `TRUE`, readings that already have a `sensor_value` are
#'   recomputed when their transmitter has a calibration.
#' @return `x` with `sensor_value` and `sensor_unit` filled where a
#'   calibration exists, and one log record.
#' @seealso [read_tag_specs()], [detset_log()]
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' specs <- read_tag_specs(system.file("extdata", "stoney_tag_specs.csv",
#'                                     package = "detqc"))
#' dets <- calibrate_sensors(dets, specs)
#' writeLines(strwrap(detset_log(dets)$reason[2], width = 72))
#' summary(dets$detections$sensor_value[dets$detections$sensor_unit %in% "m"])
calibrate_sensors <- function(x, specs = NULL, overwrite = FALSE) {
  validate_detset(x)
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop_detqc("`overwrite` must be TRUE or FALSE.")
  }
  before <- x
  cal <- calibration_sources(x$animals, specs)
  d <- x$detections
  raw <- if ("sensor_raw" %in% names(d)) as.numeric(d$sensor_raw) else rep(NA_real_, nrow(d))
  if (!"sensor_value" %in% names(d)) d$sensor_value <- rep(NA_real_, nrow(d))
  if (!"sensor_unit" %in% names(d)) d$sensor_unit <- rep(NA_character_, nrow(d))
  k <- match(d$transmitter, cal$transmitter)
  has_raw <- !is.na(raw)
  had_value <- has_raw & !is.na(d$sensor_value)
  usable <- !is.na(k) & !is.na(cal$slope[k]) & !is.na(cal$intercept[k])
  no_unit <- has_raw & usable & is.na(cal$unit[k])
  not_count <- warn_not_counts(d$transmitter, raw, has_raw & usable)
  todo <- has_raw & usable & !no_unit & !not_count & (overwrite | !had_value)
  d$sensor_value[todo] <- cal$intercept[k[todo]] + cal$slope[k[todo]] * raw[todo]
  d$sensor_unit[todo] <- cal$unit[k[todo]]
  x$detections <- d
  # Assigning NULL would drop the part, so a detset with no animals keeps it.
  if (!is.null(x$animals)) {
    x$animals <- fill_animal_calibration(x$animals, cal)
    x$animals <- fill_sensor_type(x$animals, specs)
  }
  counts <- list(raw = sum(has_raw), done = sum(todo), from_specs = sum(todo & cal$from_specs[k]),
                 kept = sum(had_value & !todo & !not_count), no_unit = sum(no_unit),
                 not_count = sum(not_count),
                 none = sum(has_raw & !usable),
                 tx_none = length(unique(d$transmitter[has_raw & !usable])))
  reason <- calibration_reason(counts)
  params <- list(specs = if (is.null(specs)) "none" else specs, overwrite = overwrite)
  x <- log_step(x, "calibrate_sensors", params, before, 0L, reason, fn = "calibrate_sensors")
  validate_detset(x)
}

calibration_reason <- function(n) {
  sprintf(paste(
    "%s of %s calibrated (%s with a calibration from specs);",
    "kept, with a sensor_value already: %s; with a calibration but no unit: %s;",
    "with no calibration: %s (%s); not whole counts of zero or more, so not",
    "calibrated: %s; sensor_value left NA where not calibrated"
  ), fmt_n(n$done), n_of(n$raw, "raw sensor reading"), fmt_n(n$from_specs), fmt_n(n$kept),
  fmt_n(n$no_unit), fmt_n(n$none), n_of(n$tx_none, "transmitter"), fmt_n(n$not_count))
}

# A raw count is a whole number of zero or more. A negative or fractional
# sensor_raw is a value already calibrated, and calibrating it again gives a
# wrong number with a clean log; it is left alone and counted.
warn_not_counts <- function(transmitter, raw, consider) {
  odd <- consider & (raw < 0 | raw != round(raw))
  if (any(odd)) {
    warn_detqc(paste("%s sensor_raw readings, such as %g on %s, are not whole counts of zero or",
                     "more: they look calibrated already and were not calibrated again."),
               fmt_n(sum(odd)), raw[odd][1], transmitter[odd][1])
  }
  odd
}

# One calibration per transmitter: the animals table first, then `specs` for
# transmitters the animals table leaves uncalibrated.
calibration_sources <- function(ani, specs) {
  one <- function(tbl, from_specs) {
    if (is.null(tbl) || !nrow(tbl)) return(NULL)
    get <- function(nm, na) if (nm %in% names(tbl)) tbl[[nm]] else rep(na, nrow(tbl))
    unit <- get("sensor_unit", NA_character_)
    out <- data.frame(
      transmitter = as.character(tbl$transmitter),
      slope = as.numeric(get("sensor_slope", NA_real_)),
      intercept = as.numeric(get("sensor_intercept", NA_real_)),
      unit = unit_key(unit), type = as.character(get("sensor_type", NA_character_)),
      from_specs = from_specs, stringsAsFactors = FALSE
    )
    out$unit[is.na(out$unit)] <- unit_key(out$type)[is.na(out$unit)]
    out[!is.na(out$slope) & !is.na(out$intercept), , drop = FALSE]
  }
  if (!is.null(specs)) {
    if (!is.data.frame(specs) || !"transmitter" %in% names(specs)) {
      stop_detqc("`specs` must be a data frame with a `transmitter` column.")
    }
  }
  both <- rbind(one(ani, FALSE), one(specs, TRUE))
  if (is.null(both)) {
    both <- data.frame(transmitter = character(), slope = numeric(), intercept = numeric(),
                       unit = character(), type = character(), from_specs = logical())
  }
  both[!duplicated(both$transmitter), , drop = FALSE]
}

# A sensor with no calibration still has a class in the specifications,
# such as InnovaSea's V7D "predation" tag. Without it the triage would read
# the tag as a sensor of unknown class.
fill_sensor_type <- function(ani, specs) {
  if (is.null(specs) || !"sensor_type" %in% names(specs)) return(ani)
  if (!"sensor_type" %in% names(ani)) ani$sensor_type <- NA_character_
  k <- match(ani$transmitter, specs$transmitter)
  fill <- is.na(ani$sensor_type) & !is.na(k)
  ani$sensor_type[fill] <- as.character(specs$sensor_type[k[fill]])
  ani
}

fill_animal_calibration <- function(ani, cal) {
  use <- cal[cal$from_specs, , drop = FALSE]
  k <- match(ani$transmitter, use$transmitter)
  hit <- !is.na(k)
  if (!any(hit)) return(ani)
  cols <- c(sensor_slope = "slope", sensor_intercept = "intercept", sensor_unit = "unit",
            sensor_type = "type")
  for (nm in names(cols)) {
    if (!nm %in% names(ani)) ani[[nm]] <- if (nm %in% c("sensor_slope", "sensor_intercept")) {
      NA_real_
    } else {
      NA_character_
    }
    fill <- hit & is.na(ani[[nm]])
    ani[[nm]][fill] <- use[[cols[[nm]]]][k[fill]]
  }
  ani
}
