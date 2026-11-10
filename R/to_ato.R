# ---- Export to the trackyverse ATO ----

#' Convert a detset to the tables of the trackyverse ATO
#'
#' Returns the detset as the four ATO tables, `det`, `dep`, `tag` and `ani`.
#' Each table carries every column of ATO's own template for it (ATO
#' 0.0.0.9006, `R/ATO_class.R`), in the template's order, then the mandatory
#' `tz` column set to `"UTC"`, then the detqc columns ATO does not name, as
#' ATO allows. Template columns detqc has no value for are `NA`, and `valid`
#' is `TRUE`. The mapping: `animal_id` becomes `animal`
#' in `ani` and `tag`; `station_id` becomes `deploy_location`; `tag_model`,
#' `tag_serial` and `tag_life_days` become `model`, `serial` and
#' `battery_life`; and the delay range becomes `ping_rate`, the midpoint, and
#' `ping_variation`, the half-width. `sensor_unit` moves from the detections
#' to `tag`. ATO reads `power_level` as a number: a power level given as
#' text, such as "H", is `NA` there, with a warning, and is kept as text in
#' the extra column `power_level_text` of `tag`, present only when such a
#' value exists. In `det`, ATO creates `animal` itself when it matches detections
#' to tags, so the detection's own animal stays in the extra column
#' `animal_id`. `release_location` in `ani` names the release site by its
#' position, `"R1"`, `"R2"` and so on, as [to_actel()] names them; it is `NA`
#' for an animal with no release position.
#'
#' `as = "ATO"`, which would build ATO's S4 object, stops: detqc does not
#' build that object. Build it from these tables
#' with ATO's `make_ani()`, `make_dep()`, `make_det()`, `make_tag()` and
#' `init_ato()`.
#'
#' **Deployments.** ATO requires a recovery time, and a deployment that ends
#' before it starts or overlaps the next one of its receiver is not a
#' deployment ATO can read. So a deployment still in the water ends at the
#' next deployment of its receiver, or, when there is none, one second after
#' the last detection in the set, so that the last detection falls inside it
#' under the contract's half-open rule. A deployment that runs past the next
#' deployment of its receiver ends where that one starts, as [detset()]
#' gives the overlap to the later one. Only the deployments that overlap the
#' detections, from the first to the last, are exported. The attribute
#' `open_deployments` counts the open deployments, and `deployments` counts
#' each kind; a warning gives the counts when a deployment was trimmed or
#' left out. [to_actel()] uses the same rule. No detection is removed.
#'
#' @param x A `detset`.
#' @param as `"tables"` for a list of data frames. `"ATO"` stops, as above.
#' @return A list of the data frames `det`, `dep`, `tag` and `ani`, with the
#'   attributes `open_deployments` and `deployments`.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' ato_tables <- to_ato(dets)
#' names(ato_tables)
#' head(ato_tables$det)
to_ato <- function(x, as = c("tables", "ATO")) {
  validate_detset(x)
  as <- check_choice(as, c("tables", "ATO"), "as")
  if (as == "ATO") {
    stop_detqc(paste("as = \"ATO\" is not supported. Build the object from the tables with",
                     "ATO's make_ani(), make_dep(), make_det(), make_tag() and init_ato()."))
  }
  d <- x$detections
  n <- nrow(d)
  det <- data.frame(datetime = d$datetime, frac_second = rep(NA_real_, n),
                    receiver_serial = d$receiver_serial, transmitter = d$transmitter,
                    sensor_value = ato_col(d, "sensor_value", NA_real_), valid = rep(TRUE, n),
                    tz = rep("UTC", n), deploy_location = d$station_id,
                    sensor_raw = ato_col(d, "sensor_raw", NA_real_), animal_id = d$animal_id,
                    stringsAsFactors = FALSE)
  bounded <- bounded_deployments(x$deployments, d, "to_ato")
  tabs <- list(det = det, dep = ato_deployments(bounded), tag = ato_tags(x$animals, d),
               ani = ato_animals(x$animals))
  attr(tabs, "open_deployments") <- attr(bounded, "counts")[["open"]]
  attr(tabs, "deployments") <- attr(bounded, "counts")
  tabs
}

ato_col <- function(df, nm, na) {
  if (!is.null(df) && nm %in% names(df)) df[[nm]] else rep(na, NROW(df))
}

ato_deployments <- function(dep) {
  n <- nrow(dep)
  na_chr <- rep(NA_character_, n)
  na_num <- rep(NA_real_, n)
  data.frame(deploy_datetime = dep$deploy_datetime, recover_datetime = dep$recover_datetime,
             deploy_location = dep$station_id, deploy_lat = dep$deploy_lat,
             deploy_lon = dep$deploy_lon, deploy_z = ato_col(dep, "deploy_z", NA_real_),
             recover_lat = na_num, recover_lon = na_num, receiver_manufacturer = na_chr,
             receiver_model = ato_col(dep, "receiver_model", NA_character_),
             receiver_serial = dep$receiver_serial, receiver_codeset = na_chr,
             transmitter = na_chr, transmitter_manufacturer = na_chr,
             transmitter_ping_rate = na_num, transmitter_model = na_chr,
             transmitter_serial = na_chr, valid = rep(TRUE, n), tz = rep("UTC", n),
             stringsAsFactors = FALSE)
}

# Sensor units sit on the detections in a detset and on the tag in ATO: a
# transmitter's unit is the one its calibrated readings carry, or the one
# the animals table gives.
ato_tags <- function(ani, d) {
  if (is.null(ani)) {
    ani <- data.frame(transmitter = unique(d$transmitter), animal_id = NA_character_)
  }
  n <- nrow(ani)
  lo <- ato_col(ani, "min_delay", NA_real_)
  hi <- ato_col(ani, "max_delay", NA_real_)
  unit <- ato_col(ani, "sensor_unit", NA_character_)
  if ("sensor_unit" %in% names(d)) {
    seen <- d[!is.na(d$sensor_unit), c("transmitter", "sensor_unit")]
    from_det <- seen$sensor_unit[match(ani$transmitter, seen$transmitter)]
    unit[is.na(unit)] <- from_det[is.na(unit)]
  }
  power <- ato_col(ani, "power_level", NA_real_)
  out <- data.frame(manufacturer = rep(NA_character_, n),
                    model = ato_col(ani, "tag_model", NA_character_),
                    power_level = ato_power(power),
                    ping_rate = (lo + hi) / 2, ping_variation = (hi - lo) / 2,
                    serial = ato_col(ani, "tag_serial", NA_character_),
                    transmitter = ani$transmitter,
                    activation_datetime = empty_utc(n),
                    battery_life = ato_col(ani, "tag_life_days", NA_real_),
                    sensor_type = ato_col(ani, "sensor_type", NA_character_), sensor_unit = unit,
                    animal = ani$animal_id, valid = rep(TRUE, n), tz = rep("UTC", n),
                    sensor_slope = ato_col(ani, "sensor_slope", NA_real_),
                    sensor_intercept = ato_col(ani, "sensor_intercept", NA_real_),
                    stringsAsFactors = FALSE)
  text <- power_text(power)
  if (any(!is.na(text))) out$power_level_text <- text
  out
}

# ATO reads power_level as a number. A tag sheet writes the step as text,
# "L" or "H". A bare coercion would give NA with a warning on every tag.
# The number column stays NA, as ATO reads it, and the text is kept in the
# extra column power_level_text.
ato_power <- function(v) {
  num <- suppressWarnings(as.numeric(v))
  text <- !is.na(v) & is.na(num)
  if (any(text)) {
    warn_detqc(paste("to_ato(): %s of %s %s text, such as \"%s\", and ATO reads a",
                     "number, so %s NA in power_level and kept as text in",
                     "power_level_text of the tag table."), fmt_n(sum(text)),
               n_of(length(v), "power level"), verb(sum(text), "is", "are"),
               as.character(v[text][1]), verb(sum(text), "it is", "they are"))
  }
  num
}

power_text <- function(v) {
  num <- suppressWarnings(as.numeric(v))
  ifelse(!is.na(v) & is.na(num), as.character(v), NA_character_)
}

ato_animals <- function(ani) {
  if (is.null(ani)) ani <- data.frame(animal_id = character(), transmitter = character())
  first <- !duplicated(ani$animal_id)
  a <- ani[first, , drop = FALSE]
  n <- nrow(a)
  data.frame(animal = a$animal_id, release_location = release_sites(a)$site_of,
             release_datetime = ato_col(a, "release_datetime", empty_utc(1)),
             release_lat = ato_col(a, "release_lat", NA_real_),
             release_lon = ato_col(a, "release_lon", NA_real_),
             capture_location = rep(NA_character_, n), capture_datetime = empty_utc(n),
             capture_lat = rep(NA_real_, n), capture_lon = rep(NA_real_, n),
             valid = rep(TRUE, n), tz = rep("UTC", n),
             species = ato_col(a, "species", NA_character_), stringsAsFactors = FALSE)
}
