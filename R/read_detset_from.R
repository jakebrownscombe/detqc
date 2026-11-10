# ---- Build a detset from a glatos or actel object ----

#' Build a detset from glatos detections or an actel preload
#'
#' Reads detections that another package holds into a `detset`, so work
#' started in glatos or actel can continue here.
#'
#' **glatos.** `x` is a `glatos_detections` data frame, or any data frame
#' with its columns `detection_timestamp_utc`, `receiver_sn`,
#' `transmitter_codespace`, `transmitter_id` and `animal_id`. `station_id`
#' is `station` where present, else `glatos_array`. The animals table is
#' built from the animal columns GLATOS exports repeat on every row.
#' Deployments come from `receivers`, a glatos receiver table (as from
#' `glatos::read_glatos_receivers()`). Without it they are inferred: one per
#' receiver and station, from its first detection to one second after its
#' last, at the first position the detections give. Inferred deployments say
#' nothing about when a receiver listened without hearing anything, so the
#' log says they were inferred.
#'
#' **actel.** `x` is the list `actel::preload()` returns. Detections are the
#' ones actel kept, those of the tags in its biometrics; actel's time stamps
#' are moved back to UTC from its study area time zone. `animal_id` is the
#' biometrics' `animal_id` column where it has one, as [to_actel()] writes,
#' and the transmitter code otherwise. actel keeps receivers as whole
#' numbers, and names a receiver deployed more than once `"number.dpl.k"`
#' for its k-th deployment; the suffix is dropped on the way back. Give
#' `receivers = attr(a, "receivers")`, the lookup that [to_actel()] returned
#' as `a`, to restore `receiver_serial` and `receiver_model`. Without it
#' `receiver_serial` holds actel's number, and the log says so.
#'
#' The first log record is the one [detset()] writes; a second, from this
#' function, names the source and its row counts.
#'
#' @param x A `glatos_detections` data frame or an actel preload list.
#' @param receivers Optional. With glatos input, a glatos receiver table.
#'   With actel input, the `receivers` attribute of the [to_actel()] result.
#' @return A `detset`.
#' @seealso [to_glatos()], [to_actel()]
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' glatos_dets <- to_glatos(dets)
#' from_glatos <- read_detset_from(glatos_dets)
#' writeLines(strwrap(detset_log(from_glatos)$reason[2], width = 72))
read_detset_from <- function(x, receivers = NULL) {
  if (is_actel_preload(x)) {
    tabs <- from_actel(x, receivers)
    src <- "an actel preload"
  } else if (is.data.frame(x) && all(c("detection_timestamp_utc", "receiver_sn") %in% names(x))) {
    tabs <- from_glatos(x, receivers)
    src <- "glatos detections"
  } else {
    stop_detqc(paste("`x` must be glatos detections (a data frame with",
                     "detection_timestamp_utc and receiver_sn) or the list actel::preload()",
                     "returns."))
  }
  out <- detset(tabs$detections, tabs$deployments, tabs$animals)
  reason <- sprintf("read from %s: %s, %s (%s), %s",
                    src, n_of(nrow(tabs$detections), "detection"),
                    n_of(nrow(tabs$deployments), "deployment"), tabs$dep_note,
                    n_of(NROW(tabs$animals), "animal row"))
  log_step(out, "read_detset_from", list(source = src), out, 0L, reason,
           fn = "read_detset_from")
}

is_actel_preload <- function(x) {
  is.list(x) && !is.data.frame(x) &&
    all(c("bio", "deployments", "spatial", "detections.list", "tz") %in% names(x))
}

# ---- Internal: glatos ----
from_glatos <- function(g, receivers) {
  g <- as.data.frame(g, stringsAsFactors = FALSE)
  pick <- function(nm, na = NA_character_) if (nm %in% names(g)) g[[nm]] else rep(na, nrow(g))
  station <- pick("station")
  station[is.na(station)] <- pick("glatos_array")[is.na(station)]
  det <- data.frame(
    datetime = lubridate::with_tz(g$detection_timestamp_utc, "UTC"),
    receiver_serial = as.character(g$receiver_sn),
    transmitter = paste(g$transmitter_codespace, g$transmitter_id, sep = "-"),
    animal_id = as.character(pick("animal_id")), station_id = as.character(station),
    sensor_value = as.numeric(pick("sensor_value", NA_real_)),
    sensor_unit = as.character(pick("sensor_unit")),
    deploy_lat = as.numeric(pick("deploy_lat", NA_real_)),
    deploy_lon = as.numeric(pick("deploy_long", NA_real_)), stringsAsFactors = FALSE
  )
  det <- split_raw_sensors(det)
  if (is.null(receivers)) {
    dep <- infer_deployments(det)
    note <- "inferred from the detections"
  } else {
    dep <- glatos_receiver_table(receivers)
    note <- "from the receiver table"
  }
  list(detections = det, deployments = dep, animals = glatos_animals(g), dep_note = note)
}

infer_deployments <- function(det) {
  dt <- as_dt(det)[!is.na(receiver_serial)]
  out <- dt[, list(deploy_datetime = min(datetime), recover_datetime = max(datetime) + 1,
                   deploy_lat = deploy_lat[!is.na(deploy_lat)][1],
                   deploy_lon = deploy_lon[!is.na(deploy_lon)][1]),
            by = list(receiver_serial, station_id)]
  as_df(out[!is.na(deploy_lat) & !is.na(deploy_lon)])
}

glatos_receiver_table <- function(r) {
  r <- as.data.frame(r, stringsAsFactors = FALSE)
  need <- c("ins_serial_no", "deploy_date_time", "recover_date_time", "deploy_lat", "deploy_long")
  miss <- setdiff(need, names(r))
  if (length(miss)) stop_detqc("`receivers` has no column `%s`.", miss[1])
  station <- if ("station" %in% names(r)) r$station else r$glatos_array
  utc <- function(v) {
    if (inherits(v, "POSIXct")) return(lubridate::with_tz(v, "UTC"))
    parse_utc(v, "UTC", "receivers", strict = FALSE)
  }
  data.frame(receiver_serial = as.character(r$ins_serial_no), station_id = as.character(station),
             deploy_datetime = utc(r$deploy_date_time), recover_datetime = utc(r$recover_date_time),
             deploy_lat = as.numeric(r$deploy_lat), deploy_lon = as.numeric(r$deploy_long),
             stringsAsFactors = FALSE)[!is.na(r$deploy_date_time), , drop = FALSE]
}

glatos_animals <- function(g) {
  if (!"animal_id" %in% names(g)) return(NULL)
  pick <- function(nm, na = NA_character_) if (nm %in% names(g)) g[[nm]] else rep(na, nrow(g))
  rel <- pick("utc_release_date_time", NA)
  if (!inherits(rel, "POSIXct")) rel <- parse_utc(rel, "UTC", "animals", strict = FALSE)
  a <- data.frame(
    animal_id = as.character(g$animal_id),
    transmitter = paste(g$transmitter_codespace, g$transmitter_id, sep = "-"),
    species = as.character(pick("common_name_e")),
    release_datetime = lubridate::with_tz(rel, "UTC"),
    release_lat = as.numeric(pick("release_latitude", NA_real_)),
    release_lon = as.numeric(pick("release_longitude", NA_real_)),
    tag_model = as.character(pick("tag_model")), stringsAsFactors = FALSE
  )
  a <- a[!is.na(a$animal_id) & !duplicated(paste(a$animal_id, a$transmitter)), , drop = FALSE]
  rownames(a) <- NULL
  a
}

# ---- Internal: actel ----
from_actel <- function(p, receivers = NULL) {
  dl <- data.table::rbindlist(lapply(p$detections.list, as_dt), use.names = TRUE, fill = TRUE)
  bio <- as.data.frame(p$bio, stringsAsFactors = FALSE)
  ani <- actel_animals(bio, p)
  tx <- as.character(dl$Transmitter)
  id <- ani$animal_id[match(tx, ani$transmitter)]
  det <- data.frame(
    datetime = lubridate::with_tz(dl$Timestamp, "UTC"), receiver_serial = actel_serial(dl$Receiver),
    transmitter = tx, animal_id = ifelse(is.na(id), tx, id),
    station_id = as.character(actel_station_of(dl, p)),
    sensor_value = suppressWarnings(as.numeric(dl$Sensor.Value)),
    sensor_unit = as.character(dl$Sensor.Unit), stringsAsFactors = FALSE
  )
  det <- split_raw_sensors(det)
  dep <- actel_deployment_table(p)
  note <- "from actel's deployments; receiver_serial is actel's receiver number"
  if (!is.null(receivers)) {
    det <- restore_receivers(det, receivers)
    dep <- restore_receivers(dep, receivers)
    note <- "from actel's deployments; receiver names restored from the to_actel() lookup"
  }
  list(detections = det, deployments = dep, animals = ani, dep_note = note)
}

restore_receivers <- function(df, rx) {
  if (!is.data.frame(rx) || !all(c("Receiver", "receiver_serial") %in% names(rx))) {
    stop_detqc("With actel input, `receivers` must be the `receivers` attribute of to_actel().")
  }
  k <- match(df$receiver_serial, as.character(rx$Receiver))
  if (anyNA(k[!is.na(df$receiver_serial)])) {
    stop_detqc("actel receiver %s is not in `receivers`.", df$receiver_serial[is.na(k)][1])
  }
  df$receiver_serial <- as.character(rx$receiver_serial[k])
  if ("receiver_model" %in% names(rx)) df$receiver_model <- as.character(rx$receiver_model[k])
  df
}

# actel renames a receiver with several deployments "123456.dpl.2"; the
# lookup of to_actel() holds the bare number.
actel_serial <- function(v) sub("\\.dpl\\.[0-9]+$", "", as.character(v))

actel_station_of <- function(dl, p) {
  st <- p$spatial$stations
  st$Station.name[match(as.character(dl$Standard.name), st$Standard.name)]
}

actel_deployment_table <- function(p) {
  dep <- data.table::rbindlist(lapply(p$deployments, as_dt), use.names = TRUE, fill = TRUE)
  st <- p$spatial$stations
  k <- match(dep$Station.name, st$Station.name)
  data.frame(receiver_serial = actel_serial(dep$Receiver),
             station_id = as.character(dep$Station.name),
             deploy_datetime = lubridate::with_tz(dep$Start, "UTC"),
             recover_datetime = lubridate::with_tz(dep$Stop, "UTC"),
             deploy_lat = as.numeric(st$Latitude[k]), deploy_lon = as.numeric(st$Longitude[k]),
             stringsAsFactors = FALSE)
}

# One row per transmitter: a biometrics row with Signal "4450|4451" carries
# two transmitters of one animal.
actel_animals <- function(bio, p) {
  sig <- strsplit(as.character(bio$Signal), "|", fixed = TRUE)
  cs <- if ("Code.space" %in% names(bio)) as.character(bio$Code.space) else
    sub("-[^-]*$", "", as.character(bio$Transmitter))
  n <- lengths(sig)
  row <- rep(seq_len(nrow(bio)), n)
  tx <- paste(cs[row], unlist(sig), sep = "-")
  id <- if ("animal_id" %in% names(bio)) as.character(bio$animal_id)[row] else tx
  rel <- p$spatial$release.sites
  site <- if ("Release.site" %in% names(bio)) as.character(bio$Release.site)[row] else NA
  k <- match(site, rel$Standard.name)
  data.frame(animal_id = id, transmitter = tx,
             release_datetime = lubridate::with_tz(bio$Release.date[row], "UTC"),
             release_lat = suppressWarnings(as.numeric(rel$Latitude[k])),
             release_lon = suppressWarnings(as.numeric(rel$Longitude[k])),
             stringsAsFactors = FALSE)
}
