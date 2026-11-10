# ---- Summaries of a detection set ----

#' Summarise detections by animal, station, receiver or day
#'
#' Counts detections and what they cover, one row per unit of `by`. Each
#' animal takes one species, so a join on species can never multiply rows;
#' an animal recorded under two species takes the first in the animals table,
#' and the attribute `species_conflicts` names such animals.
#'
#' @param x A [detset()].
#' @param by One of `"animal"`, `"station"`, `"receiver"` or `"day"` (UTC
#'   date).
#' @return A data frame. By animal: `animal_id`, `species`, `transmitters`,
#'   `detections`, `stations`, `days` (UTC dates with a detection), `first`
#'   and `last`; detections with no `animal_id` are left out and counted in
#'   the attribute `no_animal`. When no detection has an `animal_id`, the
#'   table by animal has no row and a message says so. By station or receiver: `detections`,
#'   `animals`, `transmitters`, `days`, `first` and `last`. By day:
#'   `detections`, `animals`, `stations` and `receivers`.
#' @seealso [summarise_animals()], [residency_index()]
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' head(summarise_detections(dets, "animal"))
#' head(summarise_detections(dets, "day"))
summarise_detections <- function(x, by = c("animal", "station", "receiver", "day")) {
  need_detset(x)
  by <- check_choice(by, c("animal", "station", "receiver", "day"), "by")
  summarise_by(x, by, quiet = FALSE)
}

# The advice of every message that finds no detection with an animal_id. With
# no animals table the user must give one; with a table that matched no
# detection the codes in it are wrong, and asking for a table again misleads.
no_animal_advice <- function(x) {
  if (is.null(x$animals) || !nrow(x$animals)) {
    "Give detset() an animals table that names the transmitters."
  } else {
    paste("The animals table names none of the detected transmitters: check its",
          "transmitter codes against the detections.")
  }
}

# summarise_animals() calls this with quiet = TRUE and gives its own message.
summarise_by <- function(x, by, quiet) {
  d <- as_dt(x$detections)
  d[, day := as.Date(datetime)]
  out <- switch(by,
    animal = {
      a <- animal_detection_rows(d)
      if (!nrow(a) && nrow(d) && !quiet) {
        message(sprintf(paste("summarise_detections(): no detection has an animal_id, so",
                              "there is no animal to summarise; %s are left out. %s"),
                        n_of(nrow(d), "detection"), no_animal_advice(x)))
      }
      sp <- one_species(x$animals)
      a[, species := sp$species[match(animal_id, sp$animal_id)]]
      data.table::setcolorder(a, c("animal_id", "species"))
      a
    },
    station = unit_summary(d, "station_id"),
    receiver = unit_summary(d, "receiver_serial"),
    day = d[, list(detections = .N, animals = length(unique(stats::na.omit(animal_id))),
                   stations = length(unique(stats::na.omit(station_id))),
                   receivers = length(unique(receiver_serial))), by = list(date = day)]
  )
  data.table::setorderv(out, names(out)[1])
  out <- as_df(out)
  if (by == "animal") {
    attr(out, "no_animal") <- sum(is.na(d$animal_id))
    attr(out, "species_conflicts") <- one_species(x$animals)$conflicts
  }
  out
}

# One row per animal. With no detection that has an animal_id the grouped
# min() and max() would warn on an empty vector, so the empty table is built.
animal_detection_rows <- function(d) {
  d <- d[!is.na(animal_id)]
  if (!nrow(d)) {
    return(data.table::data.table(animal_id = character(), transmitters = integer(),
                                  detections = integer(), stations = integer(),
                                  days = integer(), first = empty_utc(), last = empty_utc()))
  }
  d[, list(
    transmitters = length(unique(transmitter)), detections = .N,
    stations = length(unique(stats::na.omit(station_id))), days = length(unique(day)),
    first = min(datetime), last = max(datetime)
  ), by = "animal_id"]
}

unit_summary <- function(d, key) {
  d[, list(detections = .N, animals = length(unique(stats::na.omit(animal_id))),
           transmitters = length(unique(transmitter)), days = length(unique(day)),
           first = min(datetime), last = max(datetime)), by = key]
}

# One species per animal: the first non-missing value in table order.
one_species <- function(ani) {
  if (is.null(ani) || !"species" %in% names(ani)) {
    return(list(animal_id = character(), species = character(), conflicts = character()))
  }
  a <- as_dt(ani)[!is.na(species)]
  n <- a[, list(n = length(unique(tolower(trimws(species))))), by = "animal_id"]
  first <- a[!duplicated(animal_id)]
  list(animal_id = first$animal_id, species = first$species,
       conflicts = n$animal_id[n$n > 1])
}

#' Summarise animals, detected or not
#'
#' One row per animal in the animals table, plus any animal that has
#' detections and no record there. Animals never detected keep a row with
#' zero detections, so the denominator of a detection rate is all tagged
#' animals.
#'
#' @param x A [detset()].
#' @return A data frame: `animal_id`, `species` (one per animal),
#'   `transmitters`, `release_datetime` (earliest), `detections`, `days`
#'   (UTC dates with a detection), `stations`, `first`, `last` and
#'   `days_at_liberty` (from release, or the first detection, to the last
#'   detection; `NA` when never detected). When no detection has an
#'   `animal_id`, every animal has 0 detections and a message says so.
#' @seealso [summarise_detections()]
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' head(summarise_animals(dets))
summarise_animals <- function(x) {
  need_detset(x)
  det <- summarise_by(x, "animal", quiet = TRUE)
  if (!nrow(det) && nrow(x$detections)) {
    remedy <- if (is.null(x$animals) || !nrow(x$animals)) " and the table has no row. " else ". "
    remedy <- paste0(remedy, no_animal_advice(x))
    message(sprintf(paste("summarise_animals(): no detection has an animal_id, so %s count",
                          "for no animal%s"),
                    n_of(nrow(x$detections), "detection"), remedy))
  }
  ani <- if (is.null(x$animals)) NULL else as_dt(x$animals)
  ids <- unique(c(if (!is.null(ani)) ani$animal_id, det$animal_id))
  out <- data.table::data.table(animal_id = ids)
  sp <- one_species(x$animals)
  out[, species := sp$species[match(animal_id, sp$animal_id)]]
  tx <- if (is.null(ani)) NULL else ani[, list(n = length(unique(transmitter))), by = "animal_id"]
  out[, transmitters := if (is.null(tx)) NA_integer_ else tx$n[match(animal_id, tx$animal_id)]]
  rel <- if (!is.null(ani) && "release_datetime" %in% names(ani)) {
    ani[!is.na(release_datetime), list(r = min(release_datetime)), by = "animal_id"]
  }
  rel_at <- if (is.null(rel)) empty_utc(nrow(out)) else rel$r[match(out$animal_id, rel$animal_id)]
  out[, release_datetime := rel_at]
  m <- match(out$animal_id, det$animal_id)
  out[, c("detections", "days", "stations") := list(
    data.table::fcoalesce(det$detections[m], 0L), data.table::fcoalesce(det$days[m], 0L),
    data.table::fcoalesce(det$stations[m], 0L)
  )]
  out[, c("first", "last") := list(det$first[m], det$last[m])]
  start <- data.table::fifelse(is.na(out$release_datetime), out$first, out$release_datetime)
  out[, days_at_liberty := as.numeric(last - start, units = "days")]
  data.table::setorderv(out, "animal_id")
  as_df(out)
}

#' Residency index of each animal, at each station or in the array
#'
#' The residency index is the share of days an animal could have been
#' detected on which it was detected. Its denominator counts only days on
#' which a receiver listened, from the receiver listening table (see
#' [audit_listening()]), within the animal's period: from its release, or
#' its first detection when the release is unknown, to its last detection.
#' A day with a detection counts as a listening day whatever the table says,
#' because a detection proves the receiver worked, so the index is at most 1.
#'
#' Appert et al. (2023) found two main forms of the index in 72 studies:
#' days detected over days at liberty, and days detected over the monitoring
#' period. This function uses the first form, with the period ending at the
#' last detection. Counting only the days on which a receiver listened is
#' this package's own rule; it keeps receiver outages out of the denominator.
#' The function is named `residency_index()` so that it does not mask
#' `actel::residency()`, an unrelated analysis.
#'
#' @references Appert, C., Udyawer, V., Simpfendorfer, C. A., Heupel, M. R.,
#'   Scott, M., Currey-Randall, L. M., Harborne, A. R., Jaine, F., and Chin,
#'   A. (2023). Use, misuse, and ambiguity of indices of residence in
#'   acoustic telemetry studies. Marine Ecology Progress Series 714, 27-44.
#'   \doi{10.3354/meps14300}
#'
#' @param x A [detset()]. Its `listening` table is used as it stands; run
#'   [audit_listening()] first to drop days on which receivers failed.
#' @param by `"station"` for one row per animal and station, or `"animal"`
#'   for one row per animal over the whole array.
#' @return A data frame with `animal_id`, `station_id` (by station),
#'   `days_detected`, `days_listening` (the denominator), `days_period` (all
#'   days of the animal's period) and `residency` (`days_detected /
#'   days_listening`). With no detection that has an `animal_id`, it has no
#'   rows, and a message says so.
#' @seealso [summarise_animals()]
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' head(residency_index(dets, by = "station"))
#' head(residency_index(dets, by = "animal"))
residency_index <- function(x, by = c("station", "animal")) {
  need_detset(x)
  by <- check_choice(by, c("station", "animal"), "by")
  per <- residency_periods(x)
  if (!nrow(per)) {
    message(paste("residency_index(): no detection has an animal_id, so there is no",
                  "residency to compute.", no_animal_advice(x)))
    keys <- if (by == "station") c("animal_id", "station_id") else "animal_id"
    out <- data.frame(matrix(character(), 0, length(keys), dimnames = list(NULL, keys)),
                      days_detected = integer(), days_listening = integer(),
                      days_period = integer(), residency = numeric(),
                      stringsAsFactors = FALSE)
    return(out)
  }
  lst <- as_dt(x$listening)
  if ("listening" %in% names(lst)) lst <- lst[listening %in% TRUE]
  lst <- unique(lst[, list(station_id, date)])
  det <- unique(as_dt(x$detections)[!is.na(animal_id) & !is.na(station_id),
                                    list(animal_id, station_id, date = as.Date(datetime))])
  keys <- if (by == "station") c("animal_id", "station_id") else "animal_id"
  avail <- if (by == "station") {
    unique(det[, keys, with = FALSE])[lst, on = "station_id", allow.cartesian = TRUE,
                                      nomatch = NULL]
  } else {
    data.table::CJ(animal_id = per$animal_id, date = unique(lst$date))
  }
  both <- unique(rbind(avail, det[, c(keys, "date"), with = FALSE]))
  both <- both[per, on = "animal_id", nomatch = NULL][date >= d0 & date <= d1]
  out <- both[, list(days_listening = .N), by = keys]
  dd <- unique(det[, c(keys, "date"), with = FALSE])[, list(days_detected = .N), by = keys]
  out <- merge(dd, out, by = keys)
  out[, days_period := per$days[match(animal_id, per$animal_id)]]
  out[, ("residency") := days_detected / days_listening]
  data.table::setorderv(out, keys)
  as_df(out)
}

# Each detected animal's period, as UTC dates: release (or first detection)
# to last detection. No row when no detection has an animal_id; the grouped
# min() and max() would then warn on an empty vector.
residency_periods <- function(x) {
  d <- as_dt(x$detections)[!is.na(animal_id)]
  if (!nrow(d)) {
    return(data.table::data.table(animal_id = character(), d0 = as.Date(character()),
                                  d1 = as.Date(character()), days = integer()))
  }
  d <- d[, list(first = min(datetime), last = max(datetime)), by = "animal_id"]
  rel <- if (!is.null(x$animals) && "release_datetime" %in% names(x$animals)) {
    as_dt(x$animals)[!is.na(release_datetime), list(r = min(release_datetime)), by = "animal_id"]
  }
  r <- if (is.null(rel)) empty_utc(nrow(d)) else rel$r[match(d$animal_id, rel$animal_id)]
  start <- data.table::fifelse(is.na(r) | r > d$first, d$first, r)
  out <- data.table::data.table(animal_id = d$animal_id, d0 = as.Date(start), d1 = as.Date(d$last))
  out[, days := as.integer(d1 - d0) + 1L]
  out
}
