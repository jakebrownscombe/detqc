# ---- Export to actel ----

#' Convert a detset to the four tables of actel::preload()
#'
#' Returns `biometrics`, `spatial`, `deployments` and `detections` in the
#' forms actel 1.4.0 reads, so that
#' `do.call(actel::preload, c(to_actel(x, tz), list(tz = tz)))` runs.
#'
#' **Time.** actel reads detections in UTC and converts them to the study
#' area's time zone, but reads release and deployment times as already in
#' that zone. So `detections$Timestamp` stays in UTC, while
#' `biometrics$Release.date` and the deployments' `Start` and `Stop` are the
#' same instants shown in `tz`. Give `preload()` the same `tz`.
#'
#' **Arrays.** actel places stations in arrays joined by a graph, which suits
#' rivers and gates. A lake array has none, so by default every station is in
#' array `"A1"`; give `arrays`, a named vector of array codes by `station_id`,
#' to set your own, and the matching `dot` to `preload()`. Every row of
#' `spatial` is in section `"All"`, the section actel gives a spatial table
#' that names none.
#'
#' **Receivers.** actel names a receiver by a whole number. Each distinct
#' `receiver_serial` gets its own: the serial itself where it is all digits
#' once a model prefix such as `"VR2Tx-"` is dropped and no other receiver
#' shares it, and a new number above every serial otherwise, as for OTN names
#' such as `"HFX047-250049-2"` or `"144(lost/found)"`. The attribute
#' `receivers` holds the lookup (`Receiver`, `receiver_serial`,
#' `receiver_model`); give it to [read_detset_from()] to restore the names.
#'
#' **Sensors.** A calibrated reading goes out as `Sensor.Value` with its unit.
#' A raw reading with no calibration goes out as `Sensor.Value` with
#' `Sensor.Unit` `"ADC"`, as GLATOS exports write it, so the way back puts it
#' in `sensor_raw` again.
#'
#' **Deployments.** actel needs a `Stop` time for every deployment and
#' stops on a receiver whose deployments overlap, so the deployments are
#' bounded as [to_ato()] bounds them (see there): an open deployment ends at
#' the next deployment of its receiver, or one second after the last
#' detection; a deployment that runs past the next one of its receiver ends
#' where that one starts; and only the deployments that overlap the
#' detections, from the first to the last, are exported. The attribute
#' `deployments` counts each kind, and a warning gives the counts when a
#' deployment was trimmed or left out. `spatial` lists the stations of the
#' exported deployments.
#'
#' **Rows left out.** Detections with no `station_id` match no deployment in
#' actel and are left out, as are detections whose code cannot be read; the
#' attribute `dropped` counts them by reason, and a warning gives the total.
#' An animal whose transmitter has no acoustic code space, such as a
#' satellite tag in an OTN tag release file, is left out of `biometrics`;
#' the attribute `dropped_animals` counts them, and a warning names one.
#' `actel::preload()` itself then drops the detections of transmitters that
#' are not in `biometrics` and those before their animal's release. The
#' attribute `actel_drops` counts both ahead of time, and a warning gives
#' the total, so the loss is on record before actel makes it.
#'
#' **Animals.** Biometrics have one row per animal, with its transmitters' IDs
#' in `Signal` joined by `|`, as actel reads tags with several codes, and
#' `animal_id` kept as an extra column. When no animal has two codes,
#' `Signal` is an integer, as actel requires. An animal whose codes lie in
#' two code spaces has one row per code space, because actel reads one code
#' space per row; a warning counts them. actel reads one animal per
#' transmitter, so a transmitter reused on a second animal stops
#' `to_actel()`, unless all but one of the animals that share it have no
#' detection in the set: those are left out of `biometrics`, a message
#' counts them, and the attribute `shared_code_unheard` names them. Release
#' sites come from the release positions in the animals table; without them
#' actel puts every animal at one unspecified site. When some animals have a
#' release position and others none, `spatial` gains a release row named
#' `"unspecified"` with no position, which actel needs for the animals it
#' puts there, and a warning counts those animals.
#'
#' @param x A `detset` with an animals table.
#' @param tz The study area's time zone, from [OlsonNames()].
#' @param arrays Optional named character vector: the actel array of each
#'   station, named by `station_id`.
#' @return A list of four data frames: `biometrics`, `spatial`,
#'   `deployments` and `detections`, with the attributes `receivers`,
#'   `open_deployments`, `deployments`, `dropped`, `dropped_animals`,
#'   `actel_drops` and `shared_code_unheard`.
#' @seealso [read_detset_from()] for the way back.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' actel_input <- to_actel(dets, tz = "America/Toronto")
#' lapply(actel_input, nrow)
#' head(actel_input$biometrics)
to_actel <- function(x, tz, arrays = NULL) {
  validate_detset(x)
  tz <- check_tz(tz)
  if (is.null(x$animals) || !nrow(x$animals)) {
    stop_detqc("actel needs biometrics: build the detset with an animals table.")
  }
  ani <- drop_unheard_shared(x$animals, x$detections)
  stop_reused_for_actel(ani)
  bounded <- bounded_deployments(x$deployments, x$detections, "to_actel")
  stations <- sort(unique(stats::na.omit(bounded$station_id)))
  arr <- actel_arrays(stations, arrays)
  rx <- actel_receivers(x$deployments, x$detections)
  dep <- actel_deployments(bounded, tz, rx)
  det <- actel_detections(x$detections, rx)
  sites <- release_sites(ani)
  bio <- actel_biometrics(ani, tz, sites)
  out <- list(
    biometrics = bio, spatial = actel_spatial(bounded, arr, unspecified_site(sites, bio)),
    deployments = dep, detections = det
  )
  attr(out$biometrics, "dropped") <- NULL
  attr(out, "receivers") <- rx
  attr(out, "open_deployments") <- attr(bounded, "counts")[["open"]]
  attr(out, "deployments") <- attr(bounded, "counts")
  attr(out, "dropped") <- attr(det, "dropped")
  attr(out, "dropped_animals") <- attr(bio, "dropped")
  attr(out, "actel_drops") <- actel_drops(det, out$biometrics)
  attr(out$detections, "dropped") <- NULL
  attr(out, "shared_code_unheard") <- attr(ani, "shared_code_unheard")
  out
}

# An animal with no detection in the set whose transmitter is also on another
# animal is left out of biometrics: its code would stop actel, and it has no
# detection to lose. Kept, the reused code of undetected animals would stop
# the export of the detected ones.
drop_unheard_shared <- function(ani, det) {
  tx <- unique(ani[, c("animal_id", "transmitter")])
  dup <- unique(tx$transmitter[duplicated(tx$transmitter)])
  heard <- unique(stats::na.omit(det$animal_id))
  gone <- unique(tx$animal_id[tx$transmitter %in% dup & !tx$animal_id %in% heard])
  if (length(gone)) {
    message(sprintf(paste("to_actel(): %s %s a transmitter with another animal and %s no",
                          "detection in the set, such as %s; %s left out of biometrics."),
                    n_of(length(gone), "animal"), verb(length(gone), "shares", "share"),
                    verb(length(gone), "has", "have"), gone[1],
                    verb(length(gone), "it is", "they are")))
    ani <- ani[!ani$animal_id %in% gone, , drop = FALSE]
  }
  attr(ani, "shared_code_unheard") <- gone
  ani
}

# actel fills a blank Release.site with "unspecified" and stops unless
# spatial lists that site, so when some animals have a release site and
# others none, the site is added as a release row with no position. actel
# 1.3.0 preloads it.
unspecified_site <- function(sites, bio) {
  if (is.null(sites$sites) || !"Release.site" %in% names(bio) || !anyNA(bio$Release.site)) {
    return(sites$sites)
  }
  n_na <- sum(is.na(bio$Release.site))
  warn_detqc(paste("%s of %s %s no release site; actel puts %s at the release site",
                   "\"unspecified\", which spatial lists with no position."),
             fmt_n(n_na), n_of(nrow(bio), "biometrics row"), verb(n_na, "has", "have"),
             verb(n_na, "it", "them"))
  rbind(sites$sites, data.frame(Station.name = "unspecified", Latitude = NA_real_,
                                Longitude = NA_real_, stringsAsFactors = FALSE))
}

stop_reused_for_actel <- function(ani) {
  tx <- unique(ani[, c("animal_id", "transmitter")])
  dup <- unique(tx$transmitter[duplicated(tx$transmitter)])
  if (length(dup)) {
    stop_detqc(paste("actel reads one animal per transmitter, and %s %s on more than one",
                     "animal, such as %s (%s). Export each animal's period on its own."),
               n_of(length(dup), "transmitter"), verb(length(dup), "is", "are"), dup[1],
               paste(tx$animal_id[tx$transmitter == dup[1]], collapse = ", "))
  }
  invisible(TRUE)
}

# One whole number per distinct receiver_serial. A serial that is all digits
# once the model is split off keeps its number when no other serial shares
# it; every other serial gets the next number above the largest. Trailing
# digits of a name are not unique, and "144(lost/found)" has none.
actel_receivers <- function(dep, det) {
  serial <- unique(stats::na.omit(c(dep$receiver_serial, det$receiver_serial)))
  all_serial <- c(dep$receiver_serial, det$receiver_serial)
  all_model <- c(ato_col(dep, "receiver_model", NA_character_),
                 ato_col(det, "receiver_model", NA_character_))
  has <- !is.na(all_model)
  model <- all_model[has][match(serial, all_serial[has])]
  sp <- split_receiver(serial)
  bare <- sp$serial
  num <- suppressWarnings(as.numeric(ifelse(grepl("^[0-9]+$", bare), bare, NA)))
  num[num %in% num[duplicated(num)] | num > .Machine$integer.max] <- NA
  base <- max(c(0, num), na.rm = TRUE)
  num[is.na(num)] <- base + seq_len(sum(is.na(num)))
  model[is.na(model)] <- sp$model[is.na(model)]
  data.frame(Receiver = as.integer(num), receiver_serial = serial, receiver_model = model,
             stringsAsFactors = FALSE)
}

actel_drops <- function(det, bio) {
  sig <- strsplit(as.character(bio$Signal), "|", fixed = TRUE)
  known <- paste(rep(bio$Code.space, lengths(sig)), unlist(sig), sep = "-")
  rel <- rep(bio$Release.date, lengths(sig))
  key <- paste(det$CodeSpace, det$Signal, sep = "-")
  k <- match(key, known)
  out <- c(not_in_biometrics = sum(is.na(k)),
           before_release = sum(!is.na(k) & det$Timestamp < rel[k]))
  if (sum(out)) {
    warn_detqc(paste("actel::preload() will drop %s of %s: %s of transmitters not in",
                     "the animals table and %s before their animal's release."),
               fmt_n(sum(out)), n_of(nrow(det), "detection"), fmt_n(out[["not_in_biometrics"]]),
               fmt_n(out[["before_release"]]))
  }
  out
}

actel_arrays <- function(stations, arrays) {
  if (is.null(arrays)) return(stats::setNames(rep("A1", length(stations)), stations))
  if (!is.character(arrays) || is.null(names(arrays))) {
    stop_detqc("`arrays` must be a character vector named by station_id.")
  }
  miss <- setdiff(stations, names(arrays))
  if (length(miss)) stop_detqc("`arrays` gives no array for station %s.", miss[1])
  arrays[stations]
}

actel_deployments <- function(dep, tz, rx) {
  out <- data.frame(
    Receiver = rx$Receiver[match(dep$receiver_serial, rx$receiver_serial)],
    Station.name = dep$station_id,
    Start = lubridate::with_tz(dep$deploy_datetime, tz),
    Stop = lubridate::with_tz(dep$recover_datetime, tz), stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$Receiver), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- Internal: deployments for an export ----
# One rule for to_actel() and to_ato(). A deployment still in the water ends
# at the next deployment of its receiver, else one second after the last
# detection, so the last detection falls inside it under the half-open rule.
# A deployment that runs past the next one of its receiver ends where that
# one starts, as detset() gives the overlap to the later one. Only the
# deployments that overlap the detections are kept. Ending every open
# deployment at the last detection would end some deployments before they
# start, and actel stops on them; open GLATOS deployments would then
# overlap later ones of the same receiver.
bounded_deployments <- function(dep, det, fn) {
  n <- nrow(dep)
  if (!n) {
    attr(dep, "counts") <- c(deployments = 0L, open = 0L, open_to_next = 0L, open_to_last = 0L,
                             trimmed = 0L, outside = 0L, exported = 0L)
    return(dep)
  }
  first <- if (nrow(det)) min(det$datetime) else min(dep$deploy_datetime)
  last <- if (nrow(det)) max(det$datetime) else max(dep$deploy_datetime)
  ord <- order(dep$receiver_serial, dep$deploy_datetime, na.last = TRUE)
  d <- dep[ord, , drop = FALSE]
  same_next <- c(d$receiver_serial[-1] == d$receiver_serial[-n], FALSE) %in% TRUE
  next_start <- c(d$deploy_datetime[-1], d$deploy_datetime[n])
  next_start[!same_next] <- NA
  end <- d$recover_datetime
  open <- is.na(end)
  open_next <- open & !is.na(next_start)
  end[open_next] <- next_start[open_next]
  open_last <- open & is.na(next_start)
  end[open_last] <- pmax(d$deploy_datetime[open_last], last) + 1
  trimmed <- !open & !is.na(next_start) & end > next_start
  end[trimmed] <- next_start[trimmed]
  d$recover_datetime <- end
  keep <- end > d$deploy_datetime & end > first & d$deploy_datetime <= last
  out <- d[keep, , drop = FALSE]
  rownames(out) <- NULL
  counts <- c(deployments = n, open = sum(open), open_to_next = sum(open_next),
              open_to_last = sum(open_last), trimmed = sum(trimmed), outside = sum(!keep),
              exported = sum(keep))
  if (counts[["trimmed"]] || counts[["outside"]]) {
    warn_detqc(paste("%s() bounds the deployments: of %s, %s open ones end at the next deployment",
                     "of their receiver and %s one second after the last detection; %s that ran",
                     "past the next deployment of their receiver end where it starts; %s that do",
                     "not overlap the detections (%s to %s) are left out; %s are exported."),
               fn, fmt_n(n), fmt_n(counts[["open_to_next"]]), fmt_n(counts[["open_to_last"]]),
               fmt_n(counts[["trimmed"]]), fmt_n(counts[["outside"]]), format(first, "%Y-%m-%d"),
               format(last, "%Y-%m-%d"), fmt_n(counts[["exported"]]))
  }
  attr(out, "counts") <- counts
  out
}

actel_detections <- function(d, rx) {
  keep <- !is.na(d$station_id) & !is.na(d$receiver_serial)
  tx <- split_transmitter(d$transmitter[keep])
  value <- ato_col(d, "sensor_value", NA_real_)[keep]
  unit <- ato_col(d, "sensor_unit", NA_character_)[keep]
  raw <- ato_col(d, "sensor_raw", NA_real_)[keep]
  # A raw reading goes out as ADC, as to_glatos() writes it.
  as_adc <- is.na(value) & !is.na(raw)
  value[as_adc] <- raw[as_adc]
  unit[as_adc] <- "ADC"
  out <- data.frame(
    Timestamp = d$datetime[keep],
    Receiver = rx$Receiver[match(d$receiver_serial[keep], rx$receiver_serial)],
    CodeSpace = tx$codespace, Signal = suppressWarnings(as.integer(tx$id)),
    Sensor.Value = value, Sensor.Unit = unit, stringsAsFactors = FALSE
  )
  ok <- !is.na(out$Signal) & !is.na(out$CodeSpace)
  out <- out[ok, , drop = FALSE]
  rownames(out) <- NULL
  dropped <- c(no_station = sum(!keep), unreadable_code = sum(!ok))
  if (sum(dropped)) {
    warn_detqc("to_actel() leaves out %s of %s: %s with no station, %s with a code %s",
               fmt_n(sum(dropped)), n_of(nrow(d), "detection"), fmt_n(dropped[[1]]),
               fmt_n(dropped[[2]]),
               "actel cannot read.")
  }
  attr(out, "dropped") <- dropped
  out
}

release_sites <- function(ani) {
  has <- all(c("release_lat", "release_lon") %in% names(ani))
  if (!has || all(is.na(ani$release_lat) | is.na(ani$release_lon))) {
    return(list(sites = NULL, site_of = rep(NA_character_, nrow(ani))))
  }
  key <- ifelse(is.na(ani$release_lat) | is.na(ani$release_lon), NA_character_,
                sprintf("%.6f %.6f", ani$release_lat, ani$release_lon))
  u <- unique(stats::na.omit(key))
  sites <- data.frame(
    Station.name = paste0("R", seq_along(u)),
    Latitude = as.numeric(sub(" .*$", "", u)), Longitude = as.numeric(sub("^.* ", "", u)),
    stringsAsFactors = FALSE
  )
  list(sites = sites, site_of = sites$Station.name[match(key, u)])
}

actel_biometrics <- function(ani, tz, sites) {
  dt <- as_dt(ani)
  dt[, site := sites$site_of]
  tx <- split_transmitter(dt$transmitter)
  dt[, `:=`(code_space = tx$codespace, signal = tx$id)]
  get <- function(nm) if (nm %in% names(dt)) dt[[nm]] else rep(NA, nrow(dt))
  dt[, `:=`(release = get("release_datetime"), group = get("species"))]
  # actel gives a biometrics row one code space, so an animal whose codes lie
  # in two code spaces takes one row per code space; animal_id joins them.
  per <- dt[, list(
    Release.date = release[1], Signal = paste(signal, collapse = "|"),
    Group = group[1], Release.site = site[1]
  ), by = list(animal_id, Code.space = code_space)]
  per <- drop_no_code_space(per)
  split_ids <- unique(per$animal_id[duplicated(per$animal_id)])
  if (length(split_ids)) {
    warn_detqc(paste("%s %s codes in two code spaces, such as %s; actel sees one tag per",
                     "code space, and animal_id keeps them together."),
               n_of(length(split_ids), "animal"), verb(length(split_ids), "carries", "carry"),
               split_ids[1])
  }
  if (anyNA(per$Release.date)) {
    n_none <- sum(is.na(per$Release.date))
    stop_detqc("actel needs a release time for every animal; %s %s none, such as %s.",
               n_of(n_none, "animal"), verb(n_none, "has", "have"),
               per$animal_id[is.na(per$Release.date)][1])
  }
  out <- as_df(per[, list(Release.date = lubridate::with_tz(Release.date, tz), Signal,
                          Code.space, Group, Release.site, animal_id)])
  if (all(is.na(out$Release.site))) out$Release.site <- NULL
  if (all(is.na(out$Group))) out$Group <- NULL
  # actel reads Signal as integers unless some row holds "|" (actel 1.4.0,
  # loadBio): text stops every study of single-code tags.
  as_int <- suppressWarnings(as.integer(out$Signal))
  if (!any(grepl("|", out$Signal, fixed = TRUE)) && !anyNA(as_int)) out$Signal <- as_int
  attr(out, "dropped") <- attr(per, "dropped")
  out
}

# An OTN tag release file lists pop-up satellite tags beside acoustic ones;
# their transmitter has no code space, and actel stops on a biometrics row
# without one.
drop_no_code_space <- function(per) {
  none <- is.na(per$Code.space)
  if (any(none)) {
    warn_detqc(paste("to_actel() leaves out %s of %s whose transmitter has no acoustic",
                     "code space, such as %s; actel reads acoustic tags only."),
               fmt_n(sum(none)), n_of(length(unique(per$animal_id)), "animal"),
               per$animal_id[none][1])
  }
  out <- per[!none]
  attr(out, "dropped") <- c(no_acoustic_code = sum(none))
  out
}

actel_spatial <- function(dep, arr, sites) {
  st <- as_dt(dep)[!is.na(station_id), list(Latitude = deploy_lat[1], Longitude = deploy_lon[1]),
                   by = "station_id"]
  data.table::setorderv(st, "station_id")
  out <- data.frame(Station.name = st$station_id, Latitude = st$Latitude,
                    Longitude = st$Longitude, Array = unname(arr[st$station_id]),
                    Type = "Hydrophone", stringsAsFactors = FALSE)
  if (!is.null(sites)) {
    rel <- data.frame(Station.name = sites$Station.name, Latitude = sites$Latitude,
                      Longitude = sites$Longitude, Array = unname(arr[1]), Type = "Release",
                      stringsAsFactors = FALSE)
    out <- rbind(out, rel)
  }
  # actel 1.4.0 (loadSpatial) warns on a spatial table with no Section column
  # and then puts every row in section "All". Writing that column gives the
  # same preload with no warning.
  out$Section <- "All"
  out
}

# Columns built inside the data.table calls above.
utils::globalVariables(c(
  "site", "signal", "code_space", "release", "group", "Release.date", "Signal",
  "Code.space", "Group", "Release.site"
))
