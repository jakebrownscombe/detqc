# ---- The detset object ----

#' Build a detection set
#'
#' A detection set holds the detections, the receiver deployments and,
#' optionally, the animals, with the column names of the positionR table
#' contract. It also holds the receiver listening days, an append-only
#' processing log and metadata. Every later step in detqc takes one.
#'
#' Each detection is matched to the deployment of its receiver whose interval
#' holds it. The interval is half-open, `deploy_datetime <= datetime <
#' recover_datetime`, so a detection at the moment of a receiver swap belongs
#' to the later deployment. A deployment with no `recover_datetime` is open.
#' The matched `station_id` replaces any `station_id` the detections carried.
#' Detections that match no deployment keep their own `station_id`, or `NA`,
#' and stay in the set: no row is removed here. A detection or deployment with
#' no `receiver_serial` matches nothing.
#'
#' Receivers are matched on `receiver_serial` as written. A detection that
#' matches no deployment that way is matched again on the bare serial, so
#' `"VR2Tx-480013"` finds the deployment of `"480013"` and the reverse; its
#' `receiver_serial` then takes the deployment's form, which the listening
#' days use too. Only a known receiver family (VR, HR, TBR and others)
#' followed by three or more digits is read as model and serial. The log
#' counts these matches, and `detset()` warns when fewer than half of the
#' detections match a deployment.
#'
#' A deployment sheet that records stations and no receiver serials, as OTN's
#' deployment CSV does, is matched on `station_id` and time instead: a
#' detection that matched no deployment by serial takes the serial-less
#' deployment of its own `station_id` whose interval holds it. The log counts
#' these matches.
#'
#' A deployment that ends at the moment it starts can hold no detection under
#' the half-open rule. It is dropped with a warning and counted in the log. A
#' deployment that ends before it starts stops `detset()` when a detection is
#' on its receiver (or at its station, for a deployment with no serial); the
#' message names the row and points to [check_detset()], which lists every
#' such row. On a receiver no detection is on, as in the network-wide GLATOS
#' receiver file, it is set aside with a warning and counted in the log.
#'
#' Blank text, `""` or spaces, in `animal_id`, `transmitter`,
#' `receiver_serial` or `station_id` is a missing value: `data.table::fread()`
#' reads an empty column as `""`, which would otherwise name an animal. The
#' log counts the values read as `NA`.
#'
#' When `animals` is given, a warning counts the detected transmitters it
#' lists when that is none, or when they carry under 5% of the detections.
#' `animal_id` is filled from it by `transmitter` for detections that have
#' none; a detection whose own `animal_id` disagrees with
#' the animals table keeps its own, with a warning that counts them. A
#' transmitter listed on more than one animal, as after tag reuse, is filled
#' by release time when every one of its
#' animals has a distinct `release_datetime`: a detection goes to the animal
#' released last at or before it, and a detection before the first release
#' stays `NA`. Where a release time is missing or shared, the transmitter
#' fills nothing. The log counts both kinds.
#'
#' @param detections A data frame with `datetime`, `receiver_serial`,
#'   `transmitter` and `animal_id`, as [read_detections()] returns.
#' @param deployments A data frame with `receiver_serial`, `station_id`,
#'   `deploy_datetime`, `recover_datetime`, `deploy_lat` and `deploy_lon`, as
#'   [read_deployments()] returns.
#' @param animals Optional data frame with one row per animal and transmitter
#'   and at least `animal_id` and `transmitter`, as [read_animals()] returns.
#' @param crs EPSG code that the detset records for later steps, such as a
#'   projected code for coordinates in metres. The default, 4326, is
#'   geographic. `deploy_lat` and `deploy_lon` are always WGS 84 whatever
#'   this is.
#' @param tz Time zone of any datetime column given as text. Text is read in
#'   this zone and converted to UTC. Datetime columns that are already
#'   POSIXct must be in UTC.
#'
#' @return An object of class `detset`: a list of `detections`, `deployments`,
#'   `animals`, `listening`, `log` and `meta`. `meta$sources` lists the files
#'   the import functions read, from the `source` attribute of each table, and the
#'   first log record counts the rows the import functions set aside. When
#'   the deployments carry `download_datetime`, the detections gain the
#'   logical column `after_download`: `TRUE` for a detection after its
#'   deployment's recorded download and before the deployment ends. The set
#'   keeps these detections; [audit_listening()] keeps their days
#'   `"not_downloaded"`, and the check `detections_after_download` of
#'   [check_detset()] lists them. The column passes through the filters, so
#'   later steps can read it. Where the download time on the sheet is wrong,
#'   correct it and build the set again. The log record counts them.
#' @seealso [validate_detset()], [detset_log()]
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' dets
#' detset_log(dets)[, c("step", "fn", "rows_before", "rows_after", "removed")]
detset <- function(detections, deployments, animals = NULL, crs = 4326, tz = "UTC") {
  if (!is.character(tz) || length(tz) != 1 || !tz %in% OlsonNames()) {
    stop_detqc("`tz` must be one time zone name from OlsonNames().")
  }
  crs <- suppressWarnings(as.integer(crs))
  if (length(crs) != 1 || is.na(crs)) stop_detqc("`crs` must be one EPSG code.")
  sources <- source_table(detections, deployments, animals)
  read_counts <- reader_counts(detections, deployments)
  det <- prepare_times(detections, "datetime", tz, "detections")
  dep <- prepare_times(deployments, c("deploy_datetime", "recover_datetime", "download_datetime"),
                       tz, "deployments")
  check_columns(det, contract_spec$detections, "detections")
  check_columns(dep, contract_spec$deployments, "deployments")
  det <- blank_to_na(det, c("animal_id", "transmitter", "receiver_serial", "station_id"))
  dep <- blank_to_na(dep, c("receiver_serial", "station_id"))
  if (!is.null(animals)) animals <- blank_to_na(as_df(animals), c("animal_id", "transmitter"))
  read_counts[["blank"]] <- sum(attr(det, "blanks"), attr(dep, "blanks"), attr(animals, "blanks"))
  dep <- check_deployments(dep, det)
  read_counts[["bad_window"]] <- attr(dep, "bad_window")
  rows_in <- nrow(det)
  animals_in <- count_animals(det)
  fa <- detset_animals(animals, det, tz)
  joined <- join_stations(fa$detections, dep)
  warn_low_match(joined, det, dep)
  if ("download_datetime" %in% names(dep)) {
    joined$detections$after_download <- !is.na(after_download_index(dep, joined$detections))
  }
  x <- structure(list(
    detections = joined$detections, deployments = as_df(dep), animals = fa$animals,
    listening = build_listening(dep, joined$detections), log = new_log(),
    meta = list(epsg = crs, tz = "UTC", created = lubridate::now("UTC"),
                detqc_version = as.character(utils::packageVersion("detqc")),
                sources = sources)
  ), class = "detset")
  params <- list(crs = crs, tz = tz, deployments = nrow(dep),
                 animals = if (is.null(fa$animals)) 0L else nrow(fa$animals))
  rows_read <- reader_rows_read(detections, deployments, animals)
  if (length(rows_read)) params$rows_read <- rows_read
  reason <- detset_reason(joined, rows_in, attr(dep, "zero_length"), read_counts, fa$counts)
  if ("after_download" %in% names(joined$detections)) {
    reason <- paste0(reason, sprintf("; %s after the recorded download, marked in after_download",
                                     n_of(sum(joined$detections$after_download), "detection")))
  }
  x <- log_step(x, "detset", params, c(rows_in, animals_in), 0L, reason, fn = "detset")
  validate_detset(x)
}

detset_reason <- function(joined, rows_in, zero_length, read_counts, filled) {
  sprintf(
    paste0("station from a deployment for %s of %s (half-open intervals); ",
           "%s of them matched on the bare receiver serial and %s on station_id to a ",
           "deployment with no receiver serial; ",
           "%s matched none; %s replaced; ",
           "%s read as NA; ",
           "%s dropped; ",
           "deployments that end before they start, on receivers no detection is on, ",
           "set aside: %s; ",
           "deployment rows with no deployment time dropped by the read function: %s; ",
           "%s and %s set aside by the read ",
           "function; ",
           "animal_id filled for %s from animals; disagreeing with animals: %s; ",
           "%s dropped; ",
           "transmitters on more than one animal, filling nothing: %s; ",
           "transmitters on more than one animal with distinct release times: %s, filling %s ",
           "by release time and leaving %s before the first release NA"),
    fmt_n(joined$matched), n_of(rows_in, "detection"), fmt_n(joined$by_bare),
    fmt_n(joined$by_station), fmt_n(rows_in - joined$matched),
    n_of(joined$replaced, "source station_id value"),
    n_of(read_counts[["blank"]], "blank text value"),
    n_of(zero_length, "zero-length deployment"), fmt_n(read_counts[["bad_window"]]),
    fmt_n(read_counts[["no_deploy_time"]]),
    n_of(read_counts[["release_rows"]], "OTN release record"),
    n_of(read_counts[["repeated_records"]], "repeated VDAT record"),
    n_of(filled[["filled"]], "detection"), fmt_n(filled[["conflicts"]]),
    n_of(filled[["duplicates"]], "duplicate animal row"), fmt_n(filled[["reused"]]),
    fmt_n(filled[["timed_tx"]]), n_of(filled[["by_release"]], "detection"),
    fmt_n(filled[["before_release"]])
  )
}

detset_animals <- function(animals, det, tz) {
  counts <- c(filled = 0L, conflicts = 0L, duplicates = 0L, reused = 0L, timed_tx = 0L,
              by_release = 0L, before_release = 0L)
  if (is.null(animals)) return(list(animals = NULL, detections = det, counts = counts))
  ani <- prepare_times(animals, "release_datetime", tz, "animals")
  check_columns(ani, contract_spec$animals, "animals")
  ani <- dedupe_animals(ani)
  fa <- fill_animal_ids(det, ani$table, ani$reused)
  warn_conflicts(attr(fa, "detections"), fa[["conflicts"]], attr(fa, "conflict_example"))
  warn_unlisted_transmitters(det, ani$table)
  counts <- c(filled = fa[["filled"]], conflicts = fa[["conflicts"]],
              duplicates = ani$duplicates, reused = length(ani$reused) - fa[["timed_tx"]],
              timed_tx = fa[["timed_tx"]], by_release = fa[["by_release"]],
              before_release = fa[["before_release"]])
  list(animals = ani$table, detections = attr(fa, "detections"), counts = counts)
}

# A detection that names one animal while the animals table gives its
# transmitter to another keeps its own id. The count is logged.
warn_conflicts <- function(det, n, example) {
  if (!n) return(invisible(0L))
  warn_detqc(paste("%s of %s %s an animal_id that disagrees with the animals",
                   "table, such as %s; each keeps its own animal_id. Check the detections'",
                   "animal_id column, or clear it to take the ids from the animals table."),
             fmt_n(n), n_of(nrow(det), "detection"), verb(n, "carries", "carry"), example)
  invisible(n)
}

# An animals table that lists none, or few, of the detected transmitters
# belongs to another study or another code space, and every step that reads
# animals then acts on nothing. Few is under 5% of the detections: a raw
# receiver export also holds reference tags and other projects' tags, so the
# share of transmitters alone would warn on a sound set.
warn_unlisted_transmitters <- function(det, ani) {
  tx <- unique(stats::na.omit(det$transmitter))
  if (!length(tx)) return(invisible(FALSE))
  listed <- det$transmitter %in% ani$transmitter
  n_tx <- sum(tx %in% ani$transmitter)
  if (n_tx > 0 && mean(listed) >= 0.05) return(invisible(FALSE))
  warn_detqc(paste("%s of %s %s in the animals table, which lists %s;",
                   "%s of %s %s one of them. Check that the animals table is the",
                   "tagging sheet of these detections, and that both write transmitters as code",
                   "space and ID, such as \"A69-1601-12345\"."),
             fmt_n(n_tx), n_of(length(tx), "detected transmitter"), verb(n_tx, "is", "are"),
             n_of(length(unique(ani$transmitter)), "transmitter"), fmt_n(sum(listed)),
             n_of(nrow(det), "detection"), verb(sum(listed), "carries", "carry"))
  invisible(TRUE)
}

# Blank text is a missing value: fread() reads an empty column as "", and a
# "" animal_id would block every fill from the animals table and count as
# one animal. The attribute "blanks" counts the values changed.
blank_to_na <- function(df, cols) {
  n <- 0L
  for (cl in intersect(cols, names(df))) {
    v <- df[[cl]]
    if (!is.character(v)) next
    b <- !is.na(v) & !nzchar(trimws(v))
    if (any(b)) {
      v[b] <- NA_character_
      df[[cl]] <- v
      n <- n + sum(b)
    }
  }
  attr(df, "blanks") <- n
  df
}

# Rows the readers set aside before detset() saw the tables, read from the
# "source" attribute the readers write: one record, or a list of them when
# several files were read at once.
reader_counts <- function(detections, deployments) {
  get_n <- function(tbl, field) {
    src <- attr(tbl, "source")
    if (is.null(src)) return(0L)
    if (!is.null(src$file)) src <- list(src)
    sum(vapply(src, function(r) as.numeric(if (is.null(r[[field]])) 0 else r[[field]]),
               numeric(1)))
  }
  c(no_deploy_time = get_n(deployments, "dropped_no_deploy_time"),
    release_rows = get_n(detections, "release_rows"),
    repeated_records = get_n(detections, "repeated_records"))
}

# The data rows each table's reader read from its files, before any cut,
# summed per table: a named integer vector, empty when no reader recorded
# them. detset() logs it in its record's parameters, so the report shows
# rows read beside rows kept from the log.
reader_rows_read <- function(detections, deployments, animals) {
  one <- function(tbl) {
    src <- attr(tbl, "source")
    if (is.null(src)) return(NA_integer_)
    if (!is.null(src$file)) src <- list(src)
    n <- vapply(src, function(r) if (is.null(r$rows_read)) NA_real_ else as.numeric(r$rows_read),
                numeric(1))
    if (anyNA(n) || sum(n) > .Machine$integer.max) NA_integer_ else as.integer(sum(n))
  }
  out <- c(detections = one(detections), deployments = one(deployments),
           animals = if (is.null(animals)) NA_integer_ else one(animals))
  out[!is.na(out)]
}

# CONTRACT.md section 7: one row per file read, for the report.
source_table <- function(detections, deployments, animals) {
  rows <- list()
  for (tb in c("detections", "deployments", "animals")) {
    src <- attr(switch(tb, detections = detections, deployments = deployments,
                       animals = animals), "source")
    if (is.null(src)) next
    if (!is.null(src$file)) src <- list(src)
    for (r in src) {
      rows[[length(rows) + 1]] <- data.frame(table = tb, file = as.character(r$file),
                                             format = as.character(r$format),
                                             rows = as.integer(r$rows),
                                             stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) {
    return(data.frame(table = character(), file = character(), format = character(),
                      rows = integer(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

#' Check a detection set
#'
#' Checks that a `detset` has its six parts and that each table carries the
#' contract columns with the right types. In the animals and tags table it
#' also checks that an animal lists each transmitter once, that `min_delay` is not
#' above `max_delay`, and that a sensor slope comes with an intercept. It
#' stops at the first problem and names it.
#'
#' @param x A `detset`.
#' @return `x`, invisibly, when every check passes.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' validate_detset(dets)
validate_detset <- function(x) {
  if (!inherits(x, "detset")) stop_detqc("`x` must be a detset, not %s.", class(x)[1])
  parts <- c("detections", "deployments", "animals", "listening", "log", "meta")
  missing <- setdiff(parts, names(x))
  if (length(missing)) stop_detqc("The detset has no `%s` part.", missing[1])
  check_columns(x$detections, contract_spec$detections, "detections")
  check_columns(x$deployments, contract_spec$deployments, "deployments")
  if (!is.null(x$animals)) check_columns(x$animals, contract_spec$animals, "animals")
  check_columns(x$listening, spec_table(
    c("receiver_serial", "station_id", "date"), c("character", "character", "date"),
    rep(TRUE, 3)
  ), "listening")
  check_log(x$log)
  check_animal_tags(x$animals)
  has_unit <- all(c("sensor_value", "sensor_unit") %in% names(x$detections))
  bad <- if (has_unit) {
    !is.na(x$detections$sensor_value) & is.na(x$detections$sensor_unit)
  } else {
    "sensor_value" %in% names(x$detections) & !is.na(x$detections$sensor_value)
  }
  if (any(bad)) {
    stop_detqc("%s %s a `sensor_value` with no `sensor_unit`.", n_of(sum(bad), "detection"),
               verb(sum(bad), "carries", "carry"))
  }
  if (!is.list(x$meta) || !is.integer(x$meta$epsg)) {
    stop_detqc("`meta$epsg` must be one integer EPSG code.")
  }
  check_unkeyed(x$deployments, x$listening)
  invisible(x)
}

# OTN simplified sheets give the station but no receiver serial.
# Their listening rows keep `receiver_serial` NA beside the station, and
# detfield matches such rows to detections by station (key
# "station:<station_id>"). A row with neither cannot be matched to anything.
check_unkeyed <- function(dep, lst) {
  bad <- is.na(dep$receiver_serial) & is.na(dep$station_id)
  if (any(bad)) {
    stop_detqc("%s %s neither a receiver_serial nor a station_id.",
               n_of(sum(bad), "deployment"), verb(sum(bad), "has", "have"))
  }
  bad <- is.na(lst$receiver_serial) & is.na(lst$station_id)
  if (any(bad)) {
    stop_detqc("%s listening rows have neither a receiver_serial nor a station_id.",
               fmt_n(sum(bad)))
  }
  invisible(sum(is.na(dep$receiver_serial)))
}

# CONTRACT.md, "Animals and tags": a slope with no intercept, or the reverse,
# calibrates nothing, and a delay range must run from low to high.
check_animal_tags <- function(ani) {
  if (is.null(ani)) return(invisible(TRUE))
  # A transmitter reused on a second animal is two rows, which is allowed;
  # one animal with two rows for one transmitter says two different things.
  pair <- paste(ani$animal_id, ani$transmitter, sep = "\r")
  dup <- which(duplicated(pair))
  if (length(dup)) {
    stop_detqc("Animal %s has %s for transmitter %s; give one.", ani$animal_id[dup[1]],
               n_of(sum(pair == pair[dup[1]]), "row"), ani$transmitter[dup[1]])
  }
  if (all(c("min_delay", "max_delay") %in% names(ani))) {
    bad <- which(!is.na(ani$min_delay) & !is.na(ani$max_delay) & ani$min_delay > ani$max_delay)
    if (length(bad)) {
      stop_detqc("Transmitter %s has min_delay %g above max_delay %g.", ani$transmitter[bad[1]],
                 ani$min_delay[bad[1]], ani$max_delay[bad[1]])
    }
  }
  has <- function(nm) if (nm %in% names(ani)) !is.na(ani[[nm]]) else rep(FALSE, nrow(ani))
  half <- which(xor(has("sensor_slope"), has("sensor_intercept")))
  if (length(half)) {
    stop_detqc("Transmitter %s has a sensor slope or intercept but not both.",
               ani$transmitter[half[1]])
  }
  invisible(TRUE)
}

#' The processing log of a detection set
#'
#' Every step that changes a detection set appends one row to its log. Rows
#' are never edited or removed.
#'
#' @param x A `detset`.
#' @return A data frame, one row per step, with the fields `step`, `fn`, `at` (UTC), `parameters`,
#'   `rows_before`, `rows_after`, `animals_before`, `animals_after`, `removed` (detections),
#'   `removed_by` (a list of counts named by reason), `reason` and
#'   `detqc_version`.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments)
#' detset_log(dets)[, c("step", "fn", "rows_before", "rows_after", "removed")]
detset_log <- function(x) {
  validate_detset(x)
  x$log
}

# ---- Print and summary ----

#' @export
print.detset <- function(x, ...) {
  d <- x$detections
  rng <- if (nrow(d)) format(range(d$datetime), "%Y-%m-%d") else c("none", "none")
  dep <- x$deployments
  rx <- heard_counts(d, dep)
  # Deployments with no receiver serial are matched by station, so a set
  # whose deployments name no receiver is counted by station alone.
  on_rx <- if (rx$deployed) {
    sprintf("on %s of %s ", fmt_n(rx$heard), n_of(rx$deployed, "receiver"))
  } else {
    ""
  }
  cat_wrap(sprintf("<detset> %s of %s (%s), heard %sat %s of %s",
                   n_of(nrow(d), "detection"), n_of(count_animals(d), "animal"),
                   n_of(length(unique(d$transmitter)), "transmitter"), on_rx,
                   fmt_n(rx$stations_heard), n_of(rx$stations, "station")))
  # With no serial in any deployment every detection is matched by station,
  # so a serial in no deployment says nothing.
  if (rx$unmatched && rx$deployed) {
    cat_wrap(sprintf("%s in the detections %s in no deployment",
                     n_of(rx$unmatched, "receiver serial"), if (rx$unmatched == 1) "is" else "are"),
             indent = 2, exdent = 4)
  }
  lst <- x$listening
  days <- if ("listening" %in% names(lst)) {
    sprintf("%s of %s listening (audited)", fmt_n(sum(lst$listening)),
            n_of(nrow(lst), "receiver-day"))
  } else {
    sprintf("%s deployed (listening not audited)", n_of(nrow(lst), "receiver-day"))
  }
  cat_wrap(sprintf("%s to %s (UTC); %s; %s; %s", rng[1], rng[2],
                   n_of(nrow(dep), "deployment"),
                   n_of(if (is.null(x$animals)) 0L else nrow(x$animals), "animal row"),
                   days), indent = 2, exdent = 4)
  bare <- sum(is.na(x$deployments$receiver_serial))
  if (bare) {
    cat_wrap(sprintf(paste("%s of %s %s no receiver_serial; %s listening days",
                           "are matched to detections by station_id"),
                     fmt_n(bare), n_of(nrow(x$deployments), "deployment"),
                     verb(bare, "has", "have"), verb(bare, "its", "their")),
             indent = 2, exdent = 4)
  }
  cat_wrap(sprintf("log: %s (%s)", n_of(nrow(x$log), "step"), paste(x$log$step, collapse = ", ")),
           indent = 2, exdent = 4)
  if (!is.null(x$mortality)) {
    cat(sprintf("  mortality calls: %s\n", n_of(nrow(x$mortality), "animal")))
  }
  invisible(x)
}

# Receivers and stations that heard, counted among the deployed ones, so the
# first count never exceeds the second; serials the deployments do not name
# are counted apart.
heard_counts <- function(d, dep) {
  dep_rx <- unique(split_receiver(stats::na.omit(dep$receiver_serial))$serial)
  det_rx <- unique(split_receiver(stats::na.omit(d$receiver_serial))$serial)
  dep_st <- unique(stats::na.omit(dep$station_id))
  list(heard = sum(det_rx %in% dep_rx), deployed = length(dep_rx),
       unmatched = sum(!det_rx %in% dep_rx),
       stations_heard = sum(unique(stats::na.omit(d$station_id)) %in% dep_st),
       stations = length(dep_st))
}

#' @export
summary.detset <- function(object, ...) {
  d <- as_dt(object$detections)[!is.na(animal_id)]
  per <- d[, list(
    transmitters = length(unique(transmitter)), detections = .N,
    detection_days = length(unique(as.Date(datetime))),
    stations = length(unique(stats::na.omit(station_id))),
    first = min(datetime), last = max(datetime)
  ), by = "animal_id"]
  data.table::setorderv(per, "animal_id")
  structure(list(
    tables = data.frame(
      table = c("detections", "deployments", "animals", "listening", "log"),
      rows = c(nrow(object$detections), nrow(object$deployments),
               if (is.null(object$animals)) 0L else nrow(object$animals),
               nrow(object$listening), nrow(object$log))
    ),
    animals = as_df(per)
  ), class = "summary.detset")
}

#' @export
print.summary.detset <- function(x, ...) {
  cat("Rows per table:\n")
  tb <- x$tables
  tb$rows <- fmt_n(tb$rows)
  print(tb, row.names = FALSE)
  cat(sprintf("\nPer animal (%s; detection_days counts UTC dates):\n",
              n_of(nrow(x$animals), "animal")))
  print(utils::head(x$animals, 20), row.names = FALSE)
  if (nrow(x$animals) > 20) cat(sprintf("... %s\n", n_of(nrow(x$animals) - 20, "more animal")))
  invisible(x)
}

# ---- Internal: times, deployments, animals ----
prepare_times <- function(df, cols, tz, label) {
  if (!is.data.frame(df)) stop_detqc("`%s` must be a data frame, not %s.", label, class(df)[1])
  df <- as_df(df)
  for (col in intersect(cols, names(df))) {
    df[[col]] <- utc_column(df[[col]], tz, paste0(label, "$", col))
  }
  df
}

utc_column <- function(v, tz, label) {
  if (is.character(v) || is.factor(v)) return(parse_utc(as.character(v), tz, label))
  if (is.logical(v) && all(is.na(v))) return(empty_utc(length(v)))
  if (inherits(v, "POSIXct") && !identical(attr(v, "tzone"), "UTC")) {
    stop_detqc("`%s` is in time zone \"%s\"; convert it to UTC first.", label,
               paste(attr(v, "tzone"), collapse = ""))
  }
  v
}

# OTN's hfx_deployments.csv records "caught: 2014-06-22" against a deployment
# on 2014-06-22, both date-only: a zero-length interval, dropped and counted.
check_deployments <- function(dep, det) {
  dep <- drop_backward_deployments(dep, det)
  n_backward <- attr(dep, "bad_window")
  if (anyNA(dep$deploy_datetime)) {
    n_na <- sum(is.na(dep$deploy_datetime))
    stop_detqc("%s %s no `deploy_datetime`.", n_of(n_na, "deployment"), verb(n_na, "has", "have"))
  }
  bad_pos <- which(is.na(dep$deploy_lat) | abs(dep$deploy_lat) > 90 |
                     is.na(dep$deploy_lon) | abs(dep$deploy_lon) > 180)
  if (length(bad_pos)) {
    stop_detqc("Deployment row %s has no valid position (station %s).", fmt_n(bad_pos[1]),
               dep$station_id[bad_pos[1]])
  }
  zero <- !is.na(dep$recover_datetime) & dep$recover_datetime == dep$deploy_datetime
  if (any(zero)) {
    warn_detqc("%s %s at the moment %s and %s dropped, such as station %s.",
               n_of(sum(zero), "deployment"), verb(sum(zero), "ends", "end"),
               verb(sum(zero), "it starts", "they start"), verb(sum(zero), "was", "were"),
               dep$station_id[which(zero)[1]])
    dep <- dep[!zero, , drop = FALSE]
    rownames(dep) <- NULL
  }
  attr(dep, "zero_length") <- sum(zero)
  attr(dep, "bad_window") <- n_backward
  dep
}

# A deployment that ends before it starts can hold no detection. GLATOS gives
# every user the network-wide receiver file, where one such row in another
# project would stop detset() for every user. A row on a receiver, or at a
# station for a row with no serial, that some detection is on stops, and says
# where to look; any other is set aside with a warning and counted.
drop_backward_deployments <- function(dep, det) {
  bad <- which(!is.na(dep$recover_datetime) & dep$recover_datetime < dep$deploy_datetime)
  attr(dep, "bad_window") <- 0L
  if (!length(bad)) return(dep)
  key <- split_receiver(dep$receiver_serial[bad])$serial
  used <- data.table::fifelse(is.na(key), dep$station_id[bad] %in% det$station_id,
                              key %in% split_receiver(det$receiver_serial)$serial)
  where <- function(i) {
    proj <- intersect(c("glatos_project", "otn_array", "glatos_array"), names(dep))
    sprintf("row %s: receiver %s at station %s%s, deployed %s, recovered %s", fmt_n(i),
            dep$receiver_serial[i], dep$station_id[i],
            if (length(proj)) sprintf(" (%s %s)", proj[1], dep[[proj[1]]][i]) else "",
            format(dep$deploy_datetime[i], "%Y-%m-%d"), format(dep$recover_datetime[i], "%Y-%m-%d"))
  }
  n <- length(bad)
  ends <- sprintf("%s %s before %s", n_of(n, "deployment row"), verb(n, "ends", "end"),
                  verb(n, "it starts", "they start"))
  if (any(used)) {
    stop_detqc(paste("%s,%s on a receiver the detections are on, such as %s.",
                     "check_detset(list(detections = , deployments = )) lists every such row",
                     "under recover_before_deploy; repair or drop %s first."),
               ends, if (n == 1) "" else sprintf(" %s of them", fmt_n(sum(used))),
               where(bad[used][1]), verb(n, "it", "them"))
  }
  warn_detqc("%s, such as %s. No detection is on %s, so %s set aside.", ends, where(bad[1]),
             verb(n, "its receiver", "their receivers"), verb(n, "it was", "they were"))
  out <- dep[-bad, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "bad_window") <- length(bad)
  out
}

# A transmitter can be reused on a second animal after the first is lost
# (a reused tag). Filling uses match() and a rolling join, so the
# detections never gain rows.
dedupe_animals <- function(ani) {
  dt <- unique(as_dt(ani))
  per_tx <- dt[, list(n = length(unique(animal_id))), by = "transmitter"]
  list(table = as_df(dt), duplicates = nrow(ani) - nrow(dt),
       reused = per_tx$transmitter[per_tx$n > 1])
}

fill_animal_ids <- function(det, ani, reused) {
  one <- ani[!ani$transmitter %in% reused, c("transmitter", "animal_id")]
  one <- one[!duplicated(one$transmitter), , drop = FALSE]
  from_ani <- one$animal_id[match(det$transmitter, one$transmitter)]
  by_time <- reused_by_release(det, ani, reused)
  timed <- !is.na(by_time)
  from_ani[timed] <- by_time[timed]
  fill <- is.na(det$animal_id) & !is.na(from_ani)
  differ <- !is.na(det$animal_id) & !is.na(from_ani) & det$animal_id != from_ani
  conflicts <- sum(differ)
  det$animal_id[fill] <- from_ani[fill]
  out <- c(filled = sum(fill), conflicts = conflicts, by_release = sum(fill & timed),
           before_release = attr(by_time, "before"), timed_tx = attr(by_time, "timed_tx"))
  attr(out, "detections") <- det
  if (conflicts) {
    k <- which(differ)[1]
    attr(out, "conflict_example") <- sprintf(
      "transmitter %s named %s here and %s in the animals table",
      det$transmitter[k], det$animal_id[k], from_ani[k]
    )
  }
  out
}

# A reused transmitter whose animals all have distinct release times gives
# each detection to the animal released last at or before it: the second
# fish takes over the code at its release. A detection before the first
# release, or of a transmitter whose animals lack or share a release time,
# is left NA, because nothing says whose it is.
reused_by_release <- function(det, ani, reused) {
  out <- rep(NA_character_, nrow(det))
  attr(out, "before") <- 0L
  attr(out, "timed_tx") <- 0L
  if (!length(reused) || !"release_datetime" %in% names(ani)) return(out)
  a <- as_dt(ani)[transmitter %in% reused, list(transmitter, animal_id, release_datetime)]
  a <- unique(a, by = c("transmitter", "animal_id"))
  ok <- a[, list(ok = !anyNA(release_datetime) && !anyDuplicated(release_datetime)),
          by = "transmitter"]
  a <- a[transmitter %in% ok$transmitter[ok$ok]]
  if (!nrow(a)) return(out)
  q <- data.table::data.table(transmitter = det$transmitter, release_datetime = det$datetime,
                              i = seq_len(nrow(det)))[transmitter %in% a$transmitter]
  hit <- a[q, on = list(transmitter, release_datetime), roll = TRUE, mult = "last"]
  out[hit$i] <- hit$animal_id
  attr(out, "before") <- sum(is.na(hit$animal_id))
  attr(out, "timed_tx") <- length(unique(a$transmitter))
  out
}

count_animals <- function(det) {
  length(unique(stats::na.omit(det$animal_id)))
}

# ---- Internal: the half-open deployment join ----
# Returns, per detection, the row of `dep` whose interval holds it, or NA.
# Overlapping deployments of one receiver are a data error; the later one
# wins so the result never has more rows than the detections. Attribute
# `by_bare` flags the detections matched on the bare serial, `by_station`
# those matched on their own station_id to a deployment with no serial: an
# OTN deployment CSV names stations only. detfield matches such listening rows by
# station in the same way.
match_deployment <- function(det, dep) {
  row <- match_by_serial(det, dep)
  by_station <- rep(FALSE, length(row))
  st <- if ("station_id" %in% names(det)) as.character(det$station_id) else NULL
  bare_dep <- is.na(dep$receiver_serial)
  miss <- which(is.na(row))
  if (length(miss) && !is.null(st) && any(bare_dep)) {
    key <- data.table::fifelse(bare_dep, as.character(dep$station_id), NA_character_)
    r3 <- join_interval(st[miss], det$datetime[miss], dep, key)
    row[miss] <- r3
    by_station[miss] <- !is.na(r3)
  }
  attr(row, "by_station") <- by_station
  row
}

match_by_serial <- function(det, dep) {
  row <- join_interval(det$receiver_serial, det$datetime, dep, dep$receiver_serial)
  by_bare <- rep(FALSE, length(row))
  miss <- which(is.na(row) & !is.na(det$receiver_serial))
  if (length(miss) && nrow(dep)) {
    sp <- split_receiver(dep$receiver_serial)
    # A bare serial shared by two receiver forms, such as "VR2W-123456" and
    # "HR2-123456", names neither.
    forms <- tapply(dep$receiver_serial, sp$serial, function(v) length(unique(v)))
    key <- sp$serial
    key[key %in% names(forms)[forms > 1]] <- NA_character_
    r2 <- join_interval(split_receiver(det$receiver_serial[miss])$serial, det$datetime[miss],
                        dep, key)
    row[miss] <- r2
    by_bare[miss] <- !is.na(r2)
  }
  attr(row, "by_bare") <- by_bare
  row
}

# data.table's equi-join matches an NA key to an NA key, which would give a
# detection with no serial the station of whichever deployment with no
# serial was open. Both sides drop NA keys.
join_interval <- function(serial, datetime, dep, dep_key) {
  far <- as.POSIXct("9999-12-31", tz = "UTC")
  keep <- which(!is.na(dep_key))
  d <- data.table::data.table(
    rx = dep_key[keep], start = dep$deploy_datetime[keep],
    end = data.table::fifelse(is.na(dep$recover_datetime[keep]), far, dep$recover_datetime[keep]),
    row = keep
  )
  data.table::setorderv(d, c("rx", "start"))
  q <- data.table::data.table(rx = as.character(serial), datetime = datetime)
  hit <- d[q, on = list(rx, start <= datetime, end > datetime), which = TRUE, mult = "last"]
  out <- d$row[hit]
  out[is.na(serial)] <- NA_integer_
  out
}

join_stations <- function(det, dep) {
  row <- match_deployment(det, dep)
  bare <- attr(row, "by_bare")
  from_dep <- dep$station_id[row]
  own <- if ("station_id" %in% names(det)) as.character(det$station_id) else NA_character_
  own <- rep_len(own, nrow(det))
  replaced <- sum(!is.na(from_dep) & !is.na(own) & from_dep != own)
  det$station_id <- data.table::fifelse(is.na(from_dep), own, from_dep)
  det$receiver_serial[bare] <- dep$receiver_serial[row[bare]]
  list(detections = det, matched = sum(!is.na(row)), replaced = replaced, by_bare = sum(bare),
       by_station = sum(attr(row, "by_station")))
}

warn_low_match <- function(joined, det, dep) {
  n <- nrow(det)
  if (!n || !nrow(dep) || joined$matched >= n / 2) return(invisible(FALSE))
  dep_rx <- stats::na.omit(dep$receiver_serial)
  warn_detqc(paste(
    "Only %s of %s %s a deployment by receiver_serial and time.",
    "Detections name receivers such as \"%s\"; %s."
  ), fmt_n(joined$matched), n_of(n, "detection"), verb(joined$matched, "matches", "match"),
  stats::na.omit(det$receiver_serial)[1],
  if (length(dep_rx)) sprintf("deployments name \"%s\"", dep_rx[1]) else
    "the deployments carry no receiver_serial")
  invisible(TRUE)
}

# Receiver-days a deployment covered, by UTC date. A deployment still in the
# water is counted to the last detection in the set.
build_listening <- function(dep, det) {
  last <- if (nrow(det)) max(det$datetime, na.rm = TRUE) else max(dep$deploy_datetime)
  end <- data.table::fifelse(is.na(dep$recover_datetime), last, dep$recover_datetime - 1)
  d <- data.table::data.table(
    receiver_serial = dep$receiver_serial, station_id = dep$station_id,
    d0 = as.Date(dep$deploy_datetime), d1 = as.Date(end)
  )[d1 >= d0]
  if (!nrow(d)) {
    return(data.frame(receiver_serial = character(), station_id = character(),
                      date = as.Date(character())))
  }
  out <- d[, list(date = seq(d0, d1, by = "day")),
           by = list(receiver_serial, station_id, k = seq_len(nrow(d)))]
  out <- unique(out[, list(receiver_serial, station_id, date)])
  data.table::setorderv(out, c("receiver_serial", "date"))
  as_df(out)
}

# Column of the animals table read inside the rolling join above.
utils::globalVariables("release_datetime")
