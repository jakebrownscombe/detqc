# ---- The receiver listening audit ----

#' Audit which receivers listened on which days
#'
#' Builds one row per receiver serial and UTC date on which a deployment had
#' the receiver in the water, and decides whether the receiver was
#' listening. A missing detection is evidence of absence only on a day the
#' receiver was listening, so a positioning or residency analysis reads this
#' table instead of the deployment dates alone. The table is keyed by the
#' receiver serial: a receiver swap leaves two receivers at one station on
#' one day, each with its own row.
#'
#' A deployment with no `receiver_serial`, as in OTN's deployment CSV and
#' stations-receivers file, is keyed by its `station_id` instead. Its rows
#' keep `receiver_serial` as `NA`, and a detection at that station counts
#' for them unless its receiver has a deployment of its own. This is the
#' rule `detfield::bin_detections()` uses for such rows.
#'
#' Each receiver-day gets one `status`:
#' * `"not_downloaded"`: a day that starts at or after the end of the
#'   receiver's data, so its data are not in the set. The data end at the
#'   recorded download (`download_datetime`) when it comes before the
#'   recovery. Not listening, also on a day with detections after the
#'   download (see below).
#' * `"silent_run"`: a day of a receiver at a station that logged no
#'   detection of any transmitter over all its days in the table (a day or
#'   more in the water), or, when `max_silent_days` is given, a day in a run
#'   of at least that many consecutive silent days. Not listening: a
#'   receiver that hears nothing while the array logs detections is more
#'   likely dead than alone.
#' * `"partial"`: a day the receiver's data cover in part: a deployment or
#'   recovery day, or the day of a download that comes before the recovery.
#'   `deployed_share` gives the share of the day in the water, so it is 1 on
#'   such a download day. Listening; [as_detfield_input()] ends the
#'   deployment at the download, so the hours after it are not used.
#' * `"reference_silent"`: a full day with no detection of any transmitter,
#'   on which the receiver was expected to hear at least one reference tag.
#'   Not listening.
#' * `"reference_missed"`: a full day on which the receiver heard none of the
#'   reference tags it is expected to hear but logged other detections. Kept
#'   as listening, because a receiver that logs detections was working; the
#'   reference tag may have been out of range that day.
#' * `"silent"`: a full day with no detection of any transmitter and no
#'   reference tag to say whether the receiver worked. Kept as listening,
#'   because a quiet day can be a day with no animal near; check these days.
#' * `"listening"`: every other day.
#'
#' **Receiver logs.** With `logs`, from [read_receiver_logs()], the audit
#' also reads the receiver's own status records (DIAG, TEMP, ATTITUDE,
#' BATTERY and HEALTH, hourly on a VR2Tx), on the full days, before the
#' download, on which the receiver logged no detection. A receiver's log
#' covers the UTC dates from its first to its last record, events included,
#' in each file; the receiver's EVENT_OFFLOAD record, written at download,
#' closes the span. On such a day inside the span:
#' * `"off"`: no status record at all. The receiver was not running, as
#'   after a power failure or a flat battery. Not listening.
#' * `"deaf"`: status records, and a reference tag expected and not heard
#'   (the day would otherwise be `"reference_silent"`). The receiver ran and
#'   decoded nothing, the reference tag included. Not listening. The
#'   `pings` column shows whether its hydrophone counted pulses that day: if
#'   it did, noise or a reference tag out of range are more likely than a
#'   dead hydrophone.
#' * `"logging_silent"`: status records and no reference tag expected (the
#'   day would otherwise be `"silent"`). The receiver ran and heard nothing.
#'   Kept as listening, as a silent day is.
#' * `"listening"`: status records whose `log_detections` count detections.
#'   A receiver counts every transmitter it decodes, whatever project the
#'   tag belongs to and whether or not the detection is in the set, so the
#'   receiver heard that day. A day that would otherwise be
#'   `"reference_silent"` or `"deaf"` becomes `"reference_missed"`: the
#'   receiver heard something, though not the reference tag. A DIAG record
#'   counts the detections since the receiver's previous DIAG record, so a
#'   count is given to a day only when that interval lies inside the day:
#'   the record at midnight, the first record of a file and the first after
#'   a gap across midnight are left out. The log record counts these days.
#'
#' When the evidence disagrees: a detection in the set beats the log, so a
#' day with a detection keeps its status, and a day with detections and no
#' status record inside the span is counted in the log record. A logged
#' count beats the rules that make a quiet day not listening: the day is
#' listening, and a receiver whose log counted detections on any day did
#' not hear nothing on all its days, so the first `"silent_run"` rule above
#' does not apply to it. `"off"` beats `"silent_run"`, and no count can lift an `"off"` day:
#' counts are read from status records only, so a day with a count has a
#' status record. A run of `"logging_silent"` days still becomes
#' `"silent_run"` under the run rules below, because a log shows that a
#' receiver ran and cannot show that it heard; `known_listening` turns those days back
#' to `"logging_silent"`. Days outside every span of their receiver, and
#' every day of a receiver with no log, get the statuses above, as without
#' `logs`. `logs` may also be a table of your own with `receiver_serial`,
#' `datetime` (UTC) and `record_type`; a record type other than `EVENT`,
#' `EVENT_FAULT`, `EVENT_INIT` and `EVENT_OFFLOAD` counts as a status record.
#'
#' The audit reads every transmitter in the set. Cut to one species, it
#' marks as silent the receivers that heard only other species; run it on
#' every transmitter the receivers logged, reference tags included, and cut
#' with [filter_animals()] after it.
#'
#' **Where evidence of listening is lost.** A detection removed before the
#' audit is no longer evidence that the receiver heard. Three steps remove
#' them: an export that holds one project's tags (below); [filter_animals()]
#' run before the audit; and the filters of false and unwanted detections,
#' [filter_min_lag()], [filter_post_release()] and [filter_duplicates()],
#' whose removed detections were still decoded by a working receiver. On the
#' example, run after those three filters, the audit gives 8 of 180
#' receiver-days `"silent"` that it gives `"listening"` when run before
#' them, as `vignette("detqc")` runs it. A receiver's log counts every
#' detection it decoded, so `logs` recover such days, and the evidence an
#' export never held.
#'
#' A GLATOS or OTN detection export is cut in the same way before it reaches
#' you: it holds one project's tags only. A partner receiver that worked but
#' heard none of them, or heard none for `max_silent_days`, reads as
#' `"silent_run"`. In a GLATOS export, a partner receiver that logged
#' detections in a month can still read as `"silent_run"` for many of that
#' month's receiver-days, because none of its detections were of the export's
#' tags. The audit warns with these counts
#' when any day is `"silent_run"` and the set looks cut: its detections come
#' from such an export, [filter_animals()] ran before the audit, or the
#' detections hold fewer species than the animals table lists. The log record names
#' the reasons, and [processing_report()] repeats them beside the count.
#' `known_listening` names the
#' receivers known to have worked, such as those in the network's receiver
#' file with data, so their silent days stay `"silent"` and listening.
#'
#' A receiver is expected to hear a reference tag when `reference_tags` pairs
#' them by `receiver_serial`. Without that column, a receiver is expected to
#' hear a tag when it heard it on at least `min_share` of its full days
#' within the tag's span, from the tag's first to its last detection by any
#' receiver. Days outside that span are not tested against that tag.
#'
#' A deployment still in the water listens until its download, or, with no
#' download time, until the last detection in the set. A deployment with no
#' recovery time ends where the same receiver (by `receiver_serial`) is
#' deployed again.
#'
#' **Detections after the download.** A deployment's data end at its
#' recorded download, whatever the detections say. A detection after that
#' download and before the deployment ends stays in the set. Its day stays
#' `"not_downloaded"`, which is not listening, and the detection is counted
#' in `detections`. Such a detection can mean a download time recorded too
#' early, a later download missing from the sheet, a receiver that kept
#' logging after the download, or a clock that was wrong. [detset()] marks
#' these detections in the column `after_download`, and the check
#' `detections_after_download` of [check_detset()] lists them by deployment.
#' A detection the receiver logged at its next deployment belongs to that
#' deployment. A message counts these detections, their deployments and the
#' `"not_downloaded"` receiver-days that hold them, and the log record
#' repeats the counts. Where the sheet is wrong, correct the download time
#' and build the set again; where the detections are false, remove them
#' before the audit. The table covers the UTC dates from the first to the
#' last detection in the set: outside them the set holds no data to audit,
#' as when it was cut to one month.
#'
#' @param x A [detset()].
#' @param reference_tags Optional data frame with `transmitter`, and
#'   optionally `receiver_serial` to name the receivers expected to hear each.
#'   When it is `NULL` and the deployments carry `receiver_transmitter` (see
#'   [read_deployments()]), those ids are the reference tags, with a
#'   message, and with no `receiver_serial`: a receiver is expected to hear
#'   one when it heard it on `min_share` of its full days, so a receiver that
#'   never logs its own built-in transmitter is not held to it. An id the
#'   animals table also lists is left out with a warning.
#' @param min_share Share of full days, from 0 to 1, a receiver must hear a
#'   reference tag to be expected to hear it every day, when `reference_tags`
#'   has no `receiver_serial`.
#' @param max_silent_days Optional length, in days, of a run of silent days
#'   that marks the receiver as not listening for the whole run. No default:
#'   without it only a receiver silent on every day is `"silent_run"`.
#' @param known_listening Optional receivers known to have worked: a
#'   character vector of receiver serials, or a data frame with
#'   `receiver_serial` and, optionally, `date` for receiver-days. Their
#'   `"silent_run"` days become `"silent"`, which is listening.
#' @param logs Optional receiver logs, from [read_receiver_logs()].
#' @return `x` with `listening` replaced by the audit and one log record. The
#'   table has `receiver_serial` (`NA` for a deployment with no serial),
#'   `station_id`, `date`, `deployed_share`,
#'   `detections` (all transmitters), `reference_detections` (`NA` where no
#'   reference tag is expected), `status` and `listening` (logical). With
#'   `logs` it also has `log_records`, the status records of the day (`NA`
#'   outside the receiver's log), and `pings`, the pulses its DIAG records
#'   counted (`NA` without them), before `status`. The table's attribute
#'   `data_end` holds each deployment's data end as the audit set it
#'   (`receiver_serial`, `station_id`, `deploy_datetime`, `data_end`), and
#'   [as_detfield_input()] reads these ends.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' dets <- audit_listening(dets, known_listening = unique(dets$deployments$receiver_serial))
#' table(dets$listening$status)
#' # With the receivers' own logs
#' log_files <- list.files(system.file("extdata", "receiver_logs", package = "detqc"),
#'                         full.names = TRUE)
#' logs <- read_receiver_logs(log_files)
#' dets <- audit_listening(dets, known_listening = unique(dets$deployments$receiver_serial),
#'                         logs = logs)
#' table(dets$listening$status)
audit_listening <- function(x, reference_tags = NULL, min_share = 0.5, max_silent_days = NULL,
                            known_listening = NULL, logs = NULL) {
  need_detset(x)
  need_share(min_share)
  if (!is.null(max_silent_days)) need_number(max_silent_days, "max_silent_days", 1)
  known <- known_days(known_listening)
  logs <- check_logs(logs)
  if (is.null(reference_tags)) {
    reference_tags <- deployment_reference_tags(x$deployments, x$animals, "audit_listening")
  }
  days <- audit_receiver_days(x)
  det <- data.table::data.table(rx = detection_keys(x$detections, x$deployments),
                                date = as.Date(x$detections$datetime),
                                transmitter = x$detections$transmitter)
  heard <- det[, list(detections = .N), by = c("rx", "date")]
  days[, detections := heard$detections[match(paste(rx, date), paste(heard$rx, heard$date))]]
  days[is.na(detections), detections := 0L]
  ref <- audit_reference_counts(det, days, reference_tags, min_share)
  days[, reference_detections := ref$count]
  days[, status := audit_day_status(days)]
  ev <- log_evidence(logs, days)
  days[, base := status]
  days[, status := silent_runs(days, max_silent_days)]
  n_known <- keep_known_listening(days, known)
  cut <- warn_cut_silence(days, x)
  days[, listening := !status %in% not_listening]
  late <- late_detections(x, days, det)
  note_late_detections(late)
  cols <- c("receiver_serial", "station_id", "date", "deployed_share", "detections",
            "reference_detections", if (!is.null(logs)) c("log_records", "pings"), "status",
            "listening")
  out <- as_df(days[, cols, with = FALSE])
  attr(out, "data_end") <- audit_data_end(x)
  before <- x
  x$listening <- out
  levels <- c("listening", "partial", "silent", "reference_missed", "reference_silent",
              "silent_run", "not_downloaded", if (!is.null(logs)) log_statuses)
  tab <- table(factor(out$status, levels))
  parameters <- list(reference_tags = reference_parameter(reference_tags),
                     min_share = min_share, max_silent_days = max_silent_days,
                     known_listening = known_listening)
  if (!is.null(logs)) parameters$logs <- nrow(logs)
  log_step(x, "audit_listening", parameters, before, 0L, sprintf(
    paste("%s of %s: %s; %s, %s; %s of receivers known to work kept as %s%s%s%s"),
    n_of(nrow(out), "receiver-day"), receiver_count(days),
    paste(sprintf("%s %s", fmt_n(as.integer(tab)), names(tab)), collapse = ", "),
    n_of(ref$n_tags, "reference tag"), n_of(ref$n_pairs, "expected receiver pair"),
    n_of(n_known, "silent_run day"),
    if (is.null(logs)) "silent" else "silent or logging_silent", log_note(ev), cut_note(cut),
    late_note(late)
  ), fn = "audit_listening")
}

# The detections after their deployment's recorded download, the
# deployments that hold them, and the receiver-days of the audit table that
# hold them and are not_downloaded. `det` holds each detection's receiver
# key and date, as audit_listening() builds it.
late_detections <- function(x, days, det) {
  idx <- after_download_index(x$deployments, x$detections)
  hit <- !is.na(idx)
  key <- unique(paste(det$rx[hit], det$date[hit]))
  off <- days$status == "not_downloaded" & paste(days$rx, days$date) %in% key
  list(detections = sum(hit), deployments = length(unique(idx[hit])), days = sum(off))
}

late_note <- function(late) {
  if (!late$detections) return("")
  sprintf("; %s after the recorded download on %s, on %s not_downloaded",
          n_of(late$detections, "detection"), n_of(late$deployments, "deployment"),
          n_of(late$days, "receiver-day"))
}

# The message of audit_listening() when detections fall after a recorded
# download.
note_late_detections <- function(late) {
  if (!late$detections) return(invisible(NULL))
  message(sprintf(paste(
    "audit_listening(): %s on %s %s after the recorded download. The data end at the",
    "download: %s with such detections %s not_downloaded and not listening. The detections",
    "stay in the set, marked by `after_download`; check_detset(checks =",
    "\"detections_after_download\") lists them. They can mean a download time recorded too",
    "early, a later download missing from the sheet, a receiver that kept logging, or a clock",
    "problem."
  ), n_of(late$detections, "detection"), n_of(late$deployments, "deployment"),
  verb(late$detections, "falls", "fall"), n_of(late$days, "receiver-day"),
  verb(late$days, "is", "are")))
  invisible(NULL)
}

cut_note <- function(cut) {
  if (!length(cut)) return("")
  paste0("; audited on a cut set: ", paste(cut, collapse = "; "))
}

# known_listening as a table of bare serials and, optionally, dates.
known_days <- function(known) {
  if (is.null(known)) return(NULL)
  if (is.character(known)) known <- data.frame(receiver_serial = known, stringsAsFactors = FALSE)
  if (!is.data.frame(known) || !"receiver_serial" %in% names(known)) {
    stop_detqc(paste("`known_listening` must be receiver serials, or a data frame with",
                     "`receiver_serial` and, optionally, `date`."))
  }
  out <- data.table::data.table(rx = split_receiver(as.character(known$receiver_serial))$serial)
  if ("date" %in% names(known)) out[, date := as.Date(known$date)]
  unique(out)
}

# Silent runs of receivers known to work are silent days, which listen; a
# day the receiver's log showed it running goes back to logging_silent.
keep_known_listening <- function(days, known) {
  if (is.null(known)) return(0L)
  on <- if ("date" %in% names(known)) {
    paste(days$rx, days$date) %in% paste(known$rx, known$date)
  } else {
    days$rx %in% known$rx
  }
  hit <- on & days$status == "silent_run"
  if (any(hit)) {
    days[hit, status := data.table::fifelse(base == "logging_silent", "logging_silent",
                                            "silent")]
  }
  sum(hit)
}

# A detection set cut before the audit makes working receivers look silent:
# a GLATOS or OTN export holds one project's tags, and a set cut to one
# species marks receiver-days silent_run that heard other species, with no
# warning. Returns the reasons the set may be cut, after warning with them
# when any day is silent_run.
warn_cut_silence <- function(days, x) {
  run <- days$status == "silent_run"
  if (!any(run)) return(character())
  why <- cut_reasons(x)
  if (!length(why)) return(character())
  heard <- unique(days$rx[days$detections > 0])
  warn_detqc(paste("audit_listening(): %s of %s receiver-days are silent_run, %s of them on",
                   "receivers that logged detections in the set, and the set may be cut: %s. A",
                   "receiver that heard only transmitters cut away worked. Audit every",
                   "transmitter the receivers logged and cut with filter_animals() after the",
                   "audit, or give the receivers known to work in `known_listening`."),
             fmt_n(sum(run)), fmt_n(nrow(days)), fmt_n(sum(run & days$rx %in% heard)),
             paste(why, collapse = "; "))
  why
}

cut_reasons <- function(x) {
  src <- x$meta$sources
  fm <- if (is.data.frame(src) && "format" %in% names(src)) {
    unique(src$format[src$table == "detections"])
  } else {
    character()
  }
  net <- intersect(fm, c("glatos", "otn"))
  why <- character()
  if (length(net)) {
    why <- c(why, sprintf("the detections come from %s exports, which hold one project's tags",
                          paste(toupper(net), collapse = " and ")))
  }
  if (any(grepl("^filter_animals", x$log$step))) why <- c(why, "filter_animals() ran before it")
  ani <- x$animals
  if (!is.null(ani) && "species" %in% names(ani)) {
    listed <- unique(stats::na.omit(tolower(ani$species)))
    heard <- unique(stats::na.omit(tolower(ani$species[ani$animal_id %in% x$detections$animal_id])))
    if (length(heard) < length(listed)) {
      why <- c(why, sprintf("the detections hold %s of the %s species the animals table lists",
                            fmt_n(length(heard)), fmt_n(length(listed))))
    }
  }
  why
}

# One row per receiver and UTC date with the share of the day a deployment
# covered. A receiver's data end as deployment_data_end() says; a deployment
# still in the water with no download ends at the last detection in the
# set. `rx` is the key: the bare serial, or "station:<station_id>" for a
# deployment with no serial, so that the serial-less deployments of a sheet
# do not share one NA receiver. `data_share` is the
# share of the day the receiver's data cover, below 1 on a download day;
# full-day rules read it.
audit_receiver_days <- function(x, span = TRUE) {
  dep <- as_dt(x$deployments)
  last <- if (nrow(x$detections)) max(x$detections$datetime) else max(dep$deploy_datetime)
  end <- data.table::fifelse(is.na(dep$recover_datetime), last, dep$recover_datetime)
  nxt <- next_deployment_start(x$deployments)
  end <- data.table::fifelse(is.na(dep$recover_datetime) & !is.na(nxt) & nxt < end, nxt, end)
  data_end <- deployment_data_end(x$deployments)
  data_end <- data.table::fifelse(is.na(data_end) | data_end > end, end, data_end)
  d <- data.table::data.table(receiver_serial = dep$receiver_serial, station_id = dep$station_id,
                              rx = listening_key(dep$receiver_serial, dep$station_id),
                              s = as.numeric(dep$deploy_datetime), e = as.numeric(end),
                              de = as.numeric(data_end))[e > s]
  rows <- d[, list(day = seq(floor(s / 86400), ceiling(e / 86400) - 1)),
            by = list(receiver_serial, station_id, rx, s, e, de)]
  rows[, deployed_share := (pmin(e, (day + 1) * 86400) - pmax(s, day * 86400)) / 86400]
  rows[, data_share := pmax(0, pmin(de, (day + 1) * 86400) - pmax(s, day * 86400)) / 86400]
  rows[, after_download := day * 86400 >= de]
  rows <- rows[order(-deployed_share, -s)]
  out <- rows[, list(receiver_serial = receiver_serial[1], station_id = station_id[1],
                     deployed_share = min(1, sum(deployed_share)),
                     data_share = min(1, sum(data_share)),
                     after_download = all(after_download)), by = c("rx", "day")]
  out[, date := as.Date(day, origin = "1970-01-01")]
  if (span && nrow(x$detections)) {
    dates <- as.Date(range(x$detections$datetime))
    out <- out[date >= dates[1] & date <= dates[2]]
  }
  data.table::setorderv(out, c("receiver_serial", "rx", "date"), na.last = TRUE)
  out
}

# The start of the same receiver's next deployment, by serial; NA for a
# deployment with no serial or no later deployment of its receiver. A
# receiver cannot listen at two stations at once, so a deployment with no
# recovery time ends there.
next_deployment_start <- function(dep) {
  key <- split_receiver(dep$receiver_serial)$serial
  s <- as.numeric(dep$deploy_datetime)
  out <- rep(NA_real_, nrow(dep))
  ok <- !is.na(key) & !is.na(s)
  if (any(ok)) {
    d <- data.table::data.table(i = which(ok), rk = key[ok], s = s[ok])
    d[, nx := {
      u <- sort(unique(s))
      u[findInterval(s, u) + 1L]
    }, by = "rk"]
    out[d$i] <- d$nx
  }
  as.POSIXct(out, origin = "1970-01-01", tz = "UTC")
}

listening_key <- function(serial, station) {
  data.table::fifelse(is.na(serial), paste0("station:", station),
                      split_receiver(serial)$serial)
}

# Each detection's receiver key. A detection at a station whose deployments
# carry no serial counts for the station's rows, unless its own receiver has
# a deployment with a serial, as detfield::bin_detections() matches them.
detection_keys <- function(det, dep) {
  rx <- split_receiver(det$receiver_serial)$serial
  bare <- unique(dep$station_id[is.na(dep$receiver_serial)])
  if (length(bare)) {
    known <- unique(split_receiver(stats::na.omit(dep$receiver_serial))$serial)
    at <- !is.na(det$station_id) & det$station_id %in% bare & !rx %in% known
    rx[at] <- paste0("station:", det$station_id[at])
  }
  rx
}

# Each deployment's end: its recovery, else the start of the same
# receiver's next deployment, else NA for a deployment still in the water.
deployment_end <- function(dep) {
  rec <- dep$recover_datetime
  nxt <- next_deployment_start(dep)
  data.table::fifelse(is.na(rec) & !is.na(nxt), nxt, rec)
}

# TRUE for a deployment whose recorded download falls inside it, before its
# end. Its data end at that download.
download_cuts <- function(dep, end = deployment_end(dep)) {
  if (!"download_datetime" %in% names(dep)) return(rep(FALSE, nrow(dep)))
  dl <- dep$download_datetime
  !is.na(dl) & dl > dep$deploy_datetime & (is.na(end) | dl < end)
}

# The time each deployment's data end: its recorded download when that
# comes before the deployment's end, else the end of deployment_end(). The
# set holds no data after a download; detections after it are flagged by
# after_download_index().
deployment_data_end <- function(dep) {
  end <- deployment_end(dep)
  cut <- download_cuts(dep, end)
  if (!any(cut)) return(end)
  data.table::fifelse(cut, dep$download_datetime, end)
}

# For each detection, the row of `dep` whose recorded download it follows,
# when it falls after that download and before the deployment ends; NA for
# every other detection. A detection the receiver logged at its next
# deployment belongs to that deployment. Detections are keyed to receivers
# as the audit keys them, so a serial-less deployment holds the detections
# at its station.
after_download_index <- function(dep, det) {
  out <- rep(NA_integer_, nrow(det))
  if (!nrow(det) || !nrow(dep)) return(out)
  end <- deployment_end(dep)
  cut <- download_cuts(dep, end)
  if (!any(cut)) return(out)
  far <- as.POSIXct("9999-12-31", tz = "UTC")
  d <- data.table::data.table(i = which(cut),
                              rx = listening_key(dep$receiver_serial, dep$station_id)[cut],
                              s = dep$download_datetime[cut],
                              e = data.table::fifelse(is.na(end[cut]), far, end[cut]))
  q <- data.table::data.table(k = seq_len(nrow(det)), rx = detection_keys(det, dep),
                              t = det$datetime)
  hit <- d[q, on = list(rx, s <= t, e > t), nomatch = NULL, list(i, k)]
  if (!nrow(hit)) return(out)
  # Overlapping deployments of one receiver are a data error that another
  # check reports; the later deployment takes the detection, as in the join.
  hit <- hit[, list(i = max(i)), by = "k"]
  out[hit$k] <- hit$i
  out
}

# A share of full days cannot exceed 1.
need_share <- function(v) {
  ok <- is.numeric(v) && length(v) == 1 && isTRUE(v >= 0 && v <= 1)
  if (!ok) stop_detqc("`min_share` must be one number from 0 to 1, a share of full days.")
  invisible(v)
}

# Each deployment's data end as the audit sets it, keyed by deployment. The
# tables handed on read this record, so they end each deployment where the
# audit ended it.
audit_data_end <- function(x) {
  dep <- x$deployments
  end <- deployment_data_end(dep)
  data.frame(receiver_serial = dep$receiver_serial, station_id = dep$station_id,
             deploy_datetime = dep$deploy_datetime,
             data_end = as.POSIXct(as.numeric(end), origin = "1970-01-01", tz = "UTC"),
             stringsAsFactors = FALSE)
}

# The audit's data ends matched to the rows of `dep`; NA where the listening
# table carries none or no deployment of the audit matches. Filters leave
# the deployments as they are, so the rows match in order. A sheet can hold
# two deployments with one serial, station and start and different ends, so
# a deployment out of order is matched only by a key no other row shares.
audited_data_end <- function(lst, dep) {
  rec <- attr(lst, "data_end")
  if (!is.data.frame(rec) || !nrow(rec)) return(rep(NA_real_, nrow(dep)))
  key <- function(d) paste(d$receiver_serial, d$station_id, as.numeric(d$deploy_datetime))
  k_rec <- key(rec)
  k_dep <- key(dep)
  if (identical(k_rec, k_dep)) return(as.numeric(rec$data_end))
  once <- !k_rec %in% k_rec[duplicated(k_rec)]
  as.numeric(rec$data_end[once])[match(k_dep, k_rec[once])]
}

# The log record counts receivers and serial-less stations apart.
receiver_count <- function(days) {
  n_rx <- length(unique(days$rx[!is.na(days$receiver_serial)]))
  n_st <- length(unique(days$rx[is.na(days$receiver_serial)]))
  out <- n_of(n_rx, "receiver")
  if (n_st) out <- paste(out, "and", n_of(n_st, "station"), "with no receiver serial")
  out
}

# Counts of expected reference detections per receiver-day, NA where none
# is expected.
audit_reference_counts <- function(det, days, reference_tags, min_share) {
  none <- list(count = rep(NA_integer_, nrow(days)), n_tags = 0L, n_pairs = 0L)
  if (is.null(reference_tags)) return(none)
  if (!is.data.frame(reference_tags) || !"transmitter" %in% names(reference_tags)) {
    stop_detqc("`reference_tags` must be a data frame with a `transmitter` column.")
  }
  rd <- det[transmitter %in% reference_tags$transmitter]
  warn_absent_reference(reference_tags, det, "audit_listening")
  if (!nrow(rd)) return(none)
  span <- rd[, list(first = min(date), last = max(date)), by = "transmitter"]
  pairs <- audit_expected_pairs(rd, days, span, reference_tags, min_share)
  if (!nrow(pairs)) return(utils::modifyList(none, list(n_tags = nrow(span))))
  per_day <- rd[, list(n = .N), by = c("rx", "date", "transmitter")]
  grid <- days[, list(rx, date, k = .I)][pairs, on = "rx", allow.cartesian = TRUE, nomatch = NULL]
  grid <- grid[date >= first & date <= last]
  grid[, n := per_day$n[match(paste(rx, date, transmitter),
                              paste(per_day$rx, per_day$date, per_day$transmitter))]]
  grid[is.na(n), n := 0L]
  tot <- grid[, list(n = sum(n)), by = "k"]
  count <- rep(NA_integer_, nrow(days))
  count[tot$k] <- as.integer(tot$n)
  list(count = count, n_tags = nrow(span), n_pairs = nrow(pairs))
}

audit_expected_pairs <- function(rd, days, span, reference_tags, min_share) {
  if ("receiver_serial" %in% names(reference_tags)) {
    p <- data.table::data.table(transmitter = reference_tags$transmitter,
                                rx = split_receiver(reference_tags$receiver_serial)$serial)
    return(merge(p, span, by = "transmitter"))
  }
  full <- days[data_share >= 1 & !after_download, list(rx, date)]
  cand <- full[span, on = list(date >= first, date <= last), allow.cartesian = TRUE,
               nomatch = NULL, list(rx, date = x.date, transmitter)]
  heard <- unique(rd[, list(rx, date, transmitter)])
  cand[, hit := paste(rx, date, transmitter) %in% paste(heard$rx, heard$date, heard$transmitter)]
  share <- cand[, list(share = mean(hit)), by = c("rx", "transmitter")]
  merge(share[share >= min_share, list(transmitter, rx)], span, by = "transmitter")
}

# The statuses that make a receiver-day not listening. The last two come
# only from receiver logs.
not_listening <- c("not_downloaded", "reference_silent", "silent_run", "deaf", "off")
log_statuses <- c("logging_silent", "deaf", "off")

# A reference tag missed on a day the receiver logged other detections does
# not make the receiver deaf: on such days a receiver can log hundreds of
# animal detections.
audit_day_status <- function(days) {
  no_ref <- !is.na(days$reference_detections) & days$reference_detections == 0
  data.table::fcase(
    days$after_download, "not_downloaded",
    days$data_share < 1, "partial",
    no_ref & days$detections == 0, "reference_silent",
    no_ref, "reference_missed",
    days$detections == 0, "silent",
    default = "listening"
  )
}

# Silent and partial days of a receiver at a station that logged nothing on
# any of its days (a day or more in the water), and, with max_silent_days,
# silent days in a run of consecutive dates at least that long, become
# "silent_run". check_detset() flags the first case as receiver_silent; the
# audit makes the same days not listening, so detfield does not read them as
# evidence that no animal was near.
silent_runs <- function(days, max_silent_days) {
  st <- days$status
  quiet <- st %in% c("silent", "partial", "logging_silent")
  # A day the receiver's log counted detections is a day it heard.
  heard <- if ("log_heard" %in% names(days)) days$log_heard else rep(FALSE, nrow(days))
  g <- days[, list(total = sum(detections) + sum(heard[.I]),
                   water = sum(deployed_share * !after_download)),
            by = c("rx", "station_id")]
  dead <- paste(g$rx, g$station_id)[g$total == 0 & g$water >= 1]
  st[quiet & paste(days$rx, days$station_id) %in% dead] <- "silent_run"
  if (!is.null(max_silent_days)) {
    s <- st %in% c("silent", "logging_silent")
    brk <- c(TRUE, days$rx[-1] != days$rx[-nrow(days)] |
               diff(as.numeric(days$date)) != 1 | s[-1] != s[-nrow(days)])
    run <- cumsum(brk)
    long <- s & stats::ave(as.numeric(s), run, FUN = length) >= max_silent_days
    st[long] <- "silent_run"
  }
  st
}

# Column built inside data.table calls in this file.
utils::globalVariables(c("data_share", "k", "nx"))
