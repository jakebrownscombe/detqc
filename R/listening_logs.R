# ---- Receiver logs as evidence of listening ----
# audit_listening(logs = ) reads the receiver's own status records on the
# quiet days: full days in the water, before the download, with no
# detection. The rules are stated in audit_listening()'s documentation; the
# functions here apply them.

# NULL, or the logs with a bare serial key. A table of the user's own needs
# the three columns the rules read.
check_logs <- function(logs) {
  if (is.null(logs)) return(NULL)
  ok <- is.data.frame(logs) && all(c("receiver_serial", "datetime", "record_type") %in%
                                     names(logs))
  if (!ok) {
    stop_detqc(paste("`logs` must be a data frame from read_receiver_logs(), with",
                     "`receiver_serial`, `datetime` and `record_type`."))
  }
  if (!is_utc(logs$datetime)) stop_detqc("`logs$datetime` must be a date-time in UTC.")
  out <- data.table::data.table(
    rx = split_receiver(as.character(logs$receiver_serial))$serial,
    datetime = logs$datetime, record_type = as.character(logs$record_type),
    pings = if ("pings" %in% names(logs)) as.numeric(logs$pings) else NA_real_,
    log_det = if ("log_detections" %in% names(logs)) as.numeric(logs$log_detections) else NA_real_,
    file = if ("source_file" %in% names(logs)) as.character(logs$source_file) else NA_character_
  )
  # A count of detections is read from status records only, so a day with
  # a count is never a day with no status record ("off").
  out[record_type %in% log_event_types, log_det := NA_real_]
  out[!is.na(rx) & !is.na(datetime)]
}

# Detections the receiver's DIAG records counted within each UTC day. A DIAG
# record counts the detections since the receiver's previous DIAG record, so
# a count is given to a day only when that interval lies inside the day; the
# first record of a file, and a record after a midnight or a gap that spans
# two dates, are left out, as their detections may belong to another day.
log_heard_days <- function(logs) {
  d <- logs[!is.na(log_det)]
  data.table::setorderv(d, c("rx", "file", "datetime"))
  d[, prev := data.table::shift(datetime), by = c("rx", "file")]
  d <- d[!is.na(prev) & as.Date(prev) == as.Date(datetime)]
  d[, list(log_det = sum(log_det)), by = list(rx, date = as.Date(datetime))]
}

# Adds `log_records` and `pings` to `days`, then gives the quiet days inside
# a receiver's log their status from it: "off" with no status record,
# "deaf" in place of "reference_silent", "logging_silent" in place of
# "silent". Returns the counts the log record reports, or NULL without logs.
log_evidence <- function(logs, days) {
  if (is.null(logs)) return(NULL)
  per_day <- logs[, list(n = sum(!record_type %in% log_event_types),
                         pings = if (all(is.na(pings))) NA_real_ else sum(pings, na.rm = TRUE)),
                  by = list(rx, date = as.Date(datetime))]
  span <- logs[, list(first = as.Date(min(datetime)), last = as.Date(max(datetime))),
               by = c("rx", "file")]
  inside <- days_in_spans(days, span)
  k <- match(paste(days$rx, days$date), paste(per_day$rx, per_day$date))
  days[, log_records := data.table::fifelse(inside, data.table::fcoalesce(per_day$n[k], 0L),
                                            NA_integer_)]
  days[, pings := per_day$pings[k]]
  quiet <- days$status %in% c("silent", "reference_silent") & days$deployed_share >= 1 &
    !days$after_download & days$detections == 0
  st <- days$status
  st[quiet & inside & days$log_records == 0] <- "off"
  st[quiet & inside & days$log_records > 0 & st == "reference_silent"] <- "deaf"
  st[quiet & inside & days$log_records > 0 & st == "silent"] <- "logging_silent"
  # The receiver's own count of detections, of any transmitter, shows it
  # heard: the day listens. A missed reference tag then reads as on a day
  # with detections in the set.
  hd <- log_heard_days(logs)
  ld <- hd$log_det[match(paste(days$rx, days$date), paste(hd$rx, hd$date))]
  heard <- quiet & inside & st != "off" & !is.na(ld) & ld > 0
  st[heard & st %in% c("silent", "logging_silent")] <- "listening"
  st[heard & st %in% c("reference_silent", "deaf")] <- "reference_missed"
  days[, `:=`(status = st, log_heard = heard)]
  list(receivers = length(unique(logs$rx)), with_log = length(unique(days$rx[inside])),
       of = length(unique(days$rx)), inside = sum(inside),
       heard_no_log = sum(inside & days$detections > 0 & days$log_records == 0),
       log_heard = sum(heard), not_deployed = length(setdiff(unique(logs$rx), days$rx)))
}

# TRUE for each receiver-day whose date lies inside a span of its receiver.
days_in_spans <- function(days, span) {
  q <- data.table::data.table(rx = days$rx, date = days$date, .q = seq_len(nrow(days)))
  hit <- span[q, on = list(rx, first <= date, last >= date), list(.q = .q), nomatch = NULL,
              allow.cartesian = TRUE]
  seq_len(nrow(days)) %in% hit$.q
}

log_note <- function(ev) {
  if (is.null(ev)) return("")
  sprintf(paste("; receiver logs of %s cover %s of %s of the %s audited; logged",
                "receivers with no deployment in the set: %s; receiver-days inside a log",
                "with detections and no status record: %s; quiet receiver-days on which",
                "the log counted detections, kept as listening: %s"),
          n_of(ev$receivers, "receiver"), n_of(ev$inside, "receiver-day"), fmt_n(ev$with_log),
          n_of(ev$of, "receiver"), fmt_n(ev$not_deployed), fmt_n(ev$heard_no_log),
          fmt_n(ev$log_heard))
}

utils::globalVariables(c("log_records", "pings", "base", "record_type", ".q", "file", "rx",
                         "datetime", "log_det", "prev", "log_heard"))
