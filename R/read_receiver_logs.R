# ---- Read receiver logs: the receiver's own status records ----

#' Read the status records of receiver logs
#'
#' Reads the records a receiver writes about itself, as opposed to its
#' detections: hourly temperature, noise, tilt and pulse counts, battery and
#' memory, and events such as a reboot, a pause in recording, a fault, the
#' initialisation and the download. [audit_listening()] reads them, through
#' its `logs` argument, as evidence that a receiver was running on a day it
#' heard nothing.
#'
#' **Format.** `"vdat"` reads a Fathom VDAT CSV (`VEMCO DATA LOG`), the CSV
#' that Fathom Connect or VDAT writes from a receiver's VRL or VDAT file; the
#' same file [read_detections()] reads for detections. The VRL or VDAT file
#' itself is binary, and a zip archive holds other files: each stops with a
#' message that says to export it with Fathom Connect, or to unzip it. Formats 1.0.0 and
#' 2.0.0 are read. The records kept are `DIAG` and `DIAG_NEXTRAK` (ambient
#' temperature, noise, tilt, depth, and the pulses and detections counted
#' since the last record), `TEMP`, `ATTITUDE`, `BATTERY`, `HEALTH_VR2TX` and
#' `HEALTH_NEXTRAK`, which are the status records, and `EVENT`, `EVENT_FAULT`,
#' `EVENT_INIT` and `EVENT_OFFLOAD`. Detections and configuration records
#' are skipped. A VR2Tx writes its status records hourly; a NexTrak writes
#' `DIAG_NEXTRAK` every 10 minutes and gives noise in dB where a VR2Tx gives
#' mV. A file
#' with no status record, as the exports of a VR2Tx-RLD file are, gives no
#' rows, and a message counts such files.
#'
#' **Receivers and exports read.** Checked against field exports, with the
#' records read matching the records each file holds: VR2Tx-69 receivers,
#' format 2.0.0, as exported by VDAT 13.9.0, 14.10.1 and 15.1.1; and
#' NexTrak-R1 receivers, format 2.0.0, as exported by VDAT 13.2.2, 13.9.0
#' and 15.1.1 and by Fathom Connect 4.0 and 4.1. Format 1.0.0 is read from
#' its layout and is not tested on a field export. Other models
#' and versions may read; they have not been checked. The yearly
#' receiver-event CSV files that VUE exports are not read.
#'
#' **What a file may lack.** A NexTrak export of Fathom Connect 4.0 can lack
#' the `DIAG_NEXTRAK` record, and with it the noise, pings and detection
#' counts. A file
#' with status records and no `DIAG` or `DIAG_NEXTRAK` record is named in a
#' message; its `noise`, `pings` and `log_detections` are `NA`, and
#' [audit_listening()] reads its other status records. `DATA_ERROR` records
#' carry no time or serial, so they are not rows of the table; a message
#' counts them, and `attr(, "source")` gives the count per file.
#'
#' **Time.** `datetime` is the receiver's corrected `Time` at the row's
#' `Time Offset (h)`, as [read_detections()] reads it, or the uncorrected
#' `Device Time (UTC)` where `Time` is blank. Both are written in UTC.
#'
#' **Damaged files.** A file of 0 bytes stops with its name. A last line cut
#' part-way, with no line end, is dropped with a warning, and a file that
#' declares an `EVENT_OFFLOAD` record but holds none is named in a warning,
#' as in [read_detections()]. A file that is not a VDAT CSV, such as a VUE
#' detection export, stops with its name and what to use instead.
#'
#' @param file Path to a VDAT CSV file, or several paths. Several files are
#'   read one by one and stacked, with their name in `source_file`.
#' @param format `"vdat"`, the one format read so far; VUE receiver-event
#'   CSV files are not read yet.
#' @return A data frame, one row per record:
#'   `receiver_serial` (the bare serial, as deployment sheets write it),
#'   `receiver_model`, `datetime` (UTC), `record_type` (the VDAT record type),
#'   `temperature_c`, `noise`, `noise_unit` (`"mV"` or `"dB"`), `tilt_deg`,
#'   `depth_m`, `pings` and `log_detections` (pulses and detections counted
#'   since the receiver's previous `DIAG` record), `battery_v`, `battery_pct`,
#'   `memory_pct`, `event` (an event's type or a fault's importance),
#'   `event_detail` (an event's subtype, description and values, which is
#'   where a NexTrak writes its text, or a fault's description and code) and
#'   `source_file`. A field the record does not carry is
#'   `NA`. The attribute `source` holds one record per file: the file, the
#'   format version, the rows kept, the `DIAG` records, the `DATA_ERROR`
#'   records and the records found by type.
#' @seealso [audit_listening()], [read_detections()]
#' @export
#' @examples
#' # The synthetic logs of the example study, one file per receiver
#' log_files <- list.files(system.file("extdata", "receiver_logs", package = "detqc"),
#'                         full.names = TRUE)
#' logs <- read_receiver_logs(log_files)
#' table(logs$record_type)
#' head(logs[logs$record_type == "DIAG", c("receiver_serial", "datetime", "noise",
#'                                         "noise_unit", "pings")], 3)
#' # A synthetic NexTrak log: DIAG_NEXTRAK every 10 minutes, noise in dB
#' nextrak <- read_receiver_logs(system.file("extdata", "nextrak_receiver_log.csv",
#'                                           package = "detqc"))
#' table(nextrak$record_type)
read_receiver_logs <- function(file, format = "vdat") {
  format <- check_format_arg(format, "vdat")
  if (!is.character(file) || !length(file)) stop_detqc("`file` must be one or more paths.")
  parts <- lapply(file, function(f) {
    if (length(file) == 1) return(read_one_log(f))
    tryCatch(read_one_log(f), error = function(e) {
      stop_detqc("Reading %s: %s", basename(f), conditionMessage(e))
    })
  })
  srcs <- lapply(parts, attr, "source")
  out <- as_df(data.table::rbindlist(c(list(empty_logs()), parts), use.names = TRUE))
  empty <- vapply(srcs, function(s) s$rows == 0L, logical(1))
  if (any(empty)) {
    message(sprintf(paste("Files with no status record (DIAG, TEMP, ATTITUDE, BATTERY or",
                          "HEALTH): %s of %s, such as %s. A VR2Tx-RLD export holds none."),
                    fmt_n(sum(empty)), fmt_n(length(file)), basename(file[empty][1])))
  }
  log_file_notes(srcs, file, empty)
  attr(out, "source") <- srcs
  out
}

# Says what a file lacks or holds that the table does not show: status
# records but no DIAG record, so no noise, pulse or detection counts (a
# NexTrak export of Fathom Connect 4.0 has none), and DATA_ERROR records,
# which carry no time or serial and are not kept as rows.
log_file_notes <- function(srcs, file, empty) {
  no_diag <- !empty & vapply(srcs, function(s) s$diag == 0L, logical(1))
  if (any(no_diag)) {
    message(sprintf(paste("Files with status records but no DIAG or DIAG_NEXTRAK record: %s",
                          "of %s, such as %s. Their noise, pings and log_detections are NA;",
                          "audit_listening() reads their other status records."),
                    fmt_n(sum(no_diag)), fmt_n(length(file)), basename(file[no_diag][1])))
  }
  err <- vapply(srcs, function(s) s$data_errors, integer(1))
  if (any(err > 0)) {
    message(sprintf(paste("DATA_ERROR records, which name an error, a memory page and an",
                          "offset: %s in %s of %s, such as %s. They carry no time or serial",
                          "and are not kept as rows; attr(, \"source\") counts them per file."),
                    fmt_n(sum(err)), n_of(sum(err > 0), "file"), fmt_n(length(file)),
                    basename(file[err > 0][1])))
  }
  invisible(NULL)
}

# The status records: written while the receiver runs, so a day with none,
# inside the span of a receiver's log, is a day it was not running.
log_status_types <- c("DIAG", "DIAG_NEXTRAK", "TEMP", "ATTITUDE", "BATTERY", "HEALTH_VR2TX",
                      "HEALTH_NEXTRAK")
log_event_types <- c("EVENT", "EVENT_FAULT", "EVENT_INIT", "EVENT_OFFLOAD")

log_columns <- c("receiver_serial", "receiver_model", "datetime", "record_type", "temperature_c",
                 "noise", "noise_unit", "tilt_deg", "depth_m", "pings", "log_detections",
                 "battery_v", "battery_pct", "memory_pct", "event", "event_detail", "source_file")

empty_logs <- function(n = 0L) {
  num <- rep(NA_real_, n)
  chr <- rep(NA_character_, n)
  data.table::data.table(
    receiver_serial = chr, receiver_model = chr, datetime = empty_utc(n), record_type = chr,
    temperature_c = num, noise = num, noise_unit = chr, tilt_deg = num, depth_m = num,
    pings = num, log_detections = num, battery_v = num, battery_pct = num, memory_pct = num,
    event = chr, event_detail = chr, source_file = chr
  )
}

read_one_log <- function(file) {
  if (!file.exists(file)) stop_detqc("File not found: %s", file)
  stop_empty_file(file)
  stop_binary_file(file)
  first <- readLines(file, n = 1, warn = FALSE, encoding = "UTF-8")
  if (!length(first) || !grepl("VEMCO DATA LOG", first, fixed = TRUE)) {
    stop_detqc(paste("%s is not a Fathom VDAT CSV: its first line is not \"VEMCO DATA LOG\".",
                     "Receiver logs are read from the CSV that Fathom Connect or VDAT writes from",
                     "a VRL or VDAT file; a detection export is read with read_detections()."),
               basename(file))
  }
  src <- plain_text(file)
  if (src$path != file) on.exit(unlink(src$path))
  txt <- data.table::fread(src$path, sep = NULL, header = FALSE, skip = 2, col.names = "txt",
                           colClasses = "character", showProgress = FALSE, encoding = "UTF-8")
  lines <- txt$txt
  type <- sub(",.*$", "", lines)
  if (vdat_cut_line(file, lines, type, src$path)) {
    lines <- lines[-length(lines)]
    type <- type[-length(type)]
  }
  vdat_trailer(file, type)
  keep <- intersect(c(log_status_types, log_event_types), type)
  parts <- lapply(keep, function(k) log_record_table(lines, type, k))
  out <- data.table::rbindlist(c(list(empty_logs()), parts), use.names = TRUE)
  out[, source_file := basename(file)]
  data.table::setorderv(out, c("receiver_serial", "datetime"))
  counts <- table(type[!endsWith(type, "_DESC")])
  attr(out, "source") <- list(
    file = file, format = "vdat", format_version = vdat_version(first),
    rows = sum(out$record_type %in% log_status_types),
    events = sum(!out$record_type %in% log_status_types),
    diag = sum(out$record_type %in% c("DIAG", "DIAG_NEXTRAK")),
    data_errors = sum(type == "DATA_ERROR"),
    records = stats::setNames(as.integer(counts), names(counts))
  )
  out
}

vdat_version <- function(first) {
  v <- strsplit(first, ",", fixed = TRUE)[[1]]
  if (length(v) >= 2) trimws(v[2]) else NA_character_
}

# One record type as rows of the log table. The header row of the type
# names its fields; the noise unit is read from that header, "(mV)" or "(dB)".
log_record_table <- function(lines, type, which) {
  tab <- vdat_type(lines, type, which)
  if (is.null(tab) || !nrow(tab)) return(NULL)
  head <- lines[type == paste0(which, "_DESC")][1]
  tab <- vdat_offsets(as_df(tab))
  pick <- function(...) {
    for (nm in c(...)) if (nm %in% names(tab)) return(tab[[nm]])
    rep(NA_character_, nrow(tab))
  }
  num <- function(...) parse_number(pick(...))
  stamp <- pick("time")
  stamp[is.na(stamp) | !nzchar(stamp)] <- pick("device_time_utc")[is.na(stamp) | !nzchar(stamp)]
  out <- empty_logs(nrow(tab))
  out[, `:=`(
    receiver_serial = split_receiver(pick("serial_number"))$serial,
    receiver_model = pick("model"),
    datetime = parse_utc(stamp, "UTC", paste(which, "time"), strict = FALSE),
    record_type = which,
    temperature_c = num("ambient_temperature", "ambient"),
    noise = num("noise_mean", "noise"),
    tilt_deg = num("tilt"), depth_m = num("depth"),
    pings = num("ppm_pings", "pings"), log_detections = num("ppm_detections", "detections"),
    battery_v = num("battery_voltage"), battery_pct = num("battery_remaining"),
    memory_pct = num("memory_remaining")
  )]
  if (which %in% c("DIAG", "DIAG_NEXTRAK")) {
    out[, noise_unit := if (grepl("Noise[^,]*\\(dB\\)", head)) "dB" else "mV"]
  }
  log_events(out, which, pick)
}

# What an event says: its type and detail, or a fault's importance and
# description. Format 1.0.0 names the fields Event Type and Event Details.
log_events <- function(out, which, pick) {
  if (which == "EVENT") {
    detail <- paste_present(pick("subtype"), pick("description"), pick("values"),
                            pick("event_details"))
    out[, `:=`(event = pick("type", "event_type"), event_detail = detail)]
  } else if (which == "EVENT_FAULT") {
    out[, `:=`(event = pick("fault_importance"),
               event_detail = paste_present(pick("fault_description"), pick("fault_code")))]
  }
  out
}

paste_present <- function(...) {
  parts <- list(...)
  out <- apply(do.call(cbind, parts), 1, function(r) {
    r <- r[!is.na(r) & nzchar(trimws(r))]
    if (length(r)) paste(r, collapse = "; ") else NA_character_
  })
  as.character(out)
}

utils::globalVariables(c("source_file", "noise_unit"))
