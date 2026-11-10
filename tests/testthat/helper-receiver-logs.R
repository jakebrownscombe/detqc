# ---- Synthetic receiver logs in the layout of Fathom VDAT CSV exports ----
# Every file written here is synthetic: serials, dates and values are
# invented. The layout follows Fathom VDAT CSV exports ("VEMCO DATA LOG"),
# formats 1.0.0 and 2.0.0, as field exports write them (nothing copied):
# the record header rows
# ("<TYPE>_DESC") are the field names those exports carry, VR2Tx status
# records come hourly, NexTrak DIAG_NEXTRAK records every 10 minutes with
# noise in dB, and a VR2Tx-RLD export holds configuration and offload
# records only. The third field of the first line says the file is
# synthetic. data-raw/receiver_logs_example.R sources this file to write the
# example the package ships.

vdat_log_desc <- function(version = "2.0.0") {
  common <- paste("Device Time (UTC),Time,Time Offset (h),Time Correction (s),Model",
                  "Serial Number", sep = ",")
  d <- c(
    ATTITUDE = "Tilt (deg)",
    BATTERY = paste("Battery Position,Battery Type,Battery Serial Number,Battery Voltage (V)",
                    "Battery Remaining (%)", sep = ","),
    CFG_TRANSMITTER = paste("Transmission Type,Full ID,ID,Power Level,Min Delay (s)",
                            "Max Delay (s)", sep = ","),
    DIAG = paste("Ambient Temperature (deg C),Noise Mean (mV),Tilt (deg),Depth (m),PPM Pings",
                 "PPM Detections", sep = ","),
    DIAG_NEXTRAK = "Ambient Temperature (deg C),Noise (dB),Tilt (deg),Depth (m),Pings,Detections",
    DATA_ERROR = "Type,Error,Page,Offset,Description",
    EVENT = if (version == "1.0.0") {
      "Event Type,Event Details"
    } else {
      "Type,Subtype,Description,Values,,,,,,,Latitude,Longitude,GPS HDOP"
    },
    EVENT_FAULT = "Fault Importance,Fault Code,Fault Description",
    EVENT_INIT = "Prior Device Time (UTC),External Time Zone,Firmware Version",
    EVENT_OFFLOAD = paste("External Time (UTC),External Time Zone,HR Total Accepted Detections",
                          "PPM Total Accepted Detections,Memory Remaining (%)",
                          "Battery Remaining (%),Original File", sep = ","),
    HEALTH_NEXTRAK = "Memory Remaining (%)",
    HEALTH_VR2TX = "Memory Remaining (%),RTC Time",
    TEMP = "Ambient (deg C),Internal (deg C)"
  )
  out <- stats::setNames(paste0(names(d), "_DESC,", common, ",", d), names(d))
  # A DATA_ERROR record has no time or serial fields.
  out[["DATA_ERROR"]] <- paste0("DATA_ERROR_DESC,", d[["DATA_ERROR"]])
  out
}

# Writes one synthetic VDAT CSV of one receiver.
# * `start`, `end`: the receiver logs from `start` to `end` (UTC);
#   EVENT_INIT is written an hour before `start` and EVENT_OFFLOAD an hour
#   after `end`, unless `offload = FALSE`.
# * `off`: a list of c(from, to) periods with no status record, as when the
#   receiver lost power; a REBOOT_COMPLETED event follows each.
# * `pings`: pulses per DIAG record; `noise_mv` the noise.
# * `variant`: "vr2tx" (format 2.0.0), "vr2tx_1" (format 1.0.0),
#   "nextrak" or "rld" (configuration and offload records only).
# * `cut`: drop the line end and half of the last line, as an interrupted
#   copy leaves a file.
# * `records`: the hourly status records a VR2Tx file gets; the shipped
#   example keeps DIAG, TEMP and HEALTH_VR2TX only, to keep the package small.
# * `detections`: times (UTC) of the detections the receiver decoded. Each
#   DIAG record then counts those since the previous DIAG record, as a
#   receiver does. Without it the counts cycle 0 to 3 (0 to 2 on a NexTrak).
#   `records` filters a NexTrak file's status records too, when given.
# * `data_error`: write one DATA_ERROR record, as a NexTrak export can hold.
#   A NexTrak event's text goes in the Values field, where a NexTrak export
#   writes it.
write_vdat_log <- function(path, serial, start, end, variant = "vr2tx", off = list(),
                           pings = 400, noise_mv = 300, offload = TRUE, cut = FALSE,
                           fault = FALSE,
                           records = c("DIAG", "TEMP", "ATTITUDE", "BATTERY", "HEALTH_VR2TX"),
                           detections = NULL, data_error = FALSE) {
  utc <- function(v) as.POSIXct(v, tz = "UTC")
  start <- utc(start)
  end <- utc(end)
  version <- if (variant == "vr2tx_1") "1.0.0" else "2.0.0"
  model <- switch(variant, nextrak = "NexTrak-R1", rld = "VR2Tx-RLD", "VR2Tx-69")
  desc <- vdat_log_desc(version)
  stamp <- function(t) format(t, "%Y-%m-%d %H:%M:%S")
  row <- function(type, t, fields) {
    sprintf("%s,%s,%s.0001,,0.000100,%s,%s,%s", type, stamp(t), stamp(t), model, serial, fields)
  }
  step <- if (variant == "nextrak") 600 else 3600
  t <- seq(start, end, by = step)
  for (p in off) t <- t[t < utc(p[1]) | t > utc(p[2])]
  i <- seq_along(t)
  n_det <- log_counts(t, detections, if (variant == "nextrak") 3 else 4)
  temp <- sprintf("%.1f", 18 + 2 * sin(as.numeric(t) / 86400 * 2 * pi))
  status <- if (variant == "rld") character() else if (variant == "nextrak") c(
    row("DIAG_NEXTRAK", t, sprintf("%s,%.1f,%d,,%d,%d", temp, 62 + i %% 5, 4, pings, n_det)),
    row("TEMP", t, paste0(temp, ",")), row("ATTITUDE", t, "4"),
    row("BATTERY", t, ",,,,97"), row("HEALTH_NEXTRAK", t, "88.0")
  ) else c(
    row("DIAG", t, sprintf("%s,%.1f,%d,,%d,%d", temp, noise_mv + i %% 7, 6, pings, n_det)),
    row("TEMP", t, paste0(temp, ",")), row("ATTITUDE", t, "6"),
    row("BATTERY", t, ",,,3.6,"),
    row("HEALTH_VR2TX", t[format(t, "%H:%M") == "00:00"],
        paste0("93.0,", stamp(t[format(t, "%H:%M") == "00:00"])))
  )
  status <- keep_records(status, variant, records, !missing(records))
  ev <- function(type, text = "") event_fields(type, text, version, variant)
  events <- c(
    row("EVENT_INIT", start - 3600, paste0(stamp(start - 3600), ",UTC,1.2.3")),
    unlist(lapply(off, function(p) {
      row("EVENT", utc(p[2]), ev("REBOOT_COMPLETED", "Power-on reset"))
    })),
    if (fault) row("EVENT_FAULT", start + 7200, "UNKNOWN,,Unknown fault condition"),
    if (offload) row("EVENT_OFFLOAD", end + 3600,
                     paste0(stamp(end + 3600), ",UTC,0,0,90.0,80,synthetic.vrl"))
  )
  body <- c(events, status)
  ord <- order(substr(body, regexpr(",", body) + 1, regexpr(",", body) + 19))
  lines <- c(sprintf("VEMCO DATA LOG,%s,detqc synthetic example; layout follows Fathom VDAT CSV",
                     version),
             paste(c("RECORD TYPE", rep("FIELD", 25)), collapse = ","),
             unname(desc[names(desc) != "DATA_ERROR" | data_error]),
             row("CFG_TRANSMITTER", start - 3600,
                 sprintf("PPM,A69-1601-%s,%s,VERY_HIGH,540,660",
                         substr(serial, nchar(serial) - 4, nchar(serial)),
                         substr(serial, nchar(serial) - 4, nchar(serial)))),
             body[ord],
             if (data_error) "DATA_ERROR,PAGE_READ,CRC,412,0,Page checksum mismatch")
  write_log_lines(lines, path, cut)
}

# A VR2Tx file keeps the status records named in `records`; a NexTrak file
# keeps them only when `records` was given.
keep_records <- function(status, variant, records, given) {
  if (!variant %in% c("vr2tx", "vr2tx_1") && !(variant == "nextrak" && given)) return(status)
  status[sub(",.*$", "", status) %in% records]
}

# With `cut`, the line end and half of the last line go, as an interrupted
# copy leaves a file.
write_log_lines <- function(lines, path, cut) {
  if (!cut) {
    writeLines(lines, path, useBytes = TRUE)
    return(invisible(path))
  }
  last <- lines[length(lines)]
  lines[length(lines)] <- substr(last, 1, nchar(last) %/% 2)
  con <- file(path, "wb")
  writeBin(charToRaw(paste(lines, collapse = "\n")), con)
  close(con)
  invisible(path)
}

# The detections each status record at times `t` counts: those after the
# record before, up to and including this one. Without `detections`, a
# count that cycles from 0 to `cycle` - 1.
log_counts <- function(t, detections, cycle) {
  i <- seq_along(t)
  if (is.null(detections)) return(i %% cycle)
  at <- findInterval(as.numeric(as.POSIXct(detections, tz = "UTC")), c(-Inf, as.numeric(t)),
                     left.open = TRUE)
  tabulate(at, length(t) + 1)[i]
}

# The fields of an EVENT record after the common ones. Format 1.0.0 has Event
# Type and Event Details; 2.0.0 has Type, Subtype, Description, Values and
# nine more, and a NexTrak writes its text in Values.
event_fields <- function(type, text, version, variant) {
  if (version == "1.0.0") return(paste0(type, ",", text))
  if (variant == "nextrak") return(paste0(type, ",,,", text, ",,,,,,,,,"))
  paste0(type, ",,", text, ",,,,,,,,,,")
}
