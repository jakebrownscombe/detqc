# ---- Internal: Fathom VDAT CSV records ----
# A VDAT CSV ("VEMCO DATA LOG", format 2.0.0 and later) holds many record
# types in one file. Each type has a header row named "<TYPE>_DESC", and each
# data row starts with its type. Only DET, DET_SENS and CFG_TRANSMITTER are
# read here; glatos::read_vdat_csv() reads every type.

read_vdat_records <- function(file) {
  first <- readLines(file, n = 1, warn = FALSE, encoding = "UTF-8")
  if (!length(first) || !grepl("VEMCO DATA LOG", first, fixed = TRUE)) {
    stop_detqc(paste("%s is not a Fathom VDAT CSV: its first line is not \"VEMCO DATA LOG\".",
                     "A file with the VUE header is read with format = \"vue\"."), basename(file))
  }
  src <- plain_text(file)
  if (src$path != file) on.exit(unlink(src$path))
  txt <- data.table::fread(src$path, sep = NULL, header = FALSE, skip = 2, col.names = "txt",
                           colClasses = "character", showProgress = FALSE, encoding = "UTF-8")
  type <- sub(",.*$", "", txt$txt)
  cut <- vdat_cut_line(file, txt$txt, type, src$path)
  if (cut) {
    # Row positions, not -nrow(txt): inside a data.table call `txt` is the
    # column of that name.
    txt <- txt[seq_len(length(type) - 1L)]
    type <- type[-length(type)]
  }
  vdat_trailer(file, type)
  det <- vdat_type(txt$txt, type, "DET")
  sens <- vdat_type(txt$txt, type, "DET_SENS")
  both <- vdat_merge_sensors(det, sens)
  list(detections = vdat_offsets(both$table), repeated = both$repeated,
       transmitters = vdat_transmitters(vdat_type(txt$txt, type, "CFG_TRANSMITTER")),
       counts = table(type[!endsWith(type, "_DESC")]), cut_rows = as.integer(cut))
}

# A file cut part-way through its last line, as an interrupted copy or sync
# leaves it: the line has no line end and fewer fields than its record's
# _DESC header, or a record type with no header at all. fill = TRUE would
# pad it into a detection with no receiver and no transmitter, or a time cut
# to "2". The line number counts the file's own lines.
vdat_cut_line <- function(file, lines, type, path = file) {
  if (is.null(last_line_cut(path)) || !length(lines)) return(FALSE)
  k <- length(lines)
  head <- lines[type == paste0(type[k], "_DESC")]
  if (length(head) && n_fields(lines[k]) >= n_fields(head[1])) return(FALSE)
  warn_detqc(paste("%s ends part-way through line %s, a %s record with %s of %s fields and no",
                   "line end, as an interrupted copy or sync leaves a file. The line was",
                   "dropped."),
             basename(file), fmt_n(k + 2L), type[k], fmt_n(n_fields(lines[k])),
             if (length(head)) fmt_n(n_fields(head[1])) else "an unknown number of")
  TRUE
}

# Fathom writes EVENT_OFFLOAD as the file's last record. A file that declares
# the record but holds none ends before its trailer, so its last detections
# may be missing even though every line is whole.
vdat_trailer <- function(file, type) {
  if ("EVENT_OFFLOAD_DESC" %in% type && !"EVENT_OFFLOAD" %in% type) {
    warn_detqc(paste("%s has no EVENT_OFFLOAD record, which Fathom writes at the end of the",
                     "file: the file may be cut short, and detections after the cut are",
                     "missing."), basename(file))
  }
  invisible(TRUE)
}

# One record type as a table of text columns, named from its _DESC row.
vdat_type <- function(lines, type, which) {
  head <- lines[type == paste0(which, "_DESC")]
  body <- lines[type == which]
  if (!length(head)) return(NULL)
  dt <- data.table::fread(text = c(head[1], body, ""), sep = ",", header = TRUE,
                          colClasses = "character", na.strings = "", fill = TRUE,
                          showProgress = FALSE)
  nm <- clean_names(names(dt))
  nm[!nzchar(nm)] <- paste0("column_", which(!nzchar(nm)))
  names(dt) <- make.unique(nm, sep = "_")
  dt[, -1, with = FALSE]
}

# A sensor detection can be written twice, as DET and as DET_SENS; the
# DET_SENS record carries the reading, so the DET copy goes.
vdat_merge_sensors <- function(det, sens) {
  if (is.null(det) && is.null(sens)) {
    stop_detqc("The VDAT file holds no DET or DET_SENS record header.")
  }
  if (is.null(sens) || !nrow(sens)) return(list(table = as_df(det), repeated = 0L))
  if (is.null(det) || !nrow(det)) return(list(table = as_df(sens), repeated = 0L))
  key <- c("device_time_utc", "serial_number", "full_id")
  dup <- paste(det$device_time_utc, det$serial_number, det$full_id) %in%
    paste(sens$device_time_utc, sens$serial_number, sens$full_id)
  out <- data.table::rbindlist(list(det[!dup], sens), use.names = TRUE, fill = TRUE)
  data.table::setorderv(out, key[1])
  list(table = as_df(out), repeated = sum(dup))
}

# "Time" is the corrected clock at the row's "Time Offset (h)". The offset is
# written onto the stamp so the generic parser reads it as UTC.
vdat_offsets <- function(df) {
  if (!all(c("time", "time_offset") %in% names(df))) return(df)
  off <- parse_number(df$time_offset)
  fix <- !is.na(off) & !is.na(df$time)
  if (any(fix)) {
    h <- trunc(abs(off[fix]))
    m <- round((abs(off[fix]) - h) * 60)
    df$time[fix] <- paste0(df$time[fix], ifelse(off[fix] < 0, "-", "+"),
                           sprintf("%02d:%02d", h, m))
  }
  df
}

vdat_transmitters <- function(cfg) {
  if (is.null(cfg) || !nrow(cfg)) return(NULL)
  pick <- function(nm) if (nm %in% names(cfg)) cfg[[nm]] else rep(NA_character_, nrow(cfg))
  data.frame(
    receiver_serial = pick("serial_number"), receiver_model = pick("model"),
    datetime = parse_utc(pick("device_time_utc"), "UTC", "CFG_TRANSMITTER", strict = FALSE),
    transmitter = pick("full_id"), power_level = pick("power_level"),
    min_delay = parse_number(pick("min_delay")), max_delay = parse_number(pick("max_delay")),
    stringsAsFactors = FALSE
  )
}
