# ---- Read detections from network and manufacturer exports ----

#' Read detections from an OTN, GLATOS, InnovaSea or detqc file
#'
#' Reads one or more detection files into a data frame with the column names
#' of the positionR table contract. The column map for each format is a CSV
#' file in the package (see [column_map()]), and `read_detections()` takes its names,
#' types and time zones from it. Columns are found by name, so their order in
#' the file does not matter. Where a map lists two source columns for one
#' target, the first that holds a value is used, row by row.
#'
#' **Formats.** `"auto"` reads the header and picks one of:
#' * `"otn"`: an OTN detection extract, CSV or parquet, in the column names
#'   OTN used from June 2025 or before. OTN publishes six kinds of extract.
#'   Three hold detections of animals or tags and are read: *matched to
#'   animals* (`catalogNumber` names the animal), *mapped to other trackers*
#'   ("qualified") and *unqualified*. The sentinel extracts are read the same
#'   way. In every extract but the matched one `catalogNumber` names a
#'   detection or a tag, so `animal_id` is `NA`: these extracts are read
#'   with the rows of `column_map("detections", "otn_qualified")`, and a
#'   full map that reads `animal_id` from `catalogNumber` stops on them. The
#'   extract *matched to external partners* holds counts of possible
#'   detections, and stops `read_detections()`. A matched extract also holds
#'   one release record per animal (receiver `"release"`); these are not
#'   detections, so they are set aside in the attribute `releases` and
#'   counted.
#' * `"glatos"`: a GLATOS detection export.
#' * `"vue"`: a VUE or Fathom detection export with the VUE header.
#' * `"vdat"`: a Fathom VDAT CSV (`VEMCO DATA LOG`), from the receiver's VRL
#'   or VDAT file. `DET` and `DET_SENS` records are read; a `DET` record that
#'   repeats a `DET_SENS` record is dropped and counted. The receiver's own
#'   transmitter settings (`CFG_TRANSMITTER`: code, power level, delays) are in
#'   the attribute `receiver_transmitters`.
#' * `"detqc"`: a table with detqc's own column names (`datetime`,
#'   `receiver_serial`, `transmitter` and the others below), such as a
#'   detset's `detections` written with `write.csv()`. A blank-headed first
#'   column of unique whole numbers, the row names `write.csv()` writes by
#'   default, is dropped.
#'
#' `"auto"` picks the format whose map finds the most of `datetime`,
#' `transmitter` and `receiver_serial` in the header, and needs two. When a
#' file read with no `col_map` in the guessed format then stops, the error
#' first names that format and the columns the guess rests on. With a full map as `col_map`,
#' a file that no format fits is read by the map alone, and the attribute
#' `source` gives its format as `"col_map"`.
#'
#' **Time.** Every datetime is returned in UTC. OTN, GLATOS and VUE exports are
#' written in UTC. A header that states an offset, such as
#' `"Date and Time (UTC-04:00)"`, and a stamp that carries one, such as
#' `"2024-05-01T10:00:00-04:00"`, are read at that offset. In a VDAT file the
#' `Time` column is the receiver's corrected time at the `Time Offset (h)` of
#' its row; where `Time` is blank the uncorrected `Device Time (UTC)` is used.
#' `tz` reads stamps that carry no zone in another zone, for a file exported
#' in local time.
#'
#' **Sensors.** Readings with no unit, or the unit `"ADC"`, are raw counts.
#' They go to `sensor_raw` and leave `sensor_value` and `sensor_unit` as `NA`
#' until [calibrate_sensors()] converts them. Readings with a physical unit go
#' to `sensor_value`, with the unit in `sensor_unit` ("m", "C" or "m/s2"). A
#' unit in brackets, such as `"[m]"`, reads as the unit inside them. In
#' an OTN extract a sensor detection's code is `sensorName`, so `transmitter`
#' takes it, and `tag_name` keeps the pinger code OTN files it under.
#'
#' **Receivers.** A receiver written as model and serial, such as
#' `"VR2Tx-480013"`, is split: `receiver_serial` holds the serial,
#' `"480013"`, as deployment sheets write it, and `receiver_model` holds the
#' model. Only a known receiver family (VR, HR, TBR and others) followed by
#' three or more digits is split, so OTN collector numbers such as
#' `"HFX068-00348-1"` stay whole.
#'
#' Columns the map does not name are kept after the contract columns, with
#' their names in lower case and underscores. No row is removed, except the
#' OTN release records and repeated VDAT records named above.
#'
#' **Raw receiver files.** A receiver's own VRL or VDAT file is binary and
#' a zip archive holds other files, so neither is read: each stops with a
#' message that says to export the receiver file to a VDAT CSV with Fathom
#' Connect, or to unzip the archive. OTN publishes some extracts zipped.
#'
#' **Damaged files.** A file of 0 bytes stops `read_detections()` with its name. A
#' file whose last line stops part-way through a row, with no line end, as an
#' interrupted copy or sync leaves it, has that row dropped with a warning.
#' A last row cut short that still ends in a line end has its first fields
#' and NA in the rest. Rows at the end of a file with no `transmitter`,
#' `receiver_serial` or `station_id` are dropped with one warning that
#' counts them, and `cut_rows` in the `source` record counts them too.
#' A VDAT file that declares an `EVENT_OFFLOAD` record but holds none is named
#' in a warning, because its end may be missing.
#'
#' **Compressed files.** A gzip, bzip2 or xz file is read through R's own
#' decoder, so `R.utils` is not needed, and a gzip file of several members
#' reads as one text. Its decompressed text is tested for a cut row in the
#' same way, and its stream for its end record. A stream that stops before
#' its end gives a warning that names the file and counts the rows read; a
#' part-row at the cut is dropped. Padding after a gzip stream, zero bytes
#' as a tape or tar block leaves or a run of 4 or more 0xFF bytes, is not
#' read as text: a whole stream so padded reads with no warning, and one cut
#' in its text is read to the cut, with the warning. A whole stream followed
#' by 1 to 3 bytes of any value also reads with no warning. 4 or more bytes
#' that are not padding give the warning although every row was read, and
#' stop the reader with an error when they begin as a gzip member does
#' (`1f 8b`). bzip2 decodes whole blocks of up to 900 kB of text, so a small
#' bzip2 file cut part-way holds no row, and stops with an error that names
#' the file.
#'
#' A gzip stream ends in an 8-byte record: a check value, then the length
#' of the text. A file cut inside its end record stops with an error that
#' names the file; a file that lacks the whole record stops, or reads every
#' row and warns, by how the stream was written. Bytes after a cut inside the
#' check value change nothing. A file cut inside the length stops too when
#' fewer bytes follow the cut than it lacks. When at least as many follow,
#' it reads every row. It gives no warning when those bytes are 4 or more
#' bytes of padding, and otherwise it can warn that rows are missing
#' although none are.
#'
#' @param file Path to a detection file, or several paths. Several files are
#'   read one by one and stacked, with their name in the column
#'   `source_file`; columns one file lacks are `NA` in its rows.
#' @param format One of `"auto"`, `"otn"`, `"glatos"`, `"vue"`, `"vdat"` or
#'   `"detqc"`. The default, the full vector, reads as `"auto"`.
#' @param col_map Optional column map. A data frame, or the path to a CSV
#'   file, laid out as [column_map()] describes, replaces the packaged map.
#'   Pairs written as `c(target = "source")` are merged into the format's
#'   map: each replaces the rows of its target, and the other targets are
#'   read as the format reads them. A pair whose column is not in the file
#'   stops; [column_map()] gives the order in which a pair names the columns
#'   the format joins, as GLATOS joins two for `transmitter`.
#' @param tz Optional time zone, from [OlsonNames()], for stamps that carry no
#'   zone of their own. `NULL` uses the zone in the column map (UTC for every
#'   packaged format).
#' @param cut Optional list that keeps part of each file as it is read, before
#'   the files are stacked, so memory holds one whole file at a time and not
#'   the whole set: `from` and `to`, date-times in UTC (POSIXct, or text such
#'   as `"2024-06-03"`), keep `from <= datetime < to`; `transmitters`, a
#'   character vector, keeps those codes. On a study of several gigabytes of
#'   VDAT files, a cut to one month and one species bounds the memory by the
#'   largest file instead of every row.
#' @return A data frame with `datetime`, `receiver_serial`, `transmitter`,
#'   `animal_id`, `station_id`, `sensor_raw`, `sensor_value`, `sensor_unit`,
#'   the other mapped columns and the unmapped ones. The attribute `source` is
#'   a list with one record per file, also for one file: the file, format,
#'   OTN extract kind, time zone, rows read and kept, rows dropped as cut
#'   (`cut_rows`) or outside `cut` (`rows_outside_cut`), and the map rows
#'   used.
#' @seealso [read_deployments()], [read_animals()], [detset()]
#' @export
#' @examples
#' vue_file <- system.file("extdata", "stoney_vue.csv", package = "detqc")
#' vue_detections <- read_detections(vue_file)
#' head(vue_detections)
#' attr(vue_detections, "source")[[1]]$format
#'
#' # A Fathom VDAT CSV
#' vdat_file <- system.file("extdata", "stoney_vdat.csv", package = "detqc")
#' vdat_detections <- read_detections(vdat_file, "vdat")
#' table(vdat_detections$transmitter)
#' attr(vdat_detections, "receiver_transmitters")
read_detections <- function(file, format = c("auto", "otn", "glatos", "vue", "vdat", "detqc"),
                            col_map = NULL, tz = NULL, cut = NULL) {
  format <- check_format_arg(format, c("auto", "otn", "glatos", "vue", "vdat", "detqc"))
  if (!is.null(tz) && (!is.character(tz) || length(tz) != 1 || !tz %in% OlsonNames())) {
    stop_detqc("`tz` must be NULL or one time zone name from OlsonNames().")
  }
  if (!is.character(file) || !length(file)) stop_detqc("`file` must be one or more paths.")
  cut <- check_cut(cut)
  if (length(file) > 1) {
    df <- read_many_detections(file, format, col_map, tz, cut)
  } else {
    df <- read_one_detections(file, format, col_map, tz, cut)
    # One record per file, in a list, for one file as for several.
    attr(df, "source") <- list(attr(df, "source"))
  }
  report_cut(attr(df, "source"), cut)
  warn_no_animal_id(df, attr(df, "source"))
  df
}

read_one_detections <- function(file, format, col_map, tz, cut = NULL) {
  if (!file.exists(file)) stop_detqc("File not found: %s", file)
  # No format of detections is a workbook. An .xlsx or .xlsm file is a zip
  # archive, so the workbook test runs before stop_binary_file(), which
  # would ask to unzip it.
  stop_workbook(file)
  stop_empty_file(file)
  stop_binary_file(file)
  guess <- NULL
  if (format == "auto") {
    format <- detect_format(file, own_map = !is.null(col_map) && !is_map_pairs(col_map))
    # The guess is quoted only when no `col_map` was given: with one, the
    # user has named the columns.
    if (is.null(col_map)) guess <- attr(format, "why")
    format <- as.vector(format)
  }
  if (format == "vdat") {
    vd <- read_vdat_records(file)
    raw <- vd$detections
  } else {
    raw <- read_raw_table(file)
    vd <- NULL
  }
  check_source_tz(raw)
  kind <- otn_kind(raw, file, format)
  matched <- identical(kind, "matched")
  rel <- if (matched) split_otn_releases(raw) else list(raw = raw, releases = NULL)
  map_format <- if (format == "otn" && !matched) "otn_qualified" else format
  # A full map read on a file no format fits takes its rows alone; the
  # detqc rows, which name the contract columns, stand in as the format's.
  if (format == "col_map") map_format <- "detqc"
  map <- get_column_map("detections", map_format, col_map)
  stop_catalogue_animal(map, kind, file)
  if (!is.null(tz)) map$tz[map$type %in% c("datetime", "date")] <- tz
  trimmed <- drop_unfilled_tail(raw, rel, map, file, vd)
  raw <- trimmed$raw
  rel <- trimmed$rel
  out <- tryCatch(apply_column_map(rel$raw, map, "detections"),
                  detqc_error = function(e) stop_guessed(e, guess))
  df <- tidy_detections(out$data, format)
  n_split <- attr(df, "serials_split")
  inside <- cut_keep(df, cut)
  df <- df[inside, , drop = FALSE]
  rownames(df) <- NULL
  rec <- detection_source(file, format, out, df, raw, vd, rel, map)
  rec$otn_kind <- kind
  rec$serials_split <- n_split
  rec$rows_outside_cut <- sum(!inside)
  attr(df, "serials_split") <- NULL
  attr(df, "source") <- rec
  if (!is.null(rel$releases)) attr(df, "releases") <- rel$releases
  if (!is.null(vd)) attr(df, "receiver_transmitters") <- vd$transmitters
  report_set_aside(rec, file)
  df
}

# In an OTN extract that is not matched to animals, catalogNumber names a
# detection or a tag. A full map or a pair of the user's own that reads
# animal_id from it would name animals that are not there. A packaged map
# copied whole is read as `format =` reads it, with the "otn_qualified"
# rows, which have no animal_id row, so it passes.
stop_catalogue_animal <- function(map, kind, file) {
  pairs <- attr(map, "pairs")
  own <- (isTRUE(attr(map, "user_map")) || !is.null(pairs)) && !is.na(kind) && kind != "matched"
  if (!own || !any(map$target == "animal_id" & map$source == "catalognumber")) {
    return(invisible(TRUE))
  }
  fix <- if (is.null(pairs)) {
    paste("Start from column_map(\"detections\", \"otn_qualified\"), the rows format = \"otn\"",
          "reads this extract with.")
  } else {
    paste("Leave out the pair for animal_id, and give detset() an animals table from",
          "read_animals() to name the animals.")
  }
  stop_detqc(paste("%s is an OTN extract of the kind \"%s\", in which catalogNumber names a",
                   "detection or a tag, and the `col_map` given reads animal_id from it. %s"),
             basename(file), kind, fix)
}

# A last row cut short that still ends in a line end passes the test for a
# cut row: its line end is there, and fread(fill = TRUE) pads the missing
# fields with NA. It keeps its first fields, such as an animal ID and part
# of a time, and loses the transmitter, receiver and station. A detection
# with none of the three is no detection, so the rows at the end of the
# file that hold none of the three the map fills are dropped with one
# warning, and counted with the rows dropped as cut. A file with no row
# that holds one is left to the reader's own errors. A VDAT file has its
# own test for a cut record.
drop_unfilled_tail <- function(raw, rel, map, file, vd) {
  keep <- list(raw = raw, rel = rel)
  if (!is.null(vd) || !nrow(rel$raw)) return(keep)
  keys <- c("transmitter", "receiver_serial", "station_id")
  rows <- map[map$target %in% keys, , drop = FALSE]
  held <- vapply(strsplit(rows$source, ";", fixed = TRUE),
                 function(p) all(p %in% names(rel$raw)), logical(1))
  rows <- rows[held, , drop = FALSE]
  filled <- rep(FALSE, nrow(rel$raw))
  for (r in seq_len(nrow(rows))) {
    parts <- strsplit(rows$source[r], ";", fixed = TRUE)[[1]]
    filled <- filled | !is.na(compose_source(rel$raw, parts, rows$sep[r]))
  }
  n <- nrow(rel$raw)
  if (!any(filled) || filled[n]) return(keep)
  k <- n - max(which(filled))
  named <- paste0("`", intersect(keys, rows$target), "`")
  if (length(named) > 1) {
    named <- paste(paste(named[-length(named)], collapse = ", "), "or", named[length(named)])
  }
  warn_detqc(paste("%s ends in %s with no %s, as a file reads when an interrupted copy or",
                   "sync cuts its last row short. %s dropped."),
             basename(file), n_of(k, "row"), named, verb(k, "The row was", "The rows were"))
  kept <- rel$raw[seq_len(n - k), , drop = FALSE]
  # Subsetting a data frame drops the reader's attributes; the map reads
  # the time offsets stated in the header from one of them.
  for (a in setdiff(names(attributes(rel$raw)), c("names", "row.names", "class"))) {
    attr(kept, a) <- attr(rel$raw, a)
  }
  rel$raw <- kept
  n_before <- attr(raw, "cut_rows")
  attr(raw, "cut_rows") <- (if (is.null(n_before)) 0L else n_before) + as.integer(k)
  list(raw = raw, rel = rel)
}

# The source record of one file: what was read, kept and set aside.
detection_source <- function(file, format, out, df, raw, vd, rel, map) {
  n_file <- nrow(raw) + if (is.null(vd)) 0L else vd$repeated
  n_cut <- if (is.null(vd)) attr(raw, "cut_rows") else vd$cut_rows
  c(source_record(file, format, out, nrow(df), rows_read = n_file),
    list(release_rows = if (is.null(rel$releases)) 0L else nrow(rel$releases),
         repeated_records = if (is.null(vd)) 0L else vd$repeated,
         cut_rows = if (is.null(n_cut)) 0L else as.integer(n_cut),
         animal_id_column = any(map$target == "animal_id" & map$source %in% names(rel$raw))))
}

tidy_detections <- function(df, format) {
  for (col in c("animal_id", "station_id", "sensor_unit")) {
    if (!col %in% names(df)) df[[col]] <- rep(NA_character_, nrow(df))
  }
  df <- split_raw_sensors(df)
  df <- split_receiver_column(df)
  n_split <- attr(df, "serials_split")
  if (format == "vue") df <- drop_zero_positions(df)
  df <- order_columns(df, contract_spec$detections$column)
  check_columns(df, contract_spec$detections, "detections")
  attr(df, "serials_split") <- n_split
  df
}

report_set_aside <- function(rec, file) {
  if (rec$release_rows > 0) {
    message(sprintf(paste("%s: %s OTN release records are not detections; they are in",
                          "attr(, \"releases\")."), basename(file), fmt_n(rec$release_rows)))
  }
  if (rec$repeated_records > 0) {
    message(sprintf("%s: %s DET records repeat a DET_SENS record and were dropped.",
                    basename(file), fmt_n(rec$repeated_records)))
  }
  invisible(NULL)
}

# ---- Internal: the cut applied while reading ----
check_cut <- function(cut) {
  if (is.null(cut)) return(NULL)
  cut_names(cut)
  for (nm in intersect(c("from", "to"), names(cut))) cut[[nm]] <- cut_time(cut[[nm]], nm)
  tx <- cut$transmitters
  ok_tx <- is.null(tx) || (is.character(tx) && length(tx) > 0)
  if (!ok_tx) stop_detqc("`cut$transmitters` must be a character vector of transmitter codes.")
  cut
}

cut_names <- function(cut) {
  named <- is.list(cut) && !is.data.frame(cut) && length(cut) && !is.null(names(cut))
  if (!named) stop_detqc("`cut` must be a named list of `from`, `to` and `transmitters`.")
  bad <- setdiff(names(cut), c("from", "to", "transmitters"))
  if (length(bad)) stop_detqc("`cut` has no element `%s`; use from, to and transmitters.", bad[1])
  invisible(TRUE)
}

cut_time <- function(v, nm) {
  if (is.character(v)) v <- parse_utc(v, "UTC", paste0("cut$", nm))
  if (!inherits(v, "POSIXct") || length(v) != 1 || is.na(v)) {
    stop_detqc("`cut$%s` must be one date-time in UTC.", nm)
  }
  lubridate::with_tz(v, "UTC")
}

cut_keep <- function(df, cut) {
  keep <- rep(TRUE, nrow(df))
  if (is.null(cut)) return(keep)
  t <- df$datetime
  if (!is.null(cut$from)) keep <- keep & !is.na(t) & t >= cut$from
  if (!is.null(cut$to)) keep <- keep & !is.na(t) & t < cut$to
  if (!is.null(cut$transmitters)) keep <- keep & df$transmitter %in% cut$transmitters
  keep
}

report_cut <- function(src, cut) {
  if (is.null(cut)) return(invisible(NULL))
  kept <- sum(vapply(src, function(r) as.numeric(r$rows), numeric(1)))
  out <- sum(vapply(src, function(r) as.numeric(r$rows_outside_cut), numeric(1)))
  message(sprintf("read_detections(): the cut kept %s of %s in %s.",
                  fmt_n(kept), n_of(kept + out, "detection"), n_of(length(src), "file")))
  invisible(NULL)
}

# A format whose map reads animal_id, as a GLATOS export's does, but whose
# file leaves it blank on every row gives a detset with no animal: the
# triage would then run on nothing, with no other warning.
warn_no_animal_id <- function(df, src) {
  # A VUE export's Transmitter Name is blank by habit, so only the network
  # exports, which name animals, are tested.
  has_col <- any(vapply(src, function(r) {
    isTRUE(r$animal_id_column) && r$format %in% c("glatos", "otn")
  }, logical(1)))
  if (nrow(df) && has_col && all(is.na(df$animal_id))) {
    warn_detqc(paste("animal_id is blank on %s, though the %s file has the",
                     "column. Give detset() an animals table, from read_animals(), to name the",
                     "animals; otherwise no step can tell which detections are whose."),
               verb(nrow(df), "the only detection", paste("all", n_of(nrow(df), "detection"))),
               src[[1]]$format)
  }
  invisible(NULL)
}

read_many_detections <- function(files, format, col_map, tz, cut = NULL) {
  parts <- lapply(files, function(f) {
    # One unreadable file of many is named, so it can be found and
    # fixed.
    d <- tryCatch(read_one_detections(f, format, col_map, tz, cut), error = function(e) {
      stop_detqc("Reading %s: %s", basename(f), conditionMessage(e))
    })
    d$source_file <- rep(basename(f), nrow(d))
    d
  })
  srcs <- lapply(parts, attr, "source")
  rel <- lapply(parts, attr, "releases")
  # Each VDAT file carries its receiver's built-in transmitters, so they are
  # gathered from every file read in one call.
  rt <- lapply(parts, attr, "receiver_transmitters")
  rt <- rt[!vapply(rt, is.null, logical(1))]
  df <- as_df(data.table::rbindlist(parts, use.names = TRUE, fill = TRUE))
  df <- order_columns(df, contract_spec$detections$column)
  attr(df, "source") <- srcs
  rel <- rel[!vapply(rel, is.null, logical(1))]
  if (length(rel)) attr(df, "releases") <- as_df(data.table::rbindlist(rel, fill = TRUE))
  if (length(rt)) {
    attr(df, "receiver_transmitters") <- unique(as_df(data.table::rbindlist(rt, fill = TRUE)))
  }
  df
}

# ---- Internal: which format and which OTN extract ----
# A receiver's own VRL or VDAT file is binary, and a zip archive holds other
# files; neither has a header to read. Their first four bytes say which, so
# the message names the step that makes them readable.
file_signature <- function(file) {
  con <- file(file, "rb")
  on.exit(close(con))
  head <- readBin(con, "raw", 4L)
  if (length(head) < 4L) return(NA_character_)
  if (identical(head, as.raw(c(0x50, 0x4b, 0x03, 0x04)))) return("zip")
  txt <- rawToChar(head[head != as.raw(0)])
  if (txt %in% c("VRLF", "VDAT")) return(tolower(txt))
  NA_character_
}

stop_binary_file <- function(file) {
  sig <- file_signature(file)
  if (is.na(sig)) return(invisible(TRUE))
  if (sig == "zip") {
    stop_detqc("%s is a zip archive. Unzip it and read the files it holds.", basename(file))
  }
  stop_detqc(paste("%s is a receiver's own %s file, which is binary. Export it to a VDAT CSV with",
                   "Fathom Connect, then read that CSV: read_detections(format = \"vdat\") for",
                   "the detections, read_receiver_logs() for the status records."),
             basename(file), toupper(if (sig == "vrlf") "vrl" else sig))
}

# A workbook stops with its columns listed, as a file of no format
# read_detections() knows does. An empty workbook, or one readxl cannot
# open, stops with the first sentence alone.
stop_workbook <- function(file) {
  if (!tolower(tools::file_ext(file)) %in% c("xls", "xlsx", "xlsm")) return(invisible(TRUE))
  cols <- if (isTRUE(file.size(file) > 0) && requireNamespace("readxl", quietly = TRUE)) {
    tryCatch(sheet_columns(file, "transmitter"), error = function(e) NULL)
  }
  stop_detqc("%s is a workbook; detections are read from CSV or parquet files.%s",
             basename(file), if (is.null(cols)) "" else paste0(" ", cols, "."))
}

detect_format <- function(file, own_map = FALSE) {
  ext <- tolower(tools::file_ext(file))
  if (ext == "parquet") {
    heads <- parquet_names(file)
  } else {
    first <- readLines(file, n = 1, warn = FALSE, encoding = "UTF-8")
    if (!length(first)) stop_no_text(file, compressed_kind(readBin(file, "raw", 6)))
    if (length(first) && grepl("VEMCO DATA LOG", first, fixed = TRUE)) return("vdat")
    # The header is read as text: fread's guess takes the first data row as
    # the header when the rows hold fewer fields than it, as a VUE export
    # with three-field rows under a twelve-name header does.
    heads <- gsub("^\"|\"$", "", strsplit(sub("^\ufeff", "", first), ",", fixed = TRUE)[[1]])
  }
  pick_format(clean_names(heads), heads, file, own_map)
}

# Each format scores the targets of its column map that the header can fill.
# A file is read in the best scorer, so one stray column from another format,
# such as VUE's "Date and Time (UTC)" in a GLATOS export, does not decide it.
# A tie stops and names both. A file no format fits is read by a full map
# of the user's own, as "col_map", and stops otherwise. The attribute "why"
# keeps the columns behind the guess, for a later refusal to quote.
pick_format <- function(h, heads, file, own_map = FALSE) {
  h <- sub("^(date_and_time_utc)_.*$", "\\1", h)
  maps <- list(vue = "vue", glatos = "glatos", otn = c("otn", "otn_qualified"), detqc = "detqc")
  keys <- c("datetime", "transmitter", "receiver_serial")
  fills <- lapply(maps, function(fm) {
    m <- get_column_map("detections", fm[1], NULL)
    if (length(fm) > 1) m <- rbind(m, get_column_map("detections", fm[2], NULL))
    has <- vapply(strsplit(m$source, ";", fixed = TRUE), function(p) all(p %in% h), logical(1))
    m <- m[has & m$target %in% keys, , drop = FALSE]
    m[!duplicated(m$target), c("target", "source"), drop = FALSE]
  })
  score <- vapply(fills, nrow, numeric(1))
  if ("count" %in% h && "detectionregion" %in% h) score["otn"] <- 3
  best <- names(score)[score == max(score)]
  if (max(score) < 2) {
    if (own_map) return("col_map")
    stop_detqc(paste("The format of %s is not one read_detections() knows: no format's column map",
                     "finds two of `datetime`, `transmitter` and `receiver_serial` in its header.",
                     "%s. Give a full map as `col_map`, or `format` and pairs."),
               basename(file), file_columns("transmitter", clean_names(heads)))
  }
  if (length(best) > 1) {
    stop_detqc("%s fits the %s formats equally well; give `format`.", basename(file),
               paste0("\"", best, "\"", collapse = " and "))
  }
  structure(best, why = list(file = basename(file), format = best, fills = fills[[best]],
                             lacks = setdiff(keys, fills[[best]]$target)))
}

# A refusal of a file whose format "auto" guessed starts with the guess and
# the columns it rests on, since a missing column of the guessed format can
# mean the file is of another kind.
stop_guessed <- function(e, guess) {
  if (is.null(guess)) stop(e)
  f <- guess$fills
  lacks <- if (length(guess$lacks)) {
    sprintf(" and none for %s", paste0("`", guess$lacks, "`", collapse = " or "))
  } else {
    ""
  }
  stop_detqc(paste("%s was read as format = \"%s\", which \"auto\" guessed from its header: it has",
                   "the \"%s\" columns for %s (%s)%s. If it is not a %s file, give `format` and a",
                   "`col_map`. %s"),
             guess$file, guess$format, guess$format, paste0("`", f$target, "`", collapse = " and "),
             paste(f$source, collapse = ", "), lacks, format_names[[guess$format]],
             conditionMessage(e))
}

format_names <- list(vue = "VUE", glatos = "GLATOS", otn = "OTN", detqc = "detqc")

parquet_names <- function(file) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop_detqc("Reading %s needs the arrow package: install.packages(\"arrow\").",
               basename(file))
  }
  names(arrow::open_dataset(file)$schema)
}

# OTN extracts made before the parquet rollout carry a timezone column.
check_source_tz <- function(raw) {
  if (!"timezone" %in% names(raw)) return(invisible(TRUE))
  tzs <- unique(stats::na.omit(toupper(raw$timezone)))
  bad <- setdiff(tzs, c("UTC", "GMT", "Z"))
  if (length(bad)) {
    stop_detqc("The file's timezone column holds %s; read_detections() expects UTC.", bad[1])
  }
  invisible(TRUE)
}

# The kind of OTN extract, from its columns and, where OTN's file name says
# it, from the name. NA for formats other than OTN.
otn_kind <- function(raw, file, format) {
  if (format != "otn") return(NA_character_)
  if (all(c("count", "detectionregion") %in% names(raw))) {
    stop_detqc(paste("%s is an OTN extract matched to external partners: it holds counts of",
                     "possible detections per project, and no detection."), basename(file))
  }
  if (!"basisofrecord" %in% names(raw)) return("matched")
  nm <- tolower(basename(file))
  kinds <- c("sentinel_tag_owner", "sentinel_tag", "unqualified", "qualified")
  hit <- kinds[vapply(kinds, function(k) grepl(k, nm, fixed = TRUE), logical(1))]
  if (length(hit)) hit[1] else "qualified"
}

# A matched extract holds one release record per animal, with "release" as
# the receiver. The record is the animal's release and is kept apart from
# the detections.
split_otn_releases <- function(raw) {
  rcv <- if ("receiver" %in% names(raw)) tolower(trimws(raw$receiver)) else character()
  is_rel <- !is.na(rcv) & rcv == "release"
  if (!any(is_rel)) return(list(raw = raw, releases = NULL))
  r <- raw[is_rel, , drop = FALSE]
  pick <- function(nms) {
    nm <- intersect(nms, names(r))
    if (length(nm)) r[[nm[1]]] else rep(NA_character_, nrow(r))
  }
  releases <- data.frame(
    animal_id = pick("catalognumber"), transmitter = pick("tagname"),
    release_datetime = parse_utc(pick(c("datecollectedutc", "datecollected")), "UTC",
                                 "releases$release_datetime", strict = FALSE),
    release_lat = parse_number(pick(c("decimallatitude", "latitude"))),
    release_lon = parse_number(pick(c("decimallongitude", "longitude"))),
    stringsAsFactors = FALSE
  )
  kept <- raw[!is_rel, , drop = FALSE]
  attr(kept, "header_offsets") <- attr(raw, "header_offsets")
  list(raw = kept, releases = releases)
}
