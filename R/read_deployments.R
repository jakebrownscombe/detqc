# ---- Read receiver deployments ----

#' Read receiver deployments from an OTN, GLATOS or detqc sheet
#'
#' Reads a deployment file into a data frame with the contract's deployment
#' columns. It reads CSV files and the sheet of an Excel workbook whose header
#' names the most mapped columns; the header row is found by its names, so
#' title rows above it are skipped. Mapped: OTN deployment sheets (the
#' instrument deployment short form), OTN deployment CSV files, OTN AAT
#' receiver files, the OTN moorings receivers file (receiver serial in
#' `rcvrserial`), the OTN stations-receivers file (`rcvr_serial`), GLATOS
#' receiver location files and the deployment sheet of a GLATOS project
#' workbook. Excel needs the readxl package.
#'
#' **Format.** `"auto"` reads the file with the map, OTN or GLATOS, that
#' fills more of the contract columns from the file's columns, and OTN on a
#' tie. When the other format's map would fill a contract column that the
#' chosen one leaves empty, such as `deploy_z`, a warning names the column
#' and the format. Where that column is one the table needs, such as
#' `station_id` of a GLATOS receiver file read with `format = "otn"`, the
#' reader stops and names the format `"auto"` picks. A format other than the
#' four stops with the formats listed. A file
#' with detqc's own column names, such as a detset's `deployments` written
#' with `write.csv()`, fills the most contract columns and is read as
#' `"detqc"`. A blank-headed first column of unique whole numbers, the row
#' names `write.csv()` writes by default, is dropped.
#'
#' A GLATOS
#' workbook keeps its recoveries on its `Recovery` sheet, which is joined to
#' the deployments as `glatos::read_glatos_workbook()` joins it, and its
#' times in local time, in `glatos_deploy_date_time` and
#' `glatos_recover_date_time` at the zone in `glatos_timezone`; where the UTC
#' columns are blank, these are converted to UTC. The attribute `source`
#' counts both, in `recoveries_joined` and `local_times`.
#'
#' A sheet that records no receiver serial, as OTN's deployment CSV does,
#' gives `receiver_serial` as `NA`; [detset()] then matches detections to
#' those deployments by `station_id`, and [audit_listening()] keys their
#' days by station. A moorings or stations-receivers file with the serial
#' blank on some rows is read the same way on those rows. An OTN deployment
#' CSV names its station in `STATION_NAME`, or in `STATION_NO` beside
#' `ARRAY`, which is read alone. A workbook's `OTN_ARRAY` and `STATION_NO`
#' are joined. OTN projects differ in which of the two their detections use,
#' so a project named the other way gives `col_map = c(station_id =
#' "station_no")` for a workbook or `col_map = c(station_id =
#' "array;station_no")` for a CSV. A file from which the map fills no
#' `station_id` in any row is read with a warning that names the columns the
#' map looked for.
#'
#' A file with no column the map reads for `recover_datetime`, and no
#' recovery in any row, is read with a warning that names the columns the
#' map looked for, since every deployment then reads as never recovered. A
#' recovery column that is blank in every row gives no warning: its
#' receivers are still deployed.
#'
#' Rows with no deployment time are dropped with a warning that counts them
#' and names their stations; [detset()] logs the count. Rows whose
#' `receiver_model` names an instrument that is not a receiver, such as the
#' `"anchor"` rows of an OTN short form, and that carry no receiver serial,
#' are not deployments: they are set aside with a message and counted in
#' `dropped_not_receiver`. A recovery date
#' written with a status, such as `"failed: 2010-03-07"` or
#' `"caught: 2014-06-22"`, keeps its date, and the text goes to
#' `recovery_status`; a warning counts these rows. A recovery with no date,
#' such as `"failed"`, is read from the next mapped column (`last_download` in
#' OTN sheets) or left `NA`.
#'
#' A station is one fixed place. A warning counts the `station_id` values
#' whose deployments lie more than 1 km apart, as GLATOS station codes that
#' repeat across projects do, and the attribute `source` keeps the count
#' (`stations_over_1km`).
#'
#' **The receiver's own transmitter.** The `TRANSMITTER` column of an OTN
#' instrument deployment short form holds, for a transceiver, the pinger
#' built into the receiver, and otherwise a sentinel tag on the same
#' mooring. It is read as `receiver_transmitter`, in capitals with the ends
#' trimmed. A value that is not a transmitter id (code space and ID, as
#' `"A69-1601-12345"`), such as a bare number, becomes `NA` with a warning
#' that counts such rows, and the attribute `source` keeps the count
#' (`receiver_transmitter_malformed`). [check_detset()] and
#' [audit_listening()] take these ids as the reference tags when you give
#' none. The GLATOS receiver and workbook layouts mapped here have no such
#' column.
#'
#' Times in OTN and GLATOS sheets are UTC. A sheet kept in local time is read
#' with `tz`, and an Excel date-time number is read in the same zone.
#'
#' @inheritParams read_detections
#' @param file Path to a CSV or Excel file.
#' @param format One of `"auto"`, `"otn"`, `"glatos"` or `"detqc"`.
#' @return A data frame with `receiver_serial`, `station_id`,
#'   `deploy_datetime`, `recover_datetime` (UTC; `NA` while deployed),
#'   `deploy_lat`, `deploy_lon`, `deploy_z`, `receiver_transmitter` where the
#'   sheet has it, `recovery_status` and any other columns. The attribute
#'   `source` records the file, the map rows used and the rows dropped for
#'   want of a deployment time.
#' @seealso [column_map()] to read a sheet with other column names.
#' @export
#' @examples
#' receivers_file <- system.file("extdata", "stoney_receivers.csv", package = "detqc")
#' deployments <- read_deployments(receivers_file)
#' attr(deployments, "source")$format
#' head(deployments)
#' # An OTN short form whose TRANSMITTER column holds each receiver's own
#' # transmitter; R020's id has no code space and is read as NA
#' otn_file <- system.file("extdata", "stoney_otn_deployments.csv", package = "detqc")
#' otn_deployments <- read_deployments(otn_file, format = "otn")
#' tail(otn_deployments[, c("receiver_serial", "receiver_transmitter")], 3)
read_deployments <- function(file, format = c("auto", "otn", "glatos", "detqc"),
                             col_map = NULL, tz = NULL) {
  format <- check_format_arg(format, c("auto", "otn", "glatos", "detqc"))
  map <- get_column_map("deployments", if (format == "auto") "otn" else format, col_map)
  if (!is.null(tz)) map$tz[map$type %in% c("datetime", "date")] <- check_tz(tz)
  # A workbook with no sheet the maps know lists the columns nearest to the
  # targets a deployment table needs.
  needed <- contract_spec$deployments$column[contract_spec$deployments$required]
  raw <- read_raw_table(file, header_names = sheet_header_names("deployments", format, map),
                        near = needed)
  prep <- function(r, fm) {
    if (fm == "glatos") return(glatos_local_times(glatos_recoveries(file, r)))
    if (fm == "otn") return(otn_station_no(r))
    r
  }
  # A full map of the user's own replaces the packaged ones, so they are not
  # compared with it. Pairs are merged into the map of the format picked.
  picked <- if (is.null(col_map) || format == "auto") {
    sheet_format(raw, "deployments", format, file, prep, warn = is.null(col_map))
  } else {
    format
  }
  map <- picked_map(map, "deployments", format, picked, col_map)
  if (format == "auto" && !is.null(tz)) map$tz[map$type %in% c("datetime", "date")] <- tz
  format <- picked
  raw <- prep(raw, format)
  out <- apply_column_map(raw, map, "deployments")
  df <- split_receiver_column(out$data)
  attr(df, "serials_split") <- NULL
  df$recovery_status <- recovery_status(raw, map)
  df <- order_columns(df, contract_spec$deployments$column)
  own <- clean_receiver_transmitter(df, file)
  df <- own$df
  not_rx <- not_a_receiver(df)
  if (any(not_rx)) {
    k <- sum(not_rx)
    message(sprintf(paste("%s: %s %s an instrument that is not a receiver, such as \"%s\",",
                          "and no receiver serial; %s not %s and %s set aside."),
                    basename(file), n_of(k, "row"), verb(k, "names", "name"),
                    df$receiver_model[not_rx][1], verb(k, "it is", "they are"),
                    verb(k, "a deployment", "deployments"), verb(k, "was", "were")))
    df <- df[!not_rx, , drop = FALSE]
  }
  no_start <- is.na(df$deploy_datetime)
  if (any(no_start)) {
    k <- sum(no_start)
    st <- utils::head(unique(df$station_id[no_start]), 5)
    warn_detqc("%s %s no deployment time and %s dropped (%s %s).",
               n_of(k, "deployment row"), verb(k, "has", "have"), verb(k, "was", "were"),
               verb(length(st), "station", "stations"), paste(st, collapse = ", "))
  }
  df <- df[!no_start, , drop = FALSE]
  rownames(df) <- NULL
  check_columns(df, contract_spec$deployments, "deployments")
  warn_no_station(df, map, raw, basename(file))
  warn_no_recovery(df, map, raw, basename(file))
  reused <- warn_reused_stations(df, basename(file))
  attr(df, "source") <- c(source_record(file, format, out, nrow(df)),
                          list(dropped_no_deploy_time = sum(no_start),
                               dropped_not_receiver = sum(not_rx),
                               stations_over_1km = reused,
                               receiver_transmitter_malformed = own$malformed,
                               local_times = attr(raw, "local_times"),
                               recoveries_joined = attr(raw, "recoveries_joined")))
  df
}

# `station_id` may be blank row by row, but a table with no station in any
# row gives every listening day an NA station in audit_listening() and
# as_detfield_input(), and detfield joins receivers by station_id. So a
# file from which the map fills no station warns, with the columns the map
# looked for and the file's own.
warn_no_station <- function(df, map, raw, file) {
  if (!nrow(df) || any(!is.na(df$station_id))) return(invisible(FALSE))
  tried <- unique(map$source[map$target == "station_id"])
  warn_detqc(paste("%s: no row has a `station_id`; the map looks for %s. %s. Without it,",
                   "audit_listening() and as_detfield_input() give every listening day an NA",
                   "station. Give the station column as col_map = c(station_id = \"<column>\")."),
             file, if (length(tried)) paste(tried, collapse = ", ") else "no column",
             file_columns("station_id", names(raw), raw))
  invisible(TRUE)
}

# An empty recover_datetime means a receiver still in the water. A file with
# no column the map reads for it, in which no row has one, would read every
# deployment as never recovered, and the listening days audit_listening()
# and as_detfield_input() count would rest on that. A recovery column that is
# present and blank in every row is a set of receivers still deployed, and
# does not warn.
warn_no_recovery <- function(df, map, raw, file) {
  if (!nrow(df) || any(!is.na(df$recover_datetime))) return(invisible(FALSE))
  tried <- unique(map$source[map$target == "recover_datetime"])
  parts <- unlist(strsplit(tried, ";", fixed = TRUE))
  if (any(parts %in% names(raw))) return(invisible(FALSE))
  warn_detqc(paste("%s: no row has a `recover_datetime`, and the file has no column the map looks",
                   "for: %s. %s. Every deployment then reads as never recovered, which sets the",
                   "listening days of audit_listening() and as_detfield_input(). Give the",
                   "recovery column as col_map = c(recover_datetime = \"<column>\")."),
             file, if (length(tried)) paste(tried, collapse = ", ") else "no column",
             file_columns("recover_datetime", names(raw), raw))
  invisible(TRUE)
}

# The OTN deployment CSV names its array in ARRAY and its station in
# STATION_NO, and the detections of a project such as TEQ name the station
# by STATION_NO alone. The map reads `array_station_no`, a copy made only when the file has
# ARRAY, so a GLATOS receiver file, whose STATION_NO sits beside
# GLATOS_ARRAY, gives the OTN map no station and still reads as GLATOS.
otn_station_no <- function(raw) {
  if (!all(c("array", "station_no") %in% names(raw))) return(raw)
  raw$array_station_no <- raw$station_no
  raw
}

# A station is one fixed place (CONTRACT.md section 5), but GLATOS station
# codes repeat across projects: one code can name stations of two projects
# hundreds of kilometres apart, and detfield joins receivers by station_id.
warn_reused_stations <- function(df, file) {
  d <- as_dt(df)[!is.na(station_id) & !is.na(deploy_lat) & !is.na(deploy_lon)]
  if (!nrow(d)) return(0L)
  if (!"glatos_project" %in% names(d)) d[, glatos_project := NA_character_]
  sp <- d[, list(spread_km = max(dist_km_matrix(deploy_lat, deploy_lon)),
                 projects = length(unique(stats::na.omit(glatos_project)))),
          by = "station_id"]
  far <- sp[spread_km > 1]
  if (!nrow(far)) return(0L)
  far <- far[order(-spread_km)]
  warn_detqc(paste("%s: %s of %s station_id values hold deployments more than 1 km apart (%s more",
                   "than 50 km; %s across more than one glatos_project), such as %s, %s km apart.",
                   "A station is one fixed place, and detset() and detfield join receivers by",
                   "station_id: keep your study area's deployments (as_detfield_input(extent =)",
                   "does) or rename these stations. check_detset() lists them as station_reused."),
             file, fmt_n(nrow(far)), fmt_n(nrow(sp)), fmt_n(sum(far$spread_km > 50)),
             fmt_n(sum(far$projects > 1)), far$station_id[1], fmt_n(round(far$spread_km[1])))
  nrow(far)
}

# OTN short forms list the mooring's other instruments beside the receiver:
# "anchor" rows with no serial, never recovered, which the checks read as
# overlapping, silent deployments of one receiver. A row with no serial
# whose model is one of these words is not a deployment.
not_receiver_words <- "\\b(anchor|release|float|buoy|mooring|weight|sinker|tripod)\\b"

not_a_receiver <- function(df) {
  if (!"receiver_model" %in% names(df)) return(rep(FALSE, nrow(df)))
  m <- tolower(trimws(as.character(df$receiver_model)))
  is.na(df$receiver_serial) & !is.na(m) & grepl(not_receiver_words, m, perl = TRUE) &
    !grepl(receiver_pattern, paste0(m, "-000"), perl = TRUE, ignore.case = TRUE)
}

# A GLATOS project workbook keeps its times in local time, in the columns
# that start glatos_, with the zone of each row in glatos_timezone
# ("Eastern"); its UTC columns are often empty. Each local time is converted
# here, as glatos::read_glatos_workbook() does, and written to a column
# ending _utc that the glatos map reads after the UTC column. The trailing
# "Z" keeps a `tz` argument from moving it again.
glatos_local_times <- function(raw) {
  cols <- intersect(c("glatos_deploy_date_time", "glatos_recover_date_time",
                      "glatos_release_date_time"), names(raw))
  if (!length(cols) || !"glatos_timezone" %in% names(raw)) return(raw)
  zone <- glatos_zone(raw$glatos_timezone)
  n <- 0L
  for (cl in cols) {
    utc <- empty_utc(nrow(raw))
    for (z in unique(stats::na.omit(zone))) {
      rows <- which(zone == z)
      utc[rows] <- parse_utc(raw[[cl]][rows], z, cl, strict = FALSE)
    }
    raw[[paste0(cl, "_utc")]] <- ifelse(is.na(utc), NA_character_,
                                        format(utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    n <- n + sum(!is.na(utc))
  }
  attr(raw, "local_times") <- n
  raw
}

# A GLATOS workbook records recoveries on their own sheet. Without them
# every deployment reads as still in the water. Rows are joined on the keys
# glatos::read_glatos_workbook() uses; a deployment with no recovery row
# stays open, and the attribute "recoveries_joined" counts the joined rows.
glatos_recoveries <- function(file, raw) {
  if (!tolower(tools::file_ext(file)) %in% c("xls", "xlsx", "xlsm")) return(raw)
  sheets <- readxl::excel_sheets(file)
  sh <- sheets[tolower(sheets) == "recovery"]
  key <- c("glatos_array", "station_no", "consecutive_deploy_no", "ins_serial_no")
  if (!length(sh) || !all(key %in% names(raw))) return(raw)
  rec <- as.data.frame(readxl::read_excel(file, sheet = sh[1], col_types = "text"))
  names(rec) <- clean_names(names(rec))
  names(rec)[names(rec) == "ins_serial_number"] <- "ins_serial_no"
  if (!all(key %in% names(rec))) return(raw)
  take <- intersect(c("recover_date_time", "glatos_recover_date_time", "glatos_timezone",
                      "recover_lat", "recover_long", "recovered", "data_downloaded"), names(rec))
  id <- function(df) do.call(paste, c(lapply(key, function(k) trimws(df[[k]])), sep = "\r"))
  rec <- rec[!duplicated(id(rec)), , drop = FALSE]
  k <- match(id(raw), id(rec))
  for (cl in take) {
    v <- rec[[cl]][k]
    raw[[cl]] <- if (cl %in% names(raw)) coalesce_values(blank_na(raw[[cl]]), v) else v
  }
  attr(raw, "recoveries_joined") <- sum(!is.na(k))
  raw
}

# GLATOS names US zones without the "US/" prefix, as glatos does.
glatos_zone <- function(z) {
  z <- trimws(as.character(z))
  olson <- OlsonNames()
  hit <- match(tolower(z), tolower(olson))
  us <- match(tolower(paste0("US/", z)), tolower(olson))
  out <- olson[ifelse(is.na(hit), us, hit)]
  bad <- !is.na(z) & is.na(out)
  if (any(bad)) {
    warn_detqc("%s %s a time zone that is not in OlsonNames(), such as \"%s\"; %s %s",
               n_of(sum(bad), "row"), verb(sum(bad), "names", "name"), z[bad][1],
               verb(sum(bad), "its local time is", "their local times are"), "read as NA.")
  }
  out
}

check_tz <- function(tz) {
  if (!is.character(tz) || length(tz) != 1 || !tz %in% OlsonNames()) {
    stop_detqc("`tz` must be NULL or one time zone name from OlsonNames().")
  }
  tz
}

# The first recovery text of each row that holds letters, as in
# "failed: 2010-03-07"; NA where the recovery is a plain date or blank.
recovery_status <- function(raw, map) {
  cols <- intersect(map$source[map$target == "recover_datetime"], names(raw))
  status <- rep(NA_character_, nrow(raw))
  for (cl in cols) {
    v <- trimws(as.character(raw[[cl]]))
    take <- is.na(status) & !is.na(v) & grepl("[A-Za-z]{2,}", v)
    status[take] <- v[take]
  }
  if (any(!is.na(status))) {
    k <- sum(!is.na(status))
    warn_detqc("%s %s a status, such as \"%s\"; the text is in recovery_status.",
               n_of(k, "recovery value"), verb(k, "carries", "carry"), status[!is.na(status)][1])
  }
  status
}

# Columns named inside data.table calls in this file.
utils::globalVariables(c("spread_km", "projects", "glatos_project"))
