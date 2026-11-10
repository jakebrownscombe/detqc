# ---- Hand the detections to detfield ----

#' The tables detfield::bin_detections() takes
#'
#' Returns the `detections`, `listening` and `tag_delay` inputs of
#' `detfield::bin_detections()` from a detset, with the names detfield
#' reads. detqc and detfield do not import each other; they meet through
#' these tables. Two inputs are often missing from real files, and
#' `bin_detections()` stops until they are given: transmission delays
#' (network exports carry none; see `tag_delay` below), and `codes`, how
#' the codes of a tag that sends two are counted (see
#' `detfield::bin_detections()`).
#'
#' * `detections`: `datetime`, `receiver_serial`, `transmitter`, `animal_id`
#'   and `station_id`. Rows with no `animal_id`, such as reference tags, are
#'   kept; `bin_detections()` leaves them out.
#' * `listening`: by default `list(deployments = , days = )`, the form in
#'   which a receiver listens in a bin when its deployment covers the bin and
#'   the bin's UTC date is a listed day. Each deployment ends at its
#'   `recover_datetime` or its `download_datetime`, whichever comes first:
#'   the set holds no data after a download, so the hours after it on the
#'   download day are not listening. A deployment with no recovery time
#'   ends where the same receiver is deployed again. A detection after the
#'   recorded download keeps the end at the download: the detection is
#'   returned, and `detfield::bin_detections()` gives its rows
#'   `listening = FALSE`. A message counts such detections in the tables
#'   returned; they are the rows [detset()] marks in `after_download`. With
#'   a listening table from `audit_listening()`, each deployment ends where
#'   the audit ended it. The default list
#'   and the days table alone can differ by a few receiver-days and hourly
#'   rows on the first and last day of a deployment, and at a station whose
#'   deployments overlap, where the hourly rows take the serial of one of
#'   them. On the first and last day, the days table lists whole UTC days
#'   and the deployments table starts and ends at the recorded times.
#'   `days` is `x$listening`, with the logical column `listening` that
#'   `audit_listening()` writes: a day whose receiver was not downloaded,
#'   did not hear its own reference tag, or falls in a silent run, is not
#'   used, nor, with receiver logs, a day the receiver was `"deaf"` or
#'   `"off"`; a silent day, and a `"logging_silent"` one, counts as
#'   listening. `listening = "days"` or `"deployments"` gives one form alone.
#' * `tag_delay`: `transmitter`, `animal_id`, `min_delay` and `max_delay`
#'   from the animals table, for transmitters with both delays; `NULL`, with a
#'   message, when none has them. [read_animals()] with `tag_specs` fills the
#'   delays from the manufacturer's sheet. Otherwise give `tag_delay` to
#'   `bin_detections()` yourself.
#'
#' **Network receiver files.** A GLATOS or OTN receiver file lists every
#' receiver in the network, so every bin would list thousands of receivers
#' in other lakes as silent listeners, and `detfield::location_surface()`
#' stops on listening receivers its field does not hold. Give `stations`,
#' such as the `receivers` of a field keyed by `station_id`, or `extent`, to
#' keep only those deployments with their detections and listening days. A
#' message counts what was kept, and, with neither given, says so when the
#' deployments name more than ten times as many stations as heard anything.
#'
#' @param x A `detset`.
#' @param listening `"both"`, `"days"` or `"deployments"`.
#' @param stations Optional `station_id` values to keep.
#' @param extent Optional `c(lon_min, lon_max, lat_min, lat_max)`, decimal
#'   degrees, WGS 84: keep the deployments whose position lies inside.
#' @return A list of `detections`, `listening` and `tag_delay`.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' detfield_input <- as_detfield_input(dets, listening = "days")
#' head(detfield_input$detections, 3)
#' head(detfield_input$listening)
#' # Keep two_stations stations only, as for a field built on them.
#' two_stations <- as_detfield_input(dets, stations = c("S010", "S011"))
as_detfield_input <- function(x, listening = c("both", "days", "deployments"), stations = NULL,
                              extent = NULL) {
  validate_detset(x)
  listening <- check_choice(listening, c("both", "days", "deployments"), "listening")
  det <- x$detections[, c("datetime", "receiver_serial", "transmitter", "animal_id",
                          "station_id")]
  days <- x$listening
  # audit_listening() writes `listening` itself; a silent day still listened.
  if ("status" %in% names(days) && !"listening" %in% names(days)) {
    days$listening <- !days$status %in% not_listening
  }
  dep <- x$deployments[, c("receiver_serial", "station_id", "deploy_datetime",
                           "recover_datetime")]
  data_end <- deployment_data_end(x$deployments)
  audited_end <- audited_data_end(x$listening, x$deployments)
  data_end[!is.na(audited_end)] <- as.POSIXct(audited_end[!is.na(audited_end)],
                                              origin = "1970-01-01", tz = "UTC")
  dep$recover_datetime <- as.POSIXct(as.numeric(data_end), origin = "1970-01-01", tz = "UTC")
  late <- !is.na(after_download_index(x$deployments, x$detections))
  kept <- restrict_deployments(x$deployments, stations, extent)
  if (!is.null(kept)) {
    key <- function(df) paste(df$receiver_serial, df$station_id)
    keys <- key(x$deployments[kept, ])
    # A deployment with no serial is kept by its station, so a detection
    # there is kept whatever receiver logged it.
    bare <- x$deployments$station_id[kept & is.na(x$deployments$receiver_serial)]
    det_in <- key(det) %in% keys | det$station_id %in% bare
    message(sprintf(paste("as_detfield_input(): kept %s of %s, %s of %s\nand %s of %s at",
                          "the stations given."),
                    fmt_n(sum(kept)), n_of(length(kept), "deployment"), fmt_n(sum(det_in)),
                    n_of(nrow(det), "detection"), fmt_n(sum(key(days) %in% keys)),
                    n_of(nrow(days), "receiver-day")))
    det <- det[det_in, , drop = FALSE]
    late <- late[det_in]
    days <- days[key(days) %in% keys, , drop = FALSE]
    dep <- dep[kept, , drop = FALSE]
  } else {
    note_network_listening(dep, det)
  }
  note_late_input(sum(late), nrow(det))
  rownames(det) <- NULL
  rownames(days) <- NULL
  rownames(dep) <- NULL
  lst <- switch(listening, days = days, deployments = dep,
                both = list(deployments = dep, days = days))
  list(detections = det, listening = lst, tag_delay = detfield_delays(x$animals))
}

# The message of as_detfield_input() when detections it returns fall after
# their deployment's recorded download.
note_late_input <- function(n, of) {
  if (!n) return(invisible(NULL))
  message(sprintf(paste(
    "as_detfield_input(): %s of %s %s after the deployment's recorded download, where the",
    "listening tables end. detfield::bin_detections() keeps such rows with listening = FALSE;",
    "check_detset(checks = \"detections_after_download\") lists the detections."
  ), fmt_n(n), n_of(of, "detection"), verb(n, "falls", "fall")))
  invisible(NULL)
}

# The deployments kept by `stations` and `extent`, or NULL when neither is
# given. A network receiver file would make every bin list thousands of
# silent receivers in other lakes.
restrict_deployments <- function(dep, stations, extent) {
  if (is.null(stations) && is.null(extent)) return(NULL)
  keep <- at_stations(dep, stations) & in_extent(dep, extent)
  if (!any(keep)) stop_detqc("No deployment lies at the stations or in the extent given.")
  keep
}

at_stations <- function(dep, stations) {
  if (is.null(stations)) return(rep(TRUE, nrow(dep)))
  if (!is.character(stations) || !length(stations)) {
    stop_detqc("`stations` must be station_id values.")
  }
  dep$station_id %in% stations
}

in_extent <- function(dep, extent) {
  if (is.null(extent)) return(rep(TRUE, nrow(dep)))
  ok <- is.numeric(extent) && length(extent) == 4 && !anyNA(extent) &&
    extent[1] < extent[2] && extent[3] < extent[4]
  if (!ok) stop_detqc("`extent` must be c(lon_min, lon_max, lat_min, lat_max).")
  dep$deploy_lon >= extent[1] & dep$deploy_lon <= extent[2] &
    dep$deploy_lat >= extent[3] & dep$deploy_lat <= extent[4]
}

note_network_listening <- function(dep, det) {
  n_dep <- length(unique(stats::na.omit(dep$station_id)))
  n_det <- length(unique(stats::na.omit(det$station_id)))
  if (n_det && n_dep > 10 * n_det) {
    message(sprintf(paste("The deployments name %s stations, and %s of them heard a detection.",
                          "This looks like a network receiver file. Give `stations` or `extent`",
                          "to keep your study area's receivers."), fmt_n(n_dep), fmt_n(n_det)))
  }
  invisible(NULL)
}

detfield_delays <- function(ani) {
  has <- !is.null(ani) && all(c("min_delay", "max_delay") %in% names(ani))
  ok <- if (has) !is.na(ani$min_delay) & !is.na(ani$max_delay) else FALSE
  if (is.null(ani)) {
    message("The detset has no animals table, so it holds no delays; tag_delay is NULL.")
    return(NULL)
  }
  if (!any(ok)) {
    message("The animals table has no min_delay and max_delay; tag_delay is NULL.")
    return(NULL)
  }
  out <- ani[ok, c("transmitter", "animal_id", "min_delay", "max_delay")]
  rownames(out) <- NULL
  out
}
