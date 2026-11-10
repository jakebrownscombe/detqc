# ---- Shared state of one check_detset() run ----

# Columns that the checks, filters, the listening audit and the summaries
# build and read inside data.table calls. R CMD check would report them as
# undefined globals.
utils::globalVariables(c(
  "a", "active", "after_download", "basis", "conflict", "day", "days", "days_at_liberty",
  "days_detected", "days_listening", "days_period", "de", ".dep", "dep_a", "dep_b",
  "deployed_share", "drawn_to", "dst", "e", "edge", "flag", "heard", "hit", "i", "i.t", "i.t0",
  ".key",
  "key", "key_a", "key_b", "lag_h", "lead", "len", "listening", "N", "n_orphans", "offset_h",
  "ok", "out", "overruled", "partner", "prior_dep", "prior_end", "problem", ".q", ".r", "reach",
  "ref_tv", "reference_detections", "release_datetime", "row_a", "row_b", "rx_a", "rx_b", "s",
  ".SD", ".serial", "side", "species", "split_day", "spread_m", "status", "step_min", "t0", "t1",
  "tag_life_days", "tail", "total", "transmitters", "tv", "V1", "water", "way", "x..dep",
  "x..row", "x.date", "x.end", "x.rx", "x.start", "x.t"
))

# Every check reads one context, built once: the tables in data.table form
# with row ids, each detection's deployment, and the classifications that
# let one check defer to a more specific one. A classification is computed
# here, from the data alone, so a check defers in the same way whichever
# other checks the user selects.

check_context <- function(x, settings, water, field, reference_tags) {
  tabs <- check_tables(x)
  tz <- vapply(tabs[c("detections", "deployments")], time_zones, "")
  det <- as_dt(tabs$detections)
  dep <- as_dt(tabs$deployments)
  to_utc_columns(det, "datetime")
  to_utc_columns(dep, c("deploy_datetime", "recover_datetime", "download_datetime"))
  det[, .row := .I]
  dep[, .dep := .I]
  det[, .dep := as.vector(match_deployment(det, dep))]
  det[, .serial := serial_in_deployments(det$receiver_serial, dep$receiver_serial)]
  ani <- if (is.null(tabs$animals)) NULL else as_dt(tabs$animals)
  if (!is.null(ani)) to_utc_columns(ani, "release_datetime")
  ref <- check_reference_tags(reference_tags)
  ctx <- new.env(parent = emptyenv())
  ctx$det <- det
  ctx$dep <- dep
  ctx$ani <- ani
  ctx$ref <- ref
  ctx$tz <- tz
  ctx$settings <- settings
  ctx$water <- water
  ctx$field <- field
  ctx$position <- classify_positions(dep, settings)
  ctx$orphans <- classify_orphans(det, dep, settings)
  ctx
}

check_tables <- function(x) {
  if (inherits(x, "detset")) {
    validate_detset(x)
    return(list(detections = x$detections, deployments = x$deployments, animals = x$animals))
  }
  if (!is.list(x) || is.data.frame(x) || !all(c("detections", "deployments") %in% names(x))) {
    stop_detqc("`x` must be a detset or a list with `detections` and `deployments`.")
  }
  need_columns(x$detections, "detections", c("datetime", "receiver_serial", "transmitter"))
  need_columns(x$deployments, "deployments", c("receiver_serial", "station_id", "deploy_datetime",
                                               "recover_datetime", "deploy_lat", "deploy_lon"))
  if (!is.null(x$animals)) check_columns(x$animals, contract_spec$animals[1:2, ], "animals")
  if (!"animal_id" %in% names(x$detections)) x$detections$animal_id <- NA_character_
  x[c("detections", "deployments", "animals")]
}

need_columns <- function(df, label, cols) {
  if (!is.data.frame(df)) stop_detqc("`x$%s` must be a data frame.", label)
  miss <- setdiff(cols, names(df))
  if (length(miss)) stop_detqc("`%s` has no column `%s`.", label, miss[1])
  invisible(TRUE)
}

# Time zones as found, before conversion: the non-UTC check reads them.
time_zones <- function(df) {
  cols <- names(df)[vapply(df, inherits, logical(1), "POSIXct")]
  zones <- unique(vapply(cols, function(cl) {
    z <- attr(df[[cl]], "tzone")
    if (is.null(z) || !nzchar(z[1])) "local" else z[1]
  }, ""))
  paste(setdiff(zones, "UTC"), collapse = ", ")
}

to_utc_columns <- function(dt, cols) {
  for (cl in intersect(cols, names(dt))) {
    v <- dt[[cl]]
    if (is.character(v)) v <- as.POSIXct(v, tz = "UTC")
    if (!inherits(v, "POSIXct")) stop_detqc("Column `%s` must be a date-time.", cl)
    data.table::set(dt, j = cl, value = lubridate::with_tz(v, "UTC"))
  }
  invisible(dt)
}

# Seconds since 1970 as UTC date-times. R before 4.3 needs the origin.
num_utc <- function(x) {
  as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
}

# TRUE where a serial, as written or as the bare serial, names a deployment.
# A missing serial names no receiver: `NA %in% c(NA, ...)` is TRUE in R, so
# an NA serial would match every serial-less deployment.
serial_in_deployments <- function(det_serial, dep_serial) {
  dep_keys <- stats::na.omit(unique(c(dep_serial, split_receiver(dep_serial)$serial)))
  !is.na(det_serial) &
    (det_serial %in% dep_keys | split_receiver(det_serial)$serial %in% dep_keys)
}

check_reference_tags <- function(ref) {
  if (is.null(ref)) return(NULL)
  if (!is.data.frame(ref) || !"transmitter" %in% names(ref)) {
    stop_detqc("`reference_tags` must be a data frame with a `transmitter` column.")
  }
  as_dt(ref)
}

# ---- Internal: positions ----
# Each deployment row gets one label: "ok", "units", "swapped", "sign" or
# "outside". The swap is tried before the sign, and a position that neither
# brings inside the extent is "units" when it is not in degrees at all.
classify_positions <- function(dep, settings) {
  lat <- dep$deploy_lat
  lon <- dep$deploy_lon
  inside <- extent_test(lat, lon, settings)
  degrees <- !is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180
  label <- rep("ok", nrow(dep))
  label[is.na(lat) | is.na(lon)] <- "missing"
  bad <- label == "ok" & !(degrees & inside(lat, lon))
  swap <- bad & inside(lon, lat)
  sign <- bad & !swap & (inside(-lat, lon) | inside(lat, -lon) | inside(-lat, -lon))
  label[bad] <- "outside"
  label[bad & !degrees] <- "units"
  label[swap] <- "swapped"
  label[sign] <- "sign"
  label
}

# A function of (lat, lon) that is TRUE inside the study extent: the given
# extent, or the outlier radius around the median position of the
# deployments given in degrees.
extent_test <- function(lat, lon, settings) {
  ext <- settings$extent
  if (!is.null(ext)) {
    return(function(la, lo) {
      !is.na(la) & !is.na(lo) & lo >= ext[1] & lo <= ext[2] & la >= ext[3] & la <= ext[4]
    })
  }
  ok <- !is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180
  if (!any(ok)) return(function(la, lo) rep(FALSE, length(la)))
  c_lat <- stats::median(lat[ok])
  c_lon <- stats::median(lon[ok])
  spread <- stats::median(haversine_km(c_lat, c_lon, lat[ok], lon[ok]))
  radius <- max(settings$outlier_km, settings$outlier_mult * spread)
  function(la, lo) {
    deg <- !is.na(la) & !is.na(lo) & abs(la) <= 90 & abs(lo) <= 180
    out <- rep(FALSE, length(la))
    out[deg] <- haversine_km(c_lat, c_lon, la[deg], lo[deg]) <= radius
    out
  }
}

# ---- Internal: detections outside every deployment ----
# Orphans are detections at a known serial that no deployment holds. Two
# errors explain them and are reported by their own checks: a whole-hour
# offset at the deployment ends (non_utc) and a date a year off
# (deploy_year_off). What remains is outside_deployment.
classify_orphans <- function(det, dep, settings) {
  orph <- det[is.na(.dep) & .serial == TRUE, c(".row", "receiver_serial", "datetime")]
  orph[, .key := split_receiver(receiver_serial)$serial]
  d <- data.table::copy(dep)
  d[, .key := split_receiver(receiver_serial)$serial]
  tz <- tz_offset_deployments(det, d, orph, settings)
  year <- year_off_deployments(det, d, orph, tz$near_rows, settings)
  bad <- bad_window_spans(d, year$rows$.dep)
  explained <- c(orph$.row[orphans_in(orph, year$windows)], orph$.row[orphans_in(orph, bad)])
  if (tz$flag) explained <- c(explained, orph$.row[orphans_in(orph, tz$windows)])
  list(rows = orph$.row, year = year, tz = tz, explained = unique(explained))
}

# A deployment that ends before it starts holds nothing, so the detections of
# its receiver from its start to the next deployment of that receiver are
# orphans. They are reported with the bad recovery date as their cause.
bad_window_spans <- function(d, year_rows) {
  far_end <- as.POSIXct("9999-12-31", tz = "UTC")
  b <- d[!is.na(recover_datetime) & recover_datetime < deploy_datetime & !.dep %in% year_rows,
         list(.dep, .key, start = deploy_datetime)]
  if (!nrow(b)) return(NULL)
  b[, end := vapply(seq_len(.N), function(i) {
    nxt <- d$deploy_datetime[d$.key == .key[i] & d$deploy_datetime > start[i]]
    as.numeric(if (length(nxt)) min(nxt) else far_end)
  }, numeric(1))]
  b[, end := num_utc(end)]
  b[, list(.key, start, end)]
}

# TRUE for each orphan inside one of the windows (`.key`, `start`, `end`).
orphans_in <- function(orph, windows) {
  if (!nrow(orph) || is.null(windows) || !nrow(windows)) return(rep(FALSE, nrow(orph)))
  w <- data.table::as.data.table(windows)
  q <- data.table::data.table(.key = orph$.key, t = orph$datetime, .q = seq_len(nrow(orph)))
  hit <- w[q, on = list(.key, start <= t, end >= t), list(.q = .q), nomatch = NULL,
           allow.cartesian = TRUE]
  seq_len(nrow(orph)) %in% hit$.q
}
