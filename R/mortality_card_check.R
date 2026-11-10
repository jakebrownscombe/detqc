# ---- Read-back check of the review cards ----
# A check that recomputed the numbers from the writer's own context would
# pass a card that shows 0 detections, so this check shares no code with
# the writer: what it compares is the card file as a reader opens it, and
# the counts from `x$detections`, `x$deployments`, `x$animals` and the
# bands. Every quantity must be equal: an empty map on a card whose animal
# has located detections fails, and so does a sensor series scaled by any
# factor.

# The quantities compared, in the order `failed` names them.
card_quantities <- c(
  "detections", "detection_days", "daily_counts", "first_day", "last_day", "stations",
  "station_list", "station_days", "located", "sensor_days", "sensor_values", "band",
  "candidate_date", "votes", "release", "expiry", "to_horizon", "species"
)

# Returns a data frame: `checked` and `failed` per file, in the order given.
card_readback <- function(files, x, cb) {
  if (!length(files)) return(data.frame(checked = logical(), failed = character()))
  truth <- card_truth(x)
  res <- lapply(files, function(f) {
    p <- read_card_data(f)
    ok <- compare_card(p, truth, cb)
    data.frame(checked = all(ok), failed = paste(names(ok)[!ok], collapse = ", "),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, res)
}

# The text between the data block's opening tag and the next closing script
# tag. The writer escapes every "<" inside the JSON, so that tag ends it.
read_card_data <- function(file) {
  h <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  open_tag <- "<script id=\"detqc-data\" type=\"application/json\">"
  parts <- strsplit(h, open_tag, fixed = TRUE)[[1]]
  if (length(parts) != 2) stop_detqc("%s holds %s, not 1.", basename(file),
                                     n_of(length(parts) - 1L, "data block"))
  json <- strsplit(parts[2], "</script>", fixed = TRUE)[[1]][1]
  jsonlite::fromJSON(json, simplifyVector = TRUE)
}

# Everything the comparison needs, computed once for the whole detset:
# detections per animal and UTC day, per day and station_id, and per day and
# position; the stations; the data horizon; and the daily sensor summaries.
# Positions are kept as integer micro-degrees, so the comparison is exact.
card_truth <- function(x) {
  d <- x$detections
  ll <- located_truth(d, x$deployments)
  dt <- data.table::data.table(a = d$animal_id, day = as.integer(as.Date(d$datetime)),
                               st = as.character(d$station_id),
                               la = round(ll$lat * 1e6), lo = round(ll$lon * 1e6))
  dt <- dt[!is.na(a)]
  ids <- c(as.character(x$deployments$station_id), as.character(d$station_id))
  list(per_day = dt[, list(n = .N), keyby = list(a, day)],
       per_station = dt[!is.na(st), list(n = .N), keyby = list(a, day, st)],
       per_position = dt[!is.na(la) & !is.na(lo), list(n = .N), keyby = list(a, day, la, lo)],
       all_stations = sort(unique(ids[!is.na(ids)])), horizon = max(dt$day),
       animals = x$animals, sensor = sensor_truth(x))
}

# The position of each detection: the deployment of its receiver whose
# interval holds it, matched on the full serial and then on the bare serial
# number, then a deployment with no serial at the detection's own station,
# else the detection's own deploy_lat and deploy_lon. Where two
# deployments of one receiver overlap, the later one is used. The serial
# forms are those of `receiver_pattern`, the package's definition of a
# receiver serial; nothing else is shared with the card writer.
located_truth <- function(d, dep) {
  row <- rep(NA_integer_, nrow(d))
  if (!is.null(dep) && nrow(dep) && "receiver_serial" %in% names(d)) {
    rx <- as.character(d$receiver_serial)
    row <- window_row(rx, d$datetime, as.character(dep$receiver_serial), dep)
    miss <- which(is.na(row) & !is.na(rx))
    if (length(miss)) {
      dep_bare <- bare_serial(as.character(dep$receiver_serial))
      forms <- tapply(dep$receiver_serial, dep_bare, function(v) length(unique(v)))
      dep_bare[dep_bare %in% names(forms)[forms > 1]] <- NA_character_
      row[miss] <- window_row(bare_serial(rx[miss]), d$datetime[miss], dep_bare, dep)
    }
  }
  row <- station_rows(row, d, dep)
  lat <- if (is.null(dep)) rep(NA_real_, nrow(d)) else as.numeric(dep$deploy_lat[row])
  lon <- if (is.null(dep)) rep(NA_real_, nrow(d)) else as.numeric(dep$deploy_lon[row])
  if (all(c("deploy_lat", "deploy_lon") %in% names(d))) {
    own <- is.na(lat) | is.na(lon)
    lat[own] <- as.numeric(d$deploy_lat[own])
    lon[own] <- as.numeric(d$deploy_lon[own])
  }
  list(lat = lat, lon = lon)
}

# A deployment with no receiver serial holds the detections of its station.
station_rows <- function(row, d, dep) {
  if (is.null(dep) || !nrow(dep) || !"station_id" %in% names(d) || !anyNA(dep$receiver_serial)) {
    return(row)
  }
  miss <- which(is.na(row) & !is.na(d$station_id))
  dep_st <- ifelse(is.na(dep$receiver_serial), as.character(dep$station_id), NA_character_)
  if (length(miss)) {
    row[miss] <- window_row(as.character(d$station_id[miss]), d$datetime[miss], dep_st, dep)
  }
  row
}

bare_serial <- function(s) {
  hit <- !is.na(s) & grepl(receiver_pattern, s, perl = TRUE, ignore.case = TRUE)
  s[hit] <- sub(receiver_pattern, "\\2", s[hit], perl = TRUE, ignore.case = TRUE)
  s
}

# The row of `dep` whose half-open interval [deploy, recover) holds each time
# on the same key; a deployment still in the water has no end.
window_row <- function(key, time, dep_key, dep) {
  ok <- which(!is.na(dep_key))
  w <- data.table::data.table(k = dep_key[ok], s = dep$deploy_datetime[ok],
                              e = dep$recover_datetime[ok], r = ok)
  w[is.na(e), e := as.POSIXct("9999-12-31", tz = "UTC")]
  q <- data.table::data.table(k = key, t = time, i = seq_along(key))[!is.na(k)]
  m <- w[q, on = list(k, s <= t, e > t), list(i = i.i, r = x.r, s0 = x.s), nomatch = NULL,
         allow.cartesian = TRUE]
  data.table::setorderv(m, c("i", "s0", "r"), order = c(1L, -1L, -1L))
  m <- m[!duplicated(m$i)]
  out <- rep(NA_integer_, length(key))
  out[m$i] <- m$r
  out
}

# Calibrated sensor readings summarised by animal, class and UTC day: the
# median and the 5th and 95th percentiles (type 7, as stats::quantile). A
# reading counts where `sensor_value` is set, or where `sensor_raw` meets a
# slope and an intercept in the animals table. The class is read from the
# type and unit text, once per distinct combination.
sensor_truth <- function(x) {
  d <- x$detections
  if (!any(c("sensor_raw", "sensor_value") %in% names(d))) return(NULL)
  col <- function(tbl, nm) if (nm %in% names(tbl)) tbl[[nm]] else rep(NA, nrow(tbl))
  r <- data.table::data.table(a = d$animal_id, tx = d$transmitter,
                              val = as.numeric(col(d, "sensor_value")),
                              raw = as.numeric(col(d, "sensor_raw")), u = col(d, "sensor_unit"),
                              ty = col(d, "sensor_type"), datetime = d$datetime)
  r <- r[!is.na(a) & (!is.na(val) | !is.na(raw))]
  r <- sensor_calibration(x$animals, col)[r, on = "tx"]
  r[, v := data.table::fifelse(!is.na(val), val, icpt + slope * raw)]
  r <- r[!is.na(v)]
  combos <- unique(r[, list(u, ty, aty, au)])
  txt <- tolower(paste(combos$u, combos$ty, combos$aty, combos$au))
  combos[, cls := ifelse(grepl("accel|activity|tailbeat|m/s|m s-2", txt), "acceleration",
                         ifelse(grepl("depth|pressure|metre|meter|\\bm\\b", txt), "depth",
                                "other"))]
  r <- combos[r, on = c("u", "ty", "aty", "au")]
  r[, day := as.integer(as.Date(datetime))]
  data.table::setorderv(r, c("a", "cls", "day", "v"))
  r[, list(med = q7(v, 0.5), lo = q7(v, 0.05), hi = q7(v, 0.95)), keyby = list(a, cls, day)]
}

sensor_calibration <- function(ani, col) {
  cal <- data.table::data.table(tx = character(), slope = numeric(), icpt = numeric(),
                                aty = character(), au = character())
  if (is.null(ani)) return(cal)
  unique(data.table::data.table(tx = ani$transmitter, slope = as.numeric(col(ani, "sensor_slope")),
                                icpt = as.numeric(col(ani, "sensor_intercept")),
                                aty = col(ani, "sensor_type"), au = col(ani, "sensor_unit")),
         by = "tx")
}

# Type 7 quantile of a sorted vector.
q7 <- function(v, p) {
  n <- length(v)
  h <- (n - 1) * p + 1
  j <- floor(h)
  v[j] + (h - j) * (v[min(j + 1, n)] - v[j])
}

# ---- Comparing one card ----
# One logical per quantity in `card_quantities`. `cb` holds the bands and
# votes as build_mortality_cards() received them.
compare_card <- function(p, truth, cb) {
  if (is.null(p$animal$id) || is.null(p$series$dets)) stop_detqc("The file is not a review card.")
  id <- p$animal$id
  wanted <- data.table::data.table(a = id)
  pd <- truth$per_day[wanted, on = "a", nomatch = NULL]
  # A card of an animal the detset does not hold matches nothing.
  if (!nrow(pd)) return(stats::setNames(rep(FALSE, length(card_quantities)), card_quantities))
  t0 <- as.integer(as.Date(p$t0))
  cells <- truth$per_position[wanted, on = "a", nomatch = NULL]
  ok <- c(
    day_checks(p, pd, as.Date(p$t0)),
    station_checks(p, truth$per_station[wanted, on = "a", nomatch = NULL], truth, t0),
    located = same_cells(card_cells(p$ds, t0, p$stations), cells[, list(day, la, lo, n)]),
    sensor_checks(p, truth$sensor, id, t0),
    band_checks(p, cb, t0),
    animal_checks(p, truth, pd)
  )
  ok[card_quantities]
}

# The detections of every day, from the card's array and from the detset.
day_checks <- function(p, pd, t0) {
  dets <- p$series$dets
  n_det <- sum(pd$n)
  per_day <- integer(p$nd)
  i <- pd$day - as.integer(t0) + 1L
  ok_i <- i >= 1L & i <= p$nd
  per_day[i[ok_i]] <- pd$n[ok_i]
  shown <- which(dets > 0)
  day1 <- format(as.Date(min(pd$day), origin = "1970-01-01"))
  dayn <- format(as.Date(max(pd$day), origin = "1970-01-01"))
  any_shown <- length(shown) > 0
  c(
    detections = sum(dets) == n_det && p$animal$detections == n_det,
    detection_days = length(shown) == nrow(pd) && p$animal$detection_days == nrow(pd),
    daily_counts = all(ok_i) && identical(as.integer(dets), per_day),
    first_day = any_shown && format(t0 + shown[1] - 1L) == p$animal$first &&
      p$animal$first == day1,
    last_day = any_shown && format(t0 + max(shown) - 1L) == p$animal$last &&
      p$animal$last == dayn
  )
}

# The station panel: one row per station_id that heard the animal, and the
# detections of each day at each station.
station_checks <- function(p, ps, truth, t0) {
  sids <- as.character(p$sids)
  heard <- sids[as.integer(p$heard) + 1L]
  own <- unique(ps$st)
  dss <- matrix(as.integer(p$dss), ncol = 3, byrow = TRUE)
  cells <- data.table::data.table(day = dss[, 1] + t0, st = sids[dss[, 2] + 1L], n = dss[, 3])
  c(
    stations = isTRUE(p$animal$stations == length(own)) && length(heard) == length(own) &&
      setequal(heard, own),
    station_list = identical(sort(sids), truth$all_stations),
    station_days = same_cells(cells, ps[, list(day, st, n)])
  )
}

# Day-by-position cells of the map, as integer micro-degrees.
card_cells <- function(ds, t0, stations) {
  m <- matrix(as.integer(ds), ncol = 3, byrow = TRUE)
  j <- m[, 2] + 1L
  data.table::data.table(day = m[, 1] + t0, la = round(as.numeric(stations$lat[j]) * 1e6),
                         lo = round(as.numeric(stations$lon[j]) * 1e6), n = m[, 3])
}

same_cells <- function(a, b) {
  if (nrow(a) != nrow(b)) return(FALSE)
  if (!nrow(a)) return(TRUE)
  a <- as.data.frame(a)[do.call(order, unname(as.list(a))), , drop = FALSE]
  b <- as.data.frame(b)[do.call(order, unname(as.list(b))), , drop = FALSE]
  all(vapply(seq_along(a), function(k) isTRUE(all(a[[k]] == b[[k]])), logical(1)))
}

# The depth panel and the third sensor panel: the same days, and every daily
# median and percentile within 0.001 (the card rounds them to 3 decimals).
sensor_checks <- function(p, tab, id, t0) {
  rows <- if (is.null(tab)) NULL else tab[data.table::data.table(a = id), on = "a", nomatch = NULL]
  sr <- p$series
  third <- "acceleration"
  if (!isTRUE(p$has$accel) && !is.null(p$has$other)) third <- "other"
  dep <- sensor_series_ok(rows, "depth", sr$dep, sr$depLo, sr$depHi, t0)
  acc <- sensor_series_ok(rows, third, sr$acc, sr$accLo, sr$accHi, t0)
  c(sensor_days = dep[1] && acc[1], sensor_values = dep[2] && acc[2])
}

# Two logicals for one card series: its days, and its values.
sensor_series_ok <- function(rows, cl, med, lo, hi, t0) {
  s <- if (is.null(rows)) NULL else rows[rows$cls == cl]
  s_n <- if (is.null(s)) 0L else nrow(s)
  shown <- which(!is.na(med))
  if (length(shown) != s_n) return(c(FALSE, FALSE))
  if (!s_n) return(c(TRUE, TRUE))
  i <- s$day - t0 + 1L
  if (any(i < 1L | i > length(med)) || !setequal(i, shown)) return(c(FALSE, FALSE))
  gap <- abs(c(med[i] - s$med, lo[i] - s$lo, hi[i] - s$hi))
  c(TRUE, all(!is.na(gap)) && max(gap) <= 0.001)
}

# The band, the candidate date (in the header and as the red line) and the
# votes of every rule stream.
band_checks <- function(p, cb, t0) {
  id <- p$animal$id
  b <- cb$bands[cb$bands$animal_id == id, , drop = FALSE]
  if (nrow(b) != 1) return(c(band = FALSE, candidate_date = FALSE, votes = FALSE))
  dd <- as.Date(b$death_date)
  flag <- if (is.null(p$marks$flag)) NA_integer_ else as.integer(p$marks$flag)
  v <- cb$votes[cb$votes$animal_id == id, , drop = FALSE]
  want <- paste(v$rule, v$vote, ifelse(is.na(v$date), "", format(as.Date(v$date))))
  pv <- p$votes
  shown <- character()
  if (NROW(pv)) {
    pdate <- if (is.null(pv$date)) rep("", NROW(pv)) else ifelse(is.na(pv$date), "", pv$date)
    shown <- paste(pv$rule, pv$vote, pdate)
  }
  c(band = identical(p$band$band, b$band),
    candidate_date = identical(flag, as.integer(dd) - t0) &&
      same_text(p$band$death_date, if (is.na(dd)) NA else format(dd)),
    votes = identical(sort(shown), sort(want)))
}

# Release, expiry, the days from the last detection to the data horizon, and
# species, from the animals table.
animal_checks <- function(p, truth, pd) {
  a <- p$animal
  ani <- truth$animals
  rows <- if (is.null(ani)) NULL else ani[!is.na(ani$animal_id) & ani$animal_id == a$id, ,
                                          drop = FALSE]
  first_val <- function(cl) {
    if (is.null(rows) || !cl %in% names(rows)) return(NA)
    v <- rows[[cl]][!is.na(rows[[cl]])]
    if (length(v)) v[1] else NA
  }
  rel <- first_val("release_datetime")
  rel <- if (is.na(rel)) as.Date(NA) else as.Date(as.POSIXct(rel, tz = "UTC"), tz = "UTC")
  life <- suppressWarnings(as.numeric(first_val("tag_life_days")))
  expiry <- rel + round(life)
  c(release = same_text(a$release, if (is.na(rel)) NA else format(rel)),
    expiry = same_text(a$expiry, if (is.na(expiry)) NA else format(expiry)),
    to_horizon = isTRUE(a$to_horizon == truth$horizon - max(pd$day)),
    species = same_text(a$species, first_val("species")))
}

# Two values are the same text; NULL and NA are the same absence.
same_text <- function(a, b) {
  absent <- function(v) is.null(v) || length(v) == 0 || is.na(v[1])
  if (absent(a) || absent(b)) return(absent(a) && absent(b))
  identical(as.character(a[1]), as.character(b[1]))
}
