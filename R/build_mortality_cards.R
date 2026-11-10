# ---- Review cards and the review index ----

#' Write the mortality review cards and their index
#'
#' Writes one self-contained HTML card per animal, and an index page, into
#' `dir`. A card shows the animal's facts (species, size, tag, release, last
#' detection, the candidate death date and its basis, the deciding rule and
#' every rule stream's vote), a map of the station positions that heard it
#' with the trail of its daily detection-weighted centroid, and time panels
#' on one axis: detections per day by station (one row per `station_id`;
#' a station whose deployments moved has one row and several positions on
#' the map), detections per day, apparent
#' movement, depth, acceleration (or the sensor the tag carries), one evidence
#' lane per rule stream and the reviewer's day notes. A day scrubber with
#' play, speed and a trailing window drives the map. The reviewer calls each
#' animal alive, dead_or_shed or uncertain, sets or types a death date and
#' writes a note; calls are kept in the browser, keyed by `animal_id`, and
#' exported as one CSV for [apply_mortality_calls()].
#'
#' Size is any length, weight or mass column of the animals table, shown
#' under the column's name. The table contract carries no unit for size, so
#' the card prints a unit only when the animals table states one: a unit
#' suffix on the column name (`length_mm` gives "length 400 mm") or a column
#' named after it with `_unit` (`length` and `length_unit`). Otherwise the
#' value is followed by "(unit as recorded)", the unit of the file it was read
#' from.
#'
#' The index groups the animals by band, with progress per band, a search
#' box, a sidebar that follows the scroll, and buttons that export and import
#' every call. Inside the review band, animals are ordered by the persistence
#' score of the bands when it is present, highest first.
#'
#' The cards and the index load nothing from the network. After the cards are
#' written, each card file is read back from disk and its numbers are compared
#' with the detset, the animals table and the bands by code that shares
#' nothing with the card writer. Each of these must be equal: detections and
#' detection days, the detections of every day, the first and last day, the
#' stations and the detections of every day at each station, the list of
#' stations, the located detections of every day at each position, the depth
#' and sensor days with their daily median and 5th and 95th percentiles, the
#' band, the candidate date, the votes, release, expiry, the days from the
#' last detection to the data horizon, and species. A card that disagrees is
#' kept, marked `checked = FALSE` with the quantities in `failed`, and named
#' in a warning.
#'
#' A card's time axis starts at the release, or at the first day of the
#' detset when the release is earlier, so a card built from one month of a
#' long study does not draw years with no data. The cursor opens on the
#' animal's first day with a detection, so the first map shows where it was
#' heard; an animal first heard weeks into the detset opened on an empty
#' map. The map opens on the
#' stations that heard the animal when they fill less than a quarter of the
#' array's extent; a button switches to the whole array. The D key, or the dead/shed
#' button, stamps the candidate death date when no date is set, never the day
#' in view. The candidate is the first detection of the final run; where
#' the site stopped listening before the run, its basis gives the detection
#' before the run as the earliest the death could be (see
#' [detect_mortality()]). Where the sensor panels show a
#' later death, set the date from them. With no candidate, the call is saved
#' without a date and the card
#' stays open and asks for one: move the cursor to the death day and press X
#' (or "= cursor"), or type the date. X moves a date already set.
#'
#' @param x The `detset` the bands were computed from.
#' @param bands The result of [detect_mortality()].
#' @param dir Folder for `index.html` and the `cards` folder. It is created
#'   if needed; existing cards in it are replaced.
#' @param which Bands that get a card. The index lists every animal.
#' @param all If `TRUE`, every band gets a card, as `which = c("dead",
#'   "review", "alive")` does.
#' @param control A list of options: `title` for the index; `store`, the key
#'   of the browser store, which keeps calls apart between studies (the
#'   default is built from a hash of the full path of `dir`, so two folders
#'   named `review` in different studies do not share calls, and a rebuild in
#'   the same folder keeps them); `outline`, an sf polygon in WGS 84 drawn
#'   under the stations (needs the sf package); `cores`, the number of
#'   processes that write cards (the default is the option `detqc.cores`, or
#'   1; above 1 the cards are written by [parallel::mclapply()], which runs one
#'   process on Windows).
#'
#' @return `x`, invisibly, with one log record and `x$cards`: a data frame
#'   with one row per card (`animal_id`, `band`, `file`, `bytes`, `checked`
#'   and `failed`), whose attribute `index` holds the path of `index.html`.
#' @seealso [review_mortality()], [apply_mortality_calls()]
#' @export
#' @examples
#' study <- example_mortality
#' dets <- detset(study$detections, study$deployments, study$animals)
#' mort_rules <- mortality_rules(min_days = 3, min_calendar_days = 3)
#' bands <- detect_mortality(dets, rules = mort_rules)
#' dets <- build_mortality_cards(dets, bands, file.path(tempdir(), "review"))
#' dets$cards[, c("animal_id", "band", "bytes", "checked")]
build_mortality_cards <- function(x, bands, dir, which = c("dead", "review"), all = FALSE,
                                  control = list()) {
  validate_detset(x)
  cb <- card_bands(bands)
  # The bands are not part of the detset, so their own log record is written
  # here, where they meet it again (log_mortality_bands(), rules module).
  x <- log_mortality_bands(x, bands)
  bands_all <- c("dead", "review", "alive")
  which <- if (isTRUE(all)) bands_all else check_choice(which, bands_all, "which", TRUE)
  ctl <- card_control(control, dir)
  ctx <- card_context(x, cb, ctl)
  b <- ctx$bands
  b$card <- b$band %in% which
  card_dir <- file.path(dir, "cards")
  dir.create(card_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(card_dir, pattern = "\\.html$", full.names = TRUE))
  nav <- b[b$card, , drop = FALSE]
  res <- write_cards(nav, ctx, card_dir, ctl$cores)
  ok <- card_readback(res$file, x, cb)
  res$checked <- ok$checked
  res$failed <- ok$failed
  index <- file.path(dir, "index.html")
  ip <- index_payload(ctx, b, ctl)
  ip$detset_key <- detset_key(x)
  ip$cards_for <- I(which)
  check_index(ip, x)
  fill_template(read_template("index.html"), ip, ctl$title, index)
  warn_failed_cards(res)
  attr(res, "index") <- index
  before <- c(nrow(x$detections), count_animals(x$detections))
  x$cards <- res
  invisible(log_step(x, "build_mortality_cards",
                     list(which = which, dir = basename(normalizePath(dir))), before, 0L,
                     card_reason(res, b)))
}

# Columns named inside data.table calls in the card and store files.
utils::globalVariables(c(
  "animal_id", "transmitter", "datetime", "station_id", "date", "class", "scale", "value",
  "unit", "own_station_id", "lat", "lon", "idx", "i.idx", "cx", "cy", "n", "di", "rule",
  "vote", "death", "lat0", "lon0", "km", "a", "day", "st", "tx", "val", "raw", "u", "ty",
  "aty", "au", "slope", "icpt", "cls", "la", "lo", "s", "e", "k", "i.i", "x.r", "x.s", "v",
  "sidx", "i.sidx"
))

card_reason <- function(res, b) {
  per <- table(factor(res$band, c("dead", "review", "alive")))
  sprintf(paste("%s written (dead %s, review %s, alive %s) for %s in the bands",
                "(dead %s, review %s, alive %s); read back from their HTML equal to the",
                "detset, the animals table and the bands on %s each (%s): %s of %s; the",
                "index lists %s"),
          n_of(nrow(res), "card"), fmt_n(per[["dead"]]), fmt_n(per[["review"]]),
          fmt_n(per[["alive"]]), n_of(nrow(b), "animal"), fmt_n(sum(b$band == "dead")),
          fmt_n(sum(b$band == "review")), fmt_n(sum(b$band == "alive")),
          n_of(length(card_quantities), "quantity", "quantities"),
          gsub("_", " ", paste(card_quantities, collapse = ", ")), fmt_n(sum(res$checked)),
          n_of(nrow(res), "card"), n_of(nrow(b), "animal"))
}

# A key of the detections a review was built from: rows, animals and the last
# detection. review_mortality() rebuilds when it changes.
detset_key <- function(x) {
  d <- x$detections
  ids <- sort(unique(stats::na.omit(as.character(d$animal_id))))
  last <- if (nrow(d)) format(max(d$datetime, na.rm = TRUE), "%Y-%m-%d %H:%M:%S") else ""
  hash_text(paste(nrow(d), last, paste(ids, collapse = ","), sep = "|"))
}

# ---- Internal: options ----
card_control <- function(control, dir) {
  if (!is.list(control)) stop_detqc("`control` must be a list.")
  bad <- setdiff(names(control), c("title", "store", "outline", "cores"))
  if (length(bad)) stop_detqc("`control` has no option `%s`.", bad[1])
  if (!is.character(dir) || length(dir) != 1) stop_detqc("`dir` must be one folder path.")
  # The key holds the full folder path, so lakeA/review and lakeB/review
  # keep their calls apart. The animal set is left out of the key so that
  # calls survive a rebuild after new detections arrive.
  path <- normalizePath(dir, mustWork = FALSE)
  key <- paste0(gsub("[^A-Za-z0-9_]+", "_", basename(path)), "_", hash_text(path))
  list(
    title = if (is.null(control$title)) "Mortality review" else as.character(control$title),
    store = paste0("detqc_calls_", if (is.null(control$store)) key else control$store),
    outline = control$outline, cores = card_cores(control$cores)
  )
}

card_cores <- function(cores) {
  if (is.null(cores)) cores <- getOption("detqc.cores", 1L)
  if (!is.numeric(cores) || length(cores) != 1 || is.na(cores) || cores < 1) {
    stop_detqc("`control$cores` must be one number of 1 or more.")
  }
  as.integer(cores)
}

# ---- Internal: the bands, as the cards read them ----
# The cards read four columns of `bands$bands` and the `votes` table; every
# other column is optional, so a new rule stream reaches the cards as a new
# lane without a change here.
card_bands <- function(bands) {
  if (!inherits(bands, "mortality_bands")) stop_detqc("`bands` must come from detect_mortality().")
  b <- as.data.frame(bands$bands, stringsAsFactors = FALSE)
  need <- c("animal_id", "band", "deciding_rule", "death_date")
  miss <- setdiff(need, names(b))
  if (length(miss)) stop_detqc("`bands$bands` has no column `%s`.", miss[1])
  # A candidate from the final run is the time of a detection, and a call
  # that takes it keeps the time. A candidate at 00:00:00 UTC is a day, such
  # as the accelerometer's day after the last active day, and keeps no time.
  b$death_time <- if (inherits(b$death_date, "POSIXct")) b$death_date else as.POSIXct(NA)
  b$death_time[!is.na(b$death_time) &
                 format(b$death_time, "%H:%M:%S", tz = "UTC") == "00:00:00"] <- NA
  b$death_date <- as.Date(b$death_date)
  score <- intersect(c("persistence", "persistence_score"), names(b))
  b$persistence <- if (length(score)) as.numeric(b[[score[1]]]) else NA_real_
  b$basis <- NA_character_
  if ("death_date_basis" %in% names(b)) b$basis <- as.character(b$death_date_basis)
  if (!"rule_label" %in% names(b)) b$rule_label <- NA_character_
  v <- bands$votes
  if (is.null(v) || !nrow(v)) {
    v <- data.frame(animal_id = character(), rule = character(), vote = character(),
                    date = as.Date(character()), detail = character(), stringsAsFactors = FALSE)
  }
  v <- as.data.frame(v, stringsAsFactors = FALSE)
  na <- rep(NA_character_, nrow(v))
  v$date <- if ("date" %in% names(v)) as.Date(v$date) else as.Date(na)
  if (!"detail" %in% names(v)) v$detail <- na
  if (!"basis" %in% names(v)) v$basis <- na
  unloc <- bands$settings$unlocated_animals
  list(bands = b, votes = v, unlocated = if (is.null(unloc)) NA_integer_ else as.integer(unloc))
}

# Review order: the dead band, the review band by persistence (highest
# first, as the review band is ranked by it), then the alive band.
order_bands <- function(b) {
  bo <- match(b$band, c("dead", "review", "alive"))
  p <- ifelse(b$band == "review" & !is.na(b$persistence), -b$persistence, Inf)
  b <- b[order(bo, p, b$animal_id), , drop = FALSE]
  b$rank <- stats::ave(seq_len(nrow(b)), b$band, FUN = seq_along)
  rownames(b) <- NULL
  b
}

# ---- Internal: shared context ----
card_context <- function(x, cb, ctl) {
  pos <- card_positions(x)
  all_det <- as_dt(x$detections)[!is.na(animal_id), list(animal_id, transmitter, datetime,
                                                         station_id)]
  all_det[, date := as.Date(datetime)]
  r <- sensor_readings(x)
  sens <- r[!is.na(animal_id) & scale == "calibrated",
            list(med = stats::median(value), lo = q(value, 0.05), hi = q(value, 0.95),
                 unit = unit[1]),
            by = list(animal_id, class, date)]
  b <- order_bands(cb$bands)
  b$slug <- make.unique(gsub("[^A-Za-z0-9_.-]+", "_", b$animal_id), sep = "_")
  b$species <- species_of(x$animals, b$animal_id)
  data.table::setkeyv(all_det, "animal_id")
  data.table::setkeyv(pos$det, "animal_id")
  data.table::setkeyv(sens, "animal_id")
  data.table::setkeyv(r, "animal_id")
  list(det = pos$det, all_det = all_det, readings = r, sens = sens, stations = pos$stations,
       sids = pos$sids,
       bands = b, votes = as_dt(cb$votes), unlocated_animals = cb$unlocated,
       animals = x$animals, horizon = max(all_det$date), data_start = min(all_det$date),
       lake = lake_geometry(pos$stations, ctl$outline),
       source = sprintf("detqc %s; detset of %s", utils::packageVersion("detqc"),
                        n_of(nrow(x$detections), "detection")),
       store = ctl$store)
}

species_of <- function(animals, ids) {
  if (is.null(animals) || !"species" %in% names(animals)) return(rep(NA_character_, length(ids)))
  sp <- animals[!is.na(animals$species), c("animal_id", "species")]
  sp$species[match(ids, sp$animal_id)]
}

# Each detection takes the position of the deployment that heard it, so a
# station that moved is placed where it was at the time; a detection with no
# deployment falls back to its own deploy_lat and deploy_lon, as in OTN
# extracts. Positions shared by two station ids are one point on the map.
card_positions <- function(x) {
  d <- x$detections
  dep <- x$deployments
  row <- match_deployment(d, dep)
  lat <- dep$deploy_lat[row]
  lon <- dep$deploy_lon[row]
  if (all(c("deploy_lat", "deploy_lon") %in% names(d))) {
    lat <- data.table::fifelse(is.na(lat), as.numeric(d$deploy_lat), lat)
    lon <- data.table::fifelse(is.na(lon), as.numeric(d$deploy_lon), lon)
  }
  det <- data.table::data.table(animal_id = d$animal_id, datetime = d$datetime,
                                own_station_id = d$station_id, lat = lat, lon = lon)
  det <- det[!is.na(animal_id) & !is.na(lat) & !is.na(lon)]
  det[, date := as.Date(datetime)]
  st <- unique(data.table::rbindlist(list(
    data.table::data.table(station_id = dep$station_id, lat = dep$deploy_lat, lon = dep$deploy_lon),
    det[, list(station_id = own_station_id, lat, lon)]
  )))
  st <- st[!is.na(lat) & !is.na(lon)]
  sids <- station_ids(x, st)
  st <- unique(st, by = c("lat", "lon"))
  data.table::setorderv(st, c("lat", "lon"))
  st[, idx := seq_len(.N) - 1L]
  det[st, on = c("lat", "lon"), idx := i.idx]
  list(det = det, stations = st, sids = sids)
}

# The stations by station_id, the unit of the station panel and of every
# station count on a card. A station whose deployments moved has one row, at
# the mean latitude of its positions; a station with no position sorts last.
station_ids <- function(x, st) {
  ids <- c(as.character(x$deployments$station_id), as.character(x$detections$station_id))
  ids <- sort(unique(ids[!is.na(ids)]))
  lat <- tapply(st$lat, as.character(st$station_id), mean)
  data.table::data.table(station_id = ids, lat = unname(lat[ids]), sidx = seq_along(ids) - 1L)
}

q <- function(v, p) unname(stats::quantile(v, p, names = FALSE, type = 7))

lake_geometry <- function(st, outline) {
  rings <- list()
  if (!is.null(outline)) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop_detqc("`control$outline` needs the sf package.")
    }
    xy <- as.data.frame(sf::st_coordinates(sf::st_transform(sf::st_geometry(outline), 4326)))
    grp <- interaction(xy[, intersect(c("L1", "L2", "L3"), names(xy))], drop = TRUE)
    rings <- lapply(split(xy, grp), function(r) {
      list(x = I(round(r$X, 5)), y = I(round(r$Y, 5)))
    })
  }
  lon <- c(st$lon, unlist(lapply(rings, `[[`, "x")))
  lat <- c(st$lat, unlist(lapply(rings, `[[`, "y")))
  pad <- max(diff(range(lon)), diff(range(lat)), 0.002) * 0.04
  bbox <- c(min(lon) - pad, min(lat) - pad, max(lon) + pad, max(lat) + pad)
  width_km <- haversine_km(mean(lat), bbox[1], mean(lat), bbox[3])
  nice <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500)
  list(bbox = I(bbox), rings = unname(rings),
       scale_km = nice[max(1, findInterval(width_km / 5, nice))])
}

# ---- Internal: writing the cards ----
# One process per card when `cores` is above 1: the forked processes share
# the context without a copy, and each process writes its own file.
write_cards <- function(nav, ctx, card_dir, cores) {
  empty <- data.frame(animal_id = character(), band = character(), file = character(),
                      bytes = numeric(), stringsAsFactors = FALSE)
  if (!nrow(nav)) return(empty)
  tpl <- read_template("profile.html")
  one <- function(k) {
    p <- card_payload(nav$animal_id[k], ctx, nav, k)
    file <- file.path(card_dir, paste0(nav$slug[k], ".html"))
    fill_template(tpl, p, paste("Mortality review:", nav$animal_id[k]), file)
    data.frame(animal_id = nav$animal_id[k], band = nav$band[k], file = file,
               bytes = file.size(file), stringsAsFactors = FALSE)
  }
  ks <- seq_len(nrow(nav))
  rows <- if (cores > 1L && .Platform$OS.type != "windows") {
    parallel::mclapply(ks, one, mc.cores = cores, mc.preschedule = TRUE)
  } else {
    lapply(ks, one)
  }
  bad <- vapply(rows, inherits, logical(1), what = "try-error")
  if (any(bad)) {
    k <- which(bad)[1]
    stop_detqc("Card %s failed: %s", nav$animal_id[k], rows[[k]])
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ---- Internal: the payload of one card ----
# The rows of one animal, by binary search on the key set in card_context().
# The join table is one variable, which data.table reads in this frame: in
# `dt[animal_id == id]`, a column named `id` (a VDAT file's DET field ID)
# would stand in for the argument and select no row.
of_animal <- function(dt, id) {
  wanted <- data.table::data.table(animal_id = id)
  dt[wanted, on = "animal_id", nomatch = NULL]
}

card_payload <- function(id, ctx, nav, k) {
  b <- nav[k, ]
  det <- of_animal(ctx$det, id)
  all_det <- of_animal(ctx$all_det, id)
  ani <- animal_facts(id, ctx, all_det)
  # The axis starts at the release, or at the first day of the detset when
  # the release is earlier: days before the detset hold no data, and drawn
  # they would read as days with 0 detections.
  start <- max(ani$release_date, ctx$data_start, na.rm = TRUE)
  t0 <- min(c(all_det$date, b$death_date, start), na.rm = TRUE)
  nd <- as.integer(ctx$horizon - t0) + 1L
  ix <- function(d) {
    i <- as.integer(as.Date(d) - t0)
    i[is.na(i) | i < 0 | i > nd - 1L] <- NA_integer_
    i
  }
  sens <- of_animal(ctx$sens, id)
  list(
    t0 = format(t0), nd = nd, source = ctx$source, store = ctx$store,
    dmax = max(c(1L, tabulate_days(all_det$date, ix, nd))),
    animal = ani$facts, band = band_facts(b, ctx), votes = vote_rows(id, ctx),
    has = sensor_presence(sens, of_animal(ctx$readings, id)),
    series = card_series(det, all_det, sens, ix, nd),
    ds = I(day_station_triples(det, ix)), sids = I(ctx$sids$station_id),
    dss = I(day_sid_triples(all_det, ctx$sids, ix)), heard = I(heard_order(all_det, ctx$sids)),
    streams = card_lanes(id, ctx, ix),
    stations = lapply(seq_len(nrow(ctx$stations)), function(j) {
      list(id = ctx$stations$station_id[j], lon = round(ctx$stations$lon[j], 6),
           lat = round(ctx$stations$lat[j], 6))
    }),
    marks = list(flag = if (is.na(b$death_date)) NULL else ix(b$death_date),
                 first = if (nrow(all_det)) ix(min(all_det$date)) else NULL,
                 horizon = ix(ctx$horizon), horizon_date = format(ctx$horizon),
                 expiry = if (is.na(ani$expiry)) NULL else ix(ani$expiry),
                 release = if (is.na(ani$release_date)) NULL else ix(ani$release_date),
                 release_before = !is.na(ani$release_date) && ani$release_date < t0,
                 data_start = format(ctx$data_start)),
    nav = card_nav(nav, k), lake = ctx$lake, extent = animal_extent(det, ctx$lake)
  )
}

# The map opens on the animal's own stations when they fill less than a
# quarter of the array's extent, as on a network-wide GLATOS receiver file:
# an animal heard at a few stations close together would collapse to one bubble
# on a map of the whole network. The button under the map switches to the whole array.
animal_extent <- function(det, lake) {
  whole <- as.numeric(lake$bbox)
  if (!nrow(det)) return(list(animal = I(whole), open = "whole"))
  lon <- range(det$lon)
  lat <- range(det$lat)
  pad <- max(diff(lon), diff(lat), 0.01) * 0.25
  bb <- c(lon[1] - pad, lat[1] - pad, lon[2] + pad, lat[2] + pad)
  span <- function(b) sqrt(((b[3] - b[1]) * cos(mean(b[c(2, 4)]) * pi / 180))^2 + (b[4] - b[2])^2)
  list(animal = I(round(bb, 6)), open = if (span(bb) < 0.25 * span(whole)) "animal" else "whole")
}

card_nav <- function(nav, k) {
  list(pos = k, n = nrow(nav), prev_slug = if (k > 1) nav$slug[k - 1] else NULL,
       next_slug = if (k < nrow(nav)) nav$slug[k + 1] else NULL,
       order = lapply(seq_len(nrow(nav)), function(j) {
         list(s = nav$slug[j], id = nav$animal_id[j], sp = nav$species[j], b = nav$band[j],
              c = if (is.na(nav$death_date[j])) "" else format(nav$death_date[j]))
       }))
}

tabulate_days <- function(dates, ix, nd) {
  i <- ix(dates)
  tabulate(i[!is.na(i)] + 1L, nbins = nd)
}

# The third sensor panel shows acceleration, or the sensor the tag carries
# when it has no accelerometer, such as temperature.
sensor_presence <- function(sens, readings) {
  other <- setdiff(unique(sens$class), c("depth", "acceleration", "unknown"))
  raw <- readings[scale == "raw"]
  list(depth = any(sens$class == "depth"), accel = any(sens$class == "acceleration"),
       other = if (length(other)) sort(other)[1] else NULL,
       other_unit = if (length(other)) sens$unit[sens$class == sort(other)[1]][1] else NULL,
       raw_readings = nrow(raw),
       raw_classes = if (nrow(raw)) paste(sort(unique(raw$class)), collapse = ", ") else NULL)
}

card_series <- function(det, all_det, sens, ix, nd) {
  blank <- rep(NA_real_, nd)
  chan <- function(cl, col) {
    s <- sens[class == cl]
    v <- blank
    i <- ix(s$date)
    v[i[!is.na(i)] + 1L] <- round(s[[col]][!is.na(i)], 3)
    I(v)
  }
  third <- setdiff(sort(unique(sens$class)), c("depth", "unknown"))[1]
  if (any(sens$class == "acceleration")) third <- "acceleration"
  # The detection-weighted mean station position of each day:
  # sum(detections * coordinate) / sum(detections).
  cen <- det[, list(cx = mean(lon), cy = mean(lat)), by = "date"]
  data.table::setorderv(cen, "date")
  cx <- blank
  cy <- blank
  i <- ix(cen$date)
  cx[i[!is.na(i)] + 1L] <- round(cen$cx[!is.na(i)], 6)
  cy[i[!is.na(i)] + 1L] <- round(cen$cy[!is.na(i)], 6)
  step <- blank
  if (nrow(cen) > 1) {
    km <- haversine_km(cen$cy[-nrow(cen)], cen$cx[-nrow(cen)], cen$cy[-1], cen$cx[-1])
    j <- ix(cen$date[-1])
    step[j[!is.na(j)] + 1L] <- round(km[!is.na(j)], 3)
  }
  list(dets = I(tabulate_days(all_det$date, ix, nd)),
       dep = chan("depth", "med"), depLo = chan("depth", "lo"), depHi = chan("depth", "hi"),
       acc = chan(third, "med"), accLo = chan(third, "lo"), accHi = chan(third, "hi"),
       cx = I(cx), cy = I(cy), step = I(step))
}

day_station_triples <- function(det, ix) {
  if (!nrow(det)) return(integer())
  ds <- det[, list(n = .N), by = list(date, idx)]
  ds[, di := ix(date)]
  ds <- ds[!is.na(di)]
  data.table::setorderv(ds, c("di", "idx"))
  as.integer(t(as.matrix(ds[, list(di, idx, n)])))
}

# Day, station and count of the station panel, keyed by station_id. Each
# detection counts at the station it names, located or not.
day_sid_triples <- function(all_det, sids, ix) {
  s <- all_det[!is.na(station_id), list(n = .N), by = list(date, station_id)]
  if (!nrow(s)) return(integer())
  s[sids, on = "station_id", sidx := i.sidx]
  s[, di := ix(date)]
  s <- s[!is.na(di) & !is.na(sidx)]
  data.table::setorderv(s, c("di", "sidx"))
  as.integer(t(as.matrix(s[, list(di, sidx, n)])))
}

# Rows of the station panel: the stations that heard the animal, north at
# the top, so a move along the lake reads as a move up or down the panel.
heard_order <- function(all_det, sids) {
  h <- sids[station_id %in% unique(stats::na.omit(all_det$station_id))]
  if (!nrow(h)) return(integer())
  data.table::setorderv(h, c("lat", "station_id"), order = c(-1L, 1L), na.last = TRUE)
  h$sidx
}

# ---- Internal: the facts of one card ----
animal_facts <- function(id, ctx, all_det) {
  ani <- animal_rows(ctx$animals, id)
  rel <- pick_first(ani, "release_datetime")
  rel_date <- as.Date(NA)
  if (!is.na(rel)) rel_date <- as.Date(as.POSIXct(rel, origin = "1970-01-01", tz = "UTC"))
  life <- suppressWarnings(as.numeric(pick_first(ani, "tag_life_days")))
  expiry <- if (is.na(life) || is.na(rel_date)) as.Date(NA) else rel_date + round(life)
  last <- max(all_det$date)
  facts <- list(
    id = id, slug = ctx$bands$slug[ctx$bands$animal_id == id],
    species = null_if_na(pick_first(ani, "species")), size = size_text(ani),
    transmitters = I(sort(unique(all_det$transmitter))),
    tag_model = null_if_na(pick_first(ani, "tag_model")), tag_life_days = null_if_na(life),
    expiry = null_if_na(format(expiry)), release = null_if_na(format(rel_date)),
    sensors = sensors_text(of_animal(ctx$readings, id)), first = format(min(all_det$date)),
    last = format(last), detection_days = length(unique(all_det$date)),
    span_days = as.integer(last - min(all_det$date)) + 1L,
    detections = nrow(all_det), stations = length(unique(stats::na.omit(all_det$station_id))),
    to_horizon = as.integer(ctx$horizon - last),
    to_expiry = if (is.na(expiry)) NULL else as.integer(expiry - last)
  )
  list(facts = facts, release_date = rel_date, expiry = expiry)
}

animal_rows <- function(animals, id) {
  if (is.null(animals)) return(NULL)
  animals[!is.na(animals$animal_id) & animals$animal_id == id, , drop = FALSE]
}

pick_first <- function(ani, col) {
  if (is.null(ani) || !col %in% names(ani)) return(NA)
  v <- ani[[col]][!is.na(ani[[col]])]
  if (length(v)) v[1] else NA
}

# Size is not in the table contract, so any length, weight or mass column of
# the animals table is shown under its own name. The unit is printed when the
# table states it: a unit suffix on the column name (length_mm) or a column
# `<name>_unit`. Otherwise the value is followed by "(unit as recorded)",
# because the card has no way to know the unit.
size_units <- c("mm", "cm", "m", "g", "kg")

size_text <- function(ani) {
  if (is.null(ani)) return(NULL)
  cols <- grep("length|weight|mass", names(ani), value = TRUE, ignore.case = TRUE)
  cols <- cols[!grepl("_type$|_unit$", cols)]
  vals <- vapply(cols, function(cl) {
    v <- pick_first(ani, cl)
    if (is.na(v)) return(NA_character_)
    suffix <- sub("^.*_", "", cl)
    unit <- if (grepl("_", cl) && suffix %in% size_units) {
      suffix
    } else {
      pick_first(ani, paste0(cl, "_unit"))
    }
    name <- if (grepl("_", cl) && suffix %in% size_units) sub("_[^_]*$", "", cl) else cl
    paste(c(name, format(v), if (is.na(unit)) "(unit as recorded)" else unit), collapse = " ")
  }, character(1))
  vals <- vals[!is.na(vals)]
  if (length(vals)) paste(vals, collapse = ", ") else NULL
}

# The rule streams call the run of detections at the end of a record the
# final run, with the stream id in brackets after a date's basis. A card is
# read by a reviewer, so it drops the id.
card_words <- function(txt) {
  if (is.null(txt) || all(is.na(txt))) return(txt)
  sub(" \\([a-z_]+\\)$", "", txt)
}

sensors_text <- function(rs) {
  if (!nrow(rs)) return("none")
  paste(unique(paste0(rs$class, ifelse(rs$scale == "raw", " (raw, not calibrated)", ""))),
        collapse = ", ")
}

# A time as the review pages write it, "2024-06-03T15:05:39Z", or "".
utc_stamp <- function(t) {
  if (length(t) != 1 || is.na(t)) return("")
  format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# JSON has no NA in the card's facts: a missing value is left out.
null_if_na <- function(v) if (length(v) != 1 || is.na(v)) NULL else v

rule_labels <- c(
  accel_flatline = "an accelerometer flatline",
  terminal_run = "a final stationary run with no alive vote",
  terminal_run_beside_activity = "a final stationary run beside an active accelerometer",
  terminal_run_split_site = "a final stationary run across a site boundary",
  depth_flatline = "a depth flatline",
  record_end = "a record that ends near tag life or the data horizon",
  movement_resumption = "movement after the candidate date",
  persistence = "a stay longer than the species' usual",
  no_position = "a record with no station position",
  sparse_record = "a record too sparse to score",
  none = "no death evidence"
)

stream_labels <- c(
  terminal_run = "final run", accel_flatline = "accelerometer", depth_flatline = "depth",
  record_end = "record end", movement_resumption = "movement", persistence = "persistence",
  site_listening = "site listening", position_radius = "position radius"
)

label_of <- function(key, dict) {
  out <- unname(dict[key])
  out[is.na(out)] <- gsub("_", " ", key[is.na(out)])
  out
}

band_facts <- function(b, ctx) {
  dd <- if (is.na(b$death_date)) NULL else format(b$death_date)
  basis <- b$basis
  if (is.na(basis) && !is.null(dd)) {
    v <- ctx$votes[animal_id == b$animal_id & !is.na(date) & date == b$death_date]
    basis <- if (nrow(v)) {
      sprintf("the date of the %s vote", label_of(v$rule[1], stream_labels))
    } else {
      "the date the bands give"
    }
  }
  rank <- if (b$band == "review" && !is.na(b$persistence)) b$rank else NULL
  list(band = b$band, rule = b$deciding_rule,
       rule_label = card_words(rule_label_of(b)),
       death_date = dd, death_time = utc_stamp(b$death_time),
       basis = if (is.na(basis)) NULL else card_words(basis),
       persistence = null_if_na(b$persistence), rank = rank,
       of = if (is.null(rank)) NULL else sum(ctx$bands$band == "review"))
}

# The label the bands give the deciding rule, or the cards' own.
rule_label_of <- function(b) {
  if (is.na(b$rule_label)) label_of(b$deciding_rule, rule_labels) else b$rule_label
}

vote_rows <- function(id, ctx) {
  v <- ctx$votes[animal_id == id]
  lapply(seq_len(nrow(v)), function(k) {
    why <- c(v$basis[k], v$detail[k])
    # A basis the detail already states goes, so the cell names it once.
    if (!is.na(why[1]) && !is.na(why[2]) &&
          grepl(sub("^the ", "", why[1]), why[2], fixed = TRUE)) {
      why <- why[2]
    }
    why <- why[!is.na(why) & nzchar(why)]
    list(rule = v$rule[k], label = label_of(v$rule[k], stream_labels), vote = v$vote[k],
         date = if (is.na(v$date[k])) NULL else format(v$date[k]),
         detail = if (length(why)) card_words(paste(unique(why), collapse = "; ")) else NULL)
  })
}

# One lane per rule stream that voted on any animal, so every card has the
# same lanes in the same order.
card_lanes <- function(id, ctx, ix) {
  rules <- sort(unique(ctx$votes$rule))
  v <- ctx$votes[animal_id == id]
  lapply(rules, function(r) {
    row <- v[rule == r]
    vote <- if (nrow(row)) row$vote[1] else "unavailable"
    d <- if (nrow(row) && !is.na(row$date[1])) ix(row$date[1]) else NA_integer_
    list(label = label_of(r, stream_labels), vote = vote,
         state = switch(vote, dead = "dead vote", alive = "alive vote", review = "review vote",
                        none = "no vote", split = "stationary across a site boundary",
                        unavailable = "not available", vote),
         day = if (is.na(d)) NULL else d)
  })
}

# ---- Internal: the index payload ----
index_lede <- paste(
  "%s from the detset (%s). Bands: dead %s, review %s, alive %s (denominator: animals",
  "with at least one detection%s). %s a review card. The band is a triage from the rule",
  "streams; the call you make here or on a card is the verdict. A dead/shed call takes the",
  "candidate date unless you set another. Calls live in this browser under the key %s and",
  "survive a rebuild; export them to CSV for apply_mortality_calls()."
)

index_payload <- function(ctx, b, ctl) {
  ad <- ctx$all_det[, list(dets = .N, ddays = length(unique(date)), first = min(date),
                           last = max(date), stns = length(unique(stats::na.omit(station_id)))),
                    by = "animal_id"]
  sens <- ctx$readings[, list(sensor = paste(sort(unique(class)), collapse = ", ")),
                       by = "animal_id"]
  ad <- ad[match(b$animal_id, ad$animal_id)]
  sv <- sens$sensor[match(b$animal_id, sens$animal_id)]
  rows <- lapply(seq_len(nrow(b)), function(k) {
    list(id = b$animal_id[k], slug = b$slug[k], sp = b$species[k], b = b$band[k],
         rank = b$rank[k], pers = null_if_na(b$persistence[k]),
         rule = rule_label_of(b[k, ]), card = b$card[k],
         date = if (is.na(b$death_date[k])) "" else format(b$death_date[k]),
         time = utc_stamp(b$death_time[k]),
         ddays = ad$ddays[k], span = as.integer(ad$last[k] - ad$first[k]) + 1L, dets = ad$dets[k],
         stns = ad$stns[k], sensor = if (is.na(sv[k])) "none" else sv[k],
         first = format(ad$first[k]), last = format(ad$last[k]))
  })
  lab <- c(dead = "Dead band", review = "Review band", alive = "Alive band")
  bands <- lapply(names(lab), function(k) list(k = k, label = unname(lab[k]), n = sum(b$band == k)))
  unloc <- ""
  if (!is.na(ctx$unlocated_animals)) {
    unloc <- sprintf("; %s of them %s no located detection", fmt_n(ctx$unlocated_animals),
                     verb(ctx$unlocated_animals, "has", "have"))
  }
  list(
    title = ctl$title, store = ctx$store, rows = rows, bands = bands,
    ranked = any(!is.na(b$persistence[b$band == "review"])),
    lede = sprintf(index_lede, n_of(nrow(b), "animal"), ctx$source,
                   fmt_n(sum(b$band == "dead")), fmt_n(sum(b$band == "review")),
                   fmt_n(sum(b$band == "alive")), unloc,
                   paste(fmt_n(sum(b$card)), verb(sum(b$card), "has", "have")), ctx$store),
    # Only streams that cast a dead, alive or review vote on some animal: a
    # stream that votes on no animal is left out.
    streams = I(label_of(sort(unique(ctx$votes$rule[ctx$votes$vote %in% c("dead", "alive",
                                                                          "review")])),
                         stream_labels)),
    built = sprintf("Built %s UTC by detqc %s.", format(lubridate::now("UTC"), "%Y-%m-%d %H:%M"),
                    utils::packageVersion("detqc"))
  )
}

# ---- Internal: templates ----
read_template <- function(name) {
  path <- system.file("templates", name, package = "detqc")
  if (!nzchar(path)) stop_detqc("Template %s is missing from the detqc installation.", name)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# The payload goes into a JSON script block. Every "<" is written as <
# so no value, such as a note, can close the block.
fill_template <- function(tpl, payload, title, file) {
  json <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null", null = "null",
                                        digits = NA))
  json <- gsub("<", "\\u003c", json, fixed = TRUE)
  html_title <- gsub("<", "&lt;", gsub("&", "&amp;", title, fixed = TRUE), fixed = TRUE)
  parts <- strsplit(tpl, "__PAYLOAD__", fixed = TRUE)[[1]]
  if (length(parts) != 2) stop_detqc("The template must hold one __PAYLOAD__ placeholder.")
  html <- paste0(parts[1], json, parts[2])
  parts <- strsplit(html, "__TITLE__", fixed = TRUE)[[1]]
  html <- paste(parts, collapse = html_title)
  con <- file(file, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(html, con, useBytes = TRUE)
  invisible(file)
}

warn_failed_cards <- function(res) {
  bad <- !res$checked
  if (any(bad)) {
    warn_detqc("%s of %s %s with the detset, such as %s on %s.", fmt_n(sum(bad)),
               n_of(nrow(res), "card"), verb(sum(bad), "disagrees", "disagree"),
               res$animal_id[bad][1], res$failed[bad][1])
  }
  invisible(res)
}

# The index must list every animal with a detection, located or not.
check_index <- function(ip, x) {
  want <- length(unique(stats::na.omit(x$detections$animal_id)))
  if (length(ip$rows) != want) {
    stop_detqc("The index lists %s; the detset holds %s with a detection.",
               n_of(length(ip$rows), "animal"), fmt_n(want))
  }
  invisible(TRUE)
}
