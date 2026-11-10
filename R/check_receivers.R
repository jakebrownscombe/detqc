# ---- Receiver and deployment checks ----
# Each check takes the context of check_context() and returns check_result()
# or not_run(). Deployment rows are returned with `deployment`, their row in
# the deployments table, so a user can find and fix them.

dep_rows <- function(ctx, idx, ...) {
  out <- ctx$dep[idx]
  data.table::setnames(out, ".dep", "deployment")
  extra <- list(...)
  for (nm in names(extra)) data.table::set(out, j = nm, value = extra[[nm]])
  out
}

det_rows <- function(ctx, idx, ...) {
  out <- ctx$det[idx]
  data.table::setnames(out, c(".row", ".dep"), c("row", "deployment"))
  out[, .serial := NULL]
  extra <- list(...)
  for (nm in names(extra)) data.table::set(out, j = nm, value = extra[[nm]])
  out
}

# ---- Positions ----
position_check <- function(ctx, label, note) {
  lab <- ctx$position
  idx <- which(lab %in% label)
  dep <- ctx$dep
  rows <- dep_rows(ctx, idx, problem = lab[idx])
  if (identical(label, "swapped")) {
    rows[, c("suggested_lat", "suggested_lon") := list(deploy_lon, deploy_lat)]
  }
  check_result(length(idx), nrow(dep), note, rows)
}

chk_coordinate_units <- function(ctx) {
  position_check(ctx, c("units", "missing"),
                 "positions missing or not in decimal degrees, as projected metres would be")
}

chk_swapped_lat_lon <- function(ctx) {
  position_check(ctx, "swapped", "swapping latitude and longitude brings each inside the extent")
}

chk_wrong_sign <- function(ctx) {
  position_check(ctx, "sign", "changing the sign of latitude or longitude brings each inside")
}

chk_outside_extent <- function(ctx) {
  ext <- ctx$settings$extent
  how <- if (is.null(ext)) {
    sprintf("further than max(%g km, %g x the median spread) from the median deployment",
            ctx$settings$outlier_km, ctx$settings$outlier_mult)
  } else {
    "outside settings$extent"
  }
  position_check(ctx, "outside", paste(how, "and not explained by a swap or a sign"))
}

# The water layer gives TRUE (water), FALSE (land) or NA (off the layer).
in_water <- function(water, lat, lon) {
  if (inherits(water, "SpatRaster")) {
    v <- raster_values(water, lat, lon)
    return(ifelse(is.na(v$cell), NA, !is.na(v$value)))
  }
  if (inherits(water, c("sf", "sfc"))) {
    if (!requireNamespace("sf", quietly = TRUE)) stop_detqc("The sf package is needed for `water`.")
    pts <- sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
    pts <- sf::st_transform(pts, sf::st_crs(water))
    return(lengths(sf::st_intersects(pts, sf::st_union(sf::st_geometry(water)))) > 0)
  }
  stop_detqc("`water` must be an sf polygon or a terra raster, not %s.", class(water)[1])
}

raster_values <- function(r, lat, lon) {
  if (!requireNamespace("terra", quietly = TRUE)) stop_detqc("The terra package is needed.")
  # terra::project() stops on an empty point set.
  if (!length(lat)) return(list(cell = integer(), value = numeric()))
  v <- terra::vect(data.frame(lon = lon, lat = lat), geom = c("lon", "lat"), crs = "EPSG:4326")
  v <- terra::project(v, terra::crs(r))
  cell <- terra::cellFromXY(r, terra::crds(v))
  value <- rep(NA_real_, length(cell))
  value[!is.na(cell)] <- terra::values(r)[cell[!is.na(cell)], 1]
  list(cell = cell, value = value)
}

chk_on_land <- function(ctx) {
  if (is.null(ctx$water)) return(not_run("no `water` supplied"))
  ok <- which(ctx$position == "ok")
  wet <- in_water(ctx$water, ctx$dep$deploy_lat[ok], ctx$dep$deploy_lon[ok])
  idx <- ok[wet %in% FALSE]
  note <- sprintf("%s off the water layer not checked; %s left to the position checks",
                  n_of(sum(is.na(wet)), "position"), fmt_n(sum(ctx$position != "ok")))
  check_result(length(idx), sum(!is.na(wet)), note, dep_rows(ctx, idx))
}

chk_depth_below_bottom <- function(ctx) {
  if (!inherits(ctx$water, "SpatRaster")) return(not_run("no depth raster in `water`"))
  if (!"deploy_z" %in% names(ctx$dep)) return(not_run("the deployments have no `deploy_z`"))
  ok <- which(ctx$position == "ok" & !is.na(ctx$dep$deploy_z))
  if (!length(ok)) return(not_run("no deployment with a checked position has a `deploy_z`"))
  v <- raster_values(ctx$water, ctx$dep$deploy_lat[ok], ctx$dep$deploy_lon[ok])$value
  deep <- !is.na(v) & ctx$dep$deploy_z[ok] > v + ctx$settings$depth_tol_m
  idx <- ok[deep]
  note <- sprintf("receiver deeper than the bathymetry by more than %g m; %s with no depth there",
                  ctx$settings$depth_tol_m, fmt_n(sum(is.na(v))))
  check_result(length(idx), sum(!is.na(v)), note, dep_rows(ctx, idx, bottom_m = v[deep]))
}

# ---- Stations ----
# Distances between the coordinate sets of each station, among positions
# that no position check flagged.
station_spread <- function(ctx) {
  ok <- ctx$position == "ok"
  d <- ctx$dep[ok, list(.dep, station_id, deploy_lat, deploy_lon)]
  d[, spread_m := 1000 * max(dist_km_matrix(deploy_lat, deploy_lon)), by = "station_id"]
  d
}

chk_station_coordinates <- function(ctx) {
  s <- ctx$settings
  d <- station_spread(ctx)
  two_sets <- d$.dep[d$spread_m > s$station_tol_m & d$spread_m <= s$reuse_m]
  places <- unique(d[, list(station_id, deploy_lat, deploy_lon)])
  m <- 1000 * dist_km_matrix(places$deploy_lat, places$deploy_lon)
  near <- which(m <= s$same_place_m & outer(places$station_id, places$station_id, "!="),
                arr.ind = TRUE)
  shared <- d$.dep[d$station_id %in% places$station_id[near[, 1]]]
  idx <- sort(unique(c(two_sets, shared)))
  what <- ifelse(idx %in% two_sets, "two coordinate sets", "two stations at one place")
  note <- sprintf("coordinate sets %g to %g m apart, or stations within %g m",
                  s$station_tol_m, s$reuse_m, s$same_place_m)
  rows <- dep_rows(ctx, idx, problem = what, spread_m = d$spread_m[match(idx, d$.dep)])
  check_result(length(idx), nrow(d), note, rows)
}

chk_station_reused <- function(ctx) {
  d <- station_spread(ctx)
  idx <- d$.dep[d$spread_m > ctx$settings$reuse_m]
  rows <- dep_rows(ctx, idx, spread_m = d$spread_m[match(idx, d$.dep)])
  check_result(length(idx), nrow(d),
               sprintf("one station name at places more than %g m apart", ctx$settings$reuse_m),
               rows)
}

# ---- Deployment times ----
valid_window <- function(dep) {
  is.na(dep$recover_datetime) | dep$recover_datetime >= dep$deploy_datetime
}

chk_overlapping_deployments <- function(ctx) {
  o <- overlapping_deps(ctx)
  rows <- dep_rows(ctx, o$idx, overlaps_with = o$with)
  note <- "deployments of one receiver serial overlap in time"
  if (o$no_serial) {
    note <- sprintf("%s; %s with no receiver serial %s not tested", note,
                    n_of(o$no_serial, "deployment"), verb(o$no_serial, "is", "are"))
  }
  check_result(length(o$idx), o$of, note, rows)
}

# Both members of every overlapping pair, kept in the context: the field
# check reads it, so an overlap is not reported again as co-detections out
# of range.
overlapping_deps <- function(ctx) {
  if (!is.null(ctx$overlap)) return(ctx$overlap)
  # A deployment with no serial is not a receiver to compare: station rows
  # with no serial would all overlap as one receiver.
  d <- ctx$dep[valid_window(ctx$dep), list(.dep, deploy_datetime, recover_datetime,
                                           key = split_receiver(receiver_serial)$serial)]
  no_serial <- sum(is.na(d$key))
  d <- d[!is.na(key)]
  d[, end := as.numeric(recover_datetime)]
  d[is.na(end), end := Inf]
  data.table::setorderv(d, c("key", "deploy_datetime"))
  d[, prior_end := c(-Inf, cummax(end)[-.N]), by = "key"]
  d[, prior_dep := c(NA_integer_, .dep[running_argmax(end)][-.N]), by = "key"]
  hit <- d[as.numeric(deploy_datetime) < prior_end]
  idx <- sort(unique(c(hit$.dep, hit$prior_dep)))
  ctx$overlap <- list(idx = idx, with = hit$prior_dep[match(idx, hit$.dep)], of = nrow(d),
                      no_serial = no_serial)
  ctx$overlap
}

# For each position, the index of the largest value so far (ties: the first).
running_argmax <- function(v) {
  vapply(seq_along(v), function(i) which.max(v[seq_len(i)]), integer(1))
}

chk_recover_before_deploy <- function(ctx) {
  dep <- ctx$dep
  bad <- which(!valid_window(dep))
  idx <- setdiff(bad, ctx$orphans$year$rows$.dep)
  n_off <- length(bad) - length(idx)
  note <- sprintf("%s more %s a year off and reported by deploy_year_off", fmt_n(n_off),
                  verb(n_off, "is", "are"))
  check_result(length(idx), nrow(dep), note, dep_rows(ctx, idx))
}

chk_deploy_year_off <- function(ctx) {
  y <- ctx$orphans$year$rows
  rows <- dep_rows(ctx, y$.dep, problem = y$problem, days = y$days)
  check_result(nrow(y), nrow(ctx$dep), sprintf(paste(
    "a gap of %g days or more between a deployment end and its detections, or a one-year",
    "shift that covers the receiver's orphan detections"
  ), ctx$settings$year_off_days), rows)
}

chk_recover_after_download <- function(ctx) {
  dep <- ctx$dep
  if (!"download_datetime" %in% names(dep)) {
    return(not_run("the deployments have no `download_datetime`"))
  }
  both <- !is.na(dep$recover_datetime) & !is.na(dep$download_datetime)
  late <- both & as.numeric(dep$recover_datetime - dep$download_datetime, units = "secs") >
    ctx$settings$download_tol_s
  idx <- which(late)
  check_result(length(idx), sum(both),
               sprintf("recovery more than %g s after the download", ctx$settings$download_tol_s),
               dep_rows(ctx, idx))
}

# Detections after a deployment's recorded download and before its end. The
# data end at the download, so these detections lie outside the receiver's
# data; the rows say how many, when, and on how many UTC days.
chk_detections_after_download <- function(ctx) {
  dep <- ctx$dep
  if (!"download_datetime" %in% names(dep)) {
    return(not_run("the deployments have no `download_datetime`"))
  }
  of <- sum(download_cuts(dep))
  idx <- after_download_index(dep, ctx$det)
  hit <- !is.na(idx)
  late <- data.table::data.table(i = idx[hit], t = ctx$det$datetime[hit])
  per <- late[, list(n = .N, first = min(t), last = max(t),
                     days = length(unique(as.Date(t)))), by = "i"]
  data.table::setorderv(per, "i")
  rows <- dep_rows(ctx, per$i, detections_after = per$n, first_after = per$first,
                   last_after = per$last, days_after = per$days)
  check_result(nrow(per), of, sprintf(paste(
    "%s after the recorded download; a deployment is tested when its download comes before",
    "its end. The data end at the download, so audit_listening() keeps those days",
    "not_downloaded and detset() marks the detections in after_download. They can mean a",
    "download time recorded too early, a later download missing from the sheet, a receiver",
    "that kept logging, or a clock problem"
  ), n_of(sum(hit), "detection")), rows)
}

# ---- Receivers and their detections ----
# A detection matched on its station to a deployment with no serial is
# deployed. A detection with no serial at all is counted on its own line,
# not as one more receiver.
chk_serial_not_deployed <- function(ctx) {
  det <- ctx$det
  idx <- which(!det$.serial & is.na(det$.dep))
  no_serial <- sum(is.na(det$receiver_serial[idx]))
  n_serial <- length(unique(stats::na.omit(det$receiver_serial[idx])))
  note <- sprintf("%s %s in no deployment", n_of(n_serial, "receiver serial"),
                  verb(n_serial, "appears", "appear"))
  if (no_serial) {
    note <- sprintf("%s; %s of the detections %s no receiver serial", note, fmt_n(no_serial),
                    verb(no_serial, "has", "have"))
  }
  check_result(length(idx), nrow(det), note, det_rows(ctx, idx))
}

# Only deployments that overlap the detections' time span by a day or more
# are tested: a set cut to one month says nothing about the other months.
chk_receiver_silent <- function(ctx) {
  dep <- ctx$dep
  span <- range(as.numeric(ctx$det$datetime))
  end <- data.table::fifelse(is.na(dep$recover_datetime), Inf, as.numeric(dep$recover_datetime))
  overlap <- pmin(end, span[2]) - pmax(as.numeric(dep$deploy_datetime), span[1])
  ok <- which(valid_window(dep) & overlap >= 86400)
  idx <- setdiff(ok, unique(ctx$det$.dep))
  bare <- sum(is.na(dep$receiver_serial[idx]))
  note <- paste("deployments that hold no detection of any transmitter, of those that overlap",
                "the detections' time span by a day or more")
  if (bare) {
    note <- sprintf("%s; %s of them %s no receiver serial and %s detections only by station",
                    note, fmt_n(bare), verb(bare, "has", "have"), verb(bare, "holds", "hold"))
  }
  check_result(length(idx), length(ok), note, dep_rows(ctx, idx))
}
