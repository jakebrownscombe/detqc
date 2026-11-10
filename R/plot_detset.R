# ---- Plots of a detection set ----
# Each function builds a ggplot with theme_positionr() and returns it. The
# plot's data is the data frame of what is drawn, with its counts as
# attributes, for a test or a caption to read.

#' Abacus plot of detections by animal over time
#'
#' Draws one row per animal and one point per animal, station and hour with a
#' detection. With at most 12 stations the points are coloured by station on
#' the turbo option of [ggplot2::scale_colour_viridis_d()], keyed "Station" in
#' natural order (STN2 before STN10), so neighbouring stations of the key
#' differ in hue. With more stations the points are one colour, and
#' `rows = "station"` names the stations.
#'
#' Detections with no animal, such as those of reference tags, get one row per
#' transmitter below the animals, labelled with its code. A tagged animal
#' never detected keeps an empty, labelled row when it was released inside
#' the time the detections span. Animals released outside that span with no
#' detection are left out, so a month cut from a long study keeps its own
#' time axis. The time axis runs over the detections, and a triangle marks
#' each release inside it.
#'
#' With `rows = "station"`, each animal gets one row per station that heard
#' it, labelled with the station. The animals are coloured in turn, parted by
#' grey lines and named at the right in their colour. An animal heard at one
#' station day after day, as a dead animal or a shed tag is, shows as one
#' long run on one row.
#'
#' Row labels and animal names that would print over one another are thinned
#' when the plot is drawn. [detqc_example], four walleye from 2 to 10 June 2024,
#' gives 37 rows with `rows = "station"`, and every row is named. A study of
#' many animals can give hundreds: then only some station rows are labelled,
#' an animal with one or two rows can lose its name to a neighbour, and the
#' rows have no grid lines (none above 60 rows). Draw fewer animals with
#' `animals`, or a taller figure, to name every row.
#'
#' @param x A [detset()].
#' @param animals Optional `animal_id` values to draw, in order. Given, they
#'   replace the animal rows and no transmitter rows are drawn.
#' @param rows `"animal"`, one row per animal coloured by station, or
#'   `"station"`, one row per animal and station.
#' @return A ggplot object; print it to draw it, or add layers, scales and
#'   labels to it with `+`. Its `data` is the data frame of the points drawn:
#'   `animal_id` (the transmitter code on a transmitter row), `station_id`
#'   and `hour`. The attribute `never_detected` of the data names the animals
#'   with an empty row, `transmitter_rows` the transmitters drawn with no
#'   animal, and `left_out` counts the tagged animals not drawn. With
#'   `rows = "station"`, a column `row` gives the row of each point.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' plot_abacus(dets) +
#'   ggplot2::labs(title = "Detections, one point per animal, station and hour")
#' abacus <- plot_abacus(dets, animals = c("ST-01", "ST-02"), rows = "station")
#' attr(abacus$data, "never_detected")
#' abacus
plot_abacus <- function(x, animals = NULL, rows = c("animal", "station")) {
  need_detset(x)
  form <- check_choice(rows, c("animal", "station"), "rows")
  rows <- abacus_rows(x, animals)
  if (form == "station") return(abacus_by_station(x, rows))
  ids <- rows$ids
  rel <- release_times(x, ids)
  rng <- if (nrow(rows$pts)) range(rows$pts$hour) else range(x$deployments$deploy_datetime)
  inside <- !is.na(rel) & rel >= rng[1] & rel <= rng[2]
  rng <- range(c(rng, rel[inside]))
  out <- abacus_data(rows)
  lab <- ifelse(ids %in% rows$never, paste(ids, "(none)"), ids)
  ggplot2::ggplot(out, ggplot2::aes(hour, match(animal_id, ids))) +
    abacus_points(out) +
    ggplot2::geom_point(data = data.frame(hour = rel[inside], y = seq_along(ids)[inside]),
                        ggplot2::aes(hour, y), shape = 17, size = 1.8, colour = "black") +
    abacus_axes(rng, seq_along(ids), lab, max(1, length(ids))) +
    ggplot2::labs(y = if (length(rows$tx)) "Animal or transmitter" else "Animal") +
    theme_positionr() +
    date_margin()
}

# Up to abacus_key_max stations the colour is keyed; above it a key cannot
# be read, so the points take one colour.
abacus_key_max <- 12
# The key runs in natural order (STN2 before STN10) on the turbo option of
# the viridis scales, whose neighbouring colours differ in hue. In text order
# on the default option, names that end in 1, 10 and 2 take three dark
# purples a reader cannot tell apart.
abacus_points <- function(out) {
  stations <- natural_sort(unique(stats::na.omit(out$station_id)))
  if (length(stations) > abacus_key_max) {
    return(ggplot2::geom_point(shape = 16, size = 0.6, colour = "#31688E"))
  }
  list(
    ggplot2::geom_point(ggplot2::aes(colour = station_id), shape = 16, size = 0.6),
    ggplot2::scale_colour_viridis_d(
      name = "Station", na.value = NA, option = "turbo", begin = 0.05, end = 0.95,
      limits = stations, guide = ggplot2::guide_legend(override.aes = list(size = 2))
    )
  )
}

# The station form: one row per animal and station, animals in turn from the
# top, each in its own colour of the viridis scale, named at the right.
abacus_by_station <- function(x, rows) {
  pts <- rows$pts[!is.na(station_id)]
  ids <- intersect(rows$ids, pts$animal_id)
  lanes <- unique(pts[, list(animal_id, station_id)])
  lanes <- lanes[order(match(animal_id, ids), station_id)]
  gap <- cumsum(c(0, lanes$animal_id[-1] != lanes$animal_id[-nrow(lanes)]))
  lanes[, row := max(seq_len(.N) + gap) + 1L - (seq_len(.N) + gap)]
  key <- paste(lanes$animal_id, lanes$station_id)
  pts[, row := lanes$row[match(paste(animal_id, station_id), key)]]
  rng <- if (nrow(pts)) range(pts$hour) else range(x$deployments$deploy_datetime)
  edge <- lanes$row[!duplicated(lanes$animal_id)][-1] + 1
  mid <- tapply(lanes$row, factor(lanes$animal_id, ids), mean)
  # An infinite time places the names at the right edge of the panel.
  names_at <- data.frame(animal_id = factor(ids, ids), row = as.numeric(mid[ids]),
                         edge_time = .POSIXct(Inf, tz = "UTC"))
  # check_overlap keeps the names drawn first, so the animals with most rows
  # keep theirs where two names would print over each other.
  lanes_per <- table(factor(lanes$animal_id, ids))
  names_at <- names_at[order(-as.integer(lanes_per[ids])), ]
  rows$pts <- pts
  out <- abacus_data(rows)
  # The names print outside the panel, so the right margin holds the
  # longest name at the 8 pt of the axis text.
  right <- 8 + 5 * max(0, nchar(ids))
  ggplot2::ggplot(out, ggplot2::aes(hour, row, colour = factor(animal_id, ids))) +
    ggplot2::geom_hline(yintercept = edge, colour = "grey70", linewidth = 0.3) +
    ggplot2::geom_point(shape = 16, size = 0.6) +
    ggplot2::geom_text(data = names_at,
                       ggplot2::aes(x = edge_time, y = row, label = paste0("  ", animal_id),
                                    colour = animal_id),
                       hjust = 0, size = 8 / ggplot2::.pt, check_overlap = TRUE,
                       inherit.aes = FALSE) +
    ggplot2::scale_colour_viridis_d(guide = "none", end = 0.75, drop = FALSE) +
    abacus_axes(rng, lanes$row, lanes$station_id, max(1, lanes$row)) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_positionr() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.85)),
                   plot.margin = ggplot2::margin(5, right, 5, 5),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.grid.major.y = if (nrow(lanes) > grid_rows_max) {
                     ggplot2::element_blank()
                   })
}

# Above this many station rows, a grid line on every row fills a panel of
# ordinary height with grey (256 rows do). The grey lines between animals
# then part the rows on their own.
grid_rows_max <- 60

# The axes both forms share: dates in UTC along the bottom and one labelled
# row a line up the side. The animal form titles its rows; the station form
# labels each row with its station and names the animal at the right. The row labels are
# thinned at draw time where they would print over one another: 256 station
# rows in a panel of ordinary height leave 1.2 pt a label.
abacus_axes <- function(rng, at, labels, n_rows) {
  list(
    ggplot2::scale_x_datetime(breaks = date_breaks, date_labels = "%Y-%m-%d"),
    ggplot2::scale_y_continuous(breaks = at, labels = labels, limits = c(0.5, n_rows + 0.5),
                                expand = c(0, 0),
                                guide = ggplot2::guide_axis(check.overlap = TRUE)),
    ggplot2::expand_limits(x = rng),
    ggplot2::labs(x = "Date (UTC)", y = NULL)
  )
}

# The scale passes its expanded limits as seconds; pretty() on the times,
# asked for six intervals, gives the round dates a base plot's axis gave.
date_breaks <- function(lim) pretty(.POSIXct(lim, tz = "UTC"), n = 6)

# A date label centred on a break near the right edge of the panel overhangs
# it by up to half its width, which the theme's plot margin does not hold.
# The right margin is widened to half a ten-character date at the size of
# the axis text.
date_margin <- function() {
  th <- theme_positionr()
  size <- ggplot2::calc_element("axis.text.x", th)$size
  pad <- as.numeric(ggplot2::calc_element("plot.margin", th))
  pad[2] <- max(pad[2], 0.55 * 10 * size / 2 + 2)
  ggplot2::theme(plot.margin = ggplot2::margin(pad[1], pad[2], pad[3], pad[4]))
}

abacus_data <- function(rows) {
  out <- as_df(rows$pts)
  attr(out, "never_detected") <- rows$never
  attr(out, "transmitter_rows") <- rows$tx
  attr(out, "left_out") <- rows$left_out
  out
}

# The rows of the abacus: detected animals, tagged animals released inside
# the detections' span with no detection, then one row per transmitter of
# the detections with no animal.
abacus_rows <- function(x, animals) {
  d <- as_dt(x$detections)
  hour <- function(t) num_utc(floor(as.numeric(t) / 3600) * 3600)
  pts <- unique(d[!is.na(animal_id), list(animal_id, station_id, hour = hour(datetime))])
  tx_pts <- unique(d[is.na(animal_id), list(animal_id = transmitter, station_id,
                                            hour = hour(datetime))])
  tagged <- unique(stats::na.omit(x$animals$animal_id))
  if (!is.null(animals)) {
    ids <- animals
    tx <- character()
  } else {
    span <- if (nrow(d)) range(d$datetime) else as.POSIXct(c(NA, NA), tz = "UTC")
    rel <- release_times(x, tagged)
    new <- tagged[!is.na(rel) & rel >= span[1] & rel <= span[2]]
    ids <- sort(unique(c(pts$animal_id, new)))
    tx <- sort(unique(tx_pts$animal_id))
    ids <- c(ids, setdiff(tx, ids))
  }
  pts <- data.table::rbindlist(list(pts, tx_pts[animal_id %in% tx]))[animal_id %in% ids]
  list(pts = pts, ids = ids, tx = tx, never = setdiff(ids, pts$animal_id),
       left_out = length(setdiff(tagged, ids)))
}

release_times <- function(x, ids) {
  ani <- x$animals
  if (is.null(ani) || !"release_datetime" %in% names(ani)) return(empty_utc(length(ids)))
  r <- as_dt(ani)[!is.na(release_datetime), list(r = min(release_datetime)), by = "animal_id"]
  r$r[match(ids, r$animal_id)]
}

#' Map of the receiver stations
#'
#' Draws each station at its deployment position as a ring, never filled: a
#' yellow ring (`"#FDE725"`) over a dark ring (`"#1A1A1A"`) one step wider,
#' sized by the station's detections. A station that detected nothing is a
#' white ring (`"#FFFFFF"`) over the dark ring, at the smallest size, so it
#' reads on deep water. With `water`, the water body is drawn
#' under the stations: a depth raster on [scale_fill_depth()], or a polygon as
#' its outline on the white panel. Land is blank.
#'
#' The axes are equal, with longitude scaled by the cosine of the mean
#' latitude, so distances read true; a scale bar gives the length. With
#' `water` and `fit = "all"`, the map spans the water body and every station,
#' so a basin with no station is still drawn; without `water` it spans the
#' stations. The map runs a tenth of its height below the southernmost station, so the scale bar
#' at the bottom left covers no station. The key gives the ring size of up to
#' three detection counts and, when the map has a station with no detections,
#' its white ring, labelled "no detections".
#'
#' Station names are placed most detections first. With ggrepel installed,
#' [ggrepel::geom_text_repel()] places them clear of every ring and of each
#' other, with a thin grey line to a name moved from its ring, and leaves out
#' a name it cannot place. Without ggrepel, a name prints to the right of its
#' station, or to the left when another station sits there, and a name that
#' would print over another is left out. These names can touch a neighbouring
#' ring on a dense array; install ggrepel, or give `labels = FALSE`.
#'
#' With `fit = "heard"`, the map is fitted to the stations that heard a
#' detection, with a margin, and only their names print. A network deployment
#' file can span several lakes; [processing_report()] draws the map this way.
#'
#' @param x A [detset()].
#' @param water Optional water body: an `sf` polygon, drawn as its outline, or
#'   a `terra` raster of depth in metres, drawn on [scale_fill_depth()] from
#'   `abs()` of its values, so depth stored as negative numbers draws the same.
#'   A raster in a projected system is projected to WGS 84 for the map.
#' @param labels Write the station names beside the points.
#' @param key Draw the key to the point sizes.
#' @param fit `"all"` to show every station, or `"heard"` to fit the map to
#'   the stations that heard a detection.
#' @return A ggplot object; print it to draw it, or add to it with `+`. Its
#'   `data` is a data frame with one row per station drawn: `station_id`,
#'   `deploy_lat`, `deploy_lon`, `detections`, `label_side` (`"right"` or
#'   `"left"`) and `labelled` (the name is offered to the map). The attribute
#'   `key` of the data holds the detection counts in the key, and
#'   `not_drawn` counts the stations outside the map.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' plot_receivers(dets) +
#'   ggplot2::labs(title = "Stations; ring size shows detections")
#' \donttest{
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   tif <- system.file("extdata", "stoney_depth.tif", package = "detqc")
#'   lake <- terra::rast(tif)
#'   plot_receivers(dets, water = lake)
#' }
#' }
plot_receivers <- function(x, water = NULL, labels = TRUE, key = TRUE, fit = c("all", "heard")) {
  need_detset(x)
  fit <- check_choice(fit, c("all", "heard"), "fit")
  st <- as_dt(x$deployments)[, list(deploy_lat = stats::median(deploy_lat),
                                    deploy_lon = stats::median(deploy_lon)), by = "station_id"]
  n <- as_dt(x$detections)[, list(detections = .N), by = "station_id"]
  st[, detections := data.table::fcoalesce(n$detections[match(station_id, n$station_id)], 0L)]
  box <- map_box(st, fit)
  if (fit == "all" && !is.null(water)) box <- union_box(box, water_box(water))
  inside <- st$deploy_lon >= box[1] & st$deploy_lon <= box[2] & st$deploy_lat >= box[3] &
    st$deploy_lat <= box[4]
  not_drawn <- sum(!inside)
  st <- st[inside]
  st[, label_side := label_sides(deploy_lon, deploy_lat, station_id, box)]
  named <- if (fit == "heard") st$detections > 0 else rep(TRUE, nrow(st))
  st[, labelled := labels & named]
  most <- max(1, st$detections)
  box <- label_room(st[labelled == TRUE], box, most)
  # The scale bar sits at the bottom left on an opaque box; a strip below the
  # southernmost station keeps it off the stations.
  box[3] <- box[3] - 0.1 * diff(box[3:4])
  shown <- if (key) key_counts(st$detections) else numeric()
  silent <- key && any(st$detections == 0)
  out <- as_df(st)
  attr(out, "key") <- shown
  attr(out, "not_drawn") <- not_drawn
  ggplot2::ggplot(out, ggplot2::aes(deploy_lon, deploy_lat)) +
    water_layer(water) +
    station_rings(out) +
    station_labels(out) +
    station_scales(shown, most, key, silent) +
    ggplot2::coord_fixed(ratio = 1 / cos(mean(box[3:4]) * pi / 180), xlim = box[1:2],
                         ylim = box[3:4]) +
    annotation_scale_bar(lonlat = TRUE) +
    ggplot2::labs(x = "Longitude (degrees)", y = "Latitude (degrees)") +
    theme_positionr()
}

# Receivers are drawn as rings, never filled, so the map under them stays
# visible: the light end of viridis over a dark ring one step wider, so a
# ring reads on white, on pale water and on deep water.
# The colours are read when a plot is drawn, from the shared theme file.
ring_stroke <- c(dark = 1.8, light = 0.7)

# Three layers: a dark ring at every station, keyed by shape so a station
# with no detections has its own key entry; a yellow ring over the stations
# that heard a detection; and a white ring over the stations that heard
# none, because a dark ring alone vanishes on deep water. The white ring
# joins the shape key only, so the size key stays yellow.
station_rings <- function(st) {
  ring <- positionr_ring_colours()
  status <- ifelse(st$detections > 0, "heard", "none")
  silent <- st[st$detections == 0, ]
  list(
    ggplot2::geom_point(ggplot2::aes(size = detections, shape = status), colour = ring[["edge"]],
                        stroke = ring_stroke[["dark"]], show.legend = TRUE),
    ggplot2::geom_point(data = st[st$detections > 0, ], ggplot2::aes(size = detections),
                        shape = 1, colour = ring[["heard"]], stroke = ring_stroke[["light"]]),
    if (nrow(silent)) {
      ggplot2::geom_point(data = silent,
                          ggplot2::aes(size = detections, shape = "none"),
                          colour = ring[["silent"]], stroke = ring_stroke[["light"]],
                          show.legend = c(size = FALSE, shape = TRUE))
    }
  )
}

# The extent of the map: every station, or the stations that heard a detection
# with a margin of a fifth of their span and at least 1 km.
map_box <- function(st, fit) {
  use <- if (fit == "heard" && any(st$detections > 0)) st[detections > 0] else st
  lon <- range(use$deploy_lon)
  lat <- range(use$deploy_lat)
  km_lat <- 1 / 111.195
  km_lon <- km_lat / cos(mean(lat) * pi / 180)
  if (fit == "all") return(c(lon, lat))
  pad_lon <- max(0.2 * diff(lon), km_lon)
  pad_lat <- max(0.2 * diff(lat), km_lat)
  c(lon + c(-1, 1) * pad_lon, lat + c(-1, 1) * pad_lat)
}

# The extent of the water body in WGS 84: the cells that hold water, or the
# polygon's bounding box, as c(lon_min, lon_max, lat_min, lat_max).
water_box <- function(water) {
  if (inherits(water, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop_detqc("The terra package is needed for a raster `water`.")
    }
    e <- terra::ext(terra::project(terra::trim(water[[1]]), "EPSG:4326", method = "near"))
    return(c(e$xmin, e$xmax, e$ymin, e$ymax))
  }
  if (!requireNamespace("sf", quietly = TRUE)) stop_detqc("The sf package is needed for `water`.")
  bb <- sf::st_bbox(sf::st_transform(water, 4326))
  unname(c(bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]]))
}

union_box <- function(a, b) {
  unname(c(min(a[1], b[1]), max(a[2], b[2]), min(a[3], b[3]), max(a[4], b[4])))
}

# The water body under the stations. A raster is drawn by depth; a polygon
# as rings in longitude and latitude, holes kept as subgroups, so that it
# draws under coord_fixed() without coord_sf().
water_layer <- function(water) {
  if (is.null(water)) return(NULL)
  if (inherits(water, "SpatRaster")) return(water_raster_layer(water))
  if (!requireNamespace("sf", quietly = TRUE)) stop_detqc("The sf package is needed for `water`.")
  g <- sf::st_cast(sf::st_geometry(sf::st_transform(water, 4326)), "POLYGON")
  xy <- sf::st_coordinates(g)
  ring <- data.frame(lon = xy[, "X"], lat = xy[, "Y"], poly = xy[, "L2"], hole = xy[, "L1"])
  ggplot2::geom_polygon(data = ring, ggplot2::aes(lon, lat, group = poly, subgroup = hole),
                        fill = NA, colour = "grey40", linewidth = 0.3, inherit.aes = FALSE)
}

# A depth raster in WGS 84 cells, nearest value, depth as abs() so that a
# raster of negative depths draws as deep.
water_raster_layer <- function(water) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop_detqc("The terra package is needed for a raster `water`.")
  }
  ll <- terra::project(water[[1]], "EPSG:4326", method = "near")
  cells <- terra::as.data.frame(ll, xy = TRUE, na.rm = TRUE)
  cells <- data.frame(lon = cells[[1]], lat = cells[[2]], depth_m = abs(cells[[3]]))
  list(ggplot2::geom_raster(data = cells, ggplot2::aes(lon, lat, fill = depth_m),
                            inherit.aes = FALSE),
       scale_fill_depth())
}

# Station names, 6.3 pt. A name is padded with spaces so that it starts
# clear of its point; names are offered most detections first, and
# check_overlap leaves out a name that would print over one already drawn.
label_pt <- 6.3
space_mm <- 0.278 * label_pt / ggplot2::.pt
char_mm <- 0.6 * label_pt / ggplot2::.pt

label_pad <- function(n, most) {
  ceiling((station_size(n, most) / 2 + 0.4) / space_mm)
}

station_labels <- function(st) {
  lab <- st[st$labelled, ]
  if (!nrow(lab)) return(NULL)
  if (has_repel()) return(repel_labels(st))
  lab <- lab[order(-lab$detections), ]
  pad <- strrep(" ", label_pad(lab$detections, max(1, st$detections)))
  left <- lab$label_side == "left"
  lab$text <- ifelse(left, paste0(lab$station_id, pad), paste0(pad, lab$station_id))
  lab$hjust <- as.numeric(left)
  ggplot2::geom_text(data = lab, ggplot2::aes(deploy_lon, deploy_lat, label = text, hjust = hjust),
                     size = label_pt / ggplot2::.pt, check_overlap = TRUE)
}

# ggrepel is in Suggests: on a 70-station array and on a dense harbour
# array, names thinned only against other names are drawn over
# neighbouring points. Every station is in the layer's data, its name
# blank when it is not labelled, so the names keep clear of every point;
# each point's size is passed as `point.size`. The force, iterations and
# overlap limit are set for a 70-station array at 7.5 by 4.2 in: there the
# defaults leave names touching, and these place 45 names with none
# touching and leave out the rest.
has_repel <- function() requireNamespace("ggrepel", quietly = TRUE)

repel_labels <- function(st) {
  d <- st[order(-st$detections), ]
  d$text <- ifelse(d$labelled, d$station_id, "")
  d$point_mm <- station_size(d$detections, max(1, st$detections))
  ggrepel::geom_text_repel(
    data = d, ggplot2::aes(deploy_lon, deploy_lat, label = text, point.size = point_mm),
    size = label_pt / ggplot2::.pt, seed = 1, force = 3, max.iter = 2e5, max.time = 2,
    max.overlaps = 12, box.padding = 0.2, point.padding = 0.1, min.segment.length = 0.1,
    segment.colour = "grey40", segment.size = 0.2, inherit.aes = FALSE, show.legend = FALSE
  )
}

# Text is measured in data units on a nominal panel of 120 by 100 mm, a
# little smaller than the report's, so that the room left for names errs
# wide; ggplot2 places the names at draw time. The result is degrees of
# latitude a millimetre.
lat_per_mm <- function(box) {
  coslat <- cos(mean(box[3:4]) * pi / 180)
  max(diff(box[1:2]) * coslat / 120, diff(box[3:4]) / 100)
}

# The map is widened so that the names at its edges print inside it.
label_room <- function(st, box, most) {
  if (!nrow(st)) return(box)
  lon_mm <- lat_per_mm(box) / cos(mean(box[3:4]) * pi / 180)
  w <- (label_pad(st$detections, most) * space_mm + nchar(st$station_id) * char_mm) * lon_mm
  right <- st$label_side == "right"
  c(min(c(box[1], st$deploy_lon[!right] - w[!right])),
    max(c(box[2], st$deploy_lon[right] + w[right])), box[3:4])
}

# Point size in mm: area grows with the detections, from 1.5 mm for none to
# 7 mm for the most, as scale_size() draws it.
station_range <- c(1.5, 7)
station_size <- function(n, most) {
  station_range[1] + diff(station_range) * sqrt(n / most)
}

# The open circle of a station with no detections is keyed only when the
# map draws one; on a map where every station heard something, a fixed
# entry would key nothing.
station_scales <- function(shown, most, key, silent = FALSE) {
  if (!key) {
    return(list(ggplot2::scale_size(range = station_range, limits = c(0, most), guide = "none"),
                ggplot2::scale_shape_manual(values = c(heard = 1, none = 1), guide = "none")))
  }
  list(
    ggplot2::scale_size(
      name = "Detections", range = station_range, limits = c(0, most), breaks = shown,
      labels = format(shown, big.mark = ",", trim = TRUE),
      guide = ggplot2::guide_legend(order = 1, override.aes = list(shape = 1))
    ),
    ggplot2::scale_shape_manual(
      name = NULL, values = c(heard = 1, none = 1), limits = c("heard", "none"),
      breaks = "none", labels = "no detections",
      guide = if (silent) {
        ggplot2::guide_legend(order = 2, override.aes = list(size = station_range[1]))
      } else {
        "none"
      }
    )
  )
}

# The key to the point sizes: up to three round counts no larger than the
# largest.
key_counts <- function(n) {
  most <- max(n)
  if (most < 1) return(numeric())
  # Five intervals: pretty() with three gives 0, 50,000 and 100,000 for a
  # largest count of 74,000, and the key would show one size.
  v <- pretty(c(0, most), n = 5)
  v <- v[v > 0 & v <= most & v == round(v)]
  if (!length(v)) return(most)
  v[unique(round(seq(1, length(v), length.out = min(3, length(v)))))]
}

# A name prints to the left of its station when another station lies where
# it would print on the right. Two labels of stations 400 m apart otherwise
# print over each other at the zoom of a whole array.
label_sides <- function(lon, lat, ids, box) {
  coslat <- cos(mean(box[3:4]) * pi / 180)
  reach <- 1.3 * nchar(paste0(ids, "  ")) * char_mm * lat_per_mm(box) / coslat
  tall <- 1.5 * label_pt / ggplot2::.pt * lat_per_mm(box)
  hit <- vapply(seq_along(ids), function(i) {
    any(lon > lon[i] & lon - lon[i] < reach[i] & abs(lat - lat[i]) < tall)
  }, logical(1))
  ifelse(hit, "left", "right")
}

#' Deployment timeline by station
#'
#' Draws one row per station and one bar per deployment, from deployment to
#' recovery. Bars of stations that heard a detection in the set are dark
#' grey; bars of stations that heard none are light grey. A deployment still
#' in the water (no recovery) runs to the last detection in the set and ends
#' in a blue arrow; one that starts after the last detection is a blue ">" at
#' its start. When the listening table carries a status from
#' [audit_listening()], red ticks mark the receiver-days that were not
#' listening. The key names each mark the plot draws.
#'
#' Stations are in natural order, so STN2 comes before STN10. Station labels
#' that would print over one another are thinned when the plot is drawn.
#' Above 60 stations the bars are thinner, the rows have no grid lines, and
#' only the stations that heard a detection are labelled and carry the arrow
#' of a deployment still in the water.
#'
#' With `span = "detections"`, the time axis runs over the detections, with a
#' margin of a tenth of their span, and only stations with a deployment in
#' that time are drawn. [processing_report()] draws it this way.
#'
#' @param x A [detset()].
#' @param span `"deployments"` for the whole deployment history, or
#'   `"detections"` for the time the detections cover.
#' @return A ggplot object; print it to draw it, or add to it with `+`. Its
#'   `data` is the deployments drawn, with `drawn_to` (the end drawn) and
#'   `active` (no recovery). The attribute `not_drawn` of the data counts
#'   the deployments outside the time drawn.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' plot_timeline(dets) + ggplot2::labs(title = "Deployments by station")
plot_timeline <- function(x, span = c("deployments", "detections")) {
  need_detset(x)
  win <- timeline_window(x, check_choice(span, c("deployments", "detections"), "span"))
  dep <- win$dep
  rng <- win$rng
  st <- natural_sort(unique(dep$station_id))
  dep[, `:=`(y = match(station_id, st), from = pmax(deploy_datetime, rng[1]),
             to = pmin(drawn_to, rng[2]))]
  # An open deployment that starts after the last detection has no length to
  # draw, so it is a ">" at its start.
  heard_st <- unique(stats::na.omit(x$detections$station_id))
  many <- length(st) > grid_rows_max
  # Above 60 stations the arrows of stations that heard nothing are left out:
  # on a network set they would mark hundreds of rows outside the study.
  open_rows <- dep$active & (!many | dep$station_id %in% heard_st)
  go <- dep[open_rows & drawn_to > deploy_datetime]
  late <- dep[open_rows & drawn_to <= deploy_datetime]
  out <- as_df(dep[, !c("y", "from", "to")])
  attr(out, "not_drawn") <- win$not_drawn
  open_lab <- "Not recovered: drawn to the last detection"
  ticks <- not_listening_layer(x$listening, st)
  dep[, heard := data.table::fifelse(station_id %in% heard_st, heard_lab, silent_lab)]
  shown_heard <- intersect(c(heard_lab, silent_lab), dep$heard)
  # Above 60 stations only the stations that heard a detection are named.
  # Blank names for the rest would take their height, and the overlap check
  # would then drop every name.
  at <- if (many) which(st %in% heard_st) else seq_along(st)
  ggplot2::ggplot(out) +
    ggplot2::geom_segment(data = dep, ggplot2::aes(x = from, xend = to, y = y, yend = y,
                                                   colour = heard),
                          linewidth = if (many) 0.5 else 1.4, lineend = "butt",
                          key_glyph = "path") +
    ggplot2::geom_segment(data = go, ggplot2::aes(x = from, xend = drawn_to, y = y, yend = y,
                                                  colour = open_lab),
                          linewidth = 0.7, key_glyph = "path",
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.08, "inches"))) +
    ggplot2::geom_text(data = late, ggplot2::aes(x = deploy_datetime, y = y, colour = open_lab),
                       label = ">", size = 8 / ggplot2::.pt, show.legend = FALSE) +
    ticks +
    timeline_key(open_lab, nrow(go) + nrow(late) > 0, !is.null(ticks), shown_heard) +
    ggplot2::scale_x_datetime(breaks = date_breaks, date_labels = "%Y-%m-%d") +
    ggplot2::scale_y_continuous(breaks = at, labels = st[at],
                                limits = c(0.5, length(st) + 0.5), expand = c(0, 0),
                                guide = ggplot2::guide_axis(check.overlap = TRUE)) +
    ggplot2::expand_limits(x = rng) +
    ggplot2::labs(x = "Date (UTC)", y = NULL) +
    theme_positionr() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major.y = if (many) ggplot2::element_blank(),
                   panel.grid.minor.y = if (many) ggplot2::element_blank()) +
    date_margin()
}

# The key to the two marks, each named only when drawn. The bars of
# stations that heard a detection stand out from the rest: on hundreds of
# network stations around a few that heard the animals, every bar in dark
# grey would fill the panel. Both are keyed only when both are drawn.
not_listening_lab <- "Receiver-day not listening (audit_listening())"
heard_lab <- "Station heard a detection"
silent_lab <- "Station heard none"
timeline_key <- function(open_lab, open, off, heard = character()) {
  vals <- c(stats::setNames("steelblue", open_lab), stats::setNames("red", not_listening_lab),
            stats::setNames("grey30", heard_lab), stats::setNames("grey85", silent_lab))
  shown <- c(if (length(heard) > 1) heard, c(open_lab, not_listening_lab)[c(open, off)])
  ggplot2::scale_colour_manual(name = NULL, values = vals, breaks = shown,
                               guide = if (length(shown)) {
                                 ggplot2::guide_legend(override.aes = list(linewidth = 1))
                               } else {
                                 "none"
                               })
}

# Receiver-days audit_listening() counts as not listening, as red ticks at
# midday.
not_listening_layer <- function(lst, st) {
  if (!"listening" %in% names(lst) || !any(!lst$listening)) return(NULL)
  off <- lst[!lst$listening & lst$station_id %in% st, ]
  if (!nrow(off)) return(NULL)
  ticks <- data.frame(t = as.POSIXct(off$date, tz = "UTC") + 43200, y = match(off$station_id, st))
  ggplot2::geom_text(data = ticks, ggplot2::aes(x = t, y = y, colour = not_listening_lab),
                     label = "|", size = 8 / ggplot2::.pt, key_glyph = "vline")
}

# The deployments a timeline draws and its time axis.
timeline_window <- function(x, span) {
  dep <- as_dt(x$deployments)
  has_det <- nrow(x$detections) > 0
  last <- if (has_det) max(x$detections$datetime) else max(dep$deploy_datetime)
  dep[, active := is.na(recover_datetime)]
  dep[, drawn_to := data.table::fifelse(active, pmax(last, deploy_datetime), recover_datetime)]
  rng <- range(c(dep$deploy_datetime, dep$drawn_to))
  n_all <- nrow(dep)
  if (span == "detections" && has_det) {
    rng <- range(x$detections$datetime)
    rng <- rng + c(-1, 1) * max(86400, 0.1 * as.numeric(diff(rng), units = "secs"))
    dep <- dep[deploy_datetime <= rng[2] & (drawn_to >= rng[1] | active)]
  }
  list(dep = dep, rng = rng, not_drawn = n_all - nrow(dep))
}

# Natural order: "STN2" before "STN10".
natural_sort <- function(v) {
  v <- as.character(v)
  num <- suppressWarnings(as.numeric(sub("^.*?([0-9]+)$", "\\1", v, perl = TRUE)))
  num[!grepl("[0-9]$", v)] <- NA
  stem <- sub("[0-9]+$", "", v)
  v[order(stem, num, v, na.last = FALSE)]
}

utils::globalVariables(c(
  "hour", "y", "row", "edge_time", "text", "hjust", "lon", "lat", "poly", "hole", "from", "to", "t",
  "rows_after", "record", "breach", "detections", "label_side", "labelled", "heard", "point_mm",
  "depth_m", "label"
))
