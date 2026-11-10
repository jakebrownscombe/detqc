# The three plots return ggplot objects drawn with theme_positionr(); the
# plot's data is what is drawn, with its counts as attributes.

layer_geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], ""))

test_that("plot_abacus keeps animals never detected that were released in the span", {
  ex <- hard_cases()
  late <- transform(ex$animals[1, ], animal_id = "NEVER", transmitter = "X-1",
                    release_datetime = ex$animals$release_datetime[1] + 86400 * 20)
  early <- transform(ex$animals[1, ], animal_id = "LONG-AGO", transmitter = "X-2",
                     release_datetime = as.POSIXct("2020-05-01", tz = "UTC"))
  x <- detset(ex$detections, ex$deployments, rbind(ex$animals, late, early))
  out <- plot_data(plot_abacus(x))
  expect_equal(attr(out, "never_detected"), "NEVER")
  # An animal released years before the detections, never detected, is
  # left out and counted, so it does not stretch the axis.
  expect_equal(attr(out, "left_out"), 1L)
  expect_false("LONG-AGO" %in% out$animal_id)
  expect_named(out, c("animal_id", "station_id", "hour"))
  one <- plot_data(plot_abacus(x, animals = c("EX-01", "NEVER")))
  expect_setequal(unique(one$animal_id), "EX-01")
  # With no animal ids at all every transmitter gets its own row.
  d <- ex$detections
  d$animal_id <- NA_character_
  y <- detset(d, ex$deployments)
  ty <- plot_data(plot_abacus(y))
  expect_equal(attr(ty, "never_detected"), character())
  expect_setequal(attr(ty, "transmitter_rows"), unique(d$transmitter))
})

test_that("plot_abacus is a ggplot: points by station, release triangles, labelled rows", {
  ex <- hard_cases()
  late <- transform(ex$animals[1, ], animal_id = "NEVER", transmitter = "X-1",
                    release_datetime = ex$animals$release_datetime[1] + 86400 * 20)
  x <- detset(ex$detections, ex$deployments, rbind(ex$animals, late))
  p <- plot_abacus(x)
  expect_identical(layer_geoms(p), c("GeomPoint", "GeomPoint", "GeomBlank"))
  expect_identical(ggplot2::as_label(p$mapping$x), "hour")
  # 24 stations are more than a key can name, so the points take one colour
  # and carry no colour mapping without a key.
  expect_null(p$layers[[1]]$mapping$colour)
  expect_identical(p$layers[[1]]$aes_params$colour, "#31688E")
  expect_identical(p$layers[[2]]$aes_params$shape, 17)
  expect_identical(p$labels$x, "Date (UTC)")
  expect_identical(p$labels$y, "Animal")
  b <- ggplot2::ggplot_build(p)
  # One labelled row per animal and transmitter; the never-detected animal
  # is marked (none).
  ys <- b$layout$panel_params[[1]]$y
  expect_true("NEVER (none)" %in% ys$get_labels())
  # Round dates along the time axis, one a month over six months of data.
  xs <- stats::na.omit(b$layout$panel_params[[1]]$x$get_labels())
  expect_identical(as.character(xs), format(seq(as.Date("2023-05-01"), by = "month",
                                                length.out = 7)))
  expect_equal(nrow(b$data[[2]]), sum(!is.na(x$animals$release_datetime) &
                                        x$animals$release_datetime >= min(x$detections$datetime) &
                                        x$animals$release_datetime <= max(x$detections$datetime)))
  # A user adds to it.
  q <- p + ggplot2::labs(title = "Mine")
  expect_identical(q$labels$title, "Mine")
  expect_identical(ggplot2::calc_element("plot.title", q$theme)$face, "plain")
})

test_that("plot_abacus draws the detections with no animal, one row per transmitter", {
  # Detections with no animal, such as reference tags, get rows.
  x <- example_detset()
  ref <- x$detections$transmitter[x$detections$animal_id %in% "EX-01"][1]
  x$detections$animal_id[x$detections$transmitter == ref] <- NA
  out <- plot_data(plot_abacus(x))
  expect_equal(attr(out, "transmitter_rows"), ref)
  expect_gt(sum(out$animal_id == ref), 0L)
  on <- x$detections$transmitter == ref
  hours <- data.frame(s = x$detections$station_id[on],
                      h = floor(as.numeric(x$detections$datetime[on]) / 3600))
  expect_equal(sum(out$animal_id == ref), nrow(unique(hours)))
})

test_that("plot_abacus(rows = 'station') gives each animal one row per station", {
  x <- example_detset()
  ids <- c("EX-01", "EX-03")
  p <- plot_abacus(x, animals = ids, rows = "station")
  out <- plot_data(p)
  expect_setequal(unique(out$animal_id), ids)
  lanes <- unique(out[, c("animal_id", "station_id", "row")])
  expect_equal(nrow(lanes), length(unique(paste(out$animal_id, out$station_id))))
  expect_equal(anyDuplicated(lanes$row), 0L)
  # The first animal sits above the second, with one empty row between them,
  # and each animal's rows are consecutive.
  expect_equal(min(lanes$row[lanes$animal_id == "EX-01"]),
               max(lanes$row[lanes$animal_id == "EX-03"]) + 2)
  for (a in ids) {
    r <- sort(lanes$row[lanes$animal_id == a])
    expect_equal(diff(r), rep(1, length(r) - 1))
  }
  pts <- out[out$animal_id == "EX-01", ]
  d <- x$detections[x$detections$animal_id %in% "EX-01", ]
  expect_equal(nrow(pts), nrow(unique(data.frame(d$station_id,
                                                 floor(as.numeric(d$datetime) / 3600)))))
  # Grey lines part the animals; each animal is named at the right in the
  # colour of its points.
  expect_identical(layer_geoms(p), c("GeomHline", "GeomPoint", "GeomText", "GeomBlank"))
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$data[[1]]$yintercept, max(lanes$row[lanes$animal_id == "EX-03"]) + 1)
  names_drawn <- b$data[[3]]
  expect_identical(trimws(names_drawn$label), ids)
  expect_true(all(is.infinite(names_drawn$x)))
  first <- b$data[[2]]$colour[b$data[[2]]$y == max(lanes$row)][1]
  expect_identical(names_drawn$colour[1], first)
  expect_false(isTRUE(p$coordinates$clip == "on"))
  expect_error(plot_abacus(x, rows = "receiver"), "`rows` must be one of", class = "detqc_error")
})

test_that("plot_abacus thins row labels and animal names that would collide", {
  x <- example_detset()
  p <- plot_abacus(x, rows = "station")
  expect_true(p$scales$get_scales("y")$guide$params$check.overlap)
  names_layer <- p$layers[[which(layer_geoms(p) == "GeomText")]]
  expect_true(names_layer$geom_params$check_overlap)
  # Names are offered animals with most rows first, so those keep theirs.
  lanes <- unique(plot_data(p)[, c("animal_id", "station_id")])
  per <- table(lanes$animal_id)
  expect_equal(as.integer(per[as.character(names_layer$data$animal_id)]),
               sort(as.integer(per), decreasing = TRUE))
  expect_true(plot_abacus(x)$scales$get_scales("y")$guide$params$check.overlap)
  # Above 60 station rows the rows carry no grid lines; below, they keep them.
  grid_y <- function(p) class(ggplot2::calc_element("panel.grid.major.y", p$theme))
  expect_true("element_blank" %in% grid_y(p))
  expect_true("element_line" %in% grid_y(plot_abacus(x, animals = "EX-01", rows = "station")))
})

test_that("date axes leave room at the right for the last date label", {
  x <- example_detset()
  room <- function(p) as.numeric(ggplot2::calc_element("plot.margin", p$theme))[2]
  size <- ggplot2::calc_element("axis.text.x", theme_positionr())$size
  expect_gte(room(plot_timeline(x)), 0.55 * 10 * size / 2)
  expect_gte(room(plot_abacus(x)), 0.55 * 10 * size / 2)
})

test_that("plot_receivers draws every station, with or without water", {
  x <- example_detset()
  out <- plot_data(plot_receivers(x))
  expect_equal(nrow(out), length(unique(x$deployments$station_id)))
  expect_equal(sum(out$detections), sum(!is.na(x$detections$station_id)))
  testthat::skip_if_not_installed("sf")
  bb <- c(range(x$deployments$deploy_lon), range(x$deployments$deploy_lat))
  ring <- matrix(c(bb[1], bb[3], bb[2], bb[3], bb[2], bb[4], bb[1], bb[4], bb[1], bb[3]),
                 ncol = 2, byrow = TRUE)
  water <- sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  p2 <- plot_receivers(x, water = water, labels = FALSE)
  out2 <- plot_data(p2)
  expect_equal(nrow(out2), nrow(out))
  expect_identical(layer_geoms(p2)[1], "GeomPolygon")
  expect_equal(nrow(p2$layers[[1]]$data), 5L)
  # A polygon is drawn as its outline on the white panel.
  expect_true(is.na(p2$layers[[1]]$aes_params$fill))
  expect_false("GeomText" %in% layer_geoms(p2))
})

test_that("plot_receivers draws a depth raster on the depth scale from abs(depth)", {
  testthat::skip_if_not_installed("terra")
  f <- function(n) system.file("extdata", n, package = "detqc")
  x <- detset(read_detections(f("stoney_detections.csv")),
              read_deployments(f("stoney_receivers.csv"), format = "glatos"))
  lake <- terra::rast(f("stoney_depth.tif"))
  p <- plot_receivers(x, water = lake, labels = FALSE)
  expect_identical(layer_geoms(p)[1], "GeomRaster")
  cells <- p$layers[[1]]$data
  # The raster stores depth as zero or negative metres; the map reads it as positive.
  expect_true(all(cells$depth_m >= 0))
  expect_equal(max(cells$depth_m), max(abs(terra::values(lake)), na.rm = TRUE),
               tolerance = 1e-6)
  expect_identical(p$scales$get_scales("fill")$name, "Depth (m)")
})

test_that("plot_receivers is a map: equal coordinates, scale bar, sized points, units", {
  x <- detset(hard_cases()$detections, hard_cases()$deployments, hard_cases()$animals)
  x$detections <- x$detections[x$detections$station_id != "G05", ]
  p <- plot_receivers(x)
  lat <- mean(range(x$deployments$deploy_lat))
  expect_equal(p$coordinates$ratio, 1 / cos(lat * pi / 180), tolerance = 1e-3)
  expect_identical(p$labels$x, "Longitude (degrees)")
  expect_identical(p$labels$y, "Latitude (degrees)")
  geoms <- layer_geoms(p)
  expect_true(all(c("GeomPoint", "ScaleBarRect", "ScaleBarText") %in% geoms))
  expect_true(any(c("GeomText", "GeomTextRepel") %in% geoms))
  b <- ggplot2::ggplot_build(p)
  rings <- which(geoms == "GeomPoint")
  pts <- b$data[[rings[1]]]
  light <- b$data[[rings[2]]]
  # Receivers are rings with no fill: a dark ring at every
  # station, a narrower yellow ring over those that heard a detection, and a
  # white ring over those that heard none. G05, with none, is white over dark.
  st <- p$data
  expect_length(rings, 3L)
  white <- b$data[[rings[3]]]
  expect_true(all(pts$shape == 1) && all(light$shape == 1) && all(white$shape == 1))
  expect_true(all(pts$colour == "#1A1A1A") && all(light$colour == "#FDE725"))
  expect_true(all(white$colour == "#FFFFFF"))
  ring_col <- positionr_ring_colours()
  expect_identical(c(light$colour[1], white$colour[1], pts$colour[1]),
                   unname(ring_col[c("heard", "silent", "edge")]))
  expect_gt(pts$stroke[1], light$stroke[1])
  expect_equal(nrow(pts), nrow(st))
  expect_equal(nrow(light), sum(st$detections > 0))
  expect_equal(nrow(white), sum(st$detections == 0))
  expect_false("G05" %in% p$layers[[rings[2]]]$data$station_id)
  expect_identical(p$layers[[rings[3]]]$data$station_id, "G05")
  big <- which.max(st$detections)
  expect_equal(pts$size[big], max(pts$size))
  expect_true(cor(sqrt(st$detections), pts$size) > 0.999)
  bar <- b$data[[which(geoms == "ScaleBarText")]]
  expect_equal(nrow(bar), 1L)
  # The size key holds the counts of the data's key attribute.
  key <- ggplot2::get_guide_data(p, "size")
  expect_equal(key$.value, attr(st, "key"))
  expect_identical(ggplot2::get_guide_data(p, "shape")$.label, "no detections")
  # With every station heard, the open circle has no key entry.
  all_heard <- plot_receivers(detset(hard_cases()$detections, hard_cases()$deployments))
  expect_true(all(all_heard$data$detections > 0))
  expect_null(ggplot2::get_guide_data(all_heard, "shape"))
})

test_that("plot_abacus keys the station colours on a small array", {
  ex <- hard_cases()
  keep <- sort(unique(ex$detections$station_id))[1:5]
  dep <- ex$deployments[ex$deployments$station_id %in% keep, ]
  x <- detset(ex$detections[ex$detections$station_id %in% keep, ], dep, ex$animals)
  p <- plot_abacus(x, animals = unique(stats::na.omit(x$detections$animal_id)))
  expect_identical(ggplot2::as_label(p$layers[[1]]$mapping$colour), "station_id")
  key <- ggplot2::get_guide_data(p, "colour")
  expect_setequal(key$.label, unique(stats::na.omit(p$data$station_id)))
  expect_identical(p$scales$get_scales("colour")$name, "Station")
  expect_identical(p$labels$y, "Animal")
})

test_that("plot_receivers keeps close labels apart and keys the point sizes", {
  x <- detset(hard_cases()$detections, hard_cases()$deployments, hard_cases()$animals)
  p <- plot_receivers(x)
  out <- plot_data(p)
  # P1A and P1B lie 400 m apart on one latitude; P1A's name moves left.
  side <- setNames(out$label_side, out$station_id)
  expect_equal(unname(side[c("P1A", "P1B")]), c("left", "right"))
  expect_equal(unname(side["G13"]), "right")
  key <- attr(out, "key")
  expect_true(length(key) >= 1 && length(key) <= 3)
  expect_true(all(key > 0 & key <= max(out$detections)))
  # A largest count of 74,000 keys three sizes.
  expect_equal(key_counts(c(0, 74000)), c(20000, 40000, 60000))
  expect_equal(key_counts(c(0, 3)), c(1, 2, 3))
  # The map runs below the southernmost station, so the scale bar clears it.
  expect_lt(p$coordinates$limits$y[1], min(out$deploy_lat))
  # Without ggrepel, names are offered most detections first and never print
  # over each other.
  testthat::local_mocked_bindings(has_repel = function() FALSE)
  p <- plot_receivers(x)
  text_layer <- p$layers[[which(layer_geoms(p) == "GeomText")]]
  expect_true(text_layer$geom_params$check_overlap)
  expect_equal(trimws(text_layer$data$text),
               out$station_id[order(-out$detections)][out$labelled[order(-out$detections)]])
  expect_equal(text_layer$data$hjust[text_layer$data$station_id == "P1A"], 1)
  # The map is widened so the names at its edge print inside it.
  lim <- p$coordinates$limits$x
  expect_gt(lim[2], max(out$deploy_lon))
  none <- plot_receivers(x, key = FALSE)
  expect_length(attr(plot_data(none), "key"), 0)
  expect_null(ggplot2::get_guide_data(none, "size"))
})

test_that("plot_receivers repels names off the points when ggrepel is installed", {
  skip_if_not_installed("ggrepel")
  x <- detset(hard_cases()$detections, hard_cases()$deployments, hard_cases()$animals)
  p <- plot_receivers(x)
  geoms <- layer_geoms(p)
  expect_true("GeomTextRepel" %in% geoms)
  expect_false("GeomText" %in% geoms)
  lay <- p$layers[[which(geoms == "GeomTextRepel")]]
  # Every station is in the layer, so names keep clear of every point; the
  # names drawn are those offered, most detections first.
  out <- plot_data(p)
  expect_equal(nrow(lay$data), nrow(out))
  expect_equal(lay$data$text[nzchar(lay$data$text)],
               out$station_id[order(-out$detections)][out$labelled[order(-out$detections)]])
  expect_equal(lay$data$point_mm, station_size(lay$data$detections, max(out$detections)))
  withr::local_pdf(NULL)
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
  # With fit = "heard", only stations that heard a detection carry names.
  heard <- plot_receivers(x, fit = "heard")
  hl <- heard$layers[[which(layer_geoms(heard) == "GeomTextRepel")]]
  expect_true(all(hl$data$detections[nzchar(hl$data$text)] > 0))
  expect_false("GeomTextRepel" %in% layer_geoms(plot_receivers(x, labels = FALSE)))
})

test_that("plot_abacus keys stations in natural order on turbo", {
  ex <- hard_cases()
  x <- detset(ex$detections, ex$deployments, ex$animals)
  ten <- paste0("TPS", 1:10)
  st <- sort(unique(x$detections$station_id))[1:10]
  keep <- x$detections$station_id %in% st
  x$detections <- x$detections[keep, ]
  x$detections$station_id <- ten[match(x$detections$station_id, st)]
  p <- plot_abacus(x)
  sc <- p$scales$get_scales("colour")
  expect_identical(sc$get_limits(), ten)
  cols <- sc$map(ten)
  turbo <- ggplot2::scale_colour_viridis_d(option = "turbo", begin = 0.05, end = 0.95)
  expect_equal(unname(cols), turbo$palette(10), ignore_attr = TRUE)
  expect_false(cols[1] == cols[2])
  rgb_gap <- function(a, b) sqrt(sum((grDevices::col2rgb(a) - grDevices::col2rgb(b))^2))
  expect_gt(min(vapply(1:9, function(i) rgb_gap(cols[i], cols[i + 1]), 0)), 40)
})

test_that("plot_timeline sets the stations that heard a detection apart", {
  x <- example_detset()
  heard <- unique(stats::na.omit(x$detections$station_id))
  quiet <- setdiff(unique(x$deployments$station_id), heard)
  if (!length(quiet)) {
    extra <- x$deployments[1, ]
    extra$station_id <- "ZZ-QUIET"
    extra$receiver_serial <- "VR2W-999999"
    x <- detset(x$detections, rbind(x$deployments, extra), x$animals)
    quiet <- "ZZ-QUIET"
  }
  p <- plot_timeline(x)
  b <- ggplot2::ggplot_build(p)
  seg <- b$data[[1]]
  st <- natural_sort(unique(plot_data(p)$station_id))
  lab <- st[seg$y]
  expect_true(all(seg$colour[lab %in% heard] == "grey30"))
  expect_true(all(seg$colour[lab %in% quiet] == "grey85"))
  key <- ggplot2::get_guide_data(p, "colour")$.label
  expect_true(all(c("Station heard a detection", "Station heard none") %in% key))
  # Every station heard: the bars are blue and the pair is not keyed.
  y <- detset(x$detections, x$deployments[x$deployments$station_id %in% heard, ], x$animals)
  ky <- ggplot2::get_guide_data(plot_timeline(y), "colour")$.label
  expect_false(any(c("Station heard a detection", "Station heard none") %in% ky))
})

test_that("above 60 stations only the stations that heard a detection are labelled", {
  ex <- hard_cases()
  d <- ex$deployments
  more <- do.call(rbind, lapply(1:6, function(i) {
    transform(d, station_id = paste0(station_id, "_", i),
              receiver_serial = paste0(receiver_serial, "_", i))
  }))
  x <- detset(ex$detections, rbind(d, more), ex$animals)
  p <- plot_timeline(x)
  st <- natural_sort(unique(plot_data(p)$station_id))
  expect_gt(length(st), 60)
  labs <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
  heard <- unique(stats::na.omit(x$detections$station_id))
  # Breaks sit only at the stations that heard a detection, so the overlap
  # check keeps their names.
  expect_setequal(labs, intersect(st, heard))
  # The arrows of open deployments are drawn only on those stations.
  arrows <- p$layers[[2]]$data
  expect_true(all(arrows$station_id %in% heard))
})

test_that("plot_timeline keeps deployments still in the water", {
  x <- audit_listening(example_detset(), reference_tags = data.frame(transmitter = "A69-1604-5012"))
  x$listening$listening[1:3] <- FALSE
  p <- plot_timeline(x)
  out <- plot_data(p)
  expect_equal(nrow(out), nrow(x$deployments))
  act <- out[out$active, ]
  expect_gt(nrow(act), 0)
  expect_true(all(act$drawn_to == max(x$detections$datetime)))
  # Bars, an arrow for each open deployment, and a red tick for each
  # receiver-day not listening.
  expect_identical(layer_geoms(p),
                   c("GeomSegment", "GeomSegment", "GeomText", "GeomText", "GeomBlank"))
  expect_false(is.null(p$layers[[2]]$geom_params$arrow))
  expect_equal(nrow(p$layers[[2]]$data), nrow(act))
  ticks <- p$layers[[4]]
  expect_equal(nrow(ticks$data), 3L)
  expect_identical(p$labels$x, "Date (UTC)")
  b <- ggplot2::ggplot_build(p)
  # The red ticks and the blue arrows are keyed.
  expect_true(all(b$data[[4]]$colour == "red"))
  expect_true(all(b$data[[2]]$colour == "steelblue"))
  key <- ggplot2::get_guide_data(p, "colour")
  expect_setequal(key$.label, c("Not recovered: drawn to the last detection",
                                "Receiver-day not listening (audit_listening())"))
  expect_true(p$scales$get_scales("y")$guide$params$check.overlap)
  expect_identical(b$layout$panel_params[[1]]$y$get_labels(),
                   natural_sort(unique(x$deployments$station_id)))
})

test_that("plot_attrition() draws one bar per log record, in the order the steps ran", {
  x <- example_detset()
  x <- filter_duplicates(x)
  x <- filter_post_release(x, hours = 6)
  p <- plot_attrition(x)
  expect_s3_class(p, "ggplot")
  lg <- detset_log(x)
  expect_equal(nrow(p$data), nrow(lg))
  expect_equal(p$data$rows_after, lg$rows_after)
  # Record 1 is the top bar: the last factor level
  expect_equal(levels(p$data$record)[nlevels(p$data$record)],
               paste(1, lg$step[1]))
  expect_false(any(p$data$breach))
  # The key names both kinds of bar, a breach included when there is none.
  expect_identical(ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")$get_labels(),
                   c("Record adds up", "Record breaks the log's arithmetic"))
  expect_error(plot_attrition(x$detections), "must be a detset")
})
