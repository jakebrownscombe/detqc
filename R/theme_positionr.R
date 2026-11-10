# This file is shared: detfield/R/theme_positionr.R and detqc/R/theme_positionr.R
# are byte-identical, and the shared step of tools/check_all.sh compares them.
# Edit both copies the same way.

#' Receiver ring colours of the positionR maps
#'
#' Every positionR map draws a receiver as a ring: the viridis yellow for a
#' receiver that heard the animal, or for any receiver on a plain map; white
#' for a listening receiver that heard none; each over a dark edge ring one
#' step wider. The packages that draw receivers take the three colours from
#' here, so a map in one package matches a map in another.
#'
#' @return A named character vector of three hex colours: `heard`, `silent`
#'   and `edge`.
#' @export
#' @examples
#' positionr_ring_colours()
#' positionr_ring_colours()[["heard"]]
positionr_ring_colours <- function() {
  c(heard = "#FDE725", silent = "#FFFFFF", edge = "#1A1A1A")
}

#' The positionR figure theme
#'
#' `theme_positionr()` is the theme every positionR figure is drawn with:
#' [ggplot2::theme_minimal()] at its own text sizes, with its light grey major
#' grid and plain titles, and the legend on the right.
#'
#' A map adds to the theme [scale_fill_probability()] for a detection
#' probability or other probability surface, or [scale_fill_depth()] for depth,
#' receivers as hollow markers (shape 21) in a colour outside the fill
#' palette, equal coordinates ([ggplot2::coord_equal()]) and
#' [annotation_scale_bar()]. Other figures take
#' [ggplot2::scale_colour_viridis_d()] for groups and
#' [ggplot2::scale_colour_viridis_c()] for a continuous colour; a single series
#' is drawn in the viridis blue `"#31688E"`. Axis titles carry units, such as
#' "Distance through water (m)".
#'
#' @param base_size Base font size in points, as in
#'   [ggplot2::theme_minimal()]; titles and text are sized relative to it.
#' @param base_family Base font family.
#' @return A complete ggplot2 theme, to add to a plot with `+`.
#' @export
#' @examples
#' # The Stoney range model at receiver depths of 1 m and 25 m: d50 and d05
#' # are 250 m and 500 m at 1 m, 750 m and 1,500 m at 35 m, linear between.
#' curve <- expand.grid(distance = seq(0, 1600, by = 10), depth = c(1, 25))
#' d50 <- 250 + (curve$depth - 1) * 500 / 34
#' d05 <- 500 + (curve$depth - 1) * 1000 / 34
#' logit <- stats::qlogis(0.05) * (curve$distance - d50) / (d05 - d50)
#' curve$p <- stats::plogis(logit)
#' ggplot2::ggplot(curve, ggplot2::aes(distance, p, colour = factor(depth))) +
#'   ggplot2::geom_line(linewidth = 1) +
#'   ggplot2::scale_colour_viridis_d(name = "Receiver depth (m)", end = 0.7) +
#'   ggplot2::labs(x = "Distance through water (m)",
#'                 y = "Detection probability") +
#'   theme_positionr()
theme_positionr <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(legend.position = "right")
}

#' Depth scale for a map
#'
#' The fill scale of every positionR depth map: cornflower blue in the
#' shallows to dark blue (`"blue4"`) at the deepest cell, so deeper water reads
#' darker. Depth is in metres, stored either way: positive downward, or as
#' negative numbers (elevation below the surface, as many bathymetry rasters
#' store it). When every value is 0 or less the ramp runs the other way, so the
#' most negative cell, the deepest, is still blue4.
#'
#' @param name Title of the key.
#' @param ... Passed to [ggplot2::scale_fill_gradient()], such as `limits`
#'   or `na.value`.
#' @return A ggplot2 fill scale, to add to a plot with `+`.
#' @export
#' @examples
#' # Stoney Lake, Ontario (100 m cells, depth in metres, zero or negative, land
#' # NA). detfield and detqc each ship a copy of the raster.
#' tif <- system.file("extdata", "stoney_depth.tif", package = "detfield")
#' if (!nzchar(tif)) {
#'   tif <- system.file("extdata", "stoney_depth.tif", package = "detqc")
#' }
#' lake <- terra::as.data.frame(abs(terra::rast(tif)), xy = TRUE)
#' names(lake)[3] <- "depth"
#' ggplot2::ggplot(lake, ggplot2::aes(x, y, fill = depth)) +
#'   ggplot2::geom_raster() +
#'   scale_fill_depth() +
#'   ggplot2::coord_equal() +
#'   ggplot2::labs(x = "Easting (m, UTM 17N)", y = "Northing (m, UTM 17N)") +
#'   annotation_scale_bar(location = "br") +
#'   theme_positionr()
scale_fill_depth <- function(name = "Depth (m)", ...) {
  ggplot2::scale_fill_gradient(name = name, low = "cornflowerblue", high = "blue4",
                               rescaler = depth_rescaler, ...)
}

# Rescales depth to 0 (shallowest) and 1 (deepest) whichever sign it is
# stored with: when every value is 0 or less, the most negative is deepest.
depth_rescaler <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE)) {
  if (all(from <= 0)) to <- rev(to)
  if (diff(from) == 0) return(ifelse(is.na(x), NA_real_, mean(to)))
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

#' Probability scale for a map
#'
#' The fill scale of every positionR map of a probability: the magma option
#' of [ggplot2::scale_fill_viridis_c()], near-black at 0 to pale yellow at 1,
#' on the fixed limits 0 to 1, so two maps of detection probability read on
#' the same colours. Cells with no value are not drawn. Points or outlines
#' drawn over it need a colour outside magma with a dark edge, such as the
#' viridis yellow `"#FDE725"` over a dark ring, so they read at both ends.
#'
#' @param name Title of the key.
#' @param limits Limits of the scale. Keep `c(0, 1)` for a detection
#'   probability; `NULL` spans the data, for a surface whose cells hold a
#'   small share each, such as a location surface.
#' @param na_value Fill of cells with no value; `NA` draws none.
#' @param ... Passed to [ggplot2::scale_fill_viridis_c()], such as `breaks`.
#' @return A ggplot2 fill scale, to add to a plot with `+`.
#' @export
#' @examples
#' # Stoney Lake, Ontario (100 m cells, depth in metres, zero or negative, land
#' # NA). detfield and detqc each ship a copy of the raster.
#' tif <- system.file("extdata", "stoney_depth.tif", package = "detfield")
#' if (!nzchar(tif)) {
#'   tif <- system.file("extdata", "stoney_depth.tif", package = "detqc")
#' }
#' lake <- terra::as.data.frame(abs(terra::rast(tif)), xy = TRUE)
#' names(lake)[3] <- "depth"
#' # Detection probability of one transmission from each cell for a receiver
#' # in the deepest cell, on a logistic curve with d50 600 m and d05 1,200 m,
#' # by straight-line distance.
#' rx <- lake[which.max(lake$depth), c("x", "y")]
#' d <- sqrt((lake$x - rx$x)^2 + (lake$y - rx$y)^2)
#' lake$p <- stats::plogis(stats::qlogis(0.05) * (d - 600) / 600)
#' ggplot2::ggplot(lake, ggplot2::aes(x, y, fill = p)) +
#'   ggplot2::geom_raster() +
#'   scale_fill_probability() +
#'   ggplot2::coord_equal() +
#'   ggplot2::labs(x = "Easting (m, UTM 17N)", y = "Northing (m, UTM 17N)") +
#'   annotation_scale_bar(location = "br") +
#'   theme_positionr()
scale_fill_probability <- function(name = "Probability", limits = c(0, 1), na_value = NA, ...) {
  ggplot2::scale_fill_viridis_c(name = name, option = "magma", limits = limits,
                                na.value = na_value, ...)
}

# detqc and detfield share this file. The two checks below raise the error
# class of the package they are installed in, and each names the user's call,
# so the message reads as the rest of the package.
check_location <- function(location, call = sys.call(-1)) {
  corners <- c("bl", "br", "tl", "tr")
  if (is.character(location) && length(location) == 1 && location %in% corners) {
    return(invisible(TRUE))
  }
  stop(structure(
    class = c(paste0(utils::packageName(), "_error"), "error", "condition"),
    list(message = paste0("location must be one of ",
                          paste0('"', corners, '"', collapse = ", "), "."),
         call = call)
  ))
}

check_width_hint <- function(width_hint, call = sys.call(-1)) {
  ok <- is.numeric(width_hint) && length(width_hint) == 1 && !is.na(width_hint) &&
    width_hint > 0 && width_hint < 1
  if (ok) return(invisible(TRUE))
  stop(structure(
    class = c(paste0(utils::packageName(), "_error"), "error", "condition"),
    list(message = "width_hint must be one number between 0 and 1 (exclusive).", call = call)
  ))
}

#' Scale bar for a map
#'
#' Adds a scale bar to each panel of a map: a bar of two segments, black and
#' white, with its length written above it, on a white box at 80% opacity so
#' that it reads on a dark surface. The length is a round number (1, 2 or 5
#' times a power of ten) no longer than `width_hint` of the panel width, so
#' each panel of a faceted map with free scales gets its own bar.
#'
#' @param location Corner of the panel: `"bl"` (bottom left), `"br"`, `"tl"`
#'   or `"tr"`.
#' @param width_hint Longest bar, as a share of the panel width.
#' @param lonlat `FALSE` when x and y are projected coordinates in metres.
#'   `TRUE` when they are longitude and latitude in degrees; the length is
#'   then measured along the middle latitude of the panel, at 111.195 km a
#'   degree of latitude.
#' @param text_size Size of the length label in mm, as `size` in
#'   [ggplot2::geom_text()].
#' @return A list of two ggplot2 layers, to add to a plot with `+`.
#' @export
#' @examples
#' # Stoney Lake, Ontario (100 m cells, depth in metres, zero or negative, land
#' # NA). detfield and detqc each ship a copy of the raster.
#' tif <- system.file("extdata", "stoney_depth.tif", package = "detfield")
#' if (!nzchar(tif)) {
#'   tif <- system.file("extdata", "stoney_depth.tif", package = "detqc")
#' }
#' lake <- terra::as.data.frame(abs(terra::rast(tif)), xy = TRUE)
#' names(lake)[3] <- "depth"
#' # Bottom right, where Stoney Lake has no water.
#' ggplot2::ggplot(lake, ggplot2::aes(x, y, fill = depth)) +
#'   ggplot2::geom_raster() +
#'   scale_fill_depth() +
#'   ggplot2::coord_equal() +
#'   ggplot2::labs(x = "Easting (m, UTM 17N)", y = "Northing (m, UTM 17N)") +
#'   annotation_scale_bar(location = "br") +
#'   theme_positionr()
annotation_scale_bar <- function(location = "bl", width_hint = 0.15, lonlat = FALSE,
                                 text_size = 3) {
  check_location(location)
  check_width_hint(width_hint)
  bar <- list(location = location, width_hint = width_hint, lonlat = isTRUE(lonlat))
  one_layer <- function(geom, params) {
    ggplot2::layer(
      geom = geom, stat = "identity", position = "identity",
      data = data.frame(scale_bar = 1), mapping = NULL,
      inherit.aes = FALSE, show.legend = FALSE, params = params
    )
  }
  bar$text_size <- text_size
  list(
    one_layer(scale_bar_rect_geom, bar),
    one_layer(scale_bar_text_geom, bar)
  )
}

# ---- Scale bar internals ----
# The bar is sized from the panel's own x and y ranges at draw time, so it
# fits the zoom of each panel. coord_sf() names the ranges x_range and
# y_range; the Cartesian coords name them x.range and y.range.
scale_bar_parts <- function(panel_params, location, width_hint, lonlat) {
  xr <- if (is.null(panel_params$x_range)) panel_params$x.range else panel_params$x_range
  yr <- if (is.null(panel_params$y_range)) panel_params$y.range else panel_params$y_range
  dx <- diff(xr)
  dy <- diff(yr)
  metres_per_x <- if (lonlat) 111195 * cos(mean(yr) * pi / 180) else 1
  longest <- dx * width_hint * metres_per_x
  step <- 10^floor(log10(longest))
  metres <- step * max(c(1, 2, 5)[c(1, 2, 5) * step <= longest])
  width <- metres / metres_per_x
  height <- 0.015 * dy
  x0 <- if (substr(location, 2, 2) == "l") xr[1] + 0.05 * dx else xr[2] - 0.05 * dx - width
  y0 <- if (substr(location, 1, 1) == "b") yr[1] + 0.05 * dy else yr[2] - 0.12 * dy
  label <- if (metres >= 1000) paste(format(metres / 1000), "km") else paste(format(metres), "m")
  list(
    rect = data.frame(
      xmin = x0 + c(0, width / 2), xmax = x0 + c(width / 2, width),
      ymin = y0, ymax = y0 + height, fill = c("black", "white"), colour = "black",
      linewidth = 0.3, linetype = 1, alpha = NA, group = 1:2
    ),
    text = data.frame(
      x = x0 + width / 2, y = y0 + height + 0.01 * dy, label = label, colour = "black",
      angle = 0, hjust = 0.5, vjust = 0, alpha = NA, family = "", fontface = 1,
      lineheight = 1.2, group = 1
    ),
    metres = metres
  )
}

scale_bar_rect_geom <- ggplot2::ggproto(
  "ScaleBarRect", ggplot2::Geom,
  draw_panel = function(data, panel_params, coord, location = "bl", width_hint = 0.15,
                        lonlat = FALSE, text_size = 3) {
    parts <- scale_bar_parts(panel_params, location, width_hint, lonlat)
    rect <- parts$rect
    rect$PANEL <- data$PANEL[1]
    grid::grobTree(
      scale_bar_box(parts, panel_params, coord, text_size),
      ggplot2::GeomRect$draw_panel(rect, panel_params, coord)
    )
  }
)

# The white box behind the bar and its label. The bar is placed in data
# units, but the label is sized in mm, so the box is built in grid units: the
# bar's extent in npc, widened to the label's width when the label is wider,
# and raised by the label's height. It is padded by 1% of the panel.
scale_bar_box <- function(parts, panel_params, coord, text_size) {
  corners <- coord$transform(data.frame(
    x = c(parts$rect$xmin[1], parts$rect$xmax[2], parts$text$x),
    y = c(parts$rect$ymin[1], parts$rect$ymax[2], parts$text$y)
  ), panel_params)
  pad <- grid::unit(0.01, "npc")
  label <- grid::textGrob(parts$text$label, gp = grid::gpar(fontsize = text_size * ggplot2::.pt))
  half <- max(grid::unit(diff(corners$x[1:2]) / 2, "npc"), 0.5 * grid::grobWidth(label))
  grid::rectGrob(
    x = grid::unit(mean(corners$x[1:2]), "npc") - half - pad,
    y = grid::unit(corners$y[1], "npc") - pad,
    width = 2 * half + 2 * pad,
    height = grid::unit(corners$y[3] - corners$y[1], "npc") + grid::grobHeight(label) + 2 * pad,
    just = c("left", "bottom"),
    gp = grid::gpar(fill = ggplot2::alpha("white", 0.8), col = NA)
  )
}

scale_bar_text_geom <- ggplot2::ggproto(
  "ScaleBarText", ggplot2::Geom,
  draw_panel = function(data, panel_params, coord, location = "bl", width_hint = 0.15,
                        lonlat = FALSE, text_size = 3) {
    text <- scale_bar_parts(panel_params, location, width_hint, lonlat)$text
    text$PANEL <- data$PANEL[1]
    text$size <- text_size
    ggplot2::GeomText$draw_panel(text, panel_params, coord)
  }
)
