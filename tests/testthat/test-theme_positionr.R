# ---- theme_positionr ----
test_that("theme_positionr() is a complete ggplot2 theme", {
  th <- theme_positionr()
  expect_true(inherits(th, "theme"))
  expect_true(isTRUE(attr(th, "complete")))
})

test_that("theme_positionr() is theme_minimal() with the legend on the right", {
  th <- theme_positionr()
  base <- ggplot2::theme_minimal()
  el <- function(name, theme) ggplot2::calc_element(name, theme)
  for (name in c("plot.title", "axis.title.x", "axis.text.y", "legend.title", "legend.text",
                 "strip.text.x", "panel.grid.major.x", "panel.grid.minor.y")) {
    expect_equal(el(name, th), el(name, base), info = name)
  }
  expect_true(inherits(el("panel.grid.major.x", th), "element_line"))
  expect_identical(el("plot.title", th)$face, "plain")
  expect_equal(el("plot.title", th)$size, 13.2)
  expect_equal(el("axis.title.x", th)$size, 11)
  expect_identical(th$legend.position, "right")
})

test_that("theme_positionr() scales its text with base_size", {
  th <- theme_positionr(base_size = 22)
  expect_equal(ggplot2::calc_element("axis.title.y", th)$size, 22)
  expect_equal(ggplot2::calc_element("axis.text.x", th)$size, 17.6)
})

# ---- positionr_ring_colours ----
test_that("positionr_ring_colours() gives the heard, silent and edge colours", {
  ring_col <- positionr_ring_colours()
  expect_identical(ring_col, c(heard = "#FDE725", silent = "#FFFFFF", edge = "#1A1A1A"))
  # The heard ring is the yellow end of viridis.
  skip_if_not_installed("scales")
  expect_identical(ring_col[["heard"]], toupper(substr(scales::viridis_pal()(2)[2], 1, 7)))
})

# ---- scale_fill_depth ----
test_that("scale_fill_depth() runs from cornflower blue in the shallows to blue4", {
  sc <- scale_fill_depth()
  expect_s3_class(sc, "ScaleContinuous")
  expect_identical(sc$name, "Depth (m)")
  sc$train(c(0, 30))
  ends <- sc$map(c(0, 30))
  expect_identical(toupper(ends), toupper(c("#6495ED", "#00008B")))
  expect_identical(scale_fill_depth(name = "Water depth (m)")$name, "Water depth (m)")
  # Depth stored as negative numbers, as the bundled Stoney raster stores it:
  # the deepest (most negative) cell is still blue4, the shallowest cornflower.
  neg <- scale_fill_depth()
  neg$train(c(-32.5, 0))
  expect_identical(toupper(neg$map(c(0, -32.5))), toupper(c("#6495ED", "#00008B")))
})

# ---- scale_fill_probability ----
test_that("scale_fill_probability() is magma on the fixed limits 0 to 1", {
  sc <- scale_fill_probability()
  expect_s3_class(sc, "ScaleContinuous")
  expect_identical(sc$name, "Probability")
  expect_equal(sc$limits, c(0, 1))
  expect_true(is.na(sc$na.value))
  # Trained on 0.2 to 0.6 the ends are still the magma ends at 0 and 1.
  sc$train(c(0.2, 0.6))
  # The magma ends: near-black at 0, pale yellow at 1.
  expect_identical(toupper(substr(sc$map(c(0, 1)), 1, 7)), c("#000004", "#FCFDBF"))
  open <- scale_fill_probability(limits = NULL, name = "Share")
  expect_null(open$limits)
  expect_identical(open$name, "Share")
})

# ---- annotation_scale_bar ----
test_that("the scale bar is a round length no longer than width_hint of the panel", {
  bar <- scale_bar_parts(list(x.range = c(0, 2000), y.range = c(0, 1000)), "bl", 0.15, FALSE)
  expect_equal(bar$metres, 200)
  expect_identical(bar$text$label, "200 m")
  expect_equal(bar$rect$xmax[2] - bar$rect$xmin[1], 200)
  wide <- scale_bar_parts(list(x_range = c(0, 40000), y_range = c(0, 20000)), "tr", 0.15, FALSE)
  expect_identical(wide$text$label, "5 km")
  expect_equal(wide$rect$xmax[2], 40000 - 0.05 * 40000)
})

test_that("the scale bar measures longitude along the middle latitude", {
  pp <- list(x.range = c(-64, -63.9), y.range = c(44, 44.1))
  bar <- scale_bar_parts(pp, "bl", 0.15, TRUE)
  metres_per_degree <- 111195 * cos(44.05 * pi / 180)
  expect_identical(bar$text$label, "1 km")
  expect_equal((bar$rect$xmax[2] - bar$rect$xmin[1]) * metres_per_degree, 1000)
})

test_that("annotation_scale_bar() draws on every panel of a map", {
  withr::local_pdf(NULL)
  lake <- expand.grid(x = seq(0, 3000, by = 100), y = seq(0, 1500, by = 100))
  lake$side <- ifelse(lake$x < 1500, "west", "east")
  p <- ggplot2::ggplot(lake, ggplot2::aes(x, y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = y)) +
    ggplot2::facet_wrap(~side, scales = "free") +
    ggplot2::coord_cartesian() +
    annotation_scale_bar() +
    theme_positionr()
  built <- ggplot2::ggplot_build(p)
  expect_length(annotation_scale_bar(), 2)
  expect_equal(nrow(built$data[[2]]), 2)
  grob <- ggplot2::ggplotGrob(p)
  expect_s3_class(grob, "gtable")
  # Each panel's bar sits on a white box at 80% opacity.
  # A gtable keeps its grobs in $grobs and a gTree in $children, so the
  # rectangles are collected by walking both.
  fills <- function(g) {
    own <- if (inherits(g, "rect")) g$gp$fill
    kids <- c(if (inherits(g, "gtable")) g$grobs, if (inherits(g, "gTree")) g$children)
    c(own, unlist(lapply(kids, fills)))
  }
  expect_equal(sum(fills(grob) == ggplot2::alpha("white", 0.8)), 2)
  expect_error(annotation_scale_bar(location = "middle"))
  expect_error(annotation_scale_bar(width_hint = 2))
})
