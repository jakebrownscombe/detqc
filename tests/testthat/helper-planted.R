# Planted-error fixtures for check_detset(): the clean base and one patch per
# check, written by fixtures/planted/make_planted.R.

planted_base <- function() readRDS(fixture(file.path("planted", "base.rds")))

planted_patch <- function(check) {
  readRDS(fixture(file.path("planted", paste0("patch_", check, ".rds"))))
}

apply_patch <- function(base, patch) {
  for (e in patch) {
    tb <- base[[e$table]]
    tb <- switch(e$op,
      set = {
        tb[[e$col]][e$rows] <- e$value
        tb
      },
      add = rbind(tb, e$data[, names(tb)]),
      drop = tb[-e$rows, , drop = FALSE],
      dup = tb[sort(c(seq_len(nrow(tb)), e$rows)), , drop = FALSE]
    )
    rownames(tb) <- NULL
    base[[e$table]] <- tb
  }
  base
}

# The water raster: depth in metres over the lake, NA on land.
planted_water <- function(base) {
  testthat::skip_if_not_installed("terra")
  lk <- base$lake
  r <- terra::rast(xmin = lk$extent[1], xmax = lk$extent[2], ymin = lk$extent[3],
                   ymax = lk$extent[4], resolution = lk$res, crs = "EPSG:4326")
  xy <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  wet <- xy[, 1] >= lk$lon[1] & xy[, 1] <= lk$lon[2] & xy[, 2] >= lk$lat[1] &
    xy[, 2] <= lk$lat[2]
  terra::values(r) <- ifelse(wet, lk$depth, NA)
  r
}

# The base spans three months, so the two checks of reused codes run at
# thresholds that fit it: one year at liberty, and a 20-day silence.
planted_settings <- function() check_settings(max_liberty_years = 1, reappear_gap_days = 20)

planted_checks <- function(base, water = planted_water(base), settings = planted_settings(),
                           ...) {
  tabs <- base[c("detections", "deployments", "animals")]
  check_detset(tabs, settings = settings, water = water, field = base$field_m,
               reference_tags = base$reference_tags, ...)
}

flagged_checks <- function(res) res$summary$check[res$summary$status == "flag"]
