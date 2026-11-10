# ---- Example files in inst/extdata ----
# inst/extdata/README.md states these counts; the test keeps them true.

test_that("the Stoney raster holds the water cells the extdata README states", {
  skip_if_not_installed("terra")
  tif <- system.file("extdata", "stoney_depth.tif", package = "detqc")
  expect_true(nzchar(tif))
  r <- terra::rast(tif)
  expect_identical(terra::crs(r, describe = TRUE)$code, "32617")
  v <- terra::values(r, mat = FALSE)
  expect_identical(sum(!is.na(v)), 4818L)
  # Depth is zero or negative metres; 56 water cells hold 0.
  expect_true(all(v[!is.na(v)] <= 0))
  expect_identical(sum(v == 0, na.rm = TRUE), 56L)
})

test_that("the range test holds 8 transmitters on 1 receiver", {
  rt <- utils::read.csv(system.file("extdata", "stoney_range_test.csv", package = "detqc"))
  expect_identical(length(unique(rt$transmitter)), 8L)
  expect_identical(length(unique(rt$receiver_serial)), 1L)
})
