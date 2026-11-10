# ---- The whole pipeline, end to end, on the bundled data ----
# One script of the calls a user makes: detset() on the bundled tables (the
# readers feed it in the second test), check_detset(), the filters,
# audit_listening(), detect_mortality(), review_mortality(),
# apply_mortality_calls() and filter_mortality(), processing_report(), and
# the detfield hand-off: water_distances(), de_model(), bin_detections(),
# location_surface(), system_de() and sample_locations(). Each filter
# threshold is given, as no filter has a default.
#
# The thresholds are the defaults of positionRtools, the package detqc
# replaces: max_lag_s 3600, 3 m/s and 2,000 m. A change in any count pinned
# below is a change in what a module does to the data, and should be a
# deliberate one.

reviewer_calls_file <- function(x, cases, env = parent.frame()) {
  # The reviewer's calls, as the review index exports them: the truth of the
  # synthetic cases, for the animals still in the set.
  dead <- !is.na(cases$true_death_date)
  calls <- data.frame(animal_id = cases$animal_id, call = ifelse(dead, "dead_or_shed", "alive"),
                      death_date = ifelse(dead, format(cases$true_death_date), ""),
                      stringsAsFactors = FALSE)
  calls <- calls[calls$animal_id %in% x$detections$animal_id, ]
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  utils::write.csv(calls, f, row.names = FALSE)
  f
}

test_that("the whole detqc pipeline runs on the hard cases and every count adds up", {
  ex <- hard_cases()
  dir <- withr::local_tempdir()

  # ---- Clean the detections ----
  x <- detset(ex$detections, ex$deployments, ex$animals)
  x <- check_detset(x)
  x <- x |>
    filter_deployment_period() |>
    filter_release() |>
    filter_duplicates() |>
    filter_min_lag(max_lag_s = 3600) |>
    filter_speed(max_speed_ms = 3, min_dist_m = 2000) |>
    filter_tag_life()
  x <- audit_listening(x)

  # ---- Mortality: flag, review, apply ----
  bands <- detect_mortality(x)
  x <- review_mortality(x, dir, bands = bands, open = FALSE)
  index <- attr(x$cards, "index")
  x <- apply_mortality_calls(x, reviewer_calls_file(x, ex$cases))
  x <- filter_mortality(x, margin_days = 0)
  report <- processing_report(x, file.path(dir, "run_report.html"), bands = bands)

  # Every step is in the log, and the log's arithmetic holds at every step.
  lg <- detset_log(x)
  expect_equal(lg$step, c("detset", "check_detset", "filter_deployment_period", "filter_release",
                          "filter_duplicates", "filter_min_lag", "filter_speed", "filter_tag_life",
                          "audit_listening", "detect_mortality", "build_mortality_cards",
                          "apply_mortality_calls", "filter_mortality"))
  expect_equal(lg$rows_after, c(40506, 40506, 40506, 40506, 40506, 26568, 26568, 26565, 26565,
                                26565, 26565, 26565, 15526))
  expect_equal(lg$rows_after, lg$rows_before - lg$removed)
  expect_equal(lg$rows_before[-1], lg$rows_after[-nrow(lg)])
  expect_equal(nrow(log_breaches(x$log)), 0L)
  expect_equal(nrow(x$detections), lg$rows_after[nrow(lg)])

  # The triage sorts the twenty synthetic cases as built: 20 of 20.
  b <- merge(bands$bands[, c("animal_id", "band")], ex$cases, by = "animal_id")
  expect_equal(nrow(b), 20L)
  expect_equal(b$band, b$expected_band)

  # One card per animal in the dead and review bands, and an index.
  expect_true(file.exists(index))
  cards <- list.files(file.path(dir, "cards"), pattern = "\\.html$")
  expect_length(cards, sum(bands$bands$band %in% c("dead", "review")))

  # No detection of a dead animal survives its death date.
  d <- x$detections
  dd <- x$mortality$death_date[match(d$animal_id, x$mortality$animal_id)]
  expect_equal(sum(!is.na(dd) & as.Date(d$datetime) > dd), 0L)

  # Every number in the report traces to the detset.
  rc <- report_check(report, x, bands)
  expect_true(all(rc$traced$match))
  expect_length(rc$untraced, 0L)
  expect_equal(NROW(rc$invariants), 0L)
})

test_that("the bundled export files feed the same chain through the readers", {
  ex <- hard_cases()
  det <- read_detections(ext_file("vue_example.csv"))
  dep <- read_deployments(ext_file("glatos_receivers_example.csv"),
                          "glatos")
  x <- detset(det, dep, ex$animals)
  expect_equal(nrow(x$detections), nrow(det))
  x <- check_detset(x)
  x <- x |>
    filter_deployment_period() |>
    filter_release() |>
    filter_duplicates() |>
    filter_min_lag(max_lag_s = 3600) |>
    filter_speed(max_speed_ms = 3, min_dist_m = 2000) |>
    filter_tag_life()
  # The VUE detections are not of the example's animals, so every receiver
  # reads as silent and the audit says the set looks cut.
  expect_warning(x <- audit_listening(x), "the detections hold 0 of the 1 species")
  bands <- detect_mortality(x)
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, bands = bands)
  lg <- detset_log(x)
  expect_equal(nrow(lg), 9L)
  expect_equal(lg$rows_after, lg$rows_before - lg$removed)
  expect_equal(nrow(log_breaches(x$log)), 0L)
  rc <- report_check(f, x, bands)
  expect_true(all(rc$traced$match))
  expect_length(rc$untraced, 0L)
})

test_that("the detfield hand-off runs on the cleaned detset", {
  skip_if_not_installed("terra")
  skip_if_not_installed("detfield")
  water_distances <- detfield::water_distances
  de_model <- detfield::de_model
  de_logistic <- detfield::de_logistic
  de_field <- detfield::de_field
  bin_detections <- detfield::bin_detections
  location_surface <- detfield::location_surface
  system_de <- detfield::system_de
  sample_locations <- detfield::sample_locations
  ex <- hard_cases()
  x <- detset(ex$detections, ex$deployments, ex$animals) |>
    filter_deployment_period() |>
    filter_duplicates() |>
    filter_min_lag(max_lag_s = 3600) |>
    audit_listening()
  # hard_cases() carries no tag delays; 80 to 160 s is the V13AP default
  # programme of tag_specs_example.csv.
  x$animals$min_delay <- 80
  x$animals$max_delay <- 160

  # ---- Where were the fish ----
  # The synthetic array has no lake, so the grid is open water: 200 m cells
  # over the stations and 600 m around them, in UTM zone 17N.
  dep <- x$deployments
  st <- terra::project(terra::vect(dep, geom = c("deploy_lon", "deploy_lat"), crs = "EPSG:4326"),
                       "EPSG:26917")
  grid <- terra::rast(terra::ext(st) + 600, resolution = 200, crs = "EPSG:26917")
  terra::values(grid) <- 1
  xy <- terra::crds(st)
  keys <- c("receiver_serial", "station_id", "deploy_datetime", "recover_datetime")
  rcv <- data.frame(dep[, keys], x = xy[, 1], y = xy[, 2])
  dist <- suppressWarnings(water_distances(grid, rcv))
  model <- de_model(de_logistic(d50 = 250, d05 = 600), sigma = list(station_hour = 2.55))
  field <- de_field(dist, model)
  inp <- as_detfield_input(x, "days")
  # One animal-day keeps the test fast: EX-04 on its first detection day.
  day <- as.Date(min(inp$detections$datetime[inp$detections$animal_id %in% "EX-04"]))
  one <- inp$detections$animal_id %in% "EX-04" & as.Date(inp$detections$datetime) == day
  lst <- inp$listening[inp$listening$date == day, ]
  counts <- bin_detections(inp$detections[one, ], lst, inp$tag_delay, codes = "shared")
  surface <- location_surface(counts, field)
  cover <- system_de(field, lst)
  draws <- sample_locations(surface, n = 100, seed = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(surface)

  bins <- unique(surface$bin_start)
  expect_gt(length(bins), 0L)
  # location_surface() keeps the cells of each bin's 0.999 region,
  # so a bin's stored mass lies between 0.999 and 1.
  mass <- as.vector(tapply(surface$prob, surface$bin_start, sum))
  expect_true(all(mass >= 0.999 - 1e-9 & mass <= 1 + 1e-9))
  expect_equal(nrow(draws), 100L * length(bins))
  expect_true(all(cover$p >= 0 & cover$p <= 1))
  expect_equal(unique(cover$date), day)
})
