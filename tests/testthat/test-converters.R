# One of the three slowest test files of the package. CRAN limits check
# time; devtools::test() runs this file in full.
skip_on_cran()

# Converters to glatos, actel, ATO and detfield, and the way back.

example_x <- function() {
  ex <- hard_cases()
  detset(ex$detections, ex$deployments, ex$animals)
}

test_that("to_glatos() returns every detection with the columns glatos reads", {
  x <- example_x()
  g <- to_glatos(x)
  expect_s3_class(g, "glatos_detections")
  expect_equal(nrow(g), nrow(x$detections))
  expect_true(all(c("animal_id", "detection_timestamp_utc", "deploy_lat", "deploy_long",
                    "transmitter_codespace", "transmitter_id", "receiver_sn",
                    "glatos_array") %in% names(g)))
  expect_false(anyNA(g$deploy_lat))
  expect_equal(g$transmitter_codespace[1], "A69-1604")
  expect_equal(g$transmitter_id[1], "5001")
  expect_equal(sum(g$sensor_unit %in% "ADC"), sum(!is.na(x$detections$sensor_raw)))
  expect_equal(unique(g$common_name_e[!is.na(g$animal_id)]), "Walleye")
  expect_equal(split_transmitter(c("A69-1601-1", "PTT", NA))$id, c("1", "PTT", NA))
})

test_that("glatos reads to_glatos() output with its own functions", {
  skip_if_not_installed("glatos", "0.9.8")
  validate <- glatos::validate_glatos_detections
  g <- to_glatos(example_x())
  expect_true(validate(g))
  ml <- glatos::min_lag(g)
  expect_equal(nrow(ml), nrow(g))
  fd <- suppressMessages(glatos::false_detections(g, tf = 3600, show_plot = FALSE))
  expect_equal(nrow(fd), nrow(g))
  sm <- glatos::summarize_detections(g)
  expect_equal(nrow(sm), length(unique(stats::na.omit(g$animal_id))))
  events <- glatos::detection_events
  ev <- suppressMessages(events(g, location_col = "station"))
  expect_gt(nrow(ev), 0)
})

test_that("glatos detections come back with no row lost", {
  x <- example_x()
  y <- read_detset_from(to_glatos(x))
  expect_equal(nrow(y$detections), nrow(x$detections))
  key <- function(d) sort(paste(format(d$datetime), d$transmitter, d$receiver_serial, d$animal_id))
  expect_identical(key(y$detections), key(x$detections))
  expect_match(detset_log(y)$reason[2], "deployments \\(inferred from the detections\\)")
  # With the receiver table, the deployments are the real ones.
  rcv <- utils::read.csv(ext_file("glatos_receivers_example.csv"), colClasses = "character")
  z <- read_detset_from(to_glatos(x), receivers = rcv)
  expect_equal(nrow(z$deployments), 25L)
  expect_match(detset_log(z)$reason[2], "from the receiver table")
  expect_error(read_detset_from(to_glatos(x), receivers = rcv[, 1:3]), "has no column")
  expect_error(read_detset_from(list(1)), "must be glatos detections")
})

test_that("glatos's own example detections read into a detset", {
  skip_if_not_installed("glatos", "0.9.8")
  read_g <- glatos::read_glatos_detections
  g <- read_g(glatos_file("walleye_detections.csv"))
  rcv <- glatos::read_glatos_receivers(glatos_file("sample_receivers.csv"))
  y <- suppressWarnings(read_detset_from(g, receivers = rcv))
  expect_equal(nrow(y$detections), nrow(g))
  expect_equal(nrow(y$animals), length(unique(paste(g$animal_id, g$transmitter_id))))
})

test_that("to_actel() writes four tables in actel's times", {
  x <- example_x()
  expect_warning(a <- to_actel(x, tz = "America/Toronto"), "two code spaces, such as EX-18")
  expect_named(a, c("biometrics", "spatial", "deployments", "detections"))
  expect_equal(nrow(a$detections), nrow(x$detections))
  # Detections stay in UTC; release and deployment times move to local time.
  expect_identical(attr(a$detections$Timestamp, "tzone"), "UTC")
  expect_identical(attr(a$biometrics$Release.date, "tzone"), "America/Toronto")
  expect_equal(format(a$biometrics$Release.date[1]), "2023-05-01 11:00:00")
  expect_equal(as.numeric(a$deployments$Start[1]), as.numeric(x$deployments$deploy_datetime[1]))
  expect_equal(attr(a, "open_deployments"), 1L)
  expect_equal(unname(attr(a, "dropped")), c(0L, 0L))
  expect_equal(unique(a$spatial$Array), "A1")
  expect_equal(unique(a$spatial$Section), "All")
  expect_equal(sum(a$biometrics$animal_id == "EX-16"), 1L)
  expect_equal(a$biometrics$Signal[a$biometrics$animal_id == "EX-16"], "7004|8001")
  expect_error(to_actel(x), "argument \"tz\" is missing")
  expect_error(to_actel(detset(x$detections, x$deployments), "UTC"), "needs biometrics")
  st <- sort(unique(x$deployments$station_id))
  arr <- stats::setNames(paste0("A", seq_along(st)), st)
  b <- suppressWarnings(to_actel(x, "UTC", arrays = arr))
  expect_equal(length(unique(b$spatial$Array)), length(st))
  expect_error(suppressWarnings(to_actel(x, "UTC", arrays = arr[-1])), "no array for station")
  expect_error(suppressWarnings(to_actel(x, "UTC", arrays = 1:3)), "named by station_id")
})

test_that("actel::preload() runs on to_actel() output and the detset comes back whole", {
  skip_if_not_installed("actel")
  preload <- actel::preload
  x <- example_x()
  tz <- "America/Toronto"
  a <- suppressWarnings(to_actel(x, tz))
  withr::local_dir(withr::local_tempdir())
  warned <- character()
  p <- withCallingHandlers(suppressMessages(do.call(preload, c(a, list(tz = tz)))),
                           warning = function(w) {
                             warned <<- c(warned, conditionMessage(w))
                             invokeRestart("muffleWarning")
                           })
  # The spatial table names its section, so actel:::loadSpatial does not warn.
  expect_false(any(grepl("Section", warned, fixed = TRUE)))
  expect_identical(as.character(unique(p$spatial$stations$Section)), "All")
  expect_equal(sum(vapply(p$detections.list, nrow, integer(1))), nrow(x$detections))
  y <- read_detset_from(p)
  key <- function(d) sort(paste(format(d$datetime), d$transmitter, d$station_id, d$animal_id))
  expect_identical(key(y$detections), key(x$detections))
  expect_equal(nrow(y$deployments), nrow(x$deployments))
  expect_equal(sort(unique(y$animals$animal_id)), sort(unique(x$animals$animal_id)))
  expect_match(detset_log(y)$reason[2], "read from an actel preload")
})

test_that("release positions become actel release sites", {
  x <- example_x()
  x$animals$release_lat <- ifelse(x$animals$animal_id %in% c("EX-01", "EX-02"), 44.55, 44.6)
  x$animals$release_lon <- -78.1
  a <- suppressWarnings(to_actel(x, "UTC"))
  expect_equal(sum(a$spatial$Type == "Release"), 2L)
  expect_setequal(unique(a$biometrics$Release.site), c("R1", "R2"))
  x$animals$release_datetime[1] <- NA
  expect_error(suppressWarnings(to_actel(x, "UTC")), "needs a release time for every animal")
})

test_that("to_ato() writes the ATO names and tz = UTC", {
  x <- example_x()
  x$animals$min_delay <- 80
  x$animals$max_delay <- 160
  a <- to_ato(x)
  expect_named(a, c("det", "dep", "tag", "ani"))
  expect_equal(nrow(a$det), nrow(x$detections))
  expect_true(all(c("datetime", "receiver_serial", "transmitter", "tz", "animal_id") %in%
                    names(a$det)))
  expect_equal(unique(c(a$det$tz, a$dep$tz, a$tag$tz, a$ani$tz)), "UTC")
  expect_equal(unique(a$tag$ping_rate), 120)
  expect_equal(unique(a$tag$ping_variation), 40)
  expect_equal(a$tag$model[1], "V13-1x")
  expect_equal(nrow(a$ani), 20L)
  expect_equal(attr(a, "open_deployments"), 1L)
  expect_false(anyNA(a$dep$recover_datetime))
  expect_equal(sum(!is.na(a$tag$sensor_unit)), sum(!is.na(x$animals$sensor_unit)))
  expect_error(to_ato(x, as = "ATO"), "is not supported")
  bare <- to_ato(detset(x$detections, x$deployments))
  expect_equal(nrow(bare$ani), 0L)
  expect_equal(nrow(bare$tag), length(unique(x$detections$transmitter)))
})

test_that("as_detfield_input() gives bin_detections() its three inputs", {
  x <- example_x()
  expect_message(inp <- as_detfield_input(x), "tag_delay is NULL")
  expect_named(inp, c("detections", "listening", "tag_delay"))
  expect_named(inp$detections, c("datetime", "receiver_serial", "transmitter", "animal_id",
                                 "station_id"))
  expect_named(inp$listening, c("deployments", "days"))
  x$animals$min_delay <- 80
  x$animals$max_delay <- 160
  # A silent day listened; a day not downloaded did not.
  x$listening$status <- rep_len(c("listening", "silent", "not_downloaded", "reference_silent"),
                                nrow(x$listening))
  inp <- as_detfield_input(x, "days")
  expect_equal(sum(!inp$listening$listening),
               sum(x$listening$status %in% c("not_downloaded", "reference_silent")))
  x$listening$listening <- TRUE
  expect_true(all(as_detfield_input(x, "days")$listening$listening))
  x$listening$listening <- NULL
  expect_equal(nrow(inp$tag_delay), nrow(x$animals))
  expect_named(as_detfield_input(x, "deployments")$listening,
               c("receiver_serial", "station_id", "deploy_datetime", "recover_datetime"))
  skip_if_not_installed("detfield")
  bin <- detfield::bin_detections
  counts <- suppressWarnings(do.call(bin, c(as_detfield_input(x, "days"), list(codes = "shared"))))
  expect_gt(nrow(counts), 0)
})

test_that("the actel round trip of the exporting vignette runs where actel is installed", {
  skip_if_not_installed("actel")
  in_file <- ext_file
  x <- detset(read_detections(in_file("otn_detections_example.csv")),
              read_deployments(in_file("glatos_receivers_example.csv"), format = "glatos"),
              read_animals(in_file("glatos_detections_example.csv"), format = "glatos"))
  tz <- "America/Toronto"
  a <- to_actel(x, tz = tz)
  withr::local_dir(withr::local_tempdir())
  # actel removes a sink it did not open, which capture.output() reports.
  suppressWarnings(utils::capture.output(pre <- suppressMessages(
    do.call(actel::preload, c(a, list(tz = tz)))
  )))
  y <- read_detset_from(pre, receivers = attr(a, "receivers"))
  expect_equal(nrow(y$detections), nrow(x$detections))
  expect_setequal(unique(y$detections$transmitter), unique(x$detections$transmitter))
  expect_equal(detset_log(y)$step, c("detset", "read_detset_from"))
})

# A deployment sheet shaped like a real one: a receiver redeployed at its
# station, an open deployment followed by a later one of its receiver, a
# closed deployment that runs past the next one of its receiver, and an
# open deployment that starts after the last detection. The two dual-code
# animals are left out, so every animal sends one code, and a satellite tag
# with no acoustic code space is added, as OTN tag release files list them.
real_shaped_x <- function() {
  ex <- hard_cases()
  dep <- ex$deployments
  t <- function(s) as.POSIXct(s, tz = "UTC")
  g01 <- dep[dep$station_id == "G01", ]
  dep$recover_datetime[dep$station_id == "G01"] <- t("2023-07-01 12:00:00")
  g01$deploy_datetime <- t("2023-07-01 12:00:00")
  dep$recover_datetime[dep$station_id == "G02"] <- NA
  g02 <- dep[dep$station_id == "G02", ]
  g02$deploy_datetime <- t("2023-08-01 12:00:00")
  g02$recover_datetime <- t("2023-11-05 12:00:00")
  g03 <- dep[dep$station_id == "G03", ]
  g03$deploy_datetime <- t("2023-09-01 12:00:00")
  late <- dep[dep$station_id == "G20", ]
  late$receiver_serial <- "VR2Tx-480099"
  late$station_id <- "G99"
  late$deploy_datetime <- t("2023-12-01 12:00:00")
  dep <- rbind(dep, g01, g02, g03, late)
  one <- ex$animals[!ex$animals$animal_id %in% c("EX-16", "EX-18"), ]
  sat <- one[1, ]
  sat$animal_id <- "SAT-01"
  sat$transmitter <- "152092"
  det <- ex$detections[!ex$detections$animal_id %in% c("EX-16", "EX-18"), ]
  suppressWarnings(detset(det, dep, rbind(one, sat)))
}

test_that("exported deployments start before they stop, no overlap", {
  x <- real_shaped_x()
  expect_warning(expect_warning(a <- to_actel(x, "UTC"), "1 that ran past the next deployment"),
                 "no acoustic code space")
  b <- suppressWarnings(to_ato(x))
  for (d in list(data.frame(rx = a$deployments$Receiver, start = a$deployments$Start,
                            stop = a$deployments$Stop),
                 data.frame(rx = b$dep$receiver_serial, start = b$dep$deploy_datetime,
                            stop = b$dep$recover_datetime))) {
    expect_equal(sum(d$stop <= d$start), 0L)
    d <- d[order(d$rx, d$start), ]
    same <- d$rx[-1] == d$rx[-nrow(d)]
    expect_equal(sum(d$start[-1][same] < d$stop[-nrow(d)][same]), 0L)
  }
  n <- attr(a, "deployments")
  expect_equal(unname(n[c("deployments", "open", "open_to_next", "open_to_last", "trimmed",
                          "outside", "exported")]), c(29L, 3L, 1L, 2L, 1L, 1L, 28L))
  expect_identical(attr(b, "deployments"), n)
  expect_false("G99" %in% a$spatial$Station.name)
  # Every detection falls inside an exported ATO deployment of its receiver,
  # under the half-open rule.
  dep <- b$dep
  inside <- vapply(seq_len(nrow(b$det)), function(i) {
    k <- dep$receiver_serial == b$det$receiver_serial[i]
    any(dep$deploy_datetime[k] <= b$det$datetime[i] & b$det$datetime[i] < dep$recover_datetime[k])
  }, logical(1))
  expect_equal(sum(!inside), 0L)
})

test_that("single-code Signal is integer; satellite tags left out", {
  x <- real_shaped_x()
  a <- suppressWarnings(to_actel(x, "UTC"))
  expect_type(a$biometrics$Signal, "integer")
  expect_false("SAT-01" %in% a$biometrics$animal_id)
  expect_equal(attr(a, "dropped_animals"), c(no_acoustic_code = 1L))
  expect_warning(expect_warning(to_actel(x, "UTC"), "leaves out 1 of 19 animals .* SAT-01"),
                 "bounds the deployments")
  # Two codes on one animal keep the text form actel reads.
  y <- suppressWarnings(to_actel(example_x(), "UTC"))
  expect_type(y$biometrics$Signal, "character")
})

test_that("actel reads a real-shaped sheet, and serials come back", {
  skip_if_not_installed("actel")
  x <- real_shaped_x()
  tz <- "America/Toronto"
  a <- suppressWarnings(to_actel(x, tz))
  withr::local_dir(withr::local_tempdir())
  # actel removes a sink it did not open, which capture.output() reports.
  suppressWarnings(utils::capture.output(p <- suppressMessages(
    do.call(actel::preload, c(a, list(tz = tz)))
  )))
  kept <- sum(vapply(p$detections.list, nrow, integer(1)))
  expect_equal(kept, nrow(a$detections))
  expect_true(any(grepl("\\.dpl\\.", unlist(lapply(p$detections.list, function(d) d$Receiver)))))
  y <- read_detset_from(p, receivers = attr(a, "receivers"))
  expect_false(any(grepl("dpl", y$detections$receiver_serial)))
  expect_setequal(unique(y$detections$receiver_serial), unique(x$detections$receiver_serial))
  key <- function(d) sort(paste(format(d$datetime), d$transmitter, d$receiver_serial))
  expect_identical(key(y$detections), key(x$detections))
  z <- read_detset_from(p)
  expect_false(any(grepl("dpl", z$detections$receiver_serial)))
})

test_that("as_detfield_input() keeps a study area out of a network file", {
  x <- example_x()
  # A network-wide receiver file: 300 stations in other lakes that heard nothing.
  far <- x$deployments[rep(1, 300), ]
  far$receiver_serial <- sprintf("VR2W-%06d", seq_len(300))
  far$station_id <- sprintf("FAR-%03d", seq_len(300))
  far$deploy_lat <- 46.7
  far$deploy_lon <- -90.8
  ani <- x$animals
  ani$min_delay <- 80
  ani$max_delay <- 160
  net <- suppressWarnings(detset(x$detections, rbind(x$deployments, far), ani))
  expect_message(all_in <- as_detfield_input(net), "name 324 stations, and 24 of them heard")
  expect_equal(length(unique(all_in$listening$deployments$station_id)), 324L)
  expect_message(by_box <- as_detfield_input(net, extent = c(-79, -77, 44, 45)),
                 "kept 25 of 325 deployments, 40,506 of 40,506 detections")
  expect_setequal(unique(by_box$listening$days$station_id), unique(x$deployments$station_id))
  expect_message(two <- as_detfield_input(net, stations = c("G01", "G02")),
                 "kept 2 of 325 deployments")
  expect_setequal(unique(two$detections$station_id), c("G01", "G02"))
  expect_setequal(unique(two$listening$deployments$station_id), c("G01", "G02"))
  expect_error(as_detfield_input(net, extent = c(1, 0, 2, 3)), "lon_min, lon_max")
  expect_error(as_detfield_input(net, stations = "nowhere"), "No deployment lies")
  # The help names the two inputs bin_detections() stops on (tag delays and
  # codes) and does not promise that the one-line hand-off runs as it is.
  rd <- help_text("as_detfield_input")
  expect_false(grepl("as_detfield_input(x))} runs", rd, fixed = TRUE))
  expect_match(rd, "codes")
})
