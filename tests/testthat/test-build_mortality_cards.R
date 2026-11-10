# One of the three slowest test files of the package. CRAN limits check
# time; devtools::test() runs this file in full.
skip_on_cran()

test_that("cards are written for the dead and review bands, logged and read back clean", {
  dir <- withr::local_tempdir()
  r <- example_review(dir)
  y <- r$y
  out <- y$cards
  n <- table(factor(r$bands$bands$band, c("dead", "review", "alive")))
  expect_s3_class(y, "detset")
  expect_equal(nrow(out), n[["dead"]] + n[["review"]])
  expect_setequal(unique(out$band), names(n)[1:2][n[1:2] > 0])
  expect_true(all(out$checked))
  expect_true(file.exists(attr(out, "index")))
  lg <- detset_log(y)
  expect_equal(lg$step, c("detset", "detect_mortality", "build_mortality_cards"))
  expect_equal(lg$removed[3], 0L)
  expect_equal(lg$rows_after[3], nrow(r$x$detections))
  want <- paste("%d cards written \\(dead %d, review %d, alive 0\\) for 20 animals in the bands",
                "\\(dead %d, review %d, alive %d\\); read back.*: %d of %d cards;")
  expect_match(lg$reason[3], sprintf(want, nrow(out), n[["dead"]], n[["review"]], n[["dead"]],
                                     n[["review"]], n[["alive"]], nrow(out), nrow(out)))
  # The bands' own record is written once, however often they meet the detset.
  again <- build_mortality_cards(y, r$bands, dir)
  expect_equal(sum(detset_log(again)$step == "detect_mortality"), 1L)
  all_out <- build_mortality_cards(r$x, r$bands, dir, all = TRUE)$cards
  expect_equal(nrow(all_out), 20L)
  expect_equal(length(list.files(file.path(dir, "cards"))), 20L)
  only_review <- build_mortality_cards(r$x, r$bands, dir, which = "review",
                                       control = list(cores = 1))$cards
  expect_equal(nrow(only_review), n[["review"]])
  expect_equal(length(list.files(file.path(dir, "cards"))), n[["review"]])
})

test_that("each card holds one data block, the scrubber and no network address", {
  dir <- withr::local_tempdir()
  out <- example_review(dir, all = TRUE)$y$cards
  files <- c(out$file, attr(out, "index"))
  for (f in files) {
    h <- readLines(f, warn = FALSE)
    expect_false(any(grepl("https?://|//cdn|<link |src=\"http|__PAYLOAD__|__TITLE__", h)),
                 label = basename(f))
    expect_equal(sum(grepl('id="detqc-data"', h, fixed = TRUE)), 1L, label = basename(f))
  }
  card <- readLines(out$file[1], warn = FALSE)
  for (needle in c('id="slider"', "function tick(ts)", "function step(n)", 'id="deathinput"',
                   "function stampCursor()", 'data-c="uncertain"')) {
    expect_true(any(grepl(needle, card, fixed = TRUE)), label = needle)
  }
})

test_that("the card numbers equal the detset, read from the HTML on disk", {
  dir <- withr::local_tempdir()
  r <- example_review(dir, all = TRUE)
  x <- r$x
  out <- r$y$cards
  for (k in seq_len(nrow(out))) {
    p <- read_payload(out$file[k])
    id <- out$animal_id[k]
    d <- x$detections[x$detections$animal_id == id, ]
    days <- as.Date(d$datetime)
    expect_equal(p$animal$id, id)
    expect_equal(sum(p$series$dets), nrow(d))
    expect_equal(p$series$dets, tabulate(as.integer(days - as.Date(p$t0)) + 1L, p$nd))
    expect_equal(p$animal$first, format(min(days)))
    expect_equal(p$animal$last, format(max(days)))
    expect_equal(p$animal$stations, length(unique(d$station_id)))
    b <- r$bands$bands[r$bands$bands$animal_id == id, ]
    expect_equal(p$band$band, b$band)
    if (!is.na(b$death_date)) {
      expect_equal(p$band$death_date, format(as.Date(b$death_date)))
      expect_equal(p$marks$flag, as.integer(as.Date(b$death_date) - as.Date(p$t0)))
    }
    v <- r$bands$votes[r$bands$votes$animal_id == id, ]
    expect_setequal(p$votes$vote, v$vote)
    expect_equal(nrow(p$streams), length(unique(r$bands$votes$rule)))
  }
  p3 <- read_payload(out$file[out$animal_id == "EX-03"])
  expect_true(p3$has$accel)
  expect_equal(p3$streams$vote[p3$streams$label %in% c("accelerometer", "final run")],
               c("dead", "dead"))
  ex03 <- r$bands$bands$animal_id == "EX-03"
  expect_equal(p3$band$basis, card_words(r$bands$bands$death_date_basis[ex03]))
  idx <- read_payload(attr(out, "index"))
  expect_equal(length(idx$rows$id), 20L)
  n <- table(factor(r$bands$bands$band, c("dead", "review", "alive")))
  expect_equal(idx$bands$n, as.vector(n))
  expect_equal(idx$rows$rank[idx$rows$b == "review"], seq_len(n[["review"]]))
})

test_that("the read-back check reads the card file alone", {
  # A card that shows no detections must fail, whatever the writer held.
  dir <- withr::local_tempdir()
  r <- example_review(dir)
  out <- r$y$cards
  f <- out$file[out$animal_id == "EX-01"]
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  p <- read_card_data(f)
  dets <- paste0("\"dets\":[", paste(p$series$dets, collapse = ","), "]")
  expect_true(grepl(dets, h, fixed = TRUE))
  zero <- paste0("\"dets\":[", paste(rep(0L, p$nd), collapse = ","), "]")
  writeLines(sub(dets, zero, h, fixed = TRUE), f)
  cb <- card_bands(r$bands)
  ok <- card_readback(f, r$x, cb)
  expect_false(ok$checked)
  expect_match(ok$failed, "detections, detection_days, daily_counts, first_day, last_day")
  # A card whose candidate date is moved, and one with a sensor day lost.
  writeLines(h, f)
  expect_true(card_readback(f, r$x, cb)$checked)
  cb2 <- cb
  cb2$bands$death_date[cb2$bands$animal_id == "EX-01"] <- as.Date("2023-06-01")
  expect_equal(card_readback(f, r$x, cb2)$failed, "candidate_date")
  f3 <- out$file[out$animal_id == "EX-03"]
  h3 <- paste(readLines(f3, warn = FALSE), collapse = "\n")
  p3 <- read_card_data(f3)
  k <- which(!is.na(p3$series$acc))[1]
  acc <- p3$series$acc
  acc[k] <- NA
  cells <- ifelse(is.na(acc), "null", format(acc, digits = 15, trim = TRUE))
  new_acc <- paste0("\"acc\":[", paste(cells, collapse = ","), "]")
  h3b <- sub("\"acc\":\\[[^]]*\\]", new_acc, h3)
  writeLines(h3b, f3)
  expect_match(card_readback(f3, r$x, cb)$failed, "sensor_days")
  expect_error(card_readback(attr(out, "index"), r$x, cb), "not a review card")
  writeLines("<html></html>", f3)
  expect_error(read_card_data(f3), "holds 0 data blocks, not 1")
  expect_equal(nrow(card_readback(character(), r$x, cb)), 0L)
  res <- data.frame(animal_id = c("A", "B"), checked = c(TRUE, FALSE), failed = c("", "located"))
  expect_warning(warn_failed_cards(res),
                 "1 of 2 cards disagrees with the detset, such as B on located")
})

# Rewrites the data block of a built card through `edit` and returns the
# new file. The structure is kept: arrays stay arrays and nulls stay null.
tamper_card <- function(f, edit) {
  h <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  open_tag <- "<script id=\"detqc-data\" type=\"application/json\">"
  parts <- strsplit(h, open_tag, fixed = TRUE)[[1]]
  rest <- strsplit(parts[2], "</script>", fixed = TRUE)[[1]]
  p <- edit(jsonlite::fromJSON(rest[1], simplifyVector = FALSE))
  json <- jsonlite::toJSON(p, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  out <- withr::local_tempfile(fileext = ".html", .local_envir = parent.frame())
  writeLines(paste0(parts[1], open_tag, json, "</script>", paste(rest[-1], collapse = "</script>")),
             out)
  out
}

# Edits of a card's data block, for the tampering test.
set_at <- function(path, value) {
  function(p) {
    p[[path]] <- value
    p
  }
}
map_at <- function(path, f) {
  function(p) {
    p[[path]] <- f(p[[path]])
    p
  }
}
each_value <- function(v, f) lapply(v, function(z) if (is.null(z)) NULL else f(z))
halve_counts <- function(ds) {
  k <- seq(3, length(ds), by = 3)
  ds[k] <- lapply(ds[k], function(z) max(1L, z %/% 2L))
  ds
}
drop_first_value <- function(v) {
  v[which(!vapply(v, is.null, NA))[1]] <- list(NULL)
  v
}

test_that("the read-back fails every card that shows a number other than the data", {
  # Each edit below must fail the read-back: an emptied map, halved located
  # counts, a scaled sensor, and a changed band, vote, date or fact.
  dir <- withr::local_tempdir()
  r <- example_review(dir)
  out <- r$y$cards
  cb <- card_bands(r$bands)
  f <- out$file[out$animal_id == "EX-03"]
  edits <- list(
    located = set_at("ds", list()),
    located = map_at("ds", halve_counts),
    station_days = set_at("dss", list()),
    stations = set_at("heard", list()),
    station_list = map_at("sids", function(v) utils::head(v, -1)),
    sensor_values = map_at(c("series", "acc"), function(v) each_value(v, function(z) z * 10)),
    sensor_values = map_at(c("series", "accLo"), function(v) each_value(v, function(z) z + 0.01)),
    sensor_days = map_at(c("series", "acc"), drop_first_value),
    band = set_at(c("band", "band"), "alive"),
    candidate_date = set_at(c("band", "death_date"), "2023-06-01"),
    votes = map_at("votes", function(v) lapply(v, function(r) replace(r, "vote", "dead"))),
    release = set_at(c("animal", "release"), "2020-01-01"),
    expiry = set_at(c("animal", "expiry"), "2030-01-01"),
    to_horizon = set_at(c("animal", "to_horizon"), 999L),
    species = set_at(c("animal", "species"), "Unknown fish")
  )
  expect_true(card_readback(tamper_card(f, identity), r$x, cb)$checked)
  for (k in seq_along(edits)) {
    res <- card_readback(tamper_card(f, edits[[k]]), r$x, cb)
    expect_false(res$checked, label = names(edits)[k])
    expect_match(res$failed, names(edits)[k], label = names(edits)[k])
  }
  expect_true(any(vapply(read_card_data(f)$votes$vote, function(v) v != "dead", NA)))
  expect_equal(nrow(read_card_data(f)$votes), sum(r$bands$votes$animal_id == "EX-03"))
  lg <- detset_log(r$y)
  expect_match(lg$reason[3],
               "equal to the\\s+detset, the animals table and the bands on 18 quantities each")
})

test_that("the read-back places detections by its own join to the deployments", {
  t <- as.POSIXct(c("2023-05-02", "2023-06-02", "2023-05-02", "2023-05-02", "2023-05-02",
                    "2023-08-02"), tz = "UTC")
  dep <- data.frame(
    receiver_serial = c("VR2W-100001", "VR2W-100001", "VR2W-100002", "HR2-100003",
                        "VR2W-100003", "VR2W-100004"),
    station_id = c("A", "A", "B", "C", "C", "D"),
    deploy_datetime = as.POSIXct(c("2023-05-01", "2023-06-01", "2023-05-01", "2023-05-01",
                                   "2023-05-01", "2023-05-01"), tz = "UTC"),
    recover_datetime = as.POSIXct(c("2023-07-01", NA, "2023-07-01", "2023-07-01",
                                    "2023-07-01", "2023-07-01"), tz = "UTC"),
    deploy_lat = c(44.1, 44.2, 44.3, 44.4, 44.5, 44.6), deploy_lon = -78
  )
  d <- data.frame(receiver_serial = c("VR2W-100001", "VR2W-100001", "100002", "100003", NA,
                                      "VR2W-100004"),
                  datetime = t, deploy_lat = c(NA, NA, NA, NA, 45, 46),
                  deploy_lon = c(NA, NA, NA, NA, -79, -80))
  ll <- located_truth(d, dep)
  # Full serial; the later of two overlapping deployments; the bare serial;
  # a bare serial of two receiver forms names neither; no serial and a
  # detection after its deployment take the detection's own position.
  expect_equal(ll$lat, c(44.1, 44.2, 44.3, NA, 45, 46))
  expect_equal(ll$lon, c(-78, -78, -78, NA, -79, -80))
  expect_equal(located_truth(d[, 1:2], NULL)$lat, rep(NA_real_, 6))
  expect_equal(bare_serial(c("VR2Tx-123456", "123456", NA)), c("123456", "123456", NA))
  expect_equal(q7(sort(c(3, 1, 2, 10)), c(0.5)), stats::median(c(3, 1, 2, 10)))
  expect_equal(q7(sort(c(3, 1, 2, 10)), 0.05), unname(stats::quantile(c(3, 1, 2, 10), 0.05)))
  expect_true(same_text(NULL, NA))
  expect_false(same_text("a", NULL))
})

test_that("cards of a detset with no animals table and no sensor read back clean", {
  ex <- hard_cases()
  sensor_cols <- c("sensor_raw", "sensor_value", "sensor_unit", "sensor_type")
  det <- ex$detections[, setdiff(names(ex$detections), sensor_cols)]
  x <- detset(det, ex$deployments)
  mb <- detect_mortality(x)
  y <- build_mortality_cards(x, mb, withr::local_tempdir())
  expect_true(all(y$cards$checked))
  expect_null(sensor_truth(x))
  cb <- card_bands(mb)
  f <- y$cards$file[1]
  p <- read_card_data(f)
  p$animal$id <- "NOT-AN-ANIMAL"
  truth <- card_truth(x)
  expect_false(any(compare_card(p, truth, cb)))
  expect_length(compare_card(p, truth, cb), 18L)
})

test_that("a station whose deployments moved keeps one row on the card", {
  ex <- hard_cases()
  dep <- ex$deployments
  g7 <- which(dep$station_id == "G07")
  moved <- dep[g7, ]
  cut <- as.POSIXct("2023-07-01", tz = "UTC")
  moved$deploy_datetime <- cut
  moved$deploy_lat <- moved$deploy_lat + 0.0005
  dep$recover_datetime[g7] <- cut
  x <- detset(ex$detections, rbind(dep, moved), ex$animals)
  mb <- detect_mortality(x)
  y <- build_mortality_cards(x, mb, withr::local_tempdir(), all = TRUE)
  expect_true(all(y$cards$checked))
  p <- read_payload(y$cards$file[y$cards$animal_id == "EX-01"])
  d1 <- x$detections[x$detections$animal_id == "EX-01", ]
  expect_equal(length(p$sids), length(unique(dep$station_id)))
  expect_equal(nrow(p$stations), length(unique(dep$station_id)) + 1L)
  expect_equal(length(p$heard), length(unique(d1$station_id)))
  expect_equal(p$animal$stations, length(p$heard))
  dss <- matrix(p$dss, ncol = 3, byrow = TRUE)
  g7i <- which(p$sids == "G07") - 1L
  expect_equal(sum(dss[dss[, 2] == g7i, 3]), sum(d1$station_id == "G07"))
  ds <- matrix(p$ds, ncol = 3, byrow = TRUE)
  g7lat <- round(c(dep$deploy_lat[g7], moved$deploy_lat), 6)
  g7pos <- which(p$stations$lon[ds[, 2] + 1L] == round(dep$deploy_lon[g7], 6) &
                   p$stations$lat[ds[, 2] + 1L] %in% g7lat)
  expect_equal(length(unique(ds[g7pos, 2])), 2L)
  card <- readLines(y$cards$file[1], warn = FALSE)
  expect_true(any(grepl("stations heard in window (of \" + D.sids.length", card, fixed = TRUE)))
  expect_true(any(grepl("one row per station (station_id)", card, fixed = TRUE)))
})

test_that("a failed read-back warns and is logged, and the card is kept", {
  dir <- withr::local_tempdir()
  x <- example_detset()
  mb <- detect_mortality(x)
  local_mocked_bindings(compare_card = function(p, truth, bands) {
    c(detections = p$animal$id != "EX-02", stations = TRUE)
  })
  n <- sum(mb$bands$band %in% c("dead", "review"))
  expect_warning(y <- build_mortality_cards(x, mb, dir),
                 sprintf("1 of %d cards disagrees with the detset, such as EX-02 on detections", n))
  expect_equal(y$cards$failed[y$cards$animal_id == "EX-02"], "detections")
  expect_match(detset_log(y)$reason[3], sprintf("each \\(.*\\): %d of %d cards", n - 1L, n))
})

test_that("a value cannot close the data block", {
  ex <- hard_cases()
  bad <- "</script><b>EX"
  ex$detections$animal_id[ex$detections$animal_id == "EX-01"] <- bad
  ex$animals$animal_id[ex$animals$animal_id == "EX-01"] <- bad
  x <- detset(ex$detections, ex$deployments, ex$animals)
  mb <- detect_mortality(x)
  dir <- withr::local_tempdir()
  out <- build_mortality_cards(x, mb, dir)$cards
  f <- out$file[out$animal_id == bad]
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl("</script><b>", h, fixed = TRUE))
  expect_equal(read_payload(f)$animal$id, bad)
  expect_true(out$checked[out$animal_id == bad])
  expect_match(h, "<title>Mortality review: &lt;/script>&lt;b>EX</title>", fixed = TRUE)
})

test_that("a column named id does not empty a card", {
  # A VDAT DET field named `id` must not clash with the card's own id.
  ex <- hard_cases()
  ex$detections$id <- seq_len(nrow(ex$detections))
  x <- detset(ex$detections, ex$deployments, ex$animals)
  dir <- withr::local_tempdir()
  y <- build_mortality_cards(x, detect_mortality(x), dir)
  expect_true(all(y$cards$checked))
  p <- read_payload(y$cards$file[1])
  expect_gt(sum(p$series$dets), 0)
})

test_that("control sets the title, the store key, the cores and an outline", {
  x <- example_detset()
  mb <- detect_mortality(x)
  dir <- withr::local_tempdir()
  out <- build_mortality_cards(x, mb, dir, control = list(title = "Lake A", store = "lake_a"))
  idx <- read_payload(attr(out$cards, "index"))
  expect_equal(idx$title, "Lake A")
  expect_equal(idx$store, "detqc_calls_lake_a")
  expect_equal(read_payload(out$cards$file[1])$store, "detqc_calls_lake_a")
  expect_error(build_mortality_cards(x, mb, dir, control = list(colour = 1)),
               "no option `colour`")
  expect_error(build_mortality_cards(x, mb, dir, control = 1), "must be a list")
  expect_error(build_mortality_cards(x, mb, dir, control = list(cores = 0)),
               "`control\\$cores` must be one number")
  expect_error(build_mortality_cards(x, mb, c("a", "b")), "one folder path")
  expect_error(build_mortality_cards(x, list(), dir), "detect_mortality")
  bad <- mb
  bad$bands$band <- NULL
  expect_error(build_mortality_cards(x, bad, dir), "has no column `band`")
  skip_on_os("windows")
  par <- build_mortality_cards(x, mb, dir, all = TRUE, control = list(cores = 2))
  expect_equal(nrow(par$cards), 20L)
  expect_true(all(par$cards$checked))
  skip_if_not_installed("sf")
  ring <- matrix(c(-78.14, 44.51, -78.06, 44.51, -78.06, 44.6, -78.14, 44.6, -78.14, 44.51),
                 ncol = 2, byrow = TRUE)
  poly <- sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  out2 <- build_mortality_cards(x, mb, dir, control = list(outline = poly))
  lake <- read_payload(out2$cards$file[1])$lake
  expect_equal(length(lake$rings$x[[1]]), 5L)
  expect_lt(lake$bbox[1], -78.14)
})

test_that("new rule streams, a persistence score and size columns reach the cards", {
  x <- example_detset()
  x$animals$length_mm <- 400
  mb <- detect_mortality(x)
  rv <- mb$bands$animal_id[mb$bands$band == "review"]
  mb$bands$persistence <- NA_real_
  nr <- length(rv)
  score <- seq(0.1, 0.9, length.out = nr)[c(2, nr, seq_len(nr)[-c(2, nr)])]
  mb$bands$persistence[match(rv, mb$bands$animal_id)] <- score
  extra <- data.frame(animal_id = rv, rule = "late_stream", vote = "review",
                      date = as.POSIXct("2023-09-01", tz = "UTC"), detail = "ends at the horizon")
  mb$votes <- rbind(mb$votes[, names(extra)], extra)
  dir <- withr::local_tempdir()
  y <- build_mortality_cards(x, mb, dir, which = "review")
  out <- y$cards
  expect_equal(out$animal_id, rv[order(-score)])
  p <- read_payload(out$file[1])
  expect_equal(p$band$rank, 1L)
  expect_equal(p$band$of, length(rv))
  expect_equal(p$band$persistence, 0.9)
  # The size names its column and says the unit is the table's.
  expect_equal(p$animal$size, "length 400 mm")
  expect_true("late stream" %in% p$streams$label)
  expect_equal(p$streams$state[p$streams$label == "late stream"], "review vote")
  idx <- read_payload(attr(out, "index"))
  expect_true(idx$ranked)
  expect_true("late stream" %in% idx$streams)
  expect_equal(idx$rows$pers[idx$rows$b == "review"], sort(score, decreasing = TRUE))
  mb$votes <- mb$votes[0, ]
  y2 <- build_mortality_cards(x, mb, dir, which = "dead")
  expect_length(read_payload(y2$cards$file[1])$streams, 0L)
})

test_that("a tag with another sensor or raw readings only is shown as such", {
  ex <- hard_cases()
  x <- detset(ex$detections, ex$deployments, ex$animals)
  mb <- detect_mortality(x)
  acc_tx <- unique(ex$animals$transmitter[!is.na(ex$animals$sensor_type) &
                                            grepl("accel", ex$animals$sensor_type)])
  ani <- ex$animals
  ani$sensor_type[ani$transmitter %in% acc_tx] <- "temperature"
  ani$sensor_unit[ani$transmitter %in% acc_tx] <- "C"
  xt <- detset(ex$detections, ex$deployments, ani)
  dir <- withr::local_tempdir()
  y <- build_mortality_cards(xt, mb, dir, which = "dead")
  p <- read_payload(y$cards$file[y$cards$animal_id == "EX-03"])
  expect_false(p$has$accel)
  expect_equal(p$has$other, "temperature")
  expect_gt(sum(!is.na(p$series$acc)), 0)
  expect_true(y$cards$checked[y$cards$animal_id == "EX-03"])
  ani$sensor_slope[ani$transmitter %in% acc_tx] <- NA
  ani$sensor_intercept[ani$transmitter %in% acc_tx] <- NA
  xr <- detset(ex$detections, ex$deployments, ani)
  y2 <- build_mortality_cards(xr, mb, dir, which = "dead")
  p2 <- read_payload(y2$cards$file[y2$cards$animal_id == "EX-03"])
  expect_gt(p2$has$raw_readings, 0)
  expect_match(p2$animal$sensors, "raw, not calibrated")
  expect_true(all(y2$cards$checked))
})

test_that("an animal with no located detection gets a card and an index row", {
  ex <- hard_cases()
  dep <- ex$deployments
  x <- detset(ex$detections, dep, ex$animals)
  mb <- detect_mortality(x)
  x$deployments$deploy_lat[x$deployments$station_id %in%
                             unique(x$detections$station_id[x$detections$animal_id == "EX-01"])] <-
    NA_real_
  ctx <- card_context(x, card_bands(mb), card_control(list(), withr::local_tempdir()))
  nav <- ctx$bands
  k <- which(nav$animal_id == "EX-01")
  p <- card_payload("EX-01", ctx, nav, k)
  located <- if (length(p$ds)) sum(matrix(p$ds, ncol = 3, byrow = TRUE)[, 3]) else 0
  expect_lt(located, sum(p$series$dets))
  # The station panel counts every detection at the station it names, located or not.
  d1 <- x$detections[x$detections$animal_id == "EX-01", ]
  expect_equal(length(p$heard), length(unique(d1$station_id)))
  expect_equal(sum(matrix(p$dss, ncol = 3, byrow = TRUE)[, 3]), nrow(d1))
  expect_equal(heard_order(ctx$all_det[0], ctx$sids), integer())
  expect_equal(day_sid_triples(ctx$all_det[0], ctx$sids, identity), integer())
  expect_equal(day_station_triples(ctx$det[0], identity), integer())
  ip <- index_payload(ctx, transform(nav, card = TRUE), card_control(list(), tempdir()))
  expect_true(check_index(ip, x))
  ip$rows <- ip$rows[-1]
  expect_error(check_index(ip, x), "The index lists 19 animals; the detset holds 20")
})

test_that("the cards of every synthetic hard case build and read back clean", {
  ex <- hard_mortality()
  x <- detset(ex$detections, ex$deployments, ex$animals)
  expect_warning_fresh(mb <- detect_mortality(x), "cast no sensor vote")
  y <- build_mortality_cards(x, mb, withr::local_tempdir(), all = TRUE)
  expect_equal(nrow(y$cards), count_animals(x$detections))
  expect_true(all(y$cards$checked))
  for (k in seq_len(nrow(y$cards))) {
    p <- read_payload(y$cards$file[k])
    d <- x$detections[!is.na(x$detections$animal_id) &
                        x$detections$animal_id == y$cards$animal_id[k], ]
    expect_equal(sum(p$series$dets), nrow(d))
    expect_equal(p$animal$detection_days, length(unique(as.Date(d$datetime))))
  }
})

# ---- The vote cells of the Get started cards ----
# The final-run cell names the run and its last days, so the window's dates
# beside the run's start date do not read as a contradiction.
test_that("the vote cells of the Get started cards read one way", {
  study <- example_mortality
  dets <- detset(study$detections, study$deployments, study$animals)
  dets <- audit_listening(dets,
                          known_listening = unique(dets$deployments$receiver_serial))
  dets <- filter_post_release(dets, hours = 6)
  dets <- filter_min_lag(dets, max_lag_s = 4500)
  mort_rules <- mortality_rules(min_days = 3, min_calendar_days = 3,
                                min_flat_days = 2, within_days = 2)
  bands <- detect_mortality(dets, rules = mort_rules)
  ctx <- list(votes = data.table::as.data.table(bands$votes))
  cell <- function(id, rule) {
    v <- vote_rows(id, ctx)
    v[[which(vapply(v, function(r) r$rule, "") == rule)]]$detail
  }
  expect_identical(cell("ST-02", "terminal_run"),
                   paste("the first detection of the final run; the final run, 2024-06-03 to",
                         "2024-06-10, is at one site; its last 3 detection days, 2024-06-08",
                         "to 2024-06-10, span 0.000 km"))
  expect_identical(cell("ST-03", "terminal_run"),
                   paste("the record's last 3 detection days, 2024-06-07 to 2024-06-09, span",
                         "4 sites; diagonal 2.939 km"))
  expect_match(cell("ST-02", "movement_resumption"), "from the candidate date, 2024-06-03$")
  expect_identical(cell("ST-03", "site_listening"),
                   "the data end 1 day after the last detection; record_end judges it")
  all_cells <- unlist(lapply(unique(ctx$votes$animal_id), function(id) {
    vapply(vote_rows(id, ctx), function(r) if (is.null(r$detail)) "" else r$detail, "")
  }))
  expect_false(any(grepl("\\b1 days\\b|the window of", all_cells)))
})

test_that("a vote cell does not repeat a basis its detail states", {
  # The record_end basis is "the last detection" and its detail starts
  # "last detection <date>"; the cell keeps the detail alone. A basis the
  # detail does not state is kept before it.
  ctx <- list(votes = data.table::data.table(
    animal_id = "a", rule = c("record_end", "terminal_run"), vote = c("review", "dead"),
    date = as.POSIXct(c("2024-06-04", "2024-06-03"), tz = "UTC"),
    basis = c("the last detection", "the first detection of the final run"),
    detail = c("last detection 2024-06-04, 6 days before the tag life (2024-06-10)",
               "3 days at one site")
  ))
  rows <- vote_rows("a", ctx)
  expect_equal(rows[[1]]$detail,
               card_words("last detection 2024-06-04, 6 days before the tag life (2024-06-10)"))
  expect_equal(rows[[2]]$detail,
               card_words("the first detection of the final run; 3 days at one site"))
  expect_false(grepl("the last detection; last detection", rows[[1]]$detail, fixed = TRUE))
})

# ---- Card dates and the card index ----
# The mortality rule texts, the review cards and the card index: the dates
# they print and what the index lists. Fixtures are the bundled examples or
# derived from them.

test_that("a card starts at the detset's first day when the release is earlier", {
  ex <- hard_cases()
  ani <- ex$animals
  ani$release_datetime[ani$animal_id == "EX-03"] <- min(ex$detections$datetime) - 30 * 86400
  x <- detset(ex$detections, ex$deployments, ani)
  mb <- quiet_raw(detect_mortality(x))
  dir <- withr::local_tempdir()
  y <- build_mortality_cards(x, mb, dir)
  p <- read_payload(y$cards$file[y$cards$animal_id == "EX-03"])
  first <- as.Date(min(ex$detections$datetime))
  expect_equal(p$t0, format(first))
  expect_true(p$marks$release_before)
  expect_equal(p$marks$data_start, format(first))
  # The 30 days before the detset are not drawn as days with 0 detections.
  expect_equal(length(p$series$dets), p$nd)
  expect_equal(p$nd, as.integer(as.Date(p$marks$horizon_date) - first) + 1L)
  expect_true(all(y$cards$checked))
  # The release itself stays in the facts, and the read-back check agrees.
  expect_equal(p$animal$release,
               format(as.Date(ani$release_datetime[ani$animal_id == "EX-03"][1])))
})

test_that("record_end counts whole UTC days, as the card's header does", {
  x <- example_detset()
  last <- max(x$detections$datetime[x$detections$animal_id %in% "EX-07"])
  # A horizon late on the day of the last detection: under a day apart, a
  # fraction that must not print as "1 days".
  hz <- as.POSIXct(paste(format(as.Date(last)), "23:59:53"), tz = "UTC")
  keep <- x$detections$datetime <= hz
  x$detections <- x$detections[keep, ]
  mb <- quiet_raw(detect_mortality(x, control = list(horizon = hz)))
  v <- mb$votes[mb$votes$animal_id == "EX-07" & mb$votes$rule == "record_end", ]
  expect_match(v$detail, "0 days before the data horizon")
  # Every record_end text gives the calendar-day gap to its horizon.
  r <- mb$votes[mb$votes$rule == "record_end" & grepl("data horizon", mb$votes$detail), ]
  shown <- as.integer(sub(".*, ([0-9]+) days? (before|after) the.*", "\\1", r$detail))
  expect_equal(shown, abs(as.integer(as.Date(hz) - as.Date(r$date))))
  expect_false(any(grepl("\\b1 days\\b", mb$votes$detail)))
})

test_that("the flatline basis prints the onset date, the day after the last active day", {
  mb <- example_bands()
  v <- mb$votes[mb$votes$rule == "accel_flatline" & mb$votes$vote == "dead", ]
  expect_gt(nrow(v), 0L)
  txt <- paste(v$basis, v$detail)
  re <- "onset ([0-9-]{10}), the day after the last active day \\(([0-9-]{10})\\)"
  m <- regmatches(txt, regexec(re, txt))
  expect_true(all(lengths(m) == 3L))
  onset <- as.Date(vapply(m, `[`, "", 2))
  active <- as.Date(vapply(m, `[`, "", 3))
  expect_equal(onset, active + 1)
})

test_that("the index lists only the rule streams that voted dead, alive or review", {
  x <- example_detset()
  mb <- quiet_raw(detect_mortality(x))
  y <- build_mortality_cards(x, mb, withr::local_tempdir())
  idx <- read_payload(attr(y$cards, "index"))
  voted <- unique(mb$votes$rule[mb$votes$vote %in% c("dead", "alive", "review")])
  expect_setequal(idx$streams, label_of(voted, stream_labels))
  silent <- setdiff(unique(mb$votes$rule), voted)
  expect_gt(length(silent), 0L)
  expect_false(any(label_of(silent, stream_labels) %in% idx$streams))
})

test_that("the map opens on the animal's stations when the array is far larger", {
  ex <- hard_cases()
  far <- ex$deployments[1, ]
  far$receiver_serial <- "999001"
  far$station_id <- "FAR"
  far$deploy_lat <- far$deploy_lat + 1.5
  far$deploy_lon <- far$deploy_lon - 2
  x <- detset(ex$detections, rbind(ex$deployments, far), ex$animals)
  mb <- quiet_raw(detect_mortality(x))
  y <- build_mortality_cards(x, mb, withr::local_tempdir())
  p <- read_payload(y$cards$file[1])
  expect_equal(p$extent$open, "animal")
  expect_lt(p$extent$animal[3] - p$extent$animal[1], p$lake$bbox[3] - p$lake$bbox[1])
  # The bundled array alone opens whole.
  z <- build_mortality_cards(example_detset(), mb, withr::local_tempdir())
  expect_equal(read_payload(z$cards$file[1])$extent$open, "whole")
})

test_that("a detection column named id does not empty the review cards", {
  # read_detections() keeps a VDAT file's DET field ID as a column `id`. The
  # cards subset by `animal_id == id`, where data.table would read `id` as
  # that column; each card must still hold its animal's detections.
  ex <- hard_cases()
  det <- ex$detections
  det$id <- seq_len(nrow(det))
  x <- detset(det, ex$deployments, ex$animals)
  bands <- detect_mortality(x)
  dir <- withr::local_tempdir()
  out <- build_mortality_cards(x, bands, dir)$cards
  expect_true(all(out$checked))
  n <- table(x$detections$animal_id)
  for (k in seq_len(nrow(out))) {
    p <- read_payload(out$file[k])
    expect_equal(p$animal$detections, as.integer(n[[out$animal_id[k]]]))
  }
})
