# How the votes set the band, tested on planted votes.

utc <- function(s) as.POSIXct(s, tz = "UTC")

# One animal's votes, as run_rules() returns them, and its record.
plant_votes <- function(..., last = "2024-12-31") {
  v <- list(...)
  out <- data.table::rbindlist(lapply(names(v), function(nm) {
    z <- v[[nm]]
    data.table::data.table(animal_id = "a", rule = nm, kind = z[[1]], vote = z[[2]],
                           score = NA_real_,
                           date = if (length(z) > 2) utc(z[[3]]) else empty_utc(1),
                           basis = "planted", detail = "planted")
  }))
  rec <- data.table::data.table(animal_id = "a", detections = 10L, n_days = 20L,
                                first = utc("2024-01-01"), last = utc(last), located = 10L,
                                expected_end = utc("2024-12-31"), end_basis = "data horizon")
  list(run = list(votes = out, extras = list(), evidence = list()),
       data = list(records = rec))
}

band_of <- function(p, sparse = TRUE, censor = 30) {
  b <- combine_votes(p$run, p$data, list(sparse_to_review = sparse), censor)$bands
  c(band = b$band, rule = b$deciding_rule)
}

test_that("a death vote with no alive vote is dead, beside one it is review", {
  dead <- plant_votes(accel_flatline = list("vote", "dead", "2024-06-01"))
  expect_equal(band_of(dead), c(band = "dead", rule = "accel_flatline"))
  both <- plant_votes(terminal_run = list("vote", "dead", "2024-06-01"),
                      accel_flatline = list("vote", "alive"))
  expect_equal(band_of(both), c(band = "review", rule = "terminal_run_beside_activity"))
  moved <- plant_votes(accel_flatline = list("vote", "dead", "2024-06-01"),
                       movement_resumption = list("vote", "alive"))
  expect_equal(band_of(moved), c(band = "review", rule = "movement_resumption"))
  other <- plant_votes(mine = list("vote", "dead", "2024-06-01"), yours = list("vote", "alive"))
  expect_equal(band_of(other), c(band = "review", rule = "mine_beside_yours"))
})

test_that("death evidence near the end of a censored record is review, far from it dead", {
  near <- plant_votes(terminal_run = list("vote", "dead", "2024-12-10"),
                      record_end = list("censor", "review"))
  expect_equal(band_of(near), c(band = "review", rule = "record_end"))
  far <- plant_votes(terminal_run = list("vote", "dead", "2024-06-01"),
                     record_end = list("censor", "review"))
  expect_equal(band_of(far), c(band = "dead", rule = "terminal_run"))
  # The window is the record-end stream's own setting.
  expect_equal(band_of(far, censor = 300), c(band = "review", rule = "record_end"))
  alone <- plant_votes(record_end = list("censor", "review"))
  expect_equal(band_of(alone), c(band = "review", rule = "record_end"))
})

test_that("review votes, sparse records, rankings and silence set their bands", {
  split <- plant_votes(terminal_run = list("vote", "review", "2024-06-01"))
  split$run$extras$terminal_run <- data.frame(animal_id = "a", subtype = "split")
  expect_equal(band_of(split), c(band = "review", rule = "terminal_run_split_site"))
  user <- plant_votes(mine = list("vote", "review"))
  expect_equal(band_of(user), c(band = "review", rule = "mine"))
  sparse <- plant_votes(terminal_run = list("vote", "unavailable"))
  expect_equal(band_of(sparse), c(band = "review", rule = "sparse_record"))
  expect_equal(band_of(sparse, sparse = FALSE), c(band = "alive", rule = "none"))
  ranked <- plant_votes(persistence = list("rank", "review"))
  expect_equal(band_of(ranked), c(band = "review", rule = "persistence"))
  quiet <- plant_votes(terminal_run = list("vote", "none"), persistence = list("rank", "none"))
  expect_equal(band_of(quiet), c(band = "alive", rule = "none"))
  evidence <- plant_votes(depth_flatline = list("evidence", "dead", "2024-06-01"))
  expect_equal(band_of(evidence), c(band = "alive", rule = "none"))
})

test_that("an animal with no located detection is review unless a sensor votes dead", {
  p <- plant_votes(terminal_run = list("vote", "unavailable"))
  p$data$records$located <- 0L
  expect_equal(band_of(p), c(band = "review", rule = "no_position"))
  p2 <- plant_votes(accel_flatline = list("vote", "dead", "2024-06-01"),
                    terminal_run = list("vote", "unavailable"))
  p2$data$records$located <- 0L
  expect_equal(band_of(p2), c(band = "dead", rule = "accel_flatline"))
})

test_that("the death date is the deciding stream's for the dead, the earliest for review", {
  p <- plant_votes(terminal_run = list("vote", "dead", "2024-06-10"),
                   accel_flatline = list("vote", "dead", "2024-06-01"))
  b <- combine_votes(p$run, p$data, list(sparse_to_review = TRUE))$bands
  expect_equal(b$deciding_rule, "terminal_run")
  expect_equal(b$death_date, utc("2024-06-10"))
  expect_equal(b$death_date_basis, "planted (terminal_run)")
  p2 <- plant_votes(terminal_run = list("vote", "dead", "2024-06-10"),
                    mine = list("vote", "review", "2024-05-01"),
                    accel_flatline = list("vote", "alive"))
  b2 <- combine_votes(p2$run, p2$data, list(sparse_to_review = TRUE))$bands
  expect_equal(b2$band, "review")
  expect_equal(b2$death_date, utc("2024-05-01"))
  quiet <- plant_votes(terminal_run = list("vote", "none"))
  b3 <- combine_votes(quiet$run, quiet$data, list(sparse_to_review = TRUE))$bands
  expect_true(is.na(b3$death_date))
  expect_true(is.na(b3$death_date_basis))
})

test_that("the bands table keeps the columns the cards and the report read", {
  b <- example_bands()$bands
  keep <- c("animal_id", "band", "deciding_rule", "rule_label", "death_date",
            "death_date_basis", "persistence", "terminal_vote", "accel_vote",
            "terminal_site", "run_start", "run_detection_days", "run_calendar_days",
            "window_diag_km", "detection_days", "first_detection", "last_detection",
            "detections", "located", "accel_transmitter", "accel_onset", "accel_last_active",
            "accel_flat_days", "expected_end", "end_basis")
  expect_true(all(keep %in% names(b)))
  expect_equal(b$band, sort(factor(b$band, c("dead", "review", "alive"))) |> as.character())
  expect_false(any(is.na(b$rule_label)))
  v <- example_bands()$votes
  expect_equal(names(v), c("animal_id", "rule", "vote", "score", "date", "basis", "detail"))
  expect_equal(nrow(v), 20L * 7L)
  expect_true(all(c("site_listening_vote", "terminal_vote") %in% names(b)))
  expect_equal(label_rule(c("terminal_run_stray", "site_listening")),
               c("a final run at one site apart from stray detections on a few days",
                 "a record that ends when its last site stopped listening"))
  expect_s3_class(v$date, "POSIXct")
  expect_equal(label_rule(c("record_end", "my_rule")),
               c("a record that ends near tag life or the data horizon", "my rule"))
})

test_that("empty inputs give empty tables of the same columns", {
  expect_equal(names(empty_bands()), c("animal_id", "band", "deciding_rule", "rule_label",
                                       "death_date", "death_date_basis"))
  expect_equal(nrow(empty_votes()), 0L)
  b <- add_stream_columns(data.table::data.table(animal_id = "a"), list())
  expect_true(is.na(b$persistence))
  expect_true(is.na(b$run_start))
})

test_that("death evidence on an uncalibrated sensor tag goes to review, named", {
  ex <- hard_cases()
  x <- detset(ex$detections, ex$deployments, ex$animals)
  mb <- suppressWarnings(detect_mortality(x))
  b <- mb$bands[mb$bands$animal_id == "EX-18", ]
  expect_equal(c(b$band, b$deciding_rule), c("dead", "terminal_run"))
  expect_false(b$sensor_uncalibrated)
  # The same depth tag with its calibration missing, as a tag absent from the
  # specification sheet is.
  ani <- ex$animals
  ani[ani$animal_id == "EX-18", c("sensor_slope", "sensor_intercept")] <- NA
  y <- detset(ex$detections, ex$deployments, ani)
  expect_message(mb2 <- suppressWarnings(detect_mortality(y)),
                 paste("1 animal with death evidence carries sensor readings with no",
                       "calibration, such as EX-18 \\(A69-9004-8002\\), so it goes to review"))
  b2 <- mb2$bands[mb2$bands$animal_id == "EX-18", ]
  expect_equal(c(b2$band, b2$deciding_rule), c("review", "uncalibrated_sensor"))
  expect_true(b2$sensor_uncalibrated)
  expect_match(b2$rule_label, "sensor readings are not calibrated")
  expect_match(mb2$settings$reason, "1 animal with death evidence and uncalibrated sensor")
  # No other animal changes band.
  same <- merge(mb$bands[, c("animal_id", "band")], mb2$bands[, c("animal_id", "band")],
                by = "animal_id")
  expect_equal(same$animal_id[same$band.x != same$band.y], "EX-18")
})
