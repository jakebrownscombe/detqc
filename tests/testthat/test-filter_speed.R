# filter_speed() on planted faults, counted as true positives, false
# positives and false negatives.

speed_case <- function(false_times, n = 360, animals = "F1") {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  t0 <- utc("2024-06-01 00:00:00")
  dep <- data.frame(receiver_serial = c("RA", "RB"), station_id = c("A", "B"),
                    deploy_datetime = utc("2024-05-01"), recover_datetime = utc("2024-07-01"),
                    deploy_lat = c(45, 45 + 60 / 111.2), deploy_lon = -80)
  tt <- t0 + 3600 + (seq_len(n) - 1) * 120
  rows <- lapply(animals, function(a) {
    rbind(data.frame(datetime = tt, receiver_serial = "RA", transmitter = paste0("T", a),
                     animal_id = a, truth = TRUE),
          data.frame(datetime = false_times(tt, t0), receiver_serial = "RB",
                     transmitter = paste0("T", a), animal_id = a, truth = FALSE))
  })
  d <- do.call(rbind, rows)
  d$rid <- seq_len(nrow(d))
  list(d = d, x = detset(d, dep))
}

speed_score_at <- function(case, min_dist_m) {
  f <- filter_speed(case$x, max_speed_ms = 3, min_dist_m = min_dist_m)
  kept <- case$d$rid %in% f$detections$rid
  c(tp = sum(!kept & !case$d$truth), fp = sum(!kept & case$d$truth),
    fn = sum(kept & !case$d$truth))
}

speed_score <- function(case, ...) {
  f <- filter_speed(case$x, max_speed_ms = 3, min_dist_m = 2000, ...)
  kept <- case$d$rid %in% f$detections$rid
  c(tp = sum(!kept & !case$d$truth), fp = sum(!kept & case$d$truth),
    fn = sum(kept & !case$d$truth))
}

test_that("one false detection first, simultaneous, inside or last costs no true one", {
  cases <- list(
    first = function(tt, t0) t0 + 3600 - 60,
    simultaneous = function(tt, t0) tt[100],
    inside = function(tt, t0) tt[100] + 60,
    last = function(tt, t0) tt[length(tt)] + 60
  )
  for (nm in names(cases)) {
    expect_equal(speed_score(speed_case(cases[[nm]])), c(tp = 1, fp = 0, fn = 0), label = nm)
  }
})

test_that("many planted faults across animals are found with no true loss", {
  set.seed(1)
  plant <- function(tt, t0) sort(sample(tt, 12)) + 30
  s <- speed_score(speed_case(plant, animals = c("F1", "F2", "F3")))
  expect_equal(s, c(tp = 36, fp = 0, fn = 0))
})

test_that("receivers within min_dist_m never violate", {
  case <- speed_case(function(tt, t0) tt[50] + 30)
  s <- speed_score_at(case, 1e5)
  expect_equal(s, c(tp = 0, fp = 0, fn = 1))
})

test_that("the log names the reasons and the rows it left alone", {
  case <- speed_case(function(tt, t0) c(t0 + 3000, tt[100]))
  f <- filter_speed(case$x, max_speed_ms = 3, min_dist_m = 2000)
  lg <- f$log[nrow(f$log), ]
  expect_equal(lg$removed, 2L)
  expect_true(all(names(lg$removed_by[[1]]) %in% c("spike", "weaker_endpoint")))
  expect_match(lg$reason, "passes; 0 detections with no animal or position kept")
  expect_error(filter_speed(case$x), "max_speed_ms")
  expect_error(filter_speed(case$x, max_speed_ms = 3), "`min_dist_m` has no default")
  expect_equal(speed_violations(data.frame(a = "x", t = 1, lat = 1, lon = 1), 3, 10), FALSE)
})

test_that("one transmission heard 2.8 km apart is not a violation where reference tags say so", {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  # Two stations 2,800 m apart. Reference tag R is heard at both within 2 s,
  # every hour; the animal is heard at A every 2 min and, 2.5 s later, at B
  # on 10 of its transmissions.
  dep <- data.frame(receiver_serial = c("RA", "RB"), station_id = c("A", "B"),
                    deploy_datetime = utc("2024-05-01"), recover_datetime = utc("2024-07-01"),
                    deploy_lat = c(45, 45 + 2.8 / 111.2), deploy_lon = -80)
  t0 <- utc("2024-06-01 00:00:00")
  tt <- t0 + 120 * (0:359)
  ref_t <- t0 + 3600 * (0:23)
  det <- rbind(
    data.frame(datetime = tt, receiver_serial = "RA", transmitter = "F", animal_id = "f1"),
    data.frame(datetime = tt[seq(5, 350, by = 35)] + 2.5, receiver_serial = "RB",
               transmitter = "F", animal_id = "f1"),
    data.frame(datetime = c(ref_t, ref_t + 2), receiver_serial = rep(c("RA", "RB"), each = 24),
               transmitter = "R", animal_id = NA)
  )
  x <- detset(det, dep)
  n_fish <- sum(!is.na(det$animal_id))
  at2000 <- filter_speed(x, max_speed_ms = 3, min_dist_m = 2000)
  expect_equal(detset_log(at2000)$removed[2], 10L)
  with_ref <- filter_speed(x, max_speed_ms = 3, min_dist_m = 2000,
                           reference_tags = data.frame(transmitter = "R"))
  expect_equal(detset_log(with_ref)$removed[2], 0L)
  expect_equal(sum(!is.na(with_ref$detections$animal_id)), n_fish)
  expect_match(detset_log(with_ref)$reason[2], "1 station pair cleared by reference tags")
  at3000 <- filter_speed(x, max_speed_ms = 3, min_dist_m = 3000)
  expect_equal(detset_log(at3000)$removed[2], 0L)
  # A reference tag never co-detected clears nothing.
  expect_warning(other <- filter_speed(x, max_speed_ms = 3, min_dist_m = 2000,
                                       reference_tags = data.frame(transmitter = "NOPE")),
                 "1 reference tag given; it does not occur in the detections, so it clears")
  expect_equal(detset_log(other)$removed[2], 10L)
  expect_error(filter_speed(x, max_speed_ms = 3, min_dist_m = 2000,
                            reference_tags = data.frame(code = 1)), "transmitter")
})

test_that("speed_steps() returns the steps filter_speed() tests", {
  # Spikes 60 km away, one every 24 h: each spike is two violating steps.
  case <- speed_case(function(tt, t0) tt[seq(30, 360, by = 60)] + 30)
  steps <- speed_steps(case$x, max_speed_ms = 3, min_dist_m = 2000)
  expect_equal(nrow(steps), nrow(case$d) - 1L)
  far <- steps$from_station != steps$station_id
  expect_true(all(abs(steps$dist_m[far] - 60000) < 200))
  expect_equal(steps$speed_ms, ifelse(steps$dist_m == 0, 0, steps$dist_m / steps$secs))
  expect_equal(sum(steps$violation), 2L * sum(!case$d$truth))
  y <- filter_speed(case$x, max_speed_ms = 3, min_dist_m = 2000)
  expect_equal(sum(speed_steps(y, max_speed_ms = 3, min_dist_m = 2000)$violation), 0L)
  expect_false("violation" %in% names(speed_steps(case$x)))
  expect_error(speed_steps(case$x, max_speed_ms = 3), "Give both")
})

test_that("adjacent spikes lose only their least supported members", {
  # A B A B A: a false B 60 km away on each side of the true A at tt[101].
  # That A is a spike as well; it has about 60 supporting detections, each
  # B has 1, so both B go and the A stays.
  case <- speed_case(function(tt, t0) tt[c(100, 101)] + 30)
  steps <- speed_steps(case$x, max_speed_ms = 3, min_dist_m = 2000)
  expect_equal(sum(steps$violation), 4L)
  expect_equal(speed_score(case), c(tp = 2, fp = 0, fn = 0))
})

test_that("a set with no animal ids says the filter tested nothing", {
  case <- speed_case(function(tt, t0) tt[50] + 30)
  d <- case$d
  d$animal_id <- NA_character_
  x <- detset(d, case$x$deployments)
  expect_message(f <- filter_speed(x, max_speed_ms = 3, min_dist_m = 2000),
                 "no detection has an animal_id")
  expect_equal(nrow(f$detections), nrow(x$detections))
  expect_no_message(filter_speed(case$x, max_speed_ms = 3, min_dist_m = 2000))
})
