# filter_min_lag(): glatos false_detections() is the oracle.

test_that("minimum lags follow the definition", {
  utc <- function(s) as.POSIXct(s, tz = "UTC")
  det <- data.frame(
    datetime = utc("2024-01-01 00:00:00") + c(0, 60, 1000, 5000, 0, 10, 0),
    receiver_serial = c("R1", "R1", "R1", "R1", "R2", "R2", "R3"),
    transmitter = c("T", "T", "T", "T", "T", "T", "T")
  )
  expect_equal(nearest_lag(det), c(60, 60, 940, 4000, 10, 10, NA))
  expect_equal(nearest_lag(det[0, ]), numeric())
  tt <- toy_tables()
  x <- detset(transform(det, animal_id = NA_character_),
              data.frame(receiver_serial = c("R1", "R2", "R3"), station_id = c("A", "B", "C"),
                         deploy_datetime = utc("2023-12-31"), recover_datetime = utc(NA),
                         deploy_lat = 45, deploy_lon = -78))
  y <- filter_min_lag(x, max_lag_s = 3600, min_lag_s = 30)
  expect_equal(nrow(y$detections), 3L)
  expect_equal(y$log$removed_by[[2]], c(isolated = 2L, too_close = 2L))
  expect_error(filter_min_lag(x), "max_lag_s")
})

test_that("filter_min_lag keeps the rows glatos false_detections() keeps", {
  testthat::skip_if_not_installed("glatos")
  runs <- 0L
  for (f in c("walleye_detections.csv", "lamprey_detections.csv")) {
    path <- system.file("extdata", f, package = "glatos")
    testthat::skip_if(!nzchar(path), paste("glatos does not ship", f))
    d <- read_detections(path, "glatos")
    dep <- data.table::as.data.table(d)[, list(
      station_id = "all", deploy_datetime = min(datetime), recover_datetime = max(datetime) + 1,
      deploy_lat = 45, deploy_lon = -80
    ), by = "receiver_serial"]
    x <- suppressWarnings(detset(d, as.data.frame(dep)))
    g <- glatos::read_glatos_detections(path)
    # A GLATOS export carries the server's min_lag column, which
    # false_detections() reuses; drop it so glatos computes the lags from
    # these rows. With it, tf = 120 differs by 1 lamprey row.
    g$min_lag <- NULL
    for (tf in c(3600, 1800, 120)) {
      k <- filter_min_lag(x, max_lag_s = tf)$detections
      gf <- suppressMessages(glatos::false_detections(g, tf = tf))
      kg <- gf[gf$passed_filter == 1, ]
      kp <- paste(k$transmitter, k$receiver_serial, format(k$datetime, "%Y-%m-%d %H:%M:%OS3"))
      kq <- paste(paste(kg$transmitter_codespace, kg$transmitter_id, sep = "-"), kg$receiver_sn,
                  format(kg$detection_timestamp_utc, "%Y-%m-%d %H:%M:%OS3"))
      expect_identical(sort(kp), sort(kq))
      runs <- runs + 1L
    }
  }
  expect_equal(runs, 6L)
})

test_that("minimum_lags() returns the lags filter_min_lag() tests", {
  ex <- detqc_example
  dets <- detset(ex$detections, ex$deployments, ex$animals)
  lags <- minimum_lags(dets)
  expect_length(lags, nrow(dets$detections))
  expect_identical(lags, minimum_lags(dets$detections))
  for (max_lag in c(600, 3600, 86400)) {
    kept <- filter_min_lag(dets, max_lag_s = max_lag)
    expect_equal(nrow(dets$detections) - nrow(kept$detections),
                 sum(is.na(lags) | lags > max_lag))
    expect_equal(detset_log(kept)$removed[nrow(detset_log(kept))],
                 sum(is.na(lags) | lags > max_lag))
  }
  expect_error(minimum_lags(1:3), "detset or a data frame")
  expect_error(minimum_lags(data.frame(transmitter = "a")), "receiver_serial")
})

# ---- Pincock's second condition: long_lag_s ----

# A detset of hand-built detections on receivers R1 and R2, all in 2024.
lag_case <- function(tx, rx, s) {
  utc <- function(v) as.POSIXct(v, tz = "UTC")
  det <- data.frame(datetime = utc("2024-01-01 00:00:00") + s, receiver_serial = rx,
                    transmitter = tx, animal_id = rep(NA_character_, length(s)),
                    stringsAsFactors = FALSE)
  dep <- data.frame(receiver_serial = c("R1", "R2"), station_id = c("A", "B"),
                    deploy_datetime = utc("2023-12-31"), recover_datetime = utc(NA),
                    deploy_lat = 45, deploy_lon = -78)
  detset(det, dep)
}

test_that("long_lag_s = NULL applies the first condition only", {
  ex <- detqc_example
  dets <- detset(ex$detections, ex$deployments, ex$animals)
  for (m in c(600, 3600, 4500)) {
    a <- filter_min_lag(dets, max_lag_s = m)
    b <- filter_min_lag(dets, max_lag_s = m, long_lag_s = NULL)
    expect_identical(a$detections, b$detections)
    expect_identical(detset_log(a)$parameters, detset_log(b)$parameters)
    expect_identical(detset_log(a)$reason, detset_log(b)$reason)
    # With long_lag_s = NULL the parameters text does not name long_lag_s.
    expect_false(grepl("long_lag_s", detset_log(a)$parameters[2]))
  }
  # The text of the default record, for max_lag_s = 3600.
  expect_identical(detset_log(filter_min_lag(dets, 3600))$reason[2],
                   "removed 51 of 3,297 detections (1.55%); 0 animals lost every detection")
})

test_that("the second condition counts intervals per transmitter on each receiver", {
  # max_lag_s = 300, long_lag_s = 7200.
  # T1 on R1 at 0, 100, 86400 and 172800 s. Intervals: 100 (short),
  #   86300 and 86400 (long). Short 1, long 2: the pair fails. The first
  #   condition keeps 0 and 100 (lag 100) and removes 86400 and 172800
  #   (lags 86300 and 86400, isolated), so 2 go as isolated and 2 as
  #   few_short_intervals.
  # T1 on R2 at 0, 100, 200 and 86400 s. Intervals 100, 100 (short), 86200
  #   (long). Short 2, long 1: passes. 86400 is isolated. 1 removed.
  # T2 on R1 at 0, 0 (a tie) and 50 s. Intervals 0 and 50, both short with
  #   min_lag_s = 0; long 0: passes, nothing removed.
  # T3 on R2, one detection at 500 s: no interval, short 0 = long 0, fails;
  #   it is isolated under the first condition, which names it.
  # T4 on R1, a code no animal carries and no delay is known for, at 0, 60
  #   and 120 s: short 2, long 0, passes.
  x <- lag_case(
    tx = c(rep("T1", 4), rep("T1", 4), rep("T2", 3), "T3", rep("T4", 3)),
    rx = c(rep("R1", 4), rep("R2", 4), rep("R1", 3), "R2", rep("R1", 3)),
    s = c(0, 100, 86400, 172800, 0, 100, 200, 86400, 0, 0, 50, 500, 0, 60, 120)
  )
  y <- filter_min_lag(x, max_lag_s = 300, long_lag_s = 7200)
  expect_equal(nrow(y$detections), 15L - 6L)
  expect_equal(y$log$removed_by[[2]], c(few_short_intervals = 2L, isolated = 4L))
  kept <- paste(y$detections$transmitter, y$detections$receiver_serial)
  expect_equal(sum(kept == "T1 R1"), 0L)
  expect_equal(sum(kept == "T1 R2"), 3L)
  expect_equal(sum(kept == "T2 R1"), 3L)
  expect_equal(sum(kept == "T4 R1"), 3L)
  # Pairs: T1 R1, T1 R2, T2 R1, T3 R2, T4 R1; T1 R1 and T3 R2 fail.
  expect_match(y$log$reason[2], "2 of 5 transmitter and receiver pairs had no more short")
  expect_match(y$log$parameters[2], "long_lag_s = 7200", fixed = TRUE)
  # Without the option, the first condition alone removes the 4 isolated.
  z <- filter_min_lag(x, max_lag_s = 300)
  expect_equal(z$log$removed_by[[2]], c(isolated = 4L))
})

test_that("an equal count of short and long intervals fails, as Pincock asks for more", {
  # T1 on R1 at 0, 100 and 90000 s: intervals 100 (short) and 89900 (long).
  # Short 1 equals long 1, which is no majority: every detection goes, 90000 as
  # isolated and 0 and 100 as few_short_intervals.
  x <- lag_case("T1", "R1", c(0, 100, 90000))
  y <- filter_min_lag(x, max_lag_s = 300, long_lag_s = 7200)
  expect_equal(nrow(y$detections), 0L)
  expect_equal(y$log$removed_by[[2]], c(few_short_intervals = 2L, isolated = 1L))
  # One more short interval (a detection at 200 s) makes it pass: short 2,
  # long 1; only 90000 goes.
  y2 <- filter_min_lag(lag_case("T1", "R1", c(0, 100, 200, 90000)), 300, long_lag_s = 7200)
  expect_equal(y2$log$removed_by[[2]], c(isolated = 1L))
})

test_that("a code heard only in close pairs passes; lone detections between pairs fail", {
  # Four close pairs two days apart: T1 on R1 at 0 and 100 s, then the same
  # two offsets on days 2, 4 and 6. Intervals: 4 of 100 s (short) and 3
  # near 2 days (long). Short 4, long 3: more short than long, so all 8 are
  # kept. This is Pincock's rule; it does not remove a code for being rare.
  d <- 2 * 86400
  s1 <- c(0, 100, d, d + 100, 2 * d, 2 * d + 100, 3 * d, 3 * d + 100)
  y <- filter_min_lag(lag_case("T1", "R1", s1), 300, long_lag_s = 7200)
  expect_equal(nrow(y$detections), 8L)
  expect_match(y$log$reason[2], "0 of 1 transmitter and receiver pair had")
  # Two close pairs with three lone detections between them: T1 on R1 at 0
  # and 100 s, alone on days 2, 4 and 6, and on day 8 at 0 and 100 s.
  # Intervals: 2 short, 4 long. The pair fails: the 3 lone detections go as
  # isolated and the 4 in close pairs as few_short_intervals.
  s2 <- c(0, 100, d, 2 * d, 3 * d, 4 * d, 4 * d + 100)
  y2 <- filter_min_lag(lag_case("T1", "R1", s2), 300, long_lag_s = 7200)
  expect_equal(nrow(y2$detections), 0L)
  expect_equal(y2$log$removed_by[[2]], c(few_short_intervals = 4L, isolated = 3L))
  # Four close pairs with one lone detection on day 4 between them: short
  # 4, long 4, a tie. All 9 go: 8 as few_short_intervals, 1 as isolated.
  s3 <- c(0, 100, d, d + 100, 2 * d, 3 * d, 3 * d + 100, 4 * d, 4 * d + 100)
  y3 <- filter_min_lag(lag_case("T1", "R1", s3), 300, long_lag_s = 7200)
  expect_equal(nrow(y3$detections), 0L)
  expect_equal(y3$log$removed_by[[2]], c(few_short_intervals = 8L, isolated = 1L))
})

test_that("intervals between long_lag_s and max_lag_s count as neither", {
  # T1 on R1 at 0, 100 and 3700 s: intervals 100 (short) and 3600, which is
  # above max_lag_s = 300 and not above long_lag_s = 7200. Short 1, long 0:
  # passes. The first condition removes 3700 (lag 3600).
  y <- filter_min_lag(lag_case("T1", "R1", c(0, 100, 3700)), 300, long_lag_s = 7200)
  expect_equal(y$log$removed_by[[2]], c(isolated = 1L))
  # An interval of exactly long_lag_s is not long; exactly max_lag_s is short.
  y2 <- filter_min_lag(lag_case("T1", "R1", c(0, 300, 7500)), 300, long_lag_s = 7200)
  expect_equal(y2$log$removed_by[[2]], c(isolated = 1L))
})

test_that("ties and min_lag_s: an interval under min_lag_s is not short", {
  # T1 on R1 at 0, 0 and 20000 s. With min_lag_s = 0: intervals 0 (short)
  # and 20000 (long), short 1 = long 1, fails; 20000 is isolated, the two
  # tied detections go as few_short_intervals.
  x <- lag_case("T1", "R1", c(0, 0, 20000))
  y <- filter_min_lag(x, max_lag_s = 300, long_lag_s = 7200)
  expect_equal(y$log$removed_by[[2]], c(few_short_intervals = 2L, isolated = 1L))
  # With min_lag_s = 1 the tie is too close under the first condition, which
  # names it first.
  y1 <- filter_min_lag(x, max_lag_s = 300, min_lag_s = 1, long_lag_s = 7200)
  expect_equal(y1$log$removed_by[[2]], c(isolated = 1L, too_close = 2L))
})

test_that("one detection, an empty set and bad values of long_lag_s", {
  y <- filter_min_lag(lag_case("T1", "R1", 0), max_lag_s = 300, long_lag_s = 7200)
  expect_equal(nrow(y$detections), 0L)
  expect_equal(y$log$removed_by[[2]], c(isolated = 1L))
  expect_match(y$log$reason[2], "1 of 1 transmitter and receiver pair had")
  e <- lag_case(character(), character(), numeric())
  ye <- filter_min_lag(e, max_lag_s = 300, long_lag_s = 7200)
  expect_equal(nrow(ye$detections), 0L)
  expect_match(ye$log$reason[2], "0 of 0 transmitter and receiver pairs")
  x <- lag_case("T1", "R1", c(0, 100))
  expect_error(filter_min_lag(x, 300, long_lag_s = 300), "must exceed `max_lag_s`")
  expect_error(filter_min_lag(x, 300, long_lag_s = -1), "long_lag_s")
  expect_error(filter_min_lag(x, 300, long_lag_s = c(1e4, 2e4)), "long_lag_s")
})

test_that("the second condition removes only what the first kept, on the example", {
  ex <- detqc_example
  dets <- detset(ex$detections, ex$deployments, ex$animals)
  max_lag <- 30 * mean((ex$animals$min_delay + ex$animals$max_delay) / 2)
  one <- filter_min_lag(dets, max_lag_s = max_lag)
  two <- filter_min_lag(dets, max_lag_s = max_lag, long_lag_s = 24 * max_lag)
  # Every detection the two-condition filter keeps, the one-condition keeps.
  k1 <- paste(one$detections$transmitter, one$detections$receiver_serial,
              as.numeric(one$detections$datetime))
  k2 <- paste(two$detections$transmitter, two$detections$receiver_serial,
              as.numeric(two$detections$datetime))
  expect_true(all(k2 %in% k1))
  rb <- two$log$removed_by[[2]]
  expect_equal(unname(rb["isolated"]), one$log$removed[2])
  expect_equal(nrow(dets$detections) - nrow(two$detections), sum(rb))
})

test_that("the help's worked example gives 8 kept, then a tie and 9 removed", {
  # As in ?filter_min_lag: max_lag_s 3600 and long_lag_s 86400. Four close
  # pairs, 60 s apart within a pair and 3 days apart between pairs: short 4,
  # long 3, all 8 stay. One lone detection midway between the first two
  # pairs, 1.5 days from each, splits one long interval into two that both
  # exceed 86400 s: short 4, long 4, a tie, and all 9 go.
  d <- 3 * 86400
  pairs <- as.vector(outer(c(0, 60), d * 0:3, "+"))
  y <- filter_min_lag(lag_case("T1", "R1", pairs), max_lag_s = 3600, long_lag_s = 86400)
  expect_equal(nrow(y$detections), 8L)
  lone <- sort(c(pairs, 60 + (d - 60) / 2))
  expect_true(all(diff(lone)[diff(lone) > 3600] > 86400))
  y2 <- filter_min_lag(lag_case("T1", "R1", lone), max_lag_s = 3600, long_lag_s = 86400)
  expect_equal(nrow(y2$detections), 0L)
  expect_equal(y2$log$removed_by[[2]], c(few_short_intervals = 8L, isolated = 1L))
})
