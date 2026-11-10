# The checks of check_detset() on single planted errors. Each test builds
# its error on the planted base (fixtures/planted).

utc <- function(s) as.POSIXct(s, tz = "UTC")
flags <- function(res) {
  s <- res$summary[res$summary$status == "flag", ]
  stats::setNames(s$flagged, s$check)
}

# The receiver moved from S1 on 2023-08-10 hears what the receiver at `to`
# hears, 0.3 s later.
move_s1 <- function(base, to, keep_ref = FALSE) {
  d <- base$detections
  dep <- base$deployments
  dest <- utils::tail(dep$receiver_serial[dep$station_id == to], 1)
  mv <- d[d$receiver_serial == dest & d$datetime >= utc("2023-08-10"), ]
  mv$receiver_serial <- "480011"
  mv$datetime <- mv$datetime + 0.3
  gone <- d$receiver_serial == "480011" & d$datetime >= utc("2023-08-10")
  if (keep_ref) gone <- gone & d$transmitter != "A69-1604-9001"
  base$detections <- rbind(d[!gone, ], mv)
  base
}

split_of <- function(base, dep_row) {
  ctx <- check_context(base[c("detections", "deployments", "animals")], check_settings(), NULL,
                       base$field_m, base$reference_tags)
  sp <- partner_splits(ctx)
  sp[sp$.dep == dep_row]
}

test_that("co-detection pairs match a plain self-join", {
  base <- planted_base()
  ctx <- check_context(base[c("detections", "deployments")], check_settings(), NULL, NULL, NULL)
  p <- codetections(ctx)
  # A plain self-join over a 5 s window is the oracle.
  d <- ctx$det[!is.na(.dep), list(transmitter, t = as.numeric(datetime), .row,
                                  rx = receiver_serial)]
  q <- d[, list(transmitter, t0 = t, t1 = t + 5, row_a = .row, rx_a = rx)]
  old <- d[q, on = list(transmitter, t >= t0, t <= t1), nomatch = NULL, allow.cartesian = TRUE,
           list(row_a, rx_a, row_b = x..row, rx_b = x.rx)]
  old <- old[rx_a != rx_b]
  key <- function(a, b) paste(pmin(a, b), pmax(a, b))
  old_keys <- unique(key(old$row_a, old$row_b))
  expect_setequal(key(p$row_a, p$row_b), old_keys)
  expect_equal(nrow(p), length(old_keys))
  expect_true(all(ctx$det$datetime[p$row_a] <= ctx$det$datetime[p$row_b]))
  expect_equal(nrow(codetect_pairs("T", 1, "R", 5)), 0L)
  expect_equal(nrow(codetect_pairs(c("T", "T"), c(1, 2), c("R", "R"), 5)), 0L)
  expect_equal(as.integer(unlist(codetect_pairs(c("T", "T", "T"), c(9, 1, 3), c("A", "B", "C"),
                                                5))), c(2L, 3L))
})

test_that("a move of one grid spacing scores below the threshold", {
  base <- planted_base()
  # 600 m and 849 m are missed; 1,200 m is caught. The roxygen states this
  # limit.
  near <- split_of(move_s1(base, "S2"), 2L)
  diag <- split_of(move_s1(base, "S6"), 2L)
  far <- split_of(move_s1(base, "S3"), 2L)
  expect_lt(near$tv, 0.75)
  expect_lt(diag$tv, 0.75)
  expect_gte(far$tv, 0.75)
  expect_false(near$flag)
  expect_true(far$flag)
  expect_equal(far$basis, "animals")
})

test_that("reference tags that keep their partners overrule an animal flag", {
  base <- planted_base()
  moved <- move_s1(base, "S8")
  expect_true(split_of(moved, 2L)$flag)
  # The same animal change with the reference tag still heard at S1 with S2:
  # the mooring did not move.
  kept <- split_of(move_s1(base, "S8", keep_ref = TRUE), 2L)
  expect_gte(kept$tv, 0.75)
  expect_true(kept$overruled)
  expect_false(kept$flag)
  res <- planted_checks(move_s1(base, "S8", keep_ref = TRUE))
  expect_false("moved_mooring" %in% names(flags(res)))
  expect_match(res$summary$note[res$summary$check == "moved_mooring"], "1 animal flag overruled")
})

test_that("a receiver logging on deck is not a time-zone offset", {
  base <- planted_base()
  dep <- base$deployments
  rec <- which(!is.na(dep$recover_datetime))
  expect_length(rec, 9L)
  for (side in c("after", "before")) {
    for (k in c(5L, 9L)) {
      b <- base
      add <- do.call(rbind, lapply(rec[seq_len(k)], function(i) {
        t <- if (side == "after") dep$recover_datetime[i] + 300 * (1:18) else
          dep$deploy_datetime[i] - 300 * (1:18)
        data.frame(datetime = t, receiver_serial = dep$receiver_serial[i],
                   transmitter = "A69-1604-9001", animal_id = NA)
      }))
      b$detections <- rbind(b$detections, add)
      f <- flags(planted_checks(b))
      expect_false("non_utc" %in% names(f), label = paste(side, k))
      expect_equal(f[["outside_deployment"]], nrow(add), label = paste(side, k))
    }
  }
  # A true four-hour offset is still found (the planted patch).
  f <- flags(planted_checks(apply_patch(base, planted_patch("non_utc"))))
  expect_named(f, "non_utc")
})

test_that("a check with nothing to test reports not run", {
  base <- planted_base()
  d <- base$detections
  d <- d[d$datetime >= utc("2023-07-02 00:00:00") & d$datetime < utc("2023-07-02 20:00:00") &
           d$receiver_serial != "480005", ]
  tabs <- list(detections = d, deployments = base$deployments)
  res <- check_detset(tabs, checks = c("receiver_silent", "non_utc", "duplicate_rows"))
  s <- res$summary
  expect_equal(s$status, c("not run", "not run", "pass"))
  expect_match(s$note[1], "nothing to test, the denominator is 0")
  expect_true(is.na(s$of[1]))
  expect_output(print(res), "0 flagged, 1 passed, 2 not run")
})

test_that("dst_jump flags only rows in logging order and never passes", {
  base <- apply_patch(planted_base(), planted_patch("dst_jump"))
  tabs <- base[c("detections", "deployments")]
  st <- function(det) {
    check_detset(list(detections = det, deployments = tabs$deployments),
                 checks = "dst_jump")$summary
  }
  arrival <- st(tabs$detections)
  expect_equal(arrival$status, "flag")
  expect_equal(arrival$flagged, 1L)
  expect_match(arrival$note, "^1 step back of 30 to 90 minutes")
  sorted <- st(tabs$detections[order(tabs$detections$datetime), ])
  expect_equal(sorted$status, "not run")
  expect_match(sorted$note, "sorted by time")
  set.seed(4)
  clean <- planted_base()$detections
  for (k in 1:5) {
    shuffled <- st(clean[sample(nrow(clean)), ])
    expect_equal(shuffled$status, "not run")
    expect_match(shuffled$note, "10 have steps back that no clock jump explains")
  }
  # A jump forward leaves no step back: never visible.
  fwd <- clean
  i <- which(fwd$receiver_serial == "480006" & fwd$datetime > utc("2023-07-15"))
  fwd$datetime[i] <- fwd$datetime[i] + 3600
  expect_equal(st(fwd)$status, "not run")
})

test_that("an overlapping deployment is not also out of range", {
  base <- planted_base()
  for (cs in list(c("480006", "S5"), c("480003", "S8"), c("480004", "S3"))) {
    b <- base
    dep <- b$deployments
    add <- dep[dep$station_id == cs[2], ][1, ]
    add$receiver_serial <- cs[1]
    add$deploy_datetime <- utc("2023-08-16 12:00:00")
    b$deployments <- rbind(dep, add)
    f <- flags(planted_checks(b))
    expect_named(f, "overlapping_deployments")
    expect_equal(f[[1]], 2L)
  }
})

test_that("both forms of one water body agree off the edges", {
  testthat::skip_if_not_installed("sf")
  base <- planted_base()
  lk <- base$lake
  r <- planted_water(base)
  ring <- matrix(c(lk$lon[1], lk$lat[1], lk$lon[2], lk$lat[1], lk$lon[2], lk$lat[2],
                   lk$lon[1], lk$lat[2], lk$lon[1], lk$lat[1]), ncol = 2, byrow = TRUE)
  poly <- sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  lat <- c(mean(lk$lat), mean(lk$lat), lk$lat[1] - 0.002, mean(lk$lat))
  lon <- c(mean(lk$lon), lk$lon[2] + 0.002, mean(lk$lon), lk$lon[1])
  a <- in_water(r, lat, lon)
  b <- in_water(poly, lat, lon)
  expect_equal(a[1:3], c(TRUE, FALSE, FALSE))
  expect_equal(b[1:3], a[1:3])
  # On the west edge each form gives an answer, not NA; they may differ, as
  # the roxygen of check_detset() says.
  expect_false(is.na(a[4]))
  expect_false(is.na(b[4]))
})
