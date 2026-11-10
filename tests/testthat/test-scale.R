# Scale: detset() and filter_min_lag() on synthetic detections of growing
# size, timed, with the peak memory R used. Off on CRAN and by default; set
# DETQC_SCALE_ROWS to a comma-separated list of row counts to run it, as
# DETQC_SCALE_ROWS=1e7,4e7,2e8.

scale_detections <- function(n, n_rx = 80L, n_tx = 400L) {
  set.seed(42)
  t0 <- as.numeric(as.POSIXct("2023-01-01", tz = "UTC"))
  tx <- sprintf("A69-1604-%05d", seq_len(n_tx))
  rx <- sprintf("4800%02d", seq_len(n_rx))
  data.frame(
    datetime = as.POSIXct(t0 + sort(stats::runif(n, 0, 3.15e7)), origin = "1970-01-01",
                          tz = "UTC"),
    receiver_serial = rx[sample.int(n_rx, n, TRUE)],
    transmitter = tx[sample.int(n_tx, n, TRUE)],
    animal_id = NA_character_, stringsAsFactors = FALSE
  )
}

# Peak memory R used since the last reset, in MB: the last column of gc().
peak_mb <- function() {
  g <- gc()
  sum(g[, ncol(g)])
}

scale_run <- function(n) {
  det <- scale_detections(n)
  dep <- data.frame(receiver_serial = unique(det$receiver_serial), station_id = "S",
                    deploy_datetime = as.POSIXct("2022-12-31", tz = "UTC"),
                    recover_datetime = as.POSIXct(NA, tz = "UTC"), deploy_lat = 45,
                    deploy_lon = -78)
  invisible(gc(reset = TRUE))
  t1 <- system.time(x <- detset(det, dep))[["elapsed"]]
  m1 <- peak_mb()
  rm(det)
  invisible(gc(reset = TRUE))
  t2 <- system.time(y <- filter_min_lag(x, max_lag_s = 3600))[["elapsed"]]
  m2 <- peak_mb()
  data.frame(rows = n, detset_s = t1, detset_peak_mb = m1, min_lag_s = t2,
             min_lag_peak_mb = m2, kept = nrow(y$detections))
}

test_that("detset() and filter_min_lag() scale", {
  testthat::skip_on_cran()
  sizes <- Sys.getenv("DETQC_SCALE_ROWS")
  testthat::skip_if(!nzchar(sizes), "set DETQC_SCALE_ROWS to run the scale test")
  rows <- as.numeric(strsplit(sizes, ",")[[1]])
  res <- do.call(rbind, lapply(rows, scale_run))
  print(res)
  expect_equal(res$rows, rows)
  expect_true(all(res$kept <= res$rows))
})

# A structured array for the co-detection checks: receivers on a 500 m grid,
# transmitters that walk it, each transmission heard at the animal's cell (p
# 0.6) and at each of its four neighbours (p 0.3) up to 2 s later. Random
# receivers, as above, give almost no co-detections, so they cannot show
# the memory cost of a self-join of all pairs.
scale_array <- function(n, nx = 10L, n_tx = 300L) {
  set.seed(11)
  per <- ceiling(n / n_tx / 1.8)
  tx <- rep(seq_len(n_tx), each = per)
  t <- as.numeric(as.POSIXct("2024-01-01", tz = "UTC")) +
    stats::ave(stats::runif(length(tx), 60, 120), tx, FUN = cumsum)
  walk <- function() {
    v <- sample.int(nx, n_tx, TRUE)[tx] - 1L +
      stats::ave(sample(-1:1, length(tx), TRUE, c(0.02, 0.96, 0.02)), tx, FUN = cumsum)
    p <- 2L * (nx - 1L)
    m <- v %% p
    ifelse(m > nx - 1L, p - m, m)
  }
  ix <- walk()
  iy <- walk()
  hear <- function(dx, dy, p) {
    jx <- ix + dx
    jy <- iy + dy
    ok <- jx >= 0 & jx < nx & jy >= 0 & jy < nx & stats::runif(length(tx)) < p
    data.table::data.table(t = t[ok] + if (p < 0.5) stats::runif(sum(ok), 0, 2) else 0,
                           k = jy[ok] * nx + jx[ok] + 1L, tx = tx[ok])
  }
  d <- data.table::rbindlist(list(hear(0, 0, 0.6), hear(1, 0, 0.3), hear(-1, 0, 0.3),
                                  hear(0, 1, 0.3), hear(0, -1, 0.3)))
  d <- d[sort(sample.int(nrow(d), min(n, nrow(d))))]
  g <- expand.grid(ix = seq_len(nx) - 1L, iy = seq_len(nx) - 1L)
  dep <- data.frame(receiver_serial = sprintf("49%04d", seq_len(nrow(g))),
                    station_id = sprintf("G%03d", seq_len(nrow(g))),
                    deploy_datetime = as.POSIXct("2023-12-31", tz = "UTC"),
                    recover_datetime = as.POSIXct(NA, tz = "UTC"),
                    deploy_lat = 45 + g$iy * 500 / 111195,
                    deploy_lon = -79 + g$ix * 500 / (111195 * cos(45 * pi / 180)))
  det <- data.frame(datetime = as.POSIXct(d$t, origin = "1970-01-01", tz = "UTC"),
                    receiver_serial = dep$receiver_serial[d$k],
                    transmitter = sprintf("A69-9001-%05d", d$tx), animal_id = NA_character_)
  list(detections = det, deployments = dep)
}

test_that("the co-detection checks scale", {
  testthat::skip_on_cran()
  sizes <- Sys.getenv("DETQC_SCALE_ROWS")
  testthat::skip_if(!nzchar(sizes), "set DETQC_SCALE_ROWS to run the scale test")
  rows <- as.numeric(strsplit(sizes, ",")[[1]])
  res <- do.call(rbind, lapply(rows, function(n) {
    a <- scale_array(n)
    x <- detset(a$detections, a$deployments)
    rm(a)
    invisible(gc(reset = TRUE))
    tm <- system.time(r <- check_detset(x, checks = c("moved_mooring",
                                                      "codetection_out_of_range"),
                                        field = 800))[["elapsed"]]
    data.frame(rows = nrow(x$detections), codet_s = tm, codet_peak_mb = peak_mb(),
               tested = r$checks$summary$of[1])
  }))
  print(res)
  expect_true(all(res$tested > 0))
})
