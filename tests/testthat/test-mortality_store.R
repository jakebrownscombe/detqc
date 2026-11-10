test_that("reconcile_mortality() names every call the new bands put in question", {
  x <- example_detset()
  mb <- detect_mortality(x)
  # Two animals set to the alive band, so every reason has a case whatever
  # the rules give.
  alive_band <- c("EX-04", "EX-05")
  mb$bands$band[mb$bands$animal_id %in% alive_band] <- "alive"
  mb$bands$band[mb$bands$animal_id == "EX-01"] <- "dead"
  b <- mb$bands
  calls <- data.frame(
    animal_id = c("EX-01", "EX-02", alive_band[1], alive_band[2], "EX-03"),
    call = c("alive", "dead_or_shed", "dead_or_shed", "uncertain", "dead_or_shed"),
    death_date = c("", "", "2023-06-01", "", "2023-08-20")
  )
  y <- suppressWarnings(apply_mortality_calls(x, write_calls(calls)))
  z <- reconcile_mortality(y, mb)
  r <- z$reconcile
  expect_equal(nrow(r), nrow(b))
  why <- stats::setNames(r$reason, r$animal_id)
  expect_equal(why[["EX-01"]], "called alive, now dead band")
  expect_equal(why[["EX-02"]], "dead_or_shed without a date")
  expect_equal(why[[alive_band[1]]], "called dead_or_shed, now alive band")
  expect_equal(why[[alive_band[2]]], "called uncertain")
  # Called at the true date, the day of the last move, EX-03 stays put after it.
  expect_true(is.na(why[["EX-03"]]))
  uncalled <- setdiff(b$animal_id[b$band %in% c("dead", "review")], calls$animal_id)
  expect_true(all(why[uncalled] == "no call, dead or review band"))
  expect_equal(sum(r$needs_review), 4L + length(uncalled))
  lg <- detset_log(z)
  expect_equal(lg$step[3], "reconcile_mortality")
  expect_match(lg$reason[3], sprintf("%d of 20 animals in the bands need another look",
                                     4L + length(uncalled)))
  expect_error(reconcile_mortality(x, mb), "apply_mortality_calls\\(\\) first")
  expect_error(reconcile_mortality(y, mb, moved_km = 0),
               "`moved_km` must be one finite positive number",
               class = "detqc_error")
  expect_error(reconcile_mortality(y, mb, moved_km = NA_real_), class = "detqc_error")
  expect_error(reconcile_mortality(y, mb, moved_km = Inf), "one finite positive number",
               class = "detqc_error")
  expect_error(reconcile_mortality(y, mb, sites_km = 1), "unused argument")
})

test_that("a dead_or_shed call heard far away after its date is marked as moved", {
  x <- example_detset()
  mb <- detect_mortality(x)
  # EX-04 is alive and ranges over the array; a death date early in its
  # record leaves detections kilometres away after it.
  d <- x$detections[x$detections$animal_id == "EX-04", ]
  early <- format(min(as.Date(d$datetime)) + 5)
  y <- apply_mortality_calls(x, write_calls(data.frame(animal_id = c("EX-04", "EX-01"),
                                                       call = "dead_or_shed",
                                                       death_date = c(early, "2023-07-15"))))
  mb$bands$band[mb$bands$animal_id == "EX-04"] <- "dead"
  r <- reconcile_mortality(y, mb)$reconcile
  expect_equal(r$reason[r$animal_id == "EX-04"], "moved after the death date")
  expect_true(is.na(r$reason[r$animal_id == "EX-01"]))
  expect_equal(moved_after_death(x, r[0, ], 1), character())
})

test_that("reconcile_mortality() reads moved_km and never the bands' sites_km", {
  x <- example_detset()
  d <- x$detections[x$detections$animal_id == "EX-04", ]
  early <- format(min(as.Date(d$datetime)) + 5)
  y <- apply_mortality_calls(x, write_calls(data.frame(animal_id = "EX-04",
                                                       call = "dead_or_shed",
                                                       death_date = early)))
  # Bands that group stations within 5 km and within 50 km into one site.
  mb5 <- detect_mortality(x, control = list(sites_km = 5))
  mb50 <- detect_mortality(x, control = list(sites_km = 50))
  expect_equal(mb50$settings$control$sites_km, 50)
  moved <- function(r) r$animal_id[r$reason %in% "moved after the death date"]
  r5 <- reconcile_mortality(y, mb5)
  r50 <- reconcile_mortality(y, mb50)
  expect_equal(moved(r5$reconcile), "EX-04")
  expect_equal(moved(r50$reconcile), "EX-04")
  lg <- detset_log(r50)
  expect_match(lg$parameters[nrow(lg)], "moved_km = 1")
  expect_no_match(lg$parameters[nrow(lg)], "sites_km")
  # A threshold wider than the array calls no detection a move.
  far <- reconcile_mortality(y, mb5, moved_km = 1000)
  expect_equal(moved(far$reconcile), character())
})
