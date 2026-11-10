# ---- log_step(): the record ----

test_that("detset() writes one record with every field of CONTRACT.md section 7", {
  x <- example_detset()
  expect_named(x$log, detqc:::log_fields)
  expect_equal(nrow(x$log), 1L)
  expect_identical(x$log$fn, "detset")
  expect_identical(x$log$detqc_version, as.character(utils::packageVersion("detqc")))
  expect_identical(x$log$removed_by[[1]], stats::setNames(integer(), character()))
  expect_silent(detqc:::check_log(x$log))
})

test_that("log_step() reads counts after from x and takes counts before three ways", {
  x <- example_detset()
  y <- x
  y$detections <- y$detections[-(1:10), ]
  a <- detqc:::log_step(y, "filter_a", "k=1", x, c(why = 10L), "dropped ten")
  b <- detqc:::log_step(y, "filter_a", "k=1", x$detections, c(why = 10L), "dropped ten")
  cc <- detqc:::log_step(y, "filter_a", "k=1", c(40506, 20), c(why = 10L), "dropped ten")
  for (z in list(a, b, cc)) {
    r <- z$log[2, ]
    expect_equal(c(r$rows_before, r$rows_after, r$removed), c(40506L, 40496L, 10L))
    expect_equal(c(r$animals_before, r$animals_after), c(20L, 20L))
    expect_identical(r$removed_by[[1]], c(why = 10L))
  }
  expect_error(detqc:::log_step(y, "f", "", "x", 0L), "`before` must be")
  expect_error(detqc:::log_step(y, "f", "", c(1, -1), 0L), "`before` must be")
})

test_that("log_step() warns when rows vanish with no reason and allows added rows", {
  x <- example_detset()
  y <- x
  y$detections <- y$detections[-(1:5), ]
  expect_warning(detqc:::log_step(y, "filter_b", "", x, 2L), "lost 3 detections")
  z <- x
  z$detections <- rbind(z$detections, z$detections[1:4, ])
  out <- expect_silent(detqc:::log_step(z, "read_append", "", x, 0L, "appended"))
  expect_equal(out$log$rows_after[2] - out$log$rows_before[2], 4L)
})

test_that("removed counts are checked and named", {
  rc <- detqc:::removal_counts
  expect_identical(rc(0L), stats::setNames(integer(), character()))
  expect_identical(rc(integer()), stats::setNames(integer(), character()))
  expect_identical(rc(7), c(no_breakdown = 7L))
  expect_identical(rc(c(a = 0, b = 2)), c(a = 0L, b = 2L))
  expect_error(rc(c(1, 2)), "must name each reason")
  expect_error(rc(c(a = 1, a = 2)), "unique name")
  expect_error(rc(stats::setNames(1, "")), "unique name")
  expect_error(rc(-1), "whole, non-negative")
  expect_error(rc(1.5), "whole, non-negative")
  expect_error(rc("3"), "whole, non-negative")
})

test_that("parameters are deparsed into one fixed form", {
  dp <- detqc:::deparse_parameters
  expect_identical(dp("crs=4326"), "crs=4326")
  expect_identical(dp(list()), "")
  expect_identical(dp(list(min_lag = 3600, by = "animal_id")),
                   "min_lag = 3600; by = \"animal_id\"")
  expect_identical(dp(quote(filter_x(x, k = 2))), "filter_x(x, k = 2)")
  x <- example_detset()
  expect_identical(dp(list(x = x, d = x$deployments, f = mean)),
                   "x = <detset>; d = <data frame, 25 rows>; f = <function>")
  expect_match(dp(list(v = 1:50)), "^v = 1:10 \\.\\.\\. \\(50 values\\)$")
  expect_match(dp(list(s = strrep("a", 400))), "\\.\\.\\.$")
  expect_lte(nchar(dp(list(s = strrep("a", 400)))), 304L)
  expect_error(dp(list(1)), "needs a name")
  expect_error(dp(data.frame(a = 1)), "named list, a call or one string")
  expect_error(dp(NA_character_), "named list, a call or one string")
})

test_that("step, fn and reason must be one string", {
  x <- example_detset()
  expect_error(detqc:::log_step(x, c("a", "b"), "", x), "`step` must be one string")
  expect_error(detqc:::log_step(x, "a", "", x, fn = NA_character_), "`fn` must be one string")
  expect_error(detqc:::log_step(x, "a", "", x, reason = 1), "`reason` must be one string")
})

# ---- Log shape and the removals table ----

test_that("a log without the twelve fields of CONTRACT.md section 7 is rejected", {
  x <- example_detset()
  x$log <- x$log[, c("step", "at", "parameters", "rows_before", "rows_after", "animals_before",
                     "animals_after", "removed", "reason")]
  expect_error(validate_detset(x), "`log` has no column `fn`")
})

test_that("check_log() stops when reasons do not sum to removed", {
  x <- example_detset()
  lg <- x$log
  lg$removed <- 3L
  expect_error(detqc:::check_log(lg), "removes 3 detections but its reasons sum to 0")
  lg$removed_by <- "a"
  lg$removed <- 0L
  expect_error(detqc:::check_log(lg), "list of named counts")
})

test_that("log_removals() gives one row per step and reason with its denominator", {
  x <- example_detset()
  expect_equal(nrow(detqc:::log_removals(x$log)), 0L)
  y <- x
  y$detections <- y$detections[-(1:7), ]
  y <- detqc:::log_step(y, "filter_c", "", x, c(dup = 5L, early = 2L, late = 0L), "x")
  r <- detqc:::log_removals(y$log)
  expect_equal(r$reason, c("dup", "early", "late"))
  expect_equal(r$removed, c(5L, 2L, 0L))
  expect_equal(r$of, rep(40506L, 3))
  expect_equal(r$record, rep(2L, 3))
})

# ---- The arithmetic between records ----

test_that("log_breaches() is empty on a clean log and names each breach by record", {
  x <- example_detset()
  b <- detqc:::log_breaches(x$log)
  expect_named(b, c("record", "step", "kind", "expected", "found"))
  expect_equal(nrow(b), 0L)
  expect_equal(nrow(detqc:::log_breaches(x$log[0, ])), 0L)
  y <- x
  y$detections <- y$detections[-(1:3), ]
  y <- suppressWarnings(detqc:::log_step(y, "filter_leak", "", x, 1L))
  b <- detqc:::log_breaches(y$log)
  expect_identical(b$kind, "lost")
  expect_equal(c(b$record, b$expected, b$found), c(2, 40505, 40503))
})

test_that("log_step() checks the new record against the one before it", {
  x <- example_detset()
  x$detections <- x$detections[-(1:7), ]
  w <- capture_warnings(detqc:::log_step(x, "check_a", "", x, 0L))
  expect_match(w, "starts from 40,499 detections, but the record before it ended at 40,506",
               all = FALSE, fixed = TRUE)
  expect_error(detqc:::log_step(x, "filter_a", "", x, 40500L),
               "counts 40,500 detections removed, but only 40,499 came in")
})
