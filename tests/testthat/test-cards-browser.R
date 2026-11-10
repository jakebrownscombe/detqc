# Headless Chrome checks of a card and the index: the page runs with no
# script error and no network request, the scrubber moves the cursor date,
# and a call with its date goes through the browser store, the CSV and
# apply_mortality_calls() unchanged.

test_that("a card runs headless, scrubs, refuses a first-day stamp and round-trips a call", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  r <- example_review(dir)
  out <- r$y$cards
  res <- run_card_check(out$file[out$animal_id == "EX-03"], attr(out, "index"))
  expect_null(res$failure)
  expect_length(res$errors, 0)
  expect_length(res$network_requests_not_file, 0)
  expect_true(res$has_payload)
  # The readout sits beside the map and the call bar loads in view at 1500 x
  # 1000.
  expect_true(res$readout_beside_map)
  expect_lt(res$callbar_bottom, res$inner_h)
  # The time panels start in the first screen, with room for the station
  # panel's first rows.
  expect_lt(res$stack_top + 60, res$inner_h)
  # The scrubber: arrows, the slider and play move the cursor date. The axis
  # starts at the first day of the detset, 2023-05-02, because EX-03 was
  # released the day before it; the card opens on EX-03's first day with a
  # detection, 2023-05-03, day 1 of the axis.
  expect_equal(res$date_start, "2023-05-03")
  expect_equal(res$date_after_3_right, "2023-05-06")
  expect_equal(res$date_after_slider_40, "2023-06-11")
  expect_gt(res$i_after_play_600ms, 40)
  # On a fresh card X and "= cursor" stamp nothing: the day in view is the
  # first day of the record. D stamps the candidate.
  expect_equal(res$i_on_open, 1L)
  expect_null(res$x_on_open)
  expect_null(res$button_on_open)
  expect_equal(res$hint_on_open, "move the cursor to the death day first")
  expect_equal(res$candidate, "2023-08-18")
  expect_equal(res$stamp_on_open, res$candidate)
  expect_equal(res$stamp_src_on_open, "candidate")
  expect_match(res$deathdate_text_on_open, "2023-08-18 \\(candidate\\)")
  # Once the cursor has moved, D still stamps the candidate and only X takes
  # the day in view, so a stray arrow key and D cannot stamp a day the
  # reviewer never chose. X moves a date already set.
  expect_false(identical(res$cursor_before_call, res$candidate))
  expect_equal(res$stamp_after_move, res$candidate)
  expect_equal(res$x_after_move, res$cursor_before_call)
  expect_equal(res$hint_after_move, "")
  # The keys work after a real click on the slider and on the checkbox.
  expect_equal(res$focus_after_slider, "slider")
  expect_gt(res$i_after_slider_click, 0)
  expect_equal(res$slider_d, res$candidate)
  expect_equal(res$slider_d_src, "candidate")
  expect_equal(res$slider_x, res$slider_cursor)
  expect_equal(res$slider_shift_step, 7L)
  expect_true(res$slider_space_playing)
  expect_equal(res$focus_after_checkbox, "autoadv")
  expect_false(res$autoadv_after_two_clicks)
  expect_equal(res$checkbox_u, "uncertain")
  # A finger drag from 30% to 80% of the panels moves the cursor.
  expect_equal(res$touch_i_before, 1L)
  expect_gt(res$touch_i_after, res$touch_nd * 0.6)
  # The readout names the accelerometer, as the panel does.
  expect_match(res$readout_text, "acceleration days in window")
  expect_match(res$readout_text, "median daily acceleration in window \\(m/s2\\)")
  # Another review's call in the same store is not counted.
  expect_match(res$navpos_with_foreign_call, "\\|\\s+0 called$")
  expect_true(all(c("accelerometer", "final run") %in% res$panel_titles))
  # A typed date goes from the store to the CSV and back onto the detset.
  expect_equal(res$typed_store$death_date, "2023-09-02")
  expect_equal(res$typed_store$death_src, "typed")
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(res$typed_export, csv)
  y <- apply_mortality_calls(r$x, csv)
  expect_equal(y$mortality$death_date, as.Date("2023-09-02"))
  expect_equal(y$mortality$date_source, "typed")
  expect_equal(y$mortality$call, "dead_or_shed")
  # The index: every animal, the spy follows the scroll, export and import.
  expect_equal(res$index_rows, 20L)
  # The index fits the window, and keeps the persistence column only when
  # some animal has a score.
  expect_lte(res$index_scroll_w, res$index_inner_w)
  has_score <- any(!is.na(r$bands$bands$persistence))
  expect_equal("persistence" %in% res$index_heads, has_score)
  expect_equal(res$index_bandheads, c("band-dead", "band-review", "band-alive"))
  present <- c("dead", "review", "alive")
  present <- present[present %in% r$bands$bands$band]
  last_band <- present[length(present)]
  expect_equal(res$spy_band_after_scroll, last_band)
  expect_match(res$index_export, "\"EX-03\",\"dead\",\"dead_or_shed\"")
  expect_equal(res$index_cand_call$stored, res$index_cand_call$cand)
  expect_equal(res$index_typed_call$stored, "2023-09-09")
  expect_match(res$index_export_after,
               paste0("^animal_id,band,call,death_date,death_time,date_source,note,",
                      "annotated_at,candidate_date"))
  expect_equal(res$after_clear, "OTHER-STUDY-1")
  csv2 <- withr::local_tempfile(fileext = ".csv")
  writeLines(res$index_export_after, csv2)
  z <- apply_mortality_calls(r$x, csv2)
  expect_equal(nrow(z$mortality), 3L)
  expect_equal(z$mortality$date_source, c("candidate", "typed", "typed"))
  expect_equal(format(z$mortality$death_date), c("2023-07-15", "2023-09-02", "2023-09-09"))
})

test_that("a card with no candidate date stamps nothing, asks for a date and stays open", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  r <- example_review(dir, all = TRUE)
  out <- r$y$cards
  none <- r$bands$bands$animal_id[is.na(r$bands$bands$death_date)]
  skip_if(!length(none), "every animal has a candidate date")
  res <- run_card_check(out$file[out$animal_id == none[1]])
  expect_null(res$failure)
  expect_length(res$errors, 0)
  expect_null(res$candidate)
  expect_null(res$stamp_on_open)
  expect_null(res$x_on_open)
  expect_match(res$deathdate_text_on_open, "not set: move the cursor")
  # D with advance on call: the call is saved undated, the card asks for the
  # date and does not move on.
  expect_true(res$nocand_autoadv)
  expect_true(res$nocand_has_next)
  expect_true(res$nocand_href_same)
  expect_equal(res$nocand_store$call, "dead_or_shed")
  expect_null(res$nocand_store$death_date)
  expect_match(res$nocand_hint, "no candidate date: move the cursor to the death day and press X")
  expect_equal(res$stamp_after_move, NULL)
  expect_equal(res$x_after_move, res$cursor_before_call)
})

test_that("a tag with another sensor and no accelerometer names it in the readout", {
  skip_on_cran()
  ex <- hard_cases()
  acc_tx <- unique(ex$animals$transmitter[!is.na(ex$animals$sensor_type) &
                                            grepl("accel", ex$animals$sensor_type)])
  ani <- ex$animals
  ani$sensor_type[ani$transmitter %in% acc_tx] <- "temperature"
  ani$sensor_unit[ani$transmitter %in% acc_tx] <- "C"
  xt <- detset(ex$detections, ex$deployments, ani)
  dir <- withr::local_tempdir()
  y <- build_mortality_cards(xt, detect_mortality(xt), dir, which = "dead")
  res <- run_card_check(y$cards$file[y$cards$animal_id == "EX-03"])
  expect_null(res$failure)
  expect_length(res$errors, 0)
  expect_match(res$readout_text, "temperature days in window")
  expect_match(res$readout_text, "median daily temperature in window \\(C\\)")
  expect_false(grepl("acceleration", res$readout_text))
})
