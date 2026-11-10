# The whole workflow on the layouts of four lake arrays' exports: readers,
# converters, the report and the review cards. Each test names the
# behaviour it pins.

test_that("read_tag_specs() binds several sheets", {
  f <- ext_file("tag_specs_example.csv")
  dir <- withr::local_tempdir()
  f2 <- file.path(dir, "second.csv")
  s1 <- utils::read.csv(f, check.names = FALSE, colClasses = "character")
  utils::write.csv(s1, f2, row.names = FALSE)
  one <- read_tag_specs(f)
  both <- NULL
  expect_message(both <- read_tag_specs(c(f, f2)), "on more than one of the 2 sheets")
  expect_equal(nrow(both), nrow(one))
  expect_identical(unique(both$source_file), "tag_specs_example.csv")
  expect_length(attr(both, "source"), 2)
  expect_error(read_tag_specs(c(f, file.path(dir, "none.csv"))), "Reading none.csv")
})

test_that("read_detections() keeps the built-in transmitters of every VDAT file", {
  v <- ext_file("vdat_example.csv")
  dir <- withr::local_tempdir()
  v2 <- file.path(dir, "copy.csv")
  file.copy(v, v2)
  one <- attr(read_detections(v, "vdat"), "receiver_transmitters")
  two <- attr(suppressWarnings(read_detections(c(v, v2), "vdat")), "receiver_transmitters")
  expect_gt(NROW(one), 0)
  expect_equal(NROW(two), NROW(unique(one)))
})

test_that("filter_duplicates() names a stamp mix only among detection files", {
  src <- data.frame(table = c("detections", "deployments"), file = c("a.csv", "b.xlsx"),
                    format = c("vdat", "otn"), rows = c(10L, 2L))
  expect_silent(note_mixed_stamps(src))
  src$table[2] <- "detections"
  expect_message(note_mixed_stamps(src), "vdat and otn files")
})

test_that("calls may name tagged animals with no detection; removals have a reason", {
  ex <- hard_cases()
  quiet_row <- ex$animals[1, ]
  quiet_row$animal_id <- "QUIET-1"
  quiet_row$transmitter <- "A69-9999-1"
  x <- detset(ex$detections, ex$deployments, rbind(ex$animals, quiet_row))
  quiet <- setdiff(x$animals$animal_id, x$detections$animal_id)
  heard <- unique(stats::na.omit(x$detections$animal_id))
  expect_identical(quiet, "QUIET-1")
  dead_on <- format(as.Date(min(x$detections$datetime[x$detections$animal_id %in% heard[1]])) + 2)
  calls <- data.frame(animal_id = c(heard[1], quiet[1]), call = "dead_or_shed",
                      death_date = c(dead_on, "2023-07-01"))
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(calls, f, row.names = FALSE)
  y <- NULL
  expect_message(y <- apply_mortality_calls(x, f), "1 called animal has no detection")
  expect_setequal(y$mortality$animal_id, calls$animal_id)
  z <- filter_mortality(y)
  lg <- detset_log(z)
  rb <- lg$removed_by[[nrow(lg)]]
  expect_identical(names(rb), "after_death_date")
  expect_equal(unname(rb), lg$removed[nrow(lg)])
  expect_gt(lg$removed[nrow(lg)], 0)
})

test_that("to_actel() leaves out undetected animals on a shared code", {
  ex <- hard_cases()
  ani <- ex$animals
  extra <- ani[1, ]
  extra$animal_id <- "NEVER-HEARD"
  extra$release_datetime <- extra$release_datetime + 86400
  x <- suppressWarnings(detset(ex$detections, ex$deployments, rbind(ani, extra)))
  a <- NULL
  expect_message(a <- suppressWarnings(to_actel(x, tz = "America/Toronto")),
                 "1 animal shares a transmitter")
  expect_identical(attr(a, "shared_code_unheard"), "NEVER-HEARD")
  expect_false("NEVER-HEARD" %in% a$biometrics$animal_id)
})

test_that("to_actel() lists the unspecified release site when some animals lack one", {
  ex <- hard_cases()
  ani <- ex$animals
  ani$release_lat <- NA_real_
  ani$release_lon <- NA_real_
  j <- seq(1, nrow(ani), 2)
  ani$release_lat[j] <- ex$deployments$deploy_lat[1]
  ani$release_lon[j] <- ex$deployments$deploy_lon[1]
  x <- detset(ex$detections, ex$deployments, ani)
  a <- suppressWarnings(to_actel(x, tz = "America/Toronto"))
  rel <- a$spatial[a$spatial$Type == "Release", ]
  expect_true("unspecified" %in% rel$Station.name)
  expect_true(is.na(rel$Latitude[rel$Station.name == "unspecified"]))
  w <- testthat::capture_warnings(to_actel(x, tz = "America/Toronto"))
  expect_true(any(grepl("have no release site", w)))
  skip_if_not_installed("actel")
  pre <- function() do.call(actel::preload, c(a, list(tz = "America/Toronto")))
  expect_no_error(suppressWarnings(suppressMessages(pre())))
})

test_that("to_ato() says when a power level is text", {
  expect_warning(out <- ato_power(c("H", "L", NA, "7")), "2 of 4 power levels are text")
  expect_equal(out, c(NA, NA, NA, 7))
  expect_silent(ato_power(c(1, 2)))
})

test_that("the GLATOS animals map reads EST_TAG_LIFE", {
  m <- column_map("animals", "glatos")
  expect_identical(m$source[m$target == "tag_life_days"], "est_tag_life")
})

test_that("a card opens on the animal's first day with a detection", {
  # The example has raw sensor readings, about which detect_mortality() warns.
  r <- suppressWarnings(example_review(withr::local_tempdir()))
  out <- r$y$cards
  f <- out$file[out$animal_id == "EX-03"]
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  json <- sub("(?s)^.*<script id=\"detqc-data\" type=\"application/json\">(.*?)</script>.*$",
              "\\1", html, perl = TRUE)
  d <- jsonlite::fromJSON(json)
  first <- as.Date(min(r$x$detections$datetime[r$x$detections$animal_id %in% "EX-03"]))
  expect_equal(d$marks$first, as.integer(first - as.Date(d$t0)))
  expect_match(html, "S.i = D.marks.first != null ? D.marks.first : 0;", fixed = TRUE)
})

test_that("read_animals() names a sheet code its serial's specifications do not list", {
  hdr <- paste("ANIMAL_ID,TAG_CODE_SPACE,TAG_ID_CODE,TAG_SERIAL_NUMBER,TAG_MODEL,EST_TAG_LIFE,",
               "COMMON_NAME_E,UTC_RELEASE_DATE_TIME", sep = "")
  # Serial 1500001 sends A69-9004-7001 in the specifications; the sheet gives
  # code space 9002, as a real tagging sheet can.
  rows <- c("F1,A69-9002,7001,1500001,V13AP,900,walleye,2023-05-01 15:00:00",
            "F2,A69-9004,7002,1500002,V13AP,900,walleye,2023-05-01 15:00:00")
  f <- write_lines_file(c(hdr, rows))
  specs <- read_tag_specs(ext_file("tag_specs_example.csv"))
  w <- character()
  ani <- withCallingHandlers(read_animals(f, "otn", tag_specs = specs), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  hit <- grep("not among the codes", w, value = TRUE)
  expect_length(hit, 1L)
  expect_match(hit, "^1 code of the sheet is not among the codes `tag_specs` lists for its tag")
  expect_match(hit, "A69-9002-7001 for serial 1500001 of F1 \\(specifications: A69-9004-7001\\)")
  # Both codes stay on F1; only the user can say which is right.
  expect_setequal(ani$transmitter[ani$animal_id == "F1"], c("A69-9002-7001", "A69-9004-7001"))
  expect_equal(attr(ani, "source")$codes_not_in_specs, 1L)
  # A sheet whose codes are all listed gives no such warning.
  ok <- write_lines_file(c(hdr, rows[2]))
  w2 <- character()
  withCallingHandlers(read_animals(ok, "otn", tag_specs = specs), warning = function(cnd) {
    w2 <<- c(w2, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_false(any(grepl("not among the codes", w2)))
})

test_that("to_ato() keeps a text power level in power_level_text", {
  ex <- hard_cases()
  a <- ex$animals
  a$power_level <- rep(c("H", "L"), length.out = nrow(a))
  x <- detset(ex$detections, ex$deployments, a)
  expect_warning(out <- to_ato(x), "kept as text in power_level_text")
  tag <- out$tag
  expect_true(all(is.na(tag$power_level)))
  expect_equal(tag$power_level_text, a$power_level[match(tag$transmitter, a$transmitter)])
  # Numeric power levels pass as numbers, and no text column is added.
  a$power_level <- 7
  y <- to_ato(detset(ex$detections, ex$deployments, a))
  expect_equal(unique(y$tag$power_level), 7)
  expect_false("power_level_text" %in% names(y$tag))
  expect_equal(power_text(c("H", "7", NA)), c("H", NA, NA))
})

read_page <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")

test_that("the report shows rows read beside rows kept, from the log", {
  files <- c(ext_file("vdat_example.csv"), ext_file("vue_example.csv"))
  all <- suppressMessages(read_detections(files))
  cut <- list(from = "2023-05-10", to = as.POSIXct("2023-06-01", tz = "UTC"))
  d <- suppressMessages(read_detections(files, cut = cut))
  x <- suppressWarnings(detset(d, hard_cases()$deployments))
  read <- log_rows_read(x$log)
  # Rows read count every data row of the files, the repeated VDAT records
  # the reader set aside included.
  expect_equal(read[["detections"]], sum(vapply(attr(d, "source"), `[[`, 0, "rows_read")))
  expect_gte(read[["detections"]], nrow(all))
  expect_gt(read[["detections"]], nrow(d))
  expect_match(x$log$parameters[1], "rows_read = c\\(detections = [0-9]+L")
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL)
  h <- read_page(f)
  expect_match(h, sprintf("data-src=\"read:detections\">%s</span> rows read from",
                          formatC(read[["detections"]], format = "d", big.mark = ",")))
  expect_match(h, "rows read from <span data-src=\"srcn:detections\">2</span> files")
  chk <- report_check(f, x)
  expect_true(all(chk$traced$match[grepl("^(read|srcn):", chk$traced$key)]))
  expect_true(any(grepl("^read:", chk$traced$key)))
  # A log with no rows_read (tables passed in memory) leaves the line out.
  y <- example_detset()
  expect_length(log_rows_read(y$log), 0L)
  g <- withr::local_tempfile(fileext = ".html")
  processing_report(y, g, figures = NULL)
  expect_false(grepl("rows read from", read_page(g), fixed = TRUE))
})

test_that("the card index keeps its columns on a 1,300 px window", {
  css <- readLines(system.file("templates", "index.html", package = "detqc"), warn = FALSE)
  css <- paste(css, collapse = "\n")
  # The sidebar narrows to 200 px up to 1,500 px, and a table wider than the
  # page scrolls inside its own box while the page stays still.
  expect_match(css, "@media (max-width: 1500px){", fixed = TRUE)
  expect_match(css, "#toc{width:200px}", fixed = TRUE)
  expect_match(css, "#tables{overflow-x:auto}", fixed = TRUE)
  expect_match(css, "input.note{width:56px}", fixed = TRUE)
})

test_that("the card names the size's column and says its unit is the table's", {
  expect_equal(size_text(data.frame(length = 0.75, weight = NA)),
               "length 0.75 (unit as recorded)")
  expect_equal(size_text(data.frame(length = 468, length_unit = "mm")), "length 468 mm")
  expect_equal(size_text(data.frame(length_mm = 400)),
               "length 400 mm")
  expect_null(size_text(data.frame(animal_id = "a")))
})

test_that("size units come only from a template that states them", {
  # OTN: glatos extdata otn_nsbs_tag_metadata.xls heads "LENGTH (m)" and
  # "WEIGHT (kg)". GLATOS: walleye_workbook.xlsm heads "LENGTH" and "WEIGHT"
  # with no unit, so read_animals() adds none and the card says so.
  f <- system.file("extdata", "stoney_detections.csv", package = "detqc")
  ani <- read_animals(f, format = "glatos")
  expect_false(any(c("length_unit", "weight_unit") %in% names(ani)))
  expect_equal(size_text(ani[ani$animal_id == "ST-02", ]), "length 0.468 (unit as recorded)")
  otn <- data.table::data.table(animal_id = c("a", "b"), length = c(0.5, NA), weight = 1.2)
  otn <- add_size_units(otn, "otn")
  expect_equal(otn$length_unit, c("m", NA))
  expect_equal(otn$weight_unit, c("kg", "kg"))
  expect_equal(size_text(as.data.frame(otn[1])), "length 0.5 m, weight 1.2 kg")
})

test_that("read_animals() stops when more than half the rows have no animal_id", {
  hdr <- "ANIMAL_ID,TAG_CODE_SPACE,TAG_ID_CODE,UTC_RELEASE_DATE_TIME"
  rows <- c("F1,A69-1601,1,2024-05-01T12:00:00", ",A69-1601,2,2024-05-01T12:00:00",
            ",A69-1601,3,2024-05-01T12:00:00")
  err <- tryCatch(read_animals(write_lines_file(c(hdr, rows)), "otn"), error = function(cnd) cnd)
  expect_s3_class(err, "detqc_error")
  expect_match(conditionMessage(err), "2 of its 3 distinct rows have no animal_id, more than half")
  expect_match(conditionMessage(err), "read from the column animal_id or release_reference_id")
  expect_equal(nrow(read_animals(write_lines_file(c(hdr, rows)), "otn",
                                 missing_id = "transmitter")), 3L)
  # One of three rows without an id is dropped with a warning.
  expect_warning(a <- read_animals(write_lines_file(c(hdr, rows[1:2], "F3,A69-1601,3,2024-05-01")),
                                   "otn"), "1 of 3 distinct rows were dropped")
  expect_equal(nrow(a), 2L)
})

test_that("read_animals() stops when more than half the rows have no transmitter", {
  hdr <- "ANIMAL_ID,TAG_CODE_SPACE,TAG_ID_CODE,UTC_RELEASE_DATE_TIME"
  row <- function(id, space) sprintf("F%d,%s,%d,2024-05-01T12:00:00", id, space, id)
  # A blank TAG_CODE_SPACE on 8 of 10 rows leaves 8 animals with no transmitter.
  sheet <- c(hdr, row(1:2, "A69-1601"), row(3:10, ""))
  err <- tryCatch(read_animals(write_lines_file(sheet), "otn"), error = function(cnd) cnd)
  expect_s3_class(err, "detqc_error")
  expect_match(conditionMessage(err),
               "8 of its 10 distinct rows have no transmitter, more than half")
  expect_match(conditionMessage(err), "Give `tag_specs`", fixed = TRUE)
  # Exactly half is a warning, and the rows with a transmitter are kept.
  half <- c(hdr, row(1:5, "A69-1601"), row(6:10, ""))
  expect_warning(a <- read_animals(write_lines_file(half), "otn"),
                 "5 of 10 distinct rows were dropped: 5 have no transmitter")
  expect_equal(nrow(a), 5L)
})
