# One of the three slowest test files of the package. CRAN limits check
# time; devtools::test() runs this file in full.
skip_on_cran()

# ---- A six-step pipeline on the positionRtools example ----
# Steps 1, 4 and 5 are detqc functions. Steps 2, 3 and 6 stand in for the
# check, filter and listening audit of the other modules: each writes
# what CONTRACT.md section 7 says the report reads, through log_step().

six_step <- function() {
  x <- example_detset()
  # 2: a check with offending rows, a pass and one not run.
  x0 <- x
  bad <- x$deployments[1:2, c("station_id", "receiver_serial", "deploy_lat")]
  x$checks <- list(
    summary = data.frame(
      check = c("coords_on_land", "recovery_before_deploy", "depth_vs_bathymetry"),
      group = c("Receivers", "Receivers", "Receivers"), status = c("flag", "pass", "not run"),
      flagged = c(2L, 0L, NA), of = c(25L, 25L, NA), unit = "deployments",
      note = c("2 of 25 on land", "", "no bathymetry given"), stringsAsFactors = FALSE
    ),
    rows = list(coords_on_land = bad)
  )
  x <- detqc:::log_step(x, "check_test", list(checks = 3L), x0, 0L, "3 checks: 1 flagged")
  # 3: a filter with two named reasons and one reason that removed nothing.
  x0 <- x
  d <- x$detections
  early <- d$datetime < as.POSIXct("2023-05-10", tz = "UTC")
  late <- d$datetime >= as.POSIXct("2023-10-25", tz = "UTC")
  x$detections <- d[!(early | late), ]
  x <- detqc:::log_step(x, "filter_window", list(start = "2023-05-10", end = "2023-10-25"), x0,
                        c(before_start = sum(early), after_end = sum(late), outside_hours = 0L),
                        "study window", fn = "filter_window")
  # 4 and 5: reviewer calls and the mortality filter.
  calls <- data.frame(animal_id = c("EX-01", "EX-04"), call = c("dead_or_shed", "alive"),
                      death_date = c("2023-07-15", ""), note = "")
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  utils::write.csv(calls, f, row.names = FALSE)
  x <- filter_mortality(apply_mortality_calls(x, f), margin_days = 0)
  # 6: a listening audit that marks receiver-days with no detection silent.
  x0 <- x
  heard <- unique(paste(x$detections$receiver_serial, as.Date(x$detections$datetime)))
  x$listening$status <- ifelse(paste(x$listening$receiver_serial, x$listening$date) %in% heard,
                               "listening", "silent")
  detqc:::log_step(x, "audit_listening_test", "", x0, 0L, "receiver-days marked")
}

read_html <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Rendered as logged, no stray digit, and a log whose arithmetic holds.
expect_clean_check <- function(chk, invariants = TRUE) {
  bad <- chk$traced[!chk$traced$match, ]
  expect_equal(nrow(bad), 0L, info = paste(bad$key, bad$shown, bad$expected, collapse = "; "))
  expect_identical(chk$untraced, character())
  if (invariants) expect_equal(nrow(chk$invariants), 0L, info = paste(chk$invariants$kind))
}

# ---- Rendering and tracing ----

test_that("the report renders on the example after a six-step pipeline and every number traces", {
  x <- six_step()
  expect_equal(nrow(x$log), 6L)
  expect_warning_fresh(mb <- detect_mortality(x), "cast no sensor vote")
  f <- withr::local_tempfile(fileext = ".html")
  draw <- user_figure
  out <- processing_report(x, f, bands = mb, figures = list(dots = draw))
  expect_identical(out, f)
  h <- read_html(f)
  ids <- regmatches(h, gregexpr("<(section|header) id=\"([a-z]+)\"", h, perl = TRUE))[[1]]
  expect_identical(sub(".*id=\"([a-z]+)\"", "\\1", ids),
                   c("provenance", "summary", "inputs", "steps", "checks", "listening",
                     "removals", "mortality", "figures", "session"))
  chk <- report_check(f, x, mb)
  expect_clean_check(chk)
  expect_gt(nrow(chk$traced), 150L)
  # The title, and the name and caption of the user's figure.
  expect_equal(chk$given, 3L)
})

test_that("every count of every log record is in the HTML under its own key", {
  x <- six_step()
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL)
  tr <- report_check(f, x)$traced
  cols <- c("rows_before", "rows_after", "removed", "animals_before", "animals_after")
  for (i in seq_len(nrow(x$log))) {
    for (cl in cols) {
      hit <- tr[tr$key == paste("log", i, cl, sep = ":"), ]
      expect_gte(nrow(hit), 1L)
      expect_identical(unique(hit$shown), formatC(x$log[[cl]][i], format = "d", big.mark = ","))
    }
  }
  rm <- unique(tr[startsWith(tr$key, "rm:"), c("key", "shown")])
  rm <- rm[order(rm$key), ]
  expect_identical(rm$key, sort(c("rm:3:before_start", "rm:3:after_end", "rm:3:outside_hours",
                                  "rm:5:after_death_date")))
  # filter_mortality() names its reason.
  want <- c(x$log$removed_by[[3]], after_death_date = x$log$removed[5])
  expect_identical(as.integer(gsub(",", "", rm$shown)),
                   unname(want[sub("^rm:[0-9]+:", "", rm$key)]))
  expect_identical(x$log$rows_after[6], x$log$rows_before[1] - sum(x$log$removed))
})

test_that("report_check() catches a changed number and a stray one", {
  x <- six_step()
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL)
  h <- read_html(f)
  shown <- formatC(x$log$rows_after[6], format = "d", big.mark = ",")
  wrong <- formatC(x$log$rows_after[6] + 1L, format = "d", big.mark = ",")
  h2 <- sub(paste0("data-src=\"log:6:rows_after\">", shown),
            paste0("data-src=\"log:6:rows_after\">", wrong), h, fixed = TRUE)
  h2 <- sub("</main>", "<p>About 12 more.</p></main>", h2, fixed = TRUE)
  g <- withr::local_tempfile(fileext = ".html")
  writeLines(h2, g)
  chk <- report_check(g, x)
  expect_equal(sum(!chk$traced$match), 1L)
  expect_identical(chk$traced$key[!chk$traced$match], "log:6:rows_after")
  expect_identical(chk$untraced, "12")
})

test_that("the HTML loads nothing from outside the file", {
  x <- six_step()
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = list(dots = function(x) user_figure(x, 3)))
  h <- read_html(f)
  srcs <- regmatches(h, gregexpr("\\s(src|href|action|poster)=\"[^\"]*", h, perl = TRUE))[[1]]
  expect_length(srcs, 1L)
  expect_match(srcs, "^ src=\"data:image/png;base64,[A-Za-z0-9+/=]+$")
  h <- gsub("data:image/png;base64,[A-Za-z0-9+/=]+", "", h)
  expect_false(grepl("https?:|ftp:|//[a-z]", h, ignore.case = TRUE))
  expect_false(grepl("<script|<link|<iframe|@import|url\\(", h, ignore.case = TRUE))
})

test_that("the template renders with an empty log and with no figures", {
  x <- example_detset()
  x$log <- x$log[0, ]
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL)
  h <- read_html(f)
  expect_match(h, "The log is empty: no step was recorded. The detection set holds")
  expect_match(h, "No figures were requested.")
  expect_match(h, "No logged step removed detections.")
  expect_match(h, "No mortality step ran")
  expect_clean_check(report_check(f, x))
  processing_report(x, f, figures = character())
  expect_match(read_html(f), "No figures were requested.")
})

# ---- Figures ----

test_that("figures are curated: at most six, named, and a failure is listed", {
  x <- example_detset()
  f <- withr::local_tempfile(fileext = ".html")
  seven <- c("attrition", "abacus", "receivers", "timeline", "a", "b", "c")
  expect_error(processing_report(x, f, figures = seven), "at most six figures; 7 were given")
  expect_error(processing_report(x, f, figures = "map"), "Unknown figure \"map\"")
  expect_error(processing_report(x, f, figures = list(function(x) user_figure(x, 1))),
               "needs a name")
  expect_error(processing_report(x, f, figures = 3), "must name built-in figures")
  boom <- function(x) stop("no stations in 2023")
  quiet <- function(x) NULL
  capd <- structure(function(x) user_figure(x, 1), caption = "A figure of my own.")
  processing_report(x, f, figures = list(boom = boom, quiet = quiet, mine = capd))
  h <- read_html(f)
  stopped <- "not drawn, the plot function stopped: <span data-check=\"given\">"
  nm <- function(n) sprintf("<span data-check=\"given\">%s</span>: ", n)
  expect_match(h, paste0(nm("boom"), stopped, "no stations in 2023</span>"), fixed = TRUE)
  expect_match(h, paste0(nm("quiet"), stopped, "the figure function returned no ggplot</span>"),
               fixed = TRUE)
  expect_match(h, "A figure of my own.", fixed = TRUE)
  expect_clean_check(report_check(f, x))
})

test_that("a built-in figure is drawn when its plot function exists and listed when not", {
  x <- example_detset()
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = c("attrition", "abacus", "receivers", "timeline"))
  h <- read_html(f)
  for (nm in names(detqc:::figure_registry)) {
    fn <- detqc:::figure_registry[[nm]]
    has <- exists(fn, envir = asNamespace("detqc"), inherits = FALSE)
    if (has) {
      expect_match(h, sprintf("<figure id=\"fig-%s\">", nm), fixed = TRUE)
    } else {
      expect_match(h, sprintf(paste0("<span data-check=\"given\">%s</span>: not drawn, %s() is",
                                     " not in this version of detqc."), nm, fn), fixed = TRUE)
    }
  }
  expect_match(h, "<figure id=\"fig-attrition\">", fixed = TRUE)
  expect_clean_check(report_check(f, x))
})

test_that("built-in captions read their numbers from the detection set", {
  x <- example_detset()
  for (nm in c("attrition", "abacus", "receivers", "timeline")) {
    cap <- detqc:::figure_caption(x, nm)
    expect_match(cap, "data-src=")
    bare <- gsub("<span[^>]*>[^<]*</span>", "", cap)
    expect_false(grepl("[0-9]", gsub("<[^>]+>", "", bare)))
  }
  spec <- detqc:::one_figure_spec("abacus", "")
  spec$fun <- function(x) user_figure(x, 1)
  html <- detqc:::one_figure(x, spec)
  expect_match(html, paste0("<b data-check=\"given\">abacus.</b> Detections over time, one",
                            " point per animal, station and hour"), fixed = TRUE)
  # The caption counts the rows the plot draws, by kind.
  drawn <- plot_data(plot_abacus(x))
  expect_match(html, sprintf("reference tags, take <span data-src=\"aba:tx_rows\">%d</span>",
                             length(attr(drawn, "transmitter_rows"))), fixed = TRUE)
  expect_match(html, sprintf("data-src=\"aba:left_out\">%d</span>", attr(drawn, "left_out")),
               fixed = TRUE)
  spec$fun <- NULL
  expect_match(detqc:::one_figure(x, spec),
               "abacus</span>: not drawn, plot_abacus() is not in this version of detqc.",
               fixed = TRUE)
})

test_that("a figure is saved with ggsave() at 1,800 px wide and as tall as asked", {
  out <- detqc:::draw_png(user_figure, NULL, height = 1300L)
  expect_null(out$error)
  png <- jsonlite::base64_dec(out$data)
  expect_identical(png[2:4], charToRaw("PNG"))
  # The width and height sit in the IHDR chunk, bytes 17 to 24.
  px <- function(b) sum(as.integer(b) * 256^(3:0))
  expect_equal(c(px(png[17:20]), px(png[21:24])), c(1800, 1300))
  base_plot <- function(x) invisible(NULL)
  expect_identical(detqc:::draw_png(base_plot, NULL)$error,
                   "the figure function returned no ggplot")
})

# ---- Added sections ----

test_that("a user adds sections through the hook and through the option", {
  x <- example_detset()
  f <- withr::local_tempfile(fileext = ".html")
  secs <- list(
    "Per animal" = function(x) as.data.frame(table(animal_id = x$detections$animal_id)),
    "A note" = function(x) c("First paragraph.", "Second <b>bold</b>."),
    "Both" = function(x) list(text = "Text then table.", table = data.frame(a = 1:2)),
    "Broken" = function(x) stop("cannot read 5 files"),
    "Wrong" = function(x) 42
  )
  processing_report(x, f, figures = NULL, sections = secs)
  h <- read_html(f)
  expect_equal(lengths(regmatches(h, gregexpr("<section class=\"added\"", h))), 5L)
  open_tag <- "<section class=\"added\" data-check=\"user\" id=\"added-1-Per-animal\">"
  expect_match(h, paste0(open_tag, "<h2>Per animal</h2>"), fixed = TRUE)
  expect_match(h, "Second &lt;b&gt;bold&lt;/b&gt;.", fixed = TRUE)
  expect_match(h, "This section stopped: cannot read 5 files", fixed = TRUE)
  expect_match(h, "This section stopped: a section must return a data frame", fixed = TRUE)
  expect_lt(regexpr("id=\"added-5-Wrong\"", h), regexpr("id=\"session\"", h))
  expect_gt(regexpr("id=\"added-1-Per-animal\"", h), regexpr("id=\"figures\"", h))
  chk <- report_check(f, x)
  expect_equal(chk$user_sections, 5L)
  expect_clean_check(chk)
  withr::local_options(detqc.report_sections = list("From the option" = function(x) "Hello."))
  processing_report(x, f, figures = NULL)
  expect_match(read_html(f), "<h2>From the option</h2><p>Hello.</p>", fixed = TRUE)
  expect_error(processing_report(x, f, sections = list(function(x) 1)), "named list of functions")
  expect_error(processing_report(x, f, sections = list(a = 1)), "named list of functions")
  expect_error(processing_report(x, f, sections = "a"), "named list of functions")
})

# ---- The parts the other modules write ----

test_that("sources, checks given as a summary only, and malformed checks are shown", {
  x <- example_detset()
  x$meta$sources <- data.frame(table = c("detections", "deployments"),
                               file = c("/data/VR2W_480013_2023.csv", "/data/deploy 2023.xlsx"),
                               format = c("vue", "glatos"), rows = c(40506L, 25L))
  x$checks <- data.frame(check = "dup_rows", group = "Time", status = "pass", flagged = 0L,
                         of = 40506L, unit = "detections")
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL)
  h <- read_html(f)
  expect_match(h, "VR2W_480013_2023.csv", fixed = TRUE)
  expect_false(grepl("/data/", h, fixed = TRUE))
  expect_match(h, "<h3>Time</h3>", fixed = TRUE)
  expect_clean_check(report_check(f, x))
  x$checks <- list(results = 1)
  processing_report(x, f, figures = NULL)
  expect_match(read_html(f), "not in the form the contract sets")
  expect_match(read_html(f), "check_detset() was not run.", fixed = TRUE)
})

test_that("offending rows are folded and capped, with both counts traced", {
  x <- example_detset()
  rows <- x$detections[1:150, c("datetime", "receiver_serial", "transmitter")]
  s <- data.frame(check = "dups", group = "Other group", status = "flag", flagged = 150L,
                  of = 40506L, unit = "detections", note = "")
  x$checks <- list(summary = s, rows = list(dups = rows))
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL)
  h <- read_html(f)
  expect_match(h, "<details><summary>Offending rows of", fixed = TRUE)
  expect_match(h, paste0("data-src=\"shown:dups\">100</span> shown of ",
                         "<span data-src=\"crn:dups\">150</span>"), fixed = TRUE)
  expect_match(h, "<h3>Other group</h3>", fixed = TRUE)
  expect_clean_check(report_check(f, x))
})

test_that("calls without bands, and a log that disagrees with the table, are reported", {
  x <- six_step()
  x$detections <- x$detections[-1, ]
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL)
  h <- read_html(f)
  expect_match(h, "The detections changed after the last logged step")
  expect_match(h, "Reviewer calls are applied for")
  expect_match(h, "Dead_or_shed calls with a death date: <span data-src=\"calls:dated\">1</span>.",
               fixed = TRUE)
  chk <- report_check(f, x)
  expect_clean_check(chk, invariants = FALSE)
  expect_identical(chk$invariants$kind, "table_rows")
})

test_that("rows lost with no reason are flagged in the summary and the steps table", {
  x <- example_detset()
  y <- x
  y$detections <- y$detections[-(1:9), ]
  y <- suppressWarnings(detqc:::log_step(y, "filter_leaky", "", x, c(dup = 4L), "leaky"))
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(y, f, figures = NULL)
  h <- read_html(f)
  expect_match(h, paste0("<li class=\"flag\">Record <span data-src=\"brk:1:record\">2</span> (",
                         "<span data-src=\"brk:1:step\">filter_leaky</span>) lost ",
                         "<span data-src=\"brk:1:gap\">5</span> detections that it does not"),
               fixed = TRUE)
  expect_match(h, "<tr class=\"flag\">", fixed = TRUE)
  expect_match(h, "data-src=\"log:2:change\">-5</td>", fixed = TRUE)
  chk <- report_check(f, y)
  expect_clean_check(chk, invariants = FALSE)
  expect_identical(chk$invariants$kind, "lost")
})

test_that("text from the log is escaped and never filled into the template twice", {
  x <- example_detset()
  x <- detqc:::log_step(x, "check_odd", "a<b", x, 0L, "{{css}} <script>alert(1)</script>")
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL, control = list(title = "Lake <2024>"))
  h <- read_html(f)
  expect_match(h, "{{css}} &lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
  expect_match(h, "<title>Lake &lt;2024&gt;</title>", fixed = TRUE)
  expect_false(grepl("<script", h, fixed = TRUE))
  chk <- report_check(f, x)
  expect_clean_check(chk)
  expect_equal(chk$given, 1L)
})

test_that("arguments are checked", {
  x <- example_detset()
  f <- withr::local_tempfile(fileext = ".html")
  expect_error(processing_report(x, file.path(tempdir(), "no", "such", "r.html")), "does not exist")
  expect_error(processing_report(x, c("a", "b")), "one path")
  expect_error(processing_report(x, f, control = list(title = 1)), "`title` must be one string")
  expect_error(processing_report(x, f, control = list(lake = 1)), "`control` has no option `lake`")
  expect_error(processing_report(x, f, bands = list()), "must come from detect_mortality")
  expect_error(processing_report(list(), f), "must be a detset")
  expect_error(detqc:::fill_report("{{a}} {{b}}", list(a = "1")), "do not match")
  expect_error(detqc:::fill_report("{{a}} {{a}}", list(a = "1")), "do not match")
  filled <- detqc:::fill_report("<{{a}}|{{b}}>", list(b = "\\1", a = "{{b}}"))
  expect_identical(filled, "<{{b}}|\\1>")
})

# ---- The resolver behind report_check() ----

test_that("keys that do not resolve give NA, and formats are fixed", {
  x <- example_detset()
  rk <- function(k) resolve_key(k, x, NULL)
  expect_true(is.na(rk("nosuch:1")))
  expect_true(is.na(rk("log:9:rows_after")))
  expect_true(is.na(rk("log:1:nosuch")))
  expect_true(is.na(rk("tab:detections:nosuch")))
  expect_true(is.na(rk("tab:nosuch:rows")))
  expect_true(is.na(rk("lst:nosuch")))
  expect_true(is.na(rk("lstn:NOSTATION:days")))
  expect_true(is.na(rk("lstn:G01:nosuch")))
  expect_true(is.na(rk("calls:n")))
  expect_true(is.na(rk("crn:none")))
  expect_identical(rk("calls:nosuch"), NA_character_)
  expect_identical(rk("tab:animals:first"), "2023-05-01 15:00:00")
  expect_identical(rk("meta:epsg"), "4326")
  expect_true(is.na(rk("brk:1:nosuch")))
  expect_true(is.na(rk("more:nosuch")))
  expect_true(is.na(rk("rsum:nosuch:removed")))
  expect_true(is.na(rk("rsum:nosuch:steps")))
  expect_identical(detqc:::pct_of(1, 0), NA_character_)
  expect_identical(utf8ToInt(rk("sys:generated")), 8211L)
  expect_identical(detqc:::fmt_value(0.5), "0.5")
  expect_identical(detqc:::fmt_value(TRUE), "TRUE")
  expect_identical(utf8ToInt(detqc:::fmt_value(NULL)), 8211L)
  expect_identical(utf8ToInt(detqc:::fmt_value(NA)), 8211L)
  expect_identical(detqc:::pl(1, "step"), "step")
})

test_that("a detset with no listening rows and an unlogged meta still renders", {
  x <- example_detset()
  x$listening <- x$listening[0, ]
  x$meta$created <- NULL
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL)
  h <- read_html(f)
  expect_match(h, "The detection set holds no receiver-days of listening.")
  expect_match(h, "<dd>not recorded</dd>", fixed = TRUE)
  expect_clean_check(report_check(f, x))
})

# ---- In a browser ----

test_that("the report opens in headless Chrome with no errors and no requests", {
  skip_on_cran()
  chrome <- find_chrome()
  node <- Sys.which("node")
  skip_if(!nzchar(chrome) || !nzchar(node), "headless Chrome and node are needed")
  x <- six_step()
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, bands = detect_mortality(x),
                    figures = list(dots = user_figure))
  port <- as.character(9400L + sample.int(200L, 1L))
  res <- system2(node, c(shQuote(test_path("browser", "report_check.mjs")), shQuote(f),
                         shQuote(chrome), port), stdout = TRUE, stderr = TRUE)
  out <- jsonlite::fromJSON(res[length(res)])
  expect_null(out$failure)
  expect_length(out$errors, 0L)
  expect_length(out$console_errors, 0L)
  expect_length(out$requests, 0L)
  expect_identical(out$title, "Detection processing report")
  expect_equal(out$images_decoded, 1L)
  expect_identical(out$sections, c("provenance", "summary", "inputs", "steps", "checks",
                                   "listening", "removals", "mortality", "figures", "session"))
})

# ---- Faulty logs ----
# Each plant goes through log_step(), the one writer of the log.

drop_logged <- function(x, n, step, removed = c(dup = n)) {
  x0 <- x
  x$detections <- x$detections[-seq_len(n), ]
  detqc:::log_step(x, step, "", x0, removed, "plant")
}

strip_tags <- function(h) gsub("\\s+", " ", gsub("<[^>]+>", "", h))

render <- function(x, ...) {
  f <- withr::local_tempfile(fileext = ".html", .local_envir = parent.frame())
  processing_report(x, f, figures = NULL, ...)
  list(file = f, html = read_html(f))
}

test_that("detections changed between two records break the chain and are flagged", {
  x <- drop_logged(example_detset(), 1270L, "filter_early")
  x$detections <- x$detections[-(1:500), ]
  w <- capture_warnings(y <- drop_logged(x, 100L, "filter_late"))
  expect_match(w, "starts from 38,736 detections, but the record before it ended at 39,236",
               all = FALSE, fixed = TRUE)
  r <- render(y)
  txt <- strip_tags(r$html)
  expect_match(txt, paste("Record 3 (filter_late) starts from 38,736 detections, but the record",
                          "before it ended at 39,236"), fixed = TRUE)
  expect_match(txt, paste("The counts do not close: 40,506 detections came in, 1,370 were removed",
                          "and load steps added 0, which leaves 39,136, but the log ends at",
                          "38,636, 500 detections apart."), fixed = TRUE)
  expect_match(r$html, "<tr class=\"flag\"><td class=\"num\" data-src=\"idx:3\">", fixed = TRUE)
  chk <- report_check(r$file, y)
  expect_clean_check(chk, invariants = FALSE)
  expect_true("chain_rows" %in% chk$invariants$kind)
  expect_true(all(chk$invariants$record == 3L))
})

test_that("counts before that do not follow the last record are caught", {
  x <- example_detset()
  w <- capture_warnings(y <- detqc:::log_step(x, "filter_odd", "", c(10, 1), 0L))
  expect_length(w, 3L)
  expect_match(w[1], "ends with 40,496 more detections than its removals allow")
  expect_match(w[2], "starts from 10 detections, but the record before it ended at 40,506")
  expect_match(w[3], "starts from 1 animal, but the record before it ended at 20")
  expect_identical(detqc:::log_breaches(y$log)$kind, c("added", "chain_rows", "chain_animals"))
})

test_that("an over-count warns, an impossible count stops, an append does not warn", {
  x <- example_detset()
  expect_warning(y <- drop_logged(x, 5L, "filter_dup", c(dup = 10L)),
                 "ends with 5 more detections than its removals allow")
  r <- render(y)
  expect_match(strip_tags(r$html), "Record 2 (filter_dup) ends with 5 more detections",
               fixed = TRUE)
  expect_match(r$html, "data-src=\"log:2:change\">5</td>", fixed = TRUE)
  expect_match(r$html, "<tr class=\"flag\">", fixed = TRUE)
  chk <- report_check(r$file, y)
  expect_identical(chk$invariants$kind, "added")
  expect_error(detqc:::log_step(x, "filter_big", "", x, c(dup = 50000L)),
               "counts 50,000 detections removed, but only 40,506 came in")
  lg <- x$log
  lg$removed <- 50000L
  lg$removed_by <- list(c(dup = 50000L))
  expect_identical(detqc:::log_breaches(lg)$kind, "over")
  expect_warning(detqc:::warn_breaches(lg), "counts 50,000 detections removed, but only 40,506")
  z <- x
  z$detections <- rbind(z$detections, z$detections[1:4, ])
  expect_silent(detqc:::log_step(z, "append_more", "", x, 0L))
  expect_true(all(detqc:::adds_rows(c("detset", "read_vue", "append_more"))))
  expect_false(detqc:::adds_rows("filter_x"))
})

test_that("report_check() returns the log's breaches and says what each match proves", {
  x <- six_step()
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, bands = detect_mortality(x), figures = NULL)
  chk <- report_check(f, x, detect_mortality(x))
  expect_clean_check(chk)
  expect_setequal(unique(chk$traced$basis), c("label", "log", "table", "text"))
  expect_identical(key_basis(c("idx:1", "log:2:rows_after", "log:2:reason",
                               "tab:detections:rows", "lstn:G01:station", "sys:R",
                               "chk:a:check", "mrule:dead:x")),
                   c("label", "log", "text", "table", "label", "text", "label", "table"))
  # A record whose own words carry a wrong number: the copy matches, the
  # note is marked text, and the arithmetic of the log still holds.
  y <- drop_logged(example_detset(), 100L, "filter_p7")
  y$log$reason[2] <- "removed 9,999 of 40,506 detections (24.7%)"
  g <- render(y)
  ch2 <- report_check(g$file, y)
  expect_identical(ch2$traced$basis[ch2$traced$key == "log:2:reason"], "text")
  # A faulty log: every value matches its copy, and invariants are not empty.
  z <- suppressWarnings(drop_logged(example_detset(), 5L, "filter_dup", c(dup = 10L)))
  h <- render(z)
  ch3 <- report_check(h$file, z)
  expect_true(all(ch3$traced$match))
  expect_equal(nrow(ch3$invariants), 1L)
})

test_that("the summary names each reason, its step and its share, largest first", {
  x <- six_step()
  txt <- strip_tags(render(x)$html)
  expect_match(txt, paste("4,265 detections were removed in 2 steps, for 3 distinct reasons.",
                          "Largest first: 1,570 after_end (filter_window, 3.9% of 40,506);",
                          "1,425 after_death_date (filter_mortality, 3.8% of 37,666);",
                          "1,270 before_start (filter_window, 3.1% of 40,506)."), fixed = TRUE)
})

test_that("What came in gives the first record's counts, and the table is labelled after", {
  r <- render(six_step())
  expect_match(strip_tags(r$html), paste("At the first logged step, record 1 (detset), 40,506",
                                         "detections came in, naming 20 animals; the step left",
                                         "20 animals."), fixed = TRUE)
  expect_match(r$html, "<h3>The tables after the last logged step</h3>", fixed = TRUE)
  x <- example_detset()
  x$log <- x$log[0, ]
  expect_match(render(x)$html, "The log is empty, so the counts that came in are not recorded.")
})

test_that("per cent retained, mortality per deciding rule, and the attrition ladder", {
  x <- six_step()
  mb <- detect_mortality(x)
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, bands = mb, figures = "attrition")
  h <- read_html(f)
  expect_match(h, "data-src=\"ret:3\">93.0</td>", fixed = TRUE)
  expect_match(h, "Animals per band and the rule that decided the band.", fixed = TRUE)
  key <- sprintf("data-src=\"mrule:dead:terminal_run\">%d</td>",
                 sum(mb$bands$band == "dead" & mb$bands$deciding_rule == "terminal_run"))
  expect_match(h, key, fixed = TRUE)
  expect_match(h, "<figure id=\"fig-attrition\">", fixed = TRUE)
  # The report logs the bands as a seventh step.
  expect_match(strip_tags(h), paste("Attrition ladder: detections left after each of 7 logged",
                                    "steps"), fixed = TRUE)
  expect_clean_check(report_check(f, x, mb))
  e <- example_detset()
  e$log <- e$log[0, ]
  processing_report(e, f, figures = "attrition")
  expect_match(read_html(f),
               "the plot function stopped: <span data-check=\"given\">the log is empty",
               fixed = TRUE)
  y <- suppressWarnings(drop_logged(example_detset(), 5L, "filter_dup", c(dup = 10L)))
  png <- detqc:::draw_png(plot_attrition, y)
  expect_null(png$error)
  expect_match(detqc:::attrition_caption(y$log), "data-src=\"brk:records\">1</span>", fixed = TRUE)
})

test_that("no repeated Function column, the time is in the fold, wide tables scroll", {
  x <- example_detset()
  x <- detqc:::log_step(x, "check_odd", "", x, 0L, fn = "check_detset")
  r <- render(x)
  expect_false(grepl("<th>Function</th>", r$html, fixed = TRUE))
  expect_match(r$html, "<br><span class=\"fn\" data-src=\"log:2:fn\">check_detset</span>",
               fixed = TRUE)
  expect_false(grepl("data-src=\"log:1:fn\"", r$html, fixed = TRUE))
  expect_match(r$html, "<th class=\"num\">Logged at (UTC)</th>", fixed = TRUE)
  expect_match(r$html, "<div class=\"wide\"><table>", fixed = TRUE)
  css <- detqc:::template_text("report.css")
  expect_match(css, "td[class^=\"status-\"] { white-space: nowrap; }", fixed = TRUE)
  expect_match(css, "div.wide { overflow-x: auto;", fixed = TRUE)
  expect_clean_check(report_check(r$file, x))
})

test_that("a long log is folded, and reasons are counted by name", {
  x <- example_detset()
  for (i in seq_len(120)) {
    x <- drop_logged(x, 1L, "filter_one", stats::setNames(1L, paste0("r", i %% 3)))
  }
  r <- render(x)
  expect_match(r$html, paste0("<details><summary><span data-src=\"more:steps\">71</span> more ",
                              "steps of <span data-src=\"total:steps\">121</span></summary>"),
               fixed = TRUE)
  txt <- strip_tags(r$html)
  expect_match(txt, "120 detections were removed in 120 steps, for 3 distinct reasons.",
               fixed = TRUE)
  expect_match(txt, "; and 115 more.", fixed = TRUE)
  expect_match(r$html, "Detections removed by reason, summed over every step that gave it.",
               fixed = TRUE)
  expect_match(r$html, "data-src=\"rsum:r0:removed\">40</td>", fixed = TRUE)
  expect_match(r$html, "Every step and reason, <span data-src=\"rmpairs:n\">120</span> rows",
               fixed = TRUE)
  expect_lt(nchar(r$html), 400000L)
  expect_clean_check(report_check(r$file, x))
  rk <- function(k) resolve_key(k, x, NULL)
  expect_true(is.na(rk("rsum:r0:nosuch")))
  expect_identical(rk("brk:shown"), "0")
  y <- drop_logged(example_detset(), 1L, "filter_one")
  expect_match(strip_tags(render(y)$html),
               "1 detection was removed in 1 step, for 1 distinct reason.", fixed = TRUE)
})

test_that("the EPSG code is labelled for what it is", {
  x <- example_detset()
  h <- render(x)$html
  expect_match(h, "<dt>Coordinate reference system, EPSG code</dt>", fixed = TRUE)
  expect_match(h, "4326</span> (geographic, not projected)", fixed = TRUE)
  x$meta$epsg <- 32617L
  h <- render(x)$html
  expect_false(grepl("geographic", h, fixed = TRUE))
  expect_match(h, "data-src=\"meta:epsg\">32617</span>", fixed = TRUE)
})

test_that("figure names are unique and every id is valid and unique", {
  x <- example_detset()
  f <- withr::local_tempfile(fileext = ".html")
  d <- function(x) user_figure(x, 1)
  expect_error(processing_report(x, f, figures = list("my fig" = d, "my fig" = d)),
               "\"my fig\" is given twice")
  expect_error(processing_report(x, f, figures = c("abacus", "abacus")),
               "\"abacus\" is given twice")
  processing_report(x, f, figures = list("my fig" = d),
                    sections = list("A b" = function(x) "a", "A-b" = function(x) "b"))
  h <- read_html(f)
  ids <- sub("id=\"", "", regmatches(h, gregexpr("id=\"[^\"]*", h))[[1]])
  expect_false(anyDuplicated(ids) > 0)
  expect_false(any(grepl("\\s", ids)))
  expect_true(all(c("fig-my-fig", "added-1-A-b", "added-2-A-b") %in% ids))
})

test_that("a step that leaves no detections is flagged and the abacus is not drawn", {
  x <- example_detset()
  y <- drop_logged(x, nrow(x$detections), "filter_all")
  r <- render(y)
  expect_match(strip_tags(r$html), "Record 2 (filter_all) left no detections", fixed = TRUE)
  expect_clean_check(report_check(r$file, y))
  spec <- detqc:::one_figure_spec("abacus", "")
  spec$fun <- function(x) user_figure(x, 1)
  expect_match(detqc:::one_figure(y, spec), "abacus</span>: not drawn, no detections remain.",
               fixed = TRUE)
})

test_that("the animals in the table are checked against the last record", {
  x <- example_detset()
  ids <- sort(unique(x$detections$animal_id))
  x$detections$animal_id[x$detections$animal_id %in% ids[1:5]] <- ids[6]
  r <- render(x)
  expect_match(strip_tags(r$html), paste("The animals changed after the last logged step: the log",
                                         "ends at 20 animals and the table holds 15."),
               fixed = TRUE)
  chk <- report_check(r$file, x)
  expect_clean_check(chk, invariants = FALSE)
  expect_identical(chk$invariants$kind, "table_animals")
})

test_that("a figure name that holds a digit is given as text", {
  x <- example_detset()
  f <- withr::local_tempfile(fileext = ".html")
  figs <- list("Figure 2" = function(x) user_figure(x, 1),
               "depth 2024" = function(x) stop("no depth"))
  processing_report(x, f, figures = figs)
  chk <- report_check(f, x)
  expect_clean_check(chk)
  # The title, two figure names, the default user caption and the error.
  expect_equal(chk$given, 5L)
})

test_that("more than ten breaches are listed up to the cap, with the total", {
  x <- example_detset()
  for (i in seq_len(12)) {
    x0 <- x
    x$detections <- x$detections[-1, ]
    x <- suppressWarnings(detqc:::log_step(x, "filter_leak", "", x0, 0L))
  }
  r <- render(x)
  txt <- strip_tags(r$html)
  expect_match(txt, "The log breaks its arithmetic at 12 places; the first 10 are listed.",
               fixed = TRUE)
  chk <- report_check(r$file, x)
  expect_clean_check(chk, invariants = FALSE)
  expect_equal(nrow(chk$invariants), 12L)
})

test_that("a share never rounds to 0.0 or 100.0 when it is neither", {
  pc <- detqc:::pct_of
  expect_identical(pc(8, 26568), "0.03")
  expect_identical(pc(26560, 26568), "99.97")
  expect_identical(pc(0, 10), "0.0")
  expect_identical(pc(10, 10), "100.0")
  expect_identical(pc(1270, 40506), "3.1")
})

test_that("every kind of breach has its sentence in the summary", {
  x <- example_detset()
  x$log$removed <- 50000L
  x$log$removed_by <- list(c(dup = 50000L))
  txt <- strip_tags(render(x)$html)
  expect_match(txt, "Record 1 (detset) counts 50,000 detections removed, but only 40,506 came in.",
               fixed = TRUE)
  p4 <- suppressWarnings(detqc:::log_step(example_detset(), "filter_odd", "", c(10, 1), 0L))
  r <- render(p4)
  expect_match(strip_tags(r$html), paste("Record 2 (filter_odd) starts from 1 animal, but the",
                                         "record before it ended at 20"), fixed = TRUE)
  expect_clean_check(report_check(r$file, p4), invariants = FALSE)
})

test_that("bands without a deciding rule, and an unknown calls statistic", {
  x <- six_step()
  mb <- detect_mortality(x)
  mb$bands$deciding_rule <- NULL
  expect_identical(detqc:::mortality_rules_table(mb$bands), "")
  expect_true(is.na(resolve_key("calls:nosuch", x, NULL)))
})

test_that("the map fits heard stations, the timeline detections", {
  ex <- hard_cases()
  # A network deployment file: 40 stations 300 km away, deployed for years.
  far <- ex$deployments[rep(1, 40), ]
  far$receiver_serial <- sprintf("VR2W-%06d", seq_len(40))
  far$station_id <- sprintf("FAR%d", seq_len(40))
  far$deploy_lat <- 46.7
  far$deploy_lon <- -81 + seq_len(40) / 100
  far$deploy_datetime <- as.POSIXct("2019-05-01", tz = "UTC")
  far$recover_datetime <- as.POSIXct("2020-10-01", tz = "UTC")
  x <- suppressWarnings(detset(ex$detections, rbind(ex$deployments, far), ex$animals))
  m <- plot_data(plot_receivers(x, fit = "heard"))
  expect_equal(attr(m, "not_drawn"), 40L)
  expect_true(all(m$detections[m$labelled] > 0))
  all_st <- plot_data(plot_receivers(x))
  expect_equal(nrow(all_st), length(unique(x$deployments$station_id)))
  t <- plot_data(plot_timeline(x, span = "detections"))
  expect_equal(attr(t, "not_drawn"), 40L)
  expect_false(any(grepl("^FAR", t$station_id)))
  expect_equal(natural_sort(c("S10", "S2", "S1", "A")), c("A", "S1", "S2", "S10"))
  # Each figure is as tall as its rows need.
  built_in <- function(name) list(builtin = TRUE, name = name)
  expect_equal(figure_height(x, built_in("timeline")), as.integer(300 + 30 * 24))
  expect_equal(figure_height(x, built_in("attrition")), 1000L)
  # The report draws them so, with an outline when one is given, and every
  # number in it is traced.
  skip_if_not_installed("sf")
  bb <- c(-78.3, -77.9, 44.4, 44.7)
  ring <- matrix(c(bb[1], bb[3], bb[2], bb[3], bb[2], bb[4], bb[1], bb[4], bb[1], bb[3]),
                 ncol = 2, byrow = TRUE)
  lake <- sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, control = list(outline = lake))
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(html, "The map is fitted to the <span data-src=\"tab:detections:stations\">")
  expect_match(html, "deployments outside it are not drawn,\\s+and stations are in natural order")
  chk <- report_check(f, x)
  expect_true(all(chk$traced$match))
  expect_identical(chk$untraced, character())
})

test_that("plain listening words, rows read and kept, and a link to the cards", {
  ex <- hard_cases()
  x <- audit_listening(detset(ex$detections, ex$deployments, ex$animals))
  x$meta$sources <- data.frame(table = c("detections", "deployments"), file = c("d.csv", "r.csv"),
                               format = "glatos", rows = c(nrow(ex$detections), 30601L))
  dir <- withr::local_tempdir()
  x <- suppressMessages(suppressWarnings(review_mortality(x, file.path(dir, "review"),
                                                          open = FALSE)))
  f <- file.path(dir, "report.html")
  processing_report(x, f, figures = NULL)
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl("are listening by audit_listening", html, fixed = TRUE))
  expect_match(html, "The audit counts <span data-src=\"lst:listening\">")
  expect_match(html, "silent</span> \\(nothing heard and no reference tag to show the receiver")
  expect_match(html, paste0("<span data-src=\"srcsum:deployments\">30,601</span> rows kept by ",
                            "the read function and"))
  expect_match(html, "<a href=\"review/index.html\">open the card index</a>")
  chk <- report_check(f, x)
  expect_true(all(chk$traced$match))
  expect_identical(chk$untraced, character())
  expect_equal(relative_href("/a/b/c/index.html", "/a/x"), "../b/c/index.html")
})

test_that("a listening table with no station gives a sentence and no base warning", {
  lst <- data.frame(receiver_serial = c("R1", "R2"), station_id = NA_character_,
                    date = as.Date("2024-06-01") + 0:1, stringsAsFactors = FALSE)
  expect_message(out <- expect_no_warning(listening_stations(lst)),
                 "no receiver-day of the listening table names a station")
  expect_match(out, "No receiver-day of the listening table names a station")
})

test_that("a deployment sheet with no receiver serials gives a report whose numbers all trace", {
  # A sheet that names stations and no receivers, as glatos's
  # hfx_deployments.csv does, leaves NA serials in the listening table. The
  # report and report_check() must count those rows the same way.
  ex <- hard_cases()
  dep <- ex$deployments
  dep$receiver_serial <- NA_character_
  x <- suppressWarnings(detset(ex$detections, dep, ex$animals))
  x <- audit_listening(filter_deployment_period(x))
  expect_true(anyNA(x$listening$receiver_serial))
  f <- withr::local_tempfile(fileext = ".html")
  processing_report(x, f, figures = NULL)
  rc <- report_check(f, x)
  expect_equal(rc$traced$key[!rc$traced$match], character())
  expect_length(rc$untraced, 0L)
})

test_that("the attrition caption reads in the singular for a log of one step", {
  lg <- data.frame(record = 1L, step = "detset", rows_before = 1, rows_after = 1, removed = 0)
  expect_match(strip_tags(attrition_caption(lg)),
               "detections left after its 1 logged step, one bar", fixed = TRUE)
  expect_match(strip_tags(attrition_caption(lg)), "marks the 1 detection that came", fixed = TRUE)
})

test_that("the timeline caption says there are no ticks at zero, and counts them above it", {
  x <- suppressMessages(suppressWarnings(audit_listening(example_detset())))
  x$listening$listening <- TRUE
  cap <- strip_tags(timeline_caption(x))
  expect_match(cap, sprintf(paste("counts all %s as listening, silent days included, so the",
                                  "figure has no ticks."),
                            format(nrow(x$listening), big.mark = ",")), fixed = TRUE)
  # The zero is said once.
  expect_identical(lengths(regmatches(cap, gregexpr("as listening", cap, fixed = TRUE))), 1L)
  expect_false(grepl("Ticks mark", cap, fixed = TRUE))
  expect_false(grepl(" 0 receiver-day", cap, fixed = TRUE))
  x$listening$listening[1] <- FALSE
  expect_match(strip_tags(timeline_caption(x)),
               "Ticks mark the 1 receiver-day it counts as not listening.", fixed = TRUE)
  x$listening$listening[1:3] <- FALSE
  expect_match(strip_tags(timeline_caption(x)),
               "Ticks mark the 3 receiver-days it counts as not listening.", fixed = TRUE)
})

test_that("the timeline caption counts receivers by serial, else stations, else deployments", {
  x <- suppressMessages(suppressWarnings(audit_listening(example_detset())))
  dep <- x$deployments
  n_dep <- format(nrow(dep), big.mark = ",")
  n_rx <- length(unique(dep$receiver_serial))
  n_st <- length(unique(dep$station_id))
  expect_match(strip_tags(timeline_caption(x)),
               sprintf("%s deployments of %d receivers,", n_dep, n_rx), fixed = TRUE)
  x$deployments$receiver_serial <- NA_character_
  cap <- strip_tags(timeline_caption(x))
  expect_match(cap, sprintf("%s deployments with no receiver serial, at %d stations,", n_dep, n_st),
               fixed = TRUE)
  expect_false(grepl("0 receivers", cap, fixed = TRUE))
  x$deployments$receiver_serial[1] <- dep$receiver_serial[1]
  expect_match(strip_tags(timeline_caption(x)),
               sprintf(paste("%s deployments, some with no receiver serial, of 1 receiver at",
                             "%d stations,"), n_dep, n_st), fixed = TRUE)
  x$deployments$receiver_serial <- NA_character_
  x$deployments$station_id <- NA_character_
  expect_match(strip_tags(timeline_caption(x)),
               sprintf("over time: %s deployments, ", n_dep), fixed = TRUE)
})
