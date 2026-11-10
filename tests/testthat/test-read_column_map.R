# The sheet reader: .xls, .xlsx and .xlsm files go through read_raw_sheet().
# The fixture is built by data-raw/fixture_workbook.R: a notes sheet with no
# table, then a deployment sheet and a tagging sheet, each table under a
# title row and a blank row, with its times as Excel date cells.

workbook <- function() {
  skip_if_not_installed("readxl")
  fixture("otn_workbook_example.xlsx")
}

utc <- function(s) as.POSIXct(s, tz = "UTC")

test_that("a deployment sheet under a title row reads as its CSV twin does", {
  f <- workbook()
  dep <- read_deployments(f, format = "otn")
  expect_equal(nrow(dep), 3L)
  expect_identical(dep$receiver_serial, c("900001", "900002", "900003"))
  expect_identical(dep$station_id, c("SYN1", "SYN2", "SYN3"))
  expect_equal(dep$deploy_datetime,
               utc(c("2024-05-01 12:00:00", "2024-05-02 08:30:00", "2024-05-02 15:45:00")))
  expect_equal(dep$recover_datetime, utc(c("2024-10-01 12:00:00", "2024-10-01 13:00:00", NA)))
  expect_equal(dep$deploy_lat, c(46.10, 46.12, 46.15))
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    OTN_ARRAY = "SYN", STATION_NO = c("1", "2", "3"),
    DEPLOY_DATE_TIME = c("2024-05-01 12:00:00", "2024-05-02 08:30:00", "2024-05-02 15:45:00"),
    DEPLOY_LAT = c(46.10, 46.12, 46.15), DEPLOY_LONG = c(-81.10, -81.14, -81.18),
    INS_MODEL_NO = "VR2W", INS_SERIAL_NO = c("900001", "900002", "900003"),
    RECOVER_DATE_TIME = c("2024-10-01 12:00:00", "2024-10-01 13:00:00", "")
  ), csv, row.names = FALSE)
  twin <- read_deployments(csv, format = "otn")
  cols <- c("receiver_serial", "station_id", "deploy_datetime", "recover_datetime",
            "deploy_lat", "deploy_lon")
  expect_equal(dep[cols], twin[cols])
})

test_that("the tagging sheet of the same workbook reads as the animals table", {
  ani <- read_animals(workbook(), format = "otn")
  expect_equal(nrow(ani), 2L)
  expect_identical(ani$animal_id, c("SYN-A1", "SYN-A2"))
  expect_identical(ani$transmitter, c("A69-9001-101", "A69-9001-102"))
  expect_equal(ani$release_datetime, utc(c("2024-05-03 10:00:00", "2024-05-03 11:30:00")))
  expect_equal(ani$tag_life_days, c(365, 365))
})

test_that("a sheet search finds the table, and a workbook with none stops", {
  f <- workbook()
  raw <- read_raw_sheet(f, c("ins_serial_no", "station_no", "deploy_lat", "deploy_long"))
  expect_equal(nrow(raw), 3L)
  expect_true(all(c("ins_serial_no", "station_no") %in% names(raw)))
  expect_error(read_raw_sheet(f, c("no_such_column", "nor_this_one")),
               "No sheet in otn_workbook_example.xlsx has a header row")
})

# ---- detqc's own column names ----
# A detset table written with write.csv() reads back with no column map.
# write.csv() writes row names unless told not to, so these files carry a
# blank-headed first column of row indices.

written <- function(table) {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  utils::write.csv(detqc_example[[table]], f, row.names = FALSE)
  f
}

same_table <- function(got, want) {
  expect_identical(names(got)[seq_along(want)], names(want))
  expect_equal(as.data.frame(got)[, names(want)], as.data.frame(want), ignore_attr = TRUE)
}

test_that("read_detections() reads the detections detqc writes", {
  got <- read_detections(written("detections"))
  expect_identical(attr(got, "source")[[1]]$format, "detqc")
  same_table(got, detqc_example$detections)
  expect_identical(names(got)[seq_along(detqc_example$detections)], names(detqc_example$detections))
  expect_false("v1" %in% names(got))
})

test_that("read_deployments() reads the deployments detqc writes", {
  got <- expect_silent(read_deployments(written("deployments")))
  expect_identical(attr(got, "source")$format, "detqc")
  same_table(got, detqc_example$deployments)
  want <- names(detqc_example$deployments)
  expect_identical(names(got)[seq_along(want)], want)
  expect_false("v1" %in% names(got))
})

test_that("read_animals() reads the animals detqc writes", {
  got <- read_animals(written("animals"))
  expect_identical(attr(got, "source")$format, "detqc")
  same_table(got, detqc_example$animals)
  expect_identical(names(got)[seq_along(detqc_example$animals)], names(detqc_example$animals))
  expect_false("v1" %in% names(got))
})

# A detset table with one column renamed, written as write.csv() writes it.
renamed <- function(table, from, to) {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  d <- detqc_example[[table]]
  names(d)[names(d) == from] <- to
  utils::write.csv(d, f, row.names = FALSE)
  f
}

test_that("a one-pair col_map is merged into the format's map", {
  f <- renamed("detections", "transmitter", "tag_code")
  for (fm in c("auto", "detqc")) {
    got <- read_detections(f, fm, col_map = c(transmitter = "tag_code"))
    expect_identical(attr(got, "source")[[1]]$format, "detqc")
    same_table(got, detqc_example$detections)
  }
  got <- read_deployments(renamed("deployments", "station_id", "site"),
                          col_map = c(station_id = "site"))
  same_table(got, detqc_example$deployments)
  got <- read_animals(renamed("animals", "species", "spp"), col_map = c(species = "spp"))
  same_table(got, detqc_example$animals)
})

test_that("a pair that names no target, or a missing file, stops with what col_map takes", {
  f <- renamed("detections", "transmitter", "tag_code")
  takes <- "`col_map` takes a data frame with the columns `target` and `source`"
  e <- expect_error(read_detections(f, col_map = c(tag_code = "transmitter")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), takes, fixed = TRUE)
  expect_match(conditionMessage(e), "The pair tag_code = \"transmitter\" names no target",
               fixed = TRUE)
  e <- expect_error(read_animals(written("animals"), col_map = "no_such_map.csv"),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "No file is at \"no_such_map.csv\"", fixed = TRUE)
  expect_error(read_detections(f, col_map = 3), "It is of class numeric", class = "detqc_error")
})

test_that("row names written by write.csv() are dropped, and a numbered column is kept", {
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(detqc_example$detections, f)
  plain <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(detqc_example$detections, plain, row.names = FALSE)
  expect_identical(names(read_detections(f)), names(read_detections(plain)))
  # A named first column of whole numbers is data, and stays.
  d <- cbind(row_no = seq_len(nrow(detqc_example$detections)), detqc_example$detections)
  utils::write.csv(d, plain, row.names = FALSE)
  expect_true("row_no" %in% names(read_detections(plain)))
})

# The GLATOS example with its transmitter_id column renamed to tag_id.
glatos_renamed <- function() {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  z <- readLines(system.file("extdata", "stoney_detections.csv", package = "detqc"))
  z[1] <- sub("transmitter_id", "tag_id", z[1], fixed = TRUE)
  writeLines(z, f)
  f
}

test_that("a GLATOS transmitter pair names both columns and keeps the format's sep", {
  ref <- read_detections(system.file("extdata", "stoney_detections.csv", package = "detqc"))
  f <- glatos_renamed()
  got <- read_detections(f, col_map = c(transmitter = "transmitter_codespace;tag_id"))
  expect_identical(got$transmitter, ref$transmitter)
  expect_identical(nrow(got), 3297L)
  e <- expect_error(read_detections(f, col_map = c(transmitter = "tag_id")),
                    class = "detqc_error")
  expect_match(conditionMessage(e),
               paste("joins `transmitter` from 2 columns, with \"-\" between them, in the",
                     "order transmitter_codespace;transmitter_id"), fixed = TRUE)
  expect_match(conditionMessage(e), "c(transmitter = \"transmitter_codespace;transmitter_id\")",
               fixed = TRUE)
  expect_match(conditionMessage(e), "copy column_map(\"detections\", \"glatos\")", fixed = TRUE)
})

test_that("a pair in another order or of another part count stops with the format's order", {
  f <- glatos_renamed()
  order <- "in the order transmitter_codespace;transmitter_id"
  e <- expect_error(read_detections(f, col_map = c(transmitter = "tag_id;transmitter_codespace")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "names transmitter_codespace as part 2 of 2", fixed = TRUE)
  expect_match(conditionMessage(e), order, fixed = TRUE)
  expect_match(conditionMessage(e), "Write c(transmitter = \"transmitter_codespace;tag_id\")",
               fixed = TRUE)
  expect_identical(conditionCall(e)[[1]], as.name("read_detections"))
  # The format's own names swapped, on the file as shipped
  ext <- system.file("extdata", "stoney_detections.csv", package = "detqc")
  swapped <- c(transmitter = "transmitter_id;transmitter_codespace")
  expect_error(read_detections(ext, col_map = swapped), "names transmitter_id as part 1 of 2",
               class = "detqc_error")
  three <- c(transmitter = "transmitter_codespace;tag_id;tag_id")
  e <- expect_error(read_detections(f, col_map = three), class = "detqc_error")
  expect_match(conditionMessage(e), "names 3 columns", fixed = TRUE)
  expect_match(conditionMessage(e), order, fixed = TRUE)
  three <- c(transmitter = "transmitter_codespace;glatos_array;tag_id")
  expect_error(read_detections(f, col_map = three), "names 3 columns", class = "detqc_error")
  # An OTN sheet joins its station with nothing between the parts.
  otn <- system.file("extdata", "stoney_otn_deployments.csv", package = "detqc")
  expect_error(read_deployments(otn, "otn", col_map = c(station_id = "station_no;otn_array")),
               "with nothing between them, in the order otn_array;station_no", fixed = TRUE,
               class = "detqc_error")
})

test_that("a pair in the format's order reads as the format's own map on the shipped files", {
  ext <- system.file("extdata", "stoney_detections.csv", package = "detqc")
  pair <- c(transmitter = "transmitter_codespace;transmitter_id")
  expect_identical(read_detections(ext, col_map = pair)$transmitter,
                   read_detections(ext)$transmitter)
  expect_identical(read_animals(ext, "glatos", col_map = pair)$transmitter,
                   read_animals(ext, "glatos")$transmitter)
  otn <- system.file("extdata", "stoney_otn_deployments.csv", package = "detqc")
  pair <- c(station_id = "otn_array;station_no")
  got <- suppressWarnings(read_deployments(otn, "otn", col_map = pair))
  expect_identical(got$station_id, suppressWarnings(read_deployments(otn, "otn"))$station_id)
})

test_that("a one-column GLATOS animals pair names the joined row the file holds", {
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(animal_id = "A1", tag_code_space = "A69-1601", tag_id_code = "7",
                              utc_release_date_time = "2024-05-01 10:00:00"),
                   f, row.names = FALSE)
  e <- expect_error(read_animals(f, "glatos", col_map = c(transmitter = "tag_id_code")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "in the order tag_code_space;tag_id_code.", fixed = TRUE)
  expect_match(conditionMessage(e), "as c(transmitter = \"tag_code_space;tag_id_code\")",
               fixed = TRUE)
  expect_false(grepl("transmitter_codespace", conditionMessage(e), fixed = TRUE))
  got <- read_animals(f, "glatos", col_map = c(transmitter = "tag_code_space;tag_id_code"))
  expect_identical(got$transmitter, "A69-1601-7")
  # A file with neither row's columns gets both rows.
  g <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(animal_id = "A1", tid = "7"), g, row.names = FALSE)
  e <- expect_error(read_animals(g, "glatos", col_map = c(transmitter = "tid")),
                    class = "detqc_error")
  expect_match(conditionMessage(e),
               "transmitter_codespace;transmitter_id or tag_code_space;tag_id_code", fixed = TRUE)
})

test_that("a data-frame map whose column is not in the file stops and lists the file's columns", {
  map <- column_map("deployments", "detqc")
  map$source[map$target == "station_id"] <- "site"
  e <- expect_error(read_deployments(written("deployments"), col_map = map),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "row for `station_id` names the column \"site\"", fixed = TRUE)
  expect_match(conditionMessage(e), "The file has: receiver_serial, station_id,", fixed = TRUE)
  # The map read against the file that has its column fills station_id in
  # every row. The optional columns the file lacks are named once.
  expect_warning(got <- read_deployments(renamed("deployments", "station_id", "site"),
                                         col_map = map),
                 "`download_datetime` is read from `download_datetime`, which this file lacks;",
                 fixed = TRUE, class = "detqc_warning")
  expect_false(anyNA(got$station_id))
  # A packaged map copied whole still reads a file that lacks some of its
  # optional columns.
  ext <- system.file("extdata", "stoney_detections.csv", package = "detqc")
  expect_identical(nrow(read_animals(ext, "glatos", col_map = column_map("animals", "glatos"))),
                   nrow(read_animals(ext, "glatos")))
})

test_that("read_deployments() refuses a format it does not read, with the formats it does", {
  e <- expect_error(read_deployments(written("deployments"), format = "innovasea"),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "`format` must be one of \"auto\", \"otn\", \"glatos\",",
               fixed = TRUE)
  expect_identical(conditionCall(e)[[1]], as.name("read_deployments"))
})

test_that("a pair whose column is not in the file stops and lists the file's columns", {
  f <- renamed("detections", "transmitter", "tag_code")
  e <- expect_error(read_detections(f, col_map = c(receiver_serial = "zz_missing")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "names a column this detections file does not have",
               fixed = TRUE)
  expect_match(conditionMessage(e), "The file has: datetime, receiver_serial, tag_code",
               fixed = TRUE)
  expect_error(read_deployments(written("deployments"), col_map = c(deploy_z = "zz_missing")),
               "names a column this deployments file does not have", class = "detqc_error")
  expect_error(read_animals(written("animals"), col_map = c(length = "zz_missing")),
               "names a column this animals file does not have", class = "detqc_error")
  # A file of 20 or more columns gets the nearest ones.
  e <- expect_error(read_detections(glatos_renamed(),
                                    col_map = c(transmitter = "transmitter_codespace;tag_idd")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "names the column \"tag_idd\", which this detections file",
               fixed = TRUE)
  expect_match(conditionMessage(e), "Its nearest columns: tag_id,", fixed = TRUE)
})

test_that("a pair with no source says so", {
  f <- written("detections")
  for (cm in list(c(transmitter = NA), c(transmitter = NA_character_))) {
    expect_error(read_detections(f, col_map = cm),
                 "The pair for `transmitter` gives no source column (NA)", fixed = TRUE,
                 class = "detqc_error")
  }
  expect_error(read_detections(f, col_map = c(transmitter = "")),
               "gives no source column (\"\")", fixed = TRUE, class = "detqc_error")
})

test_that("a one-row data frame map says that it replaces the packaged map", {
  f <- renamed("detections", "transmitter", "tag_code")
  e <- expect_error(read_detections(f, col_map = data.frame(target = "transmitter",
                                                            source = "tag_code")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "replaces the packaged map and has no row for `datetime`",
               fixed = TRUE)
  expect_match(conditionMessage(e), "c(target = \"source\"), which are merged", fixed = TRUE)
})

test_that("read_animals() refuses a format it does not read, with the formats it does", {
  e <- expect_error(read_animals(written("animals"), format = "innovasea"), class = "detqc_error")
  expect_match(conditionMessage(e), "`format` must be one of \"auto\", \"otn\", \"glatos\",",
               fixed = TRUE)
  expect_identical(conditionCall(e)[[1]], as.name("read_animals"))
})

test_that("a sheet read with the other network's format stops when a needed column is empty", {
  rx <- system.file("extdata", "stoney_receivers.csv", package = "detqc")
  e <- expect_error(read_deployments(rx, format = "otn"), class = "detqc_error")
  expect_match(conditionMessage(e), "format = \"otn\" fills no `station_id`, which the",
               fixed = TRUE)
  expect_match(conditionMessage(e), "Read it with format = \"glatos\"", fixed = TRUE)
  otn <- system.file("extdata", "stoney_otn_deployments.csv", package = "detqc")
  expect_error(read_deployments(otn, format = "glatos"), "Read it with format = \"otn\"",
               fixed = TRUE, class = "detqc_error")
  ext <- system.file("extdata", "stoney_detections.csv", package = "detqc")
  expect_error(read_animals(ext, format = "otn"), "fills no `transmitter`", fixed = TRUE,
               class = "detqc_error")
  # A column the table does not need still gives the warning alone.
  d <- utils::read.csv(otn, check.names = FALSE)
  d$station <- paste0("S", seq_len(nrow(d)))
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(d, f, row.names = FALSE)
  expect_warning(got <- read_deployments(f, format = "glatos"), "fills no `deploy_z`",
                 class = "detqc_warning")
  expect_false(anyNA(got$station_id))
})

test_that("read_tag_specs() stops on a file that is not a tag sheet and lists its columns", {
  ext <- function(f) system.file("extdata", f, package = "detqc")
  for (f in c("stoney_vue.csv", "stoney_otn_deployments.csv", "stoney_range_test.csv")) {
    e <- expect_error(read_tag_specs(ext(f)), class = "detqc_error")
    expect_match(conditionMessage(e), paste(f, "is not a tag sheet"), fixed = TRUE)
    expect_match(conditionMessage(e), "The file has: ", fixed = TRUE)
  }
  expect_identical(nrow(read_tag_specs(ext("stoney_tag_specs.csv"))), 4L)
})

test_that("read_animals() refuses a missing_id it does not take, with the values it does", {
  e <- expect_error(read_animals(written("animals"), missing_id = "keep"), class = "detqc_error")
  expect_match(conditionMessage(e),
               "`missing_id` must be one of \"drop\", \"transmitter\"; it has \"keep\".",
               fixed = TRUE)
  expect_identical(conditionCall(e)[[1]], as.name("read_animals"))
  expect_identical(nrow(read_animals(written("animals"), missing_id = "transmitter")),
                   nrow(read_animals(written("animals"))))
})

# ---- Joined columns in a data-frame map, and parts renamed both ----
# The GLATOS example with both transmitter columns renamed.
glatos_both_renamed <- function() {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  z <- readLines(system.file("extdata", "stoney_detections.csv", package = "detqc"))
  z[1] <- sub("transmitter_codespace", "cs", z[1], fixed = TRUE)
  z[1] <- sub("transmitter_id", "tid", z[1], fixed = TRUE)
  writeLines(z, f)
  f
}

glatos_map_with <- function(source) {
  map <- column_map("detections", "glatos")
  map$source[map$target == "transmitter"] <- source
  map
}

test_that("a data-frame map row gets the order and count checks a pair gets", {
  f <- glatos_renamed()
  order <- "in the order transmitter_codespace;transmitter_id"
  e <- expect_error(read_detections(f, col_map = glatos_map_with("tag_id;transmitter_codespace")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "row transmitter = \"tag_id;transmitter_codespace\" names",
               fixed = TRUE)
  expect_match(conditionMessage(e), "names transmitter_codespace as part 2 of 2", fixed = TRUE)
  expect_match(conditionMessage(e), order, fixed = TRUE)
  expect_match(conditionMessage(e), "Write its source as \"transmitter_codespace;tag_id\"",
               fixed = TRUE)
  expect_identical(conditionCall(e)[[1]], as.name("read_detections"))
  three <- glatos_map_with("transmitter_codespace;tag_id;tag_id")
  e <- expect_error(read_detections(f, col_map = three), class = "detqc_error")
  expect_match(conditionMessage(e), "names 3 columns", fixed = TRUE)
  expect_match(conditionMessage(e), order, fixed = TRUE)
  ref <- read_detections(system.file("extdata", "stoney_detections.csv", package = "detqc"))
  got <- read_detections(f, col_map = glatos_map_with("transmitter_codespace;tag_id"))
  expect_identical(got$transmitter, ref$transmitter)
})

test_that("two renamed parts in the other order are refused by the shape of their values", {
  f <- glatos_both_renamed()
  ref <- read_detections(system.file("extdata", "stoney_detections.csv", package = "detqc"))
  for (cm in list(c(transmitter = "tid;cs"), glatos_map_with("tid;cs"))) {
    e <- expect_error(read_detections(f, col_map = cm), class = "detqc_error")
    expect_match(conditionMessage(e), "holds IDs of digits in \"tid\" and code spaces in \"cs\"",
                 fixed = TRUE)
    expect_match(conditionMessage(e), "in 100 of the first 100 rows", fixed = TRUE)
    expect_match(conditionMessage(e), "in the order transmitter_codespace;transmitter_id: code",
                 fixed = TRUE)
  }
  expect_match(conditionMessage(e), "Write its source as \"cs;tid\"", fixed = TRUE)
  for (cm in list(c(transmitter = "cs;tid"), glatos_map_with("cs;tid"))) {
    expect_identical(read_detections(f, col_map = cm)$transmitter, ref$transmitter)
  }
  # A data-frame row of the ID column alone gives codes with no code space.
  expect_error(read_detections(f, col_map = glatos_map_with("tid")),
               "reads one column, \"tid\". It holds values such as \"61002\", with digits alone",
               class = "detqc_error")
})

test_that("reversed and three-part rows stop on walleye and lamprey, and the right order reads", {
  for (nm in c("walleye_detections.csv", "lamprey_detections.csv")) {
    src <- glatos_file(nm)
    ref <- read_detections(src)
    d <- utils::read.csv(src, check.names = FALSE, colClasses = "character")
    names(d)[names(d) == "transmitter_id"] <- "tid"
    one <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(d, one, row.names = FALSE)
    names(d)[names(d) == "transmitter_codespace"] <- "cs"
    both <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(d, both, row.names = FALSE)
    e <- expect_error(read_detections(one, col_map = glatos_map_with("tid;transmitter_codespace")),
                      class = "detqc_error")
    expect_match(conditionMessage(e), "in the order transmitter_codespace;transmitter_id",
                 fixed = TRUE)
    e <- expect_error(read_detections(one,
                                      col_map = glatos_map_with("transmitter_codespace;tid;tid")),
                      class = "detqc_error")
    expect_match(conditionMessage(e), "names 3 columns", fixed = TRUE)
    e <- expect_error(read_detections(both, col_map = c(transmitter = "tid;cs")),
                      class = "detqc_error")
    expect_match(conditionMessage(e), "code space first", fixed = TRUE)
    got <- read_detections(one, col_map = glatos_map_with("transmitter_codespace;tid"))
    expect_identical(got$transmitter, ref$transmitter)
    got <- read_detections(both, col_map = c(transmitter = "cs;tid"))
    expect_identical(got$transmitter, ref$transmitter)
  }
  expect_identical(nrow(ref), 5923L)
  expect_identical(nrow(read_detections(glatos_file("walleye_detections.csv"))), 7180L)
})

test_that("a space after \";\" is dropped before the checks", {
  f <- glatos_renamed()
  e <- expect_error(read_detections(f, col_map = c(transmitter = "tag_id; transmitter_codespace")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "The pair transmitter = \"tag_id;transmitter_codespace\"",
               fixed = TRUE)
  expect_match(conditionMessage(e), "Write c(transmitter = \"transmitter_codespace;tag_id\")",
               fixed = TRUE)
})

test_that("a reversed workbook pair names the joined row the workbook holds", {
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(animal_id = "A1", tag_code_space = "A69-1601", tag_id_code = "7",
                              utc_release_date_time = "2024-05-01 10:00:00"),
                   f, row.names = FALSE)
  e <- expect_error(read_animals(f, "glatos",
                                 col_map = c(transmitter = "tag_id_code;tag_code_space")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "in the order tag_code_space;tag_id_code.", fixed = TRUE)
  expect_false(grepl("transmitter_codespace", conditionMessage(e), fixed = TRUE))
})

# ---- The shape of joined codes, in every format ----
# A GLATOS export with its two transmitter columns renamed, written to a
# temporary CSV.
renamed_codes <- function(name, from = c("transmitter_id", "transmitter_codespace"),
                          to = c("tid", "cs")) {
  d <- utils::read.csv(glatos_file(name), check.names = FALSE, colClasses = "character")
  for (i in seq_along(from)) names(d)[names(d) == from[i]] <- to[i]
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  utils::write.csv(d, f, row.names = FALSE)
  f
}

vue_rows <- function(transmitter, sep) {
  map <- column_map("detections", "vue")
  map <- map[map$target %in% c("datetime", "receiver_serial", "transmitter"), ]
  map$source <- c("detection_timestamp_utc", "receiver_sn", transmitter)
  map$sep[map$target == "transmitter"] <- sep
  map
}

test_that("joined codes are tested in VUE and OTN maps, and refusals advise what reads", {
  for (nm in c("walleye_detections.csv", "lamprey_detections.csv")) {
    ref <- read_detections(glatos_file(nm))$transmitter
    f <- renamed_codes(nm)
    vue_pairs <- function(tr) {
      c(datetime = "detection_timestamp_utc", receiver_serial = "receiver_sn", transmitter = tr)
    }
    e <- expect_error(read_detections(f, "vue", col_map = vue_rows("tid;cs", "-")),
                      class = "detqc_error")
    expect_match(conditionMessage(e), "Write its source as \"cs;tid\".", fixed = TRUE)
    otn <- column_map("detections", "otn")
    otn <- otn[otn$target %in% c("datetime", "receiver_serial", "transmitter"), ][c(1, 3, 5), ]
    otn$source <- c("detection_timestamp_utc", "receiver_sn", "tid;cs")
    otn$sep[3] <- "-"
    expect_error(read_detections(f, "otn", col_map = otn), "code spaces in \"cs\"",
                 class = "detqc_error")
    e <- expect_error(read_detections(f, "vue", col_map = vue_pairs("tid;cs")),
                      class = "detqc_error")
    expect_match(conditionMessage(e), "source \"cs;tid\" and the sep \"-\"", fixed = TRUE)
    # The right order with VUE's empty sep, as a pair and as a row
    e <- expect_error(read_detections(f, "vue", col_map = vue_pairs("cs;tid")),
                      class = "detqc_error")
    expect_match(conditionMessage(e), "is joined with nothing between its columns", fixed = TRUE)
    expect_match(conditionMessage(e), "which is nothing for `transmitter` in format \"vue\"",
                 fixed = TRUE)
    expect_error(read_detections(f, "vue", col_map = vue_rows("cs;tid", "")),
                 "Write its source as \"cs;tid\" and its sep as \"-\".", fixed = TRUE,
                 class = "detqc_error")
    expect_identical(read_detections(f, "vue", col_map = vue_rows("cs;tid", "-"))$transmitter, ref)
    otn$source[3] <- "cs;tid"
    expect_identical(read_detections(f, "otn", col_map = otn)$transmitter, ref)
    # Codes of the wrong shape, and a receiver column in a code
    one <- renamed_codes(nm, "transmitter_id", "tid")
    expect_error(read_detections(one, col_map = c(transmitter = "glatos_array;tid")),
                 "give a transmitter code", class = "detqc_error")
    e <- expect_error(read_detections(one, col_map = c(transmitter = "tid;glatos_array")),
                      class = "detqc_error")
    expect_false(grepl("Write c(", conditionMessage(e), fixed = TRUE))
    expect_error(read_detections(f, col_map = c(transmitter = "cs;receiver_sn")),
                 "names \"receiver_sn\", a column the column maps read for a receiver",
                 fixed = TRUE, class = "detqc_error")
  }
})

test_that("R64K, A69, OPi and OPs code spaces read in the right order and stop reversed", {
  for (cs in c("R64K", "A69-1601", "OPi", "OPs")) {
    f <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(data.frame(detection_timestamp_utc = "2024-05-01 10:00:00", receiver_sn = "1",
                                cs = cs, tid = c("1234", "1235")), f, row.names = FALSE)
    want <- paste(cs, c("1234", "1235"), sep = "-")
    expect_identical(read_detections(f, "glatos", col_map = c(transmitter = "cs;tid"))$transmitter,
                     want)
    # The full GLATOS map names optional columns this small file lacks.
    expect_warning(got <- read_detections(f, "glatos", col_map = glatos_map_with("cs;tid")),
                   "lacks", class = "detqc_warning")
    expect_identical(got$transmitter, want)
    expect_identical(read_detections(f, "vue", col_map = vue_rows("cs;tid", "-"))$transmitter, want)
    expect_error(read_detections(f, "glatos", col_map = c(transmitter = "tid;cs")),
                 "Write c(transmitter = \"cs;tid\").", fixed = TRUE, class = "detqc_error")
    expect_error(read_detections(f, "vue", col_map = vue_rows("tid;cs", "-")),
                 "would have their parts swapped", class = "detqc_error")
    g <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(data.frame(detection_timestamp_utc = "2024-05-01 10:00:00", receiver_sn = "1",
                                code = want), g, row.names = FALSE)
    expect_identical(read_detections(g, "glatos", col_map = c(transmitter = "code"))$transmitter,
                     want)
  }
})

test_that("three or more joined columns read only as a code space, then IDs, joined by \"-\"", {
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(detection_timestamp_utc = "2024-05-01 10:00:00", receiver_sn = "1",
                              cs = "A69-1601", tid = c("1234", "1235"), tid2 = "5678",
                              a = "A69", b = "1601", id = c("12345", "12346")),
                   f, row.names = FALSE)
  # The format's empty sep joins a code space and two IDs into one long ID.
  for (fm in c("vue", "otn", "detqc")) {
    pairs <- c(datetime = "detection_timestamp_utc", receiver_serial = "receiver_sn",
               transmitter = "cs;tid;tid2")
    expect_error(read_detections(f, fm, col_map = pairs), "give a transmitter code",
                 class = "detqc_error")
  }
  expect_error(read_detections(f, "vue", col_map = vue_rows("cs;tid;tid2", "")),
               "give a transmitter code", class = "detqc_error")
  expect_error(read_detections(f, "vue", col_map = vue_rows("cs;tid;tid2", "-")),
               "give a transmitter code", class = "detqc_error")
  expect_error(read_detections(f, "vue", col_map = vue_rows("a;b;id", "")),
               "Write its source as \"a;b;id\" and its sep as \"-\".", fixed = TRUE,
               class = "detqc_error")
  expect_identical(read_detections(f, "vue", col_map = vue_rows("a;b;id", "-"))$transmitter,
                   c("A69-1601-12345", "A69-1601-12346"))
})

test_that("a one-column transmitter source is tested in OTN and VUE files too", {
  otn <- system.file("extdata", "stoney_otn.csv", package = "detqc")
  vue <- system.file("extdata", "stoney_vue.csv", package = "detqc")
  for (col in c("receiver", "station", "dateCollectedUTC", "decimalLatitude")) {
    expect_error(read_detections(otn, col_map = c(transmitter = col)),
                 "a column the column maps read for a receiver, a station, a time or a position",
                 fixed = TRUE, class = "detqc_error")
  }
  for (col in c("Receiver", "Station Name")) {
    expect_error(read_detections(vue, col_map = c(transmitter = col)),
                 "a column the column maps read for a receiver", fixed = TRUE,
                 class = "detqc_error")
  }
  e <- expect_error(read_detections(otn, col_map = c(transmitter = "catalogNumber")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "with the shape of a transmitter code in 0 of the first 100",
               fixed = TRUE)
  expect_match(conditionMessage(e), "Name a column of whole transmitter codes", fixed = TRUE)
  ref <- read_detections(otn)$transmitter
  expect_identical(read_detections(otn, col_map = c(transmitter = "tagName"))$transmitter, ref)
  expect_identical(read_detections(vue, col_map = c(transmitter = "Transmitter"))$transmitter,
                   read_detections(vue)$transmitter)
  # A code-space column under a name no packaged map reads has the shape of
  # a whole code, and reads.
  expect_true(all(read_detections(otn, col_map = c(transmitter = "codeSpace"))$transmitter %in%
                    unique(utils::read.csv(otn)$codeSpace)))
})

test_that("a needed column a pair leaves NA in some rows warns once with its counts", {
  hfx <- glatos_file("hfx_detections.csv")
  e <- expect_warning(d <- read_detections(hfx, col_map = c(transmitter = "sensorname")),
                      class = "detqc_warning")
  expect_match(conditionMessage(e), "`transmitter` is NA in 60,750 of 65,499 rows", fixed = TRUE)
  expect_identical(sum(is.na(d$transmitter)), 60750L)
  wd <- glatos_file("walleye_detections.csv")
  right <- c(transmitter = "transmitter_codespace;transmitter_id")
  expect_silent(w <- read_detections(wd, col_map = right))
  expect_identical(sum(is.na(w$transmitter)), 0L)
})

otn_csv_sheet <- function() {
  data.frame(ARRAY = "ABC", STATION_NO = c("ABC01", "ABC02"),
             DEPLOY_DATE_TIME = "2024-05-01T12:00:00", DEPLOY_LAT = c(45.1, 45.2),
             DEPLOY_LONG = c(-78.1, -78.2), INS_MODEL_NO = "VR2W",
             INS_SERIAL_NO = c("1001", "1002"),
             RECOVER_DATE_TIME = "2024-09-01T12:00:00", check.names = FALSE)
}

test_that("the NA warning counts its columns and asks a full recovery column of no one", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Date and Time (UTC),Rx,Code", "2024-05-01 10:00:00,VR2W-1,A69-1601-1",
               "2024-05-01 10:05:00,,A69-1601-1", "2024-05-01 10:10:00,VR2W-1,"), f)
  e <- expect_warning(read_detections(f, "vue", col_map = c(receiver_serial = "Rx",
                                                            transmitter = "Code")),
                      class = "detqc_warning")
  expect_match(conditionMessage(e), paste("leaves 2 columns the detections table needs empty in",
                                          "some rows: `receiver_serial` is NA in 1 of 3 rows;",
                                          "`transmitter` is NA in 1 of 3 rows. Check that the",
                                          "columns named for `receiver_serial` and `transmitter`",
                                          "hold a value in every row."), fixed = TRUE)
  sheet <- otn_csv_sheet()
  sheet$RECOVER_DATE_TIME[2] <- ""
  names(sheet)[names(sheet) == "RECOVER_DATE_TIME"] <- "Recovered on"
  g <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(sheet, g, row.names = FALSE)
  e <- expect_warning(read_deployments(g, "otn", col_map = c(recover_datetime = "Recovered on")),
                      class = "detqc_warning")
  expect_match(conditionMessage(e), paste("leaves a column the deployments table needs empty in",
                                          "some rows: `recover_datetime` is NA in 1 of 2 rows. A",
                                          "row with no `recover_datetime` reads as a receiver",
                                          "still deployed."), fixed = TRUE)
  expect_false(grepl("every row", conditionMessage(e), fixed = TRUE))
})

test_that("a code-space column of whole codes is told to rename it in the file", {
  d <- utils::read.csv(system.file("extdata", "stoney_detections.csv", package = "detqc"),
                       colClasses = "character", check.names = FALSE)
  d$transmitter_codespace <- paste(d$transmitter_codespace, d$transmitter_id, sep = "-")
  d$transmitter_id <- NULL
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(d, f, row.names = FALSE)
  e <- expect_error(read_detections(f, col_map = c(transmitter = "transmitter_codespace")),
                    class = "detqc_error")
  held <- paste("This column holds whole codes, and the column maps read",
                "\"transmitter_codespace\" for a code space, so rename it in the file and name it",
                "alone.")
  expect_match(conditionMessage(e), held, fixed = TRUE)
  expect_false(grepl("Name 2 columns", conditionMessage(e), fixed = TRUE))
  # With both columns in the file, the advice names them in the format's order.
  e <- expect_error(read_detections(system.file("extdata", "stoney_detections.csv",
                                                package = "detqc"),
                                    col_map = c(transmitter = "transmitter_id")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "Name 2 columns in that order", fixed = TRUE)
})

test_that("an OTN deployment CSV reads STATION_NO alone beside ARRAY, with no warning", {
  sheet <- otn_csv_sheet()
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(sheet, f, row.names = FALSE)
  d <- expect_silent(read_deployments(f))
  expect_identical(d$station_id, c("ABC01", "ABC02"))
  expect_identical(attr(d, "source")$format, "otn")
  # A workbook's OTN_ARRAY is still joined to STATION_NO.
  g <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(stats::setNames(sheet, sub("^ARRAY$", "OTN_ARRAY", names(sheet))), g,
                   row.names = FALSE)
  expect_identical(read_deployments(g)$station_id, c("ABCABC01", "ABCABC02"))
  h <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(sheet[, -(1:2)], h, row.names = FALSE)
  expect_warning(none <- read_deployments(h), "no row has a `station_id`; the map looks for",
                 class = "detqc_warning")
  expect_true(all(is.na(none$station_id)))
})

test_that("a GLATOS receiver file keeps its stations although it has STATION_NO", {
  rx <- glatos_file("sample_receivers.csv")
  d <- suppressWarnings(read_deployments(rx))
  expect_identical(attr(d, "source")$format, "glatos")
  expect_identical(d$station_id, utils::read.csv(rx, colClasses = "character")$station)
})

test_that("a deployment file with no recovery column warns, and a blank one does not", {
  sheet <- otn_csv_sheet()
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(stats::setNames(sheet, sub("^RECOVER_DATE_TIME$", "RECOVER_DATE", names(sheet))),
                   f, row.names = FALSE)
  e <- expect_warning(d <- read_deployments(f), class = "detqc_warning")
  expect_match(conditionMessage(e), paste("no row has a `recover_datetime`, and the file has no",
                                          "column the map looks for: recover_date_time"),
               fixed = TRUE)
  expect_match(conditionMessage(e), "recover_date", fixed = TRUE)
  expect_true(all(is.na(d$recover_datetime)))
  blank <- sheet
  blank$RECOVER_DATE_TIME <- ""
  g <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(blank, g, row.names = FALSE)
  expect_silent(open <- read_deployments(g))
  expect_true(all(is.na(open$recover_datetime)))
  for (f in c("stoney_receivers.csv", "stoney_otn_deployments.csv")) {
    w <- character()
    withCallingHandlers(read_deployments(system.file("extdata", f, package = "detqc")),
                        warning = function(x) {
                          w <<- c(w, conditionMessage(x))
                          invokeRestart("muffleWarning")
                        })
    expect_false(any(grepl("no row has a", w, fixed = TRUE)))
  }
})

test_that("whole codes read as a pair and as a row; IDs and GLATOS's code-space column stop", {
  for (nm in c("walleye_detections.csv", "lamprey_detections.csv")) {
    ref <- read_detections(glatos_file(nm))
    d <- utils::read.csv(glatos_file(nm), check.names = FALSE, colClasses = "character")
    d$code <- paste(d$transmitter_codespace, d$transmitter_id, sep = "-")
    d$transmitter_codespace <- NULL
    d$transmitter_id <- NULL
    f <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(d, f, row.names = FALSE)
    expect_identical(read_detections(f, col_map = c(transmitter = "code"))$transmitter,
                     ref$transmitter)
    expect_identical(read_detections(f, col_map = glatos_map_with("code"))$transmitter,
                     ref$transmitter)
  }
  f <- glatos_renamed()
  e <- expect_error(read_detections(f, col_map = c(transmitter = "transmitter_codespace")),
                    class = "detqc_error")
  held <- paste("a column the column maps read for the code space of a transmitter code.",
                "It holds values such as \"A69-9002\"")
  expect_match(conditionMessage(e), held, fixed = TRUE)
  expect_error(read_detections(f, col_map = glatos_map_with("transmitter_codespace")),
               "A transmitter code needs its code space and ID", class = "detqc_error")
  e <- expect_error(read_detections(f, col_map = c(transmitter = "glatos_array")),
                    class = "detqc_error")
  expect_match(conditionMessage(e), "a column the column maps read for a receiver, a station",
               fixed = TRUE)
})

test_that("a workbook's one-column row is told the order the workbook holds", {
  wb <- glatos_file("walleye_workbook.xlsm")
  map <- column_map("animals", "glatos")
  map <- map[!(map$target == "transmitter" & grepl("^transmitter_", map$source)), ]
  map$source[map$target == "transmitter"] <- "tag_id_code"
  e <- expect_error(read_animals(wb, "glatos", col_map = map), class = "detqc_error")
  expect_match(conditionMessage(e), "in the order tag_code_space;tag_id_code.", fixed = TRUE)
  expect_false(grepl("transmitter_codespace", conditionMessage(e), fixed = TRUE))
})

test_that("a pair that reads animal_id from catalogNumber stops on an extract not matched", {
  hfx <- glatos_file("hfx_detections.csv")
  for (s in c("catalognumber", "catalogNumber")) {
    e <- expect_error(read_detections(hfx, col_map = c(animal_id = s)), class = "detqc_error")
    expect_match(conditionMessage(e), "Leave out the pair for animal_id", fixed = TRUE)
  }
  otn <- system.file("extdata", "stoney_otn.csv", package = "detqc")
  expect_identical(read_detections(otn, col_map = c(animal_id = "catalognumber"))$animal_id,
                   read_detections(otn)$animal_id)
})

# ---- A data-frame map that leaves a needed column empty ----
test_that("a data-frame map whose needed source is absent stops and names the column it missed", {
  for (s in c("station", "station_no")) {
    map <- column_map("deployments", "detqc")
    map$source[map$target == "station_id"] <- s
    e <- expect_error(read_deployments(written("deployments"), col_map = map),
                      class = "detqc_error")
    expect_match(conditionMessage(e),
                 sprintf("row for `station_id` names \"%s\", which this deployments file", s),
                 fixed = TRUE)
    expect_match(conditionMessage(e),
                 "packaged \"detqc\" map fills it from this file's \"station_id\"", fixed = TRUE)
    expect_match(conditionMessage(e), "The file has: receiver_serial, station_id,", fixed = TRUE)
  }
  # The shipped OTN sheet: "station" is absent, "station_no" is the file's.
  otn <- system.file("extdata", "stoney_otn_deployments.csv", package = "detqc")
  map <- column_map("deployments", "otn")
  map$source[map$target == "station_id"] <- "station"
  expect_error(read_deployments(otn, col_map = map),
               "fills it from this file's \"otn_array;station_no\"", fixed = TRUE,
               class = "detqc_error")
  ext <- system.file("extdata", "stoney_detections.csv", package = "detqc")
  map <- column_map("detections", "glatos")
  map$source[map$target == "receiver_serial"] <- "receiver"
  e <- expect_error(read_detections(ext, col_map = map), class = "detqc_error")
  expect_match(conditionMessage(e), "fills it from this file's \"receiver_sn\"", fixed = TRUE)
  expect_match(conditionMessage(e), "Its nearest columns: receiver_sn,", fixed = TRUE)
  map <- column_map("detections", "detqc")
  map$source[map$target == "receiver_serial"] <- "receiver_sn"
  expect_error(read_detections(written("detections"), col_map = map),
               "row for `receiver_serial` names \"receiver_sn\"", class = "detqc_error")
  # The GLATOS deployments map copied whole onto the OTN sheet
  expect_error(read_deployments(otn, col_map = column_map("deployments", "glatos")),
               "rows for `station_id` name \"station\", \"glatos_array;station_no\"",
               fixed = TRUE, class = "detqc_error")
})

test_that("an optional column whose source is absent gives one warning that says its fate", {
  map <- column_map("deployments", "detqc")
  map$source[map$target == "deploy_z"] <- "ins_depth"
  # The written file holds a deploy_z column of its own, which is kept.
  w <- testthat::capture_warnings(got <- read_deployments(written("deployments"), col_map = map))
  expect_length(w, 1L)
  expect_match(w, paste("`deploy_z` is read from `ins_depth`, which this file lacks; the file's",
                        "own `deploy_z` column is read in its place."), fixed = TRUE)
  expect_match(w, paste("`download_datetime` is read from `download_datetime`, which this file",
                        "lacks; the table holds no value for it."), fixed = TRUE)
  expect_identical(got$deploy_z, detqc_example$deployments$deploy_z)
  # A file with no column of that name: the table holds no value for it.
  f <- withr::local_tempfile(fileext = ".csv")
  d <- detqc_example$deployments
  d$deploy_z <- NULL
  utils::write.csv(d, f, row.names = FALSE)
  w <- testthat::capture_warnings(got <- read_deployments(f, col_map = map))
  expect_length(w, 1L)
  expect_match(w, paste("`deploy_z` is read from `ins_depth`, which this file lacks; the table",
                        "holds no value for it."), fixed = TRUE)
  expect_true(all(is.na(got[["deploy_z"]])))
  # A needed column that may be blank: the table holds no value for it.
  map <- column_map("deployments", "detqc")
  map$source[map$target == "receiver_serial"] <- "ins_serial_no"
  map <- map[map$target != "deploy_z", ]
  f2 <- withr::local_tempfile(fileext = ".csv")
  d <- detqc_example$deployments
  d$receiver_serial <- NULL
  utils::write.csv(d, f2, row.names = FALSE)
  w <- testthat::capture_warnings(got <- read_deployments(f2, col_map = map))
  expect_match(w, paste("`receiver_serial` is read from `ins_serial_no`, which this file lacks;",
                        "the table holds no value for it."), fixed = TRUE, all = FALSE)
  expect_true(all(is.na(got$receiver_serial)))
  # hfx: read_detections() adds sensor_unit as a column of NA.
  hfx <- glatos_file("hfx_detections.csv")
  map <- column_map("detections", "otn_qualified")
  map$source[map$target == "sensor_unit"] <- "sensor_unit"
  w <- testthat::capture_warnings(got <- read_detections(hfx, col_map = map))
  expect_match(w, paste("`sensor_unit` is read from `sensor_unit`, which this file lacks; the",
                        "table holds"), fixed = TRUE, all = FALSE)
  expect_true(all(is.na(got$sensor_unit)))
})

test_that("a file of no format read_detections() knows lists its columns", {
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(as.data.frame(stats::setNames(as.list(1:10), sprintf("col_%02d", 1:10))), f,
                   row.names = FALSE)
  expect_error(read_detections(f), "in its header. The file has: col_01, col_02,", fixed = TRUE,
               class = "detqc_error")
  d <- as.data.frame(stats::setNames(as.list(1:30), sprintf("col_%02d", 1:30)))
  names(d)[17] <- "transmitter_code"
  utils::write.csv(d, f, row.names = FALSE)
  expect_error(read_detections(f), "in its header. Its nearest columns: transmitter_code, ",
               fixed = TRUE,
               class = "detqc_error")
})

# ---- A packaged map copied whole reads as its format does ----
test_that("a packaged map copied whole gives the table format = gives", {
  ext <- function(f) system.file("extdata", f, package = "detqc")
  cases <- list(list(read_detections, "detections", "glatos", ext("stoney_detections.csv")),
                list(read_detections, "detections", "otn", ext("stoney_otn.csv")),
                list(read_detections, "detections", "vue", ext("stoney_vue.csv")),
                list(read_detections, "detections", "vdat", ext("stoney_vdat.csv")),
                list(read_deployments, "deployments", "glatos", ext("stoney_receivers.csv")),
                list(read_deployments, "deployments", "otn", ext("stoney_otn_deployments.csv")),
                list(read_animals, "animals", "glatos", ext("stoney_detections.csv")))
  for (k in cases) {
    for (fm in c(k[[3]], "auto")) {
      copied <- column_map(k[[2]], k[[3]])
      own <- suppressWarnings(suppressMessages(k[[1]](k[[4]], format = fm)))
      copy <- suppressWarnings(suppressMessages(k[[1]](k[[4]], format = fm, col_map = copied)))
      expect_identical(copy, own)
    }
  }
  specs <- ext("stoney_tag_specs.csv")
  expect_identical(read_tag_specs(specs, col_map = column_map("animals", "innovasea")),
                   read_tag_specs(specs))
})

test_that("copied maps read hfx, walleye and lamprey as format = does", {
  hfx <- glatos_file("hfx_detections.csv")
  own <- suppressWarnings(read_detections(hfx, "otn"))
  copy <- suppressWarnings(read_detections(hfx, "otn", col_map = column_map("detections", "otn")))
  expect_identical(copy, own)
  expect_true(all(is.na(copy$animal_id)))
  hfx_dep <- glatos_file("hfx_deployments.csv")
  expect_identical(suppressWarnings(read_deployments(hfx_dep, "otn",
                                                     col_map = column_map("deployments", "otn"))),
                   suppressWarnings(read_deployments(hfx_dep, "otn")))
  for (nm in c("walleye_detections.csv", "lamprey_detections.csv")) {
    f <- glatos_file(nm)
    expect_identical(read_detections(f, col_map = column_map("detections", "glatos")),
                     read_detections(f, "glatos"))
  }
  # An edited copy of the matched-extract rows stops on an extract of
  # another kind, where catalogNumber names no animal.
  map <- column_map("detections", "otn")
  map$source[map$target == "species"] <- "scientificname"
  expect_error(read_detections(hfx, col_map = map),
               "Start from column_map(\"detections\", \"otn_qualified\")", fixed = TRUE,
               class = "detqc_error")
})

# ---- Lists of the file's columns ----
test_that("the nearest columns rank a shared word first", {
  have <- c("animal_id", "detection_timestamp_utc", "glatos_array", "station_no", "sex",
            "length", "weight", "station", "min_lag", "receiver_sn", "tag_type", "tag_model",
            "deploy_lat", "deploy_long", "common_name_e", "transmitter_id",
            "transmitter_codespace", "sensor_unit", "sensor_value", "release_group")
  expect_identical(nearest_columns("rx_sn", have, 5)[1], "receiver_sn")
  expect_match(file_columns("rx_sn", have), "^Its nearest columns: receiver_sn, ")
  expect_match(file_columns("rx_sn", have[1:19]), "^The file has: animal_id, ")
})

test_that("read_tag_specs() lists the columns of a file with no transmitter column", {
  f <- withr::local_tempfile(fileext = ".csv")
  d <- as.data.frame(stats::setNames(as.list(rep("x", 22)), sprintf("col_%02d", 1:22)))
  names(d)[3] <- "transmitter_id"
  utils::write.csv(d, f, row.names = FALSE)
  e <- expect_error(read_tag_specs(f), class = "detqc_error")
  expect_match(conditionMessage(e), "No column for `transmitter` in this tag sheet.", fixed = TRUE)
  expect_match(conditionMessage(e), "Its nearest columns: transmitter_id, ", fixed = TRUE)
  utils::write.csv(d[, 1:12], f, row.names = FALSE)
  e <- expect_error(read_tag_specs(f), class = "detqc_error")
  expect_match(conditionMessage(e), "The file has: col_01, col_02, transmitter_id, col_04,",
               fixed = TRUE)
})

test_that("read_detections() and read_tag_specs() list the widest sheet of a workbook", {
  for (nm in c("walleye_workbook.xlsm", "walleye_workbook.xlsx")) {
    wb <- glatos_file(nm)
    for (fm in c("auto", "glatos")) {
      e <- expect_error(read_detections(wb, format = fm), class = "detqc_error")
      expect_match(conditionMessage(e), paste(nm, "is a workbook;"), fixed = TRUE)
      expect_match(conditionMessage(e), "Its widest sheet, \"Tagging\", has 60 columns.",
                   fixed = TRUE)
      expect_match(conditionMessage(e), "Its nearest columns: tag_code_space, tag_id_code, ",
                   fixed = TRUE)
      expect_false(grepl("Unzip", conditionMessage(e), fixed = TRUE))
    }
  }
  hfx <- glatos_file("hfx_deploy_simplified.xlsx")
  e <- expect_error(read_tag_specs(hfx), class = "detqc_error")
  expect_match(conditionMessage(e), "header row this format's column map recognises. Its widest",
               fixed = TRUE)
  expect_match(conditionMessage(e), "\"Sheet1\", has: ", fixed = TRUE)
})

test_that("a VDAT CSV is named, a headerless file says so, and a codes column ranks first", {
  log <- system.file("extdata", "receiver_logs", "VR2Tx_R001_20240611_1.csv", package = "detqc")
  for (f in c(log, system.file("extdata", "stoney_vdat.csv", package = "detqc"))) {
    e <- expect_error(read_tag_specs(f), class = "detqc_error")
    expect_match(conditionMessage(e),
                 "The file is a Fathom VDAT CSV: read its detections, if it holds any, with",
                 fixed = TRUE)
  }
  tags <- utils::read.csv(system.file("extdata", "stoney_tag_specs.csv", package = "detqc"),
                          colClasses = "character")
  bare <- withr::local_tempfile(fileext = ".csv")
  utils::write.table(tags, bare, sep = ",", row.names = FALSE, col.names = FALSE)
  e <- expect_error(read_tag_specs(bare), class = "detqc_error")
  expect_match(conditionMessage(e), "The file has no header row of column names.", fixed = TRUE)
  for (fm in c("auto", "glatos")) {
    e <- expect_error(read_animals(glatos_file("lamprey_tag_specs.xls"), format = fm),
                      class = "detqc_error")
    expect_match(conditionMessage(e), "Its nearest columns: vue_tag_id, ", fixed = TRUE)
  }
  # hfx's receiver column holds OTN deployment ids, which take the shape of a
  # code; a column the maps read for a receiver does not rank as codes.
  e <- expect_error(read_tag_specs(glatos_file("hfx_detections.csv")), class = "detqc_error")
  expect_match(conditionMessage(e), "Its nearest columns: tagname, ", fixed = TRUE)
})

test_that("a deployment workbook's columns rank against the targets a deployment table needs", {
  hfx <- glatos_file("hfx_deploy_simplified.xlsx")
  map <- column_map("deployments", "otn")
  map$source <- paste0("zz_", seq_len(nrow(map)))
  e <- expect_error(read_deployments(hfx, "otn", col_map = map), class = "detqc_error")
  expect_match(conditionMessage(e), paste("Its widest sheet, \"Sheet1\", has: otn_array,",
                                          "station_no, deploy_date_time,"), fixed = TRUE)
  # The workbook's own seven column names
  have <- names(read_raw_table(hfx, c("otn_array", "station_no")))
  expect_length(have, 7L)
  needed <- contract_spec$deployments$column[contract_spec$deployments$required]
  expect_identical(nearest_columns(needed, have, 3),
                   c("deploy_lat", "deploy_date_time", "recover_date_time"))
  expect_identical(nearest_columns("transmitter", have, 1), "otn_array")
})

# ---- Choice arguments ----
test_that("a format of more than one value stops, and the default takes its first", {
  ext <- system.file("extdata", "stoney_detections.csv", package = "detqc")
  e <- expect_error(read_detections(ext, format = c("otn", "glatos")), class = "detqc_error")
  expect_match(conditionMessage(e), "`format` must be one of \"auto\", \"otn\",",
               fixed = TRUE)
  expect_match(conditionMessage(e), "it has \"otn\", \"glatos\".", fixed = TRUE)
  expect_identical(conditionCall(e)[[1]], as.name("read_detections"))
  full <- eval(formals(read_detections)$format)
  expect_identical(read_detections(ext, format = full), read_detections(ext))
  expect_error(read_deployments(written("deployments"), format = c("otn", "glatos")),
               "must be one of", class = "detqc_error")
  expect_identical(attr(read_deployments(written("deployments")), "source")$format, "detqc")
})

test_that("the other choice arguments stop with a classed error that lists their values", {
  e <- expect_error(column_map("receivers"), class = "detqc_error")
  expect_match(conditionMessage(e), paste("`table` must be one of \"detections\", \"deployments\",",
                                          "\"animals\"; it has \"receivers\"."), fixed = TRUE)
  expect_identical(conditionCall(e)[[1]], as.name("column_map"))
  expect_identical(nrow(column_map("dep", "otn")), nrow(column_map("deployments", "otn")))
  expect_error(summarise_detections(stoney_clean, by = "week"), "`by` must be one of",
               class = "detqc_error")
  expect_error(write_detset(stoney_clean, tempfile(), format = "csv"), "`format` must be one of",
               class = "detqc_error")
  expect_error(as_detfield_input(stoney_clean, listening = "hours"), "`listening` must be one of",
               class = "detqc_error")
  expect_error(to_ato(stoney_clean, as = "csv"), "`as` must be one of", class = "detqc_error")
})
