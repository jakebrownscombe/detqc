# The receiver's own transmitter from the deployment sheet.

utc <- function(s) as.POSIXct(s, tz = "UTC")

# An OTN instrument deployment short form with a TRANSMITTER column, in
# the column names of the short form. Values are invented.
otn_sheet <- function(transmitters) {
  n <- length(transmitters)
  raw <- data.frame(
    OTN_ARRAY = "EXA", STATION_NO = sprintf("%03d", seq_len(n)),
    DEPLOY_DATE_TIME = "2024-06-01T00:00:00", DEPLOY_LAT = 45 + seq_len(n) / 1000,
    DEPLOY_LONG = -78, INS_MODEL_NO = "VR2Tx", INS_SERIAL_NO = paste0("R", seq_len(n)),
    TRANSMITTER = transmitters, RECOVER_DATE_TIME = "2024-06-06T00:00:00",
    stringsAsFactors = FALSE
  )
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  utils::write.csv(raw, f, row.names = FALSE)
  f
}

test_that("read_deployments() reads TRANSMITTER as receiver_transmitter, tidied", {
  f <- otn_sheet(c("A69-1601-60001", " a69-1601-60002 ", "", "-"))
  dep <- read_deployments(f, "otn")
  expect_equal(dep$receiver_transmitter, c("A69-1601-60001", "A69-1601-60002", NA, NA))
  expect_false("transmitter" %in% names(dep))
  expect_equal(attr(dep, "source")$receiver_transmitter_malformed, 0L)
})

test_that("a malformed id becomes NA with a warning that counts it", {
  # "12345" has no code space and "A69 1601" no ID: both are malformed.
  f <- otn_sheet(c("A69-1601-60001", "12345", "A69 1601"))
  expect_warning(dep <- read_deployments(f, "otn"),
                 "2 receiver_transmitter values are not transmitter ids.*such as \"12345\"")
  expect_equal(dep$receiver_transmitter, c("A69-1601-60001", NA, NA))
  expect_equal(attr(dep, "source")$receiver_transmitter_malformed, 2L)
})

test_that("one malformed id and one reference tag are counted in the singular", {
  f <- otn_sheet(c("A69-1601-60001", "12345"))
  expect_warning(dep <- read_deployments(f, "otn"),
                 "1 receiver_transmitter value is not a transmitter id.*it was read as NA")
  expect_message(deployment_reference_tags(dep, NULL, "test"),
                 "so the 1 receiver_transmitter id of the deployments is the reference tag.",
                 fixed = TRUE)
})

test_that("sheets without the column have no receiver_transmitter", {
  dep <- read_deployments(system.file("extdata", "stoney_receivers.csv", package = "detqc"),
                          format = "glatos")
  expect_false("receiver_transmitter" %in% names(dep))
  expect_null(deployment_reference_tags(dep, NULL, "test"))
})

# Three receivers, 1 to 5 June 2024; the audit covers 1 to 4 June, from
# the first to the last detection. R1 carries A69-1601-60001 and R2
# A69-1601-60002; R3 has none.
# * R1 hears its own tag at 12:00 on 1, 2 and 4 June, and nothing on 3 June.
# * R2 never logs its own tag; it hears animal a1 on 1, 2 and 4 June and
#   R1's tag once, on 1 June.
# * R3 hears a1 on 1 and 4 June.
# With the ids as reference tags (no receiver_serial) and min_share 0.5, a
# receiver is expected to hear a tag it heard on half its full days within
# the tag's span (1 to 4 June for 60001; 60002 is never heard, so it has no
# span and tests nothing). R1 heard 60001 on 3 of 4 days: expected, and on
# 3 June it heard nothing, so that day is reference_silent. R2 heard 60001
# on 1 of 4 days: not expected, so its silent 3 June stays silent.
own_case <- function(animals_extra = NULL) {
  t <- function(d) utc(sprintf("2024-06-%02d 12:00:00", d))
  det <- data.frame(
    datetime = c(t(c(1, 2, 4)), t(c(1, 2, 4)), t(1), t(c(1, 4))),
    receiver_serial = c(rep("R1", 3), rep("R2", 3), "R2", rep("R3", 2)),
    transmitter = c(rep("A69-1601-60001", 3), rep("A69-9001-1", 3), "A69-1601-60001",
                    rep("A69-9001-1", 2)),
    animal_id = c(NA, NA, NA, "a1", "a1", "a1", NA, "a1", "a1"), stringsAsFactors = FALSE
  )
  dep <- data.frame(receiver_serial = c("R1", "R2", "R3"), station_id = c("S1", "S2", "S3"),
                    deploy_datetime = utc("2024-06-01"), recover_datetime = utc("2024-06-06"),
                    deploy_lat = c(45.001, 45.002, 45.003), deploy_lon = -78,
                    receiver_transmitter = c("A69-1601-60001", "A69-1601-60002", NA),
                    stringsAsFactors = FALSE)
  ani <- rbind(data.frame(animal_id = "a1", transmitter = "A69-9001-1"), animals_extra)
  detset(det, dep, ani)
}

test_that("audit_listening() takes the ids as reference tags when none are given", {
  x <- own_case()
  expect_message(y <- audit_listening(x),
                 "the 2 receiver_transmitter ids of the deployments are the reference tags")
  st <- function(rx) y$listening$status[y$listening$receiver_serial == rx]
  expect_equal(st("R1"), c("listening", "listening", "reference_silent", "listening"))
  expect_equal(st("R2"), c("listening", "listening", "silent", "listening"))
  expect_equal(st("R3"), c("listening", "silent", "silent", "listening"))
  expect_match(tail(y$log$parameters, 1),
               "reference_tags = \"receiver_transmitter of the deployments (2)\"", fixed = TRUE)
  # 1 reference tag heard (60002 never is), expected at 1 receiver (R1).
  expect_match(tail(y$log$reason, 1), "1 reference tag, 1 expected receiver pair")
  # Without the ids, R1's 3 June is silent.
  x0 <- x
  x0$deployments$receiver_transmitter <- NULL
  z <- audit_listening(x0)
  expect_equal(z$listening$status[z$listening$receiver_serial == "R1"][3], "silent")
})

test_that("reference_tags given by hand win over the deployments' ids", {
  x <- own_case()
  # R1's id paired with R2 alone: R2 is expected to hear it on 1 to 4 June,
  # so its silent 3 June is reference_silent and its 2 and 4 June, with a1
  # heard, reference_missed; R1 is not paired, so its 3 June is silent.
  ref <- data.frame(transmitter = "A69-1601-60001", receiver_serial = "R2")
  expect_silent(y <- audit_listening(x, reference_tags = ref))
  st <- function(rx) y$listening$status[y$listening$receiver_serial == rx]
  expect_equal(st("R1")[3], "silent")
  expect_equal(st("R2"), c("listening", "reference_missed", "reference_silent",
                           "reference_missed"))
  expect_no_match(tail(y$log$parameters, 1), "receiver_transmitter of the deployments")
})

test_that("an id the animals table also lists is not a reference tag", {
  # The animals table lists R1's id as animal a2's tag: the id is left out
  # with a warning, so R1's 3 June is silent and has no reference tag.
  x <- own_case(data.frame(animal_id = "a2", transmitter = "A69-1601-60001"))
  w <- character()
  y <- withCallingHandlers(suppressMessages(audit_listening(x)), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_true(any(grepl("1 receiver_transmitter id of the deployments is also in the animals",
                        w, fixed = TRUE)))
  expect_true(any(grepl("it is not used as a reference tag", w, fixed = TRUE)))
  expect_equal(y$listening$status[y$listening$receiver_serial == "R1"][3], "silent")
})

test_that("check_detset() reads the ids for the reference-tag checks", {
  x <- own_case()
  expect_message(y <- suppressWarnings(check_detset(x, checks = "transmitter_no_animal")),
                 "receiver_transmitter ids of the deployments are the reference tags")
  s <- y$checks$summary
  # The only transmitters heard are a1's and R1's id, which is now a
  # reference tag: nothing unknown.
  expect_equal(s$flagged[s$check == "transmitter_no_animal"], 0L)
  expect_match(s$note[s$check == "transmitter_no_animal"], "2 reference tags given")
  # Without the column the id is an unknown transmitter.
  x0 <- x
  x0$deployments$receiver_transmitter <- NULL
  y0 <- check_detset(x0, checks = "transmitter_no_animal")
  expect_equal(y0$checks$summary$flagged, 1L)
  # reference_position runs on the ids' deployment positions with a field:
  # 1,000 m of range, and R1's id heard at R1 and at R2, 111 m away.
  y2 <- suppressMessages(suppressWarnings(check_detset(x, checks = "reference_position",
                                                       field = 1000)))
  s2 <- y2$checks$summary
  expect_equal(s2$status, "pass")
  expect_equal(s2$of, 4L)
  # Given by hand, the ids of the deployments are not read.
  y3 <- suppressWarnings(check_detset(x, checks = "transmitter_no_animal",
                                      reference_tags = data.frame(transmitter = "none")))
  expect_equal(y3$checks$summary$flagged, 1L)
})

test_that("check_detset() runs 30 checks", {
  expect_length(all_checks(), 30L)
})
