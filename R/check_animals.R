# ---- Animal and tag checks ----
# These need an animals table; without one each reports "not run". An
# animal's release is its earliest release record, so a second record never
# makes its earlier detections look pre-release: duplicate_animal_id
# reports the second record instead.

animal_release <- function(ani) {
  if (!"release_datetime" %in% names(ani)) return(NULL)
  r <- ani[!is.na(release_datetime), list(release = min(release_datetime)), by = "animal_id"]
  if (nrow(r)) r else NULL
}

# The animal of each detection: its own animal_id, else the one animal whose
# transmitter it is. A transmitter on two animals names neither.
detection_animal <- function(det, ani) {
  one <- ani[, list(n = length(unique(animal_id)), animal_id = animal_id[1]), by = "transmitter"]
  one <- one[n == 1]
  from_tx <- one$animal_id[match(det$transmitter, one$transmitter)]
  data.table::fifelse(is.na(det$animal_id), from_tx, det$animal_id)
}

# One row per unknown transmitter with its detection count, most heard
# first: a code-space error in the tagging records (a tag heard thousands of
# times that no animal carries) tops the list, above the scattered codes of
# other projects' tags.
chk_transmitter_no_animal <- function(ctx) {
  if (is.null(ctx$ani)) return(not_run("no animals table"))
  known <- c(ctx$ani$transmitter, if (!is.null(ctx$ref)) ctx$ref$transmitter)
  tx <- ctx$det[, list(detections = .N, receivers = length(unique(receiver_serial)),
                       first = min(datetime), last = max(datetime)), by = "transmitter"]
  bad <- tx[!transmitter %in% known]
  data.table::setorderv(bad, c("detections", "transmitter"), c(-1L, 1L))
  note <- if (is.null(ctx$ref)) {
    "reference tags count as unknown unless given in `reference_tags`"
  } else {
    paste(n_of(nrow(ctx$ref), "reference tag"), "given")
  }
  note <- paste0(note, "; rows sorted by detections, most first")
  check_result(nrow(bad), nrow(tx), note, bad)
}

# Animals in the tagging records whose transmitters were never heard. With
# transmitter_no_animal it catches a code entered wrong: the animal is never
# heard under the code on record, and its real code is heard with no animal.
chk_animal_never_heard <- function(ctx) {
  if (is.null(ctx$ani)) return(not_run("no animals table"))
  ani <- ctx$ani[!is.na(animal_id)]
  heard_tx <- unique(ctx$det$transmitter)
  heard_id <- if ("animal_id" %in% names(ctx$det)) unique(ctx$det$animal_id) else character()
  per <- ani[, list(transmitters = paste(unique(transmitter), collapse = ", "),
                    heard = any(transmitter %in% heard_tx) | animal_id[1] %in% heard_id),
             by = "animal_id"]
  bad <- per[heard == FALSE, list(animal_id, transmitters)]
  check_result(nrow(bad), nrow(per),
               "animals with no detection of any of their transmitters", bad)
}

chk_before_release <- function(ctx) {
  if (is.null(ctx$ani)) return(not_run("no animals table"))
  rel <- animal_release(ctx$ani)
  if (is.null(rel)) return(not_run("the animals table has no release times"))
  who <- detection_animal(ctx$det, ctx$ani)
  release <- rel$release[match(who, rel$animal_id)]
  has <- !is.na(release)
  idx <- which(has & ctx$det$datetime < release)
  check_result(length(idx), sum(has), "detections before the animal's earliest release",
               det_rows(ctx, idx, release_datetime = release[idx]))
}

chk_after_tag_life <- function(ctx) {
  ani <- ctx$ani
  if (is.null(ani)) return(not_run("no animals table"))
  miss <- setdiff(c("tag_life_days", "release_datetime"), names(ani))
  if (length(miss)) {
    # Name only the column that is missing.
    return(not_run(sprintf("the animals table has no %s", paste(miss, collapse = " or "))))
  }
  life <- ani[!is.na(tag_life_days) & !is.na(release_datetime),
              list(expiry = max(release_datetime + tag_life_days * 86400)),
              by = c("animal_id", "transmitter")]
  who <- detection_animal(ctx$det, ani)
  expiry <- life$expiry[match(paste(who, ctx$det$transmitter),
                              paste(life$animal_id, life$transmitter))]
  has <- !is.na(expiry)
  idx <- which(has & ctx$det$datetime > expiry)
  check_result(length(idx), sum(has), "detections after release plus tag_life_days",
               det_rows(ctx, idx, tag_expiry = expiry[idx]))
}

# A GLATOS export has no tag life, so after_tag_life cannot run, and an
# animal can be heard more than a decade after release, almost certainly
# on another project's reused code. These two checks need no tag
# life.
chk_long_after_release <- function(ctx) {
  if (is.null(ctx$ani)) return(not_run("no animals table"))
  years <- ctx$settings$max_liberty_years
  if (is.null(years)) {
    return(not_run(paste("give check_settings(max_liberty_years = ), the longest life in years",
                         "of the study's tag models")))
  }
  rel <- animal_release(ctx$ani)
  if (is.null(rel)) return(not_run("the animals table has no release times"))
  who <- detection_animal(ctx$det, ctx$ani)
  release <- rel$release[match(who, rel$animal_id)]
  has <- !is.na(release)
  idx <- which(has & ctx$det$datetime > release + years * 365.25 * 86400)
  check_result(length(idx), sum(has), sprintf("detections more than %g years after release", years),
               det_rows(ctx, idx, release_datetime = release[idx]))
}

# A transmitter silent for more than `reappear_gap_days`, then heard only at
# stations it never used before the gap: a code reused by another project,
# or a tag moved by a person.
chk_reappeared_elsewhere <- function(ctx) {
  det <- ctx$det
  if (!nrow(det)) return(check_result(0L, 0L))
  st <- if ("station_id" %in% names(det)) det$station_id else ctx$dep$station_id[det$.dep]
  d <- data.table::data.table(transmitter = det$transmitter, datetime = det$datetime,
                              station_id = st)[!is.na(transmitter)]
  data.table::setorderv(d, c("transmitter", "datetime"))
  n_tx <- length(unique(d$transmitter))
  note <- sprintf("silent more than %g days, then heard only at stations not used before",
                  ctx$settings$reappear_gap_days)
  gap_s <- ctx$settings$reappear_gap_days * 86400
  d[, gap := c(0, diff(as.numeric(datetime))), by = "transmitter"]
  d[, after := cumsum(gap > gap_s), by = "transmitter"]
  last_gap <- d[, list(k = max(after)), by = "transmitter"][k > 0]
  if (!nrow(last_gap)) return(check_result(0L, n_tx, note))
  d <- d[last_gap, on = "transmitter"]
  per <- d[, list(
    gap_start = max(datetime[after < k]), gap_end = min(datetime[after == k]),
    stations_before = paste(sort(unique(stats::na.omit(station_id[after < k]))), collapse = ", "),
    stations_after = paste(sort(unique(stats::na.omit(station_id[after == k]))), collapse = ", "),
    detections_after = sum(after == k),
    new_only = !any(stats::na.omit(station_id[after == k]) %in% station_id[after < k])
  ), by = "transmitter"]
  bad <- per[new_only == TRUE][, new_only := NULL]
  bad[, gap_days := round(as.numeric(difftime(gap_end, gap_start, units = "days")))]
  check_result(nrow(bad), n_tx, note, bad)
}

chk_animal_conflict <- function(ctx) {
  ani <- ctx$ani
  if (is.null(ani)) return(not_run("no animals table"))
  cols <- intersect(c("species", "length", "weight"), names(ani))
  if (!length(cols)) return(not_run("the animals table has no species, length or weight"))
  norm <- function(v) if (is.character(v)) tolower(trimws(v)) else v
  per <- ani[, lapply(.SD, function(v) length(unique(stats::na.omit(norm(v))))),
             by = "animal_id", .SDcols = cols]
  bad <- per$animal_id[apply(as.matrix(per[, cols, with = FALSE]) > 1, 1, any)]
  what <- vapply(bad, function(a) {
    paste(cols[unlist(per[animal_id == a, cols, with = FALSE]) > 1], collapse = ", ")
  }, "")
  rows <- ani[animal_id %in% bad]
  rows[, conflict := what[match(animal_id, bad)]]
  check_result(length(bad), nrow(per), sprintf("animals with two values of %s",
                                               paste(cols, collapse = ", ")), rows)
}

chk_duplicate_animal_id <- function(ctx) {
  ani <- ctx$ani
  if (is.null(ani)) return(not_run("no animals table"))
  ani <- data.table::copy(ani)
  ani[, .r := .I]
  exact <- ani$animal_id[duplicated(ani[, !".r"])]
  tx_twice <- ani[, .N, by = c("animal_id", "transmitter")][N > 1, animal_id]
  two_releases <- if ("release_datetime" %in% names(ani)) {
    r <- ani[!is.na(release_datetime), length(unique(release_datetime)), by = "animal_id"]
    r$animal_id[r$V1 > 1]
  } else {
    character()
  }
  bad <- unique(c(exact, tx_twice, two_releases))
  rows <- ani[animal_id %in% bad]
  rows[, problem := data.table::fcase(
    animal_id %in% two_releases, "two release records",
    animal_id %in% tx_twice, "one transmitter twice",
    default = "a repeated row"
  )]
  rows[, .r := NULL]
  check_result(length(bad), length(unique(ani$animal_id)),
               "animal ids on two release records, a transmitter twice or a repeated row", rows)
}

# Columns named inside data.table calls in this file.
utils::globalVariables(c("gap", "after", "k", "new_only", "gap_days", "gap_start", "gap_end"))
