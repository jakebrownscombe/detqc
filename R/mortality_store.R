# ---- The store of reviewer calls ----
# `x$mortality` holds one row per animal the reviewer called (CONTRACT.md,
# section 7). The browser keeps the same calls, keyed by animal_id, until
# they are exported as CSV; this file reads that CSV and compares the calls
# with bands computed later.

call_levels <- c("alive", "dead_or_shed", "uncertain")

store_columns <- c("animal_id", "call", "death_date", "death_time", "date_source", "note",
                   "band", "annotated_at", "file", "md5")

#' Find the calls that new data or new bands put in question
#'
#' Compares the reviewer's calls in `x$mortality` with bands computed after
#' the calls were made, for example after a new year of detections was added
#' and [detect_mortality()] was run again. An animal needs another look when
#' it has no call and sits in the dead or review band; when it was called
#' alive and is now in the dead band; when it was called dead_or_shed and is
#' now in the alive band, or has detections more than `moved_km` from its
#' last position on or before the death date; when its call is uncertain; or when
#' a dead_or_shed call has no date. Calls are not changed: the reviewer makes
#' the new call on the cards, and [apply_mortality_calls()] brings it back.
#'
#' @param x A `detset` with calls from [apply_mortality_calls()].
#' @param bands The result of [detect_mortality()] on `x` as it is now.
#' @param moved_km Movement after the death date, in km. A detection after
#'   the death date counts as movement when it lies more than `moved_km` from
#'   the station that heard the animal last on or before that date. One
#'   finite positive number; 1 km by default. The bands carry their own
#'   `sites_km`, the distance [detect_mortality()] uses to group stations
#'   into sites; `reconcile_mortality()` does not read it.
#' @return `x` with `x$reconcile`, a data frame of every animal in the bands:
#'   `animal_id`, `call`, `death_date`, `band`, `needs_review` and `reason`;
#'   and a new log row that counts each reason.
#' @seealso [review_mortality()], which can build cards for these animals.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' calls <- data.frame(animal_id = c("ST-02", "ST-03"),
#'                     call = c("alive", "uncertain"))
#' calls_file <- tempfile(fileext = ".csv")
#' utils::write.csv(calls, calls_file, row.names = FALSE)
#' dets <- reconcile_mortality(apply_mortality_calls(dets, calls_file), detect_mortality(dets))
#' dets$reconcile[dets$reconcile$needs_review, c("animal_id", "call", "band", "reason")]
reconcile_mortality <- function(x, bands, moved_km = 1) {
  validate_detset(x)
  cb <- card_bands(bands)
  if (is.null(x$mortality)) stop_detqc("No calls on `x`. Run apply_mortality_calls() first.")
  if (!is_positive_number(moved_km) || !is.finite(moved_km)) {
    stop_detqc("`moved_km` must be one finite positive number.")
  }
  b <- cb$bands[, c("animal_id", "band")]
  m <- x$mortality
  out <- data.frame(animal_id = b$animal_id, call = m$call[match(b$animal_id, m$animal_id)],
                    death_date = m$death_date[match(b$animal_id, m$animal_id)], band = b$band,
                    stringsAsFactors = FALSE)
  moved <- moved_after_death(x, out, moved_km)
  out$reason <- reconcile_reason(out, moved)
  out$needs_review <- !is.na(out$reason)
  keep <- c("animal_id", "call", "death_date", "band", "needs_review", "reason")
  out <- out[order(!out$needs_review, out$animal_id), keep]
  rownames(out) <- NULL
  x$reconcile <- out
  tab <- table(factor(out$reason, reconcile_reasons))
  reason <- sprintf("%s of %s in the bands need another look: %s", fmt_n(sum(out$needs_review)),
                    n_of(nrow(out), "animal"),
                    paste(sprintf("%s %s", names(tab), fmt_n(as.integer(tab))), collapse = "; "))
  before <- c(nrow(x$detections), count_animals(x$detections))
  log_step(x, "reconcile_mortality", list(calls = nrow(m), moved_km = moved_km), before, 0L,
           reason)
}

reconcile_reasons <- c("no call, dead or review band", "called alive, now dead band",
                       "called dead_or_shed, now alive band", "moved after the death date",
                       "called uncertain", "dead_or_shed without a date")

reconcile_reason <- function(out, moved) {
  r <- rep(NA_character_, nrow(out))
  call <- ifelse(is.na(out$call), "", out$call)
  set <- function(r, hit, k) {
    r[is.na(r) & hit] <- reconcile_reasons[k]
    r
  }
  r <- set(r, call == "" & out$band %in% c("dead", "review"), 1)
  r <- set(r, call == "alive" & out$band == "dead", 2)
  r <- set(r, call == "dead_or_shed" & out$band == "alive", 3)
  r <- set(r, call == "dead_or_shed" & out$animal_id %in% moved, 4)
  r <- set(r, call == "uncertain", 5)
  set(r, call == "dead_or_shed" & is.na(out$death_date), 6)
}

# Animals called dead_or_shed whose detections after the death date lie more
# than `moved_km` from the station that heard them last on or before it: a
# tag that keeps sending from one place is consistent with the call, a tag
# heard elsewhere is not.
moved_after_death <- function(x, out, moved_km) {
  dead <- out[!is.na(out$call) & out$call == "dead_or_shed" & !is.na(out$death_date), ]
  if (!nrow(dead)) return(character())
  pos <- card_positions(x)$det[animal_id %in% dead$animal_id]
  pos[, death := dead$death_date[match(animal_id, dead$animal_id)]]
  data.table::setorderv(pos, c("animal_id", "datetime"))
  last <- pos[date <= death, list(lat0 = lat[.N], lon0 = lon[.N]), by = "animal_id"]
  after <- pos[date > death][last, on = "animal_id", nomatch = NULL]
  if (!nrow(after)) return(character())
  after[, km := haversine_km(lat0, lon0, lat, lon)]
  unique(after[km > moved_km]$animal_id)
}

# ---- Internal: reading the calls file ----
read_calls <- function(raw, x, label) {
  get <- function(nm) if (nm %in% names(raw)) trimws(raw[[nm]]) else rep("", nrow(raw))
  calls <- data.frame(animal_id = trimws(raw$animal_id), call = tolower(trimws(raw$call)),
                      death_date = get("death_date"), death_time = get("death_time"),
                      date_source = get("date_source"),
                      note = get("note"), band = get("band"), annotated_at = get("annotated_at"),
                      candidate_date = get("candidate_date"), stringsAsFactors = FALSE)
  skipped <- sum(!nzchar(calls$call))
  calls <- calls[nzchar(calls$call), , drop = FALSE]
  bad <- setdiff(calls$call, call_levels)
  if (length(bad)) {
    stop_detqc("%s: call \"%s\" is not one of %s.", label, bad[1],
               paste(call_levels, collapse = ", "))
  }
  # An animal of the animals table with no detection in the set is known: a
  # calls file for a whole study applies to a month cut from it.
  known <- c(x$detections$animal_id, if (!is.null(x$animals)) x$animals$animal_id)
  unknown <- setdiff(calls$animal_id, known)
  unheard <- setdiff(intersect(calls$animal_id, known), x$detections$animal_id)
  if (length(unheard)) {
    one <- length(unheard) == 1
    message(sprintf(paste("%s: %s %s no detection in the detset, such as \"%s\"; %s",
                          "stored and %s nothing."),
                    label, n_of(length(unheard), "called animal"), if (one) "has" else "have",
                    unheard[1], if (one) "its call is" else "their calls are",
                    if (one) "removes" else "remove"))
  }
  if (length(unknown)) {
    stop_detqc("%s: %s %s not in the detset, such as \"%s\".", label,
               n_of(length(unknown), "animal"), verb(length(unknown), "is", "are"), unknown[1])
  }
  calls <- calls[order(calls$animal_id, calls$annotated_at), , drop = FALSE]
  calls <- calls[!duplicated(calls$animal_id, fromLast = TRUE), , drop = FALSE]
  times <- parse_call_times(calls$death_time, label)
  parsed <- parse_call_dates(calls$death_date, label)
  # A death time sets the death date: the UTC day it falls on.
  parsed[!is.na(times)] <- as.Date(times[!is.na(times)], tz = "UTC")
  alive_dated <- calls$call == "alive" & !is.na(parsed)
  parsed[alive_dated] <- as.Date(NA)
  times[calls$call == "alive"] <- NA
  calls$death_date <- parsed
  calls$death_time <- times
  calls$date_source[is.na(parsed)] <- ""
  cand <- suppressWarnings(as.Date(substr(calls$candidate_date, 1, 10), format = "%Y-%m-%d"))
  has_cand <- "candidate_date" %in% names(raw)
  calls$candidate_date <- NULL
  for (cl in c("date_source", "note", "band", "annotated_at")) {
    calls[[cl]][!nzchar(calls[[cl]])] <- NA_character_
  }
  rownames(calls) <- NULL
  structure(calls, dropped_dates = sum(alive_dated), skipped = skipped,
            candidate = if (has_cand) cand else NULL)
}

parse_call_dates <- function(dates, label) {
  dates[!nzchar(dates)] <- NA_character_
  parsed <- as.Date(substr(dates, 1, 10), format = "%Y-%m-%d")
  if (any(!is.na(dates) & is.na(parsed))) {
    stop_detqc("%s: death_date \"%s\" is not a date (YYYY-MM-DD).", label,
               dates[!is.na(dates) & is.na(parsed)][1])
  }
  parsed
}

# A death time is UTC, written as "2024-06-04 02:00:00" or
# "2024-06-04T02:00:00Z"; the review pages write the second form.
parse_call_times <- function(times, label) {
  times[!nzchar(times)] <- NA_character_
  txt <- sub("Z$", "", sub("T", " ", times, fixed = TRUE))
  parsed <- as.POSIXct(txt, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M"),
                       optional = TRUE)
  if (any(!is.na(times) & is.na(parsed))) {
    stop_detqc("%s: death_time \"%s\" is not a UTC time (YYYY-MM-DD HH:MM:SS).", label,
               times[!is.na(times) & is.na(parsed)][1])
  }
  parsed
}

# The cheapest guard against a stamp on the wrong day: a death date before
# the first detection removes the whole record, an undated dead_or_shed call
# removes nothing, and a date set weeks before the candidate date removes the
# detections between them. One arrow key and D can stamp the second day of
# the record, which would remove nearly all of it.
check_call_dates <- function(calls, x) {
  d <- x$detections[!is.na(x$detections$animal_id), c("animal_id", "datetime")]
  first <- tapply(as.Date(d$datetime), d$animal_id, min)
  first <- as.Date(first[calls$animal_id], origin = "1970-01-01")
  dead <- calls$call == "dead_or_shed"
  undated <- calls$animal_id[dead & is.na(calls$death_date)]
  early <- calls$animal_id[dead & !is.na(calls$death_date) & !is.na(first) &
                             calls$death_date < first]
  if (length(undated)) {
    warn_detqc("%s %s no death date and will remove nothing: %s.",
               n_of(length(undated), "dead_or_shed call"), verb(length(undated), "has", "have"),
               paste(utils::head(undated, 10), collapse = ", "))
  }
  if (length(early)) {
    warn_detqc("%s %s before the animal's first detection: %s.",
               n_of(length(early), "death date"), verb(length(early), "falls", "fall"),
               paste(utils::head(early, 10), collapse = ", "))
  }
  cand <- attr(calls, "candidate")
  far <- "no candidate_date column"
  before_cand <- character()
  if (!is.null(cand)) {
    gap <- as.integer(calls$death_date - cand)
    far <- sprintf("%s more than 30 days from the candidate date (of %s with both)",
                   n_of(sum(dead & abs(gap) > 30, na.rm = TRUE), "dated call"),
                   fmt_n(sum(dead & !is.na(gap))))
    before_cand <- calls$animal_id[dead & !is.na(gap) & gap < -30]
  }
  if (length(before_cand)) {
    warn_detqc(paste("%s %s more than 30 days before the candidate date, and",
                     "filter_mortality() will remove the detections between them: %s."),
               n_of(length(before_cand), "death date"), verb(length(before_cand), "falls", "fall"),
               paste(utils::head(before_cand, 10), collapse = ", "))
  }
  list(undated = undated, early = early, far = far, before_candidate = before_cand)
}
