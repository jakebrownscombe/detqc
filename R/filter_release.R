# ---- Filters on release and tag life ----

#' Remove detections before release
#'
#' Removes each animal's detections before its release. An animal with more
#' than one release record is taken at its earliest, so a second record never
#' removes detections; [check_detset()] reports such animals. Detections with
#' no `animal_id`, or of an animal with no release time, are kept.
#'
#' A detection whose `animal_id` the animals table does not list, as when an
#' OTN matched extract names animals by `catalogNumber` and the tagging sheet
#' leaves `ANIMAL_ID` blank, takes the release of its transmitter: the
#' animal of that transmitter released last at or before the detection, or
#' its first release for a detection before every release. A message counts
#' these detections. When no detection matches a release, a warning says so
#' and counts the detected transmitters the animals table lists. The same
#' rules hold in [filter_post_release()] and [filter_tag_life()].
#'
#' @param x A [detset()] with an animals table that has `release_datetime`.
#'   Without one the step does not run: it says so in a message and in its
#'   log record, and removes nothing.
#' @return `x` with the detections kept and one log record (reason
#'   `before_release`).
#' @family filters
#' @export
#' @examples
#' study <- detqc_example
#' dets <- filter_release(detset(study$detections, study$deployments, study$animals))
#' writeLines(strwrap(detset_log(dets)$reason[2], width = 72))
filter_release <- function(x) {
  need_detset(x)
  skip <- skip_without_release(x, "filter_release", list())
  if (!is.null(skip)) return(skip)
  release <- release_per_detection(x, "filter_release")
  early <- !is.na(release) & x$detections$datetime < release
  apply_filter(x, !early, "filter_release", list(), rep("before_release", length(early)))
}

#' Remove detections in a window after release
#'
#' Removes each animal's detections from its release until `hours` later,
#' when handling and recovery from surgery can change behaviour. Detections
#' before release are left to [filter_release()].
#'
#' @param x A [detset()] with an animals table that has `release_datetime`.
#'   Without one the step does not run, as in [filter_release()].
#' @param hours Length of the window after release, in hours.
#' @return `x` with the detections kept and one log record (reason
#'   `post_release_window`).
#' @family filters
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' dets <- filter_post_release(dets, hours = 24)
#' writeLines(strwrap(detset_log(dets)$reason[2], width = 72))
filter_post_release <- function(x, hours) {
  need_detset(x)
  need_number(hours, "hours")
  skip <- skip_without_release(x, "filter_post_release", list(hours = hours))
  if (!is.null(skip)) return(skip)
  release <- release_per_detection(x, "filter_post_release")
  t <- x$detections$datetime
  inside <- !is.na(release) & t >= release & t < release + hours * 3600
  apply_filter(x, !inside, "filter_post_release", list(hours = hours),
               rep("post_release_window", length(inside)))
}

#' Remove detections after the tag's expected life
#'
#' Removes the detections of each transmitter later than its release plus
#' `tag_life_days` from the animals table. Transmitters are matched by
#' `animal_id` and `transmitter` together, so a transmitter reused on a
#' second animal takes the life of the right tag.
#'
#' @param x A [detset()] with an animals table that has `release_datetime`.
#'   Without one the step does not run, as in [filter_release()]; nor does it
#'   when no transmitter has a tag life and `default_days` is `NULL`.
#' @param default_days Tag life in days for a transmitter whose
#'   `tag_life_days` is missing. `NULL` keeps its detections.
#' @return `x` with the detections kept and one log record (reason
#'   `after_tag_life`).
#' @family filters
#' @export
#' @examples
#' study <- detqc_example
#' dets <- filter_tag_life(detset(study$detections, study$deployments, study$animals))
#' writeLines(strwrap(detset_log(dets)$reason[2], width = 72))
filter_tag_life <- function(x, default_days = NULL) {
  need_detset(x)
  if (!is.null(default_days)) need_number(default_days, "default_days")
  params <- list(default_days = default_days)
  skip <- skip_without_release(x, "filter_tag_life", params)
  if (!is.null(skip)) return(skip)
  ani <- x$animals
  life <- if ("tag_life_days" %in% names(ani)) ani$tag_life_days else rep(NA_real_, nrow(ani))
  if (!is.null(default_days)) life[is.na(life)] <- default_days
  a <- as_dt(ani)[, list(animal_id, transmitter, expiry = release_datetime + life * 86400)]
  # The aggregate runs only on rows with an expiry: max() warns on none.
  a <- a[!is.na(expiry)]
  if (!nrow(a)) {
    return(skip_step(x, "filter_tag_life", params, paste(
      "no transmitter has a tag life; give `default_days`, the longest life of the study's tag",
      "models, to remove detections after it, as for a GLATOS export with no tag life"
    )))
  }
  a <- a[, list(expiry = max(expiry)), by = c("animal_id", "transmitter")]
  det <- x$detections
  expiry <- a$expiry[match(paste(det$animal_id, det$transmitter),
                           paste(a$animal_id, a$transmitter))]
  k <- transmitter_rows(x)
  by_tx <- !is.na(k)
  expiry[by_tx] <- ani$release_datetime[k[by_tx]] + life[k[by_tx]] * 86400
  note_by_transmitter(x, by_tx & !is.na(expiry), "filter_tag_life")
  warn_no_release_match(x, expiry, "filter_tag_life")
  late <- !is.na(expiry) & det$datetime > expiry
  apply_filter(x, !late, "filter_tag_life", params, rep("after_tag_life", length(late)))
}

# ---- Internal: release times by transmitter ----
# One release time per detection: its animal's earliest, or, for a
# detection whose animal_id the animals table does not list, its
# transmitter's. An OTN matched extract names animals by catalogNumber while
# an OTN tagging sheet read with missing_id = "transmitter" names them by
# transmitter, so the two tables share no animal_id.
release_per_detection <- function(x, step) {
  release <- release_per_animal(x)
  k <- transmitter_rows(x)
  by_tx <- !is.na(k)
  release[by_tx] <- x$animals$release_datetime[k[by_tx]]
  note_by_transmitter(x, by_tx, step)
  warn_no_release_match(x, release, step)
  release
}

# The row of the animals table each detection takes by transmitter and
# release time, for detections with an animal_id the table does not list:
# the row of that transmitter released last at or before the detection, or
# its first release for a detection before every release. NA elsewhere, and
# for a detection with no animal_id, which stays unassigned as in detset().
transmitter_rows <- function(x) {
  det <- x$detections
  ani <- x$animals
  k <- rep(NA_integer_, nrow(det))
  free <- !is.na(det$animal_id) & !det$animal_id %in% ani$animal_id
  if (!any(free)) return(k)
  a <- data.table::data.table(row = seq_len(nrow(ani)), transmitter = ani$transmitter,
                              t = ani$release_datetime)[!is.na(t) & !is.na(transmitter)]
  if (!nrow(a)) return(k)
  q <- data.table::data.table(transmitter = det$transmitter[free], t = det$datetime[free])
  hit <- a$row[a[q, on = c("transmitter", "t"), roll = TRUE, which = TRUE]]
  first <- a[order(t)][!duplicated(transmitter)]
  early <- is.na(hit)
  hit[early] <- first$row[match(q$transmitter[early], first$transmitter)]
  k[which(free)] <- hit
  k
}

note_by_transmitter <- function(x, by_tx, step) {
  if (!any(by_tx)) return(invisible(0L))
  message(sprintf(paste("%s(): %s of %s %s an animal_id the animals table does not",
                        "list (%s), and took the release of %s transmitter."),
                  step, fmt_n(sum(by_tx)), n_of(length(by_tx), "detection"),
                  verb(sum(by_tx), "carries", "carry"),
                  n_of(length(unique(x$detections$animal_id[by_tx])), "animal_id"),
                  verb(sum(by_tx), "its", "their")))
  invisible(sum(by_tx))
}

# A release filter that matches no detection to a release removes nothing,
# and a log record of 0 removed cannot tell that from a clean set, so it
# warns.
warn_no_release_match <- function(x, release, step) {
  det <- x$detections
  if (!nrow(det) || any(!is.na(release))) return(invisible(FALSE))
  tx <- unique(stats::na.omit(det$transmitter))
  listed <- sum(tx %in% x$animals$transmitter[!is.na(x$animals$release_datetime)])
  warn_detqc(paste("%s(): no detection matched an animal with a release time, so nothing was",
                   "removed. %s of %s %s an animal_id; %s of %s %s in the animals",
                   "table with a release time. Check that the",
                   "animals table is the tagging sheet of these detections."),
             step, fmt_n(sum(!is.na(det$animal_id))), n_of(nrow(det), "detection"),
             verb(sum(!is.na(det$animal_id)), "has", "have"), fmt_n(listed),
             n_of(length(tx), "detected transmitter"), verb(listed, "is", "are"))
  invisible(TRUE)
}
