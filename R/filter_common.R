# ---- Shared machinery of the filters ----
# Every filter is opt-in: nothing runs unless the user calls it, and each
# threshold that decides whether a detection is removed has no default. A
# setting that only picks which of two conflicting detections goes, such as
# the support window of filter_speed(), may have one. A filter keeps a
# subset of the detections, so it can never add a row; apply_filter() takes
# a logical keep vector and is the only place a filter changes the table.

apply_filter <- function(x, keep, step, parameters, reasons, extra = "") {
  n <- nrow(x$detections)
  if (!is.logical(keep) || length(keep) != n || anyNA(keep)) {
    stop_detqc("Internal error in %s: the keep vector does not match the detections.", step)
  }
  removed_by <- removal_reasons(reasons, keep)
  before <- c(n, count_animals(x$detections))
  lost <- unique(stats::na.omit(x$detections$animal_id[!keep]))
  lost <- setdiff(lost, x$detections$animal_id[keep])
  x$detections <- x$detections[keep, , drop = FALSE]
  rownames(x$detections) <- NULL
  reason <- sprintf("removed %s of %s (%s%%)%s; %s lost every detection%s",
                    fmt_n(sum(!keep)), n_of(n, "detection"), format(round(100 * mean(!keep), 2)),
                    reason_text(removed_by), n_of(length(lost), "animal"), extra)
  log_step(x, step, parameters, before, removed_by, reason, fn = step)
}

# `reasons` is a character vector, one per detection, naming why a removed
# detection was removed; it is read only where keep is FALSE.
removal_reasons <- function(reasons, keep) {
  if (all(keep)) return(stats::setNames(integer(), character()))
  tab <- table(reasons[!keep])
  stats::setNames(as.integer(tab), names(tab))
}

reason_text <- function(removed_by) {
  if (length(removed_by) < 2) return("")
  paste0(": ", paste(sprintf("%s %s", fmt_n(removed_by), names(removed_by)), collapse = ", "))
}

need_detset <- function(x) {
  if (!inherits(x, "detset")) stop_detqc("`x` must be a detset, not %s.", class(x)[1])
  validate_detset(x)
}

need_number <- function(v, name, lower = 0) {
  if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < lower) {
    stop_detqc("`%s` must be one number of at least %s.", name, format(lower))
  }
  invisible(v)
}

# One release time per animal: the earliest of its records. Matching by
# animal_id on a one-row-per-animal table can never add rows.
release_per_animal <- function(x) {
  r <- as_dt(x$animals)[!is.na(release_datetime), list(release = min(release_datetime)),
                        by = "animal_id"]
  r$release[match(x$detections$animal_id, r$animal_id)]
}

# The release and tag-life filters need release times. A detset with none,
# as from an OTN qualified extract, skips the step with a message and a log
# record that says it did not run, rather than stopping: a pipeline run over
# several sources then needs no branch per source, and the report still
# shows the step and why it removed nothing.
skip_without_release <- function(x, step, parameters) {
  ani <- x$animals
  why <- if (is.null(ani)) {
    "the detset has no animals table"
  } else if (!"release_datetime" %in% names(ani)) {
    "the animals table has no release_datetime"
  } else if (all(is.na(ani$release_datetime))) {
    "no animal has a release_datetime"
  } else {
    return(NULL)
  }
  skip_step(x, step, parameters, why)
}

skip_step <- function(x, step, parameters, why) {
  message(sprintf("%s() did not run: %s. Nothing was removed.", step, why))
  log_step(x, step, parameters, x, 0L, sprintf("not run: %s; removed 0 of %s", why,
                                               n_of(nrow(x$detections), "detection")), fn = step)
}

#' Keep the detections inside a deployment of their receiver
#'
#' Removes the detections that no deployment of their receiver holds. A
#' deployment holds the detections from `deploy_datetime` up to, but not
#' including, `recover_datetime`. A deployment with no `recover_datetime` is
#' still in the water and holds every later detection, so a missing recovery
#' never removes detections. Receivers are matched on the serial as written,
#' then on the bare serial, and a deployment with no serial on its
#' `station_id`, as in [detset()].
#'
#' @param x A [detset()].
#' @param buffer_s Seconds added before each deployment and after each
#'   recovery, for clock drift or a recovery time written late.
#' @return `x` with the detections kept and one log record, which counts the
#'   removed detections by reason: `unknown_receiver` (the serial is in no
#'   deployment) or `outside_deployment`.
#' @family filters
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' dets <- filter_deployment_period(dets)
#' detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")]
#' writeLines(strwrap(detset_log(dets)$reason[2], width = 72))
filter_deployment_period <- function(x, buffer_s = 0) {
  need_detset(x)
  need_number(buffer_s, "buffer_s")
  dep <- x$deployments
  dep$deploy_datetime <- dep$deploy_datetime - buffer_s
  dep$recover_datetime <- dep$recover_datetime + buffer_s
  det <- x$detections
  row <- match_deployment(det, dep)
  known <- serial_in_deployments(det$receiver_serial, dep$receiver_serial)
  # A station that only serial-less deployments name is known by its station.
  bare_st <- dep$station_id[is.na(dep$receiver_serial)]
  if (length(bare_st) && "station_id" %in% names(det)) {
    known <- known | (!is.na(det$station_id) & det$station_id %in% bare_st)
  }
  reasons <- ifelse(known, "outside_deployment", "unknown_receiver")
  apply_filter(x, !is.na(as.vector(row)), "filter_deployment_period",
               list(buffer_s = buffer_s), reasons)
}

#' Remove duplicated detections
#'
#' Keeps the first of each set of detections that agree in the columns `by`.
#'
#' One detection read from two exports, such as a GLATOS export and the VDAT
#' file of the same receiver, can carry two stamps: GLATOS writes whole
#' seconds and VDAT keeps the fraction. On such pairs the GLATOS time
#' equals the rounded VDAT time, and exact matching finds none of them.
#' `tolerance_s` treats two detections of one transmitter on one receiver
#' less than that many seconds apart, after sorting by time, as one
#' detection and keeps the earlier. Keep it below the shortest delay
#' between transmissions of the tags in the set, so that two
#' real transmissions are never merged; 1 s suits the 0.5 s difference above.
#' With `tolerance_s = 0`, the default, and detections from both a VDAT file
#' and a whole-second format, a message says so.
#'
#' @param x A [detset()].
#' @param by Columns that define a duplicate. The default is one detection:
#'   one transmitter logged by one receiver at one time.
#' @param tolerance_s Seconds within which two detections that agree in the
#'   other columns of `by` are one detection. `by` must hold `datetime`.
#' @return `x` with the copies removed and one log record (reason
#'   `duplicate`).
#' @family filters
#' @export
#' @examples
#' study <- detqc_example
#' doubled <- rbind(study$detections, study$detections[1:5, ])
#' dets <- filter_duplicates(detset(doubled, study$deployments, study$animals))
#' writeLines(strwrap(detset_log(dets)$reason[2], width = 72))
#'
#' # The same five detections a fraction of a second later, as a second
#' # export of the receiver writes them.
#' late <- study$detections[1:5, ]
#' late$datetime <- late$datetime + 0.4
#' with_late <- detset(rbind(study$detections, late), study$deployments,
#'                     study$animals)
#' detset_log(filter_duplicates(with_late, tolerance_s = 1))$removed[2]
filter_duplicates <- function(x, by = c("datetime", "receiver_serial", "transmitter"),
                              tolerance_s = 0) {
  need_detset(x)
  need_number(tolerance_s, "tolerance_s")
  miss <- setdiff(by, names(x$detections))
  if (length(miss)) stop_detqc("The detections have no column `%s`.", miss[1])
  if (tolerance_s > 0 && !"datetime" %in% by) {
    stop_detqc("`tolerance_s` needs `datetime` in `by`.")
  }
  if (tolerance_s == 0) note_mixed_stamps(x$meta$sources)
  dup <- duplicate_rows(x$detections, by, tolerance_s)
  apply_filter(x, !dup, "filter_duplicates", list(by = by, tolerance_s = tolerance_s),
               rep("duplicate", length(dup)))
}

# TRUE for each row that repeats an earlier one. With a tolerance, rows that
# agree in the other columns and lie within tolerance_s of the row before
# them in time order are repeats; the earliest of each run is kept.
duplicate_rows <- function(det, by, tolerance_s = 0) {
  if (tolerance_s == 0) return(duplicated(as_dt(det)[, by, with = FALSE]))
  if (!nrow(det)) return(logical())
  rest <- setdiff(by, "datetime")
  d <- as_dt(det)[, c(rest, "datetime"), with = FALSE]
  d[, .r := .I]
  data.table::setorderv(d, c(rest, "datetime"))
  t <- as.numeric(d$datetime)
  same <- c(FALSE, Reduce(`&`, lapply(rest, function(cl) {
    v <- d[[cl]]
    n <- length(v)
    if (n < 2) return(logical())
    a <- v[-1]
    b <- v[-n]
    (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  }), init = rep(TRUE, max(0, nrow(d) - 1))))
  rep_prev <- same & c(FALSE, diff(t) < tolerance_s)
  out <- logical(nrow(d))
  out[d$.r] <- rep_prev
  out
}

# VDAT keeps fractions of a second and the network and VUE exports do not,
# so exact matching misses a detection read from both.
note_mixed_stamps <- function(src) {
  # Only the detection files count: an OTN deployment sheet beside VDAT
  # detections is not a mix of stamps.
  if (is.data.frame(src) && "table" %in% names(src)) src <- src[src$table == "detections", ]
  fm <- if (is.data.frame(src) && "format" %in% names(src)) unique(src$format) else character()
  whole <- intersect(fm, c("glatos", "otn", "vue"))
  if ("vdat" %in% fm && length(whole)) {
    message(sprintf(paste("filter_duplicates(): the detections come from vdat and %s files,",
                          "whose stamps for one detection differ by a fraction of a second;",
                          "tolerance_s = 1 matches them."), paste(whole, collapse = " and ")))
  }
  invisible(NULL)
}

#' Remove the detections of animals with too few detections
#'
#' Removes every detection of an animal detected fewer than `min_detections`
#' times or on fewer than `min_days` UTC dates. Detections with no
#' `animal_id`, such as reference tags, are kept.
#'
#' @param x A [detset()].
#' @param min_detections Fewest detections an animal needs to be kept.
#' @param min_days Fewest distinct UTC dates with a detection.
#' @return `x` with the detections kept and one log record (reasons
#'   `few_detections` and `few_days`).
#' @family filters
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' dets <- filter_min_detections(dets, min_detections = 10, min_days = 2)
#' writeLines(strwrap(detset_log(dets)$reason[2], width = 72))
filter_min_detections <- function(x, min_detections, min_days = 1) {
  need_detset(x)
  need_number(min_detections, "min_detections", 1)
  need_number(min_days, "min_days", 1)
  d <- as_dt(x$detections)[, list(animal_id, day = as.Date(datetime))]
  per <- d[!is.na(animal_id), list(n = .N, days = length(unique(day))), by = "animal_id"]
  few_n <- per$animal_id[per$n < min_detections]
  few_d <- setdiff(per$animal_id[per$days < min_days], few_n)
  a <- x$detections$animal_id
  reasons <- ifelse(a %in% few_n, "few_detections", "few_days")
  apply_filter(x, !(a %in% c(few_n, few_d)), "filter_min_detections",
               list(min_detections = min_detections, min_days = min_days), reasons)
}

#' Keep the detections of some animals or species
#'
#' Cuts a detection set to the animals of the given species, or to the
#' given animals, as a logged step. Cut a set after [detset()]:
#' [detset()] gives a reused transmitter's detections to the animal released
#' last before each one, and it can do so only when every animal is in the
#' animals table. A cut to one species made before [detset()] can give the
#' detections of another species' animal to an animal of that species. Run
#' [audit_listening()] and [check_detset()] before the cut too: both read
#' every transmitter the receivers logged, and a receiver that heard only
#' other species would look silent after it.
#'
#' The animals table is cut to the animals kept. Detections with no
#' `animal_id`, such as reference tags, are removed unless their transmitter
#' is in `transmitters`.
#'
#' @param x A [detset()].
#' @param species Optional species to keep, matched to `species` in the
#'   animals table, ignoring case.
#' @param animals Optional `animal_id` values to keep. With `species` too, an
#'   animal is kept when it meets either.
#' @param transmitters Optional transmitter codes kept whatever their animal,
#'   such as the reference tags.
#' @return `x` with the detections kept, the animals table cut to the animals
#'   kept, and one log record (reasons `other_animal` and `no_animal`).
#' @family filters
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' st02_only <- filter_animals(dets, animals = "ST-02")
#' writeLines(strwrap(detset_log(st02_only)$reason[2], width = 72))
filter_animals <- function(x, species = NULL, animals = NULL, transmitters = NULL) {
  need_detset(x)
  if (is.null(species) && is.null(animals)) stop_detqc("Give `species`, `animals` or both.")
  for (nm in c("species", "animals", "transmitters")) {
    v <- get(nm)
    if (!is.null(v) && (!is.character(v) || !length(v))) {
      stop_detqc("`%s` must be a character vector.", nm)
    }
  }
  ids <- union(as.character(animals), species_ids(x$animals, species))
  a <- x$detections$animal_id
  keep <- (!is.na(a) & a %in% ids) | x$detections$transmitter %in% transmitters
  reasons <- ifelse(is.na(a), "no_animal", "other_animal")
  y <- apply_filter(x, keep, "filter_animals",
                    list(species = species, animals = animals, transmitters = transmitters),
                    reasons)
  if (!is.null(y$animals)) {
    y$animals <- y$animals[y$animals$animal_id %in% ids, , drop = FALSE]
    rownames(y$animals) <- NULL
  }
  y
}

# The animal ids of the given species in the animals table.
species_ids <- function(ani, species) {
  if (is.null(species)) return(character())
  if (is.null(ani) || !"species" %in% names(ani)) {
    stop_detqc("`species` needs an animals table with a `species` column.")
  }
  hit <- !is.na(ani$species) & tolower(trimws(ani$species)) %in% tolower(trimws(species))
  if (!any(hit)) {
    stop_detqc("No animal in the animals table is of species %s.", paste(species, collapse = ", "))
  }
  ani$animal_id[hit]
}
