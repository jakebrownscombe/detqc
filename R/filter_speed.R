# ---- The speed filter ----

#' Remove detections that imply an impossible speed
#'
#' Orders each animal's detections in time and computes the straight-line
#' speed between consecutive detections from the positions of the receivers'
#' deployments. A step is a violation when the receivers are more than
#' `min_dist_m` apart and the speed exceeds `max_speed_ms`, or when two
#' receivers that far apart log the animal at the same moment; receivers
#' closer than `min_dist_m` can both hear one position, so steps between them
#' are never violations.
#'
#' `min_dist_m` is therefore the largest distance at which two receivers of
#' the array can hear one transmission, and it has no default: set too low,
#' the filter removes true detections of one transmission heard at two
#' receivers seconds apart. In a large lake array, reference tags can be
#' heard by stations more than 3 km apart within 5 s, and a `min_dist_m`
#' below that distance removes detections of one transmission heard at two
#' stations. `reference_tags` adds
#' what the reference tags show: two stations that heard one transmission of
#' a reference tag within the `codetect_s` of [check_settings()] (5 s) can
#' both hear one position, whatever their distance, so steps between them
#' are never violations.
#'
#' The filter works in passes. A detection whose steps in and out are both
#' violations is a spike and goes first. Spikes can sit next to each other:
#' in A B A B A, with a false B on each side of a true A, that A is a spike
#' too. In each run of adjacent spikes, only the spikes with the fewest
#' supporting detections go in a pass, and the next pass tests the rest
#' again. When no spike remains, each violating step loses its weaker
#' endpoint: the one with fewer detections of the same animal within
#' `support_s` seconds at receivers within `min_dist_m`. A tie removes the
#' later endpoint. The passes repeat until no violation remains, at most 100
#' times.
#'
#' Steps of zero time between distant receivers are violations. As the
#' endpoint with less support goes, one false first detection removes itself
#' and leaves the run of true detections after it.
#'
#' Detections with no `animal_id` take no part. When no detection of `x`
#' has an `animal_id`, the filter removes nothing and says so in a message.
#'
#' @param x A [detset()].
#' @param max_speed_ms Highest plausible speed, metres per second.
#' @param min_dist_m Receivers closer than this, in metres, are never a
#'   violation: the largest distance at which two receivers can hear one
#'   transmission.
#' @param support_s Half-width, in seconds, of the window that counts each
#'   endpoint's supporting detections. It decides which endpoint of a
#'   violation goes. Which steps are violations does not depend on it.
#' @param reference_tags Optional data frame with a `transmitter` column.
#'   Pairs of stations that heard one transmission of these tags are never a
#'   violation.
#' @return `x` with the detections kept and one log record (reasons `spike`
#'   and `weaker_endpoint`). Detections with no `animal_id` or no deployment
#'   position are kept and counted in the record, with the station pairs the
#'   reference tags cleared.
#' @family filters
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' dets <- filter_speed(dets, max_speed_ms = 3, min_dist_m = 2000)
#' writeLines(strwrap(detset_log(dets)$reason[2], width = 72))
filter_speed <- function(x, max_speed_ms, min_dist_m, support_s = 3600, reference_tags = NULL) {
  need_detset(x)
  need_number(max_speed_ms, "max_speed_ms")
  if (missing(min_dist_m)) {
    stop_detqc(paste("`min_dist_m` has no default: give the largest distance, in metres, at",
                     "which two receivers of the array hear one transmission."))
  }
  need_number(min_dist_m, "min_dist_m")
  need_number(support_s, "support_s")
  det <- x$detections
  w <- speed_rows(x)
  cleared <- reference_pairs(det, w, reference_tags)
  skipped <- sum(is.na(w$a) | is.na(w$lat))
  if (nrow(det) && all(is.na(w$a))) {
    message(paste("filter_speed(): no detection has an animal_id, so the speed filter tests",
                  "no step and removes nothing.", no_animal_advice(x)))
  }
  w <- w[!is.na(a) & !is.na(lat)]
  data.table::setorderv(w, c("a", "t", "i"))
  gone <- speed_passes(w, max_speed_ms, min_dist_m, support_s, cleared)
  keep <- !seq_len(nrow(det)) %in% gone$i
  reasons <- character(nrow(det))
  reasons[gone$i] <- gone$reason
  x <- apply_filter(x, keep, "filter_speed",
                    list(max_speed_ms = max_speed_ms, min_dist_m = min_dist_m,
                         support_s = support_s, reference_tags = !is.null(reference_tags)),
                    reasons)
  n <- nrow(x$log)
  x$log$reason[n] <- sprintf(paste(
    "%s; %s; %s with no animal or position kept; %s cleared by reference tags"
  ), x$log$reason[n], n_of(gone$passes, "pass", "passes"), n_of(skipped, "detection"),
  n_of(length(cleared), "station pair"))
  x
}

# One row per detection, with the position of the deployment that heard it.
speed_rows <- function(x) {
  det <- x$detections
  row <- as.vector(match_deployment(det, x$deployments))
  data.table::data.table(i = seq_len(nrow(det)), a = det$animal_id,
                         t = as.numeric(det$datetime),
                         lat = x$deployments$deploy_lat[row],
                         lon = x$deployments$deploy_lon[row],
                         st = x$deployments$station_id[row])
}

#' The steps the speed filter tests
#'
#' Returns one row per step between consecutive detections of one animal,
#' with the distance between the deployments that heard them and the speed
#' the step implies. These are the steps [filter_speed()] tests, positioned
#' in the same way: each detection takes the position of the deployment of
#' its receiver in the water at the time. Plot `speed_ms` against `dist_m`
#' with the two settings drawn on before you choose them.
#'
#' @param x A [detset()].
#' @param max_speed_ms,min_dist_m Optional settings of [filter_speed()].
#'   Given together, they add the column `violation`: the steps the filter
#'   would treat as violations in its first pass.
#' @return A data frame: `animal_id`, `from` and `datetime` (the times of
#'   the two detections, UTC), `from_station` and `station_id`, `dist_m`
#'   (great-circle distance, metres), `secs` and `speed_ms` (`Inf` for two
#'   stations apart at one moment; 0 within one position). Detections with
#'   no animal or no deployment position take no part, as in the filter.
#' @seealso [filter_speed()]
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' steps <- speed_steps(dets, max_speed_ms = 3, min_dist_m = 2000)
#' sum(steps$violation)
speed_steps <- function(x, max_speed_ms = NULL, min_dist_m = NULL) {
  need_detset(x)
  if (is.null(max_speed_ms) != is.null(min_dist_m)) {
    stop_detqc("Give both `max_speed_ms` and `min_dist_m`, or neither.")
  }
  w <- speed_rows(x)[!is.na(a) & !is.na(lat)]
  data.table::setorderv(w, c("a", "t", "i"))
  n <- nrow(w)
  k <- if (n > 1) which(w$a[-1] == w$a[-n]) + 1L else integer()
  out <- data.frame(
    animal_id = w$a[k], from = num_utc(w$t[k - 1L]), datetime = num_utc(w$t[k]),
    from_station = w$st[k - 1L], station_id = w$st[k],
    dist_m = 1000 * haversine_km(w$lat[k - 1L], w$lon[k - 1L], w$lat[k], w$lon[k]),
    secs = w$t[k] - w$t[k - 1L], stringsAsFactors = FALSE
  )
  out$speed_ms <- ifelse(out$dist_m == 0, 0, out$dist_m / out$secs)
  if (!is.null(max_speed_ms)) {
    need_number(max_speed_ms, "max_speed_ms")
    need_number(min_dist_m, "min_dist_m")
    out$violation <- speed_violations(w, max_speed_ms, min_dist_m)[k]
  }
  out
}

# Station pairs, as "a|b" with a before b, that heard one transmission of a
# reference tag within the co-detection window of check_settings().
reference_pairs <- function(det, w, reference_tags) {
  if (is.null(reference_tags)) return(character())
  if (!is.data.frame(reference_tags) || !"transmitter" %in% names(reference_tags)) {
    stop_detqc("`reference_tags` must be a data frame with a `transmitter` column.")
  }
  warn_absent_reference(reference_tags, det, "filter_speed")
  k <- which(det$transmitter %in% reference_tags$transmitter & !is.na(w$st))
  pr <- codetect_pairs(det$transmitter[k], w$t[k], det$receiver_serial[k],
                       check_defaults$codetect_s)
  a <- w$st[k[pr$a]]
  b <- w$st[k[pr$b]]
  unique(station_pair(a, b)[a != b])
}

station_pair <- function(a, b) paste(pmin(a, b), pmax(a, b), sep = "|")

# Reference tags that never occur in the detections clear no pair and test no
# receiver. A set cut to one species holds none of them. The step warns, so
# that the log does not record the reference tags as a setting that worked.
warn_absent_reference <- function(reference_tags, det, fn) {
  tags <- unique(stats::na.omit(as.character(reference_tags$transmitter)))
  present <- sum(tags %in% det$transmitter)
  if (length(tags) && present == 0) {
    one <- length(tags) == 1
    warn_detqc(paste("%s(): %s given; %s in the detections, so %s %s %s. Keep %s detections in",
                     "the set, such as with filter_animals(..., transmitters = ), to use %s."),
               fn, n_of(length(tags), "reference tag"),
               if (one) "it does not occur" else "none occurs", if (one) "it" else "they",
               if (fn == "filter_speed") verb(length(tags), "clears", "clear") else
                 verb(length(tags), "tests", "test"),
               if (fn == "filter_speed") "no station pair" else "no receiver",
               if (one) "its" else "their", if (one) "it" else "them")
  }
  invisible(present)
}

# Each pass marks violations on the animals still affected and removes
# spikes, else weaker endpoints. Returns the removed rows with reasons.
speed_passes <- function(w, max_speed, min_dist, support_s, cleared = character()) {
  gone <- list()
  passes <- 0L
  repeat {
    edge <- speed_violations(w, max_speed, min_dist, cleared)
    if (!any(edge) || passes >= 100L) break
    passes <- passes + 1L
    spike <- which(edge & c(edge[-1], FALSE))
    drop <- if (length(spike)) {
      data.table::data.table(i = w$i[lone_spikes(w, spike, min_dist, support_s)],
                             reason = "spike")
    } else {
      weaker_endpoints(w, which(edge), min_dist, support_s)
    }
    gone[[passes]] <- drop
    affected <- unique(w$a[edge])
    w <- w[!i %in% drop$i & a %in% affected]
  }
  if (any(speed_violations(w, max_speed, min_dist, cleared))) {
    warn_detqc("filter_speed() stopped after 100 passes with violations left.")
  }
  out <- data.table::rbindlist(gone)
  if (!nrow(out)) out <- data.table::data.table(i = integer(), reason = character())
  list(i = out$i, reason = out$reason, passes = passes)
}

# Spikes in adjacent rows, as in A B A B A where a false B sits on each
# side of a true A, make the true A a spike too. In each run of adjacent
# spikes only those with the least support go in this pass.
lone_spikes <- function(w, spike, min_dist, support_s) {
  run <- cumsum(c(1L, diff(spike) != 1L))
  if (!anyDuplicated(run)) return(spike)
  sup <- endpoint_support(w, min_dist, support_s)
  s <- vapply(spike, sup, numeric(1))
  keep <- s == stats::ave(s, run, FUN = min)
  spike[keep]
}

# edge[k] is TRUE when the step from row k-1 to row k of the same animal is
# a violation: far enough apart, outside the pairs the reference tags
# cleared, and too fast or at the same moment.
speed_violations <- function(w, max_speed, min_dist, cleared = character()) {
  n <- nrow(w)
  if (n < 2) return(rep(FALSE, n))
  same <- c(FALSE, w$a[-1] == w$a[-n])
  dist <- c(NA, 1000 * haversine_km(w$lat[-n], w$lon[-n], w$lat[-1], w$lon[-1]))
  dt <- c(NA, diff(w$t))
  edge <- same & dist > min_dist & (dt <= 0 | dist / dt > max_speed)
  edge[is.na(edge)] <- FALSE
  if (length(cleared) && any(edge)) {
    k <- which(edge)
    edge[k] <- !station_pair(w$st[k - 1L], w$st[k]) %in% cleared
  }
  edge
}

# For each violating step (row k-1 to row k), the endpoint with fewer
# supporting detections of its animal nearby in space and time.
weaker_endpoints <- function(w, k, min_dist, support_s) {
  support <- endpoint_support(w, min_dist, support_s)
  s_prev <- vapply(k - 1L, support, numeric(1))
  s_this <- vapply(k, support, numeric(1))
  pick <- ifelse(s_prev < s_this, k - 1L, k)
  data.table::data.table(i = w$i[unique(pick)], reason = "weaker_endpoint")
}

# A function of a row that counts the other detections of its animal within
# support_s seconds at receivers within min_dist metres. One sorted key over
# animal and time lets a binary search find the rows without a scan.
endpoint_support <- function(w, min_dist, support_s) {
  runs <- rle(w$a)
  key <- rep(seq_along(runs$lengths), runs$lengths) * 1e10 + (w$t - min(w$t))
  function(j) {
    lo <- findInterval(key[j] - support_s, key, left.open = TRUE) + 1L
    rows <- lo:findInterval(key[j] + support_s, key)
    sum(1000 * haversine_km(w$lat[rows], w$lon[rows], w$lat[j], w$lon[j]) <= min_dist) - 1L
  }
}
