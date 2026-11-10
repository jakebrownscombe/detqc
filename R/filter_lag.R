# ---- The short-interval (min-lag) filter ----

#' Remove detections isolated in time
#'
#' A false detection, from two transmissions that collide, is rarely
#' followed by the same code on the same receiver soon after. For each
#' detection the minimum lag is the shorter of two times: back to the
#' previous detection and forward to the next, both of its own transmitter on
#' its own receiver. A detection with no other of its transmitter on its
#' receiver has none. The
#' filter keeps detections whose minimum lag lies between `min_lag_s` and
#' `max_lag_s`, inclusive. With `min_lag_s = 0` it keeps the same rows as
#' `false_detections(det, tf = max_lag_s)` in the glatos package when glatos
#' computes the lags itself. A GLATOS export carries a `min_lag` column
#' computed on the server, which `false_detections()` uses when present; it
#' can differ from the lags of the rows in hand (1 of 5,923 lamprey rows in
#' the glatos example), so drop that column before comparing.
#'
#' Pincock (2012) suggests a short interval of 30 times the average delay of
#' the tags as `max_lag_s`; detqc takes the average delay as the mean of
#' each tag's minimum and maximum delay.
#' The threshold is a choice the analysis must state. [minimum_lags()] returns
#' the lags this filter tests, so their distribution can inform it.
#'
#' Pincock's test has two conditions: at least one short interval, and more
#' short intervals than long ones. By default this filter applies only the
#' first, as glatos does. `long_lag_s` adds the second; Pincock suggests 720
#' times the average delay, 24 times `max_lag_s` when that is 30 times the
#' delay. The note does not say over which detections or which period the
#' intervals are counted. Here they are counted for each transmitter on each
#' receiver, over all of its detections in the set when the filter runs: an
#' interval is the time between two consecutive detections of the pair, it
#' is short from `min_lag_s` to `max_lag_s` inclusive, and long above
#' `long_lag_s`. A pair with no more short intervals than long ones fails,
#' and every detection of that transmitter on that receiver is removed, with
#' the reason `few_short_intervals` where the first condition kept it. The
#' test catches a code heard now and then over a long record, as a false
#' code that repeats or a tag at the edge of a receiver's range for weeks,
#' when it is heard alone between its close pairs. Where every gap between
#' visits is long, n close pairs give n short intervals and n - 1 long ones,
#' so a code heard only in close pairs passes however rare. With
#' `max_lag_s = 3600` and `long_lag_s = 86400`, four close pairs, 60 s
#' apart within a pair and 3 days apart between pairs, give 4 short
#' intervals against 3 long, and all 8 detections stay. Each lone detection
#' between the pairs adds a long interval when both of its gaps exceed
#' `long_lag_s`: one lone detection midway between two pairs, 1.5 days from
#' each, makes a tie, 4 against 4, and all 9 detections go. The test does
#' not look across receivers, so it cannot tell a real visit of a few
#' detections from a false one. It also does not read the receiver's own
#' statistics, which Pincock uses for a second scan of the detections the
#' first scan rejects.
#'
#' @references Pincock, D. G. (2012). False detections: what they are and
#'   how to remove them from detection data. Application note
#'   DOC-004691-03, Amirix Systems Inc. (VEMCO).
#'
#' @param x A [detset()].
#' @param max_lag_s Longest minimum lag kept, in seconds.
#' @param min_lag_s Shortest minimum lag kept, in seconds. Above 0 it also
#'   removes detections closer together than any tag transmits.
#' @param long_lag_s Optional interval, in seconds, above which an interval
#'   is long. `NULL`, the default, leaves the second condition off. It must
#'   exceed `max_lag_s`.
#' @return `x` with the detections kept and one log record, with the reasons
#'   `isolated` (minimum lag above `max_lag_s`, or none), `too_close` and,
#'   with `long_lag_s`, `few_short_intervals`; the record then also counts
#'   the transmitter and receiver pairs tested and those that failed.
#' @family filters
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' dets <- filter_min_lag(dets, max_lag_s = 3600)
#' writeLines(strwrap(detset_log(dets)$reason[2], width = 72))
#' # Add Pincock's second condition: more short intervals than long ones
#' both <- filter_min_lag(dets, max_lag_s = 3600, long_lag_s = 24 * 3600)
#' writeLines(strwrap(detset_log(both)$reason[3], width = 72))
filter_min_lag <- function(x, max_lag_s, min_lag_s = 0, long_lag_s = NULL) {
  need_detset(x)
  need_number(max_lag_s, "max_lag_s")
  need_number(min_lag_s, "min_lag_s")
  ml <- nearest_lag(x$detections)
  isolated <- is.na(ml) | ml > max_lag_s
  close <- !isolated & ml < min_lag_s
  reasons <- ifelse(isolated, "isolated", "too_close")
  keep <- !(isolated | close)
  parameters <- list(max_lag_s = max_lag_s, min_lag_s = min_lag_s)
  if (is.null(long_lag_s)) return(apply_filter(x, keep, "filter_min_lag", parameters, reasons))
  need_number(long_lag_s, "long_lag_s")
  if (long_lag_s <= max_lag_s) {
    stop_detqc("`long_lag_s` (%s) must exceed `max_lag_s` (%s).", format(long_lag_s),
               format(max_lag_s))
  }
  ratio <- interval_ratio(x$detections, min_lag_s, max_lag_s, long_lag_s)
  reasons[keep & ratio$fail] <- "few_short_intervals"
  extra <- sprintf("; %s of %s had no more short intervals than long",
                   fmt_n(ratio$n_fail), n_of(ratio$n_pairs, "transmitter and receiver pair"))
  apply_filter(x, keep & !ratio$fail, "filter_min_lag",
               c(parameters, list(long_lag_s = long_lag_s)), reasons, extra)
}

# Pincock's second condition, per transmitter on a receiver: the intervals
# between consecutive detections of the pair, short from min_s to max_s
# inclusive and long above long_s. A pair fails when its short intervals do
# not outnumber its long ones; a pair of one detection has none of either
# and fails. Returns, per detection in the input order, whether its pair
# failed, with the counts of pairs.
interval_ratio <- function(det, min_s, max_s, long_s) {
  n <- nrow(det)
  if (!n) return(list(fail = logical(), n_pairs = 0L, n_fail = 0L))
  d <- data.table::data.table(tx = det$transmitter, rx = det$receiver_serial,
                              t = as.numeric(det$datetime), i = seq_len(n))
  data.table::setorderv(d, c("tx", "rx", "t"))
  d[, gap := c(NA_real_, diff(t)), by = c("tx", "rx")]
  pairs <- d[, list(short = sum(gap >= min_s & gap <= max_s, na.rm = TRUE),
                    long = sum(gap > long_s, na.rm = TRUE)), by = c("tx", "rx")]
  pairs[, fail := short <= long]
  d <- pairs[d, on = c("tx", "rx")]
  fail <- logical(n)
  fail[d$i] <- d$fail
  list(fail = fail, n_pairs = nrow(pairs), n_fail = sum(pairs$fail))
}

utils::globalVariables(c("gap", "fail", "short", "long", "tx", "rx", "t"))

#' The minimum lag of every detection
#'
#' Returns, for each detection, the time to the nearer of its two neighbours
#' in time among the detections of its transmitter on its receiver: the
#' quantity [filter_min_lag()] tests, from the same computation. Use it to
#' look at the distribution of lags before choosing `max_lag_s`. The name
#' differs from `glatos::min_lag()`, which adds a column to a glatos
#' detection table, so both packages can be attached.
#'
#' @param x A [detset()], or a data frame of detections with `transmitter`,
#'   `receiver_serial` and `datetime`.
#' @return A numeric vector of minimum lags in seconds, one per detection in
#'   the order of the rows; `NA` for a detection that is the only one of its
#'   transmitter on its receiver.
#' @seealso [filter_min_lag()]
#' @family filters
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' lags <- minimum_lags(dets)
#' summary(lags)
minimum_lags <- function(x) {
  det <- if (inherits(x, "detset")) x$detections else x
  if (!is.data.frame(det)) {
    stop_detqc("`x` must be a detset or a data frame of detections.")
  }
  need_columns(det, "detections", c("transmitter", "receiver_serial", "datetime"))
  nearest_lag(det)
}

# Minimum lag in seconds per detection, in the input order; NA for a
# detection alone on its receiver. Sorting once and comparing neighbours is
# faster than a grouped pass when most groups are small.
nearest_lag <- function(det) {
  n <- nrow(det)
  if (!n) return(numeric())
  d <- data.table::data.table(tx = det$transmitter, rx = det$receiver_serial,
                              t = as.numeric(det$datetime), i = seq_len(n))
  data.table::setorderv(d, c("tx", "rx", "t"))
  same <- c(FALSE, d$tx[-1] == d$tx[-n] & d$rx[-1] == d$rx[-n])
  same[is.na(same)] <- FALSE
  gap <- c(NA, diff(d$t))
  prev <- data.table::fifelse(same, gap, NA_real_)
  nxt <- c(prev[-1], NA)
  ml <- pmin(prev, nxt, na.rm = TRUE)
  out <- numeric(n)
  out[d$i] <- ml
  out
}
