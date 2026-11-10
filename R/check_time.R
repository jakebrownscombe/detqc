# ---- Time checks, and the two explanations of orphan detections ----

# A year off: a deployment that starts long before its first detection or
# ends long after its last, within the time the detections cover, or one
# whose window shifted by a year would hold the receiver's orphan detections.
# A shift explains orphans only when they reach more than half a year past
# the recorded window, so a stray detection a few days out stays with
# outside_deployment.
year_off_deployments <- function(det, d, orph, near_rows, settings) {
  gap <- settings$year_off_days * 86400
  span <- det[!is.na(.dep), list(first = min(datetime), last = max(datetime)), by = ".dep"]
  g <- merge(d[, list(.dep, deploy_datetime, recover_datetime)], span, by = ".dep")
  # Gaps are measured inside the time the detections cover, so a set cut to
  # one month does not make every deployment a year off.
  data_span <- range(as.numeric(det$datetime))
  g[, lead := as.numeric(first) - pmax(as.numeric(deploy_datetime), data_span[1])]
  g[, tail := pmin(as.numeric(recover_datetime), data_span[2]) - as.numeric(last)]
  gaps <- rbind(
    g[lead >= gap, list(.dep, problem = "starts a year before its first detection",
                        days = lead / 86400)],
    g[!is.na(tail) & tail >= gap, list(.dep, problem = "ends a year after its last detection",
                                       days = tail / 86400)]
  )
  far <- orph[!.row %in% near_rows]
  shifts <- year_shifts(d, far)
  rows <- rbind(gaps, shifts$rows)
  rows <- rows[!duplicated(rows$.dep)]
  list(rows = rows, windows = shifts$windows)
}

year_shifts <- function(d, far) {
  empty <- list(rows = data.table::data.table(.dep = integer(), problem = character(),
                                              days = numeric()),
                windows = NULL)
  if (!nrow(far)) return(empty)
  yr <- 365.25 * 86400
  far_end <- as.POSIXct("9999-12-31", tz = "UTC")
  s <- as.numeric(d$deploy_datetime)
  e <- as.numeric(data.table::fifelse(is.na(d$recover_datetime), far_end, d$recover_datetime))
  one <- function(start, end, way) {
    data.table::data.table(.dep = d$.dep, .key = d$.key, start = start, end = end, way = way)
  }
  cands <- data.table::rbindlist(list(
    one(s - yr, e, "early start"), one(s, e + yr, "late end"),
    one(s - yr, e - yr, "both a year late"), one(s + yr, e + yr, "both a year early")
  ))
  cands <- cands[start < end]
  q <- data.table::data.table(.key = far$.key, t = as.numeric(far$datetime))
  hits <- cands[q, on = list(.key, start <= t, end >= t), nomatch = NULL, allow.cartesian = TRUE,
                list(.dep, way, t = i.t)]
  # No shifted window holds an orphan: no shift explains them, and they stay
  # with outside_deployment. max() below would warn on no rows.
  if (!nrow(hits)) return(empty)
  # How far the farthest orphan lies outside the recorded window, in years.
  hits[, out := pmax(s[match(.dep, d$.dep)] - t, t - e[match(.dep, d$.dep)], 0)]
  per <- hits[, list(n = .N, reach = max(out) / yr), by = c(".dep", "way")]
  per[, .key := d$.key[match(.dep, d$.dep)]]
  per[, n_orphans := q[, .N, by = ".key"][match(per$.key, .key), N]]
  per <- per[n >= 0.9 * n_orphans & reach > 0.5]
  if (!nrow(per)) return(empty)
  # Of the shifts that hold the orphans, the narrowest window is the reading
  # that changes least: both dates a year late, rather than a start two years
  # wide.
  per[, len := cands$end[match(paste(.dep, way), paste(cands$.dep, cands$way))] -
        cands$start[match(paste(.dep, way), paste(cands$.dep, cands$way))]]
  per <- per[order(.dep, len)][!duplicated(.dep)]
  w <- cands[per, on = c(".dep", "way")]
  list(rows = per[, list(.dep, problem = paste("a one-year shift,", way), days = 365.25)],
       windows = w[, list(.key, start = num_utc(start),
                          end = num_utc(pmin(end, 253402214400)))])
}

# A whole-hour offset: orphans within `tz_window_h` outside a deployment's
# ends, reaching at least 45 minutes out, at two or more deployments and at
# no fewer than half of the deployments that were detecting near their ends.
# Reach alone cannot tell an offset from a receiver that logs on deck after
# recovery or before deployment. An offset also empties the other end: a
# clock an hour late leaves no detection in the first hour after deployment,
# at every deployment. So the offset is confirmed only where the deployments
# detecting near that other end leave it empty for at least 0.9 of the
# median reach; more than one in ten that do not is a contradiction, and the
# orphans stay with outside_deployment. When the offset is found it applies
# to every deployment, so the orphans near every end are explained, those
# that reach 45 minutes and the nearer ones alike.
tz_offset_deployments <- function(det, d, orph, settings) {
  w <- settings$tz_window_h * 3600
  ends <- rbind(
    d[!is.na(recover_datetime), list(.dep, .key, start = recover_datetime,
                                     end = recover_datetime + w, side = "after recovery")],
    d[, list(.dep, .key, start = deploy_datetime - w, end = deploy_datetime,
             side = "before deployment")]
  )
  q <- data.table::data.table(.key = orph$.key, t = orph$datetime, .row = orph$.row)
  hits <- ends[q, on = list(.key, start <= t, end >= t), nomatch = NULL, allow.cartesian = TRUE,
               list(.dep, side, .row, t = as.numeric(i.t), edge = data.table::fifelse(
                 side == "after recovery", as.numeric(x.start), as.numeric(x.end)
               ))]
  hits[, lag_h := abs(t - edge) / 3600]
  per <- if (nrow(hits)) {
    hits[, list(orphans = .N, offset_h = max(lag_h)), by = c(".dep", "side")]
  } else {
    data.table::data.table(.dep = integer(), side = character(), orphans = integer(),
                           offset_h = numeric())
  }
  active <- active_near_ends(det, d, w)
  other <- offset_other_end(per[offset_h >= 0.75], det, d, w)
  counted <- other$rows
  n_dep <- length(unique(counted$.dep))
  of <- length(unique(c(active, counted$.dep)))
  flag <- n_dep >= 2 && n_dep >= 0.5 * of && other$contradict <= 0.1 * other$measured
  list(flag = flag, rows = counted, of = of, near_rows = unique(hits$.row),
       contradict = other$contradict, measured = other$measured,
       windows = if (flag) ends[, list(.key, start, end)] else NULL)
}

# The reaching deployments on the side most of them share, and how many
# deployments detecting near the other end contradict an offset of the
# median reach by detecting inside it.
offset_other_end <- function(reach, det, d, w) {
  if (!nrow(reach)) return(list(rows = reach, contradict = 0L, measured = 0L))
  side <- names(which.max(table(reach$side)))
  reach <- reach[reach$side == side]
  delta <- 0.9 * 3600 * stats::median(reach$offset_h)
  m <- det[!is.na(.dep), list(first = min(as.numeric(datetime)), last = max(as.numeric(datetime))),
           by = ".dep"]
  k <- match(m$.dep, d$.dep)
  gap <- if (side == "after recovery") {
    m$first - as.numeric(d$deploy_datetime[k])
  } else {
    as.numeric(d$recover_datetime[k]) - m$last
  }
  near <- !is.na(gap) & gap <= w
  list(rows = reach, contradict = sum(near & gap < delta), measured = sum(near))
}

# Deployments with a detection within `w` seconds inside either end.
active_near_ends <- function(det, d, w) {
  m <- det[!is.na(.dep), list(.dep, t = as.numeric(datetime))]
  m[, c("s", "e") := list(as.numeric(d$deploy_datetime)[match(.dep, d$.dep)],
                          as.numeric(d$recover_datetime)[match(.dep, d$.dep)])]
  unique(m[t <= s + w | (!is.na(e) & t >= e - w), .dep])
}

chk_non_utc <- function(ctx) {
  tz <- ctx$orphans$tz
  zones <- ctx$tz[nzchar(ctx$tz)]
  note <- sprintf(paste(
    "detections up to %s h outside the ends of %s of %s active near them;",
    "%s of %s detecting near the other end %s inside the offset"
  ), if (nrow(tz$rows)) format(round(max(tz$rows$offset_h), 1)) else "0",
  fmt_n(length(unique(tz$rows$.dep))), n_of(tz$of, "deployment"), fmt_n(tz$contradict),
  n_of(tz$measured, "deployment"), verb(tz$contradict, "detects", "detect"))
  if (length(zones)) {
    note <- paste0(note, sprintf("; time zones converted to UTC: %s",
                                 paste(names(zones), zones, sep = " ", collapse = ", ")))
  }
  idx <- if (tz$flag) sort(unique(tz$rows$.dep)) else integer()
  r <- tz$rows[match(idx, tz$rows$.dep)]
  rows <- dep_rows(ctx, idx, side = r$side, orphans = r$orphans, offset_h = r$offset_h)
  check_result(length(idx), tz$of, note, rows)
}

# A fall-back step shows only as a step back in the order the receiver
# logged its rows. A receiver whose rows step back by other amounts is not
# in that order (rows grouped by animal, or shuffled), and one with no step
# back at all may be clean or sorted by time, which hides a jump; neither
# can pass. The check therefore flags or does not run.
chk_dst_jump <- function(ctx) {
  s <- ctx$settings
  d <- ctx$det[, list(.row, receiver_serial, t = as.numeric(datetime))]
  data.table::setorderv(d, ".row")
  d[, step_min := (t - data.table::shift(t)) / 60, by = "receiver_serial"]
  back <- d[!is.na(step_min) & step_min < 0]
  back[, dst := step_min <= -s$dst_min & step_min >= -s$dst_max]
  per <- back[, list(dst = sum(dst), other = sum(!dst)), by = "receiver_serial"]
  n_rx <- length(unique(d$receiver_serial))
  shuffled <- per$receiver_serial[per$other > 0]
  jumped <- per$receiver_serial[per$other == 0]
  why <- sprintf(paste(
    "%s of %s %s rows in time order, which a table sorted by time also gives, and",
    "%s %s steps back that no clock jump explains, so their rows are not in the order",
    "logged; a jump forward, and a jump on a receiver silent for %g minutes before it, are",
    "never visible"
  ), fmt_n(n_rx - nrow(per)), n_of(n_rx, "receiver"), verb(n_rx - nrow(per), "has", "have"),
  fmt_n(length(shuffled)), verb(length(shuffled), "has", "have"), s$dst_min)
  if (!length(jumped)) return(not_run(why))
  hit <- back[receiver_serial %in% jumped]
  check_result(length(jumped), n_rx - length(shuffled), sprintf(
    "%s back of %g to %g minutes in the order the rows arrived; %s",
    n_of(nrow(hit), "step"), s$dst_min, s$dst_max, why
  ), det_rows(ctx, hit$.row, step_min = hit$step_min))
}

chk_outside_deployment <- function(ctx) {
  o <- ctx$orphans
  idx <- setdiff(o$rows, o$explained)
  of <- sum(ctx$det$.serial)
  n_more <- length(o$rows) - length(idx)
  note <- sprintf("%s more %s explained by a year off, a time-zone offset or a bad recovery date",
                  fmt_n(n_more), verb(n_more, "is", "are"))
  check_result(length(idx), of, note, det_rows(ctx, sort(idx)))
}

# The same rule as filter_duplicates(), with the tolerance of the settings.
chk_duplicate_rows <- function(ctx) {
  tol <- ctx$settings$duplicate_tol_s
  by <- c("datetime", "receiver_serial", "transmitter")
  idx <- which(duplicate_rows(ctx$det, by, tol))
  note <- "copies of a row with the same time, receiver and transmitter"
  if (tol > 0) note <- sprintf("%s, within %g s", note, tol)
  check_result(length(idx), nrow(ctx$det), note, det_rows(ctx, idx))
}
