# ---- Mortality rule streams ----

#' The rule streams of the mortality triage
#'
#' Returns the default rule streams as a list that [detect_mortality()] runs
#' in order. Each stream is a small function of the detection set that
#' returns, per animal, a vote, a score, a candidate date and the rows behind
#' them. The streams sort animals for a person to review; they never make
#' the call. Add a stream with [mortality_rule()], drop one with `drop`, or
#' change a setting by name. Each setting is given with its default.
#'
#' * `accel_flatline` (votes dead or alive). A day is flat when the 95th
#'   percentile of its calibrated acceleration is at or below `accel_flat`
#'   (0.10 m/s2). The stream votes dead when at least `min_flat_days` (14)
#'   observed days are flat from the last active day to the end of the sensor
#'   record, and alive when a day within `active_within_days` (30) of the
#'   last detection is active. A day needs `min_readings` (1) readings, and
#'   an active spell `min_active_days` (1) days. Raw readings are converted
#'   with the transmitter's `sensor_slope` and `sensor_intercept`; without
#'   them the stream cannot vote. The candidate date is the day after the
#'   last active day.
#' * `terminal_run` (votes dead, or review). Stations are clustered into
#'   sites (see `control` in [detect_mortality()]). The final window holds
#'   the last `min_days` (14) detection days and at least the last
#'   `min_calendar_days` (14) calendar days. The stream votes dead when every
#'   detection in the window falls in one site and the bounding-box diagonal
#'   of its stations is at most the site size. It votes review when the
#'   window spans more than one site but its stations lie within the site
#'   size of each other; when one station heard it all and the station's
#'   recorded position moved; when its stations lie within `near_factor` (2)
#'   times the site size; or when every window day but at most `stray_days`
#'   (2) is at one site. The candidate date is the first detection of the
#'   final run, the stray days aside.
#' * `depth_flatline` (evidence only). A day is flat when its depth range is
#'   at most `depth_flat` (0.3 m), or `depth_flat_share` (0.1) of the day's
#'   median depth when that is smaller. It votes dead after `min_flat_days`
#'   (14) flat days to the end of the sensor record, and never decides a
#'   band. A day needs `min_readings` (1) readings.
#'
#' **Sensor resolution.** A sensor reports whole counts, so a dead tag whose
#' reading moves one count either side of its resting value spans two counts
#' in a day. Both flatline streams therefore never set a flat limit below two
#' counts of the sensor: the limit is the larger of the limit above and two
#' counts. A count is the transmitter's `sensor_slope` in the animals table,
#' on the calibrated scale; the example's depth tags have 0.1504 m per count,
#' so two counts are 0.3008 m and a day of one-count noise reads flat. Where
#' the readings arrive calibrated with no slope, a count is the smallest gap
#' between two distinct calibrated values of that transmitter; readings that
#' are not whole counts give a small gap, which leaves the limit as set. At a
#' resolution as coarse as the example's, two counts exceed the shallow-water
#' share of the median depth, so the share narrows no limit below 0.3008 m.
#' Values are compared with a floating-point tolerance.
#' * `record_end` (censors). Votes review when the last detection falls
#'   within `within_days` (30) of the expected end: the earliest of the tag's
#'   life from release, the data horizon and the end of the detection record
#'   (`control$horizon` in [detect_mortality()]). Its detail on the card
#'   names which end applied and why: a record heard to the end of the
#'   detection record is cut off there, its end unknown, so the rules
#'   cannot see what followed.
#' * `site_listening` (evidence; votes review when switched on). Counts the
#'   days, of the `within_days` (30) after the last detection, on which a
#'   receiver at the site of the last detection was listening. When
#'   `listening_share` (0) is above 0 and the site listened on a smaller
#'   share of those days, it votes review. At the default it is evidence for
#'   the cards only.
#' * `movement_resumption` (votes alive). After the earliest candidate death
#'   date of the streams before it, a transmitter heard on at least
#'   `min_days_away` (2) days at stations more than the site size from every
#'   station that heard it on its first `ref_days` (3) detection days from
#'   that date has moved, and the stream votes alive.
#' * `persistence` (ranks). The trailing-core Jaccard score. For each day,
#'   the core is the set of stations that logged `coverage` (0.95) of the
#'   animal's detections in the previous `window_days` (30); the score is the
#'   mean Jaccard similarity between cores `lag_days` (7) apart, from 0 to 1.
#'   A record needs `min_record_days` (45) detection days, windows need
#'   `min_window_days` (10), and the mean needs `min_pairs` (20) pairs. The
#'   score orders the review band. A score of at least `review_at` (0.95)
#'   sends an animal with no other evidence to review; `review_at = Inf`
#'   ranks only.
#'
#' **Sources.** The kinds of evidence the streams read are those Klinard
#' and Matley (2020, Table 1) found in use across acoustic telemetry
#' studies: a stop in detections, stationary horizontal space use,
#' stationary depth use and stationary acceleration. The streams themselves,
#' their thresholds and defaults, the site clustering, the candidate date and
#' the persistence score are this package's own rules. They follow no single
#' published method, which is why a person makes the call on the cards.
#'
#' @references Klinard, N. V., and Matley, J. K. (2020). Living until proven
#'   dead: addressing mortality in acoustic telemetry research. Reviews in
#'   Fish Biology and Fisheries 30, 485-499. \doi{10.1007/s11160-020-09613-z}
#'
#' @param ... Settings to change, by name, such as `min_days = 21` or
#'   `accel_flat = 0.05`. A name sets that setting in every stream that has
#'   it; a name no stream has is an error. The options `sites_km`,
#'   `site_distance` and `sparse_to_review` of [detect_mortality()]'s
#'   `control` may be given here too; they travel with the rules, and
#'   `control` overrides them.
#' @param drop Names of default streams to leave out.
#'
#' @return A named list of class `mortality_rules`, one `mortality_rule` per
#'   stream, in the order they run.
#' @seealso [mortality_rule()], [detect_mortality()]
#' @export
#' @examples
#' mortality_rules()
#' mortality_rules(min_days = 21, drop = "persistence")
mortality_rules <- function(..., drop = character()) {
  accel <- list(accel_flat = 0.10, min_flat_days = 14, active_within_days = 30,
                min_readings = 1, min_active_days = 1)
  terminal <- list(min_days = 14, min_calendar_days = 14, near_factor = 2, stray_days = 2)
  depth <- list(depth_flat = 0.3, depth_flat_share = 0.1, min_flat_days = 14, min_readings = 1)
  persist <- list(window_days = 30, lag_days = 7, coverage = 0.95, min_record_days = 45,
                  min_pairs = 20, min_window_days = 10, review_at = 0.95)
  rules <- list(
    accel_flatline = mortality_rule("accel_flatline", rule_accel_flatline, "vote", accel,
                                    "accelerometer"),
    terminal_run = mortality_rule("terminal_run", rule_terminal_run, "vote", terminal,
                                  "final run"),
    depth_flatline = mortality_rule("depth_flatline", rule_depth_flatline, "evidence", depth,
                                    "depth"),
    record_end = mortality_rule("record_end", rule_record_end, "censor", list(within_days = 30),
                                "record end"),
    site_listening = mortality_rule("site_listening", rule_site_listening, "vote",
                                    list(within_days = 30, listening_share = 0),
                                    "site listening"),
    movement_resumption = mortality_rule("movement_resumption", rule_movement, "vote",
                                         list(ref_days = 3, min_days_away = 2), "movement"),
    persistence = mortality_rule("persistence", rule_persistence, "rank", persist, "persistence")
  )
  if (!is.character(drop)) stop_detqc("`drop` must name streams.")
  bad <- setdiff(drop, names(rules))
  if (length(bad)) stop_detqc("There is no default stream `%s` to drop.", bad[1])
  rules <- rules[setdiff(names(rules), drop)]
  new <- list(...)
  if (length(new) && (is.null(names(new)) || any(!nzchar(names(new))))) {
    stop_detqc("Settings in `...` must be named, such as `min_days = 21`.")
  }
  opts <- intersect(names(new), c("sites_km", "site_distance", "sparse_to_review"))
  out <- set_rule_settings(structure(rules, class = "mortality_rules"),
                           new[setdiff(names(new), opts)])
  if (length(opts)) attr(out, "control") <- mortality_control(new[opts])[opts]
  out
}

# The lowest value each default setting can take; a setting of a user's
# stream is passed through unchecked.
setting_floor <- c(
  min_days = 1, min_calendar_days = 1, near_factor = 0, stray_days = 0, listening_share = 0,
  accel_flat = 0, min_flat_days = 1,
  active_within_days = 0, min_readings = 1, min_active_days = 1, depth_flat = 0,
  depth_flat_share = 0, within_days = 0, ref_days = 1, min_days_away = 1, window_days = 1,
  lag_days = 1, coverage = 0, min_record_days = 1, min_pairs = 1, min_window_days = 1,
  review_at = 0
)

set_rule_settings <- function(rules, new) {
  for (nm in names(new)) {
    hit <- vapply(rules, function(r) nm %in% names(r$settings), logical(1))
    if (!any(hit)) stop_detqc("No stream has a setting `%s`.", nm)
    if (nm %in% names(setting_floor)) check_number(new[[nm]], nm, setting_floor[[nm]])
    if (nm %in% c("coverage", "listening_share") && new[[nm]] > 1) {
      stop_detqc("`%s` must be at most 1.", nm)
    }
    for (k in which(hit)) rules[[k]]$settings[[nm]] <- new[[nm]]
  }
  rules
}

check_number <- function(v, name, lowest) {
  if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < lowest) {
    stop_detqc("`%s` must be one number of %g or more.", name, lowest)
  }
  invisible(TRUE)
}

#' Define a mortality rule stream
#'
#' Wraps a function so that [detect_mortality()] can run it beside the
#' default streams of [mortality_rules()]. Add the result to that list.
#'
#' `fun(data, settings)` receives `settings` and a list `data` built once per
#' run: `x`, the detection set; `detections`, the located detections of
#' animals, with `animal_id`, `transmitter`, `datetime`, `date`,
#' `station_id`, `lat`, `lon` and `site`; `records`, one row per animal with
#' `detections`, `n_days`, `first`, `last`, `located`, `tag_end`, `horizon`,
#' `expected_end` and `end_basis`; `horizon`, the data horizon, and
#' `record_end`, the last detection in `x`; `readings`, the sensor readings
#' with `class`, calibrated `value` and `step`, one count of the sensor on
#' the calibrated scale; `sites`; `sites_km`; and `votes`, the
#' votes of the streams that ran before it. The tables are data.table
#' objects.
#'
#' It returns a data frame with one row per animal it judges: `animal_id`,
#' `vote` (one of "dead", "alive", "review", "none" or "unavailable"), and
#' optionally `score`, `date` (POSIXct in UTC, the candidate death date),
#' `basis` (what the date is) and `detail` (why). It may instead return a
#' list of that data frame as `votes` and the rows behind them as
#' `evidence`. Animals it leaves out get "unavailable".
#'
#' @param name Name of the stream, used in the votes and on the cards.
#' @param fun The function, as above.
#' @param kind How the bands read the votes: `"vote"` (a dead vote is death
#'   evidence and an alive vote is alive evidence), `"censor"` (a review vote
#'   says the record ends where the rules cannot see), `"evidence"` (shown on
#'   the cards, never decides) or `"rank"` (the score orders the review
#'   band).
#' @param settings A named list passed to `fun`.
#' @param label Short label for the cards.
#'
#' @return An object of class `mortality_rule`.
#' @seealso [mortality_rules()]
#' @export
#' @examples
#' # A stream that sends every animal heard on fewer than 3 days to review.
#' few_days <- mortality_rule("few_days", function(data, settings) {
#'   records <- data$records
#'   data.frame(animal_id = records$animal_id,
#'              vote = ifelse(records$n_days < settings$days, "review", "none"))
#' }, settings = list(days = 3))
#' rules <- mortality_rules()
#' rules$few_days <- few_days
#' study <- example_mortality
#' dets <- detset(study$detections, study$deployments, study$animals)
#' table(detect_mortality(dets, rules)$bands$deciding_rule)
mortality_rule <- function(name, fun, kind = c("vote", "censor", "evidence", "rank"),
                           settings = list(), label = name) {
  if (!is.character(name) || length(name) != 1 || !grepl("^[a-z][a-z0-9_]*$", name)) {
    stop_detqc("`name` must be one lower-case name, such as \"few_days\".")
  }
  if (!is.function(fun)) stop_detqc("`fun` must be a function of (data, settings).")
  kind <- check_choice(kind, c("vote", "censor", "evidence", "rank"), "kind")
  if (!is.list(settings)) stop_detqc("`settings` must be a list.")
  structure(list(name = name, fun = fun, kind = kind, settings = settings,
                 label = as.character(label)), class = "mortality_rule")
}

check_rule_list <- function(rules) {
  is_rule <- vapply(rules, inherits, logical(1), "mortality_rule")
  if (!is.list(rules) || !length(rules) || !all(is_rule)) {
    stop_detqc("`rules` must be a list of streams from mortality_rules() and mortality_rule().")
  }
  nm <- vapply(rules, function(r) r$name, "")
  if (is.null(names(rules)) || !identical(unname(nm), names(rules))) {
    stop_detqc("Each stream in `rules` must sit under its own name, such as rules$%s.", nm[1])
  }
  if (anyDuplicated(nm)) {
    stop_detqc("Two streams in `rules` share the name `%s`.", nm[anyDuplicated(nm)])
  }
  invisible(TRUE)
}

rule_table <- function(rules) {
  data.frame(
    rule = names(rules), kind = vapply(rules, function(r) r$kind, ""),
    label = vapply(rules, function(r) r$label, ""),
    settings = vapply(rules, function(r) deparse_parameters(r$settings), ""),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

rule_setting <- function(rules, rule, setting) {
  v <- rules[[rule]]$settings[[setting]]
  if (is.null(v)) NA_real_ else v
}

#' @export
print.mortality_rules <- function(x, ...) {
  cat(sprintf("<mortality_rules> %s, run in this order:\n", n_of(length(x), "stream")))
  for (r in x) print(r)
  invisible(x)
}

#' @export
print.mortality_rule <- function(x, ...) {
  # The name and kind keep their columns; the settings wrap beside them.
  lead <- sprintf("  %-20s %-8s ", x$name, x$kind)
  sets <- strwrap(deparse_parameters(x$settings),
                  width = max(20, getOption("width") - nchar(lead)))
  cat(paste0(c(lead, rep(strrep(" ", nchar(lead)), length(sets) - 1)), sets), sep = "\n")
  invisible(x)
}

# ---- Stream: terminal stationary run ----
rule_terminal_run <- function(data, settings) {
  det <- data$detections
  if (!nrow(det)) return(list(votes = data.frame(animal_id = character(), vote = character())))
  s <- det[, terminal_one(.SD, settings, data$sites_km, data$site_distance), by = "animal_id",
           .SDcols = c("datetime", "date", "site", "lat", "lon", "station_id")]
  days <- det[s[, list(animal_id, window_start)], on = "animal_id"][date >= window_start]
  days <- days[, list(detections = .N, stations = length(unique(station_id)),
                      sites = length(unique(site))), by = list(animal_id, date, site)]
  s[, score := window_diag_km]
  s[, date := data.table::fifelse(vote %in% c("dead", "review"), run_start, empty_utc(1))]
  s <- listening_gaps(s, det, data$listening)
  s[, basis := data.table::fifelse(vote %in% c("dead", "review"),
                                   paste0("the first detection of the final run", gap_note),
                                   NA_character_)]
  # The gap note goes in the basis of the date only: the card joins basis and
  # detail in one cell, so the note appears once.
  s[, detail := reason]
  list(votes = s, evidence = days)
}

# A run that starts after its site stopped listening may have started
# earlier: the first detection only bounds the death from above. The
# earliest bound is the animal's detection before the run.
listening_gaps <- function(s, det, lst) {
  s[, `:=`(gap_days = NA_integer_, earliest_date = empty_utc(.N), gap_note = "")]
  k <- which(s$vote %in% c("dead", "review") & !is.na(s$run_start) &
               s$terminal_site %in% lst$site)
  if (!length(k)) return(s)
  runs <- s[k, list(animal_id, run_start, terminal_site)]
  prev <- det[runs, on = list(animal_id, datetime < run_start), mult = "last", nomatch = NULL,
              list(animal_id, prev = x.datetime, run_start = i.run_start, site = i.terminal_site)]
  if (!nrow(prev)) return(s)
  prev[, gap := mapply(function(p, r, st) {
    if (as.Date(r) - as.Date(p) < 2) return(0L)
    days <- seq(as.Date(p) + 1, as.Date(r) - 1, by = "day")
    sum(!days %in% lst$date[lst$site == st])
  }, prev, run_start, site)]
  prev <- prev[gap > 0]
  s[prev, on = "animal_id", `:=`(gap_days = i.gap, earliest_date = i.prev)]
  fmt <- paste0(", after %s when its site was not listening; the death may be as early",
                " as the detection before the run, %s")
  s[!is.na(gap_days), gap_note := sprintf(fmt, n_of(gap_days, "day"),
                                          format(as.Date(earliest_date)))]
  s
}

terminal_one <- function(d, settings, sites_km, dist_fun = NULL) {
  days <- unique(d$date)
  nd <- length(days)
  site <- d$site
  away <- which(site != site[length(site)])
  first_run <- if (length(away)) max(away) + 1L else 1L
  base <- c(list(detection_days = nd, first_detection = d$datetime[1],
                 last_detection = d$datetime[nrow(d)], terminal_site = site[length(site)],
                 subtype = NA_character_),
            run_fields(d$datetime, d$date, first_run))
  min_days <- settings$min_days
  if (nd < min_days) {
    return(c(base, list(window_start = days[1], window_sites = NA_integer_,
                        window_diag_km = NA_real_, vote = "unavailable",
                        reason = sprintf("sparse: %s of the %s the run needs",
                                         n_of(nd, "detection day"), fmt_n(min_days)))))
  }
  w_start <- min(days[nd - min_days + 1L], days[nd] - (settings$min_calendar_days - 1))
  inw <- d$date >= w_start
  near <- if (is.null(settings$near_factor)) 0 else settings$near_factor
  w <- window_geometry(d[inw], sites_km, dist_fun, near)
  if (w$vote == "none") w <- window_strays(w, d[inw], settings$stray_days)
  if (w$vote %in% c("split", "near")) {
    reach <- if (w$vote == "near") w$near_km else sites_km
    base[names(run_fields(d$datetime, d$date, 1L))] <-
      run_fields(d$datetime, d$date, geometric_run(d$lat, d$lon, reach, dist_fun))
  }
  if (w$vote == "moved") {
    base[names(run_fields(d$datetime, d$date, 1L))] <-
      run_fields(d$datetime, d$date, station_run(d$station_id))
  }
  if (w$vote == "stray") {
    base$terminal_site <- w$home
    keep <- !(inw & d$site != w$home)
    base[names(run_fields(d$datetime, d$date, 1L))] <-
      run_fields(d$datetime[keep], d$date[keep], site_run(d$site[keep], w$home))
  }
  vote <- switch(w$vote, dead = "dead", split = "review", moved = "review", near = "review",
                 stray = "review", "none")
  base$subtype <- if (vote == "review") w$vote else NA_character_
  c(base, list(window_start = w_start, window_sites = w$n_sites, window_diag_km = w$diag,
               vote = vote, reason = terminal_reason(w, days[days >= w_start], base,
                                                     sites_km)))
}

# The terminal window's geometry and the vote it gives: dead at one site;
# split when the stations span sites but lie within sites_km of each other;
# moved when one station heard it all at positions further apart; near when
# the stations lie within near_factor times sites_km.
window_geometry <- function(w, sites_km, dist_fun, near_factor = 2) {
  n_sites <- length(unique(w$site))
  diag <- haversine_km(min(w$lat), min(w$lon), max(w$lat), max(w$lon))
  spread <- if (is.null(dist_fun)) diag else window_spread(w$lat, w$lon, dist_fun)
  vote <- if (n_sites == 1 && diag <= sites_km) {
    "dead"
  } else if (n_sites > 1 && spread <= sites_km) {
    "split"
  } else if (length(unique(w$station_id)) == 1 && !anyNA(w$station_id)) {
    "moved"
  } else if (spread <= near_factor * sites_km) {
    "near"
  } else {
    "none"
  }
  list(n_sites = n_sites, diag = diag, spread = spread, vote = vote,
       near_km = near_factor * sites_km)
}

# A terminal window at one site apart from detections elsewhere on at most
# `stray_days` days. Such a window goes to review, because a dead tag with a
# few stray detections and a resident with a rare trip look the same.
window_strays <- function(w, wd, stray_days) {
  if (is.null(stray_days) || stray_days < 1 || w$n_sites < 2) return(w)
  ds <- unique(data.frame(date = wd$date, site = wd$site))
  tab <- table(ds$site)
  top <- names(tab)[tab == max(tab)]
  last_site <- as.character(wd$site[length(wd$site)])
  home <- as.integer(if (last_site %in% top) last_site else top[1])
  off <- sort(unique(ds$date[ds$site != home]))
  if (length(off) > stray_days) return(w)
  w$vote <- "stray"
  w$home <- home
  w$stray_dates <- off
  w
}

# Index of the first detection of the final stretch at site `home`.
site_run <- function(site, home) {
  away <- which(site != home)
  if (length(away)) max(away) + 1L else 1L
}

run_fields <- function(datetime, date, first) {
  run_days <- unique(date[first:length(date)])
  list(run_start = datetime[first], run_detection_days = length(run_days),
       run_calendar_days = as.integer(max(run_days) - min(run_days)) + 1L)
}

# The candidate date is the first detection of the run, and the window is
# the record's last detection days, so the text names both with their dates.
# A window named alone, with dates days after the candidate date, reads as a
# contradiction. The window lies inside the run on a dead, split, near and
# moved vote, so "its last" days are the run's; a stray run sets days aside,
# and no vote has no run.
terminal_reason <- function(w, win_days, run, sites_km) {
  n <- length(win_days)
  days_txt <- sprintf("%s, %s to %s,", n_of(n, "detection day"), format(win_days[1]),
                      format(win_days[n]))
  run_day <- as.Date(run$run_start)
  run_txt <- sprintf("the final run, %s to %s,", format(run_day),
                     format(run_day + run$run_calendar_days - 1L))
  switch(w$vote,
    dead = sprintf("%s is at one site; its last %s span %.3f km", run_txt, days_txt, w$diag),
    split = sprintf(paste("%s is stationary across a site boundary: its last %s span %s,",
                          "but their stations lie within %.3f km"),
                    run_txt, days_txt, n_of(w$n_sites, "site"), w$spread),
    moved = sprintf(paste("%s is at one station whose recorded position moved %.3f km over its",
                          "last %s a moved mooring"), run_txt, w$diag, sub(",$", ":", days_txt)),
    near = sprintf(paste("%s is nearly stationary: its last %s span %s, but their",
                         "stations lie within %.3f km, inside %g km"),
                   run_txt, days_txt, n_of(w$n_sites, "site"), w$spread, w$near_km),
    stray = sprintf(paste("%s is at one site apart from %s heard at other sites (%s) in the",
                          "record's last %s stationary with stray detections"),
                    run_txt, n_of(length(w$stray_dates), "day"),
                    paste(format(w$stray_dates), collapse = ", "), sub(",$", ":", days_txt)),
    if (w$n_sites > 1) {
      sprintf("the record's last %s span %s; diagonal %.3f km", days_txt,
              n_of(w$n_sites, "site"), w$diag)
    } else {
      sprintf("the record's last %s are at one site, but their diagonal of %.3f km exceeds %g km",
              days_txt, w$diag, sites_km)
    }
  )
}

window_spread <- function(lat, lon, dist_fun) {
  u <- unique(data.frame(lat = lat, lon = lon))
  if (nrow(u) < 2) return(0)
  max(dist_fun(u$lat, u$lon))
}

# Index of the first detection of the longest final stretch whose stations
# lie within sites_km of each other. Walks back over station positions in
# the order the animal last used them.
geometric_run <- function(lat, lon, sites_km, dist_fun = NULL) {
  n <- length(lat)
  if (is.null(dist_fun)) {
    rl <- rev(lat)
    ro <- rev(lon)
    dg <- haversine_km(cummin(rl), cummin(ro), cummax(rl), cummax(ro))
    over <- which(dg > sites_km)
    return(if (length(over)) n - over[1] + 2L else 1L)
  }
  key <- paste(lat, lon)
  last <- tapply(seq_len(n), key, max)
  last <- sort(last, decreasing = TRUE)
  first <- match(names(last), key)
  dm <- dist_fun(lat[first], lon[first])
  for (k in seq_along(last)[-1]) {
    if (max(dm[seq_len(k), seq_len(k)]) > sites_km) return(unname(last[k]) + 1L)
  }
  1L
}

# Index of the first detection of the final stretch at one station.
station_run <- function(station) {
  away <- which(station != station[length(station)])
  if (length(away)) max(away) + 1L else 1L
}

# ---- Stream: accelerometer flatline ----
rule_accel_flatline <- function(data, settings) {
  s <- sensor_stream(data$readings, data$records, settings)
  v <- s$votes
  if (nrow(v)) {
    v[, score := as.numeric(flat_days)]
    v[, date := data.table::fifelse(vote == "dead", flat_onset, empty_utc(1))]
    v[, basis := data.table::fifelse(vote == "dead", "the day after the last active day",
                                     NA_character_)]
    v[, detail := paste0(transmitter, ": ", reason)]
  }
  list(votes = v, evidence = list(days = s$days, streams = s$streams))
}

# The last detection is read from every detection of the animal, located or
# not, so an animal with no station position still gets an alive vote.
sensor_stream <- function(r, records, settings) {
  last_det <- records[, list(animal_id, last_detection = last)]
  if (!nrow(r)) return(list(streams = empty_streams(), votes = empty_streams()[0],
                            days = data.table::data.table()))
  streams <- r[, list(animal_id = animal_id[1], readings = .N, raw_only = sum(scale == "raw"),
                      first_reading = min(datetime), last_reading = max(datetime)),
               by = list(transmitter, class)]
  acc <- r[class == "acceleration" & scale == "calibrated" & !is.na(animal_id)]
  days <- daily_stat(acc, function(v) stats::quantile(v, 0.95, names = FALSE),
                     settings$min_readings)
  days[, flat := at_most(stat, flat_limit(settings$accel_flat,
                                          transmitter_step(acc, transmitter)))]
  fl <- flatlines(days, settings$min_flat_days, settings$min_active_days)
  fl <- last_det[fl, on = "animal_id"]
  fl[, gap_days := as.numeric(difftime(last_detection, last_active, units = "days"))]
  fl[, active_recent := !is.na(last_active) & gap_days <= settings$active_within_days]
  fl[, vote := data.table::fcase(flatline, "dead", active_recent, "alive", default = "none")]
  fl_cols <- fl[, list(transmitter, flat_days, active_days, spikes_ignored, last_active,
                       flat_onset, first_flat_observed, vote)]
  streams <- fl_cols[streams, on = "transmitter"]
  streams[, reason := stream_reason(class, raw_only, readings, vote, flat_days,
                                    settings$min_flat_days)]
  streams[, reason := paste0(reason, onset_note(vote, last_active, first_flat_observed,
                                                spikes_ignored))]
  streams[is.na(vote), vote := "unavailable"]
  data.table::setcolorder(streams, c("animal_id", "transmitter", "class", "readings", "raw_only"))
  data.table::setorderv(streams, c("animal_id", "transmitter"))
  data.table::setnames(days, "stat", "q95")
  list(streams = streams, votes = pick_accel_vote(streams), days = days)
}

# Daily statistic of one sensor class, per animal and transmitter, on days
# with at least `min_readings` readings.
daily_stat <- function(r, fun, min_readings) {
  if (!nrow(r)) {
    return(data.table::data.table(animal_id = character(), transmitter = character(),
                                  date = as.Date(character()), readings = integer(),
                                  stat = numeric(), med = numeric(),
                                  first_reading = empty_utc()))
  }
  out <- r[, list(readings = .N, stat = fun(value), med = stats::median(value),
                  first_reading = min(datetime)), by = list(animal_id, transmitter, date)]
  out <- out[readings >= min_readings]
  data.table::setorderv(out, c("animal_id", "transmitter", "date"))
  out
}

# The step of each transmitter's sensor in `r`, one sensor class, matched to
# `tx`; NA where it is not known.
transmitter_step <- function(r, tx) {
  if (!"step" %in% names(r)) return(rep(NA_real_, length(tx)))
  steps <- r[, list(step = step[1]), by = "transmitter"]
  steps$step[match(tx, steps$transmitter)]
}

flatlines <- function(days, min_days, min_active_days) {
  if (!nrow(days)) {
    return(data.table::data.table(animal_id = character(), transmitter = character(),
                                  flat_days = integer(), active_days = integer(),
                                  spikes_ignored = integer(), last_active = as.Date(character()),
                                  flat_onset = empty_utc(), first_flat_observed = empty_utc(),
                                  flatline = logical()))
  }
  days[, flatline_one(date, flat, first_reading, min_days, min_active_days),
       by = list(animal_id, transmitter)]
}

empty_streams <- function() {
  data.table::data.table(
    animal_id = character(), transmitter = character(), class = character(),
    readings = integer(), raw_only = integer(), first_reading = empty_utc(),
    last_reading = empty_utc(), flat_days = integer(), active_days = integer(),
    spikes_ignored = integer(), last_active = as.Date(character()), flat_onset = empty_utc(),
    first_flat_observed = empty_utc(), vote = character(), reason = character()
  )
}

# The last active day ends the record of life, and the observed flat days
# after it are the terminal run. A sensor that was never active is flat over
# its whole record. The onset is the day after the last active day. The
# first flat day observed can be much later: a gap in readings can put it
# more than a year after the death. An active spell shorter than min_active_days with flat days
# after it is counted as a spike and does not end the run.
flatline_one <- function(date, flat, first_reading, min_days, min_active_days = 1L) {
  n <- length(flat)
  r <- rle(!flat)
  ends <- cumsum(r$lengths)
  counts <- r$values & (r$lengths >= min_active_days | ends == n)
  la <- if (any(counts)) date[ends[max(which(counts))]] else as.Date(NA)
  after <- if (is.na(la)) rep(TRUE, n) else date > la
  run <- sum(after & flat)
  first_flat <- if (run > 0) first_reading[which(after & flat)[1]] else empty_utc(1)
  onset <- empty_utc(1)
  if (run > 0) {
    onset <- if (is.na(la)) first_flat else as.POSIXct(format(la + 1), tz = "UTC")
  }
  list(flat_days = as.integer(run), active_days = sum(!flat),
       spikes_ignored = sum(after & !flat), last_active = la, flat_onset = onset,
       first_flat_observed = first_flat, flatline = run >= min_days)
}

onset_note <- function(vote, last_active, first_flat, spikes) {
  out <- character(length(vote))
  k <- !is.na(vote) & vote == "dead" & !is.na(last_active) & !is.na(first_flat)
  gap <- as.integer(as.Date(first_flat[k]) - last_active[k]) - 1L
  # The onset is printed first, where a reader looks for it, and the last
  # active day after it.
  out[k] <- sprintf(paste("; onset %s, the day after the last active day (%s);",
                          "first flat day observed %s, %s later"),
                    format(last_active[k] + 1), format(last_active[k]),
                    format(as.Date(first_flat[k])), n_of(gap, "day"))
  s <- !is.na(spikes) & spikes > 0
  out[s] <- paste0(out[s], sprintf("; %s after the last active day ignored",
                                   n_of(spikes[s], "short active spell")))
  out
}

stream_reason <- function(class, raw_only, readings, vote, flat_days, min_days) {
  out <- character(length(class))
  is_acc <- class == "acceleration"
  out[!is_acc & class != "unknown"] <- paste0(class[!is_acc & class != "unknown"],
                                              " sensor: evidence only, no vote")
  out[class == "unknown"] <- "sensor class unknown: give sensor_type or sensor_unit"
  raw <- is_acc & raw_only == readings
  out[raw] <- "accelerometer readings are raw: give sensor_slope and sensor_intercept"
  v <- is_acc & !raw & !is.na(vote)
  out[v] <- sprintf("%s after the last active day (%s needed); vote %s",
                    n_of(flat_days[v], "flat day"), fmt_n(min_days), vote[v])
  out[is_acc & !raw & is.na(vote)] <- "too few readings per day to judge"
  out
}

# An animal with two accelerometer transmitters is judged on the one with
# more calibrated readings.
pick_accel_vote <- function(streams) {
  v <- streams[class == "acceleration" & vote %in% c("dead", "alive", "none")]
  if (!nrow(v)) return(v)
  v <- v[order(animal_id, -(readings - raw_only))]
  unique(v, by = "animal_id")
}

# ---- Stream: depth flatline, evidence only ----
# An absolute range threshold makes a shallow fish read as dead, so the
# threshold shrinks with the day's median depth.
rule_depth_flatline <- function(data, settings) {
  r <- data$readings[class == "depth" & scale == "calibrated" & !is.na(animal_id)]
  days <- daily_stat(r, function(v) max(v) - min(v), settings$min_readings)
  days[, limit := flat_limit(pmin(settings$depth_flat, settings$depth_flat_share * pmax(med, 0)),
                             transmitter_step(r, transmitter))]
  days[, flat := at_most(stat, limit)]
  fl <- flatlines(days, settings$min_flat_days, 1L)
  if (!nrow(fl)) return(list(votes = data.frame(animal_id = character(), vote = character()),
                             evidence = days))
  fl <- fl[order(animal_id, -flat_days)]
  fl <- unique(fl, by = "animal_id")
  fl[, vote := data.table::fifelse(flatline, "dead", "none")]
  fl[, score := as.numeric(flat_days)]
  fl[, date := data.table::fifelse(flatline, flat_onset, empty_utc(1))]
  fl[, basis := data.table::fifelse(flatline, "the day after the last day of varied depth",
                                    NA_character_)]
  fl[, detail := sprintf("%s: %s to the end of the record (%s needed)",
                         transmitter, n_of(flat_days, "flat depth day"),
                         fmt_n(settings$min_flat_days))]
  data.table::setnames(days, "stat", "range_m")
  list(votes = fl, evidence = days)
}

# ---- Stream: record end at tag life or the data horizon ----
rule_record_end <- function(data, settings) {
  r <- data.table::copy(data$records)
  r[, gap_days := as.numeric(difftime(expected_end, last, units = "days"))]
  r[, vote := data.table::fifelse(gap_days <= settings$within_days, "review", "none")]
  r[, score := gap_days]
  r[, date := last]
  r[, basis := "the last detection"]
  # The text counts whole UTC days, as the card's header and the site
  # listening stream do; the fraction decides the vote only.
  r[, cal := as.integer(as.Date(expected_end) - as.Date(last))]
  r[, detail := sprintf("last detection %s, %s %s the %s (%s)%s", format(as.Date(last)),
                        n_of(abs(cal), "day"),
                        data.table::fifelse(gap_days >= 0, "before", "after"),
                        end_basis, format(as.Date(expected_end)),
                        end_reason(end_basis, vote, data))]
  list(votes = r[, list(animal_id, vote, score, date, basis, detail, expected_end, end_basis)])
}

# Why a review vote at the data's end is a censor: the data cut the record
# off, and the card states it. Empty for tag life and for no vote.
end_reason <- function(end_basis, vote, data) {
  hz <- format(as.Date(data$horizon))
  cut <- sprintf(paste0("; the detset holds no detection after it, though the data run to %s, so ",
                        "the record is cut there and the rules cannot see what ",
                        "followed"), hz)
  own <- if (isTRUE(data$horizon_given)) {
    "; the rules cannot see what followed the data horizon"
  } else {
    "; the data horizon is the last detection in the detset, so the rules cannot see what followed"
  }
  at_end <- vote == "review" & end_basis != "tag life"
  ifelse(!at_end, "", ifelse(end_basis == "end of the detection record", cut, own))
}

# ---- Stream: was the last site still listening ----
# A record that ends when the receivers
# at its last site come out of the water says nothing about the animal: a
# dead tag at a recovered station stops being heard as surely as a live
# animal that left. Such a record goes to review.
rule_site_listening <- function(data, settings) {
  det <- data$detections
  lst <- data$listening
  if (!nrow(det)) return(data.frame(animal_id = character(), vote = character()))
  last <- det[det[, .I[.N], by = "animal_id"]$V1, list(animal_id, last_date = date, site)]
  seen <- if (is.null(data$record_end)) data$horizon else seen_to(data$horizon, data$record_end)
  last[, end := pmin(last_date + settings$within_days, as.Date(seen))]
  heard <- lst[last, on = list(site, date > last_date, date <= end), .N, by = .EACHI]
  last[, listened := heard$N]
  last[, ahead := as.integer(end - last_date)]
  last[, score := if (settings$within_days > 0) listened / settings$within_days else NA_real_]
  last[, vote := data.table::fcase(!site %in% lst$site, "unavailable",
                                   ahead < settings$within_days | is.na(score), "none",
                                   score < settings$listening_share, "review",
                                   default = "none")]
  heard_txt <- sprintf("the last site listened on %s of the %s after the last detection (%s)",
                       fmt_n(last$listened), n_of(settings$within_days, "day"),
                       format(last$last_date))
  short_txt <- sprintf("the data end %s after the last detection; record_end judges it",
                       n_of(last$ahead, "day"))
  last[, detail := data.table::fcase(vote == "unavailable",
                                     "no listening record for the stations of the last site",
                                     ahead < settings$within_days, short_txt,
                                     default = heard_txt)]
  last[, list(animal_id, vote, score, detail)]
}

# ---- Stream: movement after the candidate death date ----
rule_movement <- function(data, settings) {
  ev <- data$votes[kind == "vote" & vote %in% c("dead", "review") & !is.na(date)]
  det <- data$detections
  if (!nrow(ev) || !nrow(det)) return(data.frame(animal_id = character(), vote = character()))
  cand <- ev[, list(cand = min(date)), by = "animal_id"]
  after <- det[cand, on = list(animal_id, datetime >= cand), nomatch = NULL,
               list(animal_id, transmitter, datetime = x.datetime, date = x.date, station_id, lat,
                    lon)]
  data.table::setorderv(after, c("animal_id", "transmitter", "datetime"))
  # Judged per transmitter, so the moving tag of an animal with two tags
  # shows its away days even while a shed tag is heard at its station every
  # day.
  out <- after[, moved_one(.SD, settings, data$sites_km), by = c("animal_id", "transmitter")]
  out[, n_tx := .N, by = "animal_id"]
  out[n_tx > 1, detail := paste0(transmitter, ": ", detail)]
  out <- unique(out[order(animal_id, -(vote == "alive"), -score)], by = "animal_id")
  out[, c("transmitter", "n_tx") := NULL]
  out <- out[cand, on = "animal_id"]
  out[is.na(vote), `:=`(vote = "none", score = 0, detail = "no detection after the candidate date")]
  # The candidate date is the earliest of the dead and review votes before
  # this stream, which need not be the band's date, so the text gives it.
  out[grepl("the candidate date$", detail), detail := paste0(detail, ", ", format(as.Date(cand)))]
  out[, date := data.table::fifelse(vote == "alive", first_away, empty_utc(1))]
  out[, basis := data.table::fifelse(vote == "alive",
                                     "the first detection away from the candidate site",
                                     NA_character_)]
  out
}

# An away day is a day on which every detection is far from the reference
# stations. A dead tag in open water is heard now and then at a distant
# station, but on the same day its near stations hear it too; an animal that
# has left is heard only away.
moved_one <- function(d, settings, sites_km) {
  days <- unique(d$date)
  n_ref <- min(settings$ref_days, length(days))
  ref <- unique(d[date %in% days[seq_len(n_ref)], list(station_id, lat, lon)])
  # Judged once per station position: a dead tag can be
  # heard a million times after its candidate date.
  pos <- unique(d[, list(station_id, lat, lon)])
  pos_far <- vapply(seq_len(nrow(pos)), function(i) {
    !pos$station_id[i] %in% ref$station_id &&
      all(haversine_km(pos$lat[i], pos$lon[i], ref$lat, ref$lon) > sites_km)
  }, logical(1))
  far <- pos_far[match(paste(d$station_id, d$lat, d$lon),
                       paste(pos$station_id, pos$lat, pos$lon))]
  away <- tapply(far, d$date, all)
  away_days <- sum(away)
  first_day <- if (away_days) as.Date(names(away)[which(away)[1]]) else as.Date(NA)
  moved <- away_days >= settings$min_days_away
  list(vote = if (moved) "alive" else "none", score = as.numeric(away_days),
       first_away = if (away_days) d$datetime[which(d$date == first_day)[1]] else empty_utc(1),
       detail = sprintf(paste("%s heard only more than %g km from the %s heard on the first",
                              "%s from the candidate date"),
                        n_of(away_days, "day"), sites_km, n_of(nrow(ref), "station"),
                        n_of(n_ref, "detection day")))
}

# ---- Stream: persistence of the core stations ----
# A 30-day trailing window, 95% coverage cores, and the Jaccard similarity
# of cores 7 days apart. A ranking, never a call.
rule_persistence <- function(data, settings) {
  det <- data$detections
  if (!nrow(det)) return(data.frame(animal_id = character(), vote = character()))
  dsd <- det[!is.na(station_id), list(n = .N), by = list(animal_id, date, station_id)]
  enough <- dsd[, list(nd = length(unique(date))), by = "animal_id"][nd >= settings$min_record_days]
  pairs <- dsd[animal_id %in% enough$animal_id, persistence_pairs(.SD, settings),
               by = "animal_id"]
  score <- pairs[, list(pairs = .N, score = mean(jaccard)), by = "animal_id"]
  score <- score[pairs >= settings$min_pairs]
  review_at <- if (is.null(settings$review_at)) Inf else settings$review_at
  score[, vote := data.table::fifelse(score >= review_at, "review", "none")]
  score[, detail := sprintf("mean Jaccard %.3f of core stations %g days apart, over %s",
                            score, settings$lag_days, n_of(pairs, "pair"))]
  list(votes = score, evidence = pairs)
}

persistence_pairs <- function(d, settings) {
  m <- data.table::dcast(d, date ~ station_id, value.var = "n", fill = 0)
  dayv <- m$date
  mm <- as.matrix(m[, -1, drop = FALSE])
  cum <- rbind(0, apply(mm, 2, cumsum))
  lo <- findInterval(dayv - settings$window_days, dayv, left.open = TRUE)
  hi <- seq_along(dayv) - 1L
  ok <- (hi - lo) >= settings$min_window_days
  if (!any(ok)) return(data.table::data.table(date = as.Date(character()), jaccard = numeric()))
  v <- cum[hi[ok] + 1L, , drop = FALSE] - cum[lo[ok] + 1L, , drop = FALSE]
  core <- t(apply(v, 1, core_of, coverage = settings$coverage))
  if (ncol(mm) == 1) core <- t(core)
  days <- dayv[ok]
  j <- match(days + settings$lag_days, days)
  k <- which(!is.na(j))
  a <- core[k, , drop = FALSE]
  b <- core[j[k], , drop = FALSE]
  data.table::data.table(date = days[k], jaccard = rowSums(a & b) / rowSums(a | b))
}

core_of <- function(v, coverage) {
  out <- logical(length(v))
  if (sum(v) <= 0) return(out)
  o <- order(v, decreasing = TRUE)
  k <- which(cumsum(v[o]) / sum(v) >= coverage)[1]
  out[o[seq_len(k)]] <- TRUE
  out
}

# Columns named inside data.table calls in this file.
utils::globalVariables(c(
  "window_start", "window_diag_km", "run_start", "reason", "flat", "stat", "med", "limit",
  "flat_days", "flatline", "flat_onset", "gap_days", "last_detection", "last_active",
  "active_recent", "active_days", "spikes_ignored", "first_flat_observed", "readings",
  "raw_only", "first_reading", "expected_end", "last", "x.datetime", "x.date", "cand",
  "first_away", "nd", "jaccard", "n", "transmitter", "datetime", "date", "station_id", "lat",
  "lon", "site", "animal_id", "gap_days", "earliest_date", "gap_note", "prev", "gap", "i.gap",
  "i.prev", "i.run_start", "i.terminal_site", "terminal_site", "n_tx", "last_date", "end",
  "listened", "ahead", ".EACHI", "cal"
))
