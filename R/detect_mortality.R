# ---- Mortality triage: run the rule streams and sort animals into bands ----

#' Sort animals into dead, review and alive bands
#'
#' Runs each rule stream of `rules` on the detection set and sorts every
#' animal with a detection into the dead, review or alive band. The bands
#' are a triage for the review cards of [build_mortality_cards()]: the
#' call a person makes on a card is the verdict, and the rules never make it.
#'
#' The streams run in the order of `rules`, and each sees the votes of the
#' streams before it, so movement after a candidate death date can be judged.
#' The default streams are described in [mortality_rules()]. How the votes
#' set the band:
#'
#' * **dead**: a death vote (an accelerometer flatline, or a terminal
#'   stationary run) with no alive vote, unless the death evidence begins
#'   within the record-end window of a record that ends near tag life or the
#'   data horizon, or the animal carries a sensor whose readings are not
#'   calibrated.
#' * **review**: a death vote beside an alive vote (an active accelerometer,
#'   or movement after the candidate date); death evidence near the end of a
#'   record that ends near tag life or the horizon; a final run that is
#'   stationary across a site boundary, at one station whose recorded
#'   position moved, within `near_factor` times the site size, or at one site
#'   apart from stray detections on at most `stray_days` days; a record whose
#'   last site listened on less than `listening_share` of the `within_days`
#'   after its last detection, only when `listening_share` is set above its
#'   default of 0 (at the default the `site_listening` stream never votes and
#'   only reports the days on the cards); a record with no station position;
#'   a record too sparse for the final run; a persistence score at or
#'   above `review_at`; and a record with no death vote that ends near tag
#'   life or the horizon, because the rules cannot see what happened after
#'   it. A record with a death vote that ends there goes to review only
#'   when its death evidence begins within the record-end window
#'   (`within_days` of the `record_end` stream) of its last detection;
#'   evidence that begins earlier keeps the dead band, whatever
#'   `record_end` votes.
#' * **alive**: no death evidence on a record that ends well before tag life
#'   and the horizon; with `listening_share` above 0, also at a site that
#'   kept listening.
#'
#' **The candidate death date.** Only streams of kind `"vote"` give it: for
#' an animal in the dead band, the date of the deciding stream's vote; for one
#' in review, the earliest date of a dead or review vote. The final-run
#' stream (`terminal_run`) dates a death by the first detection of the final
#' stationary run. Where the site of the run listened on every day between
#' the animal's detection before the run and the run's first detection, that
#' date is the earliest the death could be given the detections: an animal
#' may hold station alive at that site before it dies. Where the site did not
#' listen on one or more of those days, the run may have started unheard. The
#' date stays the first detection of the run, and its basis
#' (`death_date_basis`, and on the card) counts the days the site was not
#' listening and gives the date of the detection before the run as the
#' earliest the death could be. A site listens on a day when a receiver at
#' one of its stations is in the water and, after [audit_listening()], not
#' flagged as not listening. Evidence streams, such as
#' `depth_flatline`, never set the date. Their records on the card are what
#' the reviewer uses to date the death, and [filter_mortality()] cuts from
#' the date of the call. The candidate date only guides the call.
#'
#' Read the alive band for what it is. Because no record that ends near tag
#' life or the horizon is put in the alive band, it holds only records that
#' stopped early: animals that left the array, were harvested or eaten, or
#' whose tags failed. Animals heard to the end of the study are never
#' cleared without a card.
#'
#' Inside the review band the persistence score ranks animals: the higher the
#' score, the more stable the set of stations that heard the animal.
#'
#' Accelerometers with raw readings only cast no vote, dead or alive, and
#' `detect_mortality()` warns with their count. An animal that carries any
#' sensor transmitter with raw readings only is never put in the dead band:
#' its death evidence sends it to review under the rule
#' `uncalibrated_sensor`, the column `sensor_uncalibrated` marks it, and a
#' message names it, because the sensor it carries might have said alive.
#'
#' The way votes combine into bands is this package's own rule; see
#' [mortality_rules()] for the published kinds of evidence the streams read
#' (Klinard and Matley 2020).
#'
#' @references Klinard, N. V., and Matley, J. K. (2020). Living until proven
#'   dead: addressing mortality in acoustic telemetry research. Reviews in
#'   Fish Biology and Fisheries 30, 485-499. \doi{10.1007/s11160-020-09613-z}
#'
#' @param x A `detset`.
#' @param rules A list of rule streams from [mortality_rules()], with any
#'   stream added by [mortality_rule()].
#' @param control A list of options shared by the streams:
#'   * `sites_km`, the largest distance in km between two stations of one
#'     site (default 1); stations are clustered by complete linkage.
#'   * `site_distance`, `NULL` for straight-line distance, or a function of
#'     `(lat, lon)` that returns a square matrix of distances in km between
#'     the positions, such as in-water distances.
#'   * `horizon`, the end of the data as a POSIXct time in UTC; by default
#'     the last detection in `x`, of any transmitter. A horizon on a later
#'     date than the last detection in `x`, as for a month cut from a longer
#'     record, sets the tag-life comparison and the cards' days to the
#'     horizon, but record_end and site_listening still stop at the end of
#'     the detection record: `x` holds nothing after it, so an animal heard
#'     on its last day is cut off there, its end unknown, and goes to review with the
#'     end basis "end of the detection record". On a cut, death evidence that
#'     begins within `within_days` of the cut's end stays in review, whatever
#'     the horizon; a longer cut, or a smaller `within_days` in
#'     [mortality_rules()], lets a final run reach the dead band.
#'   * `sparse_to_review`, `TRUE` (default) to send a record with too few
#'     detection days for the final run to the review band, or `FALSE` to
#'     leave it alive.
#'
#' @return An object of class `mortality_bands`, a list of:
#'   * `bands`: one row per animal with its band, the deciding rule and its
#'     label, the candidate death date and its basis, the persistence score,
#'     each stream's vote, and the statistics behind them.
#'   * `votes`: one row per animal and stream: `rule`, `vote`, `score`,
#'     `date`, `basis` and `detail`.
#'   * `sensor`: one row per sensor transmitter, with its accelerometer vote.
#'   * `evidence`: the rows behind the votes, one data frame per stream:
#'     the detection days of each final window, the daily sensor
#'     statistics and the persistence pairs.
#'   * `sites`: the station positions and their site.
#'   * `settings`: the options, the streams and counts used, and the
#'     detections and animals of `x`, which [processing_report()] and the
#'     cards use to log the step.
#' @seealso [mortality_rules()], [build_mortality_cards()]
#' @export
#' @examples
#' study <- example_mortality
#' dets <- detset(study$detections, study$deployments, study$animals)
#' # The defaults read final runs of 14 days; this study runs 2 to 10 June.
#' table(detect_mortality(dets)$bands$band)
#' mort_rules <- mortality_rules(min_days = 3, min_calendar_days = 3,
#'                          min_flat_days = 2, within_days = 2)
#' bands <- detect_mortality(dets, rules = mort_rules)
#' bands
#' table(bands$bands$band)
detect_mortality <- function(x, rules = mortality_rules(), control = list()) {
  validate_detset(x)
  check_rule_list(rules)
  if (!is.list(control)) stop_detqc("`control` must be a list.")
  ctl <- mortality_control(utils::modifyList(as.list(attr(rules, "control")), control))
  note_unassigned(x$detections, no_animal_advice(x))
  data <- mortality_data(x, ctl)
  run <- run_rules(rules, data)
  censor_days <- rule_setting(rules, "record_end", "within_days")
  b <- combine_votes(run, data, ctl, if (is.na(censor_days)) 30 else censor_days)
  acc <- run$evidence$accel_flatline
  raw_acc <- warn_raw_accelerometers(acc)
  note_uncalibrated(data$readings, b$bands)
  out <- structure(list(
    bands = b$bands, votes = b$votes,
    sensor = if (is.list(acc) && !is.null(acc$streams)) as_df(acc$streams) else empty_df_streams(),
    evidence = lapply(run$evidence, evidence_frames),
    sites = as_df(data$sites),
    settings = list(control = ctl[c("sites_km", "sparse_to_review")],
                    site_distance = if (is.null(ctl$site_distance)) "straight-line" else
                      "user-supplied",
                    horizon = data$horizon, rules = rule_table(rules),
                    unlocated = data$unlocated,
                    unlocated_animals = sum(data$records$located == 0),
                    n_sites = data$n_sites, rows = nrow(x$detections),
                    animals = count_animals(x$detections),
                    min_days = rule_setting(rules, "terminal_run", "min_days"),
                    sites_km = ctl$sites_km, raw_accelerometers = raw_acc,
                    rule_settings = stats::setNames(lapply(rules, function(r) r$settings),
                                                    names(rules)))
  ), class = "mortality_bands")
  out$settings$reason <- bands_reason(out)
  out
}

# ---- Internal: options ----
mortality_control <- function(control) {
  if (!is.list(control)) stop_detqc("`control` must be a list.")
  known <- c("sites_km", "site_distance", "horizon", "sparse_to_review")
  bad <- setdiff(names(control), known)
  if (length(bad)) stop_detqc("`control` has no option `%s`.", bad[1])
  ctl <- utils::modifyList(list(sites_km = 1, site_distance = NULL, horizon = NULL,
                                sparse_to_review = TRUE), control)
  check_control_values(ctl)
  ctl
}

check_control_values <- function(ctl) {
  ok <- c(sites_km = is_positive_number(ctl$sites_km),
          site_distance = is.null(ctl$site_distance) || is.function(ctl$site_distance),
          horizon = is.null(ctl$horizon) || is_one_utc(ctl$horizon),
          sparse_to_review = is_flag(ctl$sparse_to_review))
  msg <- c(sites_km = "one positive number",
           site_distance = "NULL or a function of (lat, lon)",
           horizon = "one POSIXct time in UTC", sparse_to_review = "TRUE or FALSE")
  bad <- names(ok)[!ok]
  if (length(bad)) stop_detqc("`control$%s` must be %s.", bad[1], msg[[bad[1]]])
  invisible(TRUE)
}

is_positive_number <- function(v) is.numeric(v) && length(v) == 1 && isTRUE(v > 0)

is_one_utc <- function(v) is_utc(v) && length(v) == 1 && !is.na(v)

is_flag <- function(v) is.logical(v) && length(v) == 1 && !is.na(v)

# ---- Internal: what every stream reads ----
# Built once per run, so each stream reads positions, sites, per-animal
# records and sensor readings without computing them again.
mortality_data <- function(x, ctl) {
  loc <- locate_detections(x)
  sites <- build_sites(loc$positions, ctl$sites_km, ctl$site_distance)
  det <- loc$detections[sites$map, on = "pos", site := i.site]
  record_end <- if (nrow(x$detections)) max(x$detections$datetime) else empty_utc(1)
  horizon <- if (is.null(ctl$horizon)) record_end else ctl$horizon
  records <- animal_records(x$detections, det)
  records <- expected_end(records, x$animals, horizon, record_end)
  list(x = x, detections = det, records = records, readings = sensor_readings(x),
       sites = sites$table, n_sites = sites$n_sites, sites_km = ctl$sites_km,
       site_distance = ctl$site_distance, horizon = horizon, record_end = record_end,
       horizon_given = !is.null(ctl$horizon), unlocated = loc$unlocated,
       listening = listening_sites(x$listening, sites$table), votes = empty_votes_dt())
}

# The last time the detset can show: the data horizon, or the end of the
# detection record when a horizon given in `control` lies on a later UTC
# date. A detset cut from a longer record holds no detection after its cut,
# so a record heard to the cut has not ended.
seen_to <- function(horizon, record_end) {
  later <- !is.na(record_end) & as.Date(horizon) > as.Date(record_end)
  if (later) record_end else horizon
}

# The days each site was listening, from the receiver-day table of the
# detset: a day counts when any receiver at a station of the site was in the
# water and, after audit_listening(), was flagged as listening.
listening_sites <- function(lst, sites) {
  out <- data.table::data.table(site = integer(), date = as.Date(character()))
  if (is.null(lst) || !nrow(lst) || !all(c("station_id", "date") %in% names(lst))) return(out)
  l <- as_dt(lst)
  if ("listening" %in% names(l)) l <- l[is.na(l[["listening"]]) | l[["listening"]]]
  st <- unique(data.table::as.data.table(sites)[, list(station_id, site)])
  out <- unique(st[unique(l[, list(station_id, date)]), on = "station_id", nomatch = NULL,
                   list(site, date), allow.cartesian = TRUE])
  data.table::setkeyv(out, c("site", "date"))
  out
}

# One row per animal with a detection, located or not, so that no animal
# leaves the bands.
animal_records <- function(detections, located) {
  d <- as_dt(detections)[!is.na(animal_id), list(animal_id, datetime)]
  if (!nrow(d)) {
    return(data.table::data.table(animal_id = character(), detections = integer(),
                                  n_days = integer(), first = empty_utc(), last = empty_utc(),
                                  located = integer()))
  }
  out <- d[, list(detections = .N, n_days = length(unique(as.Date(datetime))),
                  first = min(datetime), last = max(datetime)), by = "animal_id"]
  n_loc <- located[, list(located = .N), by = "animal_id"]
  out[n_loc, on = "animal_id", located := i.located]
  out[is.na(located), located := 0L]
  data.table::setorderv(out, "animal_id")
  out
}

# The end the record could reach: tag life from release (or from the first
# detection when no release time is given), cut at the data horizon or at
# the end of the detection record, whichever is earlier (seen_to()). An
# animal with two transmitters takes the later tag end.
expected_end <- function(records, animals, horizon, record_end = horizon) {
  r <- data.table::copy(records)
  r[, tag_end := empty_utc(.N)]
  if (!is.null(animals) && "tag_life_days" %in% names(animals)) {
    a <- as_dt(animals)
    if (!"release_datetime" %in% names(a)) a[, release_datetime := empty_utc(.N)]
    a <- a[!is.na(tag_life_days), list(animal_id, tag_life_days, release = release_datetime)]
    a <- a[r[, list(animal_id, first)], on = "animal_id", nomatch = NULL]
    a[is.na(release), release := first]
    if (nrow(a)) {
      a <- a[, list(tag_end = max(release + tag_life_days * 86400)), by = "animal_id"]
      r[a, on = "animal_id", tag_end := i.tag_end]
    }
  }
  seen <- seen_to(horizon, record_end)
  data_end <- if (identical(seen, horizon)) "data horizon" else "end of the detection record"
  r[, horizon := horizon]
  r[, expected_end := data.table::fifelse(!is.na(tag_end) & tag_end < seen, tag_end, seen)]
  r[, end_basis := data.table::fifelse(!is.na(tag_end) & tag_end < seen, "tag life", data_end)]
  r[]
}

# ---- Internal: positions and sites ----
# Each detection takes the position of the deployment that heard it, so a
# station that moved is placed where it was at the time. Detections with no
# deployment fall back to their own deploy_lat and deploy_lon, as in OTN
# extracts.
locate_detections <- function(x) {
  d <- x$detections
  dep <- x$deployments
  row <- match_deployment(d, dep)
  lat <- dep$deploy_lat[row]
  lon <- dep$deploy_lon[row]
  if (all(c("deploy_lat", "deploy_lon") %in% names(d))) {
    lat <- data.table::fifelse(is.na(lat), as.numeric(d$deploy_lat), lat)
    lon <- data.table::fifelse(is.na(lon), as.numeric(d$deploy_lon), lon)
  }
  dt <- data.table::data.table(
    animal_id = d$animal_id, transmitter = d$transmitter, datetime = d$datetime,
    station_id = d$station_id, lat = lat, lon = lon
  )[!is.na(animal_id)]
  unlocated <- sum(is.na(dt$lat) | is.na(dt$lon))
  dt <- dt[!is.na(lat) & !is.na(lon)]
  dt[, date := as.Date(datetime)]
  data.table::setorderv(dt, c("animal_id", "datetime"))
  # Sites belong to the array: every deployment position is clustered, so a
  # subset of animals gets the same sites as the whole set.
  stn <- unique(data.table::rbindlist(list(
    data.table::data.table(station_id = dep$station_id, lat = dep$deploy_lat, lon = dep$deploy_lon),
    dt[, list(station_id, lat, lon)]
  )))[!is.na(lat) & !is.na(lon)]
  pos <- unique(stn[, list(lat, lon)])
  pos[, pos := seq_len(.N)]
  dt[pos, on = c("lat", "lon"), pos := i.pos]
  stn[pos, on = c("lat", "lon"), pos := i.pos]
  list(detections = dt, positions = stn, unlocated = unlocated)
}

build_sites <- function(stn, sites_km, site_distance) {
  pos <- unique(stn[, list(pos, lat, lon)])
  data.table::setorderv(pos, "pos")
  n <- nrow(pos)
  site <- rep(1L, n)
  if (n > 1) {
    dm <- if (is.null(site_distance)) {
      dist_km_matrix(pos$lat, pos$lon)
    } else {
      site_distance(pos$lat, pos$lon)
    }
    if (!is.matrix(dm) || any(dim(dm) != n)) {
      stop_detqc("`site_distance` must return a %s by %s matrix; it returned a %s.",
                 fmt_n(n), fmt_n(n), paste(dim(as.matrix(dm)), collapse = " by "))
    }
    site <- stats::cutree(stats::hclust(stats::as.dist(dm), method = "complete"), h = sites_km)
  }
  map <- data.table::data.table(pos = pos$pos, site = as.integer(site))
  tab <- stn[map, on = "pos"][, list(station_id, deploy_lat = lat, deploy_lon = lon, site)]
  data.table::setorderv(tab, c("site", "station_id"))
  list(map = map, table = tab, n_sites = length(unique(site)))
}

# ---- Internal: sensor readings ----
# One row per sensor reading, on the physical scale where a calibration
# allows it. Most readers fill sensor_raw only, and sensor_value is filled by
# calibrate_sensors() or by an OTN extract, so both columns are read. A raw
# reading with a calibration in the animals table is converted here.
sensor_readings <- function(x) {
  d <- x$detections
  if (!any(c("sensor_raw", "sensor_value") %in% names(d))) return(empty_readings())
  col <- function(nm, na) if (nm %in% names(d)) d[[nm]] else rep(na, nrow(d))
  r <- data.table::data.table(
    animal_id = d$animal_id, transmitter = d$transmitter, datetime = d$datetime,
    raw = as.numeric(col("sensor_raw", NA_real_)),
    value = as.numeric(col("sensor_value", NA_real_)),
    unit = as.character(col("sensor_unit", NA_character_)),
    type = as.character(col("sensor_type", NA_character_))
  )[!is.na(raw) | !is.na(value)]
  if (!nrow(r)) return(empty_readings())
  cal <- calibration_table(x$animals)
  r <- cal[r, on = "transmitter"]
  convert <- is.na(r$value) & !is.na(r$raw) & !is.na(r$slope) & !is.na(r$intercept)
  r[convert, `:=`(value = intercept + slope * raw, unit = cal_unit)]
  r[, scale := data.table::fifelse(is.na(value), "raw", "calibrated")]
  # Classes and units are read once per spelling: a study can hold tens of
  # millions of readings and only a few spellings.
  key <- unique(r[, list(type, unit, cal_type, cal_unit)])
  key[, class := sensor_class(type, unit)]
  key[is.na(class), class := sensor_class(cal_type, cal_unit)]
  key[is.na(class), class := "unknown"]
  key[, unit_out := unit_key(unit)]
  r[key, on = c("type", "unit", "cal_type", "cal_unit"), `:=`(class = i.class,
                                                              unit_out = i.unit_out)]
  r[, unit := unit_out]
  r[, date := as.Date(datetime)]
  r <- r[, list(animal_id, transmitter, datetime, date, raw, value, unit, class, scale,
                step = abs(slope))]
  sensor_steps(r)
}

empty_readings <- function() {
  data.table::data.table(
    animal_id = character(), transmitter = character(), datetime = empty_utc(),
    date = as.Date(character()), raw = numeric(), value = numeric(), unit = character(),
    class = character(), scale = character(), step = numeric()
  )
}

# The step of each sensor: one count on the calibrated scale. It is the
# slope of the calibration where the set carries one. Where the readings
# arrived calibrated with no slope, it is the smallest gap between two
# distinct calibrated values of that transmitter and class; readings that
# are not quantised give a small step, which then changes no flat limit.
sensor_steps <- function(r) {
  r[!is.finite(step) | step <= 0, step := NA_real_]
  est <- r[is.na(step) & scale == "calibrated",
           list(est = smallest_gap(value)), by = list(transmitter, class)]
  if (nrow(est)) {
    r[est, on = c("transmitter", "class"), step := data.table::fcoalesce(step, i.est)]
  }
  r
}

smallest_gap <- function(v) {
  u <- sort(unique(v[is.finite(v)]))
  if (length(u) < 2) return(NA_real_)
  d <- diff(u)
  # A gap of floating-point size is one value written two ways.
  d <- d[d > sqrt(.Machine$double.eps) * max(1, abs(u))]
  if (length(d)) min(d) else NA_real_
}

# A sensor cannot tell a range or a level finer than `flat_steps` of its
# steps from noise: one count either side of a dead tag's reading spans two
# steps. A flat limit is never set below it, nor below the configured limit.
flat_steps <- 2
flat_limit <- function(limit, step) {
  pmax(limit, flat_steps * data.table::fcoalesce(as.numeric(step), 0))
}

# Floating-point-safe `stat <= limit`: two counts of 0.1504 m computed from
# two readings may differ from 2 * 0.1504 in the last bit.
at_most <- function(stat, limit) {
  stat <= limit + sqrt(.Machine$double.eps) * pmax(1, abs(limit))
}

calibration_table <- function(ani) {
  out <- data.table::data.table(
    transmitter = character(), slope = numeric(), intercept = numeric(),
    cal_unit = character(), cal_type = character()
  )
  if (is.null(ani)) return(out)
  get <- function(nm, na) if (nm %in% names(ani)) ani[[nm]] else rep(na, nrow(ani))
  cal <- data.table::data.table(
    transmitter = ani$transmitter, slope = as.numeric(get("sensor_slope", NA_real_)),
    intercept = as.numeric(get("sensor_intercept", NA_real_)),
    cal_unit = unit_key(get("sensor_unit", NA_character_)),
    cal_type = as.character(get("sensor_type", NA_character_))
  )
  cal[is.na(cal_unit), cal_unit := unit_key(cal_type)]
  unique(cal, by = "transmitter")
}

# ---- Internal: running the streams ----
vote_values <- c("dead", "alive", "review", "none", "unavailable")

empty_votes_dt <- function() {
  data.table::data.table(animal_id = character(), rule = character(), kind = character(),
                         vote = character(), score = numeric(), date = empty_utc(),
                         basis = character(), detail = character())
}

run_rules <- function(rules, data) {
  extras <- list()
  evidence <- list()
  for (nm in names(rules)) {
    r <- rules[[nm]]
    res <- tryCatch(r$fun(data, r$settings), error = function(e) {
      stop_detqc("Rule stream `%s` failed: %s", nm, conditionMessage(e))
    })
    res <- check_rule_result(res, nm, data$records$animal_id)
    v <- complete_votes(res$votes, data$records$animal_id, nm, r$kind)
    data$votes <- data.table::rbindlist(list(data$votes, v), use.names = TRUE)
    extras[[nm]] <- res$votes
    evidence[[nm]] <- res$evidence
  }
  list(votes = data$votes, extras = extras, evidence = evidence)
}

# A stream returns a data frame of votes, or a list of `votes` and
# `evidence`. The checks name the stream, so a user's stream that returns
# the wrong shape is found at once.
check_rule_result <- function(res, nm, ids) {
  if (is.data.frame(res)) res <- list(votes = res, evidence = NULL)
  if (!is.list(res) || !is.data.frame(res$votes)) {
    stop_detqc("Rule stream `%s` must return a data frame of votes, or a list with `votes`.", nm)
  }
  v <- as_dt(res$votes)
  miss <- setdiff(c("animal_id", "vote"), names(v))
  if (length(miss)) stop_detqc("The votes of rule stream `%s` have no column `%s`.", nm, miss[1])
  bad <- setdiff(unique(v$vote), vote_values)
  if (length(bad)) {
    stop_detqc("Rule stream `%s` voted \"%s\"; a vote is one of %s.", nm, bad[1],
               paste(vote_values, collapse = ", "))
  }
  if (anyDuplicated(v$animal_id)) stop_detqc("Rule stream `%s` voted twice on one animal.", nm)
  unknown <- setdiff(v$animal_id, ids)
  if (length(unknown)) {
    stop_detqc("Rule stream `%s` voted on animal %s, which has no detection.", nm, unknown[1])
  }
  if ("date" %in% names(v) && nrow(v) && !is_utc(v$date)) {
    stop_detqc("The `date` of rule stream `%s` must be POSIXct in UTC.", nm)
  }
  res$votes <- v
  res
}

# One vote per animal and stream. An animal a stream did not return gets
# "unavailable", or "none" from a ranking stream, which never votes.
complete_votes <- function(v, ids, nm, stream_kind) {
  fill <- if (stream_kind == "rank") "none" else "unavailable"
  out <- data.table::data.table(animal_id = ids, rule = nm, kind = stream_kind)
  cols <- list(vote = NA_character_, score = NA_real_, date = empty_utc(1),
               basis = NA_character_, detail = NA_character_)
  for (cl in names(cols)) {
    out[, (cl) := if (cl %in% names(v)) v[[cl]][match(ids, v$animal_id)] else cols[[cl]]]
  }
  absent <- is.na(out$vote)
  out[absent, vote := fill]
  out[absent & is.na(detail), detail := "the stream returned no vote for this animal"]
  out[, date := as.POSIXct(as.numeric(date), origin = "1970-01-01", tz = "UTC")]
  out[]
}

# An accelerometer with raw readings only cannot vote, dead or alive, so an
# active resident is judged on space alone: without calibration, alive
# animals can move from review to the dead band, and a warning says so. It
# is given once per session for one set of such transmitters, since every
# run on the same data has the same cause; the count stays in the bands'
# settings, the log record and print().
warn_raw_accelerometers <- function(acc) {
  s <- if (is.list(acc)) acc$streams else NULL
  if (is.null(s) || !nrow(s)) return(c(raw = 0L, accelerometers = 0L))
  is_acc <- s$class == "acceleration"
  is_raw <- is_acc & s$raw_only == s$readings
  raw <- sum(is_raw)
  if (raw > 0) {
    who <- if ("transmitter" %in% names(s)) s$transmitter[is_raw] else s$animal_id[is_raw]
    warn_once(paste0("raw_accelerometers:", paste(sort(unique(who)), collapse = ",")),
              paste("%s of %s accelerometer transmitters have raw readings only and cast no",
                    "sensor vote; give sensor_slope and sensor_intercept (see",
                    "calibrate_sensors()). This warning is given once per session for these",
                    "transmitters."), fmt_n(raw), fmt_n(sum(is_acc)))
  }
  c(raw = as.integer(raw), accelerometers = as.integer(sum(is_acc)))
}

# Names the animals whose death evidence went to review because a sensor
# transmitter of theirs has raw readings only.
note_uncalibrated <- function(readings, bands) {
  capped <- bands$animal_id[bands$deciding_rule %in% "uncalibrated_sensor"]
  if (!length(capped)) return(invisible(character()))
  r <- readings[animal_id %in% capped]
  tx <- r[, list(raw_only = all(scale == "raw")), by = c("animal_id", "transmitter")]
  tx <- tx[raw_only == TRUE]
  message(sprintf(paste("detect_mortality(): %s with death evidence %s sensor readings with no",
                        "calibration, such as %s (%s), so %s to review and %s not called dead.",
                        "Calibrate them with calibrate_sensors(x, specs)."),
                  n_of(length(capped), "animal"), verb(length(capped), "carries", "carry"),
                  tx$animal_id[1],
                  paste(tx$transmitter[tx$animal_id == tx$animal_id[1]], collapse = ", "),
                  verb(length(capped), "it goes", "they go"),
                  verb(length(capped), "is", "are")))
  invisible(capped)
}

# The triage reads detections with an animal_id only, so a GLATOS export with
# blank animal_id gives no band. With no animal at all it warns; otherwise a
# message counts what it set aside, reference tags included.
note_unassigned <- function(det, advice) {
  na <- is.na(det$animal_id)
  if (!any(na)) return(invisible(0L))
  tx <- length(unique(det$transmitter[na]))
  if (all(na)) {
    warn_detqc(paste("No detection has an animal_id, so there is no animal to band: %s",
                     "detections of %s set aside. %s"),
               fmt_n(sum(na)), n_of(tx, "transmitter"), advice)
  } else {
    message(sprintf(paste("detect_mortality(): %s of %s, of %s, %s no animal_id, such as",
                          "reference tags, and %s not banded."),
                    fmt_n(sum(na)), n_of(length(na), "detection"), n_of(tx, "transmitter"),
                    verb(sum(na), "has", "have"), verb(sum(na), "is", "are")))
  }
  invisible(sum(na))
}

# Warnings given once per session, keyed by what they are about.
warned <- new.env(parent = emptyenv())

warn_once <- function(key, ...) {
  if (exists(key, envir = warned, inherits = FALSE)) return(invisible(FALSE))
  assign(key, TRUE, envir = warned)
  warn_detqc(...)
  invisible(TRUE)
}

forget_warnings <- function() {
  rm(list = ls(warned), envir = warned)
  invisible(TRUE)
}

evidence_frames <- function(e) {
  if (is.null(e)) return(NULL)
  if (is.data.frame(e)) return(as_df(e))
  lapply(e, function(z) if (is.data.frame(z)) as_df(z) else z)
}

empty_df_streams <- function() as_df(empty_streams())

# ---- Print ----

#' @export
print.mortality_bands <- function(x, ...) {
  b <- x$bands
  n <- nrow(b)
  s <- x$settings
  cat_wrap(sprintf("<mortality_bands> %s in %s of at most %g km; streams: %s",
                   n_of(n, "animal"), n_of(s$n_sites, "site"), s$sites_km,
                   paste(s$rules$rule, collapse = ", ")))
  for (bd in c("dead", "review", "alive")) {
    k <- b$band == bd
    rules <- table(b$deciding_rule[k])
    why <- ""
    if (length(rules)) why <- paste0("  (", paste(names(rules), rules, collapse = "; "), ")")
    cat(sprintf("  %-6s %4s of %s%s\n", bd, fmt_n(sum(k)), n_of(n, "animal"), why))
  }
  sv <- x$sensor
  voted <- sum(sv$vote %in% c("dead", "alive"))
  cat(sprintf("  sensor streams: %s; %s voted (%s dead, %s alive)\n",
              n_of(nrow(sv), "row"), n_of(voted, "accelerometer"), fmt_n(sum(sv$vote == "dead")),
              fmt_n(sum(sv$vote == "alive"))))
  raw <- s$raw_accelerometers
  if (isTRUE(raw[["raw"]] > 0)) {
    cat(sprintf("  %s of %s %s raw readings only and cast no vote\n",
                fmt_n(raw[["raw"]]), n_of(raw[["accelerometers"]], "accelerometer"),
                verb(raw[["raw"]], "has", "have")))
  }
  if (isTRUE(s$unlocated > 0)) {
    cat_wrap(sprintf("%s had no station position and %s not scored by the final run",
                     n_of(s$unlocated, "detection"), verb(s$unlocated, "was", "were")),
             indent = 2, exdent = 4)
  }
  if (isTRUE(s$unlocated_animals > 0)) {
    cat_wrap(sprintf(paste("%s %s no located detection; %s in the bands,",
                           "review unless an accelerometer votes dead"),
                     n_of(s$unlocated_animals, "animal"), verb(s$unlocated_animals, "has", "have"),
                     verb(s$unlocated_animals, "it is", "they are")), indent = 2, exdent = 4)
  }
  cat_wrap("The bands are a triage. The call made on the review cards is the verdict.",
           indent = 2, exdent = 4)
  invisible(x)
}

# Columns named inside data.table calls in the mortality files.
utils::globalVariables(c(
  "i.site", "i.located", "located", "tag_end", "tag_life_days", "release", "first",
  "i.tag_end", "end_basis", "horizon", "i.pos", "pos", "slope", "intercept", "cal_unit",
  "cal_type", "raw", "value", "unit", "scale", "class", "kind", "vote", "score", "basis",
  "detail", "rule", "release_datetime", "unit_out", "i.class", "i.unit_out", "type", "step",
  "i.est"
))
