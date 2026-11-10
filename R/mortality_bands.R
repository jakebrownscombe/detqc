# ---- Mortality bands: votes to bands, and the log record ----

# What each deciding rule means, for the bands table and the cards.
band_rule_labels <- c(
  accel_flatline = "an accelerometer flatline",
  terminal_run = "a final stationary run with no alive vote",
  terminal_run_beside_activity = "a final stationary run beside an active accelerometer",
  terminal_run_split_site = "a final stationary run across a site boundary",
  terminal_run_moved_station = "a final run at one station whose position moved",
  terminal_run_near_stationary = "a final run within twice the site size",
  terminal_run_stray = "a final run at one site apart from stray detections on a few days",
  site_listening = "a record that ends when its last site stopped listening",
  movement_resumption = "death evidence, then movement after the candidate date",
  record_end = "a record that ends near tag life or the data horizon",
  persistence = "core stations as stable as a tag on the bottom",
  uncalibrated_sensor = "death evidence on a tag whose sensor readings are not calibrated",
  no_position = "a record with no station position",
  sparse_record = "a record too sparse to score",
  none = "no death evidence"
)

# ---- Internal: votes to bands ----
# The order of the cases is the order of the band rules in
# detect_mortality()'s help.
combine_votes <- function(run, data, ctl, censor_days = 30) {
  rec <- data$records
  if (!nrow(rec)) return(list(bands = empty_bands(), votes = empty_votes()))
  v <- run$votes
  per <- vote_summary(v, rec, censor_days)
  unc <- uncalibrated_animals(data$readings)
  per[, uncalibrated := animal_id %in% unc]
  term <- run$extras$terminal_run
  per[, term_sub := if (!is.null(term) && "subtype" %in% names(term)) {
    term$subtype[match(animal_id, term$animal_id)]
  } else {
    NA_character_
  }]
  per[, c("band", "deciding_rule") := decide_band(per, ctl$sparse_to_review)]
  per[, rule_label := label_rule(deciding_rule)]
  dd <- death_dates(v, per)
  per[dd, on = "animal_id", `:=`(death_date = i.date, death_date_basis = i.basis)]
  b <- band_columns(per, run, rec)
  b[, band := factor(band, levels = c("dead", "review", "alive"))]
  data.table::setorderv(b, c("band", "animal_id"))
  b[, band := as.character(band)]
  votes <- v[, list(animal_id, rule, vote, score, date, basis, detail)]
  data.table::setorderv(votes, c("animal_id", "rule"))
  list(bands = as_df(b), votes = as_df(votes))
}

# Per animal: which rules voted dead, alive or review, whether a censoring
# stream voted, and whether the death evidence begins within `censor_days`
# of the last detection.
vote_summary <- function(v, rec, censor_days) {
  s <- v[, list(
    dead = list(rule[kind == "vote" & vote == "dead"]),
    alive = list(rule[kind == "vote" & vote == "alive"]),
    review = list(rule[kind == "vote" & vote == "review"]),
    ranked = list(rule[kind == "rank" & vote == "review"]),
    censored = any(kind == "censor" & vote == "review"),
    death_start = min_or_na(date[kind == "vote" & vote %in% c("dead", "review")])
  ), by = "animal_id"]
  s <- s[rec[, list(animal_id, located, last)], on = "animal_id"]
  term <- v[rule == "terminal_run", list(animal_id, term_vote = vote)]
  s[term, on = "animal_id", term_vote := i.term_vote]
  s[, near_end := censored & !is.na(death_start) &
      as.numeric(difftime(last, death_start, units = "days")) <= censor_days]
  s
}

min_or_na <- function(d) {
  d <- d[!is.na(d)]
  if (length(d)) min(d) else empty_utc(1)
}

decide_band <- function(per, sparse_to_review) {
  n_dead <- lengths(per$dead)
  n_alive <- lengths(per$alive)
  n_review <- lengths(per$review)
  first <- function(l) vapply(l, function(z) if (length(z)) z[1] else NA_character_, "")
  conflict <- data.table::fcase(
    vapply(per$dead, function(z) "terminal_run" %in% z, TRUE) &
      vapply(per$alive, function(z) "accel_flatline" %in% z, TRUE),
    "terminal_run_beside_activity",
    vapply(per$alive, function(z) "movement_resumption" %in% z, TRUE), "movement_resumption",
    default = paste0(first(per$dead), "_beside_", first(per$alive))
  )
  sub <- c(split = "split_site", moved = "moved_station", near = "near_stationary",
           stray = "stray")
  review_rule <- as.character(ifelse(first(per$review) %in% "terminal_run" &
                                       per$term_sub %in% names(sub),
                                     paste0("terminal_run_", sub[per$term_sub]),
                                     first(per$review)))
  rule <- data.table::fcase(
    n_dead > 0 & n_alive > 0, conflict,
    n_dead > 0 & per$near_end, "record_end",
    n_dead > 0 & per$uncalibrated, "uncalibrated_sensor",
    n_dead > 0, first(per$dead),
    n_review > 0, review_rule,
    per$located == 0, "no_position",
    per$term_vote %in% "unavailable" & sparse_to_review, "sparse_record",
    per$censored, "record_end",
    lengths(per$ranked) > 0, first(per$ranked),
    default = "none"
  )
  band <- data.table::fcase(
    n_dead > 0 & n_alive == 0 & !per$near_end & !per$uncalibrated, "dead",
    rule == "none" | (rule == "sparse_record" & !sparse_to_review), "alive",
    default = "review"
  )
  list(band, rule)
}

label_rule <- function(rule) {
  out <- unname(band_rule_labels[rule])
  out[is.na(out)] <- gsub("_", " ", rule[is.na(out)])
  out
}

# Animals with a sensor transmitter whose readings are all raw. Such a tag
# cannot vote, and read as an animal with no sensor, a fish that is alive
# could reach the dead band on a terminal run alone. Death evidence on such
# an animal goes to review.
uncalibrated_animals <- function(readings) {
  if (is.null(readings) || !nrow(readings)) return(character())
  r <- readings[!is.na(animal_id)]
  if (!nrow(r)) return(character())
  tx <- r[, list(raw_only = all(scale == "raw")), by = c("animal_id", "transmitter")]
  unique(tx$animal_id[tx$raw_only])
}

# The candidate death date: the date of the deciding stream's vote for a
# dead animal, and the earliest death evidence for one in review.
death_dates <- function(v, per) {
  ev <- v[kind == "vote" & vote %in% c("dead", "review") & !is.na(date)]
  if (!nrow(ev)) return(data.table::data.table(animal_id = character(), date = empty_utc(),
                                               basis = character()))
  ev <- per[, list(animal_id, deciding_rule, band)][ev, on = "animal_id"]
  ev[, pick := data.table::fifelse(band == "dead", rule == deciding_rule, TRUE)]
  ev <- ev[pick == TRUE][order(animal_id, date)]
  ev <- unique(ev, by = "animal_id")
  ev[, basis := paste0(basis, " (", rule, ")")]
  ev[, list(animal_id, date, basis)]
}

# The bands table keeps the columns that the cards and the report use, with
# one vote column per stream.
band_columns <- function(per, run, rec) {
  b <- per[, list(animal_id, band, deciding_rule, rule_label, death_date, death_date_basis,
                  sensor_uncalibrated = uncalibrated)]
  wide <- data.table::dcast(run$votes, animal_id ~ rule, value.var = "vote")
  vote_cols <- setdiff(names(wide), "animal_id")
  data.table::setnames(wide, vote_cols, paste0(vote_cols, "_vote"))
  b <- wide[b, on = "animal_id"]
  b <- add_stream_columns(b, run$extras)
  b[rec, on = "animal_id", `:=`(detections = i.detections, located = i.located,
                                first_detection = i.first, last_detection = i.last,
                                expected_end = i.expected_end, end_basis = i.end_basis)]
  data.table::setnames(b, c("accel_flatline_vote", "terminal_run_vote"),
                       c("accel_vote", "terminal_vote"), skip_absent = TRUE)
  b
}

stream_columns <- list(
  terminal_run = c("terminal_site", "run_start", "run_detection_days", "run_calendar_days",
                   "window_diag_km", "detection_days"),
  accel_flatline = c(accel_transmitter = "transmitter", accel_onset = "flat_onset",
                     accel_last_active = "last_active", accel_flat_days = "flat_days"),
  persistence = c(persistence = "score")
)

add_stream_columns <- function(b, extras) {
  for (nm in names(stream_columns)) {
    cols <- stream_columns[[nm]]
    if (is.null(names(cols))) names(cols) <- cols
    e <- extras[[nm]]
    for (k in seq_along(cols)) {
      out <- names(cols)[k]
      b[, (out) := if (!is.null(e) && cols[[k]] %in% names(e)) {
        e[[cols[[k]]]][match(animal_id, e$animal_id)]
      } else {
        NA
      }]
    }
  }
  b
}

empty_bands <- function() {
  data.frame(animal_id = character(), band = character(), deciding_rule = character(),
             rule_label = character(), death_date = empty_utc(),
             death_date_basis = character(), stringsAsFactors = FALSE)
}

empty_votes <- function() {
  data.frame(animal_id = character(), rule = character(), vote = character(),
             score = numeric(), date = empty_utc(), basis = character(), detail = character(),
             stringsAsFactors = FALSE)
}

# ---- Internal: the log record ----
# The bands are not part of the detset, so detect_mortality() cannot append
# to `x$log` itself. Its record is written by log_mortality_bands() when the
# bands meet the detset again: in build_mortality_cards() and
# processing_report(). A second call with the same bands adds nothing.
bands_reason <- function(mb) {
  b <- mb$bands
  n <- function(bd) sum(b$band == bd)
  raw <- mb$settings$raw_accelerometers
  sprintf(paste("%s sorted by %s: dead %s, review %s, alive %s;",
                "sites of at most %g km (%s); with no located detection: %s;",
                "%s of %s raw, with no sensor vote;",
                "%s with death evidence and uncalibrated sensor readings sent to review;",
                "votes by stream: %s"),
          n_of(nrow(b), "animal"), n_of(nrow(mb$settings$rules), "rule stream"),
          fmt_n(n("dead")), fmt_n(n("review")), fmt_n(n("alive")), mb$settings$sites_km,
          n_of(mb$settings$n_sites, "site"), n_of(mb$settings$unlocated_animals, "animal"),
          fmt_n(raw[["raw"]]), n_of(raw[["accelerometers"]], "accelerometer"),
          n_of(sum(b$deciding_rule %in% "uncalibrated_sensor"), "animal"),
          stream_counts(mb$votes))
}

# "terminal_run: dead 7, review 4, none 8, unavailable 1; ..." in run order.
stream_counts <- function(votes) {
  rules <- unique(votes$rule)
  one <- function(r) {
    tab <- table(factor(votes$vote[votes$rule == r], levels = vote_values))
    tab <- tab[tab > 0]
    sprintf("%s: %s", r, paste(names(tab), tab, collapse = ", "))
  }
  paste(vapply(rules, one, ""), collapse = "; ")
}

# The record names every setting of every stream, and a digest of the bands
# and votes, so two runs that differ only in a setting both appear.
mortality_log_parameters <- function(s, mb) {
  streams <- vapply(s$rule_settings, function(z) sprintf("(%s)", deparse_parameters(z)), "")
  c(list(sites_km = s$sites_km, site_distance = s$site_distance,
         sparse_to_review = s$control$sparse_to_review,
         horizon = format(s$horizon, "%Y-%m-%d %H:%M:%S UTC")),
    as.list(streams), list(digest = bands_digest(mb)))
}

bands_digest <- function(mb) {
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f))
  saveRDS(list(mb$bands, mb$votes), f, compress = FALSE, version = 2)
  substr(unname(tools::md5sum(f)), 1, 12)
}

log_mortality_bands <- function(x, bands) {
  if (!inherits(bands, "mortality_bands")) stop_detqc("`bands` must come from detect_mortality().")
  s <- bands$settings
  params <- deparse_parameters(mortality_log_parameters(s, bands))
  done <- x$log$step %in% "detect_mortality" & x$log$parameters %in% params
  if (any(done)) return(x)
  reason <- s$reason
  if (!identical(as.integer(s$rows), nrow(x$detections))) {
    reason <- sprintf("%s; the bands were computed on %s and the set holds %s",
                      reason, n_of(s$rows, "detection"), fmt_n(nrow(x$detections)))
  }
  log_step(x, "detect_mortality", params, x, 0L, reason, fn = "detect_mortality")
}

# Columns named inside data.table calls in this file.
utils::globalVariables(c(
  "band", "deciding_rule", "rule_label", "death_date", "death_date_basis", "i.date",
  "i.basis", "pick", "term_vote", "i.term_vote", "censored", "death_start", "near_end",
  "term_sub", "ranked", "i.detections", "i.first", "i.last", "i.expected_end", "i.end_basis",
  "located", "uncalibrated", "raw_only"
))

# ---- The Stoney Lake study, prepared for the mortality rules ----

#' The Stoney Lake example study, prepared for the mortality rules
#'
#' [detqc_example] after the steps of `vignette("mortality-review")`:
#' [calibrate_sensors()] converts the depth readings of ST-02 and ST-03 to
#' metres, [filter_post_release()] removes the first 6 hours after release
#' (106 detections) and [filter_min_lag()] at 4,500 s removes 43 isolated
#' detections. A fourth table gives the true fate of each animal: ST-02 dies
#' at 02:00 UTC on 4 June 2024 and its tag is then heard only at station
#' S011; the other three live to the end of the record. No real data are
#' included with the package.
#'
#' The default rules of [mortality_rules()] read final runs of 14 days, and
#' the study runs from 2 to 10 June 2024, so they send all four animals to
#' review. Scaled to the study, as in the example below, they band ST-02
#' dead.
#'
#' @format A list of four data frames:
#' \describe{
#'   \item{detections}{3,148 detections, with the columns of
#'     `detqc_example$detections` and the depth in metres in `sensor_value`.}
#'   \item{deployments}{the 20 deployments of [detqc_example].}
#'   \item{animals}{the 4 animals of [detqc_example].}
#'   \item{fates}{one row per animal: `animal_id`, `fate` (`"alive"` or
#'     `"dead"`) and `death_datetime` (UTC).}
#' }
#' @source Built by
#'   <https://github.com/jakebrownscombe/detqc/blob/main/data-raw/detqc_example.R>, which
#'   is in the repository and not in the installed package.
#' @examples
#' study <- example_mortality
#' dets <- detset(study$detections, study$deployments, study$animals)
#' # Rules scaled to a study of 2 to 10 June 2024
#' mort_rules <- mortality_rules(min_days = 3, min_calendar_days = 3,
#'                          min_flat_days = 2, within_days = 2)
#' bands <- detect_mortality(dets, rules = mort_rules)$bands
#' bands[, c("animal_id", "band", "deciding_rule")]
#' study$fates
"example_mortality"
