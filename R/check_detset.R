# ---- Data-entry and deployment checks ----

#' Check detections, deployments and animals for data-entry errors
#'
#' Runs the data-entry and deployment checks
#' and reports what each found. A check never changes the data: it returns
#' the offending rows with counts and denominators, and the user decides what
#' to fix. [all_checks()] lists the checks with their groups.
#'
#' Some errors explain others. A deployment whose latitude and longitude are
#' swapped also lies outside the study extent and gives its station a second
#' coordinate set; detections orphaned by a deployment date a year off also
#' fall outside every deployment. Each such row is reported once, by the most
#' specific check: swapped coordinates, then a wrong sign, then wrong units,
#' then outside the extent; a year off or a time-zone offset before
#' detections outside the deployment period. The checks that defer say so in
#' their `note`.
#'
#' Checks that need data not supplied report `"not run"` with the reason:
#' the land check needs `water`, the depth check a raster in `water` and a
#' `deploy_z` column, the two download checks a `download_datetime` column, the
#' animal checks an animals table, `long_after_release` the setting
#' `max_liberty_years`, and the two checks against the detection
#' field need `field`. A check with nothing to test, a denominator of 0,
#' also reports `"not run"`.
#'
#' Two checks catch a tag code entered wrong in the tagging records.
#' `transmitter_no_animal` lists the transmitters heard that no animal
#' carries, most heard first, so a code heard thousands of times stands above
#' the stray codes of other projects; `animal_never_heard` lists the animals
#' whose transmitters were never heard.
#'
#' `detections_after_download` counts, per deployment, the detections after
#' its recorded download (`download_datetime`) and before it ends. The
#' deployment's data end at that download, so [audit_listening()] keeps those
#' days `"not_downloaded"`, and [detset()] marks the detections in the column
#' `after_download`. They can mean a download time recorded too early, a
#' later download missing from the sheet, a receiver that kept logging after
#' the download, or a clock problem. Its rows give, per deployment, the
#' download, the count, the first and last such detection and the UTC days
#' they fall on. The check counts deployments whose download comes before
#' their end; the note counts the detections.
#'
#' Three checks read every transmitter: `receiver_silent`, `moved_mooring`
#' and the co-detection checks. Cut to one species, the detections make a
#' receiver that heard only other species look silent and change its
#' co-detection partners; run the checks on every transmitter the receivers
#' logged, reference tags included.
#'
#' Limits of three checks, each measured on test arrays:
#' * `moved_mooring` compares the stations that co-detect with a deployment
#'   before and after each date. With `reference_tags`, a deployment with
#'   enough co-detections of reference tags is tested on those alone; the
#'   others are tested on animal co-detections, which also change when the
#'   animals shift their range, so their flags are candidates to confirm
#'   against field logs. Moves shorter than about two receiver spacings
#'   score below the default threshold: on the 600 m test grid, moves of 600
#'   and 849 m scored 0.40 to 0.69 and were missed, and 1,200 m scored 0.87.
#' * `dst_jump` sees only a step back in the order the receiver logged its
#'   rows. A jump forward is never visible, nor a jump on a receiver that
#'   logged nothing in the `dst_min` minutes before it, and a table sorted by
#'   time hides every jump. It flags or reports `"not run"`; it never passes.
#' * `on_land` takes a position exactly on the edge of a raster cell as
#'   terra assigns it, which can be the dry neighbour, and a position on a
#'   polygon boundary as sf assigns it. The two forms of one water body can
#'   disagree at such points.
#'
#' @param x A [detset()], or a list of plain tables `detections`,
#'   `deployments` and, optionally, `animals`, with the contract columns.
#'   Plain tables let the checks see errors that [detset()] refuses, such as
#'   a recovery before its deployment.
#' @param checks Names of the checks to run; see [all_checks()].
#' @param settings Thresholds from [check_settings()].
#' @param water Optional water body: an `sf` polygon (water inside), or a
#'   `terra` raster of water depth in metres, positive down, with `NA` on
#'   land. A raster also serves the depth check. The two forms count
#'   differently: a raster tests only the positions inside its extent, and
#'   its note counts the rest as off the layer; a polygon tests every
#'   position, and every position outside it is on land. So a polygon must
#'   cover every deployment of the detset, or the deployments outside it
#'   count as on land.
#' @param field Optional detection field for the two field checks: one number,
#'   the detection range in metres along a straight line; a `de_field` from
#'   the detfield package; or `list(distances = , range_m = )`, with a
#'   `water_distances` table from detfield and a range in metres. A field
#'   over some of the stations judges only them: detections at a receiver it
#'   does not hold are not tested, and the check's note counts them.
#' @param reference_tags Optional data frame of reference tags: `transmitter`
#'   and their stated position, `deploy_lat` and `deploy_lon`. Their
#'   detections are not counted as unknown transmitters. When it is `NULL`
#'   and the deployments carry `receiver_transmitter` (see
#'   [read_deployments()]), those ids are the reference tags, each at the
#'   position of its latest deployment, with a message; an id the animals
#'   table also lists is left out with a warning. Give `reference_tags` to
#'   use your own instead.
#'
#' @return For a detset, the detset with the results in `checks` and one log
#'   record. For plain tables, the results alone. The results are a list of
#'   class `detset_checks`: `summary`, a data frame with one row per check
#'   (`check`, `group`, `status` of `"pass"`, `"flag"` or `"not run"`,
#'   `flagged`, `of`, `unit` and `note`), and `rows`, the offending rows of
#'   each check that flagged, named by check.
#' @seealso [all_checks()], [check_settings()]
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' dets <- check_detset(dets, checks = c("duplicate_rows", "receiver_silent"))
#' dets$checks
check_detset <- function(x, checks = all_checks(), settings = check_settings(), water = NULL,
                         field = NULL, reference_tags = NULL) {
  bad <- setdiff(checks, check_registry$check)
  if (length(bad)) stop_detqc("Unknown check `%s`; see all_checks().", bad[1])
  is_set <- inherits(x, "detset")
  field_checks <- c("codetection_out_of_range", "reference_position")
  if (is.null(field) && any(field_checks %in% checks)) {
    message("The two checks against the detection field need `field`; they did not run.")
  }
  if (is.null(reference_tags) && is.list(x)) {
    reference_tags <- deployment_reference_tags(x$deployments, x$animals, "check_detset")
  }
  ctx <- check_context(x, settings, water, field, reference_tags)
  todo <- check_registry[check_registry$check %in% checks, , drop = FALSE]
  res <- lapply(seq_len(nrow(todo)), function(i) {
    out <- check_fn(todo$check[i])(ctx)
    out$check <- todo$check[i]
    out
  })
  report <- assemble_checks(res, todo)
  if (!is_set) return(report)
  before <- x
  x$checks <- report
  s <- report$summary
  log_step(x, "check_detset", list(checks = checks, water = !is.null(water),
                                   field = !is.null(field)),
           before, 0L, sprintf(
             "%s: %s flagged, %s passed, %s not run%s", n_of(nrow(s), "check"),
             fmt_n(sum(s$status == "flag")), fmt_n(sum(s$status == "pass")),
             fmt_n(sum(s$status == "not run")),
             if (any(s$status == "flag")) {
               paste0("; flagged: ", paste(s$check[s$status == "flag"], collapse = ", "))
             } else {
               ""
             }
           ), fn = "check_detset")
}

#' The data-entry and deployment checks
#'
#' Lists every check [check_detset()] can run, in the order it runs them,
#' with its group and what its counts count.
#'
#' @param group Optional group names to keep: `"Receivers"`, `"Animals and
#'   tags"`, `"Time"` or `"Against the detection field"`.
#' @return A character vector of check names. The attribute `table` holds a
#'   data frame with `check`, `group`, `unit` and `description`.
#' @export
#' @examples
#' checks <- all_checks()
#' length(checks)
#' head(c(checks))
#' time_checks <- attr(all_checks("Time"), "table")
#' time_checks[, c("check", "unit")]
#' writeLines(strwrap(paste0(time_checks$check, ": ", time_checks$description),
#'                    width = 72, exdent = 2))
all_checks <- function(group = NULL) {
  reg <- check_registry
  if (!is.null(group)) {
    bad <- setdiff(group, unique(reg$group))
    if (length(bad)) stop_detqc("Unknown group `%s`.", bad[1])
    reg <- reg[reg$group %in% group, , drop = FALSE]
  }
  rownames(reg) <- NULL
  structure(reg$check, table = reg)
}

#' Thresholds of the data-entry checks
#'
#' Returns the thresholds [check_detset()] uses, with any given here in place
#' of the defaults. Each default is a stated choice that the data do not
#' fix; change it when the study needs another.
#'
#' * `extent`: the study extent as `c(lon_min, lon_max, lat_min, lat_max)`.
#'   Without it, a deployment is outside the extent when it lies further
#'   from the median deployment position than both `outlier_km` and
#'   `outlier_mult` times the median distance of the deployments from that
#'   position.
#' * `outlier_km` (20) and `outlier_mult` (10): see `extent`.
#' * `station_tol_m` (100): one station's coordinate sets further apart than
#'   this are two coordinate sets.
#' * `reuse_m` (1000): one station's coordinate sets further apart than this
#'   are a station name reused at another place.
#' * `same_place_m` (10): two station names closer than this share one place.
#' * `year_off_days` (300): a deployment that starts this long before its
#'   first detection, or ends this long after its last, is a year off.
#' * `download_tol_s` (3600): a recovery more than this after the download.
#' * `depth_tol_m` (1): a receiver deeper than the bathymetry by more than
#'   this.
#' * `codetect_s` (5): two receivers that log one transmitter within this
#'   many seconds heard one transmission.
#' * `move_min` (20) and `move_tv` (0.75): a deployment's co-detection
#'   partners change when, at some date with at least `move_min`
#'   co-detections on each side, the total variation distance between the
#'   partner shares before and after reaches `move_tv`. On a 600 m grid a
#'   move of one spacing scored 0.40 to 0.61, below the threshold.
#' * `tz_window_h` (15): detections this close outside a deployment boundary
#'   count towards a time-zone offset. An offset is confirmed only where
#'   the deployments detecting near the other end leave it empty, so a
#'   receiver that logs on deck after recovery is not read as one.
#' * `dst_min` (30) and `dst_max` (90): a step back in time of this many
#'   minutes, in the order the rows arrived, marks a daylight-saving jump.
#'   Rows must be in the order each receiver logged them; see
#'   [check_detset()] for what the check cannot see.
#' * `p_min` (0.001): a receiver hears a position when the detection field
#'   gives it at least this probability there.
#' * `max_liberty_years` (no default): detections more than this many years
#'   after release are flagged by `long_after_release`, which runs without a
#'   tag life, as for a GLATOS export. Give the longest life of the study's
#'   tag models, from the manufacturer's specifications.
#' * `reappear_gap_days` (365): a transmitter silent for longer than this,
#'   then heard only at stations it never used before, is flagged by
#'   `reappeared_elsewhere`: a code reused by another project, or a tag
#'   moved by a person.
#' * `duplicate_tol_s` (0): two detections of one transmitter on one receiver
#'   this many seconds apart or less are one detection, as in
#'   [filter_duplicates()]; 0 matches exact stamps only.
#'
#' @param ... Named thresholds to change.
#' @return A named list of every threshold.
#' @export
#' @examples
#' check_settings(station_tol_m = 50)$station_tol_m
check_settings <- function(...) {
  given <- list(...)
  if (length(given) && (is.null(names(given)) || any(!nzchar(names(given))))) {
    stop_detqc("Every setting needs a name.")
  }
  bad <- setdiff(names(given), names(check_defaults))
  if (length(bad)) stop_detqc("Unknown setting `%s`; see ?check_settings.", bad[1])
  out <- utils::modifyList(check_defaults, given)
  check_setting_values(out)
  out
}

check_setting_values <- function(s) {
  yrs <- s$max_liberty_years
  if (!is.null(yrs) && !is_positive_number(yrs)) {
    stop_detqc("`max_liberty_years` must be one positive number.")
  }
  ext <- s$extent
  if (!is.null(ext) && (!is.numeric(ext) || length(ext) != 4 || anyNA(ext))) {
    stop_detqc("`extent` must be c(lon_min, lon_max, lat_min, lat_max).")
  }
  invisible(TRUE)
}

check_defaults <- list(
  extent = NULL, outlier_km = 20, outlier_mult = 10, station_tol_m = 100, reuse_m = 1000,
  same_place_m = 10, year_off_days = 300, download_tol_s = 3600, depth_tol_m = 1,
  codetect_s = 5, move_min = 20, move_tv = 0.75, tz_window_h = 15, dst_min = 30, dst_max = 90,
  p_min = 0.001, duplicate_tol_s = 0, max_liberty_years = NULL, reappear_gap_days = 365
)

#' @export
print.detset_checks <- function(x, ...) {
  s <- x$summary
  cat(sprintf("<detset_checks> %s: %s flagged, %s passed, %s not run\n", n_of(nrow(s), "check"),
              fmt_n(sum(s$status == "flag")), fmt_n(sum(s$status == "pass")),
              fmt_n(sum(s$status == "not run"))))
  show <- s[, c("check", "status", "flagged", "of", "unit")]
  print(show, row.names = FALSE)
  for (i in which(s$status != "pass" & nzchar(s$note))) {
    cat_wrap(sprintf("%s: %s", s$check[i], s$note[i]), indent = 2, exdent = 4)
  }
  invisible(x)
}

# ---- Internal: the registry ----
check_registry <- data.frame(
  check = c(
    "coordinate_units", "swapped_lat_lon", "wrong_sign", "outside_extent", "on_land",
    "station_coordinates", "station_reused", "overlapping_deployments", "recover_before_deploy",
    "deploy_year_off", "recover_after_download", "detections_after_download",
    "depth_below_bottom", "serial_not_deployed",
    "receiver_silent", "moved_mooring",
    "transmitter_no_animal", "animal_never_heard", "before_release", "after_tag_life",
    "long_after_release",
    "reappeared_elsewhere", "animal_conflict", "duplicate_animal_id",
    "non_utc", "dst_jump", "outside_deployment", "duplicate_rows",
    "codetection_out_of_range", "reference_position"
  ),
  group = c(rep("Receivers", 16), rep("Animals and tags", 8), rep("Time", 4),
            rep("Against the detection field", 2)),
  unit = c(
    rep("deployments", 13), "detections", "deployments", "deployments",
    "transmitters", "animals", "detections", "detections", "detections", "transmitters",
    "animals",
    "animals",
    "deployments", "receivers", "detections", "detections",
    "co-detections", "detections"
  ),
  description = c(
    "coordinates not in decimal degrees (projected metres or degrees and minutes)",
    "latitude and longitude swapped",
    "latitude or longitude with the wrong sign",
    "a position outside the study extent that no swap or sign explains",
    "a receiver position on land",
    "one station with two coordinate sets, or two stations at one place",
    "a station name reused at another place with no new station",
    "deployments of one receiver that overlap in time",
    "a recovery before its deployment",
    "a deployment or recovery date a year off against the detections",
    "a recovery after the receiver's data were downloaded",
    paste("detections after the deployment's recorded download, where its data end:",
          "a download time recorded early, a receiver that kept logging, or a clock problem"),
    "a stated receiver depth deeper than the bathymetry",
    "detections at a receiver serial no deployment names",
    "a deployment that records no detection",
    "a deployment whose co-detection partners change part-way through",
    paste("a transmitter that no animal carries, listed most heard first: one heard",
          "thousands of times is likely a code entered wrong in the tagging records"),
    "an animal in the tagging records with no detection of its transmitter",
    "detections before the animal's release",
    "detections after the tag's expected life",
    "detections more years after release than any tag of the study lasts",
    "a transmitter silent for a long gap, then heard only at stations it never used",
    "one animal under two species or two sizes",
    "one animal_id on two release records",
    "times that are not UTC: a time-zone attribute or a whole-hour offset at deployment ends",
    "a daylight-saving jump: a receiver's time steps back by about an hour",
    "detections at a known receiver outside all its deployments",
    "duplicated detection rows",
    "co-detection within seconds by receivers the field puts out of range of each other",
    paste("a reference tag heard by a receiver that cannot hear its stated position",
          "(given, or its deployment's position for a receiver_transmitter id)")
  ),
  stringsAsFactors = FALSE
)

# The function of a check is named after it, with the prefix `chk_`.
check_fn <- function(name) {
  get(paste0("chk_", name), mode = "function", envir = asNamespace("detqc"))
}

# One result: status, counts, note and the offending rows. A check with
# nothing to test reports "not run", because "pass" over a denominator of 0
# reads as a clean result.
check_result <- function(flagged, of, note = "", rows = NULL) {
  if (of == 0 && flagged == 0) {
    return(not_run(paste0("nothing to test, the denominator is 0; ", note)))
  }
  list(status = if (flagged > 0) "flag" else "pass", flagged = as.integer(flagged),
       of = as.integer(of), note = note, rows = rows)
}

not_run <- function(note) {
  list(status = "not run", flagged = NA_integer_, of = NA_integer_, note = note, rows = NULL)
}

assemble_checks <- function(res, todo) {
  summary <- data.frame(
    check = todo$check, group = todo$group,
    status = vapply(res, `[[`, "", "status"),
    flagged = vapply(res, function(r) r$flagged, integer(1)),
    of = vapply(res, function(r) r$of, integer(1)),
    unit = todo$unit, note = vapply(res, `[[`, "", "note"),
    stringsAsFactors = FALSE
  )
  rows <- lapply(res[summary$status == "flag"], function(r) as_df(r$rows))
  names(rows) <- summary$check[summary$status == "flag"]
  structure(list(summary = summary, rows = rows), class = "detset_checks")
}
