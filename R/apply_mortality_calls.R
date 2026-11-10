# ---- Apply the reviewer's calls, then filter ----

#' Bring the reviewer's calls back onto a detection set
#'
#' Reads the CSV exported from the review index or a card and stores each
#' animal's call, death date and note in `x$mortality`. Calls from a later
#' file replace earlier calls for the same animals. The step is logged with
#' the file's name and MD5 checksum.
#'
#' The file needs `animal_id` and `call`; `death_date`, `death_time`,
#' `date_source` (how the date was set on the page: the candidate date, the
#' cursor, typed, or a day note), `note`, `band` and `annotated_at` are read
#' when present. `death_time` is a UTC time, such as "2024-06-04 02:00:00" or
#' "2024-06-04T02:00:00Z"; a review card writes it when a call takes the
#' candidate date, which is the time of a detection. A death time sets the
#' death date to the UTC day it falls on. A file
#' that names an animal the detset does not hold is refused whole, because a
#' file from another study or a renamed animal would otherwise apply in part.
#' Calls must be `alive`,
#' `dead_or_shed` or `uncertain`, and every animal must be in the detset: in
#' its detections or in its animals table. An animal of the animals table
#' with no detection in the set keeps its call, which removes nothing, and a
#' message counts such animals, so a calls file for a whole study applies to
#' a month cut from it. A
#' death date left on an alive call is dropped and counted. A `dead_or_shed`
#' call with no date is kept, and [filter_mortality()] leaves its detections
#' in place; a warning names these animals. A warning also names animals whose
#' death date falls before their first detection, which removes the whole
#' record. When the file has a `candidate_date` column, as the review pages
#' export, the log counts the dated calls more than 30 days from it, and a
#' warning names the animals whose death date falls more than 30 days before
#' it, because [filter_mortality()] removes the detections in between.
#'
#' @param x A `detset`.
#' @param file Path to the exported CSV.
#' @return `x` with `x$mortality`, a data frame of `animal_id`, `call`,
#'   `death_date` (Date, UTC), `death_time` (POSIXct, UTC, or `NA` for a call
#'   with a date only), `date_source`, `note`, `band` (the band the
#'   animal was in when it was called), `annotated_at`, `file` and `md5`, and a
#'   new log row.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' calls <- data.frame(animal_id = c("ST-02", "ST-01"),
#'                     call = c("dead_or_shed", "alive"),
#'                     death_date = c("2024-06-04", ""), note = "")
#' calls_file <- tempfile(fileext = ".csv")
#' utils::write.csv(calls, calls_file, row.names = FALSE)
#' dets <- apply_mortality_calls(dets, calls_file)
#' dets$mortality
apply_mortality_calls <- function(x, file) {
  validate_detset(x)
  if (!is.character(file) || length(file) != 1 || !file.exists(file)) {
    stop_detqc("File not found: %s", paste(file, collapse = ", "))
  }
  raw <- utils::read.csv(file, colClasses = "character", na.strings = character(),
                         check.names = FALSE, encoding = "UTF-8")
  names(raw) <- clean_names(names(raw))
  if (!all(c("animal_id", "call") %in% names(raw))) {
    stop_detqc("%s needs the columns animal_id and call.", basename(file))
  }
  calls <- read_calls(raw, x, basename(file))
  checks <- check_call_dates(calls, x)
  md5 <- unname(tools::md5sum(file))
  calls$file <- basename(file)
  calls$md5 <- md5
  x$mortality <- merge_calls(x$mortality, calls)
  tab <- table(factor(calls$call, levels = call_levels))
  reason <- sprintf(
    paste("%s read: %s alive, %s dead_or_shed, %s uncertain;",
          "%s dropped from alive calls; %s with no date;",
          "%s before the first detection; %s;",
          "%s with no call"),
    n_of(nrow(calls), "call"), fmt_n(tab[["alive"]]), fmt_n(tab[["dead_or_shed"]]),
    fmt_n(tab[["uncertain"]]),
    n_of(attr(calls, "dropped_dates"), "death date"),
    n_of(length(checks$undated), "dead_or_shed call"),
    n_of(length(checks$early), "death date"), checks$far, n_of(attr(calls, "skipped"), "row")
  )
  before <- c(nrow(x$detections), count_animals(x$detections))
  log_step(x, "apply_mortality_calls", sprintf("file=%s; md5=%s", basename(file), md5),
           before, 0L, reason)
}

#' Remove detections after each death date
#'
#' For every animal called `dead_or_shed` with a death time, drops its
#' detections after the death time less `margin_days` days. A call with a
#' death date and no time drops the detections on the UTC dates after the
#' death date less `margin_days`; with `margin_days = 0` it keeps the whole
#' death day. Nothing else is removed:
#' alive and uncertain animals keep every detection, and a `dead_or_shed`
#' call with no date keeps its record. Every filter in detqc is applied only
#' when called.
#'
#' The margin allows for a death the reviewer dates late: a tag that settled
#' on the bottom can look alive for a few days before the record shows it. A
#' margin longer than the time between release and death removes every
#' detection of that animal.
#'
#' @param x A `detset` with calls from [apply_mortality_calls()].
#' @param margin_days Days before each death time (or date) from which the
#'   detections are removed. 0 removes only the detections after the death.
#' @return `x` with the detections removed and a new log row that counts them
#'   under the reason `after_death_date` and states the margin.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' # ST-02 died at 02:00 UTC on 4 June 2024 (example_mortality$fates).
#' calls <- data.frame(animal_id = "ST-02", call = "dead_or_shed",
#'                     death_time = "2024-06-04 02:00:00")
#' calls_file <- tempfile(fileext = ".csv")
#' utils::write.csv(calls, calls_file, row.names = FALSE)
#' dets <- apply_mortality_calls(dets, calls_file)
#' # The default margin is 7 days; margin_days = 1 cuts from 02:00 on 3 June.
#' dets <- filter_mortality(dets, margin_days = 1)
#' detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")]
filter_mortality <- function(x, margin_days = 7) {
  validate_detset(x)
  need_number(margin_days, "margin_days")
  m <- x$mortality
  if (is.null(m)) stop_detqc("No calls on `x`. Run apply_mortality_calls() first.")
  if (!"death_time" %in% names(m)) m$death_time <- as.POSIXct(NA, tz = "UTC")
  dead <- m[m$call == "dead_or_shed" & !is.na(m$death_date),
            c("animal_id", "death_date", "death_time")]
  d <- x$detections
  cut <- match(d$animal_id, dead$animal_id)
  timed <- !is.na(cut) & !is.na(dead$death_time[cut])
  drop <- !is.na(cut) & as.Date(d$datetime) > dead$death_date[cut] - margin_days
  drop[timed] <- d$datetime[timed] > dead$death_time[cut[timed]] - margin_days * 86400
  before <- c(nrow(d), count_animals(d))
  x$detections <- d[!drop, , drop = FALSE]
  rownames(x$detections) <- NULL
  hit <- length(unique(d$animal_id[drop]))
  reason <- sprintf(
    paste("%s of %s called dead_or_shed removed after each death time less a margin of %s,",
          "or from the UTC day after the death date less the margin for a date with no",
          "time (%s with a time); %s called dead_or_shed with a date had none after it;",
          "%s called dead_or_shed without a date kept whole; %s called uncertain and %s",
          "called alive kept whole"),
    n_of(sum(drop), "detection"), n_of(hit, "animal"), n_of(margin_days, "day"),
    n_of(sum(!is.na(dead$death_time)), "call"), n_of(nrow(dead) - hit, "animal"),
    n_of(sum(m$call == "dead_or_shed" & is.na(m$death_date)), "animal"),
    n_of(sum(m$call == "uncertain"), "animal"), n_of(sum(m$call == "alive"), "animal")
  )
  removed <- if (any(drop)) c(after_death_date = sum(drop)) else 0L
  log_step(x, "filter_mortality",
           sprintf(paste("calls=%s; margin_days=%s; rule=datetime > death_time - margin,",
                         "else date > death_date - margin"), fmt_n(nrow(m)), format(margin_days)),
           before, removed, reason)
}

# Calls from a later file replace earlier calls for the same animals. A store
# that lacks a store column gets it as NA.
merge_calls <- function(old, calls) {
  calls <- calls[, store_columns]
  if (!is.null(old)) {
    for (cl in setdiff(store_columns, names(old))) {
      old[[cl]] <- if (cl == "death_time") as.POSIXct(NA, tz = "UTC") else NA
    }
    calls <- rbind(old[!old$animal_id %in% calls$animal_id, store_columns, drop = FALSE], calls)
  }
  calls <- calls[order(calls$animal_id), , drop = FALSE]
  rownames(calls) <- NULL
  calls
}
