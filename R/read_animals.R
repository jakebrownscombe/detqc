# ---- Read animal, tag and tag specification records ----

#' Read animal and tag records from an OTN, GLATOS or detqc sheet
#'
#' Reads tagging records into the contract's animals and tags table, one row
#' per animal and transmitter. It reads the OTN tagging sheet (Excel, header row found by
#' its names), the OTN AAT tag release file, the tagging sheet of a GLATOS
#' project workbook, and GLATOS detection exports, whose animal columns repeat
#' on every row and are reduced to one row per animal and transmitter. A
#' transmitter reused on a second animal keeps one row per animal, with a
#' warning that names it; [detset()] then gives each detection to the animal
#' released last before it. A GLATOS workbook's release times are local, in
#' `glatos_release_date_time` at the zone in `glatos_timezone`; where
#' `utc_release_date_time` is blank they are converted to UTC.
#'
#' **Format.** `"auto"` takes the OTN or GLATOS map, whichever fills more of
#' the contract columns from the file's columns, and OTN on a tie, as
#' [read_deployments()] does; a warning names a contract column the other
#' format would fill and the chosen one leaves empty, such as
#' `release_datetime` of a GLATOS workbook read with `format = "otn"`.
#' Where that column is one the table needs, such as `transmitter` of a
#' GLATOS detection export read with `format = "otn"`, the reader stops and
#' names the format `"auto"` picks. A format other than the four stops
#' with the formats listed. A
#' file with detqc's own column names, such as a detset's `animals` written
#' with `write.csv()`, fills the most contract columns and is read as
#' `"detqc"`. A blank-headed first column of unique whole numbers, the row
#' names `write.csv()` writes by default, is dropped.
#'
#' **OTN tagging sheets and detection extracts.** The OTN tagging template
#' often leaves `ANIMAL_ID` blank, while the OTN matched extract names each
#' animal by its `catalogNumber`. Read such a sheet with
#' `missing_id = "transmitter"`: [detset()] then warns that the extract's
#' `animal_id` disagrees with the animals table, and [filter_release()],
#' [filter_post_release()] and [filter_tag_life()] take the release of a
#' detection whose `animal_id` the animals table does not list by its
#' transmitter, with a message that counts them.
#'
#' **Rows with no animal or no transmitter.** A row needs both. Rows that lack
#' one are dropped with a warning that counts each cause apart and names an
#' example, and the attribute `source` keeps the counts
#' (`dropped_no_animal_id`, `dropped_no_transmitter`). In an OTN sheet a
#' blank `TAG_CODE_SPACE` leaves the transmitter blank though the animal has
#' an id. With `tag_specs`,
#' such a row takes its transmitters from the tag specifications by
#' `tag_serial`. A GLATOS detection export can leave `animal_id` blank on
#' every row; `missing_id = "transmitter"` then names each animal by its
#' transmitter, as glatos does. When every row would be dropped,
#' `read_animals()` stops. It stops too when more than half the rows have no
#' `animal_id`, with an error that names the columns it read the id from,
#' or no transmitter after `tag_specs` has filled what it can, with an
#' error that names the blank code space. Exactly half is a warning.
#'
#' **Tags that send two codes.** A tag with a pinger and a sensor, or two
#' sensors, sends two transmitter codes, and tagging sheets often list one.
#' With `tag_specs`, every code the specifications list for an animal's
#' `tag_serial` gets its own row on that animal, with the tag columns of the
#' specifications. The second codes can carry a large share of an
#' animal's detections.
#'
#' **Tag columns from the specifications.** Sensor calibrations and
#' transmission delays are not in the tagging sheets. They are in the
#' manufacturer's tag specification sheet, which [read_tag_specs()] reads.
#' With `tag_specs`, every row whose transmitter the specifications list
#' takes their tag columns (`tag_model`, `tag_life_days`, `min_delay`,
#' `max_delay`, `sensor_type`, `sensor_unit`, `sensor_slope` and
#' `sensor_intercept`) where its own value is missing; the codes the sheet
#' lists and the second codes added from the specifications then agree. A
#' value the sheet gives is kept; where it differs from the specifications,
#' a warning counts the cells by column and names one. Tag serials of the
#' sheet that the specifications do not list get a warning too, sensor tags
#' first: such a tag loses its second code and its calibration, so find its
#' vendor sheet. A code the sheet gives for a serial whose specifications
#' list other codes, such as A69-9002-13710 where the specifications list
#' A69-9007-13710 for the same serial, is most often a slip in the sheet's
#' code space. It is kept beside the specifications' codes, and a warning
#' names each such pair, so that the slip can be checked against the
#' detections and dropped. The attribute `source` counts the cells filled
#' (`spec_cells_filled`), the differing cells (`spec_conflicts`), the
#' serials not listed (`serials_not_in_specs`) and the sheet codes not
#' listed for their serial (`codes_not_in_specs`).
#'
#' @inheritParams read_deployments
#' @param tag_specs Optional result of [read_tag_specs()], or a data frame
#'   with `transmitter` and `tag_serial`: the codes of each tag serial.
#' @param format One of `"auto"`, `"otn"`, `"glatos"` or `"detqc"`.
#' @param missing_id `"drop"` (default) drops rows with no `animal_id`;
#'   `"transmitter"` names such an animal by its transmitter.
#' @return A data frame, one row per animal and transmitter, with
#'   `animal_id` and `transmitter` and, where the sheet gives them,
#'   `species`, `release_datetime` (UTC), `release_lat`, `release_lon`,
#'   `tag_model`, `tag_serial`, `tag_life_days`, `min_delay` and `max_delay`
#'   (s), `sensor_type`, `sensor_unit`, `sensor_slope` and
#'   `sensor_intercept`, then any other mapped column such as `length`.
#'   The OTN tagging template states length in metres and weight in
#'   kilograms, so for `format = "otn"` the units are added as `length_unit`
#'   (`"m"`) and `weight_unit` (`"kg"`) where the sheet has those columns,
#'   and the review cards print them. The GLATOS workbook names no unit for
#'   `LENGTH` or `WEIGHT`, so for `format = "glatos"` no unit column is added
#'   and the cards print the value with "(unit as recorded)".
#' @export
#' @examples
#' csv_file <- system.file("extdata", "stoney_detections.csv", package = "detqc")
#' animals <- read_animals(csv_file, format = "glatos")
#' animals[, c("animal_id", "transmitter", "species", "release_datetime")]
read_animals <- function(file, format = c("auto", "otn", "glatos", "detqc"), col_map = NULL,
                         tz = NULL, tag_specs = NULL, missing_id = c("drop", "transmitter")) {
  format <- check_format_arg(format, c("auto", "otn", "glatos", "detqc"))
  missing_id <- check_format_arg(missing_id, c("drop", "transmitter"), "missing_id")
  map <- get_column_map("animals", if (format == "auto") "otn" else format, col_map)
  if (!is.null(tz)) map$tz[map$type %in% c("datetime", "date")] <- check_tz(tz)
  raw <- read_raw_table(file, header_names = sheet_header_names("animals", format, map))
  prep <- function(r, fm) if (fm == "glatos") glatos_local_times(r) else r
  # A full map of the user's own replaces the packaged ones, so they are not
  # compared with it. Pairs are merged into the map of the format picked.
  picked <- if (is.null(col_map) || format == "auto") {
    sheet_format(raw, "animals", format, file, prep, warn = is.null(col_map))
  } else {
    format
  }
  map <- picked_map(map, "animals", format, picked, col_map)
  if (format == "auto" && !is.null(tz)) map$tz[map$type %in% c("datetime", "date")] <- tz
  format <- picked
  raw <- prep(raw, format)
  out <- apply_column_map(raw, map, "animals", keep_unmapped = FALSE)
  dt <- unique(as_dt(out$data))
  if (missing_id == "transmitter") dt[is.na(animal_id), animal_id := transmitter]
  specs <- spec_codes(tag_specs)
  filled <- fill_from_specs(dt, specs)
  dt <- filled$dt
  # Counted before rows with no transmitter are dropped: a serial absent from
  # the specifications is often why a row has none.
  absent <- warn_serials_not_in_specs(dt, specs)
  drops <- count_drops(dt, basename(file), map$source[map$target == "animal_id"])
  dt <- dt[!is.na(transmitter) & !is.na(animal_id)]
  n_ided <- nrow(dt)
  # One row per animal and transmitter. A tag can be reused: recovered from
  # one animal and put in another. Rows keyed on the transmitter alone keep
  # only the first animal, and the detections of the second go to the first.
  # A reused tag therefore keeps one row per animal.
  dt <- unique(dt, by = c("animal_id", "transmitter"))
  stray <- warn_codes_not_in_specs(dt, specs)
  second <- add_second_codes(dt, specs)
  dt <- second$dt
  from_specs <- fill_spec_columns(dt, specs)
  dt <- from_specs$dt
  reused <- warn_reused_tags(dt)
  dt <- add_size_units(dt, format)
  df <- order_columns(as_df(dt), contract_spec$animals$column)
  check_columns(df, contract_spec$animals, "animals")
  attr(df, "source") <- c(source_record(file, format, out, nrow(df), rows_read = nrow(raw)),
                          list(dropped_no_id = drops[["no_animal_id"]] +
                                 drops[["no_transmitter"]],
                               dropped_no_animal_id = drops[["no_animal_id"]],
                               dropped_no_transmitter = drops[["no_transmitter"]],
                               transmitters_from_specs = filled$n,
                               second_codes = second$n,
                               spec_cells_filled = from_specs$filled,
                               spec_conflicts = from_specs$conflicts,
                               serials_not_in_specs = absent,
                               codes_not_in_specs = stray,
                               dropped_repeat_rows = n_ided + second$n - nrow(df),
                               reused_transmitters = reused,
                               local_times = attr(raw, "local_times")))
  df
}

# Rows with no animal_id and rows with no transmitter are two different
# faults: the first is a sheet with blank ids, the second, in an OTN sheet,
# usually a blank TAG_CODE_SPACE. Each is counted and named. When more than
# half the rows lose either one, a warning would let a run go on with a few
# animals of many, so the reader stops. Blank ids are common on OTN tagging
# sheets, which can hold no id in any column; the error names
# missing_id = "transmitter" first, and `col_map` for ids kept in a column
# the map does not read.
count_drops <- function(dt, file, id_columns) {
  no_ani <- is.na(dt$animal_id) & !is.na(dt$transmitter)
  no_tx <- is.na(dt$transmitter)
  out <- c(no_animal_id = sum(no_ani), no_transmitter = sum(no_tx))
  all_lost <- sum(out) && sum(out) == nrow(dt)
  half <- function(n) {
    sprintf("%s of its %s distinct rows have no %%s, more than half", fmt_n(n), fmt_n(nrow(dt)))
  }
  if (all_lost || 2 * out[["no_animal_id"]] > nrow(dt)) {
    lead <- if (all_lost) {
      sprintf("none of its %s distinct rows has both an animal_id and a transmitter",
              fmt_n(nrow(dt)))
    } else {
      sprintf(half(out[["no_animal_id"]]), "animal_id")
    }
    stop_detqc(paste("%s: %s (%s with no animal_id, %s with no transmitter). The animal_id is",
                     "read from the column %s. OTN tagging sheets often leave ANIMAL_ID",
                     "blank, and GLATOS detection exports can leave animal_id blank. Read",
                     "such a file with missing_id = \"transmitter\", which names each animal",
                     "by its transmitter; when the ids sit in another column, name it with",
                     "`col_map`. A blank transmitter can be filled from `tag_specs` by",
                     "tag_serial. See \"OTN tagging sheets and detection extracts\" in",
                     "?read_animals."),
               file, lead, fmt_n(out[["no_animal_id"]]), fmt_n(out[["no_transmitter"]]),
               paste(unique(id_columns), collapse = " or "))
  }
  if (2 * out[["no_transmitter"]] > nrow(dt)) {
    stop_detqc(paste("%s: %s (%s with no animal_id, %s with no transmitter). In an OTN",
                     "tagging sheet the transmitter is TAG_CODE_SPACE and TAG_ID_CODE, and a",
                     "blank TAG_CODE_SPACE leaves it empty. Give `tag_specs`, which fills the",
                     "transmitter by tag_serial, or fill the code space in the sheet. See",
                     "\"OTN tagging sheets and detection extracts\" in ?read_animals."),
               file, sprintf(half(out[["no_transmitter"]]), "transmitter"),
               fmt_n(out[["no_animal_id"]]), fmt_n(out[["no_transmitter"]]))
  }
  parts <- character()
  if (out[["no_transmitter"]]) {
    ids <- stats::na.omit(dt$animal_id[no_tx])
    parts <- c(parts, sprintf(paste("%s have no transmitter (in an OTN sheet, TAG_CODE_SPACE or",
                                    "TAG_ID_CODE is blank)%s; `tag_specs` fills it by",
                                    "tag_serial"),
                              fmt_n(out[["no_transmitter"]]),
                              if (length(ids)) paste0(", such as animal ", ids[1]) else ""))
  }
  if (out[["no_animal_id"]]) {
    parts <- c(parts, sprintf(paste("%s have no animal_id, such as transmitter %s;",
                                    "missing_id = \"transmitter\" names them by transmitter"),
                              fmt_n(out[["no_animal_id"]]), dt$transmitter[no_ani][1]))
  }
  if (length(parts)) {
    warn_detqc("%s: %s of %s distinct rows were dropped: %s.", file, fmt_n(sum(out)),
               fmt_n(nrow(dt)), paste(parts, collapse = "; "))
  }
  out
}

# The codes each tag serial sends, from the tag specifications.
spec_codes <- function(specs) {
  if (is.null(specs)) return(NULL)
  if (!is.data.frame(specs) || !all(c("transmitter", "tag_serial") %in% names(specs))) {
    stop_detqc("`tag_specs` must be a data frame with `transmitter` and `tag_serial`.")
  }
  s <- as_dt(specs)
  s[, tag_serial := as.character(tag_serial)]
  s <- s[!is.na(transmitter) & !is.na(tag_serial)]
  unique(s, by = "transmitter")
}

# A row with an animal and a tag serial but no transmitter takes every code
# the specifications list for that serial.
fill_from_specs <- function(dt, specs) {
  if (is.null(specs) || !"tag_serial" %in% names(dt)) return(list(dt = dt, n = 0L))
  need <- which(is.na(dt$transmitter) & !is.na(dt$tag_serial) &
                  as.character(dt$tag_serial) %in% specs$tag_serial)
  if (!length(need)) return(list(dt = dt, n = 0L))
  rows <- dt[need][, transmitter := NULL]
  rows[, tag_serial := as.character(tag_serial)]
  add <- specs[, list(tag_serial, transmitter)][rows, on = "tag_serial", allow.cartesian = TRUE]
  list(dt = data.table::rbindlist(list(dt[-need], add), use.names = TRUE, fill = TRUE),
       n = nrow(add))
}

# A sheet code that the specifications do not list for its tag serial,
# though they list that serial: a sheet that writes one code space for a
# serial and specifications that write another leave the detections, which
# carry one of the two, on a code no animal holds. The row is
# kept, since only the user can say which is right, and the pair is named.
warn_codes_not_in_specs <- function(dt, specs) {
  if (is.null(specs) || !"tag_serial" %in% names(dt) || !nrow(dt)) return(0L)
  rows <- unique(dt[!is.na(tag_serial), list(animal_id, transmitter,
                                             tag_serial = as.character(tag_serial))])
  rows <- rows[tag_serial %in% specs$tag_serial]
  listed <- paste(specs$tag_serial, specs$transmitter)
  odd <- rows[!paste(tag_serial, transmitter) %in% listed]
  if (!nrow(odd)) return(0L)
  pairs <- vapply(seq_len(min(3L, nrow(odd))), function(i) {
    sprintf("%s for serial %s of %s (specifications: %s)", odd$transmitter[i], odd$tag_serial[i],
            odd$animal_id[i],
            paste(specs$transmitter[specs$tag_serial == odd$tag_serial[i]], collapse = ", "))
  }, "")
  warn_detqc(paste("%s of the sheet %s not among the codes `tag_specs` lists for %s tag serial:",
                   "%s. Each is kept beside the listed codes; a code-space slip in the sheet",
                   "is the usual cause, so check it against the detections and drop it with",
                   "ani[ani$transmitter != \"<code>\", ]."),
             paste(fmt_n(nrow(odd)), if (nrow(odd) == 1) "code" else "codes"),
             if (nrow(odd) == 1) "is" else "are", if (nrow(odd) == 1) "its" else "their",
             paste(pairs, collapse = "; "))
  nrow(odd)
}

# Second codes: every code of an animal's tag serial that the sheet does not
# list for that animal becomes a row of its own, with the tag columns of the
# specifications and the animal columns of the sheet.
add_second_codes <- function(dt, specs) {
  if (is.null(specs) || !"tag_serial" %in% names(dt) || !nrow(dt)) return(list(dt = dt, n = 0L))
  have <- unique(dt[!is.na(tag_serial), list(animal_id, tag_serial = as.character(tag_serial))])
  cand <- specs[have, on = "tag_serial", nomatch = NULL, allow.cartesian = TRUE]
  cand <- cand[!paste(animal_id, transmitter) %in% paste(dt$animal_id, dt$transmitter)]
  if (!nrow(cand)) return(list(dt = dt, n = 0L))
  base <- dt[!is.na(tag_serial)][, tag_serial := as.character(tag_serial)]
  base <- unique(base, by = c("animal_id", "tag_serial"))
  tag_cols <- intersect(setdiff(names(specs), c("transmitter", "tag_serial")), names(base))
  base[, c("transmitter", tag_cols) := NULL]
  add <- cand[base, on = c("animal_id", "tag_serial"), nomatch = NULL]
  keep <- intersect(names(add), union(names(dt), names(specs)))
  add <- add[, keep, with = FALSE]
  list(dt = data.table::rbindlist(list(dt, add), use.names = TRUE, fill = TRUE), n = nrow(add))
}

# The tag columns of the specifications, onto every row whose transmitter
# they list, where the row's own value is missing. Every code of a tag gets
# them, the pinger code as well as the sensor code, because
# bin_detections() stops on a transmitter with no delays.
spec_tag_columns <- c("tag_model", "tag_life_days", "min_delay", "max_delay", "sensor_type",
                      "sensor_unit", "sensor_slope", "sensor_intercept")

fill_spec_columns <- function(dt, specs) {
  none <- list(dt = dt, filled = 0L, conflicts = stats::setNames(integer(), character()))
  if (is.null(specs) || !nrow(dt)) return(none)
  k <- match(dt$transmitter, specs$transmitter)
  if (all(is.na(k))) return(none)
  cols <- intersect(spec_tag_columns, names(specs))
  res <- lapply(cols, function(col) fill_spec_column(dt, col, specs[[col]][k]))
  for (i in seq_along(cols)) data.table::set(dt, j = cols[i], value = res[[i]]$value)
  conflicts <- stats::setNames(vapply(res, function(r) sum(r$differ), integer(1)), cols)
  conflicts <- conflicts[conflicts > 0]
  if (length(conflicts)) {
    first <- res[[match(names(conflicts)[1], cols)]]
    warn_detqc(paste("%s of the sheet differ from the tag specifications (%s), such as %s;",
                     "the sheet's values are kept."),
               paste(fmt_n(sum(conflicts)), if (sum(conflicts) == 1) "cell" else "cells"),
               paste(names(conflicts), conflicts, collapse = ", "), first$example)
  }
  list(dt = dt, filled = sum(vapply(res, function(r) sum(r$fill), integer(1))),
       conflicts = conflicts)
}

# One tag column: the sheet's value where it has one, else the
# specifications'. `differ` marks cells where both are given and disagree.
fill_spec_column <- function(dt, col, from) {
  own <- if (col %in% names(dt)) dt[[col]] else from[NA_integer_]
  own <- if (is.numeric(from)) suppressWarnings(as.numeric(own)) else as.character(own)
  fill <- is.na(own) & !is.na(from)
  differ <- !is.na(own) & !is.na(from) & own != from
  # A sheet often gives the model's short form: "V13AP" for "V13AP-1x".
  if (col == "tag_model") differ <- differ & !startsWith(toupper(from), toupper(own))
  i <- which(differ)[1]
  example <- ""
  if (!is.na(i)) {
    example <- sprintf("%s %s in the sheet against %s in the specifications for %s", col,
                       format(own[i]), format(from[i]), dt$transmitter[i])
  }
  own[fill] <- from[fill]
  list(value = own, fill = fill, differ = differ, example = example)
}

# A sensor tag absent from the specifications loses its second code and its
# calibration with no word. Models such as V13AP, V9AP and V16AT carry sensors.
warn_serials_not_in_specs <- function(dt, specs) {
  if (is.null(specs) || !"tag_serial" %in% names(dt)) return(0L)
  model <- if ("tag_model" %in% names(dt)) as.character(dt$tag_model) else NA_character_
  tags <- data.frame(serial = as.character(dt$tag_serial), model = model,
                     stringsAsFactors = FALSE)
  tags <- tags[!is.na(tags$serial) & !duplicated(tags$serial), , drop = FALSE]
  out <- tags[!tags$serial %in% specs$tag_serial, , drop = FALSE]
  if (!nrow(out)) return(0L)
  out$sensor <- grepl("^V[0-9]+[ADPT]", out$model)
  out <- out[order(!out$sensor), , drop = FALSE]
  named <- ifelse(is.na(out$model), out$serial, sprintf("%s (%s)", out$serial, out$model))
  shown <- utils::head(named, 3)
  warn_detqc(paste("%s of %s tag serials in the sheet are not in `tag_specs`, %s of them sensor",
                   "tags that may send a second code and need a calibration, such as %s. Read",
                   "their manufacturer's sheet with read_tag_specs() and bind it to `tag_specs`."),
             fmt_n(nrow(out)), fmt_n(nrow(tags)), fmt_n(sum(out$sensor)),
             paste(shown, collapse = ", "))
  nrow(out)
}

# Columns named inside data.table calls in this file.
utils::globalVariables("tag_serial")

# A transmitter on two animals is kept as two rows; detset() assigns each
# detection by release time. The warning names them so the sheet is checked.
warn_reused_tags <- function(dt) {
  per <- dt[, list(n = .N, ids = paste(animal_id, collapse = ", ")), by = "transmitter"]
  per <- per[per$n > 1]
  if (nrow(per)) {
    warn_detqc(paste("%s %s on more than one animal, such as %s (%s). Each animal keeps its",
                     "row; detset() gives a detection to the animal released last before it."),
               n_of(nrow(per), "transmitter"), verb(nrow(per), "is", "are"), per$transmitter[1],
               per$ids[1])
  }
  nrow(per)
}

# A file whose only tag column is a transmitter, as a detection export or a
# deployment sheet with a TRANSMITTER column, would read as a tag sheet of
# codes with no delays, life or calibration. It stops with the file's
# columns.
stop_not_tag_sheet <- function(out, raw, map, file) {
  found <- setdiff(unique(out$map_used$target), "transmitter")
  if (length(found)) return(invisible(TRUE))
  stop_detqc(paste("%s is not a tag sheet: it has no column for the serial, model, life,",
                   "delays or sensor calibration of a tag. Looked for: %s. %s."),
             basename(file),
             paste(unique(map$source[map$target != "transmitter"]), collapse = ", "),
             file_columns("min_delay_sec", names(raw), raw))
}

#' Read a tag specification sheet
#'
#' Reads the tag specification sheet a manufacturer supplies with its tags into
#' one row per transmitter: model, serial, life, the programmed delays, the
#' power step, and for sensor transmitters the sensor type, unit, slope and
#' intercept. The InnovaSea sheet (`"Tag Summary"`, header row found by its
#' names) is mapped, and so is a CSV with the same columns under shorter
#' names, as labs often keep them: `transmitter`, `tag_serial_number`,
#' `tag_type`, `tag_life`, `min_delay_sec`, `max_delay_sec`, `sensor_type`,
#' `units`, `slope` and `intercept`.
#'
#' A file with no column for a tag's serial, model, life, delays or sensor
#' calibration, such as a detection export or a deployment sheet, stops with
#' an error that lists its columns, or the 5 nearest when it has 20 or more.
#' For a workbook, these are the columns of its widest sheet. A Fathom VDAT
#' CSV, which names its columns in a `RECORD TYPE` row and rows ending
#' `_DESC`, gets an error that names it. The error says to read its
#' detections, if it holds any, with `read_detections(format = "vdat")`, and
#' its status records with [read_receiver_logs()]. A text file with no header
#' row gets an error that says so.
#'
#' The delays are those of the first step of the tag's programme. A tag
#' programmed with several steps changes its delays over its life; the
#' sheet's later steps are kept in the columns that start `step_`.
#'
#' Sensor types written as one letter are read as `"A"` acceleration, `"T"`
#' temperature, and `"P"` or `"D"` depth; words such as "pressure" or
#' "predation (digestion)" are read as `"depth"` or `"predation"`. The sheet's
#' own text is kept in `sensor_type_raw`, and the attribute `source` counts
#' the rows whose type `read_tag_specs()` could not class (`sensor_type_unclassed`,
#' not counting `"none"`). Units are written as the contract spells them:
#' `"m"`, `"C"` and `"m/s2"`.
#'
#' @inheritParams read_deployments
#' @param file Path to an InnovaSea tag specification sheet (`.xls` or
#'   `.xlsx`) or a CSV with its columns, or several such paths: each is read
#'   and the rows are bound, the first row of a transmitter kept.
#' @return A data frame, one row per transmitter, with `transmitter`,
#'   `tag_serial`, `tag_model`, `tag_life_days`, `min_delay`, `max_delay`,
#'   `power_level` where the sheet gives it (the InnovaSea sheet does, the
#'   shorter CSV does not), `sensor_type`, `sensor_unit`, `sensor_slope`,
#'   `sensor_intercept` and `sensor_type_raw`, the sheet's own text of the
#'   sensor type. A tag programmed with several steps adds the `step_`
#'   columns. With several files, the attribute `source` is a list of
#'   one record per file, and the column `source_file` names the file of
#'   each row.
#' @seealso [calibrate_sensors()], which takes the result as `specs`.
#' @export
#' @examples
#' csv_file <- system.file("extdata", "stoney_tag_specs.csv", package = "detqc")
#' specs <- read_tag_specs(csv_file)
#' specs[, c("transmitter", "sensor_type", "sensor_unit", "sensor_slope")]
read_tag_specs <- function(file, col_map = NULL) {
  # A vendor ships one sheet per order, so a lab holds many.
  if (is.character(file) && length(file) > 1) return(read_many_tag_specs(file, col_map))
  map <- get_column_map("animals", "innovasea", col_map)
  raw <- read_raw_table(file, header_names = attr(map, "source_parts"))
  out <- apply_column_map(raw, map, "tag_specs", keep_unmapped = FALSE)
  stop_not_tag_sheet(out, raw, map, file)
  df <- out$data
  steps <- grep("^step_[2-9]", names(raw), value = TRUE)
  for (s in steps) df[[s]] <- raw[[s]]
  type <- if ("sensor_type" %in% names(df)) df$sensor_type else rep(NA_character_, nrow(df))
  unit <- if ("sensor_unit" %in% names(df)) df$sensor_unit else rep(NA_character_, nrow(df))
  letter <- c(A = "acceleration", T = "temperature", P = "depth", D = "depth")
  short <- toupper(trimws(type))
  # The class replaces the sheet's text in sensor_type, as the contract names
  # classes; the text is kept beside it, so "predation (digestion)" is not
  # lost as NA.
  df$sensor_type_raw <- type
  df$sensor_type <- unname(ifelse(short %in% names(letter), letter[short],
                                  sensor_class(type, unit)))
  df$sensor_unit <- unit_key(unit)
  df <- df[!is.na(df$transmitter), , drop = FALSE]
  df <- df[!duplicated(df$transmitter), , drop = FALSE]
  df <- order_columns(df, c("transmitter", "tag_serial", "tag_model", "tag_life_days",
                            "min_delay", "max_delay", "power_level", "sensor_type",
                            "sensor_unit", "sensor_slope", "sensor_intercept"))
  check_columns(df, contract_spec$tag_specs, "tag specifications")
  text <- trimws(df$sensor_type_raw)
  unclassed <- !is.na(text) & nzchar(text) & tolower(text) != "none" & is.na(df$sensor_type)
  if (any(unclassed)) {
    message(sprintf(paste("read_tag_specs(): %s of %s %s a sensor type no class fits",
                          "(%s); %s NA and the text is in sensor_type_raw."),
                    fmt_n(sum(unclassed)), n_of(nrow(df), "row"),
                    verb(sum(unclassed), "has", "have"),
                    paste(utils::head(unique(text[unclassed]), 3), collapse = ", "),
                    verb(sum(unclassed), "its sensor_type is", "their sensor_type is")))
  }
  attr(df, "source") <- c(source_record(file, "innovasea", out, nrow(df)),
                          list(sensor_type_unclassed = sum(unclassed),
                               sensor_type_none = sum(!is.na(text) & tolower(text) == "none")))
  df
}

read_many_tag_specs <- function(files, col_map) {
  parts <- lapply(files, function(f) {
    d <- tryCatch(read_tag_specs(f, col_map), error = function(e) {
      stop_detqc("Reading %s: %s", basename(f), conditionMessage(e))
    })
    d$source_file <- rep(basename(f), nrow(d))
    d
  })
  srcs <- lapply(parts, attr, "source")
  df <- as_df(data.table::rbindlist(parts, use.names = TRUE, fill = TRUE))
  dup <- duplicated(df$transmitter)
  if (any(dup)) {
    n_dup <- length(unique(df$transmitter[dup]))
    message(sprintf(paste("read_tag_specs(): %s %s on more than one of the %s sheets, such as",
                          "%s; the first sheet's row is kept."),
                    n_of(n_dup, "transmitter"), verb(n_dup, "is", "are"), fmt_n(length(files)),
                    df$transmitter[dup][1]))
    df <- df[!dup, , drop = FALSE]
  }
  rownames(df) <- NULL
  attr(df, "source") <- srcs
  df
}

# The units a tagging template states for size. A row with no value gets no
# unit. OTN: the "Tag Metadata" sheet of glatos's extdata
# otn_nsbs_tag_metadata.xls heads its columns "LENGTH (m)" and
# "WEIGHT (kg)". GLATOS: the Tagging sheet of glatos's walleye_workbook.xlsm
# heads them "LENGTH" and "WEIGHT", and the glatos 0.8.0 help names no unit,
# so none is added.
size_units_by_format <- list(otn = c(length = "m", weight = "kg"))

add_size_units <- function(dt, format) {
  units <- size_units_by_format[[format]]
  if (is.null(units)) return(dt)
  for (cl in intersect(names(units), names(dt))) {
    dt[, (paste0(cl, "_unit")) := data.table::fifelse(is.na(dt[[cl]]), NA_character_,
                                                      units[[cl]])]
  }
  dt
}
