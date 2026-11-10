#' The Stoney Lake example study, as read
#'
#' Four walleye tracked by 20 receivers from 2 to 10 June 2024 in Stoney Lake,
#' Ontario, read from the files `stoney_detections.csv`,
#' `stoney_receivers.csv` and `stoney_tag_specs.csv` in
#' `system.file("extdata", package = "detqc")` with [read_detections()],
#' [read_deployments()] and [read_animals()]. The study is simulated from known
#' tracks with the range and movement models of the positionR packages; no
#' real data are included with the package. ST-02 and ST-03 carry depth sensors,
#' whose readings are raw counts here. ST-02 dies at 02:00 UTC on 4 June 2024,
#' and its tag is then heard only at station S011. [example_mortality] is the
#' same study prepared for the mortality rules.
#'
#' @format A list of three data frames:
#' \describe{
#'   \item{detections}{3,297 detections: `datetime` (UTC), `receiver_serial`,
#'     `transmitter`, `animal_id`, `station_id`, `sensor_raw`, `sensor_value`
#'     and `sensor_unit`.}
#'   \item{deployments}{20 deployments of 20 receivers at 20 stations, with
#'     the contract's deployment columns.}
#'   \item{animals}{4 transmitters on 4 animals, with release time and place,
#'     tag model, serial and life, transmission delays and, for ST-02 and
#'     ST-03, the depth calibration.}
#' }
#' @source Simulated by
#'   <https://github.com/jakebrownscombe/detqc/blob/main/data-raw/stoney_example.R>
#'   and read by
#'   <https://github.com/jakebrownscombe/detqc/blob/main/data-raw/detqc_example.R>.
#'   Both scripts are in the repository and not in the installed package.
#' @examples
#' # Rows in each table, and detections per animal
#' sapply(detqc_example, nrow)
#' table(detqc_example$detections$animal_id)
"detqc_example"

#' The Stoney Lake example study, read, checked and filtered
#'
#' The detection set that `vignette("detqc")` builds from the Stoney Lake
#' files: read and joined with [detset()], sensors converted with
#' [calibrate_sensors()], checked with [check_detset()] and the depth raster,
#' audited with [audit_listening()] before the filters, with every receiver
#' named as working, then [filter_duplicates()], [filter_post_release()]
#' (6 h), [filter_min_lag()] (4,500 s), the reviewer's calls stored with [apply_mortality_calls()]
#' (ST-02 dead or shed at 02:00 UTC on 4 June 2024, the other three alive)
#' and [filter_mortality()] with `margin_days = 1`. Of the 3,297 detections
#' read, 1,938 are left. Use it to try the exports and the processing report
#' without rerunning the steps.
#'
#' @format A `detset` with 1,938 detections of 4 animals, 20 deployments,
#'   180 receiver-days in `listening`, the results of 29 checks in `checks`,
#'   4 calls in `mortality` and 9 records in `log`. The file names it
#'   records, in `meta$sources` and in the `source` attribute of each table,
#'   are kept without their folders.
#' @source Built by
#'   <https://github.com/jakebrownscombe/detqc/blob/main/data-raw/stoney_clean.R>, which
#'   is in the repository and not in the installed package.
#' @examples
#' stoney_clean
#' detset_log(stoney_clean)[, c("step", "rows_before", "rows_after")]
"stoney_clean"
