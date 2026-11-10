# detqc 0.1.1

Repairs found by the first public checks, which run on current R on Linux, macOS and Windows.

- GLATOS zone names such as "Eastern" are mapped to the zones they stand for
  (`America/New_York`), not to the legacy `US/` names, which some systems' time-zone
  databases no longer carry. On those systems the local times of a GLATOS file were read
  as NA with a warning.
- `detset()` no longer stops with a data.table type error when it builds the listening
  table on R 4.3 or later, where a sequence of dates can be stored as integers for some
  deployments and as doubles for others.
- The log's times survive a Parquet round trip unchanged: `write_detset()` stores them as
  seconds, because a Parquet timestamp holds whole microseconds.
- `check_detset()` no longer warns from `min()` and `max()` on an empty table when no
  detection follows a download.

# detqc 0.1.0

First release. Its functions are listed by task.

## Read

- `read_detections()` reads OTN extracts (CSV and Parquet), GLATOS detection exports, InnovaSea
  VUE exports and Fathom VDAT CSV files, recognising the format from the column names. Several
  files are read and stacked in one call, and `cut` reads only a time window or a set of
  transmitters from each file.
- `read_deployments()` reads OTN deployment sheets and GLATOS receiver files, and warns when the
  deployments of one station lie more than 1 km apart. It reads the `TRANSMITTER` column of an
  OTN deployment sheet, a transceiver's own pinger or a sentinel tag on the mooring, as
  `receiver_transmitter`; an id with no code space becomes `NA` with a warning.
- `read_deployments()` reads the station of an OTN deployment CSV from `STATION_NAME`, or from
  `STATION_NO` alone beside `ARRAY`. A workbook's `OTN_ARRAY` and `STATION_NO` are joined. A
  project whose detections name stations the other way gives the column as a `col_map` pair. A
  file from which the map fills no `station_id` in any row reads with a warning that names the
  columns the map looked for.
- `read_deployments()` warns when no row has a `recover_datetime` and the file has no column the
  map reads for it, since every deployment then reads as never recovered. The warning names the
  columns the map looked for. A recovery column that is blank in every row gives none.
- `read_animals()` reads OTN and GLATOS tagging records into one row per animal and transmitter,
  and assigns the detections of a reused tag to the right animal by its release time.
- `read_deployments()` and `read_animals()` take `format = "auto"` by default and warn when the
  other format would fill a contract column the chosen one leaves empty. When that column is one
  the table needs, such as `station_id` of a GLATOS receiver file read with `format = "otn"`, they
  stop and name the format `"auto"` picks. A format they do not read stops with the formats
  listed.
- `read_tag_specs()` stops on a file with no tag column beside the transmitter, such as a
  detection export or a deployment sheet, and lists the file's columns, or the 5 nearest when it
  has 20 or more. For a workbook, these are the columns of its widest sheet. A Fathom VDAT CSV
  gets an error that names it. The error says to read its detections, if it holds any, with
  `read_detections(format = "vdat")`, and its status records with `read_receiver_logs()`. A text
  file with no header row gets an error that says so.
- The three readers read detqc's own column names as `format = "detqc"`, which `"auto"` picks,
  so a detset table written with `write.csv()` reads back with no column map.
- `col_map` takes a data frame or CSV file, which replaces the packaged map, or pairs written as
  `c(target = "source")`, which are merged into the format's map. A pair whose name is no target
  of the table stops with the targets listed. A pair whose column is not in the file stops with
  the file's columns listed, or the 5 nearest when it has 20 or more.
- A pair of columns joined by `;` is joined in the order given. Where the format joins columns,
  the pair names them in the format's order and takes its `sep`. GLATOS joins `transmitter` from
  `transmitter_codespace;transmitter_id` in a detection export and from
  `tag_code_space;tag_id_code` in a workbook's tagging sheet, with `-` between them. A pair, or a
  row of a full map, that joins another number of columns stops and gives the format's order. So
  does one that puts one of the format's own columns at another place.
- Columns joined for `transmitter`, in a pair or a full map of any format, must give codes such
  as `A69-1601-12345` or `R64K-1234`. The first 100 rows that hold every part are read. More
  than half must hold a code space, then digits in each later column, joined by `"-"`.
  No part may be a column the maps read for a receiver, a station, a time or a position.
  Otherwise the reader stops and says whether to change the order, the `sep` or the columns.
- A code space is letters, a digit, then letters or digits, with at most one dash and digits
  after it, such as `A69-1601` or `R64K`. Two capitals and a small letter, such as the Thelma
  `OPi` and `OPs`, are a code space too.
- A pair takes the format's `sep`. The VUE, OTN, VDAT and detqc detection maps have none for
  `transmitter`. Two columns joined for it in those formats need a full map whose `transmitter`
  row has the `sep` `"-"`.
- A pair or row of one column for `transmitter` is tested in every format, unless the format
  reads that column for `transmitter` itself. A one-column source of whole codes reads. So does a
  code-space column under a name no packaged map reads, because shape cannot tell the two apart.
  A code-space column a packaged map reads, such as `transmitter_codespace`, stops by its name.
  So does a column the maps read for a receiver, a station, a time or a position. Any other
  column stops, such as one of IDs alone.
- A column the table needs that a pair or a full map fills, and that is NA in some rows after
  the read, gives one warning. It names the column, the NA rows and the rows read. A read with
  `format =` alone, or with a packaged map copied whole, gives none.
- A pair that reads `animal_id` from catalogNumber stops on an OTN extract not matched to
  animals, as a full map does.
- A full map stops when a target's rows name no column the file has and one of them names a
  column no packaged map reads. It stops too when it leaves a column the table needs empty and
  the packaged map of the format being read fills that column from the file. Another target whose
  rows name no column the file has gets one warning. The warning says whether the column is read
  from the file's own column of that name or the table holds no value for it. A packaged map
  copied whole reads as `format =` does.
- `read_detections()` on a file of no format it knows, a workbook among them, lists the file's
  columns, or the 5 nearest when it has 20 or more. For a workbook, these are the columns of its
  widest sheet. Where the nearest columns are listed for `transmitter`, a column of whole codes
  ranks first. The columns a packaged map joins into a code, such as `tag_code_space` and
  `tag_id_code`, rank next.
- `read_detections(format = "auto")` with no `col_map` names the format it guessed, and the
  header columns the guess rests on, when a file read in that format stops. A full map as
  `col_map` reads a file that no format fits, and the file's `source` record gives the format
  as `"col_map"`.
- A sensor unit in brackets, such as `"[m]"` in a Thelma Biotel export, reads as the unit inside
  them.
- `?column_map` says how to read a file with a code space that shares a column, such as a
  Thelma Biotel export, and why each transmitter's frequency is checked after the read.
- A choice argument given a value it does not take stops with a classed error that lists the
  values it takes. An argument that takes one value stops the same way when given two or more
  values other than its full default vector. Given that vector, as the usage writes it, the
  argument takes its first value. `which` in `build_mortality_cards()` takes one or more values.
- `read_deployments()` reads the receiver serial of the OTN moorings (`rcvrserial`) and
  stations-receivers (`rcvr_serial`) files.
- `read_animals()` stops when more than half the rows of a sheet have no `animal_id`, as OTN
  tagging sheets often do, with an error that names `missing_id = "transmitter"` and the help
  section on such sheets. It stops too when more than half have no transmitter, as a blank
  `TAG_CODE_SPACE` leaves them, and names `tag_specs`. Fewer such rows are dropped with a
  warning that counts them.
- `read_detections()` drops the last row of a file cut part-way through it, with a warning.
  A last row cut short that still ends in a line end is padded with NA and then, like any
  rows at the end of a file with no transmitter, receiver serial or station, dropped with one
  warning that counts them. It reads gzip, bzip2 and xz files through R's own decoder, with no
  need for R.utils, tests their decompressed text the same way, and warns when a stream stops
  before its end record. Padding after a gzip stream, zero bytes or a run of 4 or more 0xFF
  bytes, is not read as text, so a cut and padded file is read to the cut. A whole gzip stream
  followed by 1 to 3 bytes of any value reads with no warning.
- `read_detections()` drops binary and list columns of a Parquet file, such as an OTN extract's
  `geometry`, with a message.
- `read_detections()` and `read_receiver_logs()` name a raw VRL or VDAT file, or a zip archive,
  and say to export it with Fathom Connect or unzip it.
- `read_tag_specs()` reads an InnovaSea tag specification sheet; given to `read_animals()`, it
  adds each transmitter's delays and sensor calibration.
- `read_receiver_logs()` reads a receiver's own status records (temperature, noise, tilt, pulses,
  battery, events) from Fathom VDAT CSV files, formats 1.0.0 and 2.0.0, VR2Tx and NexTrak. A
  NexTrak event's text, written in its Values field, is kept in `event_detail`. A message names
  files with no DIAG record, whose noise and counts are then `NA`, and counts `DATA_ERROR`
  records, which are not kept as rows. The yearly receiver-event CSV files that VUE exports are
  not read.
- `column_map()` returns the column maps the import functions use, for you to extend to your own
  layout.
- Every read function takes a `tz` argument, reads time offsets written in a header or a time
  stamp, and returns times in UTC. A local time inside a spring daylight-saving gap is an error,
  and the message gives the remedy.
- `calibrate_sensors()` converts raw sensor values of any type (depth, temperature,
  acceleration) to their units with the slope and intercept from the tag sheet.
- `detset()` joins detections, deployments and animals into one detection set, takes each
  detection's station from the deployment in the water at its time, and starts the processing log.
  `validate_detset()` checks a set against the table contract after you edit it by hand.
- `detset()` warns when the animals table lists none or few of the detected transmitters; the
  release filters match a detection to its release by transmitter when its `animal_id` is not in
  the animals table, and warn when no detection matches a release.
- `print()` of a detset counts heard receivers among the deployed ones and the others apart. A
  set whose deployments name no receiver serial is counted by station alone.

## Check and summarise

- `check_detset()` runs 30 data-entry and deployment checks, listed by `all_checks()` and set by
  `check_settings()`. Each reports a count and its denominator, returns the offending rows, and
  never changes the data; a check that needs data you did not give reports `not run`.
  `transmitter_no_animal` lists the unknown transmitters with their detection counts, most heard
  first, so a tag code entered wrong in the tagging records stands out.
- `audit_listening()` gives each receiver and day a listening status from the deployments,
  downloads and reference tags, the table **detfield** reads. With `logs` from
  `read_receiver_logs()`, a quiet day with no status record is `off`, one with records and a
  missed reference tag is `deaf` (both not listening), and one with records and no reference tag
  is `logging_silent` (listening). A quiet day on which the receiver's DIAG records counted
  detections, of any transmitter, is listening, since the receiver heard; this beats `silent_run`
  and `deaf`, never `off`. The help says where evidence of listening is lost before the audit:
  an export of one project's tags, `filter_animals()`, and the filters of false detections.
- `audit_listening()` keys the days of a deployment with no receiver serial by its station, so
  OTN deployment CSV and stations-receivers files give one listening row per station and day.
- `as_detfield_input()` ends each deployment at its download when the download comes before the
  recovery, and `audit_listening()` marks that day `"partial"`.
- A deployment's data end at its recorded download when the download comes before the
  recovery. Detections after that download, and before the deployment ends, stay in the set:
  `detset()` marks them `TRUE` in the logical column `after_download` when the deployments carry
  `download_datetime`, and counts them in its log record. `audit_listening()` keeps their days
  `"not_downloaded"` and not listening, and its message and log record count the detections,
  their deployments and those days. The check `detections_after_download` lists them per
  deployment, with the first and last such detection and the days they fall on. They can mean a
  download time recorded too early, a later download missing from the sheet, a receiver that
  kept logging, or a clock problem. `as_detfield_input()` returns them and says how many;
  `detfield::bin_detections()` keeps their rows with `listening = FALSE`. After an audit, both
  listening forms `as_detfield_input()` returns keep the deployment ends the audit set. The
  default list and the days table alone can differ by a few receiver-days and hourly rows on the
  first and last day of a deployment, and at a station whose deployments overlap, where the
  hourly rows take the serial of one of them. On the first and last day, the days table lists
  whole UTC days and the deployments table starts and ends at the recorded times.
- A deployment with no recovery time ends where the same receiver is deployed again, and a
  detection the receiver logged at its next deployment belongs to the next deployment.
- `check_detset()` and `audit_listening()` take the deployments' `receiver_transmitter` ids as
  the reference tags when you give none; ids you give take precedence.
- `summarise_detections()`, `summarise_animals()` and `residency_index()` summarise the set by
  animal, station, receiver or day. `residency_index()` counts only days on which a receiver
  listened in its denominator. On a set with no `animal_id`, each returns a table with no row
  and a message.
- `detset_log()` returns the processing log, one row per step. The log writes a count of one with
  its noun in the singular ("1 animal", "a margin of 1 day").

## Filter

- Ten opt-in filters, each logging the detections it removes with the reason:
  `filter_duplicates()`, `filter_release()`, `filter_post_release()`,
  `filter_deployment_period()`, `filter_tag_life()`, `filter_min_lag()`, `filter_speed()`,
  `filter_min_detections()`, `filter_animals()` and `filter_mortality()`.
- `minimum_lags()` returns the minimum lag `filter_min_lag()` tests for every detection, so you
  can look at the lags before you set `max_lag_s`.
- `filter_min_lag()` takes `long_lag_s`, off by default, for the second condition of Pincock's
  test: each transmitter on each receiver must have more short intervals than long ones, or all
  its detections there are removed (reason `few_short_intervals`).
- `filter_mortality()` removes each dead animal's detections from `margin_days` (7 by default)
  before the death.
- `filter_speed()` removes a spike first, and of adjacent spikes only the least supported in
  each pass. It treats simultaneous detections at distant receivers as violations, and of a
  violating step it removes the endpoint with less support, so one false first detection removes
  itself and the run of true detections after it stays. positionRtools removed the later endpoint
  of every violating step.
- `speed_steps()` returns the steps `filter_speed()` tests, with the speed each implies, so you
  can look at the distribution before you set the threshold.

## Review mortality

- `detect_mortality()` runs a set of rule streams over each animal's record and sorts the animals
  into dead, review and alive bands, each with a candidate death date and its basis.
- `mortality_rules()` returns the default streams with their settings; `mortality_rule()` adds a
  stream of your own. The site-listening stream votes only when `listening_share` is set above
  its default of 0.
- The depth and accelerometer flatline streams never set a flat limit below two counts of the
  sensor, the range one count of noise either side of a dead tag's reading makes. A count is the
  transmitter's `sensor_slope`, or, for readings that arrive calibrated with no slope, the
  smallest gap between two of the transmitter's values.
- `review_mortality()` and `build_mortality_cards()` write one HTML review card per animal and
  an index, read back and checked against the detection set. Calls are made in the browser and
  exported as one CSV file. The detection set lists the cards in its field `cards`.
- `apply_mortality_calls()` brings the calls back to the set; `reconcile_mortality()` lists the
  calls that new data put in question.
  A detection after the death date counts as movement beyond `moved_km` (1 km by default) of the
  last station on or before that date. `sites_km` belongs to `detect_mortality()` and
  `mortality_rules()` and groups stations into sites; `reconcile_mortality()` does not read it.

## Export

- `as_detfield_input()` hands the set to **detfield**; `to_glatos()`, `to_actel()` and `to_ato()`
  write the forms those tools read, and `read_detset_from()` reads **glatos** and **actel**
  objects back.
- `write_detset()` and `read_detset()` save and read a set with its log, as RDS or as Parquet.
- `processing_report()` writes one HTML page with every logged step, the checks, the removals by
  reason, the mortality bands and calls, and figures.

## Names

- `minimum_lags()` and `residency_index()` are named so that they do not mask
  `glatos::min_lag()` and `actel::residency()` when those packages are attached.
- **detfield** exports the same five figure functions as **detqc**; attach one of the two, or
  call them with `::`.
- Every error and warning **detqc** raises itself has the class `detqc_error` or
  `detqc_warning`, so a caller can catch them by class.

## Plot

- `plot_receivers()`, `plot_abacus()` and `plot_timeline()` return ggplot objects drawn with
  `theme_positionr()`. `plot_receivers()` draws the stations as rings sized by detections over a
  depth raster or a lake outline, and spans the whole water body when one is given. A station
  that heard none of the animals is a white ring over the dark ring, so it shows on deep water.
- `plot_attrition()` draws the processing log as the detections left after each step, one bar
  per log record, with a red bar where the log's arithmetic breaks; it is the attrition figure of
  `processing_report()`.
- `scale_fill_depth()`, `scale_fill_probability()` and `annotation_scale_bar()` draw the maps of
  the positionR suite, and `positionr_ring_colours()` gives the three receiver ring colours every
  positionR map shares.

## Example data

- `detqc_example` and `example_mortality` contain a synthetic study of four walleye tracked by 20
  receivers in Stoney Lake, Ontario, from 2 to 10 June 2024, one of which dies;
  `example_mortality` adds the true fate of each animal. `stoney_clean` is the same study read,
  checked, audited for listening before the filters, and filtered.
- The same study is included in `inst/extdata` as GLATOS, OTN, VUE and VDAT files, an OTN deployment
  sheet with a `TRANSMITTER` column, synthetic VDAT receiver logs of 19 receivers whose detection
  counts agree with the example's detections, a tag sheet, a range test and a depth raster of the
  lake. Beside it, `nextrak_receiver_log.csv` is a synthetic NexTrak log of one receiver over
  three days: one with detections, one with no record and one quiet day with records.
- The package website links four example review cards and the processing report written by the
  Get started page's code.

## Articles

- "Get started with detqc" runs the package once, end to end, on the example, with the listening
  audit before the filters.
- "Base processing: read, check, and filter telemetry data", "Mortality filtering: base algorithm
  and review cards" and "Export the dataset and generate a summary report" go deeper; the path
  from detection files to positions is in the **detfield** article "Position fish from a cleaned
  detection set".
