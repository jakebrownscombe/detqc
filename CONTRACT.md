# positionR table contract

Version 9.

It binds `detqc`, `detfield` and `arraydesign`. Each package holds an identical copy, and
`tools/check_all.sh` fails when two copies differ.

## 1. Tables and column names

The packages pass plain data frames to each other and to users; data.table is used inside the
packages only. Columns take their names from the ATO Dictionary of the trackyverse,
<https://ato.trackyverse.org/articles/ato-dictionary.html>, accessed 29 October 2026. The ATO
column below marks each departure from ATO and each concept ATO does not name.

A function that takes one of these tables checks the required columns by name and type. It stops
with the name of the first column that is missing or of the wrong type. Other columns pass through
unchanged. A new shared column enters this file before any package reads it.

### Detections: one row per detection (ATO `@det`)

| Column | Type | Required | ATO | Meaning |
|---|---|---|---|---|
| `datetime` | POSIXct, UTC | yes | `datetime` | time the receiver logged the detection |
| `receiver_serial` | character | yes | `receiver_serial` | serial number of the receiver that logged it |
| `transmitter` | character | yes | `transmitter` | code as decoded, codespace and ID: "A69-9001-1234" |
| `animal_id` | character | yes; NA for reference tags and unknown codes | `animal`, renamed | animal that carries the transmitter |
| `station_id` | character | once deployments are joined | not in `@det` | station of the deployment that logged it |
| `sensor_raw` | numeric | no | none; named here | sensor reading as transmitted, before calibration |
| `sensor_value` | numeric | no | `sensor_value` | sensor reading in `sensor_unit`; NA until calibrated |
| `sensor_unit` | character | when `sensor_value` is not NA | `sensor_unit`, held in `@tag` | unit of `sensor_value`: "m", "C" |
| `after_download` | logical | no | none; named here | TRUE for a detection logged after its deployment's `download_datetime`; added by `detset()` when the deployments carry download times, and left for the user to filter |

### Receiver deployments: one row per deployment (ATO `@dep`)

| Column | Type | Required | ATO | Meaning |
|---|---|---|---|---|
| `receiver_serial` | character | yes | `receiver_serial` | serial number of the receiver |
| `station_id` | character | yes | `deploy_location`, renamed | station where the receiver sat |
| `deploy_datetime` | POSIXct, UTC | yes | `deploy_datetime` | start of the deployment |
| `recover_datetime` | POSIXct, UTC | yes; NA while deployed | `recover_datetime` | end of the deployment |
| `deploy_lat` | numeric | yes | `deploy_lat` | latitude, decimal degrees, WGS 84 |
| `deploy_lon` | numeric | yes | `deploy_lon` | longitude, decimal degrees, WGS 84 |
| `deploy_z` | numeric | no | `deploy_z` | receiver depth, metres below the water surface, positive down |
| `download_datetime` | POSIXct, UTC | no | none; named here | last recorded download of the receiver's data; it is the end of the data, so days after it are `not_downloaded`, and detections after it stay in the set flagged `after_download` |
| `receiver_transmitter` | character | no | none; named here | transmitter at the deployment, as the OTN short form's `TRANSMITTER` records it: a transceiver's own pinger, or a sentinel tag on the mooring; "A69-1601-12345" |

A deployment holds the detections with `deploy_datetime <= datetime < recover_datetime`, so a
detection at the moment of a receiver swap belongs to the later deployment.

### Animals and tags: one row per transmitter (ATO `@tag` and `@ani`)

One table holds both: a tag that sends two codes, such as a pinger and a sensor, has two rows that
share `animal_id` and `tag_serial`. The ATO writer splits it into `@ani` (one row per animal) and
`@tag` (one row per transmitter). Added in version 2.

| Column | Type | Required | ATO | Meaning |
|---|---|---|---|---|
| `animal_id` | character | yes | `animal`, renamed | animal that carries the transmitter |
| `transmitter` | character | yes | `transmitter` (`@tag`) | code the tag sends, as decoded: "A69-9001-1234" |
| `species` | character | no | none; named here | common name of the species |
| `release_datetime` | POSIXct, UTC | no | `release_datetime` (`@ani`) | time the animal was released |
| `release_lat` | numeric | no | `release_lat` (`@ani`) | latitude of the release, decimal degrees, WGS 84 |
| `release_lon` | numeric | no | `release_lon` (`@ani`) | longitude of the release, decimal degrees, WGS 84 |
| `tag_model` | character | no | `model` (`@tag`), renamed | manufacturer's model of the tag: "V13AP-1x" |
| `tag_serial` | character | no | `serial` (`@tag`), renamed | manufacturer's serial number of the tag |
| `tag_life_days` | numeric | no | `battery_life` (`@tag`), renamed | expected life of the tag, days |
| `min_delay` | numeric | no | none; see below | shortest programmed delay between transmissions, seconds |
| `max_delay` | numeric | no | none; see below | longest programmed delay between transmissions, seconds |
| `sensor_type` | character | no | `sensor_type` (`@tag`) | what the sensor measures: "depth", "temperature", "acceleration" |
| `sensor_unit` | character | no | `sensor_unit` (`@tag`) | unit after calibration: "m", "C", "m/s2" |
| `sensor_slope` | numeric | no | none; named here | slope of the calibration, `sensor_unit` per raw count |
| `sensor_intercept` | numeric | no | none; named here | intercept of the calibration, in `sensor_unit` |

A calibrated reading is `sensor_value = sensor_intercept + sensor_slope * sensor_raw`. A row with a
slope and no intercept, or an intercept and no slope, calibrates nothing. `min_delay` and
`max_delay` keep the names `detfield::bin_detections()` reads in `tag_delay`; the ATO writer gives
`ping_rate = (min_delay + max_delay) / 2` and `ping_variation = (max_delay - min_delay) / 2`.

### Receiver listening days: one row per receiver and UTC date (no ATO slot)

Written by `detqc::detset()` (the first three columns) and replaced by `detqc::audit_listening()`
(all of them); read by `detfield::bin_detections()` as its receiver-day form of `listening`.
Added in version 3. Keyed by `receiver_serial` and `date`: at a receiver swap two
receivers share a station on one day, each with its own row. Version 5 adds the statuses
"reference_missed" (listening) and "silent_run" (not listening). Version 6
adds three statuses that only receiver logs give (`audit_listening(logs = )`): "logging_silent"
(listening), "deaf" and "off" (not listening), and the columns `log_records` and `pings`, present
only when logs were given.

| Column | Type | Required | ATO | Meaning |
|---|---|---|---|---|
| `receiver_serial` | character | yes | `receiver_serial` | serial number of the receiver |
| `station_id` | character | yes | `deploy_location`, renamed | station of the deployment that covers most of the day |
| `date` | Date | yes | none; named here | UTC date |
| `deployed_share` | numeric | no | none; named here | share of the day a deployment had the receiver in the water, 0 to 1 |
| `detections` | integer | no | none; named here | detections of any transmitter the receiver logged that day |
| `reference_detections` | integer | no | none; named here | detections of the reference tags the receiver is expected to hear; NA where none is expected |
| `log_records` | integer | no | none; named here | status records in the receiver's log that day; NA outside the log's dates; present only when logs were given |
| `pings` | numeric | no | none; named here | pulses the receiver's DIAG records counted that day; NA without them; present only when logs were given |
| `status` | character | no | none; named here | "listening", "partial", "silent", "reference_missed", "reference_silent", "silent_run", "not_downloaded", and with receiver logs "logging_silent", "deaf" or "off" |
| `listening` | logical | no | none; named here | FALSE where the receiver was not listening: "reference_silent", "silent_run", "not_downloaded", "deaf" and "off" days |

### Receiver logs: one row per record of a receiver's own log (no ATO slot)

Written by `detqc::read_receiver_logs()` and read by `detqc::audit_listening(logs = )`. Added in
version 6. A table built by hand needs the first four columns; a `record_type` other than
"EVENT", "EVENT_FAULT", "EVENT_INIT" and "EVENT_OFFLOAD" counts as a status record.

| Column | Type | Required | ATO | Meaning |
|---|---|---|---|---|
| `receiver_serial` | character | yes | `receiver_serial` | serial number of the receiver that wrote the record |
| `datetime` | POSIXct, UTC | yes | none; named here | time of the record |
| `record_type` | character | yes | none; named here | the record type as the receiver writes it: "DIAG", "TEMP", "ATTITUDE", "BATTERY", "HEALTH_VR2TX", "EVENT", ... |
| `receiver_model` | character | no | none; named here | model, as the log writes it: "VR2Tx-69" |
| `temperature_c` | numeric | no | none; named here | ambient temperature, degrees C |
| `noise` | numeric | no | none; named here | noise, in `noise_unit` |
| `noise_unit` | character | no | none; named here | "mV" (VR2Tx `DIAG`) or "dB" (NexTrak) |
| `tilt_deg` | numeric | no | none; named here | tilt of the receiver, degrees |
| `depth_m` | numeric | no | none; named here | receiver depth, metres, where the receiver measures it |
| `pings` | numeric | no | none; named here | pulses counted since the previous `DIAG` record |
| `log_detections` | numeric | no | none; named here | detections counted since the previous `DIAG` record |
| `battery_v` | numeric | no | none; named here | battery voltage, V |
| `battery_pct` | numeric | no | none; named here | battery remaining, per cent |
| `memory_pct` | numeric | no | none; named here | memory remaining, per cent |
| `event` | character | no | none; named here | an event's type, or a fault's importance |
| `event_detail` | character | no | none; named here | an event's subtype and description, or a fault's description and code |
| `source_file` | character | no | none; named here | file the record was read from |

### Used and available points: one row per point of a matched set (no ATO slot)

Written by `detfield::used_available()` for position draws and by `arraydesign::available_steps()`
for the true steps of a simulated track. Added in version 6. Both feed a conditional logit,
`clogit(case ~ covariate + strata(stratum))`, so the two columns that model reads carry the same
names in both packages. A stratum holds one row with `case = 1` and at least one with `case = 0`.

| Column | Type | Required | ATO | Meaning |
|---|---|---|---|---|
| `stratum` | integer | yes | none; named here | one matched set: in detfield one animal, bin and draw; in arraydesign one step of one path |
| `case` | integer | yes | none; named here | 1 for the used point, 0 for an available point |
| `cell` | integer | yes | none; named here | cell of the grid the point lies in |
| `x`, `y` | numeric | yes | none | projected coordinates, metres; the table carries `epsg` (section 3) |
| `animal_id` | character | detfield | `animal`, renamed | animal the stratum belongs to |
| `bin_start` | POSIXct, UTC | detfield | none; named here | start of the bin the stratum belongs to |
| `draw` | integer | detfield | none; named here | the presence draw (imputation) the stratum is matched to |
| `path_id`, `step` | as in the tracks table | arraydesign | none; named here | the path and the step that ends at the used position |
| `step_length` | numeric | arraydesign | none; named here | length of the step that ends at the point, metres |
| one column per covariate layer | any | no | none | the layer's value at `cell`; NA where the layer has no value there |

### Departures from ATO

- `animal_id`, not ATO's `animal`: `animal_id` is used everywhere in the suite.
- `station_id`, not ATO's `deploy_location`: station is the term of the method (station curves,
  station-day effects).
- `sensor_raw` has no ATO name. It keeps the positionRtools name.
- A writer to ATO renames the two columns, adds `tz = "UTC"` and moves `sensor_unit` to `@tag`.
- `tag_model`, `tag_serial` and `tag_life_days`, not ATO's `model`, `serial` and `battery_life`:
  in a table that also holds animals, a bare `model` or `serial` does not say whose it is.

The positionRtools names `transmitter_id`, `receiver_sn`, `detection_datetime_utc`,
`deploy_datetime_utc` and `recover_datetime_utc` are not used; they map to `transmitter`,
`receiver_serial`, `datetime`, `deploy_datetime` and `recover_datetime`.

## 2. Time

- Every date-time is POSIXct with the time zone attribute "UTC". Readers convert on entry. A
  function given any other time zone stops.
- There is no `tz` column. ATO's mandatory `tz` is added by the ATO writer.
- Sub-second time stays inside `datetime`. ATO's `frac_second` is not used.

## 3. Coordinates and CRS

- `deploy_lat` and `deploy_lon` are always WGS 84 (EPSG 4326).
- Projected coordinates are the columns `x` and `y`, in metres. A table or list that holds them
  carries its CRS as an integer EPSG code in the attribute `epsg`: `attr(tbl, "epsg") <- 32617L`.
- terra and sf objects keep their own CRS. A function that combines one with a table stops when
  the two EPSG codes differ.

## 4. Names of functions and arguments

- snake_case, as `object_name_linter` in `.lintr` checks.
- UK spelling. Not used: normalize, optimize, summarize, standardize, analyze, initialize,
  visualize, color, center, behavior, gray, modeling, labeled. Where a neutral word exists, use
  it. An object's summary is a `summary()` method; a function that summarises a table by its
  rows is `summarise_<table>()`, as `summarise_detections()` is. An argument passed through to another package
  keeps that package's name, such as `col` in base graphics.
- No exported name masks a function in base R or in a package R attaches by default: base, stats,
  utils, graphics, grDevices, methods and datasets.
- `tools/check_gates.R` enforces the spelling and masking rules on every check.

The exported names:

- `detfield`: annotation_scale_bar, bin_detections, credible_region, de_field, de_logistic,
  de_model, layer_receivers, location_surface, positionr_ring_colours, range_test_counts,
  sample_available, sample_locations, scale_fill_depth, scale_fill_probability, system_de,
  theme_positionr, used_available, water_distances.
- `detqc`: all_checks, annotation_scale_bar, apply_mortality_calls, as_detfield_input,
  audit_listening, build_mortality_cards, calibrate_sensors, check_detset, check_settings,
  column_map, detect_mortality, detset, detset_log, filter_animals, filter_deployment_period,
  filter_duplicates, filter_min_detections, filter_min_lag, filter_mortality, filter_post_release,
  filter_release, filter_speed, filter_tag_life, minimum_lags, mortality_rule, mortality_rules,
  plot_abacus, plot_attrition, plot_receivers, plot_timeline, positionr_ring_colours,
  processing_report, read_animals, read_deployments, read_detections, read_detset,
  read_detset_from, read_receiver_logs, read_tag_specs, reconcile_mortality, residency_index,
  review_mortality, scale_fill_depth, scale_fill_probability, speed_steps, summarise_animals,
  summarise_detections, theme_positionr, to_actel, to_ato, to_glatos, validate_detset,
  write_detset.
- `arraydesign`: available_steps, coverage_summary, crw_model, evaluate_array, example_lake,
  habitat_selection, movement_model, perturb_de_model, place_receivers, scale_colour_tracks,
  score_surfaces, simulate_detections, simulate_tracks, simulate_transmissions, track_control.

`smooth()` masks `stats::smooth` and is out. `report()`, `qc()` and `score()` mask nothing but are
too generic and are out.

## 5. One term per concept

| Term | Means | Not used for it |
|---|---|---|
| animal | a tagged individual of any species, identified by `animal_id` | fish, individual, subject, path |
| fish | prose about fish data only; never in the name of a function, argument, column or class | |
| receiver | the instrument, identified by `receiver_serial`; it can move between stations | station, logger |
| station | a fixed place where receivers are deployed, identified by `station_id`; its deployments share the ID | site, receiver, location |
| deployment | one receiver at one station for one period; one row of the deployments table | |
| site | a cluster of stations, such as the 1 km sites of the mortality rules | station |
| tag | the device on or in the animal; it carries one or more transmitters | transmitter |
| transmitter | the code a tag emits, in `transmitter` | tag, tag ID |
| transmission | one coded signal a transmitter sends, heard or not | ping |
| detection | one transmission that one receiver decoded and logged | ping, hit |
| reference tag | a transmitter at a fixed, known position, used to check receivers and range; a deployment's `receiver_transmitter` is one | sentinel; ATO's beacon is a receiver's built-in transmitter only |
| receiver log | the records a receiver writes about itself (status records and events), as opposed to its detections | diagnostics file, event log |

## 6. Changes

A change edits all copies together, with a NEWS entry in each package.

## 7. The processing log and what the report reads

The log and report module of `detqc` writes and reads what follows. The signature of
`log_step()` is fixed; a change to it is a change to this file.

### The log

Every step that reads into, checks, filters or otherwise changes a `detset` appends one record to
`x$log` through the internal helper `log_step()` in `detqc/R/log.R`. No other code writes to
`x$log`, and no record is edited or removed. `detset_log(x)` returns the log, and
`processing_report()` reads its numbers from the log and from the tables of the detset only.

The log is a data frame with one row per record:

| Column | Type | Meaning |
|---|---|---|
| `step` | character | name of the step as the report shows it |
| `fn` | character | the exported function that ran the step |
| `at` | POSIXct, UTC | time the record was written |
| `parameters` | character | the arguments, deparsed as `name = value; name = value` |
| `rows_before` | integer | detections before the step |
| `rows_after` | integer | detections after the step, read from `x$detections` by the helper |
| `animals_before` | integer | distinct non-NA `animal_id` before the step |
| `animals_after` | integer | distinct non-NA `animal_id` after the step |
| `removed` | integer | detections the step removed; the sum of `removed_by` |
| `removed_by` | list of named integer vectors | detections removed by reason; each name is a reason |
| `reason` | character | what the step did, in one string, with its own counts |
| `detqc_version` | character | version of detqc that wrote the record |

The records obey four rules of arithmetic (version 4). `log_breaches()` in `detqc/R/log.R` tests
them, `log_step()` warns when the record it appends breaks one, and `processing_report()` flags
every record that breaks one. None stops, so a log already written stays readable.

1. **Chain.** Each record starts where the one before it ended: `rows_before` equals the previous
   `rows_after`, and `animals_before` equals the previous `animals_after`. A break means the
   detections changed between two steps with no record.
2. **Nothing lost.** `rows_before - removed - rows_after` is never above 0. Above 0, rows were lost
   with no reason.
3. **Added only by a load.** `rows_before - removed - rows_after` is below 0 only for a load step,
   whose `step` is `detset` or starts with `read_` or `append_`. Below 0 at any other step, the
   step over-counted its removals or added rows it should not have.
4. **No more removed than came in.** `removed` is never above `rows_before`. `log_step()` stops on
   this one, because no step can remove rows it did not have.

Two more hold between the log and the tables: the last record's `rows_after` and `animals_after`
equal the rows and distinct animals of `x$detections`. The report flags a difference.

### The helper

```r
log_step(x, step, parameters, before, removed = 0L, reason = "", fn = step)
```

- `x`: the detset after the step.
- `step`: one string. Its prefix sets the group in the report: `read_` and `detset` are loads,
  `check_` and `audit_` are checks, `filter_` is a filter, and a name that holds `mortality` is a
  mortality step.
- `parameters`: a named list of the argument values, which the helper deparses; or a call, such as
  `match.call()`; or one string already in the form above. A data frame or detset argument is
  written as its row count, not printed.
- `before`: the detset before the step, its detections data frame, or `c(rows, animals)`.
- `removed`: a named integer vector of detections removed by reason, such as
  `c(isolated = 120L, before_release = 3L)`; or one unnamed count, stored under the reason
  `no_breakdown`. A reason with no removals may be given as 0 and is shown as 0. The helper stops
  when the counts sum to more than `before` holds.
- `reason`: one string.
- `fn`: the exported function; the default is `step`.

It returns `x` with one record appended.

### What the report reads besides the log

Each part is optional. When a part is absent, the report says that its step did not run.

- `x$meta$sources`: a data frame, one row per file read, with `table` ("detections",
  "deployments" or "animals"), `file`, `format` and `rows`. `detset()` writes it from the
  `"source"` attribute that the readers set.
- `x$checks`: a list of `summary` and `rows`. `summary` is a data frame with one row per check:
  `check` (name), `group` ("Receivers", "Animals and tags", "Time" or "Against the detection
  field"), `status` ("pass", "flag" or "not run"), `flagged` (integer), `of` (integer, the
  denominator), `unit` (what `flagged` and `of` count, such as "deployments") and `note`. `rows` is
  a named list of data frames, the offending rows of each check that flagged, named by `check`.
  `check_detset()` writes it.
- `x$listening$status`: character, one value per receiver-day, such as "listening" or "silent";
  the statuses are those of the listening table above.
  `audit_listening()` writes it; the report counts receiver-days by status.
- `x$mortality`: the calls that `apply_mortality_calls()` writes. The mortality bands of
  `detect_mortality()` are passed to the report as its `bands` argument.
- The figures: `plot_abacus(x)`, `plot_receivers(x)` and `plot_timeline(x)`. Each draws on the
  open graphics device or returns a ggplot. The report draws each to a PNG and embeds it. The
  attrition ladder is drawn by the report from the log alone.
