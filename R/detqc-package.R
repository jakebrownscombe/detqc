#' @keywords internal
"_PACKAGE"

# data.table syntax works inside a package only when its namespace imports
# data.table or sets this flag. The packages call data.table through `::`,
# so they set the flag.
.datatable.aware <- TRUE # nolint: object_name_linter.

# Symbols that data.table and the table contract (CONTRACT.md) use inside
# `[.data.table` calls. R CMD check would report them as undefined globals.
utils::globalVariables(c(
  ".I", ".N", ":=", "datetime", "receiver_serial", "transmitter",
  "animal_id", "station_id", "sensor_raw", "sensor_value", "sensor_unit",
  "deploy_datetime", "recover_datetime", "deploy_lat", "deploy_lon",
  "deploy_z", "x", "y"
))

# Column names detqc builds and reads inside its own data.table calls.
utils::globalVariables(c(
  "accel_onset", "accel_transmitter", "accel_vote", "active_days", "active_recent",
  "band", "cal_type", "cal_unit", "class", "d0", "d1", "date", "death_date",
  "deciding_rule", "di", "end", "first_reading", "flat", "flat_days", "flat_onset",
  "flatline", "gap_days", "i.pos", "i.site", "idx", "intercept", "k", "last_active",
  "last_detection", "lat", "lon", "n", "pos", "q95", "raw", "raw_only", "readings",
  "reason", "rule", "run_start", "scale", "site", "slope", "start", "terminal_vote", "type",
  "unit", "value", "vote", "window_start", "detections", "located", "i.located", "n_days",
  "first", "last", "window_diag_km", "rx", "spikes_ignored", "first_flat_observed", "row",
  "label_side", "labelled", "shown"
))
