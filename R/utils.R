# ---- Internal helpers shared by the detqc modules ----

# The packages pass plain data frames at every boundary. These two helpers
# are the only way a table enters or leaves data.table form.
as_dt <- function(x) {
  data.table::as.data.table(x)
}

as_df <- function(x) {
  out <- data.table::copy(data.table::as.data.table(x))
  data.table::setDF(out)
  rownames(out) <- NULL
  out
}

fmt_n <- function(x) {
  formatC(x, format = "d", big.mark = ",")
}

# A count and its noun, the noun singular for exactly one: "1 animal",
# "3 animals", "1,938 detections". Every count the processing log writes
# beside its noun goes through this. A count that is not whole, as a margin
# of 0.5 days, is written as it is.
n_of <- function(n, noun, plural = paste0(noun, "s")) {
  whole <- !is.na(n) & n == round(n)
  num <- ifelse(whole, fmt_n(round(n)), format(n))
  paste(num, ifelse(!is.na(n) & n == 1, noun, plural))
}

# The verb that agrees with a count: verb(1, "is", "are") is "is".
verb <- function(n, one, many) if (isTRUE(n == 1)) one else many

is_utc <- function(x) {
  inherits(x, "POSIXct") && identical(attr(x, "tzone"), "UTC")
}

empty_utc <- function(n = 0L) {
  as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
}

# Great-circle distance in km. Accurate to well under 1 m at the 1 km scale of
# the mortality sites, which is all the package asks of it.
haversine_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * 6371.0088 * asin(pmin(1, sqrt(a)))
}

dist_km_matrix <- function(lat, lon) {
  n <- length(lat)
  i <- rep(seq_len(n), times = n)
  j <- rep(seq_len(n), each = n)
  matrix(haversine_km(lat[i], lon[i], lat[j], lon[j]), n, n)
}

# ---- Classed conditions ----
# Every error and warning detqc raises itself has the class `detqc_error` or
# `detqc_warning`, so a caller can catch them by class with tryCatch() or
# withCallingHandlers(). The message is formatted with sprintf(), so a
# literal percent sign is written "%%". A choice argument is checked by
# check_format_arg() or check_choice(), so its refusal is classed too. The
# condition's call is the outermost frame that runs an exported detqc
# function, under the export's name. The frame is found by its function, so
# a call made through lapply(), Map() or do.call() is named too. An error
# raised outside any export has no call.
detqc_condition <- function(type, message) {
  cls <- paste0("detqc_", type)
  call <- user_call()
  if (type == "error") stop(errorCondition(message, class = cls, call = call))
  warning(warningCondition(message, class = cls, call = call))
}

user_call <- function() {
  ns <- asNamespace("detqc")
  exports <- getNamespaceExports("detqc")
  funs <- mget(exports, envir = ns, inherits = FALSE)
  calls <- sys.calls()
  for (i in seq_along(calls)) {
    fn <- sys.function(i)
    if (!is.function(fn) || !identical(environment(fn), ns)) next
    hit <- exports[vapply(funs, identical, logical(1), fn)]
    if (length(hit)) {
      cl <- calls[[i]]
      cl[[1]] <- as.name(hit[1])
      return(cl)
    }
  }
  NULL
}

stop_detqc <- function(...) {
  detqc_condition("error", sprintf(...))
}

warn_detqc <- function(...) {
  detqc_condition("warning", sprintf(...))
}

# ---- Contract column checks ----
# CONTRACT.md, section 1: a function that takes a table checks the required
# columns by name and type and stops naming the first one that fails.
col_types <- list(
  character = function(v) is.character(v),
  numeric = function(v) is.numeric(v),
  utc = function(v) is_utc(v),
  date = function(v) inherits(v, "Date"),
  logical = function(v) is.logical(v)
)

type_label <- c(
  character = "character", numeric = "numeric", utc = "POSIXct in UTC",
  date = "Date", logical = "logical"
)

check_columns <- function(df, spec, label) {
  if (!is.data.frame(df)) {
    stop_detqc("`%s` must be a data frame, not %s.", label, class(df)[1])
  }
  for (k in seq_len(nrow(spec))) {
    col <- spec$column[k]
    if (!col %in% names(df)) {
      if (spec$required[k]) stop_detqc("`%s` has no column `%s`.", label, col)
      next
    }
    if (!col_types[[spec$type[k]]](df[[col]])) {
      stop_detqc(
        "`%s$%s` must be %s, not %s.", label, col, type_label[[spec$type[k]]],
        paste(class(df[[col]]), collapse = "/")
      )
    }
  }
  invisible(TRUE)
}

spec_table <- function(column, type, required) {
  data.frame(column = column, type = type, required = required, stringsAsFactors = FALSE)
}

contract_spec <- list(
  detections = spec_table(
    c("datetime", "receiver_serial", "transmitter", "animal_id", "station_id",
      "sensor_raw", "sensor_value", "sensor_unit"),
    c("utc", "character", "character", "character", "character",
      "numeric", "numeric", "character"),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  ),
  deployments = spec_table(
    c("receiver_serial", "station_id", "deploy_datetime", "recover_datetime",
      "deploy_lat", "deploy_lon", "deploy_z", "download_datetime", "receiver_transmitter"),
    c("character", "character", "utc", "utc", "numeric", "numeric", "numeric", "utc",
      "character"),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  ),
  # CONTRACT.md version 2, "Animals and tags": one row per transmitter.
  animals = spec_table(
    c("animal_id", "transmitter", "species", "release_datetime", "release_lat", "release_lon",
      "tag_model", "tag_serial", "tag_life_days", "min_delay", "max_delay",
      "sensor_type", "sensor_unit", "sensor_slope", "sensor_intercept"),
    c("character", "character", "character", "utc", "numeric", "numeric",
      "character", "character", "numeric", "numeric", "numeric",
      "character", "character", "numeric", "numeric"),
    c(TRUE, TRUE, rep(FALSE, 13))
  ),
  # A manufacturer's tag specification sheet: the tag columns of the animals
  # table, before any animal is attached (read_tag_specs()).
  tag_specs = spec_table(
    c("transmitter", "tag_serial", "tag_model", "tag_life_days", "min_delay", "max_delay",
      "sensor_type", "sensor_unit", "sensor_slope", "sensor_intercept"),
    c("character", "character", "character", "numeric", "numeric", "numeric",
      "character", "character", "numeric", "numeric"),
    c(TRUE, rep(FALSE, 9))
  )
)

# ---- Sensor classes and units ----
# One word per sensor class. `sensor_type` and `sensor_unit` arrive in many
# spellings across OTN, GLATOS, VUE and lab sheets.
sensor_class <- function(type, unit) {
  n <- max(length(type), length(unit))
  type <- tolower(trimws(ascii_only(rep_len(as.character(type), n))))
  unit <- unit_key(rep_len(as.character(unit), n))
  # Some sheets put the unit in the type column, such as "m/s2".
  unit[is.na(unit)] <- unit_key(type)[is.na(unit)]
  out <- rep(NA_character_, n)
  out[grepl("accel|activity|tailbeat|m/s2|m s-2", type)] <- "acceleration"
  out[is.na(out) & grepl("depth|pressure|meter|metre|^m$", type)] <- "depth"
  out[is.na(out) & grepl("temp|degc|^c$", type)] <- "temperature"
  # InnovaSea's V7D predation tag: "predation (digestion)".
  out[is.na(out) & grepl("predat|digest", type)] <- "predation"
  unit_class <- c(m = "depth", "m/s2" = "acceleration", C = "temperature")
  fill <- is.na(out) & unit %in% names(unit_class)
  out[fill] <- unit_class[unit[fill]]
  out
}

# The contract's unit spellings: "m", "C", and "m/s2" for acceleration. Raw
# analogue-to-digital counts ("ADC") and blanks carry no unit. A unit in
# brackets, as a Thelma Biotel export writes "[m]", is read without them.
unit_key <- function(unit) {
  u <- tolower(gsub("[[:space:]]", "", ascii_only(as.character(unit))))
  u <- sub("^\\[(.*)\\]$|^\\((.*)\\)$", "\\1\\2", u)
  out <- rep(NA_character_, length(u))
  out[u %in% c("m", "meter", "meters", "metre", "metres")] <- "m"
  out[u %in% c("c", "degc", "degreesc", "celsius")] <- "C"
  out[u %in% c("m/s", "m/s2", "ms-2", "ms^-2", "m/s^2", "ms2")] <- "m/s2"
  out
}

# Drops every byte outside printable ASCII, whatever the locale: "m/s" plus a
# superscript two becomes "m/s", a degree sign and "C" becomes "C". iconv()
# from the native encoding returns "??" for these in the C locale that
# Rscript uses.
ascii_only <- function(x) {
  out <- gsub("[^\\x20-\\x7E]", "", x, perl = TRUE, useBytes = TRUE)
  Encoding(out) <- "unknown"
  out
}

# ---- Receiver serial forms ----
# The contract's receiver_serial is the bare serial number. VUE and OTN
# detection exports write "VR2Tx-480013", while GLATOS and OTN sheets write
# "480013", so the two do not join unless split. Only a known receiver family followed
# by a serial of three or more digits is split. A bare "last part after the
# dash" rule maps OTN collector numbers such as "HFX068-00348-1" to "1" and
# joins 197 hfx receivers to one key.
receiver_pattern <- paste0(
  "^((?:VR|HR|TBR|WHS|SUR|NR|VMT|NCR)[A-Za-z0-9]*(?:-[A-Za-z][A-Za-z0-9]*)*)",
  "-([0-9]{3,})$"
)

split_receiver <- function(x) {
  x <- as.character(x)
  hit <- !is.na(x) & grepl(receiver_pattern, x, perl = TRUE, ignore.case = TRUE)
  model <- rep(NA_character_, length(x))
  model[hit] <- sub(receiver_pattern, "\\1", x[hit], perl = TRUE, ignore.case = TRUE)
  serial <- x
  serial[hit] <- sub(receiver_pattern, "\\2", x[hit], perl = TRUE, ignore.case = TRUE)
  list(model = model, serial = serial, split = hit)
}

# Short stable key for a text, used to keep the browser stores of two
# reviews apart.
hash_text <- function(x) {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(enc2utf8(x), f, useBytes = TRUE)
  substr(unname(tools::md5sum(f)), 1, 10)
}

# Writes a line of a print method wrapped to the console width, so that a
# long count or list of streams does not run past the column. The first line
# starts at `indent` spaces; the lines after it at `exdent`.
cat_wrap <- function(text, indent = 0, exdent = indent + 2) {
  text <- sub("\n$", "", text)
  cat(strwrap(text, width = getOption("width"), indent = indent, exdent = exdent),
      sep = "\n")
}
