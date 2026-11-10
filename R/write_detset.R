# ---- Save and load a detset ----

#' Save a detset to disc, and read it back
#'
#' `write_detset()` saves a detset as one RDS file, or as a folder of Parquet
#' files that other software can read. `read_detset()` reads either back and
#' checks it with [validate_detset()].
#'
#' **RDS** keeps the object exactly as it is in R.
#'
#' **Parquet** needs the arrow package. The folder holds one file per table
#' (`detections.parquet`, `deployments.parquet`, `listening.parquet`,
#' `animals.parquet` when there is one, and any other table the detset holds,
#' such as `mortality.parquet`), `log.parquet` with each record's removals by
#' reason written as JSON text, and `meta.json`. Parts that are neither a
#' table nor the metadata, such as the results of `check_detset()`, go to
#' `other.rds`. Times are written as UTC timestamps to the microsecond, the
#' precision Parquet keeps; the import functions' times are whole seconds, or
#' microseconds from a VDAT file, so nothing is lost. There is no DuckDB
#' backend: data.table holds the detections in memory.
#'
#' @param x A `detset`.
#' @param path For RDS, a file path ending `.rds`; for Parquet, a folder,
#'   created if it does not exist.
#' @param format `"rds"` or `"parquet"`.
#' @param overwrite If `FALSE`, an existing file, or a folder that already
#'   holds a detset, is not replaced.
#' @return `write_detset()` returns `path`, invisibly. `read_detset()`
#'   returns the `detset`.
#' @seealso [detset()]
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' rds_file <- tempfile(fileext = ".rds")
#' write_detset(dets, rds_file)
#' from_rds <- read_detset(rds_file)
#' identical(nrow(from_rds$detections), nrow(dets$detections))
#'
#' if (requireNamespace("arrow", quietly = TRUE)) {
#'   parquet_dir <- file.path(tempdir(), "example_detset")
#'   write_detset(dets, parquet_dir, format = "parquet", overwrite = TRUE)
#'   list.files(parquet_dir)
#'   from_parquet <- read_detset(parquet_dir)
#'   all.equal(from_parquet$detections, dets$detections)
#' }
write_detset <- function(x, path, format = c("rds", "parquet"), overwrite = FALSE) {
  validate_detset(x)
  format <- check_choice(format, c("rds", "parquet"), "format")
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop_detqc("`path` must be one file or folder path.")
  }
  if (format == "rds") {
    if (file.exists(path) && !overwrite) {
      stop_detqc("%s exists; give overwrite = TRUE to replace it.", path)
    }
    saveRDS(x, path)
  } else {
    write_parquet_set(x, path, overwrite)
  }
  invisible(path)
}

write_parquet_set <- function(x, path, overwrite) {
  need_arrow("Writing Parquet")
  if (file.exists(file.path(path, "meta.json")) && !overwrite) {
    stop_detqc("%s already holds a detset; give overwrite = TRUE to replace it.", path)
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  unlink(file.path(path, c("*.parquet", "meta.json", "other.rds")))
  parts <- split_parts(x)
  for (nm in names(parts$tables)) {
    arrow::write_parquet(parts$tables[[nm]], file.path(path, paste0(nm, ".parquet")))
  }
  arrow::write_parquet(log_for_parquet(x$log), file.path(path, "log.parquet"))
  jsonlite::write_json(meta_for_json(x$meta), file.path(path, "meta.json"),
                       auto_unbox = TRUE, digits = NA, pretty = TRUE)
  if (length(parts$other)) saveRDS(parts$other, file.path(path, "other.rds"))
  invisible(path)
}

#' @rdname write_detset
#' @export
read_detset <- function(path) {
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop_detqc("No file or folder at %s.", paste(path, collapse = ", "))
  }
  if (!dir.exists(path)) {
    x <- readRDS(path)
    return(validate_detset(x))
  }
  if (!file.exists(file.path(path, "meta.json"))) {
    stop_detqc("%s is a folder with no meta.json; it is not a saved detset.", path)
  }
  need_arrow("Reading Parquet")
  files <- list.files(path, pattern = "\\.parquet$")
  x <- list()
  for (f in setdiff(files, "log.parquet")) {
    x[[sub("\\.parquet$", "", f)]] <- as_plain(arrow::read_parquet(file.path(path, f)))
  }
  if (!"animals" %in% names(x)) x["animals"] <- list(NULL)
  x$log <- log_from_parquet(as_plain(arrow::read_parquet(file.path(path, "log.parquet"))))
  x$meta <- meta_from_json(jsonlite::read_json(file.path(path, "meta.json"),
                                               simplifyVector = TRUE))
  other <- file.path(path, "other.rds")
  if (file.exists(other)) x <- c(x, readRDS(other))
  order <- c("detections", "deployments", "animals", "listening", "log", "meta")
  x <- x[c(order, setdiff(names(x), order))]
  validate_detset(structure(x, class = "detset"))
}

need_arrow <- function(what) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop_detqc("%s needs the arrow package: install.packages(\"arrow\").", what)
  }
  invisible(TRUE)
}

# Tables go to Parquet; anything else but the log and metadata to other.rds.
split_parts <- function(x) {
  nm <- setdiff(names(x), c("log", "meta"))
  is_tab <- vapply(nm, function(k) is.data.frame(x[[k]]), logical(1))
  list(tables = unclass(x)[nm[is_tab]], other = unclass(x)[nm[!is_tab & nm != "animals"]])
}

# arrow returns a tibble with arrow's own attributes; the contract passes
# plain data frames.
as_plain <- function(df) {
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  attr(out, "spec") <- NULL
  rownames(out) <- NULL
  for (nm in names(out)) {
    if (inherits(out[[nm]], "POSIXct")) out[[nm]] <- lubridate::with_tz(out[[nm]], "UTC")
  }
  out
}

log_for_parquet <- function(lg) {
  lg$removed_by <- vapply(lg$removed_by, function(v) {
    as.character(jsonlite::toJSON(as.list(v), auto_unbox = TRUE))
  }, "")
  lg
}

log_from_parquet <- function(lg) {
  lg$removed_by <- lapply(lg$removed_by, function(s) {
    v <- unlist(jsonlite::fromJSON(s, simplifyVector = TRUE))
    if (is.null(v)) stats::setNames(integer(), character()) else
      stats::setNames(as.integer(v), names(v))
  })
  for (nm in c("rows_before", "rows_after", "animals_before", "animals_after", "removed")) {
    lg[[nm]] <- as.integer(lg[[nm]])
  }
  lg[, log_fields]
}

# Times are written as seconds since 1970 in UTC, as hexadecimal text: R
# reads "%a" back bit for bit, which decimal text of 17 digits does not
# always give; tables inside the metadata go as columns.
meta_for_json <- function(meta) {
  lapply(meta, function(v) {
    if (inherits(v, "POSIXct")) return(list(utc_seconds = sprintf("%a", as.numeric(v))))
    v
  })
}

meta_from_json <- function(meta) {
  out <- lapply(meta, function(v) {
    if (is.list(v) && identical(names(v), "utc_seconds")) {
      return(as.POSIXct(as.numeric(v$utc_seconds), origin = "1970-01-01", tz = "UTC"))
    }
    v
  })
  if (!is.null(out$epsg)) out$epsg <- as.integer(out$epsg)
  if (is.data.frame(out$sources) && "rows" %in% names(out$sources)) {
    out$sources$rows <- as.integer(out$sources$rows)
  }
  if (is.list(out$sources) && !is.data.frame(out$sources) && !length(out$sources)) {
    out$sources <- source_table(NULL, NULL, NULL)
  }
  out
}
