# ---- Column maps: how the readers find the contract columns in a file ----

#' Column maps for the import functions
#'
#' Each read function takes its column names, types and time zones from a CSV file
#' in the package. This function returns the packaged map for one table, so
#' that it can be read, copied and extended.
#'
#' A map has one row per source column a target column can come from:
#' * `format`: the file format, such as `"otn"` or `"vue"`.
#' * `target`: the column the read function returns, a contract name where one
#'   exists.
#' * `source`: the column in the file, written as the file writes it or in
#'   lower case with underscores. Two or more source columns joined by `;`
#'   are pasted together with `sep` between them.
#' * `sep`: the text placed between pasted source columns.
#' * `type`: `character`, `numeric`, `integer`, `datetime` or `date`.
#' * `tz`: the time zone of a `datetime` source column, a name from
#'   [OlsonNames()]; blank means UTC.
#' * `note`: free text.
#'
#' Where several rows name one target, the first that holds a value is used,
#' row by row. To read a file the packaged maps do not know, copy the rows of
#' the nearest format, change `source`, and pass the result as `col_map` to
#' the read function. A map given that way replaces the packaged one, and its
#' `format` column, if any, is ignored. [read_detections()] still takes
#' `format`, or guesses it from the header, for the steps that belong to that
#' kind of file. A detection file that no format fits is read by the map
#' alone, and its `source` record gives the format as `"col_map"`. To rename
#' a few columns only, give
#' them as pairs, such as `col_map = c(receiver_serial = "rx")`: each pair
#' replaces the format's rows for its target, and every other target is read
#' as the format reads it. A pair of two or more columns joined by `;` is
#' joined in the order given, with the format's `sep` for its target.
#' Where the format joins columns for a target, the pair names them in the
#' format's order. GLATOS joins `transmitter` from
#' `transmitter_codespace;transmitter_id` in a detection export and from
#' `tag_code_space;tag_id_code` in the tagging sheet of a project workbook,
#' so a renamed ID column is written as
#' `c(transmitter = "transmitter_codespace;tag_id")`. The OTN deployment map
#' reads `station_id` from `STATION_NO` alone in a CSV with `ARRAY`, and from
#' `OTN_ARRAY` joined to `STATION_NO` in a workbook, so a project whose
#' detections name stations the other way gives
#' `c(station_id = "station_no")` or `c(station_id = "array;station_no")`. A
#' pair, or a row of a
#' full map, that joins another number of columns, or that puts one of the
#' format's own columns at another place, stops with an error that gives the
#' format's order. A pair whose column is not in the file stops with an
#' error that lists the file's columns, or the 5 nearest when it has 20 or
#' more.
#'
#' Columns joined for `transmitter`, in a pair or in a row of a full map of
#' any format, must give transmitter codes. A transmitter code is a code
#' space, a dash, and an ID of digits. A code space is letters, a digit, then
#' letters or digits, with at most one dash and digits after it, such as
#' `A69-1601` or `R64K`. Two capitals and a small letter, such as the Thelma
#' `OPi`, are a code space too. The first 100 rows that hold every part are
#' read. The reader stops unless more than half of them hold a code space in
#' the first column and digits in each later column, joined by `"-"`. It
#' stops too when a part is a column the maps read for a receiver, a
#' station, a time or a position. The error says what to change. It names
#' the order where the columns hold the ID before the code space, the `sep`
#' where it is not `"-"`, and otherwise the columns.
#'
#' A pair takes the format's `sep` for its target. The VUE, OTN, VDAT and
#' detqc detection maps read `transmitter` from one column, so a pair joins
#' it with nothing between its columns. Read as `"vue"`,
#' `c(transmitter = "cs;tid")` gives codes such as `A69-160112345` and
#' stops. For such a file, give a full map whose `transmitter` row has the
#' `sep` `"-"`.
#'
#' A pair or a row of one column for `transmitter` is tested in every
#' format, unless the format reads that column for `transmitter` itself. A
#' column the maps read for a receiver, a station, a time or a position
#' stops by its name. A one-column source of whole codes reads. So does a
#' code-space column under a name no packaged map reads, because shape
#' cannot tell the two apart. A code-space column a packaged map reads, such
#' as GLATOS's `transmitter_codespace`, stops by its name. Any other column
#' stops, such as one of IDs alone.
#'
#' A column the table needs that a pair or a full map fills, and that is NA
#' in some rows after the read, gives one warning. It names the column, the
#' NA rows and the rows read. A read with `format =` alone, or with a
#' packaged map copied whole, gives none.
#'
#' **A code space that shares a column.** The reader joins columns but
#' cannot split one. A Thelma Biotel export writes the code space and the
#' frequency together in `Protocol`, as `DS256` and the frequency in kHz
#' joined by a dash, and the ID in `Id`. Add a code-space column to the
#' table before the read, as `CodeSpace = sub("-.*$", "", Protocol)`, and
#' write the table to a CSV file. Read that file with a full map made from
#' the `datetime`, `receiver_serial`, `transmitter`, `sensor_value` and
#' `sensor_unit` rows of `column_map("detections", "vue")`. Its
#' `transmitter` row takes the source `CodeSpace;Id` and the sep `"-"`, and
#' its sensor rows take `Depth (Data 1)` and `Data 1 unit`. The code then
#' holds no frequency, so two tags with one code space and ID on two
#' frequencies would read as one transmitter. `Protocol` is kept as the
#' unmapped column `protocol`: check that each transmitter has one value in
#' it before you go on.
#'
#' A full map is checked against the file in three ways, target by target.
#' First, a target whose rows name no column the file has stops the reader
#' when one of those rows names a column that no packaged map reads, since
#' that is most often a typo. Second, a column the table needs stops the
#' reader when the map fills it with nothing, because its columns are
#' absent or hold no value, and the packaged map of the format being read
#' fills it from the file: the error names the column the map missed.
#' Third, any other target whose rows name only columns the file lacks gives
#' one warning, which says what becomes of each. An optional column is read
#' from the file's own column of that name where the reader keeps unmapped
#' columns and the file has one. Otherwise the table holds no value for it,
#' and the same is true of a needed column that may be blank
#' (`receiver_serial`, `station_id`, `animal_id`, `recover_datetime`), as
#' `format =` leaves it. A packaged map copied whole, as
#' `column_map(table, format)` returns it, with its rows in the same order,
#' reads the file as `format =` does.
#'
#' @param table One of `"detections"`, `"deployments"` or `"animals"`.
#' @param format Optional format; `NULL` returns every format.
#' @return A data frame with the columns `format`, `target`, `source`, `sep`,
#'   `type`, `tz` and `note`.
#' @seealso [read_detections()], [read_deployments()], [read_animals()]
#' @export
#' @examples
#' map <- column_map("detections", "vue")
#' map[, c("target", "source", "type")]
#'
#' # A file with its own names: start from the VUE rows and rename.
#' csv_file <- tempfile(fileext = ".csv")
#' writeLines(c("When (UTC),Rx,Code", "2024-05-01 10:00:00,VR2W-1,A69-1601-1"), csv_file)
#' my_map <- map[map$target %in% c("datetime", "receiver_serial", "transmitter"), ]
#' my_map$source <- c("When (UTC)", "Rx", "Code")
#' read_detections(csv_file, "vue", col_map = my_map)
#'
#' # The same file, with the three renamed columns given as pairs
#' read_detections(csv_file, "vue",
#'                 col_map = c(datetime = "When (UTC)", receiver_serial = "Rx",
#'                             transmitter = "Code"))
column_map <- function(table = c("detections", "deployments", "animals"), format = NULL) {
  table <- check_choice(table, c("detections", "deployments", "animals"), "table")
  path <- system.file("column_maps", paste0(table, ".csv"), package = "detqc")
  map <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = character())
  if (!is.null(format)) {
    if (!format %in% map$format) {
      stop_detqc("The %s map has no format \"%s\"; it has %s.", table, format,
                 paste(unique(map$format), collapse = ", "))
    }
    map <- map[map$format == format, , drop = FALSE]
  }
  rownames(map) <- NULL
  map
}

# ---- Internal: raw tables ----
# Parenthetical notes such as "(yyyy-mm-dd)" or "(m)" are dropped, except a
# time zone: VUE writes "Date and Time (UTC)", which becomes date_and_time_utc.
clean_names <- function(x) {
  x <- sub("^\ufeff", "", x)
  x <- gsub("\\((UTC)[^)]*\\)", " \\1", x, ignore.case = TRUE)
  x <- gsub("\\([^)]*\\)", "", x)
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

# A header such as "Date and Time (UTC-04:00)" states the offset of its
# column; clean_names() keeps only "utc", so the offset is read here, in
# hours, by cleaned name.
header_offsets <- function(raw_names) {
  hit <- regmatches(raw_names, regexec("\\(UTC\\s*([+-])\\s*([0-9]{1,2}):?([0-9]{2})?\\)",
                                       raw_names, ignore.case = TRUE))
  off <- vapply(hit, function(m) {
    if (!length(m)) return(NA_real_)
    mins <- if (nzchar(m[4])) as.numeric(m[4]) else 0
    (if (m[2] == "-") -1 else 1) * (as.numeric(m[3]) + mins / 60)
  }, numeric(1))
  stats::setNames(off, clean_names(raw_names))[!is.na(off)]
}

read_raw_table <- function(file, header_names = NULL, near = "transmitter") {
  if (!is.character(file) || length(file) != 1 || !file.exists(file)) {
    stop_detqc("File not found: %s", paste(file, collapse = ", "))
  }
  stop_empty_file(file)
  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("xls", "xlsx", "xlsm")) return(read_raw_sheet(file, header_names, near))
  if (ext == "parquet") return(read_raw_parquet(file))
  src <- plain_text(file)
  if (src$path != file) on.exit(unlink(src$path))
  inner <- tolower(tools::file_ext(sub("\\.(gz|bz2|xz)$", "", file, ignore.case = TRUE)))
  # Network and vendor exports are comma separated; fread's guess can pick the
  # space in a header such as "Date and Time (UTC)" on a narrow file.
  dt <- data.table::fread(src$path, sep = if (inner == "csv") "," else "auto",
                          colClasses = "character", na.strings = c("", "NA"),
                          fill = TRUE, encoding = "UTF-8", showProgress = FALSE)
  cut <- drop_cut_row(dt, file, src)
  dt <- drop_row_names(cut$dt, src$path)
  offsets <- header_offsets(names(dt))
  stop_header_repeats(dt, file)
  names(dt) <- make.unique(clean_names(names(dt)), sep = "_")
  out <- drop_unit_row(as_df(dt))
  attr(out, "vdat_layout") <- vdat_layout(out)
  attr(out, "header_offsets") <- offsets
  attr(out, "cut_rows") <- cut$n
  out
}

# A Fathom VDAT CSV names its columns in rows of its own: a `RECORD TYPE`
# row, then one row per record type ending `_DESC`. Read as a table, its
# header is no row of column names.
vdat_layout <- function(df) {
  if (!ncol(df)) return(FALSE)
  first <- utils::head(trimws(as.character(df[[1]])), 50)
  any(first %in% "RECORD TYPE" | grepl("^[A-Z_]+_DESC$", first))
}

# write.csv() writes row names by default, as a first column with a blank
# header. Such a column of unique whole numbers holds row indices and is
# dropped, so a table written either way reads back the same.
drop_row_names <- function(dt, path) {
  if (!ncol(dt) || !identical(names(dt)[1], "V1")) return(dt)
  first <- sub("^\ufeff", "", readLines(path, n = 1, warn = FALSE, encoding = "UTF-8"))
  if (!grepl("^(\"\")?[,;\t]", first)) return(dt)
  v <- dt[[1]]
  if (anyNA(v) || !all(grepl("^[0-9]+$", v)) || anyDuplicated(v)) return(dt)
  dt[, -1, with = FALSE]
}

# A file of 0 bytes holds no header to read. An online-only file of a
# cloud-synced folder reads as 0 bytes, and fread's own error names neither
# cause.
stop_empty_file <- function(file) {
  if (isTRUE(file.size(file) == 0)) {
    stop_detqc(paste("%s is empty (0 bytes). A cloud-synced file that is online-only reads",
                     "as 0 bytes: download it first, or leave it out."), basename(file))
  }
  invisible(TRUE)
}

# ---- Internal: compressed files ----
# fread() reads gzip and bzip2 only through the R.utils package, and
# cannot read xz. gzfile() reads all three, and a gzip file of several
# members as one text, so a compressed file is decompressed through it to a
# temporary file that fread() reads. The result names that file, whether
# the stream stops before its end record (`stream_cut`), and the format.
plain_text <- function(file) {
  head <- readBin(file, "raw", 6)
  kind <- compressed_kind(head)
  if (is.na(kind)) return(list(path = file, kind = NA_character_, stream_cut = FALSE))
  out <- tempfile(fileext = ".txt")
  dec <- tryCatch(decompress_to(file, out, kind), error = function(e) e)
  if (inherits(dec, "error")) {
    # Padding after a cut can stop the decoder; without it the text up to
    # the cut is read. When the file still fails, its text file is deleted
    # before the error, so that tempdir() does not fill with partial copies.
    redo <- if (kind == "gzip") decode_gzip_again(file, out, kind) else NULL
    if (is.null(redo$dec)) {
      unlink(out)
      stop(dec)
    }
    dec <- redo$dec
    cut <- redo$cut
  } else {
    cut <- compressed_stream_cut(file, kind, dec)
    if (cut && kind == "gzip") {
      redo <- decode_without_pad(file, out, kind, dec)
      dec <- redo$dec
      cut <- redo$cut
    }
  }
  if (cut && kind == "bzip2") dec$total <- dec$total + bzip2_rest(file, out, dec$total)
  if (dec$total == 0) {
    unlink(out)
    stop_no_text(file, kind)
  }
  list(path = out, kind = kind, stream_cut = cut)
}

# R's bzip2 decoder drops the text of the read call that meets the end of a
# cut stream, up to one call's worth. The text decoded so far is read again
# in large calls, and the rest one byte per call, so that the last complete
# block is kept whole. Returns the number of bytes added to `out`.
bzip2_rest <- function(file, out, total) {
  con <- gzfile(file, "rb")
  on.exit(close(con))
  got <- 0
  while (got < total) {
    block <- suppressWarnings(readBin(con, "raw", min(1048576, total - got)))
    if (!length(block)) return(0)
    got <- got + length(block)
  }
  dest <- file(out, "ab")
  on.exit(close(dest), add = TRUE)
  buf <- raw(65536)
  k <- 0L
  added <- 0
  repeat {
    one <- suppressWarnings(readBin(con, "raw", 1L))
    if (length(one)) {
      k <- k + 1L
      buf[k] <- one
    }
    if (k == length(buf) || (!length(one) && k > 0L)) {
      writeBin(buf[seq_len(k)], dest)
      added <- added + k
      k <- 0L
    }
    if (!length(one)) break
  }
  added
}

# gzfile() meets a gzip file that stops inside its 8-byte end record with
# an unclassed error that blames a full disk, so the error is raised again
# with the file's name. The xz decoder's warning on a cut stream is the only
# sign of the cut, so it is recorded.
decompress_to <- function(file, out, kind) {
  con <- gzfile(file, "rb")
  on.exit(close(con))
  dest <- file(out, "wb")
  on.exit(close(dest), add = TRUE)
  total <- 0
  warned <- FALSE
  failed <- tryCatch(withCallingHandlers(repeat {
    block <- readBin(con, "raw", 1048576)
    if (!length(block)) break
    writeBin(block, dest)
    total <- total + length(block)
  }, warning = function(cnd) {
    warned <<- TRUE
    invokeRestart("muffleWarning")
  }), error = function(e) conditionMessage(e))
  if (is.character(failed)) {
    stop_detqc(paste("%s is a %s file whose stream stops before its end, as an interrupted copy",
                     "or sync leaves a file, and its decoder stopped (%s). Copy it again."),
               basename(file), kind, failed)
  }
  list(total = total, warned = warned)
}

stop_no_text <- function(file, kind) {
  why <- if (identical(kind, "bzip2")) {
    paste(" bzip2 decodes whole blocks of up to 900 kB of text, so a file cut inside its",
          "first block gives no row.")
  } else {
    ""
  }
  stop_detqc(paste0("%s holds no line of text. A compressed file cut part-way can decompress",
                    " to nothing.%s Copy it again."), basename(file), why)
}

# Whether a compressed stream stops before its end record. A gzip member
# ends with its text's length modulo 2^32, least significant byte first;
# zero bytes after it, as a tape or tar block pads a file, are skipped. A
# bzip2 stream ends with the 48-bit marker 0x177245385090 at any bit
# offset, then a 32-bit check and at most 7 bits of padding, so within its
# last 11 bytes. xz is judged by its decoder's warning.
compressed_stream_cut <- function(file, kind, dec) {
  if (kind == "xz") return(dec$warned)
  size <- file.size(file)
  con <- file(file, "rb")
  on.exit(close(con))
  if (kind == "gzip") {
    seek(con, max(0, size - 65536))
    last <- readBin(con, "raw", 65536)
    return(gzip_cut(file, last, dec$total))
  }
  seek(con, max(0, size - 11))
  last <- readBin(con, "raw", 11)
  bits <- paste(vapply(last, function(b) paste(rev(as.integer(rawToBits(b))), collapse = ""),
                       character(1)), collapse = "")
  !grepl("000101110111001001000101001110000101000010010000", bits, fixed = TRUE)
}

# The bytes at the end of a file that pad it: a run of zero bytes of any
# length, or a run of `ff_min` or more 0xFF bytes, as erased flash memory
# reads. A gzip end record can itself end in 0xFF bytes, so a shorter run
# counts only where the decoder has already failed on the file.
trailing_pad <- function(bytes, ff_min = 4L) {
  n <- length(bytes)
  if (!n || !bytes[n] %in% as.raw(c(0x00, 0xff))) return(0L)
  other <- which(bytes != bytes[n])
  run <- if (length(other)) n - max(other) else n
  if (bytes[n] == as.raw(0xff) && run < ff_min) 0L else as.integer(run)
}

# A cut gzip stream followed by padding decodes the padding as more of the
# stream, into text that was never in the file: zero bytes give rows that
# repeat earlier text. The stream is decoded again without the padding, so
# the text stops where the file was cut. When that decode stops with an
# error, the compressed text ran to its end and only the end record is
# short, so the first decode holds every row and the file is not cut.
# `dec` is NULL for a file whose first decode failed. In such a file a run
# of zero or 0xFF bytes of any length at the end counts as padding.
decode_without_pad <- function(file, out, kind, dec) {
  bytes <- readBin(file, "raw", file.size(file))
  least <- if (is.null(dec)) 1L else 4L
  pad <- trailing_pad(bytes, least)
  if (pad < least) return(list(dec = dec, cut = TRUE))
  part <- tempfile(fileext = ".gz")
  text <- tempfile(fileext = ".txt")
  on.exit(unlink(c(part, text)))
  writeBin(bytes[seq_len(length(bytes) - pad)], part)
  redo <- tryCatch(decompress_to(part, text, kind), error = function(e) NULL)
  if (is.null(redo)) return(list(dec = dec, cut = FALSE))
  file.copy(text, out, overwrite = TRUE)
  list(dec = redo, cut = TRUE)
}

# Called when R's decoder stops with an error on a gzip file. A whole
# stream followed by padding, or by 1 to 3 other bytes, gives that error
# too. So the file is decoded again without a run of zero or 0xFF bytes at
# its end, and when that also fails, without its last 1, 2 or 3 bytes.
decode_gzip_again <- function(file, out, kind) {
  redo <- decode_without_pad(file, out, kind, NULL)
  if (is.null(redo$dec)) redo <- decode_without_tail(file, out, kind)
  redo
}

# 1 to 3 bytes after a whole gzip stream that begin as a member header
# (1f 8b) make the decoder look for a second member and fail. The stream is
# decoded again without them. It counts as whole only when that decode
# succeeds and the length record in its last 4 bytes matches the text.
decode_without_tail <- function(file, out, kind) {
  bytes <- readBin(file, "raw", file.size(file))
  part <- tempfile(fileext = ".gz")
  text <- tempfile(fileext = ".txt")
  on.exit(unlink(c(part, text)))
  for (k in seq_len(min(3L, length(bytes) - 4L))) {
    m <- length(bytes) - k
    writeBin(bytes[seq_len(m)], part)
    redo <- tryCatch(decompress_to(part, text, kind), error = function(e) NULL)
    if (is.null(redo)) next
    isize <- sum(as.numeric(bytes[(m - 3):m]) * 256^(0:3))
    if (isize != redo$total %% 2^32 && !gzip_last_member_whole(part, isize)) next
    file.copy(text, out, overwrite = TRUE)
    return(list(dec = redo, cut = FALSE))
  }
  list(dec = NULL, cut = TRUE)
}

# The 4-byte length record can end at the last byte of the file. When the
# file ends in padding, the record can also end anywhere from the last byte
# before the padding to 4 bytes into it, because the high bytes of the
# length are often zero themselves. A file with 1 to 3 other bytes after
# the stream has the record end 1 to 3 bytes before the file does; those
# ends are tested on the length alone.
gzip_cut <- function(file, last, total) {
  n <- length(last)
  if (n < 4) return(TRUE)
  pad <- trailing_pad(last)
  ends <- unique(c(n, seq(n - pad, min(n, n - pad + 4))))
  ends <- ends[ends >= 4]
  short_tail <- setdiff((n - 3):(n - 1), ends)
  short_tail <- short_tail[short_tail >= 4]
  isize_at <- function(e) sum(as.numeric(last[e - 3:0]) * 256^(0:3))
  isizes <- vapply(ends, isize_at, numeric(1))
  tail_sizes <- vapply(short_tail, isize_at, numeric(1))
  if (any(c(isizes, tail_sizes) == total %% 2^32)) return(FALSE)
  for (i in seq_along(ends)) {
    if (gzip_last_member_whole(file, isizes[i], n - ends[i])) return(FALSE)
  }
  TRUE
}

# A gzip file of several members, as `cat a.gz b.gz` or a parallel
# compressor writes, records only its last member's length. Each place the
# member header (1f 8b 08, flags below 32) occurs is tried from the end:
# the file is whole when the text from that place decodes with no error to
# the recorded length. A cut file's last 4 bytes are compressed data, which
# match the length decoded from a member start by chance only.
gzip_last_member_whole <- function(file, isize, pad = 0) {
  bytes <- readBin(file, "raw", file.size(file))
  bytes <- bytes[seq_len(max(0, length(bytes) - pad))]
  n <- length(bytes)
  if (n < 4) return(FALSE)
  at <- which(bytes[-c(n - 1, n)] == as.raw(0x1f))
  at <- at[at > 1 & at + 3 <= n]
  at <- at[bytes[at + 1] == as.raw(0x8b) & bytes[at + 2] == as.raw(0x08) &
             as.integer(bytes[at + 3]) < 32]
  part <- tempfile(fileext = ".gz")
  on.exit(unlink(part))
  for (i in rev(at)) {
    writeBin(bytes[i:n], part)
    got <- tryCatch(suppressWarnings(decoded_length(part)), error = function(e) NA_real_)
    if (isTRUE(got %% 2^32 == isize)) return(TRUE)
  }
  FALSE
}

decoded_length <- function(file) {
  con <- gzfile(file, "rb")
  on.exit(close(con))
  total <- 0
  repeat {
    block <- readBin(con, "raw", 1048576)
    if (!length(block)) break
    total <- total + length(block)
  }
  total
}

# ---- Internal: a file cut mid-row ----
# An interrupted copy or sync leaves a file whose last line stops part-way
# through a row, with no line end. fread(fill = TRUE) pads such a row with
# NA, so a serial cut to "119" would read as a receiver and a time cut to
# "2" as a date. The last line is cut when the text does not end in a line end
# and the line holds fewer fields than the header. A file whose rows are
# short by design, as Fathom's flat exports are, still ends each row with a
# line end, so it is not affected. A compressed file is tested on its
# decompressed text, and its stream on its end record.
last_line_cut <- function(path) {
  size <- file.size(path)
  if (is.na(size) || size == 0) return(NULL)
  con <- file(path, "rb")
  on.exit(close(con))
  n <- min(size, 1048576)
  seek(con, size - n)
  tail <- readBin(con, "raw", n)
  if (!length(tail) || tail[length(tail)] %in% as.raw(c(10, 13))) return(NULL)
  nl <- which(tail == as.raw(10))
  part <- tail[if (length(nl)) (max(nl) + 1):length(tail) else seq_along(tail)]
  rawToChar(part[part != as.raw(0)])
}

# The magic bytes of gzip (1f 8b), bzip2 ("BZh") and xz (fd "7zXZ" 00).
compressed_kind <- function(head) {
  starts <- function(magic) length(head) >= length(magic) && all(head[seq_along(magic)] == magic)
  if (starts(as.raw(c(0x1f, 0x8b)))) return("gzip")
  if (starts(charToRaw("BZh"))) return("bzip2")
  if (starts(as.raw(c(0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00)))) return("xz")
  NA_character_
}

is_compressed <- function(head) !is.na(compressed_kind(head))

# Fields in one line of comma-separated text; quoted commas do not count.
n_fields <- function(line) {
  bare <- gsub("\"[^\"]*(\"|$)", "", line)
  nchar(gsub("[^,]", "", bare)) + 1L
}

drop_cut_row <- function(dt, file, src = list(path = file, stream_cut = FALSE)) {
  line <- last_line_cut(src$path)
  short <- !is.null(line) && nrow(dt) > 0 && n_fields(line) < ncol(dt)
  if (isTRUE(src$stream_cut)) {
    if (short) dt <- dt[-nrow(dt)]
    warn_detqc(paste("%s is a compressed file whose data stop before the end of the stream, as",
                     "an interrupted copy or sync leaves a file. %s data rows were read; the",
                     "rows after them are missing%s."),
               basename(file), fmt_n(nrow(dt)),
               if (short) ", and the part-row at the cut was dropped" else "")
    return(list(dt = dt, n = as.integer(short)))
  }
  if (!short) return(list(dt = dt, n = 0L))
  warn_detqc(paste("%s ends part-way through data row %s (%s of %s fields and no line end), as",
                   "an interrupted copy or sync leaves a file. The row was dropped."),
             basename(file), fmt_n(nrow(dt)), fmt_n(n_fields(line)), fmt_n(ncol(dt)))
  list(dt = dt[-nrow(dt)], n = 1L)
}

# A header repeated inside the data, as two exports pasted into one file
# give, would read as a row of junk: in an animals file, as an animal named
# "animal_id". A row that repeats the header in its first column and in at
# least half of its filled cells stops the reader.
stop_header_repeats <- function(dt, file) {
  if (!nrow(dt) || ncol(dt) < 2) return(invisible(TRUE))
  heads <- names(dt)
  cand <- which(dt[[1]] %in% heads[1])
  for (i in cand) {
    row <- unlist(dt[i], use.names = FALSE)
    filled <- !is.na(row)
    if (sum(row[filled] == heads[filled]) >= max(2, sum(filled) / 2)) {
      stop_detqc(paste("Data row %s of %s repeats the header, as files pasted together do",
                       "(%s with the header's first name). Read each file on its",
                       "own, or remove those rows."),
                 fmt_n(i), basename(file),
                 paste(n_of(length(cand), "row"), verb(length(cand), "starts", "start")))
    }
  }
  invisible(TRUE)
}

# OTN publishes its extracts as parquet as well as CSV. Every column is read
# as text so the map's types apply as they do to a CSV file. A binary or
# list column, such as the WKB `geometry` of an OTN extract, has no text
# form, so it is dropped with a message that names it; no map reads it.
read_raw_parquet <- function(file) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop_detqc("Reading %s needs the arrow package: install.packages(\"arrow\").",
               basename(file))
  }
  df <- as.data.frame(arrow::read_parquet(file))
  nested <- names(df)[vapply(df, function(v) is.list(v) || inherits(v, "arrow_binary"),
                             logical(1))]
  if (length(nested)) {
    message(sprintf("%s: %s %s binary or list values and %s dropped: %s.", basename(file),
                    n_of(length(nested), "column"), if (length(nested) == 1) "holds" else "hold",
                    if (length(nested) == 1) "was" else "were",
                    paste(nested, collapse = ", ")))
    df <- df[setdiff(names(df), nested)]
  }
  for (nm in names(df)) {
    v <- df[[nm]]
    df[[nm]] <- if (inherits(v, "POSIXct")) {
      format(lubridate::with_tz(v, "UTC"), "%Y-%m-%d %H:%M:%OS6")
    } else {
      as.character(v)
    }
  }
  names(df) <- make.unique(clean_names(names(df)), sep = "_")
  df
}

# OTN AAT files put a row of units under the header.
drop_unit_row <- function(df) {
  if (!nrow(df)) return(df)
  first <- unlist(df[1, ], use.names = FALSE)
  first <- first[!is.na(first)]
  units <- c("degrees_north", "degrees_east", "utc", "m", "km", "s", "degrees")
  if (length(first) && all(tolower(first) %in% units)) df <- df[-1, , drop = FALSE]
  df
}

# Excel sheets carry title rows above the header. The header is the first row
# in which at least two cells name a column the map knows.
read_raw_sheet <- function(file, header_names, near = "transmitter") {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop_detqc("Reading %s needs the readxl package: install.packages(\"readxl\").",
               basename(file))
  }
  best <- list(hits = 1, x = NULL, row = 0)
  for (sh in readxl::excel_sheets(file)) {
    x <- read_sheet_text(file, sh)
    if (!nrow(x)) next
    hits <- vapply(seq_len(min(30, nrow(x))), function(i) {
      sum(clean_names(unlist(x[i, ], use.names = FALSE)) %in% header_names)
    }, numeric(1))
    # A workbook holds several sheets; the one whose header names the most
    # mapped columns is the table, as a GLATOS workbook's deployment sheet is.
    if (max(hits) > best$hits) best <- list(hits = max(hits), x = x, row = which.max(hits))
  }
  if (is.null(best$x)) {
    stop_detqc("No sheet in %s has a header row this format's column map recognises. %s.",
               basename(file), sheet_columns(file, near))
  }
  x <- best$x
  h <- best$row
  body <- as.data.frame(x[-seq_len(h), , drop = FALSE], stringsAsFactors = FALSE)
  nm <- clean_names(unlist(x[h, ], use.names = FALSE))
  nm[is.na(nm) | !nzchar(nm)] <- paste0("column_", which(is.na(nm) | !nzchar(nm)))
  names(body) <- make.unique(nm, sep = "_")
  body[rowSums(!is.na(body)) > 0, , drop = FALSE]
}

read_sheet_text <- function(file, sheet) {
  suppressMessages(readxl::read_excel(file, sheet = sheet, col_names = FALSE,
                                      col_types = "text", .name_repair = "minimal"))
}

# The columns of a workbook, for a message: those of its widest sheet, in
# the row of its first 30 that fills the most cells, through file_columns().
# The rows below that header are passed too, so that a column of codes
# ranks first.
sheet_columns <- function(file, near) {
  best <- list(n = 0, sheet = NA_character_, names = character(), body = NULL)
  for (sh in readxl::excel_sheets(file)) {
    x <- read_sheet_text(file, sh)
    if (!nrow(x)) next
    filled <- vapply(seq_len(min(30, nrow(x))), function(i) sum(!is.na(unlist(x[i, ]))), numeric(1))
    if (max(filled) <= best$n) next
    h <- which.max(filled)
    cells <- unlist(x[h, ], use.names = FALSE)
    body <- as.data.frame(x[-seq_len(h), !is.na(cells), drop = FALSE], stringsAsFactors = FALSE)
    names(body) <- make.unique(clean_names(cells[!is.na(cells)]), sep = "_")
    best <- list(n = max(filled), sheet = sh, names = names(body), body = body)
  }
  if (!best$n) return("Its sheets hold no cells")
  owner <- sprintf("Its widest sheet, \"%s\",", best$sheet)
  if (length(best$names) < 20) return(file_columns(near, best$names, owner = owner))
  sprintf("%s has %s columns. %s", owner, fmt_n(length(best$names)),
          file_columns(near, best$names, best$body))
}

# ---- Internal: column maps ----
get_column_map <- function(table, format, col_map) {
  if (!is.null(col_map) && !is_map_pairs(col_map)) {
    col_map <- if (is.character(col_map)) read_map_file(col_map) else check_map_frame(col_map)
    if (packaged_copy(col_map, table, format)) col_map <- NULL
  }
  if (is.null(col_map)) {
    map <- column_map(table, format)
  } else if (is_map_pairs(col_map)) {
    map <- merge_map_pairs(table, format, col_map)
  } else {
    map <- col_map
    need <- c("target", "source")
    if (!all(need %in% names(map))) stop_detqc("`col_map` needs columns `target` and `source`.")
    for (k in setdiff(c("sep", "type", "tz"), names(map))) map[[k]] <- ""
    map$type[is.na(map$type) | !nzchar(map$type)] <- "character"
    attr(map, "format_rows") <- column_map(table, format)
  }
  map$sep[is.na(map$sep)] <- ""
  map$tz[is.na(map$tz) | !nzchar(map$tz)] <- "UTC"
  bad_tz <- setdiff(map$tz, OlsonNames())
  if (length(bad_tz)) stop_detqc("Column map time zone \"%s\" is not in OlsonNames().", bad_tz[1])
  map$source <- vapply(strsplit(map$source, ";", fixed = TRUE), function(s) {
    paste(clean_names(s), collapse = ";")
  }, "")
  structure(map, source_parts = unique(unlist(strsplit(map$source, ";", fixed = TRUE))),
            user_map = !is.null(col_map) && !is_map_pairs(col_map),
            pairs = if (is_map_pairs(col_map)) col_map)
}

# A packaged map copied whole, as column_map(table, format) returns it, is
# read as `format =` reads the file. OTN reads a detection extract with its
# "otn" rows when the extract is matched to animals and with its
# "otn_qualified" rows otherwise, so a copy of either reads as the reader
# picks for the extract.
packaged_copy <- function(col_map, table, format) {
  cols <- c("target", "source", "sep", "type", "tz")
  if (!is.data.frame(col_map) || !all(cols %in% names(col_map))) return(FALSE)
  maps <- column_map(table)
  kin <- if (table == "detections" && format %in% c("otn", "otn_qualified")) {
    c("otn", "otn_qualified")
  } else {
    format
  }
  plain <- function(d) {
    lapply(d[cols], function(v) {
      v <- as.character(v)
      v[is.na(v)] <- ""
      v
    })
  }
  any(vapply(intersect(kin, maps$format), function(fm) {
    identical(plain(col_map), plain(maps[maps$format == fm, , drop = FALSE]))
  }, logical(1)))
}

# `col_map` is a data frame or the path to a CSV file of one, which replaces
# the packaged map, or a named character vector of pairs, which is merged
# into the format's map.
col_map_takes <- paste("`col_map` takes a data frame with the columns `target` and `source`, as",
                       "column_map() returns, the path to a CSV file of one, or pairs written as",
                       "c(target = \"source\").")

# A named vector of NA, such as c(transmitter = NA), is taken as pairs too,
# so that its refusal names the missing source.
is_map_pairs <- function(col_map) {
  !is.null(names(col_map)) && !is.list(col_map) &&
    (is.character(col_map) || (is.atomic(col_map) && all(is.na(col_map))))
}

# The map of a sheet read with format = "auto", once its format is picked:
# that format's map, with any pairs merged into it. A full map of the
# user's own is checked against the picked format's rows. A format given by
# name keeps the map built for it.
picked_map <- function(map, table, format, picked, col_map) {
  if (format != "auto") return(map)
  get_column_map(table, picked, col_map)
}

# Each pair replaces the format's rows for its target; every other target
# keeps the format's rows. A name that is no target of the table is refused,
# so pairs written the other way round, c(source = "target"), stop with the
# targets listed. A target the format builds from two columns only, as
# GLATOS builds `transmitter`, keeps the format's `sep` for a pair of two
# columns joined by ";", and refuses a pair of one column, whose codes
# would lack a part.
merge_map_pairs <- function(table, format, pairs) {
  all_rows <- column_map(table)
  known <- unique(all_rows$target)
  tg <- names(pairs)
  no_source <- is.na(pairs) | !nzchar(pairs)
  if (any(no_source)) {
    stop_detqc("%s The pair for `%s` gives no source column (%s). Write it as c(%s = \"source\").",
               col_map_takes, tg[no_source][1],
               if (is.na(pairs[no_source][1])) "NA" else "\"\"", tg[no_source][1])
  }
  bad <- is.na(tg) | !nzchar(tg) | !tg %in% known
  if (any(bad)) {
    stop_detqc(paste("%s The pair %s names no target of the %s map. Write each pair as",
                     "c(target = \"source\"), with a target from: %s."),
               col_map_takes, sprintf("%s = \"%s\"", tg[bad][1], pairs[bad][1]), table,
               paste(known, collapse = ", "))
  }
  if (anyDuplicated(tg)) {
    stop_detqc("%s The target `%s` is named twice.", col_map_takes, tg[duplicated(tg)][1])
  }
  base <- column_map(table, format)
  like <- rbind(base, all_rows)
  first <- like[match(tg, like$target), , drop = FALSE]
  sep <- vapply(seq_along(tg), function(i) {
    pair_sep(base[base$target == tg[i], , drop = FALSE], pairs[[i]])
  }, "")
  add <- data.frame(format = format, target = tg, source = unname(pairs), sep = sep,
                    type = first$type, tz = first$tz, note = "from col_map",
                    stringsAsFactors = FALSE)
  structure(rbind(add, base[!base$target %in% tg, , drop = FALSE]), format_rows = base)
}

# The `sep` of one pair, from the format's rows for its target. A pair of
# two or more columns joined by ";" takes the `sep` of the format's joined
# row, and none where the format reads the target from one column.
pair_sep <- function(rows, source) {
  joined <- grepl(";", rows$source, fixed = TRUE)
  if (!any(joined) || !grepl(";", source, fixed = TRUE)) return("")
  rows$sep[joined][1]
}

# The parts of each row that joins columns for one target, in lower case
# with underscores, as the file's names are compared.
joined_parts <- function(rows, target) {
  src <- rows$source[rows$target == target]
  lapply(strsplit(src[grepl(";", src, fixed = TRUE)], ";", fixed = TRUE), clean_names)
}

# The checks on one pair. A pair is joined in the order given, so a pair
# whose part count differs from the format's joined row, or that puts one
# of that row's columns at another place, would give codes whose parts are
# missing or swapped. It stops with the format's order. A pair whose
# column is not in the file stops, so that a typo cannot leave its target
# empty. The values of a `transmitter` pair are then tested by
# check_code_shape(). Spaces around ";" are dropped first.
check_pair_sources <- function(raw, map, table, keep_unmapped = TRUE) {
  pairs <- attr(map, "pairs")
  if (is.null(pairs)) return(check_map_sources(raw, map, table, keep_unmapped))
  rows <- attr(map, "format_rows")
  have <- names(raw)
  for (i in seq_along(pairs)) {
    tg <- names(pairs)[i]
    parts <- trimws(strsplit(pairs[[i]], ";", fixed = TRUE)[[1]])
    src <- paste(parts, collapse = ";")
    lead <- list(what = sprintf("The pair %s = \"%s\"", tg, src), pair = TRUE,
                 sep = map$sep[match(tg, map$target)])
    if (length(parts) > 1) {
      check_pair_order(parts, joined_parts(rows, tg), tg, lead, rows, table, have)
    }
    gone <- parts[!clean_names(parts) %in% have]
    if (length(gone)) {
      what <- if (length(parts) > 1) sprintf("the column \"%s\", which", gone[1]) else "a column"
      stop_detqc("The pair %s = \"%s\" names %s this %s file does not have. %s.",
                 tg, src, what, table, file_columns(gone[1], have))
    }
    check_code_shape(raw, clean_names(parts), tg, lead, map, table)
  }
  invisible(TRUE)
}

# The file's columns for a message: all of them, or the 5 nearest to
# `name` when the file has 20 or more. A text file with no header row gets
# the names fread() gives such a file, V1, V2 and on, which say nothing, so
# the message says the file has no header row, or names a VDAT CSV, whose
# column names sit in rows of its own. `raw`, when given, lets a column
# that holds transmitter codes rank first for `transmitter`.
file_columns <- function(name, have, raw = NULL, owner = "The file") {
  if (isTRUE(attr(raw, "vdat_layout"))) {
    return(sprintf(paste("%s is a Fathom VDAT CSV: read its detections, if it holds any, with",
                         "read_detections(format = \"vdat\") and its status records with",
                         "read_receiver_logs()"), owner))
  }
  if (length(have) && identical(have, paste0("v", seq_along(have)))) {
    return(sprintf("%s has no header row of column names", owner))
  }
  if (length(have) < 20) return(sprintf("%s has: %s", owner, paste(have, collapse = ", ")))
  paste("Its nearest columns:", paste(nearest_columns(name, have, 5, raw), collapse = ", "))
}

# Columns that share a word with `name`, words being the parts between
# underscores, rank first, most shared words first. Edit distance orders
# the rest and breaks ties, so "rx_sn" ranks "receiver_sn" above "sex".
# `name` may hold several names, as the targets a deployment sheet needs;
# a column then scores by the name it is nearest to. For `transmitter`, a
# column whose values are whole transmitter codes ranks above all of them,
# whatever its name, as `vue_tag_id` does; the columns a packaged map
# joins into a code, such as tag_code_space and tag_id_code, come next.
nearest_columns <- function(name, have, n, raw = NULL) {
  key <- clean_names(name)
  shared <- vapply(strsplit(have, "_", fixed = TRUE), function(w) {
    max(vapply(strsplit(key, "_", fixed = TRUE), function(k) sum(k %in% w), numeric(1)))
  }, numeric(1))
  d <- apply(utils::adist(key, have), 2, min)
  rank <- rep(0, length(have))
  if ("transmitter" %in% key) {
    rank[have %in% code_part_columns()] <- 1
    if (!is.null(raw)) {
      codes <- vapply(have, function(k) holds_codes(raw[[k]]), logical(1))
      rank[codes & !have %in% receiver_columns(NULL)] <- 2
    }
  }
  have[order(-rank, -shared, d)][seq_len(min(n, length(have)))]
}

# Every column a packaged map joins for `transmitter`.
code_part_columns <- function() {
  src <- unlist(lapply(c("detections", "animals"), function(tb) {
    m <- column_map(tb)
    m$source[m$target == "transmitter" & grepl(";", m$source, fixed = TRUE)]
  }))
  unique(clean_names(unlist(strsplit(src, ";", fixed = TRUE))))
}

# A code space is letters, a digit, then letters or digits, with at most
# one dash and digits after it, as A69-1601, A180-1702 or R64K; or two
# capitals and a small letter, as the Thelma OPi and OPs. An ID is digits. A transmitter code is
# the two joined by a dash. The narrow shape keeps words, such as a species
# or a sex, and tag models, such as V13-1x, from reading as code spaces.
space_shape <- "([A-Za-z]+[0-9][A-Za-z0-9]*(-[0-9]+)?|[A-Z]{2}[a-z])"
space_pattern <- paste0("^", space_shape, "$")
code_pattern <- paste0("^", space_shape, "-[0-9]+$")

is_space <- function(v) grepl(space_pattern, trimws(v))
is_id <- function(v) grepl("^[0-9]+$", trimws(v))
is_code <- function(v) grepl(code_pattern, trimws(v))

# TRUE when more than half the first 100 values that are not blank are
# transmitter codes.
holds_codes <- function(v) {
  if (is.null(v)) return(FALSE)
  v <- utils::head(stats::na.omit(blank_na(as.character(v))), 100)
  length(v) > 0 && sum(is_code(v)) > length(v) / 2
}

# How a map joins `target`, for a message: the joined rows in order, the
# number of columns and the text between them.
joined_order <- function(joined, rows, target, table) {
  sep <- rows$sep[rows$target == target & grepl(";", rows$source, fixed = TRUE)][1]
  list(order = paste(vapply(joined, paste, "", collapse = ";"), collapse = " or "),
       n = length(joined[[1]]),
       that = if (length(joined) > 1) "one of those orders" else "that order",
       how = sprintf("the %s map of format \"%s\" joins `%s` from %s columns, with %s",
                     table, rows$format[1], target, fmt_n(length(joined[[1]])),
                     if (nzchar(sep)) sprintf("\"%s\"", sep) else "nothing"))
}

# The joined rows a message quotes: those whose columns the file holds
# most of, or every joined row when it holds none of them.
held_rows <- function(joined, have) {
  held <- vapply(joined, function(p) sum(p %in% have), numeric(1))
  if (max(held) > 0) joined[held == max(held)] else joined
}

# The end of a refusal: a pair is rewritten as a pair, and a row of a
# data-frame map by its `source`.
joined_fix <- function(lead, target, fixed) {
  if (lead$pair) return(sprintf("Write c(%s = \"%s\").", target, fixed))
  sprintf("Write its source as \"%s\".", fixed)
}

joined_given <- function(lead) {
  if (lead$pair) "A pair is joined in the order given" else "A row is joined in the order given"
}

check_pair_order <- function(parts, joined, target, lead, rows, table, have) {
  if (!length(joined)) return(invisible(TRUE))
  jo <- joined_order(held_rows(joined, have), rows, target, table)
  if (length(parts) != jo$n) {
    copy <- if (lead$pair) {
      sprintf(paste(", or give a full map as a data frame: copy column_map(\"%s\", \"%s\") and",
                    "change `source`"), table, rows$format[1])
    } else {
      ""
    }
    stop_detqc(paste("%s names %s columns, and %s between them, in the order %s. %s. Name %s",
                     "columns in %s, with each column as the file has it%s."),
               lead$what, fmt_n(length(parts)), jo$how, jo$order, joined_given(lead),
               fmt_n(jo$n), jo$that, copy)
  }
  key <- clean_names(parts)
  if (anyDuplicated(key)) {
    stop_detqc("%s names the column \"%s\" twice; %s between them.",
               lead$what, parts[duplicated(key)][1], jo$how)
  }
  for (p in joined) {
    at <- match(key, p)
    moved <- !is.na(at) & at != seq_along(key)
    if (!any(moved)) next
    fixed <- rep(NA_character_, length(p))
    fixed[at[!is.na(at)]] <- parts[!is.na(at)]
    fixed[is.na(fixed)] <- parts[is.na(at)]
    stop_detqc(paste("%s names %s as part %s of %s, and %s between them, in the order %s. %s,",
                     "so these codes would have their parts swapped. %s"),
               lead$what, parts[moved][1], fmt_n(which(moved)[1]), fmt_n(length(parts)),
               joined_order(list(p), rows, target, table)$how, paste(p, collapse = ";"),
               joined_given(lead), joined_fix(lead, target, paste(fixed, collapse = ";")))
  }
  invisible(TRUE)
}

# The names of renamed columns cannot show what they hold, so the values
# of a `transmitter` source are read: the first 100 rows that hold every
# part. Joined, they must give transmitter codes (`code_pattern`) in more
# than half of those rows, on every map and pair, whatever the format. A
# source of one column is tested the same way in every format, unless it
# is a column the format itself reads for `transmitter` alone, as VUE's
# `transmitter` or OTN's `tagname`.
check_code_shape <- function(raw, keys, target, lead, map, table) {
  if (target != "transmitter" || !all(keys %in% names(raw))) return(invisible(TRUE))
  vals <- code_values(raw, keys)
  if (is.null(vals)) return(invisible(TRUE))
  rows <- attr(map, "format_rows")
  joined <- joined_parts(rows, target)
  jo <- if (length(joined)) joined_order(held_rows(joined, names(raw)), rows, target, table)
  if (length(keys) > 1) return(check_joined_code(vals, keys, lead, jo, map, rows, table))
  own <- clean_names(rows$source[rows$target == target & !grepl(";", rows$source, fixed = TRUE)])
  if (!keys %in% own) check_one_part(vals, keys, lead, jo, map, rows, table)
  invisible(TRUE)
}

# The values of each part in the first 100 rows that hold every part, or
# NULL when no row holds them all.
code_values <- function(raw, keys) {
  vals <- lapply(keys, function(k) trimws(blank_na(as.character(raw[[k]]))))
  ok <- utils::head(which(Reduce(`&`, lapply(vals, function(v) !is.na(v) & nzchar(v)))), 100)
  if (!length(ok)) return(NULL)
  lapply(vals, `[`, ok)
}

# The columns any packaged map, or the map given, reads for a receiver, a
# station, a time or a position. A code's parts are the tag's, so such a
# column in a code is a wrong column, even where its values take the shape
# of a code, as OTN deployment ids such as HFX068-00348-1 do. A column a
# packaged map reads for `transmitter` is left out.
receiver_columns <- function(map) {
  tg <- c("receiver_serial", "receiver_model", "station_id", "deploy_lat", "deploy_lon",
          "release_lat", "release_lon")
  pick <- function(m, what) {
    if (is.null(m)) return(character())
    when <- if (is.null(m$type)) FALSE else m$type %in% c("datetime", "date")
    rows <- if (what == "tx") m$target == "transmitter" else m$target %in% tg | when
    unlist(strsplit(m$source[rows], ";", fixed = TRUE))
  }
  maps <- lapply(c("detections", "deployments", "animals"), column_map)
  src <- unlist(lapply(c(maps, list(map)), pick, "other"))
  tx <- unlist(lapply(maps, pick, "tx"))
  setdiff(unique(clean_names(src)), clean_names(tx))
}

# A joined source must hold a code space in its first column and IDs of
# digits in the rest, joined by "-" (code_rows()). A source of two columns
# whose first part is digits and second a code space is refused as
# swapped, and a source that would give codes with "-" in place of its own
# `sep` is refused with that `sep`. Only these two refusals advise a
# source, so that the advice reads right.
check_joined_code <- function(vals, keys, lead, jo, map, rows, table) {
  n <- length(vals[[1]])
  sep <- lead$sep
  if (sum(code_rows(vals, sep)) > n / 2) {
    near <- intersect(keys, receiver_columns(map))
    if (length(near)) stop_receiver_part(lead, near[1], jo)
    return(invisible(TRUE))
  }
  flip <- if (length(keys) == 2) sum(is_id(vals[[1]]) & is_space(vals[[2]])) else 0
  if (flip > n / 2) stop_swapped_code(lead, keys, flip, n, jo, rows, table)
  if (sum(code_rows(vals, "-")) > n / 2) {
    stop_code_sep(lead, do.call(paste, c(vals, sep = sep))[1], keys, rows, table)
  }
  stop_code_shape(lead, do.call(paste, c(vals, sep = sep)), sum(code_rows(vals, sep)), n, jo)
}

# Which rows of the parts give a transmitter code when joined with `sep`:
# a code space in the first part, digits in every later part, and "-"
# between them. The joined text alone cannot tell a code space and two ID
# columns joined with nothing from one long ID, so the parts are tested.
# The joined text is tested as well, so that three parts give a code only
# where the first two form its code space, as A69, 1601 and 12345 do.
code_rows <- function(vals, sep) {
  ids <- Reduce(`&`, lapply(vals[-1], is_id))
  ok <- is_space(vals[[1]]) & ids & sep == "-"
  if (length(vals) == 2) return(ok)
  ok & is_code(do.call(paste, c(vals, sep = sep)))
}

# The format's order of a code, for a message, or the shape of a code
# where the format reads `transmitter` from one column.
code_order <- function(jo) {
  if (is.null(jo)) return("a transmitter code is its code space, then its ID, as A69-1601-12345")
  sprintf("%s between them, in the order %s", jo$how, jo$order)
}

# The advice on a joined source. A pair takes the format's `sep`, so a pair
# that needs "-" where the format has none is given as a row of a full map.
code_fix <- function(lead, source, dash, rows, table) {
  if (!dash) {
    if (lead$pair) return(sprintf("Write c(transmitter = \"%s\").", source))
    return(sprintf("Write its source as \"%s\".", source))
  }
  if (lead$pair) {
    return(sprintf(paste("A pair takes the format's `sep`, which is %s for `transmitter` in format",
                         "\"%s\": give a full map as a data frame instead, copied from",
                         "column_map(\"%s\", \"%s\"), whose `transmitter` row has the source",
                         "\"%s\" and the sep \"-\"."),
                   sep_text(lead$sep), rows$format[1], table, rows$format[1], source))
  }
  sprintf("Write its source as \"%s\" and its sep as \"-\".", source)
}

sep_text <- function(sep) if (nzchar(sep)) sprintf("\"%s\"", sep) else "nothing"

stop_swapped_code <- function(lead, keys, flip, n, jo, rows, table) {
  dash <- lead$sep != "-"
  stop_detqc(paste("%s holds IDs of digits in \"%s\" and code spaces in \"%s\" in %s of the first",
                   "%s rows that hold both, and %s%s. %s, so these codes would have their parts",
                   "swapped. %s"),
             lead$what, keys[1], keys[2], fmt_n(flip), fmt_n(n), code_order(jo),
             if (is.null(jo)) "" else ": code space first", joined_given(lead),
             code_fix(lead, paste(rev(keys), collapse = ";"), dash, rows, table))
}

stop_code_sep <- function(lead, example, keys, rows, table) {
  stop_detqc(paste("%s is joined with %s between its columns, so its codes read as \"%s\", where a",
                   "transmitter code has \"-\" between its code space and its ID, as",
                   "A69-1601-12345. %s"),
             lead$what, sep_text(lead$sep), example,
             code_fix(lead, paste(keys, collapse = ";"), TRUE, rows, table))
}

stop_code_shape <- function(lead, codes, k, n, jo) {
  stop_detqc(paste("%s gives codes such as \"%s\", and %s of the first %s rows that hold every",
                   "part give a transmitter code: %s, then a dash and the ID in digits. Name the",
                   "code space column, then the ID column%s."),
             lead$what, codes[1], fmt_n(k), fmt_n(n), space_text,
             if (is.null(jo)) "" else paste(";", code_order(jo)))
}

space_text <- paste("a code space of letters, a digit, then letters or digits, with at most one",
                    "dash and digits after it, as A69-1601 or R64K, or of two capitals and a small",
                    "letter, as OPi")

stop_receiver_part <- function(lead, column, jo) {
  stop_detqc(paste("%s names \"%s\", a column the column maps read for %s. A transmitter code",
                   "joins the code space and ID of the tag. Name the code space column, then the",
                   "ID column%s."),
             lead$what, column, not_code_text, if (is.null(jo)) "" else paste(";", code_order(jo)))
}

not_code_text <- "a receiver, a station, a time or a position"

# The first column of every row a packaged map joins for `transmitter`:
# the code-space columns, such as GLATOS's transmitter_codespace.
space_columns <- function() {
  src <- unlist(lapply(c("detections", "animals"), function(tb) {
    m <- column_map(tb)
    m$source[m$target == "transmitter" & grepl(";", m$source, fixed = TRUE)]
  }))
  unique(clean_names(vapply(strsplit(src, ";", fixed = TRUE), `[`, "", 1)))
}

# A source of one column. A column of whole codes reads. A column any map
# reads for a receiver, a station, a time or a position stops by its name,
# since OTN deployment ids and some station names take the shape of a code.
# A code-space column a packaged map reads stops by its name too: a code
# space such as A69-1601 has the shape of a whole code of a family with no
# dash in its code space, such as R64K-1234, so its values cannot tell it
# from a column of codes. For the same reason a code-space column under a
# name no packaged map reads passes as codes. Any other column stops on its
# values.
check_one_part <- function(vals, keys, lead, jo, map, rows, table) {
  v <- vals[[1]]
  named <- keys %in% receiver_columns(map)
  space_col <- keys %in% space_columns()
  codes <- sum(is_code(v)) > length(v) / 2
  if (!named && !space_col && codes) return(invisible(TRUE))
  why <- if (named) {
    sprintf(", a column the column maps read for %s", not_code_text)
  } else if (space_col) {
    ", a column the column maps read for the code space of a transmitter code"
  } else {
    ""
  }
  fix <- if (codes && space_col) {
    one_part_whole(jo, keys)
  } else {
    one_part_fix(lead, jo, rows, table)
  }
  stop_detqc("%s reads one column, \"%s\"%s. It holds %s. %s",
             lead$what, keys, why, one_part_values(v), fix)
}

# The advice on a column of whole codes under a name the maps read for a
# code space. Such a column stops by its name in any map, so naming the
# format's two columns would not help: the column is renamed in the file.
one_part_whole <- function(jo, keys) {
  sprintf(paste("A transmitter code needs its code space and ID: %s. This column holds whole",
                "codes, and the column maps read \"%s\" for a code space, so rename it in the",
                "file and name it alone."),
          code_order(jo), keys)
}

# What a column holds, for a message: its first value and the count of
# rows that give digits alone or a transmitter code.
one_part_values <- function(v) {
  n <- length(v)
  ids <- sum(is_id(v))
  codes <- sum(is_code(v))
  what <- if (ids > n / 2) {
    sprintf("digits alone in %s", fmt_n(ids))
  } else {
    sprintf("the shape of a transmitter code in %s", fmt_n(codes))
  }
  sprintf("values such as \"%s\", with %s of the first %s rows that hold it", v[1], what, fmt_n(n))
}

# The advice on a source of one column: the format's joined order where
# the format joins columns, or else a column of whole codes.
one_part_fix <- function(lead, jo, rows, table) {
  if (table == "tag_specs") table <- "animals"
  if (is.null(jo)) {
    return(sprintf(paste("Name a column of whole transmitter codes, such as A69-1601-12345, or",
                         "give the code space and ID columns, in that order, in a full map whose",
                         "`transmitter` row has the sep \"-\", copied from",
                         "column_map(\"%s\", \"%s\")."),
                   table, rows$format[1]))
  }
  orders <- strsplit(strsplit(jo$order, " or ", fixed = TRUE)[[1]], ";", fixed = TRUE)
  advice <- if (lead$pair) {
    sprintf(", as %s, with each column as the file has it, or give a full map as a data frame: %s",
            paste(sprintf("c(transmitter = \"%s\")", vapply(orders, paste, "", collapse = ";")),
                  collapse = " or "),
            sprintf("copy column_map(\"%s\", \"%s\") and change `source`", table, rows$format[1]))
  } else {
    ", with each column as the file has it"
  }
  sprintf("A transmitter code needs its code space and ID: %s. Name %s columns in %s%s.",
          code_order(jo), fmt_n(jo$n), jo$that, advice)
}

# A full map given as `col_map` replaces the packaged one. Each row that
# joins columns gets the checks a pair gets. A target whose rows name no
# column the file has stops the reader when one of them names a column of
# the user's own, one that no packaged map reads. A target the table needs
# stops too when the map fills it with nothing and the packaged map of the
# format being read fills it from this file, because the map has missed a
# column the file holds. Only that format's rows count: an OTN extract that
# is not matched to animals has no animal_id, though the matched rows read
# one from catalogNumber. Any other target whose rows name no column the
# file has is named in one warning, which says what the table holds for
# it: the file's own column of the target's name where the reader keeps
# one, or no value.
check_map_sources <- function(raw, map, table, keep_unmapped = TRUE) {
  if (!isTRUE(attr(map, "user_map"))) return(invisible(TRUE))
  have <- names(raw)
  rows <- attr(map, "format_rows")
  check_map_rows(raw, map, rows, table)
  names_read <- packaged_names()
  spec <- contract_spec[[table]]
  needed <- spec$column[spec$required]
  fills <- filled_targets(raw, map)
  could <- rows[rows$target %in% setdiff(needed, fills), , drop = FALSE]
  could <- could[filling_rows(raw, could), , drop = FALSE]
  empty <- character()
  for (tg in unique(c(map$target, needed))) {
    parts <- strsplit(map$source[map$target == tg], ";", fixed = TRUE)
    by_name <- any(vapply(parts, function(p) all(p %in% have), logical(1)))
    own <- setdiff(unlist(parts), c(names_read, have))
    if (!by_name && length(own)) {
      stop_detqc(paste("The `col_map` row for `%s` names the column \"%s\", which this %s file",
                       "does not have, and no row for `%s` names a column it has. %s."),
                 tg, own[1], table, tg, file_columns(own[1], have))
    }
    if (tg %in% could$target) stop_needed_unfilled(tg, parts, by_name, could, raw, table)
    if (length(parts) && !by_name) empty <- c(empty, tg)
  }
  if (length(empty)) warn_absent_sources(empty, map, have, needed, table, keep_unmapped)
  invisible(TRUE)
}

# One warning for every target whose rows name only columns the file lacks,
# saying what becomes of each column. A needed column that may be blank is
# written empty by add_missing_targets(); any other needed column stops
# there, so it is not named here. An optional target is read from a file
# column of its own name where the reader keeps unmapped columns
# (add_unmapped()); otherwise the table holds no value for it, as a column
# of NA or none at all, by reader.
warn_absent_sources <- function(empty, map, have, needed, table, keep_unmapped) {
  empty <- empty[!empty %in% setdiff(needed, blank_ok_targets)]
  if (!length(empty)) return(invisible(TRUE))
  says <- vapply(empty, function(tg) {
    from <- paste0("`", unique(map$source[map$target == tg]), "`", collapse = " or ")
    fate <- if (keep_unmapped && tg %in% have && !tg %in% needed) {
      sprintf("the file's own `%s` column is read in its place", tg)
    } else {
      "the table holds no value for it"
    }
    sprintf("`%s` is read from %s, which this file lacks; %s.", tg, from, fate)
  }, "")
  warn_detqc("The `col_map` given names columns this %s file lacks. %s", table,
             paste(says, collapse = " "))
}

# Every source column name a packaged map reads.
packaged_names <- function() {
  src <- unlist(lapply(c("detections", "deployments", "animals"), function(tb) {
    column_map(tb)$source
  }))
  unique(clean_names(unlist(strsplit(src, ";", fixed = TRUE))))
}

# Every row of a data-frame map that joins columns gets the pair checks:
# part count, order against the format's joined row, and the code shape.
# Rows whose columns this file lacks get the order and count checks too,
# because one map is often read against several files.
check_map_rows <- function(raw, map, rows, table) {
  for (r in seq_len(nrow(map))) {
    tg <- map$target[r]
    parts <- strsplit(map$source[r], ";", fixed = TRUE)[[1]]
    lead <- list(what = sprintf("The `col_map` row %s = \"%s\"", tg, map$source[r]),
                 pair = FALSE, sep = map$sep[r])
    if (length(parts) > 1) {
      check_pair_order(parts, joined_parts(rows, tg), tg, lead, rows, table, names(raw))
    }
    check_code_shape(raw, parts, tg, lead, map, table)
  }
  invisible(TRUE)
}

# A target the table needs that the user's map leaves empty, though the
# packaged map of the format being read fills it from this file. `by_name` says whether the map's
# columns are in the file and hold no value, or are not in the file.
stop_needed_unfilled <- function(tg, parts, by_name, could, raw, table) {
  hit <- could[could$target == tg, , drop = FALSE][1, ]
  named <- unique(vapply(parts, paste, "", collapse = ";"))
  given <- if (!length(parts)) {
    sprintf("given replaces the packaged map and has no row for `%s`", tg)
  } else {
    sprintf("%s for `%s` %s %s, which this %s file %s",
            verb(length(parts), "row", "rows"), tg, verb(length(parts), "names", "name"),
            paste0("\"", named, "\"", collapse = ", "), table,
            if (by_name) "holds with no value in any row" else "does not have")
  }
  stop_detqc(paste("The `col_map` %s. The %s table needs `%s`, and the packaged \"%s\" map fills",
                   "it from this file's \"%s\". Name that column for `%s`, or give only the",
                   "renamed columns as c(target = \"source\"), which are merged into the",
                   "format's map. %s."),
             given, table, tg, hit$format, hit$source, tg,
             file_columns(if (length(parts)) parts[[1]][1] else tg, names(raw)))
}

read_map_file <- function(col_map) {
  if (length(col_map) != 1 || is.na(col_map) || !file.exists(col_map)) {
    stop_detqc("%s No file is at %s.", col_map_takes,
               paste0("\"", utils::head(col_map, 1), "\"", if (length(col_map) > 1) " and others"))
  }
  utils::read.csv(col_map, stringsAsFactors = FALSE, na.strings = character())
}

check_map_frame <- function(col_map) {
  if (!is.data.frame(col_map)) {
    stop_detqc("%s It is of class %s.", col_map_takes, class(col_map)[1])
  }
  as.data.frame(col_map, stringsAsFactors = FALSE)
}

# ---- Internal: the format of a deployment or animals sheet ----
# OTN and GLATOS sheets share many column names, and each format's map reads
# a contract column the other leaves empty: read as OTN, a GLATOS receiver
# file has no station_id and a GLATOS workbook no release times.
# `prep(raw, format)` applies a format's own preparation, such as the
# local times of a GLATOS workbook, before its map is scored. "auto" takes
# the format that fills more contract columns, and OTN on a tie. The other
# format filling a contract column the chosen one leaves empty gives a
# warning.
sheet_format <- function(raw, table, format, file, prep, warn = TRUE) {
  formats <- c("otn", "glatos", "detqc")
  contract <- contract_spec[[table]]$column
  got <- lapply(stats::setNames(formats, formats), function(fm) {
    # Quiet here, so the reader warns about the chosen format's values once.
    prepared <- suppressWarnings(prep(raw, fm))
    intersect(filled_targets(prepared, get_column_map(table, fm, NULL)), contract)
  })
  # which.max() takes the first of equal scores: OTN, then GLATOS.
  auto <- formats[which.max(lengths(got))]
  given <- format != "auto"
  if (!given) format <- auto
  # A table detqc wrote names the contract columns themselves; the warning
  # compares the two network formats only.
  if (format == "detqc") return(format)
  other <- setdiff(c("otn", "glatos"), format)
  lost <- setdiff(got[[other]], got[[format]])
  if (warn && given) stop_needed_empty(lost, table, format, other, auto, file)
  if (warn && length(lost)) {
    warn_detqc(paste("%s: read as format = \"%s\", which fills no %s. The file has the \"%s\"",
                     "columns for %s; read it with format = \"%s\" if it is a %s sheet."),
               basename(file), format, paste0("`", lost, "`", collapse = ", "), other,
               if (length(lost) == 1) "it" else "them", other, toupper(other))
  }
  format
}

# A format named by the user that leaves a required column empty, which
# the other format fills from this file, would read every row wrong.
stop_needed_empty <- function(lost, table, format, other, auto, file) {
  spec <- contract_spec[[table]]
  need <- intersect(lost, spec$column[spec$required])
  if (!length(need)) return(invisible(TRUE))
  stop_detqc(paste("%s: format = \"%s\" fills no %s, which the %s table needs. The file has",
                   "the \"%s\" columns for %s, and format = \"auto\" reads it as \"%s\".",
                   "Read it with format = \"%s\", or give `col_map`."),
             basename(file), format, paste0("`", need, "`", collapse = ", "), table, other,
             if (length(need) == 1) "it" else "them", auto,
             if (auto %in% c(format, "detqc")) other else auto)
}

# The targets of a map that a table can fill: every source column of one of
# its rows is present and holds a value.
filled_targets <- function(raw, map) unique(map$target[filling_rows(raw, map)])

filling_rows <- function(raw, map) {
  vapply(strsplit(map$source, ";", fixed = TRUE), function(p) {
    all(p %in% names(raw)) && all(vapply(p, function(k) any(!is.na(blank_na(raw[[k]]))),
                                         logical(1)))
  }, logical(1))
}

# The header names a workbook sheet is chosen by: the format's own, or both
# formats' for "auto".
sheet_header_names <- function(table, format, map) {
  if (format != "auto") return(attr(map, "source_parts"))
  unique(c(attr(get_column_map(table, "otn", NULL), "source_parts"),
           attr(get_column_map(table, "glatos", NULL), "source_parts"),
           attr(get_column_map(table, "detqc", NULL), "source_parts"), attr(map, "source_parts")))
}

apply_column_map <- function(raw, map, table, keep_unmapped = TRUE) {
  check_pair_sources(raw, map, table, keep_unmapped)
  out <- list()
  used_rows <- integer()
  present <- character()
  empty <- character()
  for (tg in unique(map$target)) {
    one <- map_target(raw, map, tg, table)
    present <- c(present, one$present)
    used_rows <- c(used_rows, one$used)
    if (one$empty) empty <- c(empty, tg)
    if (!is.null(one$val)) out[[tg]] <- one$val
  }
  required <- contract_spec[[table]]$column[contract_spec[[table]]$required]
  warn_user_na(out, required, map, table, nrow(raw))
  out <- add_missing_targets(out, required, map, raw, table, nrow(raw), empty)
  df <- as.data.frame(out, stringsAsFactors = FALSE)
  if (keep_unmapped) df <- add_unmapped(df, raw, present)
  list(data = df, map_used = map[used_rows, c("target", "source", "sep", "type", "tz")])
}

# A needed column the user's `col_map` fills, as a pair or a row of a full
# map, can still be empty in many rows: a column such as OTN's sensorname
# holds a value only on sensor detections. One warning names each such
# column with its count of NA rows. A format read, and a packaged map copied
# whole, keep their own rows quiet. An NA recover_datetime is a receiver
# still in the water, so it gets no advice to fill every row.
warn_user_na <- function(out, required, map, table, n) {
  pairs <- attr(map, "pairs")
  mapped <- if (!is.null(pairs)) names(pairs) else if (isTRUE(attr(map, "user_map"))) map$target
  tg <- intersect(intersect(required, mapped), names(out))
  na <- vapply(tg, function(k) sum(is.na(out[[k]])), numeric(1))
  tg <- tg[na > 0]
  if (!length(tg)) return(invisible(FALSE))
  says <- sprintf("`%s` is NA in %s of %s", tg, fmt_n(na[tg]), n_of(n, "row"))
  fill <- setdiff(tg, "recover_datetime")
  advice <- if (length(fill)) {
    sprintf(" Check that the %s named for %s %s a value in every row.",
            verb(length(fill), "column", "columns"), paste0("`", fill, "`", collapse = " and "),
            verb(length(fill), "holds", "hold"))
  } else {
    ""
  }
  if ("recover_datetime" %in% tg) {
    advice <- paste(advice, "A row with no `recover_datetime` reads as a receiver still deployed.")
  }
  warn_detqc("The `col_map` given leaves %s the %s table needs empty in some rows: %s.%s",
             verb(length(tg), "a column", paste(fmt_n(length(tg)), "columns")), table,
             paste(says, collapse = "; "), advice)
  invisible(TRUE)
}

# One target from the map rows that name it, the first value row by row.
map_target <- function(raw, map, tg, table) {
  res <- list(val = NULL, used = integer(), present = character(), empty = FALSE)
  offsets <- attr(raw, "header_offsets")
  for (r in which(map$target == tg)) {
    parts <- strsplit(map$source[r], ";", fixed = TRUE)[[1]]
    if (!all(parts %in% names(raw))) next
    res$present <- c(res$present, parts)
    v <- compose_source(raw, parts, map$sep[r])
    # A header-only file, as VUE writes for a receiver that heard nothing,
    # still names its columns: they give zero-length columns.
    if (length(v) && all(is.na(v))) {
      res$empty <- TRUE
      next
    }
    v <- convert_type(v, map$type[r], map$tz[r], paste0(table, "$", tg),
                      strict = tg %in% c("datetime", "deploy_datetime"),
                      offset = offsets[parts[1]])
    res$used <- c(res$used, r)
    res$val <- if (is.null(res$val)) v else coalesce_values(res$val, v)
  }
  res
}

compose_source <- function(raw, parts, sep) {
  if (length(parts) == 1) return(blank_na(raw[[parts]]))
  cols <- lapply(parts, function(p) blank_na(raw[[p]]))
  v <- do.call(paste, c(cols, sep = sep))
  v[Reduce(`|`, lapply(cols, is.na))] <- NA_character_
  v
}

coalesce_values <- function(a, b) {
  fill <- is.na(a) & !is.na(b)
  a[fill] <- b[fill]
  a
}

# A required column the file cannot supply stops the reader, except these
# four, which some network sheets do not record: the table then has them
# empty.
blank_ok_targets <- c("receiver_serial", "station_id", "animal_id", "recover_datetime")

add_missing_targets <- function(out, required, map, raw, table, n, empty = character()) {
  for (tg in setdiff(required, names(out))) {
    if (tg %in% blank_ok_targets) {
      out[[tg]] <- if (tg == "recover_datetime") empty_utc(n) else rep(NA_character_, n)
      next
    }
    tried <- paste(map$source[map$target == tg], collapse = ", ")
    label <- if (table == "tag_specs") "tag sheet" else paste(table, "file")
    if (tg %in% empty) {
      stop_detqc("No values for `%s` in this %s: its column %s is empty in %s.",
                 tg, label, tried, verb(n, "the file's only row", paste("all", n_of(n, "row"))))
    }
    if (!nzchar(tried) && isTRUE(attr(map, "user_map"))) {
      stop_detqc(paste("No column for `%s` in this %s: the `col_map` given replaces the",
                       "packaged map and has no row for `%s`. Add one, or give only the renamed",
                       "columns as c(target = \"source\"), which are merged into the format's",
                       "map. %s."),
                 tg, label, tg, file_columns(tg, names(raw), raw))
    }
    stop_detqc("No column for `%s` in this %s. Looked for: %s. %s.",
               tg, label, if (nzchar(tried)) tried else "(none in the map)",
               file_columns(tg, names(raw), raw))
  }
  out
}

add_unmapped <- function(df, raw, used) {
  rest <- setdiff(names(raw), c(used, names(df)))
  for (nm in rest) df[[nm]] <- utils::type.convert(raw[[nm]], as.is = TRUE)
  df
}

convert_type <- function(v, type, tz, label, strict = TRUE, offset = NA_real_) {
  switch(type,
    datetime = parse_utc(v, tz, label, strict, offset),
    date = as.Date(parse_utc(v, tz, label, strict, offset)),
    numeric = parse_number(v),
    integer = as.integer(parse_number(v)),
    blank_na(as.character(trimws(v)))
  )
}

# A blank cell is a missing value. fread reads a quoted "" as text, so a
# file saved by write.csv() would otherwise give empty codes and names.
blank_na <- function(v) {
  v[!is.na(v) & !nzchar(v)] <- NA_character_
  v
}

# Placeholders that sheets use for a missing value.
na_text <- c("-", "--", "NA", "N/A", "NULL", "NAN", "NONE")

# Numbers written as text: "+0", "3650 days", "NaN". The leading number is kept.
parse_number <- function(v) {
  v <- trimws(as.character(v))
  v[toupper(v) %in% na_text] <- NA_character_
  out <- suppressWarnings(as.numeric(v))
  lead <- is.na(out) & !is.na(v) & grepl("^[-+]?[0-9.]", v)
  out[lead] <- suppressWarnings(as.numeric(sub("^([-+]?[0-9]*\\.?[0-9]+).*$", "\\1", v[lead])))
  out[is.nan(out)] <- NA_real_
  out
}

# A detection or deployment start that is not a date stops the reader. Other
# dates, such as a recovery written "failed", become NA with a warning, and
# the next source column in the map is tried for those rows. A stamp that
# carries its own offset, "2024-05-01T10:00:00-04:00" or "...Z", is read at
# that offset whatever `tz` says; `offset` (hours) applies to stamps with
# none, as a header such as "(UTC-04:00)" gives it. Every stamp is first read
# as a clock time in UTC, where no clock time is missing or repeated, and only
# then placed in `tz` (dst_place()).
parse_utc <- function(v, tz, label, strict = TRUE, offset = NA_real_) {
  v <- trimws(as.character(v))
  v[!nzchar(v) | toupper(v) %in% na_text] <- NA_character_
  ok <- !is.na(v)
  out <- empty_utc(length(v))
  if (!any(ok)) return(out)
  off <- stamp_offsets(v)
  clock <- read_clock(attr(off, "stripped"), ok)
  if (length(offset) == 1 && !is.na(offset)) off[is.na(off)] <- offset
  fixed <- ok & !is.na(off) & !is.na(clock)
  out[fixed] <- clock[fixed] - off[fixed] * 3600
  local <- ok & is.na(off) & !is.na(clock)
  if (any(local)) out[local] <- dst_place(clock[local], tz, label, v[local], strict)
  bad <- ok & is.na(clock)
  if (any(bad) && strict) {
    stop_detqc("%s: %s %s, such as \"%s\" (row %s).", label, n_of(sum(bad), "value"),
               verb(sum(bad), "is not a date", "are not dates"), v[bad][1], fmt_n(which(bad)[1]))
  }
  if (any(bad)) {
    warn_detqc("%s: %s %s, such as \"%s\"; read as NA.", label, n_of(sum(bad), "value"),
               verb(sum(bad), "is not a date", "are not dates"), v[bad][1])
  }
  lubridate::with_tz(out, "UTC")
}

# Clock times as written, held as UTC instants. Excel stores date-times as
# days since 1899-12-30.
excel_serial_min <- 10000

read_clock <- function(s, ok) {
  out <- empty_utc(length(s))
  # A number below 10,000 (1927) is not a date-time a receiver wrote: a time
  # cut to "2" read as day 2 of the Excel calendar, 1900-01-01.
  serial <- ok & grepl("^[0-9]+(\\.[0-9]+)?$", s)
  serial[serial] <- as.numeric(s[serial]) >= excel_serial_min
  if (any(serial)) {
    out[serial] <- as.POSIXct(round(as.numeric(s[serial]) * 86400), origin = "1899-12-30",
                              tz = "UTC")
  }
  text <- ok & !serial
  if (any(text)) {
    out[text] <- suppressWarnings(lubridate::parse_date_time(
      s[text], orders = c("Ymd HMOS", "Ymd HMS", "Ymd HM", "Ymd"), tz = "UTC", quiet = TRUE
    ))
  }
  out
}

# A clock time in a zone with daylight saving can be missing (the spring
# gap) or repeated (the autumn hour). A missing one means the file's clock
# does not follow `tz`, so every summer time would be an hour out: it stops
# a strict column, with the remedy. A repeated one is read as the second
# occurrence, standard time, and counted in a warning.
dst_place <- function(clock, tz, label, text, strict) {
  if (tz == "UTC") return(clock)
  post <- lubridate::force_tz(clock, tz, roll_dst = c("NA", "post"))
  gap <- is.na(post)
  if (any(gap)) {
    msg <- paste("%s: %s did not occur in %s (the spring daylight-saving gap), such as",
                 "\"%s\". The file's clock does not follow %s; read it with a fixed-offset",
                 "zone, such as \"Etc/GMT+5\" for a receiver clock kept on UTC-5 all year.")
    args <- list(msg, label, n_of(sum(gap), "clock time"), tz, text[gap][1], tz)
    if (strict) do.call(stop_detqc, args)
    do.call(warn_detqc, args)
  }
  pre <- lubridate::force_tz(clock, tz, roll_dst = c("NA", "pre"))
  fold <- !gap & pre != post
  if (any(fold)) {
    warn_detqc(paste("%s: %s in the repeated autumn hour of %s, such as \"%s\";",
                     "each is read as the second occurrence (standard time)."),
               label, paste(n_of(sum(fold), "clock time"), verb(sum(fold), "falls", "fall")), tz,
               text[fold][1])
  }
  post
}

# Hours east of UTC written at the end of each stamp, NA where none; the
# attribute "stripped" holds the stamps without it, with an ISO "T" between
# date and time replaced by a space. Other text keeps every letter, so a
# message quotes it as the file has it.
stamp_offsets <- function(v) {
  re <- "([+-])([0-9]{2}):?([0-9]{2})$|Z$"
  has <- !is.na(v) & grepl(paste0("[0-9]", "(", re, ")"), v)
  off <- rep(NA_real_, length(v))
  m <- regmatches(v[has], regexec(re, v[has]))
  off[has] <- vapply(m, function(p) {
    if (p[1] == "Z") return(0)
    (if (p[2] == "-") -1 else 1) * (as.numeric(p[3]) + as.numeric(p[4]) / 60)
  }, numeric(1))
  stripped <- v
  stripped[has] <- sub(re, "", v[has])
  stripped <- sub("^([0-9]{4}-[0-9]{1,2}-[0-9]{1,2})T", "\\1 ", stripped)
  structure(off, stripped = stripped)
}

# ---- Internal: sensor columns and tidying ----
split_raw_sensors <- function(df) {
  if (!"sensor_raw" %in% names(df)) df$sensor_raw <- rep(NA_real_, nrow(df))
  if (!"sensor_value" %in% names(df)) df$sensor_value <- rep(NA_real_, nrow(df))
  unit <- if ("sensor_unit" %in% names(df)) df$sensor_unit else rep(NA_character_, nrow(df))
  key <- unit_key(unit)
  bare <- sub("^\\[(.*)\\]$|^\\((.*)\\)$", "\\1\\2", tolower(trimws(unit)))
  raw_unit <- is.na(unit) | bare %in% c("", "adc", "raw")
  raw_unit <- raw_unit & !not_counts(df, unit)
  move <- !is.na(df$sensor_value) & raw_unit & is.na(df$sensor_raw)
  df$sensor_raw[move] <- df$sensor_value[move]
  df$sensor_value[raw_unit] <- NA_real_
  unit[raw_unit] <- NA_character_
  unit[!is.na(key)] <- key[!is.na(key)]
  df$sensor_unit <- unit
  df
}

# A reading with no unit is taken as a raw count, but a count is a whole
# number of zero or more. A transmitter with any unitless reading that is
# negative or fractional holds calibrated values that lost their unit, and
# calibrating them again would be wrong. Its unitless readings stay in
# sensor_value with no unit, so validate_detset() stops on them.
not_counts <- function(df, unit) {
  v <- suppressWarnings(as.numeric(df$sensor_value))
  blank <- is.na(unit) | !nzchar(trimws(unit))
  odd <- blank & !is.na(v) & (v < 0 | v != round(v))
  if (!any(odd)) return(rep(FALSE, nrow(df)))
  tx <- unique(df$transmitter[odd])
  out <- blank & !is.na(v) & df$transmitter %in% tx
  warn_detqc(paste("%s of %s, such as %s, have no unit and are not whole counts of zero or",
                   "more (%s negative or fractional), so they are not raw counts. They stay",
                   "in sensor_value with no unit; give their unit in a sensor_unit column."),
             n_of(sum(out), "sensor reading"), n_of(length(tx), "transmitter"), tx[1],
             fmt_n(sum(odd)))
  out
}

split_receiver_column <- function(df) {
  sp <- split_receiver(df$receiver_serial)
  if (any(sp$split)) {
    if (!"receiver_model" %in% names(df)) df$receiver_model <- NA_character_
    fill <- sp$split & is.na(df$receiver_model)
    df$receiver_model[fill] <- sp$model[fill]
    df$receiver_serial <- sp$serial
  }
  attr(df, "serials_split") <- sum(sp$split)
  df
}

drop_zero_positions <- function(df) {
  if (all(c("deploy_lat", "deploy_lon") %in% names(df))) {
    zero <- !is.na(df$deploy_lat) & df$deploy_lat == 0 & df$deploy_lon == 0
    df$deploy_lat[zero] <- NA_real_
    df$deploy_lon[zero] <- NA_real_
  }
  df
}

order_columns <- function(df, first) {
  front <- intersect(first, names(df))
  df <- df[, c(front, setdiff(names(df), front)), drop = FALSE]
  rownames(df) <- NULL
  df
}

# `rows_read` counts the data rows of the file, before the reader sets any
# aside; `rows` counts the rows the reader returns, which is what detset()
# lists in meta$sources.
source_record <- function(file, format, out, rows = nrow(out$data), rows_read = nrow(out$data)) {
  used <- out$map_used
  list(file = normalizePath(file), format = format,
       tz_source = unique(used$tz[used$type %in% c("datetime", "date")]),
       rows_read = as.integer(rows_read), rows = as.integer(rows), map_used = used)
}

# A choice argument that names a value outside `choices`, or more than one
# value, stops with the values it takes. The whole vector of choices, as the
# default in the usage gives it, takes its first.
check_format_arg <- function(format, choices, arg = "format") {
  if (identical(format, choices)) return(choices[1])
  one <- is.character(format) && length(format) == 1 && !is.na(format)
  if (!one || !format %in% choices) {
    stop_detqc("`%s` must be one of %s; it has %s.", arg,
               paste0("\"", choices, "\"", collapse = ", "), given_values(format))
  }
  format
}

# The same check for the other choice arguments, which take an unambiguous
# start of a value, as match.arg() does: `by = "anim"` reads as "animal".
# `several_ok` takes one or more values.
check_choice <- function(value, choices, arg, several_ok = FALSE) {
  if (!several_ok && identical(value, choices)) return(choices[1])
  hit <- choice_hits(value, choices, several_ok)
  if (is.null(hit)) {
    stop_detqc("`%s` must be %s of %s; it has %s.", arg, if (several_ok) "one or more" else "one",
               paste0("\"", choices, "\"", collapse = ", "), given_values(value))
  }
  choices[hit]
}

# The place in `choices` of each value, or NULL when a value fits none or
# the argument takes fewer values than it was given.
choice_hits <- function(value, choices, several_ok) {
  fits <- length(value) == 1 || (several_ok && length(value) > 1)
  if (!fits || !is.character(value) || anyNA(value)) return(NULL)
  hit <- pmatch(value, choices, duplicates.ok = TRUE)
  if (anyNA(hit)) NULL else hit
}

given_values <- function(v) {
  if (!length(v)) return("no value")
  shown <- paste0("\"", utils::head(as.character(v), 3), "\"", collapse = ", ")
  if (length(v) > 3) shown <- paste(shown, "and", fmt_n(length(v) - 3), "more")
  shown
}
