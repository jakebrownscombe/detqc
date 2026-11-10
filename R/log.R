# ---- The processing log ----
# CONTRACT.md, section 7. Every reader, check, filter and mortality step
# appends one record to `x$log` through log_step(), and nothing else writes
# there. processing_report() reads its numbers from these records, so a step
# that changes the detections without a record is invisible in the report.
#
# The first six arguments of log_step() are positional in every call, in
# detset() and in the mortality functions, so their order is fixed.

log_fields <- c("step", "fn", "at", "parameters", "rows_before", "rows_after",
                "animals_before", "animals_after", "removed", "removed_by", "reason",
                "detqc_version")

new_log <- function() {
  out <- data.frame(
    step = character(), fn = character(), at = empty_utc(), parameters = character(),
    rows_before = integer(), rows_after = integer(), animals_before = integer(),
    animals_after = integer(), removed = integer(), reason = character(),
    detqc_version = character(), stringsAsFactors = FALSE
  )
  out$removed_by <- list()
  out[, log_fields]
}

# `x` is the detset after the step: the counts after it are read from `x`,
# so the log cannot disagree with the object. `removed` names each reason;
# one unnamed count is stored under `no_breakdown`.
log_step <- function(x, step, parameters, before, removed = 0L, reason = "", fn = step) {
  check_text1(step, "step")
  check_text1(fn, "fn")
  check_text1(reason, "reason")
  counts <- before_counts(before)
  by <- removal_counts(removed)
  rec <- data.frame(
    step = step, fn = fn, at = lubridate::now("UTC"),
    parameters = deparse_parameters(parameters),
    rows_before = counts[1], rows_after = nrow(x$detections),
    animals_before = counts[2], animals_after = count_animals(x$detections),
    removed = as.integer(sum(by)), reason = reason, detqc_version = detqc_version(),
    stringsAsFactors = FALSE
  )
  rec$removed_by <- list(by)
  rec <- rec[, log_fields]
  if (rec$removed > rec$rows_before) {
    stop_detqc("Step `%s` counts %s removed, but only %s came in.",
               step, n_of(rec$removed, "detection"), fmt_n(rec$rows_before))
  }
  warn_breaches(rbind(utils::tail(x$log, 1L), rec))
  x$log <- rbind(x$log, rec)
  rownames(x$log) <- NULL
  x
}

# Only the last record of `lg` is new; the record before it, if any, is
# there for the chain test.
warn_breaches <- function(lg) {
  b <- log_breaches(lg)
  b <- b[b$record == nrow(lg), , drop = FALSE]
  for (i in seq_len(nrow(b))) warn_detqc("%s", breach_message(b[i, ]))
  invisible(b)
}

breach_message <- function(b) {
  gap <- abs(b$found - b$expected)
  switch(b$kind,
    lost = sprintf("Step `%s` lost %s that its log record does not count as removed.",
                   b$step, n_of(gap, "detection")),
    added = sprintf(paste("Step `%s` ends with %s more %s than its removals allow: it",
                          "over-counts its removals, or it added rows and is not a load step."),
                    b$step, fmt_n(gap), verb(gap, "detection", "detections")),
    over = sprintf("Step `%s` counts %s removed, but only %s came in.", b$step,
                   n_of(b$found, "detection"), fmt_n(b$expected)),
    chain_rows = sprintf(paste("Step `%s` starts from %s, but the record before it",
                               "ended at %s: the detections changed between the two steps with",
                               "no log record."), b$step, n_of(b$found, "detection"),
                         fmt_n(b$expected)),
    chain_animals = sprintf(paste("Step `%s` starts from %s, but the record before it",
                                  "ended at %s: the animals changed between the two steps with",
                                  "no log record."), b$step, n_of(b$found, "animal"),
                            fmt_n(b$expected))
  )
}

# ---- The arithmetic of the log ----
# CONTRACT.md, section 7. Each record starts where the one before it ended
# (the chain), loses no row it does not count as removed, removes no more
# than came in, and adds rows only when it is a load step. A breach is a
# fault in the log itself, so the report flags each one and the test
# helper report_check() returns them. None stops: a log already written stays
# readable, and the report is where a reader sees the fault.

# A load reads rows in and an append adds them; no other step may add rows.
adds_rows <- function(step) grepl("^read_|^append_|^detset$", step)

breach_kinds <- c("over", "lost", "added", "chain_rows", "chain_animals")

log_breaches <- function(lg) {
  n <- nrow(lg)
  k <- seq_len(n)
  change <- lg$rows_after - (lg$rows_before - lg$removed)
  pieces <- list(
    breach_rows(lg, k, "over", lg$rows_before, lg$removed, lg$removed > lg$rows_before),
    breach_rows(lg, k, "lost", lg$rows_before - lg$removed, lg$rows_after, change < 0),
    breach_rows(lg, k, "added", lg$rows_before - lg$removed, lg$rows_after,
                change > 0 & !adds_rows(lg$step))
  )
  if (n > 1) {
    j <- k[-1]
    pieces <- c(pieces, list(
      breach_rows(lg, j, "chain_rows", lg$rows_after[j - 1], lg$rows_before[j],
                  lg$rows_before[j] != lg$rows_after[j - 1]),
      breach_rows(lg, j, "chain_animals", lg$animals_after[j - 1], lg$animals_before[j],
                  lg$animals_before[j] != lg$animals_after[j - 1])
    ))
  }
  out <- do.call(rbind, pieces)
  out <- out[order(out$record, match(out$kind, breach_kinds)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

breach_rows <- function(lg, k, kind, expected, found, hit) {
  hit <- which(hit)
  data.frame(record = k[hit], step = lg$step[k[hit]], kind = rep(kind, length(hit)),
             expected = as.numeric(expected[hit]), found = as.numeric(found[hit]),
             stringsAsFactors = FALSE)
}

check_text1 <- function(v, name) {
  if (!is_one_string(v)) {
    stop_detqc("`%s` must be one string.", name)
  }
  invisible(TRUE)
}

before_counts <- function(before) {
  if (inherits(before, "detset")) before <- before$detections
  if (is.data.frame(before)) return(c(nrow(before), count_animals(before)))
  if (is.numeric(before) && length(before) == 2 && all(is.finite(before)) && all(before >= 0)) {
    return(as.integer(before))
  }
  stop_detqc("`before` must be the detset before the step, its detections, or c(rows, animals).")
}

removal_counts <- function(removed) {
  if (!is_count_vector(removed)) {
    stop_detqc("`removed` must hold whole, non-negative counts of detections.")
  }
  nm <- names(removed)
  if (is.null(nm)) return(unnamed_removal(removed))
  if (anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
    stop_detqc("Each reason in `removed` needs one unique name.")
  }
  stats::setNames(as.integer(removed), nm)
}

is_count_vector <- function(v) {
  is.numeric(v) && all(is.finite(v)) && all(v >= 0) && all(v == round(v))
}

unnamed_removal <- function(removed) {
  if (length(removed) > 1) stop_detqc("`removed` with more than one count must name each reason.")
  if (!length(removed) || removed == 0) return(stats::setNames(integer(), character()))
  c(no_breakdown = as.integer(removed))
}

# Arguments in a fixed form for the log: `name = value; ...`. Large values
# are described by their size, so a data frame argument cannot fill the log.
deparse_parameters <- function(parameters) {
  if (is_one_string(parameters)) return(parameters)
  if (is.call(parameters)) return(one_line(parameters))
  if (!is.list(parameters) || is.data.frame(parameters)) {
    stop_detqc("`parameters` must be a named list, a call or one string.")
  }
  if (!length(parameters)) return("")
  nm <- names(parameters)
  if (is.null(nm) || any(!nzchar(nm))) stop_detqc("Every element of `parameters` needs a name.")
  vals <- vapply(parameters, describe_value, "")
  paste(sprintf("%s = %s", nm, vals), collapse = "; ")
}

is_one_string <- function(v) {
  is.character(v) && length(v) == 1 && !is.na(v)
}

describe_value <- function(v) {
  if (inherits(v, "detset")) return("<detset>")
  if (is.data.frame(v)) return(sprintf("<data frame, %s>", n_of(nrow(v), "row")))
  if (is.function(v)) return("<function>")
  if (is.atomic(v) && length(v) > 10) {
    return(sprintf("%s ... (%s values)", one_line(utils::head(v, 10)), fmt_n(length(v))))
  }
  one_line(v)
}

one_line <- function(v) {
  out <- paste(trimws(deparse(v, width.cutoff = 500L)), collapse = " ")
  if (nchar(out) > 300) out <- paste0(substr(out, 1, 297), "...")
  out
}

detqc_version <- function() {
  as.character(utils::packageVersion("detqc"))
}

# ---- Checking and reading the log ----
# validate_detset() calls check_log(): it stops on a log of the wrong shape
# and on reasons that do not sum to the removed count. The arithmetic
# between records is log_breaches(), which flags and never stops.
check_log <- function(lg) {
  check_columns(lg, spec_table(
    setdiff(log_fields, "removed_by"),
    c("character", "character", "utc", "character", rep("numeric", 5), "character",
      "character"),
    rep(TRUE, length(log_fields) - 1L)
  ), "log")
  if (!is.list(lg$removed_by)) stop_detqc("`log$removed_by` must be a list of named counts.")
  sums <- vapply(lg$removed_by, function(v) as.numeric(sum(v)), numeric(1))
  bad <- which(sums != lg$removed)
  if (length(bad)) {
    stop_detqc("Log record %d (%s) removes %s but its reasons sum to %s.",
               bad[1], lg$step[bad[1]], n_of(lg$removed[bad[1]], "detection"), fmt_n(sums[bad[1]]))
  }
  invisible(lg)
}

# One row per step and reason: what the report's removals table shows.
log_removals <- function(lg) {
  rows <- lapply(seq_len(nrow(lg)), function(i) {
    by <- lg$removed_by[[i]]
    if (!length(by)) return(NULL)
    data.frame(record = i, step = lg$step[i], reason = names(by), removed = unname(by),
               of = lg$rows_before[i], stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    return(data.frame(record = integer(), step = character(), reason = character(),
                      removed = integer(), of = integer(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}
