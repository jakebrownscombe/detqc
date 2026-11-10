# ---- report_check(): the test suite's check of a processing report ----
# Sourced by testthat before the tests. The functions run in a child of the
# detqc namespace, so they call its internal functions directly.
#
# The check of a report. Each value the report shows sits in an
# element whose data-src key names its source. The check reads the HTML,
# resolves every key from `x`, and lists any digit left outside a keyed
# element. Added sections and given text (the title, figure names, an error
# message) are counted and not traced.
#
# What a match proves depends on the key, and `traced$basis` says which:
# - "table": recomputed from the tables of `x` in base R, while the report
#   computed it with data.table. A match is independent evidence.
# - "log": read from `x$log`, or computed from it. A match shows the value is
#   rendered as logged. Whether the log is right is a separate test.
# - "text": a string stored on `x` (log notes, checks, sources, session).
#   Numbers inside such a string are not checked.
# - "label": the expected value is the key's own text, such as a station.
# Whether the log itself is right is `invariants`: every breach of the log's
# arithmetic (log_breaches()) and any disagreement between the last record
# and the detections table. A clean report has none.
report_check <- function(file, x, bands = NULL) {
  # processing_report() logs the bands on the detset it renders.
  if (!is.null(bands)) {
    n <- nrow(x$log)
    x <- log_mortality_bands(x, bands)
    if (nrow(x$log) > n) attr(x, "bands_record") <- nrow(x$log)
  }
  html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- gsub("(?s)<head>.*?</head>|<!--.*?-->", "", html, perl = TRUE)
  html <- gsub("src=\"data:[^\"]*\"", "", html, perl = TRUE)
  user_re <- "(?s)<section class=\"added\" data-check=\"user\".*?</section>"
  n_user <- count_matches(user_re, html)
  html <- gsub(user_re, "", html, perl = TRUE)
  given_re <- "(?s)<(\\w+)[^>]*data-check=\"(given|user)\"[^>]*>.*?</\\1>"
  n_given <- count_matches(given_re, html)
  html <- gsub(given_re, " ", html, perl = TRUE)
  tag_re <- "<(\\w+)[^>]*data-src=\"([^\"]*)\"[^>]*>([^<]*)</\\1>"
  m <- regmatches(html, gregexpr(tag_re, html, perl = TRUE))[[1]]
  keys <- unesc(sub(tag_re, "\\2", m, perl = TRUE))
  shown <- unesc(sub(tag_re, "\\3", m, perl = TRUE))
  expected <- vapply(keys, resolve_key, "", x = x, bands = bands, USE.NAMES = FALSE)
  ok <- !is.na(expected) & shown == expected
  ok[keys == "sys:generated"] <- is_stamp(shown[keys == "sys:generated"])
  # The bands' record that processing_report() wrote, and this check wrote
  # again, differ only in the moment each was written.
  if (!is.null(attr(x, "bands_record"))) {
    at <- keys == sprintf("log:%d:at", attr(x, "bands_record"))
    ok[at] <- is_stamp(shown[at])
  }
  text <- unesc(gsub("<[^>]+>", " ", gsub(tag_re, " ", html, perl = TRUE)))
  list(
    traced = data.frame(key = keys, shown = shown, expected = expected, match = ok,
                        basis = key_basis(keys), stringsAsFactors = FALSE),
    untraced = regmatches(text, gregexpr("\\S*[0-9]\\S*", text, perl = TRUE))[[1]],
    invariants = log_invariants(x), user_sections = n_user, given = n_given
  )
}

log_invariants <- function(x) {
  b <- log_breaches(x$log)
  lg <- x$log
  n <- nrow(lg)
  if (!n) return(b)
  tab <- data.frame(
    record = n, step = lg$step[n], kind = c("table_rows", "table_animals"),
    expected = as.numeric(c(lg$rows_after[n], lg$animals_after[n])),
    found = as.numeric(c(nrow(x$detections), count_animals(x$detections))),
    stringsAsFactors = FALSE
  )
  out <- rbind(b, tab[tab$expected != tab$found, , drop = FALSE])
  rownames(out) <- NULL
  out
}

# The basis of a key is set by its prefix, except where the last part of the
# key names a label or a stored string.
basis_by_prefix <- c(
  idx = "label", rmname = "label", callname = "label", mortname = "label", rsname = "label",
  runame = "label", log = "log", rm = "log", pct = "log", ret = "log", brk = "log",
  more = "log", total = "log", rmpairs = "log", rsum = "log", tab = "table", lst = "table",
  lstn = "table", lsts = "table", aba = "table", mort = "table", mrule = "table", call = "table",
  calls = "table", cr = "table", crn = "table", shown = "table"
)

basis_by_last <- list(
  lstn = c(station = "label"), lsts = c(status = "label"), chk = c(check = "label"),
  log = c(step = "text", fn = "text", parameters = "text", reason = "text")
)

key_basis <- function(keys) {
  vapply(strsplit(keys, ":", fixed = TRUE), function(k) {
    by_last <- basis_by_last[[k[1]]][k[length(k)]]
    if (length(by_last) && !is.na(by_last)) return(unname(by_last))
    out <- basis_by_prefix[k[1]]
    if (is.na(out)) "text" else unname(out)
  }, "")
}

count_matches <- function(re, s) {
  sum(gregexpr(re, s, perl = TRUE)[[1]] > 0)
}

unesc <- function(s) {
  s <- gsub("&lt;", "<", s, fixed = TRUE)
  s <- gsub("&gt;", ">", s, fixed = TRUE)
  s <- gsub("&quot;", "\"", s, fixed = TRUE)
  gsub("&amp;", "&", s, fixed = TRUE)
}

is_stamp <- function(s) {
  !is.na(as.POSIXct(s, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
}

# A key that does not resolve gives NA, which never matches.
resolve_key <- function(key, x, bands) {
  p <- vapply(strsplit(key, ":", fixed = TRUE)[[1]], utils::URLdecode, "", USE.NAMES = FALSE)
  f <- key_resolvers()[[p[1]]]
  if (is.null(f)) return(NA_character_)
  tryCatch(fmt_value(f(p[-1], x, bands)), error = function(e) NA_character_)
}

pick <- function(v, i) {
  i <- as.integer(i)
  if (is.na(i) || i < 1 || i > length(v)) stop("index out of range")
  v[[i]]
}

known <- function(name, pool) {
  if (!name %in% pool) stop("unknown name")
  name
}

n_distinct <- function(v) length(unique(v[!is.na(v)]))

edge <- function(v, f) if (any(!is.na(v))) f(v, na.rm = TRUE) else NA

resolve_log <- function(p, x, bands) {
  lg <- x$log
  whole <- list(
    n = function() nrow(lg),
    removed_total = function() sum(lg$removed),
    n_reasons = function() {
      length(unique(unlist(lapply(lg$removed_by, function(v) names(v)[v > 0]))))
    },
    n_removing_steps = function() sum(vapply(lg$removed_by, function(v) any(v > 0), logical(1))),
    added_total = function() log_closure(lg)$added,
    closure_expected = function() log_closure(lg)$expected,
    closure_gap = function() abs(log_closure(lg)$gap),
    pairs_more = function() sum(unlist(lg$removed_by) > 0) - top_reasons
  )
  if (!is.null(whole[[p[1]]])) return(whole[[p[1]]]())
  if (p[2] == "change") {
    return(pick(lg$rows_after, p[1]) - pick(lg$rows_before, p[1]) + pick(lg$removed, p[1]))
  }
  pick(lg[[known(p[2], names(lg))]], p[1])
}

resolve_pct <- function(p, x, bands) {
  lg <- x$log
  pct_of(resolve_removal(p, x, bands), pick(lg$rows_before, p[1]))
}

resolve_breach <- function(p, x, bands) {
  b <- log_breaches(x$log)
  if (p[1] == "n") return(nrow(b))
  if (p[1] == "shown") return(min(nrow(b), breach_cap))
  if (p[1] == "records") return(length(unique(b$record)))
  i <- as.integer(p[1])
  switch(p[2], record = pick(b$record, i), step = pick(b$step, i),
         expected = pick(b$expected, i), found = pick(b$found, i),
         gap = abs(pick(b$found, i) - pick(b$expected, i)), stop("unknown field"))
}

resolve_capped <- function(p, x, bands, more) {
  n <- switch(p[1], steps = nrow(x$log), stop("unknown table"))
  if (more) n - step_cap else n
}

resolve_reason_sum <- function(p, x, bands) {
  rm <- log_removals(x$log)
  on <- rm$reason == p[1]
  if (!any(on)) stop("unknown reason")
  switch(p[2], removed = sum(rm$removed[on]), steps = sum(rm$removed[on] > 0),
         stop("unknown statistic"))
}

resolve_rule <- function(p, x, bands) {
  b <- bands$bands
  sum(b$band == p[1] & b$deciding_rule == p[2], na.rm = TRUE)
}

resolve_removal <- function(p, x, bands) {
  by <- pick(x$log$removed_by, p[1])
  by[[known(p[2], names(by))]]
}

resolve_table <- function(p, x, bands) {
  tb <- x[[known(p[1], c("detections", "deployments", "listening", "animals"))]]
  time_col <- c(detections = "datetime", deployments = "deploy_datetime", listening = "date",
                animals = "release_datetime")[[p[1]]]
  last_col <- if (p[1] == "deployments") "recover_datetime" else time_col
  switch(p[2],
    rows = nrow(tb), animals = n_distinct(tb$animal_id),
    transmitters = n_distinct(tb$transmitter), receivers = n_distinct(tb$receiver_serial),
    stations = n_distinct(tb$station_id), first = edge(tb[[time_col]], min),
    last = edge(tb[[last_col]], max), stop("unknown statistic")
  )
}

resolve_checks <- function(p, x, bands) {
  s <- if (is.data.frame(x$checks)) x$checks else x$checks$summary
  if (!"note" %in% names(s)) s$note <- ""
  row <- match(p[1], s$check)
  pick(s[[known(p[2], names(s))]], row)
}

resolve_check_count <- function(p, x, bands) {
  s <- if (is.data.frame(x$checks)) x$checks else x$checks$summary
  if (p[1] == "all") nrow(s) else sum(s$status == known(p[1], c("flag", "not run")))
}

check_rows_of <- function(x, ck) {
  r <- x$checks$rows[[ck]]
  if (!is.data.frame(r)) stop("no rows")
  r
}

resolve_listening <- function(p, x, bands) {
  lst <- x$listening
  on <- if ("listening" %in% names(lst)) {
    !is.na(lst$listening) & lst$listening
  } else {
    !lst$status %in% not_listening
  }
  switch(p[1], receiver_days = nrow(lst), stations = n_distinct(lst$station_id),
         listening = sum(on), not_listening = sum(!on), stop("unknown statistic"))
}

# The abacus rows recounted in base R from the tables: animals with a
# detection, tagged animals released inside the detections' span, and the
# transmitters of detections with no animal.
resolve_abacus <- function(p, x, bands) {
  d <- x$detections
  det_ids <- unique(d$animal_id[!is.na(d$animal_id)])
  tagged <- unique(x$animals$animal_id[!is.na(x$animals$animal_id)])
  rel <- if ("release_datetime" %in% names(x$animals)) {
    vapply(tagged, function(a) {
      r <- x$animals$release_datetime[x$animals$animal_id %in% a]
      if (all(is.na(r))) NA_real_ else min(as.numeric(r), na.rm = TRUE)
    }, numeric(1))
  } else {
    rep(NA_real_, length(tagged))
  }
  t <- as.numeric(d$datetime)
  inside <- if (length(t)) !is.na(rel) & rel >= min(t) & rel <= max(t) else rep(FALSE, length(rel))
  never <- setdiff(tagged[inside], det_ids)
  tx <- unique(d$transmitter[is.na(d$animal_id)])
  tx <- setdiff(tx, c(det_ids, never))
  animals <- length(det_ids) + length(never)
  switch(p[1], rows = animals + length(tx), animal_rows = animals, never = length(never),
         tx_rows = length(tx), left_out = length(setdiff(tagged, c(det_ids, never))),
         stop("unknown statistic"))
}

resolve_station <- function(p, x, bands) {
  lst <- x$listening
  on <- !is.na(lst$station_id) & lst$station_id == p[1]
  if (!any(on)) stop("unknown station")
  switch(p[2], station = p[1], receivers = n_distinct(lst$receiver_serial[on]),
         days = sum(on), first = min(lst$date[on]), last = max(lst$date[on]),
         stop("unknown statistic"))
}

resolve_status <- function(p, x, bands) {
  s <- x$listening$status
  known(p[1], s)
  if (p[2] == "status") p[1] else sum(s == p[1], na.rm = TRUE)
}

resolve_bands <- function(p, x, bands) {
  b <- bands$bands$band
  if (p[1] == "animals") return(length(b))
  sum(b == known(p[2], c("dead", "review", "alive", b)))
}

resolve_calls <- function(p, x, bands) {
  m <- x$mortality
  if (is.null(m)) stop("no calls")
  switch(p[1], n = nrow(m),
         dated = sum(m$call == "dead_or_shed" & !is.na(m$death_date)), stop("unknown statistic"))
}

resolve_session <- function(p, x, bands) {
  switch(p[1], R = R.version.string, platform = R.version$platform, os = utils::osVersion,
         generated = NA, as.character(utils::packageVersion(p[1])))
}

# Built at each call, so the functions it names are the ones defined when it
# runs.
key_resolvers <- function() {
  list(
    idx = function(p, x, bands) as.integer(p[1]),
    log = resolve_log,
    rm = resolve_removal,
    pct = resolve_pct,
    ret = function(p, x, bands) {
      lg <- x$log
      pct_of(pick(lg$rows_after, p[1]), pick(lg$rows_before, p[1]))
    },
    brk = resolve_breach,
    more = function(p, x, bands) resolve_capped(p, x, bands, TRUE),
    total = function(p, x, bands) resolve_capped(p, x, bands, FALSE),
    rmpairs = function(p, x, bands) nrow(log_removals(x$log)),
    rsname = function(p, x, bands) known(p[1], log_removals(x$log)$reason),
    rsum = resolve_reason_sum,
    runame = function(p, x, bands) known(p[1], bands$bands$deciding_rule),
    mrule = resolve_rule,
    rmname = function(p, x, bands) known(p[2], names(pick(x$log$removed_by, p[1]))),
    tab = resolve_table,
    src = function(p, x, bands) {
      v <- pick(x$meta$sources[[known(p[2], names(x$meta$sources))]], p[1])
      if (p[2] == "file") basename(v) else v
    },
    chk = resolve_checks,
    chk_n = resolve_check_count,
    cr = function(p, x, bands) pick(check_rows_of(x, p[1])[[p[3]]], p[2]),
    crn = function(p, x, bands) nrow(check_rows_of(x, p[1])),
    crcol = function(p, x, bands) pick(names(check_rows_of(x, p[1])), p[2]),
    shown = function(p, x, bands) min(nrow(check_rows_of(x, p[1])), report_row_cap),
    lst = resolve_listening,
    srcsum = function(p, x, bands) {
      src <- x$meta$sources
      sum(src$rows[src$table == known(p[1], src$table)])
    },
    srcn = function(p, x, bands) sum(x$meta$sources$table == known(p[1], x$meta$sources$table)),
    read = function(p, x, bands) {
      prm <- x$log$parameters[x$log$step == "detset"][1]
      seg <- grep("^rows_read = ", strsplit(prm, "; ", fixed = TRUE)[[1]], value = TRUE)
      hit <- regmatches(seg, gregexpr("[a-z]+ = [0-9]+", seg))[[1]]
      v <- as.numeric(sub("^.* = ", "", hit))
      names(v) <- sub(" = .*$", "", hit)
      v[[known(p[1], names(v))]]
    },
    aba = resolve_abacus,
    lstn = resolve_station,
    lsts = resolve_status,
    mort = resolve_bands,
    mortname = function(p, x, bands) known(p[1], c("dead", "review", "alive", bands$bands$band)),
    call = function(p, x, bands) sum(x$mortality$call == p[1]),
    calls = resolve_calls,
    callname = function(p, x, bands) known(p[1], c("alive", "dead_or_shed", "uncertain")),
    meta = function(p, x, bands) {
      v <- x$meta[[known(p[1], names(x$meta))]]
      if (is.numeric(v)) as.character(v) else v
    },
    sys = resolve_session
  )
}
