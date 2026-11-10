# ---- The processing report ----

#' Write the processing report of a detection set
#'
#' Writes one self-contained HTML file that records what happened to a
#' detection set: what came in, every logged step with its counts before and
#' after, the data-entry checks, receiver listening, the detections removed
#' by reason, the mortality bands and calls, up to six figures, and the R
#' session. The sections and their order are the same in every report. Every
#' number is read from the processing log ([detset_log()]) or from the tables
#' of the detection set, never typed. A section whose step did not run says
#' so.
#'
#' The report also checks the log. Each record must start where the one
#' before it ended, lose no detection it does not count as removed, and add
#' detections only if it is a load step. The summary flags every record that
#' breaks one of these rules, and any change to the detections after the
#' last record.
#'
#' The file holds its styles and figures inline (PNG images in base64) and
#' loads nothing from outside, so it opens offline and can be sent by email.
#' No pandoc or R Markdown is needed.
#'
#' **Figures.** `figures` names up to six figures. The built-in ones are
#' `"attrition"` (detections left after each logged step, drawn from the
#' log), `"abacus"` (detections over time per animal, from `plot_abacus()`),
#' `"receivers"` (stations and their detections, from `plot_receivers()`)
#' and `"timeline"` (deployments over time, from `plot_timeline()`). The
#' receivers map is fitted to the stations that heard a detection and draws
#' `control$outline`, the lake, under them; the timeline covers the time of the
#' detections; and each figure is as tall as its rows need. A figure
#' whose plot function is missing or stops is listed as not drawn, with the
#' reason. An element of `figures` can also be a function of `x` that
#' returns a ggplot of your own; name it, and give it a caption with
#' `attr(f, "caption")`. Each figure needs its own name. The report saves
#' every figure with [ggplot2::ggsave()], 1,800 px wide at 200 dpi.
#'
#' **Adding a section.** `sections` is a named list of functions. Each takes
#' the detection set and returns a data frame (shown as a table), a
#' character vector (shown as paragraphs), or a list of `text` and `table`.
#' The name is the section heading. Added sections come after the figures,
#' marked as added, and their numbers are not traced to the log. To add a
#' section to every report in a session, set
#' `options(detqc.report_sections = list(...))`.
#'
#' @param x A `detset`.
#' @param file Path of the HTML file to write. Its folder must exist.
#' @param figures Up to six figures: names of built-in figures, or functions; see
#'   Figures. `NULL` or an empty vector draws none.
#' @param bands Optional result of [detect_mortality()], shown as the
#'   mortality bands. When `x` carries review cards from
#'   [build_mortality_cards()] or [review_mortality()], the mortality
#'   section links to their index.
#' @param sections Named list of functions that add sections; see Adding a
#'   section.
#' @param control A list of options, as [build_mortality_cards()] takes:
#'   `title`, the title of the report (default "Detection processing
#'   report"), and `outline`, an `sf` polygon of the water body in WGS 84,
#'   drawn under the stations of the receivers map.
#' @return The path of the file, invisibly.
#' @seealso [detset_log()]
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' report_file <- processing_report(dets, tempfile(fileext = ".html"), figures = NULL)
#' file.exists(report_file)
#'
#' # A section of your own: detections per animal.
#' per_animal <- function(dets) {
#'   as.data.frame(table(animal_id = dets$detections$animal_id))
#' }
#' processing_report(dets, tempfile(fileext = ".html"), figures = NULL,
#'                   sections = list("Detections per animal" = per_animal))
processing_report <- function(x, file,
                              figures = c("attrition", "abacus", "receivers", "timeline"),
                              bands = NULL,
                              sections = getOption("detqc.report_sections", list()),
                              control = list()) {
  validate_detset(x)
  check_report_file(file)
  ctl <- report_control(control)
  title <- ctl$title
  if (!is.null(bands) && !inherits(bands, "mortality_bands")) {
    stop_detqc("`bands` must come from detect_mortality().")
  }
  # The bands do not live on the detset, so their log record is written where
  # they meet it, here as in build_mortality_cards(); a record already
  # written for the same bands is not written again.
  if (!is.null(bands)) x <- log_mortality_bands(x, bands)
  figs <- figure_specs(figures)
  check_sections(sections)
  parts <- list(
    title = esc(title), css = template_text("report.css"),
    provenance = part_provenance(x, title), summary = part_summary(x, bands),
    inputs = part_inputs(x), steps = part_steps(x$log), checks = part_checks(x$checks),
    listening = part_listening(x$listening), removals = part_removals(x$log),
    mortality = paste0(part_mortality(x$mortality, bands), review_link(x$cards, file)),
    figures = part_figures(x, figs, ctl$outline),
    added = part_added(x, sections), session = part_session()
  )
  html <- fill_report(template_text("report.html"), parts)
  writeLines(enc2utf8(html), file, useBytes = TRUE)
  invisible(file)
}

# ---- Internal: arguments ----
report_control <- function(control) {
  if (!is.list(control)) stop_detqc("`control` must be a list.")
  bad <- setdiff(names(control), c("title", "outline"))
  if (length(bad)) {
    stop_detqc("`control` has no option `%s`; it takes `title` and `outline`.", bad[1])
  }
  ctl <- utils::modifyList(list(title = "Detection processing report"), control)
  check_text1(ctl$title, "title")
  ctl
}

check_report_file <- function(file) {
  if (!is_one_string(file)) stop_detqc("`file` must be one path.")
  if (!dir.exists(dirname(file))) stop_detqc("The folder %s does not exist.", dirname(file))
  invisible(TRUE)
}

check_sections <- function(sections) {
  if (!is.list(sections)) stop_detqc("`sections` must be a named list of functions.")
  if (!length(sections)) return(invisible(TRUE))
  nm <- names(sections)
  ok <- !is.null(nm) && all(nzchar(nm)) && all(vapply(sections, is.function, logical(1)))
  if (!ok) stop_detqc("`sections` must be a named list of functions.")
  invisible(TRUE)
}

# Built-in figures: the plot function each name calls (CONTRACT.md, section
# 7) and the caption, whose numbers are read from the detection set.
figure_registry <- list(
  attrition = "plot_attrition", abacus = "plot_abacus", receivers = "plot_receivers",
  timeline = "plot_timeline"
)

# The report holds at most six figures.
max_figures <- 6L

figure_specs <- function(figures) {
  if (is.null(figures) || !length(figures)) return(list())
  if (length(figures) > max_figures) {
    stop_detqc("A report holds at most six figures; %s were given.", fmt_n(length(figures)))
  }
  if (is.character(figures)) figures <- as.list(figures)
  if (!is.list(figures)) stop_detqc("`figures` must name built-in figures or give functions.")
  nm <- names(figures)
  if (is.null(nm)) nm <- rep("", length(figures))
  specs <- lapply(seq_along(figures), function(i) one_figure_spec(figures[[i]], nm[i]))
  names_in <- vapply(specs, function(f) f$name, "")
  if (anyDuplicated(names_in)) {
    stop_detqc("Each figure needs its own name; \"%s\" is given twice.",
               names_in[anyDuplicated(names_in)])
  }
  specs
}

one_figure_spec <- function(f, name) {
  if (is.function(f)) {
    if (!nzchar(name)) stop_detqc("A figure given as a function needs a name.")
    return(list(name = name, fun = f, builtin = FALSE))
  }
  if (!is_one_string(f) || !f %in% names(figure_registry)) {
    stop_detqc("Unknown figure \"%s\". Built-in figures: %s.", paste(f, collapse = " "),
               paste(names(figure_registry), collapse = ", "))
  }
  fn <- figure_registry[[f]]
  list(name = f, fn = fn, builtin = TRUE,
       fun = get0(fn, envir = asNamespace("detqc"), mode = "function", inherits = FALSE))
}

# ---- Internal: template and HTML helpers ----
template_text <- function(name) {
  path <- system.file("templates", name, package = "detqc")
  if (!nzchar(path)) stop_detqc("detqc is missing its template %s.", name)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# One pass over the template: a value that holds "{{...}}" text, such as a
# note from the log, is never filled a second time.
fill_report <- function(tpl, parts) {
  m <- gregexpr("\\{\\{[a-z_]+\\}\\}", tpl)
  keys <- gsub("[{}]", "", regmatches(tpl, m)[[1]])
  if (!setequal(keys, names(parts)) || anyDuplicated(keys)) {
    stop_detqc("The report template and the report parts do not match.")
  }
  text <- regmatches(tpl, m, invert = TRUE)[[1]]
  paste0(paste0(text[-length(text)], unlist(parts[keys]), collapse = ""), text[length(text)])
}

esc <- function(s) {
  s <- gsub("&", "&amp;", as.character(s), fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  gsub("\"", "&quot;", s, fixed = TRUE)
}

# The key names where a number comes from; the test helper report_check()
# resolves it.
src_key <- function(...) {
  parts <- vapply(list(...), function(p) utils::URLencode(as.character(p), reserved = TRUE), "")
  paste(parts, collapse = ":")
}

fmt_value <- function(v) {
  if (is_missing_value(v)) return("\u2013")
  if (inherits(v, "POSIXct")) return(format(v, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  if (is.numeric(v)) return(fmt_number(v))
  as.character(v)
}

is_missing_value <- function(v) {
  is.null(v) || !length(v) || (length(v) == 1 && is.na(v))
}

fmt_number <- function(v) {
  if (all(v == round(v))) fmt_n(v) else format(v, digits = 7)
}

traced <- function(v, key, tag = "span", cls = NULL) {
  sprintf("<%s%s data-src=\"%s\">%s</%s>", tag,
          if (is.null(cls)) "" else sprintf(" class=\"%s\"", cls), esc(key), esc(fmt_value(v)), tag)
}

td_n <- function(v, key) traced(v, key, "td", "num")

td_t <- function(v, key) traced(v, key, "td", "code")

html_table <- function(head, rows, caption = NULL, num = integer(), row_cls = NULL) {
  th <- vapply(seq_along(head), function(i) {
    sprintf("<th%s>%s</th>", if (i %in% num) " class=\"num\"" else "", head[i])
  }, "")
  cls <- if (is.null(row_cls)) rep("", length(rows)) else
    ifelse(nzchar(row_cls), sprintf(" class=\"%s\"", row_cls), "")
  tr <- sprintf("<tr%s>%s</tr>", cls, vapply(rows, paste, "", collapse = ""))
  paste0("<table>", if (!is.null(caption)) sprintf("<caption>%s</caption>", caption) else "",
         "<thead><tr>", paste(th, collapse = ""), "</tr></thead><tbody>",
         paste(tr, collapse = "\n"), "</tbody></table>")
}

pl <- function(n, word) if (n == 1) word else paste0(word, "s")

none_p <- function(text) sprintf("<p class=\"none\">%s</p>", text)

dt_dd <- function(term, value) sprintf("<dt>%s</dt><dd>%s</dd>", esc(term), value)

# ---- Internal: provenance and summary ----
part_provenance <- function(x, title) {
  created <- x$meta$created
  paste0(
    sprintf("<h1 data-check=\"given\">%s</h1>", esc(title)),
    "<p class=\"lede\">Written by processing_report() from the processing log and the tables ",
    "of one detection set. Every number below is read from them.</p>",
    "<dl class=\"facts\">",
    dt_dd("Written (UTC)", traced(lubridate::now("UTC"), "sys:generated")),
    dt_dd("detqc version", traced(detqc_version(), "sys:detqc")),
    dt_dd("Detection set created (UTC)",
          if (is.null(created)) "not recorded" else traced(created, "meta:created")),
    dt_dd("Coordinate reference system, EPSG code", crs_value(x$meta$epsg)),
    dt_dd("Logged steps", traced(nrow(x$log), "log:n")),
    "</dl>"
  )
}

crs_value <- function(epsg) {
  v <- traced(as.character(epsg), "meta:epsg")
  if (identical(as.character(epsg), "4326")) v <- paste(v, "(geographic, not projected)")
  v
}

part_summary <- function(x, bands) {
  lg <- x$log
  items <- c(summary_flow(x, lg), summary_removed(lg), summary_checks(x$checks),
             summary_listening(x$listening, x$log), summary_mortality(x$mortality, bands))
  li <- paste0("<li", names_cls(items), ">", items, "</li>", collapse = "\n")
  paste0("<ul class=\"key\">", li, "</ul>")
}

names_cls <- function(items) {
  cls <- names(items)
  if (is.null(cls)) return(rep("", length(items)))
  ifelse(nzchar(cls), sprintf(" class=\"%s\"", cls), "")
}

summary_flow <- function(x, lg) {
  n <- nrow(lg)
  if (!n) {
    return(c(flag = sprintf(
      paste("The log is empty: no step was recorded. The detection set holds %s %s",
            "of %s %s."),
      traced(nrow(x$detections), "tab:detections:rows"), pl(nrow(x$detections), "detection"),
      traced(count_animals(x$detections), "tab:detections:animals"),
      pl(count_animals(x$detections), "animal")
    )))
  }
  # The first step's animals are counted after it: detections read from a
  # network export carry no animal_id until detset() joins the animals
  # table, so a count before it would give 0 animals.
  out <- sprintf(
    paste("%s %s came in at the first logged step, %s, which left %s %s; %s",
          "%s of %s %s %s after %s logged %s."),
    traced(lg$rows_before[1], "log:1:rows_before"), pl(lg$rows_before[1], "detection"),
    traced(lg$step[1], "log:1:step"),
    traced(lg$animals_after[1], "log:1:animals_after"), pl(lg$animals_after[1], "animal"),
    traced(lg$rows_after[n], src_key("log", n, "rows_after")),
    pl(lg$rows_after[n], "detection"),
    traced(lg$animals_after[n], src_key("log", n, "animals_after")),
    pl(lg$animals_after[n], "animal"), verb(lg$rows_after[n], "remains", "remain"),
    traced(n, "log:n"), pl(n, "step")
  )
  c(out, flow_table_flags(x, lg), flow_closure(lg), flow_breaches(lg), flow_empty(lg))
}

# The table must hold what the last record says it left.
flow_table_flags <- function(x, lg) {
  n <- nrow(lg)
  out <- character()
  if (lg$rows_after[n] != nrow(x$detections)) {
    out <- c(out, flag = sprintf(
      paste("The detections changed after the last logged step: the log ends at %s",
            "detections and the table holds %s."),
      traced(lg$rows_after[n], src_key("log", n, "rows_after")),
      traced(nrow(x$detections), "tab:detections:rows")
    ))
  }
  if (lg$animals_after[n] != count_animals(x$detections)) {
    out <- c(out, flag = sprintf(
      paste("The animals changed after the last logged step: the log ends at %s %s and",
            "the table holds %s."),
      traced(lg$animals_after[n], src_key("log", n, "animals_after")),
      pl(lg$animals_after[n], "animal"),
      traced(count_animals(x$detections), "tab:detections:animals")
    ))
  }
  out
}

# In, minus removed, plus what load steps added, must equal out. The terms
# are the log's own; a gap is the sum of the breaches listed after it.
flow_closure <- function(lg) {
  cl <- log_closure(lg)
  if (cl$gap == 0) return(character())
  c(flag = sprintf(
    paste("The counts do not close: %s %s came in, %s %s removed and load steps",
          "added %s, which leaves %s, but the log ends at %s, %s %s apart."),
    traced(cl$rows_in, "log:1:rows_before"), pl(cl$rows_in, "detection"),
    traced(cl$removed, "log:removed_total"), verb(cl$removed, "was", "were"),
    traced(cl$added, "log:added_total"), traced(cl$expected, "log:closure_expected"),
    traced(cl$rows_out, src_key("log", nrow(lg), "rows_after")),
    traced(abs(cl$gap), "log:closure_gap"), pl(abs(cl$gap), "detection")
  ))
}

log_closure <- function(lg) {
  n <- nrow(lg)
  change <- lg$rows_after - (lg$rows_before - lg$removed)
  added <- sum(pmax(change, 0)[adds_rows(lg$step)])
  expected <- lg$rows_before[1] - sum(lg$removed) + added
  list(rows_in = lg$rows_before[1], removed = sum(lg$removed), added = added,
       expected = expected, rows_out = lg$rows_after[n], gap = lg$rows_after[n] - expected)
}

breach_cap <- 10L

flow_breaches <- function(lg) {
  b <- log_breaches(lg)
  if (!nrow(b)) return(character())
  k <- seq_len(min(nrow(b), breach_cap))
  out <- vapply(k, function(i) breach_sentence(b[i, ], i), "")
  names(out) <- rep("flag", length(out))
  if (nrow(b) > breach_cap) {
    out <- c(out, flag = sprintf(
      "The log breaks its arithmetic at %s places; the first %s are listed.",
      traced(nrow(b), "brk:n"), traced(breach_cap, "brk:shown")
    ))
  }
  out
}

breach_sentence <- function(b, i) {
  rec <- sprintf("Record %s (%s)", traced(b$record, src_key("brk", i, "record")),
                 traced(b$step, src_key("brk", i, "step")))
  e <- traced(b$expected, src_key("brk", i, "expected"))
  f <- traced(b$found, src_key("brk", i, "found"))
  gap <- abs(b$found - b$expected)
  g <- traced(gap, src_key("brk", i, "gap"))
  switch(b$kind,
    over = sprintf("%s counts %s %s removed, but only %s came in.", rec, f,
                   pl(b$found, "detection"), e),
    lost = sprintf("%s lost %s %s that it does not count as removed.", rec, g,
                   pl(gap, "detection")),
    added = sprintf(paste("%s ends with %s more %s than its removals allow: it",
                          "over-counts its removals, or it added rows and is not a load step."),
                    rec, g, pl(gap, "detection")),
    chain_rows = sprintf(paste("%s starts from %s %s, but the record before it ended",
                               "at %s: the detections changed between them with no record."),
                         rec, f, pl(b$found, "detection"), e),
    chain_animals = sprintf(paste("%s starts from %s %s, but the record before it ended at",
                                  "%s: the animals changed between them with no record."),
                            rec, f, pl(b$found, "animal"), e)
  )
}

flow_empty <- function(lg) {
  k <- which(lg$rows_after == 0)
  if (!length(k)) return(character())
  k <- k[1]
  c(flag = sprintf("Record %s (%s) left no detections; every later step and figure works on none.",
                   traced(k, src_key("idx", k)), traced(lg$step[k], src_key("log", k, "step"))))
}

# Distinct reason names, and the largest step-reason pairs by name, so the
# first screen says what removed the detections.
summary_removed <- function(lg) {
  total <- sum(lg$removed)
  if (!total) return("No logged step removed detections.")
  rm <- log_removals(lg)
  rm <- rm[rm$removed > 0, , drop = FALSE]
  n_reasons <- length(unique(rm$reason))
  n_steps <- length(unique(rm$record))
  head <- sprintf("%s %s removed in %s %s, for %s distinct %s.",
                  traced(total, "log:removed_total"),
                  if (total == 1) "detection was" else "detections were",
                  traced(n_steps, "log:n_removing_steps"),
                  pl(n_steps, "step"), traced(n_reasons, "log:n_reasons"),
                  pl(n_reasons, "reason"))
  rm <- rm[order(-rm$removed, rm$record), , drop = FALSE]
  k <- seq_len(min(nrow(rm), top_reasons))
  parts <- vapply(k, function(i) {
    r <- rm[i, ]
    sprintf("%s %s (%s, %s%% of %s)", traced(r$removed, src_key("rm", r$record, r$reason)),
            traced(r$reason, src_key("rmname", r$record, r$reason)),
            traced(r$step, src_key("log", r$record, "step")),
            traced(pct_of(r$removed, r$of), src_key("pct", r$record, r$reason)),
            traced(r$of, src_key("log", r$record, "rows_before")))
  }, "")
  more <- ""
  if (nrow(rm) > top_reasons) {
    more <- sprintf("; and %s more", traced(nrow(rm) - top_reasons, "log:pairs_more"))
  }
  paste0(head, " Largest first: ", paste(parts, collapse = "; "), more, ".")
}

top_reasons <- 5L

# One decimal place, unless that would round a share that is not 0 or 100
# to 0.0 or 100.0; a share of 0.03% shows as 0.03, not 0.0. A step that started
# from no rows has no share.
pct_of <- function(part, whole) {
  if (is.na(whole) || whole == 0) return(NA_character_)
  p <- 100 * part / whole
  if (p > 0 && p < 0.05) return(sprintf("%.2g", p))
  if (p < 100 && p >= 99.95) return(sprintf("%.*f", ceiling(-log10(100 - p)), p))
  sprintf("%.1f", p)
}

summary_checks <- function(checks) {
  s <- checks_summary(checks)
  if (is.null(s)) return("check_detset() was not run.")
  out <- sprintf("%s of %s data-entry checks flagged rows; checks not run: %s.",
                 traced(sum(s$status == "flag"), "chk_n:flag"), traced(nrow(s), "chk_n:all"),
                 traced(sum(s$status == "not run"), "chk_n:not run"))
  if (any(s$status == "flag")) names(out) <- "flag"
  out
}

# A receiver-day is listening only where the audit says so.
summary_listening <- function(lst, lg = NULL) {
  n_st <- length(unique(stats::na.omit(lst$station_id)))
  head <- sprintf("%s %s at %s %s",
                  traced(nrow(lst), "lst:receiver_days"), pl(nrow(lst), "receiver-day"),
                  traced(n_st, "lst:stations"), pl(n_st, "station"))
  if (!"status" %in% names(lst) || !nrow(lst)) {
    return(paste0(head, " from the deployment intervals; audit_listening() was not run, so",
                  " none is tested for listening."))
  }
  st <- sort(unique(lst$status))
  by <- paste(vapply(st, function(k) {
    words <- status_words[k]
    sprintf("%s %s%s", traced(sum(lst$status == k), src_key("lsts", k, "days")),
            traced(k, src_key("lsts", k, "status")),
            if (is.na(words)) "" else paste0(" (", words, ")"))
  }, ""), collapse = ", ")
  out <- sprintf(paste("%s, by the status audit_listening() gave them: %s. The audit counts %s",
                       "of these receiver-days as listening, silent days included, and %s as",
                       "not listening (%s)."),
                 head, by, traced(sum(listening_flag(lst)), "lst:listening"),
                 traced(sum(!listening_flag(lst)), "lst:not_listening"),
                 paste(not_listening_named(lst), collapse = ", "))
  paste0(out, cut_audit_note(lg))
}

# The statuses in plain words. "listening" names one status here, so the
# summary says "detections heard" for it and keeps "listening" for the
# days the audit counts as listening.
status_words <- c(
  listening = "detections heard", partial = "in the water for part of the day",
  silent = "nothing heard and no reference tag to show the receiver worked",
  reference_missed = "the reference tag missed, other detections heard",
  reference_silent = "nothing heard on a day a reference tag was expected",
  silent_run = "in a run of silent days", not_downloaded = "after the last download",
  logging_silent = "nothing heard while the receiver's log shows it running",
  deaf = "running by its log, but deaf to the reference tag expected",
  off = "no record in the receiver's log: not running"
)

# The not-listening statuses the report names: the three the detections and
# reference tags give, and those from receiver logs where the table has them.
not_listening_named <- function(lst) {
  base <- setdiff(not_listening, log_statuses)
  c(base, intersect(log_statuses, intersect(not_listening, lst$status)))
}

# An audit of a cut set marks working receivers silent_run; its log record
# says so, and the report repeats it beside the count.
cut_audit_note <- function(lg) {
  if (is.null(lg) || !nrow(lg)) return("")
  k <- which(lg$step == "audit_listening")
  if (!length(k) || !grepl("audited on a cut set", lg$reason[max(k)], fixed = TRUE)) return("")
  k <- max(k)
  sprintf(paste(" The audit ran on a detection set that looks cut (record %s says why), so",
                "silent_run days may be receivers that heard only the transmitters cut away."),
          traced(k, src_key("idx", k)))
}

listening_flag <- function(lst) {
  if ("listening" %in% names(lst)) return(lst$listening %in% TRUE)
  !lst$status %in% not_listening
}

summary_mortality <- function(calls, bands) {
  out <- character()
  if (!is.null(bands)) {
    b <- bands$bands$band
    out <- sprintf("%s of %s %s %s in the dead band and %s in the review band.",
                   traced(sum(b == "dead"), "mort:band:dead"), traced(length(b), "mort:animals"),
                   pl(length(b), "animal"), verb(sum(b == "dead"), "is", "are"),
                   traced(sum(b == "review"), "mort:band:review"))
  }
  if (!is.null(calls)) {
    out <- c(out, sprintf("Reviewer calls are applied for %s %s, %s of them dead_or_shed.",
                          traced(nrow(calls), "calls:n"), pl(nrow(calls), "animal"),
                          traced(sum(calls$call == "dead_or_shed"), "call:dead_or_shed")))
  }
  if (!length(out)) out <- "No mortality step ran."
  out
}

# ---- Internal: what came in ----
part_inputs <- function(x) {
  paste0(inputs_first(x$log), inputs_read(x), inputs_sources(x$meta$sources), inputs_cut(x),
         "\n",
         "<h3>The tables after the last logged step</h3>", inputs_tables(x))
}

# The counts that came in are the first record's; the table below them
# holds the counts after processing.
inputs_first <- function(lg) {
  if (!nrow(lg)) return(none_p("The log is empty, so the counts that came in are not recorded."))
  sprintf(paste("<p>At the first logged step, record %s (%s), %s %s came in, naming %s",
                "%s; the step left %s %s.</p>"),
          traced(1L, "idx:1"), traced(lg$step[1], "log:1:step"),
          traced(lg$rows_before[1], "log:1:rows_before"), pl(lg$rows_before[1], "detection"),
          traced(lg$animals_before[1], "log:1:animals_before"),
          pl(lg$animals_before[1], "animal"),
          traced(lg$animals_after[1], "log:1:animals_after"), pl(lg$animals_after[1], "animal"))
}

# Rows read against rows in the set. A GLATOS receiver file can hold tens of
# thousands of deployment rows of which a few hundred enter the set; the
# report says how many were cut.
inputs_cut <- function(x) {
  src <- x$meta$sources
  if (!is.data.frame(src) || !all(c("table", "rows") %in% names(src))) return("")
  tabs <- intersect(c("deployments", "animals"), unique(src$table))
  tabs <- tabs[vapply(tabs, function(tb) !is.null(x[[tb]]), logical(1))]
  if (!length(tabs)) return("")
  parts <- vapply(tabs, function(tb) {
    sprintf("%s, %s %s kept by the read function and %s in the detection set", tb,
            traced(sum(src$rows[src$table == tb]), src_key("srcsum", tb)),
            pl(sum(src$rows[src$table == tb]), "row"),
            traced(nrow(x[[tb]]), src_key("tab", tb, "rows")))
  }, "")
  sprintf(paste("<p>%s. Rows kept by the read functions but not in the set were cut before",
                "detset() was called, or set aside by it; record %s counts what detset() set",
                "aside.</p>"),
          paste(parts, collapse = "; "), traced(1L, "idx:1"))
}

# Rows read against rows kept, per table, from the detset record of the
# log. Per file, the rows read stay in the readers' attr(, "source"): the
# files table holds what x$meta$sources holds, and a column for them is a
# change to CONTRACT.md.
inputs_read <- function(x) {
  read <- log_rows_read(x$log)
  src <- x$meta$sources
  if (!length(read) || !is.data.frame(src) || !all(c("table", "rows") %in% names(src))) {
    return("")
  }
  tabs <- intersect(names(read), unique(src$table))
  if (!length(tabs)) return("")
  parts <- vapply(tabs, function(tb) {
    sprintf("%s: %s %s read from %s %s, %s kept", tb, traced(read[[tb]], src_key("read", tb)),
            pl(read[[tb]], "row"),
            traced(sum(src$table == tb), src_key("srcn", tb)),
            if (sum(src$table == tb) == 1) "file" else "files",
            traced(sum(src$rows[src$table == tb]), src_key("srcsum", tb)))
  }, "")
  sprintf(paste("<p>Rows read by the import functions, from record %s of the log, against the",
                "rows they kept after any cut: %s.</p>"),
          traced(1L, "idx:1"), paste(parts, collapse = "; "))
}

# The rows read per table that detset() logged in its parameters, or an
# empty vector when the log holds none, as for a detset built from tables
# with no reader record.
log_rows_read <- function(lg) {
  k <- which(lg$step == "detset")
  if (!length(k)) return(stats::setNames(numeric(), character()))
  m <- regmatches(lg$parameters[k[1]], regexec("rows_read = c\\(([^)]*)\\)", lg$parameters[k[1]]))
  if (!length(m[[1]])) return(stats::setNames(numeric(), character()))
  parts <- strsplit(m[[1]][2], ",\\s*")[[1]]
  stats::setNames(as.numeric(sub("L$", "", sub("^.*= *", "", parts))),
                  trimws(sub(" *=.*$", "", parts)))
}

inputs_sources <- function(src) {
  need <- c("table", "file", "format", "rows")
  if (!is.data.frame(src) || !all(need %in% names(src)) || !nrow(src)) {
    return(none_p(paste("No source files are recorded on this detection set: its tables were",
                        "passed to detset() in memory, or the read function did not record them.")))
  }
  rows <- lapply(seq_len(nrow(src)), function(i) {
    c(
      td_t(src$table[i], src_key("src", i, "table")),
      td_t(basename(src$file[i]), src_key("src", i, "file")),
      td_t(src$format[i], src_key("src", i, "format")),
      td_n(src$rows[i], src_key("src", i, "rows"))
    )
  })
  # Each reader keeps the rows inside cut = of read_detections(), so the
  # column counts rows kept, which can be a small part of a file's rows.
  html_table(esc(c("Table", "File", "Format", "Rows kept")), rows,
             paste("Files read into the detection set, one row per file. Rows kept counts the",
                   "rows each read function kept, after any cut; the rows read from each file are",
                   "in attr(, \"source\") of the table the read function returned."), num = 4L)
}

# Distinct counts per table, computed with data.table; the test helper
# report_check() recomputes each one with base R.
table_stats <- function(x) {
  det <- as_dt(x$detections)
  dep <- as_dt(x$deployments)
  lst <- as_dt(x$listening)
  uniq <- function(v) data.table::uniqueN(v, na.rm = TRUE)
  rng <- function(v) if (length(v) && any(!is.na(v))) range(v, na.rm = TRUE) else c(NA, NA)
  out <- list(
    detections = list(rows = nrow(det), animals = uniq(det$animal_id),
                      transmitters = uniq(det$transmitter), receivers = uniq(det$receiver_serial),
                      stations = uniq(det$station_id), first = rng(det$datetime)[1],
                      last = rng(det$datetime)[2]),
    deployments = list(rows = nrow(dep), receivers = uniq(dep$receiver_serial),
                       stations = uniq(dep$station_id), first = rng(dep$deploy_datetime)[1],
                       last = rng(dep$recover_datetime)[2]),
    listening = list(rows = nrow(lst), receivers = uniq(lst$receiver_serial),
                     stations = uniq(lst$station_id), first = rng(lst$date)[1],
                     last = rng(lst$date)[2])
  )
  if (!is.null(x$animals)) {
    ani <- as_dt(x$animals)
    out$animals <- list(rows = nrow(ani), animals = uniq(ani$animal_id),
                        transmitters = uniq(ani$transmitter))
  }
  out
}

inputs_tables <- function(x) {
  st <- table_stats(x)
  cols <- c("rows", "animals", "transmitters", "receivers", "stations", "first", "last")
  rows <- lapply(names(st), function(tb) {
    c(sprintf("<td>%s</td>", tb), vapply(cols, function(cl) {
      if (is.null(st[[tb]][[cl]])) return("<td></td>")
      td_n(st[[tb]][[cl]], src_key("tab", tb, cl))
    }, ""))
  })
  head <- c("Table", "Rows", "Animals", "Transmitters", "Receivers", "Stations", "First (UTC)",
            "Last (UTC)")
  html_table(
    esc(head), rows,
    paste("Rows and distinct values in each table of the detection set after the last logged",
          "step. Listening rows are receiver-days; the last deployment date is the latest",
          "recovery."),
    num = 2:8
  )
}

# ---- Internal: the steps ----
step_group <- function(step) {
  out <- rep("other", length(step))
  out[grepl("^read_|^detset$", step)] <- "load"
  out[grepl("^check_|^audit_", step)] <- "check"
  out[grepl("^filter_", step)] <- "filter"
  out[grepl("mortality", step)] <- "mortality"
  out
}

# A long log keeps its first rows in view and folds the rest, so the
# sections after it stay within reach. The summary names every breach by its
# record number, so a flagged row in the fold is still found.
step_cap <- 50L

part_steps <- function(lg) {
  if (!nrow(lg)) return(none_p("The log is empty: no step was recorded."))
  i <- seq_len(nrow(lg))
  change <- lg$rows_after - (lg$rows_before - lg$removed)
  rows <- lapply(i, function(k) step_row(lg, k, change[k]))
  head <- c("Step", "Name and kind", "Detections before", "Removed", "Added or lost",
            "Detections after", "Per cent retained", "Animals before", "Animals after")
  cap <- paste("One row per logged step, in order. Added or lost counts detections that changed",
               "with no logged reason: above zero, rows were added, which only a load step may",
               "do; below zero, rows were lost. Per cent retained is detections after over",
               "detections before. A flagged row breaks the log's arithmetic; the summary says",
               "how. Under each name: the kind of step (load, check, filter, mortality or",
               "other) and, when it differs from the name, the function that ran it.")
  flag <- ifelse(i %in% log_breaches(lg)$record, "flag", "")
  main <- capped_table(esc(head), rows, cap, num = c(1L, 3:9), row_cls = flag, what = "steps")
  notes <- lapply(i, function(k) {
    c(td_n(k, src_key("idx", k)), td_n(lg$at[k], src_key("log", k, "at")),
      td_t(lg$parameters[k], src_key("log", k, "parameters")),
      td_t(lg$reason[k], src_key("log", k, "reason")))
  })
  paste0(main, "<details><summary>Parameters and notes of each step</summary>",
         wide(html_table(esc(c("Step", "Logged at (UTC)", "Parameters",
                               "What the step recorded, in its own words")), notes,
                         paste("The notes are the step's own text: numbers inside them are not",
                               "checked against the counts."), num = 1:2)),
         "</details>")
}

step_row <- function(lg, k, change) {
  name <- paste0(traced(lg$step[k], src_key("log", k, "step")),
                 "<br><span class=\"fn\">", step_group(lg$step[k]), "</span>")
  if (!identical(lg$fn[k], lg$step[k])) {
    name <- paste0(name, "<br><span class=\"fn\" data-src=\"", src_key("log", k, "fn"), "\">",
                   esc(lg$fn[k]), "</span>")
  }
  c(
    td_n(k, src_key("idx", k)),
    sprintf("<td class=\"code\">%s</td>", name),
    td_n(lg$rows_before[k], src_key("log", k, "rows_before")),
    td_n(lg$removed[k], src_key("log", k, "removed")),
    td_n(change, src_key("log", k, "change")),
    td_n(lg$rows_after[k], src_key("log", k, "rows_after")),
    td_n(pct_of(lg$rows_after[k], lg$rows_before[k]), src_key("ret", k)),
    td_n(lg$animals_before[k], src_key("log", k, "animals_before")),
    td_n(lg$animals_after[k], src_key("log", k, "animals_after"))
  )
}

# The first `step_cap` rows in view and the rest in a fold, with both counts.
capped_table <- function(head, rows, caption, num, row_cls, what) {
  n <- length(rows)
  if (n <= step_cap) return(wide(html_table(head, rows, caption, num = num, row_cls = row_cls)))
  k <- seq_len(step_cap)
  paste0(
    wide(html_table(head, rows[k], caption, num = num, row_cls = row_cls[k])),
    "<details><summary>", traced(n - step_cap, src_key("more", what)), " more ", what,
    " of ", traced(n, src_key("total", what)), "</summary>",
    wide(html_table(head, rows[-k], num = num, row_cls = row_cls[-k])), "</details>"
  )
}

# A wide table scrolls inside its own box, so the page never scrolls sideways.
# Its caption sits above the box, at the page's width: inside it, the end of
# each caption line would scroll out of view with the table.
wide <- function(table) {
  m <- regmatches(table, regexec("^<table><caption>(.*?)</caption>", table, perl = TRUE))[[1]]
  cap <- ""
  if (length(m)) {
    cap <- sprintf("<p class=\"tcap\">%s</p>", m[2])
    table <- paste0("<table>", substring(table, nchar(m[1]) + 1L))
  }
  paste0(cap, "<div class=\"wide\">", table, "</div>")
}

# ---- Internal: checks ----
check_groups <- c("Receivers", "Animals and tags", "Time", "Against the detection field")

report_row_cap <- 100L

checks_summary <- function(checks) {
  s <- if (is.data.frame(checks)) checks else checks$summary
  need <- c("check", "group", "status", "flagged", "of", "unit")
  if (!is.data.frame(s) || !all(need %in% names(s))) return(NULL)
  if (!"note" %in% names(s)) s$note <- ""
  s
}

part_checks <- function(checks) {
  if (is.null(checks)) return(none_p("check_detset() was not run on this detection set."))
  s <- checks_summary(checks)
  if (is.null(s)) {
    return(none_p("The check results on this detection set are not in the form the contract sets."))
  }
  rows_list <- if (is.data.frame(checks)) list() else checks$rows
  groups <- c(intersect(check_groups, s$group), setdiff(unique(s$group), check_groups))
  out <- vapply(groups, function(g) {
    k <- which(s$group == g)
    paste0(sprintf("<h3>%s</h3>", esc(g)), checks_table(s[k, , drop = FALSE]),
           paste(vapply(s$check[k], function(ck) offending_rows(ck, rows_list[[ck]]), ""),
                 collapse = ""))
  }, "")
  paste(out, collapse = "\n")
}

checks_table <- function(s) {
  rows <- lapply(seq_len(nrow(s)), function(i) {
    ck <- s$check[i]
    c(td_t(ck, src_key("chk", ck, "check")),
      sprintf("<td class=\"status-%s\">%s</td>", gsub(" ", "-", s$status[i]), esc(s$status[i])),
      td_n(s$flagged[i], src_key("chk", ck, "flagged")), td_n(s$of[i], src_key("chk", ck, "of")),
      td_t(s$unit[i], src_key("chk", ck, "unit")), td_t(s$note[i], src_key("chk", ck, "note")))
  })
  wide(html_table(esc(c("Check", "Status", "Flagged", "Of", "Counting", "Note")), rows, num = 3:4,
                  row_cls = ifelse(s$status == "flag", "flag", "")))
}

offending_rows <- function(ck, rows) {
  if (!is.data.frame(rows) || !nrow(rows)) return("")
  n <- min(nrow(rows), report_row_cap)
  body <- lapply(seq_len(n), function(i) {
    vapply(names(rows), function(cl) td_t(rows[[cl]][i], src_key("cr", ck, i, cl)), "")
  })
  head <- vapply(seq_along(rows), function(j) traced(names(rows)[j], src_key("crcol", ck, j)), "")
  paste0("<details><summary>Offending rows of ", traced(ck, src_key("chk", ck, "check")), ": ",
         traced(n, src_key("shown", ck)), " shown of ", traced(nrow(rows), src_key("crn", ck)),
         "</summary>", html_table(head, body), "</details>")
}

# ---- Internal: listening ----
part_listening <- function(lst) {
  if (!nrow(lst)) return(none_p("The detection set holds no receiver-days of listening."))
  head <- if ("status" %in% names(lst)) listening_status(lst) else none_p(paste(
    "audit_listening() was not run: the receiver-days below come from the deployment",
    "intervals alone."
  ))
  paste0(head, listening_stations(lst))
}

listening_status <- function(lst) {
  tab <- as_dt(lst)[, list(days = .N), by = "status"]
  data.table::setorderv(tab, "status")
  rows <- lapply(seq_len(nrow(tab)), function(i) {
    c(
      td_t(tab$status[i], src_key("lsts", tab$status[i], "status")),
      td_n(tab$days[i], src_key("lsts", tab$status[i], "days")),
      td_n(nrow(lst), "lst:receiver_days")
    )
  })
  html_table(esc(c("Status", "Receiver-days", "Of")), rows,
             "Receiver-days by the status audit_listening() gave them.", num = 2:3)
}

# A listening table with no station_id, as from deployments read in the
# wrong format, has no row per station; the grouped min() and max() would
# then warn on an empty vector.
listening_stations <- function(lst) {
  if (!any(!is.na(lst$station_id))) {
    message(paste("processing_report(): no receiver-day of the listening table names a",
                  "station, so the report has no table of receiver-days per station."))
    return(none_p("No receiver-day of the listening table names a station."))
  }
  per <- as_dt(lst)[!is.na(station_id), list(
    receivers = data.table::uniqueN(receiver_serial, na.rm = TRUE), days = .N,
    first = min(date), last = max(date)
  ), by = "station_id"]
  data.table::setorderv(per, "station_id")
  rows <- lapply(seq_len(nrow(per)), function(i) {
    st <- per$station_id[i]
    c(td_t(st, src_key("lstn", st, "station")),
      vapply(c("receivers", "days", "first", "last"), function(cl) {
        td_n(per[[cl]][i], src_key("lstn", st, cl))
      }, ""))
  })
  paste0("<details><summary>Receiver-days per station, ",
         traced(nrow(per), "lst:stations"), " stations</summary>",
         html_table(esc(c("Station", "Receivers", "Receiver-days", "First", "Last")), rows,
                    num = 2:5), "</details>")
}

# ---- Internal: removals and mortality ----
part_removals <- function(lg) {
  rm <- log_removals(lg)
  if (!nrow(rm)) return(none_p("No logged step removed detections."))
  rows <- lapply(seq_len(nrow(rm)), function(i) {
    k <- rm$record[i]
    r <- rm$reason[i]
    c(td_n(k, src_key("idx", k)), td_t(rm$step[i], src_key("log", k, "step")),
      td_t(r, src_key("rmname", k, r)), td_n(rm$removed[i], src_key("rm", k, r)),
      td_n(rm$of[i], src_key("log", k, "rows_before")),
      td_n(pct_of(rm$removed[i], rm$of[i]), src_key("pct", k, r)))
  })
  head <- esc(c("Step", "Name", "Reason", "Detections removed", "Of, before the step",
                "Per cent"))
  cap <- paste("One row per step and reason. The reason no_breakdown marks a step that",
               "logged one total.")
  if (nrow(rm) <= step_cap) return(wide(html_table(head, rows, cap, num = c(1L, 4:6))))
  paste0(removals_by_reason(rm), "<details><summary>Every step and reason, ",
         traced(nrow(rm), "rmpairs:n"), " rows</summary>",
         wide(html_table(head, rows, cap, num = c(1L, 4:6))), "</details>")
}

# A long log gives one row per reason name, so the table stays short.
removals_by_reason <- function(rm) {
  sums <- tapply(rm$removed, rm$reason, sum)
  steps <- tapply(rm$removed > 0, rm$reason, sum)
  agg <- data.frame(reason = names(sums), removed = as.numeric(sums),
                    steps = as.numeric(steps[names(sums)]), stringsAsFactors = FALSE)
  agg <- agg[order(-agg$removed, agg$reason), , drop = FALSE]
  rows <- lapply(seq_len(nrow(agg)), function(i) {
    r <- agg$reason[i]
    c(td_t(r, src_key("rsname", r)), td_n(agg$removed[i], src_key("rsum", r, "removed")),
      td_n(agg$steps[i], src_key("rsum", r, "steps")))
  })
  html_table(esc(c("Reason", "Detections removed", "Steps that removed any")), rows,
             "Detections removed by reason, summed over every step that gave it.", num = 2:3)
}

# The report links the review cards it was written beside. The link is
# relative where the cards sit below the report's folder or beside it.
review_link <- function(cards, file) {
  index <- attr(cards, "index")
  if (is.null(index) || !file.exists(index)) return("")
  sprintf(paste("<p>The review cards: <a href=\"%s\">open the card index</a>, where a person",
                "makes the call on each animal.</p>"), esc(relative_href(index, dirname(file))))
}

relative_href <- function(target, from) {
  t <- strsplit(normalizePath(target, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1]]
  f <- strsplit(normalizePath(from, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1]]
  n <- min(length(t), length(f))
  k <- which(t[seq_len(n)] != f[seq_len(n)])[1]
  k <- if (is.na(k)) n else k - 1L
  rel <- c(rep("..", length(f) - k), t[-seq_len(k)])
  utils::URLencode(paste(rel, collapse = "/"))
}

part_mortality <- function(calls, bands) {
  out <- character()
  if (!is.null(bands)) out <- c(out, mortality_bands_table(bands$bands))
  if (!is.null(calls)) out <- c(out, mortality_calls_table(calls))
  if (!length(out)) {
    return(none_p("No mortality step ran: no bands were given and no calls are applied."))
  }
  paste(out, collapse = "\n")
}

mortality_bands_table <- function(b) {
  lv <- c("dead", "review", "alive")
  lv <- c(lv, setdiff(sort(unique(b$band)), lv))
  rows <- lapply(lv, function(l) {
    c(
      td_t(l, src_key("mortname", l)), td_n(sum(b$band == l), src_key("mort", "band", l)),
      td_n(nrow(b), "mort:animals")
    )
  })
  paste0(html_table(esc(c("Band", "Animals", "Of")), rows,
                    paste("Mortality bands from detect_mortality(), one per animal with a",
                          "detection. A person decides each animal on the review pages."),
                    num = 2:3),
         mortality_rules_table(b))
}

mortality_rules_table <- function(b) {
  if (!"deciding_rule" %in% names(b) || !nrow(b)) return("")
  tab <- as_dt(b)[, list(animals = .N), by = c("band", "deciding_rule")]
  data.table::setorderv(tab, c("band", "deciding_rule"))
  rows <- lapply(seq_len(nrow(tab)), function(i) {
    bd <- tab$band[i]
    r <- tab$deciding_rule[i]
    c(td_t(bd, src_key("mortname", bd)), td_t(r, src_key("runame", r)),
      td_n(tab$animals[i], src_key("mrule", bd, r)))
  })
  html_table(esc(c("Band", "Deciding rule", "Animals")), rows,
             "Animals per band and the rule that decided the band.", num = 3L)
}

mortality_calls_table <- function(calls) {
  lv <- c("alive", "dead_or_shed", "uncertain")
  rows <- lapply(lv, function(l) {
    c(
      td_t(l, src_key("callname", l)), td_n(sum(calls$call == l), src_key("call", l)),
      td_n(nrow(calls), "calls:n")
    )
  })
  dated <- sum(calls$call == "dead_or_shed" & !is.na(calls$death_date))
  paste0(html_table(esc(c("Call", "Animals", "Of")), rows,
                    "Reviewer calls applied by apply_mortality_calls().", num = 2:3),
         "<p>Dead_or_shed calls with a death date: ", traced(dated, "calls:dated"), ".</p>")
}

# ---- Internal: figures ----
part_figures <- function(x, figs, outline = NULL) {
  if (!length(figs)) return(none_p("No figures were requested."))
  out <- vapply(figs, function(f) one_figure(x, f, outline), "")
  paste(out, collapse = "\n")
}

# The figure name is the user's text, so it is marked given and is not traced.
one_figure <- function(x, f, outline = NULL) {
  name <- sprintf("<span data-check=\"given\">%s</span>", esc(f$name))
  if (f$builtin && is.null(f$fun)) {
    return(sprintf("<p class=\"none\">%s: not drawn, %s() is not in this version of detqc.</p>",
                   name, esc(f$fn)))
  }
  if (f$builtin && f$name == "abacus" && !nrow(x$detections)) {
    return(sprintf("<p class=\"none\">%s: not drawn, no detections remain.</p>", name))
  }
  png <- draw_png(report_figure_fun(f, outline), x, figure_height(x, f))
  if (!is.null(png$error)) {
    return(sprintf(paste0("<p class=\"flag\">%s: not drawn, the plot function stopped: ",
                          "<span data-check=\"given\">%s</span></p>"),
                   name, esc(png$error)))
  }
  cap <- if (f$builtin) figure_caption(x, f$name) else user_caption(f)
  sprintf(paste0("<figure id=\"fig-%s\"><img alt=\"%s\" src=\"data:image/png;base64,%s\">",
                 "<figcaption><b data-check=\"given\">%s.</b> %s</figcaption></figure>"),
          html_id(f$name), esc(f$name), png$data, esc(f$name), cap)
}

# The report draws the receivers map fitted to the stations that heard a
# detection, with the lake, and the timeline over the detections.
report_figure_fun <- function(f, outline) {
  if (!f$builtin) return(f$fun)
  switch(f$name,
    receivers = function(x) plot_receivers(x, water = outline, fit = "heard"),
    timeline = function(x) plot_timeline(x, span = "detections"),
    f$fun
  )
}

# An HTML id holds no space; letters, digits, _ and - are kept.
html_id <- function(name) gsub("[^A-Za-z0-9_-]+", "-", name)

# Each figure is saved with ggsave() at 1,800 px wide and 200 dpi, on white.
draw_png <- function(fun, x, height = 1000) {
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  err <- tryCatch({
    res <- fun(x)
    if (!inherits(res, "ggplot")) stop_detqc("the figure function returned no ggplot")
    ggplot2::ggsave(path, res, width = 1800, height = height, units = "px", dpi = 200,
                    bg = "white")
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(err)) return(list(error = err))
  if (!file.exists(path) || !file.size(path)) return(list(error = "no image was drawn"))
  # base64_enc() wraps its output in lines; a data URI holds none.
  b64 <- jsonlite::base64_enc(readBin(path, "raw", file.size(path)))
  list(data = gsub("[\r\n]", "", b64))
}

# A figure with one row per station or animal is as tall as its rows need, 30
# px a row at 200 dpi, from 1,000 px to 4,000 px. At a fixed 1,000 px, 80
# timeline rows would get 9 px each.
figure_height <- function(x, f) {
  rows <- switch(if (f$builtin) f$name else "",
    timeline = length(unique(timeline_window(x, "detections")$dep$station_id)),
    abacus = length(abacus_rows(x, NULL)$ids),
    0L
  )
  as.integer(min(4000, max(1000, 300 + 30 * rows)))
}

figure_caption <- function(x, name) {
  switch(name,
    attrition = attrition_caption(x$log),
    abacus = abacus_caption(x),
    receivers = receivers_caption(x),
    timeline = timeline_caption(x)
  )
}

receivers_caption <- function(x) {
  ts <- table_stats(x)
  n_dep_st <- ts$deployments$stations
  n_heard <- ts$detections$stations
  sprintf(paste("Receiver stations and the detections at each: %s %s of %s %s, %s %s.",
                "The map is fitted to the %s %s that heard a detection, and only %s named;",
                "open circles heard nothing, and stations outside the map are not drawn.",
                "Scale bar length in metres or kilometres, as labelled."),
          traced(n_dep_st, "tab:deployments:stations"), pl(n_dep_st, "station"),
          traced(nrow(x$deployments), "tab:deployments:rows"),
          pl(nrow(x$deployments), "deployment"),
          traced(nrow(x$detections), "tab:detections:rows"), pl(nrow(x$detections), "detection"),
          traced(n_heard, "tab:detections:stations"), pl(n_heard, "station"),
          verb(n_heard, "it is", "they are"))
}

# The caption counts the rows plot_abacus() draws, by its own rule.
abacus_caption <- function(x) {
  rows <- abacus_rows(x, NULL)
  n_rows <- length(rows$ids)
  n_ani <- n_rows - length(rows$tx)
  sprintf(paste("Detections over time, one point per animal, station and hour: %s %s,",
                "%s to %s (UTC), on %s %s. Animals take %s of the rows, %s of them tagged",
                "animals released inside this span with no detection, marked (none);",
                "transmitters of detections with no animal, such as reference tags, take %s,",
                "labelled by code. Tagged animals with no detection released outside this span",
                "are not drawn: %s. Triangles mark releases."),
          traced(nrow(x$detections), "tab:detections:rows"), pl(nrow(x$detections), "detection"),
          traced(table_stats(x)$detections$first, "tab:detections:first"),
          traced(table_stats(x)$detections$last, "tab:detections:last"),
          traced(n_rows, "aba:rows"), pl(n_rows, "row"),
          traced(n_ani, "aba:animal_rows"),
          traced(length(rows$never), "aba:never"), traced(length(rows$tx), "aba:tx_rows"),
          traced(rows$left_out, "aba:left_out"))
}

timeline_caption <- function(x) {
  lst <- x$listening
  head <- sprintf("Receiver deployments over time: %s, %s %s", deployment_count(x),
                  traced(nrow(lst), "lst:receiver_days"), pl(nrow(lst), "receiver-day"))
  axis <- paste(" The time axis covers the detections, deployments outside it are not drawn,",
                "and stations are in natural order.")
  if (!"status" %in% names(lst)) {
    return(paste0(head, " from the deployment intervals; audit_listening() was not run.", axis))
  }
  n_on <- traced(sum(listening_flag(lst)), "lst:listening")
  n_not <- sum(!listening_flag(lst))
  # The figure draws a tick for each receiver-day not listening; at zero it has none.
  body <- if (n_not == 0) {
    sprintf(paste("; audit_listening() counts all %s as listening, silent days included, so the",
                  "figure has no ticks."), n_on)
  } else {
    sprintf(paste("; audit_listening() counts %s of them as listening, silent days included.",
                  "Ticks mark the %s %s it counts as not listening."),
            n_on, traced(n_not, "lst:not_listening"), pl(n_not, "receiver-day"))
  }
  paste0(head, body, axis)
}

# The deployments and what they ran on. A deployment with no receiver
# serial is matched by station, so a set with such deployments names its
# stations too; a set with neither serial nor station gives the count alone.
deployment_count <- function(x) {
  dep <- x$deployments
  bare <- is.na(dep$receiver_serial)
  out <- paste(traced(nrow(dep), "tab:deployments:rows"), pl(nrow(dep), "deployment"))
  n_rx <- data.table::uniqueN(dep$receiver_serial, na.rm = TRUE)
  n_st <- data.table::uniqueN(dep$station_id, na.rm = TRUE)
  rx <- paste(traced(n_rx, "tab:deployments:receivers"), pl(n_rx, "receiver"))
  st <- paste(traced(n_st, "tab:deployments:stations"), pl(n_st, "station"))
  if (!any(bare)) return(paste(out, "of", rx))
  if (!n_st) return(out)
  if (all(bare)) return(sprintf("%s with no receiver serial, at %s", out, st))
  sprintf("%s, some with no receiver serial, of %s at %s", out, rx, st)
}

# ---- The attrition ladder ----
#' Draw the detections left after each logged step
#'
#' Draws the processing log of a detection set as an attrition ladder: one
#' bar per log record, in the order the steps ran from the top, each the
#' length of the detections left after its step. A dashed line marks the
#' detections that came in at the first step. The key names two kinds of
#' bar: a record that adds up, and a record that breaks the log's
#' arithmetic, whose rows or animals before the step are not those after the
#' record above, or whose rows before, removed and after do not add up. A
#' step that removed detections carries their count inside the end of its
#' bar. [processing_report()] draws the same
#' figure as `"attrition"`. The figure is drawn from the log alone.
#'
#' @param x A [detset()].
#' @return A ggplot object; print it to draw it, or add to it with `+`. Its
#'   `data` has one row per log record: `record` (the record number and
#'   step, as the bar's label), `rows_after`, `removed` and `breach`.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' dets <- filter_duplicates(dets)
#' dets <- filter_post_release(dets, hours = 6)
#' plot_attrition(dets)
plot_attrition <- function(x) {
  if (!inherits(x, "detset")) stop_detqc("`x` must be a detset.")
  lg <- x$log
  if (!nrow(lg)) stop_detqc("the log is empty")
  n <- nrow(lg)
  bars <- data.frame(
    record = factor(paste(seq_len(n), lg$step), levels = rev(paste(seq_len(n), lg$step))),
    rows_after = lg$rows_after, removed = lg$removed,
    breach = seq_len(n) %in% log_breaches(lg)$record
  )
  # The count a step removed, inside the end of its bar; nothing on a step
  # that removed none.
  bars$label <- ifelse(!is.na(bars$removed) & bars$removed > 0,
                       paste0("-", comma_labels(bars$removed)), "")
  ggplot2::ggplot(bars, ggplot2::aes(rows_after, record, fill = breach)) +
    ggplot2::geom_col(width = 0.8, show.legend = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = label), hjust = 1.15, size = 3.6,
                       colour = "white", fontface = "bold") +
    ggplot2::geom_vline(xintercept = lg$rows_before[1], linetype = 2) +
    # Both kinds are keyed on every log, so a reader sees what a breach
    # would look like when there is none.
    ggplot2::scale_fill_manual(name = NULL, values = c("FALSE" = "#5c7a99", "TRUE" = "#a3271b"),
                               limits = c("FALSE", "TRUE"),
                               labels = c("Record adds up", "Record breaks the log's arithmetic")) +
    ggplot2::scale_x_continuous(labels = comma_labels,
                                expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = "Detections after the step", y = NULL) +
    theme_positionr() +
    ggplot2::theme(legend.position = "bottom")
}

comma_labels <- function(b) format(b, big.mark = ",", trim = TRUE, scientific = FALSE)

attrition_caption <- function(lg) {
  n <- nrow(lg)
  sprintf(paste("Attrition ladder: detections left after %s%s logged %s, one bar per",
                "log record, in order from the top, labelled with the record number and step.",
                "The dashed line marks the %s %s that came in at the first step;",
                "%s %s after the last. Records that break the log's arithmetic, as the key",
                "marks them: %s."),
          verb(n, "its ", "each of "), traced(n, "log:n"), pl(n, "step"),
          traced(lg$rows_before[1], "log:1:rows_before"),
          pl(lg$rows_before[1], "detection"),
          traced(lg$rows_after[n], src_key("log", n, "rows_after")),
          verb(lg$rows_after[n], "remains", "remain"),
          traced(length(unique(log_breaches(lg)$record)), "brk:records"))
}

user_caption <- function(f) {
  cap <- attr(f$fun, "caption")
  if (!is_one_string(cap)) cap <- "Figure supplied to processing_report()."
  sprintf("<span data-check=\"user\">%s</span>", esc(cap))
}

# ---- Internal: added sections ----
part_added <- function(x, sections) {
  if (!length(sections)) return("")
  out <- vapply(seq_along(sections), function(i) {
    body <- tryCatch(render_added(sections[[i]](x)), error = function(e) {
      sprintf("<p class=\"flag\">This section stopped: %s</p>", esc(conditionMessage(e)))
    })
    # The index keeps two names that clean to the same id apart.
    sprintf(paste0("<section class=\"added\" data-check=\"user\" id=\"added-%d-%s\">",
                   "<h2>%s</h2>%s</section>"),
            i, html_id(names(sections)[i]), esc(names(sections)[i]), body)
  }, "")
  paste(out, collapse = "\n")
}

render_added <- function(res) {
  if (is.data.frame(res)) return(plain_table(res))
  if (is.character(res)) return(paste0("<p>", esc(res), "</p>", collapse = "\n"))
  if (is.list(res) && any(c("text", "table") %in% names(res))) {
    return(paste0(if (!is.null(res$text)) render_added(as.character(res$text)) else "",
                  if (is.data.frame(res$table)) plain_table(res$table) else ""))
  }
  stop_detqc("a section must return a data frame, text, or a list of text and table.")
}

plain_table <- function(df) {
  n <- min(nrow(df), report_row_cap)
  rows <- lapply(seq_len(n), function(i) {
    vapply(names(df), function(cl) sprintf("<td>%s</td>", esc(fmt_value(df[[cl]][i]))), "")
  })
  html_table(esc(names(df)), rows)
}

# ---- Internal: session ----
session_packages <- c("detqc", "data.table", "jsonlite", "lubridate")

part_session <- function() {
  rows <- list(
    c("<td>R</td>", td_t(R.version.string, "sys:R")),
    c("<td>Platform</td>", td_t(R.version$platform, "sys:platform")),
    c("<td>Operating system</td>", td_t(utils::osVersion, "sys:os"))
  )
  pk <- lapply(session_packages, function(p) {
    ver <- as.character(utils::packageVersion(p))
    c(sprintf("<td>%s</td>", esc(p)), td_t(ver, src_key("sys", p)))
  })
  html_table(esc(c("Item", "Version")), c(rows, pk),
             "R and the packages detqc imports, in the session that wrote this report.")
}
