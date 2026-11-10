# ---- Open the review ----

#' Open the mortality review in a browser
#'
#' Opens the review index in `dir` with [utils::browseURL()], or builds the
#' cards and index first with [build_mortality_cards()]. It builds when
#' `bands` is given, or when `dir` holds no `index.html` yet; with no
#' `bands`, a first build uses [detect_mortality()] at its default rules,
#' and a message says so.
#'
#' `review_mortality()` does not rebuild an existing review unless `bands`
#' is given. When the index in
#' `dir` was built from other detections, carries no key of its detections,
#' or lacks the alive-band cards that `all = TRUE` asks for, and no `bands`
#' are given, `review_mortality()` stops and names the call that rebuilds
#' it with the rules you chose. Calls made in the browser are kept through
#' a rebuild in the same folder. The page and the cards are static files:
#' no R session runs behind them, and they can be sent to a co-author who
#' has no R. Calls made in the browser stay in that browser until they are
#' exported as CSV.
#'
#' @param x The `detset`.
#' @param dir Folder that holds, or will hold, `index.html`.
#' @param bands Optional result of [detect_mortality()]. When given, the cards
#'   and index are written again.
#' @param all Passed to [build_mortality_cards()]: `TRUE` writes a card for
#'   the alive band too.
#' @param open If `FALSE`, the page is not opened.
#' @param control Passed to [build_mortality_cards()]: `title`, `store`,
#'   `outline` and `cores`.
#' @return `x`, invisibly, with a log record. When the review was built, `x`
#'   carries the build's record and `x$cards`, whose attribute `index`
#'   holds the path of `index.html`; when an existing review was opened, the
#'   record of `review_mortality` says so.
#' @export
#' @examples
#' study <- detqc_example
#' dets <- detset(study$detections, study$deployments, study$animals)
#' dets <- review_mortality(dets, file.path(tempdir(), "review_open"), open = FALSE)
#' basename(attr(dets$cards, "index"))
review_mortality <- function(x, dir, bands = NULL, all = FALSE, open = interactive(),
                             control = list()) {
  validate_detset(x)
  if (!is.character(dir) || length(dir) != 1) stop_detqc("`dir` must be one folder path.")
  index <- file.path(dir, "index.html")
  if (is.null(bands)) bands <- first_build_bands(index, x, all, dir)
  if (!is.null(bands)) {
    x <- build_mortality_cards(x, bands, dir, all = all, control = control)
  } else {
    before <- c(nrow(x$detections), count_animals(x$detections))
    x <- log_step(x, "review_mortality", list(dir = basename(normalizePath(dir)), all = all),
                  before, 0L, sprintf(paste("opened the review in %s, built from these %s",
                                            "of %s; no card was rewritten"),
                                      basename(normalizePath(dir)),
                                      n_of(nrow(x$detections), "detection"),
                                      n_of(count_animals(x$detections), "animal")))
  }
  if (isTRUE(open)) utils::browseURL(normalizePath(index))
  invisible(x)
}

# The bands of a first build, NULL when the review in `index` can be opened
# as it stands, and an error when it is stale. A stale review is not rebuilt
# at the default rules, because that would replace the bands a user built
# with their own.
first_build_bands <- function(index, x, all, dir) {
  stale <- stale_review(index, x, all)
  if (is.na(stale)) return(NULL)
  if (!file.exists(index)) {
    message("Building the review in the folder \"", basename(dir), "\" at the default ",
            "rules of\ndetect_mortality(); give `bands` to use your own.")
    return(detect_mortality(x))
  }
  stop_detqc(paste("The review in %s cannot be opened as it is: %s. Build it again with the",
                   "rules you chose: review_mortality(x, dir, bands = detect_mortality(x,",
                   "rules)%s)."), dir, stale, if (isTRUE(all)) ", all = TRUE" else "")
}

# Why the review in `index` cannot be opened as it is, or NA when it can.
stale_review <- function(index, x, all) {
  if (!file.exists(index)) return("no index.html yet")
  p <- tryCatch(read_card_data(index), error = function(e) NULL)
  if (is.null(p) || is.null(p$detset_key)) return("the index carries no key of its detections")
  if (!identical(p$detset_key, detset_key(x))) {
    return("the index was built from other detections")
  }
  if (isTRUE(all) && !"alive" %in% p$cards_for) return("all = TRUE asks for alive-band cards")
  NA_character_
}
