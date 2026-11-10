# Shared fixtures for the detqc tests.

example_detset <- function() {
  ex <- hard_cases()
  detset(ex$detections, ex$deployments, ex$animals)
}

# glatos 0.9.8 ships the OTN, GLATOS and VUE files the readers are tested on.
# They are used where installed, never copied.
glatos_file <- function(name) {
  testthat::skip_if_not_installed("glatos", minimum_version = "0.9.8")
  path <- system.file("extdata", name, package = "glatos")
  testthat::skip_if(!nzchar(path), paste("glatos does not ship", name))
  path
}

fixture <- function(name) testthat::test_path("fixtures", name)

# A small toy array: two stations, one receiver that moves between them.
toy_tables <- function() {
  t <- function(s) as.POSIXct(s, tz = "UTC")
  dep <- data.frame(
    receiver_serial = c("R1", "R1", "R2"), station_id = c("A", "B", "C"),
    deploy_datetime = t(c("2024-01-01 00:00:00", "2024-01-10 12:00:00", "2024-01-01 00:00:00")),
    recover_datetime = t(c("2024-01-10 12:00:00", "2024-01-20 00:00:00", NA)),
    deploy_lat = c(45, 45.001, 45.1), deploy_lon = c(-78, -78, -78.1), stringsAsFactors = FALSE
  )
  det <- data.frame(
    datetime = t(c("2024-01-05 00:00:00", "2024-01-10 12:00:00", "2024-01-15 00:00:00",
                   "2024-03-01 00:00:00", "2024-02-01 00:00:00")),
    receiver_serial = c("R1", "R1", "R1", "R2", "R9"),
    transmitter = c("T-1", "T-1", "T-1", "T-2", "T-3"),
    animal_id = c("a1", "a1", NA, "a2", NA), stringsAsFactors = FALSE
  )
  list(det = det, dep = dep)
}

find_chrome <- function() {
  cands <- c("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
             Sys.which(c("google-chrome", "chromium", "chromium-browser")))
  cands <- cands[nzchar(cands) & file.exists(cands)]
  if (length(cands)) cands[1] else ""
}

# The JSON payload a card or index carries.
read_payload <- function(file) {
  h <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  open_tag <- '<script id="detqc-data" type="application/json">'
  json <- sub(paste0("(?s).*", open_tag, "(.*?)</script>.*"), "\\1", h, perl = TRUE)
  jsonlite::fromJSON(json, simplifyVector = TRUE)
}

# A figure of the user's own for processing_report(): a ggplot of n points.
user_figure <- function(x, n = 10) {
  ggplot2::ggplot(data.frame(i = seq_len(n)), ggplot2::aes(i, i)) + ggplot2::geom_point()
}

# A plot function returns a ggplot. plot_data() builds the plot as print()
# would, so that a drawing error fails the test, and gives back its data.
plot_data <- function(p) {
  testthat::expect_s3_class(p, "ggplot")
  ggplot2::ggplot_build(p)
  p$data
}

# The text of the package's help pages, from the source man/ folder under
# devtools::load_all() and from the installed package under R CMD check.
help_text <- function(topic = NULL) {
  man <- system.file("man", package = "detqc")
  db <- if (nzchar(man)) tools::Rd_db(dir = dirname(man)) else tools::Rd_db("detqc")
  if (!is.null(topic)) db <- db[paste0(topic, ".Rd")]
  paste(vapply(db, function(rd) paste(as.character(rd), collapse = ""), ""), collapse = "\n")
}
