# ---- Build stoney_clean: the Stoney Lake study, read, checked and filtered ----
# Run from the package root: Rscript data-raw/stoney_clean.R > data-raw/stoney_clean.Rout
# stoney_clean is the detection set the Get started page builds: the Stoney
# files of inst/extdata read and joined, sensors calibrated, the checks run
# with the depth raster, audit_listening() with every receiver named as
# working, then filter_duplicates(), filter_post_release() (6 h),
# filter_min_lag() (4,500 s), the reviewer's calls (ST-02 dead or shed at 02:00 UTC on
# 4 June, the other three alive) and filter_mortality() with a margin of
# 1 day. The export page loads it in one line instead of rerunning the steps
# the Get started, Base processing and Mortality filtering pages teach.
# Needs pkgload.

# ---- Libraries ----
pkgload::load_all(".", quiet = TRUE)

# ---- Read the Stoney files ----
in_file <- function(name) file.path("inst", "extdata", name)
specs <- read_tag_specs(in_file("stoney_tag_specs.csv"))
detections <- read_detections(in_file("stoney_detections.csv"))
deployments <- read_deployments(in_file("stoney_receivers.csv"), format = "glatos")
animals <- read_animals(in_file("stoney_detections.csv"), format = "glatos",
                        tag_specs = specs)
nrow(detections)  # detections read
nrow(deployments)  # deployments
nrow(animals)  # transmitters

# ---- Build, calibrate and check ----
dets <- calibrate_sensors(detset(detections, deployments, animals))
lake <- terra::rast(in_file("stoney_depth.tif"))
dets <- check_detset(dets, water = abs(lake))
table(dets$checks$summary$status)  # checks by status

# ---- Audit the listening days ----
# Before the filters: a detection they remove still shows the receiver
# was listening.
dets <- audit_listening(dets,
                        known_listening = unique(dets$deployments$receiver_serial))
table(dets$listening$status)  # receiver-days by status

# ---- Filter ----
dets <- filter_duplicates(dets)
dets <- filter_post_release(dets, hours = 6)
# 30 times the average delay of the tags, (120 + 180) / 2 = 150 s
dets <- filter_min_lag(dets, max_lag_s = 4500)

# ---- The reviewer's calls and the mortality cut ----
calls <- data.frame(animal_id = c("ST-01", "ST-02", "ST-03", "ST-04"),
                    call = c("alive", "dead_or_shed", "alive", "alive"),
                    death_date = c("", "2024-06-04", "", ""),
                    death_time = c("", "2024-06-04 02:00:00", "", ""))
calls_file <- file.path(tempdir(), "mortality_calls.csv")
write.csv(calls, calls_file, row.names = FALSE)
dets <- apply_mortality_calls(dets, calls_file)
dets <- filter_mortality(dets, margin_days = 1)

# ---- The log of the steps ----
print(detset_log(dets)[, c("step", "rows_before", "rows_after", "removed")])
nrow(dets$detections)  # detections left
stopifnot(nrow(dets$detections) == 1938L)

# ---- Keep no path of the build machine ----
# The set records the files it was read from, in meta$sources and in the
# "source" attribute each reader sets on its table. Keep the file names only.
base_files <- function(o) {
  at <- attributes(o)
  if (is.list(o) && !is.data.frame(o)) {
    o <- lapply(o, base_files)
    if ("file" %in% names(o) && is.character(o$file)) o$file <- basename(o$file)
  } else if (is.data.frame(o)) {
    o[] <- lapply(o, base_files)
  }
  for (n in setdiff(names(at), c("names", "row.names", "class", "dim", "dimnames"))) {
    at[[n]] <- base_files(at[[n]])
  }
  attributes(o) <- at
  o
}
dets <- base_files(dets)
dets$meta$sources$file <- basename(dets$meta$sources$file)
dets$meta$sources
# Stop if any stored string still starts at a root folder.
paths_left <- function(o) {
  s <- if (is.character(o)) sum(grepl("^(/|[A-Za-z]:[/\\\\])", o)) else 0L
  if (is.list(o)) s <- s + sum(vapply(o, paths_left, integer(1)))
  at <- attributes(o)
  for (n in setdiff(names(at), c("names", "row.names", "class"))) s <- s + paths_left(at[[n]])
  as.integer(s)
}
paths_left(dets)  # strings that hold an absolute path
stopifnot(paths_left(dets) == 0L)

# ---- Save ----
stoney_clean <- dets
save(stoney_clean, file = "data/stoney_clean.rda", compress = "xz")
file.size("data/stoney_clean.rda")  # bytes
