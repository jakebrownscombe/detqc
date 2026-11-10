# ---- Build the example receiver logs of inst/extdata/receiver_logs ----
# Run from the package root: Rscript data-raw/receiver_logs_example.R
# The files are synthetic. Their layout follows Fathom VDAT CSV exports of
# VR2Tx receivers (VEMCO DATA LOG 2.0.0: hourly DIAG and TEMP records, a
# daily HEALTH_VR2TX record, EVENT_INIT before the deployment and
# EVENT_OFFLOAD at the download). A real VR2Tx file also holds hourly
# ATTITUDE and BATTERY records; they are left out here to keep the
# installed package under 5 MB, and the test files carry them; the writer and its notes
# are in tests/testthat/helper-receiver-logs.R. The serials are those of the
# example study (R001 to R020), the dates its deployment, 14:00 UTC on
# 1 June 2024 to 15:00 UTC on 11 June 2024. Two receivers are set up to show
# the statuses audit_listening() reads from logs:
# * R007 stops logging from 18:00 UTC on 5 June to 06:00 UTC on 8 June, as
#   after a power failure, and reboots; 6 and 7 June have no status record.
# * R020 has no log file, so its days keep the statuses the detections give.
# Each DIAG record counts the detections the receiver has in the example
# (detqc_example$detections, every transmitter) since the record before, and
# nothing else, so the logs agree with the detections: a day on which a
# receiver has no detection in the example has a count of 0 in its log.

# ---- Libraries ----
source("tests/testthat/helper-receiver-logs.R")

# ---- Detections of each receiver in the example ----
load("data/detqc_example.rda")
det <- detqc_example$detections
nrow(det)  # detections, all receivers and transmitters
table(det$receiver_serial)  # detections per receiver

# ---- Write one file per receiver ----
out_dir <- file.path("inst", "extdata", "receiver_logs")
unlink(out_dir, recursive = TRUE)
dir.create(out_dir)
serials <- sprintf("R%03d", 1:19)
for (s in serials) {
  off <- if (s == "R007") list(c("2024-06-05 18:00:00", "2024-06-08 06:00:00")) else list()
  write_vdat_log(file.path(out_dir, sprintf("VR2Tx_%s_20240611_1.csv", s)), s,
                 start = "2024-06-01 14:00:00", end = "2024-06-11 15:00:00", off = off,
                 pings = 350 + 10 * match(s, serials),
                 records = c("DIAG", "TEMP", "HEALTH_VR2TX"),
                 detections = det$datetime[det$receiver_serial == s])
}
length(list.files(out_dir))  # files written
sum(file.size(list.files(out_dir, full.names = TRUE)))  # bytes

# ---- Check: logged counts equal the detections ----
logged <- vapply(serials, function(s) {
  x <- read.csv(file.path(out_dir, sprintf("VR2Tx_%s_20240611_1.csv", s)), header = FALSE,
                skip = 2, colClasses = "character", fill = TRUE, col.names = paste0("V", 1:26))
  sum(as.numeric(x$V13[x$V1 == "DIAG"]))
}, numeric(1))
heard <- as.vector(table(factor(det$receiver_serial, serials)))
stopifnot(identical(as.numeric(heard), unname(logged)))
cat("logged detections equal the example's on", length(serials), "of", length(serials),
    "receivers;", sum(logged), "detections in all\n")
