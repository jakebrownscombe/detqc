# ---- Build the example NexTrak receiver log of inst/extdata ----
# Run from the package root: Rscript data-raw/nextrak_log_example.R
# The file is synthetic: serial, times and values are invented. Its layout
# follows the Fathom VDAT CSV (format 2.0.0) exports of NexTrak-R1
# receivers: DIAG_NEXTRAK records every 10 minutes with noise in dB and the
# detections counted since the record before, and an event's text in its
# Values field. The writer and its notes are in
# tests/testthat/helper-receiver-logs.R. One receiver, 800001, over three
# UTC days:
# * 1 June 2024: 5 detections, at the times below, which its DIAG_NEXTRAK
#   records count.
# * 2 June: no record at all, as after a power failure; the receiver
#   reboots at 23:59:59 and an EVENT says so.
# * 3 June: status records and no detection, a quiet day the log shows the
#   receiver ran.
# Status records kept: DIAG_NEXTRAK and HEALTH_NEXTRAK, to keep the file
# small; a real export also has TEMP, ATTITUDE and BATTERY records.

# ---- Libraries ----
source("tests/testthat/helper-receiver-logs.R")

# ---- Detections of 1 June ----
heard <- as.POSIXct(c("2024-06-01 08:03:12", "2024-06-01 08:41:50", "2024-06-01 09:15:07",
                      "2024-06-01 13:22:40", "2024-06-01 13:58:31"), tz = "UTC")
length(heard)  # detections the log counts

# ---- Write the file ----
out <- file.path("inst", "extdata", "nextrak_receiver_log.csv")
write_vdat_log(out, "800001", start = "2024-06-01 00:00:00", end = "2024-06-03 23:50:00",
               variant = "nextrak", off = list(c("2024-06-02 00:00:00", "2024-06-02 23:59:59")),
               pings = 40, records = c("DIAG_NEXTRAK", "HEALTH_NEXTRAK"), detections = heard)
file.size(out)  # bytes

# ---- Check what the file holds ----
x <- read.csv(out, header = FALSE, skip = 2, colClasses = "character", fill = TRUE,
              col.names = paste0("V", 1:26))
table(x$V1[!grepl("_DESC$", x$V1)])  # records by type
diag <- x[x$V1 == "DIAG_NEXTRAK", ]
table(substr(diag$V2, 1, 10))  # DIAG_NEXTRAK records per UTC day
stopifnot(sum(as.numeric(diag$V13)) == length(heard),
          !any(substr(diag$V2, 1, 10) == "2024-06-02"))
