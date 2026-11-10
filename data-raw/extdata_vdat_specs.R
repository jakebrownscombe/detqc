# ---- Build the VDAT and tag specification fixtures of the reader tests ----
# Run from the package root: Rscript data-raw/extdata_vdat_specs.R
# Both files are synthetic. They are written from the hard cases of
# tests/testthat/helper-hardcases.R in the layouts of a Fathom VDAT CSV
# (VEMCO DATA LOG 2.0.0, record headers as VDAT writes them in that format)
# and of a CSV of an InnovaSea tag specification sheet.

# ---- Libraries ----
library(data.table)
# The synthetic hard cases of the tests.
source("tests/testthat/helper-hardcases.R")
ex <- hard_cases()

# ---- VDAT example: receiver 480013, the first 150 detections ----
d <- as.data.table(ex$detections)[receiver_serial == "VR2Tx-480013"][order(datetime)][1:150]
nrow(d)
table(d$transmitter)
stamp <- function(t) format(t, "%Y-%m-%d %H:%M:%OS4")
corr <- 0.0123
# The record headers, written as Fathom 2.0.0 writes them.
head_common <- paste("Device Time (UTC),Time,Time Offset (h),Time Correction (s),Model",
                     "Serial Number", sep = ",")
det_fields <- paste("Channel,Detection Type,Full ID,ID,Raw Data,Transmitter Serial",
                    "Signal Strength (dB),Noise (dB),Gain (dB),Quality Score,Station Name",
                    "Latitude,Longitude,GPS HDOP", sep = ",")
desc <- c(
  paste("CFG_TRANSMITTER_DESC", head_common,
        "Transmission Type,Full ID,ID,Power Level,Min Delay (s),Max Delay (s)", sep = ","),
  paste("DET_DESC", head_common, det_fields, sep = ","),
  paste("DET_SENS_DESC", head_common, det_fields,
        "Transmitter Type,Sensor Function,Sensor Value,Sensor Unit,Sensor Precision",
        "Definition Source", sep = ",")
)
id <- sub("^.*-", "", d$transmitter)
raw <- ifelse(is.na(d$sensor_raw), "", as.character(d$sensor_raw))
det <- sprintf("DET,%s,%s,,%.6f,VR2Tx-69,480013,1,PPM,%s,%s,%s,,,,,,,,,",
               stamp(d$datetime - corr), stamp(d$datetime), corr, d$transmitter, id, raw)
# Five sensor detections are also written as DET_SENS with the calibrated
# value, as Fathom does when it holds the tag's specification.
s <- which(!is.na(d$sensor_raw))[1:5]
sens <- sprintf(
  paste0("DET_SENS,%s,%s,,%.6f,VR2Tx-69,480013,1,PPM,%s,%s,%s,,,,,,,,,,PPM,Acceleration,",
         "%.4f,m/s2,0.01,Tag Specs"),
  stamp(d$datetime[s] - corr), stamp(d$datetime[s]), corr, d$transmitter[s], id[s], raw[s],
  0.013588 * d$sensor_raw[s]
)
cfg <- paste0("CFG_TRANSMITTER,2023-04-25 12:00:00,2023-04-25 12:00:00.0001,,0.000100,",
              "VR2Tx-69,480013,PPM,A69-1601-60013,60013,VERY_HIGH,540,660")
lines <- c("VEMCO DATA LOG,2.0.0,vdati-14.10.1-20251118-de93af-release",
           paste(c("RECORD TYPE", rep("FIELD", 25)), collapse = ","),
           desc, cfg, det, sens)
writeLines(lines, "tests/testthat/fixtures/extdata/vdat_example.csv", useBytes = TRUE)
length(det)
length(sens)

# ---- Tag specification example: the sensor transmitters of detqc_example ----
ani <- ex$animals
tx <- ani$transmitter[grepl("^A69-9004", ani$transmitter)]
# One serial per tag. EX-16 carries one tag that sends two codes, 7004 and
# 8001, so both rows take its serial, as the manufacturer's sheet lists them.
owner <- ani$animal_id[match(tx, ani$transmitter)]
spec <- data.frame(
  TAG_SERIAL_NUMBER = as.character(1500000 + match(owner, unique(owner))),
  Transmitter = tx, Tag.Type = "V13AP-1x", Tag.Life = 900,
  Min.Delay.sec = 80, Max.Delay.sec = 160,
  Sensor.Type = ifelse(grepl("-8", tx), "P", "A"),
  Units = ifelse(grepl("-8", tx), "Meters", "m/s2"),
  Slope = ifelse(grepl("-8", tx), 0.1504, 0.013588),
  Intercept = ifelse(grepl("-8", tx), -0.6018, 0)
)
write.csv(spec, "tests/testthat/fixtures/extdata/tag_specs_example.csv", row.names = FALSE)
spec
