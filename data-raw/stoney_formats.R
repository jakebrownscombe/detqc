# ---- Build the Stoney Lake format examples in inst/extdata ----
# Run from the package root, after data-raw/detqc_example.R:
#   Rscript data-raw/stoney_formats.R
# Writes the Stoney Lake study in the three other formats the readers know,
# so that every reader example reads a Stoney file:
#   stoney_vue.csv   a VUE detection export of 3 June 2024;
#   stoney_otn.csv   the same detections as an OTN detection extract;
#   stoney_vdat.csv  a Fathom VDAT CSV of receiver R011, the station where
#                    ST-02 dies, its first 150 detections.
# The record headers of the VDAT file are those of Fathom 2.0.0 (VEMCO DATA
# LOG 2.0.0), as in tests/testthat/fixtures/extdata/vdat_example.csv. The
# files are kept small; the full study is stoney_detections.csv.
# R011 is written as a VR2Tx (model "VR2Tx-69", as its receiver log in
# inst/extdata/receiver_logs writes it), with its own transmitter, as in
# stoney_receivers.csv.

# ---- Libraries ----
library(data.table)
load("data/detqc_example.rda")
ex <- detqc_example
det <- as.data.table(ex$detections)
dep <- as.data.table(ex$deployments)
ani <- as.data.table(ex$animals)

# ---- One day of detections: 3 June 2024 ----
day <- det[as.Date(datetime) == as.Date("2024-06-03")][order(datetime, receiver_serial)]
day <- dep[, .(receiver_serial, deploy_lat, deploy_lon)][day, on = "receiver_serial"]
nrow(day)  # detections on 3 June
table(day$animal_id)
stamp <- function(t) format(t, "%Y-%m-%d %H:%M:%S", tz = "UTC")

# ---- VUE export ----
# A VUE export does not know the animal. The Stoney receivers carry the
# serials R001 to R020 of stoney_receivers.csv, written here without a model,
# because the readers split only a numeric serial from its model. Station
# positions of 0, 0 are what VUE writes when none was entered.
vue <- day[, .(`Date and Time (UTC)` = stamp(datetime),
               Receiver = receiver_serial, Transmitter = transmitter,
               `Transmitter Name` = "", `Transmitter Serial` = "",
               `Sensor Value` = sensor_raw,
               `Sensor Unit` = fifelse(is.na(sensor_raw), "", "ADC"),
               `Station Name` = station_id, Latitude = "+0", Longitude = "+0")]
fwrite(vue, "inst/extdata/stoney_vue.csv", na = "")

# ---- OTN detection extract ----
otn <- day[, .(catalogNumber = animal_id, commonName = "walleye",
               receiver = receiver_serial, station = station_id,
               tagName = transmitter, codeSpace = sub("-[0-9]+$", "", transmitter),
               sensorRaw = sensor_raw, sensorType = "", sensorValue = "", sensorUnit = "",
               dateCollectedUTC = stamp(datetime), decimalLongitude = round(deploy_lon, 6),
               decimalLatitude = round(deploy_lat, 6))]
fwrite(otn, "inst/extdata/stoney_otn.csv", na = "")

# ---- VDAT CSV of receiver R011 ----
d <- det[receiver_serial == "R011"][order(datetime)][1:150]
table(d$transmitter)
vstamp <- function(t) format(t, "%Y-%m-%d %H:%M:%OS4", tz = "UTC")
corr <- 0.0123
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
lines_det <- sprintf("DET,%s,%s,,%.6f,VR2Tx-69,R011,1,PPM,%s,%s,%s,,,,,,,,,",
                     vstamp(d$datetime - corr), vstamp(d$datetime), corr, d$transmitter, id,
                     raw)
# Five depth detections are also written as DET_SENS with the depth in
# metres, as Fathom does when it holds the tag's specification.
s <- which(!is.na(d$sensor_raw))[1:5]
cal <- ani[match(d$transmitter[s], transmitter)]
lines_sens <- sprintf(
  paste0("DET_SENS,%s,%s,,%.6f,VR2Tx-69,R011,1,PPM,%s,%s,%s,,,,,,,,,,PPM,Pressure,",
         "%.4f,m,0.15,Tag Specs"),
  vstamp(d$datetime[s] - corr), vstamp(d$datetime[s]), corr, d$transmitter[s], id[s], raw[s],
  cal$sensor_intercept + cal$sensor_slope * d$sensor_raw[s]
)
cfg <- paste0("CFG_TRANSMITTER,2024-06-01 14:00:00,2024-06-01 14:00:00.0001,,0.000100,",
              "VR2Tx-69,R011,PPM,A69-1601-60011,60011,VERY_HIGH,540,660")
lines <- c("VEMCO DATA LOG,2.0.0,vdati-14.10.1-20251118-de93af-release",
           paste(c("RECORD TYPE", rep("FIELD", 25)), collapse = ","),
           desc, cfg, lines_det, lines_sens)
writeLines(lines, "inst/extdata/stoney_vdat.csv", useBytes = TRUE)
length(lines_det)  # DET records
length(lines_sens)  # DET_SENS records

# ---- Sizes ----
file.size(file.path("inst/extdata", c("stoney_vue.csv", "stoney_otn.csv", "stoney_vdat.csv")))
