# ---- Build the example OTN deployment sheet in inst/extdata ----
# Run from the package root, after data-raw/detqc_example.R:
#   Rscript data-raw/stoney_otn_deployments.R
# Writes stoney_otn_deployments.csv: the 20 synthetic receivers of the
# Stoney Lake example (detqc_example$deployments) in the columns of the OTN
# instrument deployment short form, with its TRANSMITTER column. R001 to
# R019 are VR2Tx transceivers (model "VR2Tx-69", as the example receiver
# logs write it), and TRANSMITTER holds the transmitter built into each,
# A69-1601-600nn, as the VDAT example of R011 writes it. R020 is a VR2W,
# which has none; its TRANSMITTER is a sentinel tag on the mooring, written
# without its code space, "60020", as a sheet filled in by hand might hold
# it, so that read_deployments() shows its warning. OTN_ARRAY "S" and
# STATION_NO "001" join to the station ids of the example, "S001". No real
# data.

# ---- Libraries ----
library(data.table)
load("data/detqc_example.rda")
dep <- as.data.table(detqc_example$deployments)
nrow(dep)  # receivers

# ---- Short form ----
iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
num <- as.integer(sub("^R", "", dep$receiver_serial))
otn <- dep[, list(
  OTN_ARRAY = "S", STATION_NO = sub("^S", "", station_id),
  DEPLOY_DATE_TIME = iso(deploy_datetime), DEPLOY_LAT = round(deploy_lat, 7),
  DEPLOY_LONG = round(deploy_lon, 7), INS_DEPTH = round(deploy_z, 2),
  INS_MODEL_NO = ifelse(receiver_serial == "R020", "VR2W", "VR2Tx-69"),
  INS_SERIAL_NO = receiver_serial,
  TRANSMITTER = sprintf("A69-1601-600%02d", num),
  RECOVER_DATE_TIME = iso(recover_datetime)
)]
otn[INS_SERIAL_NO == "R020", TRANSMITTER := "60020"]
fwrite(otn, "inst/extdata/stoney_otn_deployments.csv", na = "")
nrow(otn)  # rows written
file.size("inst/extdata/stoney_otn_deployments.csv")  # bytes
