# ---- Build the serial-less deployment sheet fixture ----
# Run from the package root: Rscript data-raw/fixture_serialless_sheet.R
# Writes two CSV files to tests/testthat/fixtures/ with the columns of an
# OTN station deployment sheet, which names stations and no receiver
# serials, and of an OTN detection extract:
#   otn_serialless_deployments.csv  5 deployments at 4 stations
#   otn_serialless_detections.csv   14 detections of 2 transmitters
# The cases the listening audit must handle on such a sheet:
#   SYN001  two deployments in a row, the second starting the day the first
#           is recovered
#   SYN002  a download (11 January) before the recovery, and a detection
#           after it, on 15 January, so the data end moves to that detection
#   SYN003  a recovery written as a status ("failed"), read as NA
#   SYN004  in the water and silent on every day
# The values are invented; the stations, receivers and tags exist nowhere.
# Base R only.

fixtures <- file.path("tests", "testthat", "fixtures")
stopifnot(dir.exists(fixtures))

# ---- Deployments ----
dep <- data.frame(
  collectioncode = "SYN", seriescode = "SYNGlobal", ocean = "LAKE",
  station_name = c("SYN001", "SYN001", "SYN002", "SYN003", "SYN004"),
  station_type = "Acoustic",
  stn_lat = c(45.000, 45.000, 45.010, 45.020, 45.030),
  stn_long = c(-78.000, -78.000, -78.010, -78.020, -78.030),
  stationstatus = "active", locality = "Synthetic lake", model = "VR2W",
  instrumenttype = "rcvr",
  deploy_date = c("2024-01-01", "2024-01-21", "2024-01-01", "2024-01-01", "2024-01-05"),
  last_download = c("2024-01-21", "2024-01-31", "2024-01-11", "", "2024-01-25"),
  recovery_date = c("2024-01-21", "2024-01-31", "2024-01-21", "failed", "2024-01-25"),
  downloads = c(1, 1, 1, 0, 1), off_set = 0,
  stringsAsFactors = FALSE
)
nrow(dep)                          # deployments
length(unique(dep$station_name))   # stations

# ---- Detections ----
hit <- data.frame(
  datecollected = c("2024-01-02 10:00:00", "2024-01-05 11:00:00", "2024-01-10 12:00:00",
                    "2024-01-22 09:00:00", "2024-01-30 13:00:00",
                    "2024-01-03 10:00:00", "2024-01-08 10:30:00", "2024-01-08 10:45:00",
                    "2024-01-15 14:00:00",
                    "2024-01-04 08:00:00", "2024-01-12 08:00:00", "2024-01-12 09:00:00",
                    "2024-01-02 12:00:00", "2024-01-20 12:00:00"),
  station = c(rep("SYN001", 5), rep("SYN002", 4), rep("SYN003", 3), "SYN002", "SYN001"),
  collectornumber = c(rep("VR2W-900101", 3), rep("VR2W-900105", 2), rep("VR2W-900102", 4),
                      rep("VR2W-900103", 3), "VR2W-900102", "VR2W-900101"),
  catalognumber = c(rep("A69-9001-1001", 12), "A69-9001-1002", "A69-9001-1002"),
  stringsAsFactors = FALSE
)
xy <- dep[!duplicated(dep$station_name), c("station_name", "stn_lat", "stn_long")]
k <- match(hit$station, xy$station_name)
det <- data.frame(
  basisofrecord = "MachineObservation", institutioncode = "SYN", collectioncode = "SYN",
  catalognumber = hit$catalognumber, datelastmodified = "2024-02-01",
  datecollected = hit$datecollected, timezone = "UTC",
  yearcollected = 2024, monthcollected = 1,
  daycollected = as.integer(substr(hit$datecollected, 9, 10)),
  julianday = as.integer(format(as.Date(substr(hit$datecollected, 1, 10)), "%j")),
  timeofday = as.numeric(substr(hit$datecollected, 12, 13)) +
    as.numeric(substr(hit$datecollected, 15, 16)) / 60,
  latitude = xy$stn_lat[k], longitude = xy$stn_long[k], receiver = "",
  station = hit$station, collectornumber = hit$collectornumber,
  fieldnumber = hit$catalognumber, sensorname = "", sensorraw = "", the_geom = "",
  notes = "", scientificname = "", commonname = "", receiver_group = "SYN",
  tagname = hit$catalognumber, codespace = "A69-9001",
  stringsAsFactors = FALSE
)
nrow(det)                           # detections
length(unique(det$catalognumber))   # transmitters
table(det$station)                  # detections per station

# ---- Write ----
utils::write.csv(dep, file.path(fixtures, "otn_serialless_deployments.csv"), row.names = FALSE,
                 na = "")
utils::write.csv(det, file.path(fixtures, "otn_serialless_detections.csv"), row.names = FALSE,
                 na = "")
file.size(file.path(fixtures, c("otn_serialless_deployments.csv",
                                "otn_serialless_detections.csv")))  # bytes
