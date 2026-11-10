# Fixtures for the reader, writer and converter tests.

# The format examples of a second synthetic array (VUE, VDAT, OTN, GLATOS
# and a tag sheet with a two-code tag) are test fixtures; the Stoney Lake
# files install with the package. A name is looked up in fixtures/extdata
# first.
ext_file <- function(name) {
  local <- testthat::test_path("fixtures", "extdata", name)
  if (file.exists(local)) return(normalizePath(local))
  system.file("extdata", name, package = "detqc")
}

write_lines_file <- function(lines, ext = ".csv", env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ext, .local_envir = env)
  writeLines(lines, f, useBytes = TRUE)
  f
}

# A VDAT CSV with the record headers Fathom writes, `det` DET rows and
# optional DET_SENS rows.
vdat_lines <- function(det, sens = character(), cfg = character()) {
  c("VEMCO DATA LOG,2.0.0,vdati-14.10.1-20251118-de93af-release",
    "RECORD TYPE,FIELD,FIELD,FIELD",
    paste0("CFG_TRANSMITTER_DESC,Device Time (UTC),Time,Time Offset (h),Time Correction (s),",
           "Model,Serial Number,Transmission Type,Full ID,ID,Power Level,Min Delay (s),",
           "Max Delay (s)"),
    paste0("DET_DESC,Device Time (UTC),Time,Time Offset (h),Time Correction (s),Model,",
           "Serial Number,Channel,Detection Type,Full ID,ID,Raw Data,Transmitter Serial,",
           "Signal Strength (dB),Noise (dB),Gain (dB),Quality Score,Station Name,Latitude,",
           "Longitude,GPS HDOP"),
    paste0("DET_SENS_DESC,Device Time (UTC),Time,Time Offset (h),Time Correction (s),Model,",
           "Serial Number,Channel,Detection Type,Full ID,ID,Raw Data,Transmitter Serial,",
           "Signal Strength (dB),Noise (dB),Gain (dB),Quality Score,Station Name,Latitude,",
           "Longitude,GPS HDOP,Transmitter Type,Sensor Function,Sensor Value,Sensor Unit,",
           "Sensor Precision,Definition Source"),
    cfg, det, sens)
}
