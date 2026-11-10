# ---- Build the workbook fixture of the sheet reader tests ----
# Run from the package root: Rscript data-raw/fixture_workbook.R
# Writes tests/testthat/fixtures/otn_workbook_example.xlsx, a synthetic OTN
# metadata workbook: a notes sheet with no table, a deployment sheet and a
# tagging sheet. Each table sits under a title row and a blank row, as in
# the OTN templates, and its times are Excel date cells, so the test covers
# the header search and the conversion of Excel date numbers. The values are
# invented; the stations, serials and tags exist nowhere.
# Needs openxlsx.

utc <- function(s) as.POSIXct(s, tz = "UTC")

# ---- Tables ----
deployments <- data.frame(
  OTN_ARRAY = "SYN",
  STATION_NO = c("1", "2", "3"),
  DEPLOY_DATE_TIME = utc(c("2024-05-01 12:00:00", "2024-05-02 08:30:00", "2024-05-02 15:45:00")),
  DEPLOY_LAT = c(46.10, 46.12, 46.15),
  DEPLOY_LONG = c(-81.10, -81.14, -81.18),
  INS_MODEL_NO = "VR2W",
  INS_SERIAL_NO = c("900001", "900002", "900003"),
  RECOVER_DATE_TIME = utc(c("2024-10-01 12:00:00", "2024-10-01 13:00:00", NA)),
  stringsAsFactors = FALSE
)
tagging <- data.frame(
  ANIMAL_ID = c("SYN-A1", "SYN-A2"),
  TAG_CODE_SPACE = "A69-9001",
  TAG_ID_CODE = c("101", "102"),
  TAG_MODEL = "V9-2x",
  EST_TAG_LIFE = c("365 days", "365 days"),
  UTC_RELEASE_DATE_TIME = utc(c("2024-05-03 10:00:00", "2024-05-03 11:30:00")),
  RELEASE_LATITUDE = c(46.11, 46.13),
  RELEASE_LONGITUDE = c(-81.12, -81.16),
  COMMON_NAME_E = "walleye",
  stringsAsFactors = FALSE
)
nrow(deployments)  # deployments written
nrow(tagging)  # animals written

# ---- Workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Notes")
openxlsx::writeData(wb, "Notes", c(
  "Synthetic OTN metadata workbook for the detqc tests.",
  "No value in it describes a real station, receiver or animal."
))
add_table <- function(sheet, title, df) {
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, title, startRow = 1)
  openxlsx::writeData(wb, sheet, df, startRow = 3)
}
add_table("Deployment", "Receiver deployments (synthetic)", deployments)
add_table("Tagging", "Tag releases (synthetic)", tagging)
out <- "tests/testthat/fixtures/otn_workbook_example.xlsx"
openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
file.size(out)  # bytes written
names(loadWorkbook(out))  # sheets written
