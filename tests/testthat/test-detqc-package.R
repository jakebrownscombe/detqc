test_that("the contract columns and data.table symbols are declared as globals", {
  contract <- c(
    ".I", ".N", ":=", "datetime", "receiver_serial", "transmitter",
    "animal_id", "station_id", "sensor_raw", "sensor_value", "sensor_unit",
    "deploy_datetime", "recover_datetime", "deploy_lat", "deploy_lon",
    "deploy_z", "x", "y"
  )
  expect_true(all(contract %in% utils::globalVariables(package = "detqc")))
})
