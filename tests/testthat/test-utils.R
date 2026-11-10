# ---- Classed conditions ----
test_that("detqc's errors and warnings carry the classes detqc_error and detqc_warning", {
  e <- tryCatch(audit_listening(data.frame()), error = function(cnd) cnd)
  expect_s3_class(e, "detqc_error")
  expect_s3_class(e, "error")
  expect_identical(conditionCall(e), quote(audit_listening(data.frame())))
  # Through lapply(), do.call() and Map() the call keeps the export's name.
  caught <- function(expr) tryCatch(expr, error = function(cnd) cnd)
  via <- list(caught(lapply("nope.csv", read_animals)),
              caught(do.call(read_animals, list("nope.csv"))),
              caught(Map(read_animals, "nope.csv")),
              caught(detqc::read_animals("nope.csv")))
  for (cnd in via) {
    expect_s3_class(cnd, "detqc_error")
    expect_identical(as.character(conditionCall(cnd)[[1]]), "read_animals")
  }
  expect_null(conditionCall(tryCatch(stop_detqc("no rows"), error = function(cnd) cnd)))
  w <- tryCatch(warn_detqc("%s of %s rows", "1", "2"), warning = function(cnd) cnd)
  expect_s3_class(w, "detqc_warning")
  expect_identical(conditionMessage(w), "1 of 2 rows")
  # A caller catches detqc's errors by class and lets others through.
  caught <- tryCatch(stop_detqc("no rows"), detqc_error = function(cnd) "caught")
  expect_identical(caught, "caught")
  expect_error(tryCatch(stop("other"), detqc_error = function(cnd) "caught"), "other")
})

test_that("a sensor unit in brackets reads as the unit inside them", {
  expect_identical(unit_key(c("[m]", "(degC)", "[ms-2]", "m", "[ADC]", NA)),
                   c("m", "C", "m/s2", "m", NA, NA))
})
