# Tests for hh_dms_to_decimal

test_that("hh_dms_to_decimal() converts standard DMS format to decimal degrees", {
  x <- c("0 0 0", "1 30 0", "10 20 30")

  out <- hh_dms_to_decimal(x)

  expect_equal(out[1], 0)
  expect_equal(out[2], 1.5)
  expect_equal(round(out[3], 5), 10.34167, tolerance = 0.00001)
})

test_that("hh_dms_to_decimal() handles DMS format with degree symbol", {
  x <- c("1°30'0\"", "10°20'30\"")

  out <- hh_dms_to_decimal(x)

  expect_equal(out[1], 1.5)
  expect_equal(round(out[2], 5), 10.34167, tolerance = 0.00001)
})

test_that("hh_dms_to_decimal() handles already-decimal numeric input", {
  x <- c("1.5", "10.34167", "45.5")

  out <- hh_dms_to_decimal(x)

  expect_equal(out[1], 1.5)
  expect_equal(round(out[2], 5), 10.34167, tolerance = 0.00001)
  expect_equal(out[3], 45.5)
})

test_that("hh_dms_to_decimal() handles numeric input directly", {
  x <- c(1.5, 10.34167, 45.5)

  out <- hh_dms_to_decimal(x)

  expect_equal(out[1], 1.5)
  expect_equal(round(out[2], 5), 10.34167, tolerance = 0.00001)
  expect_equal(out[3], 45.5)
})

test_that("hh_dms_to_decimal() handles whitespace variations", {
  x <- c("1  30  0", "1 30 0", "1 30   0")

  out <- hh_dms_to_decimal(x)

  expect_equal(out[1], out[2])
  expect_equal(out[2], out[3])
})

test_that("hh_dms_to_decimal() handles NA values", {
  x <- c("1 30 0", NA, "10 20 30")

  out <- hh_dms_to_decimal(x)

  expect_equal(out[1], 1.5)
  expect_true(is.na(out[2]))
  expect_equal(round(out[3], 5), 10.34167, tolerance = 0.00001)
})

test_that("hh_dms_to_decimal() handles vector of length 1", {
  x <- "45 30 15"

  out <- hh_dms_to_decimal(x)

  expect_equal(out, 45.5041667, tolerance = 0.00001)
})

test_that("hh_dms_to_decimal() handles zero values", {
  x <- c("0 0 0", "0 0 30", "0 30 0")

  out <- hh_dms_to_decimal(x)

  expect_equal(out[1], 0)
  expect_equal(round(out[2], 5), 0.00833, tolerance = 0.00001)
  expect_equal(out[3], 0.5)
})
