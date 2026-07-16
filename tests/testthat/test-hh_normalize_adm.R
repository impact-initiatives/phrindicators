# Tests for hh_normalize_adm

test_that("hh_normalize_adm() converts to title case", {
  x <- c("region a", "REGION B", "ReGiOn C")

  out <- hh_normalize_adm(x)

  expect_equal(out[1], "Region A")
  expect_equal(out[2], "Region B")
  expect_equal(out[3], "Region C")
})

test_that("hh_normalize_adm() trims whitespace", {
  x <- c("  region a  ", "region b  ", "  region c")

  out <- hh_normalize_adm(x)

  expect_equal(out[1], "Region A")
  expect_equal(out[2], "Region B")
  expect_equal(out[3], "Region C")
})

test_that("hh_normalize_adm() converts empty strings to NA", {
  x <- c("region a", "", "region c")

  out <- hh_normalize_adm(x)

  expect_equal(out[1], "Region A")
  expect_true(is.na(out[2]))
  expect_equal(out[3], "Region C")
})

test_that("hh_normalize_adm() converts whitespace-only strings to NA", {
  x <- c("region a", "   ", "region c")

  out <- hh_normalize_adm(x)

  expect_equal(out[1], "Region A")
  expect_true(is.na(out[2]))
  expect_equal(out[3], "Region C")
})

test_that("hh_normalize_adm() handles NA values", {
  x <- c("region a", NA, "region c")

  out <- hh_normalize_adm(x)

  expect_equal(out[1], "Region A")
  expect_true(is.na(out[2]))
  expect_equal(out[3], "Region C")
})

test_that("hh_normalize_adm() handles numeric input", {
  x <- c(1, 2, 3)

  out <- hh_normalize_adm(x)

  expect_equal(out[1], "1")
  expect_equal(out[2], "2")
  expect_equal(out[3], "3")
})

test_that("hh_normalize_adm() handles complex strings", {
  x <- c("north eastern region", "SOUTH-WESTERN DISTRICT", "central-north zone")

  out <- hh_normalize_adm(x)

  expect_equal(out[1], "North Eastern Region")
  expect_equal(out[2], "South-Western District")
  expect_equal(out[3], "Central-North Zone")
})

test_that("hh_normalize_adm() handles special characters", {
  x <- c("région a", "région-b", "région c'est")

  out <- hh_normalize_adm(x)

  expect_equal(out[1], "Région A")
  expect_equal(out[2], "Région-B")
  expect_equal(out[3], "Région C'Est")
})
