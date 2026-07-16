# Tests for hh_validate_adm

test_that("hh_validate_adm() returns empty vector when all values are valid", {
  x <- c("Region A", "Region B", "Region C", "Region A")
  valid <- c("Region A", "Region B", "Region C")

  out <- hh_validate_adm(x, valid)

  expect_equal(length(out), 0)
})

test_that("hh_validate_adm() identifies invalid administrative units", {
  x <- c("Region A", "Region B", "Region D", "Region E")
  valid <- c("Region A", "Region B", "Region C")

  out <- hh_validate_adm(x, valid)

  expect_true("Region D" %in% out)
  expect_true("Region E" %in% out)
  expect_false("Region A" %in% out)
})

test_that("hh_validate_adm() returns unique invalid values only", {
  x <- c("Invalid1", "Invalid1", "Invalid2", "Invalid2")
  valid <- c("Valid1", "Valid2")

  out <- hh_validate_adm(x, valid)

  expect_equal(length(out), 2)
  expect_true("Invalid1" %in% out)
  expect_true("Invalid2" %in% out)
})

test_that("hh_validate_adm() handles NA values", {
  x <- c("Region A", "Region B", NA, "Region D")
  valid <- c("Region A", "Region B", "Region C")

  out <- hh_validate_adm(x, valid)

  expect_true("Region D" %in% out)
  expect_false(NA %in% out)
})

test_that("hh_validate_adm() handles empty valid vector", {
  x <- c("Region A", "Region B")
  valid <- character(0)

  out <- hh_validate_adm(x, valid)

  expect_equal(length(out), 2)
})

test_that("hh_validate_adm() handles empty input vector", {
  x <- character(0)
  valid <- c("Region A", "Region B")

  out <- hh_validate_adm(x, valid)

  expect_equal(length(out), 0)
})

test_that("hh_validate_adm() handles case sensitivity", {
  x <- c("region a", "Region A", "REGION A")
  valid <- c("Region A")

  out <- hh_validate_adm(x, valid)

  expect_equal(length(out), 2)
  expect_true("region a" %in% out)
  expect_true("REGION A" %in% out)
})

test_that("hh_validate_adm() warns when invalid values are found", {
  x <- c("Region A", "Invalid")
  valid <- c("Region A", "Region B")

  expect_warning(hh_validate_adm(x, valid))
})
