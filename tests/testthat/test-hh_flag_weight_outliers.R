# Tests for hh_flag_weight_outliers

test_that("hh_flag_weight_outliers() detects outliers using IQR rule", {
  # Values: 50, 51, 52, 53, 100 (100 is outlier using 3*IQR)
  w <- c(50, 51, 52, 53, 100)

  out <- suppressWarnings(hh_flag_weight_outliers(w))

  expect_true(5 %in% out)  # Index 5 is the outlier
})

test_that("hh_flag_weight_outliers() returns empty vector when no outliers", {
  w <- c(50, 51, 52, 53, 54)

  out <- suppressWarnings(hh_flag_weight_outliers(w))

  expect_equal(length(out), 0)
})

test_that("hh_flag_weight_outliers() handles NA values", {
  w <- c(50, 51, NA, 53, 150)

  out <- suppressWarnings(hh_flag_weight_outliers(w))

  expect_true(5 %in% out)
  expect_false(3 %in% out)
})

test_that("hh_flag_weight_outliers() handles zero IQR", {
  # All same values have IQR = 0
  w <- c(50, 50, 50, 50)

  out <- suppressWarnings(hh_flag_weight_outliers(w))

  expect_equal(length(out), 0)
})

test_that("hh_flag_weight_outliers() converts character to numeric", {
  w <- c("50", "51", "52", "53", "200")

  out <- suppressWarnings(hh_flag_weight_outliers(w))

  expect_true(5 %in% out)
})

test_that("hh_flag_weight_outliers() returns indices not names", {
  w <- c(50, 51, 52, 53, 150)

  out <- suppressWarnings(hh_flag_weight_outliers(w))

  expect_true(is.numeric(out))
  expect_true(all(out > 0))
})

test_that("hh_flag_weight_outliers() handles empty vector", {
  w <- numeric(0)

  out <- suppressWarnings(hh_flag_weight_outliers(w))

  expect_equal(length(out), 0)
})

test_that("hh_flag_weight_outliers() detects both high and low outliers", {
  # Q1=50, Q3=55, IQR=5
  # Lower bound: 50 - 3*5 = 35
  # Upper bound: 55 + 3*5 = 70
  w <- c(50, 51, 52, 53, 54, 55, 30, 80)

  out <- suppressWarnings(hh_flag_weight_outliers(w))

  expect_true(7 %in% out)  # 30 is below lower bound
  expect_true(8 %in% out)  # 80 is above upper bound
})

test_that("hh_flag_weight_outliers() warns when outliers detected", {
  w <- c(50, 51, 52, 53, 100)

  expect_warning(hh_flag_weight_outliers(w))
})
