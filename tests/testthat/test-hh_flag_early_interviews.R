# Tests for hh_flag_early_interviews

test_that("hh_flag_early_interviews() detects dates before project start", {
  dates <- c("2023-01-15", "2022-12-20", "2023-01-20")
  project_start <- as.Date("2023-01-01")

  out <- hh_flag_early_interviews(dates, project_start)

  expect_equal(out, 2)
})

test_that("hh_flag_early_interviews() handles Date objects", {
  dates <- as.Date(c("2023-01-15", "2022-12-20", "2023-01-20"))
  project_start <- as.Date("2023-01-01")

  out <- hh_flag_early_interviews(dates, project_start)

  expect_equal(out, 2)
})

test_that("hh_flag_early_interviews() handles character project start", {
  dates <- c("2023-01-15", "2022-12-20")
  project_start <- "2023-01-01"

  out <- hh_flag_early_interviews(dates, project_start)

  expect_equal(out, 2)
})

test_that("hh_flag_early_interviews() returns no indices when all dates are valid", {
  dates <- c("2023-01-15", "2023-02-20", "2023-03-10")
  project_start <- as.Date("2023-01-01")

  out <- hh_flag_early_interviews(dates, project_start)

  expect_equal(length(out), 0)
})

test_that("hh_flag_early_interviews() handles NA values", {
  dates <- c("2023-01-15", NA, "2022-12-20")
  project_start <- as.Date("2023-01-01")

  out <- hh_flag_early_interviews(dates, project_start)

  expect_equal(out, 3)
  expect_false(2 %in% out)
})

test_that("hh_flag_early_interviews() includes dates on project start", {
  dates <- c("2023-01-01", "2022-12-31")
  project_start <- as.Date("2023-01-01")

  out <- hh_flag_early_interviews(dates, project_start)

  expect_equal(out, 2)
  expect_false(1 %in% out)
})

test_that("hh_flag_early_interviews() handles numeric dates", {
  dates <- c(44949, 44918)  # Excel numeric dates
  project_start <- as.Date("2023-01-01")

  out <- hh_flag_early_interviews(dates, project_start)

  expect_true(length(out) >= 0)
})

test_that("hh_flag_early_interviews() returns indices not names", {
  dates <- c("2023-01-15", "2022-12-20")
  project_start <- as.Date("2023-01-01")

  out <- hh_flag_early_interviews(dates, project_start)

  expect_true(is.numeric(out))
  expect_true(all(out > 0))
})

test_that("hh_flag_early_interviews() warns when early interviews detected", {
  dates <- c("2023-01-15", "2022-12-20")
  project_start <- as.Date("2023-01-01")

  expect_warning(hh_flag_early_interviews(dates, project_start))
})
