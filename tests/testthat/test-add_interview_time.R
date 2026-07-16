# Tests for add_interview_time

test_that("add_interview_time() computes duration correctly", {
  df <- tibble::tibble(
    interview_start = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 11:30:00")),
    interview_end = as.POSIXct(c("2023-01-01 10:45:00", "2023-01-01 11:45:00"))
  )

  out <- add_interview_time(df)

  expect_equal(nrow(out), 2)
  expect_true("interview_duration_mins" %in% names(out))
  expect_equal(out$interview_duration_mins, c(45, 15))
})

test_that("add_interview_time() rounds to 2 decimal places", {
  df <- tibble::tibble(
    interview_start = as.POSIXct("2023-01-01 10:00:00"),
    interview_end = as.POSIXct("2023-01-01 10:02:33")
  )

  out <- add_interview_time(df)

  # 2 minutes 33 seconds = 2.55 minutes
  expect_equal(out$interview_duration_mins, 2.55)
})

test_that("add_interview_time() handles NA values correctly", {
  df <- tibble::tibble(
    interview_start = as.POSIXct(c("2023-01-01 10:00:00", NA)),
    interview_end = as.POSIXct(c("2023-01-01 10:30:00", "2023-01-01 11:00:00"))
  )

  out <- add_interview_time(df)

  expect_equal(out$interview_duration_mins[1], 30)
  expect_true(is.na(out$interview_duration_mins[2]))
})

test_that("add_interview_time() handles custom column names", {
  df <- tibble::tibble(
    start_time = as.POSIXct("2023-01-01 10:00:00"),
    end_time = as.POSIXct("2023-01-01 10:30:00")
  )

  out <- add_interview_time(df, start_col = "start_time", end_col = "end_time", new_col = "duration")

  expect_true("duration" %in% names(out))
  expect_equal(out$duration, 30)
})

test_that("add_interview_time() warns when output column already exists", {
  df <- tibble::tibble(
    interview_start = as.POSIXct("2023-01-01 10:00:00"),
    interview_end = as.POSIXct("2023-01-01 10:30:00"),
    interview_duration_mins = 999
  )

  expect_warning(add_interview_time(df))
})

test_that("add_interview_time() errors on empty dataset", {
  df <- tibble::tibble(
    interview_start = as.POSIXct(character(0)),
    interview_end = as.POSIXct(character(0))
  )

  expect_error(add_interview_time(df))
})

test_that("add_interview_time() errors on missing columns", {
  df <- tibble::tibble(
    interview_start = as.POSIXct("2023-01-01 10:00:00")
  )

  expect_error(add_interview_time(df))
})

test_that("add_interview_time() handles character datetime input", {
  df <- tibble::tibble(
    interview_start = "2023-01-01 10:00:00",
    interview_end = "2023-01-01 10:45:00"
  )

  out <- add_interview_time(df)

  expect_equal(out$interview_duration_mins, 45)
})
